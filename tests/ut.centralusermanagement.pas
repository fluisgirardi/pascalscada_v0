{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TCentralUserManagement: login e permissoes conferidos
            num servidor de autenticacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Numa planta com varias telas, quem diz se o operador existe e o que ele pode
  fazer nao e' cada estacao: e' um servidor central, consultado por HTTP. Este
  gerenciador e' o lado cliente disso - manda usuario e senha, recebe o
  identificador e a lista de permissoes, e guarda essa lista para nao perguntar
  de novo a cada botao da tela.

  Os testes falam com um servidor HTTP de mentira, que responde o que o teste
  mandar e anota o que recebeu. E' assim que da' para exercitar o que
  acontece quando o servidor recusa, quando ele responde besteira e quando ele
  nao responde.
}
{$ELSE}
{:
  @abstract(TCentralUserManagement tests: login and permissions checked against
            an authentication server.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On a plant with several screens, what says whether the operator exists and
  what they may do is not each station: it is a central server, asked over
  HTTP. This manager is the client side of that - it sends user and password,
  gets back an id and the list of permissions, and keeps that list so as not to
  ask again for every button on the screen.

  The tests talk to a pretend HTTP server that answers whatever the test tells
  it to and writes down what arrived. That is how what happens when the server
  refuses, when it answers nonsense and when it does not answer at all can be
  exercised.
}
{$ENDIF}
unit ut.centralusermanagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testregistry,
  CentralUserManagement, ControlSecurityManager,
  testsupport.httpstub;

type

  { TTestCentralUserManagement }

  TTestCentralUserManagement = class(TTestCase)
  private
    FServidor:TServidorHTTPDeTeste;
    FUsers:TCentralUserManagement;
    procedure ServerAnswers(aStatus:Integer; const aBody:UTF8String);
    function  Login(const aUser, aPass:UTF8String):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o login / the login
    procedure ANewManagerHasNobodyLoggedIn;
    procedure TheLoginGoesToTheServer;
    procedure TheLoginCarriesTheUserAndThePassword;
    procedure AnAcceptedLoginBringsTheUserId;
    procedure ARefusedLoginLetsNobodyIn;
    procedure AnAnswerWithoutTheUserIdIsNotAccepted;
    procedure AnAnswerThatIsNotJsonIsNotAccepted;

    //as permissoes / the permissions
    procedure ThePermissionsThatCameWithTheLoginAnswerWithoutAsking;
    procedure APermissionThatDidNotComeIsRefused;
    procedure WithNobodyLoggedInNothingIsAllowed;

    //a saida / logging out
    procedure LogoutLetsGoOfTheUser;
  end;

implementation

{ TTestCentralUserManagement }

procedure TTestCentralUserManagement.SetUp;
begin
  FServidor:=TServidorHTTPDeTeste.Create;
  FUsers:=TCentralUserManagement.Create(nil);
  FUsers.AuthServer:='127.0.0.1';
  FUsers.AuthServerPort:=FServidor.Porta;
  FUsers.UseSSL:=false;
  FUsers.UseCachedAuthorizations:=true;
  FUsers.RaiseExceptOnConnFailure:=false;
end;

procedure TTestCentralUserManagement.TearDown;
begin
  FreeAndNil(FUsers);
  FreeAndNil(FServidor);
end;

procedure TTestCentralUserManagement.ServerAnswers(aStatus:Integer; const aBody:UTF8String);
begin
  FServidor.Responder(aStatus, aBody);
end;

function TTestCentralUserManagement.Login(const aUser, aPass:UTF8String):Boolean;
var
  uid:Integer;
begin
  uid:=-1;
  Result:=FUsers.Login(aUser, aPass, uid);
end;

procedure TTestCentralUserManagement.ANewManagerHasNobodyLoggedIn;
begin
  AssertFalse('ninguem logado', FUsers.UserLogged);
  AssertTrue ('e sem identificador', FUsers.UID<0);
end;

procedure TTestCentralUserManagement.TheLoginGoesToTheServer;
begin
  ServerAnswers(200, '{"uid":7}');

  Login('fabio','segredo');

  AssertTrue  ('o servidor foi consultado', FServidor.EsperarRequisicoes(1, 3000));
  AssertEquals('no endereco de conferir usuario e senha', '/checkuserpwd', FServidor.UltimoCaminho);
end;

procedure TTestCentralUserManagement.TheLoginCarriesTheUserAndThePassword;
begin
  ServerAnswers(200, '{"uid":7}');

  Login('fabio','segredo');

  FServidor.EsperarRequisicoes(1, 3000);
  AssertTrue('o usuario foi junto', Pos('"fabio"',   FServidor.UltimoCorpo)>0);
  AssertTrue('a senha tambem',      Pos('"segredo"', FServidor.UltimoCorpo)>0);
end;

procedure TTestCentralUserManagement.AnAcceptedLoginBringsTheUserId;
begin
  ServerAnswers(200, '{"uid":7}');

  AssertTrue('entrou', Login('fabio','segredo'));

  AssertEquals('o identificador do servidor', 7, FUsers.UID);
  AssertTrue  ('e esta logado', FUsers.UserLogged);
end;

procedure TTestCentralUserManagement.ARefusedLoginLetsNobodyIn;
begin
  //401 e' o servidor dizendo que usuario ou senha nao conferem
  //401 is the server saying the user or the password do not match
  ServerAnswers(401, '{"error":"invalid user or password"}');

  AssertFalse('nao entrou', Login('fabio','errada'));

  AssertFalse('ninguem logado', FUsers.UserLogged);
  AssertTrue ('e sem identificador', FUsers.UID<0);
end;

procedure TTestCentralUserManagement.AnAnswerWithoutTheUserIdIsNotAccepted;
begin
  //servidor respondeu 200, mas sem dizer quem entrou: nao da' para logar
  //alguem sem identificador
  //the server answered 200 but did not say who came in: there is no logging
  //someone in with no id
  ServerAnswers(200, '{"nome":"fabio"}');

  AssertFalse('nao entrou', Login('fabio','segredo'));
  AssertFalse('ninguem logado', FUsers.UserLogged);
end;

procedure TTestCentralUserManagement.AnAnswerThatIsNotJsonIsNotAccepted;
begin
  ServerAnswers(200, 'isso nao e json');

  AssertFalse('nao entrou', Login('fabio','segredo'));
  AssertFalse('ninguem logado', FUsers.UserLogged);
end;

procedure TTestCentralUserManagement.ThePermissionsThatCameWithTheLoginAnswerWithoutAsking;
var
  antes:LongInt;
begin
  //a lista de permissoes vem junto com o login e fica guardada: cada botao da
  //tela pergunta uma vez, e nenhuma dessas perguntas vai ao servidor
  //the permission list comes with the login and is kept: every button on the
  //screen asks once, and none of those questions goes to the server
  ServerAnswers(200, '{"uid":7,"authorizations":{"abrir_valvula":true}}');
  Login('fabio','segredo');
  FServidor.EsperarRequisicoes(1, 3000);
  antes:=FServidor.Requisicoes;

  AssertTrue('pode abrir a valvula', FUsers.CanAccess('abrir_valvula'));

  AssertEquals('sem consultar o servidor de novo', antes, FServidor.Requisicoes);
end;

procedure TTestCentralUserManagement.APermissionThatDidNotComeIsRefused;
begin
  ServerAnswers(200, '{"uid":7,"authorizations":{"abrir_valvula":true}}');
  Login('fabio','segredo');
  FServidor.EsperarRequisicoes(1, 3000);

  AssertFalse('nao pode parar o motor', FUsers.CanAccess('parar_motor'));
end;

procedure TTestCentralUserManagement.WithNobodyLoggedInNothingIsAllowed;
begin
  AssertFalse('sem usuario, sem permissao', FUsers.CanAccess('abrir_valvula'));
end;

procedure TTestCentralUserManagement.LogoutLetsGoOfTheUser;
begin
  ServerAnswers(200, '{"uid":7,"authorizations":{"abrir_valvula":true}}');
  Login('fabio','segredo');
  FServidor.EsperarRequisicoes(1, 3000);
  AssertTrue('entrou', FUsers.UserLogged);

  FUsers.Logout;

  AssertFalse('saiu', FUsers.UserLogged);
  AssertTrue ('e sem identificador', FUsers.UID<0);
end;

initialization
  RegisterTest(TTestCentralUserManagement);

end.
