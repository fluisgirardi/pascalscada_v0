{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do controle de acesso: TBasicUserManagement,
            TCustomizedUserManagement e o TControlSecurityManager unico.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Quem decide se um botao pode ser apertado e' o gerenciador de seguranca, e
  quem responde a ele e' o gerenciador de usuarios. Nada disso precisa de tela:
  Login com usuario e senha, Logout, os codigos de seguranca e o CanAccess sao
  todos codigo comum, e e' onde um erro custa caro - um CanAccess que devolve
  true por engano libera um comando que ninguem autorizou.

  O Login sem parametros abre um dialogo modal e por isso fica de fora; o que
  se exercita aqui e' a sobrecarga Login(usuario, senha, UID), que e' a que um
  programa usa quando tem a sua propria tela de entrada.

  So pode existir um gerenciador de usuarios por programa - o construtor se
  registra no gerenciador de seguranca e recusa o segundo. Por isso cada teste
  cria e destroi o seu, e nenhum pode deixar o registro sujo para o seguinte.
}
{$ELSE}
{:
  @abstract(Access control tests: TBasicUserManagement,
            TCustomizedUserManagement and the single TControlSecurityManager.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  What decides whether a button may be pressed is the security manager, and
  what answers it is the user management. None of that needs a screen: Login
  with user and password, Logout, the security codes and CanAccess are all
  ordinary code, and that is where a mistake is expensive - a CanAccess
  returning true by accident releases a command nobody authorised.

  The parameterless Login opens a modal dialog and is therefore left out; what
  is exercised here is the Login(user, password, UID) overload, the one a
  program uses when it has its own entry screen.

  There can be only one user management per program - the constructor
  registers itself with the security manager and refuses the second one. So
  every test creates and destroys its own, and none may leave the registry
  dirty for the next.
}
{$ENDIF}
unit ut.usermanagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  BasicUserManagement, CustomizedUserManagement, ControlSecurityManager;

type

  { TUserManagementProbe }

  //A sobrecarga CanAccess(codigo, usuario) e' protegida de proposito: um
  //plugin de seguranca nao deve ser consultado direto. Quem pergunta e'
  //sempre GetControlSecurityManager.CanAccess, que repassa ao plugin
  //instalado - e' por ele que passam os testes de permissao logo abaixo. A
  //sonda alcanca a sobrecarga so porque em producao quem a chama e'
  //CheckIfUserIsAllowed, que abre um dialogo modal e nao cabe num teste.
  //
  //The CanAccess(code, user) overload is protected on purpose: a security
  //plugin is not meant to be asked directly. The one who asks is always
  //GetControlSecurityManager.CanAccess, which passes the question on to the
  //installed plugin - that is the path the permission tests below take. The
  //probe reaches the overload only because in production its caller is
  //CheckIfUserIsAllowed, which opens a modal dialog and does not fit a test.
  TUserManagementProbe = class(TCustomizedUserManagement)
  public
    function CanAccessAs(sc:UTF8String; aUID:Integer):Boolean;
  end;

  { TTestUserManagement }

  TTestUserManagement = class(TTestCase)
  private
    FUsers:TUserManagementProbe;
    //contadores dos eventos / event counters
    FSuccessCount,
    FFailureCount,
    FLogoutCount,
    FUserChangedCount:LongInt;
    FOldUser, FNewUser:UTF8String;
    //respostas programadas / programmed answers
    FValidLogin, FValidPass:UTF8String;
    FUserIdToGive:Integer;
    FGrantedCode:UTF8String;
    FCheckCalls:LongInt;

    procedure CountSuccess(Sender:TObject);
    procedure CountFailure(Sender:TObject);
    procedure CountLogout(Sender:TObject);
    procedure NoteUserChanged(Sender:TObject; const OldUsername, NewUserName:UTF8String);
    procedure AnswerUserAndPassword(user, pass:UTF8String; out aUID:Integer;
                                    var ValidUser:Boolean; LoginAction:Boolean);
    procedure GiveTheUserName(var UserInfo:UTF8String);
    procedure AnswerCanAccess(securityCode:UTF8String; var CanAccess:Boolean);
    procedure AnswerUIDCanAccess(aUID:Integer; securityCode:UTF8String; var CanAccess:Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //registro unico / single registry
    procedure ANewUserManagementRegistersItself;
    procedure ASecondUserManagementIsRefused;
    procedure DestroyingItClearsTheRegistry;

    //login e logout / login and logout
    procedure ANewUserManagementHasNobodyLoggedIn;
    procedure LoginWithTheRightPasswordLetsTheUserIn;
    procedure TheLoggedUserLoginIsReadableBack;
    procedure LoginWithTheWrongPasswordKeepsNobodyIn;
    procedure LoginCarriesTheUserIdBack;
    procedure TheThreeLoginEventsArePublished;
    procedure ASuccessfulLoginNotifiesAndReportsTheChange;
    procedure AFailedLoginDoesNotLetAnybodyIn;
    procedure AFailedLoginCallsTheFailureHook;
    procedure LogoutClearsTheUser;
    procedure LogoutWithNobodyLoggedInNotifiesNothing;
    procedure LogoutCallsTheEvent;
    procedure TheUserNameComesFromTheEventOnlyWhileLoggedIn;

    //codigos de seguranca / security codes
    procedure ARegisteredCodeIsFound;
    procedure RegisteringTheSameCodeTwiceDoesNotDuplicate;
    procedure AnUnregisteredCodeIsNotFound;
    procedure UnregisteringACodeThatIsNotThereDoesNothing;
    procedure TheListOfCodesBelongsToTheCaller;

    //permissao / permission
    procedure AnEmptyCodeIsAlwaysAllowed;
    procedure WithNobodyLoggedInNoCodeIsAllowed;
    procedure TheEventDecidesForTheLoggedUser;
    procedure PermissionByUserIdAsksTheOtherEvent;
    procedure PermissionByUserIdIsNotAskedForANegativeId;

    //gerenciador de seguranca / security manager
    procedure TheManagerIsASingleInstance;
    procedure AnEmptyCodeGoesThroughTheManagerWithoutAsking;
    procedure TheManagerDelegatesThePermissionToTheUserManagement;
    procedure TryAccessRaisesWhenThePermissionIsDenied;
    procedure TryAccessIsSilentWhenThePermissionIsGranted;
    procedure WithNoUserManagementTheManagerAllowsEverything;
  end;

implementation

uses hsstrings, typinfo;

{ TUserManagementProbe }

function TUserManagementProbe.CanAccessAs(sc:UTF8String; aUID:Integer):Boolean;
begin
  Result:=CanAccess(sc, aUID);
end;

{ TTestUserManagement }

procedure TTestUserManagement.SetUp;
begin
  FSuccessCount:=0;
  FFailureCount:=0;
  FLogoutCount:=0;
  FUserChangedCount:=0;
  FOldUser:='';
  FNewUser:='';
  FCheckCalls:=0;
  FValidLogin:='fabio';
  FValidPass:='segredo';
  FUserIdToGive:=7;
  FGrantedCode:='';

  FUsers:=TUserManagementProbe.Create(nil);
  FUsers.OnCheckUserAndPass:=@AnswerUserAndPassword;
  FUsers.SuccessfulLogin:=@CountSuccess;
  FUsers.FailureLogin:=@CountFailure;
end;

procedure TTestUserManagement.TearDown;
begin
  //destruir limpa o registro; sem isso o proximo teste nao consegue criar o
  //dele
  FreeAndNil(FUsers);
end;

procedure TTestUserManagement.CountSuccess(Sender:TObject);
begin
  inc(FSuccessCount);
end;

procedure TTestUserManagement.CountFailure(Sender:TObject);
begin
  inc(FFailureCount);
end;

procedure TTestUserManagement.CountLogout(Sender:TObject);
begin
  inc(FLogoutCount);
end;

procedure TTestUserManagement.NoteUserChanged(Sender:TObject;
  const OldUsername, NewUserName:UTF8String);
begin
  inc(FUserChangedCount);
  FOldUser:=OldUsername;
  FNewUser:=NewUserName;
end;

procedure TTestUserManagement.AnswerUserAndPassword(user, pass:UTF8String;
  out aUID:Integer; var ValidUser:Boolean; LoginAction:Boolean);
begin
  inc(FCheckCalls);
  aUID:=-1;
  ValidUser:=(user=FValidLogin) and (pass=FValidPass);
  if ValidUser then
    aUID:=FUserIdToGive;
end;

procedure TTestUserManagement.GiveTheUserName(var UserInfo:UTF8String);
begin
  UserInfo:='Fabio Luis Girardi';
end;

procedure TTestUserManagement.AnswerCanAccess(securityCode:UTF8String; var CanAccess:Boolean);
begin
  CanAccess:=(securityCode=FGrantedCode);
end;

procedure TTestUserManagement.AnswerUIDCanAccess(aUID:Integer;
  securityCode:UTF8String; var CanAccess:Boolean);
begin
  CanAccess:=(aUID=FUserIdToGive) and (securityCode=FGrantedCode);
end;

procedure TTestUserManagement.ANewUserManagementRegistersItself;
begin
  //o construtor se pendura no gerenciador de seguranca, que e' unico
  AssertSame('o registrado e o criado', FUsers,
             GetControlSecurityManager.UserManagement);
end;

procedure TTestUserManagement.ASecondUserManagementIsRefused;
var
  outro:TUserManagementProbe;
begin
  //dois gerenciadores no mesmo programa deixariam o controle de acesso
  //ambiguo; o segundo tem que ser recusado na criacao
  outro:=nil;
  try
    outro:=TUserManagementProbe.Create(nil);
    Fail('criar um segundo gerenciador tem que levantar excecao');
  except
    on e:Exception do
      AssertEquals('motivo da recusa', SUserManagementIsSet, e.Message);
  end;
  //o construtor levantou antes de se registrar, entao o objeto meio-criado
  //nao pode ser liberado por Free (o destrutor desregistraria o nosso)
  AssertSame('o registro continua com o primeiro', FUsers,
             GetControlSecurityManager.UserManagement);
  if outro<>nil then outro.Free;
end;

procedure TTestUserManagement.DestroyingItClearsTheRegistry;
begin
  FreeAndNil(FUsers);
  AssertTrue('registro vazio', GetControlSecurityManager.UserManagement=nil);

  //e ai' outro pode entrar
  FUsers:=TUserManagementProbe.Create(nil);
  AssertSame('o novo assumiu', FUsers, GetControlSecurityManager.UserManagement);
end;

procedure TTestUserManagement.ANewUserManagementHasNobodyLoggedIn;
begin
  AssertFalse('ninguem logado', FUsers.UserLogged);
  AssertEquals('sem identificador de usuario', -1, FUsers.UID);
  AssertEquals('sem login', '', FUsers.CurrentUserLogin);
  AssertEquals('sem nome', '', FUsers.CurrentUserName);
end;

procedure TTestUserManagement.LoginWithTheRightPasswordLetsTheUserIn;
var
  uid:Integer;
begin
  uid:=-1;
  AssertTrue('login aceito', FUsers.Login('fabio', 'segredo', uid));
  AssertTrue('usuario logado', FUsers.UserLogged);
end;

procedure TTestUserManagement.TheLoggedUserLoginIsReadableBack;
var
  uid:Integer;
begin
  //sem OnGetUserLogin ligado, o valor guardado pelo Login e o que responde
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  AssertEquals('login guardado', 'fabio', FUsers.CurrentUserLogin);
end;

procedure TTestUserManagement.LoginWithTheWrongPasswordKeepsNobodyIn;
var
  uid:Integer;
begin
  uid:=-1;
  AssertFalse('login recusado', FUsers.Login('fabio', 'chute', uid));
  AssertFalse('ninguem entrou', FUsers.UserLogged);
  AssertEquals('nenhum login guardado', '', FUsers.CurrentUserLogin);
  AssertEquals('identificador continua invalido', -1, FUsers.UID);
end;

procedure TTestUserManagement.LoginCarriesTheUserIdBack;
var
  uid:Integer;
begin
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  AssertEquals('o identificador veio pelo parametro', 7, uid);
  AssertEquals('e ficou guardado', 7, FUsers.UID);
end;

procedure TTestUserManagement.TheThreeLoginEventsArePublished;
begin
  //os tres avisos de entrada e saida andam juntos; publicado e' o que deixa o
  //evento aparecer no object inspector e ser gravado no .lfm
  AssertTrue('SuccessfulLogin', GetPropInfo(FUsers, 'SuccessfulLogin')<>nil);
  AssertTrue('FailureLogin',    GetPropInfo(FUsers, 'FailureLogin')<>nil);
  AssertTrue('UserChanged',     GetPropInfo(FUsers, 'UserChanged')<>nil);
end;

procedure TTestUserManagement.ASuccessfulLoginNotifiesAndReportsTheChange;
var
  uid:Integer;
begin
  FUsers.UserChanged:=@NoteUserChanged;
  uid:=-1;

  FUsers.Login('fabio', 'segredo', uid);

  AssertEquals('avisou o sucesso', 1, FSuccessCount);
  AssertEquals('nao avisou falha', 0, FFailureCount);
  AssertEquals('avisou a troca de usuario', 1, FUserChangedCount);
end;

procedure TTestUserManagement.AFailedLoginDoesNotLetAnybodyIn;
var
  uid:Integer;
begin
  FUsers.UserChanged:=@NoteUserChanged;
  uid:=-1;

  FUsers.Login('fabio', 'chute', uid);

  AssertEquals('nao avisou sucesso', 0, FSuccessCount);
  AssertEquals('nao houve troca de usuario', 0, FUserChangedCount);
end;

procedure TTestUserManagement.AFailedLoginCallsTheFailureHook;
var
  uid:Integer;
begin
  //e onde um programa conta tentativa e bloqueia conta
  uid:=-1;
  FUsers.Login('fabio', 'chute', uid);

  AssertEquals('avisou a falha', 1, FFailureCount);
end;

procedure TTestUserManagement.LogoutClearsTheUser;
var
  uid:Integer;
begin
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  FUsers.Logout;

  AssertFalse('ninguem logado', FUsers.UserLogged);
  AssertEquals('login limpo', '', FUsers.CurrentUserLogin);
  AssertEquals('identificador limpo', -1, FUsers.UID);
end;

procedure TTestUserManagement.LogoutWithNobodyLoggedInNotifiesNothing;
begin
  //sair sem ter entrado nao e' uma troca de usuario
  FUsers.UserChanged:=@NoteUserChanged;

  FUsers.Logout;

  AssertEquals('nenhum aviso de troca', 0, FUserChangedCount);
end;

procedure TTestUserManagement.LogoutCallsTheEvent;
var
  uid:Integer;
begin
  FUsers.OnLogout:=@CountLogout;
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  FUsers.Logout;

  AssertEquals('o evento de saida foi chamado', 1, FLogoutCount);
end;

procedure TTestUserManagement.TheUserNameComesFromTheEventOnlyWhileLoggedIn;
var
  uid:Integer;
begin
  FUsers.OnGetUserName:=@GiveTheUserName;

  AssertEquals('sem ninguem logado, nome vazio', '', FUsers.CurrentUserName);

  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);
  AssertEquals('logado, o nome vem do evento', 'Fabio Luis Girardi',
               FUsers.CurrentUserName);

  FUsers.Logout;
  AssertEquals('depois de sair, nome vazio de novo', '', FUsers.CurrentUserName);
end;

procedure TTestUserManagement.ARegisteredCodeIsFound;
begin
  FUsers.RegisterSecurityCode('abrir_valvula');

  AssertTrue('o codigo existe', FUsers.SecurityCodeExists('abrir_valvula'));
end;

procedure TTestUserManagement.RegisteringTheSameCodeTwiceDoesNotDuplicate;
var
  codigos:TStringList;
begin
  FUsers.RegisterSecurityCode('abrir_valvula');
  FUsers.RegisterSecurityCode('abrir_valvula');

  codigos:=FUsers.GetRegisteredAccessCodes;
  try
    AssertEquals('um codigo so', 1, codigos.Count);
  finally
    codigos.Free;
  end;
end;

procedure TTestUserManagement.AnUnregisteredCodeIsNotFound;
begin
  FUsers.RegisterSecurityCode('abrir_valvula');
  FUsers.UnregisterSecurityCode('abrir_valvula');

  AssertFalse('o codigo saiu', FUsers.SecurityCodeExists('abrir_valvula'));
end;

procedure TTestUserManagement.UnregisteringACodeThatIsNotThereDoesNothing;
begin
  FUsers.RegisterSecurityCode('abrir_valvula');

  FUsers.UnregisterSecurityCode('nunca_registrado');

  AssertTrue('o que estava continua', FUsers.SecurityCodeExists('abrir_valvula'));
end;

procedure TTestUserManagement.TheListOfCodesBelongsToTheCaller;
var
  codigos:TStringList;
begin
  //GetRegisteredAccessCodes devolve uma copia nova; mexer nela nao pode
  //mexer na lista de dentro, e quem pediu e' que tem que liberar
  FUsers.RegisterSecurityCode('abrir_valvula');

  codigos:=FUsers.GetRegisteredAccessCodes;
  try
    codigos.Clear;
  finally
    codigos.Free;
  end;

  AssertTrue('a lista de dentro ficou intacta',
             FUsers.SecurityCodeExists('abrir_valvula'));
end;

procedure TTestUserManagement.AnEmptyCodeIsAlwaysAllowed;
begin
  //controle sem codigo de seguranca nao e' protegido por ninguem
  AssertTrue('codigo vazio', GetControlSecurityManager.CanAccess(''));
  AssertTrue('so espacos',   GetControlSecurityManager.CanAccess('   '));
end;

procedure TTestUserManagement.WithNobodyLoggedInNoCodeIsAllowed;
begin
  FUsers.OnCanAccess:=@AnswerCanAccess;
  FGrantedCode:='abrir_valvula';

  //o evento so e' consultado com usuario logado
  AssertFalse('sem usuario, sem permissao',
              GetControlSecurityManager.CanAccess('abrir_valvula'));
end;

procedure TTestUserManagement.TheEventDecidesForTheLoggedUser;
var
  uid:Integer;
begin
  FUsers.OnCanAccess:=@AnswerCanAccess;
  FGrantedCode:='abrir_valvula';
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  AssertTrue('o codigo liberado passa',
             GetControlSecurityManager.CanAccess('abrir_valvula'));
  AssertFalse('o outro nao',
              GetControlSecurityManager.CanAccess('parar_motor'));
end;

procedure TTestUserManagement.PermissionByUserIdAsksTheOtherEvent;
begin
  //a sobrecarga com identificador autoriza em nome de um usuario que nao esta
  //logado. Ela e' protegida: em producao quem a chama e' o proprio
  //CheckIfUserIsAllowed, depois de pedir usuario e senha na tela de permissao
  //especial. Nenhum programa a chama direto
  FUsers.OnUIDCanAccess:=@AnswerUIDCanAccess;
  FGrantedCode:='abrir_valvula';

  AssertTrue('o usuario 7 pode',  FUsers.CanAccessAs('abrir_valvula', 7));
  AssertFalse('o usuario 8 nao',  FUsers.CanAccessAs('abrir_valvula', 8));
end;

procedure TTestUserManagement.PermissionByUserIdIsNotAskedForANegativeId;
begin
  //identificador negativo e' "nenhum usuario"; o evento nem e' consultado e
  //so o codigo vazio passa
  FUsers.OnUIDCanAccess:=@AnswerUIDCanAccess;
  FGrantedCode:='abrir_valvula';

  AssertFalse('sem usuario, sem permissao', FUsers.CanAccessAs('abrir_valvula', -1));
  AssertTrue('codigo vazio ainda passa',    FUsers.CanAccessAs('', -1));
end;

procedure TTestUserManagement.TheManagerIsASingleInstance;
begin
  AssertSame('sempre o mesmo gerenciador',
             GetControlSecurityManager, GetControlSecurityManager);
end;

procedure TTestUserManagement.AnEmptyCodeGoesThroughTheManagerWithoutAsking;
begin
  FUsers.OnCanAccess:=@AnswerCanAccess;
  FGrantedCode:='abrir_valvula';

  //o gerenciador nem chega a perguntar quando nao ha codigo
  AssertTrue('codigo vazio', GetControlSecurityManager.CanAccess(''));
end;

procedure TTestUserManagement.TheManagerDelegatesThePermissionToTheUserManagement;
var
  uid:Integer;
begin
  FUsers.OnCanAccess:=@AnswerCanAccess;
  FGrantedCode:='abrir_valvula';
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  AssertTrue('liberado',  GetControlSecurityManager.CanAccess('abrir_valvula'));
  AssertFalse('negado',   GetControlSecurityManager.CanAccess('parar_motor'));
end;

procedure TTestUserManagement.TryAccessRaisesWhenThePermissionIsDenied;
begin
  try
    GetControlSecurityManager.TryAccess('parar_motor');
    Fail('acesso negado tem que levantar excecao');
  except
    on e:Exception do
      AssertEquals('motivo', SAccessDenied, e.Message);
  end;
end;

procedure TTestUserManagement.TryAccessIsSilentWhenThePermissionIsGranted;
var
  uid:Integer;
begin
  FUsers.OnCanAccess:=@AnswerCanAccess;
  FGrantedCode:='abrir_valvula';
  uid:=-1;
  FUsers.Login('fabio', 'segredo', uid);

  GetControlSecurityManager.TryAccess('abrir_valvula');
end;

procedure TTestUserManagement.WithNoUserManagementTheManagerAllowsEverything;
begin
  //sem gerenciador de usuarios nao ha' controle de acesso nenhum, e todo
  //controle fica habilitado
  FreeAndNil(FUsers);

  AssertTrue('tudo liberado', GetControlSecurityManager.CanAccess('parar_motor'));
  GetControlSecurityManager.TryAccess('parar_motor');
end;

initialization
  RegisterTest(TTestUserManagement);

end.
