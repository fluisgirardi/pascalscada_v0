{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIBasicControl: o controle de acesso comum a todos os
            controles desenhados.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nove units da camada descendem daqui - a barra de progresso, as valvulas, o
  motor, o elevador, a polilinha, o botao transparente. Todas herdam a mesma
  maquinaria: o controle se cadastra no gerenciador de seguranca ao nascer, se
  descadastra ao morrer, e o que chega ao controle e' o E logico entre o que o
  programa pediu e o que o usuario pode.

  O cadastro e' o que faz uma troca de usuario reavaliar a tela inteira. Se um
  controle destruido continuasse na lista, a proxima reavaliacao passaria por
  um ponteiro morto - por isso o par cadastrar/descadastrar tem que fechar.
}
{$ELSE}
{:
  @abstract(THMIBasicControl tests: the access control shared by every drawn
            control.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nine units in the layer descend from here - the progress bar, the valves,
  the motor, the elevator, the polyline, the transparent button. They all
  inherit the same machinery: the control registers with the security manager
  when born, unregisters when it dies, and what reaches the control is the
  logical AND of what the program asked for and what the user may do.

  That registration is what makes a change of user re-evaluate the whole
  screen. If a destroyed control stayed on the list, the next evaluation
  would walk a dead pointer - which is why the register/unregister pair has
  to balance.
}
{$ENDIF}
unit ut.hmibasiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fpcunit, testregistry,
  hmi_draw_basiccontrol, ControlSecurityManager, CustomizedUserManagement;

type

  { TBasicControlProbe }

  //SecurityCode e MakeUnsecure sao protegidos na base; cada descendente
  //escolhe se os republica. A sonda os expoe para testar a base em si.
  //
  //SecurityCode and MakeUnsecure are protected in the base class; each
  //descendant chooses whether to republish them. The probe exposes them so
  //the base itself can be tested.
  TBasicControlProbe = class(THMIBasicControl)
  public
    procedure SetCode(const sc:UTF8String);
    function  GetCode:UTF8String;
    procedure DropTheCode;
  end;

  { TUserManagementForTest }

  TUserManagementForTest = class(TCustomizedUserManagement)
  private
    FGranted:UTF8String;
  protected
    function CanAccess(sc:UTF8String):Boolean; override;
  public
    property Granted:UTF8String read FGranted write FGranted;
  end;

  { TTestHMIBasicControl }

  TTestHMIBasicControl = class(TTestCase)
  private
    FControl:TBasicControlProbe;
    FUsers:TUserManagementForTest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o E logico / the logical AND
    procedure ANewControlIsEnabled;
    procedure EnablingARuntimeCreatedControlKeepsItEnabled;
    procedure AnEmptyCodeLeavesItEnabled;
    procedure WithoutPermissionTheControlIsDisabled;
    procedure WithPermissionTheControlStaysEnabled;
    procedure WithoutPermissionEnablingItHasNoEffect;
    procedure PermissionDoesNotOverrideTheProgramsEnabled;
    procedure DroppingTheCodeEnablesItAgain;

    //o cadastro no gerenciador / registering with the manager
    procedure TheCodeIsRegisteredWithTheManager;
    procedure ChangingWhatIsGrantedReevaluatesTheControl;
    procedure ADestroyedControlLeavesTheList;
    procedure ManyControlsComeAndGoWithoutBreakingTheList;
  end;

implementation

{ TBasicControlProbe }

procedure TBasicControlProbe.SetCode(const sc:UTF8String);
begin
  SecurityCode:=sc;
end;

function TBasicControlProbe.GetCode:UTF8String;
begin
  Result:=SecurityCode;
end;

procedure TBasicControlProbe.DropTheCode;
begin
  MakeUnsecure;
end;

{ TUserManagementForTest }

function TUserManagementForTest.CanAccess(sc:UTF8String):Boolean;
begin
  Result:=(Trim(sc)='') or (sc=FGranted);
end;

{ TTestHMIBasicControl }

procedure TTestHMIBasicControl.SetUp;
begin
  FUsers:=TUserManagementForTest.Create(nil);
  FUsers.Granted:='abrir_valvula';
  FControl:=TBasicControlProbe.Create(nil);
end;

procedure TTestHMIBasicControl.TearDown;
begin
  FreeAndNil(FControl);
  FreeAndNil(FUsers);
end;

procedure TTestHMIBasicControl.ANewControlIsEnabled;
begin
  AssertTrue('sem codigo, habilitado', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.EnablingARuntimeCreatedControlKeepsItEnabled;
begin
  //criado por codigo, sem .lfm e sem codigo de seguranca: desabilitar e
  //reabilitar tem que deixa-lo habilitado - e' o que o repasse de um clique
  //faz a cada clique fora do desenho
  //created by code, with no .lfm and no security code: disabling and
  //re-enabling has to leave it enabled - it is what forwarding a click does
  //on every click outside the drawing
  FControl.Enabled:=false;

  FControl.Enabled:=true;

  AssertTrue('habilitado', FControl.Enabled);
end;

procedure TTestHMIBasicControl.AnEmptyCodeLeavesItEnabled;
begin
  FControl.SetCode('');

  AssertTrue('codigo vazio nao protege nada', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.WithoutPermissionTheControlIsDisabled;
begin
  FControl.SetCode('parar_motor');

  AssertFalse('sem permissao, desabilitado', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.WithPermissionTheControlStaysEnabled;
begin
  FControl.SetCode('abrir_valvula');

  AssertTrue('com permissao, habilitado', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.WithoutPermissionEnablingItHasNoEffect;
begin
  //e o que faz o codigo de seguranca valer alguma coisa
  FControl.SetCode('parar_motor');

  FControl.Enabled:=true;

  AssertFalse('continua desabilitado', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.PermissionDoesNotOverrideTheProgramsEnabled;
begin
  //ter permissao nao habilita o que o programa desabilitou
  FControl.SetCode('abrir_valvula');

  FControl.Enabled:=false;

  AssertFalse('o programa desabilitou', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.DroppingTheCodeEnablesItAgain;
begin
  FControl.SetCode('parar_motor');
  AssertFalse('protegido', TControl(FControl).Enabled);

  FControl.DropTheCode;

  AssertEquals('sem codigo', '', FControl.GetCode);
  AssertTrue('e liberado',   TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.TheCodeIsRegisteredWithTheManager;
begin
  //o codigo usado por um controle passa a existir na lista do gerenciador,
  //que e de onde a tela de permissoes tira as opcoes
  FControl.SetCode('parar_motor');

  AssertTrue('o codigo foi registrado',
             GetControlSecurityManager.SecurityCodeExists('parar_motor'));
end;

procedure TTestHMIBasicControl.ChangingWhatIsGrantedReevaluatesTheControl;
begin
  //e' o cadastro do controle que faz isso funcionar: o gerenciador percorre
  //os controles cadastrados e reavalia cada um
  FControl.SetCode('parar_motor');
  AssertFalse('comeca desabilitado', TControl(FControl).Enabled);

  FUsers.Granted:='parar_motor';
  GetControlSecurityManager.UpdateControls;

  AssertTrue('a reavaliacao chegou ao controle', TControl(FControl).Enabled);
end;

procedure TTestHMIBasicControl.ADestroyedControlLeavesTheList;
var
  outro:TBasicControlProbe;
begin
  //um controle destruido que ficasse na lista faria a proxima reavaliacao
  //passar por um ponteiro morto
  outro:=TBasicControlProbe.Create(nil);
  outro.SetCode('parar_motor');
  FreeAndNil(outro);

  GetControlSecurityManager.UpdateControls;

  AssertTrue('a lista sobreviveu', true);
end;

procedure TTestHMIBasicControl.ManyControlsComeAndGoWithoutBreakingTheList;
var
  controles:array[0..19] of TBasicControlProbe;
  c:LongInt;
begin
  for c:=0 to 19 do begin
    controles[c]:=TBasicControlProbe.Create(nil);
    controles[c].SetCode('parar_motor');
  end;

  //solta os pares, depois os impares - a lista encolhe fora de ordem
  for c:=0 to 19 do
    if (c mod 2)=0 then FreeAndNil(controles[c]);
  GetControlSecurityManager.UpdateControls;

  for c:=0 to 19 do
    if (c mod 2)<>0 then FreeAndNil(controles[c]);
  GetControlSecurityManager.UpdateControls;

  AssertFalse('o controle do teste continua respondendo',
              TControl(FControl).Enabled=false);
end;

initialization
  RegisterTest(TTestHMIBasicControl);

end.
