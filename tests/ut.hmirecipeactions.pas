{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes das actions de receita (HMIRecipeActions) e do que elas
  dividem com o THMIRecipeButton.)

  Cada action e' executada de verdade e o efeito e' conferido na tabela, no
  arquivo e nos tags; depois, o que elas ganham por serem
  TPascalSCADASecureAction (o codigo de seguranca desabilita a action e o
  botao ligado nela) e o que acontece quando o botao tem Action.
}
{$ELSE}
{:
  @abstract(Tests of the recipe actions (HMIRecipeActions) and of what they
  share with THMIRecipeButton.)

  Every action is run for real and the effect is checked on the table, on the
  file and on the tags; then, what they get for being TPascalSCADASecureAction
  (the security code disables the action and the button linked to it) and
  what happens when the button has an Action.
}
{$ENDIF}
unit ut.hmirecipeactions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Controls, fpcunit, testregistry,
  HMIRecipe, HMIRecipeCommands, HMIRecipeActions, HMIRecipeButton,
  ControlSecurityManager, CustomizedUserManagement, ut.hmirecipebutton,
  testsupport.faketag;

type
  { TTestHMIRecipeActions }

  TTestHMIRecipeActions = class(TTestCase)
  private
    FGrid: TRecipeGridEdit;
    FTemp: TFakeNumber;
    FPressure: TFakeNumber;
    FLibrary: string;
    FErrors: Integer;
    FCommands: Integer;
    FUserHandler: Integer;
    FLastError: string;
    procedure GridError(Sender: TObject; const aMessage: string);
    procedure ActionError(Sender: TObject; const aMessage: string);
    procedure AfterCommand(Sender: TObject);
    procedure UserExecute(Sender: TObject);
    //: cria a action ligada na tabela e nos eventos do teste
    //: creates the action linked to the table and the test events
    function NewAction(aCommand: TRecipeCommand): TPascalSCADACustomRecipeAction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EachCommandHasItsAction;
    procedure TheCaptionComesFromTheCommand;
    procedure TheAddActionCreatesTheRecipe;
    procedure TheDeleteActionRemovesTheSelectedRecipe;
    procedure TheDeleteActionRemovesByName;
    procedure TheSaveActionWritesTheLibrary;
    procedure TheDownloadActionWritesTheTags;
    procedure TheUploadActionReadsTheTagsAndSaves;
    procedure AnActionWithoutATableReportsAnError;
    procedure TheTableGoingAwayUnlinksTheAction;
    procedure ADisabledActionDoesNothing;
    procedure AUserOnExecuteReplacesTheCommand;
    procedure WithoutPermissionTheActionAndItsButtonAreDisabled;
    procedure WithPermissionTheActionRuns;
    procedure AButtonWithAnActionRunsTheActionNotItsCommand;
  end;

implementation

type
  { TUserManagementForTest }

  TUserManagementForTest = class(TCustomizedUserManagement)
  private
    FGranted: UTF8String;
  protected
    function CanAccess(sc: UTF8String): Boolean; override;
  public
    property Granted: UTF8String read FGranted write FGranted;
  end;

function TUserManagementForTest.CanAccess(sc: UTF8String): Boolean;
begin
  Result := (Trim(sc) = '') or (sc = FGranted);
end;

{ TTestHMIRecipeActions }

procedure TTestHMIRecipeActions.GridError(Sender: TObject; const aMessage: string);
begin
  Inc(FErrors);
  FLastError := aMessage;
end;

procedure TTestHMIRecipeActions.ActionError(Sender: TObject; const aMessage: string);
begin
  Inc(FErrors);
  FLastError := aMessage;
end;

procedure TTestHMIRecipeActions.AfterCommand(Sender: TObject);
begin
  Inc(FCommands);
end;

procedure TTestHMIRecipeActions.UserExecute(Sender: TObject);
begin
  Inc(FUserHandler);
end;

function TTestHMIRecipeActions.NewAction(aCommand: TRecipeCommand): TPascalSCADACustomRecipeAction;
begin
  Result := RecipeActionClass(aCommand).Create(nil);
  Result.RecipeGrid := FGrid;
  Result.OnError := @ActionError;
  Result.OnAfterCommand := @AfterCommand;
end;

procedure TTestHMIRecipeActions.SetUp;
var
  f: TRecipeField;
begin
  FLibrary := IncludeTrailingPathDelimiter(GetTempDir) +
              'pascalscada_recipeact_' + IntToStr(GetTickCount64) + '.ini';
  if FileExists(FLibrary) then DeleteFile(FLibrary);
  FErrors := 0;
  FCommands := 0;
  FUserHandler := 0;
  FLastError := '';

  FGrid := TRecipeGridEdit.Create(nil);
  FGrid.AutoLoad := False;
  FGrid.RecipeFile := FLibrary;
  FGrid.OnError := @GridError;
  FGrid.ShowRowNumbers := False;

  f := FGrid.Fields.Add;
  f.Name := 'Temperature';
  f.Decimals := 1;
  f.CheckRange := False;
  f := FGrid.Fields.Add;
  f.Name := 'Pressure';
  f.Decimals := 2;

  FTemp := TFakeNumber.Create(nil);
  FPressure := TFakeNumber.Create(nil);
  FGrid.Fields[0].PLCTag := FTemp;
  FGrid.Fields[1].PLCTag := FPressure;
  FGrid.RefreshCells;
end;

procedure TTestHMIRecipeActions.TearDown;
begin
  FreeAndNil(FGrid);
  FreeAndNil(FTemp);
  FreeAndNil(FPressure);
  if FileExists(FLibrary) then DeleteFile(FLibrary);
end;

procedure TTestHMIRecipeActions.EachCommandHasItsAction;
var
  c: TRecipeCommand;
  a: TPascalSCADACustomRecipeAction;
begin
  for c := Low(TRecipeCommand) to High(TRecipeCommand) do begin
    a := NewAction(c);
    try
      AssertTrue('a action e'' segura', a is TPascalSCADASecureAction);
      AssertTrue('a classe executa o proprio comando', a.RecipeCommand = c);
    finally
      a.Free;
    end;
  end;
end;

procedure TTestHMIRecipeActions.TheCaptionComesFromTheCommand;
var
  c: TRecipeCommand;
  a: TPascalSCADACustomRecipeAction;
begin
  for c := Low(TRecipeCommand) to High(TRecipeCommand) do begin
    a := NewAction(c);
    try
      AssertEquals('titulo do comando', RecipeCommandCaption(c), a.Caption);
    finally
      a.Free;
    end;
  end;
  AssertEquals('o mesmo titulo do botao', 'Download', RecipeCommandCaption(rcDownload));
end;

procedure TTestHMIRecipeActions.TheAddActionCreatesTheRecipe;
var
  a: TPascalSCADAAddRecipeAction;
begin
  a := TPascalSCADAAddRecipeAction(NewAction(rcAddRecipe));
  try
    a.RecipeName := 'B';
    AssertTrue('executou', a.Execute);
    AssertEquals('uma receita', 1, FGrid.RecipeCount);
    AssertEquals('chama B', 'B', FGrid.RecipeName(0));
    AssertTrue('AutoSave gravou', FileExists(FLibrary));
    AssertEquals('o evento disparou uma vez', 1, FCommands);
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheDeleteActionRemovesTheSelectedRecipe;
var
  a: TPascalSCADACustomRecipeAction;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  FGrid.Row := 2;
  a := NewAction(rcDeleteRecipe);
  try
    a.Execute;
    AssertEquals('sobrou uma', 1, FGrid.RecipeCount);
    AssertEquals('a apagada foi a selecionada', 'A', FGrid.RecipeName(0));
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheDeleteActionRemovesByName;
var
  a: TPascalSCADADeleteRecipeAction;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  FGrid.Row := 2;
  a := TPascalSCADADeleteRecipeAction(NewAction(rcDeleteRecipe));
  try
    a.RecipeName := 'A';
    a.Execute;
    AssertEquals('sobrou uma', 1, FGrid.RecipeCount);
    AssertEquals('a apagada foi a de nome', 'B', FGrid.RecipeName(0));
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheSaveActionWritesTheLibrary;
var
  a: TPascalSCADACustomRecipeAction;
begin
  FGrid.AddRecipe('A');
  a := NewAction(rcSaveLibrary);
  try
    a.Execute;
    AssertTrue('gravou o arquivo', FileExists(FLibrary));
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheDownloadActionWritesTheTags;
var
  a: TPascalSCADACustomRecipeAction;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(2, 1, '3.25');
  FGrid.Row := 1;
  a := NewAction(rcDownload);
  try
    a.Execute;
    AssertEquals('escreveu a temperatura', 185.5, FTemp.Value, 0.0001);
    AssertEquals('escreveu a pressao',       3.25, FPressure.Value, 0.0001);
    AssertEquals('nenhum erro', 0, FErrors);
    AssertFalse('enviar nao grava a biblioteca', FileExists(FLibrary));
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheUploadActionReadsTheTagsAndSaves;
var
  a: TPascalSCADACustomRecipeAction;
begin
  FGrid.AddRecipe('A');
  FGrid.Row := 1;
  FTemp.ChegouDoCLP(77.5);
  FPressure.ChegouDoCLP(2.25);
  a := NewAction(rcUpload);
  try
    a.Execute;
    AssertEquals('trouxe a temperatura', 77.5, FGrid.ValueOf(0, 0), 0.0001);
    AssertEquals('trouxe a pressao',      2.25, FGrid.ValueOf(0, 1), 0.0001);
    AssertTrue('AutoSave gravou', FileExists(FLibrary));
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.AnActionWithoutATableReportsAnError;
var
  a: TPascalSCADACustomRecipeAction;
begin
  a := NewAction(rcSaveLibrary);
  try
    a.RecipeGrid := nil;
    AssertFalse('nao executou', a.Execute);
    AssertEquals('avisou o erro', 1, FErrors);
    AssertTrue('a mensagem fala da tabela', Pos('table', FLastError) > 0);
    AssertEquals('LastError guarda a mensagem', FLastError, a.LastError);
    AssertEquals('sem comando, sem evento', 0, FCommands);
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.TheTableGoingAwayUnlinksTheAction;
var
  a: TPascalSCADACustomRecipeAction;
begin
  a := NewAction(rcSaveLibrary);
  try
    FreeAndNil(FGrid);
    AssertNull('a action esqueceu a tabela', a.RecipeGrid);
    a.Execute;
    AssertEquals('e avisa em vez de usar memoria liberada', 1, FErrors);
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.ADisabledActionDoesNothing;
var
  a: TPascalSCADACustomRecipeAction;
begin
  a := NewAction(rcAddRecipe);
  try
    a.Enabled := False;
    AssertFalse('nao executou', a.Execute);
    AssertEquals('nenhuma receita', 0, FGrid.RecipeCount);
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.AUserOnExecuteReplacesTheCommand;
var
  a: TPascalSCADACustomRecipeAction;
begin
  a := NewAction(rcAddRecipe);
  try
    a.OnExecute := @UserExecute;
    AssertTrue('executou', a.Execute);
    AssertEquals('o tratador do usuario rodou', 1, FUserHandler);
    AssertEquals('o comando nao', 0, FGrid.RecipeCount);
  finally
    a.Free;
  end;
end;

procedure TTestHMIRecipeActions.WithoutPermissionTheActionAndItsButtonAreDisabled;
var
  users: TUserManagementForTest;
  a: TPascalSCADACustomRecipeAction;
  b: THMIRecipeButton;
begin
  users := TUserManagementForTest.Create(nil);
  a := nil;
  b := nil;
  try
    users.Granted := 'ver_producao';
    a := NewAction(rcDownload);
    a.SecurityCode := 'enviar_receita';
    b := THMIRecipeButton.Create(nil);
    b.Action := a;
    AssertFalse('a action ficou desabilitada', TCustomAction(a).Enabled);
    AssertTrue('o Enabled publicado e'' o do programa', a.Enabled);
    AssertFalse('o botao ligado tambem', TControl(b).Enabled);
  finally
    b.Free;
    a.Free;
    users.Free;
  end;
end;

procedure TTestHMIRecipeActions.WithPermissionTheActionRuns;
var
  users: TUserManagementForTest;
  a: TPascalSCADACustomRecipeAction;
begin
  users := TUserManagementForTest.Create(nil);
  a := nil;
  try
    users.Granted := 'nova_receita';
    a := NewAction(rcAddRecipe);
    a.SecurityCode := 'nova_receita';
    AssertTrue('habilitada', TCustomAction(a).Enabled);
    AssertTrue('executou', a.Execute);
    AssertEquals('criou a receita', 1, FGrid.RecipeCount);
  finally
    a.Free;
    users.Free;
  end;
end;

procedure TTestHMIRecipeActions.AButtonWithAnActionRunsTheActionNotItsCommand;
var
  a: TPascalSCADAAddRecipeAction;
  b: THMIRecipeButton;
begin
  a := TPascalSCADAAddRecipeAction(NewAction(rcAddRecipe));
  b := THMIRecipeButton.Create(nil);
  try
    a.AutoSave := False;
    a.RecipeName := 'Da action';
    b.RecipeGrid := FGrid;
    b.Command := rcSaveLibrary;
    b.Action := a;
    AssertEquals('o titulo vem da action', 'Add recipe', b.Caption);
    b.Click;
    AssertEquals('a action criou a receita', 1, FGrid.RecipeCount);
    AssertEquals('com o nome dela', 'Da action', FGrid.RecipeName(0));
    AssertFalse('o Command do botao (salvar) nao rodou', FileExists(FLibrary));
    AssertEquals('um clique, um comando', 1, FCommands);
  finally
    b.Free;
    a.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIRecipeActions);
end.
