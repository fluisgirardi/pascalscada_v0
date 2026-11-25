{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Actions da biblioteca de receitas: os comandos do THMIRecipeButton
  como actions seguras, para menus, barras de ferramentas e botoes comuns.)

  Uma action por comando - nova receita, apagar receita, salvar a biblioteca,
  enviar ao CLP e receber do CLP - todas descendentes de
  TPascalSCADASecureAction: com SecurityCode preenchido, so' o usuario com
  essa permissao executa, e a action fica desabilitada para os outros (veja
  DisableIfNotAuthorized). O que o comando faz na tabela vem do
  TRecipeCommandRunner (HMIRecipeCommands), o mesmo do botao.

  Um OnExecute escrito pelo usuario substitui o comando, como nas actions
  padrao do LCL; a verificacao de acesso continua valendo.

  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(Recipe library actions: the commands of THMIRecipeButton as secure
  actions, for menus, toolbars and plain buttons.)

  One action per command - add recipe, delete recipe, save the library,
  download to the PLC and upload from the PLC - all of them descendants of
  TPascalSCADASecureAction: with SecurityCode filled in, only the user with
  that permission runs it, and the action is disabled for the others (see
  DisableIfNotAuthorized). What the command does on the table comes from
  TRecipeCommandRunner (HMIRecipeCommands), the same one as the button.

  An OnExecute written by the user replaces the command, as on the LCL
  standard actions; the access check still applies.

  @author(PascalSCADA)
}
{$ENDIF}
unit HMIRecipeActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, ControlSecurityManager, HMIRecipe,
  HMIRecipeCommands;

type
  {$IFDEF PORTUGUES}
  {:
    Base das actions de receita: a tabela ligada, os eventos e a execucao do
    comando de RecipeCommand. Nao e' registrada; use uma das descendentes.
  }
  {$ELSE}
  {:
    Base of the recipe actions: the linked table, the events and running the
    command of RecipeCommand. It is not registered; use one of the
    descendants.
  }
  {$ENDIF}
  TPascalSCADACustomRecipeAction = class(TPascalSCADASecureAction)
  private
    FRunner: TRecipeCommandRunner;
    function  GetRecipeGrid: THMIRecipeGrid;
    procedure SetRecipeGrid(const v: THMIRecipeGrid);
    function  GetRecipeName: string;
    procedure SetRecipeName(const v: string);
    function  GetAutoSave: Boolean;
    procedure SetAutoSave(const v: Boolean);
    function  GetLastError: string;
    function  GetOnError: TRecipeCommandErrorEvent;
    procedure SetOnError(const v: TRecipeCommandErrorEvent);
    function  GetOnAfterCommand: TNotifyEvent;
    procedure SetOnAfterCommand(const v: TNotifyEvent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    //: Nome fixo da receita (publicado so' nas actions de adicionar e apagar).
    //: Fixed recipe name (published only on the add and delete actions).
    property RecipeName: string read GetRecipeName write SetRecipeName;
    //: Grava a biblioteca depois do comando (publicado onde o comando muda a
    //: biblioteca). / Writes the library after the command (published where
    //: the command changes the library).
    property AutoSave: Boolean read GetAutoSave write SetAutoSave default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: O comando que a action executa.
    {$ELSE}
    //: The command the action runs.
    {$ENDIF}
    class function RecipeCommand: TRecipeCommand; virtual; abstract;

    {$IFDEF PORTUGUES}
    //: Verifica o acesso (TPascalSCADASecureAction) e executa o comando na
    //: tabela. Nao depende de formulario ativo: a tabela e' o alvo.
    {$ELSE}
    //: Checks the access (TPascalSCADASecureAction) and runs the command on
    //: the table. It does not depend on an active form: the table is the
    //: target.
    {$ENDIF}
    function Execute: Boolean; override;

    //: Quem executa os comandos / What runs the commands
    property Runner: TRecipeCommandRunner read FRunner;
    //: ultima mensagem de erro / last error message
    property LastError: string read GetLastError;
  published
    {$IFDEF PORTUGUES}
    //: Tabela de receitas que a action comanda.
    {$ELSE}
    //: Recipe table this action commands.
    {$ENDIF}
    property RecipeGrid: THMIRecipeGrid read GetRecipeGrid write SetRecipeGrid;
    property OnError: TRecipeCommandErrorEvent read GetOnError write SetOnError;
    {$IFDEF PORTUGUES}
    //: Chamado depois que o comando terminou, com ou sem erro da tabela.
    {$ELSE}
    //: Called after the command finished, with or without a table error.
    {$ENDIF}
    property OnAfterCommand: TNotifyEvent read GetOnAfterCommand write SetOnAfterCommand;
  end;

  TPascalSCADACustomRecipeActionClass = class of TPascalSCADACustomRecipeAction;

  {$IFDEF PORTUGUES}
  //: Cria uma receita: com RecipeName vazio a tabela escolhe o proximo nome
  //: livre.
  {$ELSE}
  //: Creates a recipe: with RecipeName empty the table picks the next free
  //: name.
  {$ENDIF}
  TPascalSCADAAddRecipeAction = class(TPascalSCADACustomRecipeAction)
  public
    class function RecipeCommand: TRecipeCommand; override;
  published
    property RecipeName;
    property AutoSave;
  end;

  {$IFDEF PORTUGUES}
  //: Apaga a receita de RecipeName ou, vazio, a selecionada na tabela.
  {$ELSE}
  //: Deletes the recipe named in RecipeName or, empty, the one selected on
  //: the table.
  {$ENDIF}
  TPascalSCADADeleteRecipeAction = class(TPascalSCADACustomRecipeAction)
  public
    class function RecipeCommand: TRecipeCommand; override;
  published
    property RecipeName;
    property AutoSave;
  end;

  //: Grava a biblioteca no arquivo / Writes the library to the file
  TPascalSCADASaveRecipeLibraryAction = class(TPascalSCADACustomRecipeAction)
  public
    class function RecipeCommand: TRecipeCommand; override;
  end;

  //: Escreve a receita selecionada nos tags / Writes the selected recipe into
  //: the tags
  TPascalSCADADownloadRecipeAction = class(TPascalSCADACustomRecipeAction)
  public
    class function RecipeCommand: TRecipeCommand; override;
  end;

  //: Le os tags para a receita selecionada / Reads the tags into the selected
  //: recipe
  TPascalSCADAUploadRecipeAction = class(TPascalSCADACustomRecipeAction)
  public
    class function RecipeCommand: TRecipeCommand; override;
  published
    property AutoSave;
  end;

{$IFDEF PORTUGUES}
//: A classe de action de cada comando.
{$ELSE}
//: The action class of each command.
{$ENDIF}
function RecipeActionClass(aCommand: TRecipeCommand): TPascalSCADACustomRecipeActionClass;

implementation

function RecipeActionClass(aCommand: TRecipeCommand): TPascalSCADACustomRecipeActionClass;
begin
  case aCommand of
    rcAddRecipe:    Result := TPascalSCADAAddRecipeAction;
    rcDeleteRecipe: Result := TPascalSCADADeleteRecipeAction;
    rcSaveLibrary:  Result := TPascalSCADASaveRecipeLibraryAction;
    rcDownload:     Result := TPascalSCADADownloadRecipeAction;
    rcUpload:       Result := TPascalSCADAUploadRecipeAction;
  else
    Result := nil;
  end;
end;

{ TPascalSCADACustomRecipeAction }

constructor TPascalSCADACustomRecipeAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunner := TRecipeCommandRunner.Create(Self);
  //o alvo e' a tabela, nao um controle do formulario: sem isto o LCL
  //desabilita a action quando nenhum formulario a trata no Update.
  //the target is the table, not a control of the form: without this the LCL
  //disables the action when no form handles it on Update.
  DisableIfNoHandler := False;
  Caption := RecipeCommandCaption(RecipeCommand);
end;

destructor TPascalSCADACustomRecipeAction.Destroy;
begin
  FreeAndNil(FRunner);
  inherited Destroy;
end;

procedure TPascalSCADACustomRecipeAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FRunner <> nil) then
    FRunner.GridRemoved(AComponent);
end;

function TPascalSCADACustomRecipeAction.Execute: Boolean;
begin
  Result := False;
  if csDesigning in ComponentState then exit;

  //acesso negado (a mensagem e' do TPascalSCADASecureAction) ou um OnExecute
  //do usuario, que substitui o comando: o caminho normal da action.
  //access denied (the message is TPascalSCADASecureAction's) or a user
  //OnExecute, which replaces the command: the normal path of the action.
  if Assigned(OnExecute) or (not GetControlSecurityManager.CanAccess(FSecurityCode)) then
    exit(inherited Execute);

  if Assigned(ActionList) and (ActionList.State <> asNormal) then exit;
  Update;
  //FEnabled e FAccessAllowed: o Enabled publicado e a permissao do usuario.
  //FEnabled and FAccessAllowed: the published Enabled and the user's
  //permission.
  if not (FEnabled and FAccessAllowed) then exit;

  Result := FRunner.Execute(RecipeCommand);
end;

function TPascalSCADACustomRecipeAction.GetRecipeGrid: THMIRecipeGrid;
begin
  Result := FRunner.Grid;
end;

procedure TPascalSCADACustomRecipeAction.SetRecipeGrid(const v: THMIRecipeGrid);
begin
  FRunner.Grid := v;
end;

function TPascalSCADACustomRecipeAction.GetRecipeName: string;
begin
  Result := FRunner.RecipeName;
end;

procedure TPascalSCADACustomRecipeAction.SetRecipeName(const v: string);
begin
  FRunner.RecipeName := v;
end;

function TPascalSCADACustomRecipeAction.GetAutoSave: Boolean;
begin
  Result := FRunner.AutoSave;
end;

procedure TPascalSCADACustomRecipeAction.SetAutoSave(const v: Boolean);
begin
  FRunner.AutoSave := v;
end;

function TPascalSCADACustomRecipeAction.GetLastError: string;
begin
  Result := FRunner.LastError;
end;

function TPascalSCADACustomRecipeAction.GetOnError: TRecipeCommandErrorEvent;
begin
  Result := FRunner.OnError;
end;

procedure TPascalSCADACustomRecipeAction.SetOnError(const v: TRecipeCommandErrorEvent);
begin
  FRunner.OnError := v;
end;

function TPascalSCADACustomRecipeAction.GetOnAfterCommand: TNotifyEvent;
begin
  Result := FRunner.OnAfterCommand;
end;

procedure TPascalSCADACustomRecipeAction.SetOnAfterCommand(const v: TNotifyEvent);
begin
  FRunner.OnAfterCommand := v;
end;

{ the commands }

class function TPascalSCADAAddRecipeAction.RecipeCommand: TRecipeCommand;
begin
  Result := rcAddRecipe;
end;

class function TPascalSCADADeleteRecipeAction.RecipeCommand: TRecipeCommand;
begin
  Result := rcDeleteRecipe;
end;

class function TPascalSCADASaveRecipeLibraryAction.RecipeCommand: TRecipeCommand;
begin
  Result := rcSaveLibrary;
end;

class function TPascalSCADADownloadRecipeAction.RecipeCommand: TRecipeCommand;
begin
  Result := rcDownload;
end;

class function TPascalSCADAUploadRecipeAction.RecipeCommand: TRecipeCommand;
begin
  Result := rcUpload;
end;

end.
