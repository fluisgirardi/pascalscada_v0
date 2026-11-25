{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Comandos da biblioteca de receitas, compartilhados pelo
  THMIRecipeButton e pelas actions de receita.)

  Nova receita, apagar receita, salvar a biblioteca, enviar ao CLP e receber
  do CLP: o que cada comando faz numa THMIRecipeGrid fica aqui, num so'
  lugar, junto com a tabela ligada, o nome fixo da receita, o AutoSave e o
  aviso de erro. O botao e as actions so' guardam um TRecipeCommandRunner e
  repassam as propriedades para ele.

  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(Commands of the recipe library, shared by THMIRecipeButton and by
  the recipe actions.)

  Add recipe, delete recipe, save the library, download to the PLC and upload
  from the PLC: what each command does on a THMIRecipeGrid lives here, in one
  place, together with the linked table, the fixed recipe name, AutoSave and
  the error report. The button and the actions only keep a
  TRecipeCommandRunner and pass their properties on to it.

  @author(PascalSCADA)
}
{$ENDIF}
unit HMIRecipeCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HMIRecipe;

type
  {$IFDEF PORTUGUES}
  {:
    O que o comando faz na biblioteca de receitas.
  }
  {$ELSE}
  {:
    What the command does on the recipe library.
  }
  {$ENDIF}
  TRecipeCommand = (
    //: cria uma receita nova / creates a new recipe
    rcAddRecipe,
    //: apaga a receita selecionada ou a de RecipeName / deletes the selected
    //: recipe or the one named in RecipeName
    rcDeleteRecipe,
    //: grava a biblioteca no arquivo / writes the library to the file
    rcSaveLibrary,
    //: escreve a receita selecionada nos tags / writes the selected recipe
    //: into the tags
    rcDownload,
    //: le os tags para a receita selecionada / reads the tags into the
    //: selected recipe
    rcUpload
  );

  {$IFDEF PORTUGUES}
  //: Chamado quando o comando nao pode ser executado (nenhuma tabela
  //: ligada...). A tabela tambem avisa os erros dela, no OnError dela.
  {$ELSE}
  //: Called when the command cannot run (no table linked...). The table
  //: reports its own errors too, on its own OnError.
  {$ENDIF}
  TRecipeCommandErrorEvent = procedure(Sender: TObject; const aMessage: string) of object;

  {$IFDEF PORTUGUES}
  {:
    Executa os comandos de receita numa THMIRecipeGrid em nome de um
    componente (o botao ou a action), que e' o Sender dos eventos e quem
    recebe o aviso de que a tabela foi destruida: o Notification dele chama
    GridRemoved.
  }
  {$ELSE}
  {:
    Runs the recipe commands on a THMIRecipeGrid on behalf of a component
    (the button or the action), which is the Sender of the events and the one
    told that the table was destroyed: its Notification calls GridRemoved.
  }
  {$ENDIF}
  TRecipeCommandRunner = class
  private
    FOwner: TComponent;
    FGrid: THMIRecipeGrid;
    FRecipeName: string;
    FAutoSave: Boolean;
    FLastError: string;
    FOnError: TRecipeCommandErrorEvent;
    FOnAfterCommand: TNotifyEvent;
    procedure SetGrid(const v: THMIRecipeGrid);
    procedure DoError(const aMessage: string);
  public
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: Executa o comando na tabela ligada. @false quando nao ha tabela (o
    //: erro vai para OnError) ou quando o dono esta no designer.
    {$ELSE}
    //: Runs the command on the linked table. @false when there is no table
    //: (the error goes to OnError) or when the owner is in the designer.
    {$ENDIF}
    function Execute(aCommand: TRecipeCommand): Boolean;

    {$IFDEF PORTUGUES}
    //: Chamado pelo Notification do dono: esquece a tabela que esta indo
    //: embora.
    {$ELSE}
    //: Called by the owner's Notification: forgets the table that is going
    //: away.
    {$ENDIF}
    procedure GridRemoved(AComponent: TComponent);

    property Grid: THMIRecipeGrid read FGrid write SetGrid;
    property RecipeName: string read FRecipeName write FRecipeName;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property LastError: string read FLastError;
    property OnError: TRecipeCommandErrorEvent read FOnError write FOnError;
    property OnAfterCommand: TNotifyEvent read FOnAfterCommand write FOnAfterCommand;
  end;

{$IFDEF PORTUGUES}
//: O titulo padrao do comando, para o botao e para a action.
{$ELSE}
//: The default caption of the command, for the button and for the action.
{$ENDIF}
function RecipeCommandCaption(aCommand: TRecipeCommand): string;

{$IFDEF PORTUGUES}
//: O comando muda a biblioteca? (adicionar, apagar e receber do CLP: os que o
//: AutoSave grava depois)
{$ELSE}
//: Does the command change the library? (add, delete and upload: the ones
//: AutoSave writes afterwards)
{$ENDIF}
function RecipeCommandChangesLibrary(aCommand: TRecipeCommand): Boolean;

implementation

function RecipeCommandCaption(aCommand: TRecipeCommand): string;
begin
  {$IFDEF PORTUGUES}
  case aCommand of
    rcAddRecipe:    Result := 'Nova receita';
    rcDeleteRecipe: Result := 'Apagar receita';
    rcSaveLibrary:  Result := 'Salvar biblioteca';
    rcDownload:     Result := 'Enviar ao CLP';
    rcUpload:       Result := 'Receber do CLP';
  else
    Result := '';
  end;
  {$ELSE}
  case aCommand of
    rcAddRecipe:    Result := 'Add recipe';
    rcDeleteRecipe: Result := 'Delete recipe';
    rcSaveLibrary:  Result := 'Save library';
    rcDownload:     Result := 'Download';
    rcUpload:       Result := 'Upload';
  else
    Result := '';
  end;
  {$ENDIF}
end;

function RecipeCommandChangesLibrary(aCommand: TRecipeCommand): Boolean;
begin
  Result := aCommand in [rcAddRecipe, rcDeleteRecipe, rcUpload];
end;

{ TRecipeCommandRunner }

constructor TRecipeCommandRunner.Create(aOwner: TComponent);
begin
  inherited Create;
  FOwner := aOwner;
  FAutoSave := True;
end;

destructor TRecipeCommandRunner.Destroy;
begin
  Grid := nil;
  inherited Destroy;
end;

procedure TRecipeCommandRunner.SetGrid(const v: THMIRecipeGrid);
begin
  if FGrid = v then exit;
  if (FGrid <> nil) and (FOwner <> nil) then
    FGrid.RemoveFreeNotification(FOwner);
  FGrid := v;
  if (FGrid <> nil) and (FOwner <> nil) then
    FGrid.FreeNotification(FOwner);
end;

procedure TRecipeCommandRunner.GridRemoved(AComponent: TComponent);
begin
  if (AComponent <> nil) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TRecipeCommandRunner.DoError(const aMessage: string);
begin
  FLastError := aMessage;
  if Assigned(FOnError) then
    FOnError(FOwner, aMessage);
end;

function TRecipeCommandRunner.Execute(aCommand: TRecipeCommand): Boolean;
begin
  Result := False;
  //no designer o clique e' do IDE, nao do operador.
  //in the designer the click belongs to the IDE, not to the operator.
  if (FOwner <> nil) and (csDesigning in FOwner.ComponentState) then exit;

  if FGrid = nil then begin
    DoError('No recipe table linked to this command.');
    exit;
  end;

  case aCommand of
    rcAddRecipe:
      FGrid.AddRecipe(FRecipeName);
    rcDeleteRecipe:
      if Trim(FRecipeName) <> '' then
        FGrid.DeleteRecipe(FRecipeName)
      else
        FGrid.DeleteRecipe(FGrid.Row - 1);
    rcSaveLibrary:
      FGrid.SaveLibrary;
    rcDownload:
      FGrid.Download;
    rcUpload:
      FGrid.Upload;
  end;

  //adicionar, apagar e receber do CLP mudam a biblioteca; salvar na hora
  //evita perder receitas quando a tela fecha.
  //adding, deleting and uploading change the library; saving right away keeps
  //recipes from being lost when the screen closes.
  if FAutoSave and RecipeCommandChangesLibrary(aCommand) then
    FGrid.SaveLibrary;

  Result := True;
  if Assigned(FOnAfterCommand) then
    FOnAfterCommand(FOwner);
end;

end.
