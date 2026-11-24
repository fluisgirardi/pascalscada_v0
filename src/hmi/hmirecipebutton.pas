{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Botao de receita: um botao ligado a uma THMIRecipeGrid que executa
  um comando da biblioteca de receitas.)

  Cada instancia faz um papel: nova receita, apagar receita, salvar a
  biblioteca, enviar a receita selecionada ao CLP ou trazer os valores do CLP
  para a receita. A tabela e o comando ficam nas propriedades RecipeGrid e
  Command, e o botao aceita imagens - uma para solto e outra para apertado,
  PNG com transparencia inclusive - por arquivo ou por bitmap.

  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(Recipe button: a button linked to a THMIRecipeGrid that runs one
  command of the recipe library.)

  Each instance plays one role: add recipe, delete recipe, save the library,
  download the selected recipe to the PLC or upload the PLC values into the
  recipe. The table and the command live in the RecipeGrid and Command
  properties, and the button takes images - one for the released state and one
  for the pressed one, PNG with transparency included - either from a file or
  from a bitmap.

  @author(PascalSCADA)
}
{$ENDIF}
unit HMIRecipeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Buttons, Dialogs, HMIRecipe;

type
  {$IFDEF PORTUGUES}
  {:
    O que o botao faz quando e' clicado.
  }
  {$ELSE}
  {:
    What the button does when clicked.
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

  THMIRecipeButton = class;

  {$IFDEF PORTUGUES}
  //: Chamado quando o botao nao consegue executar o comando (nenhuma tabela
  //: ligada, receita repetida...). A tabela tambem avisa os erros dela.
  {$ELSE}
  //: Called when the button cannot run the command (no table linked, a
  //: repeated recipe...). The table reports its own errors too.
  {$ENDIF}
  TRecipeButtonErrorEvent = procedure(Sender: TObject; const aMessage: string) of object;

  {$IFDEF PORTUGUES}
  {:
    Botao de comando da biblioteca de receitas.
  }
  {$ELSE}
  {:
    Command button of the recipe library.
  }
  {$ENDIF}
  THMIRecipeButton = class(TSpeedButton)
  private
    FRecipeGrid: THMIRecipeGrid;
    FCommand: TRecipeCommand;
    FRecipeName: string;
    FAutoSave: Boolean;
    FGlyphUp: TBitmap;
    FGlyphDown: TBitmap;
    FGlyphUpFile: string;
    FGlyphDownFile: string;
    FLastError: string;
    //o titulo padrao do comando corrente; muda de comando so' enquanto o
    //usuario nao escrever um titulo proprio.
    //the default caption of the current command; switching commands only
    //replaces the caption while the user has not typed one of his own.
    FLastAutoCaption: TCaption;
    FOnError: TRecipeButtonErrorEvent;
    FOnAfterCommand: TNotifyEvent;

    procedure SetRecipeGrid(const v: THMIRecipeGrid);
    procedure SetCommand(const v: TRecipeCommand);
    procedure SetGlyphUp(const v: TBitmap);
    procedure SetGlyphDown(const v: TBitmap);
    procedure SetGlyphUpFile(const v: string);
    procedure SetGlyphDownFile(const v: string);
    procedure DoError(const aMessage: string);
    //: carrega a imagem do arquivo para o bitmap do estado. '' limpa o bitmap.
    //: loads the image file into the state bitmap; '' clears it.
    procedure LoadGlyphFromFile(aBitmap: TBitmap; const aFileName, aPropertyName: string);
    //: o bitmap do estado tem figura de verdade? / does the state bitmap hold
    //: a real picture?
    function  HasGlyph(aBitmap: TBitmap): Boolean;
    //: coloca no Glyph a figura do estado solto ou apertado, quando houver.
    //: puts the picture of the released or pressed state into Glyph, when
    //: there is one.
    procedure ApplyGlyph(aPressed: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    //: @exclude
    procedure Click; override;

    {$IFDEF PORTUGUES}
    //: Executa o comando do botao. O Click chama este metodo; chamar direto
    //: tambem vale, inclusive nos testes.
    {$ELSE}
    //: Runs the command of the button. Click calls this method; calling it
    //: directly works too, tests included.
    {$ENDIF}
    procedure Execute;
  published
    {$IFDEF PORTUGUES}
    //: Tabela de receitas que o botao comanda.
    {$ELSE}
    //: Recipe table this button commands.
    {$ENDIF}
    property RecipeGrid: THMIRecipeGrid read FRecipeGrid write SetRecipeGrid;

    {$IFDEF PORTUGUES}
    //: O papel do botao. Trocar de comando tambem troca o titulo, enquanto o
    //: titulo ainda for o padrao.
    {$ELSE}
    //: The role of the button. Changing the command also changes the caption,
    //: while the caption is still the default one.
    {$ENDIF}
    property Command: TRecipeCommand read FCommand write SetCommand default rcAddRecipe;

    {$IFDEF PORTUGUES}
    //: Nome fixo da receita. No comando de adicionar, vazio deixa a tabela
    //: escolher o proximo nome livre; no de apagar, vazio apaga a receita
    //: selecionada na tabela.
    {$ELSE}
    //: Fixed recipe name. On the add command, empty lets the table pick the
    //: next free name; on delete, empty deletes the recipe selected on the
    //: table.
    {$ENDIF}
    property RecipeName: string read FRecipeName write FRecipeName;

    {$IFDEF PORTUGUES}
    //: Grava a biblioteca no arquivo depois de adicionar, apagar ou trazer
    //: valores do CLP.
    {$ELSE}
    //: Writes the library to the file after adding, deleting or uploading
    //: values from the PLC.
    {$ENDIF}
    property AutoSave: Boolean read FAutoSave write FAutoSave default True;

    {$IFDEF PORTUGUES}
    //: Figura do estado solto. PNG com transparencia pode entrar direto aqui.
    {$ELSE}
    //: Picture of the released state. PNG with transparency can be assigned
    //: here directly.
    {$ENDIF}
    property GlyphUp: TBitmap read FGlyphUp write SetGlyphUp;

    {$IFDEF PORTUGUES}
    //: Figura do estado apertado. Vazia: o estado apertado continua com a do
    //: estado solto.
    {$ELSE}
    //: Picture of the pressed state. Empty: the pressed state keeps the
    //: released one.
    {$ENDIF}
    property GlyphDown: TBitmap read FGlyphDown write SetGlyphDown;

    {$IFDEF PORTUGUES}
    //: Arquivo da imagem do estado solto. Aceita PNG com transparencia, BMP,
    //: XPM e JPG. Caminho relativo e' procurado na pasta do executavel.
    {$ELSE}
    //: Image file of the released state. PNG with transparency, BMP, XPM and
    //: JPG are accepted. A relative path is looked up in the executable
    //: folder.
    {$ENDIF}
    property GlyphUpFile: string read FGlyphUpFile write SetGlyphUpFile;

    {$IFDEF PORTUGUES}
    //: Arquivo da imagem do estado apertado.
    {$ELSE}
    //: Image file of the pressed state.
    {$ENDIF}
    property GlyphDownFile: string read FGlyphDownFile write SetGlyphDownFile;

    //: ultima mensagem de erro / last error message
    property LastError: string read FLastError;

    property OnError: TRecipeButtonErrorEvent read FOnError write FOnError;
    {$IFDEF PORTUGUES}
    //: Chamado depois que o comando terminou, com ou sem erro da tabela.
    {$ELSE}
    //: Called after the command finished, with or without a table error.
    {$ENDIF}
    property OnAfterCommand: TNotifyEvent read FOnAfterCommand write FOnAfterCommand;
  end;

implementation

function DefaultCaption(aCommand: TRecipeCommand): TCaption;
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

constructor THMIRecipeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSave := True;
  FCommand := rcAddRecipe;
  FGlyphUp := TBitmap.Create;
  FGlyphDown := TBitmap.Create;
  FLastAutoCaption := DefaultCaption(FCommand);
  Caption := FLastAutoCaption;
  ShowHint := True;
end;

destructor THMIRecipeButton.Destroy;
begin
  FGlyphUp.Free;
  FGlyphDown.Free;
  inherited Destroy;
end;

procedure THMIRecipeButton.SetRecipeGrid(const v: THMIRecipeGrid);
begin
  if FRecipeGrid = v then exit;
  if FRecipeGrid <> nil then
    FRecipeGrid.RemoveFreeNotification(Self);
  FRecipeGrid := v;
  if FRecipeGrid <> nil then
    FRecipeGrid.FreeNotification(Self);
end;

procedure THMIRecipeButton.SetCommand(const v: TRecipeCommand);
begin
  if FCommand = v then exit;
  FCommand := v;
  //o titulo acompanha o comando enquanto ele ainda e' o padrao de algum
  //comando; o usuario que escreveu o dele nao perde o texto.
  //the caption follows the command while it is still some command's default;
  //a user typed caption keeps its text.
  if Caption = FLastAutoCaption then begin
    Caption := DefaultCaption(v);
    FLastAutoCaption := Caption;
  end else
    FLastAutoCaption := DefaultCaption(v);
end;

procedure THMIRecipeButton.SetGlyphUp(const v: TBitmap);
begin
  FGlyphUp.Assign(v);
  ApplyGlyph(False);
end;

procedure THMIRecipeButton.SetGlyphDown(const v: TBitmap);
begin
  FGlyphDown.Assign(v);
  ApplyGlyph(False);
end;

procedure THMIRecipeButton.LoadGlyphFromFile(aBitmap: TBitmap; const aFileName,
  aPropertyName: string);
var
  aFullName: string;
  aPicture: TPicture;
begin
  if Trim(aFileName) = '' then begin
    aBitmap.SetSize(0, 0);
    exit;
  end;

  aFullName := Trim(aFileName);
  //caminho relativo: procura na pasta do executavel, como o resto do SCADA.
  //relative path: look in the executable folder, as the rest of SCADA does.
  if (not FileExists(aFullName)) and (ExtractFileDrive(aFullName) = '') then
    aFullName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Trim(aFileName);

  if not FileExists(aFullName) then
    raise Exception.CreateFmt('%s: image not found: %s',
      [aPropertyName, Trim(aFileName)]);

  aPicture := TPicture.Create;
  try
    aPicture.LoadFromFile(aFullName);
    //Assign copia o alfa, entao o PNG continua transparente no botao.
    //Assign copies the alpha, so the PNG stays transparent on the button.
    aBitmap.Assign(aPicture.Graphic);
  finally
    aPicture.Free;
  end;
end;

procedure THMIRecipeButton.SetGlyphUpFile(const v: string);
begin
  FGlyphUpFile := v;
  LoadGlyphFromFile(FGlyphUp, v, 'GlyphUpFile');
  ApplyGlyph(False);
end;

procedure THMIRecipeButton.SetGlyphDownFile(const v: string);
begin
  FGlyphDownFile := v;
  LoadGlyphFromFile(FGlyphDown, v, 'GlyphDownFile');
  ApplyGlyph(False);
end;

function THMIRecipeButton.HasGlyph(aBitmap: TBitmap): Boolean;
begin
  Result := (aBitmap <> nil) and (aBitmap.Width > 0) and (aBitmap.Height > 0);
end;

procedure THMIRecipeButton.ApplyGlyph(aPressed: Boolean);
begin
  if aPressed and HasGlyph(FGlyphDown) then
    Glyph.Assign(FGlyphDown)
  else
    if HasGlyph(FGlyphUp) then
      Glyph.Assign(FGlyphUp);
end;

procedure THMIRecipeButton.Loaded;
begin
  inherited Loaded;
  ApplyGlyph(False);
end;

procedure THMIRecipeButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FRecipeGrid) then
    FRecipeGrid := nil;
end;

procedure THMIRecipeButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and HasGlyph(FGlyphDown) then
    Glyph.Assign(FGlyphDown);
end;

procedure THMIRecipeButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if HasGlyph(FGlyphUp) then
    Glyph.Assign(FGlyphUp);
end;

procedure THMIRecipeButton.DoError(const aMessage: string);
begin
  FLastError := aMessage;
  if Assigned(FOnError) then
    FOnError(Self, aMessage);
end;

procedure THMIRecipeButton.Execute;
begin
  //no designer o clique e' do IDE, nao do operador.
  //in the designer the click belongs to the IDE, not to the operator.
  if csDesigning in ComponentState then exit;

  if FRecipeGrid = nil then begin
    DoError('No recipe table linked to this button.');
    exit;
  end;

  case FCommand of
    rcAddRecipe:
      FRecipeGrid.AddRecipe(FRecipeName);
    rcDeleteRecipe:
      if Trim(FRecipeName) <> '' then
        FRecipeGrid.DeleteRecipe(FRecipeName)
      else
        FRecipeGrid.DeleteRecipe(FRecipeGrid.Row - 1);
    rcSaveLibrary:
      FRecipeGrid.SaveLibrary;
    rcDownload:
      FRecipeGrid.Download;
    rcUpload:
      FRecipeGrid.Upload;
  end;

  //adicionar, apagar e receber do CLP mudam a biblioteca; salvar na hora
  //evita perder receitas quando a tela fecha.
  //adding, deleting and uploading change the library; saving right away keeps
  //recipes from being lost when the screen closes.
  if FAutoSave and (FCommand in [rcAddRecipe, rcDeleteRecipe, rcUpload]) then
    FRecipeGrid.SaveLibrary;

  if Assigned(FOnAfterCommand) then
    FOnAfterCommand(Self);
end;

procedure THMIRecipeButton.Click;
begin
  Execute;
  inherited Click;
end;

end.
