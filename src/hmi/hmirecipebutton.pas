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

  Os comandos em si ficam na unit HMIRecipeCommands (TRecipeCommandRunner),
  a mesma que as actions de receita (HMIRecipeActions) usam. Um botao com
  Action ligada executa a action, nao o Command dele.

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

  The commands themselves live in the unit HMIRecipeCommands
  (TRecipeCommandRunner), the same one the recipe actions (HMIRecipeActions)
  use. A button with an Action linked runs the action, not its own Command.

  @author(PascalSCADA)
}
{$ENDIF}
unit HMIRecipeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Buttons, HMIRecipe, HMIRecipeCommands;

type
  //os comandos vem de HMIRecipeCommands; os nomes ficam aqui tambem, para o
  //codigo que so' usa HMIRecipeButton continuar compilando.
  //the commands come from HMIRecipeCommands; the names stay here too, so code
  //that only uses HMIRecipeButton keeps compiling.
  TRecipeCommand = HMIRecipeCommands.TRecipeCommand;
  TRecipeButtonErrorEvent = HMIRecipeCommands.TRecipeCommandErrorEvent;

const
  rcAddRecipe    = HMIRecipeCommands.rcAddRecipe;
  rcDeleteRecipe = HMIRecipeCommands.rcDeleteRecipe;
  rcSaveLibrary  = HMIRecipeCommands.rcSaveLibrary;
  rcDownload     = HMIRecipeCommands.rcDownload;
  rcUpload       = HMIRecipeCommands.rcUpload;

type
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
    FRunner: TRecipeCommandRunner;
    FCommand: TRecipeCommand;
    FGlyphUp: TBitmap;
    FGlyphDown: TBitmap;
    FGlyphUpFile: string;
    FGlyphDownFile: string;
    //o titulo padrao do comando corrente; muda de comando so' enquanto o
    //usuario nao escrever um titulo proprio.
    //the default caption of the current command; switching commands only
    //replaces the caption while the user has not typed one of his own.
    FLastAutoCaption: TCaption;

    function  GetRecipeGrid: THMIRecipeGrid;
    procedure SetRecipeGrid(const v: THMIRecipeGrid);
    function  GetRecipeName: string;
    procedure SetRecipeName(const v: string);
    function  GetAutoSave: Boolean;
    procedure SetAutoSave(const v: Boolean);
    function  GetLastError: string;
    function  GetOnError: TRecipeButtonErrorEvent;
    procedure SetOnError(const v: TRecipeButtonErrorEvent);
    function  GetOnAfterCommand: TNotifyEvent;
    procedure SetOnAfterCommand(const v: TNotifyEvent);
    procedure SetCommand(const v: TRecipeCommand);
    procedure SetGlyphUp(const v: TBitmap);
    procedure SetGlyphDown(const v: TBitmap);
    procedure SetGlyphUpFile(const v: string);
    procedure SetGlyphDownFile(const v: string);
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

    {$IFDEF PORTUGUES}
    //: Quem executa os comandos (compartilhado com as actions de receita).
    {$ELSE}
    //: What runs the commands (shared with the recipe actions).
    {$ENDIF}
    property Runner: TRecipeCommandRunner read FRunner;
  published
    {$IFDEF PORTUGUES}
    //: Tabela de receitas que o botao comanda.
    {$ELSE}
    //: Recipe table this button commands.
    {$ENDIF}
    property RecipeGrid: THMIRecipeGrid read GetRecipeGrid write SetRecipeGrid;

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
    property RecipeName: string read GetRecipeName write SetRecipeName;

    {$IFDEF PORTUGUES}
    //: Grava a biblioteca no arquivo depois de adicionar, apagar ou trazer
    //: valores do CLP.
    {$ELSE}
    //: Writes the library to the file after adding, deleting or uploading
    //: values from the PLC.
    {$ENDIF}
    property AutoSave: Boolean read GetAutoSave write SetAutoSave default True;

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
    property LastError: string read GetLastError;

    property OnError: TRecipeButtonErrorEvent read GetOnError write SetOnError;
    {$IFDEF PORTUGUES}
    //: Chamado depois que o comando terminou, com ou sem erro da tabela.
    {$ELSE}
    //: Called after the command finished, with or without a table error.
    {$ENDIF}
    property OnAfterCommand: TNotifyEvent read GetOnAfterCommand write SetOnAfterCommand;
  end;

implementation

constructor THMIRecipeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunner := TRecipeCommandRunner.Create(Self);
  FCommand := rcAddRecipe;
  FGlyphUp := TBitmap.Create;
  FGlyphDown := TBitmap.Create;
  FLastAutoCaption := RecipeCommandCaption(FCommand);
  Caption := FLastAutoCaption;
  ShowHint := True;
end;

destructor THMIRecipeButton.Destroy;
begin
  FGlyphUp.Free;
  FGlyphDown.Free;
  FreeAndNil(FRunner);
  inherited Destroy;
end;

function THMIRecipeButton.GetRecipeGrid: THMIRecipeGrid;
begin
  Result := FRunner.Grid;
end;

procedure THMIRecipeButton.SetRecipeGrid(const v: THMIRecipeGrid);
begin
  FRunner.Grid := v;
end;

function THMIRecipeButton.GetRecipeName: string;
begin
  Result := FRunner.RecipeName;
end;

procedure THMIRecipeButton.SetRecipeName(const v: string);
begin
  FRunner.RecipeName := v;
end;

function THMIRecipeButton.GetAutoSave: Boolean;
begin
  Result := FRunner.AutoSave;
end;

procedure THMIRecipeButton.SetAutoSave(const v: Boolean);
begin
  FRunner.AutoSave := v;
end;

function THMIRecipeButton.GetLastError: string;
begin
  Result := FRunner.LastError;
end;

function THMIRecipeButton.GetOnError: TRecipeButtonErrorEvent;
begin
  Result := FRunner.OnError;
end;

procedure THMIRecipeButton.SetOnError(const v: TRecipeButtonErrorEvent);
begin
  FRunner.OnError := v;
end;

function THMIRecipeButton.GetOnAfterCommand: TNotifyEvent;
begin
  Result := FRunner.OnAfterCommand;
end;

procedure THMIRecipeButton.SetOnAfterCommand(const v: TNotifyEvent);
begin
  FRunner.OnAfterCommand := v;
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
    Caption := RecipeCommandCaption(v);
    FLastAutoCaption := Caption;
  end else
    FLastAutoCaption := RecipeCommandCaption(v);
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
  if (Operation = opRemove) and (FRunner <> nil) then
    FRunner.GridRemoved(AComponent);
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

procedure THMIRecipeButton.Execute;
begin
  FRunner.Execute(FCommand);
end;

procedure THMIRecipeButton.Click;
begin
  //com uma Action ligada quem executa e' ela (no inherited Click), com a
  //seguranca dela; o Command do botao fica de fora.
  //with an Action linked it is the action that runs (in inherited Click),
  //with its security; the button's Command stays out.
  if Action = nil then
    Execute;
  inherited Click;
end;

end.
