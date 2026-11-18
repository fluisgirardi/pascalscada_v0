{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIRecipeButton: os botoes de comando da biblioteca de
  receitas.)

  O botao e' so' um gatilho, mas um gatilho que pode apertar receita errada.
  Aqui cada comando e' apertado de verdade e o que se confere e' o efeito na
  tabela, no arquivo e nos tags - e o que acontece quando ninguem ligou o
  botao em tabela nenhuma.
}
{$ELSE}
{:
  @abstract(THMIRecipeButton tests: the recipe library command buttons.)

  The button is just a trigger, but a trigger that can press the wrong recipe.
  Every command is pressed for real here and what gets checked is the effect
  on the table, on the file and on the tags - and what happens when nobody
  linked the button to a table at all.
}
{$ENDIF}
unit ut.hmirecipebutton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Buttons, fpcunit, testregistry,
  HMIRecipe, HMIRecipeButton, PLCTag, PLCNumber, ProtocolTypes,
  testsupport.faketag;

type
  { TRecipeGridEdit }

  //a edicao de celula chega ao controle pelo SetEditText, que e' protegido;
  //a sonda expoe esse caminho para o teste preencher valores como o operador.
  //
  //cell editing reaches the control through SetEditText, which is protected;
  //the probe exposes that path so the test can fill values the way the
  //operator does.
  TRecipeGridEdit = class(THMIRecipeGrid)
  public
    procedure TypeInto(ACol, ARow: Integer; const aText: string);
  end;

  { TRecipeButtonProbe }

  //MouseDown/MouseUp sao protegidos; a sonda os expoe para apertar o botao
  //como o operador aperta.
  //
  //MouseDown/MouseUp are protected; the probe exposes them so the test can
  //press the button the way the operator does.
  TRecipeButtonProbe = class(THMIRecipeButton)
  public
    procedure Press;
    procedure Release;
  end;

  { TTestHMIRecipeButton }

  TTestHMIRecipeButton = class(TTestCase)
  private
    FGrid: TRecipeGridEdit;
    FButton: TRecipeButtonProbe;
    FTemp: TFakeNumber;
    FPressure: TFakeNumber;
    FLibrary: string;
    FGlyphUpFile: string;
    FGlyphDownFile: string;
    FErrors: Integer;
    FCommands: Integer;
    FLastError: string;
    procedure GridError(Sender: TObject; const aMessage: string);
    procedure ButtonError(Sender: TObject; const aMessage: string);
    procedure AfterCommand(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheAddCommandCreatesTheRecipe;
    procedure AnUnnamedAddCommandGeneratesAFreeName;
    procedure TheDeleteCommandRemovesTheSelectedRecipe;
    procedure TheDeleteCommandRemovesByName;
    procedure TheSaveCommandWritesTheLibrary;
    procedure TheDownloadCommandWritesTheTags;
    procedure TheUploadCommandReadsTheTags;
    procedure AutoSaveWritesTheFileAfterAddingAndUploading;
    procedure AButtonWithoutATableReportsAnError;
    procedure SwitchingCommandsMovesTheDefaultCaption;
    procedure ACustomCaptionSurvivesACommandChange;
    procedure TheAfterCommandEventFiresOnce;
    procedure GlyphFilesAreLoadedAndApplied;
    procedure AMissingGlyphFileRaises;
  end;

implementation

procedure TRecipeGridEdit.TypeInto(ACol, ARow: Integer; const aText: string);
begin
  SetEditText(ACol, ARow, aText);
end;

procedure TRecipeButtonProbe.Press;
begin
  MouseDown(mbLeft, [], 0, 0);
end;

procedure TRecipeButtonProbe.Release;
begin
  MouseUp(mbLeft, [], 0, 0);
end;

procedure MakeTestPng(const aFileName: string; aSize: Integer);
var
  png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.SetSize(aSize, aSize);
    png.Canvas.Pixels[0, 0] := clRed;
    png.SaveToFile(aFileName);
  finally
    png.Free;
  end;
end;

{ TTestHMIRecipeButton }

procedure TTestHMIRecipeButton.GridError(Sender: TObject; const aMessage: string);
begin
  Inc(FErrors);
  FLastError := aMessage;
end;

procedure TTestHMIRecipeButton.ButtonError(Sender: TObject; const aMessage: string);
begin
  Inc(FErrors);
  FLastError := aMessage;
end;

procedure TTestHMIRecipeButton.AfterCommand(Sender: TObject);
begin
  Inc(FCommands);
end;

procedure TTestHMIRecipeButton.SetUp;
var
  f: TRecipeField;
begin
  FLibrary := IncludeTrailingPathDelimiter(GetTempDir) +
              'pascalscada_recipebtn_' + IntToStr(GetTickCount64) + '.ini';
  if FileExists(FLibrary) then DeleteFile(FLibrary);

  FGlyphUpFile := IncludeTrailingPathDelimiter(GetTempDir) +
                  'pascalscada_btnup_' + IntToStr(GetTickCount64) + '.png';
  FGlyphDownFile := IncludeTrailingPathDelimiter(GetTempDir) +
                    'pascalscada_btndown_' + IntToStr(GetTickCount64) + '.png';
  MakeTestPng(FGlyphUpFile, 4);
  MakeTestPng(FGlyphDownFile, 6);

  FErrors := 0;
  FLastError := '';
  FCommands := 0;

  FGrid := TRecipeGridEdit.Create(nil);
  FGrid.AutoLoad := False;
  FGrid.RecipeFile := FLibrary;
  FGrid.OnError := @GridError;

  f := FGrid.Fields.Add;
  f.Name := 'Temperature';
  f.Decimals := 1;

  f := FGrid.Fields.Add;
  f.Name := 'Pressure';
  f.Decimals := 2;

  FTemp := TFakeNumber.Create(nil);
  FPressure := TFakeNumber.Create(nil);
  FGrid.Fields[0].PLCTag := FTemp;
  FGrid.Fields[1].PLCTag := FPressure;
  FGrid.RefreshCells;

  FButton := TRecipeButtonProbe.Create(nil);
  FButton.RecipeGrid := FGrid;
  FButton.OnError := @ButtonError;
  FButton.OnAfterCommand := @AfterCommand;
end;

procedure TTestHMIRecipeButton.TearDown;
begin
  FButton.Free;
  FGrid.Free;
  FTemp.Free;
  FPressure.Free;
  if FileExists(FLibrary) then DeleteFile(FLibrary);
  if FileExists(FGlyphUpFile) then DeleteFile(FGlyphUpFile);
  if FileExists(FGlyphDownFile) then DeleteFile(FGlyphDownFile);
end;

procedure TTestHMIRecipeButton.TheAddCommandCreatesTheRecipe;
begin
  FGrid.AddRecipe('A');
  FButton.Command := rcAddRecipe;
  FButton.RecipeName := 'B';
  FButton.Execute;
  AssertEquals('duas receitas', 2, FGrid.RecipeCount);
  AssertEquals('a nova se chama B', 'B', FGrid.RecipeName(1));
  AssertEquals('o evento disparou', 1, FCommands);
end;

procedure TTestHMIRecipeButton.AnUnnamedAddCommandGeneratesAFreeName;
begin
  FButton.Command := rcAddRecipe;
  FButton.RecipeName := '';
  FButton.Execute;
  AssertEquals('uma receita', 1, FGrid.RecipeCount);
  AssertEquals('nome escolhido pela tabela', 'Recipe 1', FGrid.RecipeName(0));
end;

procedure TTestHMIRecipeButton.TheDeleteCommandRemovesTheSelectedRecipe;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  FGrid.Row := 2;
  FButton.Command := rcDeleteRecipe;
  FButton.RecipeName := '';
  FButton.Execute;
  AssertEquals('sobrou uma', 1, FGrid.RecipeCount);
  AssertEquals('a apagada foi a selecionada', 'A', FGrid.RecipeName(0));
end;

procedure TTestHMIRecipeButton.TheDeleteCommandRemovesByName;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  FButton.Command := rcDeleteRecipe;
  FButton.RecipeName := 'A';
  FButton.Execute;
  AssertEquals('sobrou uma', 1, FGrid.RecipeCount);
  AssertEquals('a apagada foi a de nome', 'B', FGrid.RecipeName(0));
end;

procedure TTestHMIRecipeButton.TheSaveCommandWritesTheLibrary;
begin
  FGrid.AddRecipe('A');
  FButton.Command := rcSaveLibrary;
  FButton.Execute;
  AssertTrue('gravou o arquivo', FileExists(FLibrary));
end;

procedure TTestHMIRecipeButton.TheDownloadCommandWritesTheTags;
begin
  FGrid.AddRecipe('A');
  FGrid.Fields[0].CheckRange := False;
  //preenche os valores pela celula, como o operador faria.
  //fills the values through the cell, the way the operator would.
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(2, 1, '3.25');
  FGrid.Row := 1;
  FButton.Command := rcDownload;
  FButton.Execute;
  AssertEquals('escreveu a temperatura', 185.5, FTemp.Value, 0.0001);
  AssertEquals('escreveu a pressao',       3.25, FPressure.Value, 0.0001);
  AssertEquals('nenhum erro', 0, FErrors);
end;

procedure TTestHMIRecipeButton.TheUploadCommandReadsTheTags;
begin
  FGrid.AddRecipe('A');
  FGrid.Row := 1;
  FTemp.ChegouDoCLP(77.5);
  FPressure.ChegouDoCLP(2.25);
  FButton.Command := rcUpload;
  FButton.Execute;
  AssertEquals('trouxe a temperatura', 77.5, FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('trouxe a pressao',      2.25, FGrid.ValueOf(0, 1), 0.0001);
  AssertEquals('nenhum erro', 0, FErrors);
end;

procedure TTestHMIRecipeButton.AutoSaveWritesTheFileAfterAddingAndUploading;
begin
  FGrid.AddRecipe('A');
  FGrid.Row := 1;
  FTemp.ChegouDoCLP(5);
  FButton.Command := rcUpload;
  FButton.Execute;
  AssertTrue('o upload gravou o arquivo', FileExists(FLibrary));

  DeleteFile(FLibrary);
  FButton.Command := rcAddRecipe;
  FButton.RecipeName := 'B';
  FButton.Execute;
  AssertTrue('o adicionar gravou o arquivo', FileExists(FLibrary));

  DeleteFile(FLibrary);
  FButton.Command := rcSaveLibrary;
  FButton.Execute;
  //salvar sempre grava, AutoSave ou nao / saving always writes, AutoSave or not
  AssertTrue('o salvar gravou o arquivo', FileExists(FLibrary));
end;

procedure TTestHMIRecipeButton.AButtonWithoutATableReportsAnError;
begin
  FButton.RecipeGrid := nil;
  FButton.Execute;
  AssertEquals('avisou o erro', 1, FErrors);
  AssertTrue('a mensagem fala da tabela', Pos('table', FLastError) > 0);
end;

procedure TTestHMIRecipeButton.SwitchingCommandsMovesTheDefaultCaption;
begin
  AssertEquals('titulo do comando inicial', 'Add recipe', FButton.Caption);
  FButton.Command := rcDownload;
  AssertEquals('titulo do comando novo', 'Download', FButton.Caption);
  FButton.Command := rcUpload;
  AssertEquals('titulo do proximo comando', 'Upload', FButton.Caption);
  FButton.Command := rcSaveLibrary;
  AssertEquals('titulo do salvar', 'Save library', FButton.Caption);
  FButton.Command := rcDeleteRecipe;
  AssertEquals('titulo do apagar', 'Delete recipe', FButton.Caption);
end;

procedure TTestHMIRecipeButton.ACustomCaptionSurvivesACommandChange;
begin
  FButton.Caption := 'Part start';
  FButton.Command := rcDownload;
  AssertEquals('o titulo escrito ficou', 'Part start', FButton.Caption);
end;

procedure TTestHMIRecipeButton.TheAfterCommandEventFiresOnce;
begin
  FGrid.AddRecipe('A');
  FButton.Command := rcDownload;
  FGrid.Row := 1;
  FButton.Execute;
  AssertEquals('um comando, um evento', 1, FCommands);
end;

procedure TTestHMIRecipeButton.GlyphFilesAreLoadedAndApplied;
begin
  FButton.GlyphUpFile := FGlyphUpFile;
  FButton.GlyphDownFile := FGlyphDownFile;

  AssertTrue('a figura do estado solto carregou',
             (FButton.GlyphUp.Width > 0) and (FButton.GlyphUp.Height > 0));
  AssertTrue('a figura do estado apertado carregou',
             (FButton.GlyphDown.Width > 0) and (FButton.GlyphDown.Height > 0));
  //a figura solta tem 4x4 e a apertada 6x6; pelo tamanho se sabe qual esta
  //sendo mostrada.
  //the released picture is 4x4 and the pressed one 6x6; the size tells which
  //one is being shown.
  AssertEquals('o botao comeca no estado solto', 4, FButton.Glyph.Width);

  //apertar troca a figura; soltar volta a do estado solto.
  //pressing swaps the picture; releasing brings the released one back.
  FButton.Press;
  AssertEquals('apertado mostra a figura de baixo', 6, FButton.Glyph.Width);
  FButton.Release;
  AssertEquals('soltou, volta a de cima', 4, FButton.Glyph.Width);
end;

procedure TTestHMIRecipeButton.AMissingGlyphFileRaises;
var
  raised: Boolean;
begin
  raised := False;
  try
    FButton.GlyphUpFile := IncludeTrailingPathDelimiter(GetTempDir) +
                           'nao_existe_' + IntToStr(GetTickCount64) + '.png';
  except
    on E: Exception do
      raised := True;
  end;
  AssertTrue('arquivo inexistente levanta erro', raised);
end;

initialization
  RegisterTest(TTestHMIRecipeButton);
end.
