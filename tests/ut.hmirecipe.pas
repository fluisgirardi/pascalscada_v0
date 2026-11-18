{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIRecipeGrid: a tabela de receitas.)

  Uma receita e' um conjunto de valores que o operador escolhe de uma vez e
  manda para o CLP. O que se testa aqui e' o que pode dar errado sem ninguem
  perceber: valor que nao entra, nome repetido que apaga a receita do vizinho,
  biblioteca que volta do arquivo trocada, e o download escrevendo no tag
  errado.
}
{$ELSE}
{:
  @abstract(THMIRecipeGrid tests: the recipe table.)

  A recipe is a set of values the operator picks at once and sends to the PLC.
  What is tested here is what can go wrong unnoticed: a value that is not
  stored, a repeated name that overwrites the neighbour's recipe, a library
  that comes back from the file scrambled, and the download writing to the
  wrong tag.
}
{$ENDIF}
unit ut.hmirecipe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Grids, fpcunit, testregistry,
  LResources, HMIRecipe, PLCTag, PLCNumber, ProtocolTypes, testsupport.faketag;

type
  { TRecipeGridProbe }

  //a edicao de celula chega ao controle pelo SetEditText, que e' protegido.
  //a sonda expoe esse caminho para o teste digitar como o operador digita.
  //
  //cell editing reaches the control through SetEditText, which is protected.
  //The probe exposes that path so the test can type the way the operator does.
  TRecipeGridProbe = class(THMIRecipeGrid)
  public
    procedure TypeInto(ACol, ARow: Integer; const aText: string);
  end;

  { TTestHMIRecipe }

  TTestHMIRecipe = class(TTestCase)
  private
    FGrid: TRecipeGridProbe;
    FTemp: TFakeNumber;
    FPressure: TFakeNumber;
    FLibrary: string;
    FErrors: Integer;
    FLastError: string;
    procedure GridError(Sender: TObject; const aMessage: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheFieldsBecomeTheColumns;
    procedure ANewRecipeStartsWithTheDefaultValues;
    procedure ARepeatedNameIsRefused;
    procedure DuplicatingCopiesTheValues;
    procedure RenamingMovesTheRecipe;
    procedure ARenameToATakenNameIsRefused;
    procedure DeletingLeavesTheOthersAlone;
    procedure SavingAndLoadingKeepsNamesAndValues;
    procedure LoadingAMissingFileKeepsTheLibrary;
    procedure DownloadWritesTheSelectedRecipeToTheTags;
    procedure UploadBringsTheTagValuesIntoTheRecipe;
    procedure TypingAValueUpdatesTheRecipe;
    procedure TypingGarbageKeepsTheOldValue;
    procedure AValueOutsideTheRangeIsClamped;
    procedure TypingANewNameInTheFirstColumnRenames;
    procedure DownloadWithNoNumericTagReportsAnError;
    procedure AddingAFieldGivesEveryRecipeAColumn;
    procedure DownloadWithAnEmptyLibraryReportsAnError;
    procedure ManyFieldsCreatedAtRuntimeStayInRange;
    procedure StreamingFourFieldsFromALfmStaysInRange;
    procedure StreamingTheUserLfmVerbatim;
    procedure AnEmptyGridKeepsOneUsableParameterColumn;
    procedure StreamingAStaleSingleColumnEmptyGridHeals;
  end;

implementation

procedure TRecipeGridProbe.TypeInto(ACol, ARow: Integer; const aText: string);
begin
  SetEditText(ACol, ARow, aText);
end;

{ TTestHMIRecipe }

procedure TTestHMIRecipe.GridError(Sender: TObject; const aMessage: string);
begin
  Inc(FErrors);
  FLastError := aMessage;
end;

procedure TTestHMIRecipe.SetUp;
var
  f: TRecipeField;
begin
  FLibrary := IncludeTrailingPathDelimiter(GetTempDir) +
              'pascalscada_recipe_' + IntToStr(GetTickCount64) + '.ini';
  if FileExists(FLibrary) then DeleteFile(FLibrary);

  FErrors := 0;
  FLastError := '';

  FGrid := TRecipeGridProbe.Create(nil);
  FGrid.AutoLoad := False;          //o teste controla quando ler o arquivo
  FGrid.RecipeFile := FLibrary;
  FGrid.NameColumnTitle := 'Recipe';
  FGrid.OnError := @GridError;

  f := FGrid.Fields.Add;
  f.Name := 'Temperature';
  f.Decimals := 1;
  f.MinValue := 0;
  f.MaxValue := 200;
  f.DefaultValue := 20;

  f := FGrid.Fields.Add;
  f.Name := 'Pressure';
  f.Decimals := 2;
  f.DefaultValue := 1.5;

  FTemp := TFakeNumber.Create(nil);
  FPressure := TFakeNumber.Create(nil);
  FGrid.Fields[0].PLCTag := FTemp;
  FGrid.Fields[1].PLCTag := FPressure;

  FGrid.RefreshCells;
end;

procedure TTestHMIRecipe.TearDown;
begin
  FGrid.Free;
  FTemp.Free;
  FPressure.Free;
  if FileExists(FLibrary) then DeleteFile(FLibrary);
end;

procedure TTestHMIRecipe.TheFieldsBecomeTheColumns;
begin
  AssertEquals('uma coluna de nome + duas de parametros', 3, FGrid.ColCount);
  AssertEquals('cabecalho da primeira coluna', 'Recipe', FGrid.Cells[0, 0]);
  AssertEquals('primeira coluna de parametro', 'Temperature', FGrid.Cells[1, 0]);
  AssertEquals('segunda coluna de parametro', 'Pressure', FGrid.Cells[2, 0]);
end;

procedure TTestHMIRecipe.ANewRecipeStartsWithTheDefaultValues;
begin
  AssertEquals('indice da primeira receita', 0, FGrid.AddRecipe('A'));
  AssertEquals('uma receita', 1, FGrid.RecipeCount);
  AssertEquals('valor padrao do primeiro campo', 20,  FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('valor padrao do segundo campo',  1.5, FGrid.ValueOf(0, 1), 0.0001);
  AssertEquals('celula formatada com as casas do campo', '20.0', FGrid.Cells[1, 1]);
  AssertEquals('celula formatada com as casas do campo', '1.50', FGrid.Cells[2, 1]);
end;

procedure TTestHMIRecipe.ARepeatedNameIsRefused;
begin
  FGrid.AddRecipe('A');
  AssertEquals('recusa a segunda com o mesmo nome', -1, FGrid.AddRecipe('A'));
  AssertEquals('continua com uma receita', 1, FGrid.RecipeCount);
  AssertEquals('avisou o erro', 1, FErrors);
  AssertTrue('a mensagem fala do nome', Pos('already', FLastError) > 0);
end;

procedure TTestHMIRecipe.DuplicatingCopiesTheValues;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(2, 1, '3.25');

  AssertEquals('indice da copia', 1, FGrid.DuplicateRecipe('A', 'B'));
  AssertEquals('duas receitas', 2, FGrid.RecipeCount);
  AssertEquals('copia o primeiro valor', 185.5, FGrid.ValueOf(1, 0), 0.0001);
  AssertEquals('copia o segundo valor',   3.25, FGrid.ValueOf(1, 1), 0.0001);
  AssertEquals('nome da copia', 'B', FGrid.RecipeName(1));
end;

procedure TTestHMIRecipe.RenamingMovesTheRecipe;
begin
  FGrid.AddRecipe('A');
  AssertTrue('renomeou', FGrid.RenameRecipe('A', 'B'));
  AssertEquals('indice pelo nome novo', 0, FGrid.IndexOfRecipe('B'));
  AssertEquals('o nome antigo sumiu',   -1, FGrid.IndexOfRecipe('A'));
  AssertEquals('celula acompanhou', 'B', FGrid.Cells[0, 1]);
end;

procedure TTestHMIRecipe.ARenameToATakenNameIsRefused;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  AssertFalse('recusou', FGrid.RenameRecipe('A', 'B'));
  AssertEquals('A continua com o nome A', 'A', FGrid.RecipeName(0));
  AssertEquals('B continua com o nome B', 'B', FGrid.RecipeName(1));
  AssertEquals('avisou o erro', 1, FErrors);
end;

procedure TTestHMIRecipe.DeletingLeavesTheOthersAlone;
begin
  FGrid.AddRecipe('A');
  FGrid.AddRecipe('B');
  FGrid.AddRecipe('C');
  FGrid.DeleteRecipe('B');
  AssertEquals('sobraram duas', 2, FGrid.RecipeCount);
  AssertEquals('a primeira ficou',  'A', FGrid.RecipeName(0));
  AssertEquals('a terceira subiu', 'C', FGrid.RecipeName(1));
end;

procedure TTestHMIRecipe.SavingAndLoadingKeepsNamesAndValues;
begin
  FGrid.AddRecipe('Aluminium');
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(2, 1, '3.25');
  FGrid.AddRecipe('Steel');
  FGrid.TypeInto(1, 2, '12.75');
  FGrid.TypeInto(2, 2, '0.5');
  FGrid.SaveLibrary;

  AssertTrue('gravou o arquivo', FileExists(FLibrary));

  //reabre com outro controle, como faria a proxima vez que a tela abre.
  //reopens with another control, the way the next screen start would.
  FGrid.Free;
  FGrid := TRecipeGridProbe.Create(nil);
  FGrid.AutoLoad := False;
  FGrid.RecipeFile := FLibrary;
  FGrid.OnError := @GridError;
  FGrid.Fields.Add.Name := 'Temperature';
  FGrid.Fields[0].Decimals := 1;
  FGrid.Fields[0].PLCTag := FTemp;
  FGrid.Fields.Add.Name := 'Pressure';
  FGrid.Fields[1].Decimals := 2;
  FGrid.Fields[1].PLCTag := FPressure;
  FGrid.LoadLibrary;

  AssertEquals('duas receitas', 2, FGrid.RecipeCount);
  AssertEquals('primeiro nome',  'Aluminium', FGrid.RecipeName(0));
  AssertEquals('segundo nome',   'Steel',     FGrid.RecipeName(1));
  AssertEquals('valor 1', 185.5, FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('valor 2',   3.25, FGrid.ValueOf(0, 1), 0.0001);
  AssertEquals('valor 3',  12.75, FGrid.ValueOf(1, 0), 0.0001);
  AssertEquals('valor 4',   0.5,  FGrid.ValueOf(1, 1), 0.0001);
end;

procedure TTestHMIRecipe.LoadingAMissingFileKeepsTheLibrary;
begin
  FGrid.AddRecipe('A');
  FGrid.RecipeFile := IncludeTrailingPathDelimiter(GetTempDir) + 'nao_existe_xyz.ini';
  FGrid.LoadLibrary;
  AssertEquals('a receita em memoria ficou', 1, FGrid.RecipeCount);
  AssertEquals('avisou o erro', 1, FErrors);
end;

procedure TTestHMIRecipe.DownloadWritesTheSelectedRecipeToTheTags;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(2, 1, '3.25');
  FGrid.Row := 1;

  FGrid.Download;

  AssertEquals('escreveu a temperatura', 185.5, FTemp.Value, 0.0001);
  AssertEquals('escreveu a pressao',       3.25, FPressure.Value, 0.0001);
  AssertEquals('uma escrita por tag', 1, FTemp.Escritas);
  AssertEquals('nenhum erro', 0, FErrors);
end;

procedure TTestHMIRecipe.UploadBringsTheTagValuesIntoTheRecipe;
begin
  FGrid.AddRecipe('A');
  FGrid.Row := 1;
  FTemp.ChegouDoCLP(77.5);
  FPressure.ChegouDoCLP(2.25);

  FGrid.Upload;

  AssertEquals('trouxe a temperatura', 77.5, FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('trouxe a pressao',      2.25, FGrid.ValueOf(0, 1), 0.0001);
  AssertEquals('mostrou na celula', '77.5', FGrid.Cells[1, 1]);
  AssertEquals('nenhum erro', 0, FErrors);
end;

procedure TTestHMIRecipe.TypingAValueUpdatesTheRecipe;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(2, 1, '3,25');   //virgula tambem vale / a comma works too
  AssertEquals('guardou o valor', 3.25, FGrid.ValueOf(0, 1), 0.0001);
  AssertEquals('formatou com duas casas', '3.25', FGrid.Cells[2, 1]);
end;

procedure TTestHMIRecipe.TypingGarbageKeepsTheOldValue;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(1, 1, '185.5');
  FGrid.TypeInto(1, 1, 'abc');
  AssertEquals('o valor anterior ficou', 185.5, FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('a celula volta ao valor anterior', '185.5', FGrid.Cells[1, 1]);
  AssertEquals('avisou o erro', 1, FErrors);
end;

procedure TTestHMIRecipe.AValueOutsideTheRangeIsClamped;
begin
  FGrid.Fields[0].CheckRange := True;
  FGrid.AddRecipe('A');
  FGrid.TypeInto(1, 1, '500');
  AssertEquals('limitou no maximo', 200, FGrid.ValueOf(0, 0), 0.0001);
  AssertEquals('avisou o erro', 1, FErrors);
end;

procedure TTestHMIRecipe.TypingANewNameInTheFirstColumnRenames;
begin
  FGrid.AddRecipe('A');
  FGrid.TypeInto(0, 1, 'B');
  AssertEquals('renomeou pela celula', 'B', FGrid.RecipeName(0));

  FGrid.AddRecipe('C');
  FGrid.TypeInto(0, 2, 'B');
  AssertEquals('nao aceitou nome repetido', 'C', FGrid.RecipeName(1));
  AssertEquals('avisou o erro', 1, FErrors);
end;

procedure TTestHMIRecipe.DownloadWithNoNumericTagReportsAnError;
begin
  FGrid.Fields[0].PLCTag := nil;
  FGrid.Fields[1].PLCTag := nil;
  FGrid.AddRecipe('A');
  FGrid.Row := 1;

  FGrid.Download;

  AssertEquals('avisou que nada foi escrito', 1, FErrors);
  AssertTrue('a mensagem fala de tag', Pos('tag', FLastError) > 0);
end;

procedure TTestHMIRecipe.AddingAFieldGivesEveryRecipeAColumn;
begin
  FGrid.AddRecipe('A');
  FGrid.Fields.Add.Name := 'Speed';

  AssertEquals('uma coluna a mais', 4, FGrid.ColCount);
  AssertEquals('cabecalho novo', 'Speed', FGrid.Cells[3, 0]);
  AssertEquals('a receita ganhou a coluna', 0, FGrid.ValueOf(0, 2), 0.0001);
end;

procedure TTestHMIRecipe.DownloadWithAnEmptyLibraryReportsAnError;
begin
  //sem receita na biblioteca a grade nem tem linha para selecionar.
  //with no recipe in the library the grid has no row to select at all.
  AssertEquals('biblioteca vazia', 0, FGrid.RecipeCount);
  FGrid.Download;
  AssertEquals('avisou', 1, FErrors);
  AssertEquals('nada escrito', 0, FTemp.Escritas);
end;

procedure TTestHMIRecipe.ManyFieldsCreatedAtRuntimeStayInRange;
var
  i: Integer;
begin
  //o erro do problema real: muitas colunas e a grade pintando depois disso.
  //the real issue: many columns and the grid painting afterwards.
  for i := 1 to 6 do
    FGrid.Fields.Add.Name := 'Field' + IntToStr(i);
  FGrid.AddRecipe('A');
  FGrid.RefreshCells;

  AssertEquals('uma coluna de nome + oito de parametro (2 do SetUp + 6 novas)',
               9, FGrid.ColCount);
  AssertEquals('cabecalho da ultima coluna', 'Field6', FGrid.Cells[8, 0]);
  AssertEquals('titulo da primeira linha', 'Recipe', FGrid.Cells[0, 0]);
  AssertEquals('nome da receita', 'A', FGrid.Cells[0, 1]);
end;

procedure TTestHMIRecipe.StreamingFourFieldsFromALfmStaysInRange;
var
  lfm: TStringStream;
  bin: TMemoryStream;
  Reader: TReader;
  Grid: THMIRecipeGrid;
  Form: TForm;

  function FieldItems: string;
  begin
    Result :=
      '    item' + LineEnding +
      '      Name = '#39'F1'#39 + LineEnding +
      '    end' + LineEnding +
      '    item' + LineEnding +
      '      Name = '#39'F2'#39 + LineEnding +
      '    end' + LineEnding +
      '    item' + LineEnding +
      '      Name = '#39'F3'#39 + LineEnding +
      '    end' + LineEnding +
      '    item' + LineEnding +
      '      Name = '#39'F4'#39 + LineEnding +
      '    end>';
  end;

  function BuildLFM(const aColCount: string): string;
  begin
    //a ordem do designer: primeiro as propriedades herdadas, depois as
    //proprias do THMIRecipeGrid (Fields, NameColumnTitle...). ColCount so'
    //entra no .lfm quando difere do padrao (5).
    //
    //designer order: inherited properties first, then the THMIRecipeGrid
    //ones (Fields, NameColumnTitle...). ColCount only goes into the .lfm
    //when it differs from the default (5).
    Result :=
      'object RecipeGrid1: THMIRecipeGrid' + LineEnding +
      '  Left = 0' + LineEnding +
      '  Height = 250' + LineEnding +
      '  Top = 0' + LineEnding +
      '  Width = 500' + LineEnding;
    if aColCount <> '' then
      Result := Result + '  ColCount = ' + aColCount + LineEnding;
    Result := Result +
      '  DefaultColWidth = 100' + LineEnding +
      '  DefaultRowHeight = 22' + LineEnding +
      '  Options = [goEditing, goColSizing, goTabs]' + LineEnding +
      '  RowCount = 2' + LineEnding +
      '  TabOrder = 0' + LineEnding +
      '  Fields = <' + LineEnding +
      FieldItems + LineEnding +
      '  NameColumnTitle = '#39'Recipe'#39 + LineEnding +
      'end';
  end;

  procedure TryStream(const aLabel, aLFM: string);
  begin
    lfm := TStringStream.Create(aLFM);
    bin := TMemoryStream.Create;
    try
      lfm.Position := 0;
      LRSObjectTextToBinary(lfm, bin);
      bin.Position := 0;
      Reader := TReader.Create(bin, 4096);
      try
        Grid := THMIRecipeGrid(Reader.ReadRootComponent(nil));
        Form := TForm.CreateNew(nil);
        try
          AssertEquals(aLabel + ': quatro campos + coluna do nome', 5,
                       Grid.ColCount);
          AssertEquals(aLabel + ': cabecalho da ultima coluna', 'F4',
                       Grid.Cells[4, 0]);
          AssertEquals(aLabel + ': titulo da coluna do nome', 'Recipe',
                       Grid.Cells[0, 0]);

          //forca o caminho completo do formulario: janela real, carregada e
          //pintada - e' onde a LCL costuma esbarrar em cache de tamanho antigo.
          //forces the full form path: a real window, loaded and painted -
          //that is where the LCL usually trips over a stale size cache.
          Grid.Parent := Form;
          Form.Show;
          Application.ProcessMessages;
          AssertEquals(aLabel + ': depois de pintar, ColCount continua certo',
                       5, Grid.ColCount);
        finally
          Form.Free;
          Grid.Free;
        end;
      finally
        Reader.Free;
      end;
    finally
      lfm.Free;
      bin.Free;
    end;
  end;

begin
  //reproduz o caminho do formulario: com 4 campos o ColCount de projeto (5)
  //iguala o padrao e o designer nao grava; com 3 campos grava 4; e um projeto
  //salvo com a grade vazia grava 1 (a grade vazia de projeto fica com uma
  //coluna so').
  //
  //replays the form path: with 4 fields the design ColCount (5) equals the
  //default and the designer stores nothing; with 3 fields it stores 4; and a
  //project saved with an empty grid stores 1 (the empty design grid has a
  //single column).
  RegisterClass(THMIRecipeGrid);
  RegisterClass(TRecipeFields);
  RegisterClass(TRecipeField);

  TryStream('ColCount ausente', BuildLFM(''));
  TryStream('ColCount = 4',    BuildLFM('4'));
  TryStream('ColCount = 1',    BuildLFM('1'));
end;

type
  //espelho do formulario do usuario que reproduz o erro: o mesmo nome de
  //classe e o mesmo campo published do unit1.pas real.
  //mirror of the user's form that reproduces the error: the same class name
  //and the same published field as the real unit1.pas.
  TReplayForm1 = class(TForm)
    HMIRecipeGrid1: THMIRecipeGrid;
  end;

const
  CUserLfmReplayText =
      'object Form1: TForm1' + LineEnding +
      '  Left = 438' + LineEnding +
      '  Height = 419' + LineEnding +
      '  Top = 250' + LineEnding +
      '  Width = 582' + LineEnding +
      '  Caption = ''Form1''' + LineEnding +
      '  ClientHeight = 419' + LineEnding +
      '  ClientWidth = 582' + LineEnding +
      '  LCLVersion = ''4.8.0.0''' + LineEnding +
      '  object HMIRecipeGrid1: THMIRecipeGrid' + LineEnding +
      '    Left = 39' + LineEnding +
      '    Height = 191' + LineEnding +
      '    Top = 21' + LineEnding +
      '    Width = 475' + LineEnding +
      '    DefaultColWidth = 100' + LineEnding +
      '    DefaultRowHeight = 22' + LineEnding +
      '    FixedCols = 0' + LineEnding +
      '    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goSmoothScroll]' + LineEnding +
      '    RowCount = 2' + LineEnding +
      '    TabOrder = 0' + LineEnding +
      '    Fields = <    ' + LineEnding +
      '      item' + LineEnding +
      '        MaxValue = 100' + LineEnding +
      '      end    ' + LineEnding +
      '      item' + LineEnding +
      '        MaxValue = 100' + LineEnding +
      '      end    ' + LineEnding +
      '      item' + LineEnding +
      '        MaxValue = 100' + LineEnding +
      '      end    ' + LineEnding +
      '      item' + LineEnding +
      '        MaxValue = 100' + LineEnding +
      '      end>' + LineEnding +
      '    NameColumnTitle = ''Recipe''' + LineEnding +
      '    ColWidths = (' + LineEnding +
      '      150' + LineEnding +
      '      100' + LineEnding +
      '      100' + LineEnding +
      '      100' + LineEnding +
      '      100' + LineEnding +
      '    )' + LineEnding +
      '    Cells = (' + LineEnding +
      '      5' + LineEnding +
      '      0' + LineEnding +
      '      0' + LineEnding +
      '      ''Recipe''' + LineEnding +
      '      1' + LineEnding +
      '      0' + LineEnding +
      '      ''Field 0''' + LineEnding +
      '      2' + LineEnding +
      '      0' + LineEnding +
      '      ''Field 1''' + LineEnding +
      '      3' + LineEnding +
      '      0' + LineEnding +
      '      ''Field 2''' + LineEnding +
      '      4' + LineEnding +
      '      0' + LineEnding +
      '      ''Field 3''' + LineEnding +
      '    )' + LineEnding +
      '  end' + LineEnding +
      'end' + LineEnding;

procedure TTestHMIRecipe.StreamingTheUserLfmVerbatim;
var
  lfm: TStringStream;
  bin: TMemoryStream;
  Reader: TReader;
  Grid: THMIRecipeGrid;
  Form: TForm;
begin
  //o .lfm real de um usuario: itens de campo sem Name (o padrao nao e'
  //gravado) mas com MaxValue, mais FixedCols = 0, ColWidths e o array
  //Cells gravado pelo designer. Lido na ordem exata em que esta' no arquivo.
  //
  //a real user .lfm: field items without Name (the default is not stored)
  //but with MaxValue, plus FixedCols = 0, ColWidths and the Cells array the
  //designer stored. Read in the exact order they appear in the file.
  RegisterClass(THMIRecipeGrid);
  RegisterClass(TRecipeFields);
  RegisterClass(TRecipeField);
  RegisterClass(TReplayForm1);
  RegisterClassAlias(TReplayForm1, 'TForm1');

  lfm := TStringStream.Create(CUserLfmReplayText);
  try
    lfm.Position := 0;
    bin := TMemoryStream.Create;
    try
      LRSObjectTextToBinary(lfm, bin);
      bin.Position := 0;
      Reader := TReader.Create(bin, 4096);
      try
        Form := TForm(Reader.ReadRootComponent(nil));
        Grid := TReplayForm1(Form).HMIRecipeGrid1;
        try
          AssertEquals('quatro campos + coluna do nome', 5, Grid.ColCount);
          AssertEquals('cabecalho da ultima coluna', 'Field 3',
                       Grid.Cells[4, 0]);
          AssertEquals('titulo da coluna do nome', 'Recipe',
                       Grid.Cells[0, 0]);
          AssertEquals('quatro campos na colecao', 4, Grid.Fields.Count);
        finally
          Form.Free;
        end;
      finally
        Reader.Free;
      end;
    finally
      bin.Free;
    end;
  finally
    lfm.Free;
  end;
end;

procedure TTestHMIRecipe.AnEmptyGridKeepsOneUsableParameterColumn;
var
  Grid: THMIRecipeGrid;
begin
  //a grade vazia de projeto tinha uma unica coluna (so' o nome) e qualquer
  //leitura de Cells[1,x] levantava EGridException. Hoje ela nasce com nome
  //mais uma coluna de parametro vazia.
  //
  //the empty design grid used to have a single column (the name only) and
  //any Cells[1,x] read raised EGridException. Now it starts with the name
  //plus one empty parameter column.
  Grid := THMIRecipeGrid.Create(nil);
  try
    AssertEquals('nome + uma coluna de parametro vazia', 2, Grid.ColCount);
    AssertEquals('a coluna extra esta vazia', '', Grid.Cells[1, 0]);
    AssertEquals('a linha de receita vazia esta vazia', '', Grid.Cells[1, 1]);
  finally
    Grid.Free;
  end;
end;

procedure TTestHMIRecipe.StreamingAStaleSingleColumnEmptyGridHeals;
var
  lfm: TStringStream;
  bin: TMemoryStream;
  Reader: TReader;
  Grid: THMIRecipeGrid;
begin
  //um projeto salvo com a grade sem campos guarda ColCount = 1; depois de
  //carregar a grade tem de voltar a ter nome + uma coluna utilizavel.
  //
  //a project saved with a fieldless grid stores ColCount = 1; after loading,
  //the grid has to be back to the name + one usable column.
  RegisterClass(THMIRecipeGrid);
  RegisterClass(TRecipeFields);
  RegisterClass(TRecipeField);

  lfm := TStringStream.Create(
    'object RecipeGrid1: THMIRecipeGrid' + LineEnding +
    '  Left = 0' + LineEnding +
    '  Height = 250' + LineEnding +
    '  Top = 0' + LineEnding +
    '  Width = 500' + LineEnding +
    '  ColCount = 1' + LineEnding +
    '  RowCount = 2' + LineEnding +
    '  TabOrder = 0' + LineEnding +
    'end');
  bin := TMemoryStream.Create;
  try
    lfm.Position := 0;
    LRSObjectTextToBinary(lfm, bin);
    bin.Position := 0;
    Reader := TReader.Create(bin, 4096);
    try
      Grid := THMIRecipeGrid(Reader.ReadRootComponent(nil));
      try
        AssertEquals('nome + uma coluna de parametro vazia', 2, Grid.ColCount);
        AssertEquals('a coluna extra esta vazia', '', Grid.Cells[1, 0]);
      finally
        Grid.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    lfm.Free;
    bin.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIRecipe);
end.
