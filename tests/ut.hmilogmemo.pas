{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TpSCADALogMemo: o destino de tela do TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O memo recebe as linhas em lotes e guarda no maximo MaxLines. Confere-se o
  memo sozinho (AddLogLines) e ligado a um logger de verdade.
}
{$ELSE}
{:
  @abstract(TpSCADALogMemo tests: the screen target of TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The memo gets the lines in batches and keeps at most MaxLines. It is checked
  alone (AddLogLines) and linked to a real logger.
}
{$ENDIF}
unit ut.hmilogmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, LazUTF8, fpcunit, testregistry,
  pscadalog, hmilogmemo;

type

  { TTestHMILogMemo }

  TTestHMILogMemo = class(TTestCase)
  private
    FForm:TForm;
    FMemo:TpSCADALogMemo;
    procedure Add(const Prefix:String; First, Last:Integer);
    procedure CheckLines(const Prefix:String; First, Last:Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheDefaultsSuitALog;
    procedure ItIsALogDisplay;
    procedure LinesAreAppended;
    procedure TheOldestLinesGoOverMaxLines;
    procedure ABatchBiggerThanMaxLinesKeepsItsLastLines;
    procedure ZeroMaxLinesKeepsEverything;
    procedure LoweringMaxLinesTrimsTheMemo;
    procedure AutoScrollMovesTheCaretToTheEnd;
    procedure WithoutAutoScrollTheCaretStays;
    procedure TheLoggerFillsTheMemo;
    procedure FreeingTheMemoClearsTheLoggerDisplay;
    procedure AComponentThatIsNotADisplayIsRefused;
  end;

implementation

procedure TTestHMILogMemo.SetUp;
begin
  FForm := TForm.CreateNew(nil);
  FMemo := TpSCADALogMemo.Create(FForm);
  FMemo.Parent := FForm;
end;

procedure TTestHMILogMemo.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TTestHMILogMemo.Add(const Prefix:String; First, Last:Integer);
var
  sl:TStringList;
  i:Integer;
begin
  sl := TStringList.Create;
  try
    for i:=First to Last do sl.Add(Prefix+IntToStr(i));
    FMemo.AddLogLines(sl);
  finally
    sl.Free;
  end;
end;

procedure TTestHMILogMemo.CheckLines(const Prefix:String; First, Last:Integer);
var
  i:Integer;
begin
  AssertEquals('line count', Last-First+1, FMemo.Lines.Count);
  for i:=First to Last do
    AssertEquals('line '+IntToStr(i), Prefix+IntToStr(i), FMemo.Lines[i-First]);
end;

procedure TTestHMILogMemo.TheDefaultsSuitALog;
begin
  AssertTrue('read only', FMemo.ReadOnly);
  AssertFalse('no word wrap', FMemo.WordWrap);
  AssertTrue('scroll bars', FMemo.ScrollBars=ssAutoBoth);
  AssertEquals('max lines', 1000, FMemo.MaxLines);
  AssertTrue('auto scroll', FMemo.AutoScroll);
  AssertEquals('starts empty', 0, FMemo.Lines.Count);
end;

procedure TTestHMILogMemo.ItIsALogDisplay;
begin
  AssertTrue('implements IpSCADALogDisplay', Supports(FMemo, IpSCADALogDisplay));
end;

procedure TTestHMILogMemo.LinesAreAppended;
begin
  Add('a', 1, 3);
  Add('a', 4, 6);
  CheckLines('a', 1, 6);
end;

procedure TTestHMILogMemo.TheOldestLinesGoOverMaxLines;
begin
  FMemo.MaxLines := 5;
  Add('m', 1, 3);
  Add('m', 4, 7);
  CheckLines('m', 3, 7);
end;

procedure TTestHMILogMemo.ABatchBiggerThanMaxLinesKeepsItsLastLines;
begin
  FMemo.MaxLines := 4;
  Add('old', 1, 2);
  Add('b', 1, 10);
  CheckLines('b', 7, 10);
end;

procedure TTestHMILogMemo.ZeroMaxLinesKeepsEverything;
begin
  FMemo.MaxLines := 0;
  Add('z', 1, 1500);
  AssertEquals('all of them', 1500, FMemo.Lines.Count);
end;

procedure TTestHMILogMemo.LoweringMaxLinesTrimsTheMemo;
begin
  Add('t', 1, 20);
  FMemo.MaxLines := 8;
  CheckLines('t', 13, 20);
  FMemo.MaxLines := -3;
  AssertEquals('negative means no limit', 0, FMemo.MaxLines);
end;

procedure TTestHMILogMemo.AutoScrollMovesTheCaretToTheEnd;
begin
  FForm.HandleNeeded;
  FMemo.HandleNeeded;
  Add('linha çã ', 1, 30);
  Application.ProcessMessages;
  AssertEquals('caret at the end', UTF8Length(FMemo.Text), FMemo.SelStart);
end;

procedure TTestHMILogMemo.WithoutAutoScrollTheCaretStays;
begin
  FForm.HandleNeeded;
  FMemo.HandleNeeded;
  FMemo.AutoScroll := false;
  FMemo.SelStart := 0;
  Add('x', 1, 30);
  Application.ProcessMessages;
  AssertEquals('caret where it was', 0, FMemo.SelStart);
end;

procedure TTestHMILogMemo.TheLoggerFillsTheMemo;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  i:Integer;
begin
  L := TpSCADALogger.Create(FForm);
  L.FlushIntervalMs := 20;
  L.LineFormat := '%level% %msg%';
  L.Display := FMemo;
  R := L.Rules.Add;
  R.MinLevel := llDebug;
  R.Targets := [ltScreen];
  for i:=1 to 25 do
    PSLog(nil, lcUser, llDebug, 'msg'+IntToStr(i));
  AssertTrue('flush', L.Flush);
  AssertEquals('lines', 25, FMemo.Lines.Count);
  AssertEquals('first', 'DEBUG msg1', FMemo.Lines[0]);
  AssertEquals('last', 'DEBUG msg25', FMemo.Lines[24]);
end;

procedure TTestHMILogMemo.FreeingTheMemoClearsTheLoggerDisplay;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := TpSCADALogger.Create(nil);
  try
    L.FlushIntervalMs := 20;
    L.Display := FMemo;
    R := L.Rules.Add;
    R.Targets := [ltScreen];
    FreeAndNil(FMemo);
    AssertNull('display cleared', L.Display);
    //nothing to show the line on, and nothing breaks
    PSLog(nil, lcUser, llInfo, 'nowhere');
    AssertTrue('flush', L.Flush);
  finally
    L.Free;
  end;
end;

procedure TTestHMILogMemo.AComponentThatIsNotADisplayIsRefused;
var
  L:TpSCADALogger;
  M:TMemo;
  raised:Boolean;
begin
  L := TpSCADALogger.Create(FForm);
  M := TMemo.Create(FForm);
  raised := false;
  try
    L.Display := M;
  except
    on E:Exception do raised := true;
  end;
  AssertTrue('a plain TMemo is refused', raised);
  AssertNull('display stays empty', L.Display);
end;

initialization
  RegisterTest(TTestHMILogMemo);

end.
