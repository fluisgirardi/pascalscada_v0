{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Apoio aos testes: um TpSCADALogger que guarda tudo o que recebe.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma regra so', para o evento, com as categorias e o nivel pedidos. Flush
  espera o logger entregar o que ja foi publicado.
}
{$ELSE}
{:
  @abstract(Test support: a TpSCADALogger that keeps everything it gets.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A single rule, to the event, with the categories and level asked for. Flush
  waits for the logger to deliver what was already posted.
}
{$ENDIF}
unit testsupport.logcollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pscadalog;

type

  { TLogCollector }

  TLogCollector = class
  private
    FLogger:TpSCADALogger;
    FEntries:array of TpSCADALogEntry;
    FCount:Integer;
    procedure LoggerLog(Sender:TObject; const Entry:TpSCADALogEntry; const Line:String);
    function  GetEntry(Index:Integer):TpSCADALogEntry;
  public
    constructor Create(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
    destructor Destroy; override;
    function  Flush:Boolean;
    procedure Clear;
    //: Index of the first entry (from From on) of the category whose message starts with Prefix; -1 if none.
    function  IndexOf(Category:TpSCADALogCategory; const Prefix:String; From:Integer = 0):Integer;
    //: Every message, one per line, for the assertion messages.
    function  Dump:String;
    property  Count:Integer read FCount;
    property  Entries[Index:Integer]:TpSCADALogEntry read GetEntry; default;
    property  Logger:TpSCADALogger read FLogger;
  end;

implementation

constructor TLogCollector.Create(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
var
  R:TpSCADALogRule;
begin
  inherited Create;
  FLogger := TpSCADALogger.Create(nil);
  FLogger.FlushIntervalMs := 20;
  FLogger.OnLog := @LoggerLog;
  R := FLogger.Rules.Add;
  R.Categories := Categories;
  R.MinLevel := MinLevel;
  R.Targets := [ltEvent];
end;

destructor TLogCollector.Destroy;
begin
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TLogCollector.LoggerLog(Sender:TObject; const Entry:TpSCADALogEntry;
  const Line:String);
begin
  if FCount>=Length(FEntries) then
    SetLength(FEntries, 2*FCount+16);
  FEntries[FCount] := Entry;
  Inc(FCount);
end;

function TLogCollector.GetEntry(Index:Integer):TpSCADALogEntry;
begin
  Result := FEntries[Index];
end;

function TLogCollector.Flush:Boolean;
begin
  Result := FLogger.Flush;
end;

procedure TLogCollector.Clear;
begin
  FCount := 0;
end;

function TLogCollector.IndexOf(Category:TpSCADALogCategory; const Prefix:String;
  From:Integer):Integer;
var
  i:Integer;
begin
  for i:=From to FCount-1 do
    if (FEntries[i].Category=Category) and
       (Copy(FEntries[i].Msg, 1, Length(Prefix))=Prefix) then
      exit(i);
  Result := -1;
end;

function TLogCollector.Dump:String;
var
  i:Integer;
begin
  Result := '';
  for i:=0 to FCount-1 do
    Result := Result+LineEnding+'  ['+PSLogCategoryName(FEntries[i].Category)+'] '+
              FEntries[i].Source+': '+FEntries[i].Msg;
end;

end.
