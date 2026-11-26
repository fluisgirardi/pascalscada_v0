unit umain;

{
  TpSCADALogger + TpSCADALogMemo demonstration.

  Everything is set on the form (see the logger's Rules in the Object
  Inspector):
    Rules[0]  every category, Info and up        -> screen (LogMemo)
    Rules[1]  every category, Trace and up        -> file logs/<category>-<date>.log
              (next to the executable)
    Rules[2]  every category, Error and up        -> OnLog event (counts the errors)

  "Post" sends one message with the chosen level and category. "Burst" starts
  four threads that post 1000 messages each (comm/Trace with a payload, which
  only goes to the files, and one Info line every 100) - the screen gets them in
  batches and the form keeps responding.

  The drivers, ports and tags of PascalSCADA post to the same hub, so a rule
  with SourceMask/ClassMask picks their messages the same way.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  pscadalog, hmilogmemo;

type

  { TBurstThread }

  TBurstThread = class(TThread)
  private
    FNumber:Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ANumber:Integer);
  end;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonBurst: TButton;
    ButtonClear: TButton;
    ButtonPost: TButton;
    CheckActive: TCheckBox;
    ComboCategory: TComboBox;
    ComboLevel: TComboBox;
    EditMessage: TEdit;
    LabelCategory: TLabel;
    LabelErrors: TLabel;
    LabelFiles: TLabel;
    LabelLevel: TLabel;
    LabelMessage: TLabel;
    LogMemo: TpSCADALogMemo;
    Logger: TpSCADALogger;
    PanelTop: TPanel;
    procedure ButtonBurstClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
    procedure CheckActiveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoggerLog(Sender: TObject; const Entry: TpSCADALogEntry;
      const Line: String);
  private
    FErrors:Integer;
    FBurst:array of TBurstThread;
    procedure StopBurst;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TBurstThread }

constructor TBurstThread.Create(ANumber:Integer);
begin
  FNumber := ANumber;
  FreeOnTerminate := false;
  inherited Create(false);
end;

procedure TBurstThread.Execute;
var
  i:Integer;
  frame:TBytes;
begin
  SetLength(frame, 12);
  for i:=1 to 1000 do begin
    if Terminated then exit;
    frame[0] := Hi(i);
    frame[1] := Lo(i);
    //the payload is built only when some rule wants comm/Trace
    if PSLogWants(lcComm, llTrace) then
      PSLog(Self, lcComm, llTrace, Format('burst %d: frame %d', [FNumber, i]), frame);
    if (i mod 100)=0 then
      PSLogFmt(Self, lcUser, llInfo, 'burst %d: %d messages', [FNumber, i]);
  end;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  c:TpSCADALogCategory;
  l:TpSCADALogLevel;
begin
  for l:=Low(l) to High(l) do
    ComboLevel.Items.Add(Trim(PSLogLevelName(l)));
  ComboLevel.ItemIndex := Ord(llInfo);
  for c:=Low(c) to High(c) do
    ComboCategory.Items.Add(PSLogCategoryName(c));
  ComboCategory.ItemIndex := Ord(lcUser);
  //a monospaced font keeps the columns of the log aligned
  {$IFDEF WINDOWS}
  LogMemo.Font.Name := 'Consolas';
  {$ELSE}
  LogMemo.Font.Name := 'Monospace';
  {$ENDIF}
  CheckActive.Checked := Logger.Active;
  LabelFiles.Caption := 'Files: '+ExtractFilePath(ParamStr(0))+'logs'+PathDelim;
  PSLog(Self, lcUser, llInfo, 'application started');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  StopBurst;
end;

procedure TFormMain.ButtonPostClick(Sender: TObject);
begin
  PSLog(Self, TpSCADALogCategory(ComboCategory.ItemIndex),
        TpSCADALogLevel(ComboLevel.ItemIndex), EditMessage.Text);
end;

procedure TFormMain.ButtonBurstClick(Sender: TObject);
var
  i:Integer;
begin
  StopBurst;
  SetLength(FBurst, 4);
  for i:=0 to High(FBurst) do
    FBurst[i] := TBurstThread.Create(i+1);
end;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  LogMemo.Clear;
  FErrors := 0;
  LabelErrors.Caption := 'Errors: 0';
end;

procedure TFormMain.CheckActiveChange(Sender: TObject);
begin
  Logger.Active := CheckActive.Checked;
end;

procedure TFormMain.LoggerLog(Sender: TObject; const Entry: TpSCADALogEntry;
  const Line: String);
begin
  //Rules[2] sends only the errors here, in the main thread
  Inc(FErrors);
  LabelErrors.Caption := Format('Errors: %d (last: %s)', [FErrors, Entry.Msg]);
end;

procedure TFormMain.StopBurst;
var
  i:Integer;
begin
  for i:=0 to High(FBurst) do begin
    FBurst[i].Terminate;
    FBurst[i].WaitFor;
    FBurst[i].Free;
  end;
  FBurst := nil;
end;

end.
