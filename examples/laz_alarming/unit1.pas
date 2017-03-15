unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  CustomDrawnControls, StdCtrls, ExtCtrls, DBGrids, ZConnection, ZDataset,
  ZPgEventAlerter, ZSqlProcessor, PLCBlock, PLCBlockElement, TagBit,
  ModBusSerial, HMICheckBox, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    HMICheckBox1: THMICheckBox;
    HMICheckBox10: THMICheckBox;
    HMICheckBox11: THMICheckBox;
    HMICheckBox12: THMICheckBox;
    HMICheckBox13: THMICheckBox;
    HMICheckBox14: THMICheckBox;
    HMICheckBox15: THMICheckBox;
    HMICheckBox16: THMICheckBox;
    HMICheckBox17: THMICheckBox;
    HMICheckBox18: THMICheckBox;
    HMICheckBox19: THMICheckBox;
    HMICheckBox2: THMICheckBox;
    HMICheckBox20: THMICheckBox;
    HMICheckBox21: THMICheckBox;
    HMICheckBox22: THMICheckBox;
    HMICheckBox23: THMICheckBox;
    HMICheckBox24: THMICheckBox;
    HMICheckBox25: THMICheckBox;
    HMICheckBox26: THMICheckBox;
    HMICheckBox27: THMICheckBox;
    HMICheckBox28: THMICheckBox;
    HMICheckBox29: THMICheckBox;
    HMICheckBox3: THMICheckBox;
    HMICheckBox30: THMICheckBox;
    HMICheckBox31: THMICheckBox;
    HMICheckBox32: THMICheckBox;
    HMICheckBox4: THMICheckBox;
    HMICheckBox5: THMICheckBox;
    HMICheckBox6: THMICheckBox;
    HMICheckBox7: THMICheckBox;
    HMICheckBox8: THMICheckBox;
    HMICheckBox9: THMICheckBox;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    ModBusRTUDriver1: TModBusRTUDriver;
    UpdateAlarms: TZQuery;
    Label1: TLabel;
    PLCBlock1: TPLCBlock;
    PLCBlock1_e0: TPLCBlockElement;
    PLCBlock1_e0_bit0: TTagBit;
    PLCBlock1_e0_bit1: TTagBit;
    PLCBlock1_e0_bit10: TTagBit;
    PLCBlock1_e0_bit11: TTagBit;
    PLCBlock1_e0_bit12: TTagBit;
    PLCBlock1_e0_bit13: TTagBit;
    PLCBlock1_e0_bit14: TTagBit;
    PLCBlock1_e0_bit15: TTagBit;
    PLCBlock1_e0_bit2: TTagBit;
    PLCBlock1_e0_bit3: TTagBit;
    PLCBlock1_e0_bit4: TTagBit;
    PLCBlock1_e0_bit5: TTagBit;
    PLCBlock1_e0_bit6: TTagBit;
    PLCBlock1_e0_bit7: TTagBit;
    PLCBlock1_e0_bit8: TTagBit;
    PLCBlock1_e0_bit9: TTagBit;
    PLCBlock1_e1: TPLCBlockElement;
    PLCBlock1_e1_bit0: TTagBit;
    PLCBlock1_e1_bit1: TTagBit;
    PLCBlock1_e1_bit10: TTagBit;
    PLCBlock1_e1_bit11: TTagBit;
    PLCBlock1_e1_bit12: TTagBit;
    PLCBlock1_e1_bit13: TTagBit;
    PLCBlock1_e1_bit14: TTagBit;
    PLCBlock1_e1_bit15: TTagBit;
    PLCBlock1_e1_bit2: TTagBit;
    PLCBlock1_e1_bit3: TTagBit;
    PLCBlock1_e1_bit4: TTagBit;
    PLCBlock1_e1_bit5: TTagBit;
    PLCBlock1_e1_bit6: TTagBit;
    PLCBlock1_e1_bit7: TTagBit;
    PLCBlock1_e1_bit8: TTagBit;
    PLCBlock1_e1_bit9: TTagBit;
    UpdateActiveAlarms: TTimer;
    ZConnection1: TZConnection;
    InsertAlarmQuery: TZQuery;
    ActiveAlarms: TZQuery;
    procedure FormCreate(Sender: TObject);
    procedure PLCBlock1ValueChangeLast(Sender: TObject);
    procedure UpdateActiveAlarmsTimer(Sender: TObject);
  private
    AlarmBitID:array[0..31] of Integer;
  public
    function InsertAlarm(dtStart:TDateTime; message, username, tagname:Utf8String; tagValue:Double):Integer;
    procedure FinishAlarm(AlarmID:Integer; dt_finish:TDateTime);
    procedure FinishAllAlarms;
  end;

var
  Form1: TForm1;

implementation

uses ControlSecurityManager;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ZConnection1.Database:=ExtractFilePath(Application.ExeName)+'alarms.db3';
  ZConnection1.Connect;
  FinishAllAlarms;
end;

procedure TForm1.PLCBlock1ValueChangeLast(Sender: TObject);
var
  w, b, idx, bitValue: Integer;
  AlarmMessages:array[0..31] of String;
  WordValue: Word;
begin
  AlarmMessages[0]  := 'Alarm message 1';
  AlarmMessages[1]  := 'Alarm message 2';
  AlarmMessages[2]  := 'Alarm message 3';
  AlarmMessages[3]  := 'Alarm message 4';
  AlarmMessages[4]  := 'Alarm message 5';
  AlarmMessages[5]  := 'Alarm message 6';
  AlarmMessages[6]  := 'Alarm message 7';
  AlarmMessages[7]  := 'Alarm message 8';
  AlarmMessages[8]  := 'Alarm message 9';
  AlarmMessages[9]  := 'Alarm message 10';
  AlarmMessages[10] := 'Alarm message 11';
  AlarmMessages[11] := 'Alarm message 12';
  AlarmMessages[12] := 'Alarm message 13';
  AlarmMessages[13] := 'Alarm message 14';
  AlarmMessages[14] := 'Alarm message 15';
  AlarmMessages[15] := 'Alarm message 16';
  AlarmMessages[16] := 'Alarm message 17';
  AlarmMessages[17] := 'Alarm message 18';
  AlarmMessages[18] := 'Alarm message 19';
  AlarmMessages[19] := 'Alarm message 20';
  AlarmMessages[20] := 'Alarm message 21';
  AlarmMessages[21] := 'Alarm message 22';
  AlarmMessages[22] := 'Alarm message 23';
  AlarmMessages[23] := 'Alarm message 24';
  AlarmMessages[24] := 'Alarm message 25';
  AlarmMessages[25] := 'Alarm message 26';
  AlarmMessages[26] := 'Alarm message 27';
  AlarmMessages[27] := 'Alarm message 28';
  AlarmMessages[28] := 'Alarm message 29';
  AlarmMessages[29] := 'Alarm message 30';
  AlarmMessages[30] := 'Alarm message 31';
  AlarmMessages[31] := 'Alarm message 32';

  for w:=0 to PLCBlock1.Size-1 do begin
    WordValue:=Trunc(PLCBlock1.ValueRaw[w]);
    for b:=0 to 15 do begin
      idx:=w*16+b;
      bitValue:=1 shl b;

      if AlarmBitID[idx]<>0 then
        FinishAlarm(AlarmBitID[idx], Now);

      if (WordValue and bitValue)=bitValue then
        AlarmBitID[idx]:=InsertAlarm(now, AlarmMessages[idx], GetControlSecurityManager.GetCurrentUserlogin, format('PLCBlock1[%d].bit%d',[w,b]), 1)
    end;
  end;
  UpdateActiveAlarms.Enabled:=false;
  UpdateActiveAlarms.Enabled:=true; //resets the timer, avoid a lot of queries in short time, updates the alarm list after one second without alarms.
end;

procedure TForm1.UpdateActiveAlarmsTimer(Sender: TObject);
begin
  ActiveAlarms.Close;
  ActiveAlarms.Open;
  UpdateActiveAlarms.Enabled:=false;
end;

function TForm1.InsertAlarm(dtStart: TDateTime; message, username,
  tagname: Utf8String; tagValue: Double): Integer;
begin

  InsertAlarmQuery.SQL.Text:='INSERT INTO tbl_alarm (dt_started, dt_finished, dt_ack, st_message, st_user, st_tagname, nr_tagvalue) VALUES (:dt_start, NULL, NULL, :message, :username, :tagname, :tagvalue);';
  InsertAlarmQuery.ParamByName('dt_start').AsDateTime := dtStart;
  InsertAlarmQuery.ParamByName('message').AsString    := message;
  InsertAlarmQuery.ParamByName('username').AsString   := username;
  InsertAlarmQuery.ParamByName('tagname').AsString    := tagname;
  InsertAlarmQuery.ParamByName('tagvalue').AsFloat    := tagValue;
  InsertAlarmQuery.ExecSQL;

  InsertAlarmQuery.SQL.Text:='SELECT last_insert_rowid() as ID;';
  InsertAlarmQuery.Open;
  try
    Result := InsertAlarmQuery.FieldByName('ID').AsInteger;
  finally
    InsertAlarmQuery.Close;
  end;
end;

procedure TForm1.FinishAlarm(AlarmID: Integer; dt_finish: TDateTime);
begin
  UpdateAlarms.SQL.Text:='UPDATE tbl_alarm SET dt_finished=:dtfinished WHERE di_alarm=:AlarmID;';
  UpdateAlarms.ParamByName('dtfinished').AsDateTime := dt_finish;
  UpdateAlarms.ParamByName('AlarmID').AsInteger     := AlarmID;
  UpdateAlarms.ExecSQL;
end;

procedure TForm1.FinishAllAlarms;
begin
  UpdateAlarms.SQL.Text:='UPDATE tbl_alarm SET dt_finished=CURRENT_TIMESTAMP WHERE dt_finished IS NULL;';
  UpdateAlarms.ExecSQL;
end;

end.

