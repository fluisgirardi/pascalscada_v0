unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, S7PlusFamily,
  tcp_udpport, PLCTagNumber, HMILabel, HMIS7PlusAlarmLogger, HMIAlarmLogger,
  S7PlusAlarms, HMIDBConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIDBConnection1: THMIDBConnection;
    HMILabel1: THMILabel;
    HMIS7PlusAlarmLogger1: THMIS7PlusAlarmLogger;
    S7CommPlusDriver1: TS7CommPlusDriver;
    tagMArea_Clock_5Hz: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    procedure HMIS7PlusAlarmLogger1IncomingAlarm(Sender: TObject;
      aTimeStamp: TDateTime; AlarmMsgItem: TAlarmItem; AlarmIntID: Int64;
      AlarmGUID: TGuid; var AlarmIncommingSQL: UTF8String);
    procedure HMIS7PlusAlarmLogger1S7PlusAlarmIncoming(Sender: TObject;
      aTimeStamp: TDateTime; const Alarm: TS7PlusAlarm; AlarmIntID: Int64;
      AlarmGUID: TGuid; var AlarmIncomingSQL: UTF8String);
    procedure HMIS7PlusAlarmLogger1S7PlusAlarmOutgoing(Sender: TObject;
      aTimeStamp: TDateTime; const Alarm: TS7PlusAlarm; AlarmIntID: Int64;
      AlarmGUID: TGuid; var AlarmOutgoingSQL: UTF8String);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.HMIS7PlusAlarmLogger1IncomingAlarm(Sender: TObject;
  aTimeStamp: TDateTime; AlarmMsgItem: TAlarmItem; AlarmIntID: Int64;
  AlarmGUID: TGuid; var AlarmIncommingSQL: UTF8String);
begin
  AlarmIncommingSQL:='';
end;

procedure TForm1.HMIS7PlusAlarmLogger1S7PlusAlarmIncoming(Sender: TObject;
  aTimeStamp: TDateTime; const Alarm: TS7PlusAlarm; AlarmIntID: Int64;
  AlarmGUID: TGuid; var AlarmIncomingSQL: UTF8String);
begin
  AlarmIncomingSQL:='';
end;

procedure TForm1.HMIS7PlusAlarmLogger1S7PlusAlarmOutgoing(Sender: TObject;
  aTimeStamp: TDateTime; const Alarm: TS7PlusAlarm; AlarmIntID: Int64;
  AlarmGUID: TGuid; var AlarmOutgoingSQL: UTF8String);
begin
   AlarmOutgoingSQL:='';
end;

end.

