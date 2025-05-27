unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids,
  StdCtrls, ZConnection, ZDataset, ZAbstractRODataset, HMIAlarmLogger,
  HMITrackBar, HMIDBConnection, PLCTagNumber;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    DBGrid1: TDBGrid;
    ds_AlarmesAtivos: TDataSource;
    HMIAlarmLogger1: THMIAlarmLogger;
    HMIDBConnection1: THMIDBConnection;
    HMITrackBar1: THMITrackBar;
    PLCTagNumber1: TPLCTagNumber;
    AtualizaDelay: TTimer;
    tbl_AlarmesAtivosds_local: TZRawStringField;
    tbl_AlarmesAtivosds_msg: TZRawStringField;
    tbl_AlarmesAtivosds_nometag: TZRawStringField;
    tbl_AlarmesAtivosds_username: TZRawStringField;
    tbl_AlarmesAtivosdt_ack: TZDateTimeField;
    tbl_AlarmesAtivosdt_inicio: TZDateTimeField;
    tbl_AlarmesAtivosdt_termino: TZDateTimeField;
    tbl_AlarmesAtivosid_alarme: TZIntegerField;
    tbl_AlarmesAtivosnr_tagvalue: TZIntegerField;
    tbl_AlarmesAtivosuuid_alarme: TZGuidField;
    ZConnection1: TZConnection;
    tbl_AlarmesAtivos: TZReadOnlyQuery;
    procedure AtualizaDelayTimer(Sender: TObject);
    procedure ComboBox1EditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HMIAlarmLogger1FinishAllPendingAlarms(Sender: TObject;
      var FinishAllPendingAlarmsSQL: UTF8String);
    procedure HMIAlarmLogger1IncomingAlarm(Sender: TObject;
      aTimeStamp: TDateTime; AlarmMsgItem: TAlarmItem; AlarmIntID: Int64;
      AlarmGUID: TGuid; var AlarmIncommingSQL: UTF8String);
    procedure HMIAlarmLogger1OutgoingAlarm(Sender: TObject;
      aTimeStamp: TDateTime; AlamrIntID: Int64; AlarmGUID: TGuid;
      var OutgoingAlarmSQL: UTF8String);
    procedure HMIAlarmLogger1RefreshActiveAlarms(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses ControlSecurityManager, PLCTag, ProtocolTypes, PLCNumber;

{$R *.lfm}

{ TForm1 }

procedure TForm1.HMIAlarmLogger1IncomingAlarm(Sender: TObject;
  aTimeStamp: TDateTime; AlarmMsgItem: TAlarmItem; AlarmIntID: Int64;
  AlarmGUID: TGuid; var AlarmIncommingSQL: UTF8String);
var
  tagValue: Extended;
  tagName: String;
begin
  tagName:='(without name)';
  tagValue := 0.0;
  if Assigned(Sender) and (Sender is TPLCNumber) then begin
    tagName:=TPLCNumber(Sender).Name;
    tagValue := (Sender as TPLCNumber).Value;
  end;

  AlarmIncommingSQL:='INSERT INTO alarms.tbl_alarmes( '+
                     '         dt_inicio, '+
                     '         cd_mensagem, '+
                     '         nr_tagvalue, '+
                     '         cd_nometag, '+
                     '         cd_nomeusuario, '+
                     '         cd_local, '+
                     '         uuid_alarme) '+
                     ' VALUES ('+HMIDBConnection1.FormatPGDatetime(aTimeStamp)+', '+
                     '         textos.update_text(NULL, '+HMIDBConnection1.FormatSQLString(AlarmMsgItem.AlarmMessage)+',                     ''pt_br''), '+
                     '         '+HMIDBConnection1.FormatSQLNumber(tagValue,5)+', '+
                     '         textos.update_text(NULL, '+HMIDBConnection1.FormatSQLString(tagName)+',                                       ''pt_br''), '+
                     '         textos.update_text(NULL, '+HMIDBConnection1.FormatSQLString(GetControlSecurityManager.GetCurrentUserlogin)+', ''pt_br''), '+
                     '         textos.update_text(NULL, ''TRV GP2E'',                                                                        ''pt_br''), '+
                     '         '+HMIDBConnection1.FormatSQLUUID(AlarmGUID)+');';
end;

procedure TForm1.HMIAlarmLogger1OutgoingAlarm(Sender: TObject;
  aTimeStamp: TDateTime; AlamrIntID: Int64; AlarmGUID: TGuid;
  var OutgoingAlarmSQL: UTF8String);
begin
  OutgoingAlarmSQL:='UPDATE alarms.tbl_alarmes SET dt_termino = '+HMIDBConnection1.FormatPGDatetime(Now)+' WHERE uuid_alarme='+HMIDBConnection1.FormatSQLUUID(AlarmGUID)+' AND dt_termino IS NULL;';
end;

procedure TForm1.HMIAlarmLogger1RefreshActiveAlarms(Sender: TObject);
begin
  AtualizaDelay.Enabled:=true;
end;

procedure TForm1.HMIAlarmLogger1FinishAllPendingAlarms(Sender: TObject;
  var FinishAllPendingAlarmsSQL: UTF8String);
begin
  FinishAllPendingAlarmsSQL:='UPDATE alarms.tbl_alarmes SET dt_termino='+HMIDBConnection1.FormatPGDatetime(Now)+' WHERE dt_termino IS NULL;';
end;

procedure TForm1.AtualizaDelayTimer(Sender: TObject);
begin
  tbl_AlarmesAtivos.Refresh;
  AtualizaDelay.Enabled:=false;
end;

procedure TForm1.ComboBox1EditingDone(Sender: TObject);
begin
  tbl_AlarmesAtivos.Close;
  tbl_AlarmesAtivos.ParamByName('view').AsInteger:=ComboBox1.ItemIndex;
  tbl_AlarmesAtivos.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ZConnection1.Connected := true;
  HMIDBConnection1.Connected := true;
  tbl_AlarmesAtivos.ParamByName('view').AsInteger:=ComboBox1.ItemIndex;
  tbl_AlarmesAtivos.Open;
end;

end.

