unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, HMIEventLogger,
  HMITrackBar, HMIText, PLCTagNumber, HMIDBConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIDBConnection1: THMIDBConnection;
    HMITrackBar2: THMITrackBar;
    PLCTagNumber2: TPLCTagNumber;
    THMIEventLogger1: THMIEventLogger;
    HMIText1: THMIText;
    HMITrackBar1: THMITrackBar;
    PLCTagNumber1: TPLCTagNumber;
    procedure THMIEventLogger1FinishAllTagEvents(Sender: TObject;
      var FinishAllEventsSQL: String);
    function THMIEventLogger1GenerateNewEventID(var EventIntID: Int64;
      var EventGUID: TGuid): Boolean;
    procedure THMIEventLogger1NewTagEvent(Sender: TObject;
      TagItem: TEventTagColletionItem; EventIntID: Int64; EventGUID: TGuid;
      EventDesc: TEventCollectionItem;
      var NewTagEventSQL: THMIDBConnectionStatementList);
    procedure THMIEventLogger1TagEventFinished(Sender: TObject;
      EventIntID: Int64; EventGUID: TGuid; var FinishEventSQL: String);
  private
      timeStart: String;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.THMIEventLogger1FinishAllTagEvents(Sender: TObject;
  var FinishAllEventsSQL: String);
var
  x: TFormatSettings;
begin
  HMIText1.Zones;
  x.DateSeparator:='-';
  x.TimeSeparator:=':';
  FinishAllEventsSQL:='UPDATE eventos.tbl_eventos SET dt_termino='''+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now, x)+''' WHERE dt_termino IS NULL;';
end;

function TForm1.THMIEventLogger1GenerateNewEventID(var EventIntID: Int64;
  var EventGUID: TGuid): Boolean;
begin
  EventIntID:=0;

  CreateGUID(EventGUID);
  exit(true);
end;

procedure TForm1.THMIEventLogger1NewTagEvent(Sender: TObject;
  TagItem: TEventTagColletionItem; EventIntID: Int64; EventGUID: TGuid;
  EventDesc: TEventCollectionItem;
  var NewTagEventSQL: THMIDBConnectionStatementList);
var
  x: TFormatSettings;
begin
  writeln('HMIEventLogger1NewTagEvent');
  x.DateSeparator:='-';
  x.TimeSeparator:=':';
  timeStart := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now, x);
  if Assigned(NewTagEventSQL) then begin
    NewTagEventSQL.Add('INSERT INTO eventos.tbl_tag_evento (id_tag, ds_tag) '+
                       ' VALUES ('+TagItem.TagID.ToString+', '''+TagItem.TagPath.Replace('''','', [rfReplaceAll])+''') '+
                       ' ON CONFLICT (id_tag) DO UPDATE SET ds_tag='''+TagItem.TagPath.Replace('''','', [rfReplaceAll])+'''');
    NewTagEventSQL.Add('INSERT INTO eventos.tbl_eventos (dt_inicio,                                                 cd_tag,                     cd_status,                         cd_ds_status                                   ,uuid_evento, cd_ds_color) '+
                       '                          VALUES(''' + timeStart + ''', '+TagItem.TagID.ToString+', '+EventDesc.EventValue.ToString+', textos.update_text(NULL,'''+EventDesc.EventDescription+''', ''pt_br'', ''Eventos''), '''+GUIDToString(EventGUID)+''', ' + inttostr(ColorToRGB(EventDesc.EventColor)) + ');');
  end;
end;

procedure TForm1.THMIEventLogger1TagEventFinished(Sender: TObject;
  EventIntID: Int64; EventGUID: TGuid; var FinishEventSQL: String);
var
  x: TFormatSettings;
begin
  writeln('HMIEventLogger1TagEventFinished1');
  x.DateSeparator:='-';
  x.TimeSeparator:=':';
  FinishEventSQL:='UPDATE eventos.tbl_eventos SET dt_termino=''' + timeStart + ''' WHERE dt_termino IS NULL AND uuid_evento='''+GUIDToString(EventGUID)+''';';
end;

end.

