unit tagstatuslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, FileUtil, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, Grids, ExtCtrls, PLCTagNumber, ProtocolDriver, PLCBlock;

type
  TfrmTagStatusList = class(TForm)
    ButtonPanel1: TButtonPanel;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  frmTagStatusList: TfrmTagStatusList;

procedure ShowTagStatusList(const aProtocol:TProtocolDriver);

implementation

procedure ShowTagStatusList(const aProtocol: TProtocolDriver);
var
  frm: TfrmTagStatusList;
begin
  frm:=TfrmTagStatusList.Create(aProtocol);
  try
    frm.Timer1Timer(frm.Timer1);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

{$R *.lfm}

procedure TfrmTagStatusList.Timer1Timer(Sender: TObject);
var
  aprot: TProtocolDriver;
  t: Integer;
begin
  if not (Owner is TProtocolDriver) then
    raise exception.Create('Tag status list owner must be a TProtocolDriver!');

  aprot:=Owner as TProtocolDriver;

  //StringGrid1.RowCount:=1;
  StringGrid1.RowCount:=aprot.TagCount+1;
  for t:=0 to aprot.TagCount-1 do begin
    while StringGrid1.Rows[t+1].Count<15 do StringGrid1.Rows[t+1].Add('');
    StringGrid1.Rows[t+1].Strings[0]:= aprot.TagName[t];
    StringGrid1.Rows[t+1].Strings[1]:= aprot.Tag[t].ClassName;

    if aprot.Tag[t] is TPLCTagNumber then begin
      with aprot.Tag[t] as TPLCTagNumber do begin
        StringGrid1.Rows[t+1].Strings[02]:=GetEnumName(TypeInfo(LastSyncReadStatus), integer(LastASyncReadStatus));
        StringGrid1.Rows[t+1].Strings[03]:=inttostr(MemAddress);
        StringGrid1.Rows[t+1].Strings[04]:=inttostr(MemFile_DB);
        StringGrid1.Rows[t+1].Strings[05]:=inttostr(MemReadFunction);
        StringGrid1.Rows[t+1].Strings[06]:=inttostr(MemSubElement);
        StringGrid1.Rows[t+1].Strings[07]:=inttostr(MemWriteFunction);
        StringGrid1.Rows[t+1].Strings[08]:=''; //inttostr(MemAddress);
        StringGrid1.Rows[t+1].Strings[09]:=inttostr(PLCRack);
        StringGrid1.Rows[t+1].Strings[10]:=inttostr(PLCSlot);
        StringGrid1.Rows[t+1].Strings[11]:=inttostr(PLCStation);
        StringGrid1.Rows[t+1].Strings[12]:=inttostr(RefreshTime);
        StringGrid1.Rows[t+1].Strings[13]:=GetEnumName(TypeInfo(TagType), integer(TagType));
        StringGrid1.Rows[t+1].Strings[14]:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ValueTimestamp);
      end;
      Continue;
    end;

    if aprot.Tag[t] is TPLCBlock then begin
      with aprot.Tag[t] as TPLCBlock do begin
        StringGrid1.Rows[t+1].Strings[02]:=GetEnumName(TypeInfo(LastSyncReadStatus), integer(LastASyncReadStatus));
        StringGrid1.Rows[t+1].Strings[03]:=inttostr(MemAddress);
        StringGrid1.Rows[t+1].Strings[04]:=inttostr(MemFile_DB);
        StringGrid1.Rows[t+1].Strings[05]:=inttostr(MemReadFunction);
        StringGrid1.Rows[t+1].Strings[06]:=inttostr(MemSubElement);
        StringGrid1.Rows[t+1].Strings[07]:=inttostr(MemWriteFunction);
        StringGrid1.Rows[t+1].Strings[08]:=''; //inttostr(MemAddress);
        StringGrid1.Rows[t+1].Strings[09]:=inttostr(PLCRack);
        StringGrid1.Rows[t+1].Strings[10]:=inttostr(PLCSlot);
        StringGrid1.Rows[t+1].Strings[11]:=inttostr(PLCStation);
        StringGrid1.Rows[t+1].Strings[12]:=inttostr(RefreshTime);
        StringGrid1.Rows[t+1].Strings[13]:=GetEnumName(TypeInfo(TagType), integer(TagType));
        StringGrid1.Rows[t+1].Strings[14]:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ValueTimestamp);
      end;
      Continue;
    end;
  end;
end;

procedure TfrmTagStatusList.StringGrid1PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow>0 then begin
    if lowercase(StringGrid1.Rows[aRow].Strings[2])<>'iook' then begin
      StringGrid1.Canvas.Brush.Color:=clYellow;
      StringGrid1.Canvas.Pen.Color:=clBlack;
    end;

  end;

end;

end.

