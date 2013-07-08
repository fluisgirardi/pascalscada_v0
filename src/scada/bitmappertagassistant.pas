{$i ../common/language.inc}
{:
  @abstract(Implementation of BitMapper for TPLCNumber.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
unit bitmappertagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ProtocolTypes, plcnumber;

type

  { TBitMapTagAssistant }

  TBitMapperTagAssistant = class(TComponent)
  private
    FNumberTag: TPLCNumberMappable;
    public
      //: Opens the bit mapper wizard.
      procedure OpenBitMapper(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
        CreateProc:TCreateTagProc); virtual;

    published
      property NumberTag:TPLCNumberMappable read FNumberTag write FNumberTag;
end;

implementation

uses
  ubitmapper, hsstrings, Controls, Dialogs, TagBit;

{ TBitMapTagAssistant }

procedure TBitMapperTagAssistant.OpenBitMapper(OwnerOfNewTags:TComponent;
  InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  dlg:TfrmBitMapper;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:Integer;
  tbit:TTagBit;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if dlg.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if dlg.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if dlg.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:String;
  var
    n:String;
  begin
    n:=IntToStr(bitnum);
    Result:=dlg.edtNamepattern.Text;
    Result := StringReplace(Result,'%b',n,[rfReplaceAll]);

    n:=IntToStr(bytenum);
    Result := StringReplace(Result,'%B',n,[rfReplaceAll]);

    n:=IntToStr(wordnum);
    Result := StringReplace(Result,'%w',n,[rfReplaceAll]);

    n:=FNumberTag.Name;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;
begin

  if not Assigned(FNumberTag) then
    begin
      ShowMessage(SNumberTagRequired);
      Exit;
    end;


  //bit mapper...

  //se não está em design sai.
  //if it's not at designtime, exit.
  if [csDesigning]*ComponentState=[] then exit;

  dlg:=TfrmBitMapper.Create(nil);
  try
    if dlg.ShowModal=mrOK then begin
      startbit:=31-dlg.StringGrid1.Selection.Right;
      endbit:=31-dlg.StringGrid1.Selection.Left;
      curbit:=startbit;
      if dlg.eachbitastag.Checked then begin
        while curbit<=endbit do begin
          updatenumbers;
          tbit:=TTagBit(CreateProc(TTagBit));
          tbit.Name:=GetNewTagBitName;
          tbit.PLCTag:=FNumberTag;
          tbit.EndBit:=curbit;
          tbit.StartBit:=curbit;
          InsertHook(tbit);
          inc(curbit);
        end;
      end else begin
        updatenumbers;
        tbit:=TTagBit(CreateProc(TTagBit));
        tbit.Name:=GetNewTagBitName;
        tbit.PLCTag:=FNumberTag;
        tbit.EndBit:=endbit;
        tbit.StartBit:=startbit;
        InsertHook(tbit);
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;


end.

