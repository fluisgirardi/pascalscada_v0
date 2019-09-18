{$i ../common/language.inc}
{:
  @abstract(Implementation of BitMapper for TPLCNumber.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  02/2014 - Changed to the old behavior (Right click on PLCNumber Tag ->
  "Map bits" to open the bit mapper editor, whithout link with gui.
  ***********************************************************************
}
unit bitmappertagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

implementation

uses
  Classes, SysUtils, ProtocolTypes, plcnumber, ubitmapper, hsstrings, Controls,
  Dialogs, TagBit;

{ TBitMapTagAssistant }

procedure OpenBitMapper(Target,
                        OwnerOfNewTags:TComponent;
                        InsertHook:TAddTagInEditorHook;
                        CreateProc:TCreateTagProc);
var
  dlg:TfrmBitMapper;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:LongInt;
  tbit:TTagBit;
  FNumberTag:TPLCNumberMappable;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if dlg.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if dlg.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if dlg.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:AnsiString;
  var
    n:AnsiString;
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
  if not Assigned(Target) then begin
    ShowMessage(SNumberTagRequired);
    Exit;
  end;

  if not (Target is TPLCNumberMappable) then begin
    ShowMessage(SNumberTagRequired);
    Exit;
  end;

  FNumberTag:=TPLCNumberMappable(Target);

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

initialization

  SetTagBitMapper(@OpenBitMapper);

end.

