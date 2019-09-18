{$i ../common/language.inc}
{:
  @abstract(Implementation of BlockElementMapper for TPLCBlock.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
unit blockstructtagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

implementation

uses
  Classes, SysUtils, ProtocolTypes, plcblock, uelementmapper, ustructuremapper,
  plcblockelement, plcstructelement, tag, plcstruct, hsstrings, Controls,
  Dialogs, math, StrUtils;

procedure BlockElementMapper(Target,
                             OwnerOfNewTags:TComponent;
                             InsertHook:TAddTagInEditorHook;
                             CreateProc:TCreateTagProc);
var
  dlg:TfrmMapElements;
  startelement,
  endelement,
  curelement,
  elementnumber:LongInt;
  tagelement:TPLCBlockElement;
  FBlockStructTag: TPLCBlock;

  function GetNewTagElementName:AnsiString;
  var
    n:AnsiString;
  begin
    n:=IntToStr(elementnumber);
    Result:=dlg.elementnames.Text;
    Result := StringReplace(Result,'%e',n,[rfReplaceAll]);

    n:=Target.Name;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;

begin
  if not Assigned(Target) then begin
    ShowMessage(SBlockRequired);
    Exit;
  end;

  if not (Target is TPLCBlock) then begin
    ShowMessage(SBlockRequired);
    Exit;
  end;

  FBlockStructTag:=TPLCBlock(Target);

  dlg:=TfrmMapElements.Create(nil);
  try
    dlg.startindex.MinValue:=0;
    dlg.startindex.MaxValue:=FBlockStructTag.Size-1;

    dlg.endindex.MinValue:=0;
    dlg.endindex.MaxValue:=FBlockStructTag.Size-1;

    if dlg.ShowModal=mrOK then begin

      if Pos('%e', dlg.elementnames.Text)=0 then
        dlg.elementnames.Text:=dlg.elementnames.Text+'%e';

      startelement:=dlg.startindex.Value;
      endelement:=dlg.endindex.Value;
      curelement:=startelement;
      while curelement<=endelement do begin
        elementnumber:=ifthen(dlg.ElementsStartFromOne.Checked,curelement+1,curelement);
        tagelement:=TPLCBlockElement(CreateProc(TPLCBlockElement));
        tagelement.Name:=GetNewTagElementName;
        tagelement.PLCBlock:=FBlockStructTag;
        tagelement.Index:=curelement;
        InsertHook(tagelement);
        inc(curelement);
      end;
    end;
  finally
    FreeAndNil(dlg);
  end;
end;

procedure StructElementMapper(Target,
                              OwnerOfNewTags:TComponent;
                              InsertHook:TAddTagInEditorHook;
                              CreateProc:TCreateTagProc);
var
  frmstructedit:TfrmStructureEditor;

  curitem,
  curidx,
  curstructitem:LongInt;

  item:TPLCStructItem;
  FBlockStructTag: TPLCStruct;

  function GetCurWordSize:LongInt;
  begin
    case frmstructedit.StructItem[curstructitem].TagType of
      pttDefault, pttShortInt, pttByte:
        Result := 1;
      pttSmallInt, pttWord:
        Result := 2;
      pttLongInt, pttDWord, pttFloat:
        Result := 4;
    end;
  end;

  function GetValueWithZeros(value, endvalue:LongInt; toFill:Boolean):AnsiString;
  var
    numdig, dig:LongInt;
    strendval, fill:AnsiString;
  begin
    strendval:=IntToStr(endvalue);

    fill:='';
    numdig:=Length(strendval);
    for dig:=1 to numdig do
      fill:=fill+'0';

    if toFill then
      Result:=RightStr(fill+IntToStr(value),numdig)
    else
      Result:=IntToStr(value);
  end;

  function GetName(namepattern:AnsiString):AnsiString;
  var
    has_atleastonereplacement:Boolean;
  begin
    {
    %i  - Numero do item comecando de 1
    %e  - Numero do item comecando de 0
    %0i - Numero do item comecando de 1, preenchido com zeros
    %0e - Numero do item comecando de 0, preenchido com zeros

    %i  - Structure item number, starting of 1
    %e  - Structure item number, starting of 0
    %0i - Structure item number, starting of 1, filled with zeros at the left
    %0e - Structure item number, starting of 0, filled with zeros at the left
    }

    has_atleastonereplacement:=(Pos('%i',namepattern)<>0) or
                               (Pos('%e',namepattern)<>0) or
                               (Pos('%0i',namepattern)<>0) or
                               (Pos('%0e',namepattern)<>0);
    if not has_atleastonereplacement then
      namepattern:=namepattern+'%i';

    Result:=namepattern;
    Result:= StringReplace(Result,'%i',IntToStr(curitem),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0i',GetValueWithZeros(curitem, frmstructedit.SpinEdit1.Value, true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%e',IntToStr(curitem-1),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0e',GetValueWithZeros(curitem-1, frmstructedit.SpinEdit1.Value-1, true),[rfReplaceAll]);
  end;
begin
  if not Assigned(Target) then begin
    ShowMessage(SBlockRequired);
    Exit;
  end;

  if not (Target is TPLCStruct) then begin
    ShowMessage(SBlockRequired);
    Exit;
  end;

  FBlockStructTag:=TPLCStruct(Target);

  frmstructedit:=TfrmStructureEditor.Create(nil);
  try
    if frmstructedit.ShowModal=mrOk then begin
      curidx:=0;
      for curitem:=1 to frmstructedit.SpinEdit1.Value do begin
        for curstructitem:=0 to frmstructedit.StructItemsCount-1 do begin
          if not frmstructedit.StructItem[curstructitem].SkipTag then begin
            item := TPLCStructItem(CreateProc(TPLCStructItem));
            with item do begin
              Name:=GetName(frmstructedit.StructItem[curstructitem].TagName);
              TagType:=frmstructedit.StructItem[curstructitem].TagType;
              SwapBytes:=frmstructedit.StructItem[curstructitem].SwapBytes;
              SwapWords:=frmstructedit.StructItem[curstructitem].SwapWords;
              Index:=curidx;
              FBlockStructTag.Size:=Max(FBlockStructTag.Size, curidx+GetCurWordSize);
              PLCBlock:=FBlockStructTag as TPLCStruct;
            end;
            InsertHook(item);
          end;
          inc(curidx,GetCurWordSize)
        end;
      end;
    end;
  finally
    frmstructedit.Destroy;
  end;
end;

initialization

  SetBlockElementMapper(@BlockElementMapper);
  SetStructItemMapper(@StructElementMapper);

end.

