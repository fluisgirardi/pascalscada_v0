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

uses
  Classes, SysUtils, ProtocolTypes, plcblock;

type

  { TBlockStructTagAssistant }

  TBlockStructTagAssistant = class(TComponent)
    private
      FBlockStructTag: TPLCBlock;
      FOwnerOfNewTags:TComponent;
      FInsertHook:TAddTagInEditorHook;
      FCreateProc:TCreateTagProc;
      procedure BlockElementMapper();
      procedure StructElementMapper();
    public
      {$IFDEF PORTUGUES}
      //: Abre a ferramenta de mapeamento de tags elementos de bloco.
      {$ELSE}
      //: Opens the tool to map block elements.
      {$ENDIF}
      procedure OpenElementMapper(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
        CreateProc:TCreateTagProc); virtual;

    published
      property BlockStructTag:TPLCBlock read FBlockStructTag write FBlockStructTag;
end;

implementation

uses
  uelementmapper, ustructuremapper, plcblockelement, plcstructelement, tag,
  plcstruct, hsstrings, Controls, Dialogs, math, StrUtils;

procedure TBlockStructTagAssistant.BlockElementMapper;
var
  dlg:TfrmMapElements;
  startelement,
  endelement,
  curelement,
  elementnumber:Integer;
  tagelement:TPLCBlockElement;

  function GetNewTagElementName:String;
  var
    n:String;
  begin
    n:=IntToStr(elementnumber);
    Result:=dlg.elementnames.Text;
    Result := StringReplace(Result,'%e',n,[rfReplaceAll]);

    n:=FBlockStructTag.Name;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;

begin
  //se não está em design sai.
  if [csDesigning]*ComponentState=[] then exit;

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
        tagelement:=TPLCBlockElement(FCreateProc(TPLCBlockElement));
        tagelement.Name:=GetNewTagElementName;
        tagelement.PLCBlock:=FBlockStructTag;
        tagelement.Index:=curelement;
        FInsertHook(tagelement);
        inc(curelement);
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;

procedure TBlockStructTagAssistant.StructElementMapper;
var
  frmstructedit:TfrmStructureEditor;

  curitem,
  curidx,
  curstructitem:Integer;

  item:TPLCStructItem;

  function GetCurWordSize:Integer;
  begin
    case frmstructedit.StructItem[curstructitem].TagType of
      pttDefault, pttShortInt, pttByte:
        Result := 1;
      pttSmallInt, pttWord:
        Result := 2;
      pttInteger, pttDWord, pttFloat:
        Result := 4;
    end;
  end;

  function GetValueWithZeros(value, endvalue:Integer; toFill:Boolean):String;
  var
    numdig, dig:Integer;
    strendval, fill:STring;
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

  function GetName(namepattern:String):String;
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
  //se não está em design sai.
  //if isn't at design time, exit.
  if [csDesigning]*ComponentState=[] then exit;

  frmstructedit:=TfrmStructureEditor.Create(nil);
  try
    if frmstructedit.ShowModal=mrOk then begin
      curidx:=0;
      for curitem:=1 to frmstructedit.SpinEdit1.Value do begin
        for curstructitem:=0 to frmstructedit.StructItemsCount-1 do begin
          if not frmstructedit.StructItem[curstructitem].SkipTag then begin
            item := TPLCStructItem(FCreateProc(TPLCStructItem));
            with item do begin
              Name:=GetName(frmstructedit.StructItem[curstructitem].TagName);
              TagType:=frmstructedit.StructItem[curstructitem].TagType;
              SwapBytes:=frmstructedit.StructItem[curstructitem].SwapBytes;
              SwapWords:=frmstructedit.StructItem[curstructitem].SwapWords;
              Index:=curidx;
              FBlockStructTag.Size:=Max(FBlockStructTag.Size, curidx+GetCurWordSize);
              PLCBlock:=FBlockStructTag as TPLCStruct;
            end;
            FInsertHook(item);
          end;
          inc(curidx,GetCurWordSize)
        end;
      end;
    end;
  finally
    frmstructedit.Destroy;
  end;
end;

procedure TBlockStructTagAssistant.OpenElementMapper(OwnerOfNewTags:TComponent;
  InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  if not Assigned(FBlockStructTag) then
    begin
      ShowMessage(SBlockRequired);
      Exit;
    end;

  FOwnerOfNewTags:=OwnerOfNewTags;
  FInsertHook:=InsertHook;
  FCreateProc:=CreateProc;

  if FBlockStructTag is TPLCStruct then
    StructElementMapper
     else
       BlockElementMapper;
end;

end.

