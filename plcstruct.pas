{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  @abstract(Implementação de um tag estrutura de comunicação com
  suporte a multi-tipos de dados.)
}
unit PLCStruct;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, PLCBlock, ProtocolTypes, Tag;

type
  TPLCStruct = class(TPLCBlock)
  protected
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
    procedure SetTagType(newType:TTagType); override;
    procedure SetSwapWords(v:Boolean); override;
    procedure SetSwapBytes(v:Boolean); override;
  public
    constructor Create(AOwner:TComponent); override;
    procedure OpenElementMapper(OwnerOfNewTags: TComponent; InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc); override;
  end;

implementation

uses ustructuremapper, Controls, PLCStructElement, sysutils, math,
     StrUtils;

constructor TPLCStruct.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Inherited SetTagType(pttByte);
end;

procedure TPLCStruct.OpenElementMapper(OwnerOfNewTags: TComponent; InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
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
  if [csDesigning]*ComponentState=[] then exit;

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
              Self.Size:=Max(Self.Size, curidx+GetCurWordSize);
              PLCBlock:=Self;
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

function TPLCStruct.IsMyCallBack(Cback: TTagCommandCallBack): Boolean;
begin
  Result:=inherited IsMyCallBack(Cback) and (TMethod(Cback).Code=@TPLCStruct.TagCommandCallBack);
end;

procedure TPLCStruct.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
begin
  inherited TagCommandCallBack(Values, ValuesTimeStamp, TagCommand, LastResult, Offset);
end;

procedure TPLCStruct.SetTagType(newType:TTagType);
begin
  Inherited SetTagType(pttByte);
end;

procedure TPLCStruct.SetSwapWords(v:Boolean);
begin
  inherited SetSwapWords(false);
end;

procedure TPLCStruct.SetSwapBytes(v:Boolean);
begin
  inherited SetSwapBytes(false);
end;

end.
