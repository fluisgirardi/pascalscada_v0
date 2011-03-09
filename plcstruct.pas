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

uses ustructuremapper, Controls;

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
    has_atleastonereplacement:=(Pos('%a',namepattern)<>0) or
                               (Pos('%i',namepattern)<>0) or
                               (Pos('%e',namepattern)<>0) or
                               (Pos('%0a',namepattern)<>0) or
                               (Pos('%0i',namepattern)<>0) or
                               (Pos('%0e',namepattern)<>0);
    if not has_atleastonereplacement then
      namepattern:=namepattern+'%i';

    Result:=namepattern;
    if frmS7tb.MemoryArea.ItemIndex in [4,9,5,10] then begin
      Result:= StringReplace(Result,'%a', IntToStr(curTCaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curTCaddress, frmS7tb.GetTheLastItemOffset div 2, true),[rfReplaceAll]);
    end else begin
      Result:= StringReplace(Result,'%a',IntToStr(curaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curaddress, frmS7tb.RealEndOffset, true),[rfReplaceAll]);
    end;
    Result:= StringReplace(Result,'%i',IntToStr(curitem),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0i',GetValueWithZeros(curitem, frmS7tb.spinNumItens.Value, true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%e',IntToStr(curitem-1),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0e',GetValueWithZeros(curitem-1, frmS7tb.spinNumItens.Value-1, true),[rfReplaceAll]);
  end;
begin
  //se não está em design sai.
  if [csDesigning]*ComponentState=[] then exit;

  frmstructedit:=TfrmStructureEditor.Create(nil);
  try
    if frmstructedit.ShowModal=mrOk then begin
      for curitem:=1 to frmstructedit.SpinEdit1.Value do begin
        for curstructitem:=0 to frmstructedit.StructItemsCount-1 do begin

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
