{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementação de um tag estrutura de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements a structure communication tag.)
}
{$ENDIF}
unit PLCStruct;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, PLCBlock, ProtocolTypes, Tag;

type
  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Classe de tag estrutura de comunicação.)
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Class of an structure communication tag.)
  }
  {$ENDIF}
  TPLCStruct = class(TPLCBlock)
  protected
    //: @seealso(TPLCTag.IsMyCallBack)
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
    //: @seealso(TPLCTag.SetTagType)
    procedure SetTagType(newType:TTagType); override;
    //: @seealso(TPLCTag.SwapDWords)
    procedure SetSwapDWords(v:Boolean); override;
    //: @seealso(TPLCTag.SetSwapWords)
    procedure SetSwapWords(v:Boolean); override;
    //: @seealso(TPLCTag.SetSwapBytes)
    procedure SetSwapBytes(v:Boolean); override;
  public
    //: @xclude
    constructor Create(AOwner:TComponent); override;
    {$IFDEF PORTUGUES}
    //: Abre o editor de strutura.
    {$ELSE}
    //: Opens the structure editor.
    {$ENDIF}
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

procedure TPLCStruct.SetSwapDWords(v: Boolean);
begin
  inherited SetSwapDWords(false);
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
