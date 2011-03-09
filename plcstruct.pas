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
begin
  //se não está em design sai.
  if [csDesigning]*ComponentState=[] then exit;

  frmstructedit:=TfrmStructureEditor.Create(nil);
  try
    if frmstructedit.ShowModal=mrOk then begin

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
