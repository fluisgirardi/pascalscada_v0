{$i ../common/language.inc}
{:
  @abstract(OPC UA client protocol driver for Codesys PLCs.)

  Codesys controllers expose their symbol table over OPC UA. This driver is a
  TOPCUADriver with all its communication features inherited unchanged, but its
  design-time Tag Builder imports a Codesys Symbolconfiguration XML exported by
  the Codesys IDE instead of browsing the OPC UA server online. This way the
  complete variable list (name, IEC type, symbol path) is available offline and
  the NodeIds are generated automatically from the symbol paths.

  Tags use the same LongAddress NodeId syntax as TOPCUADriver, e.g.
  @code(ns=4;s=|var|Application.Motion_PRG.bBackward).
}
unit CodesysDriver;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ProtocolDriver, ProtocolTypes, OPCUADriver;

//: Registers the Tag Builder used by the Codesys OPC UA driver (design package).
procedure SetTagBuilderToolForCodesysDriver(TagBuilderTool:TOpenTagEditor);

type
  { TCodesysOPCUADriver }
  {: OPC UA client driver specialised for Codesys PLCs.
     Same communication as TOPCUADriver; the Tag Builder imports a Codesys
     Symbolconfiguration XML instead of browsing the server online. }
  TCodesysOPCUADriver = class(TOPCUADriver)
  public
    {: @seealso(TProtocolDriver.HasTabBuilderEditor) }
    function  HasTabBuilderEditor:Boolean; override;
    {: @seealso(TProtocolDriver.OpenTagEditor) }
    procedure OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
  end;

implementation

var
  CodesysDriverTagBuilderEditor:TOpenTagEditor = nil;

procedure SetTagBuilderToolForCodesysDriver(TagBuilderTool:TOpenTagEditor);
begin
  if Assigned(CodesysDriverTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for the Codesys driver was already assigned.');
  CodesysDriverTagBuilderEditor := TagBuilderTool;
end;

function TCodesysOPCUADriver.HasTabBuilderEditor:Boolean;
begin
  Result := true;
end;

procedure TCodesysOPCUADriver.OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  if Assigned(CodesysDriverTagBuilderEditor) then
    CodesysDriverTagBuilderEditor(Self, Self.Owner, InsertHook, CreateProc)
  else
    inherited OpenTagEditor(InsertHook, CreateProc);
end;

end.
