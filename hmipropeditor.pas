{:
  @abstract(Implementacao dos editores de algumas propriedades de controles
            do PascalSCADA.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit hmipropeditor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$I delphiver.inc}

interface

uses
  Classes, SysUtils, HMIZones, Dialogs, Forms, Menus, ProtocolDriver, Tag,
  typinfo, HMIControlDislocatorAnimation, TagBit, PLCNumber, PLCBlock,
  {$IFDEF FPC}
    PropEdits, ComponentEditors, lazlclversion;
  {$ELSE}
    Types,
    //se for delphi 6 ou superior
    {$IF defined(DELPHI6_UP)}
      DesignIntf, DesignEditors;
    {$ELSE}
      //delphi 5 e inferiores.
      DsgnIntf;
    {$IFEND}
  {$ENDIF}

type
  //: Editor da propriedade TGraphicZone.FileName
  TZoneFileNamePropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

  //: Editor de varias propriedades de THMIControlDislocatorAnimation
  TPositionPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
  end;

  //: Editor da propriedade TZone.BlinkWith
  TZoneBlinkWithPropertyEditor = class(TIntegerProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  //: Editor de componente TagBuilder
  {$IFNDEF FPC}
  TDefaultComponentEditor = class(TComponentEditor);
  {$ENDIF}
  TTagBuilderComponentEditor = class(TDefaultComponentEditor)
  private
    procedure AddTagInEditor(Tag:TTag);
    function  CreateComponent(tagclass:TComponentClass):TComponent;
    procedure OpenTagBuilder;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
    function  ProtocolDriver: TProtocolDriver; virtual;
  end;

  TTagBitMapperComponentEditor = class(TDefaultComponentEditor)
  private
    procedure AddTagInEditor(Tag:TTag);
    function  CreateComponent(tagclass:TComponentClass):TComponent;
    procedure OpenBitMapper;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function NumericTag: TPLCNumber; virtual;
  end;

  TBlockElementMapperComponentEditor = class(TDefaultComponentEditor)
  private
    procedure AddTagInEditor(Tag:TTag);
    function  CreateComponent(tagclass:TComponentClass):TComponent;
    procedure OpenElementMapper;
  public
    procedure ExecuteVerb(Index: Integer); override;
    {$ifdef has_customhint}
    function GetCustomHint: String; override;
    {$endif}
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function Block: TPLCBlock; virtual;
  end;

implementation

function  TZoneFileNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TGraphicZone then
      Result := [paDialog{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TZoneFileNamePropertyEditor.GetValue: string;
begin
   Result := GetStrValue;
end;

procedure TZoneFileNamePropertyEditor.Edit;
var
  od: TOpenDialog;
begin
  if GetComponent(0) is TGraphicZone then begin
    od:=TOpenDialog.Create(nil);
    {$IFDEF FPC}
    od.Filter:='Imagens *.ico *.ppm *.pgm *.pbm *.png *.xpm *.bpm|*.ico;*.ppm;*.pgm;*.pbm;*.png;*.xpm;*.bpm';
    {$ELSE}
    od.Filter:='Imagens *.jpg *.jpeg *.bmp *.ico *.emf *.wmf|*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf';
    {$ENDIF}
    try
       if od.Execute then
          TGraphicZone(GetComponent(0)).FileName := od.FileName;
    finally
       od.Destroy;
    end;
  end;
end;

procedure TZoneFileNamePropertyEditor.SetValue(const Value: string);
begin
   SetStrValue(Value);
   if GetComponent(0) is TGraphicZone then
      TGraphicZone(GetComponent(0)).ImageListAsDefault := false;
end;

//editores de propriedades de BlinkWith
function  TZoneBlinkWithPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TZone then
      Result := [paValueList{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

procedure TZoneBlinkWithPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:Integer;
begin
   Proc('-1');
   if (GetComponent(0) is TZone) and (TZone(GetComponent(0)).Collection is TZones) then
      for i := 0 to TZone(GetComponent(0)).Collection.Count-1 do begin
         if TZone(GetComponent(0)).Collection.Items[i]<>GetComponent(0) then
            Proc(IntToStr(i));
      end;
end;

///////////////////////////////////////
//editor TAG BUILDER
///////////////////////////////////////

procedure TTagBuilderComponentEditor.OpenTagBuilder;
begin
  ProtocolDriver.OpenTagEditor(ProtocolDriver, AddTagInEditor, CreateComponent);
end;

procedure TTagBuilderComponentEditor.AddTagInEditor(Tag:TTag);
{$IFDEF FPC}
var
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
{$IFDEF FPC}
  Hook:=nil;
  if not GetHook(Hook) then exit;
  Hook.PersistentAdded(Tag,false);
  Modified;
{$ELSE}
  Designer.Modified;
{$ENDIF}
end;

function TTagBuilderComponentEditor.CreateComponent(tagclass:TComponentClass):TComponent;
begin
{$IFDEF FPC}
  Result := tagclass.Create(ProtocolDriver.Owner);
{$ELSE}
  Result := Designer.CreateComponent(tagclass,ProtocolDriver.Owner,0,0,0,0);
{$ENDIF}
end;

procedure TTagBuilderComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index=0 then
    OpenTagBuilder();
end;

function TTagBuilderComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index=0 then
    Result:='Tag Builder';
end;

function TTagBuilderComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TTagBuilderComponentEditor.ProtocolDriver: TProtocolDriver;
begin
  Result:=TProtocolDriver(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// BIT MAPPER
///////////////////////////////////////////////////////////////////////////////

procedure TTagBitMapperComponentEditor.AddTagInEditor(Tag:TTag);
{$IFDEF FPC}
var
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
{$IFDEF FPC}
  Hook:=nil;
  if not GetHook(Hook) then exit;
  Hook.PersistentAdded(Tag,false);
  Modified;
{$ELSE}
  Designer.Modified;
{$ENDIF}
end;

function  TTagBitMapperComponentEditor.CreateComponent(tagclass:TComponentClass):TComponent;
begin
{$IFDEF FPC}
  Result := tagclass.Create(NumericTag.Owner);
{$ELSE}
  Result := Designer.CreateComponent(tagclass,NumericTag.Owner,0,0,0,0);
{$ENDIF}
end;

procedure TTagBitMapperComponentEditor.OpenBitMapper;
begin
  NumericTag.OpenBitMapper(NumericTag, AddTagInEditor, CreateComponent);
end;

procedure TTagBitMapperComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index=0 then
    OpenBitMapper();
end;

function  TTagBitMapperComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index=0 then
    Result:='Map bits...';
end;

function  TTagBitMapperComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function  TTagBitMapperComponentEditor.NumericTag: TPLCNumber;
begin
  Result:=TPLCNumber(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// ELEMENT BLOCK MAPPER
///////////////////////////////////////////////////////////////////////////////

procedure TBlockElementMapperComponentEditor.AddTagInEditor(Tag:TTag);
{$IFDEF FPC}
var
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
{$IFDEF FPC}
  Hook:=nil;
  if not GetHook(Hook) then exit;
  Hook.PersistentAdded(Tag,false);
  Modified;
{$ELSE}
  Designer.Modified;
{$ENDIF}
end;

function  TBlockElementMapperComponentEditor.CreateComponent(tagclass:TComponentClass):TComponent;
begin
{$IFDEF FPC}
  Result := tagclass.Create(Block.Owner);
{$ELSE}
  Result := Designer.CreateComponent(tagclass,Block.Owner,0,0,0,0);
{$ENDIF}
end;

procedure TBlockElementMapperComponentEditor.OpenElementMapper;
begin
  Block.OpenElementMapper(Block, AddTagInEditor, CreateComponent);
end;

procedure TBlockElementMapperComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index=0 then
    OpenElementMapper();
end;

{$ifdef has_customhint}
function TBlockElementMapperComponentEditor.GetCustomHint: String;
var
  varPLCBlock:TPLCBlock;
begin
  varPLCBlock:=TPLCBlock(GetComponent);
  Result:=Result+'Block size:'+IntToStr(varPLCBlock.Size);
end;
{$endif}

function  TBlockElementMapperComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index=0 then
    Result:='Map block elements...';
end;

function  TBlockElementMapperComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function  TBlockElementMapperComponentEditor.Block:TPLCBlock;
begin
  Result:=TPLCBlock(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////

function  TPositionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is THMIControlDislocatorAnimation then
      Result := [paDialog{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TPositionPropertyEditor.GetValue: string;
begin
   Result := GetStrValue;
end;

procedure TPositionPropertyEditor.Edit;
var
  pname:String;
begin
  if GetComponent(0) is THMIControlDislocatorAnimation then begin
    with GetComponent(0) as THMIControlDislocatorAnimation do begin
      if Control=nil then exit;
      pname := LowerCase(GetName);
      if pname='gets_p0_position' then begin
        P0_X:=Control.Left;
        P0_Y:=Control.Top;
        exit;
      end;

      if pname='gets_p1_position' then begin
        P1_X:=Control.Left;
        P1_Y:=Control.Top;
        exit;
      end;
      if pname='goto_p0_position' then begin
        Control.Left:=P0_X;
        Control.Top:=P0_Y;
        exit;
      end;
    end;
  end;
end;

end.

