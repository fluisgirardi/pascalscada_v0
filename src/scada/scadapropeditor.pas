{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação dos editores de algumas propriedades de componentes
            do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements some property editors of PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Replaced ProtocolDriver with TagAssistant. (unit, properties and classes)
  07/2013 - Implemented Double-Click for the assistants.
  07/2013 - Replaced PlcNumber with BitMapTagAssistant. (unit, properties and classes)
  07/2013 - Replaced PlcBlock with BLockTagAssistant. (unit, properties and classes)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit scadapropeditor;

{$I ../common/delphiver.inc}

interface

uses
  Classes, SysUtils, SerialPort, PLCBlockElement, PLCStruct, Tag,
  bitmappertagassistant, blockstructtagassistant, ProtocolDriver,
  PLCNumber, ProtocolTypes,

  {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)}
  Windows,
  {$ELSE}
  Unix,
  {$IFEND}
  
  {$IFDEF FPC}
    PropEdits, ComponentEditors, lazlclversion;
  {$ELSE}
    Types,
    //Delphi 6 ou superior
    {$IF defined(DELPHI6_UP)}
      DesignIntf, DesignEditors;
    {$ELSE}
      //demais versoes do delphi
      DsgnIntf;
    {$IFEND}
  {$ENDIF}

type
  {$IFDEF PORTUGUES}
  //: Editor da propriedade TSerialPortDriver.COMPort
  {$ELSE}
  //: Property editor of TSerialPortDriver.COMPort property.
  {$ENDIF}
  TPortPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: AnsiString); override;
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade TPLCBlockElement.Index
  {$ELSE}
  //: Property editor of TPLCBlockElement.Index property.
  {$ENDIF}
  TElementIndexPropertyEditor = class(TIntegerProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFNDEF FPC}
  //: @exclude
  TDefaultComponentEditor = class(TComponentEditor);
  {$ENDIF}

  {$IFDEF PORTUGUES}
  {:
    Editor de componente base para todos os demais editores que irão inserir
    componentes na aplicação.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    Base class of Component editor for all component editors that will insert
    others componentes in application.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TInsertTagsOnFormComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddTagInEditor(Tag:TTag);
    function  CreateComponent(tagclass:TComponentClass):TComponent;
    function  GetTheOwner:TComponent; virtual;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente TagBuilder.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  TagBuilder component editor tool.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TProtocolDriverComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenTagBuilder;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function  GetVerb(Index: LongInt): AnsiString; override;
    function  GetVerbCount: LongInt; override;
    procedure Edit; override;
    function  ProtocolDriver: TProtocolDriver; virtual;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente BitMapper. Mapeia bits de um tag.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  BitMapper component editor tool. Map bits of a tag.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TTagBitMapperComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenBitMapper;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function GetVerb(Index: LongInt): AnsiString; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente BlockElementMapper. Mapeia elementos de um tag bloco.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  BlockElementMapper component editor tool. Map elements of a tag block.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TBlockElementMapperComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenElementMapper;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    {$if declared(has_customhints)}
    function GetCustomHint: AnsiString; override;
    {$ifend}
    function GetVerb(Index: LongInt): AnsiString; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
  end;

implementation

uses PLCBlock;

function  TPortPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TSerialPortDriver then
      Result := [paValueList{$IFDEF FPC}, paPickList{$ELSE}
                 {$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TPortPropertyEditor.GetValue: AnsiString;
begin
   Result := GetStrValue;
end;

procedure TPortPropertyEditor.GetValues(Proc: TGetStrProc);
{$IF defined(WIN32) or defined(WIN64)}
var
  c:LongInt;
  dcbstring, comname:AnsiString;
  d:DCB;
begin
  Proc('(none)');
  for c:=1 to 255 do begin
     comname := 'COM'+IntToStr(c);
     dcbstring := comname+': baud=1200 parity=N data=8 stop=1';
     if BuildCommDCB(PChar(dcbstring),d) then
        Proc(comname);
  end;
{$IFEND}
{$IFDEF UNIX}
var
   c, d:LongInt;
   pname:AnsiString;
begin
   Proc('(none)');
   for d:=0 to High(PortPrefix) do
      {$IFDEF SunOS}
      for c:=Ord('a') to ord('z') do begin
         pname:=PortPrefix[d]+Char(c);
      {$ELSE}
      for c:=0 to 255 do begin
         pname:=PortPrefix[d]+IntToStr(c);
      {$ENDIF}
         if FileExists('/dev/'+pname) then
            Proc(pname);
      end;
{$ENDIF}
{$IFDEF WINCE}
begin
  //ToDo
{$ENDIF}
end;

procedure TPortPropertyEditor.SetValue(const Value: AnsiString);
begin
   SetStrValue(Value);
   if GetComponent(0) is TSerialPortDriver then
      TSerialPortDriver(GetComponent(0)).Active := false;
end;

//editores de propriedades de BlinkWith
function  TElementIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TPLCBlockElement then
      Result := [paValueList{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

procedure TElementIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:LongInt;
begin
   if (GetComponent(0) is TPLCBlockElement) and (TPLCBlockElement(GetComponent(0)).PLCBlock <> nil) then
      for i := 0 to LongInt(TPLCBlockElement(GetComponent(0)).PLCBlock.Size)-1 do begin
          Proc(IntToStr(i));
      end;
end;


{function  TComponentNameEditorEx.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect];
end;}

///////////////////////////////////////
//editor base para os demais editores.
///////////////////////////////////////
procedure TInsertTagsOnFormComponentEditor.AddTagInEditor(Tag:TTag);
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

function  TInsertTagsOnFormComponentEditor.CreateComponent(tagclass:TComponentClass):TComponent;
begin
  {$IFDEF FPC}
    Result := tagclass.Create(GetTheOwner);
  {$ELSE}
    Result := Designer.CreateComponent(tagclass,GetTheOwner,0,0,0,0);
  {$ENDIF}
end;

function TInsertTagsOnFormComponentEditor.GetTheOwner:TComponent;
begin
  Result:=nil;
end;

///////////////////////////////////////
//editor TAG BUILDER
///////////////////////////////////////

function  TProtocolDriverComponentEditor.GetTheOwner: TComponent;
begin
  Result:=ProtocolDriver.Owner;
end;

procedure TProtocolDriverComponentEditor.OpenTagBuilder;
begin
  ProtocolDriver.OpenTagEditor(@AddTagInEditor, @CreateComponent);
end;

procedure TProtocolDriverComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenTagBuilder();
end;

function TProtocolDriverComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  if Index=0 then
    Result:='Tag Builder';
end;

function TProtocolDriverComponentEditor.GetVerbCount: LongInt;
begin
  if ProtocolDriver.HasTabBuilderEditor then
    Result:=1
  else
    Result:=0;
end;

procedure TProtocolDriverComponentEditor.Edit;
begin
  inherited Edit;
  OpenTagBuilder();
end;

function TProtocolDriverComponentEditor.ProtocolDriver: TProtocolDriver;
begin
  Result:=TProtocolDriver(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// BIT MAPPER
///////////////////////////////////////////////////////////////////////////////

function TTagBitMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=GetComponent().Owner;
end;

procedure TTagBitMapperComponentEditor.OpenBitMapper;
begin
  if (GetComponent is TPLCNumberMappable) then
    TPLCNumberMappable(GetComponent).OpenBitMapper(@AddTagInEditor, @CreateComponent);
end;

procedure TTagBitMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenBitMapper();
end;

function  TTagBitMapperComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  if Index=0 then
    Result:='Map bits';
end;

function  TTagBitMapperComponentEditor.GetVerbCount: LongInt;
begin
  Result:=1;
end;

procedure TTagBitMapperComponentEditor.Edit;
begin
  inherited Edit;
  OpenBitMapper();
end;

///////////////////////////////////////////////////////////////////////////////
// ELEMENT BLOCK MAPPER
///////////////////////////////////////////////////////////////////////////////

procedure TBlockElementMapperComponentEditor.OpenElementMapper;
begin
  if (GetComponent is TPLCBlock) then
    TPLCBlock(GetComponent).MapElements(@AddTagInEditor, @CreateComponent);
end;

function TBlockElementMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=GetComponent().Owner;
end;

procedure TBlockElementMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenElementMapper();
end;

{$if declared(has_customhints)}
function TBlockElementMapperComponentEditor.GetCustomHint: AnsiString;
begin
  if GetComponent is TPLCStruct then begin
    Result:=Result+'Structure size in bytes:'+IntToStr(TPLCStruct(GetComponent).Size);
    exit;
  end;

  if GetComponent is TPLCBlock then begin
    Result:=Result+'Number of elements: '+IntToStr(TPLCBlock(GetComponent).Size);
    exit;
  end;
end;
{$ifend}

function  TBlockElementMapperComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  Result:='Unknow option...';
  if Index=0 then begin
    if GetComponent is TPLCStruct then begin
      Result:='Map structure items...';
      exit;
    end;
    if GetComponent is TPLCBlock then begin
      Result:='Map block elements...';
      exit;
    end;
  end;
end;

function  TBlockElementMapperComponentEditor.GetVerbCount: LongInt;
begin
  Result:=0;
  if GetComponent is TPLCBlock then
   Result:=1;
end;

procedure TBlockElementMapperComponentEditor.Edit;
begin
  inherited Edit;
  OpenElementMapper();
end;

end.

