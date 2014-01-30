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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$I ../common/delphiver.inc}

interface

uses
  Classes, SysUtils, SerialPort, PLCBlockElement,
  PLCStruct, Tag, commontagassistant, bitmappertagassistant,
  blockstructtagassistant,

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
    function  GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
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
  TTagBuilderComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenTagBuilder;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function  GetVerb(Index: LongInt): string; override;
    function  GetVerbCount: LongInt; override;
    procedure Edit; override;
    function  TagAssistant: TCommonTagAssistant; virtual;
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
    function GetVerb(Index: LongInt): string; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
    function bitmappertagassistant: TBitMapperTagAssistant; virtual;
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
    function GetCustomHint: String; override;
    {$ifend}
    function GetVerb(Index: LongInt): string; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
    function BlockAssistant: TBlockStructTagAssistant; virtual;
  end;

implementation

function  TPortPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TSerialPortDriver then
      Result := [paValueList{$IFDEF FPC}, paPickList{$ELSE}
                 {$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TPortPropertyEditor.GetValue: string;
begin
   Result := GetStrValue;
end;

procedure TPortPropertyEditor.GetValues(Proc: TGetStrProc);
{$IF defined(WIN32) or defined(WIN64)}
var
  c:LongInt;
  dcbstring, comname:String;
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
   pname:String;
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

procedure TPortPropertyEditor.SetValue(const Value: string);
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

function  TTagBuilderComponentEditor.GetTheOwner: TComponent;
begin
  Result:=TagAssistant.Owner;
end;

procedure TTagBuilderComponentEditor.OpenTagBuilder;
begin
  TagAssistant.OpenTagEditor(TagAssistant, AddTagInEditor, CreateComponent);
end;

procedure TTagBuilderComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenTagBuilder();
end;

function TTagBuilderComponentEditor.GetVerb(Index: LongInt): string;
begin
  if Index=0 then
    Result:='Tag Builder';
end;

function TTagBuilderComponentEditor.GetVerbCount: LongInt;
begin
  Result:=1;
end;

procedure TTagBuilderComponentEditor.Edit;
begin
  inherited Edit;
  OpenTagBuilder();
end;

function TTagBuilderComponentEditor.TagAssistant: TCommonTagAssistant;
begin
  Result:=TCommonTagAssistant(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// BIT MAPPER
///////////////////////////////////////////////////////////////////////////////

function TTagBitMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=bitmappertagassistant.Owner;
end;

procedure TTagBitMapperComponentEditor.OpenBitMapper;
begin
  bitmappertagassistant.OpenBitMapper(bitmappertagassistant, AddTagInEditor, CreateComponent);
end;

procedure TTagBitMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenBitMapper();
end;

function  TTagBitMapperComponentEditor.GetVerb(Index: LongInt): string;
begin
  if Index=0 then
    Result:='Map bits...';
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

function  TTagBitMapperComponentEditor.BitMapperTagAssistant: TBitMapperTagAssistant;
begin
  Result:=TBitMapperTagAssistant(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// ELEMENT BLOCK MAPPER
///////////////////////////////////////////////////////////////////////////////

function  TBlockElementMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=BlockAssistant.Owner;
end;

procedure TBlockElementMapperComponentEditor.OpenElementMapper;
begin
  BlockAssistant.OpenElementMapper(BlockAssistant, AddTagInEditor, CreateComponent);
end;

procedure TBlockElementMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenElementMapper();
end;

{$if declared(has_customhints)}
function TBlockElementMapperComponentEditor.GetCustomHint: String;
begin
  if not Assigned(BlockAssistant.BlockStructTag) then
    begin
      Result:='Please, define block or structure';
      Exit;
    end;
  if BlockAssistant.BlockStructTag is TPLCStruct then
    Result:=Result+'Structure size in bytes:'+IntToStr(TPLCStruct(BlockAssistant.BlockStructTag).Size)
  else
    Result:=Result+'Block size:'+IntToStr(BlockAssistant.BlockStructTag.Size);
end;
{$ifend}

function  TBlockElementMapperComponentEditor.GetVerb(Index: LongInt): string;
begin
  Result:='Undefined Block';
  if Index=0 then
    begin
      if not Assigned(BlockAssistant.BlockStructTag) then
        Exit;
      if BlockAssistant.BlockStructTag is TPLCStruct then
       Result:='Map structure items...'
      else
       Result:='Map block elements...';
    end;
end;

function  TBlockElementMapperComponentEditor.GetVerbCount: LongInt;
begin
  Result:=1;
end;

procedure TBlockElementMapperComponentEditor.Edit;
begin
  inherited Edit;
  OpenElementMapper();
end;

function  TBlockElementMapperComponentEditor.BlockAssistant:TBlockStructTagAssistant;
begin
  Result:=TBlockStructTagAssistant(GetComponent);
end;


end.

