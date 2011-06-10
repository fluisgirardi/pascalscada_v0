{$IFDEF PORTUGUES}
{:
  @abstract(Implementacao dos editores de algumas propriedades de controles
            do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the some properties editors of the controls of the
            PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit hmipropeditor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$I delphiver.inc}

interface

uses
  Classes, SysUtils, HMIZones, Dialogs, Menus, ProtocolDriver, Tag,
  typinfo, HMIControlDislocatorAnimation, PLCNumber, PLCBlock, PLCStruct,
  {$IFDEF FPC}
    PropEdits, ComponentEditors, lazlclversion, GraphPropEdits, ImgList;
  {$ELSE}
    Types,
    //if is a delphi 6+
    {$IF defined(DELPHI6_UP)}
      DesignIntf, DesignEditors;
    {$ELSE}
      //delphi 5-
      DsgnIntf;
    {$IFEND}
  {$ENDIF}

type
  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade TGraphicZone.FileName
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TGraphicZone.FileName property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TZoneFileNamePropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

  {$IFDEF FPC}
  {$if defined(lcl_fullversion) and (lcl_fullversion>=093000)}
  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade TGraphicZone.ImageIndex
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TGraphicZone.ImageIndex property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TGraphiZoneImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;
  {$IFEND}
  {$ENDIF}

  {$IFDEF PORTUGUES}
  {:
  Editor das seguintes propriedades de THMIControlDislocatorAnimation:
  @unorderedList(
    @item(THMIControlDislocatorAnimation.Gets_P0_Position)
    @item(THMIControlDislocatorAnimation.Gets_P1_Position)
    @item(THMIControlDislocatorAnimation.GoTo_P0_Position)
  )

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of the following properties of THMIControlDislocatorAnimation:
  @unorderedList(
    @item(THMIControlDislocatorAnimation.Gets_P0_Position)
    @item(THMIControlDislocatorAnimation.Gets_P1_Position)
    @item(THMIControlDislocatorAnimation.GoTo_P0_Position)
  )

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TPositionPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TZone.BlinkWith property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TZoneBlinkWithPropertyEditor = class(TIntegerProperty)
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
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
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
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function NumericTag: TPLCNumber; virtual;
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
    procedure ExecuteVerb(Index: Integer); override;
    {$if declared(has_customhints)}
    function GetCustomHint: String; override;
    {$ifend}
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

{$IFDEF FPC}
{$if defined(lcl_fullversion) and (lcl_fullversion>=093000)}
procedure TGraphiZoneImageIndexPropertyEditor.SetValue(const NewValue: ansistring);
var
  x:Integer;
begin
  try
    if NewValue='(none)' then
       inherited SetValue('-1')
    else begin
      x:=StrToInt(NewValue);
      inherited SetValue(NewValue);
    end;
  except
    inherited SetValue('-1');
  end;
end;

function TGraphiZoneImageIndexPropertyEditor.GetImageList: TCustomImageList;
begin
  if GetComponent(0) is TGraphicZone then
    Result:=TGraphicZone(GetComponent(0)).ImageList
  else
    Result:=nil;
end;

procedure TGraphiZoneImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  Proc('(none)');
end;
{$IFEND}
{$ENDIF}

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
  Result:=ProtocolDriver.Owner;
end;

procedure TTagBuilderComponentEditor.OpenTagBuilder;
begin
  ProtocolDriver.OpenTagEditor(ProtocolDriver, AddTagInEditor, CreateComponent);
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

function TTagBitMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=NumericTag.Owner;
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

function  TBlockElementMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=Block.Owner;
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

{$if declared(has_customhints)}
function TBlockElementMapperComponentEditor.GetCustomHint: String;
begin
  if Block is TPLCStruct then
    Result:=Result+'Structure size in bytes:'+IntToStr(TPLCStruct(Block).Size)
  else
    Result:=Result+'Block size:'+IntToStr(Block.Size);
end;
{$ifend}

function  TBlockElementMapperComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index=0 then begin
    if Block is TPLCStruct then
      Result:='Map structure items...'
    else
      Result:='Map block elements...';
  end;
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

