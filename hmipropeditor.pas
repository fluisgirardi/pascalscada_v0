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
  
  {$IFDEF FPC}
    PropEdits, ComponentEditors;
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
    function CreateComponent(tagclass:TComponentClass):TComponent;
    procedure OpenTagBuilder;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function ProtocolDriver: TProtocolDriver; virtual;
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

end.

