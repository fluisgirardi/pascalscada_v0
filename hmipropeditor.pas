{:
  @abstract(Implementação dos editores de algumas propriedades de controles
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
  Classes, SysUtils, HMIZones, Dialogs,
  
  {$IFDEF FPC}
    PropEdits;
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

  //: Editor da propriedade TGraphicZone.FileName
  TTagEditorPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses ProtocolDriver;

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

//editores de propriedades de BlinkWith
function  TTagEditorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TProtocolDriver then
      Result := [paDialog{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TTagEditorPropertyEditor.GetValue: string;
begin
   Result := GetStrValue;
end;

procedure TTagEditorPropertyEditor.Edit;
begin
  if GetComponent(0) is TProtocolDriver then
    TProtocolDriver(GetComponent(0)).OpenTagEditor;
end;

procedure TTagEditorPropertyEditor.SetValue(const Value: string);
begin
   //SetStrValue(Value);
end;

end.

