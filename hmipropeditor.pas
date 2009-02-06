//: @exclude
unit hmipropeditor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, HMIZones, Dialogs,
  
  {$IFDEF FPC}
    PropEdits;
  {$ELSE}
    Types,
    //se for delphi 4 ou 5
    {$IF defined(VER130) or defined(VER120)}
      DsgnIntf;
    {$ELSE}
      //demais versoes do delphi
      DesignIntf, DesignEditors;
    {$IFEND}
  {$ENDIF}

type
  TZoneFileNamePropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

  TZoneBlinkWithPropertyEditor = class(TIntegerProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

function  TZoneFileNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TGraphicZone then
      Result := [paDialog, paReadOnly{$IFNDEF FPC}, paValueEditable{$ENDIF}];
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
      Result := [paValueList{$IFNDEF FPC}, paReadOnly, paValueEditable{$ENDIF}];
end;

procedure TZoneBlinkWithPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:Integer;
begin
   if (GetComponent(0) is TZone) and (TZone(GetComponent(0)).Collection is TZones) then
      for i := 0 to TZone(GetComponent(0)).Collection.Count-1 do begin
         if TZone(GetComponent(0)).Collection.Items[i]<>GetComponent(0) then
            Proc(IntToStr(i));
      end;
end;

end.

