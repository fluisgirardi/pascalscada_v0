//: @exclude
unit scadapropeditor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, SerialPort, PLCBlockElement,

  {$IF defined(WIN32) or defined(WIN64)}
  Windows,
  {$ELSE}
  Unix,
  {$IFEND}
  
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
  //: @exclude
  TPortPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  //: @exclude
  TElementIndexPropertyEditor = class(TIntegerProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

function  TPortPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TSerialPortDriver then
      Result := [paValueList{$IFDEF FPC}, paPickList{$ELSE}, paReadOnly, paValueEditable{$ENDIF}];
end;

function  TPortPropertyEditor.GetValue: string;
begin
   Result := GetStrValue;
end;

procedure TPortPropertyEditor.GetValues(Proc: TGetStrProc);
{$IF defined(WIN32) or defined(WIN64)}
var
  c:Integer;
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
{$ELSE}
var
   c, d:Integer;
   pname:String;
begin
   Proc('(none)');
   for d:=0 to High(PortPrefix) do
      for c:=0 to 255 do begin
         pname:=PortPrefix[d]+IntToStr(c);
         if FileExists('/dev/'+pname) then
            Proc(pname);
      end;
{$IFEND}
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
      Result := [paValueList{$IFNDEF FPC}, paReadOnly, paValueEditable{$ENDIF}];
end;

procedure TElementIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:Integer;
begin
   if (GetComponent(0) is TPLCBlockElement) and (TPLCBlockElement(GetComponent(0)).PLCBlock <> nil) then
      for i := 0 to Integer(TPLCBlockElement(GetComponent(0)).PLCBlock.Size)-1 do begin
          Proc(IntToStr(i));
      end;
end;


end.

