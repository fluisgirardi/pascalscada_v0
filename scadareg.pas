//: @exclude
unit scadareg;

interface

procedure Register;

implementation

uses
  Classes, SerialPort, ModBusMasterDriver, LinearScaleProcessor, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, UserScale, ValueProcessor,
  scadapropeditor, HMIEdit, HMILabel, HMICheckBox, HMIRadioButton, HMITrackBar,
   HMIProgressBar, HMIRadioGroup, HMIUpDown, HMIScrollBar, HMIAnimation,
   HMIText, HMIZones, hmipropeditor, hsstrings, HMIButton, TagBit,
   TCPIPClientPort, WestASCIIDriver,
  {$IFDEF FPC}
    LResources, PropEdits;
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
procedure Register;
begin
  RegisterComponents(strPortsPallete,     [TSerialPortDriver]);
  RegisterComponents(strPortsPallete,     [TTCPIPClientPort]);
  RegisterComponents(strProtocolsPallete, [TModBusMasterDriver]);
  RegisterComponents(strProtocolsPallete, [TWestASCIIDriver]);
  RegisterComponents(strUtilsPallete,     [TPIPE]);
  RegisterComponents(strUtilsPallete,     [TLinearScaleProcessor]);
  RegisterComponents(strUtilsPallete,     [TUserScale]);
  RegisterComponents(strTagsPallete,      [TPLCTagNumber]);
  RegisterComponents(strTagsPallete,      [TPLCBlock]);
  RegisterComponents(strTagsPallete,      [TPLCBlockElement]);
  RegisterComponents(strTagsPallete,      [TPLCString]);
  RegisterComponents(strTagsPallete,      [TTagBit]);

  RegisterComponents(strControlsPallete,  [THMIEdit]);
  RegisterComponents(strControlsPallete,  [THMILabel]);
  RegisterComponents(strControlsPallete,  [THMICheckBox]);
  RegisterComponents(strControlsPallete,  [THMIRadioButton]);
  RegisterComponents(strControlsPallete,  [THMITrackBar]);
  RegisterComponents(strControlsPallete,  [THMIProgressBar]);
  RegisterComponents(strControlsPallete,  [THMIRadioGroup]);
  RegisterComponents(strControlsPallete,  [THMIUpDown]);
  RegisterComponents(strControlsPallete,  [THMIScrollBar]);
  RegisterComponents(strControlsPallete,  [THMIAnimation]);
  RegisterComponents(strControlsPallete,  [THMIText]);
  RegisterComponents(strControlsPallete,  [THMIButton]);

  RegisterPropertyEditor(TypeInfo(string), TSerialPortDriver, 'COMPort', TPortPropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TPLCBlockElement,  'Index',   TElementIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TGraphicZone, 'FileName', TZoneFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TZone,        'BlinkWith',TZoneBlinkWithPropertyEditor);
end;

{$IFDEF FPC}
initialization
  {$I scada.lrs}
{$ENDIF}
end.

