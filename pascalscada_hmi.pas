{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_hmi;

interface

uses
  hmialfakeyboard, hmibasiccolletion, hmibasicusermanagement, 
  hmibooleanpropertyconnector, hmicolorpropertyconnector, HMIComboBox, 
  hmicontrolsecuritymanager, hmicustomizedusermanagement, 
  hmi_draw_basiccontrol, hmi_draw_basic_horizontal_control, hmi_draw_elevador, 
  hmi_draw_fita, hmi_draw_redler, hmi_draw_rosca, HMIKeyboardManager, 
  hmiobjectcolletion, hmi_polyline, hmitextstrings, HMITransparentButton, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_hmi', @Register);
end.
