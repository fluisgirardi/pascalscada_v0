{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_hmi;

interface

uses
  BasicUserManagement, ControlSecurityManager, crosskeyevents, 
  CustomizedUserManagement, hmi_draw_basic_horizontal_control, 
  hmi_draw_basiccontrol, hmi_draw_fita, hmi_draw_redler, hmi_draw_rosca, 
  hmi_polyline, HMIAnimation, hmibasiccolletion, hmibooleanpropertyconnector, 
  HMIButton, HMICheckBox, hmicolorpropertyconnector, HMIComboBox, 
  HMIControlDislocatorAnimation, HMIEdit, HMIKeyboardManager, HMILabel, 
  hmiobjectcolletion, HMIProgressBar, hmipropeditor, HMIRadioButton, 
  HMIRadioGroup, hmiregister, HMIScrollBar, HMIText, HMITrackBar, HMITypes, 
  HMIUpDown, HMIZones, ualfakeyboard, unumerickeyboard, usrmgnt_login, 
  HMI_ComputedImageList, HMI_Draw_Valves, hmi_draw_elevador, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('hmiregister', @hmiregister.Register);
end;

initialization
  RegisterPackage('pascalscada_hmi', @Register);
end.
