{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_hmi;

{$warn 5023 off : no warning about unused units}
interface

uses
  BasicUserManagement, crosskeyevents, ControlSecurityManager, 
  CustomizedUserManagement, hmi_draw_basic_horizontal_control, 
  hmi_draw_basiccontrol, hmi_draw_fita, hmi_draw_redler, hmi_draw_rosca, 
  hmi_polyline, HMIAnimation, hmibasiccolletion, hmibooleanpropertyconnector, 
  HMIButton, HMICheckBox, hmicolorpropertyconnector, HMIComboBox, 
  HMIControlDislocatorAnimation, HMIEdit, HMIKeyboardManager, HMILabel, 
  hmiobjectcolletion, HMIProgressBar, hmipropeditor, HMIRadioButton, 
  HMIRadioGroup, hmiregister, HMIScrollBar, HMIText, HMITrackBar, HMITypes, 
  HMIUpDown, HMIZones, ualfakeyboard, unumerickeyboard, usrmgnt_login, 
  HMI_Draw_Valves, hmi_draw_elevador, HMIBasicEletricMotor, hmi_flow_zones, 
  hmi_draw_flow_valve, hmi_draw_flow_pump, hmi_tachart_tag_source_list, 
  tagstatuslist, ChipCardReader, HMIFlowImage, hmifaceplatecontainer, 
  hmifaceplatedsgn, HMIBandeja, CentralUserManagement, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('hmiregister', @hmiregister.Register);
end;

initialization
  RegisterPackage('pascalscada_hmi', @Register);
end.
