{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_hmi;

interface

uses
  BasicUserManagement, ControlSecurityManager, crosskeyevents, 
  CustomizedUserManagement, HMIAnimation, HMIButton, HMICheckBox, 
  HMIControlDislocatorAnimation, HMIEdit, HMILabel, HMIProgressBar, 
  hmipropeditor, HMIRadioButton, HMIRadioGroup, HMIScrollBar, HMIText, 
  HMITrackBar, HMITypes, HMIUpDown, HMIZones, ualfakeyboard, unumerickeyboard, 
  usrmgnt_login, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_hmi', @Register);
end.
