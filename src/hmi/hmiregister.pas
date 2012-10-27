{$i ../common/language.inc}
{$I ../common/delphiver.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit de registro de componentes do PascalSCADA. Para Lazarus e Delphi.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of register of PascalSCADA components. For Lazarus and Delphi.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit hmiregister;

interface

procedure Register;

implementation

uses
  Classes, hsstrings, HMIEdit, HMILabel, HMICheckBox, HMIRadioButton,
  HMITrackBar, HMIProgressBar, HMIRadioGroup, HMIUpDown, HMIScrollBar,
  HMIAnimation, HMIText, HMIZones, hmipropeditor, HMIControlDislocatorAnimation,
  ControlSecurityManager, ActnList, CustomizedUserManagement,

  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}
  WinCCUserManagement,
  {$IFEND}

  {$IFDEF FPC}
    LResources, lazlclversion, PropEdits, ComponentEditors;
  {$ELSE}
    Types,
    {$IFDEF DELPHI6_UP}
      //demais versoes do delphi
      //others versions of delphi.
      DesignIntf, DesignEditors;
    {$ELSE}
      //se for delphi 4 ou 5
      //if is delphi 5 or below.
      DsgnIntf;
    {$ENDIF}
  {$ENDIF}

procedure Register;
begin
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
  RegisterComponents(strControlsPallete,  [THMIControlDislocatorAnimation]);
  //RegisterComponents(strControlsPallete,  [THMIButton]);

  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}
  RegisterComponents(strUserManagement,   [TWinCCUserManagement]);
  {$IFEND}
  RegisterComponents(strUserManagement,   [TCustomizedUserManagement]);

  RegisterPropertyEditor(TypeInfo(string), TGraphicZone,                   'FileName' ,        TZoneFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TZone,                          'BlinkWith',        TZoneBlinkWithPropertyEditor);

  {$IFDEF FPC}
  {$if declared(pslcl_fullversion) and (pslcl_fullversion>=093000)}
  RegisterPropertyEditor(TypeInfo(integer),TGraphicZone,                   'ImageIndex',                    TGraphiZoneImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TPascalSCADALogin_LogoutAction, 'WithUserLoggedInImageIndex',    TPascalSCADALoginLogoutImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TPascalSCADALogin_LogoutAction, 'WithoutUserLoggedInImageIndex', TPascalSCADALoginLogoutImageIndexPropertyEditor);
  {$IFEND}
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), THMIControlDislocatorAnimation, 'Gets_P0_Position', TPositionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIControlDislocatorAnimation, 'Gets_P1_Position', TPositionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIControlDislocatorAnimation, 'GoTo_P0_Position', TPositionPropertyEditor);

  //securitycode property editor.
  RegisterPropertyEditor(TypeInfo(string), THMIEdit, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMILabel, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMICheckBox, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIRadioButton, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMITrackBar, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIProgressBar, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIRadioGroup, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIUpDown, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIScrollBar, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIAnimation, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIText, 'SecurityCode', TSecurityCodePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TPascalSCADASecureAction, 'SecurityCode', TSecurityCodePropertyEditor);

  //////////////////////////////////////////////////////////////////////////////
  //Actions
  //////////////////////////////////////////////////////////////////////////////
  RegisterActions(strUserManagement,[TPascalSCADALoginAction,
                                     TPascalSCADALogoutAction,
                                     TPascalSCADALogin_LogoutAction,
                                     TPascalSCADAManageUsersAction,
                                     TPascalSCADASecureAction],nil);
end;

end.
