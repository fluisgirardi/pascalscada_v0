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
  ControlSecurityManager, ActnList, CustomizedUserManagement, Controls,

  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}
  WinCCUserManagement,
  {$IFEND}

  {$IF defined(FPC) AND (not defined(DELPHI4_UP))}
  hmibooleanpropertyconnector, HMIComboBox, HMIKeyboardManager,
  hmi_draw_elevador, hmi_draw_fita, hmi_draw_redler, hmi_draw_rosca,
  hmi_polyline, hmicolorpropertyconnector, HMITransparentButton,
  HMI_Draw_Valves,
  {$IFEND}

  {$IFDEF FPC}
    LResources, lazlclversion, PropEdits, ComponentEditors;
  {$ELSE}
    {$IFDEF DELPHI2009_UP}
      //demais versoes do delphi
      //others versions of delphi.
      Types, DesignIntf, DesignEditors
      {$IFDEF DELPHI_XE5_UP}
      , System.Actions
      {$ENDIF};
    {$ELSE}
      {$IFDEF PORTUGUES}
        {$MESSAGE ERROR 'Somente versões posteriores ao Delphi 2009 são suportadas!'}
      {$ELSE}
        {$MESSAGE ERROR 'Only Delphi 2009 or later are supported!'}
      {$ENDIF}
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
  RegisterComponents(strControlsPallete,  [THMIBooleanPropertyConnector]);
  RegisterComponents(strControlsPallete,  [THMIColorPropertyConnector]);
  RegisterComponents(strControlsPallete,  [THMITransparentButton]);
  {$IF defined(FPC) AND (not defined(DELPHI4_UP))}
  RegisterComponents(strControlsPallete,  [THMIComboBox]);
  RegisterComponents(strControlsPallete,  [THMIKeyboardManager]);
  RegisterComponents(strControlsPallete,  [THMIFitaBasica, THMIElevadorBasico,
                                           THMIRedlerBasico, THMIRoscaBasica,
                                           THMIPolyline, THMIBasicValve]);
  {$IFEND}
  //RegisterComponents(strControlsPallete,  [THMIButton]);

  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}
  RegisterComponents(strUserManagement,   [TWinCCUserManagement]);
  {$IFEND}
  RegisterComponents(strUserManagement,   [TCustomizedUserManagement]);

  RegisterPropertyEditor(TypeInfo(string), TGraphicZone,                   'FileName' ,        TZoneFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt),TZone,                          'BlinkWith',        TZoneBlinkWithPropertyEditor);

  {$IFDEF FPC}
  {$if declared(pslcl_fullversion) and (pslcl_fullversion>=093000)}
  RegisterPropertyEditor(TypeInfo(LongInt), TGraphicZone,                   'ImageIndex',                    TGraphiZoneImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt), TPascalSCADALogin_LogoutAction, 'WithUserLoggedInImageIndex',    TPascalSCADALoginLogoutImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt), TPascalSCADALogin_LogoutAction, 'WithoutUserLoggedInImageIndex', TPascalSCADALoginLogoutImageIndexPropertyEditor);
  {$IFEND}
  RegisterPropertyEditor(TypeInfo(TCaption),TTextZone,                      'Text',                          TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String)  ,TObjectWithBooleanPropetiesColletionItem,'TargetObjectProperty', TSelectOnlyBooleanPropPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String)  ,TObjectWithColorPropetiesColletionItem,  'TargetObjectProperty', TSelectOnlyTColorPropPropertyEditor);
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
  RegisterPropertyEditor(TypeInfo(string), TPascalSCADACheckSpecialTokenAction, 'SecurityCode', TSecurityCodePropertyEditor);

  //////////////////////////////////////////////////////////////////////////////
  //Actions
  //////////////////////////////////////////////////////////////////////////////
  RegisterActions(strUserManagement,[TPascalSCADACheckSpecialTokenAction,
                                     TPascalSCADALoginAction,
                                     TPascalSCADALogoutAction,
                                     TPascalSCADALogin_LogoutAction,
                                     TPascalSCADAManageUsersAction,
                                     TPascalSCADASecureAction],nil);
end;

end.

