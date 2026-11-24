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

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Modified. Register new assistant components
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit scadareg;

interface

procedure Register;

implementation

uses
  Classes, Controls, SerialPort, ModBusSerial, LinearScaleProcessor, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, PLCNumber, UserScale, ValueProcessor,
  scadapropeditor, hsstrings, TagBit, ProtocolDriver, WestASCIIDriver,
  IBoxDriver, tcp_udpport, ModBusTCP, PLCStruct, PLCStructElement, ISOTCPDriver,
  mutexserver, MutexClient, siemenstagassistant, modbustagassistant, MelsecTCP,
  westasciitagassistant, bitmappertagassistant, blockstructtagassistant,
  rockwelltagassistant, s7plustagassistant, s7pluscertpropedit, numexprtag, plcstructstring, S7PlusFamily, LGXDriver,
  XinJETCP, DeltaTCP, InovanceTCP, H5UTagTCP, OPCUADriver, opcuatagassistant, CommPort,
  SysUtils, pscadalog, ObjInspStrConsts, opcuaserver, opcuaserveredt, MQTTDriver,
  {$IFDEF FPC}
    LResources, PropEdits, ComponentEditors, IDECommands, MenuIntf, LCLType;
  {$ELSE}
    {$IFDEF PORTUGUES}
      {$MESSAGE ERROR 'Somente Lazarus/Freepascal é suportado!'}
    {$ELSE}
      {$MESSAGE ERROR 'Only Lazarus/Freepascal are supported!'}
    {$ENDIF}
  {$ENDIF}
type
  {$IFDEF PORTUGUES}
  //: Lista em TpSCADALogger.Display so' os componentes que implementam IpSCADALogDisplay.
  {$ELSE}
  //: Lists in TpSCADALogger.Display only the components that implement IpSCADALogDisplay.
  {$ENDIF}
  TpSCADALogDisplayPropertyEditor = class(TComponentPropertyEditor)
  private
    FProc:TGetStrProc;
    procedure AddIfDisplay(const S:String);
  public
    procedure GetValues(Proc:TGetStrProc); override;
  end;

procedure TpSCADALogDisplayPropertyEditor.AddIfDisplay(const S:String);
var
  C:TPersistent;
begin
  if S=oisNone then begin
    FProc(S);
    exit;
  end;
  C := nil;
  if Assigned(PropertyHook) then
    C := PropertyHook.GetComponent(S);
  if (C<>nil) and Supports(C, IpSCADALogDisplay) then
    FProc(S);
end;

procedure TpSCADALogDisplayPropertyEditor.GetValues(Proc:TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(@AddIfDisplay);
end;

procedure Register;
var
  Cat: TIDECommandCategory;
  CmdMyTool: TIDECommand;
begin
  RegisterComponents(strPortsPallete,     [TSerialPortDriver,
                                           TTCP_UDPPort]);
  RegisterComponents(strProtocolsPallete, [TModBusRTUDriver,
                                           TModBusTCPDriver,
                                           TWestASCIIDriver,
                                           TIBoxDriver,
                                           TISOTCPDriver,
                                           TMelsecTCPDriver,
                                           TXinJETCPDriver,
                                           TDeltaTCPDriver,
                                           TInovanceTCPDriver,
                                           TH5UTagTCPDriver,
                                           TS7CommPlusDriver,
                                           TLGXDriver,
                                           TMQTTDriver,
                                           TOPCUADriver]);
  RegisterComponents(strUtilsPallete,     [TScalesQueue,
                                           TLinearScaleProcessor,
                                           TUserScale,
                                           TMutexServer,
                                           TMutexClient,
                                           TpSCADALogger,
                                           TOPCUAServer]);
  RegisterComponents(strTagsPallete,      [TPLCTagNumber,
                                           TPLCBlock,
                                           TPLCBlockElement,
                                           TPLCString,
                                           TTagBit,
                                           TPLCStruct,
                                           TPLCStructItem,
                                           TNumericExprTag,
                                           TPLCStructString]);

  RegisterPropertyEditor(TypeInfo(AnsiString), TSerialPortDriver, 'COMPort', TPortPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), TpSCADALogger, 'Display', TpSCADALogDisplayPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TS7CommPlusDriver, 'ServerCertificate', TS7PlusCertificatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TS7CommPlusDriver, 'ClientCertificate', TS7PlusClientCertificatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TOPCUADriver, 'CertificateFile', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TOPCUADriver, 'PrivateKeyFile', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TOPCUADriver, 'LibraryName', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TOPCUADriver, 'CertificateDir', TDirectoryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TOPCUADriver, 'TrustListDir', TDirectoryPropertyEditor);
  //the library opens the connection to EndpointUrl: no communication port to pick
  //(published by TProtocolDriver, a descendant cannot unpublish it - only hide it here)
  RegisterPropertyEditor(TypeInfo(TCommPortDriver), TOPCUADriver, 'CommunicationPort', THiddenPropertyEditor);
  {$IF defined(WIN32) or defined(WIN64)}
  RegisterPropertyEditor(TypeInfo(AnsiString), TSerialPortDriver, 'DevDir', THiddenPropertyEditor);
  {$IFEND}
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlockElement,  'Index'  , TElementIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStructItem,    'Index'  , TElementIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStructString,  'Index'  , TElementIndexPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemWriteFunction', TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemWriteFunction', TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'Size'            , TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemWriteFunction', TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'Size'            , TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemWriteFunction', TTagAddressPropertyEditor);

  //RegisterPropertyEditor(TypeInfo(Cardinal),   TWinControl,    'Width'          , TWinControlBoundsEditor);
  //RegisterPropertyEditor(TypeInfo(Cardinal),   TWinControl,    'Height'         , TWinControlBoundsEditor);
  //RegisterPropertyEditor(TypeInfo(Cardinal),   TWinControl,    'Left'           , TWinControlBoundsEditor);
  //RegisterPropertyEditor(TypeInfo(Cardinal),   TWinControl,    'Top'            , TWinControlBoundsEditor);
  //RegisterPropertyEditor(TypeInfo(Cardinal),   TComponent,     'Tag'            , TWinControlBoundsEditor);

  RegisterComponentEditor(TProtocolDriver,    TProtocolDriverComponentEditor);
  RegisterComponentEditor(TOPCUAServer,       TOPCUAServerComponentEditor);
  RegisterComponentEditor(TPLCNumberMappable, TTagBitMapperComponentEditor);
  RegisterComponentEditor(TPLCBlock,          TBlockElementMapperComponentEditor);

  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdMyTool := RegisterIDECommand(Cat,'Change component tag', 'Change the Tag property of a TComponent', VK_F2, [ssShift], nil, @ChangeComponentTag);
  RegisterIDEMenuCommand(itmSecondaryTools, 'EditTag', 'Change component tag', nil, nil, CmdMyTool);

end;

initialization

{$I pascalscada.lrs}

end.

