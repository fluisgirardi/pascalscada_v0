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
unit scadareg;

interface

procedure Register;

implementation

uses
  Classes, SerialPort, ModBusSerial, LinearScaleProcessor, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, UserScale, ValueProcessor,
  scadapropeditor, hsstrings, TagBit, ProtocolDriver,
  WestASCIIDriver, IBoxDriver, tcp_udpport, ModBusTCP, PLCStruct, PLCNumber,
  PLCStructElement, ISOTCPDriver,
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
  RegisterComponents(strPortsPallete,     [TSerialPortDriver]);
  RegisterComponents(strPortsPallete,     [TTCP_UDPPort]);
  RegisterComponents(strProtocolsPallete, [TModBusRTUDriver]);
  RegisterComponents(strProtocolsPallete, [TModBusTCPDriver]);
  RegisterComponents(strProtocolsPallete, [TWestASCIIDriver]);
  RegisterComponents(strProtocolsPallete, [TIBoxDriver]);
  RegisterComponents(strProtocolsPallete, [TISOTCPDriver]);
  RegisterComponents(strUtilsPallete,     [TScalesQueue]);
  RegisterComponents(strUtilsPallete,     [TLinearScaleProcessor]);
  RegisterComponents(strUtilsPallete,     [TUserScale]);
  RegisterComponents(strTagsPallete,      [TPLCTagNumber]);
  RegisterComponents(strTagsPallete,      [TPLCBlock]);
  RegisterComponents(strTagsPallete,      [TPLCBlockElement]);
  RegisterComponents(strTagsPallete,      [TPLCString]);
  RegisterComponents(strTagsPallete,      [TTagBit]);
  RegisterComponents(strTagsPallete,      [TPLCStruct]);
  RegisterComponents(strTagsPallete,      [TPLCStructItem]);

  RegisterPropertyEditor(TypeInfo(string), TSerialPortDriver,              'COMPort'  ,        TPortPropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TPLCBlockElement,               'Index'    ,        TElementIndexPropertyEditor);
  //end securitycode property editor.

  RegisterComponentEditor(TProtocolDriver, TTagBuilderComponentEditor);
  RegisterComponentEditor(TPLCNumber,      TTagBitMapperComponentEditor);
  RegisterComponentEditor(TPLCBlock,       TBlockElementMapperComponentEditor);

  {$IFDEF FPC}
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20501) }
  RegisterClassAlias(TScalesQueue, 'TPIPE');
  {$ELSE}
  RegisterClassAlias(TPIPE,        'TScalesQueue');
  {$IFEND}
  {$ELSE}
  RegisterClassAlias(TPIPE,        'TScalesQueue');
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$I pascalscada.lrs}
{$ENDIF}
end.

