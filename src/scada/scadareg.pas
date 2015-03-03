{$i ../common/pscada_settings.inc}
{$I ../common/pscada_compiler_versions.inc}
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
  Classes, SerialPort, ModBusSerial, LinearScaleProcessor, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, PLCNumber, UserScale, ValueProcessor,
  scadapropeditor, pSCADA_Strings, TagBit, ProtocolDriver, WestASCIIDriver,
  IBoxDriver, tcp_udpport, ModBusTCP, PLCStruct, PLCStructElement, ISOTCPDriver,
  mutexserver, MutexClient, siemenstagassistant, modbustagassistant,
  westasciitagassistant, bitmappertagassistant, blockstructtagassistant,
  {$IFDEF FPC}
    LResources, pSCADA_Constants, PropEdits, ComponentEditors;
  {$ELSE}
    Types, 
    {$IFDEF DELPHI2009_UP}
      //demais versoes do delphi
      //others versions of delphi.
      DesignIntf, DesignEditors;
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
  RegisterComponents(strUtilsPallete,     [TMutexServer]);
  RegisterComponents(strUtilsPallete,     [TMutexClient]);
  RegisterComponents(strTagsPallete,      [TPLCTagNumber]);
  RegisterComponents(strTagsPallete,      [TPLCBlock]);
  RegisterComponents(strTagsPallete,      [TPLCBlockElement]);
  RegisterComponents(strTagsPallete,      [TPLCString]);
  RegisterComponents(strTagsPallete,      [TTagBit]);
  RegisterComponents(strTagsPallete,      [TPLCStruct]);
  RegisterComponents(strTagsPallete,      [TPLCStructItem]);

  RegisterPropertyEditor(TypeInfo(string), TSerialPortDriver,              'COMPort'  ,        TPortPropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt),TPLCBlockElement,               'Index'    ,        TElementIndexPropertyEditor);
  //end securitycode property editor.

  RegisterComponentEditor(TProtocolDriver,    TProtocolDriverComponentEditor);
  RegisterComponentEditor(TPLCNumberMappable, TTagBitMapperComponentEditor);
  RegisterComponentEditor(TPLCBlock,          TBlockElementMapperComponentEditor);
end;

{$IFDEF FPC}
initialization
  {$I pascalscada.lrs}
{$ENDIF}
end.

