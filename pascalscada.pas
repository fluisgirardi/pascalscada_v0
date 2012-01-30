{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada; 

interface

uses
    BasicUserManagement, CommPort, commtypes, ControlSecurityManager, 
  crc16utils, crossdatetime, CrossEvent, crosskeyevents, 
  CustomizedUserManagement, fpc_ps_memds, HMIAnimation, HMIButton, 
  HMICheckBox, HMIControlDislocatorAnimation, HMIDBConnection, HMIEdit, 
  HMILabel, HMIProgressBar, hmipropeditor, HMIRadioButton, HMIRadioGroup, 
  HMIScrollBar, HMIText, HMITrackBar, HMITypes, HMIUpDown, HMIZones, 
  hsstrings, hsutils, IBoxDriver, ISOTCPDriver, lazlclversion, 
  LinearScaleProcessor, MessageSpool, ModBusDriver, ModBusSerial, ModBusTCP, 
  PLCBlock, PLCBlockElement, PLCMemoryManager, PLCNumber, PLCString, 
  PLCStruct, PLCStructElement, PLCTag, PLCTagNumber, ProtocolDriver, 
  ProtocolTypes, protscan, protscanupdate, s7family, S7Types, scadapropeditor, 
  scadareg, SerialPort, Tag, TagBit, TagBlock, tagcollection, tcp_udpport, 
  ubitmapper, uelementmapper, uModbusTagBuilder, unumerickeyboard, 
  us7tagbuilder, UserScale, usrmgnt_login, ustructuremapper, uwesttagbuilder, 
  ValueProcessor, WestASCIIDriver, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('scadareg', @scadareg.Register); 
end; 

initialization
  RegisterPackage('pascalscada', @Register); 
end.
