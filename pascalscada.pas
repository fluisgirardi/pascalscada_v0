{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada; 

interface

uses
  BasicUserManagement, CommPort, commtypes, crc16utils, CrossEvent, 
  crosskeyevents, HMIAnimation, HMIButton, HMICheckBox, 
  HMIControlDislocatorAnimation, HMIDBConnection, HMIEdit, HMILabel, 
  HMIProgressBar, hmipropeditor, HMIRadioButton, HMIRadioGroup, HMIScrollBar, 
  HMIText, HMITrackBar, HMITypes, HMIUpDown, HMIZones, hsstrings, hsutils, 
  IBoxDriver, ISOTCPDriver, lazlclversion, LinearScaleProcessor, MessageSpool, 
  ModBusDriver, ModBusSerial, ModBusTCP, PLCBlock, PLCBlockElement, 
  PLCMemoryManager, PLCNumber, PLCString, PLCStruct, PLCStructElement, PLCTag, 
  PLCTagNumber, ProtocolDriver, ProtocolTypes, protscan, protscanupdate, 
  s7family, S7Types, scadapropeditor, scadareg, SerialPort, Tag, TagBit, 
  TagBlock, tagcollection, tcp_udpport, ubitmapper, uelementmapper, 
  uModbusTagBuilder, unumerickeyboard, us7tagbuilder, UserScale, 
  ustructuremapper, uwesttagbuilder, ValueProcessor, WestASCIIDriver, 
  usrmgnt_login, ControlSecurityManager, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('scadareg', @scadareg.Register); 
end; 

initialization
  RegisterPackage('pascalscada', @Register); 
end.
