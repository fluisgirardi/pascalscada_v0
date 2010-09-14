{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit scada; 

interface

uses
    CommPort, commtypes, crc16utils, CrossEvent, HMIAnimation, HMIButton, 
  HMICheckBox, HMIControlDislocatorAnimation, HMIEdit, HMILabel, 
  HMIProgressBar, hmipropeditor, HMIRadioButton, HMIRadioGroup, HMIScrollBar, 
  HMIText, HMITrackBar, HMITypes, HMIUpDown, HMIZones, hsstrings, hsutils, 
  IBoxDriver, ISOTCPDriver, LinearScaleProcessor, MessageSpool, ModBusDriver, 
  ModBusSerial, ModBusTCP, PLCBlock, PLCBlockElement, PLCMemoryManager, 
  PLCNumber, PLCString, PLCStruct, PLCStructElement, PLCTag, PLCTagNumber, 
  ProtocolDriver, ProtocolTypes, protscan, protscanupdate, s7family, S7Types, 
  scadapropeditor, scadareg, SerialPort, Tag, TagBit, TagBlock, tagcollection, 
  tcp_udpport, ubitmapper, uelementmapper, uModbusTagBuilder, UserScale, 
  uwesttagbuilder, ValueProcessor, WestASCIIDriver, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('scadareg', @scadareg.Register); 
end; 

initialization
  RegisterPackage('scada', @Register); 
end.
