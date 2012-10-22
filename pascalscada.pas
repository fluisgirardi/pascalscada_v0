{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada;

interface

uses
  CommPort, commtypes, crc16utils, IBoxDriver, ISOTCPDriver, 
  LinearScaleProcessor, ModBusDriver, ModBusSerial, ModBusTCP, OPCDAProtocol, 
  PLCBlock, PLCBlockElement, PLCMemoryManager, PLCNumber, PLCString, 
  PLCStruct, PLCStructElement, PLCTag, PLCTagNumber, ProtocolDriver, 
  ProtocolTypes, protscan, protscanupdate, s7family, S7Types, scadapropeditor, 
  scadareg, SerialPort, Tag, TagBit, TagBlock, tagcollection, tcp_udpport, 
  ubitmapper, uelementmapper, uModbusTagBuilder, us7tagbuilder, UserScale, 
  ustructuremapper, uwesttagbuilder, ValueProcessor, WestASCIIDriver, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('scadareg', @scadareg.Register);
end;

initialization
  RegisterPackage('pascalscada', @Register);
end.
