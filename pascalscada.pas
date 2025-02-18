{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada;

{$warn 5023 off : no warning about unused units}
interface

uses
  MelsecDriver, MelsecTCP, CommPort, commtypes, crc16utils, IBoxDriver, 
  ISOTCPDriver, LinearScaleProcessor, ModBusDriver, ModBusSerial, ModBusTCP, 
  MutexClient, mutexserver, OPCDAProtocol, PLCBlock, PLCBlockElement, 
  PLCMemoryManager, PLCNumber, PLCString, PLCStruct, PLCStructElement, PLCTag, 
  PLCTagNumber, ProtocolDriver, ProtocolTypes, protscan, protscanupdate, 
  s7family, s7scanreq, S7Types, SerialPort, Tag, TagBit, TagBlock, 
  tagcollection, tcp_udpport, UserScale, ValueProcessor, WestASCIIDriver, 
  numexprtag, plcstructstring, socketserver, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada', @Register);
end.
