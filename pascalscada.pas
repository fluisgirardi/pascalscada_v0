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
  numexprtag, plcstructstring, socketserver, S7PlusTypes, S7PlusVLQ, 
  S7PlusCodec, S7PlusConnection, S7PlusFamily, S7PlusSSL, S7PlusTypeInfo, 
  LGXDriver, S7PlusHarpoAesCtr, S7PlusHarpoAuth, S7PlusHarpoAuthenticator, 
  S7PlusHarpoBigInt, S7PlusHarpoBigIntX, S7PlusHarpoBlobMeta, 
  S7PlusHarpoFamily0Data, S7PlusHarpoFamily0Transforms, 
  S7PlusHarpoFingerprint, S7PlusHarpoHash, S7PlusHarpoKeyDeriv, 
  S7PlusHarpoKeys, S7PlusHarpoLegacyAuth, S7PlusHarpoLutGen, 
  S7PlusHarpoMonolith1, S7PlusHarpoMonolith10, S7PlusHarpoMonolith11, 
  S7PlusHarpoMonolith2, S7PlusHarpoMonolith3, S7PlusHarpoMonolith4, 
  S7PlusHarpoMonolith5, S7PlusHarpoMonolith6, S7PlusHarpoMonolith7, 
  S7PlusHarpoMonolith8, S7PlusHarpoMonolith9, S7PlusHarpoMonolithWrappers, 
  S7PlusHarpoNinePart1, S7PlusHarpoNinePart10, S7PlusHarpoNinePart11, 
  S7PlusHarpoNinePart2, S7PlusHarpoNinePart3, S7PlusHarpoNinePart4, 
  S7PlusHarpoNinePart5, S7PlusHarpoNinePart6, S7PlusHarpoNinePart7, 
  S7PlusHarpoNinePart8, S7PlusHarpoNinePart9, S7PlusHarpoRandom, 
  S7PlusHarpoSeedTransform, S7PlusHarpoTenPart1, S7PlusHarpoTenPart2, 
  S7PlusHarpoTenPart3, S7PlusHarpoTransform12, S7PlusHarpoTransform7, 
  S7PlusHarpoUtil, S7PlusHarpoLegitimate, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada', @Register);
end.
