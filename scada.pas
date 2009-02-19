{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit scada; 

interface

uses
    CommPort, commtypes, crc16utils, CrossEvent, HMIAnimation, HMIButton, 
  HMICheckBox, HMIEdit, HMILabel, HMIProgressBar, hmipropeditor, 
  HMIRadioButton, HMIRadioGroup, HMIScrollBar, HMIText, HMITrackBar, HMITypes, 
  HMIUpDown, HMIZones, hsstrings, hsutils, LinearScaleProcessor, MessageSpool, 
  ModBusMasterDriver, PLCBlock, PLCBlockElement, PLCMemoryMananger, PLCNumber, 
  PLCString, PLCTag, PLCTagNumber, ProtocolDriver, ProtocolTypes, 
  scadapropeditor, scadareg, SerialPort, Tag, TagBlock, UserScale, 
  ValueProcessor, TagBit, tagcollection, scadaapp, TCPIPClientPort, 
  protscanupdate, protscan, SharedSection, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('scadareg', @scadareg.Register);
end; 

initialization
  RegisterPackage('scada', @Register); 
end.
