{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_common;

interface

uses
  pscada_constants, pSCADA_cpu_utils, pscada_crc16utils, pSCADA_crossdatetime, 
  pSCADA_CrossEvent, pSCADA_MessageQueue, pSCADA_strings, pSCADA_Types, 
  pSCADA_utils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_common', @Register);
end.
