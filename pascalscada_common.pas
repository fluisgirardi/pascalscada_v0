{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_common;

interface

uses
  crossdatetime, CrossEvent, hsstrings, hsutils, MessageSpool, 
  pascalScadaMTPCPU, pscada_common, pscommontypes, pSCADAVersion, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_common', @Register);
end.
