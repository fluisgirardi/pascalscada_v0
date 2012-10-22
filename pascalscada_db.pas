{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_db;

interface

uses
  dbstructurechecker, HMIDBConnection, psbufdataset, pscada_dbreg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pscada_dbreg', @pscada_dbreg.Register);
end;

initialization
  RegisterPackage('pascalscada_db', @Register);
end.
