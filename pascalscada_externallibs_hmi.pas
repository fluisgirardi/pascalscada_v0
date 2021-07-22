{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_externallibs_hmi;

{$warn 5023 off : no warning about unused units}
interface

uses
  sycreader_rfid_reader, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_externallibs_hmi', @Register);
end.
