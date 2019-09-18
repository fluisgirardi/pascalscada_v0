{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_dsng;

{$warn 5023 off : no warning about unused units}
interface

uses
  bitmappertagassistant, blockstructtagassistant, comptagedt, 
  modbustagassistant, scadapropeditor, scadareg, siemenstagassistant, 
  ubitmapper, uelementmapper, uModbusTagBuilder, us7tagbuilder, 
  ustructuremapper, uwesttagbuilder, westasciitagassistant, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('scadareg', @scadareg.Register);
end;

initialization
  RegisterPackage('pascalscada_dsng', @Register);
end.
