unit pscada_dbreg;

{$I ../../src/common/delphiver.inc}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses Classes;

procedure Register;

implementation

uses hsstrings, HMIDBConnection, psbufdataset,
  {$IFDEF FPC}
    LResources, lazlclversion, PropEdits, ComponentEditors;
  {$ELSE}
    Types,
    {$IFDEF DELPHI6_UP}
      //demais versoes do delphi
      //others versions of delphi.
      DesignIntf, DesignEditors;
    {$ELSE}
      //se for delphi 4 ou 5
      //if is delphi 5 or below.
      DsgnIntf;
    {$ENDIF}
  {$ENDIF}

procedure Register;
begin
  RegisterComponents(strDatabasePallete,  [THMIDBConnection]);
  RegisterComponents(strFPCPallete,       [TFPSBufDataSet]);

  RegisterPropertyEditor(TypeInfo(string), THMIDBConnection, 'Protocol', THMIDBProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIDBConnection, 'Database', THMIDBDatabasePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), THMIDBConnection, 'Catalog',  THMIDBCatalogPropertyEditor);
end;

end.

