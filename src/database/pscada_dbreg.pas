{$i ../common/language.inc}
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
    {$IFDEF DELPHI2009_UP}
      //demais versoes do delphi
      //others versions of delphi.
      DesignIntf, DesignEditors;
    {$ELSE}
      {$IFDEF PORTUGUES}
        {$MESSAGE ERROR 'Somente versões posteriores ao Delphi 2009 são suportadas!'}
      {$ELSE}
        {$MESSAGE ERROR 'Only Delphi 2009 or later are supported!'}
      {$ENDIF}
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

