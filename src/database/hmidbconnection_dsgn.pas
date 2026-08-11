unit hmidbconnection_dsgn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Database.
  {$ELSE}
  //: Property editor of THMIDBConnection.Database property.
  {$ENDIF}
  THMIDBDatabasePropertyEditor = class(TStringProperty)
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Catalog
  {$ELSE}
  //: Property editor of THMIDBConnection.Catalog property.
  {$ENDIF}
  THMIDBCatalogPropertyEditor = class(TStringProperty)
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Protocol
  {$ELSE}
  //: Property editor of THMIDBConnection.Protocol property.
  {$ENDIF}
  THMIDBProtocolPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

const
  //protocolos suportados pelo backend SQLdb do THMIDBConnection.
  //protocols supported by THMIDBConnection's SQLdb backend.
  SupportedDBDrivers:array[0..3] of string = ('postgresql','sqlite','mysql','firebird');

function THMIDBProtocolPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList, paSortList];
end;

procedure THMIDBProtocolPropertyEditor.GetValues(Proc: TGetStrProc);
var
  i: LongInt;
begin
  for i:=0 to High(SupportedDBDrivers) do
    Proc(SupportedDBDrivers[i]);
end;

end.
