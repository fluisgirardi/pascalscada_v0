unit hmidbconnection_dsgn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Protocol
  {$ELSE}
  //: Property editor of THMIDBConnection.Protocol property.
  {$ENDIF}
  THMIDBProtocolPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses HMIDBConnection;

//##############################################################################
//EDITORES DE PROPRIEDADES DA CLASSE THMIDBCONNECTION
//PROPERTY EDITORS OF THE CLASS THMIDBCONNECTION
//##############################################################################


var
  SupportedDBDrivers:array[0..3] of string = ('postgresql','sqlite','mysql','firebird');
  SupportedZConnProt:array[0..25] of string = ('firebird',
                                               'firebird-1.0',
                                               'firebird-1.5',
                                               'firebird-2.0',
                                               'firebird-2.1',
                                               'firebird-2.5',
                                               'firebird-3.0',
                                               'firebirdd-1.0',
                                               'firebirdd-1.5',
                                               'firebirdd-2.0',
                                               'firebirdd-2.1',
                                               'firebirdd-2.5',
                                               'firebirdd-3.0',
                                               'mysql',
                                               'mysql-4.1',
                                               'mysql-5',
                                               'mysqld-4.1',
                                               'mysqld-5',
                                               'MariaDB-5',
                                               'MariaDB-10',
                                               'postgresql',
                                               'postgresql-7',
                                               'postgresql-8',
                                               'postgresql-9',
                                               'sqlite',
                                               'sqlite-3');

function THMIDBProtocolPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

//only accepted drivers are show.
procedure THMIDBProtocolPropertyEditor.GetValues(Proc: TGetStrProc);
var
  i, s:LongInt;
  found:Boolean;
begin
  for i:=0 to High(SupportedZConnProt) do
    Proc(SupportedZConnProt[i]);
end;

end.

