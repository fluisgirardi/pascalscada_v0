unit hmidbconnection_dsgn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZPropertyEditor;

type

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Database.
  {$ELSE}
  //: Property editor of THMIDBConnection.Database property.
  {$ENDIF}
  THMIDBDatabasePropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Catalog
  {$ELSE}
  //: Property editor of THMIDBConnection.Catalog property.
  {$ENDIF}
  THMIDBCatalogPropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Protocol
  {$ELSE}
  //: Property editor of THMIDBConnection.Protocol property.
  {$ENDIF}
  THMIDBProtocolPropertyEditor = class(TZProtocolPropertyEditor)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation

uses HMIDBConnection;

//##############################################################################
//EDITORES DE PROPRIEDADES DA CLASSE THMIDBCONNECTION
//PROPERTY EDITORS OF THE CLASS THMIDBCONNECTION
//##############################################################################

function THMIDBDatabasePropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(THMIDBConnection(Result) as IHMIDBConnection).GetSyncConnection;
end;

function THMIDBCatalogPropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(THMIDBConnection(Result) as IHMIDBConnection).GetSyncConnection;
end;

var
  SupportedDBDrivers:array[0..2] of string = ('postgresql','sqlite','mysql');

//only accepted drivers are show.
procedure THMIDBProtocolPropertyEditor.GetValueList(List: TStrings);
var
  i, s:LongInt;
  found:Boolean;
begin
  inherited GetValueList(List);
  for i:=List.Count-1 downto 0 do begin
    found:=false;
    for s:=0 to High(SupportedDBDrivers) do
      if pos(SupportedDBDrivers[s], List.Strings[i])<>0 then begin
        found:=true;
        break;
      end;
    if not found then
      List.Delete(i);
  end;
end;

end.

