{ Loads an open62541 library with LoadOpen62541 (which checks UA_TYPES and the
  encryption support against this binding) and creates/deletes a client.

    fpc -Fu.. -Fi.. load_test.pas && ./load_test /path/to/libopen62541.so }
program load_test;
{$MODE OBJFPC}{$H+}
uses SysUtils, open62541;
var
  client: PUA_Client;
begin
  LoadOpen62541(ParamStr(1));
  client := UA_Client_new();
  if client = nil then
  begin
    writeln('UA_Client_new failed');
    Halt(1);
  end;
  UA_Client_delete(client);
  writeln('OK: ', Open62541LibraryName);
end.
