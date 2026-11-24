{:
  @abstract(A console TOPCUAServer for tests/compat/opcua_asyncua_check.py.)

  Publishes a fixed set of tags (each kind the server maps: numbers, a bit, a
  string, a block, a structure, folders, writable and read only), with
  None + Sign/SignAndEncrypt endpoints, anonymous logins and the user op/123.
  OnCanWrite: the items of the Linha1 folder are written by logged users only.

  Usage: opcua_compat_server <port> <certificate dir> [seconds, default 120]
  Prints "READY <port>" once serving; stops after the given time.
}
program opcua_compat_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Tag, PLCTag, PLCTagNumber, PLCBlock, PLCString, TagBit,
  PLCStruct, PLCStructElement, PLCStructString, opcuaserver;

type
  TCompat = class(TComponent)
  public
    Server:TOPCUAServer;
    Counter:TPLCTagNumber;
    procedure CanWrite(Sender:TObject; Item:TOPCUAServerItem; const User:String; var Allowed:Boolean);
    function  Publish(ATag:TPLCTag; const AFolder:String; AWritable:Boolean):TOPCUAServerItem;
    procedure Build(APort:Word; const ACertDir:String);
  end;

procedure TCompat.CanWrite(Sender:TObject; Item:TOPCUAServerItem; const User:String; var Allowed:Boolean);
begin
  Allowed := (Item.Folder<>'Linha1') or (User<>'');
end;

function TCompat.Publish(ATag:TPLCTag; const AFolder:String; AWritable:Boolean):TOPCUAServerItem;
begin
  Result := Server.Items.Add;
  Result.Tag := ATag;
  Result.Folder := AFolder;
  Result.Writable := AWritable;
end;

procedure TCompat.Build(APort:Word; const ACertDir:String);
var
  n:TPLCTagNumber;
  b:TPLCBlock;
  s:TPLCString;
  bit:TTagBit;
  st:TPLCStruct;
  si:TPLCStructItem;
  ss:TPLCStructString;
  it:TOPCUAServerItem;
begin
  Server := TOPCUAServer.Create(Self);
  Server.Port := APort;
  Server.LibraryName := GetEnvironmentVariable('OPEN62541_LIB');
  Server.Security := ossNoneAndSecure;
  Server.CertificateDir := ACertDir;
  Server.Users.Add('op=123');
  Server.ApplicationUri := 'urn:pascalscada:compat';
  Server.ApplicationName := 'PascalSCADA compat';
  Server.OnCanWrite := @CanWrite;

  Counter := TPLCTagNumber.Create(Self);
  Counter.Name := 'Counter';
  Counter.TagType := pttLongInt;
  Publish(Counter, '', false).Description := 'Counts every 100 ms';

  n := TPLCTagNumber.Create(Self);
  n.Name := 'Setpoint';
  n.TagType := pttWord;
  n.Value := 100;
  Publish(n, '', true);

  bit := TTagBit.Create(Self);
  bit.Name := 'SetpointBit0';
  bit.PLCTag := n;
  bit.StartBit := 0;
  bit.EndBit := 0;
  Publish(bit, '', true);

  n := TPLCTagNumber.Create(Self);
  n.Name := 'Ratio';
  n.TagType := pttFloat;
  n.Value := 0.5;
  Publish(n, 'Linha1', true);

  n := TPLCTagNumber.Create(Self);
  n.Name := 'Temp';
  n.TagType := pttSmallInt;
  n.Value := -12;
  it := Publish(n, 'Linha1/Forno', false);
  it.NodeName := 'Temperatura';

  s := TPLCString.Create(Self);
  s.Name := 'Message';
  s.StringSize := 20;
  s.Value := 'hello';
  Publish(s, '', true);

  b := TPLCBlock.Create(Self);
  b.Name := 'Trend';
  b.TagType := pttSmallInt;
  b.Size := 5;
  b.ValuesRaw := [1, 2, 3, 4, 5];
  Publish(b, '', true);

  st := TPLCStruct.Create(Self);
  st.Name := 'Recipe';
  st.Size := 12;
  si := TPLCStructItem.Create(Self);
  si.Name := 'Speed';
  si.PLCBlock := st;
  si.TagType := pttWord;
  si.Index := 0;
  si.Value := 1234;
  si := TPLCStructItem.Create(Self);
  si.Name := 'Gain';
  si.PLCBlock := st;
  si.TagType := pttFloat;
  si.Index := 2;
  si.Value := 2.5;
  ss := TPLCStructString.Create(Self);
  ss.Name := 'Code';
  ss.PLCBlock := st;
  ss.StringSize := 4;
  ss.Index := 6;
  ss.Value := 'ab';
  Publish(st, '', true);

  Server.Active := true;
end;

var
  C:TCompat;
  port:Word;
  secs:Integer;
  t0, tick:QWord;
begin
  if ParamCount<2 then begin
    WriteLn('usage: opcua_compat_server <port> <certificate dir> [seconds]');
    Halt(2);
  end;
  port := StrToInt(ParamStr(1));
  secs := StrToIntDef(ParamStr(3), 120);
  C := TCompat.Create(nil);
  try
    C.Build(port, ParamStr(2));
    if not C.Server.Running then begin
      WriteLn('NOT RUNNING: ', C.Server.LastError);
      Halt(1);
    end;
    WriteLn('READY ', port);
    Flush(Output);
    t0 := GetTickCount64;
    tick := t0;
    while GetTickCount64-t0<QWord(secs)*1000 do begin
      //the writes of the clients are done in this thread
      CheckSynchronize(10);
      if GetTickCount64-tick>=100 then begin
        tick := GetTickCount64;
        C.Counter.Value := C.Counter.Value+1;
      end;
    end;
  finally
    C.Free;
  end;
end.
