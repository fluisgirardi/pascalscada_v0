{
  Testes AO VIVO do driver OPC UA contra um servidor de verdade (PLCSim / S7-1500 / outro).
  LIVE tests of the OPC UA driver against a real server (PLCSim / S7-1500 / other).

  OPCUA_URL            endpoint, opc.tcp://host:4840. Sem ele os testes são ignorados / without it the tests are ignored
  OPCUA_USER, OPCUA_PASSWORD   login (nunca no repositório / never in the repository)
  OPCUA_SECURITY       none (padrão/default), sign, encrypt
  OPCUA_TRUSTDIR       pasta com o certificado do servidor (vazio aceita qualquer) / folder with the server certificate (empty accepts any)
  OPCUA_WRITE_NODE     um nó numérico gravável, para o teste de escrita / a writable numeric node, for the write test
  OPCUA_CHANGING_NODE  um nó numérico que muda sozinho (contador) / a numeric node that changes by itself (counter)
  OPEN62541_LIB        caminho da libopen62541 / path of libopen62541
  OPCUA_DEBUG=1        mostra as mensagens do driver / shows the driver messages
}
unit lt.opcua;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math, Tag, ProtocolTypes, PLCTagNumber,
  OPCUAClient, OPCUADriver;

type

  { TTestLiveOPCUA }

  TTestLiveOPCUA = class(TTestCase)
  private
    FDriver:TOPCUADriver;
    FTags:TList;
    procedure DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
    procedure Pump(Ms:Integer);
    function NewTag(const Address:String; Refresh:Integer):TPLCTagNumber;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ConnectsAndListsVariables;
    procedure ReadsTheListedNumericScalars;
    procedure WritesAndRestoresANode;
    procedure FollowsAChangingNode;
  end;

implementation

{ TTestLiveOPCUA }

procedure TTestLiveOPCUA.DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
begin
  if GetEnvironmentVariable('OPCUA_DEBUG')='1' then
    WriteLn(StdErr, '[opcua] ', Msg);
end;

procedure TTestLiveOPCUA.Pump(Ms:Integer);
var
  t:QWord;
begin
  t := GetTickCount64;
  while GetTickCount64-t<QWord(Ms) do begin
    Sleep(10);
    CheckSynchronize;
  end;
end;

function TTestLiveOPCUA.NewTag(const Address:String; Refresh:Integer):TPLCTagNumber;
begin
  Result := TPLCTagNumber.Create(nil);
  FTags.Add(Result);
  Result.RefreshTime := Refresh;
  Result.LongAddress := Address;
  Result.ProtocolDriver := FDriver;
end;

procedure TTestLiveOPCUA.SetUp;
var
  sec:String;
begin
  inherited SetUp;
  FTags := TList.Create;
  if GetEnvironmentVariable('OPCUA_URL')='' then
    Ignore('No OPCUA_URL: no OPC UA server to test against.');
  FDriver := TOPCUADriver.Create(nil);
  FDriver.EndpointUrl := GetEnvironmentVariable('OPCUA_URL');
  FDriver.LibraryName := GetEnvironmentVariable('OPEN62541_LIB');
  FDriver.Username := GetEnvironmentVariable('OPCUA_USER');
  FDriver.Password := GetEnvironmentVariable('OPCUA_PASSWORD');
  FDriver.TrustListDir := GetEnvironmentVariable('OPCUA_TRUSTDIR');
  sec := LowerCase(GetEnvironmentVariable('OPCUA_SECURITY'));
  if sec='sign' then
    FDriver.SecurityMode := osmSign
  else if sec='encrypt' then
    FDriver.SecurityMode := osmSignAndEncrypt;
  FDriver.LogLevel := ollInfo;
  FDriver.OnLog := @DriverLog;
end;

procedure TTestLiveOPCUA.TearDown;
var
  i:Integer;
begin
  if FTags<>nil then
    for i:=FTags.Count-1 downto 0 do
      TObject(FTags[i]).Free;
  FreeAndNil(FTags);
  FreeAndNil(FDriver);
  inherited TearDown;
end;

procedure TTestLiveOPCUA.ConnectsAndListsVariables;
var
  tags:TPLCTagInfoArray;
begin
  AssertTrue('ListTags: '+FDriver.LastError, FDriver.ListTags(tags)=ioOk);
  AssertTrue('some variables', Length(tags)>0);
end;

procedure TTestLiveOPCUA.ReadsTheListedNumericScalars;
var
  tags:TPLCTagInfoArray;
  i, n:Integer;
  t:TPLCTagNumber;
  bad:String;
begin
  AssertTrue('ListTags: '+FDriver.LastError, FDriver.ListTags(tags)=ioOk);
  n := 0;
  for i:=0 to High(tags) do
    if (tags[i].Kind=tkScalar) and (n<50) then begin
      NewTag(tags[i].Path, 500);
      Inc(n);
    end;
  if n=0 then Ignore('The server lists no numeric scalar.');
  Pump(3000);
  bad := '';
  for i:=0 to FTags.Count-1 do begin
    t := TPLCTagNumber(FTags[i]);
    //a node the user may not read is a valid answer, a communication error is not
    if not (t.LastASyncReadStatus in [ioOk, ioObjectAccessNotAllowed]) then
      bad := bad+t.LongAddress+': '+t.LastASyncReadStatus.ToString+'; ';
  end;
  AssertEquals(IntToStr(n)+' scalars read', '', bad);
end;

procedure TTestLiveOPCUA.WritesAndRestoresANode;
var
  node:String;
  t:TPLCTagNumber;
  orig:Double;
begin
  node := GetEnvironmentVariable('OPCUA_WRITE_NODE');
  if node='' then Ignore('No OPCUA_WRITE_NODE.');
  t := NewTag(node, 200);
  Pump(1500);
  AssertTrue('read: '+t.LastASyncReadStatus.ToString, t.LastASyncReadStatus=ioOk);
  orig := t.Value;
  t.SyncWrites := true;
  t.Value := orig+1;
  AssertTrue('write: '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioOk);
  Pump(1500);
  AssertEquals('written', orig+1, t.Value, 1e-6);
  t.Value := orig;
  AssertTrue('restored: '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioOk);
end;

procedure TTestLiveOPCUA.FollowsAChangingNode;
var
  node:String;
  t:TPLCTagNumber;
  last:Double;
  changes:Integer;
  t0:QWord;
begin
  node := GetEnvironmentVariable('OPCUA_CHANGING_NODE');
  if node='' then Ignore('No OPCUA_CHANGING_NODE.');
  t := NewTag(node, 100);
  Pump(1500);
  AssertTrue('read: '+t.LastASyncReadStatus.ToString, t.LastASyncReadStatus=ioOk);
  last := t.Value;
  changes := 0;
  t0 := GetTickCount64;
  while GetTickCount64-t0<3000 do begin
    CheckSynchronize;
    if t.Value<>last then begin
      last := t.Value;
      Inc(changes);
    end;
    Sleep(10);
  end;
  AssertTrue('changes seen in 3 s: '+IntToStr(changes), changes>=3);
end;

initialization
  RegisterTest(TTestLiveOPCUA);
end.
