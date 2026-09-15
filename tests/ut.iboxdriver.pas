{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TIBoxDriver: validacao do endereco e o quadro de
            leitura com soma de verificacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O driver so aceita um punhado de enderecos (os PIDs que o equipamento
  expoe), e recusa o resto antes mesmo de falar com a porta - por isso boa
  parte dos testes nao precisa de porta nenhuma. O que vai para o fio e' um
  quadro de quatro bytes terminado por uma soma de verificacao em complemento
  de dois, conferida tambem na resposta.
}
{$ELSE}
{:
  @abstract(TIBoxDriver tests: address validation and the read frame with its
            checksum.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The driver only accepts a handful of addresses (the PIDs the device
  exposes), refusing the rest before even talking to the port - which is why
  most of these tests need no port at all. What goes on the wire is a four
  byte frame ending in a two's complement checksum, also checked on the way
  back.
}
{$ENDIF}
unit ut.iboxdriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, iboxdriver, PLCTagNumber,
  testsupport.bytes, testsupport.protocol, testsupport.fakeport;

type

  { TIBoxProbe }

  TIBoxProbe = class(TIBoxDriver)
  protected
    //a varredura e' uma thread de verdade; com uma estacao na lista ela
    //sairia lendo pela mesma porta de mentira
    //the scan is a real thread; with a station on the list it would go
    //reading through the same fake port
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function  ReadIt(const aTag:TTagRec; out aValores:TArrayOfDouble):TProtocolIOResult;
    procedure RemoveTag(aTag:TTag);
    function  Cached(const aTag:TTagRec):TScanReadRec;
  end;

  { TTestIBoxZones }

  //os PIDs 200 a 202: o estado de cada zona de refrigeracao, num quadro de
  //tamanho variavel - so' vem o que a zona tem
  //PIDs 200 to 202: the state of each refrigeration zone, in a frame of
  //variable size - only what the zone has comes
  TTestIBoxZones = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TIBoxProbe;
    function  Zone1(aSub:LongInt):TTagRec;
    procedure TheUnitAnswers(const aHex:String);
    function  ReadSub(aSub:LongInt):Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AZoneRequestBuildsTheFrame;
    procedure TheZoneCountAndTheAlarmShareAByte;
    procedure WithAnAlarmTheManufacturerCodeFollows;
    procedure WithoutAnAlarmThereIsNoCode;
    procedure EveryProbePresentIsReadInTenths;
    procedure TheOperatingModeIsTheLastByte;
    procedure AnAbsentProbeIsMarkedAbsentAndReadsAsZero;
    procedure AReservedBitSetIsACommError;
    procedure ABadChecksumIsRefused;
    procedure ABadChecksumWithNoFieldsIsRefusedToo;
    procedure AnAnswerFromAnotherZoneIsRefused;
    procedure TheZoneReadLeavesThePortUnlocked;
    procedure TheZoneStateIsKeptForTheScan;
  end;

  { TTestIBoxDriver }

  TTestIBoxDriver = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TIBoxProbe;
    function  RequestFor(aEstacao, aEndereco:LongInt):TTagRec;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //validacao antes de falar com a porta / validation before touching the port
    procedure AStationOutOfRangeIsRefused;
    procedure AnUnsupportedAddressIsRefused;
    procedure ASubelementOutOfRangeIsRefused;
    procedure WithNoCommunicationPortItDoesNotTryToTalk;

    //quadro enviado / frame sent
    procedure AFuelLevelRequestBuildsTheFrame;
    procedure TheChecksumIsTheTwosComplement;

    //resposta / response
    procedure TheFuelLevelComesInHalfPercentSteps;
    procedure AnAnswerWithAWrongChecksumIsRefused;
    procedure AnAnswerFromAnotherStationIsRefused;

    //remocao de tag / tag removal
    procedure RemovingATagOfAnUnknownStationDoesNothing;
  end;

implementation

{ TIBoxProbe }

function TIBoxProbe.ReadIt(const aTag:TTagRec; out aValores:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoRead(aTag, aValores, true);
end;

procedure TIBoxProbe.RemoveTag(aTag:TTag);
begin
  DoDelTag(aTag);
end;

procedure TIBoxProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TIBoxProbe.Cached(const aTag:TTagRec):TScanReadRec;
begin
  Result.Values:=nil;
  Result.LastQueryResult:=ioNone;
  DoGetValue(aTag, Result);
end;

{ TTestIBoxZones }

procedure TTestIBoxZones.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;
  FDrv:=TIBoxProbe.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestIBoxZones.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

function TTestIBoxZones.Zone1(aSub:LongInt):TTagRec;
begin
  //PID 200 e' a zona 1; o subelemento escolhe o campo
  //PID 200 is zone 1; the subelement picks the field
  Result:=TagRecFor(1, 0, 0, 200, 1);
  Result.SubElement:=aSub;
end;

procedure TTestIBoxZones.TheUnitAnswers(const aHex:String);
var
  r:BYTES;
  soma:Cardinal;
  i:Integer;
begin
  //o checksum - complemento de dois da soma - vai calculado aqui
  //the checksum - two's complement of the sum - is computed here
  r:=BytesOf(aHex);
  soma:=0;
  for i:=0 to High(r) do soma:=soma+r[i];
  SetLength(r, Length(r)+1);
  r[High(r)]:=Byte((soma xor $FFFFFFFF)+1);
  FPorta.QueueResponse(r);
end;

function TTestIBoxZones.ReadSub(aSub:LongInt):Double;
var
  v:TArrayOfDouble;
begin
  AssertEquals('leu', Ord(ioOk), Ord(FDrv.ReadIt(Zone1(aSub), v)));
  Result:=v[0];
end;

procedure TTestIBoxZones.AZoneRequestBuildsTheFrame;
var
  v:TArrayOfDouble;
begin
  TheUnitAnswers('01 C8 10 02 02');

  FDrv.ReadIt(Zone1(0), v);

  //estacao, zero, PID 200 e o checksum
  //station, zero, PID 200 and the checksum
  AssertBytesEqual('pedido', BytesOf('01 00 C8 37'), FPorta.WrittenFrame(0));
end;

procedure TTestIBoxZones.TheZoneCountAndTheAlarmShareAByte;
begin
  //nibble alto: zonas ativas; nibble baixo: alarme
  //high nibble: active zones; low nibble: alarm
  TheUnitAnswers('01 C8 20 02 03');
  AssertEquals('duas zonas',   2, ReadSub(0), 0);
  TheUnitAnswers('01 C8 20 02 03');
  AssertEquals('sem alarme',   0, ReadSub(1), 0);
end;

procedure TTestIBoxZones.WithAnAlarmTheManufacturerCodeFollows;
begin
  //alarme 1: o byte seguinte e' o codigo do fabricante, e so' depois vem
  //o mapa de campos
  //alarm 1: the next byte is the manufacturer code, and only then the field
  //map comes
  TheUnitAnswers('01 C8 21 07 02 03');
  AssertEquals('alarme',  1, ReadSub(1), 0);
  TheUnitAnswers('01 C8 21 07 02 03');
  AssertEquals('codigo',  7, ReadSub(2), 0);
  TheUnitAnswers('01 C8 21 07 02 03');
  AssertEquals('e o modo veio depois do codigo', 3, ReadSub(16), 0);
end;

procedure TTestIBoxZones.WithoutAnAlarmThereIsNoCode;
begin
  TheUnitAnswers('01 C8 10 02 03');
  AssertEquals('codigo zero', 0, ReadSub(2), 0);
end;

procedure TTestIBoxZones.EveryProbePresentIsReadInTenths;
const
  //mapa FE: todos os campos; RA1 25.0, S1 20.5, SP 5.0, EC 3.5, RA2 24.0,
  //S2 19.5, modo 2
  //map FE: every field; RA1 25.0, S1 20.5, SP 5.0, EC 3.5, RA2 24.0,
  //S2 19.5, mode 2
  Quadro = '01 C8 10 FE 00 FA 00 CD 00 32 00 23 00 F0 00 C3 02';
begin
  TheUnitAnswers(Quadro); AssertEquals('retorno 1',   25.0, ReadSub(10), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('insuflamento 1', 20.5, ReadSub(11), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('setpoint',    5.0,  ReadSub(12), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('serpentina',  3.5,  ReadSub(13), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('retorno 2',   24.0, ReadSub(14), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('insuflamento 2', 19.5, ReadSub(15), 0.001);
  TheUnitAnswers(Quadro); AssertEquals('retorno 1 presente', 1, ReadSub(3), 0);
end;

procedure TTestIBoxZones.TheOperatingModeIsTheLastByte;
const
  Quadro = '01 C8 10 FE 00 FA 00 CD 00 32 00 23 00 F0 00 C3 02';
begin
  //um byte so', depois das temperaturas; e' o modo, nao a marca de presenca
  //a single byte, after the temperatures; it is the mode, not the presence
  //mark
  TheUnitAnswers(Quadro); AssertEquals('modo presente', 1, ReadSub(9),  0);
  TheUnitAnswers(Quadro); AssertEquals('modo',          2, ReadSub(16), 0);
end;

procedure TTestIBoxZones.AnAbsentProbeIsMarkedAbsentAndReadsAsZero;
begin
  //so' o modo no mapa: as temperaturas nao vieram e nao podem ser lixo
  //only the mode on the map: the temperatures did not come and cannot be
  //garbage
  TheUnitAnswers('01 C8 10 02 03'); AssertEquals('retorno 1 ausente', 0, ReadSub(3),  0);
  TheUnitAnswers('01 C8 10 02 03'); AssertEquals('retorno 1 zero',    0, ReadSub(10), 0);
  TheUnitAnswers('01 C8 10 02 03'); AssertEquals('setpoint zero',     0, ReadSub(12), 0);
end;

procedure TTestIBoxZones.AReservedBitSetIsACommError;
var
  v:TArrayOfDouble;
begin
  TheUnitAnswers('01 C8 10 03 03');

  AssertEquals(Ord(ioCommError), Ord(FDrv.ReadIt(Zone1(0), v)));
end;

procedure TTestIBoxZones.ABadChecksumIsRefused;
var
  v:TArrayOfDouble;
begin
  FPorta.QueueResponse(BytesOf('01 C8 10 02 03 00'));

  AssertEquals(Ord(ioCommError), Ord(FDrv.ReadIt(Zone1(0), v)));
end;

procedure TTestIBoxZones.ABadChecksumWithNoFieldsIsRefusedToo;
var
  v:TArrayOfDouble;
begin
  //mapa zero: o quadro acaba nos cinco bytes, e o quinto e' o checksum
  //zero map: the frame ends in five bytes, and the fifth is the checksum
  FPorta.QueueResponse(BytesOf('01 C8 10 00 00'));

  AssertEquals(Ord(ioCommError), Ord(FDrv.ReadIt(Zone1(0), v)));
end;

procedure TTestIBoxZones.AnAnswerFromAnotherZoneIsRefused;
var
  v:TArrayOfDouble;
begin
  TheUnitAnswers('01 C9 10 02 03');

  AssertEquals(Ord(ioCommError), Ord(FDrv.ReadIt(Zone1(0), v)));
end;

procedure TTestIBoxZones.TheZoneReadLeavesThePortUnlocked;
var
  v:TArrayOfDouble;
begin
  //a leitura da zona tranca a porta para ler o quadro em duas partes sem
  //ninguem no meio; no fim tem que destrancar, ou outro driver na mesma
  //porta espera para sempre
  //the zone read locks the port to read the frame in two parts with nobody
  //in between; at the end it has to unlock, or another driver on the same
  //port waits forever
  TheUnitAnswers('01 C8 10 02 03');
  FDrv.ReadIt(Zone1(0), v);

  AssertTrue('outro driver consegue a porta', FPorta.Lock(9999));
  FPorta.Unlock(9999);
end;

procedure TTestIBoxZones.TheZoneStateIsKeptForTheScan;
var
  tag:TPLCTagNumber;
  guardado:TScanReadRec;
begin
  tag:=TPLCTagNumber.Create(nil);
  try
    tag.PLCStation:=1;
    tag.MemAddress:=200;
    tag.MemSubElement:=12;
    tag.ProtocolDriver:=FDrv;
    TheUnitAnswers('01 C8 10 FE 00 FA 00 CD 00 32 00 23 00 F0 00 C3 02');
    ReadSub(12);

    guardado:=FDrv.Cached(Zone1(12));

    AssertEquals('o setpoint guardado', 5.0, guardado.Values[0], 0.001);
    AssertEquals('leitura ok', Ord(ioOk), Ord(guardado.LastQueryResult));
  finally
    tag.Free;
  end;
end;

{ TTestIBoxDriver }

procedure TTestIBoxDriver.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;

  FDrv:=TIBoxProbe.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestIBoxDriver.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

function TTestIBoxDriver.RequestFor(aEstacao, aEndereco:LongInt):TTagRec;
begin
  Result:=TagRecFor(aEstacao, 0, 0, aEndereco, 1);
end;

procedure TTestIBoxDriver.AStationOutOfRangeIsRefused;
var
  valores:TArrayOfDouble;
begin
  //endereco de estacao cabe num byte
  AssertEquals('station 300', Ord(ioIllegalStationAddress),
               Ord(FDrv.ReadIt(RequestFor(300, 96), valores)));
  AssertEquals('nothing was sent', 0, FPorta.WriteCount);
end;

procedure TTestIBoxDriver.AnUnsupportedAddressIsRefused;
var
  valores:TArrayOfDouble;
begin
  //so os PIDs conhecidos sao aceitos; o resto nem chega na porta
  AssertEquals('address 5', Ord(ioIllegalRegAddress),
               Ord(FDrv.ReadIt(RequestFor(1, 5), valores)));
  AssertEquals('nothing was sent', 0, FPorta.WriteCount);
end;

procedure TTestIBoxDriver.ASubelementOutOfRangeIsRefused;
var
  valores:TArrayOfDouble;
  pedido:TTagRec;
begin
  //nos registradores 200 a 202 o subelemento escolhe o campo, de 0 a 16
  pedido:=RequestFor(1, 200);
  pedido.SubElement:=20;

  AssertEquals('subelement 20', Ord(ioIllegalRegAddress),
               Ord(FDrv.ReadIt(pedido, valores)));
end;

procedure TTestIBoxDriver.WithNoCommunicationPortItDoesNotTryToTalk;
var
  valores:TArrayOfDouble;
  semPorta:TIBoxProbe;
begin
  semPorta:=TIBoxProbe.Create(nil);
  try
    AssertEquals('driver with no port', Ord(ioNullDriver),
                 Ord(semPorta.ReadIt(RequestFor(1, 96), valores)));
  finally
    semPorta.Free;
  end;
end;

procedure TTestIBoxDriver.AFuelLevelRequestBuildsTheFrame;
var
  valores:TArrayOfDouble;
begin
  //estacao, zero, o PID pedido e a soma de verificacao
  FPorta.QueueResponse(BytesOf('01 60 64 3B'));
  FDrv.ReadIt(RequestFor(1, 96), valores);

  AssertEquals('one write', 1, FPorta.WriteCount);
  AssertBytesEqual('request for PID 96', BytesOf('01 00 60 9F'), FPorta.WrittenFrame(0));
end;

procedure TTestIBoxDriver.TheChecksumIsTheTwosComplement;
var
  valores:TArrayOfDouble;
begin
  //mudar a estacao muda a soma na mesma medida: $9F para a estacao 1,
  //e $9E para a 2
  FPorta.QueueResponse(BytesOf('02 60 64 3A'));
  FDrv.ReadIt(RequestFor(2, 96), valores);
  AssertBytesEqual('station 2', BytesOf('02 00 60 9E'), FPorta.WrittenFrame(0));

  FPorta.Reset;
  FPorta.QueueResponse(BytesOf('01 A8 00 57'));
  FDrv.ReadIt(RequestFor(1, 168), valores);
  AssertBytesEqual('PID 168', BytesOf('01 00 A8 57'), FPorta.WrittenFrame(0));
end;

procedure TTestIBoxDriver.TheFuelLevelComesInHalfPercentSteps;
var
  valores:TArrayOfDouble;
begin
  //o byte de dado vale meio por cento por unidade: $64 = 100 = 50%
  FPorta.QueueResponse(BytesOf('01 60 64 3B'));

  AssertEquals('read accepted', Ord(ioOk), Ord(FDrv.ReadIt(RequestFor(1, 96), valores)));
  AssertEquals('level in percent', 50, valores[0], 0);
end;

procedure TTestIBoxDriver.AnAnswerWithAWrongChecksumIsRefused;
var
  valores:TArrayOfDouble;
begin
  //mesma resposta, com o ultimo byte trocado
  FPorta.QueueResponse(BytesOf('01 60 64 3C'));

  AssertEquals('invalid checksum', Ord(ioCommError),
               Ord(FDrv.ReadIt(RequestFor(1, 96), valores)));
end;

procedure TTestIBoxDriver.AnAnswerFromAnotherStationIsRefused;
var
  valores:TArrayOfDouble;
begin
  //perguntamos a estacao 1 e respondeu a 2, com soma valida
  FPorta.QueueResponse(BytesOf('02 60 64 3A'));

  AssertEquals('wrong station', Ord(ioCommError),
               Ord(FDrv.ReadIt(RequestFor(1, 96), valores)));
end;

procedure TTestIBoxDriver.RemovingATagOfAnUnknownStationDoesNothing;
var
  tag:TPLCTagNumber;
begin
  //nenhum tag foi cadastrado, entao a lista de estacoes esta' vazia. Remover
  //um tag daqui nao pode indexar essa lista
  tag:=TPLCTagNumber.Create(nil);
  try
    tag.PLCStation:=7;
    tag.MemAddress:=96;

    FDrv.RemoveTag(tag);
  finally
    tag.Free;
  end;
end;

initialization
  RegisterTest(TTestIBoxDriver);
  RegisterTest(TTestIBoxZones);

end.
