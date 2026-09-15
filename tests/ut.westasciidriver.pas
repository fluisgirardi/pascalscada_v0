{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TWestASCIIDriver: o frame de presenca dos controladores
            West, indo e voltando por uma porta falsa.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O protocolo e' em ASCII legivel: "L", o endereco em dois digitos, e os
  caracteres de comando. O que da' para exercitar de fora da unit e' o
  DeviceActive, que e' publico e faz o ciclo completo - monta o frame, escreve
  na porta, le a resposta e decide. O resto do protocolo esta em metodos
  privados, fora do alcance de qualquer teste.
}
{$ELSE}
{:
  @abstract(TWestASCIIDriver tests: the West controllers' presence frame, going
            out and back through a fake port.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The protocol is readable ASCII: "L", the two digit address, and the command
  characters. What can be exercised from outside the unit is DeviceActive,
  which is public and does the whole round trip - builds the frame, writes it
  to the port, reads the answer and decides. The rest of the protocol lives in
  private methods, out of reach of any test.
}
{$ENDIF}
unit ut.westasciidriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, PLCTagNumber, PLCString, westasciidriver,
  testsupport.bytes, testsupport.fakeport, testsupport.protocol;

type

  { TWestProbe }

  //DoRead, DoWrite e DoGetValue sao protegidos: quem os chama e' a base, de
  //dentro de Read/Write e da varredura. A sonda os expoe, e deixa a
  //varredura sem nada para fazer - ela e' uma thread de verdade e, com um
  //equipamento na lista, sairia lendo pela mesma porta de mentira.
  //DoRead, DoWrite and DoGetValue are protected: the base calls them from
  //Read/Write and from the scan. The probe exposes them, and leaves the scan
  //with nothing to do - it is a real thread and, with a device on the list,
  //it would go reading through the same fake port.
  TWestProbe = class(TWestASCIIDriver)
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function  ReadNow(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
    function  WriteNow(const aTag:TTagRec; const aValue:Double):TProtocolIOResult;
    function  Cached(const aTag:TTagRec):TScanReadRec;
  end;

  { TTestWestASCIIDriver }

  TTestWestASCIIDriver = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TWestASCIIDriver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //montagem do pedido / request building
    procedure APresenceRequestBuildsTheFrame;
    procedure TheAddressAlwaysGoesWithTwoDigits;

    //interpretacao da resposta / response parsing
    procedure APositiveAnswerConfirmsTheDevice;
    procedure AnAnswerInTheShortFormatIsAlsoAccepted;
    procedure AnAnswerFromAnotherAddressIsRefused;
    procedure AnAnswerFullOfGarbageIsRefused;
    procedure WithNoAnswerItBecomesATimeout;

    //sem porta / with no port
    procedure WithNoCommunicationPortItDoesNotTryToTalk;
  end;

  { TWestTag }

  //IsValidTag e' protegido no tag: e' o driver que a marca ao cadastrar
  //IsValidTag is protected on the tag: it is the driver that marks it on
  //registration
  TWestTag = class(TPLCTagNumber)
  public
    function Valid:Boolean;
  end;

  { TTestWestParameters }

  //a leitura e a escrita de um parametro, byte a byte pela porta de mentira
  //reading and writing a parameter, byte by byte through the fake port
  TTestWestParameters = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TWestProbe;
    FValues:TArrayOfDouble;
    function  PV(aStation:LongInt):TTagRec;
    function  SP(aStation:LongInt):TTagRec;
    procedure TheDeviceAnswers(const aDigits:String; aCode:Byte; aParam:Byte=$4D; aFlag:Byte=$41);
    function  ReadPV:Double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a leitura / the read
    procedure AParameterReadBuildsTheFrame;
    procedure AParameterAnswerBecomesTheValue;
    procedure TheDecimalCodeScalesTheValue;
    procedure TheNegativeCodesFlipTheSign;
    procedure AnAnswerOfQuestionMarksIsAnIllegalValue;
    procedure AnAnswerWithNIsAnIllegalFunction;
    procedure AnAnswerForAnotherParameterIsRefused;
    procedure WithNoAnswerTheReadTimesOut;
    procedure AStationOutOfRangeIsRefusedBeforeTalking;
    procedure AnAddressOutOfRangeIsRefusedBeforeTalking;

    //a escrita / the write
    procedure AParameterWriteBuildsTheFrameAndCommits;
    procedure TheValueGoesWithTheMostDecimalsThatFit;
    procedure ANegativeValueGoesWithTheNegativeCode;
    procedure AValueAtTheRoundingEdgeKeepsItsMagnitude;
    procedure AValueOfTenThousandIsIllegal;
    procedure AWriteUsesTheDecimalsTheDeviceReported;
    procedure AWrongEchoIsACommError;
    procedure AWriteRefusedByTheDeviceIsAnIllegalFunction;

    //os tags / the tags
    procedure AValidTagRegistersTheDevice;
    procedure ATagOutsideTheParameterTableIsInvalid;
    procedure ATagWithStationZeroIsInvalid;
    procedure OnlyNumericTagsAreAccepted;
    procedure TheLastReadValueIsKeptForTheScan;
  end;

implementation

{ TWestProbe }

procedure TWestProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TWestProbe.ReadNow(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoRead(aTag, aValues, true);
end;

function TWestProbe.WriteNow(const aTag:TTagRec; const aValue:Double):TProtocolIOResult;
var
  v:TArrayOfDouble;
begin
  SetLength(v,1);
  v[0]:=aValue;
  Result:=DoWrite(aTag, v, true);
end;

function TWestProbe.Cached(const aTag:TTagRec):TScanReadRec;
begin
  Result.Values:=nil;
  Result.LastQueryResult:=ioNone;
  DoGetValue(aTag, Result);
end;

{ TWestTag }

function TWestTag.Valid:Boolean;
begin
  Result:=IsValidTag;
end;

{ TTestWestParameters }

procedure TTestWestParameters.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;
  FDrv:=TWestProbe.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestWestParameters.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

function TTestWestParameters.PV(aStation:LongInt):TTagRec;
begin
  //endereco $01 da tabela: a variavel de processo, parametro "M"
  //table address $01: the process variable, parameter "M"
  Result:=TagRecFor(aStation, 0, 0, $01, 1);
end;

function TTestWestParameters.SP(aStation:LongInt):TTagRec;
begin
  //endereco $00: o setpoint, parametro "S", casas decimais automaticas
  //address $00: the setpoint, parameter "S", automatic decimals
  Result:=TagRecFor(aStation, 0, 0, $00, 1);
end;

procedure TTestWestParameters.TheDeviceAnswers(const aDigits:String; aCode:Byte; aParam:Byte; aFlag:Byte);
var
  r:BYTES;
  i:Integer;
begin
  //"L" 0 1 P d d d d c A "*" - 11 bytes
  SetLength(r, 11);
  r[0]:=$4C; r[1]:=$30; r[2]:=$31; r[3]:=aParam;
  for i:=1 to 4 do r[3+i]:=Ord(aDigits[i]);
  r[8]:=aCode; r[9]:=aFlag; r[10]:=$2A;
  FPorta.QueueResponse(r);
end;

function TTestWestParameters.ReadPV:Double;
var
  v:TArrayOfDouble;
begin
  AssertEquals('leu', Ord(ioOk), Ord(FDrv.ReadNow(PV(1), v)));
  AssertEquals('um valor', 1, Length(v));
  Result:=v[0];
end;

procedure TTestWestParameters.AParameterReadBuildsTheFrame;
var
  v:TArrayOfDouble;
begin
  TheDeviceAnswers('1234', $31);

  FDrv.ReadNow(PV(1), v);

  //"L" 01 "M" "?" "*"
  AssertBytesEqual('pedido do parametro', BytesOf('4C 30 31 4D 3F 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestParameters.AParameterAnswerBecomesTheValue;
begin
  //e' a leitura sincrona de um tag - tag.Read - que passa por aqui
  //it is a tag's synchronous read - tag.Read - that goes through here
  TheDeviceAnswers('1234', $31);

  AssertEquals(123.4, ReadPV, 0.0001);
end;

procedure TTestWestParameters.TheDecimalCodeScalesTheValue;
begin
  //o quinto caractere diz onde esta' a virgula: 0 a 3 casas
  //the fifth character says where the point is: 0 to 3 places
  TheDeviceAnswers('1234', $30); AssertEquals('0 casas', 1234,  ReadPV, 0.0001);
  TheDeviceAnswers('1234', $32); AssertEquals('2 casas', 12.34, ReadPV, 0.0001);
  TheDeviceAnswers('1234', $33); AssertEquals('3 casas', 1.234, ReadPV, 0.0001);
end;

procedure TTestWestParameters.TheNegativeCodesFlipTheSign;
begin
  TheDeviceAnswers('1234', $35); AssertEquals('-1234',  -1234,  ReadPV, 0.0001);
  TheDeviceAnswers('1234', $36); AssertEquals('-123.4', -123.4, ReadPV, 0.0001);
  TheDeviceAnswers('1234', $38); AssertEquals('-1.234', -1.234, ReadPV, 0.0001);
end;

procedure TTestWestParameters.AnAnswerOfQuestionMarksIsAnIllegalValue;
var
  v:TArrayOfDouble;
begin
  //"<??>" e' o que o controlador manda quando o parametro nao tem valor
  //"<??>" is what the controller sends when the parameter has no value
  TheDeviceAnswers('<??>', $30);

  AssertEquals(Ord(ioIllegalValue), Ord(FDrv.ReadNow(PV(1), v)));
end;

procedure TTestWestParameters.AnAnswerWithNIsAnIllegalFunction;
var
  v:TArrayOfDouble;
begin
  TheDeviceAnswers('0000', $30, $4D, $4E);

  AssertEquals(Ord(ioIllegalFunction), Ord(FDrv.ReadNow(PV(1), v)));
end;

procedure TTestWestParameters.AnAnswerForAnotherParameterIsRefused;
var
  v:TArrayOfDouble;
begin
  //perguntamos "M" e veio "S"
  //we asked for "M" and "S" came back
  TheDeviceAnswers('1234', $31, $53);

  AssertEquals(Ord(ioCommError), Ord(FDrv.ReadNow(PV(1), v)));
end;

procedure TTestWestParameters.WithNoAnswerTheReadTimesOut;
var
  v:TArrayOfDouble;
begin
  AssertEquals(Ord(ioTimeOut), Ord(FDrv.ReadNow(PV(1), v)));
end;

procedure TTestWestParameters.AStationOutOfRangeIsRefusedBeforeTalking;
var
  v:TArrayOfDouble;
begin
  AssertEquals('estacao 0',   Ord(ioIllegalStationAddress), Ord(FDrv.ReadNow(PV(0), v)));
  AssertEquals('estacao 100', Ord(ioIllegalStationAddress), Ord(FDrv.ReadNow(PV(100), v)));
  AssertEquals('nada saiu',   0, FPorta.WriteCount);
end;

procedure TTestWestParameters.AnAddressOutOfRangeIsRefusedBeforeTalking;
var
  v:TArrayOfDouble;
begin
  AssertEquals(Ord(ioIllegalRegAddress), Ord(FDrv.ReadNow(TagRecFor(1, 0, 0, $1C, 1), v)));
  AssertEquals('nada saiu', 0, FPorta.WriteCount);
end;

procedure TTestWestParameters.AParameterWriteBuildsTheFrameAndCommits;
begin
  //primeiro o valor, "L" 01 "S" "#" 1234 c "*", que o controlador ecoa com
  //"I"; depois o "L" 01 "S" "I" "*" que efetiva, respondido sem "N"
  //first the value, "L" 01 "S" "#" 1234 c "*", which the controller echoes
  //with "I"; then the "L" 01 "S" "I" "*" that commits, answered with no "N"
  TheDeviceAnswers('1234', $31, $53, $49);
  TheDeviceAnswers('1234', $31, $53, $49);

  AssertEquals('escreveu', Ord(ioOk), Ord(FDrv.WriteNow(SP(1), 123.4)));

  AssertEquals('duas idas a porta', 2, FPorta.WriteCount);
  AssertBytesEqual('o valor',   BytesOf('4C 30 31 53 23 31 32 33 34 31 2A'), FPorta.WrittenFrame(0));
  AssertBytesEqual('o efetiva', BytesOf('4C 30 31 53 49 2A'),                FPorta.WrittenFrame(1));
end;

procedure TTestWestParameters.TheValueGoesWithTheMostDecimalsThatFit;
begin
  //quatro digitos: 5 vai como 5.000, 12.5 como 12.50, 1234 como 1234
  //four digits: 5 goes as 5.000, 12.5 as 12.50, 1234 as 1234
  FDrv.WriteNow(SP(1), 5);
  AssertBytesEqual('5.000', BytesOf('4C 30 31 53 23 35 30 30 30 33 2A'), FPorta.WrittenFrame(0));
  FPorta.Reset;
  FDrv.WriteNow(SP(1), 12.5);
  AssertBytesEqual('12.50', BytesOf('4C 30 31 53 23 31 32 35 30 32 2A'), FPorta.WrittenFrame(0));
  FPorta.Reset;
  FDrv.WriteNow(SP(1), 1234);
  AssertBytesEqual('1234',  BytesOf('4C 30 31 53 23 31 32 33 34 30 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestParameters.ANegativeValueGoesWithTheNegativeCode;
begin
  FDrv.WriteNow(SP(1), -7.25);
  AssertBytesEqual('-7.250', BytesOf('4C 30 31 53 23 37 32 35 30 38 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestParameters.AValueAtTheRoundingEdgeKeepsItsMagnitude;
begin
  //999.96 com uma casa arredonda para 1000.0, que nao cabe em quatro
  //digitos com uma casa: tem que ir como 1000 sem casas - nunca como "1000"
  //com uma casa, que o controlador leria como 100.0
  //999.96 with one place rounds to 1000.0, which does not fit four digits
  //with one place: it has to go as 1000 with no places - never as "1000"
  //with one place, which the controller would read as 100.0
  FDrv.WriteNow(SP(1), 999.96);
  AssertBytesEqual('1000', BytesOf('4C 30 31 53 23 31 30 30 30 30 2A'), FPorta.WrittenFrame(0));
  FPorta.Reset;
  FDrv.WriteNow(SP(1), 9.9996);
  AssertBytesEqual('10.00', BytesOf('4C 30 31 53 23 31 30 30 30 32 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestParameters.AValueOfTenThousandIsIllegal;
begin
  AssertEquals(Ord(ioIllegalValue), Ord(FDrv.WriteNow(SP(1), 10000)));
  AssertEquals('nada saiu', 0, FPorta.WriteCount);
end;

procedure TTestWestParameters.AWriteUsesTheDecimalsTheDeviceReported;
var
  tag:TWestTag;
begin
  //com o equipamento na lista, a leitura guarda quantas casas ele usa; a
  //escrita seguinte vai com essas casas, nao com o maximo que caberia
  //with the device on the list, the read keeps how many places it uses; the
  //next write goes with those places, not with the most that would fit
  tag:=TWestTag.Create(nil);
  try
    tag.PLCStation:=1;
    tag.MemAddress:=$00;
    tag.ProtocolDriver:=FDrv;
    TheDeviceAnswers('1234', $31, $53);
    FDrv.WriteNow(SP(1), 0); //so' para nao haver leitura pendente / just so no read is pending
    FPorta.Reset;
    TheDeviceAnswers('1234', $31, $53);
    FDrv.ReadNow(SP(1), FValues);
    FPorta.Reset;

    FDrv.WriteNow(SP(1), 12.34);

    AssertBytesEqual('12.3, uma casa', BytesOf('4C 30 31 53 23 30 31 32 33 31 2A'), FPorta.WrittenFrame(0));
  finally
    tag.Free;
  end;
end;

procedure TTestWestParameters.AWrongEchoIsACommError;
begin
  TheDeviceAnswers('1235', $31, $53, $49);

  AssertEquals(Ord(ioCommError), Ord(FDrv.WriteNow(SP(1), 123.4)));
end;

procedure TTestWestParameters.AWriteRefusedByTheDeviceIsAnIllegalFunction;
begin
  TheDeviceAnswers('1234', $31, $53, $49);
  TheDeviceAnswers('1234', $31, $53, $4E);

  AssertEquals(Ord(ioIllegalFunction), Ord(FDrv.WriteNow(SP(1), 123.4)));
end;

procedure TTestWestParameters.AValidTagRegistersTheDevice;
var
  tag:TWestTag;
begin
  tag:=TWestTag.Create(nil);
  try
    tag.PLCStation:=1;
    tag.MemAddress:=$01;

    tag.ProtocolDriver:=FDrv;

    AssertTrue('tag valido', tag.Valid);
  finally
    tag.Free;
  end;
end;

procedure TTestWestParameters.ATagOutsideTheParameterTableIsInvalid;
var
  tag:TWestTag;
begin
  tag:=TWestTag.Create(nil);
  try
    tag.PLCStation:=1;
    tag.MemAddress:=$1C;

    tag.ProtocolDriver:=FDrv;

    AssertFalse('tag invalido', tag.Valid);
  finally
    tag.Free;
  end;
end;

procedure TTestWestParameters.ATagWithStationZeroIsInvalid;
var
  tag:TWestTag;
begin
  tag:=TWestTag.Create(nil);
  try
    tag.PLCStation:=0;
    tag.MemAddress:=$01;

    tag.ProtocolDriver:=FDrv;

    AssertFalse('tag invalido', tag.Valid);
  finally
    tag.Free;
  end;
end;

procedure TTestWestParameters.OnlyNumericTagsAreAccepted;
var
  texto:TPLCString;
begin
  texto:=TPLCString.Create(nil);
  try
    try
      texto.ProtocolDriver:=FDrv;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;
  finally
    texto.Free;
  end;
end;

procedure TTestWestParameters.TheLastReadValueIsKeptForTheScan;
var
  tag:TWestTag;
  guardado:TScanReadRec;
begin
  //o equipamento entra na lista com o tag; o que a leitura trouxe fica
  //guardado e e' o que a varredura entrega
  //the device joins the list with the tag; what the read brought stays
  //kept and is what the scan delivers
  tag:=TWestTag.Create(nil);
  try
    tag.PLCStation:=1;
    tag.MemAddress:=$01;
    tag.ProtocolDriver:=FDrv;
    TheDeviceAnswers('1234', $31);
    ReadPV;

    guardado:=FDrv.Cached(PV(1));

    AssertEquals('um valor',     1,     Length(guardado.Values));
    AssertEquals('o que foi lido', 123.4, guardado.Values[0], 0.0001);
    AssertEquals('leitura ok',   Ord(ioOk), Ord(guardado.LastQueryResult));
  finally
    tag.Free;
  end;
end;

procedure TTestWestASCIIDriver.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;

  FDrv:=TWestASCIIDriver.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestWestASCIIDriver.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

procedure TTestWestASCIIDriver.APresenceRequestBuildsTheFrame;
begin
  //"L" (4C), endereco 01, "??" e "*" de fim
  FPorta.QueueResponse(BytesOf('4C 30 31 3F 41 2A'));
  FDrv.DeviceActive(1);

  AssertEquals('one write to the port', 1, FPorta.WriteCount);
  AssertBytesEqual('presence frame',
                   BytesOf('4C 30 31 3F 3F 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestASCIIDriver.TheAddressAlwaysGoesWithTwoDigits;
begin
  //42 vira "42"; enderecos de um digito levam zero a esquerda
  FPorta.QueueResponse(BytesOf('4C 34 32 3F 41 2A'));
  FDrv.DeviceActive(42);
  AssertBytesEqual('address 42', BytesOf('4C 34 32 3F 3F 2A'), FPorta.WrittenFrame(0));

  FPorta.Reset;
  FPorta.QueueResponse(BytesOf('4C 30 39 3F 41 2A'));
  FDrv.DeviceActive(9);
  AssertBytesEqual('address 9', BytesOf('4C 30 39 3F 3F 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestASCIIDriver.APositiveAnswerConfirmsTheDevice;
begin
  //o equipamento devolve o mesmo endereco com "A" no lugar do segundo "?"
  FPorta.QueueResponse(BytesOf('4C 30 31 3F 41 2A'));
  AssertEquals('device present', Ord(ioOk), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.AnAnswerInTheShortFormatIsAlsoAccepted;
begin
  //ha equipamentos que respondem so com o digito das unidades; o driver
  //aceita as duas formas
  FPorta.QueueResponse(BytesOf('4C 31 3F 41 2A 00'));
  AssertEquals('short format', Ord(ioOk), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.AnAnswerFromAnotherAddressIsRefused;
begin
  //perguntamos ao 1 e respondeu o 2
  FPorta.QueueResponse(BytesOf('4C 30 32 3F 41 2A'));
  AssertEquals('wrong address', Ord(ioCommError), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.AnAnswerFullOfGarbageIsRefused;
begin
  FPorta.QueueResponse(BytesOf('FF FF FF FF FF FF'));
  AssertEquals('answer that makes no sense', Ord(ioCommError), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.WithNoAnswerItBecomesATimeout;
begin
  //nada enfileirado: o equipamento nao respondeu
  AssertEquals('timeout', Ord(ioTimeOut), Ord(FDrv.DeviceActive(1)));
  AssertEquals('but the request did get sent', 1, FPorta.WriteCount);
end;

procedure TTestWestASCIIDriver.WithNoCommunicationPortItDoesNotTryToTalk;
var
  semPorta:TWestASCIIDriver;
begin
  semPorta:=TWestASCIIDriver.Create(nil);
  try
    AssertEquals('driver with no port', Ord(ioNullDriver), Ord(semPorta.DeviceActive(1)));
  finally
    semPorta.Free;
  end;
end;

initialization
  RegisterTest(TTestWestASCIIDriver);
  RegisterTest(TTestWestParameters);

end.
