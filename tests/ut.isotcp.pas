{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TISOTCPDriver: a camada ISO-on-TCP por baixo do S7.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O S7 puro - montar e ler PDUs - ja' tem os seus testes em ut.s7family. O que
  fica aqui e' o que o ISOTCP acrescenta por baixo: o cabecalho TPKT de 4
  bytes com o tamanho, o pedido de conexao COTP que leva rack e slot no TSAP
  chamado, a negociacao do tamanho da PDU logo depois da confirmacao, e a
  leitura da resposta em duas partes - cabecalho e resto - pulando os pacotes
  de 7 bytes que alguns CLPs mandam sem carga nenhuma.

  Tudo passa por uma porta de mentira: o teste enfileira o que o CLP
  responderia e confere byte a byte o que o driver mandou.
}
{$ELSE}
{:
  @abstract(TISOTCPDriver tests: the ISO-on-TCP layer beneath S7.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Plain S7 - building and reading PDUs - already has its tests in
  ut.s7family. What lives here is what ISOTCP adds underneath: the 4-byte TPKT
  header carrying the length, the COTP connect request carrying rack and slot
  in the called TSAP, the PDU size negotiation right after the confirmation,
  and reading the answer in two parts - header and rest - skipping the 7-byte
  packets some PLCs send with no payload at all.

  Everything goes through a fake port: the test queues what the PLC would
  answer and checks byte by byte what the driver sent.
}
{$ENDIF}
unit ut.isotcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, s7types, ISOTCPDriver, Tag, PLCTagNumber, ProtocolTypes,
  testsupport.bytes, testsupport.fakeport;

type

  { TISOTCPProbe }

  //connectPLC, exchange e os CLPs sao protegidos: quem os chama e' o DoRead,
  //no meio de uma varredura e com a secao critica de leitura tomada - a
  //troca a solta enquanto espera o CLP e a toma de volta. A sonda os expoe
  //para a conexao ser conduzida passo a passo, e toma a secao antes, como o
  //DoRead faria.
  //
  //connectPLC, exchange and the PLCs are protected: DoRead calls them, in
  //the middle of a scan and holding the read critical section - the exchange
  //releases it while waiting for the PLC and takes it back. The probe
  //exposes them so the connection can be driven step by step, and takes the
  //section first, as DoRead would.
  //
  //A varredura do driver e' uma thread de verdade, viva desde o construtor,
  //e com um CLP na lista ela tenta conecta-lo sozinha, a cada meio segundo,
  //pela mesma porta - e consumiria as respostas enfileiradas pelo teste. A
  //sonda a deixa sem nada para fazer.
  //The driver's scan is a real thread, alive since the constructor, and with
  //a PLC on the list it tries to connect it on its own, every half second,
  //through the same port - and would consume the answers the test queued.
  //The probe leaves it with nothing to do.
  TISOTCPProbe = class(TISOTCPDriver)
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function  AddPLC(aRack, aSlot, aStation:LongInt):LongInt;
    function  PLC(aIndex:LongInt):TS7CPU;
    function  Connect(aIndex:LongInt):Boolean;
    function  Talk(aIndex:LongInt; var aOut, aIn:BYTES):Boolean;
    function  Framed(const aBody:BYTES):BYTES;
    function  TagInfoOf(aTag:TTag):TTagRec;
    procedure PortWentDown;
  end;

  { TTestISOTCP }

  TTestISOTCP = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TISOTCPProbe;
    procedure ThePLCAcceptsTheConnection;
    procedure ThePLCAnswersTheNegotiationWith240;
    function  Connected:Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o cabecalho TPKT / the TPKT header
    procedure TheTPKTHeaderCarriesTheWholeLength;

    //o pedido de conexao / the connect request
    procedure TheConnectRequestIsACOTPConnectRequest;
    procedure TheRackAndSlotGoInTheCalledTSAP;
    procedure TheConnectionTypeGoesInTheCalledTSAP;
    procedure ViaCP243TheTSAPsAreMW;

    //a conexao / the connection
    procedure AConfirmedConnectionNegotiatesThePDUSize;
    procedure TheNegotiatedPDUSizeIsKept;
    procedure AStrayPacketBeforeTheConfirmationIsSkipped;
    procedure WithNoAnswerTheConnectionFails;
    procedure WithARefusedNegotiationTheConnectionFails;
    procedure WithNoPortTheConnectionFails;

    //a troca / the exchange
    procedure AnExchangeWrapsTheMessageInTPKTAndCOTP;
    procedure AnExchangeBringsTheWholeAnswerBack;
    procedure AJunk7BytePacketBeforeTheAnswerIsSkipped;
    procedure AnExchangeThatIsNeverAnsweredGivesUp;
    procedure AnExchangeWithNoPortFails;

    //rack, slot e estacao / rack, slot and station
    procedure TheDriverAddressOverridesTheTagAddress;
    procedure ChangingTheAddressDisconnectsThePLC;
    procedure SettingTheSameAddressKeepsTheConnection;

    //a porta caiu / the port went down
    procedure WhenThePortGoesDownEveryPLCIsDisconnected;
  end;

implementation

{ TISOTCPProbe }

procedure TISOTCPProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TISOTCPProbe.AddPLC(aRack, aSlot, aStation:LongInt):LongInt;
begin
  Result:=CreatePLC(aRack, aSlot, aStation);
end;

function TISOTCPProbe.PLC(aIndex:LongInt):TS7CPU;
begin
  Result:=FPLCs[aIndex];
end;

function TISOTCPProbe.Connect(aIndex:LongInt):Boolean;
begin
  FReadCS.Enter;
  try
    Result:=connectPLC(FPLCs[aIndex]);
  finally
    FReadCS.Leave;
  end;
end;

function TISOTCPProbe.Talk(aIndex:LongInt; var aOut, aIn:BYTES):Boolean;
begin
  FReadCS.Enter;
  try
    Result:=exchange(FPLCs[aIndex], aOut, aIn, false);
  finally
    FReadCS.Leave;
  end;
end;

function TISOTCPProbe.Framed(const aBody:BYTES):BYTES;
begin
  Result:=aBody;
  PrepareToSend(Result);
end;

function TISOTCPProbe.TagInfoOf(aTag:TTag):TTagRec;
begin
  Result:=GetTagInfo(aTag);
end;

procedure TISOTCPProbe.PortWentDown;
begin
  PortDisconnected(Self);
end;

{ TTestISOTCP }

procedure TTestISOTCP.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;
  FDrv:=TISOTCPProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
  FDrv.AddPLC(0,2,2);
end;

procedure TTestISOTCP.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

procedure TTestISOTCP.ThePLCAcceptsTheConnection;
begin
  //COTP Connect Confirm, 22 bytes com o TPKT
  //COTP Connect Confirm, 22 bytes including the TPKT
  FPort.QueueResponse(BytesOf('03 00 00 16 11 D0 00 01 00 01 00 C0 01 0A C1 02 01 00 C2 02 01 02'));
end;

procedure TTestISOTCP.ThePLCAnswersTheNegotiationWith240;
begin
  //TPKT + COTP DT + cabecalho S7 de resposta (12 bytes) + parametro de
  //negociacao dizendo 240 bytes de PDU
  //TPKT + COTP DT + S7 ack header (12 bytes) + negotiation parameter saying
  //240 bytes of PDU
  FPort.QueueResponse(BytesOf('03 00 00 1B 02 F0 80 32 03 00 00 00 00 00 08 00 00 00 00 F0 00 00 01 00 01 00 F0'));
end;

function TTestISOTCP.Connected:Boolean;
begin
  Result:=FDrv.PLC(0).Connected;
end;

procedure TTestISOTCP.TheTPKTHeaderCarriesTheWholeLength;
var
  frame:BYTES;
begin
  //versao 3, reservado 0, tamanho em 2 bytes contando o proprio cabecalho
  //version 3, reserved 0, length in 2 bytes counting the header itself
  frame:=FDrv.Framed(BytesOf('00 00 00 00 AA BB CC'));

  AssertEquals('versao',    3, frame[0]);
  AssertEquals('reservado', 0, frame[1]);
  AssertEquals('tamanho',   7, frame[2]*256+frame[3]);
end;

procedure TTestISOTCP.TheConnectRequestIsACOTPConnectRequest;
begin
  FDrv.Connect(0);

  //rack 0, slot 2, ligacao OP: TSAP chamado 01 02; TPDU 2048 (0B)
  //rack 0, slot 2, OP connection: called TSAP 01 02; TPDU 2048 (0B)
  AssertBytesEqual('connect request',
                   BytesOf('03 00 00 16 11 E0 00 00 00 01 00 C1 02 01 00 C2 02 02 02 C0 01 0B'),
                   FPort.WrittenFrame(0));
end;

procedure TTestISOTCP.TheRackAndSlotGoInTheCalledTSAP;
var
  frame:BYTES;
begin
  //rack nos 3 bits altos, slot nos 5 baixos: rack 1, slot 3 = $23
  //rack in the high 3 bits, slot in the low 5: rack 1, slot 3 = $23
  FDrv.UpdatePLCAddress(1,3,2);

  FDrv.Connect(0);

  frame:=FPort.WrittenFrame(0);
  AssertEquals('rack e slot', $23, frame[18]);
end;

procedure TTestISOTCP.TheConnectionTypeGoesInTheCalledTSAP;
var
  frame:BYTES;
begin
  FDrv.ISOTCPConnType:=ctBasic;

  FDrv.Connect(0);

  frame:=FPort.WrittenFrame(0);
  AssertEquals('tipo basic', 3, frame[17]);
end;

procedure TTestISOTCP.ViaCP243TheTSAPsAreMW;
var
  frame:BYTES;
begin
  //pelo CP243 (S7-200) os dois TSAPs sao "MW", e a leitura da resposta
  //espera ate' um segundo
  //through the CP243 (S7-200) both TSAPs are "MW", and reading the answer
  //waits up to one second
  FDrv.ConnectionWay:=ISOTCP_VIA_CP243;

  FDrv.Connect(0);

  frame:=FPort.WrittenFrame(0);
  AssertEquals('M', $4D, frame[13]);
  AssertEquals('W', $57, frame[14]);
  AssertEquals('M', $4D, frame[17]);
  AssertEquals('W', $57, frame[18]);
end;

procedure TTestISOTCP.AConfirmedConnectionNegotiatesThePDUSize;
var
  frame:BYTES;
begin
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;

  FDrv.Connect(0);

  AssertEquals('duas escritas: conexao e negociacao', 2, FPort.WriteCount);
  frame:=FPort.WrittenFrame(1);
  AssertEquals('25 bytes', 25, Length(frame));
  AssertBytesEqual('TPKT + COTP DT', BytesOf('03 00 00 19 02 F0 80'), Copy(frame,0,7));
  AssertEquals('cabecalho S7 de pedido', $32, frame[7]);
  AssertEquals('tipo job',               $01, frame[8]);
  AssertBytesEqual('pede PDU de 960', BytesOf('F0 00 00 01 00 01 03 C0'), Copy(frame,17,8));
end;

procedure TTestISOTCP.TheNegotiatedPDUSizeIsKept;
begin
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;

  AssertTrue('conectou', FDrv.Connect(0));

  AssertTrue  ('CLP conectado',        Connected);
  AssertEquals('PDU de 240',           240, FDrv.PLC(0).MaxPDULen);
  AssertEquals('bloco de 240-18',      222, FDrv.PLC(0).MaxBlockSize);
  AssertEquals('tudo consumido',       0,   FPort.PendingResponses);
end;

procedure TTestISOTCP.AStrayPacketBeforeTheConfirmationIsSkipped;
begin
  //um pacote de 7 bytes antes da confirmacao: o driver le de novo
  //a 7-byte packet before the confirmation: the driver reads again
  FPort.QueueResponse(BytesOf('03 00 00 07 02 F0 80'));
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;

  AssertTrue('conectou mesmo assim', FDrv.Connect(0));
end;

procedure TTestISOTCP.WithNoAnswerTheConnectionFails;
begin
  AssertFalse('nao conectou', FDrv.Connect(0));
  AssertFalse('CLP desconectado', Connected);
end;

procedure TTestISOTCP.WithARefusedNegotiationTheConnectionFails;
begin
  //confirmou a conexao mas ficou mudo na negociacao
  //confirmed the connection but went silent on the negotiation
  ThePLCAcceptsTheConnection;

  AssertFalse('nao conectou', FDrv.Connect(0));
  AssertFalse('CLP desconectado', Connected);
end;

procedure TTestISOTCP.WithNoPortTheConnectionFails;
begin
  FDrv.CommunicationPort:=nil;

  AssertFalse('sem porta, sem conexao', FDrv.Connect(0));
  AssertEquals('e nada foi escrito', 0, FPort.WriteCount);
end;

procedure TTestISOTCP.AnExchangeWrapsTheMessageInTPKTAndCOTP;
var
  msgOut, msgIn:BYTES;
begin
  //os 7 primeiros bytes sao reservados ao TPKT e ao COTP; o S7 vem depois
  //the first 7 bytes are reserved for TPKT and COTP; S7 comes after
  ThePLCAnswersTheNegotiationWith240;
  msgOut:=BytesOf('00 00 00 00 00 00 00 32 01 00 00 00 00 00 00 00 00');

  FDrv.Talk(0, msgOut, msgIn);

  AssertBytesEqual('TPKT + COTP DT + S7', BytesOf('03 00 00 11 02 F0 80 32 01'), Copy(FPort.WrittenFrame(0),0,9));
end;

procedure TTestISOTCP.AnExchangeBringsTheWholeAnswerBack;
var
  msgOut, msgIn:BYTES;
begin
  //a resposta e' lida em duas partes - 7 bytes de cabecalho e o resto - e
  //volta inteira, cabecalho incluido
  //the answer is read in two parts - 7 header bytes and the rest - and
  //comes back whole, header included
  ThePLCAnswersTheNegotiationWith240;
  msgOut:=BytesOf('00 00 00 00 00 00 00 32 01 00 00 00 00 00 00 00 00');

  AssertTrue('trocou', FDrv.Talk(0, msgOut, msgIn));

  AssertEquals('27 bytes de volta', 27, Length(msgIn));
  AssertBytesEqual('a resposta inteira',
                   BytesOf('03 00 00 1B 02 F0 80 32 03 00 00 00 00 00 08 00 00 00 00 F0 00 00 01 00 01 00 F0'),
                   msgIn);
end;

procedure TTestISOTCP.AJunk7BytePacketBeforeTheAnswerIsSkipped;
var
  msgOut, msgIn:BYTES;
begin
  FPort.QueueResponse(BytesOf('03 00 00 07 02 F0 80'));
  ThePLCAnswersTheNegotiationWith240;
  msgOut:=BytesOf('00 00 00 00 00 00 00 32 01 00 00 00 00 00 00 00 00');

  AssertTrue('trocou', FDrv.Talk(0, msgOut, msgIn));

  AssertEquals('a resposta de verdade', 27, Length(msgIn));
  AssertEquals('tudo consumido', 0, FPort.PendingResponses);
end;

procedure TTestISOTCP.AnExchangeThatIsNeverAnsweredGivesUp;
var
  msgOut, msgIn:BYTES;
begin
  //CLP mudo - cabo fora, sem reset TCP: a leitura so' da' timeout. Tem que
  //desistir depois de algumas tentativas, nunca esperar para sempre.
  //silent PLC - cable out, no TCP reset: the read only times out. It has to
  //give up after a few tries, never wait forever.
  msgOut:=BytesOf('00 00 00 00 00 00 00 32 01 00 00 00 00 00 00 00 00');

  AssertFalse('desistiu', FDrv.Talk(0, msgOut, msgIn));
end;

procedure TTestISOTCP.AnExchangeWithNoPortFails;
var
  msgOut, msgIn:BYTES;
begin
  FDrv.CommunicationPort:=nil;
  msgOut:=BytesOf('00 00 00 00 00 00 00 32 01 00 00 00 00 00 00 00 00');

  AssertFalse('sem porta, sem troca', FDrv.Talk(0, msgOut, msgIn));
end;

procedure TTestISOTCP.TheDriverAddressOverridesTheTagAddress;
var
  tag:TPLCTagNumber;
  info:TTagRec;
begin
  //uma conexao TCP, um CLP: o rack, o slot e a estacao sao os do driver,
  //digam o que disserem os tags
  //one TCP connection, one PLC: rack, slot and station are the driver's,
  //whatever the tags say
  tag:=TPLCTagNumber.Create(nil);
  try
    tag.PLCRack:=7;
    tag.PLCSlot:=6;
    tag.PLCStation:=5;
    FDrv.UpdatePLCAddress(1,3,2);

    info:=FDrv.TagInfoOf(tag);

    AssertEquals('rack do driver',    1, info.Rack);
    AssertEquals('slot do driver',    3, info.Slot);
    AssertEquals('estacao do driver', 2, info.Station);
  finally
    tag.Free;
  end;
end;

procedure TTestISOTCP.ChangingTheAddressDisconnectsThePLC;
begin
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;
  FDrv.Connect(0);
  AssertTrue('antes, conectado', Connected);

  FDrv.PLCRack:=1;

  AssertEquals('o CLP mudou de rack', 1, FDrv.PLC(0).Rack);
  AssertFalse ('e vai ter que reconectar', Connected);
end;

procedure TTestISOTCP.SettingTheSameAddressKeepsTheConnection;
begin
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;
  FDrv.Connect(0);

  FDrv.UpdatePLCAddress(0,2,2);

  AssertTrue('mesmo endereco, mesma conexao', Connected);
end;

procedure TTestISOTCP.WhenThePortGoesDownEveryPLCIsDisconnected;
begin
  ThePLCAcceptsTheConnection;
  ThePLCAnswersTheNegotiationWith240;
  FDrv.Connect(0);

  FDrv.PortWentDown;

  AssertFalse('CLP desconectado', Connected);
end;

initialization
  RegisterTest(TTestISOTCP);

end.
