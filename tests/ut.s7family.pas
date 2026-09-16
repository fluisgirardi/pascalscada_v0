{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TSiemensProtocolFamily: montagem das PDUs S7.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os frames esperados sao literais no formato documentado do S7comm (o mesmo
  que o dissecador do Wireshark mostra): cabecalho $32, parametro com a funcao
  e a contagem de itens, e cada item de 12 bytes com area, DB e endereco
  inicial em bits. A classe base e' testada direto, com PDUIncoming e
  PDUOutgoing em zero - assim o que se ve no teste e' so a PDU S7, sem o
  envelope ISO-TCP que os descendentes acrescentam.
}
{$ELSE}
{:
  @abstract(TSiemensProtocolFamily tests: S7 PDU building.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The expected frames are literals in the documented S7comm format (the same
  one Wireshark's dissector shows): a $32 header, a parameter carrying the
  function and the item count, and 12-byte items with area, DB and the start
  address in bits. The base class is tested directly, with PDUIncoming and
  PDUOutgoing at zero - so what the test shows is the S7 PDU alone, without
  the ISO-TCP envelope the descendants add.
}
{$ENDIF}
unit ut.s7family;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, s7types, s7family, ISOTCPDriver, ProtocolTypes, Tag,
  testsupport.bytes, testsupport.fakeport;

type

  {$IFDEF PORTUGUES}
  //: Expoe os metodos protegidos que montam a PDU.
  {$ELSE}
  //: Exposes the protected methods that build the PDU.
  {$ENDIF}

  { TS7Probe }

  TS7Probe = class(TSiemensProtocolFamily)
  public
    procedure Header(var aMsg:BYTES; aPDUType:LongInt);
    procedure PrepRead(var aMsg:BYTES);
    procedure PrepWrite(var aMsg:BYTES);
    procedure AddReadItem(var aMsg:BYTES; aArea, aDB, aStart, aByteCount:LongInt);
    procedure AddWriteParam(var aMsg:BYTES; aArea, aDB, aStart:LongInt; aBuffer:BYTES);
    procedure AddWriteData(var aMsg:BYTES; aArea, aDB, aStart:LongInt; aBuffer:BYTES);
    procedure PutParam(var aMsg:BYTES; const aParam:BYTES);
    procedure PutData(var aMsg:BYTES; const aData:BYTES);
    function  ReadPDU(var aMsg:BYTES; out aPDU:TPDU; out aError:Integer):Boolean;
    function  Swap(aWord:Word):Word;
  end;


  { TS7PortProbe }

  //um driver ISOTCP concreto (o s7family e' abstrato: quem implementa o
  //transporte e' o ISOTCP) com a varredura desligada e DoRead/DoWrite abertos,
  //tomando as secoes criticas como Read/Write da base fariam
  //a concrete ISOTCP driver (s7family is abstract: the ISOTCP is what
  //implements the transport) with the scan off and DoRead/DoWrite exposed,
  //taking the critical sections as the base's Read/Write would
  TS7PortProbe = class(TISOTCPDriver)
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function ReadDB(aDBNum, aAddress, aSize:LongInt; out aValues:TArrayOfDouble):TProtocolIOResult;
    function WriteDB(aDBNum, aAddress:LongInt; const aValues:array of Double):TProtocolIOResult;
  end;

  { TTestS7ReadWriteOverAPort }

  //o caminho inteiro do s7family pela porta de mentira: conecta (ISO connect +
  //negociacao de PDU), le e escreve DBs
  //the whole s7family path through the fake port: it connects (ISO connect +
  //PDU negotiation), reads and writes DBs
  TTestS7ReadWriteOverAPort = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TS7PortProbe;
    procedure QueueConnectAndNegotiate;
    procedure QueueReadReply(aRetCode:Byte; const aData:array of Byte);
    procedure QueueWriteReply(aRetCode:Byte);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AReadConnectsNegotiatesThenReturnsTheBytes;
    procedure TheConnectionAndNegotiationHappenOnlyOnce;
    procedure TheReadRequestAsksForTheFunctionAndDb;
    procedure APlcErrorOnReadBecomesAProtocolResult;
    procedure WithNoNegotiationTheReadFails;
    procedure AWriteSendsTheValuesAndSucceeds;
    procedure TheWriteRequestCarriesTheDataBytes;
    procedure APlcErrorOnWriteBecomesAProtocolResult;
  end;

  { TTestS7Family }

  TTestS7Family = class(TTestCase)
  private
    FDrv:TS7Probe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //cabecalho / header
    procedure APduHeaderIsTenBytes;
    procedure ATypeTwoHeaderReservesTheErrorBytes;
    procedure ByteSwappingInAWord;

    //pedidos de leitura / read requests
    procedure AReadRequestUsesFunction04;
    procedure ADbReadItemBuildsTheFrame;
    procedure TheStartAddressGoesInBits;
    procedure ABigAddressUsesTheThirdByte;
    procedure TheMerkerAreaCarriesNoDbNumber;
    procedure EveryItemBumpsTheCountAndTheSize;

    //pedidos de escrita / write requests
    procedure AWriteRequestUsesFunction05;
    procedure AWriteCarriesParameterAndData;

    //blocos de parametro e dado / parameter and data blocks
    procedure TheParametersAreConcatenatedInOrder;
    procedure TheDataGoesInAfterTheParameter;

    //leitura do cabecalho / header parsing
    procedure SetupPduReportsTheSizes;
    procedure ATypeTwoSetupPduHasATwelveByteHeader;

    //contadores, temporizadores e areas do S7-200 / counters, timers and S7-200 areas
    procedure ACounterIsAddressedByElementNumber;
    procedure DifferentCountersProduceDifferentRequests;
    procedure ATimerUsesItsOwnArea;
    procedure WritingToACounterAlsoUsesTheElementNumber;
    procedure TheS7200AnalogAreaAddressesInBits;
    procedure ReadAndWriteAgreeOnTheCounterSize;
    procedure TheCounterItemSizeGoesInElements;
    procedure TheS7200AnalogAreaAlsoCountsInElements;
    procedure AByteAreaCountsInBytes;
  end;

implementation

{ TS7Probe }

procedure TS7Probe.Header(var aMsg:BYTES; aPDUType:LongInt);
begin
  InitiatePDUHeader(aMsg, aPDUType);
end;

procedure TS7Probe.PrepRead(var aMsg:BYTES);
begin
  PrepareReadRequest(aMsg);
end;

procedure TS7Probe.PrepWrite(var aMsg:BYTES);
begin
  PrepareWriteRequest(aMsg);
end;

procedure TS7Probe.AddReadItem(var aMsg:BYTES; aArea, aDB, aStart, aByteCount:LongInt);
begin
  AddToReadRequest(aMsg, aArea, aDB, aStart, aByteCount);
end;

procedure TS7Probe.AddWriteParam(var aMsg:BYTES; aArea, aDB, aStart:LongInt; aBuffer:BYTES);
begin
  AddParamToWriteRequest(aMsg, aArea, aDB, aStart, aBuffer);
end;

procedure TS7Probe.AddWriteData(var aMsg:BYTES; aArea, aDB, aStart:LongInt; aBuffer:BYTES);
begin
  AddDataToWriteRequest(aMsg, aArea, aDB, aStart, aBuffer);
end;

procedure TS7Probe.PutParam(var aMsg:BYTES; const aParam:BYTES);
begin
  AddParam(aMsg, aParam);
end;

procedure TS7Probe.PutData(var aMsg:BYTES; const aData:BYTES);
begin
  AddData(aMsg, aData);
end;

function TS7Probe.ReadPDU(var aMsg:BYTES; out aPDU:TPDU; out aError:Integer):Boolean;
begin
  Result:=SetupPDU(aMsg, true, aPDU, aError);
end;

function TS7Probe.Swap(aWord:Word):Word;
begin
  Result:=SwapBytesInWord(aWord);
end;

{ TTestS7Family }

//Destruir o driver custa meio segundo (o destrutor espera a thread de scan
//perceber o Terminate), e criar um por teste levaria a suite inteira a oito
//segundos. Os metodos exercitados aqui recebem a mensagem por parametro e nao
//guardam estado nenhum no driver, entao uma instancia so atende a classe toda.
var
  DriverCompartilhado:TS7Probe = nil;

procedure TTestS7Family.SetUp;
begin
  if DriverCompartilhado=nil then
    DriverCompartilhado:=TS7Probe.Create(nil);
  FDrv:=DriverCompartilhado;
end;

procedure TTestS7Family.TearDown;
begin
  FDrv:=nil;
end;

procedure TTestS7Family.APduHeaderIsTenBytes;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 1);
  //$32 identifica a PDU; depois tipo, dois reservados, numero, e os dois
  //tamanhos (parametro e dado), ainda zerados.
  AssertBytesEqual('type 1 header', BytesOf('32 01 00 00 00 00 00 00 00 00'), msg);
end;

procedure TTestS7Family.ATypeTwoHeaderReservesTheErrorBytes;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 2);
  //tipos 2 e 3 tem dois bytes a mais, para o codigo de erro
  AssertBytesEqual('type 2 header', BytesOf('32 02 00 00 00 00 00 00 00 00 00 00'), msg);
end;

procedure TTestS7Family.ByteSwappingInAWord;
begin
  //a PDU e' big-endian; os records sao lidos na ordem da maquina, entao todo
  //campo de 16 bits passa por aqui.
  AssertEquals('1234', $3412, FDrv.Swap($1234));
  AssertEquals('00FF', $FF00, FDrv.Swap($00FF));
  AssertEquals('0000', $0000, FDrv.Swap($0000));
end;

procedure TTestS7Family.AReadRequestUsesFunction04;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  //tamanho de parametro 2 (big-endian nos bytes 6 e 7), funcao $04, zero itens
  AssertBytesEqual('empty read request',
                   BytesOf('32 01 00 00 00 00 00 02 00 00 04 00'), msg);
end;

procedure TTestS7Family.AWriteRequestUsesFunction05;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepWrite(msg);
  AssertBytesEqual('empty write request',
                   BytesOf('32 01 00 00 00 00 00 02 00 00 05 00'), msg);
end;

procedure TTestS7Family.ADbReadItemBuildsTheFrame;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_DB, 1, 0, 2);

  //item: 12 0A 10 = especificacao de variavel; 02 = por byte; 00 02 = dois
  //bytes; 00 01 = DB 1; 84 = area de DB; 00 00 00 = bit zero.
  AssertBytesEqual('read of DB1.DBB0, 2 bytes',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00' +   //cabecalho, parametro com 14
                           '04 01' +                           //funcao de leitura, 1 item
                           '12 0A 10 02 00 02 00 01 84 00 00 00'),
                   msg);
end;

procedure TTestS7Family.TheStartAddressGoesInBits;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_DB, 1, 10, 4);

  //byte 10 vira bit 80 = $50 - o endereco no item e' sempre em bits
  AssertBytesEqual('read starting at byte 10',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 02 00 04 00 01 84 00 00 50'),
                   msg);
end;

procedure TTestS7Family.ABigAddressUsesTheThirdByte;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_DB, 1, 8192, 2);

  //8192 bytes = 65536 bits = $010000: nao cabe em dois bytes, e o campo tem tres
  AssertBytesEqual('read starting at byte 8192',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 02 00 02 00 01 84 01 00 00'),
                   msg);
end;

procedure TTestS7Family.TheMerkerAreaCarriesNoDbNumber;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_Flags, 0, 4, 2);

  //area $83 (merkers), sem DB, byte 4 = bit 32 = $20
  AssertBytesEqual('read of MB4',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 02 00 02 00 00 83 00 00 20'),
                   msg);
end;

procedure TTestS7Family.EveryItemBumpsTheCountAndTheSize;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_DB, 1, 0, 2);
  FDrv.AddReadItem(msg, vtS7_DB, 2, 4, 2);

  //dois itens: contagem 02 e tamanho de parametro $1A = 2 + 12 + 12
  AssertBytesEqual('two reads in a single request',
                   BytesOf('32 01 00 00 00 00 00 1A 00 00' +
                           '04 02' +
                           '12 0A 10 02 00 02 00 01 84 00 00 00' +
                           '12 0A 10 02 00 02 00 02 84 00 00 20'),
                   msg);
end;

procedure TTestS7Family.AWriteCarriesParameterAndData;
var
  msg, buffer:BYTES;
begin
  buffer:=BytesOf('AA BB');

  msg:=nil;
  FDrv.PrepWrite(msg);
  FDrv.AddWriteParam(msg, vtS7_DB, 1, 0, buffer);
  FDrv.AddWriteData (msg, vtS7_DB, 1, 0, buffer);

  //o bloco de dado leva $04 (contado em bits) e o tamanho 00 10 = 16 bits
  AssertBytesEqual('write of 2 bytes to DB1.DBB0',
                   BytesOf('32 01 00 00 00 00 00 0E 00 06' +   //parametro 14, dado 6
                           '05 01' +                           //funcao de escrita, 1 item
                           '12 0A 10 02 00 02 00 01 84 00 00 00' +
                           '00 04 00 10 AA BB'),
                   msg);
end;

procedure TTestS7Family.TheParametersAreConcatenatedInOrder;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 1);
  FDrv.PutParam(msg, BytesOf('AA BB'));
  FDrv.PutParam(msg, BytesOf('CC'));

  //o tamanho do parametro acumula, e o segundo bloco entra depois do primeiro
  AssertBytesEqual('two parameters',
                   BytesOf('32 01 00 00 00 00 00 03 00 00 AA BB CC'), msg);
end;

procedure TTestS7Family.TheDataGoesInAfterTheParameter;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 1);
  FDrv.PutParam(msg, BytesOf('AA BB'));
  FDrv.PutData (msg, BytesOf('11 22 33'));

  //tamanho de parametro 2 e de dado 3, nessa ordem no cabecalho
  AssertBytesEqual('parameter and data',
                   BytesOf('32 01 00 00 00 00 00 02 00 03 AA BB 11 22 33'), msg);
end;

procedure TTestS7Family.SetupPduReportsTheSizes;
var
  msg:BYTES;
  pdu:TPDU;
  err:Integer;
begin
  msg:=nil;
  FDrv.Header(msg, 1);
  FDrv.PutParam(msg, BytesOf('AA BB'));
  FDrv.PutData (msg, BytesOf('11 22 33'));

  AssertTrue('the PDU must be recognised', FDrv.ReadPDU(msg, pdu, err));
  AssertEquals('header size',  10, pdu.header_len);
  AssertEquals('parameter size',  2,  pdu.param_len);
  AssertEquals('data size',       3,  pdu.data_len);
end;

procedure TTestS7Family.ATypeTwoSetupPduHasATwelveByteHeader;
var
  msg:BYTES;
  pdu:TPDU;
  err:Integer;
begin
  msg:=nil;
  FDrv.Header(msg, 2);
  FDrv.PutParam(msg, BytesOf('AA BB'));

  AssertTrue('the PDU must be recognised', FDrv.ReadPDU(msg, pdu, err));
  AssertEquals('type 2 header', 12, pdu.header_len);
  AssertEquals('parameter size', 2, pdu.param_len);
end;

procedure TTestS7Family.ACounterIsAddressedByElementNumber;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_Counter, 0, 3, 1);

  //contadores e temporizadores sao os unicos que nao convertem o endereco
  //para bits: o campo leva o numero do elemento. O tipo tambem muda de $02
  //(byte) para $1C (contador), e a area e' a mesma $1C.
  AssertBytesEqual('read of counter 3',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 1C 00 01 00 00 1C 00 00 03'),
                   msg);
end;

procedure TTestS7Family.DifferentCountersProduceDifferentRequests;
var
  msgA, msgB:BYTES;
begin
  //o sintoma do defeito que existia aqui: o numero do contador nao entrava no
  //frame, entao qualquer contador gerava o mesmo pedido.
  msgA:=nil;
  FDrv.PrepRead(msgA);
  FDrv.AddReadItem(msgA, vtS7_Counter, 0, 3, 1);

  msgB:=nil;
  FDrv.PrepRead(msgB);
  FDrv.AddReadItem(msgB, vtS7_Counter, 0, 99, 1);

  AssertFalse('counter 3 and counter 99 produced the same request', HexOf(msgA)=HexOf(msgB));
  AssertBytesEqual('read of counter 99',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 1C 00 01 00 00 1C 00 00 63'),
                   msgB);
end;

procedure TTestS7Family.ATimerUsesItsOwnArea;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_Timer, 0, 7, 1);

  //temporizador: tipo e area $1D, e o numero do elemento sem converter
  AssertBytesEqual('read of timer 7',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 1D 00 01 00 00 1D 00 00 07'),
                   msg);
end;

procedure TTestS7Family.WritingToACounterAlsoUsesTheElementNumber;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepWrite(msg);
  FDrv.AddWriteParam(msg, vtS7_Counter, 0, 5, BytesOf('00 0A'));

  //o mesmo calculo de endereco vale na montagem do pedido de escrita, que
  //tem o seu proprio case de areas; o tamanho aqui e' contado em elementos.
  AssertBytesEqual('write to counter 5',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 05 01' +
                           '12 0A 10 1C 00 01 00 00 1C 00 00 05'),
                   msg);
end;

procedure TTestS7Family.TheS7200AnalogAreaAddressesInBits;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_200_AnInput, 0, 2, 2);

  //area analogica do S7-200: e' area de word (tipo $04), mas o endereco
  //continua em bits como nas demais - byte 2 = bit 16 = $10. Os 2 bytes
  //pedidos viram 1 word na contagem do item.
  AssertBytesEqual('read of AIW2',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 04 00 01 00 00 06 00 00 10'),
                   msg);
end;

procedure TTestS7Family.ReadAndWriteAgreeOnTheCounterSize;
var
  msgLeitura, msgEscrita:BYTES;
begin
  //duas unidades de tamanho, dos dois lados
  msgLeitura:=nil;
  FDrv.PrepRead(msgLeitura);
  FDrv.AddReadItem(msgLeitura, vtS7_Counter, 0, 5, 2);

  msgEscrita:=nil;
  FDrv.PrepWrite(msgEscrita);
  FDrv.AddWriteParam(msgEscrita, vtS7_Counter, 0, 5, BytesOf('00 0A'));

  //byte 17 do frame = parte baixa da contagem de elementos do item
  AssertEquals('element count in the item', msgLeitura[17], msgEscrita[17]);
end;

procedure TTestS7Family.TheCounterItemSizeGoesInElements;
var
  msg:BYTES;
begin
  //cada contador ocupa dois bytes, entao 4 bytes pedidos sao 2 contadores -
  //conferido contra o snap7 e o libnodave
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_Counter, 0, 5, 4);

  AssertBytesEqual('read of 2 counters starting at 5',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 1C 00 02 00 00 1C 00 00 05'),
                   msg);
end;

procedure TTestS7Family.TheS7200AnalogAreaAlsoCountsInElements;
var
  msg:BYTES;
begin
  //area de word: 4 bytes sao 2 words
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_200_AnInput, 0, 2, 4);

  AssertBytesEqual('read of 2 analog words',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 04 00 02 00 00 06 00 00 10'),
                   msg);
end;

procedure TTestS7Family.AByteAreaCountsInBytes;
var
  msg:BYTES;
begin
  //nas areas de byte um elemento e' um byte: a conversao nao pode valer aqui
  msg:=nil;
  FDrv.PrepRead(msg);
  FDrv.AddReadItem(msg, vtS7_DB, 1, 0, 4);

  AssertBytesEqual('read of 4 bytes from a DB',
                   BytesOf('32 01 00 00 00 00 00 0E 00 00 04 01' +
                           '12 0A 10 02 00 04 00 01 84 00 00 00'),
                   msg);
end;


{ TS7PortProbe }

procedure TS7PortProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TS7PortProbe.ReadDB(aDBNum, aAddress, aSize:LongInt; out aValues:TArrayOfDouble):TProtocolIOResult;
var
  tr:TTagRec;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.ReadFunction:=4; //DB
  tr.File_DB:=aDBNum;
  tr.Address:=aAddress;
  tr.Size:=aSize;
  FWriteCS.Enter;
  FReadCS.Enter;
  try
    Result:=DoRead(tr, aValues, true);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
  end;
end;

function TS7PortProbe.WriteDB(aDBNum, aAddress:LongInt; const aValues:array of Double):TProtocolIOResult;
var
  tr:TTagRec;
  v:TArrayOfDouble;
  i:Integer;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.ReadFunction:=4;
  tr.File_DB:=aDBNum;
  tr.Address:=aAddress;
  tr.Size:=Length(aValues);
  SetLength(v, Length(aValues));
  for i:=0 to High(aValues) do v[i]:=aValues[i];
  FWriteCS.Enter;
  FReadCS.Enter;
  try
    Result:=DoWrite(tr, v, true);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
  end;
end;

{ TTestS7ReadWriteOverAPort }

//: verdadeiro se o quadro contem o byte dado / true if the frame holds the byte
function HasByte(const aFrame:BYTES; aByte:Byte):Boolean;
var
  i:Integer;
begin
  Result:=false;
  for i:=0 to High(aFrame) do
    if aFrame[i]=aByte then
      exit(true);
end;

procedure TTestS7ReadWriteOverAPort.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;
  FDrv:=TS7PortProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
end;

procedure TTestS7ReadWriteOverAPort.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

procedure TTestS7ReadWriteOverAPort.QueueConnectAndNegotiate;
begin
  //COTP Connect Confirm (22 bytes) e a resposta da negociacao dizendo PDU 240
  //COTP Connect Confirm (22 bytes) and the negotiation reply saying PDU 240
  FPort.QueueResponse(BytesOf('03 00 00 16 11 D0 00 01 00 01 00 C0 01 0A C1 02 01 00 C2 02 01 02'));
  FPort.QueueResponse(BytesOf('03 00 00 1B 02 F0 80 32 03 00 00 00 00 00 08 00 00 00 00 F0 00 00 01 00 01 00 F0'));
end;

procedure TTestS7ReadWriteOverAPort.QueueReadReply(aRetCode:Byte; const aData:array of Byte);
var
  s7, frame:BYTES;
  i, datalen, total:Integer;
begin
  //cabecalho S7 tipo 3 (12 bytes: P,tipo,a,b, number, param_len, data_len,
  //error), parametro "leitura, 1 item", e o item de dado: codigo de retorno e,
  //se sucesso, transporte 4 (bits), o tamanho em bits e os bytes
  //S7 type-3 header (12 bytes: P,type,a,b, number, param_len, data_len,
  //error), "read, 1 item" parameter, and the data item: return code and, on
  //success, transport 4 (bits), the size in bits and the bytes
  if aRetCode=$FF then
    datalen:=4+Length(aData)
  else
    datalen:=1;
  SetLength(s7, 12+2+datalen);
  s7[0]:=$32; s7[1]:=$03;                          //P, tipo 3
  s7[6]:=0;  s7[7]:=2;                             //param_len = 2 (big endian)
  s7[8]:=Hi(Word(datalen)); s7[9]:=Lo(Word(datalen)); //data_len (big endian)
  s7[12]:=$04; s7[13]:=$01;                        //param: read, 1 item
  s7[14]:=aRetCode;
  if aRetCode=$FF then begin
    s7[15]:=$04;                                   //transporte 4 (bits)
    s7[16]:=Hi(Word(Length(aData)*8)); s7[17]:=Lo(Word(Length(aData)*8));
    for i:=0 to High(aData) do s7[18+i]:=aData[i];
  end;
  total:=7+Length(s7);
  SetLength(frame, total);
  frame[0]:=$03; frame[1]:=0; frame[2]:=Hi(Word(total)); frame[3]:=Lo(Word(total));
  frame[4]:=$02; frame[5]:=$F0; frame[6]:=$80;
  Move(s7[0], frame[7], Length(s7));
  FPort.QueueResponse(frame);
end;

procedure TTestS7ReadWriteOverAPort.QueueWriteReply(aRetCode:Byte);
var
  s7, frame:BYTES;
  total:Integer;
begin
  //resposta de escrita: parametro "escrita, 1 item" e um byte de status
  //write reply: "write, 1 item" parameter and one status byte
  SetLength(s7, 12+2+1);
  s7[0]:=$32; s7[1]:=$03;   //P, tipo 3
  s7[6]:=0; s7[7]:=2;       //param_len = 2 (big endian)
  s7[8]:=0; s7[9]:=1;       //data_len = 1 (big endian)
  s7[12]:=$05; s7[13]:=$01; //param: write, 1 item
  s7[14]:=aRetCode;         //status
  total:=7+Length(s7);
  SetLength(frame, total);
  frame[0]:=$03; frame[1]:=0; frame[2]:=Hi(Word(total)); frame[3]:=Lo(Word(total));
  frame[4]:=$02; frame[5]:=$F0; frame[6]:=$80;
  Move(s7[0], frame[7], Length(s7));
  FPort.QueueResponse(frame);
end;

procedure TTestS7ReadWriteOverAPort.AReadConnectsNegotiatesThenReturnsTheBytes;
var
  v:TArrayOfDouble;
begin
  QueueConnectAndNegotiate;
  QueueReadReply($FF, [$0A,$0B,$0C,$0D]);

  AssertEquals('leu', Ord(ioOk), Ord(FDrv.ReadDB(1, 0, 4, v)));
  AssertEquals('quatro bytes', 4, Length(v));
  AssertEquals('primeiro', $0A, v[0], 0);
  AssertEquals('ultimo',   $0D, v[3], 0);
  AssertEquals('tudo consumido', 0, FPort.PendingResponses);
end;

procedure TTestS7ReadWriteOverAPort.TheConnectionAndNegotiationHappenOnlyOnce;
var
  v:TArrayOfDouble;
begin
  //conexao e negociacao ficam guardadas no CLP: a segunda leitura so' manda o
  //pedido de leitura
  //the connection and negotiation are kept on the PLC: the second read only
  //sends the read request
  QueueConnectAndNegotiate;
  QueueReadReply($FF, [1,2,3,4]);
  FDrv.ReadDB(1, 0, 4, v);
  AssertEquals('connect, negocia, leitura', 3, FPort.WriteCount);

  QueueReadReply($FF, [9,9,9,9]);
  FDrv.ReadDB(1, 0, 4, v);

  AssertEquals('mais uma so', 4, FPort.WriteCount);
  AssertEquals('o novo valor', 9, v[0], 0);
end;

procedure TTestS7ReadWriteOverAPort.TheReadRequestAsksForTheFunctionAndDb;
var
  v:TArrayOfDouble;
  req:BYTES;
begin
  //o pedido que sai (3a escrita) leva a funcao de leitura ($04) e a area DB
  //the request that goes out (3rd write) carries the read function ($04) and
  //the DB area
  QueueConnectAndNegotiate;
  QueueReadReply($FF, [$0A,$0B,$0C,$0D]);
  FDrv.ReadDB(5, 0, 4, v);

  req:=FPort.WrittenFrame(2);
  //depois do TPKT(4)+COTP(3)+cabecalho S7 de pedido(10) vem o parametro; o
  //primeiro byte do parametro e' a funcao ($04)
  //after TPKT(4)+COTP(3)+request S7 header(10) comes the parameter; its first
  //byte is the function ($04)
  AssertEquals('funcao de leitura', $04, req[7+10]);
  AssertTrue  ('o numero do DB (5) aparece no pedido', HasByte(req, 5));
end;

procedure TTestS7ReadWriteOverAPort.APlcErrorOnReadBecomesAProtocolResult;
var
  v:TArrayOfDouble;
begin
  //codigo $0A: area/endereco fora da faixa - o CLP recusa a leitura
  //code $0A: area/address out of range - the PLC refuses the read
  QueueConnectAndNegotiate;
  QueueReadReply($0A, []);

  AssertTrue('nao foi ok', FDrv.ReadDB(1, 0, 4, v)<>ioOk);
end;

procedure TTestS7ReadWriteOverAPort.WithNoNegotiationTheReadFails;
var
  v:TArrayOfDouble;
begin
  //aceita a conexao mas fica mudo na negociacao: sem PDU negociada nao le
  //accepts the connection but goes silent on the negotiation: with no
  //negotiated PDU it does not read
  FPort.QueueResponse(BytesOf('03 00 00 16 11 D0 00 01 00 01 00 C0 01 0A C1 02 01 00 C2 02 01 02'));

  AssertTrue('nao leu', FDrv.ReadDB(1, 0, 4, v)<>ioOk);
end;

procedure TTestS7ReadWriteOverAPort.AWriteSendsTheValuesAndSucceeds;
begin
  QueueConnectAndNegotiate;
  QueueWriteReply($FF);

  AssertEquals('escreveu', Ord(ioOk), Ord(FDrv.WriteDB(1, 0, [$0A,$0B])));
end;

procedure TTestS7ReadWriteOverAPort.TheWriteRequestCarriesTheDataBytes;
var
  req:BYTES;
begin
  //os bytes escritos vao no pedido de SendUnitData/escrita
  //the written bytes go in the SendUnitData/write request
  QueueConnectAndNegotiate;
  QueueWriteReply($FF);
  FDrv.WriteDB(1, 0, [$AA,$BB]);

  req:=FPort.WrittenFrame(2);
  AssertTrue('o byte AA no pedido', HasByte(req, $AA));
  AssertTrue('o byte BB no pedido', HasByte(req, $BB));
end;

procedure TTestS7ReadWriteOverAPort.APlcErrorOnWriteBecomesAProtocolResult;
begin
  QueueConnectAndNegotiate;
  QueueWriteReply($0A);

  AssertTrue('nao foi ok', FDrv.WriteDB(1, 0, [$0A,$0B])<>ioOk);
end;

initialization
  RegisterTest(TTestS7Family);
  RegisterTest(TTestS7ReadWriteOverAPort);

finalization
  FreeAndNil(DriverCompartilhado);

end.
