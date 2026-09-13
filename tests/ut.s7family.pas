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
  commtypes, s7types, s7family,
  testsupport.bytes;

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

  { TTestS7Family }

  TTestS7Family = class(TTestCase)
  private
    FDrv:TS7Probe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //cabecalho / header
    procedure CabecalhoDePduTemDezBytes;
    procedure CabecalhoTipoDoisReservaOsBytesDeErro;
    procedure TrocaDeBytesEmWord;

    //pedidos de leitura / read requests
    procedure PedidoDeLeituraUsaAFuncao04;
    procedure ItemDeLeituraDeDbMontaOFrame;
    procedure EnderecoInicialVaiEmBits;
    procedure EnderecoGrandeUsaOTerceiroByte;
    procedure AreaDeMerkersNaoLevaNumeroDeDb;
    procedure CadaItemIncrementaAContagemEOTamanho;

    //pedidos de escrita / write requests
    procedure PedidoDeEscritaUsaAFuncao05;
    procedure EscritaLevaParametroEDado;

    //blocos de parametro e dado / parameter and data blocks
    procedure ParametrosSaoConcatenadosNaOrdem;
    procedure DadoEntraDepoisDoParametro;

    //leitura do cabecalho / header parsing
    procedure SetupPduInformaOsTamanhos;
    procedure SetupPduDeTipoDoisTemCabecalhoDeDozeBytes;

    //contadores, temporizadores e areas do S7-200 / counters, timers and S7-200 areas
    procedure ContadorEhEnderecadoPeloNumeroDoElemento;
    procedure ContadoresDiferentesGeramPedidosDiferentes;
    procedure TemporizadorUsaASuaPropriaArea;
    procedure EscritaEmContadorTambemUsaONumeroDoElemento;
    procedure AreaAnalogicaDoS7200EnderecaEmBits;
    procedure LeituraEEscritaConcordamNoTamanhoDoContador;
    procedure TamanhoDoItemDeContadorVaiEmElementos;
    procedure AreaAnalogicaDoS7200TambemContaEmElementos;
    procedure AreaDeBytesContaEmBytes;
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

procedure TTestS7Family.CabecalhoDePduTemDezBytes;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 1);
  //$32 identifica a PDU; depois tipo, dois reservados, numero, e os dois
  //tamanhos (parametro e dado), ainda zerados.
  AssertBytesEqual('type 1 header', BytesOf('32 01 00 00 00 00 00 00 00 00'), msg);
end;

procedure TTestS7Family.CabecalhoTipoDoisReservaOsBytesDeErro;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 2);
  //tipos 2 e 3 tem dois bytes a mais, para o codigo de erro
  AssertBytesEqual('type 2 header', BytesOf('32 02 00 00 00 00 00 00 00 00 00 00'), msg);
end;

procedure TTestS7Family.TrocaDeBytesEmWord;
begin
  //a PDU e' big-endian; os records sao lidos na ordem da maquina, entao todo
  //campo de 16 bits passa por aqui.
  AssertEquals('1234', $3412, FDrv.Swap($1234));
  AssertEquals('00FF', $FF00, FDrv.Swap($00FF));
  AssertEquals('0000', $0000, FDrv.Swap($0000));
end;

procedure TTestS7Family.PedidoDeLeituraUsaAFuncao04;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepRead(msg);
  //tamanho de parametro 2 (big-endian nos bytes 6 e 7), funcao $04, zero itens
  AssertBytesEqual('empty read request',
                   BytesOf('32 01 00 00 00 00 00 02 00 00 04 00'), msg);
end;

procedure TTestS7Family.PedidoDeEscritaUsaAFuncao05;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepWrite(msg);
  AssertBytesEqual('empty write request',
                   BytesOf('32 01 00 00 00 00 00 02 00 00 05 00'), msg);
end;

procedure TTestS7Family.ItemDeLeituraDeDbMontaOFrame;
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

procedure TTestS7Family.EnderecoInicialVaiEmBits;
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

procedure TTestS7Family.EnderecoGrandeUsaOTerceiroByte;
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

procedure TTestS7Family.AreaDeMerkersNaoLevaNumeroDeDb;
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

procedure TTestS7Family.CadaItemIncrementaAContagemEOTamanho;
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

procedure TTestS7Family.EscritaLevaParametroEDado;
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

procedure TTestS7Family.ParametrosSaoConcatenadosNaOrdem;
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

procedure TTestS7Family.DadoEntraDepoisDoParametro;
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

procedure TTestS7Family.SetupPduInformaOsTamanhos;
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

procedure TTestS7Family.SetupPduDeTipoDoisTemCabecalhoDeDozeBytes;
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

procedure TTestS7Family.ContadorEhEnderecadoPeloNumeroDoElemento;
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

procedure TTestS7Family.ContadoresDiferentesGeramPedidosDiferentes;
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

procedure TTestS7Family.TemporizadorUsaASuaPropriaArea;
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

procedure TTestS7Family.EscritaEmContadorTambemUsaONumeroDoElemento;
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

procedure TTestS7Family.AreaAnalogicaDoS7200EnderecaEmBits;
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

procedure TTestS7Family.LeituraEEscritaConcordamNoTamanhoDoContador;
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

procedure TTestS7Family.TamanhoDoItemDeContadorVaiEmElementos;
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

procedure TTestS7Family.AreaAnalogicaDoS7200TambemContaEmElementos;
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

procedure TTestS7Family.AreaDeBytesContaEmBytes;
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

initialization
  RegisterTest(TTestS7Family);

finalization
  FreeAndNil(DriverCompartilhado);

end.
