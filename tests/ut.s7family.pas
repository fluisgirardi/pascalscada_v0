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

    //defeito conhecido / known defect
    procedure ContadorDeveEnderecarOContadorPedido;
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
  AssertBytesEqual('cabecalho tipo 1', BytesOf('32 01 00 00 00 00 00 00 00 00'), msg);
end;

procedure TTestS7Family.CabecalhoTipoDoisReservaOsBytesDeErro;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.Header(msg, 2);
  //tipos 2 e 3 tem dois bytes a mais, para o codigo de erro
  AssertBytesEqual('cabecalho tipo 2', BytesOf('32 02 00 00 00 00 00 00 00 00 00 00'), msg);
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
  AssertBytesEqual('pedido de leitura vazio',
                   BytesOf('32 01 00 00 00 00 00 02 00 00 04 00'), msg);
end;

procedure TTestS7Family.PedidoDeEscritaUsaAFuncao05;
var
  msg:BYTES;
begin
  msg:=nil;
  FDrv.PrepWrite(msg);
  AssertBytesEqual('pedido de escrita vazio',
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
  AssertBytesEqual('leitura de DB1.DBB0, 2 bytes',
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
  AssertBytesEqual('leitura a partir do byte 10',
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
  AssertBytesEqual('leitura a partir do byte 8192',
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
  AssertBytesEqual('leitura de MB4',
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
  AssertBytesEqual('duas leituras num pedido so',
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
  AssertBytesEqual('escrita de 2 bytes em DB1.DBB0',
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
  AssertBytesEqual('dois parametros',
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
  AssertBytesEqual('parametro e dado',
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

  AssertTrue('a PDU deve ser reconhecida', FDrv.ReadPDU(msg, pdu, err));
  AssertEquals('tamanho do cabecalho',  10, pdu.header_len);
  AssertEquals('tamanho do parametro',  2,  pdu.param_len);
  AssertEquals('tamanho do dado',       3,  pdu.data_len);
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

  AssertTrue('a PDU deve ser reconhecida', FDrv.ReadPDU(msg, pdu, err));
  AssertEquals('cabecalho de tipo 2', 12, pdu.header_len);
  AssertEquals('tamanho do parametro', 2, pdu.param_len);
end;

procedure TTestS7Family.ContadorDeveEnderecarOContadorPedido;
var
  msgA, msgB:BYTES;
begin
  Ignore('defeito conhecido: em AddToReadRequest (e em AddParamToWriteRequest) o ' +
         'endereco inicial so e' + #39 + ' calculado no ramo "else" do case de areas; ' +
         'para contadores e temporizadores (e para as areas analogicas do S7-200) a ' +
         'variavel intStart fica sem valor e os tres bytes de endereco do item saem ' +
         'com lixo de pilha. Efeito: le/escreve um contador arbitrario. Correcao ' +
         'provavel: atribuir intStart:=iStart nesses ramos (numero do elemento, sem ' +
         'multiplicar por 8). Remova este Ignore depois de corrigir.');

  msgA:=nil;
  FDrv.PrepRead(msgA);
  FDrv.AddReadItem(msgA, vtS7_Counter, 0, 3, 1);

  msgB:=nil;
  FDrv.PrepRead(msgB);
  FDrv.AddReadItem(msgB, vtS7_Counter, 0, 99, 1);

  //contadores diferentes tem que gerar pedidos diferentes
  AssertFalse('o contador 3 e o 99 geraram o mesmo pedido',
              HexOf(msgA)=HexOf(msgB));
end;

initialization
  RegisterTest(TTestS7Family);

finalization
  FreeAndNil(DriverCompartilhado);

end.
