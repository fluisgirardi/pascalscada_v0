{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da interpretacao de resposta do TMelsecTCPDriver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Como no S7, decodificar nao devolve nada util por si: o que interessa fica
  nos gerenciadores de memoria do driver. Os testes entregam a resposta bruta
  a DecodePkg e leem de volta por DoGetValue, os dois protegidos e expostos
  por uma sonda.

  A resposta do quadro 3E binario e' o subcabecalho $D000, rede, CLP, modulo,
  estacao, o tamanho, o codigo de termino em dois bytes e so entao os dados,
  a partir do deslocamento 11. Registradores vem em palavras de dois bytes,
  do menos para o mais significativo; bits vem dois por byte, um em cada
  metade.
}
{$ELSE}
{:
  @abstract(TMelsecTCPDriver response parsing tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  As with S7, decoding returns nothing useful on its own: what matters ends up
  in the driver's memory managers. These tests hand the raw answer to
  DecodePkg and read it back through DoGetValue, both protected and exposed by
  a probe.

  A 3E binary response is the $D000 subheader, network, PLC, module, station,
  the length, a two byte end code and only then the data, from offset 11 on.
  Registers come as two byte words, low byte first; bits come two per byte,
  one in each nibble.
}
{$ENDIF}
unit ut.melsecresposta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, MelsecTCP, PLCMemoryManager,
  testsupport.bytes, testsupport.protocol;

type

  { TMelsecRespostaProbe }

  TMelsecRespostaProbe = class(TMelsecTCPDriver)
  public
    procedure NewPLC(aEstacao:LongInt);
    procedure RegisterD(aEndereco, aTamanho:LongInt);
    procedure RegisterM(aEndereco, aTamanho:LongInt);
    function  DecodeIt(const aPedido, aResposta:BYTES; out aValores:TArrayOfDouble):TProtocolIOResult;
    function  DecodePacket(aPkg:TIOPacket; out aValores:TArrayOfDouble):TProtocolIOResult;
    function  ReadFromTheManager(const aTagRec:TTagRec; out aResultado:TProtocolIOResult):TArrayOfDouble;
  end;

  { TTestMelsecResposta }

  TTestMelsecResposta = class(TTestCase)
  private
    FDrv:TMelsecRespostaProbe;
    //: quadro de pedido de leitura, como EncodePkg o monta
    function  ReadRequest(aSubComando, aDispositivo, aEndereco, aQuantidade:LongInt):BYTES;
    //: quadro de resposta com codigo de termino zero e os dados dados
    function  AnswerOf(const aDados:BYTES; aCodigoDeTermino:LongInt=0):BYTES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //dados que chegam ao gerenciador / data reaching the memory manager
    procedure ARegisterReadReachesTheManager;
    procedure ARegisterComesLowByteFirst;
    procedure TheAddressMovesOnWithEveryRegister;
    procedure ABitReadReachesTheManager;

    //falhas / failures
    procedure AWriteFailureBecomesAProtocolResult;
    procedure ADifferentChannelInTheAnswerIsRefused;
    procedure ATimeoutOnTheReadBecomesATimeout;
    procedure ANonZeroEndCodeIsAnError;

    //o que volta pelo parametro de saida / what comes back through the out param
    procedure TheValuesReturnedHaveOnePerPoint;

    //resposta curta / short response
    procedure AnAnswerShorterThanTheHeaderIsRefused;
    procedure AnAnswerWithAHeaderButNoDataIsRefused;
    procedure ARequestBelowTheMinimumIsNotDecoded;
  end;

implementation

{ TMelsecRespostaProbe }

procedure TMelsecRespostaProbe.NewPLC(aEstacao:LongInt);
var
  plc:LongInt;

  function NovoGerenciador:TPLCMemoryManager;
  begin
    Result:=TPLCMemoryManager.Create;
    Result.MaxBlockItems:=10;
    Result.MaxHole:=10;
  end;

begin
  plc:=Length(PMelsecPLC);
  SetLength(PMelsecPLC, plc+1);
  PMelsecPLC[plc].Station     :=aEstacao;
  PMelsecPLC[plc].OutPuts_M   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_SM  :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_L   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_F   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_V   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_X   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_Y   :=NovoGerenciador;
  PMelsecPLC[plc].OutPuts_B   :=NovoGerenciador;
  PMelsecPLC[plc].Registers_D :=NovoGerenciador;
  PMelsecPLC[plc].Registers_SD:=NovoGerenciador;
end;

procedure TMelsecRespostaProbe.RegisterD(aEndereco, aTamanho:LongInt);
begin
  PMelsecPLC[0].Registers_D.AddAddress(aEndereco, aTamanho, 1, 1000);
end;

procedure TMelsecRespostaProbe.RegisterM(aEndereco, aTamanho:LongInt);
begin
  PMelsecPLC[0].OutPuts_M.AddAddress(aEndereco, aTamanho, 1, 1000);
end;

function TMelsecRespostaProbe.DecodeIt(const aPedido, aResposta:BYTES; out aValores:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DecodePkg(IOPacketFor(aPedido, aResposta), aValores);
end;

function TMelsecRespostaProbe.DecodePacket(aPkg:TIOPacket; out aValores:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DecodePkg(aPkg, aValores);
end;

function TMelsecRespostaProbe.ReadFromTheManager(const aTagRec:TTagRec; out aResultado:TProtocolIOResult):TArrayOfDouble;
var
  leitura:TScanReadRec;
begin
  leitura.Values:=nil;
  leitura.LastQueryResult:=ioNone;
  leitura.ClkMonotonicTStamp:=0;

  DoGetValue(aTagRec, leitura);

  aResultado:=leitura.LastQueryResult;
  Result:=leitura.Values;
end;

{ TTestMelsecResposta }

//DecodePkg escreve sempre em PMelsecPLC[0], entao aqui nao da' para separar
//os testes por estacao como no S7: e' um so' CLP, e cada teste usa a sua
//faixa de enderecos.
var
  DriverCompartilhado:TMelsecRespostaProbe = nil;

procedure TTestMelsecResposta.SetUp;
begin
  if DriverCompartilhado=nil then begin
    DriverCompartilhado:=TMelsecRespostaProbe.Create(nil);
    DriverCompartilhado.NewPLC(1);
  end;
  FDrv:=DriverCompartilhado;
end;

procedure TTestMelsecResposta.TearDown;
begin
  FDrv:=nil;
end;

function TTestMelsecResposta.ReadRequest(aSubComando, aDispositivo, aEndereco, aQuantidade:LongInt):BYTES;
begin
  Result:=nil;
  SetLength(Result, 22);
  Result[00]:=$50;               //subcabecalho
  Result[01]:=$00;
  Result[02]:=0;                 //rede
  Result[03]:=$FF;               //CLP
  Result[04]:=$FF;               //modulo de E/S
  Result[05]:=$03;
  Result[06]:=0;                 //canal
  Result[07]:=$0C;               //tamanho
  Result[08]:=$00;
  Result[09]:=$10;               //temporizador da CPU
  Result[10]:=$00;
  Result[11]:=$01;               //comando: leitura em bloco
  Result[12]:=$04;
  Result[13]:=aSubComando;       //0 = palavra, 1 = bit
  Result[14]:=$00;
  Result[15]:=aEndereco and $FF;
  Result[16]:=(aEndereco shr 8) and $FF;
  Result[17]:=(aEndereco shr 16) and $FF;
  Result[18]:=aDispositivo;
  Result[19]:=aQuantidade and $FF;
  Result[20]:=(aQuantidade shr 8) and $FF;
  Result[21]:=$00;
end;

function TTestMelsecResposta.AnswerOf(const aDados:BYTES; aCodigoDeTermino:LongInt=0):BYTES;
var
  c:LongInt;
begin
  Result:=nil;
  SetLength(Result, 11+Length(aDados));
  Result[00]:=$D0;               //subcabecalho de resposta
  Result[01]:=$00;
  Result[02]:=0;                 //rede
  Result[03]:=$FF;               //CLP
  Result[04]:=$FF;               //modulo de E/S
  Result[05]:=$03;
  Result[06]:=0;                 //canal: tem que bater com o do pedido
  Result[07]:=(Length(aDados)+2) and $FF;
  Result[08]:=((Length(aDados)+2) shr 8) and $FF;
  Result[09]:=aCodigoDeTermino and $FF;
  Result[10]:=(aCodigoDeTermino shr 8) and $FF;
  for c:=0 to High(aDados) do
    Result[11+c]:=aDados[c];
end;

procedure TTestMelsecResposta.ARegisterReadReachesTheManager;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FDrv.RegisterD(100, 2);

  //dois registradores: $0A0B em D100 e $0C0D em D101
  FDrv.DecodeIt(ReadRequest(0, $A8, 100, 2),
                   AnswerOf(BytesOf('0B 0A 0D 0C')), valores);

  valores:=FDrv.ReadFromTheManager(TagRecFor(1, $09, 0, 100, 2), res);

  AssertEquals('read with no failure', Ord(ioOk), Ord(res));
  AssertEquals('two registers', 2, Length(valores));
  AssertEquals('D100', $0A0B, valores[0], 0);
  AssertEquals('D101', $0C0D, valores[1], 0);
end;

procedure TTestMelsecResposta.ARegisterComesLowByteFirst;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FDrv.RegisterD(300, 1);

  //34 12 no fio e' $1234, nao $3412
  FDrv.DecodeIt(ReadRequest(0, $A8, 300, 1),
                   AnswerOf(BytesOf('34 12')), valores);

  valores:=FDrv.ReadFromTheManager(TagRecFor(1, $09, 0, 300, 1), res);
  AssertEquals('word assembled the other way round', $1234, valores[0], 0);
end;

procedure TTestMelsecResposta.TheAddressMovesOnWithEveryRegister;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FDrv.RegisterD(400, 3);

  //tres valores distintos: cada um tem que cair no seu proprio endereco
  FDrv.DecodeIt(ReadRequest(0, $A8, 400, 3),
                   AnswerOf(BytesOf('01 00 02 00 03 00')), valores);

  valores:=FDrv.ReadFromTheManager(TagRecFor(1, $09, 0, 400, 3), res);
  AssertEquals('D400', 1, valores[0], 0);
  AssertEquals('D401', 2, valores[1], 0);
  AssertEquals('D402', 3, valores[2], 0);
end;

procedure TTestMelsecResposta.ABitReadReachesTheManager;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FDrv.RegisterM(0, 4);

  //dois bits por byte, o primeiro na metade alta: $10 = 1,0 e $01 = 0,1
  FDrv.DecodeIt(ReadRequest(1, $90, 0, 4),
                   AnswerOf(BytesOf('10 01')), valores);

  valores:=FDrv.ReadFromTheManager(TagRecFor(1, $01, 0, 0, 4), res);

  AssertEquals('M0', 1, valores[0], 0);
  AssertEquals('M1', 0, valores[1], 0);
  AssertEquals('M2', 0, valores[2], 0);
  AssertEquals('M3', 1, valores[3], 0);
end;

procedure TTestMelsecResposta.AWriteFailureBecomesAProtocolResult;
var
  valores:TArrayOfDouble;
  pkg:TIOPacket;
begin
  //nem chegou a sair o pedido
  pkg:=IOPacketFor(ReadRequest(0, $A8, 100, 2), AnswerOf(BytesOf('00 00 00 00')));
  pkg.WriteIOResult:=iorTimeOut;

  AssertEquals('timeout on the write', Ord(ioTimeOut),
               Ord(FDrv.DecodePacket(pkg, valores)));
end;

procedure TTestMelsecResposta.ADifferentChannelInTheAnswerIsRefused;
var
  valores:TArrayOfDouble;
  resp:BYTES;
begin
  //o byte 6 do pedido e da resposta tem que ser o mesmo
  resp:=AnswerOf(BytesOf('00 00 00 00'));
  resp[6]:=$07;

  AssertEquals('channel swapped', Ord(ioCommError),
               Ord(FDrv.DecodeIt(ReadRequest(0, $A8, 100, 2), resp, valores)));
end;

procedure TTestMelsecResposta.ATimeoutOnTheReadBecomesATimeout;
var
  valores:TArrayOfDouble;
  pkg:TIOPacket;
begin
  //o pedido saiu, a resposta nao veio: o buffer chega zerado e o driver tem
  //que dizer que nao houve leitura, e nao entregar os zeros como se fossem
  //valores do CLP
  pkg:=IOPacketFor(ReadRequest(0, $A8, 100, 2), AnswerOf(BytesOf('00 00 00 00')));
  pkg.ReadIOResult:=iorTimeOut;
  pkg.Received:=0;

  AssertEquals('timeout on the read', Ord(ioTimeOut),
               Ord(FDrv.DecodePacket(pkg, valores)));
end;

procedure TTestMelsecResposta.ANonZeroEndCodeIsAnError;
var
  valores:TArrayOfDouble;
begin
  //codigo de termino $0055: o CLP recusou o pedido e nao ha dado nenhum
  //atras dele
  AssertTrue('a PLC error cannot turn into ioOk',
             FDrv.DecodeIt(ReadRequest(0, $A8, 100, 2),
                              AnswerOf(BytesOf(''), $0055), valores) <> ioOk);
end;

procedure TTestMelsecResposta.TheValuesReturnedHaveOnePerPoint;
var
  valores:TArrayOfDouble;
begin
  //quem le pelo caminho sincrono recebe os valores por este parametro: tem
  //que vir um por ponto pedido
  FDrv.RegisterD(500, 3);
  FDrv.DecodeIt(ReadRequest(0, $A8, 500, 3),
                   AnswerOf(BytesOf('01 00 02 00 03 00')), valores);

  AssertEquals('three points asked for, three values', 3, Length(valores));
end;

procedure TTestMelsecResposta.AnAnswerShorterThanTheHeaderIsRefused;
var
  valores:TArrayOfDouble;
  resp:BYTES;
  n:LongInt;
begin
  //antes do primeiro dado vem 11 bytes de cabecalho. Com menos que isso nao ha
  //o que conferir, e indexar o buffer seria ler fora dele
  for n:=0 to 10 do begin
    SetLength(resp, n);
    if n>0 then FillChar(resp[0], n, 0);

    AssertEquals('answer of '+IntToStr(n)+' bytes', Ord(ioCommError),
                 Ord(FDrv.DecodeIt(ReadRequest(0, $A8, 100, 2), resp, valores)));
  end;
end;

procedure TTestMelsecResposta.AnAnswerWithAHeaderButNoDataIsRefused;
var
  valores:TArrayOfDouble;
  n:LongInt;
begin
  //cabecalho inteiro e codigo de termino zero, mas os dois registradores
  //pedidos precisam de quatro bytes atras dele: 11 a 14 sao respostas pela
  //metade, e metade nao serve
  for n:=0 to 3 do
    AssertEquals('header plus '+IntToStr(n)+' bytes de dado', Ord(ioCommError),
                 Ord(FDrv.DecodeIt(ReadRequest(0, $A8, 100, 2),
                                      AnswerOf(Copy(BytesOf('0B 0A 0D 0C'), 0, n)), valores)));
end;

procedure TTestMelsecResposta.ARequestBelowTheMinimumIsNotDecoded;
var
  valores:TArrayOfDouble;
  pedido:BYTES;
begin
  //o pedido diz o endereco, o dispositivo e a quantidade: o menor que o driver
  //monta tem 22 bytes, e os ramos de escrita chegavam a ler o byte 22 de um
  //pedido que so' vai ate' o 21
  pedido:=Copy(ReadRequest(0, $A8, 100, 2), 0, 12);

  AssertEquals('incomplete request', Ord(ioDriverError),
               Ord(FDrv.DecodeIt(pedido, AnswerOf(BytesOf('0B 0A 0D 0C')), valores)));
end;

initialization
  RegisterTest(TTestMelsecResposta);

finalization
  FreeAndNil(DriverCompartilhado);

end.
