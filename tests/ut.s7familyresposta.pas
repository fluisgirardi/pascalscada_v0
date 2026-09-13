{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da interpretacao de resposta do TSiemensProtocolFamily.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Diferente da montagem do pedido, a decodificacao nao devolve nada: ela grava
  os valores nos gerenciadores de memoria internos do driver. Os testes fecham
  o ciclo em dois passos, os dois por metodos protegidos que uma sonda expoe -
  UpdateMemoryManager recebe a resposta bruta, e DoGetValue le de volta o que
  foi guardado. E' o mesmo caminho que o driver percorre numa varredura, sem
  porta e sem CLP.

  As PDUs de resposta seguem o formato S7comm: cabecalho de 12 bytes (tipo 3
  carrega o codigo de erro), o parametro com a funcao e a contagem de itens, e
  cada item de dado com codigo de retorno, tipo de transporte, tamanho e os
  bytes.
}
{$ELSE}
{:
  @abstract(TSiemensProtocolFamily response parsing tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Unlike request building, decoding returns nothing: it stores the values in
  the driver's internal memory managers. These tests close the loop in two
  steps, both through protected methods a probe exposes - UpdateMemoryManager
  takes the raw answer, and DoGetValue reads back what was stored. It is the
  same path the driver walks on a scan, with no port and no PLC.

  The response PDUs follow the S7comm format: a 12 byte header (type 3 carries
  the error code), the parameter with function and item count, and each data
  item with its return code, transport size, length and bytes.
}
{$ENDIF}
unit ut.s7familyresposta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, s7types, s7family, PLCMemoryManager,
  testsupport.bytes, testsupport.protocol;

type

  { TS7AnswerProbe }

  TS7AnswerProbe = class(TSiemensProtocolFamily)
  public
    function  NewPLC(aRack, aSlot, aEstacao:LongInt):LongInt;
    procedure PrepareDB(aPLC, aDBNum, aEndereco, aTamanho:LongInt);
    procedure DecodeIt(const aResposta:BYTES; const aReqList:TS7ReqList);
    function  ReadFromTheManager(const aTagRec:TTagRec; out aResultado:TProtocolIOResult):TArrayOfDouble;
  end;

  { TS7DisposalProbe }

  //conta as remocoes que o destrutor faz, para provar que ele passa por todos
  //os CLPs e nao so por parte deles
  //a contagem fica fora da instancia porque quem conta e' o destrutor: quando
  //ha o que ler, o objeto ja nao existe
  TS7DisposalProbe = class(TSiemensProtocolFamily)
  public
    function  NewPLC(aRack, aSlot, aEstacao:LongInt):LongInt;
    procedure DeletePLC(PLCIndex:Integer); override;
  end;

  { TTestS7FamilyAnswer }

  TTestS7FamilyAnswer = class(TTestCase)
  private
    FDrv:TS7AnswerProbe;
    FPLC:LongInt;
    FEstacao:LongInt;
    function  ListOfOneItem(aDBIdx, aEndereco, aTamanho:LongInt):TS7ReqList;
    function  DBRequest(aEndereco, aTamanho:LongInt):TTagRec;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ADbAnswerReachesTheManager;
    procedure ASizeInBitsIsConvertedToBytes;
    procedure ASizeAlreadyInBytesIsNotDivided;
    procedure APLCErrorBecomesAProtocolResult;
    procedure AnAnswerFromAnotherFunctionIsIgnored;
    procedure MoreItemsInTheAnswerThanInTheRequestDoNotOverflow;

    //ciclo de vida / lifecycle
    procedure TheDestructorRemovesEveryPLC;
  end;

implementation

var
  DescarteRemovidos:LongInt = 0;
  DescarteIndiceInvalido:Boolean = false;

{ TS7DisposalProbe }

function TS7DisposalProbe.NewPLC(aRack, aSlot, aEstacao:LongInt):LongInt;
begin
  Result:=CreatePLC(aRack, aSlot, aEstacao);
end;

procedure TS7DisposalProbe.DeletePLC(PLCIndex:Integer);
begin
  //DeletePLC ignora indice fora da faixa em silencio: sem esta marca, um
  //destrutor que pedisse indices inexistentes passaria por certo
  if (PLCIndex<0) or (PLCIndex>High(FPLCs)) then
    DescarteIndiceInvalido:=true
  else
    inc(DescarteRemovidos);
  inherited DeletePLC(PLCIndex);
end;

{ TS7AnswerProbe }

function TS7AnswerProbe.NewPLC(aRack, aSlot, aEstacao:LongInt):LongInt;
begin
  Result:=CreatePLC(aRack, aSlot, aEstacao);
end;

procedure TS7AnswerProbe.PrepareDB(aPLC, aDBNum, aEndereco, aTamanho:LongInt);
var
  db:LongInt;
begin
  //registra o DB e os enderecos que o gerenciador vai guardar, como um tag
  //faria ao ser cadastrado no driver
  db:=Length(FPLCs[aPLC].DBs);
  SetLength(FPLCs[aPLC].DBs, db+1);
  FPLCs[aPLC].DBs[db].DBNum:=aDBNum;
  FPLCs[aPLC].DBs[db].DBArea:=TPLCMemoryManager.Create;
  FPLCs[aPLC].DBs[db].DBArea.MaxBlockItems:=FPLCs[aPLC].MaxBlockSize;
  FPLCs[aPLC].DBs[db].DBArea.AddAddress(aEndereco, aTamanho, 1, 1000);
end;

procedure TS7AnswerProbe.DecodeIt(const aResposta:BYTES; const aReqList:TS7ReqList);
var
  pedido, resposta:BYTES;
  lista:TS7ReqList;
  valores:TArrayOfDouble;
begin
  pedido:=nil;
  resposta:=Copy(aResposta, 0, Length(aResposta));
  lista:=aReqList;
  valores:=nil;
  UpdateMemoryManager(resposta, pedido, false, lista, valores);
end;

function TS7AnswerProbe.ReadFromTheManager(const aTagRec:TTagRec; out aResultado:TProtocolIOResult):TArrayOfDouble;
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

{ TTestS7FamilyAnswer }

//Criar o driver custa meio segundo (o destrutor espera as threads de
//varredura). Em vez de um driver por teste, um driver para a classe e um CLP
//novo por teste: cada um recebe a sua estacao, entao os gerenciadores de
//memoria de um teste nao enxergam os do outro.
var
  DriverCompartilhado:TS7AnswerProbe = nil;
  UltimaEstacao:LongInt = 0;

procedure TTestS7FamilyAnswer.SetUp;
begin
  if DriverCompartilhado=nil then
    DriverCompartilhado:=TS7AnswerProbe.Create(nil);
  FDrv:=DriverCompartilhado;

  inc(UltimaEstacao);
  FEstacao:=UltimaEstacao;
  FPLC:=FDrv.NewPLC(0, 2, FEstacao);
  FDrv.PrepareDB(FPLC, 1, 0, 4);
end;

procedure TTestS7FamilyAnswer.TearDown;
begin
  FDrv:=nil;
end;

function TTestS7FamilyAnswer.ListOfOneItem(aDBIdx, aEndereco, aTamanho:LongInt):TS7ReqList;
begin
  SetLength(Result, 1);
  Result[0].PLCIdx      :=FPLC;
  Result[0].DBIdx       :=aDBIdx;
  Result[0].ReqType     :=vtS7_DB;
  Result[0].StartAddress:=aEndereco;
  Result[0].Size        :=aTamanho;
end;

function TTestS7FamilyAnswer.DBRequest(aEndereco, aTamanho:LongInt):TTagRec;
begin
  //funcao 4 = area de DB na numeracao interna do driver
  Result:=TagRecFor(FEstacao, 4, 0, aEndereco, aTamanho);
  Result.Rack   :=0;
  Result.Slot   :=2;
  Result.File_DB:=1;
end;

procedure TTestS7FamilyAnswer.ADbAnswerReachesTheManager;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //cabecalho tipo 3 (12 bytes), parametro "leitura, 1 item", e o item de dado
  //com codigo $FF, transporte 4 (contado em bits), 32 bits e os quatro bytes
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 08 00 00' +
                           '04 01' +
                           'FF 04 00 20 0A 0B 0C 0D'),
                   ListOfOneItem(0, 0, 4));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 4), res);

  AssertEquals('read with no failure', Ord(ioOk), Ord(res));
  AssertEquals('number of values', 4, Length(valores));
  AssertEquals('first byte', $0A, valores[0], 0);
  AssertEquals('second byte',  $0B, valores[1], 0);
  AssertEquals('third byte', $0C, valores[2], 0);
  AssertEquals('fourth byte',   $0D, valores[3], 0);
end;

procedure TTestS7FamilyAnswer.ASizeInBitsIsConvertedToBytes;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //transporte 4 declara o tamanho em BITS: 16 bits sao 2 bytes
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 06 00 00' +
                           '04 01' +
                           'FF 04 00 10 AA BB'),
                   ListOfOneItem(0, 0, 2));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 2), res);

  AssertEquals('two bytes kept', $AA, valores[0], 0);
  AssertEquals('and the second one',          $BB, valores[1], 0);
end;

procedure TTestS7FamilyAnswer.ASizeAlreadyInBytesIsNotDivided;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //transporte 3 e 9 ja vem em bytes: 2 significa dois bytes, nao dois bits
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 06 00 00' +
                           '04 01' +
                           'FF 09 00 02 11 22'),
                   ListOfOneItem(0, 0, 2));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 2), res);

  AssertEquals('first byte', $11, valores[0], 0);
  AssertEquals('second byte',  $22, valores[1], 0);
end;

procedure TTestS7FamilyAnswer.APLCErrorBecomesAProtocolResult;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //codigo $05 = endereco de memoria invalido; a falha tem que ficar guardada
  //na area, para quem ler depois saber que o valor nao vale
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 04 00 00' +
                           '04 01' +
                           '05 00 00 00'),
                   ListOfOneItem(0, 0, 4));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 4), res);
  AssertEquals('failure passed on', Ord(ioIllegalMemoryAddress), Ord(res));
end;

procedure TTestS7FamilyAnswer.AnAnswerFromAnotherFunctionIsIgnored;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //parametro com funcao de escrita ($05) numa decodificacao de leitura:
  //o driver tem que largar o pacote sem gravar nada
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 08 00 00' +
                           '05 01' +
                           'FF 04 00 20 0A 0B 0C 0D'),
                   ListOfOneItem(0, 0, 4));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 4), res);
  AssertTrue('nothing can have been stored', (Length(valores)=0) or (valores[0]<>$0A));
end;

procedure TTestS7FamilyAnswer.MoreItemsInTheAnswerThanInTheRequestDoNotOverflow;
var
  valores:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //a resposta diz ter 3 itens, mas so pedimos 1: o driver nao pode andar
  //alem da lista de requisicoes
  FDrv.DecodeIt(BytesOf('32 03 00 00 00 00 00 02 00 08 00 00' +
                           '04 03' +
                           'FF 04 00 20 0A 0B 0C 0D'),
                   ListOfOneItem(0, 0, 4));

  valores:=FDrv.ReadFromTheManager(DBRequest(0, 4), res);
  AssertEquals('the item asked for was processed', $0A, valores[0], 0);
end;

procedure TTestS7FamilyAnswer.TheDestructorRemovesEveryPLC;
var
  drv:TS7DisposalProbe;
begin
  //DeletePLC encurta o vetor a cada chamada; um laco que fixasse o limite no
  //inicio deixaria metade dos CLPs (e os gerenciadores de memoria deles) para
  //tras
  DescarteRemovidos:=0;
  DescarteIndiceInvalido:=false;

  drv:=TS7DisposalProbe.Create(nil);
  drv.NewPLC(0, 2, 1);
  drv.NewPLC(0, 2, 2);
  drv.NewPLC(0, 2, 3);
  drv.NewPLC(0, 2, 4);

  drv.Free;

  AssertEquals('all four PLCs were removed', 4, DescarteRemovidos);
  AssertFalse ('no index out of range', DescarteIndiceInvalido);
end;

initialization
  RegisterTest(TTestS7FamilyAnswer);

finalization
  FreeAndNil(DriverCompartilhado);

end.
