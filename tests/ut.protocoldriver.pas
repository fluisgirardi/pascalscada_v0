{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TProtocolDriver: a base comum de todos os drivers de
            protocolo - cadastro de tags e despacho de leitura/escrita.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Como a classe e' abstrata, os testes usam um driver falso que implementa os
  quatro metodos obrigatorios e anota o que foi chamado. Assim da' pra
  verificar o contrato da base - quem ela aceita como tag, quando chama o
  driver de verdade e o que devolve no callback - sem depender de protocolo
  nenhum.
}
{$ELSE}
{:
  @abstract(TProtocolDriver tests: the common base of every protocol driver -
            tag registration and read/write dispatching.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Since the class is abstract, the tests use a fake driver implementing the
  four required methods and recording what was called. That checks the base's
  contract - which tags it accepts, when it calls the actual driver and what
  it hands back through the callback - with no protocol involved.
}
{$ENDIF}
unit ut.protocoldriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, ProtocolDriver,
  testsupport.bytes, testsupport.protocol;

type

  {$IFDEF PORTUGUES}
  //: Tag minimo: so o que a base exige para aceitar um tag.
  {$ELSE}
  //: Minimal tag: just what the base requires to accept a tag.
  {$ENDIF}

  { TSampleTag }

  TSampleTag = class(TTag, IScanableTagInterface)
  private
    FValido:Boolean;
  public
    function  RemainingMiliseconds:Int64;
    function  RemainingMilisecondsForNextScan:Int64;
    function  IsValidTag:Boolean;
    function  IsMyCallBack(Cback:TTagCommandCallBack):Boolean;
    procedure SetTagValidity(TagValidity:Boolean);
    procedure BuildTagRec(out tr:TTagRec; Count, OffSet:LongInt);
    function  GetLastUpdateTimestamp:QWord;
    function  GetUpdateTime:Int64;
    //: o driver marca a validade ao cadastrar e ao remover o tag
    property  Valido:Boolean read FValido;
  end;

  {$IFDEF PORTUGUES}
  //: Tag que nao implementa a interface de scan - a base tem que recusar.
  {$ELSE}
  //: A tag not implementing the scan interface - the base must refuse it.
  {$ENDIF}
  TTagWithoutScan = class(TTag);

  {$IFDEF PORTUGUES}
  //: Driver falso: fecha os metodos abstratos e anota o que foi chamado.
  {$ELSE}
  //: Fake driver: closes the abstract methods and records what was called.
  {$ENDIF}

  { TTestDriver }

  TTestDriver = class(TProtocolDriver)
  public
    Leituras, Escritas:LongInt;
    UltimoTagRec:TTagRec;
    UltimosValores:TArrayOfDouble;
    ResultadoProgramado:TProtocolIOResult;
    ValorProgramado:Double;
    //resultado e endereco de cada DoWrite, na ordem em que foram chamados
    //result for, and address of, each DoWrite call, in the order they were made
    ResultadosPorEscrita:array of TProtocolIOResult;
    EnderecosEscritos:array of LongInt;
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    function  SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    //expoe o gancho protegido, que e' o que os drivers de verdade sobrescrevem
    procedure RegisterTagIn(aTag:TTag; aValido:Boolean);
    procedure CopyPacket(const aOrigem:TIOPacket; var aDestino:TIOPacket);
  end;

  { TTestBatchDriver }

  //Um driver que sabe reunir as escritas: sobrescreve DoMultiWrite e nao usa DoWrite.
  //A driver that knows how to gather writes: overrides DoMultiWrite and never uses DoWrite.
  TTestBatchDriver = class(TTestDriver)
  public
    MultiCalls, LastBatchSize:LongInt;
    LastSync:Boolean;
  protected
    procedure DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean); override;
  end;

  { TTestProtocolDriver }

  TTestProtocolDriver = class(TTestCase)
  private
    FDrv:TTestDriver;
    FTag:TSampleTag;
    //anotacoes do callback / callback bookkeeping
    FCallbacks:LongInt;
    FUltimoResultado:TProtocolIOResult;
    FUltimoComando:TTagCommand;
    FResultadosDosCallbacks:array of TProtocolIOResult;
    FIdsDosCallbacks:array of LongWord;
    procedure CallbackIn(const ReqID:LongWord; Values:TArrayOfDouble;
                      ValuesTimeStamp:QWord; TagCommand:TTagCommand;
                      LastResult:TProtocolIOResult; OffSet:LongInt);
    function  TagRecWithCallback:TTagRec;
    function  WriteAt(aAddress:LongInt; aValue:Double; aWithCallback:Boolean = true):TMultiWriteRec;
    function  WaitForCallbacks(aCount, aDeadlineMs:LongInt):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //cadastro de tags / tag registration
    procedure ATagWithNoScanInterfaceIsRefused;
    procedure ARegisteredTagEntersTheList;
    procedure ATagRegisteredTwiceIsRefused;
    procedure ARemovedTagLeavesTheList;
    procedure RemovingATagOfAnotherDriverDoesNotBreak;
    procedure RegisteringMarksTheTagAsValid;
    procedure RemovalInvalidatesTheTag;

    //acesso a lista / list access
    procedure ATagIsFoundByName;
    procedure ANameThatDoesNotExistReturnsNil;
    procedure AnIndexOutsideTheListRaises;

    //despacho de leitura e escrita / read and write dispatching
    procedure AReadCallsTheDriverAndReportsThroughTheCallback;
    procedure AWriteCallsTheDriverWithTheValues;
    procedure TheDriverResultReachesTheCallback;

    //escrita em lote / batch write
    procedure AMultiWriteByDefaultWritesEachItemInOrder;
    procedure EachItemOfAMultiWriteKeepsItsOwnResult;
    procedure EachItemOfAMultiWriteIsReportedThroughItsOwnCallback;
    procedure AnItemWithNoCallbackIsWrittenAllTheSame;
    procedure AnEmptyMultiWriteDoesNothing;
    procedure ADriverThatBatchesGetsOneCallWithAllTheItems;
    procedure ReadOnlyRefusesEveryItemOfAMultiWrite;
    procedure ReadOnlyRefusesAScanMultiWriteAndTellsEachTag;
    procedure AScanMultiWriteReachesTheDriverAndEachTagGetsItsResult;
    procedure AScanMultiWriteGivesEachItemItsOwnRequestNumber;
    procedure AnEmptyScanMultiWriteDoesNothing;

    //modo somente leitura / read only mode
    procedure ReadOnlyDoesNotCallTheDriverWrite;
    procedure ReadOnlyStillReads;
    procedure ReadOnlyWithNoCallbackMustNotRaise;

    //atualizacao em lote / batch update
    procedure RegisteringInBulkDoesNotHang;

    //utilitarios / helpers
    procedure CopyingAPacketTakesEveryField;
    procedure BaseDefaults;
  end;

implementation

{ TSampleTag }

function TSampleTag.RemainingMiliseconds:Int64;
begin
  Result:=0;
end;

function TSampleTag.RemainingMilisecondsForNextScan:Int64;
begin
  Result:=0;
end;

function TSampleTag.IsValidTag:Boolean;
begin
  Result:=FValido;
end;

function TSampleTag.IsMyCallBack(Cback:TTagCommandCallBack):Boolean;
begin
  Result:=false;
end;

procedure TSampleTag.SetTagValidity(TagValidity:Boolean);
begin
  FValido:=TagValidity;
end;

procedure TSampleTag.BuildTagRec(out tr:TTagRec; Count, OffSet:LongInt);
begin
  tr:=TagRecFor(1, 3, 0, 0, 1);
end;

function TSampleTag.GetLastUpdateTimestamp:QWord;
begin
  Result:=0;
end;

function TSampleTag.GetUpdateTime:Int64;
begin
  Result:=1000;
end;

{ TTestDriver }

constructor TTestDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Leituras:=0;
  Escritas:=0;
  ResultadoProgramado:=ioOk;
  ValorProgramado:=0;
end;

procedure TTestDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=1;
end;

procedure TTestDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin
  SetLength(values.Values, 1);
  values.Values[0]:=ValorProgramado;
  values.LastQueryResult:=ResultadoProgramado;
end;

function TTestDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  inc(Escritas);
  UltimoTagRec:=tagrec;
  UltimosValores:=Copy(Values, 0, Length(Values));
  SetLength(EnderecosEscritos, Length(EnderecosEscritos)+1);
  EnderecosEscritos[High(EnderecosEscritos)]:=tagrec.Address;
  if Escritas<=Length(ResultadosPorEscrita) then
    Result:=ResultadosPorEscrita[Escritas-1]
  else
    Result:=ResultadoProgramado;
end;

function TTestDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  inc(Leituras);
  UltimoTagRec:=tagrec;
  SetLength(Values, 1);
  Values[0]:=ValorProgramado;
  Result:=ResultadoProgramado;
end;

function TTestDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

{ TTestBatchDriver }

procedure TTestBatchDriver.DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean);
var
  i:LongInt;
begin
  inc(MultiCalls);
  LastBatchSize:=Length(Writes);
  LastSync:=Sync;
  for i:=0 to High(Writes) do
    Writes[i].Result:=ioOk;
end;

procedure TTestDriver.RegisterTagIn(aTag:TTag; aValido:Boolean);
begin
  DoAddTag(aTag, aValido);
end;

procedure TTestDriver.CopyPacket(const aOrigem:TIOPacket; var aDestino:TIOPacket);
begin
  CopyIOPacket(aOrigem, aDestino);
end;

{ TTestProtocolDriver }

procedure TTestProtocolDriver.CallbackIn(const ReqID:LongWord; Values:TArrayOfDouble;
  ValuesTimeStamp:QWord; TagCommand:TTagCommand; LastResult:TProtocolIOResult;
  OffSet:LongInt);
begin
  inc(FCallbacks);
  FUltimoResultado:=LastResult;
  FUltimoComando:=TagCommand;
  SetLength(FResultadosDosCallbacks, Length(FResultadosDosCallbacks)+1);
  FResultadosDosCallbacks[High(FResultadosDosCallbacks)]:=LastResult;
  SetLength(FIdsDosCallbacks, Length(FIdsDosCallbacks)+1);
  FIdsDosCallbacks[High(FIdsDosCallbacks)]:=ReqID;
end;

function TTestProtocolDriver.TagRecWithCallback:TTagRec;
begin
  Result:=TagRecFor(1, 3, 6, 0, 1);
  Result.CallBack:=@CallbackIn;
end;

procedure TTestProtocolDriver.SetUp;
begin
  FDrv:=TTestDriver.Create(nil);
  FTag:=TSampleTag.Create(nil);
  FTag.Name:='TagDeTeste';
  FCallbacks:=0;
  FUltimoResultado:=ioNone;
  SetLength(FResultadosDosCallbacks, 0);
  SetLength(FIdsDosCallbacks, 0);
end;

procedure TTestProtocolDriver.TearDown;
begin
  //O driver guarda ponteiro cru para os tags e, no destrutor, converte cada um
  //para TPLCTag sem checar. Um tag liberado enquanto ainda esta cadastrado
  //vira ponteiro pendurado - os tags de verdade se descadastram sozinhos no
  //proprio destrutor, e aqui fazemos a mesma coisa na mao.
  if (FDrv<>nil) and (FTag<>nil) and FDrv.IsMyTag(FTag) then
    FDrv.RemoveTag(FTag);

  FreeAndNil(FTag);
  FreeAndNil(FDrv);
end;

procedure TTestProtocolDriver.ATagWithNoScanInterfaceIsRefused;
var
  semScan:TTagWithoutScan;
  recusou:Boolean;
begin
  //um tag que nao pode ser varrido nao tem o que fazer num driver
  semScan:=TTagWithoutScan.Create(nil);
  try
    recusou:=false;
    try
      FDrv.AddTag(semScan);
    except
      on E:Exception do
        recusou:=true;
    end;
    AssertTrue('a tag with no IScanableTagInterface must be refused', recusou);
    AssertEquals('and it must not enter the list', 0, FDrv.TagCount);
  finally
    semScan.Free;
  end;
end;

procedure TTestProtocolDriver.ARegisteredTagEntersTheList;
begin
  FDrv.AddTag(FTag);
  AssertEquals('number of tags', 1, FDrv.TagCount);
  AssertTrue  ('the driver knows the tag', FDrv.IsMyTag(FTag));
  AssertSame  ('the tag in the list', FTag, FDrv.Tag[0]);
  AssertEquals('tag name', 'TagDeTeste', FDrv.TagName[0]);
end;

procedure TTestProtocolDriver.ATagRegisteredTwiceIsRefused;
var
  recusou:Boolean;
begin
  FDrv.AddTag(FTag);

  recusou:=false;
  try
    FDrv.AddTag(FTag);
  except
    on E:Exception do
      recusou:=true;
  end;

  AssertTrue  ('registering twice must be refused', recusou);
  AssertEquals('and the tag is still in the list only once', 1, FDrv.TagCount);
end;

procedure TTestProtocolDriver.ARemovedTagLeavesTheList;
begin
  FDrv.AddTag(FTag);
  FDrv.RemoveTag(FTag);
  AssertEquals('number of tags', 0, FDrv.TagCount);
  AssertFalse ('the driver no longer knows the tag', FDrv.IsMyTag(FTag));
end;

procedure TTestProtocolDriver.RemovingATagOfAnotherDriverDoesNotBreak;
var
  outro:TSampleTag;
begin
  FDrv.AddTag(FTag);

  outro:=TSampleTag.Create(nil);
  try
    //remover um tag que nunca foi cadastrado e' inocuo, nao erro
    FDrv.RemoveTag(outro);
    AssertEquals('the list does not change', 1, FDrv.TagCount);
  finally
    outro.Free; //nunca entrou na lista, entao pode ir embora direto
  end;
end;

procedure TTestProtocolDriver.RegisteringMarksTheTagAsValid;
begin
  //o gancho protegido e' onde os drivers dizem se o endereco do tag serve
  FDrv.RegisterTagIn(FTag, true);
  AssertTrue('tag registered as valid', FTag.Valido);
end;

procedure TTestProtocolDriver.RemovalInvalidatesTheTag;
begin
  FDrv.RegisterTagIn(FTag, true);
  FDrv.RemoveTag(FTag);
  AssertFalse('a removed tag stops being valid', FTag.Valido);
end;

procedure TTestProtocolDriver.ATagIsFoundByName;
begin
  FDrv.AddTag(FTag);
  AssertSame('lookup by name', FTag, FDrv.TagByName['TagDeTeste']);
end;

procedure TTestProtocolDriver.ANameThatDoesNotExistReturnsNil;
begin
  FDrv.AddTag(FTag);
  AssertTrue('a name that does not exist', FDrv.TagByName['NaoExiste']=nil);
end;

procedure TTestProtocolDriver.AnIndexOutsideTheListRaises;
var
  estourou:Boolean;
begin
  FDrv.AddTag(FTag);

  estourou:=false;
  try
    FDrv.Tag[7];
  except
    on E:Exception do
      estourou:=true;
  end;
  AssertTrue('an index outside the list must raise an exception', estourou);
end;

procedure TTestProtocolDriver.AReadCallsTheDriverAndReportsThroughTheCallback;
begin
  FDrv.Read(TagRecWithCallback);

  AssertEquals('calls to the driver DoRead', 1, FDrv.Leituras);
  AssertEquals('callbacks received', 1, FCallbacks);
  AssertEquals('command reported', Ord(tcRead), Ord(FUltimoComando));
end;

procedure TTestProtocolDriver.AWriteCallsTheDriverWithTheValues;
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 2);
  valores[0]:=10;
  valores[1]:=20;

  FDrv.Write(TagRecWithCallback, valores);

  AssertEquals('calls to the driver DoWrite', 1, FDrv.Escritas);
  AssertEquals('number of values passed on', 2, Length(FDrv.UltimosValores));
  AssertEquals('first value', 10, FDrv.UltimosValores[0], 0);
  AssertEquals('second value',  20, FDrv.UltimosValores[1], 0);
  AssertEquals('command reported', Ord(tcWrite), Ord(FUltimoComando));
end;

procedure TTestProtocolDriver.TheDriverResultReachesTheCallback;
begin
  //o que o driver concreto devolve tem que chegar inteiro em quem pediu
  FDrv.ResultadoProgramado:=ioTimeOut;
  FDrv.Read(TagRecWithCallback);
  AssertEquals('result in the callback', Ord(ioTimeOut), Ord(FUltimoResultado));
end;

function TTestProtocolDriver.WriteAt(aAddress:LongInt; aValue:Double; aWithCallback:Boolean):TMultiWriteRec;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Tag:=TagRecFor(1, 3, 6, aAddress, 1);
  if aWithCallback then
    Result.Tag.CallBack:=@CallbackIn;
  SetLength(Result.Values, 1);
  Result.Values[0]:=aValue;
  Result.Result:=ioNone;
end;

//Os resultados das escritas por scan chegam em lote pela thread principal: o teste
//precisa bombear a fila de sincronizacao enquanto espera.
//Scan write results arrive in a batch through the main thread: the test has to pump
//the synchronization queue while it waits.
function TTestProtocolDriver.WaitForCallbacks(aCount, aDeadlineMs:LongInt):Boolean;
var
  inicio:QWord;
begin
  inicio:=GetTickCount64;
  while (FCallbacks<aCount) and (GetTickCount64-inicio<QWord(aDeadlineMs)) do begin
    CheckSynchronize(5);
    Sleep(2);
  end;
  Result:=FCallbacks>=aCount;
end;

procedure TTestProtocolDriver.AMultiWriteByDefaultWritesEachItemInOrder;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 3);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);
  lote[2]:=WriteAt(30, 3);

  FDrv.MultiWrite(lote);

  //quem nao sobrescreve DoMultiWrite continua escrevendo um a um, na ordem pedida
  AssertEquals('one DoWrite per item', 3, FDrv.Escritas);
  AssertEquals('first',  10, FDrv.EnderecosEscritos[0]);
  AssertEquals('second', 20, FDrv.EnderecosEscritos[1]);
  AssertEquals('third',  30, FDrv.EnderecosEscritos[2]);
  AssertEquals('last values', 3, FDrv.UltimosValores[0], 0);
end;

procedure TTestProtocolDriver.EachItemOfAMultiWriteKeepsItsOwnResult;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 3);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);
  lote[2]:=WriteAt(30, 3);
  SetLength(FDrv.ResultadosPorEscrita, 3);
  FDrv.ResultadosPorEscrita[0]:=ioOk;
  FDrv.ResultadosPorEscrita[1]:=ioTimeOut;
  FDrv.ResultadosPorEscrita[2]:=ioOk;

  FDrv.MultiWrite(lote);

  //uma escrita que falha nao impede nem altera as outras
  AssertEquals('item 1', Ord(ioOk),      Ord(lote[0].Result));
  AssertEquals('item 2', Ord(ioTimeOut), Ord(lote[1].Result));
  AssertEquals('item 3', Ord(ioOk),      Ord(lote[2].Result));
  AssertEquals('all three were attempted', 3, FDrv.Escritas);
end;

procedure TTestProtocolDriver.EachItemOfAMultiWriteIsReportedThroughItsOwnCallback;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 3);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);
  lote[2]:=WriteAt(30, 3);
  SetLength(FDrv.ResultadosPorEscrita, 3);
  FDrv.ResultadosPorEscrita[0]:=ioOk;
  FDrv.ResultadosPorEscrita[1]:=ioTimeOut;
  FDrv.ResultadosPorEscrita[2]:=ioOk;

  FDrv.MultiWrite(lote);

  AssertEquals('one callback per item', 3, FCallbacks);
  AssertEquals('as a synchronous write', Ord(tcWrite), Ord(FUltimoComando));
  AssertEquals('callback 1', Ord(ioOk),      Ord(FResultadosDosCallbacks[0]));
  AssertEquals('callback 2', Ord(ioTimeOut), Ord(FResultadosDosCallbacks[1]));
  AssertEquals('callback 3', Ord(ioOk),      Ord(FResultadosDosCallbacks[2]));
end;

procedure TTestProtocolDriver.AnItemWithNoCallbackIsWrittenAllTheSame;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 2);
  lote[0]:=WriteAt(10, 1, false);
  lote[1]:=WriteAt(20, 2, true);

  FDrv.MultiWrite(lote);

  AssertEquals('both written', 2, FDrv.Escritas);
  AssertEquals('only the one with a callback is told', 1, FCallbacks);
end;

procedure TTestProtocolDriver.AnEmptyMultiWriteDoesNothing;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 0);
  FDrv.MultiWrite(lote);
  AssertEquals('nothing written', 0, FDrv.Escritas);
  AssertEquals('nobody told', 0, FCallbacks);
end;

procedure TTestProtocolDriver.ADriverThatBatchesGetsOneCallWithAllTheItems;
var
  drv:TTestBatchDriver;
  lote:TMultiWriteArray;
begin
  drv:=TTestBatchDriver.Create(nil);
  try
    SetLength(lote, 3);
    lote[0]:=WriteAt(10, 1);
    lote[1]:=WriteAt(20, 2);
    lote[2]:=WriteAt(30, 3);

    drv.MultiWrite(lote);

    AssertEquals('one DoMultiWrite',        1, drv.MultiCalls);
    AssertEquals('with every item',         3, drv.LastBatchSize);
    AssertTrue  ('synchronously',           drv.LastSync);
    AssertEquals('DoWrite is not used',     0, drv.Escritas);
    AssertEquals('results come from the driver', Ord(ioOk), Ord(lote[1].Result));
    AssertEquals('and still one callback each', 3, FCallbacks);
  finally
    drv.Free;
  end;
end;

procedure TTestProtocolDriver.ReadOnlyRefusesEveryItemOfAMultiWrite;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 2);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);

  FDrv.ReadOnly:=true;
  FDrv.MultiWrite(lote);

  AssertEquals('the driver must not be called', 0, FDrv.Escritas);
  AssertEquals('every tag is told', 2, FCallbacks);
  AssertEquals('item 1', Ord(ioReadOnlyProtocol), Ord(lote[0].Result));
  AssertEquals('item 2', Ord(ioReadOnlyProtocol), Ord(lote[1].Result));
  AssertEquals('reason in the callback', Ord(ioReadOnlyProtocol), Ord(FUltimoResultado));
end;

procedure TTestProtocolDriver.ReadOnlyRefusesAScanMultiWriteAndTellsEachTag;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 2);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);

  FDrv.ReadOnly:=true;
  AssertEquals('no request number', 0, FDrv.ScanMultiWrite(lote));

  AssertEquals('the driver must not be called', 0, FDrv.Escritas);
  AssertEquals('every tag is told', 2, FCallbacks);
  AssertEquals('as a scan write', Ord(tcScanWrite), Ord(FUltimoComando));
  AssertEquals('reason', Ord(ioReadOnlyProtocol), Ord(FUltimoResultado));
end;

procedure TTestProtocolDriver.AScanMultiWriteReachesTheDriverAndEachTagGetsItsResult;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 3);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);
  lote[2]:=WriteAt(30, 3);
  SetLength(FDrv.ResultadosPorEscrita, 3);
  FDrv.ResultadosPorEscrita[0]:=ioOk;
  FDrv.ResultadosPorEscrita[1]:=ioTimeOut;
  FDrv.ResultadosPorEscrita[2]:=ioOk;

  AssertTrue('a request number', FDrv.ScanMultiWrite(lote)<>0);

  //a escrita acontece na thread do driver e o resultado volta pela thread de atualizacao
  AssertTrue  ('every tag was told', WaitForCallbacks(3, 3000));
  AssertEquals('all three were written', 3, FDrv.Escritas);
  AssertEquals('in order', 10, FDrv.EnderecosEscritos[0]);
  AssertEquals('in order', 30, FDrv.EnderecosEscritos[2]);
  AssertEquals('as a scan write', Ord(tcScanWrite), Ord(FUltimoComando));
  AssertEquals('callback 1', Ord(ioOk),      Ord(FResultadosDosCallbacks[0]));
  AssertEquals('callback 2', Ord(ioTimeOut), Ord(FResultadosDosCallbacks[1]));
  AssertEquals('callback 3', Ord(ioOk),      Ord(FResultadosDosCallbacks[2]));
end;

procedure TTestProtocolDriver.AScanMultiWriteGivesEachItemItsOwnRequestNumber;
var
  lote:TMultiWriteArray;
  primeiro:Cardinal;
begin
  SetLength(lote, 3);
  lote[0]:=WriteAt(10, 1);
  lote[1]:=WriteAt(20, 2);
  lote[2]:=WriteAt(30, 3);

  primeiro:=FDrv.ScanMultiWrite(lote);

  AssertTrue('every tag was told', WaitForCallbacks(3, 3000));
  AssertEquals('first item',  primeiro,     FIdsDosCallbacks[0]);
  AssertEquals('second item', primeiro+1,   FIdsDosCallbacks[1]);
  AssertEquals('third item',  primeiro+2,   FIdsDosCallbacks[2]);
end;

procedure TTestProtocolDriver.AnEmptyScanMultiWriteDoesNothing;
var
  lote:TMultiWriteArray;
begin
  SetLength(lote, 0);
  AssertEquals('no request number', 0, FDrv.ScanMultiWrite(lote));
  AssertEquals('nothing written', 0, FDrv.Escritas);
end;

procedure TTestProtocolDriver.ReadOnlyDoesNotCallTheDriverWrite;
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 1);
  valores[0]:=1;

  FDrv.ReadOnly:=true;
  FDrv.Write(TagRecWithCallback, valores);

  AssertEquals('the driver must not be called', 0, FDrv.Escritas);
  AssertEquals('but whoever asked must be told', 1, FCallbacks);
  AssertEquals('reason for the refusal', Ord(ioReadOnlyProtocol), Ord(FUltimoResultado));
end;

procedure TTestProtocolDriver.ReadOnlyStillReads;
begin
  FDrv.ReadOnly:=true;
  FDrv.Read(TagRecWithCallback);
  AssertEquals('reading is still allowed', 1, FDrv.Leituras);
end;

procedure TTestProtocolDriver.ReadOnlyWithNoCallbackMustNotRaise;
var
  valores:TArrayOfDouble;
  semCallback:TTagRec;
begin
  SetLength(valores, 1);
  valores[0]:=1;

  //callback e' opcional: o caminho normal do Write testa "if assigned" antes
  //de chamar, e a recusa por somente-leitura tem que fazer o mesmo.
  semCallback:=TagRecFor(1, 3, 6, 0, 1);
  FDrv.ReadOnly:=true;

  //sem callback nao ha' a quem avisar, mas tambem nao pode estourar
  FDrv.Write(semCallback, valores);
  FDrv.ScanWrite(semCallback, valores);

  AssertEquals('the driver must not be called', 0, FDrv.Escritas);
  AssertEquals('and nobody was called back', 0, FCallbacks);
end;

procedure TTestProtocolDriver.RegisteringInBulkDoesNotHang;
var
  outro:TSampleTag;
begin
  //StartUpdateMultipleTags pega os mutexes uma vez so; os AddTag de dentro
  //nao podem tentar pegar de novo, senao o driver trava.
  outro:=TSampleTag.Create(nil);
  try
    FDrv.StartUpdateMultipleTags;
    try
      FDrv.AddTag(FTag);
      FDrv.AddTag(outro);
    finally
      FDrv.StopUpdateMultipleTags;
    end;

    AssertEquals('both tags went in', 2, FDrv.TagCount);

    //e depois do lote o cadastro normal continua funcionando
    FDrv.RemoveTag(outro);
    AssertEquals('one tag left', 1, FDrv.TagCount);
  finally
    outro.Free;
  end;
end;

procedure TTestProtocolDriver.CopyingAPacketTakesEveryField;
var
  origem, destino:TIOPacket;
begin
  origem:=IOPacketFor(BytesOf('01 02'), BytesOf('03 04 05'));
  origem.PacketID:=77;
  origem.WriteRetries:=3;
  origem.DelayBetweenCommand:=25;

  FDrv.CopyPacket(origem, destino);

  AssertEquals('identifier',      77, destino.PacketID);
  AssertEquals('bytes to write',   2,  destino.ToWrite);
  AssertEquals('bytes written',     2,  destino.Written);
  AssertEquals('retries',         3,  destino.WriteRetries);
  AssertEquals('delay between commands', 25, destino.DelayBetweenCommand);
  AssertEquals('bytes to read',        3,  destino.ToRead);
  AssertEquals('bytes read',        3,  destino.Received);
  AssertEquals('write result', Ord(iorOK), Ord(destino.WriteIOResult));
  AssertEquals('read result', Ord(iorOK), Ord(destino.ReadIOResult));
  AssertBytesEqual('write buffer', BytesOf('01 02'),    destino.BufferToWrite);
  AssertBytesEqual('read buffer', BytesOf('03 04 05'), destino.BufferToRead);
end;

procedure TTestProtocolDriver.BaseDefaults;
begin
  //a base pressupoe driver que fala por uma porta externa e nao anuncia
  //evento nenhum; quem precisa, sobrescreve.
  AssertTrue  ('needs an external port', FDrv.NeedsExternalPort);
  AssertEquals('empty literal address', '', FDrv.LiteralTagAddress(FTag));
  AssertFalse ('no tag editor', FDrv.HasTabBuilderEditor);
end;

initialization
  RegisterTest(TTestProtocolDriver);

end.
