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

  { TTagDeTeste }

  TTagDeTeste = class(TTag, IScanableTagInterface)
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
  TTagSemScan = class(TTag);

  {$IFDEF PORTUGUES}
  //: Driver falso: fecha os metodos abstratos e anota o que foi chamado.
  {$ELSE}
  //: Fake driver: closes the abstract methods and records what was called.
  {$ENDIF}

  { TDriverDeTeste }

  TDriverDeTeste = class(TProtocolDriver)
  public
    Leituras, Escritas:LongInt;
    UltimoTagRec:TTagRec;
    UltimosValores:TArrayOfDouble;
    ResultadoProgramado:TProtocolIOResult;
    ValorProgramado:Double;
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

  { TTestProtocolDriver }

  TTestProtocolDriver = class(TTestCase)
  private
    FDrv:TDriverDeTeste;
    FTag:TTagDeTeste;
    //anotacoes do callback / callback bookkeeping
    FCallbacks:LongInt;
    FUltimoResultado:TProtocolIOResult;
    FUltimoComando:TTagCommand;
    procedure CallbackIn(const ReqID:LongWord; Values:TArrayOfDouble;
                      ValuesTimeStamp:QWord; TagCommand:TTagCommand;
                      LastResult:TProtocolIOResult; OffSet:LongInt);
    function  TagRecWithCallback:TTagRec;
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

{ TTagDeTeste }

function TTagDeTeste.RemainingMiliseconds:Int64;
begin
  Result:=0;
end;

function TTagDeTeste.RemainingMilisecondsForNextScan:Int64;
begin
  Result:=0;
end;

function TTagDeTeste.IsValidTag:Boolean;
begin
  Result:=FValido;
end;

function TTagDeTeste.IsMyCallBack(Cback:TTagCommandCallBack):Boolean;
begin
  Result:=false;
end;

procedure TTagDeTeste.SetTagValidity(TagValidity:Boolean);
begin
  FValido:=TagValidity;
end;

procedure TTagDeTeste.BuildTagRec(out tr:TTagRec; Count, OffSet:LongInt);
begin
  tr:=TagRecFor(1, 3, 0, 0, 1);
end;

function TTagDeTeste.GetLastUpdateTimestamp:QWord;
begin
  Result:=0;
end;

function TTagDeTeste.GetUpdateTime:Int64;
begin
  Result:=1000;
end;

{ TDriverDeTeste }

constructor TDriverDeTeste.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Leituras:=0;
  Escritas:=0;
  ResultadoProgramado:=ioOk;
  ValorProgramado:=0;
end;

procedure TDriverDeTeste.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=1;
end;

procedure TDriverDeTeste.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin
  SetLength(values.Values, 1);
  values.Values[0]:=ValorProgramado;
  values.LastQueryResult:=ResultadoProgramado;
end;

function TDriverDeTeste.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  inc(Escritas);
  UltimoTagRec:=tagrec;
  UltimosValores:=Copy(Values, 0, Length(Values));
  Result:=ResultadoProgramado;
end;

function TDriverDeTeste.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  inc(Leituras);
  UltimoTagRec:=tagrec;
  SetLength(Values, 1);
  Values[0]:=ValorProgramado;
  Result:=ResultadoProgramado;
end;

function TDriverDeTeste.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

procedure TDriverDeTeste.RegisterTagIn(aTag:TTag; aValido:Boolean);
begin
  DoAddTag(aTag, aValido);
end;

procedure TDriverDeTeste.CopyPacket(const aOrigem:TIOPacket; var aDestino:TIOPacket);
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
end;

function TTestProtocolDriver.TagRecWithCallback:TTagRec;
begin
  Result:=TagRecFor(1, 3, 6, 0, 1);
  Result.CallBack:=@CallbackIn;
end;

procedure TTestProtocolDriver.SetUp;
begin
  FDrv:=TDriverDeTeste.Create(nil);
  FTag:=TTagDeTeste.Create(nil);
  FTag.Name:='TagDeTeste';
  FCallbacks:=0;
  FUltimoResultado:=ioNone;
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
  semScan:TTagSemScan;
  recusou:Boolean;
begin
  //um tag que nao pode ser varrido nao tem o que fazer num driver
  semScan:=TTagSemScan.Create(nil);
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
  outro:TTagDeTeste;
begin
  FDrv.AddTag(FTag);

  outro:=TTagDeTeste.Create(nil);
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
  outro:TTagDeTeste;
begin
  //StartUpdateMultipleTags pega os mutexes uma vez so; os AddTag de dentro
  //nao podem tentar pegar de novo, senao o driver trava.
  outro:=TTagDeTeste.Create(nil);
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
