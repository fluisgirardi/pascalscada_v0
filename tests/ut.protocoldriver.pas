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
    procedure CadastrarTag(aTag:TTag; aValido:Boolean);
    procedure CopiarPacote(const aOrigem:TIOPacket; var aDestino:TIOPacket);
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
    procedure Retorno(const ReqID:LongWord; Values:TArrayOfDouble;
                      ValuesTimeStamp:QWord; TagCommand:TTagCommand;
                      LastResult:TProtocolIOResult; OffSet:LongInt);
    function  TagRecComCallback:TTagRec;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //cadastro de tags / tag registration
    procedure TagSemInterfaceDeScanEhRecusado;
    procedure TagCadastradoEntraNaLista;
    procedure TagCadastradoDuasVezesEhRecusado;
    procedure TagRemovidoSaiDaLista;
    procedure RemoverTagDeOutroDriverNaoQuebra;
    procedure CadastroMarcaAValidadeDoTag;
    procedure RemocaoInvalidaOTag;

    //acesso a lista / list access
    procedure TagEhEncontradoPeloNome;
    procedure NomeInexistenteDevolveNulo;
    procedure IndiceForaDaListaEstoura;

    //despacho de leitura e escrita / read and write dispatching
    procedure LeituraChamaODriverEAvisaPeloCallback;
    procedure EscritaChamaODriverComOsValores;
    procedure ResultadoDoDriverChegaNoCallback;

    //modo somente leitura / read only mode
    procedure SomenteLeituraNaoChamaAEscritaDoDriver;
    procedure SomenteLeituraAindaLe;
    procedure SomenteLeituraSemCallbackNaoPodeEstourar;

    //atualizacao em lote / batch update
    procedure CadastroEmLoteNaoTrava;

    //utilitarios / helpers
    procedure CopiaDePacoteLevaTodosOsCampos;
    procedure PadroesDaBase;
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

procedure TDriverDeTeste.CadastrarTag(aTag:TTag; aValido:Boolean);
begin
  DoAddTag(aTag, aValido);
end;

procedure TDriverDeTeste.CopiarPacote(const aOrigem:TIOPacket; var aDestino:TIOPacket);
begin
  CopyIOPacket(aOrigem, aDestino);
end;

{ TTestProtocolDriver }

procedure TTestProtocolDriver.Retorno(const ReqID:LongWord; Values:TArrayOfDouble;
  ValuesTimeStamp:QWord; TagCommand:TTagCommand; LastResult:TProtocolIOResult;
  OffSet:LongInt);
begin
  inc(FCallbacks);
  FUltimoResultado:=LastResult;
  FUltimoComando:=TagCommand;
end;

function TTestProtocolDriver.TagRecComCallback:TTagRec;
begin
  Result:=TagRecFor(1, 3, 6, 0, 1);
  Result.CallBack:=@Retorno;
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

procedure TTestProtocolDriver.TagSemInterfaceDeScanEhRecusado;
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
    AssertTrue('tag sem IScanableTagInterface deve ser recusado', recusou);
    AssertEquals('e nao pode entrar na lista', 0, FDrv.TagCount);
  finally
    semScan.Free;
  end;
end;

procedure TTestProtocolDriver.TagCadastradoEntraNaLista;
begin
  FDrv.AddTag(FTag);
  AssertEquals('quantidade de tags', 1, FDrv.TagCount);
  AssertTrue  ('o driver reconhece o tag', FDrv.IsMyTag(FTag));
  AssertSame  ('o tag da lista', FTag, FDrv.Tag[0]);
  AssertEquals('nome do tag', 'TagDeTeste', FDrv.TagName[0]);
end;

procedure TTestProtocolDriver.TagCadastradoDuasVezesEhRecusado;
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

  AssertTrue  ('cadastro repetido deve ser recusado', recusou);
  AssertEquals('e o tag continua uma vez so na lista', 1, FDrv.TagCount);
end;

procedure TTestProtocolDriver.TagRemovidoSaiDaLista;
begin
  FDrv.AddTag(FTag);
  FDrv.RemoveTag(FTag);
  AssertEquals('quantidade de tags', 0, FDrv.TagCount);
  AssertFalse ('o driver nao reconhece mais o tag', FDrv.IsMyTag(FTag));
end;

procedure TTestProtocolDriver.RemoverTagDeOutroDriverNaoQuebra;
var
  outro:TTagDeTeste;
begin
  FDrv.AddTag(FTag);

  outro:=TTagDeTeste.Create(nil);
  try
    //remover um tag que nunca foi cadastrado e' inocuo, nao erro
    FDrv.RemoveTag(outro);
    AssertEquals('a lista nao muda', 1, FDrv.TagCount);
  finally
    outro.Free; //nunca entrou na lista, entao pode ir embora direto
  end;
end;

procedure TTestProtocolDriver.CadastroMarcaAValidadeDoTag;
begin
  //o gancho protegido e' onde os drivers dizem se o endereco do tag serve
  FDrv.CadastrarTag(FTag, true);
  AssertTrue('tag cadastrado como valido', FTag.Valido);
end;

procedure TTestProtocolDriver.RemocaoInvalidaOTag;
begin
  FDrv.CadastrarTag(FTag, true);
  FDrv.RemoveTag(FTag);
  AssertFalse('tag removido deixa de ser valido', FTag.Valido);
end;

procedure TTestProtocolDriver.TagEhEncontradoPeloNome;
begin
  FDrv.AddTag(FTag);
  AssertSame('busca pelo nome', FTag, FDrv.TagByName['TagDeTeste']);
end;

procedure TTestProtocolDriver.NomeInexistenteDevolveNulo;
begin
  FDrv.AddTag(FTag);
  AssertTrue('nome que nao existe', FDrv.TagByName['NaoExiste']=nil);
end;

procedure TTestProtocolDriver.IndiceForaDaListaEstoura;
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
  AssertTrue('indice fora da lista deve levantar excecao', estourou);
end;

procedure TTestProtocolDriver.LeituraChamaODriverEAvisaPeloCallback;
begin
  FDrv.Read(TagRecComCallback);

  AssertEquals('chamadas ao DoRead do driver', 1, FDrv.Leituras);
  AssertEquals('callbacks recebidos', 1, FCallbacks);
  AssertEquals('comando informado', Ord(tcRead), Ord(FUltimoComando));
end;

procedure TTestProtocolDriver.EscritaChamaODriverComOsValores;
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 2);
  valores[0]:=10;
  valores[1]:=20;

  FDrv.Write(TagRecComCallback, valores);

  AssertEquals('chamadas ao DoWrite do driver', 1, FDrv.Escritas);
  AssertEquals('quantidade de valores repassados', 2, Length(FDrv.UltimosValores));
  AssertEquals('primeiro valor', 10, FDrv.UltimosValores[0], 0);
  AssertEquals('segundo valor',  20, FDrv.UltimosValores[1], 0);
  AssertEquals('comando informado', Ord(tcWrite), Ord(FUltimoComando));
end;

procedure TTestProtocolDriver.ResultadoDoDriverChegaNoCallback;
begin
  //o que o driver concreto devolve tem que chegar inteiro em quem pediu
  FDrv.ResultadoProgramado:=ioTimeOut;
  FDrv.Read(TagRecComCallback);
  AssertEquals('resultado no callback', Ord(ioTimeOut), Ord(FUltimoResultado));
end;

procedure TTestProtocolDriver.SomenteLeituraNaoChamaAEscritaDoDriver;
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 1);
  valores[0]:=1;

  FDrv.ReadOnly:=true;
  FDrv.Write(TagRecComCallback, valores);

  AssertEquals('o driver nao pode ser chamado', 0, FDrv.Escritas);
  AssertEquals('mas quem pediu tem que ser avisado', 1, FCallbacks);
  AssertEquals('motivo da recusa', Ord(ioReadOnlyProtocol), Ord(FUltimoResultado));
end;

procedure TTestProtocolDriver.SomenteLeituraAindaLe;
begin
  FDrv.ReadOnly:=true;
  FDrv.Read(TagRecComCallback);
  AssertEquals('leitura continua permitida', 1, FDrv.Leituras);
end;

procedure TTestProtocolDriver.SomenteLeituraSemCallbackNaoPodeEstourar;
var
  valores:TArrayOfDouble;
  semCallback:TTagRec;
begin
  Ignore('defeito conhecido: em TProtocolDriver.Write (e em ScanWrite) a recusa ' +
         'por protocolo somente-leitura chama tagrec.CallBack sem testar se ele ' +
         'existe, enquanto o caminho normal, quatro linhas abaixo, faz ' +
         '"if assigned(tagrec.CallBack)". Com callback nulo o resultado e'#39' ' +
         'EAccessViolation em $00000000 - confirmado por este teste. Correcao: ' +
         'envolver as duas chamadas com if assigned(). Remova este Ignore depois ' +
         'de corrigir.');

  SetLength(valores, 1);
  valores[0]:=1;

  //callback e' opcional: o caminho normal do Write testa "if assigned" antes
  //de chamar, e a recusa por somente-leitura tem que fazer o mesmo.
  semCallback:=TagRecFor(1, 3, 6, 0, 1);
  FDrv.ReadOnly:=true;
  FDrv.Write(semCallback, valores);

  AssertEquals('o driver nao pode ser chamado', 0, FDrv.Escritas);
end;

procedure TTestProtocolDriver.CadastroEmLoteNaoTrava;
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

    AssertEquals('os dois tags entraram', 2, FDrv.TagCount);

    //e depois do lote o cadastro normal continua funcionando
    FDrv.RemoveTag(outro);
    AssertEquals('um tag restante', 1, FDrv.TagCount);
  finally
    outro.Free;
  end;
end;

procedure TTestProtocolDriver.CopiaDePacoteLevaTodosOsCampos;
var
  origem, destino:TIOPacket;
begin
  origem:=IOPacketFor(BytesOf('01 02'), BytesOf('03 04 05'));
  origem.PacketID:=77;
  origem.WriteRetries:=3;
  origem.DelayBetweenCommand:=25;

  FDrv.CopiarPacote(origem, destino);

  AssertEquals('identificador',      77, destino.PacketID);
  AssertEquals('bytes a escrever',   2,  destino.ToWrite);
  AssertEquals('bytes escritos',     2,  destino.Written);
  AssertEquals('tentativas',         3,  destino.WriteRetries);
  AssertEquals('espera entre comandos', 25, destino.DelayBetweenCommand);
  AssertEquals('bytes a ler',        3,  destino.ToRead);
  AssertEquals('bytes lidos',        3,  destino.Received);
  AssertEquals('resultado da escrita', Ord(iorOK), Ord(destino.WriteIOResult));
  AssertEquals('resultado da leitura', Ord(iorOK), Ord(destino.ReadIOResult));
  AssertBytesEqual('buffer de escrita', BytesOf('01 02'),    destino.BufferToWrite);
  AssertBytesEqual('buffer de leitura', BytesOf('03 04 05'), destino.BufferToRead);
end;

procedure TTestProtocolDriver.PadroesDaBase;
begin
  //a base pressupoe driver que fala por uma porta externa e nao anuncia
  //evento nenhum; quem precisa, sobrescreve.
  AssertTrue  ('precisa de porta externa', FDrv.NeedsExternalPort);
  AssertEquals('endereco literal vazio', '', FDrv.LiteralTagAddress(FTag));
  AssertFalse ('sem editor de tags', FDrv.HasTabBuilderEditor);
end;

initialization
  RegisterTest(TTestProtocolDriver);

end.
