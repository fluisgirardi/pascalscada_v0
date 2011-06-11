{:
@abstract(Implementação da base de um driver de protocolo.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
unit ProtocolDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommPort, CommTypes, ProtocolTypes, protscanupdate,
  protscan, CrossEvent, Tag, syncobjs {$IFNDEF FPC}, Windows{$ENDIF};

type
  {:
  @abstract(Classe base para drivers de protocolo.)

  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Para você criar um novo driver, basta sobrescrever alguns métodos e funções,
  de acordo com as necessidades de seu driver de protocolo. São eles:
  
  @code(procedure DoAddTag(TagObj:TTag);)
  Sobrescreva esse procedimento para adicionar tags ao scan do driver. Faça as
  devidas verificações do tag nesse método e caso ele não seja um tag válido
  gere uma excessão para abortar a adição do tag no driver.
  Não esqueça de chamar o método herdado com @code(inherited DoAddTag(TagObj:TTag))
  para adicionar o tag na classe base (@name).
  
  @code(procedure DoDelTag(TagObj:TTag);)
  Procedimento por remover tags do scan do driver. Não esqueça de chamar o
  método herdado com @code(inherited DoDelTag(TagObj:TTag)) para remover o tag
  da classe base (@name).

  @code(procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);)
  Procedimento usado para atualizar as informações do tag no driver.
  Caso alguma alteração torne o tag inconsistente para o seu driver,
  gere um excessão para abortar a mudança.
  
  @code(procedure DoScanRead(Sender:TObject; var NeedSleep:Integer);)
  Prodimento chamado para verificar se há algum tag necessitando ser lido.
  
  @code(procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec);)
  Procedimento chamado pelo driver para retornar os valores lidos que estão
  em algum gerenciador de memória para os tags.

  @code(function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;)
  Executa as escritas de valores sincronas e assincronas dos tags. É este método
  que escreve os valores do tag no seu equipamento.

  @code(function DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;)
  Executa as leituras sincronas e assincronas dos tags. É o método que vai
  buscar os valores no seu equipamento.
  
  @code(function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; )
  Função responsável por informar o tamanho das palavras de dados em bits
  que o tag está referenciando.

  Sobrescrevendo esses métodos e rotinas, seu driver estará pronto. @bold(Veja
  a documentação detalhada de cada método para enteder como cada um funciona.)
  }

  TProtocolDriver = class(TComponent, IPortDriverEventNotification)
  private
    //Array de tags associados ao driver.
    PTags:array of TTag;

    //thread de execução do scan dos tags
    PScanReadThread:TScanThread;
    //Thread de execução de escritas
    PScanWriteThread:TScanThread;
    //thread de atualização dos pedidos dos tags
    PScanUpdateThread:TScanUpdate;

    //excessao caso o index to tag esteja fora dos limites
    procedure DoExceptionIndexOut(index:integer);

    //metodos para manipulação da lista de tags
    function  GetTagCount:Integer;
    function  GetTag(index:integer):TTag;
    function  GetTagName(index:integer):String;
    function  GetTagByName(Nome:String):TTag;

    //metodos chamados pelas threads
    procedure SafeScanRead(Sender:TObject; var NeedSleep:Integer);
    function  SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
    procedure SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
    function  GetMultipleValues(var MultiValues:TArrayOfScanUpdateRec):Integer;

    procedure DoPortOpened(Sender: TObject);
    procedure DoPortClosed(Sender: TObject);
    procedure DoPortDisconnected(Sender: TObject);
    procedure DoPortRemoved(Sender:TObject);
  protected
    {:
    Flag que informa ao driver se ao menos uma variavel deve ser lida a cada
    ciclo de scan do driver.
    }
    PReadSomethingAlways:Boolean;
    //: Indica se o driver está pronto.
    FProtocolReady:Boolean;

    //: Armazena a ID (número único) do driver.
    PDriverID:Cardinal;
    //: Armazena o driver de porta associado a esse driver de protocolo.
    PCommPort:TCommPortDriver;
    //: Armazena o ID (número único) esses pedidos.
    FScanReadID, FScanWriteID, FReadID, FWriteID:Cardinal;
    //: Armazena a evento usado para parar as threads do driver de protocolo.
    FCritical:TCriticalSection;
    //: Forca a suspensão das threads.
    FPause:TCrossEvent;
    //: Armazena a seção crítica que protege areas comuns a muitas threads.
    PCallersCS:TCriticalSection;


    //:
    function  GetPortOpenedEvent:TNotifyEvent;
    function  GetPortClosedEvent:TNotifyEvent;
    function  GetPortDisconnectedEvent:TNotifyEvent;    
    function  NotifyThisEvents:TNotifyThisEvents; virtual;
    procedure PortOpened(Sender: TObject); virtual;
    procedure PortClosed(Sender: TObject); virtual;
    procedure PortDisconnected(Sender: TObject); virtual;

    //: Configura a porta de comunicação que será usada pelo driver.
    procedure SetCommPort(CommPort:TCommPortDriver);
    {:
    Copia uma estrutura TIOPacket para outra.
    @param(Source TIOPacket. Estrutura de origem dos dados.)
    @param(Dest TIOPacket. Estrutura para onde os dados serão copiados.)
    }
    procedure CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
    {:
    Callback @bold(assincrono) que o driver de porta (TCommPortDriver) irá
    chamar para retornar os resultados de I/O.
    @param(Result TIOPacket. Estrutura com os dados de retorno da solicitação
           de I/O. @bold(É automaticamente destruida após retornar desse
           método.)
    }
    procedure CommPortCallBack(var Result:TIOPacket); virtual;

    {:
    Método chamado pelo driver de protocolo para adicionar um tag ao scan driver.
    @param(TagObj TTag. Tag a adicionar como dependente do driver.)
    @seealso(AddTag)
    }
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); virtual;
    {:
    Método chamado pelo driver de protocolo para remover um tag do scan do driver.
    @param(TagObj TTag. Tag dependente para remover do driver.)
    @seealso(RemoveTag)
    }
    procedure DoDelTag(TagObj:TTag); virtual;

    {:
    Método chamado pelas threads do driver de protocolo para realizar leitura dos
    tags a ele associado.
    @param(Sender TObject. Thread que está solicitando a varredura de atualização.)
    @param(NeedSleep Integer. Caso o procedimento não encontrou nada que precise
                              ser lido, escreva nesse valor um valor negativo a
                              forçar o scheduler do seu sistema operacional a
                              executar outra thread ou um valor positivo para
                              fazer a thread de scan dormir. O tempo que ela
                              ficará dormindo é o valor que você escreve nessa
                              variável.
                              Caso o seu driver encontrou algum tag necessitando
                              de atualização, retorne 0 (Zero).)
    }
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); virtual; abstract;
    {:
    Método chamado pelas threads do driver de protocolo para atualizar os valores
    dos tags.
    @param(TagRec TTagRec. Estrutura com informações do tag.)
    @param(values TScanReadRec. Armazena os valores que serão enviados ao tag.)
    }
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); virtual; abstract;

    {:
    Função chamada para escrever o valor de um tag (simples ou bloco) no
    equipamento.

    @param(tagrec TTagRec. Estrutura com informações do tag.)
    @param(Values TArrayOfDouble. Valores a serem escritos no equipamento.)
    @param(Sync Boolean. Flag que indica se a escrita deve ser sincrona ou assincrona.)

    @returns(TProtocolIOResult).
    }
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;

    {:
    Função chamada para ler valores do equipamento.

    @param(tagrec TTagRec. Estrutura com informações do tag.)
    @param(Values TArrayOfDouble. Array que irá armazenar os valores lidos do equipamento.)
    @param(Sync Boolean. Flag que indica se a leitura deve ser sincrona ou assincrona.)

    @returns(TProtocolIOResult).
    }
    function DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;

    //: Informa ao driver se ele deve ler algum tag a todo scan.
    property ReadSomethingAlways:Boolean read PReadSomethingAlways write PReadSomethingAlways default true;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;

    //: @exclude
    procedure AfterConstruction; override;

    //: @exclude
    destructor  Destroy; override;

    {:
    Adiciona um tag ao scan do driver.
    @param(Tag TTag. Tag a adicionar no scan do driver.)
    @raises(Exception caso alguma configuração esteja errada.)
    }
    procedure AddTag(TagObj:TTag);

    //: Chama o editor de tags do driver.
    procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); virtual;

    {:
    Remove um tag do scan do driver.
    @param(Tag TTag. Tag a remover do scan do driver.)
    }
    procedure RemoveTag(TagObj:TTag);
    {:
    Função que informa se o Tag está associado ao driver.
    @param(TagObj TTag. Tag que deseja saber se está associado ao driver.)
    @returns(@true caso o tag esteja associado ao driver.)
    }
    function  IsMyTag(TagObj:TTag):Boolean;
    {:
    Função que retorna o tamanho em bits do registrador mapeado pelo tag.
    @param(Tag TTag. Tag que se deseja saber o tamanho do registrador.)
    @param(isWrite Boolean. Caso @true, informa o tamanho em bits usando as
           funções de escrita.)
    @returns(Tamanho em bits do registrador associado ou 0 (zero) caso falhe.
    }
    function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; virtual; abstract;

    {:
    Solicita a leitura por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    @returns(Cardinal. Número único do pedido de leitura por scan.)
    }
    function  ScanRead(const tagrec:TTagRec):Cardinal;
    {:
    Solicita a escrita por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    @returns(Cardinal. Número único do pedido de escrita por scan.)
    }
    function  ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;
    {:
    Solicita a leitura (sincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    }
    procedure Read(const tagrec:TTagRec);
    {:
    Solicita uma escrita (sincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    }
    procedure Write(const tagrec:TTagRec; const Values:TArrayOfDouble);

    //: Conta os tags dependentes desse driver de protocolo.
    property TagCount:Integer read GetTagCount;
    //: Lista cada tag dependente desse driver.
    property Tag[index:integer]:TTag read GetTag;
    //: Lista o nome de cada tag dependente desse driver.
    property TagName[index:integer]:String read GetTagName;
    //: Lista cada tag dependente desse driver usando o nome do tag como indice.
    property TagByName[Nome:String]:TTag read GetTagByName;
  published
    {:
    Driver de porta que será usado para realizar as operações de comunicação
    do protoloco.
    @seealso(TCommPortDriver)
    }
    property CommunicationPort:TCommPortDriver read PCommPort write SetCommPort nodefault;
    //: Identificação (número único) do driver.
    property DriverID:Cardinal read PDriverID;
  end;

var
   {:
   Contador de drivers criados, usado para gerar nomes únicos dos eventos
   seções críticas e semaforos em ambiente Windows.

   @bold(Não altere o valor dessa variável.)
   }
   DriverCount:Cardinal;

implementation

uses PLCTag, hsstrings, Dialogs, math;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementação de TProtocolDriver
////////////////////////////////////////////////////////////////////////////////

constructor TProtocolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDriverID := DriverCount;
  Inc(DriverCount);

  FProtocolReady:=true;

  FCritical := TCriticalSection.Create;

  FPause := TCrossEvent.Create(nil,true,true,'');

  PCallersCS := TCriticalSection.Create;

  PScanUpdateThread := TScanUpdate.Create(true, Self);
  PScanUpdateThread.Priority:=tpHighest;
  PScanUpdateThread.OnGetValue := SafeGetValue;
  PScanUpdateThread.OnScanTags := GetMultipleValues;

  PScanReadThread := TScanThread.Create(true, PScanUpdateThread);
  PScanReadThread.Priority:=tpTimeCritical;
  PScanReadThread.OnDoScanRead := SafeScanRead;
  PScanReadThread.OnDoScanWrite := nil;

  PScanWriteThread := TScanThread.Create(true, PScanUpdateThread);
  PScanWriteThread.Priority:=tpTimeCritical;
  PScanWriteThread.OnDoScanRead := nil;
  PScanWriteThread.OnDoScanWrite := SafeScanWrite;
end;

procedure TProtocolDriver.AfterConstruction;
begin
  Inherited AfterConstruction;
  PScanUpdateThread.Resume;

  PScanReadThread.Resume;
  PScanReadThread.WaitInit;

  PScanWriteThread.Resume;
  PScanWriteThread.WaitInit;
end;

destructor TProtocolDriver.Destroy;
var
  c:Integer;
begin
  PScanReadThread.Destroy;
  PScanWriteThread.Destroy;

  PScanUpdateThread.Terminate;
  PScanUpdateThread.WaitFor;

  for c:=0 to High(PTags) do
    TPLCTag(PTags[c]).RemoveDriver;

  SetCommPort(nil);

  FCritical.Destroy;

  FPause.Destroy;
  
  SetLength(PTags,0);
  PCallersCS.Destroy;
  inherited Destroy;
end;

procedure TProtocolDriver.OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  MessageDlg(SWithoutTagBuilder, mtError,[mbOK],0);
end;

procedure TProtocolDriver.SetCommPort(CommPort:TCommPortDriver);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    //se for a mesma porta cai fora...
    if CommPort=PCommPort then exit;

    if PCommPort<>nil then begin
      if PCommPort.LockedBy=PDriverID then
        PCommPort.Unlock(PDriverID);
      PCommPort.DelProtocol(Self);
    end;

    if CommPort<>nil then begin
      CommPort.AddProtocol(Self);
    end;
    PCommPort := CommPort;
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  c:integer;
begin
  for c:=0 to High(PTags) do
    if PTags[c]=TagObj then
      raise Exception.Create(STagAlreadyRegiteredWithThisDriver);

  c:=Length(Ptags);
  SetLength(PTags,c+1);
  PTags[c] := TagObj;

  (TagObj as IScanableTagInterface).SetTagValidity(TagValid);
end;

procedure TProtocolDriver.DoDelTag(TagObj:TTag);
var
  c:Integer;
  h:integer;
  found:boolean;
begin
  if Length(PTags)<=0 then exit;

  h:=High(PTags);
  found := false;
  for c:=0 to h do
    if PTags[c]=TagObj then begin
      found := true;
      break;
    end;
  if found then begin
    (PTags[c] as IScanableTagInterface).SetTagValidity(false);
    PTags[c] := PTags[h];
    SetLength(PTags,h);
  end;
end;

procedure TProtocolDriver.AddTag(TagObj:TTag);
begin
  if not Supports(TagObj, IScanableTagInterface) then
    raise Exception.Create(SScanableNotSupported);

  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;
    
    DoAddTag(TagObj,false);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.RemoveTag(TagObj:TTag);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;
    DoDelTag(TagObj);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoExceptionIndexOut(index:integer);
begin
  if (index>high(PTags)) then
    raise Exception.Create(SoutOfBounds);
end;

function TProtocolDriver.GetTagCount;
begin
  //FCritical.Enter;
  try
    Result := Length(PTags);
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.GetTag(index:integer):TTag;
begin
  //FCritical.Enter;
  try
    DoExceptionIndexOut(index);
    result:=PTags[index];
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.GetTagName(index:integer):String;
begin
  Result:='';
  //FCritical.Enter;
  try
    DoExceptionIndexOut(index);
    result:=PTags[index].Name;
  finally
    //FCritical.Leave;
  end;

end;

function TProtocolDriver.GetTagByName(Nome:String):TTag;
var
  c:Integer;
begin
  Result := nil;
  //FCritical.Enter;
  try
    for c:=0 to High(PTags) do
      if PTags[c].Name = Nome then begin
        Result := PTags[c];
        break;
      end;
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.IsMyTag(TagObj:TTag):Boolean;
var
  c:integer;
begin
  Result := false;
  //FCritical.Enter;
  try
    for c:=0 to High(PTags) do
      if TagObj=PTags[c] then begin
        Result := true;
        break;
      end;
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.ScanRead(const tagrec:TTagRec):Cardinal;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de scanReads
    //zera o contador para evitar overflow;
    if FScanReadID=$FFFFFFFF then
       FScanReadID := 0
    else
       inc(FScanReadID);

    //posta uma mensagem de Leitura por Scan
    if (PScanUpdateThread<>nil) then
      PScanUpdateThread.ScanRead(tagrec);

    Result := FScanReadID;

  finally
    PCallersCS.Leave;
  end;
end;

function TProtocolDriver.ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;
var
   pkg:PScanWriteRec;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de ScanWrites
    //zera o contador para evitar overflow;
    if FScanWriteID=$FFFFFFFF then
       FScanWriteID := 0
    else
       inc(FScanWriteID);
       
    //cria um pacote de escrita por scan
    New(pkg);
    pkg^.SWID:=FScanReadID;
    //copia o TagRec
    Move(tagrec, pkg^.Tag, sizeof(TTagRec));
    //copia os valores
    pkg^.ValuesToWrite := Values;
    pkg^.WriteResult:=ioNone;
    pkg^.ValueTimeStamp:=Now;

    //posta uma mensagem de Escrita por Scan
    if (PScanWriteThread<>nil) then begin
      PScanWriteThread.ScanWrite(pkg);
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF}
    end;

    Result := FScanWriteID;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.Read(const tagrec:TTagRec);
var
  res:TProtocolIOResult;
  Values:TArrayOfDouble;
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;
    res := DoRead(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,Now,tcRead,res,tagrec.RealOffset);
  finally
    FCritical.Leave;
    FPause.SetEvent;
    SetLength(Values,0);
  end;
end;

procedure TProtocolDriver.Write(const tagrec:TTagRec; const Values:TArrayOfDouble);
var
  res:TProtocolIOResult;
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    res := DoWrite(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,Now,tcWrite,res,tagrec.RealOffset);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.CommPortCallBack(var Result:TIOPacket);
begin
  if Result.Res2<>nil then
     CopyIOPacket(Result,PIOPacket(Result.Res2)^);
  if Result.res1 is TCrossEvent then
     TCrossEvent(Result.res1).SetEvent;
end;

procedure TProtocolDriver.CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
begin
  Dest.PacketID := Source.PacketID;
  Dest.WriteIOResult := Source.WriteIOResult;
  Dest.ToWrite := Source.ToWrite;
  Dest.Written := Source.Written;
  Dest.WriteRetries := Source.WriteRetries;
  Dest.DelayBetweenCommand := Source.DelayBetweenCommand;
  Dest.ReadIOResult := Source.ReadIOResult;
  Dest.ToRead := Source.ToRead;
  Dest.Received := Source.Received;
  Dest.ReadRetries := Source.ReadRetries;
  SetLength(Dest.BufferToRead, 0);
  SetLength(Dest.BufferToWrite, 0);
  Dest.BufferToRead := Source.BufferToRead;
  Dest.BufferToWrite:= Source.BufferToWrite;
  Dest.Res1 := Source.Res1;
  Dest.Res2 := Source.Res2;
end;

procedure TProtocolDriver.SafeScanRead(Sender:TObject; var NeedSleep:Integer);
begin
   try
      FPause.WaitFor($FFFFFFFF);
      FCritical.Enter;
      DoScanRead(Sender, NeedSleep);
   finally
      FCritical.Leave;
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF}
   end;
end;

function  TProtocolDriver.SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    Result := DoWrite(TagRec,values,false)
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    DoGetValue(TagRec,values);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

function  TProtocolDriver.GetMultipleValues(var MultiValues:TArrayOfScanUpdateRec):Integer;
var
  t, valueSet:Integer;
  first:Boolean;
  tagiface:IScanableTagInterface;
  tr:TTagRec;
  remainingMs:Int64;
  ScanReadRec:TScanReadRec;
  doneOne:Boolean;
begin
  doneOne:=false;
  try
    Result:=0;
    valueSet:=-1;
    
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    if ComponentState*[csDestroying]<>[] then exit;

    if (PCommPort=nil) or (not PCommPort.ReallyActive) then begin
      Result:=50; //forca espera de 50ms
      exit;
    end;

    for t:=0 to TagCount-1 do begin
      if Supports(Tag[t], IScanableTagInterface) then begin
        tagiface:=Tag[t] as IScanableTagInterface;
        if tagiface.IsValidTag then begin

          if PReadSomethingAlways then
            remainingMs:=tagiface.RemainingMiliseconds
          else
            remainingMs:=tagiface.RemainingMilisecondsForNextScan;

          //se o tempo restante é maior que zero
          if remainingMs>0 then begin
            if first then begin
              Result:=remainingMs;
              first:=false;
            end else
              Result := Min(remainingMs, Result);
          end else begin
            doneOne:=true;
            inc(valueSet);
            SetLength(MultiValues,valueSet+1);

            tagiface.BuildTagRec(tr,0,0);

            SetLength(ScanReadRec.Values, tr.Size);
            DoGetValue(tr, ScanReadRec);

            MultiValues[valueSet].Owner         :=Tag[t];
            MultiValues[valueSet].LastResult    :=ScanReadRec.LastQueryResult;
            MultiValues[valueSet].CallBack      :=tr.CallBack;
            MultiValues[valueSet].Values        :=ScanReadRec.Values;
            MultiValues[valueSet].ValueTimeStamp:=ScanReadRec.ValuesTimestamp;
          end;
        end;
      end;
    end;

  finally
    FCritical.Leave;
    if doneOne then Result:=0;
    FPause.SetEvent;
  end;
end;

function  TProtocolDriver.GetPortOpenedEvent:TNotifyEvent;
begin
  Result := DoPortOpened;
end;

function  TProtocolDriver.GetPortClosedEvent:TNotifyEvent;
begin
  Result := DoPortClosed;
end;

function  TProtocolDriver.GetPortDisconnectedEvent:TNotifyEvent;
begin
  Result := DoPortDisconnected;
end;

function  TProtocolDriver.NotifyThisEvents:TNotifyThisEvents;
begin
  Result:=[];
end;

procedure TProtocolDriver.DoPortOpened(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    PortOpened(Sender);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortClosed(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    PortClosed(Sender);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortDisconnected(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    while not FPause.ResetEvent do
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF};

    FCritical.Enter;

    PortDisconnected(Sender);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortRemoved(Sender:TObject);
begin
  if CommunicationPort=Sender then
    CommunicationPort:=nil;
end;

procedure TProtocolDriver.PortOpened(Sender: TObject);
begin

end;

procedure TProtocolDriver.PortClosed(Sender: TObject);
begin

end;

procedure TProtocolDriver.PortDisconnected(Sender: TObject);
begin

end;

initialization

DriverCount:=1;

end.