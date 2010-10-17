{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  @abstract(Unit que implementa as bases de um driver de porta de comunicação)
}
unit CommPort;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  Commtypes, Classes, MessageSpool, CrossEvent, SyncObjs, ExtCtrls
  {$IFNDEF FPC}, Windows{$ENDIF};

type
  {:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  @name é responsável por notificar a aplicação e os drivers sobre erros de
  comunicação, abertura, fechamento e desconecção de uma porta de comunicação.
  É usado internamente por TCommPortDriver.
  }
  TEventNotificationThread = class(TCrossThread)
  private
    PMsg:TMSMsg;
    FOwner:TComponent;
    FEvent:Pointer;
    FError:TIOResult;
    FDoSomethingEvent,
    FInitEvent:TCrossEvent;
    FSpool:TMessageSpool;
    procedure DoSomething;
    procedure WaitToDoSomething;

    procedure SyncCommErrorEvent;
    procedure SyncPortEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AOwner:TComponent);
    destructor  Destroy;
    procedure   WaitInit;
    procedure   Terminate;
    //: Envia uma mensagem de erro de comunicação para a aplicação;
    procedure DoCommErrorEvent(Event:TCommPortErrorEvent; Error:TIOResult);
    //: Envia uma mensagem de evento porta aberta, fechada e disconectada para aplicação;
    procedure DoCommPortEvent(Event:TNotifyEvent);
  end;

  {:
  @abstract(Classe base de drivers de portas de comunicação)

  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Esta classe foi criada com o intuito de diminuir os esforços na criação de
  drivers de portas de comunicações tanto no modo mono-tarefa (single thread) quanto
  no modo multi-tarefa (threads). Essa classe fornece dois métodos de comunicação,
  um sincrono (sem threads) e outro assincrono (multitarefa).

  As poucas partes a serem escritas é sobreescrever de três métodos virtuais que
  fazem todo o trabalho (e é lógico as rotinas das propriedades e demais funções
  de comunicação da sua porta). São eles:

  @code(procedure IOCommand(cmd:TIOCommand; var Packet:TIOPacket); virtual;)
  Realiza as rotinas de comunicação

  @code(procedure PortStart(var Ok:Boolean); virtual;)
  Abra a porta e retorne @true caso consiga abrir a porta de comunicação.

  @code(procedure PortStop(var Ok:Boolean); virtual;)
  Feche a porta e retorne @true caso consiga fechar a porta de comunicação.

  Tirando essas coisas, você não precisa se preocupar com threads e comunicação
  assincrona e sincrona, propriedades Active, elas já estão prontas para você!
  }
  TCommPortDriver = class(TComponent)
  private
    FLogActions,
    FReadedLogActions:Boolean;
    FLogFile:String;
    FLogFileStream:TFileStream;
    { @exclude }
    PLockedBy:Cardinal;
    {: @exclude }
    PPacketID:Cardinal;
    {: @exclude }
    FReadActive:Boolean;
    {: @exclude }
    PEventUpdater:TEventNotificationThread;
    {: @exclude }
    PIOCmdCS, PLockCS:TCriticalSection;
    {: @exclude }
    PLockEvent:TCrossEvent;
    {: @exclude }
    PUnlocked:Integer;
    {: @exclude }
    FLastOSErrorNumber:Integer;
    {: @exclude }
    FLastOSErrorMessage:String;
    {: @exclude }
    FTimer:TTimer;
    {: @exclude }
    FLastPkgId:Cardinal;
    {: @exclude }
    FCommandsSecond:Integer;
    {: @exclude }
    FTXBytes,
    FRXBytes,
    FTXBytesLast,
    FRXBytesLast,
    FTXBytesSecond,
    FRXBytesSecond:Int64;

    FOwnerThread:TPSThreadID;

    {: Abertura forcada da porta em edicao }
    FOpenInEditMode:Boolean;

    //: @exclude
    FOnCommErrorReading:TCommPortErrorEvent;
    //: @exclude
    FOnCommErrorWriting:TCommPortErrorEvent;
    //: @exclude
    FOnCommPortOpened,
    FOnCommPortOpenError:TNotifyEvent;
    //: @exclude
    FOnCommPortClosed,
    FOnCommPortCloseError:TNotifyEvent;
    //: @exclude
    FOnCommPortDisconnected:TNotifyEvent;

    procedure OpenInEditMode(v:Boolean);
    {: @exclude }
    procedure TimerStatistics(Sender:TObject);
    {: @exclude }
    function GetLocked:Boolean;
    {: @exclude }
    procedure SetActive(v:Boolean);
    {: @exclude }
    procedure InternalIOCommand(cmd:TIOCommand; Packet:PIOPacket);
    {: @exclude }
    procedure InternalPortStart(var Ok:Boolean);
    {: @exclude }
    procedure InternalPortStop(var Ok:Boolean);
    {:
    @name é o metodo chamado para realizar as leituras/escritas do driver.
    Para a criação de novos drivers, esse método precisa ser sobrescrito.

    @param(cmd TIOCommand. Informa os comandos de Leitura/escrita e sua ordem)
    @param(Packet PIOPacket. Aponta para uma estrutura TIOPacket que contem os valores a
           a serem escritos e os valores lidos.)
    @return(Retorna em Packet os valores lidos.)
    }
    procedure IOCommand(cmd:TIOCommand; Packet:PIOPacket);

    procedure  SetLogActions(Log:Boolean);
    procedure  SetLogFile(nFile:String);
    procedure  LogAction(cmd:TIOCommand; Packet:TIOPacket);
  protected
    FExclusiveDevice:Boolean;
    //: Envia uma mensagem de erro de comunicação da thread de comunicação para a aplicação
    procedure DoCommError(WriteCmd:Boolean; Error:TIOResult);
    //: Envia uma mensagem de porta aberta.
    procedure DoCommPortOpened;
    //: Envia uma mensagem de falha na abertura da porta.
    procedure DoCommPortOpenError;
    //: Envia uma mensagem de porta fechada.
    procedure DoCommPortClose;
    //: Envia uma mensagem de falha fechando a porta de comunicação.
    procedure DoCommPortCloseError;
    //: Envia uma mensagem de porta desconectada detectada pela thread de comunicação;
    procedure DoCommPortDisconected;
  protected
    {: Variável responsável por armazenar o estado atual do driver }
    PActive:Boolean;
    {: Variável responsável por armazenar se devem ser feitas limpezas após algum erro de comunicação }
    PClearBufOnErr:Boolean;
    {:
    Array que armazena os drivers de protocolo dependentes.
    @seealso(TProtocolDriver)
    }
    Protocols:array of TComponent;
    {:
    Array que armazena os drivers de protocolo dependentes.
    @seealso(TProtocolDriver)
    }
    EventInterfaces:IPortDriverEventNotificationArray;

    {:
    Lê dados da porta. É necessário sobrescrever este método para criar
    novos drivers de porta.
    @param(Packet PIOPacket. Contem as informações necessárias para executar
           a leitura).
    @seealso(TIOPacket)
    }
    procedure Read(Packet:PIOPacket); virtual; abstract;
    {:
    Escreve dados na porta. É necessário sobrescrever este método para criar
    novos drivers de porta.
    @param(Packet PIOPacket. Contem as informações necessárias para executar
           a escrita).
    @seealso(TIOPacket)
    }
    procedure Write(Packet:PIOPacket); virtual; abstract;
    {:
    @name deve ser sobrescrito em portas que desejam oferecer uma espera entre
    os comandos de leitura e escrita.
    }
    procedure NeedSleepBetweenRW; virtual; abstract;
    {:
    @name é o metodo chamado para realizar a abertura da porta.
    Para a criação de novos drivers, esse método precisa ser sobrescrito.

    @return(Retorne @true em Ok caso a porta tenha sido aberta com sucesso. @false caso contrário)
    @seealso(TDriverCommand)
    }
    procedure PortStart(var Ok:Boolean); virtual; abstract;
    {:
    @name é o metodo chamado para fechar uma porta.
    Para a criação de novos drivers, esse método precisa ser sobrescrito.

    @return(Retorne @true em Ok caso a porta tenha sido fechada com sucesso. @false caso contrário)
    @seealso(TDriverCaller)
    }
    procedure PortStop(var Ok:Boolean); virtual; abstract;
    {:
    @name é o metodo chamado para validar o conjunto de configurações de uma porta.
    Para a criação de novos drivers, se essa função não for sobrescrita, todas
    as combinações de configurações serão inválidas.

    @return(Retorne @true caso as configurações da porta estejam Ok. @false caso contrário)
    @seealso(TDriverCaller)    
    }
    function  ComSettingsOK:Boolean; virtual;
    {:
    @name é o método responsável por fazer a limpeza dos buffers de leitura/escrita
    da porta.
    É altamente recomendável você escrever esse método caso esteja criando um novo
    driver de porta.
    }
    procedure ClearALLBuffers; virtual; abstract;
    {: @exclude }
    procedure Loaded; override;
    {: @exclude }
    procedure InternalClearALLBuffers;
    {: @name gera uma exceção caso a porta esteja ativa. Use este método para
       evitar a mudança de valores de certas propriedade que não podem ser
       alterados com a porta ativa.
    }
    procedure DoExceptionInActive;
    {:
      @name atualiza as propriedades LastOSErrorNumber e LastOSErrorMessage com
      o último erro registrado pelo sistema operacional.
    }
    procedure RefreshLastOSError;

    //: Evento que sinaliza uma falha de leitura;
    property OnCommErrorReading:TCommPortErrorEvent read FOnCommErrorReading write FOnCommErrorReading;
    //: Evento que sinaliza uma falha de escrita;
    property OnCommErrorWriting:TCommPortErrorEvent read FOnCommErrorWriting write FOnCommErrorWriting;
    //: Evento que sinaliza quando a porta é aberta
    property OnCommPortOpened:TNotifyEvent read FOnCommPortOpened write FOnCommPortOpened;
    //: Evento que sinaliza falha na abetura da porta.
    property OnCommPortOpenError:TNotifyEvent read FOnCommPortOpenError write FOnCommPortOpenError;
    //: Evento que sinaliza quando a porta é fechada
    property OnCommPortClosed:TNotifyEvent read FOnCommPortClosed write FOnCommPortClosed;
    //: Evento que sinaliza quando não é possível fechar a porta
    property OnCommPortCloseError:TNotifyEvent read FOnCommPortCloseError write FOnCommPortCloseError;
    //: Evento que sinaliza quando a porta é disconectada devido a algum erro.
    property OnCommPortDisconnected:TNotifyEvent read FOnCommPortDisconnected write FOnCommPortDisconnected;
  public
    {:
    Cria o driver de porta, inicializando todas as threads e variaveis internas.
    }
    constructor Create(AOwner:TComponent); override;
    {:
    Destroi o driver de porta, fechando e informando a todos os drivers de
    protocolo dependentes sobre a destruição, consequentemente a eliminação da
    referência com este driver de porta.
    @seealso(TProtocolDriver)
    @seealso(AddProtocol)
    @seealso(DelProtocol)
    }
    destructor Destroy; override;
    {:
    Adiciona um driver de protocolo a lista de dependentes
    @param(Prot TProtocolDriver. Driver de protocolo a ser adicionado como dependente)
    @raises(Exception caso Prot não seja descendente de TProtocolDriver)
    @seealso(TProtocolDriver)
    }
    procedure AddProtocol(Prot:TComponent);
    {:
    Remove um driver de protocolo a lista de dependentes
    @param(Prot TProtocolDriver. Driver de protocolo a ser removido da lista de
           dependentes.)
    @seealso(TProtocolDriver)
    }
    procedure DelProtocol(Prot:TComponent);
    {:
    Faz um pedido de leitura/escrita sincrono para o driver (sua aplicação espera
    todo o comando terminar para continuar).
    @param(Cmd TIOCommand. Informa a combinação de comandos de leitura/escrita a
           executar)
    @param(ToWrite BYTES. Conteudo que deseja escrever)
    @param(BytesToRead Cardinal. Informa o número de @noAutoLink(bytes) que deverão ser lidos)
    @param(BytesToWrite Cardinal. Informa o número de @noAutoLink(bytes) a serem escritos)
    @param(DelayBetweenCmds Cardinal. Tempo em milisegundos entre comandos de
           leitura e escrita)
    @param(CallBack TDriverCallBack. Procedimento que será chamado para retorno
           dos dados lidos/escritos)
    @param(IsWriteValue Boolean. Caso @true informa ao driver se o conjunto de comandos é para
                        escrita de dados, aumentando a prioridade de seu processamento.)
    @return(Retorna o ID do pacote caso tenha exito. Retorna 0 (zero) caso o
            componente esteja sendo destruido ou a porta não esteja aberta.)
    @seealso(TIOCommand)
    @seealso(BYTES)
    @seealso(TDriverCallBack)
    @seealso(IOCommandASync)
    }
    function IOCommandSync(Cmd:TIOCommand; ToWrite:BYTES; BytesToRead,
                           BytesToWrite, DriverID, DelayBetweenCmds:Cardinal;
                           CallBack:TDriverCallBack; IsWriteValue:Boolean;
                           Res1:TObject; Res2:Pointer):Cardinal;

    {:
    Trava a porta para uso exclusivo
    @param(DriverID Cardinal. Identifica o quem deseja obter uso exclusivo.)
    @returns(@true caso o função trave o driver para uso exclusivo, @false para o contrário)
    }
    function Lock(DriverID:Cardinal):Boolean;
    
    {:
    Remove a exclusividade de uso do driver de porta, deixando a porta para ser usada
    livremente por todos.
    @param(DriverID Cardinal. Identifica quem tem exclusividade sobre o driver.)
    @returns(@true caso consiga remover o uso exclusivo do driver.)
    }
    function Unlock(DriverID:Cardinal):Boolean;

  published
    //:Se o valor da propriedade for @true, ativa (abre) a porta, caso contrário fecha.
    property Active:Boolean read PActive write SetActive stored true default false;
    //:Caso @true, limpa os buffers de leitura e escrita quando houver erros de comunicação.
    property ClearBuffersOnCommErrors:Boolean read PClearBufOnErr write PClearBufOnErr default true;
    //:Informa o ID (número único) de quem travou para uso exclusivo o driver de porta.
    property LockedBy:Cardinal read PLockedBy;
    //:Caso @true, informa que o driver está sendo usado exclusivamente por alguem.
    property Locked:Boolean read GetLocked;
    //: Informa o codigo do último erro registrado pelo sistema operacional.
    property LastOSErrorNumber:Integer read FLastOSErrorNumber;
    //: Informa a mensagem do último erro registrado pelo sistema operacional.
    property LastOSErrorMessage:String read FLastOSErrorMessage;
    //: Informa quantos comandos são processados por segundos. Atualizado a cada 1 segundo.
    property CommandsPerSecond:Integer read FCommandsSecond;
    //: Total de bytes transmitidos.
    property TXBytes:Int64 read FTXBytes;
    //: Total de bytes transmitidos no último segundo.
    property TXBytesSecond:Int64 read FTXBytesSecond;
    //: Total de bytes recebidos.
    property RXBytes:Int64 read FRXBytes;
    //: Total de bytes recebidos no último segundo.
    property RXBytesSecond:Int64 read FRXBytesSecond;
    //: Logar ações de leitura e escrita do driver
    property LogIOActions:Boolean read FLogActions write SetLogActions default false;
    //: Arquivo onde serão armazenados os logs do driver.
    property LogFile:String read FLogFile write SetLogFile;
  end;

{$IFNDEF FPC}
const
  LineEnding = #13#10;
{$ENDIF}

implementation

uses SysUtils, ProtocolDriver, hsstrings;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  THREAD DE NOTIFICAÇÃO DE EVENTOS DE COMUNICAÇÃO.
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TEventNotificationThread.Create(CreateSuspended: Boolean; AOwner:TComponent);
begin
  inherited Create(CreateSuspended);
  FOwner:=AOwner;
  FSpool:=TMessageSpool.Create;
  FDoSomethingEvent:=TCrossEvent.Create(nil,true,false,'DoSomethingEventThread'+IntToStr(UniqueID));
  FInitEvent:=TCrossEvent.Create(nil,true,false,'WasInitialized'+IntToStr(UniqueID));
end;

destructor TEventNotificationThread.Destroy;
begin
  inherited Destroy;
  FDoSomethingEvent.Destroy;
  FSpool.Destroy;
end;

procedure TEventNotificationThread.WaitInit;
begin
  if FInitEvent.WaitFor($FFFFFFFF)<>wrSignaled then
    raise Exception.Create(SUpdateThreadWinit);
end;

procedure TEventNotificationThread.DoSomething;
begin
  FDoSomethingEvent.SetEvent;
end;

procedure TEventNotificationThread.WaitToDoSomething;
begin
  FDoSomethingEvent.WaitFor(1);
  FDoSomethingEvent.ResetEvent;
end;

procedure TEventNotificationThread.Terminate;
begin
  inherited Terminate;
  DoSomething;
end;

procedure TEventNotificationThread.Execute;
begin
  FInitEvent.SetEvent;
  while not Terminated do begin
    try
      WaitToDoSomething;
      while FSpool.PeekMessage(PMsg,PSM_COMMERROR,PSM_PORT_EVENT,true) do begin
        case PMsg.MsgID of
          PSM_COMMERROR:
          begin
            FEvent:=PMsg.wParam;
            FError:=TIOResult(PMsg.lParam);
            Synchronize(SyncCommErrorEvent);
            Dispose(PCommPortErrorEvent(FEvent));
          end;
          PSM_PORT_EVENT:begin
            FEvent:=PMsg.wParam;
            Synchronize(SyncPortEvent);
            Dispose(PNotifyEvent(FEvent));
          end;
        end;
      end;
    except
      on e:Exception do begin
        {$IFDEF FDEBUG}
        DebugLn('Exception in UpdateThread: '+ E.Message);
        DumpStack;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TEventNotificationThread.DoCommErrorEvent(Event:TCommPortErrorEvent; Error:TIOResult);
var
  p:PCommPortErrorEvent;
begin
  new(p);
  p^:=Event;
  FSpool.PostMessage(PSM_COMMERROR, p, Pointer(Error),false);
  DoSomething;
end;

procedure TEventNotificationThread.DoCommPortEvent(Event:TNotifyEvent);
var
  p:PNotifyEvent;
begin
  new(p);
  p^:=Event;
  FSpool.PostMessage(PSM_PORT_EVENT,p,nil,false);
  DoSomething;
end;

procedure TEventNotificationThread.SyncCommErrorEvent;
var
  ievt:TCommPortErrorEvent;
begin
  if FEvent=nil then exit;
  try
    ievt:=TCommPortErrorEvent(FEvent^);
    ievt(FError);
  finally
  end;
end;

procedure TEventNotificationThread.SyncPortEvent;
var
  ievt:TNotifyEvent;
begin
  if FEvent=nil then exit;
  try
    ievt:=TNotifyEvent(FEvent^);
    ievt(FOwner);
  finally
  end;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  DECLARAÇÃO DO COMPONENTE PORTA DE COMUNICAÇÃO
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TCommPortDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOwnerThread:=GetCurrentThreadId;
  FExclusiveDevice:=false;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer:=TimerStatistics;
  FTimer.Enabled:=false;
  FTimer.Interval:=1000;
  FLastOSErrorMessage:='';
  FLastOSErrorNumber:=0;
  PIOCmdCS := TCriticalSection.Create;
  PLockCS  := TCriticalSection.Create;
  PLockEvent := TCrossEvent.Create(nil,True,True,Name);
  PUnlocked:=0;
  PClearBufOnErr := true;

  PEventUpdater:=TEventNotificationThread.Create(true, Self);
  PEventUpdater.Resume;
  PEventUpdater.WaitInit;
end;

destructor TCommPortDriver.Destroy;
var
  c:Integer;
begin
  for c:=0 to High(Protocols) do
    TProtocolDriver(Protocols[c]).CommunicationPort := nil;
  for c:=0 to High(EventInterfaces) do
    EventInterfaces[c].DoPortRemoved(self);
  PEventUpdater.Terminate;
  PEventUpdater.Destroy;
  Active := false;
  SetLength(Protocols,0);
  PIOCmdCS.Destroy;
  PLockCS.Destroy;
  PLockEvent.Destroy;
  FTimer.Destroy;
  inherited Destroy;
end;

procedure TCommPortDriver.AddProtocol(Prot:TComponent);
var
  c:Integer;
  found, interfaced:Boolean;
begin
  interfaced := Supports(Prot,IPortDriverEventNotification);
  if not interfaced then
    if not (Prot is TProtocolDriver) then
      raise Exception.Create(SCompIsntADriver);

  found := false;
  if interfaced then begin
    for c:=0 to High(EventInterfaces) do
      if EventInterfaces[c]=(Prot as IPortDriverEventNotification) then begin
        found := true;
        break;
      end;
  end else begin
    for c:=0 to High(Protocols) do
      if Protocols[c]=Prot then begin
        found := true;
        break;
      end;
  end;

  if not found then begin
    if interfaced then begin
      c:=length(EventInterfaces);
      SetLength(EventInterfaces,c+1);
      EventInterfaces[c] := (Prot as IPortDriverEventNotification);
    end else begin
      c:=length(Protocols);
      SetLength(Protocols,c+1);
      Protocols[c] := Prot;
    end;
  end;
end;

procedure TCommPortDriver.DelProtocol(Prot:TComponent);
var
  found, interfaced:Boolean;
  c:Integer;
begin
  interfaced := Supports(Prot,IPortDriverEventNotification);
  found := false;
  if interfaced then begin
    for c:=0 to High(EventInterfaces) do
      if EventInterfaces[c]=(Prot as IPortDriverEventNotification) then begin
        found := true;
        break;
      end;
  end else begin
    for c:=0 to High(Protocols) do
      if Protocols[c]=Prot then begin
        found := true;
        break;
      end;
  end;

  if found then begin
    if interfaced then begin
      EventInterfaces[c] := EventInterfaces[High(EventInterfaces)];
      SetLength(EventInterfaces,High(EventInterfaces));
    end else begin
      Protocols[c] := Protocols[High(Protocols)];
      SetLength(Protocols,High(Protocols));
    end;
  end;
end;

function  TCommPortDriver.ComSettingsOK:Boolean;
begin
   Result:=false;
end;

procedure TCommPortDriver.IOCommand(cmd:TIOCommand; Packet:PIOPacket);
begin
  if csDestroying in ComponentState then
     exit;

  case cmd of
    iocRead:
      Read(Packet);
    iocReadWrite:
      begin
        Read(Packet);
        NeedSleepBetweenRW;
        Write(Packet);
      end;
    iocWrite:
      Write(Packet);
    iocWriteRead:
      begin
        Write(Packet);
        NeedSleepBetweenRW;
        Read(Packet);
      end;
  end;
  FRXBytes := FRXBytes + Packet.Received;
  FTXBytes := FTXBytes + Packet.Wrote;
end;

procedure TCommPortDriver.DoCommError(WriteCmd:Boolean; Error:TIOResult);
var
  evt:TCommPortErrorEvent;
begin

  if (WriteCmd and (not Assigned(FOnCommErrorWriting))) or ((not WriteCmd) and (not Assigned(FOnCommErrorReading))) then
    exit;

  if FOwnerThread=GetCurrentThreadId then begin
    try
      if WriteCmd then begin
        FOnCommErrorWriting(Error);
      end else begin
        FOnCommErrorReading(Error);
      end;
    finally
    end;
  end else begin
    if WriteCmd then begin
      evt := FOnCommErrorWriting
    end else begin
      evt := FOnCommErrorReading;
    end;

    PEventUpdater.DoCommErrorEvent(evt,Error);
  end;
end;

procedure TCommPortDriver.DoCommPortOpened;
var
  c:Integer;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  if not Assigned(FOnCommPortOpened) then exit;

  if GetCurrentThreadId=FOwnerThread then begin
    try
      FOnCommPortOpened(Self);
    finally
    end;
    for c:=0 to High(EventInterfaces) do
      if ntePortOpen in EventInterfaces[c].NotifyThisEvents then
        EventInterfaces[c].DoPortOpened(Self);
  end else begin
    PEventUpdater.DoCommPortEvent(FOnCommPortOpened);
    for c:=0 to High(EventInterfaces) do
      if ntePortOpen in EventInterfaces[c].NotifyThisEvents then
        PEventUpdater.DoCommPortEvent(EventInterfaces[c].GetPortOpenedEvent);
  end;
end;

procedure TCommPortDriver.DoCommPortOpenError;
var
  c:Integer;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  if not Assigned(FOnCommPortOpened) then exit;

  if GetCurrentThreadId=FOwnerThread then begin
    try
      FOnCommPortOpenError(Self);
    finally
    end;
  end else begin
    PEventUpdater.DoCommPortEvent(FOnCommPortOpenError);
  end;
end;

procedure TCommPortDriver.DoCommPortClose;
var
  c:Integer;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  if not Assigned(FOnCommPortClosed) then exit;

  if GetCurrentThreadId=FOwnerThread then begin
    try
      FOnCommPortClosed(Self);
    finally
    end;
    for c:=0 to High(EventInterfaces) do
      if ntePortClosed in EventInterfaces[c].NotifyThisEvents then
        EventInterfaces[c].DoPortClosed(Self);
  end else begin
    PEventUpdater.DoCommPortEvent(FOnCommPortClosed);
    for c:=0 to High(EventInterfaces) do
      if ntePortClosed in EventInterfaces[c].NotifyThisEvents then
        PEventUpdater.DoCommPortEvent(EventInterfaces[c].GetPortClosedEvent);
  end;
end;

procedure TCommPortDriver.DoCommPortCloseError;
var
  c:Integer;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  if not Assigned(FOnCommPortClosed) then exit;

  if GetCurrentThreadId=FOwnerThread then begin
    try
      FOnCommPortCloseError(Self);
    finally
    end;
  end else begin
    PEventUpdater.DoCommPortEvent(FOnCommPortCloseError);
  end;
end;

procedure TCommPortDriver.DoCommPortDisconected;
var
  c:Integer;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  if not Assigned(FOnCommPortDisconnected) then exit;

  if GetCurrentThreadId=FOwnerThread then begin
    try
      FOnCommPortDisconnected(Self);
    finally
    end;
    for c:=0 to High(EventInterfaces) do
      if ntePortDisconnected in EventInterfaces[c].NotifyThisEvents then
        EventInterfaces[c].DoPortDisconnected(Self);
  end else begin
    PEventUpdater.DoCommPortEvent(FOnCommPortDisconnected);
    for c:=0 to High(EventInterfaces) do
      if ntePortDisconnected in EventInterfaces[c].NotifyThisEvents then
        PEventUpdater.DoCommPortEvent(EventInterfaces[c].GetPortDisconnectedEvent);
  end;
end;

procedure TCommPortDriver.Loaded;
begin
  inherited Loaded;
  SetActive(FReadActive);
  SetLogActions(FReadedLogActions);
end;

procedure TCommPortDriver.TimerStatistics(Sender:TObject);
begin
  FCommandsSecond:= PPacketID - FLastPkgId;
  FTXBytesSecond := FTXBytes  - FTXBytesLast;
  FRXBytesSecond := FRXBytes  - FRXBytesLast;

  FRXBytesLast := FRXBytes;
  FTXBytesLast := FTXBytes;
  FLastPkgId  := PPacketID;
end;

function TCommPortDriver.GetLocked:Boolean;
begin
  Result := (PLockedBy<>0);
end;

function TCommPortDriver.Lock(DriverID:Cardinal):Boolean;
begin
  try
    PLockCS.Enter;
    if PLockedBy=0 then begin
      PLockedBy := DriverID;
      PLockEvent.ResetEvent;
      Result := true;
    end else
      Result := false;
  finally
    PLockCS.Leave;
  end;

  //espera todos acabarem seus comandos.
  while PUnlocked>0 do
    {$IFDEF FPC}
    ThreadSwitch;
    {$ELSE}
    SwitchToThread;
    {$ENDIF}

end;

function TCommPortDriver.Unlock(DriverID:Cardinal):Boolean;
begin
  try
    PLockCS.Enter;
    if (PLockedBy=0) or (DriverID=PLockedBy) then begin
      PLockedBy := 0;
      PLockEvent.SetEvent;
      Result := true;
    end else
      Result := false;
  finally
    PLockCS.Leave;
  end;
end;

procedure TCommPortDriver.SetActive(v:Boolean);
var
   x:boolean;
begin
  //se esta carregando as propriedades
  if csReading in ComponentState then begin
    FReadActive := v;
    exit;
  end;

  //evita a abertura/fechamento da porta em edição, quando um dispositivo
  //e de uso exclusivo (porta serial).
  if FExclusiveDevice and (csDesigning in ComponentState) then begin
    if v then begin
      if ComSettingsOK then begin
        PActive := true;
      end;
    end else begin
      PActive := false;
    end;
    exit;
  end;

  if v and (not PActive) then begin
     InternalPortStart(x);
  end;
  
  if (not v) and PActive then begin
     InternalPortStop(x);
  end;
  FTimer.Enabled := PActive;
end;

procedure TCommPortDriver.OpenInEditMode(v:Boolean);
begin

end;

function TCommPortDriver.IOCommandSync(Cmd:TIOCommand; ToWrite:BYTES; BytesToRead,
                                 BytesToWrite, DriverID, DelayBetweenCmds:Cardinal;
                                 CallBack:TDriverCallBack; IsWriteValue:Boolean;
                                 Res1:TObject; Res2:Pointer):Cardinal;
var
  PPacket:TIOPacket;
  InLockCS, InIOCmdCS:Boolean;
begin
  try
    InLockCS:=false;
    InIOCmdCS:=false;

    Result := 0;

    if (csDestroying in ComponentState) or (FExclusiveDevice and (csDesigning in ComponentState)) then
       exit;

    //verify if another driver is the owner of the comm port...
    PLockCS.Enter;
    InLockCS:=true;
    while (PLockedBy<>0) and (PLockedBy<>DriverID) do begin
       PLockCS.Leave;
       InLockCS:=false;
       PLockEvent.WaitFor($FFFFFFFF);
       PLockCS.Enter;
       InLockCS:=true;
    end;
    InterLockedIncrement(PUnlocked);
    PLockCS.Leave;
    InLockCS:=false;

    PIOCmdCS.Enter;
    InIOCmdCS:=true;
    if (not PActive) then
       exit;

    inc(PPacketID);

    //cria o pacote
    PPacket.PacketID := PPacketID;
    PPacket.WriteIOResult := iorNone;
    PPacket.ToWrite := BytesToWrite;
    PPacket.Wrote := 0;
    PPacket.WriteRetries := 3;

    PPacket.BufferToWrite := ToWrite;

    PPacket.DelayBetweenCommand := DelayBetweenCmds;
    PPacket.ReadIOResult := iorNone;
    PPacket.ToRead := BytesToRead;
    PPacket.Received := 0;
    PPacket.ReadRetries := 3;
    PPacket.Res1 := Res1;
    PPacket.Res2 := Res2;
    SetLength(PPacket.BufferToRead,BytesToRead);

    //novo suspend da thread espera a thread terminar os comandos e ficar livre
    //a novos comandos, o que garante que não havera choque de comandos...
    //CommThread.Suspend; besteira... no unix a thread nao pode ser suspensa...
    //soluçao: Critical Section na procedure que executa a leitura/escrita.
    InternalIOCommand(Cmd,@PPacket);
    if Assigned(CallBack) then
      CallBack(PPacket);

    //libera a memoraia alocada...
    SetLength(PPacket.BufferToWrite,0);
    SetLength(PPacket.BufferToRead, 0);

    //retorna o packet ID....
    Result := PPacketID;
  finally
    if InIOCmdCS then
      PIOCmdCS.Leave;
    if InLockCS then
      PLockCS.Leave;
    InterLockedDecrement(PUnlocked);
  end;
end;

procedure TCommPortDriver.InternalIOCommand(cmd:TIOCommand; Packet:PIOPacket);
begin
  try
     PIOCmdCS.Enter;
     //verifica comandos e sinaliza os pacotes de acordo...
     if PActive then begin
       try
         //executa o commando de io...
         IOCommand(cmd,Packet);
       except
         if cmd in [iocRead, iocReadWrite, iocWriteRead] then
           Packet^.ReadIOResult := iorPortError;
         if cmd in [iocWrite, iocReadWrite, iocWriteRead] then
           Packet^.WriteIOResult := iorPortError;
       end;
     end else begin
       if cmd in [iocRead, iocReadWrite, iocWriteRead] then
         Packet^.ReadIOResult := iorNotReady;
       if cmd in [iocWrite, iocReadWrite, iocWriteRead] then
         Packet^.WriteIOResult := iorNotReady;
     end;
     if FLogActions then
       LogAction(cmd, Packet^);
  finally
     PIOCmdCS.Leave;
  end;
end;

procedure TCommPortDriver.InternalPortStart(var Ok:Boolean);
begin
  try
     PIOCmdCS.Enter;
     PortStart(ok);
     if Ok then
       DoCommPortOpened
     else
       DoCommPortOpenError;
  finally
     PIOCmdCS.Leave;
  end;
end;

procedure TCommPortDriver.InternalPortStop(var Ok:Boolean);
begin
  try
     PIOCmdCS.Enter;
     PortStop(ok);
     if Ok then
       DoCommPortClose
     else
       DoCommPortCloseError;
  finally
     PIOCmdCS.Leave;
  end;
end;

procedure TCommPortDriver.InternalClearALLBuffers;
begin
  try
     PIOCmdCS.Enter;
     ClearALLBuffers;
  finally
     PIOCmdCS.Leave;
  end;
end;

procedure TCommPortDriver.DoExceptionInActive;
begin
  if PActive then begin
    if (ComponentState*[csDesigning]=[]) or ((ComponentState*[csDesigning]<>[]) and FExclusiveDevice=false) then
      raise Exception.Create(SimpossibleToChangeWhenActive);
  end;
end;

procedure TCommPortDriver.RefreshLastOSError;
{$IFNDEF FPC}
{$IF defined(WIN32) or defined(WIN64)}
var
  buffer:PAnsiChar;
{$IFEND}
{$ENDIF}
begin
{$IFDEF FPC}
  FLastOSErrorNumber:=GetLastOSError;
  FLastOSErrorMessage:=SysErrorMessage(FLastOSErrorNumber);
{$ELSE}
{$IF defined(WIN32) or defined(WIN64)}
  FLastOSErrorNumber:=GetLastError;
  GetMem(buffer, 512);
  if FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,nil,FLastOSErrorNumber,LANG_NEUTRAL,Buffer,512,nil)<>0 then begin
    FLastOSErrorMessage:=Buffer;
    FreeMem(buffer);
  end else
    FLastOSErrorMessage:=SFaultGettingLastOSError;
{$IFEND}
{$ENDIF}
end;

procedure  TCommPortDriver.SetLogActions(Log:Boolean);
var
  canopen:Boolean;
begin
  PIOCmdCS.Enter;
  try
    canopen:=false;
    if Log=FLogActions then exit;

    if [csReading]*ComponentState<>[] then begin
      FReadedLogActions:=Log;
      exit;
    end;

    if [csDesigning]*ComponentState<>[] then begin
      canopen:=(Trim(FLogFile)<>'');
      exit;
    end;

    if log then begin
      FLogFileStream:=TFileStream.Create(FLogFile,fmCreate);
    end else
      FLogFileStream.Destroy;
    canopen:=true;
  finally
    FLogActions:=Log and canopen;
    PIOCmdCS.Leave;
  end;
end;

procedure  TCommPortDriver.SetLogFile(nFile:String);
var
  islogging:Boolean;
begin
  PIOCmdCS.Enter;
  try
    if nFile=FLogFile then exit;
    islogging:=FLogActions;
    LogIOActions:=false;
    FLogFile:=nFile;
    LogIOActions:=islogging;
  finally
    PIOCmdCS.Leave;
  end;
end;

procedure  TCommPortDriver.LogAction(cmd:TIOCommand; Packet:TIOPacket);
  function bufferToHex(Buf:BYTES):String;
  var
    c:integer;
  begin
    Result:='';
    for c:=0 to High(Buf) do
      Result:=Result+IntToHex(Buf[c],2)+' ';
  end;

  function TranslateCmdName(cmd:TIOCommand):String;
  begin
    case cmd of
      iocNone:
        Result := 'iocNone     ';
      iocRead:
        Result := 'iocRead     ';
      iocReadWrite:
        Result := 'iocReadWrite';
      iocWrite:
        Result := 'iocWrite    ';
      iocWriteRead:
        Result := 'iocWriteRead';
    end;
  end;

  function TranslateResultName(res:TIOResult):String;
  const
    EnumMap: array[TIOResult] of String = ('iorOK       ',
                                           'iorTimeOut  ',
                                           'iorNotReady ',
                                           'iorNone     ',
                                           'iorPortError');
  begin
    Result := EnumMap[res];
  end;

  var
    FS:TStringStream;
    timestamp:String;
begin
  try
    if not FLogActions then exit;
    FS:=TStringStream.Create('');
    timestamp := FormatDateTime('mmm-dd hh:nn:ss.zzz',Now);
    if cmd=iocRead then begin
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.ReadIOResult) +', Received: '+bufferToHex(Packet.BufferToRead)+LineEnding);
    end;
    if cmd=iocReadWrite then begin
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.ReadIOResult) +', Received: '+bufferToHex(Packet.BufferToRead)+LineEnding);
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.WriteIOResult)+', Wrote:    '+bufferToHex(Packet.BufferToWrite)+LineEnding);
    end;

    if cmd=iocWriteRead then begin
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.WriteIOResult)+', Wrote:    '+bufferToHex(Packet.BufferToWrite)+LineEnding);
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.ReadIOResult) +', Received: '+bufferToHex(Packet.BufferToRead)+LineEnding);
    end;

    if cmd=iocWrite then begin
      fs.WriteString(timestamp+', '+TranslateCmdName(cmd)+', Result='+TranslateResultName(Packet.WriteIOResult)+', Wrote:    '+bufferToHex(Packet.BufferToWrite)+LineEnding);
    end;
    FS.Position:=0;
    FLogFileStream.CopyFrom(FS,FS.Size);
  finally
    FS.Free;
  end;
end;

end.
