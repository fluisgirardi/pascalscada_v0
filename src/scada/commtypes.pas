{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit com os tipos que são usados frequentemente em drivers de portas de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit with types/definitions commonly used on communication ports.)
}
{$ENDIF}

unit commtypes;

interface

uses Classes;

type
  {$IFDEF PORTUGUES}
  {:
  Representa uma sequencia de bytes.
  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  }
  {$ELSE}
  {:
  Sequence of bytes.
  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  }
  {$ENDIF}

  BYTES = array of Byte;

  {$IFDEF PORTUGUES}
  {:
  @name define os comandos e a ordem de execução em um pedido de Leitura/Escrita.

  @value iocNone Não faz nada;
  @value iocRead Executa uma leitura;
  @value iocReadWrite Executa uma leitura e logo apos uma escrita;
  @value iocWrite Executa uma escrita;
  @value iocWriteRead Executa uma escrita e logo apos uma leitura;

  @seealso(TCommPortDriver)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  @seealso(TIOPacket)
  }
  {$ELSE}
  {:
  @name define the commands and their sequence of execution.

  @value iocNone Does nothing;
  @value iocRead Executes a read;
  @value iocReadWrite Executes a read command and after this a write command;
  @value iocWrite Executes a write command;
  @value iocWriteRead Executes a write command and after this a read command;

  @seealso(TCommPortDriver)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  @seealso(TIOPacket)
  }
  {$ENDIF}
  TIOCommand = (iocNone, iocRead, iocReadWrite, iocWrite, iocWriteRead);

  {$IFDEF PORTUGUES}
  {:
  @name define os possiveis resultados de um pedido de Leitura/Escrita.

  @value iorOk O comando teve sucesso.
  @value iorTimeOut O comando excedeu o tempo máximo de execução.
  @value iorNotReady A porta não estava pronta para executar o comando. Ex: porta fechada.
  @value iorNone O comando não foi processado;
  @value iorPortError Houve falha do driver de porta enquanto processava o comando.
  @seealso(TIOPacket)
  }
  {$ELSE}
  {:
  @name define the results of an I/O request.

  @value iorOk The request was done with successfull.
  @value iorTimeOut The request has a timeout.
  @value iorNotReady The communication port isn't ready yet. Example: communication port closed.
  @value iorNone The command was not processed;
  @value iorPortError A fault occurred while processing the I/O command.
  @seealso(TIOPacket)
  }
  {$ENDIF}
  TIOResult  = (iorOK, iorTimeOut, iorNotReady, iorNone, iorPortError);

  {$IFDEF PORTUGUES}
  {:
  Retorna os resultados de um pedido de leitura/escrita de um driver de porta.

  @member PacketID Número identificador do pacote.
  @member WriteIOResult Resultado do comando de escrita se existir. Caso não exista retorna iorNone.
  @member ToWrite Quantidade de @noAutoLink(bytes) a escrever.
  @member Written Quantidade de @noAutoLink(bytes) escritos.
  @member WriteRetries Número de tentativas para escrever ToWrite @noAutoLink(bytes).
  @member BufferToWrite Sequencia de @noAutoLink(bytes) a escrever. @bold(Deve ter o tamanho mínimo de ToWrite @noAutoLink(bytes)).
  @member DelayBetweenCommand Atraso em milisegundos entre comandos de leitura e escrita.
  @member ReadIOResult Resultado do comando de leitura caso exista. Se não foi definido um comando de leitura retorna iorNone.
  @member ToRead Quantidade de @noAutoLink(bytes) a ler da porta.
  @member Received Quantidade de @noAutoLink(bytes) lidos da porta.
  @member ReadRetries Número de tentativas para ler ToRead @noAutoLink(bytes).
  @member BufferToRead Buffer contendo os @noAutoLink(bytes) lidos na operação. Tem o tamanho ajustado para o valor passado em ToRead.

  @seealso(TCommPortDriver)
  @seealso(TDriverCallBack)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  {$ELSE}
  {:
  Return the results of an I/O request.

  @member PacketID Request identification.
  @member WriteIOResult Result of a write command, if exists. If not exists, return iorNone.
  @member ToWrite Number of @noAutoLink(bytes) to write.
  @member Written Number of @noAutoLink(bytes) written.
  @member WriteRetries Number of retries to write ToWrite @noAutoLink(bytes).
  @member BufferToWrite Sequence of @noAutoLink(bytes) to write. @bold(Must have at least ToWrite @noAutoLink(bytes) of length).
  @member DelayBetweenCommand Delay in milliseconds between the commands of Read and Write.
  @member ReadIOResult Result of a read command, if exists. If not exists, return iorNone.
  @member ToRead Number of @noAutoLink(bytes) to read.
  @member Received Number of @noAutoLink(bytes) received.
  @member ReadRetries Number of retries to read ToRead @noAutoLink(bytes).
  @member BufferToRead Buffer that stores @noAutoLink(bytes) received. Their length is adjusted to ToRead.

  @seealso(TCommPortDriver)
  @seealso(TDriverCallBack)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  {$ENDIF}
  TIOPacket = record
    PacketID:Cardinal;
    WriteIOResult:TIOResult;
    ToWrite:Cardinal;
    Written:Cardinal;
    WriteRetries:Cardinal;
    BufferToWrite:BYTES;
    DelayBetweenCommand:LongInt;
    ReadIOResult:TIOResult;
    ToRead:Cardinal;
    Received:Cardinal;
    ReadRetries:Cardinal;
    BufferToRead:BYTES;
    Res1:TObject;
    Res2:Pointer;
  end;

  {$IFDEF PORTUGUES}
  {:
  Aponta para uma estrutura TIOPacket.
  @seealso(TIOPacket)
  }
  {$ELSE}
  {:
  Pointer to a TIOPacket record.
  @seealso(TIOPacket)
  }
  {$ENDIF}
  PIOPacket = ^TIOPacket;


  {$IFDEF PORTUGUES}
  {:
  Procedimento de callback usado para retornar os resultados de um grupo de
  comandos de leitura/escrita feitos por TCommPortDriver.IOCommandSync. Passa os
  resultados atraves da variável Result.

  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  {$ELSE}
  {:
  Defines the callback procedure to return the results of a I/O command done by
  TCommPortDriver.IOCommandSync. The result is returned by the Result variable.

  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  {$ENDIF}
  TDriverCallBack = procedure(var Result:TIOPacket) of object;

  //: @exclude
  {$IFDEF FPC}
  TPSThreadID = TThreadID;
  {$ELSE}
  TPSThreadID = THandle;
  {$ENDIF}

  {$IFDEF PORTUGUES}
  //: Procedimento para sinalizar um evento de erro na porta
  {$ELSE}
  //: Defines a method called when a communication error occurs.
  {$ENDIF}
  TCommPortErrorEvent = procedure(Error:TIOResult) of object;
  {$IFDEF PORTUGUES}
  //: Aponta para um evento de sinalização de erro.
  {$ELSE}
  //: Points to a method used to report communications errors.
  {$ENDIF}
  PCommPortErrorEvent = ^TCommPortErrorEvent;

  {$IFDEF PORTUGUES}
  //: Define o tipo dos eventos de porta aberta, fechada e desconectada.
  {$ELSE}
  //: Defines the types of events of communications port opens, closed and disconnected.
  {$ENDIF}
  TCommPortGenericError = procedure of object;
  {$IFDEF PORTUGUES}
  //: Aponta para um evento de sinalização de erro.
  {$ELSE}
  //: Points to a method used to report communications errors.
  {$ENDIF}
  PCommPortGenericError = ^TCommPortGenericError;
  {$IFDEF PORTUGUES}
  //: Aponta para um evento generico.
  {$ELSE}
  //: Points to a notification method.
  {$ENDIF}
  PNotifyEvent = ^TNotifyEvent;



  {$IFDEF PORTUGUES}
  {:
  Define as possíveis notificações de eventos que o protocolo pode registrar.
  @value ntePortOpen Notifica o driver de protocolo quando a porta de comunicação é aberta.
  @value ntePortClose Notifica o driver de protocolo quando a porta de comunicação é fechada.
  @value ntePortDisconnected Notifica o driver de protocolo quando a porta de comunicação é desconectada.
  @seealso(IPortDriverEventNotification)
  }
  {$ELSE}
  {:
  Defines the notifications that the protocol driver can register.
  @value ntePortOpen Notifies the protocol driver when the communication port was open.
  @value ntePortClose Notifies the protocol driver when the communication port was closed.
  @value ntePortDisconnected Notifies the protocol driver when the communication port was disconnected.
  @seealso(IPortDriverEventNotification)
  }
  {$ENDIF}
  TPortEvents = (ntePortOpen, ntePortClosed, ntePortDisconnected);

  {$IFDEF PORTUGUES}
  {:
  Define o conjunto de notificações que um protocolo pode registrar.
  @seealso(TPortEvents)
  }
  {$ELSE}
  {:
  Defines the set of notifications that a protocol driver can register.
  @seealso(TPortEvents)
  }
  {$ENDIF}
  TNotifyThisEvents = set of TPortEvents;

  {$IFDEF PORTUGUES}
  {:
    Interface de notificações de eventos para os drivers de protocolo.
  }
  {$ELSE}
  {:
    Event notification interface for protocol drivers.
  }
  {$ENDIF}
  IPortDriverEventNotification = interface
  ['{26B0F551-5B46-49D9-BCA1-AD621B3775CF}']
    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é aberta.
    {$ELSE}
    //: Returns the event to be called when communication port opens.
    {$ENDIF}
    function  GetPortOpenedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é fechada.
    {$ELSE}
    //: Returns the event to be called when communication port closed.
    {$ENDIF}
    function  GetPortClosedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é disconectada.
    {$ELSE}
    //: Returns the event to be called when communication port is disconnected.
    {$ENDIF}
    function  GetPortDisconnectedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    {:
    Conjunto de eventos da porta de comunicação que o protocolo deseja ser
    notificado.
    @seealso(TPortEvents)
    @seealso(TNotifyThisEvents)
    }
    {$ELSE}
    {:
    Set of events that the protocol driver wants be notified.
    @seealso(TPortEvents)
    @seealso(TNotifyThisEvents)
    }
    {$ENDIF}
    function  NotifyThisEvents:TNotifyThisEvents;

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for aberta.
    {$ELSE}
    //: Procedure called when the communication port opens.
    {$ENDIF}
    procedure DoPortOpened(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for fechada.
    {$ELSE}
    //: Procedure called when the communication port was closed.
    {$ENDIF}
    procedure DoPortClosed(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for desconectada.
    {$ELSE}
    //: Procedure called when the communication port was disconnected.
    {$ENDIF}
    procedure DoPortDisconnected(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for destruida.
    {$ELSE}
    //: Procedure called when the communication port has been destroied.
    {$ENDIF}
    procedure DoPortRemoved(Sender:TObject);
  end;

  IPortDriverEventNotificationArray = array of IPortDriverEventNotification;

  {$IFDEF PORTUGUES}
  //: Mensagem de erro de comunicação (leitura ou escrita);
  {$ELSE}
  //: Communication error messsage (read or write);
  {$ENDIF}
  const PSM_COMMERROR        = 4;

  {$IFDEF PORTUGUES}
  //: Mensagem de erro de porta aberta, fechada ou disconectada.
  {$ELSE}
  //: Message of communication port open, closed or disconnected.
  {$ENDIF}
  const PSM_PORT_EVENT       = 5;

  {$IFDEF PORTUGUES}
  {:
  Concatena dois buffers de bytes.
  @seealso(BYTES)
  }
  {$ELSE}
  {:
  Concatenate two @noAutoLink(bytes) buffers.
  @seealso(BYTES)
  }
  {$ENDIF}
  function ConcatenateBYTES(const a,b:BYTES):BYTES;

implementation

//concatenate two buffers of bytes.
function ConcatenateBYTES(const a,b:BYTES):BYTES;
var
  c:LongInt;
begin
  SetLength(Result,Length(a)+Length(b));
  for c:=0 to High(a) do
    Result[c]:=a[c];
  for c:=0 to High(b) do
    Result[c+Length(a)]:=b[c];
end;

end.
