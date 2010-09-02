{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  @abstract(Unit com os tipos que são usados frequentemente em drivers de portas de comunicação.)
}
unit commtypes;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses Classes;

type
  {:
  Representa uma sequencia de bytes.
  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  }
  BYTES = array of Byte;

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
  TIOCommand = (iocNone, iocRead, iocReadWrite, iocWrite, iocWriteRead);

  {:
  @name define os possiveis resultados de um pedido de Leitura/Escrita.

  @value iorOk O comando teve sucesso.
  @value iorTimeOut O comando excedeu o tempo máximo de execução.
  @value iorNotReady A porta não estava pronta para executar o comando. Ex: porta fechada.
  @value iorNone O comando não foi processado;
  @value iorPortError Houve falha do driver de porta enquanto processava o comando.
  @seealso(TIOPacket)
  }
  TIOResult  = (iorOK, iorTimeOut, iorNotReady, iorNone, iorPortError);

  {:
  Retorna os resultados de um pedido de leitura/escrita de um driver de porta.

  @member PacketID Número identificador do pacote.
  @member WriteIOResult Resultado do comando de escrita se existir. Caso não exista retorna iorNone.
  @member ToWrite Quantidade de @noAutoLink(bytes) a escrever.
  @member Wrote Quantidade de @noAutoLink(bytes) escritos.
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
  TIOPacket = record
    PacketID:Cardinal;
    WriteIOResult:TIOResult;
    ToWrite:Cardinal;
    Wrote:Cardinal;
    WriteRetries:Cardinal;
    BufferToWrite:BYTES;
    DelayBetweenCommand:Integer;
    ReadIOResult:TIOResult;
    ToRead:Cardinal;
    Received:Cardinal;
    ReadRetries:Cardinal;
    BufferToRead:BYTES;
    Res1:TObject;
    Res2:Pointer;
  end;
  {:
  Aponta para uma estrutura TIOPacket.
  @seealso(TIOPacket)
  }
  PIOPacket = ^TIOPacket;

  {:
  Procedimento de callback usado para retornar os resultados de um grupo de
  comandos de leitura/escrita feitos por TCommPortDriver.IOCommandASync e
  TCommPortDriver.IOCommandSync. Passa os resultados atraves da variável Result.

  @seealso(TCommPortDriver)
  @seealso(TIOPacket)  
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  TDriverCallBack = procedure(var Result:TIOPacket) of object;

  TPSThreadID = TThreadID;

  //: Procedimento para sinalizar um evento de erro na porta
  TCommPortErrorEvent = procedure(Error:TIOResult) of object;
  PCommPortErrorEvent = ^TCommPortErrorEvent;

  TCommPortGenericError = procedure of object;
  PCommPortGenericError = ^TCommPortGenericError;

  PNotifyEvent = ^TNotifyEvent;

  {:
  Aponta para um procedimento do tipo TDriverCallBack.
  @seealso(TDriverCallBack)
  }
  PDriverCallBack = ^TDriverCallBack;

  {:
  Procedimento responsável por realizar os pedidos de leitura/escrita do driver.
  Definição usada como evento internamente pelo driver de porta.
  
  @param(cmd TIOCommand. Grupo de comandos a executar.)
  @param(Packet PIOPacket. Estrutura com os parametros de execução e onde devem
         ser retornados os resultados dos comandos de leitura/escrita.)

  @seealso(TCommPortDriver.IOCommand)
  @seealso(TIOPacket)
  }
  TDriverCommand  = procedure(cmd:TIOCommand; Packet:PIOPacket) of object;

  {:
  Procedimento responsável por realizar os pedidos abertura/fechamento de portas
  do driver.
  Definição usada como evento internamente pelo driver de porta.
  @param(Ok Boolean. Indica se a porta foi aberta ou fechada com sucesso.)

  @seealso(TCommPortDriver.PortStart)
  @seealso(TCommPortDriver.PortStop)
  }
  TDriverCaller   = procedure(var Ok:Boolean) of object;

  {:
  Estrutura usada internamente pelo driver de porta para associar um callback
  com a sua estrutura de retorno de resultados.
  @seealso(TCommPortDriver.IOCommandASync)
  }

  TCommandPacket = record
    Packet:TIOPacket;
    Callback:TDriverCallBack;
  end;
  {:
  Aponta para uma estrutura TCommandPacket.
  @seealso(TCommandPacket)
  }  
  PCommandPacket = ^TCommandPacket;

  {:
    Interface
  }
  IPortDriverEventNotification = interface
  ['{26B0F551-5B46-49D9-BCA1-AD621B3775CF}']
    procedure PortOpened(Sender: TObject);
    procedure PortClosed(Sender: TObject);
    procedure PortDisconnected(Sender: TObject);
    procedure ReadError(Sender:TObject);
    procedure WriteError(Sender:TObject);
    procedure PortRemoved(Sender:TObject);
  end;

  {:
  Ordena thread de atualização a chamar o TDriverCallBack solicitado informando
  dos resultados do pedido de leitura/escrita.
  @seealso(TDriverCallBack)
  @seealso(TIOPacket)
  }
  const PSM_CALLBACK         = 1;

  {:
  Ordena thread de atualização a voltar a chamar TDriverCallBack fornecido.
  @seealso(TDriverCallBack)
  }
  const PSM_RESUMECALLBACK   = 2;
  
  {:
  Ordena thread de atualização a não chamar mais TDriverCallBack fornecido a fim
  de evitar violações de acesso.
  @seealso(TDriverCallBack)
  }
  const PSM_CANCELCALLBACK   = 3;

  //: Mensagem de erro de comunicação (leitura ou escrita);
  const PSM_COMMERROR        = 4;
  //: Mensagem de erro de porta aberta, fechada ou disconectada.
  const PSM_PORT_EVENT       = 5;

  //: Mensagem de pedido de leitura de @bold(baixa) prioridade
  const PSM_READ_READ      = 50;
  //: Mensagem de pedido de leitura e outro de escrita de @bold(baixa) prioridade
  const PSM_READ_READWRITE = 52;
  //: Mensagem de pedido de escrita de @bold(baixa) prioridade
  const PSM_READ_WRITE     = 54;
  //: Mensagem de pedido de escrita e outro de leitura de @bold(baixa) prioridade
  const PSM_READ_WRITEREAD = 56;

  //: Mensagem de pedido de leitura de @bold(alta) prioridade
  const PSM_WRITE_READ      = 60;
  //: Mensagem de pedido de leitura e outro de escrita de @bold(alta) prioridade
  const PSM_WRITE_READWRITE = 62;
  //: Mensagem de pedido de escrita de @bold(alta) prioridade
  const PSM_WRITE_WRITE     = 64;
  //: Mensagem de pedido de escrita e outro de leitura de @bold(alta) prioridade
  const PSM_WRITE_WRITEREAD = 66;

  {:
  Converte uma mensagem interna para um grupo de commandos de leitura/escrita.
  @param(wMessage Cardinal. Mensagem a converter.)
  @return(Retorna um comando do tipo TIOCommand. Caso wMessage não feche com o
          case, retorna o padrão que é iocNone).
  @seealso(TIOCommand)
  @seealso(PSM_READ_READ)
  @seealso(PSM_READ_READWRITE)
  @seealso(PSM_READ_WRITE)
  @seealso(PSM_READ_WRITEREAD)
  @seealso(PSM_WRITE_READ)
  @seealso(PSM_WRITE_READWRITE)
  @seealso(PSM_WRITE_WRITE)
  @seealso(PSM_WRITE_WRITEREAD)
  
  }
  function WindowsMessageToIOCommand(wMessage:Cardinal):TIOCommand;
  
  {:
  Converte um grupo de comandos de leitura/escrita para um Id de mensagem.
  @param(ioCommand TIOCommand. Grupo de comandos a converter.)
  @param(ToWrite Boolean. Indica se é um comando de escrita (alta prioridade).)

  @seealso(TIOCommand)
  @seealso(PSM_READ_READ)
  @seealso(PSM_READ_READWRITE)
  @seealso(PSM_READ_WRITE)
  @seealso(PSM_READ_WRITEREAD)
  @seealso(PSM_WRITE_READ)
  @seealso(PSM_WRITE_READWRITE)
  @seealso(PSM_WRITE_WRITE)
  @seealso(PSM_WRITE_WRITEREAD)

  }
  function IOCommandToWindowsMessage(ioCommand:TIOCommand; ToWrite:Boolean):Cardinal;

  {:
  Concatena dois buffers de bytes.
  @seealso(BYTES)
  }
  function ConcatenateBYTES(const a,b:BYTES):BYTES;

implementation

//concatenate two buffers of bytes.
function ConcatenateBYTES(const a,b:BYTES):BYTES;
var
  c:Integer;
begin
  SetLength(Result,Length(a)+Length(b));
  for c:=0 to High(a) do
    Result[c]:=a[c];
  for c:=0 to High(b) do
    Result[c+Length(a)]:=b[c];
end;

function WindowsMessageToIOCommand(wMessage:Cardinal):TIOCommand;
begin
  case wMessage of
    PSM_READ_READ, PSM_WRITE_READ:
      Result:=iocRead;

    PSM_READ_READWRITE, PSM_WRITE_READWRITE:
      Result:=iocReadWrite;

    PSM_READ_WRITE, PSM_WRITE_WRITE:
      Result:=iocWrite;

    PSM_READ_WRITEREAD, PSM_WRITE_WRITEREAD:
      Result:=iocWriteRead;

    else
      Result:=iocNone;
  end;
end;

function IOCommandToWindowsMessage(ioCommand:TIOCommand; ToWrite:Boolean):Cardinal;
begin
  if ToWrite then
    case ioCommand of
      iocRead:
        Result:=PSM_WRITE_READ;
      iocReadWrite:
        Result:=PSM_WRITE_READWRITE;
      iocWrite:
        Result:=PSM_WRITE_WRITE;
      iocWriteRead:
        Result:=PSM_WRITE_WRITEREAD;
      else
        Result:=0;
    end
  else
    case ioCommand of
      iocRead:
        Result:=PSM_READ_READ;
      iocReadWrite:
        Result:=PSM_READ_READWRITE;
      iocWrite:
        Result:=PSM_READ_WRITE;
      iocWriteRead:
        Result:=PSM_READ_WRITEREAD;
      else
        Result:=0;
    end
end;

end.
