{$i ../common/pscada_settings.inc}
{$IFDEF PORTUGUES}
{:

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit pSCADA_Types;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$MACRO ON}
{$ELSE}
  {$ERROR This unit should be used with FPC + Lazarus, never with Delphi!}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  {$IFDEF PORTUGUES}
  {:
  Evento chamado quando é necessário saber o atual estado do componente.
  @seealso(TZones)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Procedure used to return the current state of the owner component.
  @seealso(TZones)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TNeedCompStateEvent = procedure(var CurState:TComponentState) of object;

  {$IFNDEF FPC}
    {$IFDEF DELPHI_XE2_UP}
    crossNativeUInt = NativeUInt;
    {$ELSE}
    crossNativeUInt = Cardinal;
    {$ENDIF}
  {$ELSE}
    crossNativeUInt = PtrUInt;
  {$ENDIF}

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
  TpSCADAThreadID = TThreadID;
  {$ELSE}
  TpSCADAThreadID = THandle;
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

  TpSCADA_CPU = (pSCADA_CPU1,
                 pSCADA_CPU2,
                 pSCADA_CPU3,
                 pSCADA_CPU4,
                 pSCADA_CPU5,
                 pSCADA_CPU6,
                 pSCADA_CPU7,
                 pSCADA_CPU8,
                 pSCADA_CPU9,
                 pSCADA_CPU10,
                 pSCADA_CPU11,
                 pSCADA_CPU12,
                 pSCADA_CPU13,
                 pSCADA_CPU14,
                 pSCADA_CPU15,
                 pSCADA_CPU16
                );

  TpSCADA_CPUs = set of TpSCADA_CPU;

implementation

end.

