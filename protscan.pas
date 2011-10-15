{$i language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Processa os pedidos de leitura e escrita por scan.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
@abstract(Process request of read and write by scan (asynchronous).)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit protscan;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, protscanupdate, MessageSpool, syncobjs,
  ProtocolTypes{$IFNDEF FPC}, Windows{$ENDIF};

type

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Classe de thread reponsável processar as escritas por scan e por manter os
  tags com seus valores atualizados o mais rápido possível. Usado por
  TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Thread class that processes the requests the reads and writes by scan (asynchronous)
  and keep the tag values updated. Used by the class TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  {$ENDIF}
  TScanThread = class(TCrossThread)
  private
    FInitEvent:TCrossEvent;
    FWaitToWrite:TCrossEvent;
    FEnd:TCrossEvent;

    FDoScanRead:TScanReadProc;
    FDoScanWrite:TScanWriteProc;

    FMinScan:Cardinal;
    erro:Exception;
    FSpool:TMessageSpool;
    PScanUpdater:TScanUpdate;

    procedure SyncException;
    function  WaitEnd(timeout:Cardinal):TWaitResult;
  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
    //: @exclude
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //:Ordena a thread verificar se há comandos de escrita pendentes.
    {$ELSE}
    //: Verifies if the thread has write requests on queue.
    {$ENDIF}
    procedure CheckScanWriteCmd;

    {$IFDEF PORTUGUES}
    //: Sinaliza para thread Terminar.
    {$ELSE}
    //: Requests the thread finalization.
    {$ENDIF}
    procedure Terminate;

    {$IFDEF PORTUGUES}
    {:
    Ao chamar @name, espera a thread sinalizar a sua inicialização. Se ela já foi
    inicializada, não faz nada.
    }
    {$ELSE}
    {:
    When @name is called, waits the thread initialization. If it's already
    initialized, does nothing.
    }
    {$ENDIF}
    procedure WaitInit;

    {$IFDEF PORTUGUES}
    {:
    Solicita uma escrita de valores por scan para a thread do driver de protocolo.

    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    {$ELSE}
    {:
    Put a values write request to be processed by the scan (queue) of protocol.

    @param(SWPkg PScanWriteRec. Points to a structure with informations about
           the write command.)
    @raises(Exception if the thread didn't responds.)
    }
    {$ENDIF}
    procedure ScanWrite(SWPkg:PScanWriteRec);
  published
    {$IFDEF PORTUGUES}
    {:
    Diz quantos milisegundos o driver esperar caso não seja feita nenhuma
    operação de scan, a fim de evitar alto consumo de processador inutilmente.
    }
    {$ELSE}
    {:
    How many milliseconds the thread will sleep if it didn't nothing, to avoid
    the high CPU usage.
    }
    {$ENDIF}
    property MinTimeOfScan:Cardinal read FMinScan write FMinScan nodefault;

    {$IFDEF PORTUGUES}
    //: Evento chamado para realizar a atualização do valores dos tags.
    {$ELSE}
    //: Event called to execute a scan read command.
    {$ENDIF}
    property OnDoScanRead:TScanReadProc read FDoScanRead write FDoScanRead;

    {$IFDEF PORTUGUES}
    {:
    Evento chamado para executar uma escrita por scan.
    @seealso(TScanWriteProc)
    }
    {$ELSE}
    {:
    Event called to execute a scan write command.
    @seealso(TScanWriteProc)
    }
    {$ENDIF}
    property OnDoScanWrite:TScanWriteProc read FDoScanWrite write FDoScanWrite;
  end;

implementation

uses hsstrings{$IFDEF FDEBUG}, LCLProc{$ENDIF};

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanThread
//                    implementation of TScanThread Class
////////////////////////////////////////////////////////////////////////////////

constructor TScanThread.Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
begin
  inherited Create(StartSuspended);
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  PScanUpdater := ScanUpdater;
  FInitEvent   := TCrossEvent.Create(nil,true,false,'ScanThreadInit'+IntToStr(UniqueID));
  FWaitToWrite := TCrossEvent.Create(nil,true,false,'WaitToWrite'+IntToStr(UniqueID));
  FEnd         := TCrossEvent.Create(nil,true,false,'');
  FMinScan := 0;
end;

destructor TScanThread.Destroy;
begin
  Terminate;
  FInitEvent.Destroy;
  FWaitToWrite.Destroy;
  FSpool.Destroy;
  inherited Destroy;
end;

procedure TScanThread.Execute;
var
  NeedSleep:Integer;
begin
  //sinaliza q a fila de mensagens esta criada
  //set as initialized the thread
  FInitEvent.SetEvent;
  while not Terminated do begin
    CheckScanWriteCmd;
    if Assigned(FDoScanRead) then
      try
        NeedSleep:=0;
        FDoScanRead(Self, NeedSleep);
        if NeedSleep>0 then
          Sleep(NeedSleep);
        if NeedSleep<0 then
          {$IFDEF FPC}
          ThreadSwitch;
          {$ELSE}
          SwitchToThread;
          {$ENDIF}
      except
        on E: Exception do begin
          {$IFDEF FDEBUG}
          DebugLn('TScanThread.Execute::' + e.Message);
          DumpStack;
          {$ENDIF}
          erro := E;
          Synchronize(SyncException);
        end;
      end;

    if FMinScan>0 then
      Sleep(FMinScan);
  end;
  FEnd.SetEvent;
end;

procedure TScanThread.CheckScanWriteCmd;
var
  PMsg:TMSMsg;
  pkg:PScanWriteRec;
begin
  if Assigned(FDoScanWrite) then begin
    FWaitToWrite.WaitFor(1);
    FWaitToWrite.ResetEvent;
    while (not Terminated) and FSpool.PeekMessage(PMsg,PSM_TAGSCANWRITE,PSM_TAGSCANWRITE,true) do begin
       pkg := PScanWriteRec(PMsg.wParam);

       pkg^.WriteResult := FDoScanWrite(pkg^.Tag,pkg^.ValuesToWrite);

       if PScanUpdater<>nil then
         PScanUpdater.ScanWriteCallBack(pkg);
    end;
  end;
end;

procedure TScanThread.SyncException;
begin
  try
    //Application.ShowException(erro);
  except
  end;
end;

procedure TScanThread.WaitInit;
begin
  while FInitEvent.WaitFor($FFFFFFFF)<>wrSignaled do ;
end;

procedure TScanThread.ScanWrite(SWPkg:PScanWriteRec);
begin
  if FInitEvent.WaitFor($FFFFFFFF)<>wrSignaled then
    raise Exception.Create(SthreadSuspended);

  //envia a mensagem
  //sends the message.
  FSpool.PostMessage(PSM_TAGSCANWRITE,SWPkg,nil,true);
  FWaitToWrite.SetEvent;
end;

procedure TScanThread.Terminate;
begin
  TCrossThread(self).Terminate;
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
end;

function  TScanThread.WaitEnd(timeout:Cardinal):TWaitResult;
begin
   Result := FEnd.WaitFor(timeout);
end;

end.
