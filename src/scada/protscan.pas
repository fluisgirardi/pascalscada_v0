{$i ../common/language.inc}
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
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, protscanupdate, MessageSpool, syncobjs,
  crossthreads, ProtocolTypes{$IFNDEF FPC}, Windows{$ENDIF};

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
  TScanThread = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FDoSingleScanRead: TSingleScanReadProc;
    FWaitToWrite:TCrossEvent;

    FDoScanRead:TScanReadProc;
    FDoScanWrite:TScanWriteProc;

    FMinScan:Cardinal;
    FSpool:TMessageSpool;
    PScanUpdater:TScanUpdate;

    procedure SyncException;
  protected
    {$IFDEF PORTUGUES}
    //:Ordena a thread verificar se há comandos de escrita pendentes.
    {$ELSE}
    //: Verifies if the thread has write requests on queue.
    {$ENDIF}
    procedure CheckScanWriteCmd;

    //: @exclude
    procedure Loop; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
    //: @exclude
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: Sinaliza para thread Terminar.
    {$ELSE}
    //: Requests the thread finalization.
    {$ENDIF}
    procedure Terminate; override;

    {$IFDEF PORTUGUES}
    {:
    Solicita uma escrita de valores por scan para a thread do driver de protocolo.

    @param(SWPkg PScanReadRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    {$ELSE}
    {:
    Put a values write request to be processed by the scan (queue) of protocol.

    @param(SWPkg PScanReadRec. Points to a structure with informations about
           the write command.)
    @raises(Exception if the thread didn't responds.)
    }
    {$ENDIF}
    procedure ScanWrite(SWPkg:PScanReqRec);

    {$IFDEF PORTUGUES}
    {:
    Solicita uma leitura de valores por scan para a thread do driver de protocolo.

    @param(SWPkg PScanReadRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    {$ELSE}
    {:
    Put a read request to be processed by the scan (queue) of protocol.

    @param(SWPkg PScanReadRec. Points to a structure with informations about
           the write command.)
    @raises(Exception if the thread didn't responds.)
    }
    {$ENDIF}
    procedure SingleScanRead(SRPkg:PScanReqRec);
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

    {$IFDEF PORTUGUES}
    //: Evento chamado para realizar a leitura de valoes de um tag.
    {$ELSE}
    //: Event called to execute a single scan read.
    {$ENDIF}
    property OnDoSingleScanRead:TSingleScanReadProc read FDoSingleScanRead write FDoSingleScanRead;

  end;

implementation

uses hsstrings, pascalScadaMTPCPU{$IFDEF FDEBUG}, LCLProc{$ENDIF};

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
  FWaitToWrite := TCrossEvent.Create(true, false);
  FMinScan := 0;
end;

destructor TScanThread.Destroy;
begin
  Terminate;
  FreeAndNil(FWaitToWrite);
  FreeAndNil(FSpool);
  inherited Destroy;
end;

procedure TScanThread.Loop;
var
  NeedSleep:LongInt;
begin
  CheckScanWriteCmd;
  if Assigned(FDoScanRead) then begin
    try
      NeedSleep:=0;
      FDoScanRead(Self, NeedSleep);
      if NeedSleep>0 then
        Sleep(NeedSleep);
      if NeedSleep<0 then
        CrossThreadSwitch;
    except
      //on E: Exception do begin
      //  {$IFDEF FDEBUG}
      //  DebugLn('TScanThread.Execute::' + e.Message);
      //  DumpStack;
      //  {$ENDIF}
      //  erro := E;
      //  Synchronize(@SyncException);
      //end;
    end;
  end;

  if FMinScan>0 then
    Sleep(FMinScan);
end;

procedure TScanThread.CheckScanWriteCmd;
var
  PMsg:TMSMsg;
  pkg:PScanReqRec;
begin
  while (not Terminated) and FSpool.PeekMessage(PMsg,PSM_TAGSCANWRITE,PSM_SINGLESCANREAD,true) do begin
    case PMsg.MsgID of
      PSM_SINGLESCANREAD: begin
        if Assigned(FDoSingleScanRead) then begin
          FWaitToWrite.WaitFor(1); //TODO -oFabio: Why we should wait 1ms?
          FWaitToWrite.ResetEvent;

          pkg := PScanReqRec(PMsg.wParam);

          pkg^.RequestResult := FDoSingleScanRead(pkg^.Tag, pkg^.Values);

          if PScanUpdater<>nil then
            PScanUpdater.ScanRequestCallBack(pkg, false);
        end else                            
          Dispose(PScanReqRec(PMsg.wParam));
      end;

      PSM_TAGSCANWRITE: begin
        if Assigned(FDoScanWrite) then begin
          FWaitToWrite.WaitFor(1); //TODO -oFabio: Why we should wait 1ms?
          FWaitToWrite.ResetEvent;

          pkg := PScanReqRec(PMsg.wParam);

          pkg^.RequestResult := FDoScanWrite(pkg^.Tag,pkg^.Values);

          if PScanUpdater<>nil then
            PScanUpdater.ScanRequestCallBack(pkg);
        end else
          Dispose(PScanReqRec(PMsg.wParam));
      end;
    end;
  end;
end;

procedure TScanThread.SyncException;
begin
  //try
  //  Application.ShowException(erro);
  //except
  //end;
end;

procedure TScanThread.ScanWrite(SWPkg: PScanReqRec);
begin
  WaitLoopStarts;

  //envia a mensagem
  //sends the message.
  FSpool.PostMessage(PSM_TAGSCANWRITE,SWPkg,nil,true);
  FWaitToWrite.SetEvent;
end;

procedure TScanThread.SingleScanRead(SRPkg: PScanReqRec);
begin
  WaitLoopStarts;

  //envia a mensagem
  //sends the message.
  FSpool.PostMessage(PSM_SINGLESCANREAD, SRPkg, nil,true);
  FWaitToWrite.SetEvent;
end;

procedure TScanThread.Terminate;
begin
  inherited Terminate;
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
end;

end.

