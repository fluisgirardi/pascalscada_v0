unit protscan;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, protscanupdate, MessageSpool, syncobjs,
  ProtocolTypes{$IFNDEF FPC}, Windows{$ENDIF};

type

  {:
  Classe de thread reponsável processar as escritas por scan e por manter os
  tags com seus valores atualizados o mais rápido possível. Usado por
  TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  TScanThread = class(TCrossThread)
  private
    FInitEvent:TCrossEvent;
    FWaitToWrite:TCrossEvent;

    FDoScanRead:TScanReadProc;
    FDoScanWrite:TScanWriteProc;

    FMinScan:Cardinal;
    erro:Exception;
    FSpool:TMessageSpool;
    PScanUpdater:TScanUpdate;

    procedure SyncException;

  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
    //: @exclude
    destructor Destroy; override;
    //:Ordena a thread verificar se há comandos de escrita pendentes.
    procedure CheckScanWriteCmd;
    //: Ao chamar @name, espera a thread sinalizar a sua inicialização.
    procedure WaitInit;
    {:
    Solicita uma escrita de valores por scan para a thread do driver de protocolo.

    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanWrite(SWPkg:PScanWriteRec);
    {:
    Atualiza as informações do driver a respeito dos tags dependentes. Chamado
    quando alguma propriedade de um tag sofre alguma mudança.
    @param(Tag TTag. Tag quem sofreu a mudança.)
    @param(Change TChangeType. Que propriedade sofreu a alteração.)
    @param(oldValue Cardinal. Valor antigo da propriedade.)
    @param(newValue Cardinal. Novo valor da propriedade.)
    @seealso(TProtocolDriver.TagChanges)
    }
  published
    {:
    Diz quantos milisegundos o driver esperar caso não seja feita nenhuma
    operação de scan, a fim de evitar alto consumo de processador inutilmente.
    }
    property MinTimeOfScan:Cardinal read FMinScan write FMinScan nodefault;
    //: Evento chamado para realizar a atualização do valores dos tags.
    property OnDoScanRead:TScanReadProc read FDoScanRead write FDoScanRead;
    {:
    Evento chamado para executar uma escrita por scan.
    @seealso(TScanWriteProc)
    }
    property OnDoScanWrite:TScanWriteProc read FDoScanWrite write FDoScanWrite;
  end;

implementation

uses Forms;

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanThread
////////////////////////////////////////////////////////////////////////////////

constructor TScanThread.Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
begin
  inherited Create(StartSuspended);
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  PScanUpdater := ScanUpdater;
  FInitEvent   := TCrossEvent.Create(nil,true,false,'ScanThreadInit'+IntToStr(UniqueID));
  FWaitToWrite := TCrossEvent.Create(nil,true,false,'WaitToWrite'+IntToStr(UniqueID));
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
          erro := E;
          Synchronize(SyncException);
        end;
      end;

    if FMinScan>0 then
      Sleep(FMinScan);
  end;
end;

procedure TScanThread.CheckScanWriteCmd;
var
  PMsg:TMSMsg;
  pkg:PScanWriteRec;
begin
  if Assigned(FDoScanWrite) then begin
    FWaitToWrite.WaitFor(1);
    FWaitToWrite.ResetEvent;
    while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANWRITE,WM_TAGSCANWRITE,true) do begin
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
    Application.ShowException(erro);
  except
  end;
end;

procedure TScanThread.WaitInit;
begin
  while FInitEvent.WaitFor($FFFFFFFF)<>wrSignaled do ;
end;

procedure TScanThread.ScanWrite(SWPkg:PScanWriteRec);
begin
  if FInitEvent.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  //envia a mensagem
  FSpool.PostMessage(WM_TAGSCANWRITE,SWPkg,nil,true);
  FWaitToWrite.SetEvent;
end;

end.

