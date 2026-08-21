{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Atualiza os valores dos tags.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
@abstract(Updates the tag values.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit protscanupdate;

{$IFDEF FPC}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, ProtocolTypes, MessageSpool, syncobjs, tag,
  crossthreads, fgl;

type

  TUserUpdateTimeProc = procedure (usertime: Double) of object;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Contexto de uma chamada de callback de tag pendente de execução na thread
  principal. Cada mensagem drenada da fila vira um destes, permitindo que
  vários sejam entregues em um único Synchronize.
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Context of one tag callback call pending execution on the main thread. Each
  message drained from the queue becomes one of these, allowing several to be
  delivered in a single Synchronize call.
  }
  {$ENDIF}
  TTagCallbackContext = class
  public
    ReqID:LongWord;
    Values:TArrayOfDouble;
    ValuesTimeStamp:QWord;
    Cmd:TTagCommand;
    LastResult:TProtocolIOResult;
    RealOffset:LongInt;
    CallBack:TTagCommandCallBack;
    procedure RunOnMainThread;
  end;

  TTagCallbackContextList = specialize TFPGObjectList<TTagCallbackContext>;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Classe de thread responsável por atualizar os valores dos tags. Usado por
  TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  Class of thread that updates the tag values. Used by TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  {$ENDIF}
  TScanUpdate = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FUserUpdateTimePRoc:TUserUpdateTimeProc;
    FOwnerProtocolDriver:TComponent;
    FSleepInterruptable:TCrossEvent;
    PGetValues:TGetValues;
    PScanTags:TGetMultipleValues;
    FSpool:TMessageSpool;
    PScannedValues:TArrayOfScanUpdateRec;
    FPendingCallbacks:TTagCallbackContextList;
    procedure SyncException;
    procedure UpdateMultipleTags;
    procedure CheckScanReadOrWrite;
    procedure FlushPendingCallbacks;
  protected
    //: @exclude
    procedure Loop; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean; OwnerProtocol:TComponent; usrUpdTime:TUserUpdateTimeProc);
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
    Faz a atualização de um tag que solicitou uma LEITURA por scan.

    @param(Tag TTagRec. Estrutura com as informações do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    {$ELSE}
    {:
    Updates the tag that requested a scan read.

    @param(Tag TTagRec. Structure with informations about the tag.)
    @raises(Exception if the thread was not initialized.)
    }
    {$ENDIF}
    procedure ScanRead(Tag:TTagRec);

    {$IFDEF PORTUGUES}
    {:
    Faz a atualização de um tag que solicitou uma ESCRITA por scan.

    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    {$ELSE}
    {:
    Updates the the tag that requested a scan write.

    @param(SWPkg PScanWriteRec. Points to a structure with informations about
           the scan write and Tag.)
    @raises(Exception if the thread was not initialized.)
    }
    {$ENDIF}
    procedure ScanRequestCallBack(SReqPkg:PScanReqRec; IsScanWrite:Boolean = true);
  published

    {$IFDEF PORTUGUES}
    //: Evento chamado pela thread para pegar e atualizar um tag.
    {$ELSE}
    //: Event called by thread to get values and update a tag.
    {$ENDIF}
    property OnGetValue:TGetValues read PGetValues write PGetValues;

    {$IFDEF PORTUGUES}
    //: função que retorna o tempo do proximo scan ou a lista de tags que estão na vez de serem lidos.
    {$ELSE}
    //: Returns the lists of tags to be updated or the next time to check has tags to be updated.
    {$ENDIF}
    property OnScanTags:TGetMultipleValues read PScanTags write PScanTags;
  end;

implementation

uses {$IFDEF FDEBUG}LCLProc,{$ENDIF} ProtocolDriver, hsstrings, crossdatetime,
  dateutils;

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanUpdate
//                    implementation of TScanUpdate class
////////////////////////////////////////////////////////////////////////////////
constructor TScanUpdate.Create(StartSuspended:Boolean; OwnerProtocol:TComponent; usrUpdTime:TUserUpdateTimeProc);
begin
  inherited Create(StartSuspended);
  if not (OwnerProtocol is TProtocolDriver) then
    raise Exception.Create(STheOwnerMustBeAProtocolDriver);

  FOwnerProtocolDriver:=OwnerProtocol;
  Priority := tpHighest;
  FUserUpdateTimePRoc:=usrUpdTime;
  FSpool := TMessageSpool.Create;
  FSleepInterruptable := TCrossEvent.Create(false, false);
  FPendingCallbacks := TTagCallbackContextList.Create(true);
end;

destructor TScanUpdate.Destroy;
begin
  inherited Destroy;

  FSleepInterruptable.SetEvent;
  FreeAndNil(FSleepInterruptable);
  FreeAndNil(FSpool);
  FreeAndNil(FPendingCallbacks);
end;

procedure TScanUpdate.Terminate;
begin
  inherited Terminate;
  FSleepInterruptable.SetEvent;
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
end;

procedure TScanUpdate.Loop;
var
  i, FValor, timeout:LongInt;
  FInicio:QWord;
begin
  try
    CheckScanReadOrWrite;
    if Assigned(PScanTags) then begin
      SetLength(PScannedValues,0);
      timeout:=PScanTags(PScannedValues);
      if Length(PScannedValues)>0 then begin
        FInicio:=GetTickCount64;
        Synchronize(@UpdateMultipleTags);
        FValor:=(GetTickCount64 - FInicio);

        for i:=0 to High(PScannedValues) do
          SetLength(PScannedValues[i].Values, 0);

        SetLength(PScannedValues, 0);

        timeout:=timeout-FValor;
      end;
    end else
      timeout:=1;

    if not FSleepInterruptable.ResetEvent then FSleepInterruptable.ResetEvent;
    if (timeout)>0 then begin
      FSleepInterruptable.WaitFor(timeout)
    end else
      FSleepInterruptable.WaitFor(1);
  except
  //  on E: Exception do begin
  //    {$IFDEF FDEBUG}
  //    DebugLn('TScanUpdate.Execute:: ' + e.Message);
  //    DumpStack;
  //    {$ENDIF}
  //    Ferro := E;
  //    Synchronize(@SyncException);
  //  end;
  end;

end;

procedure TScanUpdate.ScanRead(Tag:TTagRec);
var
  tagpkg:PTagRec;
begin
  New(tagpkg);
  Move(tag,tagpkg^,sizeof(TTagRec));
  FSpool.PostMessage(PSM_TAGSCANREAD,tagpkg,nil,false);
  FSleepInterruptable.SetEvent;
end;

procedure TScanUpdate.ScanRequestCallBack(SReqPkg: PScanReqRec;
  IsScanWrite: Boolean);
begin
  if IsScanWrite then
    FSpool.PostMessage(PSM_TAGSCANWRITE,SReqPkg,nil,true)
  else
    FSpool.PostMessage(PSM_SINGLESCANREAD,SReqPkg,nil,true);

  FSleepInterruptable.SetEvent;
end;

procedure TScanUpdate.SyncException;
begin
  //try
  //  Application.ShowException(Ferro);
  //except
  //end;
end;

procedure TScanUpdate.UpdateMultipleTags;
var
  c:LongInt;
  found:Boolean;
begin
  for c:=0 to High(PScannedValues) do begin
    found:=false;
    if TProtocolDriver(FOwnerProtocolDriver).IsMyTag(TTag(TMethod(PScannedValues[c].CallBack).Data)) and ((TTag(TMethod(PScannedValues[c].CallBack).Data).ComponentState*[csDestroying])=[]) then begin
      found:=true;
    end;
    if not found then continue;
    with PScannedValues[c] do
      try
        CallBack(0, Values, GetTickCount64, tcScanRead, LastResult, 0);
      finally
      end;
  end;
end;

procedure TScanUpdate.CheckScanReadOrWrite;
var
  x:PScanReqRec;
  TagRec:PTagRec;
  PMsg:TMSMsg;
  Values:TScanReadRec;
  Cmd:TTagCommand;
  Ctx:TTagCallbackContext;
begin
  while (not Terminated) and FSpool.PeekMessage(PMsg,PSM_TAGSCANREAD,PSM_SINGLESCANREAD,true) do begin
    //try
      case PMsg.MsgID of
        PSM_TAGSCANWRITE, PSM_SINGLESCANREAD: begin
          x := PScanReqRec(PMsg.wParam);

          Values.Values          := x^.Values;
          Values.Offset          := x^.Tag.OffSet;
          Values.RealOffset      := x^.Tag.RealOffset;
          Values.ClkMonotonicTStamp := x^.ClkMonotonicTStamp;
          Values.LastQueryResult := x^.RequestResult;
          if PMsg.MsgID=PSM_TAGSCANWRITE then
            Cmd:=tcScanWrite
          else
            Cmd:=tcSingleScanRead;

          //monta o contexto e enfileira para entrega em lote na thread principal.
          //builds the context and queues it for batched delivery on the main thread.
          Ctx:=TTagCallbackContext.Create;
          Ctx.ReqID           := x^.Tag.ID;
          Ctx.Values          := Values.Values;
          Ctx.ValuesTimeStamp := Values.ClkMonotonicTStamp;
          Ctx.Cmd             := Cmd;
          Ctx.LastResult      := Values.LastQueryResult;
          Ctx.RealOffset      := Values.RealOffset;
          Ctx.CallBack        := x^.Tag.CallBack;
          FPendingCallbacks.Add(Ctx);

          //libera a memoria ocupada
          //pelo pacote
          //free the memory of the request
          SetLength(x^.Values,0);
          Dispose(x);
        end;
        PSM_TAGSCANREAD: begin
          TagRec := PTagRec(PMsg.wParam);
          Values.Offset:=TagRec^.OffSet;
          Values.RealOffset:=TagRec^.RealOffset;

          if Assigned(PGetValues) then begin
            PGetValues(TagRec^, Values)
          end else
            Values.LastQueryResult := ioDriverError;

          Cmd:=tcScanRead;

          Ctx:=TTagCallbackContext.Create;
          Ctx.ReqID           := TagRec^.ID;
          Ctx.Values          := Values.Values;
          Ctx.ValuesTimeStamp := Values.ClkMonotonicTStamp;
          Ctx.Cmd             := Cmd;
          Ctx.LastResult      := Values.LastQueryResult;
          Ctx.RealOffset      := Values.RealOffset;
          Ctx.CallBack        := TagRec^.CallBack;
          FPendingCallbacks.Add(Ctx);

          //libera a memoria ocupada pelos pacotes
          //free the memory of the request
          SetLength(Values.Values, 0);
          Dispose(TagRec);
        end;
      end;
    //except
    //  on E: Exception do begin
    //    {$IFDEF FDEBUG}
    //    DebugLn('TScanUpdate.Execute:: ' + e.Message);
    //    DumpStack;
    //    {$ENDIF}
    //    Ferro := E;
    //    Synchronize(@SyncException);
    //  end;
    //end;
  end;

  //entrega todos os callbacks pendentes em uma unica ida a thread principal,
  //em vez de uma chamada Synchronize bloqueante por mensagem.
  //delivers all pending callbacks in a single trip to the main thread,
  //instead of one blocking Synchronize call per message.
  if FPendingCallbacks.Count>0 then
    Synchronize(@FlushPendingCallbacks);
end;

procedure TScanUpdate.FlushPendingCallbacks;
var
  i:LongInt;
begin
  try
    if Terminated then exit;
    for i:=0 to FPendingCallbacks.Count-1 do
      FPendingCallbacks[i].RunOnMainThread;
  finally
    FPendingCallbacks.Clear;
  end;
end;

procedure TTagCallbackContext.RunOnMainThread;
begin
  if Assigned(CallBack) then
    CallBack(ReqID, Values, ValuesTimeStamp, Cmd, LastResult, RealOffset);
end;

end.

