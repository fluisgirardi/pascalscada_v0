unit protscanupdate;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, ProtocolTypes, MessageSpool, syncobjs;

type

  {:
  Classe de thread reponsável por atualizar os tags após o driver processar
  leituras/escritas por scan. Usado por TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  TScanUpdate = class(TCrossThread)
  private
    FInitEvent:TCrossEvent;
    FDoSomethingEvent:TCrossEvent;
    FEnd:TCrossEvent;
    Ferro:Exception;
    TagCBack:TTagCommandCallBack;
    FTagRec:PTagRec;
    Fvalues:TScanReadRec;
    FCmd:TTagCommand;
    PGetValues:TGetValues;
    FSpool:TMessageSpool;
    procedure SyncCallBack;
    procedure SyncException;
    procedure DoSomething;
    procedure WaitToDoSomething;
    procedure CheckScanWrite;
    function  WaitEnd(timeout:Cardinal):TWaitResult;
  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean);
    //: @exclude
    destructor Destroy; override;
    //: Ao chamar @name, espera a thread sinalizar a sua inicialização.
    procedure WaitInit;
    //: Sinaliza para thread Terminar.
    procedure Terminate;
    {:
    Faz a atualização de um tag que solicitou uma LEITURA por scan.

    Retorna principalmente valores e resultados das leituras do driver.
    @param(Tag TTagRec. Estrutura com as informações do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanRead(Tag:TTagRec);
    {:
    Faz a atualização de um tag que solicitou uma ESCRITA por scan.

    Retorna principalmente valores e resultados da escrita do driver.
    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanWriteCallBack(SWPkg:PScanWriteRec);
  published
    //: Evento chamado pela thread para executar uma leitura por scan.
    property OnGetValue:TGetValues read PGetValues write PGetValues;
  end;

implementation

uses Forms;

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanUpdate
////////////////////////////////////////////////////////////////////////////////
constructor TScanUpdate.Create(StartSuspended:Boolean);
begin
  inherited Create(StartSuspended);
  FEnd := TCrossEvent.Create(nil, true, false, 'EndOfThread'+IntToStr(UniqueID));
  FEnd.ResetEvent;
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  FInitEvent := TCrossEvent.Create(nil,true,false,'ScanUpdateThreadInit'+IntToStr(UniqueID));
  FDoSomethingEvent := TCrossEvent.Create(nil,true,false,'ScanUpdateThreadDoSomething'+IntToStr(UniqueID));
end;

destructor TScanUpdate.Destroy;
begin
  inherited Destroy;
  FDoSomethingEvent.Destroy;
  FInitEvent.Destroy;
  FSpool.Destroy;
  FEnd.Destroy;
end;

procedure TScanUpdate.Terminate;
begin
  TCrossThread(self).Terminate;
  DoSomething;
  repeat
     Application.ProcessMessages;
  until WaitEnd(1)=wrSignaled;
end;

function  TScanUpdate.WaitEnd(timeout:Cardinal):TWaitResult;
begin
   Result := FEnd.WaitFor(timeout);
end;

procedure TScanUpdate.Execute;
var
  PMsg:TMSMsg;
begin
   //sinaliza q a fila de mensagens esta criada
   FInitEvent.SetEvent;
   while not Terminated do begin
      WaitToDoSomething;
      while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANREAD,WM_TAGSCANREAD,true) do begin
         try
            CheckScanWrite;

            FTagRec := PTagRec(PMsg.wParam);
            TagCBack:=FTagRec^.CallBack;
            Fvalues.Offset:=FTagRec^.OffSet;

            if Assigned(PGetValues) then begin
               if not Terminated then
                  PGetValues(FTagRec^, Fvalues)
            end else
               Fvalues.LastQueryResult := ioDriverError;

            FCmd:=tcScanRead;

            Synchronize(SyncCallBack);

            //libera a memoria ocupada pelos pacotes
            SetLength(Fvalues.Values, 0);
            Dispose(FTagRec);
            TagCBack:=nil;
         except
            on E: Exception do begin
               Ferro := E;
               Synchronize(SyncException);
            end;
         end;
      end;
   end;
   FEnd.SetEvent;
end;

procedure TScanUpdate.WaitInit;
begin
  while FInitEvent.WaitFor($FFFFFFFF)<>wrSignaled do ;
end;

procedure TScanUpdate.ScanRead(Tag:TTagRec);
var
  tagpkg:PTagRec;
begin
  if FInitEvent.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  New(tagpkg);
  Move(tag,tagpkg^,sizeof(TTagRec));
  FSpool.PostMessage(WM_TAGSCANREAD,tagpkg,nil,false);
  DoSomething;
end;

procedure TScanUpdate.ScanWriteCallBack(SWPkg:PScanWriteRec);
begin
  if FInitEvent.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

   FSpool.PostMessage(WM_TAGSCANWRITE,SWPkg,nil,true);
   DoSomething;
end;

procedure TScanUpdate.SyncException;
begin
  try
    Application.ShowException(Ferro);
  except
  end;
end;

procedure TScanUpdate.DoSomething;
begin
  FDoSomethingEvent.SetEvent;
end;

procedure TScanUpdate.WaitToDoSomething;
begin
  FDoSomethingEvent.ResetEvent;
  FDoSomethingEvent.WaitFor(1);
end;

procedure TScanUpdate.CheckScanWrite;
var
  x:PScanWriteRec;
  PMsg:TMSMsg;
begin
   while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANWRITE,WM_TAGSCANWRITE,true) do begin
      //recupera o pacote
      x := PScanWriteRec(PMsg.wParam);

      TagCBack                := x^.Tag.CallBack;
      Fvalues.Values          := x^.ValuesToWrite;
      Fvalues.Offset          := x^.Tag.OffSet;
      Fvalues.ValuesTimestamp := x^.ValueTimeStamp;
      Fvalues.LastQueryResult := x^.WriteResult;
      FCmd:=tcScanWrite;

      //sincroniza com o tag.
      Synchronize(SyncCallBack);

      //libera a memoria ocupada
      //pelo pacote
      SetLength(x^.ValuesToWrite,0);
      Dispose(x);
      TagCBack:=nil;
   end;
end;

procedure TScanUpdate.SyncCallBack;
begin
  if Terminated then exit;
  try
    if Assigned(TagCBack) then
      TagCBack(Fvalues.Values,Fvalues.ValuesTimestamp,FCmd,Fvalues.LastQueryResult, Fvalues.Offset);
  except
    on erro:Exception do begin
      Ferro:=erro;
      SyncException;
    end;
  end;
end;

end.

