{:
@abstract(Atualiza os valores dos tags.)
@author(Fabio Luis Girardi papelhigienico@gmail.com)
}
unit protscanupdate;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, ProtocolTypes, MessageSpool, syncobjs, tag;

type

  {:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  Classe de thread responsável por atualizar os tags após o driver processar
  leituras/escritas por scan. Usado por TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  TScanUpdate = class(TCrossThread)
  private
    FSleepInterruptable:TCrossEvent;
    FEnd:TCrossEvent;
    Ferro:Exception;
    TagCBack:TTagCommandCallBack;
    FTagRec:PTagRec;
    Fvalues:TScanReadRec;
    FCmd:TTagCommand;
    PGetValues:TGetValues;
    PScanTags:TGetMultipleValues;
    FSpool:TMessageSpool;
    PScannedValues:TArrayOfScanUpdateRec;
    procedure SyncCallBack;
    procedure SyncException;
    procedure UpdateMultipleTags;
    procedure CheckScanReadOrWrite;
    function  WaitEnd(timeout:Cardinal):TWaitResult;
  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean);
    //: @exclude
    destructor Destroy; override;
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
    //: função que retorna o tempo do proximo scan ou a lista de tags que estão na vez de serem lidos.
    property OnScanTags:TGetMultipleValues read PScanTags write PScanTags;
  end;

implementation

uses {$IFDEF FDEBUG}LCLProc,{$ENDIF} hsstrings, Forms;

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanUpdate
////////////////////////////////////////////////////////////////////////////////
constructor TScanUpdate.Create(StartSuspended:Boolean);
begin
  inherited Create(StartSuspended);
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  FEnd := TCrossEvent.Create(nil, true, false, 'EndOfThread'+IntToStr(UniqueID));
  FEnd.ResetEvent;
  FSleepInterruptable := TCrossEvent.Create(nil,false,false,'ScanUpdateThreadSleepInterruptable'+IntToStr(UniqueID));
end;

destructor TScanUpdate.Destroy;
begin
  inherited Destroy;
  FSleepInterruptable.SetEvent;
  FSleepInterruptable.Destroy;
  FSpool.Destroy;
  FEnd.Destroy;
end;

procedure TScanUpdate.Terminate;
begin
  TCrossThread(self).Terminate;
  FSleepInterruptable.SetEvent;
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
  timeout:Integer;
  sleepres:TWaitResult;
begin
  while not Terminated do begin
    try
      CheckScanReadOrWrite;
      if Assigned(PScanTags) then begin
        SetLength(PScannedValues,0);
        timeout:=PScanTags(PScannedValues);
        if Length(PScannedValues)>0 then
          Synchronize(UpdateMultipleTags);
      end else
        timeout:=1;

      if (timeout-1)>0 then
        FSleepInterruptable.WaitFor(timeout-1)
      else
        FSleepInterruptable.WaitFor(1);
    except
      on E: Exception do begin
        {$IFDEF FDEBUG}
        DebugLn('TScanUpdate.Execute:: ' + e.Message);
        DumpStack;
        {$ENDIF}
        Ferro := E;
        Synchronize(SyncException);
      end;
    end;
  end;
  FEnd.SetEvent;
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

procedure TScanUpdate.ScanWriteCallBack(SWPkg:PScanWriteRec);
begin
   FSpool.PostMessage(PSM_TAGSCANWRITE,SWPkg,nil,true);
   FSleepInterruptable.SetEvent;
end;

procedure TScanUpdate.SyncException;
begin
  try
    Application.ShowException(Ferro);
  except
  end;
end;

procedure TScanUpdate.UpdateMultipleTags;
var
  c:Integer;
begin
  for c:=0 to High(PScannedValues) do
    with PScannedValues[c] do
      try
        CallBack(Values, ValueTimeStamp, tcScanRead, LastResult, 0);
        SetLength(Values,0);
      finally
      end;
  SetLength(PScannedValues,0);
end;

procedure TScanUpdate.CheckScanReadOrWrite;
var
  x:PScanWriteRec;
  PMsg:TMSMsg;
begin
  while (not Terminated) and FSpool.PeekMessage(PMsg,PSM_TAGSCANREAD,PSM_TAGSCANWRITE,true) do begin
    try
      case PMsg.MsgID of
        PSM_TAGSCANWRITE: begin
          x := PScanWriteRec(PMsg.wParam);

          TagCBack                := x^.Tag.CallBack;
          Fvalues.Values          := x^.ValuesToWrite;
          Fvalues.Offset          := x^.Tag.OffSet;
          Fvalues.RealOffset      := x^.Tag.RealOffset;
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
        PSM_TAGSCANREAD: begin
          FTagRec := PTagRec(PMsg.wParam);
          TagCBack:=FTagRec^.CallBack;
          Fvalues.Offset:=FTagRec^.OffSet;
          Fvalues.RealOffset:=FTagRec^.RealOffset;

          if Assigned(PGetValues) then begin
            PGetValues(FTagRec^, Fvalues)
          end else
            Fvalues.LastQueryResult := ioDriverError;

          FCmd:=tcScanRead;

          Synchronize(SyncCallBack);

          //libera a memoria ocupada pelos pacotes
          SetLength(Fvalues.Values, 0);
          Dispose(FTagRec);
          TagCBack:=nil;
        end;
      end;
    except
      on E: Exception do begin
        {$IFDEF FDEBUG}
        DebugLn('TScanUpdate.Execute:: ' + e.Message);
        DumpStack;
        {$ENDIF}
        Ferro := E;
        Synchronize(SyncException);
      end;
    end;
  end;
end;

procedure TScanUpdate.SyncCallBack;
begin
  if Terminated then exit;
  try
    if Assigned(TagCBack) then
      TagCBack(Fvalues.Values,Fvalues.ValuesTimestamp,FCmd,Fvalues.LastQueryResult, Fvalues.RealOffset);
  except
    on erro:Exception do begin
      Ferro:=erro;
      SyncException;
    end;
  end;
end;

end.
