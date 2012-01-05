{$i language.inc}
{$i delphiver.inc}
{$IFDEF PORTUGUES}
{:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    @abstract(Unit de sincronização de threads por eventos.)

    Esta unit foi criada por motivos de compatibilidade
    (e performance) entre ambientes windows, linux e
    demais unix.

    Parte do codigo foi adaptado da unit rtl/unix/cthreads.pp do
    projeto Freepascal.
}
{$ELSE}
{:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    @abstract(Unit of thread synchronization using events.)

    This unit was created by compatibility and performance reasons between
    windows, linux and other unix environments.

    Pieces of code was adapted from the unit rtl/unix/cthreads.pp of FreePascal
    project.
}
{$ENDIF}
unit CrossEvent;

{$IFDEF FPC}
  {$mode delphi}

  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20404)}
  {$DEFINE USE_TTHREAD_START}
  {$IFEND}
{$ELSE}
  {$IFDEF DELPHI2010_UP}
    {$DEFINE USE_TTHREAD_START}
  {$ENDIF}
{$ENDIF}

{$if (not defined(WINDOWS)) and (not defined(WIN32)) and (not defined(WIN64))}
{$DEFINE NeedCrossEvents}
{$IFEND}

interface

uses
  SyncObjs,
  Classes,
  SysUtils
  {$if defined(NeedCrossEvents)}
  ,BaseUnix
  ,unix
  ,pthreads
  {$ELSE}
  ,windows
  {$IFEND}
  ;

type

{$if defined(NeedCrossEvents)}

  {$IFDEF PORTUGUES}
  {:
  Estrutura usada por TCrossEvent para trabalhar em ambientes Unix.
  @member condvar Armazena um objeto pthread_cond.
  @member mutex Armazena um objeto pthread_mutex.
  @member isset Armazena se o evento foi setado.
  @member IsDestroing Armazena se o evento está sendo destuido.
  @seealso(TCrossEvent)
  }
  {$ELSE}
  {:
  Record used by TCrossEvent to work in Unix environments.
  @member condvar Stores a pthread_cond object.
  @member mutex Stores a pthread_mutex object.
  @member isset Stores the actual state of the event (set or not).
  @member IsDestroing Stores if the event has been destroyed.
  @seealso(TCrossEvent)
  }
  {$ENDIF}
  TINTRTLEvent = record
    EventAttr:Pointer;
    Name:String;
    condvar: pthread_cond_t;
    mutex: pthread_mutex_t;
    isset: boolean;
    IsDestroing:boolean;
  end;
{$IFEND}

  {$IFDEF PORTUGUES}
  //: Classe base para threads.
  {$ELSE}
  //: Thread base class.
  {$ENDIF}
  TCrossThread = class(TThread)
   private
     function GetUniqueID:Int64;
   public
     {$IFDEF PORTUGUES}
     //: Acorda/inicia uma thread suspensa.
     {$ELSE}
     //: Wake/starts a suspended thread.
     {$ENDIF}
     procedure WakeUp;

     {$IFDEF PORTUGUES}
     //: Fornece o número único da thread.
     {$ELSE}
     //: Provides a unique number of identification.
     {$ENDIF}
     property UniqueID:Int64 read GetUniqueID;
  end;

  {$IFDEF PORTUGUES}
  {:
  Classe de sincronização de threads por eventos Multi-plataforma.

  Esta classe foi criada por motivos de compatibilidade (e performance)
  entre ambientes Windows e Unix.
  Emula eventos para qualquer sistema operacional. A parte Unix (Linux/FreeBSD)
  é baseado nos eventos RTL do FreePascal.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Class of thread synchronization by events multi-platform.

  This class was created by compatibility and performance reasons between
  Windows and Unix environments.
  It emulates events to any OSes. The Unix code (Linux/FreeBSD) is based on
  events of the RTL of FreePascal project.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TCrossEvent = class(TObject)
   private
      FManualReset: Boolean;
      {$if defined(NeedCrossEvents)}
      FEvent:TINTRTLEvent;
      Waiters:Integer;
      {$ELSE}
      FEvent:THandle;
      {$IFEND}
   public
      {$IFDEF PORTUGUES}
      {:
      @name cria um novo evento. Seus parametros são iguais ao da classe TEvent
      disponível no FPC e no Delphi.
      @param(EventAttributes PSecurityAttributes. Atributos de segurança.
             Somente Windows, demais sistemas deixar @code(nil).)
      @param(AManualReset Boolean. Caso @false o evento irá se resetar
             automaticamente assim que a primeira thread sair da espera. Caso
             @true é necessário resetar o evento através do método ResetEvent.)
      @param(InitialState Boolean. Diz se o evento será criado sinalizado ou não.
             Caso @true, o evento é criado sinalizado.)
      @param(Name String. Nome do evento. Somente Windows.)
      @seealso(SetEvent)
      @seealso(ResetEvent)
      @seealso(WaitFor)
      }
      {$ELSE}
      {:
      @name creates a new event object. Their parameters are the same of the
      class TEvent of FPC and Delphi.
      @param(EventAttributes PSecurityAttributes. Security attributes.
             Only Windows, other OSes must be suplied @code(nil).)
      @param(AManualReset Boolean. If @false, the event will reset automatically
             after the first thread wake up of the wait. If @true is needed call
             ResetEvent to reset the event object.)
      @param(InitialState Boolean. Initial state of the event object. If true,
             the event object will be created signaled.)
      @param(Name String. Event object name. Only Windows.)
      @seealso(SetEvent)
      @seealso(ResetEvent)
      @seealso(WaitFor)
      }
      {$ENDIF}
      constructor Create(EventAttributes : PSecurityAttributes; AManualReset,InitialState : Boolean;const Name : string);

      {$IFDEF PORTUGUES}
      //: Destroi o objeto de eventos.
      {$ELSE}
      //: Destroys the event object.
      {$ENDIF}
      destructor  Destroy; override;

      {$IFDEF PORTUGUES}
      {:
      Reseta o evento. Sinaliza-o para @false.
      @seealso(SetEvent)
      @seealso(WaitFor)
      }
      {$ELSE}
      {:
      Reset the event object. Set it to @false.
      @seealso(SetEvent)
      @seealso(WaitFor)
      }
      {$ENDIF}
      function ResetEvent:Boolean;

      {$IFDEF PORTUGUES}
      {:
      Seta o evento. Sinaliza-o para @true.
      @seealso(ResetEvent)
      @seealso(WaitFor)
      }
      {$ELSE}
      {:
      Set the event object. Set it to @true.
      @seealso(ResetEvent)
      @seealso(WaitFor)
      }
      {$ENDIF}
      function SetEvent:Boolean;

      {$IFDEF PORTUGUES}
      {:
      Espera um evento acontecer por um determinado tempo.
      @param(Timeout Cardinal. Tempo em milisegundos até o evento acontecer. Caso
             seja fornecido $FFFFFFFF, espera infinitamente ou até ser destruido.)
      @returns(@bold(wrSignaled) caso o evento tenha sido sinalizado no tempo hábil.
               @bold(wrTimeout) caso o evento não tenha sido sinalizado no tempo habil.
               @bold(wrAbandoned) caso o evento esteja sendo destruído.
               @bold(wrError) caso tenha acontecido algum erro ao tentar esperar pelo evento.)

      @seealso(SetEvent)
      @seealso(ResetEvent)
      }
      {$ELSE}
      {:
      Wait the event object to be signaled (set).
      @param(Timeout Cardinal. Timeout in milliseconds to wait the event be
             signaled. If is suplied $FFFFFFFF, waits infinitely or until be destroyed.)
      @returns(@bold(wrSignaled) if the event was signaled before the timeout.
               @bold(wrTimeout) if the event was not signaled before the timeout.
               @bold(wrAbandoned) if the event object was destroyed.
               @bold(wrError) if some error occurs trying to wait the event object.)

      @seealso(SetEvent)
      @seealso(ResetEvent)
      }
      {$ENDIF}
      function WaitFor(Timeout : Cardinal) : TWaitResult;
  end;

implementation

procedure TCrossThread.WakeUp;
begin
  {$IFDEF USE_TTHREAD_START}
    Start;
  {$ELSE}
    Resume;
  {$ENDIF}
end;

function TCrossThread.GetUniqueID:Int64;
begin
  {$IFDEF UNIX}
  Result := Int64(ThreadID);
  {$ELSE}
  Result := ThreadID;
  {$ENDIF}
end;

constructor TCrossEvent.Create(EventAttributes : PSecurityAttributes; AManualReset,InitialState : Boolean;const Name : string);
begin
  inherited Create;
  FManualReset := AManualReset;
  {$if defined(NeedCrossEvents)}
  Waiters := 0;
  pthread_cond_init(@FEvent.condvar, nil);
  pthread_mutex_init(@FEvent.mutex, nil);
  FEvent.isset:= InitialState;
  FEvent.IsDestroing := false;
  FEvent.Name:=Name;
  FEvent.EventAttr:=EventAttributes;
  {$ELSE}
  {$IFDEF WinCE}
  FEvent :=  CreateEvent(nil,AManualReset,InitialState,nil);
  {$ELSE}
  FEvent :=  CreateEvent(nil,AManualReset,InitialState,nil);
  {$ENDIF}
  {$IFEND}
end;

destructor TCrossEvent.destroy;
begin
  {$if defined(NeedCrossEvents)}
  pthread_mutex_lock(@FEvent.mutex);
  FEvent.IsDestroing := true;
  pthread_cond_broadcast(@FEvent.condvar);
  pthread_mutex_unlock(@FEvent.mutex);

  while (Waiters <> 0) do
    ThreadSwitch;

  pthread_cond_destroy(@FEvent.condvar);
  pthread_mutex_destroy(@FEvent.mutex);
  {$ELSE}
  CloseHandle(FEvent);
  {$IFEND}
  inherited destroy;
end;
function TCrossEvent.ResetEvent:Boolean;
begin
  {$if defined(NeedCrossEvents)}
  pthread_mutex_lock(@FEvent.mutex);
  FEvent.isset:=false;
  Result:=true;
  pthread_mutex_unlock(@FEvent.mutex);
  {$ELSE}
  Result:=Windows.ResetEvent(FEvent);
  {$IFEND}
end;

function TCrossEvent.SetEvent:Boolean;
begin
  {$if defined(NeedCrossEvents)}
  pthread_mutex_lock(@FEvent.mutex);
  FEvent.isset:=true;
  pthread_cond_broadcast(@FEvent.condvar);
  Result:=true;
  pthread_mutex_unlock(@FEvent.mutex);
  {$ELSE}
  Result:=Windows.SetEvent(FEvent);
  {$IFEND}
end;

function   TCrossEvent.WaitFor(Timeout : Cardinal) : TWaitResult;
{$if defined(NeedCrossEvents)}
var
  errres : cint;
  timespec : ttimespec;
  tnow : timeval;
{$IFEND}
begin
  {$if defined(NeedCrossEvents)}
  
  pthread_mutex_lock(@FEvent.mutex);
  
  if FEvent.IsDestroing then begin
     Result := wrAbandoned;
     pthread_mutex_unlock(@FEvent.mutex);
     exit;
  end;
  
  InterLockedIncrement(Waiters);

  //espera sem timeout
  //wait without timetout
  if Timeout = $FFFFFFFF then begin
     while (not FEvent.isset) and (not FEvent.IsDestroing) do
        pthread_cond_wait(@FEvent.condvar, @FEvent.mutex);

  end else begin
    //espera com timeout
    //wait the event with timeout.
    fpgettimeofday(@tnow,nil);
    timespec.tv_sec  := tnow.tv_sec + (clong(timeout) div 1000);
    timespec.tv_nsec := (clong(timeout) mod 1000)*1000000 + tnow.tv_usec*1000;
    if timespec.tv_nsec >= 1000000000 then
    begin
      inc(timespec.tv_sec);
      dec(timespec.tv_nsec, 1000000000);
    end;
    errres:=0;
    while (not FEvent.IsDestroing) and (not FEvent.isset) and (errres<>ESysETIMEDOUT) do
       errres:=pthread_cond_timedwait(@FEvent.condvar, @FEvent.mutex, @timespec);
  end;

  if (FManualReset=false) then
    FEvent.isset := false;

  //checa os resultados...
  //check the results.
  if FEvent.IsDestroing then
     Result := wrAbandoned
  else
    if FEvent.isset then
      Result := wrSignaled
    else begin
      if errres=ESysETIMEDOUT then
         Result := wrTimeout
      else
         Result := wrError;
    end;
  
  pthread_mutex_unlock(@FEvent.mutex);

  InterLockedDecrement(Waiters);
  
  {$ELSE}
  case WaitForSingleObject(FEvent, Timeout) of
    WAIT_ABANDONED: Result := wrAbandoned;
    WAIT_OBJECT_0: Result := wrSignaled;
    WAIT_TIMEOUT: Result := wrTimeout;
    WAIT_FAILED:
      begin
        Result := wrError;
      end;
  else
    Result := wrError;
  end;
  {$IFEND}
end;

end.