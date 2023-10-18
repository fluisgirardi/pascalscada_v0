{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa uma porta de comunicação serial multiplataforma.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements a multi-platform serial port driver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit SerialPort;

interface

uses

  commtypes, CommPort, SysUtils, Classes,
  {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)} Windows,{$IFEND}
  {$IFDEF UNIX} {$IFNDEF DARWIN}Serial, {$ENDIF} Unix, BaseUnix, termio, {$ENDIF}
  DateUtils;

type

  {$IFDEF PORTUGUES}
  {:
  @name enumera as velocidades possíveis de comunicação serial.
  Essas velocidade são suportados em todos os sistemas operacionais.

  @value br110    = 110 bps
  @value br300    = 300 bps
  @value br600    = 600 bps
  @value br1200   = 1200 bps
  @value br2400   = 2400 bps
  @value br4800   = 4800 bps
  @value br9600   = 9600 bps
  @value br19200  = 19200 bps
  @value br38400  = 38400 bps
  @value br57600  = 57600 bps
  @value br115200 = 115200 bps
  }
  {$ELSE}
  {:
  @name enumerates all baud rates.
  This baud rates are supported on all OSes.

  @value br110    = 110 bps
  @value br300    = 300 bps
  @value br600    = 600 bps
  @value br1200   = 1200 bps
  @value br2400   = 2400 bps
  @value br4800   = 4800 bps
  @value br9600   = 9600 bps
  @value br19200  = 19200 bps
  @value br38400  = 38400 bps
  @value br57600  = 57600 bps
  @value br115200 = 115200 bps
  }
  {$ENDIF}
  TSerialBaudRate = (br110, br300, br600, br1200, br2400, br4800, br9600,
                     br19200, br38400, br57600, br115200, br230400, br460800,
                     br500000, br576000, br921600, br1000000, br1152000,
                     br1500000, br2000000, br2500000, br3000000, br3500000,
                     br4000000);

  {$IFDEF PORTUGUES}
  {:
  @name enumera todos as possíveis quantidades de bits de parada.
  Essas quantidades são suportados em todos os sistemas operacionais.

  @value sb1 = 1 stop bit
  @value sb2 = 2 stop bit
  }
  {$ELSE}
  {:
  @name enumarates all stop bits.
  This values are supported on all OSes

  @value sb1 = 1 stop bit
  @value sb2 = 2 stop bit
  }
  {$ENDIF}
  TSerialStopBits = (sb1, sb2);

  {$IFDEF PORTUGUES}
  {:
  @name enumera as possíveis checagens de paridade.
  Essas paridades são suportados em todos os sistemas operacionais.

  @value spNone Não faz checagem de paridade.
  @value spOdd Checagem de erros por paridade impar.
  @value spEven Checagem de erros por paridade par.
  }
  {$ELSE}
  {:
  @name enumerates all parity modes.
  This values are supported on all OSes.

  @value spNone Don't check the parity.
  @value spOdd  Check errors using the odd parity.
  @value spEven Check errors using the even parity.
  }
  {$ENDIF}
  TSerialParity = (spNone, spOdd, spEven);

  {$IFDEF PORTUGUES}
  {:
  @name enumera os possíveis tamanhos de palavra de dados.
  Essas tamanhos são suportados em todos os sistemas operacionais.

  @value db5 A palavra de dados terá 5 bits de tamanho.
  @value db6 A palavra de dados terá 6 bits de tamanho.
  @value db7 A palavra de dados terá 7 bits de tamanho.
  @value db8 A palavra de dados terá 8 bits de tamanho.
  }
  {$ELSE}
  {:
  @name enumerates all data byte sizes.
  This values are supported on all OSes.

  @value db5 The data byte will have 5 bits of size.
  @value db6 The data byte will have 6 bits of size.
  @value db7 The data byte will have 7 bits of size.
  @value db8 The data byte will have 8 bits of size.
  }
  {$ENDIF}
  TSerialDataBits= (db5, db6, db7, db8);

  {$IFDEF PORTUGUES}
  {:
  @abstract(Driver genérico para portas seriais. Atualmente funcionando para
            Windows, Linux e FreeBSD.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TCommPortDriver)
  }
  {$ELSE}
  {:
  @abstract(Serial port driver. Working on  Windows, Linux and FreeBSD.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TCommPortDriver)
  }
  {$ENDIF}

  {$IFDEF DARWIN}
  TSerialHandle = cint;

  TSerialState = record
    LineState:LongWord;
    tios:termios;
  end;
                                 //IMPORTANTE , FOI MUDADO PARA "FALSE" PARA NÃO TRAVAR EM CASO DE DESLIGAMENTO DO USB
    GetOverlappedResult(PPortHandle,POverlapped,lidos,false);
  {$ENDIF}

  { TSerialPortDriver }

  TSerialPortDriver = class(TCommPortDriver)
  private
    FRenewHandleOnCommErr: Boolean;
    PActivatedOnLoad:Boolean;
    PPortNameLoaded,
    PPortName:AnsiString;
    PTimeout:LongInt;
    PBaundRate:TSerialBaudRate;
    PStopBits:TSerialStopBits;
    PParity:TSerialParity;
    PDataBits:TSerialDataBits;
    PAcceptAnyPortName:Boolean;
    {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)}
    PPortEventName:AnsiString;
    PSavedDCB:DCB;
    PDCB:DCB;
    ComTimeouts:COMMTIMEOUTS;
    POverlapped:TOverlapped;
    PPortHandle:THandle;
    {$ELSE}
    LockOpen:Boolean;
    PPortHandle:TSerialHandle;
    PSavedState:TSerialState;
    {$IFEND}

    PPortDirPrefix : string;
    PBackupPortSettings:Boolean;
    PRWTimeout:LongInt;    
    procedure SetRenewHandleOnCommErr(AValue: Boolean);
    procedure SetTimeOut(v:LongInt);
    procedure SetRWTimeout(v:LongInt);
    procedure SetBaundRate(v:TSerialBaudRate);
    procedure SetStopBits(v:TSerialStopBits);
    procedure SetParity(v:TSerialParity);
    procedure SetDataBits(v:TSerialDataBits);

    procedure SetDevDir(DDir : AnsiString);
    function GetDevDir : AnsiString;

    procedure SetCOMPort(v:AnsiString);
    {$IF defined(WIN32) or defined(WIN64)}
    function MakeDCBString:AnsiString;
    {$IFEND}
    function COMExist(v:AnsiString):Boolean;
  protected
    procedure Read(Packet:PIOPacket); override;
    procedure Write(Packet:PIOPacket); override;
    {: @exclude }
    procedure NeedSleepBetweenRW; override;
    {: @exclude }
    procedure PortStart(var Ok:Boolean); override;
    {: @exclude }
    procedure PortStop(var Ok:Boolean); override;
    {: @exclude }
    function  ComSettingsOK:Boolean; override;
    {: @exclude }
    procedure SetActive(v: Boolean); override;
    {: @exclude }
    procedure Loaded; override;
  public
    {: @exclude }
    procedure ClearALLBuffers; override;
    //: @seealso TCommPortDriver.RenewHandle
    procedure RenewHandle; override;
    //: @seealso TCommPortDriver.getPortId
    function getPortId: TPortUniqueID; override;
  public
    {$IFDEF PORTUGUES}
    {:
    Cria um novo driver de porta serial. Tem como padrao 19200bps, 8 bits de dados,
    1 stop bits, sem verificação de paridade e 100ms de timeout.
    @seealso(TCommPortDriver)
    }
    {$ELSE}
    {:
    Creates a new serial port driver with the following settings: baud rate 19200bps,
    8 data bits, 1 stop bits, without parity check and 100ms of timeout.
    @seealso(TCommPortDriver)
    }
    {$ENDIF}
    constructor Create(AOwner:TComponent); override;
    {: @exclude }
    destructor  Destroy; override;
  published
    {$IFDEF PORTUGUES}
    {:
    Nome da porta serial que será usada. Depende de cada sistema operacional.
    No Windows é COMx, no Linux ttySx e no FreeBSD cuadx.
    }
    {$ELSE}
    {:
    Serial port driver to be used. This names depends of operating system.
    On Windows the name is COMx, ob Linux is ttySx and on FreeBSD is cuadx.
    }
    {$ENDIF}
    property COMPort:AnsiString read PPortName write SetCOMPort;

    {$IFDEF PORTUGUES}
    {:
    Diretório onde o PascaSCADA irá tentar localizar e abrir (caso Active=true)
    o device da porta serial em sistema operacinais Linux/Unix.

    Esta propriedade é útil para pessoas que usam emuladores de porta serial
    nestes sistemas operacionais.

    Em sistemas operacionais Windows, esta propriedade não tem utilidade, sendo
    mantida visível apenas para compatibilidade do projeto usuário ao longo de
    multiplos sistemas operacionais.
    }
    {$ELSE}
    {:
    Folder where PascalSCADA will search and open (if Active=true) the serial
    port device on Linux/Unix operating systems.

    This property is useful for people that uses serial port emulators on these
    operating systems.

    On windows this property is useless, only visible by user project
    compatibility across multiple OS.
    }
    {$ENDIF}
    property DevDir : AnsiString read GetDevDir write SetDevDir;

    property RenewHandleOnCommError:Boolean read FRenewHandleOnCommErr write SetRenewHandleOnCommErr;

    {$IFDEF PORTUGUES}
    {: Informa a duração máxima de uma ação leitura ou escrita. }
    {$ELSE}
    {: How many time a read or write operation can take. }
    {$ENDIF}
    property Timeout:LongInt read PTimeout write SetTimeOut stored true default 5;

    {$IFDEF PORTUGUES}
    {: Informa o tempo em milisegundos entre uma leitura e uma escrita. }
    {$ELSE}
    {: Delay between commands of read and write. }
    {$ENDIF}
    property WriteReadDelay:LongInt read PRWTimeout write SetRWTimeout stored true default 20;

    {$IFDEF PORTUGUES}
    {:
    Velocidade de comunicação da porta serial.
    @seealso(TSerialBaundRate)
    }
    {$ELSE}
    {:
    Serial port baud rate.
    @seealso(TSerialBaundRate)
    }
    {$ENDIF}
    property BaudRate:TSerialBaudRate read PBaundRate write SetBaundRate stored true default br19200;

    {$IFDEF PORTUGUES}
    {:
    Tamanho da palavra de comunicação.
    @seealso(TSerialDataBits)
    }
    {$ELSE}
    {:
    Data byte size.
    @seealso(TSerialDataBits)
    }
    {$ENDIF}
    property DataBits:TSerialDataBits read PDataBits write SetDataBits stored true default db8;

    {$IFDEF PORTUGUES}
    {:
    Informa qual vai ser o modo de checagem da paridade.
    @seealso(TSerialParity)
    }
    {$ELSE}
    {:
    Parity check.
    @seealso(TSerialParity)
    }
    {$ENDIF}
    property Paridade:TSerialParity read PParity write SetParity stored true default spNone;

    {$IFDEF PORTUGUES}
    {:
    Informa a quantidade de bits de parada.
    @seealso(TSerialStopBits)
    }
    {$ELSE}
    {:
    Stop bits.
    @seealso(TSerialStopBits)
    }
    {$ENDIF}
    property StopBits:TSerialStopBits read PStopBits write SetStopBits stored true default sb1;

    {$IFDEF PORTUGUES}
    {:
    Caso @true o driver irá fazer um backup das configurações da porta serial
    antes de comecar modifica-las e as restaura após fechar a porta serial.
    }
    {$ELSE}
    {:
    If @true the driver will do a of the older settings of the serial port before
    open it, and restore when it's closed.
    }
    {$ENDIF}
    property BackupPortSettings:Boolean read PBackupPortSettings write PBackupPortSettings stored true default false;

    {$IFDEF PORTUGUES}
    {:
    Caso @true o driver irá aceitar qualquer nome de porta serial,
    mesmo que elas não existam no sistema
    }
    {$ELSE}
    {:
    If @true the driver will accept any comm port name, even if it don´t
    exists on your system.
    }
    {$ENDIF}
    property AcceptAnyPortName:Boolean read PAcceptAnyPortName write PAcceptAnyPortName stored true default false;

    //: @seealso TCommPortDriver.OnCommPortOpened
    property OnCommPortOpened;
    //: @seealso TCommPortDriver.OnCommPortOpenError
    property OnCommPortOpenError;
    //: @seealso TCommPortDriver.OnCommPortClosed
    property OnCommPortClosed;
    //: @seealso TCommPortDriver.OnCommPortCloseError
    property OnCommPortCloseError;
    //: @seealso TCommPortDriver.OnCommErrorReading
    property OnCommErrorReading;
    //: @seealso TCommPortDriver.OnCommErrorWriting
    property OnCommErrorWriting;

    //: @seealso TCommPortDriver.ReadRetries
    property ReadRetries;
    //: @seealso TCommPortDriver.WriteRetries
    property WriteRetries;
  end;


{$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
function CTL_CODE( DeviceType, Func, Method, Access:Cardinal):Cardinal;

const METHOD_BUFFERED         = 0;
const METHOD_IN_DIRECT        = 1;
const METHOD_OUT_DIRECT       = 2;
const METHOD_NEITHER          = 3;
const FILE_DEVICE_SERIAL_PORT =$0000001b;
const FILE_ANY_ACCESS         =0;
{$IFEND}

{$IFDEF UNIX}
{$IFDEF LINUX}
var PortPrefix:array[0..7] of string = ('ttyADV', 'ttyAP', 'ttyS','ttyUSB', 'vttyAP', 'ttyMP', 'ttyr', 'ttyB');
//var PortPrefix:array[0..2] of AnsiString = ('tty', 'ttyUSB', 'ttyACM');
{$ENDIF}
{$IFDEF FREEBSD}
var PortPrefix:array[0..8] of string = ('cuad','cuau','cuaU','ttyu','ttyU', 'vttyAP', 'ttyMP', 'ttyr', 'ttyB');
//var PortPrefix:array[0..0] of AnsiString = ('cuad');
{$ENDIF}
{$IFDEF NETBSD}
var PortPrefix:array[0..8] of string = ('cuad','cuau','cuaU','ttyu','ttyU', 'vttyAP', 'ttyMP', 'ttyr', 'ttyB');
//var PortPrefix:array[0..0] of AnsiString = ('cuad');
{$ENDIF}
{$IFDEF OPENBSD}
var PortPrefix:array[0..8] of string = ('cuad','cuau','cuaU','ttyu','ttyU', 'vttyAP', 'ttyMP', 'ttyr', 'ttyB');
//var PortPrefix:array[0..0] of AnsiString = ('cuad');
{$ENDIF}
{$ifdef SunOS}
var PortPrefix:array[0..0] of AnsiString = ('tty');
{$ENDIF}
{$ifdef Darwin}
var PortPrefix:array[0..0] of AnsiString = ('tty.');
{$ENDIF}
{$ENDIF}


implementation

uses hsstrings{$IFDEF UNIX}, crossdatetime{$ENDIF};

{$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
function CTL_CODE( DeviceType, Func, Method, Access:Cardinal):Cardinal;
begin
  result := (((DeviceType) shl 16) or ((Access) shl 14) or ((Func) shl 2) or (Method));
end;
{$IFEND}


constructor TSerialPortDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PAcceptAnyPortName:=false;
  FExclusiveDevice:=true;
  PBaundRate := br19200;
  PDataBits  := db8;
  PStopBits  := sb1;
  PParity    := spNone;
  PTimeout   := 100;
  PRWTimeout := 20;
  {$IFDEF UNIX}
  LockOpen:=false;
  PPortDirPrefix:='/dev/';
  {$ENDIF}
end;

destructor TSerialPortDriver.Destroy;
begin
  inherited Destroy;
end;

procedure TSerialPortDriver.NeedSleepBetweenRW;
begin
  if PRWTimeout>0 then
    Sleep(PRWTimeout);
end;

procedure TSerialPortDriver.Read(Packet:PIOPacket);
{$IF defined(WIN32) or defined(WIN64)}
var
  lidos:Cardinal;
  tentativas:Cardinal;
begin
  tentativas := 0;

  Packet^.Received:=0;
  While (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin

    ResetEvent(POverlapped.hEvent);
    POverlapped.Offset := 0;
    POverlapped.OffsetHigh := 0;

    ReadFile(PPortHandle, Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, lidos, @POverlapped);

    WaitForSingleObject(POverlapped.hEvent, PTimeout);
    // Foi mudado para FALSE para não travar em caso de desligamento do conversor USB
    // para maiores detalhes
    // https://learn.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getoverlappedresult
    // procure pelo item "bWait"
    GetOverlappedResult(PPortHandle,POverlapped,lidos,false);
    //**************************************************************************
    Packet^.Received := Packet^.Received + lidos;
    Inc(tentativas);
  end;
{$IFEND}

{$IFDEF UNIX}
var
  lidos:LongInt;
  tentativas:Cardinal;
  start:TDateTime;
  Req, Rem:TimeSpec;
begin
  tentativas := 0;
  start := CrossNow;

  Packet^.Received := 0;
  Packet^.ReadIOResult:=iorNone;
  While (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
     fpseterrno(0);
     lidos := FileRead(PPortHandle,Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received);
     if lidos >= 0 then begin
       Packet^.Received := Packet^.Received + lidos;
     end else begin
       lidos:=fpgeterrno;
       writeln('fpgeterrno=',lidos);
       if lidos=ESysEINVAL then begin
         writeln('Renew Handle called');
         RenewHandle;
       end;
     end;
     if (MilliSecondsBetween(CrossNow,start)>PTimeout) then begin
        inc(tentativas);
        start:=CrossNow;
     end;

     //faz esperar 0,1ms
     //waits 0,1ms
     Req.tv_sec:=0;
     Req.tv_nsec:=100000;
     FpNanoSleep(@Req,@Rem);
  end;
{$ENDIF}

{$IF defined(WINCE)}
var
  tentativas:LongInt;
begin
{$IFEND}

  Packet^.ReadRetries := tentativas;
  if Packet^.ToRead>Packet^.Received then begin
    Packet^.ReadIOResult := iorTimeOut;
    if FRenewHandleOnCommErr then
       RenewHandle
    else begin
      if PClearBufOnErr then
        InternalClearALLBuffers;
    end;
  end else
    Packet^.ReadIOResult := iorOK;

  if Packet^.ReadIOResult<>iorOK then
    CommError(false, Packet^.ReadIOResult);
end;

procedure TSerialPortDriver.Write(Packet:PIOPacket);
{$IF defined(WIN32) or defined(WIN64)}
var
  escritos:Cardinal;
begin
  ResetEvent(POverlapped.hEvent);
  POverlapped.Offset := 0;
  POverlapped.OffsetHigh := 0;
  if not WriteFile(PPortHandle, Packet^.BufferToWrite[0], Packet^.ToWrite, Packet^.Written, @POverlapped) then begin
    case WaitForSingleObject(POverlapped.hEvent, PTimeout) of
      WAIT_OBJECT_0:
        begin
          Packet^.WriteIOResult := iorOK;
          GetOverlappedResult(PPortHandle,POverlapped,escritos,true);
          Packet^.Written := escritos;
        end;
      else begin
        Packet^.WriteIOResult := iorTimeOut;
        if PClearBufOnErr then
           InternalClearALLBuffers;
      end;
    end;
  end else begin
    Packet^.WriteIOResult := iorOK;
  end;
{$IFEND}
{$IFDEF UNIX}
var
  escritos:LongInt;
  tentativas:Cardinal;
begin
  tentativas := 0;

  Packet^.Written := 0;
  fpseterrno(0);
  While (Packet^.Written<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    escritos := FileWrite(PPortHandle, Packet^.BufferToWrite[Packet^.Written], Packet^.ToWrite-Packet^.Written);
    if escritos>=0 then begin
      Packet^.Written := Packet^.Written + escritos;
    end else begin
      escritos:=fpgeterrno;
      if escritos=ESysEINVAL then begin
        writeln('Renew Handle called');
        RenewHandle;
      end;
    end;
    Inc(tentativas);
  end;

  Packet^.WriteRetries := tentativas;
  if Packet^.ToWrite>Packet^.Written then begin
    Packet^.WriteIOResult := iorTimeOut;
    if FRenewHandleOnCommErr then
       RenewHandle
    else begin
      if PClearBufOnErr then
        InternalClearALLBuffers;
    end;
  end else
    Packet^.WriteIOResult := iorOK;

{$ENDIF}
{$IF defined(WINCE)}
var
  tentativas:LongInt;
begin
{$IFEND}

  if Packet^.WriteIOResult<>iorOK then
    CommError(true, Packet^.WriteIOResult);
end;

procedure TSerialPortDriver.PortStart(var Ok:Boolean);
{$IF defined(WIN32) or defined(WIN64)}
var
  strdcb:AnsiString;
label erro1, erro2, erro3;
begin
  if PActive then begin
    Ok := true;
    exit;
  end;
  PPortEventName := Name+'_'+PPortName;

  POverlapped.Offset := 0;
  POverlapped.OffsetHigh := 0;
  POverlapped.Internal := 0;
  POverlapped.InternalHigh := 0;
  POverlapped.hEvent :=  CreateEvent(NIL, TRUE, FALSE, PChar(PPortEventName)) ;

  ComTimeouts.ReadIntervalTimeout := PTimeout;
  ComTimeouts.ReadTotalTimeoutMultiplier := 2;
  ComTimeouts.ReadTotalTimeoutConstant := (PTimeout div 4);
  ComTimeouts.WriteTotalTimeoutMultiplier := 2;
  ComTimeouts.WriteTotalTimeoutConstant := (PTimeout div 4);

  if not COMExist(PPortName) then
    goto erro1;

  PPortHandle := CreateFile(PChar('\\.\'+ PPortName), GENERIC_READ or GENERIC_WRITE, 0, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED, 0);
  if PPortHandle=INVALID_HANDLE_VALUE then begin
    RefreshLastOSError;
    goto erro1;
  end;

  //seta o tamanho dos buffer se leitura e escrita
  //sets the length of the buffers of read and write
  if not SetupComm(PPortHandle, 8192, 8192) then begin
    RefreshLastOSError;
    goto erro1;
  end;

  //monta string DCB
  //makes a DCB string
  strdcb := MakeDCBString;
  //zera a estrutura DCB (um bug conhecido, parametro incorreto!);
  //Fill with zeros the structure.
  FillMemory(@PDCB,sizeof(DCB),0);
  PDCB.DCBlength := sizeof(DCB);
  if not BuildCommDCB(PChar(strdcb),PDCB) then begin
    RefreshLastOSError;
    goto erro2;
  end;

  //faz backup da DCB que estava setada na porta
  //backup the old settings.
  if PBackupPortSettings then
    GetCommState(PPortHandle,PSavedDCB);

  //seta a nova estrutura DCB na porta de comunicação
  //sets the new DCB struture.
  if not SetCommState(PPortHandle,PDCB) then begin
    RefreshLastOSError;
    goto erro3;
  end;

  //Seta os timeouts
  //sets the timeouts.
  if not SetCommTimeouts(PPortHandle,ComTimeouts) then begin
    RefreshLastOSError;
    goto erro3;
  end;

  InternalClearALLBuffers;

  ok := true;
  PActive := true;
  exit;

erro3:
  if PBackupPortSettings then
    SetCommState(PPortHandle,PSavedDCB);
erro2:
  CloseHandle(PPortHandle);
erro1:
  PPortEventName := '';
  CloseHandle(POverlapped.hEvent);
  POverlapped.hEvent := 0;
  ok := false;
  PActive := false;
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDO
{$IFEND}
{$IFDEF UNIX}

var
   r:LongInt;
   tios:termios;
begin
  //abre a porta
  //open the serial port
  PPortHandle := fpopen('/dev/'+PPortName, O_RDWR or O_NOCTTY or O_NONBLOCK);
  if PPortHandle<0 then begin
     RefreshLastOSError;
     Ok := false;
     PActive := false;
     exit;
  end;
  
  //se e para salvar as configs da porta...
  //backup the serial port settings.
  {$IFNDEF DARWIN}
  if PBackupPortSettings then
    PSavedState := SerSaveState(PPortHandle);
  {$ENDIF}

  r := 0;
  fillchar(tios, sizeof(tios), #0);

  tios.c_oflag := 0;
  tios.c_lflag := 0;

  //velocidade
  //sets the baudrate
  case PBaundRate of
     br110: begin
       tios.c_ispeed := B110;
       tios.c_ospeed := B110;
     end;
     br300: begin
       tios.c_ispeed := B300;
       tios.c_ospeed := B300;
     end;
     br600: begin
       tios.c_ispeed := B600;
       tios.c_ospeed := B600;
     end;
     br1200: begin
       tios.c_ispeed := B1200;
       tios.c_ospeed := B1200;
     end;
     br2400: begin
       tios.c_ispeed := B2400;
       tios.c_ospeed := B2400;
     end;
     br4800: begin
       tios.c_ispeed := B4800;
       tios.c_ospeed := B4800;
     end;
     br9600: begin
       tios.c_ispeed := B9600;
       tios.c_ospeed := B9600;
     end;
     //estava faltando essa velocidade de baudrate *****************************
     br19200: begin
       tios.c_ispeed := B19200;
       tios.c_ospeed := B19200;
     end;
     //*************************************************************************
     br38400: begin
       tios.c_ispeed := B38400;
       tios.c_ospeed := B38400;
     end;
     br57600: begin
       tios.c_ispeed := B57600;
       tios.c_ospeed := B57600;
     end;
     br115200: begin
       tios.c_ispeed := B115200;
       tios.c_ospeed := B115200;
     end;
     br230400: begin
       tios.c_ispeed := B230400;
       tios.c_ospeed := B230400;
     end;
     br460800: begin
       tios.c_ispeed := B460800;
       tios.c_ospeed := B460800;
     end;
     //br500000: begin
     //  tios.c_ispeed := B500000;
     //  tios.c_ospeed := B500000;
     //end;
     //br576000: begin
     //  tios.c_ispeed := B576000;
     //  tios.c_ospeed := B576000;
     //end;
     //br921600: begin
     //  tios.c_ispeed := B921600;
     //  tios.c_ospeed := B921600;
     //end;
     //br1000000: begin
     //  tios.c_ispeed := B1000000;
     //  tios.c_ospeed := B1000000;
     //end;
     //br1152000: begin
     //  tios.c_ispeed := B1152000;
     //  tios.c_ospeed := B1152000;
     //end;
     //br1500000: begin
     //  tios.c_ispeed := B1500000;
     //  tios.c_ospeed := B1500000;
     //end;
     //br2000000: begin
     //  tios.c_ispeed := B2000000;
     //  tios.c_ospeed := B2000000;
     //end;
     //br2500000: begin
     //  tios.c_ispeed := B2500000;
     //  tios.c_ospeed := B2500000;
     //end;
     //br3000000: begin
     //  tios.c_ispeed := B3000000;
     //  tios.c_ospeed := B3000000;
     //end;
     //br3500000: begin
     //  tios.c_ispeed := B3500000;
     //  tios.c_ospeed := B3500000;
     //end;
     //br4000000: begin
     //  tios.c_ispeed := B4000000;
     //  tios.c_ospeed := B4000000;
     //end;
  end;
  
  tios.c_cflag := tios.c_ispeed or CREAD or CLOCAL;
  
  //databits
  case PDataBits of
     db5:
        tios.c_cflag := tios.c_cflag or CS5;
     db6:
        tios.c_cflag := tios.c_cflag or CS6;
     db7:
        tios.c_cflag := tios.c_cflag or CS7;
     else
        tios.c_cflag := tios.c_cflag or CS8;
  end;

  //seta paridade, tamanho do byte, stop bits...
  //data byte size, parity and stop bits
  case PParity of
    spOdd:
      tios.c_cflag := tios.c_cflag or PARENB or PARODD;
    spEven:
      tios.c_cflag := tios.c_cflag or PARENB;
  end;
  

  if PStopBits=sb2 then
     tios.c_cflag := tios.c_cflag or CSTOPB;

  tcflush(PPortHandle, TCIFLUSH);

  r := tcsetattr(PPortHandle, TCSANOW, tios);
  if (r = -1) then begin
     RefreshLastOSError;
     Ok := false;
     PActive := false;
     exit;
  end;
  
  tcflush(PPortHandle, TCIOFLUSH);
  
  //seta o uso exclusivo da porta.
  //makes the serial port for exclusive access
  fpioctl(LongInt(PPortHandle), TIOCEXCL, nil);

  InternalClearALLBuffers;

  PActive := true;
  ok := true;
{$ENDIF}
end;

procedure TSerialPortDriver.PortStop(var Ok:Boolean);
begin
{$IF defined(WIN32) or defined(WIN64)}
  //fecha
  //close serial port
  if PActive then begin
    {$IFNDEF FPC}
    CancelIo(PPortHandle);
    {$ENDIF}

    PPortEventName := '';
    EscapeCommFunction(PPortHandle, CLRRTS);
    EscapeCommFunction(PPortHandle, CLRDTR);
    if PBackupPortSettings then
      SetCommState(PPortHandle,PSavedDCB);
    CloseHandle(PPortHandle);
    CloseHandle(POverlapped.hEvent);
    PActive := false;
    ok := true;
  end else
    ok := true;
{$IFEND}
{$IFDEF UNIX}
  if PActive then begin
    InternalClearALLBuffers;
    {$IFNDEF DARWIN}
    if PBackupPortSettings then
      SerRestoreState(PPortHandle,PSavedState);
    {$ENDIF}
    FpClose(PPortHandle);
  end;
  ok := true;
{$ENDIF}
end;

function TSerialPortDriver.ComSettingsOK:Boolean;
{$IF defined(WIN32) or defined(WIN64)}
var
  strdcb:AnsiString;
  vardcb:DCB;
begin
  strdcb := MakeDCBString;
  Result := COMExist(PPortName) and BuildCommDCB(PChar(strdcb),vardcb);
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  Result := COMExist(PPortName);
{$ENDIF}
end;

function TSerialPortDriver.GetDevDir : AnsiString;
begin
  Result := PPortDirPrefix;
end;

procedure TSerialPortDriver.SetDevDir(DDir : AnsiString);
begin
  DoExceptionInActive;
  PPortDirPrefix := DDir;
  if (DDir = '(none)') or (DDir = '') then PPortDirPrefix := '/dev/';
end;

procedure TSerialPortDriver.SetCOMPort(v:AnsiString);
begin
  DoExceptionInActive;

  if ([csLoading,csReading]*ComponentState)<>[] then begin
    PPortNameLoaded:=v;
    Exit;
  end;

  if COMExist(v) then
    PPortName := v
  else
    if (v='(none)') or (v='') then
       PPortName:=''
    else
       raise Exception.Create(v+': '+SserialPortNotExist);
end;

function TSerialPortDriver.COMExist(v:AnsiString):Boolean;
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring:AnsiString;
  d:DCB;
begin
  if PAcceptAnyPortName then begin
    Result:=true;
    exit;
  end;
  dcbstring := v+': baud=1200 parity=N data=8 stop=1';
  Result := BuildCommDCB(PChar(dcbstring),d)
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
var
   c:LongInt;
   //fd: cint;
begin
  if PAcceptAnyPortName then begin
    Result:=true;
    exit;
  end;

  Result := false;
  for c:=0 to high(PortPrefix) do
    if (LeftStr(v, Length(PortPrefix[c]))=PortPrefix[c]) and FileExists(PPortDirPrefix+v) then begin // Added PDevDir
      //fd:=fpopen('/dev/'+PPortName, O_RDWR or O_NOCTTY or O_NONBLOCK);

      Result := true;
      exit;
    end;
{$ENDIF}
end;

procedure TSerialPortDriver.SetTimeOut(v:LongInt);
begin
  DoExceptionInActive;
  PTimeout := v;
end;

procedure TSerialPortDriver.SetRenewHandleOnCommErr(AValue: Boolean);
begin
  if FRenewHandleOnCommErr=AValue then Exit;
  DoExceptionInActive;
  FRenewHandleOnCommErr:=AValue;
end;

procedure TSerialPortDriver.SetRWTimeout(v:LongInt);
begin
  DoExceptionInActive;
  PRWTimeout := v;
end;

procedure TSerialPortDriver.SetBaundRate(v:TSerialBaudRate);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring:AnsiString;
  d:DCB;
  Old:TSerialBaudRate;
begin
  DoExceptionInActive;

  old := PBaundRate;
  PBaundRate := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then begin
    RefreshLastOSError;
    PBaundRate := old;
    raise Exception.Create(SinvalidMode);
  end;
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PBaundRate := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetStopBits(v:TSerialStopBits);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring:AnsiString;
  d:DCB;
  Old:TSerialStopBits;
begin
  DoExceptionInActive;

  old := PStopBits;
  PStopBits := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then begin
    RefreshLastOSError;
    PStopBits := old;
    raise Exception.Create(SinvalidMode);
  end;
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PStopBits := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetParity(v:TSerialParity);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring:AnsiString;
  d:DCB;
  Old:TSerialParity;
begin
  DoExceptionInActive;

  old := PParity;
  PParity := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then begin
    RefreshLastOSError;
    PParity := old;
    raise Exception.Create(SinvalidMode);
  end;
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PParity := v;
{$ENDIF}
end;

procedure TSerialPortDriver.SetDataBits(v:TSerialDataBits);
{$IF defined(WIN32) or defined(WIN64)}
var
  dcbstring:AnsiString;
  d:DCB;
  Old:TSerialDataBits;
begin
  DoExceptionInActive;

  old := PDataBits;
  PDataBits := v;
  dcbstring := MakeDCBString;

  if not BuildCommDCB(PChar(dcbstring),d) then begin
    RefreshLastOSError;
    PDataBits := old;
    raise Exception.Create(SinvalidMode);
  end;
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  PDataBits := v;
{$ENDIF}
end;

{$IF defined(WIN32) or defined(WIN64)}
function TSerialPortDriver.MakeDCBString:AnsiString;
begin
  Result := '';
  case PBaundRate of
    br110:
      Result := 'baud=110 ';
    br300:
      Result := 'baud=300 ';
    br600:
      Result := 'baud=600 ';
    br1200:
      Result := 'baud=1200 ';
    br2400:
      Result := 'baud=2400 ';
    br4800:
      Result := 'baud=4800 ';
    br9600:
      Result := 'baud=9600 ';
    br19200:
      Result := 'baud=19200 ';
    br38400:
      Result := 'baud=38400 ';
    br57600:
      Result := 'baud=57600 ';
    br115200:
      Result := 'baud=115200 ';
    // estava faltando essas velocidade (foi testato com hardware até o 2000000)
    br230400:
      Result := 'baud=230400 ';
    br460800:
      Result := 'baud=460800 ';
    br500000:
      Result := 'baud=500000 ';
    br576000:
      Result := 'baud=576000 ';
    br921600:
      Result := 'baud=921600 ';
    br1000000:
      Result := 'baud=1000000 ';
    br1152000:
      Result := 'baud=1152000 ';
    br1500000:
      Result := 'baud=1500000 ';
    br2000000:
      Result := 'baud=2000000 ';
    br2500000:
      Result := 'baud=2500000 ';
    br3000000:
      Result := 'baud=3000000 ';
    br3500000:
      Result := 'baud=3500000 ';
    br4000000:
      Result := 'baud=4000000 ';
    ////////////////////////////////////////////////////////////////////////////
    else
      Result := 'baud=19200 ';
  end;

  case PParity of
    spNone:
      Result := Result + 'parity=N ';
    spOdd:
      Result := Result + 'parity=O ';
    spEven:
      Result := Result + 'parity=E ';
    else
      Result := Result + 'parity=N ';
  end;

  case PStopBits of
    sb1:
      Result := Result + 'stop=1 ';
    sb2:
      Result := Result + 'stop=2 ';
    else
      Result := Result + 'stop=1 ';
  end;

  case PDataBits of
    db5:
      Result := Result + 'data=5';
    db6:
      Result := Result + 'data=6';
    db7:
      Result := Result + 'data=7';
    db8:
      Result := Result + 'data=8';
    else
      Result := Result + 'data=8';
  end;
end;
{$IFEND}

procedure TSerialPortDriver.ClearALLBuffers;
{$IF defined(WIN32) or defined(WIN64)}
var
  IOCTL_SERIAL_CONFIG_SIZE:Cardinal;
  dwFlags,buf:Cardinal;
  buf2:array[0..8192] of char;
begin
  IOCTL_SERIAL_CONFIG_SIZE := CTL_CODE (FILE_DEVICE_SERIAL_PORT, 32, METHOD_BUFFERED, FILE_ANY_ACCESS);
  dwFlags := PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR;
  DeviceIoControl(PPortHandle,IOCTL_SERIAL_CONFIG_SIZE,@buf2,0,@buf2,0,buf,nil);
  DeviceIoControl(PPortHandle,IOCTL_SERIAL_CONFIG_SIZE,@buf2,8192,@buf2,8192,buf,nil);
  FlushFileBuffers(PPortHandle);
  PurgeComm(PPortHandle,dwFlags);
{$IFEND}
{$IF defined(WINCE)}
begin
  //ToDo
{$IFEND}
{$IFDEF UNIX}
begin
  //flush buffers...
  tcflush(PPortHandle, TCIFLUSH);
  tcflush(PPortHandle, TCIOFLUSH);
  //purge comm...
  {$IFDEF LINUX}
  fpioctl(LongInt(PPortHandle), TCIOFLUSH, nil);
  {$ELSE}
  fpioctl(LongInt(PPortHandle), TIOCFLUSH, nil);
  {$ENDIF}
{$ENDIF}
end;

procedure TSerialPortDriver.SetActive(v: Boolean);
begin
  if [csLoading, csReading]*ComponentState<>[] then begin
    PActivatedOnLoad:=v;
    exit;
  end;
  inherited SetActive(v);
end;

procedure TSerialPortDriver.Loaded;
begin
  inherited Loaded;
  COMPort:=PPortNameLoaded;
  SetActive(PActivatedOnLoad);
end;

procedure TSerialPortDriver.RenewHandle;
var
  v: Boolean;
begin
  {$IFNDEF WINDOWS}writeln('RenewHandle: Port Stop');{$ENDIF}
  PortStop(v);
  {$IFNDEF WINDOWS}writeln('RenewHandle: Port Start');{$ENDIF}
  PortStart(v);
  {$IFNDEF WINDOWS}writeln('RenewHandle: Done');{$ENDIF}
end;

function TSerialPortDriver.getPortId: TPortUniqueID;
begin
  Result:=inherited getPortId;
  {TODO}
end;

end.
