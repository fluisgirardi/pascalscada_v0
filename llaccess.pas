//: @exclude
unit LLAccess;

interface

uses Windows , SysUtils, ddkint, Classes;

Const
  ZLIO_BYTE  = 0;
  ZLIO_WORD  = 1;
  ZLIO_DWORD = 2;
  ZLIODriverName = 'libio';

type
TzlIOData = record
  Port,DataType,Data:dword;
end;

type
  TLLAccess = class(TComponent)
  private
    ZlIOStarted:boolean;
    ZlIODirect:boolean;
    HZLIO:THandle;
    IOCTL_ZLUNI_PORT_READ:cardinal;
    IOCTL_ZLUNI_PORT_WRITE:cardinal;
    IOCTL_ZLUNI_IOPM_ON:cardinal;
    IOCTL_ZLUNI_IOPM_OFF:cardinal;
    function  zliostart:boolean;
    procedure zliostop;
    function  zlioopen(var Handle:THandle):boolean;
    procedure zlioclose(const Handle:THandle);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    function  portreadb(const Port:dword ):byte;
    function  portreadw(const Port:dword ):word;
    function  portreadl(const Port:dword ):dword;
    procedure portwriteb(const Port:Dword;const Data:BYTE);
    procedure portwritew(const Port:dword;const Data:WORD);
    procedure portwritel(const Port,Data:DWORD);
    procedure zlioportwrite(const Port,DataType,Data:DWORD);
    function  zlioportread(const Port,DataType:dword ):DWORD;
    procedure zliosetiopm(const Direct:boolean );
  published
    //property DriverName:String read ZLIODriverName write SetDriverName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SITEC', [TLLAccess]);
end;

constructor TLLAccess.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in componentState then exit;
  //ZLIODriverName := 'libio';
  IOCTL_ZLUNI_PORT_READ := CTL_CODE(FILE_DEVICE_KRNLDRVR, 1, METHOD_BUFFERED, FILE_ANY_ACCESS);
  IOCTL_ZLUNI_PORT_WRITE := CTL_CODE(FILE_DEVICE_KRNLDRVR, 2, METHOD_BUFFERED, FILE_ANY_ACCESS);
  IOCTL_ZLUNI_IOPM_ON := CTL_CODE(FILE_DEVICE_KRNLDRVR, 3, METHOD_BUFFERED, FILE_ANY_ACCESS);
  IOCTL_ZLUNI_IOPM_OFF := CTL_CODE(FILE_DEVICE_KRNLDRVR, 4, METHOD_BUFFERED, FILE_ANY_ACCESS);
  if Win32Platform<>VER_PLATFORM_WIN32_NT then begin
    zliostarted := true;
    zliodirect := true;
  end else begin
    if not zlioopen(HZLIO) then begin
      if zliostart then
        ZLIOStarted := zlioopen(HZLIO) or (Win32Platform<>VER_PLATFORM_WIN32_NT);
      end
    else
      ZLIOStarted := true;
  end;
end;

destructor TLLAccess.Destroy;
begin
  if not (csDesigning in componentState) then
    if ZLIOStarted then
      zliostop;
  inherited Destroy;
end;

procedure TLLAccess.Loaded;
begin
  inherited Loaded;
end;

procedure TLLAccess.zlioportwrite( const Port,DataType,Data:dword );
var
  resdata:TZLIOData;
  cBR:cardinal;
begin
  if (not ZLIODirect) then begin
    resdata.Port := Port;
    resdata.Data :=  Data;
    resdata.DataType :=  DataType;
    if ZLIOStarted then
      DeviceIoControl(HZLIO,IOCTL_ZLUNI_PORT_WRITE,@resdata,sizeof(resdata),nil,0,cBR,nil );
  end else begin
    Case DataType of
      ZLIO_BYTE : asm mov edx,Port;mov eax,data;out dx,al; end;
      ZLIO_WORD : asm mov edx,Port;mov eax,data;out dx,ax; end;
      ZLIO_DWORD: asm mov edx,Port;mov eax,data;out dx,eax; end;
    end;
  end;
end;

function TLLAccess.zlioportread(const Port,DataType:dword):dword;
var
  resdata:TZLIOData;
  cBR:cardinal;i:dword;
begin
  if (not ZLIODirect) then begin
    resdata.Port := Port;
    resdata.DataType :=  DataType;
    if ZLIOStarted then
      DeviceIoControl(HZLIO,IOCTL_ZLUNI_PORT_READ,@resdata,sizeof(resdata),@i,sizeof(dword),cBR,nil );
  end else begin
    Case DataType of
      ZLIO_BYTE : asm mov edx,Port;xor eax,eax;in al,dx;mov i,eax; end;
      ZLIO_WORD : asm mov edx,Port;xor eax,eax;in ax,dx;mov i,eax; end;
      ZLIO_DWORD: asm mov edx,Port;xor eax,eax;in eax,dx;mov i,eax end;
    end;
  end;
  result := i;
end;

function TLLAccess.portreadb( const Port:dword ):byte;
begin
  Result := zlioportread(Port,ZLIO_BYTE);
end;

function TLLAccess.portreadw( const Port:dword ):word;
begin
  Result := zlioportread(Port,ZLIO_WORD);
end;

function TLLAccess.portreadl( const Port:dword ):dword;
begin
  Result := zlioportread(Port,ZLIO_DWORD);
end;

procedure TLLAccess.portwriteb( const Port:Dword;const Data:byte );
begin
  zlioportwrite(Port,ZLIO_BYTE,Data);
end;

procedure TLLAccess.portwritew( const Port:dword;const Data:word );
begin
  zlioportwrite(Port,ZLIO_WORD,Data);
end;

procedure TLLAccess.portwritel( const Port,Data:dword );
begin
  zlioportwrite(Port,ZLIO_DWORD,Data);
end;

procedure TLLAccess.zliosetiopm( const Direct:boolean );
var
  cBR:cardinal;
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT then
    if ZLIOStarted then begin
      if Direct then
        DeviceIoControl(HZLIO,IOCTL_ZLUNI_IOPM_ON,nil,0,nil,0,cBR,nil )
      else
        DeviceIoControl(HZLIO,IOCTL_ZLUNI_IOPM_OFF,nil,0,nil,0,cBR,nil );
    ZLIODirect := Direct;
  end
end;

function TLLAccess.zliostart;
var
  dir:shortstring;
  aux:String;
begin
  if Win32Platform<>VER_PLATFORM_WIN32_NT then
    result := true
  else begin
    Result := false;
    aux := ExtractFilePath(ParamStr(0));
    zliostop;
    dir := aux+ZLIODriverName+'.sys'#0;
    
    driverinstall(pchar(@dir[1]),ZLIODriverName+#0);
    Result := driverstart(ZLIODriverName) = 0;
  end;
end;

procedure TLLAccess.zliostop;
begin
  if Win32Platform<>VER_PLATFORM_WIN32_NT then
    exit;
  closehandle(HZLIO);
  driverstop(ZLIODriverName);
  driverremove(ZLIODriverName);
end;

function TLLAccess.zlioopen( var Handle:thandle):boolean;
var
  cERR:integer;
  s:string;
begin
  if Win32Platform<>VER_PLATFORM_WIN32_NT then begin
    result := true;
    exit;
  end;
  Result := false;
  Handle := THandle(-1);
  Handle := createFile('\\.\ZLPORTIO',
                  GENERIC_READ or GENERIC_WRITE,
                  0,
                  nil,
                  OPEN_EXISTING,
                  FILE_ATTRIBUTE_NORMAL,
                  0 );
  cERR := getlasterror;
  s := messagestring( cerr);
  if (cERR = ERROR_ALREADY_EXISTS)or(cERR = ERROR_SUCCESS) then Result := True;
end;

procedure TLLAccess.zlioclose( const Handle:thandle);
begin
  if (Win32Platform=VER_PLATFORM_WIN32_NT) then
    closehandle(Handle);
end;

end.
