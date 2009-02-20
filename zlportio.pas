{ -----------------------------------------------------------------------------}
{ Copyright 2000-2002, Zloba Alexander.  All Rights Reserved.                  }
{ This unit can be freely used and distributed in commercial and private       }
{ environments, provided this notice is not modified in any way.               }
{ -----------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions at}
{   zal@specosoft.com (Zloba Alexander)                                        }
{ You can always find the latest version of this unit at:                      }
{   http://www.specosoft.com                                                   }

{ -----------------------------------------------------------------------------}
{ Date last modified:  24/04/2002                                              }
{ -----------------------------------------------------------------------------}
{ ZLPortIO driver interface unit v1.60                                         }
{ -----------------------------------------------------------------------------}
{ Description:                                                                 }
{   This unit allow your application direct access port input and output under }
{   all versions of Microsoft Windows®                                         }
{ Depends:                                                                     }
{   zlportio.sys ddkint.pas                                                    }
{   You must distribute zlportio.sys with your application                     }
{ Procedures and functions:                                                    }
{   procedure zlioportread( const Port,DataType:Cardinal ):Cardinal;                 }
{   procedure zlioportwrite( const Port,DataType,Data:Cardinal );                 }
{                                                                              }
{  function portreadb( const Port:Cardinal ):byte;                                }
{  function portreadw( const Port:Cardinal ):word;                                }
{  function portreadl( const Port:Cardinal ):Cardinal;                               }
{                                                                              }
{  procedure portwriteb( const Port:Cardinal;const Data:byte );                   }
{  procedure portwritew( const Port:Cardinal;const Data:word );                   }
{  procedure portwritel( const Port,Data:Cardinal );                              }
{                                                                              }
{   Examples:                                                                  }
{    //  get data bits from LPT port                                           }
{      databits := portreadb( $378 )                                           }
{    //  set data bits from LPT port                                           }
{      portwriteb( $378, databits )                                            }
{    //  The second parameter determine the databus length for operation       }
{ -----------------------------------------------------------------------------}
{ Revision History:                                                            }
{ 1.00:  + First public release                                                }
{ 1.10:  + Added new functions (portreadX,portwriteX) for convenience of usage }
{ 1.20:  + Added new function (zliosetiopm) for enabling direct access to ports}
{ 1.30:  + added compiler directives for correct compilation                   }
{ 1.40:  + added opportunity to run multiply instances client to driver        }
{ 1.50:  - fixed bug with work under win98                                     }
{ 1.60:  - fixed bug with work under win xP                                    }
{------------------------------------------------------------------------------}

{$A-,H-}
//: @exclude
unit zlportio;

interface    

uses windows,sysutils,ddkint,dialogs;

Const
  ZLIO_BYTE  = 0;
  ZLIO_WORD  = 1;
  ZLIO_Cardinal = 2;

var

// if TRUE then driver was started
// in other case something wrong
// We start driver in initialization section of unit.

  ZlIOStarted:boolean = false;

// if TRUE then we can use asm IN,OUT under NT/2000
// see zliosetiopm for more details
  ZlIODirect:boolean = false;

// handle to opened driver

  HZLIO:THandle;


function portreadb( const Port:Cardinal ):byte;
function portreadw( const Port:Cardinal ):word;
function portreadl( const Port:Cardinal ):Cardinal;

procedure portwriteb( const Port:Cardinal;const Data:byte );
procedure portwritew( const Port:Cardinal;const Data:word );
procedure portwritel( const Port,Data:Cardinal );


procedure zlioportwrite( const Port,DataType,Data:Cardinal );
function zlioportread( const Port,DataType:Cardinal ):Cardinal;

// if you need the best perfomance for your IO operations
// call zliosetiopm(TRUE). This allow your application
// to use asm command IN,OUT directly in your code.

procedure zliosetiopm( const Direct:boolean );

function zliostart:boolean;
procedure zliostop;
function zlioopen( var Handle:thandle):boolean;


implementation

const
  ZLIODriverName='libio';

var
  IOCTL_ZLUNI_PORT_READ:cardinal;
  IOCTL_ZLUNI_PORT_WRITE:cardinal;
  IOCTL_ZLUNI_IOPM_ON:cardinal;
  IOCTL_ZLUNI_IOPM_OFF:cardinal;

type
TzlIOData = record
  Port,DataType,Data:Cardinal;
end;


procedure zlioportwrite( const Port,DataType,Data:Cardinal );
var resdata:TZLIOData;
    cBR:cardinal;
begin
 if (not ZLIODirect) then begin
  resdata.Port := Port;
  resdata.Data :=  Data;
  resdata.DataType :=  DataType;
  if ZLIOStarted then
   DeviceIoControl(HZLIO,IOCTL_ZLUNI_PORT_WRITE,@resdata,sizeof(resdata),nil,0,cBR,nil );
 end
 else begin
   Case DataType of
    ZLIO_BYTE : asm mov edx,Port;mov eax,data;out dx,al; end;
    ZLIO_WORD : asm mov edx,Port;mov eax,data;out dx,ax; end;
    ZLIO_Cardinal: asm mov edx,Port;mov eax,data;out dx,eax; end;
   end;
 end;
end;

function zlioportread(const Port,DataType:Cardinal):Cardinal;
var resdata:TZLIOData;
    cBR:cardinal;i:Cardinal;
begin
 if (not ZLIODirect) then begin
   resdata.Port := Port;
   resdata.DataType :=  DataType;
   if ZLIOStarted then
    DeviceIoControl(HZLIO,IOCTL_ZLUNI_PORT_READ,@resdata,sizeof(resdata),@i,sizeof(Cardinal),cBR,nil );
 end
 else begin
   Case DataType of
    ZLIO_BYTE : asm mov edx,Port;xor eax,eax;in al,dx;mov i,eax; end;
    ZLIO_WORD : asm mov edx,Port;xor eax,eax;in ax,dx;mov i,eax; end;
    ZLIO_Cardinal: asm mov edx,Port;xor eax,eax;in eax,dx;mov i,eax end;
   end;
 end;
  result := i;
end;

function portreadb( const Port:Cardinal ):byte;
begin
 Result := zlioportread(Port,ZLIO_BYTE);
end;

function portreadw( const Port:Cardinal ):word;
begin
 Result := zlioportread(Port,ZLIO_WORD);
end;

function portreadl( const Port:Cardinal ):Cardinal;
begin
 Result := zlioportread(Port,ZLIO_Cardinal);
end;

procedure portwriteb( const Port:Cardinal;const Data:byte );
begin
 zlioportwrite(Port,ZLIO_BYTE,Data);
end;

procedure portwritew( const Port:Cardinal;const Data:word );
begin
 zlioportwrite(Port,ZLIO_WORD,Data);
end;

procedure portwritel( const Port,Data:Cardinal );
begin
 zlioportwrite(Port,ZLIO_Cardinal,Data);
end;

procedure zliosetiopm( const Direct:boolean );
var cBR:cardinal;
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

function zliostart;
var
  dir:shortstring;
  aux:String
begin
 if Win32Platform<>VER_PLATFORM_WIN32_NT then
   result := true;
 else begin
   Result := false;
   aux := ExtractFilePath(ParamStr(1));
   zliostop;
   dir := aux+ZLIODriverName+'.sys'#0;
   driverinstall(pchar(@dir[1]),ZLIODriverName+#0);
   Result := driverstart(ZLIODriverName) = 0;
 end;
end;

procedure zliostop;
begin
 if Win32Platform<>VER_PLATFORM_WIN32_NT then
  exit;
 closehandle(HZLIO); 
 driverstop(ZLIODriverName);
 driverremove(ZLIODriverName);
end;

function zlioopen( var Handle:thandle):boolean;
var cERR:integer;
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

procedure zlioclose( const Handle:thandle);
begin
 if (Win32Platform=VER_PLATFORM_WIN32_NT) then
 closehandle(Handle);
end;


initialization

IOCTL_ZLUNI_PORT_READ := CTL_CODE(FILE_DEVICE_KRNLDRVR, 1, METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_ZLUNI_PORT_WRITE := CTL_CODE(FILE_DEVICE_KRNLDRVR, 2, METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_ZLUNI_IOPM_ON := CTL_CODE(FILE_DEVICE_KRNLDRVR, 3, METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_ZLUNI_IOPM_OFF := CTL_CODE(FILE_DEVICE_KRNLDRVR, 4, METHOD_BUFFERED, FILE_ANY_ACCESS);

 if Win32Platform<>VER_PLATFORM_WIN32_NT then begin
   zliostarted := true;
   zliodirect := true;
 end
 else begin
   if not zlioopen(HZLIO) then begin
    if zliostart then
     ZLIOStarted := zlioopen(HZLIO) or (Win32Platform<>VER_PLATFORM_WIN32_NT);
   end
   else
    ZLIOStarted := true;
 end;
finalization

if ZLIOStarted then
 zliostop;



end.
