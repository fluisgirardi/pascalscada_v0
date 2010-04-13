{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de ...
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit ISOTCPDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  ProtocolDriver, S7Types, Tag, ProtocolTypes, CrossEvent;

type
  TISOTCPDriver = class(TProtocolDriver)
  private
    FCPUs:TS7CPUs;
    FReadEvent,
    FConnectEvent:TCrossEvent;
    FConnectionWay:TISOTCPConnectionWay;
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
  protected
    FConnected:Boolean;
    procedure Connect(var CPU:TS7CPU); virtual;
    procedure DoAddTag(TagObj:TTag); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property ReadSomethingAlways;
    property ConnectionWay:TISOTCPConnectionWay read FConnectionWay write SetISOConnectionWay;
  end;

implementation

uses commtypes, math;

constructor TISOTCPDriver.Create(AOwner:TComponent);
begin

end;

procedure TISOTCPDriver.Connect(var CPU:TS7CPU);
var
  IOResult:TIOPacket;
  msg:BYTES;
  res:Integer;
  len:Cardinal;
begin
  SetLength(msg,22); //verificar
  msg[00] := 3;
  msg[01] := 0;
  msg[02] := 22 div $100;
  msg[03] := 22 mod $100;
  msg[04] := $11;  //$11,
  msg[05] := $E0;  //$E0,
  msg[06] := 0;    //0,
  msg[07] := 0;    //0,
  msg[08] := 0;    //0,
  msg[09] := 1;    //1,
  msg[10] := 0;    //0,
  msg[11] := $C1;  //$C1,
  msg[12] := 2;    //2,
  msg[13] := ifthen(FConnectionWay=ISOTCP, 1, $4D);    //'M',
  msg[14] := ifthen(FConnectionWay=ISOTCP, 0, $57);    //'W',
  msg[15] := $C2;  //$C2,
  msg[16] := 2;    //2,
  msg[17] := ifthen(FConnectionWay=ISOTCP, CPU.Rack+1, $4D);    //'M',
  msg[18] := ifthen(FConnectionWay=ISOTCP, CPU.Slot,   $57);    //'W',
  msg[19] := $C0;  //$C0,
  msg[20] := 1;    //1,
  msg[21] := 9;    //9;


  try
    if PCommPort<>nil then begin
      FConnectEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocWriteRead,msg,4,22,DriverID,0,CallBack,false,FConnectEvent,IOResult);

      if (res<>0) and (FConnectEvent.WaitFor($FFFFFFFF)=wrSignaled) then begin
        if (IOResult.ReadIOResult=iorOK) and (IOResult.Received=4) then begin
          len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];
          FConnectEvent.ResetEvent;
          res := PCommPort.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CallBack,false,FConnectEvent,IOResult);
          if (res<>0) and (FConnectEvent.WaitFor($FFFFFFFF)=wrSignaled) then begin
            if (IOResult.ReadIOResult=iorOK) and (IOResult.Received=(len-4)) then begin
              CPU.Connected:=true;
              exit;
            end;
          end;
        end;
      end;
    end;
    CPU.Connected:=false;
  finally
    SetLength(msg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
  end;

end;

procedure TISOTCPDriver.SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
begin
  FConnectionWay:=NewISOConWay;
end;

procedure TISOTCPDriver.DoAddTag(TagObj:TTag);
begin

end;

procedure TISOTCPDriver.DoDelTag(TagObj:TTag);
begin

end;

procedure TISOTCPDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin

end;

procedure TISOTCPDriver.DoScanRead(Sender:TObject; var NeedSleep:Integer);
begin

end;

procedure TISOTCPDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function  TISOTCPDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TISOTCPDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

end.
