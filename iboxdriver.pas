unit IBoxDriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ProtocolDriver, commtypes, Tag, ProtocolTypes;

type
  TIBoxRegister = record
    RefCount:Cardinal;
    MinScanTime:Cardinal;
    Value:Double;
    TimeStamp:TDateTime;
    LastReadResult:TProtocolIOResult;
  end;

  TPID20xRegister = Record
    RefCount:Cardinal;
    TimeStamp:TDateTime;
    LastReadResult:TProtocolIOResult;
    MinScantime:Cardinal;
    ActiveZones:Byte;
    ActiveAlarme:Byte;
    ManufacturerAlarmCode:Byte;

    //1 caso a variavel tenha valor valido, 0 para invalidos
    ReturnAir1Active:Byte;
    Supply1Active:Byte;
    SetPointActive:Byte;
    EvaporatorCoilActive:v;
    ReturnAir2Active:Byte;
    Supply2Active:Byte;
    OperatingModeActive:Byte;

    ReturnAir1:Double;
    Supply1:Double;
    SetPoint:Double;
    EvaporatorCoil:Double;
    ReturnAir2:Double;
    Supply2:Double;
    OperatingMode:Double;
  end;

  TPID203Register = record
    RefCount:Cardinal;
    TimeStamp:TDateTime;
    LastReadResult:TProtocolIOResult;
    MinScantime:Cardinal;

    DigitalInput0State,
    DigitalInput1State,
    DigitalInput2State,
    DigitalInput3State,
    DigitalInput0Value,
    DigitalInput1Value,
    DigitalInput2Value,
    DigitalInput3Value,
    Reserved,
    Sensor1Active,
    Sensor2Active,
    Sensor3Active,
    Sensor4Active,
    Sensor5Active,
    Sensor6Active,
    HumidityActive:Byte;
    Sensor1Value,
    Sensor2Value,
    Sensor3Value,
    Sensor4Value,
    Sensor5Value,
    Sensor6Value,
    HumidityValue:Double;
  end;

  //TPID243Register=record
  //  Model:
  //end;

  TIBoxStation = record
    Address:Byte;
    PID0,
    PID96,
    PID168:TIBoxRegister;
    PID200,
    PID201,
    PID202:TPID20xRegister;
    PID203:TPID203Register;
    PID204,PID205, PID247:TIBoxRegister;
  end;

  TIBoxStations=array of TIBoxStation;


  TIBoxDriver = class(TProtocolDriver)
  private
    PStations:TIBoxStations;
    //Verifica se uma cadeia de bytes tem a sua soma de verificacao OK
    function CheckSumOk(const pkg:BYTES):Boolean;
    //calcula o checksum ate 1..n-1 posicao e coloca o calculo na posicao n
    procedure CalculateCheckSum(var pkg:BYTES);
  protected
    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoTagChange)
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);  override;
    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    //: @seealso(TProtocolDriver.DoWrite)
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
  end;

implementation

uses PLCTagNumber, CrossEvent, syncobjs;

function TIBoxDriver.CheckSumOk(const pkg:BYTES):Boolean;
var
  c,h:Integer;
  sum:cardinal;
begin
  try
    Result := false;
    sum :=0;
    h:=High(pkg);
    for c:=0 to h-1 do
      sum := sum + pkg[c];
    sum := (sum xor $FFFFFFFF)+1;
    Result := (pkg[h]=(sum and $FF));
  except
    Result := false;
  end;
end;

procedure TIBoxDriver.CalculateCheckSum(var pkg:BYTES);
var
  c,h:Integer;
  sum:cardinal;
begin
  sum :=0;
  h:=High(pkg);
  for c:=0 to h-1 do
    sum := sum + pkg[c];
  sum := (sum xor $FFFFFFFF)+1;
  pkg[h]:=(sum and $FF);
end;

procedure TIBoxDriver.DoAddTag(TagObj:TTag);
var
  plc,h:Integer;
  found :boolean;
begin
  if not (TagObj is TPLCTagNumber) then
    Raise Exception.Create('Tag Inválido!');

  with TagObj as TPLCTagNumber do begin
    if not (PLCStattion in [0..255]) then exit;
    if not (MemAddress in [0,96,168,200..205,247]) then exit;

    h:=High(PStations);
    for plc := 0 to h do
      if PStations[c].Address=PLCStation then begin
        found := true;
        break;
      end;

    if not found then begin
      plc:=Length(PStations);
      SetLength(PStations,plc+1);
      PStations[plc].Address:=PLCStation;
    end;

    case MemAddress of
      0: begin
        if not found then begin
          PStations[plc].PIDo.RefCount:=0;
          PStations[plc].PID0.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID0.RefCount);
        PStations[plc].PID0.MinScanTime:=Min(PStations[plc].PID0.MinScanTime,RefreshTime)
      end;
      96: begin
        if not found then begin
          PStations[plc].PID96.RefCount:=0;
          PStations[plc].PID96.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID96.RefCount);
        PStations[plc].PID96.MinScanTime:=Min(PStations[plc].PID96.MinScanTime,RefreshTime)
      end;
      168: begin
        if not found then begin
          PStations[plc].PID168.RefCount:=0;
          PStations[plc].PID168.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID168.RefCount);
        PStations[plc].PID168.MinScanTime:=Min(PStations[plc].PID168.MinScanTime,RefreshTime)
      end;
      200: begin
        if not found then begin
          PStations[plc].PID202.RefCount:=0;
          PStations[plc].PID202.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID200.RefCount);
        PStations[plc].PID200.MinScanTime:=Min(PStations[plc].PID200.MinScanTime,RefreshTime)
      end;
      201: begin
        if not found then begin
          PStations[plc].PID201.RefCount:=0;
          PStations[plc].PID201.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID201.RefCount);
        PStations[plc].PID201.MinScanTime:=Min(PStations[plc].PID201.MinScanTime,RefreshTime)
      end;
      202: begin
        if not found then begin
          PStations[plc].PID202.RefCount:=0;
          PStations[plc].PID202.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID202.RefCount);
        PStations[plc].PID202.MinScanTime:=Min(PStations[plc].PID202.MinScanTime,RefreshTime)
      end;
      203: begin
        if not found then begin
          PStations[plc].PID203.RefCount:=0;
          PStations[plc].PID203.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID203.RefCount);
        PStations[plc].PID203.MinScanTime:=Min(PStations[plc].PID203.MinScanTime,RefreshTime)
      end;
      204: begin
        if not found then begin
          PStations[plc].PID204.RefCount:=0;
          PStations[plc].PID204.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID204.RefCount);
        PStations[plc].PID204.MinScanTime:=Min(PStations[plc].PID204.MinScanTime,RefreshTime)
      end;
      205: begin
        if not found then begin
          PStations[plc].PID205.RefCount:=0;
          PStations[plc].PID205.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID205.RefCount);
        PStations[plc].PID205.MinScanTime:=Min(PStations[plc].PID205.MinScanTime,RefreshTime)
      end;
      247: begin
        if not found then begin
          PStations[plc].PID247.RefCount:=0;
          PStations[plc].PID247.MinScanTime:= RefreshTime;
        end;

        inc(PStations[plc].PID247.RefCount);
        PStations[plc].PID247.MinScanTime:=Min(PStations[plc].PID247.MinScanTime,RefreshTime)
      end;
    end;
  end;

  inherited DoAddTag(TagObj);
end;

procedure TIBoxDriver.DoDelTag(TagObj:TTag);
var
  refcount:Cardinal;
  plc,h:Integer;
  found :boolean;
begin
  if not (TagObj is TPLCTagNumber) then
    Raise Exception.Create('Tag Inválido!');

  with TagObj as TPLCTagNumber do begin
    if not (PLCStattion in [0..255]) then exit;
    if not (MemAddress in [0,96,168,200..205,247]) then exit;

    h:=High(PStations);
    for plc := 0 to h do
      if PStations[c].Address=PLCStation then begin
        found := true;
        break;
      end;

    if not found then exit;

    case MemAddress of
      0:
        Dec(PStations[plc].PID0.RefCount);
      96:
        Dec(PStations[plc].PID96.RefCount);
      168:
        Dec(PStations[plc].PID168.RefCount);
      200:
        Dec(PStations[plc].PID200.RefCount);
      201:
        Dec(PStations[plc].PID201.RefCount);
      202:
        Dec(PStations[plc].PID202.RefCount);
      203:
        Dec(PStations[plc].PID203.RefCount);
      204:
        Dec(PStations[plc].PID204.RefCount);
      205:
        Dec(PStations[plc].PID205.RefCount);
      247:
        Dec(PStations[plc].PID247.RefCount);
    end;
    refcount :=  PStations[plc].PID0.RefCount+PStations[plc].PID96.RefCount+
                 PStations[plc].PID168.RefCount+PStations[plc].PID200.RefCount+
                 PStations[plc].PID201.RefCount+PStations[plc].PID202.RefCount+
                 PStations[plc].PID203.RefCount+PStations[plc].PID204.RefCount+
                 PStations[plc].PID205.RefCount+PStations[plc].PID247.RefCount;
    //se este mid nao tem mais ninguem o requisitando
    //remove ele da fila de scan.
    if refcount=0 then begin
      h:=High(PStations);
      PStations[plc]:=PStations[h];
      SetLength(PStations,h);
    end;
  end;
  inherited DoDelTag(TagObj);
end;

procedure TIBoxDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin
  RemoveTag(TagObj);
  AddTag(TagObj);
end;

procedure TIBoxDriver.DoScanRead(Sender:TObject; var NeedSleep:Integer);
begin

end;

procedure TIBoxDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function TIBoxDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  //não há escrita de valores nesse driver.
  Result := ioIllegalFunction;
end;

function TIBoxDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  pkg:BYTES;
  cmdpkg:TIOPacket;
  event:TCrossEvent;
  plc:Integer;
  found:Boolean;
begin
  if not (tagrec.Station in [0..255]) then begin
    Result := ioIllegalStationAddress;
    exit;
  end;

  if not (tagrec.Address in [0,96,168,200..205,247]) then begin
    Result := ioIllegalRegAddress;
    exit;
  end;

  found := false;
  for plc:=0 to High(PStations) of
    if PStations[plc].Address=tagrec.Station then begin
      found := true;
      break;
    end;

  event := TCrossEvent.Create(nil,true,false,'IBoxID'+IntToStr(PDriverID));
  try

    if PCommPort=nil then begin
      Result := ioNullDriver;
      exit;
    end;

    SetLength(pkg,4);
    pkg[0]:=Byte(tagrec.Station);
    pkg[1]:= 0;
    pkg[2]:=Byte(tagrec.Address);
    CalculateCheckSum(pkg);

    event.ResetEvent;
    case tagrec.Address of
      96: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,4,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor($FFFFFFFF)<>wrSignaled then begin
          Result:=ioTimeOut;
          exit;
        end;

        if not CheckSumOk(cmdpkg.BufferToRead) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[0]<>cmdpkg.BufferToWrite[0]) or
           (cmdpkg.BufferToRead[0]<>tagrec.Station) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[1]<>cmdpkg.BufferToWrite[2]) or
           (cmdpkg.BufferToRead[1]<>tagrec.Address) then begin
          Result := ioCommError;
          exit;
        end;

        PStations[plc].PID96.Value:= cmdpkg.BufferToRead[2]/2;
        Result := ioOk;

      end;
      168: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,5,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor($FFFFFFFF)<>wrSignaled then begin
          Result:=ioTimeOut;
          exit;
        end;

        if not CheckSumOk(cmdpkg.BufferToRead) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[0]<>cmdpkg.BufferToWrite[0]) or
           (cmdpkg.BufferToRead[0]<>tagrec.Station) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[1]<>cmdpkg.BufferToWrite[2]) or
           (cmdpkg.BufferToRead[1]<>tagrec.Address) then begin
          Result := ioCommError;
          exit;
        end;

        PStations[plc].PID168.Value:= (cmdpkg.BufferToRead[2]*256 + cmdpkg.BufferToRead[3])/20;
        Result := ioOk;

      end;
      247: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,7,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor($FFFFFFFF)<>wrSignaled then begin
          Result:=ioTimeOut;
          exit;
        end;

        if not CheckSumOk(cmdpkg.BufferToRead) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[0]<>cmdpkg.BufferToWrite[0]) or
           (cmdpkg.BufferToRead[0]<>tagrec.Station) then begin
          Result := ioCommError;
          exit;
        end;

        if (cmdpkg.BufferToRead[1]<>cmdpkg.BufferToWrite[2]) or
           (cmdpkg.BufferToRead[1]<>tagrec.Address) then begin
          Result := ioCommError;
          exit;
        end;

        PStations[plc].PID168.Value:= ((cmdpkg.BufferToRead[2]*16777216) + (cmdpkg.BufferToRead[3]*65536) + (cmdpkg.BufferToRead[4]*256) + cmdpkg.BufferToRead[5])/20;
        Result := ioOk;

      end;
    end;
  finally
    SetLength(pkg,0);
    SetLength(cmdpkg.BufferToRead,0);
    SetLength(cmdpkg.BufferToWrite,0);
    event.Destroy;
  end;
end;

end.
