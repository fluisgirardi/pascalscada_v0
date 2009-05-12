unit IBoxDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

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

    ActiveZones,
    ActiveAlarme,
    ManufacturerAlarmCode,

    //1 caso a variavel tenha valor valido, 0 para invalidos
    ReturnAir1Active,
    Supply1Active,
    SetPointActive,
    EvaporatorCoilActive,
    ReturnAir2Active,
    Supply2Active,
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
    //constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
  end;

implementation

uses PLCTagNumber, CrossEvent, syncobjs, dateutils, math;

destructor  TIBoxDriver.Destroy;
begin
  inherited Destroy;
  SetLength(PStations,0);
end;

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

  with TPLCTagNumber(TagObj) do begin
    if not (PLCStation in [0..255]) then exit;
    if not (MemAddress in [0,96,168,200..205,247]) then exit;

    h:=High(PStations);
    found := false;
    for plc := 0 to h do
      if PStations[plc].Address=PLCStation then begin
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
          PStations[plc].PID0.RefCount:=0;
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
    if not (PLCStation in [0..255]) then exit;
    if not (MemAddress in [0,96,168,200..205,247]) then exit;

    h:=High(PStations);
    for plc := 0 to h do
      if PStations[plc].Address=PLCStation then begin
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
var
  plc:Integer;
  dosomething:boolean;
  tr:TTagRec;
  dummyValue:TArrayOfDouble;
begin
  dosomething := false;
  NeedSleep:=0;
  for plc:=0 to High(PStations) do begin
    //inicializa parte da estrutura de requisiçao.
    tr.Station:=PStations[plc].Address;
    tr.SubElement:=0;

    with PStations[plc].PID96 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=96;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;

    with PStations[plc].PID168 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=168;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;

    with PStations[plc].PID200 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=200;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;
    with PStations[plc].PID201 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=201;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;
    with PStations[plc].PID202 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=202;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;

    with PStations[plc].PID203 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=203;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;

    with PStations[plc].PID204 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=204;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;
    /////////////////////////////////////////////
    //pid 205 é um comando, deixe-o fora do scan.
    /////////////////////////////////////////////
    with PStations[plc].PID247 do
      if (RefCount>0) and (MilliSecondsBetween(Now,TimeStamp)>MinScanTime) then begin
        tr.Address:=247;
        dosomething:=true;
        DoRead(tr,dummyValue,false);
      end;
    if not dosomething then
      NeedSleep:=-1;
  end;

  //se nao ira fazer nada troca de thread para melhorar o desempenho.
  if not dosomething then
    NeedSleep:= -1;
end;

procedure TIBoxDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  plc:Integer;
  found:Boolean;
  pid20x:TPID20xRegister;
begin
  if not (tagrec.Station in [0..255]) then begin
    values.LastQueryResult := ioIllegalStationAddress;
    values.ValuesTimestamp :=now;
    exit;
  end;

  if not (tagrec.Address in [0,96,168,200..205,247]) then begin
    values.LastQueryResult := ioIllegalRegAddress;
    values.ValuesTimestamp :=now;
    exit;
  end;

  if (Tagrec.Address in [200..202]) and (not (Tagrec.SubElement in [0..16])) then begin
    values.LastQueryResult := ioIllegalRegAddress;
    values.ValuesTimestamp :=now;
    exit;
  end;

  found := false;
  for plc:=0 to High(PStations) do
    if PStations[plc].Address=TagRec.Station then begin
      found := true;
      break;
    end;

  if not found then begin
    values.LastQueryResult:=ioDriverError;
    values.ValuesTimestamp :=now;
    exit;
  end;

  SetLength(values.Values,1);
  case TagRec.Address of
    96: begin
      values.Values[0]      :=PStations[plc].PID96.Value;
      values.ValuesTimestamp:=PStations[plc].PID96.TimeStamp;
      values.LastQueryResult:=PStations[plc].PID96.LastReadResult;
    end;
    168: begin
      values.Values[0]      :=PStations[plc].PID168.Value;
      values.ValuesTimestamp:=PStations[plc].PID168.TimeStamp;
      values.LastQueryResult:=PStations[plc].PID168.LastReadResult;
    end;
    200..202: begin
      if TagRec.Address=200 then
        pid20x := PStations[plc].PID200;
      if TagRec.Address=201 then
        pid20x := PStations[plc].PID201;
      if TagRec.Address=202 then
        pid20x := PStations[plc].PID202;

      Values.ValuesTimestamp:=pid20x.TimeStamp;
      values.LastQueryResult:=pid20x.LastReadResult;

      with pid20x do begin
        Case TagRec.SubElement of
          0:
            values.Values[0] := ActiveZones;
          1:
            values.Values[0] := ActiveAlarme;
          2:
            values.Values[0] := ManufacturerAlarmCode;
          3:
            values.Values[0] := ReturnAir1Active;
          4:
            values.Values[0] := Supply1Active;
          5:
            values.Values[0] := SetPointActive;
          6:
            values.Values[0] := EvaporatorCoilActive;
          7:
            values.Values[0] := ReturnAir2Active;
          8:
            values.Values[0] := Supply2Active;
          9:
            values.Values[0] := OperatingModeActive;
          10:
            values.Values[0] := ReturnAir1;
          11:
            values.Values[0] := Supply1;
          12:
            values.Values[0] := SetPoint;
          13:
            values.Values[0] := EvaporatorCoil;
          14:
            values.Values[0] := ReturnAir2;
          15:
            values.Values[0] := Supply2;
          16:
            values.Values[0] := OperatingMode;
          else begin
            Values.ValuesTimestamp:=now;
            values.LastQueryResult:=ioIllegalRegAddress;
          end;
        end;
      end;
    end;
    203: begin
    end;
    204: begin
      values.Values[0]      :=PStations[plc].PID204.Value;
      values.ValuesTimestamp:=PStations[plc].PID204.TimeStamp;
      values.LastQueryResult:=PStations[plc].PID204.LastReadResult;
    end;
    205: begin
      values.Values[0]      :=PStations[plc].PID205.Value;
      values.ValuesTimestamp:=PStations[plc].PID205.TimeStamp;
      values.LastQueryResult:=PStations[plc].PID205.LastReadResult;
    end;
    247: begin
      values.Values[0]      :=PStations[plc].PID247.Value;
      values.ValuesTimestamp:=PStations[plc].PID247.TimeStamp;
      values.LastQueryResult:=PStations[plc].PID247.LastReadResult;
    end;
  end;
end;

function TIBoxDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  //não há escrita de valores nesse driver.
  Result := ioIllegalFunction;
end;

function TIBoxDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  pkg, pkgtotal:BYTES;
  cmdpkg:TIOPacket;
  event:TCrossEvent;
  plc, offset, bytesRemaim, b2, b3, b4, b5, b6, b7, b8:Integer;
  found:Boolean;
  pid20x:TPID20xRegister;
begin
  if not (tagrec.Station in [0..255]) then begin
    Result := ioIllegalStationAddress;
    exit;
  end;

  if not (tagrec.Address in [0,96,168,200..205,247]) then begin
    Result := ioIllegalRegAddress;
    exit;
  end;

  if (Tagrec.Address in [200..202]) and (not (Tagrec.SubElement in [0..16])) then begin
    Result := ioIllegalRegAddress;
    exit;
  end;

  found := false;
  for plc:=0 to High(PStations) do
    if PStations[plc].Address=tagrec.Station then begin
      found := true;
      break;
    end;

  SetLength(Values,1);
  event := TCrossEvent.Create(nil,true,false,'IBoxID'+IntToStr(PDriverID));
  AddPendingAction(event);
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
      //Nível de combustivel.
      96: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,4,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor(1000)<>wrSignaled then begin
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

        values[0]:= cmdpkg.BufferToRead[2]/2;
        Result := ioOk;
        if found then begin
          PStations[plc].PID96.Value := Values[0];
          PStations[plc].PID96.LastReadResult:=Result;
          PStations[plc].PID96.TimeStamp := Now;
        end;
      end;
      //Voltagem da bateria.
      168: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,5,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor(1000)<>wrSignaled then begin
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

        Values[0] := (cmdpkg.BufferToRead[2]*256 + cmdpkg.BufferToRead[3])/20;
        Result := ioOk;
        if found then begin
          PStations[plc].PID168.Value := Values[0];
          PStations[plc].PID168.LastReadResult:=Result;
          PStations[plc].PID168.TimeStamp := Now;
        end;
      end;
      200..202: begin
        //inicializa o pacote auxiliar, para nao perder
        //as informações da variavel no final
        if found then begin
          if tagrec.Address=200 then begin
            pid20x.RefCount:=PStations[plc].PID200.RefCount;
            pid20x.MinScantime:=PStations[plc].PID200.MinScantime;
          end;
          if tagrec.Address=201 then begin
            pid20x.RefCount:=PStations[plc].PID201.RefCount;
            pid20x.MinScantime:=PStations[plc].PID201.MinScantime;
          end;
          if tagrec.Address=202 then begin
            pid20x.RefCount:=PStations[plc].PID202.RefCount;
            pid20x.MinScantime:=PStations[plc].PID202.MinScantime;
          end;
        end;

        PCommPort.Lock(PDriverID);
        PCommPort.IOCommandASync(iocWriteRead,pkg,5,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor(1000)<>wrSignaled then begin
          Result:=ioTimeOut;
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

        //se chegou até aqui, a requisição aparentemente está ok
        //entao comeca a decodificar os dados.
        pid20x.ActiveZones:=(cmdpkg.BufferToRead[2] and $F0) div $10;
        pid20x.ActiveAlarme:=(cmdpkg.BufferToRead[2] and $F);
        offset:=0;
        if pid20x.ActiveAlarme>0 then begin
          pid20x.ManufacturerAlarmCode:=cmdpkg.BufferToRead[3];
          offset:=1;
        end else
          pid20x.ManufacturerAlarmCode := 0;

        //este bit não pode estar ligado... se estiver ligado, é falha
        //de comunicação.
        if (cmdpkg.BufferToRead[3+offset] and 1)=1 then begin
          Result := ioCommError;
          exit;
        end;

        //offset tbm diz se é necessario
        //ler mais um byte.
        bytesRemaim := offset;
        b2 := ifthen((cmdpkg.BufferToRead[3+offset] and $02)=$02,1, 0);
        b3 := ifthen((cmdpkg.BufferToRead[3+offset] and $04)=$04,2, 0);
        b4 := ifthen((cmdpkg.BufferToRead[3+offset] and $08)=$08,2, 0);
        b5 := ifthen((cmdpkg.BufferToRead[3+offset] and $10)=$10,2, 0);
        b6 := ifthen((cmdpkg.BufferToRead[3+offset] and $20)=$20,2, 0);
        b7 := ifthen((cmdpkg.BufferToRead[3+offset] and $40)=$40,2, 0);
        b8 := ifthen((cmdpkg.BufferToRead[3+offset] and $80)=$80,2, 0);

        inc(bytesRemaim, b2);
        inc(bytesRemaim, b3);
        inc(bytesRemaim, b4);
        inc(bytesRemaim, b5);
        inc(bytesRemaim, b6);
        inc(bytesRemaim, b7);
        inc(bytesRemaim, b8);

        pid20x.ReturnAir1Active     := ifthen(b8<>0,1,0);
        pid20x.Supply1Active        := ifthen(b7<>0,1,0);
        pid20x.SetPointActive       := ifthen(b6<>0,1,0);
        pid20x.EvaporatorCoilActive := ifthen(b5<>0,1,0);
        pid20x.ReturnAir2Active     := ifthen(b4<>0,1,0);
        pid20x.Supply2Active        := ifthen(b3<>0,1,0);
        pid20x.OperatingModeActive  := ifthen(b2<>0,1,0);

        //se sobrou bytes oara ler...
        if bytesRemaim>0 then begin
          //copia os primeiros bytes do pacote
          pkg := cmdpkg.BufferToRead;

          event.ResetEvent;
          PCommPort.IOCommandASync(iocRead,nil,bytesRemaim,0,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

          if event.WaitFor(1000)<>wrSignaled then begin
            Result:=ioTimeOut;
            exit;
          end;

          pkgtotal:=ConcatenateBYTES(pkg,cmdpkg.BufferToRead);

          if not CheckSumOk(pkgtotal) then begin
            Result := ioCommError;
            exit;
          end;

          //o trem comeca da pos 4 + offset...
          //incrementa o offset pra nao mudar os indices.
          //offset trabalha como cursor.
          if pid20x.ReturnAir1Active=1 then begin
            pid20x.ReturnAir1:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.Supply1Active=1 then begin
            pid20x.Supply1:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.SetPointActive =1 then begin
            pid20x.SetPoint:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.EvaporatorCoilActive=1 then begin
            pid20x.EvaporatorCoil:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.ReturnAir2Active=1 then begin
            pid20x.ReturnAir2:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.Supply2Active=1 then begin
            pid20x.Supply2:=((pkgtotal[4+offset]*256)+pkgtotal[5+offset])/10;
            inc(offset,2);
          end;
          if pid20x.OperatingModeActive=1 then begin
            pid20x.OperatingModeActive:=pkgtotal[4+offset];
            inc(offset,2);
          end;
        end;

        Result := ioOk;
        pid20x.LastReadResult:=Result;
        pid20x.TimeStamp:=Now;

        if found then
          case tagrec.Address of
            200:
              PStations[plc].PID200 := pid20x;
            201:
              PStations[plc].PID201 := pid20x;
            202:
              PStations[plc].PID202 := pid20x;
          end;


        with pid20x do begin
          Case TagRec.SubElement of
            0:
              Values[0] := ActiveZones;
            1:
              Values[0] := ActiveAlarme;
            2:
              Values[0] := ManufacturerAlarmCode;
            3:
              Values[0] := ReturnAir1Active;
            4:
              Values[0] := Supply1Active;
            5:
              Values[0] := SetPointActive;
            6:
              Values[0] := EvaporatorCoilActive;
            7:
              Values[0] := ReturnAir2Active;
            8:
              Values[0] := Supply2Active;
            9:
              Values[0] := OperatingModeActive;
            10:
              Values[0] := ReturnAir1;
            11:
              Values[0] := Supply1;
            12:
              Values[0] := SetPoint;
            13:
              Values[0] := EvaporatorCoil;
            14:
              Values[0] := ReturnAir2;
            15:
              Values[0] := Supply2;
            16:
              Values[0] := OperatingMode;
            else begin
              Result:=ioIllegalRegAddress;
            end;
          end;
        end;
      end;
      //status do motor e reset.
      204, 205: begin
        PCommPort.IOCommandASync(iocWriteRead,pkg,4,4,PDriverID,5,CommPortCallBack,false,event,@cmdpkg);

        if event.WaitFor(1000)<>wrSignaled then begin
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

        Values[0]:= cmdpkg.BufferToRead[2];
        Result := ioOk;

        if found then begin
          if tagrec.Address=204 then begin
            PStations[plc].PID204.Value:= Values[0];
            PStations[plc].PID204.LastReadResult:=Result;
            PStations[plc].PID204.TimeStamp:=Now;
          end else begin
            PStations[plc].PID205.Value:= Values[0];
            PStations[plc].PID205.LastReadResult:=Result;
            PStations[plc].PID205.TimeStamp:=Now;
          end;
        end;
      end;
      //Horimetro do motor.
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

        Values[0]:= ((cmdpkg.BufferToRead[2]*16777216) + (cmdpkg.BufferToRead[3]*65536) + (cmdpkg.BufferToRead[4]*256) + cmdpkg.BufferToRead[5])/20;
        Result := ioOk;
        if found then begin
          PStations[plc].PID247.Value := Values[0];
          PStations[plc].PID247.LastReadResult:=Result;
          PStations[plc].PID247.TimeStamp:=Now;
        end;
      end;
    end;
  finally
    SetLength(pkgtotal,0);
    SetLength(pkg,0);
    SetLength(cmdpkg.BufferToRead,0);
    SetLength(cmdpkg.BufferToWrite,0);
    RemovePendingAction(event);
    event.Destroy;
  end;
end;

end.
