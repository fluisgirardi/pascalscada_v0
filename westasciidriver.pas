unit WestASCIIDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ProtocolDriver, Tag, ProtocolTypes, commtypes, CrossEvent, syncobjs;

type
  TParameter = record
    ParameterID:Char;
    FunctionAllowed:Byte;
    ReadOnly:Boolean;
    Decimal:Byte;
  end;

  TWestRegister = record
    RefCount:Integer;
    Value:Double;
    Timestamp:TDateTime;
    LastReadResult, LastWriteResult:TProtocolIOResult;
    ScanTime:Array of Integer;
    MinScanTime:Integer;
  end;
  TWestRegisters = array[$00..$1b] of TWestRegister;

  TWestAddressRange = 0..99;
  TWestDevice = record
    Address:TWestAddressRange;
    Registers:TWestRegisters;
  end;

  TScanTableReg = record
    Value:Double;
    Decimal:Byte;
    IOResult:TProtocolIOResult;
  end;

  TScanTable = record
    PV,
    SP,
    Status,
    Out1,
    Out2:TScanTableReg;
    HaveOut2:Boolean;
  end;

  TWestASCIIDriver = class(TProtocolDriver)
  private
    FWestDevices:TWestDevice;
{d} procedure AddressToChar(Addr:TWestAddressRange; var ret:BYTES);
{d} function  WestToDouble(const buffer:Array of byte; var Value:Double):TProtocolIOResult;
{d} function  WestToDouble(const buffer:Array of byte; var Value:Double; var dec:Byte):TProtocolIOResult;
{d} function  DoubleToWest(var buffer:Array of Byte; const Value:Double):TProtocolIOResult; overload;
{d} function  DoubleToWest(var buffer:Array of Byte; const Value:Double; const dec:BYTE):TProtocolIOResult; overload;

{d} function  ParameterValue (const DeviceID:TWestAddressRange; const Parameter:BYTE; var   Value:Double; var   dec:BYTE):TProtocolIOResult;
{d} function  ModifyParameter(const DeviceID:TWestAddressRange; const Parameter:BYTE; const Value:Double; const dec:BYTE):TProtocolIOResult;

{d} function  ScanTable(DeviceID:TWestAddressRange; var ScanTableValues:TScanTable):TProtocolIOResult;

  protected
    procedure DoAddTag(TagObj:TTag); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;
    procedure DoScanRead(Sender:TObject); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    DeviceActive(DeviceID:TWestAddressRange):TProtocolIOResult;
  published
    property ReadSomethingAlways;
  end;

var
  ParameterList:array[$00..$1b] of TParameter;

implementation

uses PLCTagNumber, math;

constructor TWestASCIIDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PReadSomethingAlways:=true;
end;

destructor  TWestASCIIDriver.Destroy;
begin
  inherited Destroy;
end;

procedure TWestASCIIDriver.DoAddTag(TagObj:TTag);
var
  plc:Integer;
  foundplc:boolean;
begin
  if not (TagObj is TPLCTagNumber) then
    raise Exception.Create('Este driver suporta somente tags PLC simples. Tags Bloco e String não são suportados!');

  foundplc:=false;
  for plc:=0 to High(FWestDevices) do
    if FWestDevices[plc].

  inherited DoAddTag(TagObj);

end;

procedure TWestASCIIDriver.DoDelTag(TagObj:TTag);
begin
  inherited DoDelTag(TagObj);
end;

procedure TWestASCIIDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin
  inherited DoTagChange(TagObj,Change,oldValue,newValue);
end;

procedure TWestASCIIDriver.DoScanRead(Sender:TObject);
begin

end;

procedure TWestASCIIDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function  TWestASCIIDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TWestASCIIDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function TWestASCIIDriver.DeviceActive(DeviceID:TWestAddressRange):TProtocolIOResult;
var
  buffer, No:BYTES;
  pkg:TIOPacket;
  evento:TCrossEvent;
begin
  try
    evento := TCrossEvent.Create(nil, true, false, 'WestDeviceActive');

    SetLength(buffer,6);
    SetLength(No,2);

    AddressToChar(DeviceID,No);

    buffer[0]:=Ord('L');
    buffer[1]:=No[0];
    buffer[2]:=No[1];
    buffer[3]:=Ord('?');
    buffer[4]:=Ord('?');
    buffer[5]:=Ord('*');

    if PCommPort=nil then begin
      Result := ioNullDriver;
      exit;
    end;

    evento.ResetEvent;
    PCommPort.IOCommandASync(iocWriteRead, buffer, 6, 6, DriverID, 5, CommPortCallBack, false, evento, @pkg);

    if evento.WaitFor(60000)<>wrSignaled then begin
      Result := ioDriverError;
      exit;
    end;

    SetLength(buffer,0);
    buffer := pkg.BufferToRead;

    if (buffer[0]=Ord('L')) and (buffer[1]=No[0]) and (buffer[2]=No[1]) and (buffer[3]=Ord('?')) and (buffer[4]=Ord('A')) and (buffer[5]=Ord('*')) then begin
      result := ioOk;
      exit;
    end;

    if (buffer[0]=Ord('L')) and (buffer[1]=No[1]) and (buffer[2]=Ord('?')) and (buffer[3]=Ord('A')) and (buffer[4]=Ord('*')) then begin
      result := ioOk;
      exit;
    end;

    Result := ioCommError;
  finally
    SetLength(pkg.BufferToRead,0);
    SetLength(pkg.BufferToWrite,0);
    SetLength(buffer,0);
    SetLength(No,0);
    evento.Destroy;
  end;
end;

procedure TWestASCIIDriver.AddressToChar(Addr:TWestAddressRange; var ret:BYTES);
var
   Dezenas, Unidades:BYTE;
begin
  if not Assigned(ret) then exit;

  //testa as condições q fariam esse procedimento falhar
  if ((Addr<1) or (Addr>99)) then
    raise Exception.Create('Fora dos limites!');

  Unidades := Addr mod 10;
  Dezenas  := (Addr-Unidades) div 10;

  ret[0] := (48 + Dezenas);
  ret[1] := (48 + Unidades);
end;

function TWestASCIIDriver.WestToDouble(const buffer:Array of byte; var Value:Double; var dec:Byte):TProtocolIOResult;
var
  a,b,c,d,ok,r:BYTE;
  i, aux:Integer;
begin
  if ((buffer[0]=Ord('<')) and (buffer[1]=Ord('?')) and (buffer[2]=Ord('?')) and (buffer[3]=Ord('>'))) then
    Result := ioIllegalValue;

  for i:=0 to 4 do begin
    aux := (ord(buffer[i])-48);
    if ((aux<0) or (aux>9)) then begin
      Result := ioCommError;
      exit;
    end;
  end;

  a := buffer[0]-48; //ascii para decimal
  b := buffer[1]-48;
  c := buffer[2]-48;
  d := buffer[3]-48;
  r := buffer[4]-48;

  case r of
    $30: begin
      Value  := (a*1000)+(b*100)+(c*10)+d;
      dec    := 0;
      Result := ioOk;
    end;
    $31: begin
      Value  := (a*100)+(b*10)+c+(d/10);
      dec    := 1;
      Result := ioOk;
    end;
    $32: begin
      Value  := (a*10)+b+(c/10)+(d/100);
      dec    := 2;
      Result := ioOk;
    end;
    $33: begin
      Value  := a+(b/10)+(c/100)+(d/1000);
      dec    := 3;
      Result := ioOk;
    end;
    $35: begin
      Value  := ((a*1000)+(b*100)+(c*10)+d)*(-1);
      dec    := 0;
      Result := ioOk;
    end;
    $36: begin
      Value  := ((a*100)+(b*10)+c+(d/10))*(-1);
      dec    := 1;
      Result := ioOk;
    end;
    $37: begin
      Value := ((a*10)+(b)+(c/10)+(d/100))*(-1);
      dec    := 2;
      Result := ioOk;
    end;
    $38: begin
      Value := (a+(b/10)+(c/100)+(d/1000))*(-1);
      dec    := 3;
      Result := ioOk;
    end;
    else
      Result := ioCommError;
  end;
end;

function  TWestASCIIDriver.WestToDouble(const buffer:Array of byte; var Value:Double):TProtocolIOResult;
var
  cd:BYTE;
begin
  Result :=  WestToDouble(buffer,Value,cd);
end;

function  TWestASCIIDriver.DoubleToWest(var buffer:Array of Byte; const Value:Double):TProtocolIOResult;
var
   caso:BYTE;
   numaux:Extended;
   c, l:Integer;
   aux:String;
begin
  caso:=255;

  if (Value>=10000) or (Value<=-10000) then begin
    Result := ioIllegalValue;
    exit;
  end;

  caso:=IfThen((Value>=1000) and (Value<10000),$30,caso);
  caso:=IfThen((Value>=100) and (Value<1000),$31,caso);
  caso:=IfThen((Value>=10) and (Value<100),$32,caso);
  caso:=IfThen((Value>=0) and (Value<10),$33,caso);

  caso:=IfThen((Value<=-1000) and (Value>-10000),$35,caso);
  caso:=IfThen((Value<=-100) and (Value>-1000),$36,caso);
  caso:=IfThen((Value<=-10) and (Value>-100),$37,caso);
  caso:=IfThen((Value < 0) and (Value>-10),$38,caso);

  case caso of
    $30:
      numaux := Value;
    $31:
      numaux := Value*10;
    $32:
      numaux := Value*100;
    $33:
      numaux := Value*1000;
    $35:
      numaux := Value*(-1);
    $36:
      numaux := Value*(-10);
    $37:
      numaux := Value*(-100);
    $38:
      numaux := Value*(-1000);
    else begin
      Result := ioIllegalValue;
      exit;
    end;
  end;

   aux := FormatFloat('0000',Abs(numaux));

   for c:=0 to 3 do
      buffer[c] := Ord(aux[1+c]);

   buffer[4] := caso;
   Result := ioOk;
end;

function  TWestASCIIDriver.DoubleToWest(var buffer:Array of Byte; const Value:Double; const dec:BYTE):TProtocolIOResult;
var
   caso:BYTE;
   c:Integer;
   numaux:Double;
   aux:String;
begin
   caso:=255;

   if (Value>=10000) or (Value<=-10000) then begin
       Result := ioIllegalValue;
       exit;
   end;

   caso:=IfThen(((caso=255) and (dec<=0) and (Value<10000) and (Value>=0)), $30, caso);
   caso:=IfThen(((caso=255) and (dec<=1) and (Value<1000) and (Value>=0)), $31, caso);
   caso:=IfThen(((caso=255) and (dec<=2) and (Value<100) and (Value>=0)), $32, caso);
   caso:=IfThen(((caso=255) and (dec<=3) and (Value<10) and (Value>=0)), $33, caso);

   caso:=IfThen(((caso=255) and (dec<=0) and (Value>-10000) and (Value<0)), $35, caso);
   caso:=IfThen(((caso=255) and (dec<=1) and (Value>-1000) and (Value<0)), $36, caso);
   caso:=IfThen(((caso=255) and (dec<=2) and (Value>-100) and (Value<0)), $37, caso);
   caso:=IfThen(((caso=255) and (dec<=3) and (Value>-10) and (Value<0)), $38, caso);

   if (caso = 255) then begin
      Result := ioIllegalValue;
      exit;
   end;

   case caso of
      $30:
         numaux := Value;
      $31:
         numaux := (Value*10);
      $32:
         numaux := Value*100;
      $33:
         numaux := Value*1000;
      $35:
         numaux := Value*(-1);
      $36:
         numaux := Value*(-10);
      $37:
         numaux := Value*(-100);
      $38:
         numaux := Value*(-1000);
      else begin
         Result := ioIllegalValue;
         exit;
      end;
   end;

   aux := FormatFloat('0000',Abs(numaux));

   for c:=0 to 3 do
      buffer[c] := ord(aux[1+c]);

   buffer[4] := caso;
   Result := ioOk;
end;

function  TWestASCIIDriver.ParameterValue(const DeviceID:TWestAddressRange;
                                          const Parameter:BYTE;
                                          var   Value:Double;
                                          var   dec:BYTE):TProtocolIOResult;
var
  buffer, No:BYTES;
  b1, b2:Boolean;
  pkg:TIOPacket;
  evento:TCrossEvent;
begin
  try

    evento := TCrossEvent.Create(nil, true, false, 'WestGetParamValue');

    SetLength(buffer,11);
    SetLength(No,2);

    AddressToChar(DeviceID,No);

    buffer[0]:=Ord('L');
    buffer[1]:=No[0];
    buffer[2]:=No[1];
    buffer[3]:=Parameter;
    buffer[4]:=Ord('?');
    buffer[5]:=Ord('*');

    if PCommPort=nil then begin
      Result := ioNullDriver;
      exit;
    end;

    evento.ResetEvent;
    PCommPort.IOCommandASync(iocWriteRead, buffer, 11, 6, DriverID, 5, CommPortCallBack, false, evento, @pkg);

    if evento.WaitFor(60000)<>wrSignaled then begin
      Result := ioDriverError;
      exit;
    end;

    SetLength(buffer,0);
    buffer := pkg.BufferToRead;

    b1 := (buffer[0]=Ord('L')) and (buffer[1]=No[0]) and (buffer[2]=No[1]) and (buffer[3]=Parameter) and (buffer[9]=Ord('N')) and (buffer[10]=Ord('*'));
    b2 := (buffer[0]=Ord('L')) and (buffer[1]=No[1]) and (buffer[2]=Parameter) and (buffer[8]=Ord('N')) and (buffer[9]=Ord('*'));
    if (b1 or b2) then
      Result := ioIllegalFunction
    else begin
      b1 := (buffer[0]=Ord('L')) and (buffer[1]=No[0]) and (buffer[2]=No[1]) and (buffer[3]=Parameter) and (buffer[9]=Ord('A')) and (buffer[10]=Ord('*'));
      b2 := (buffer[0]=Ord('L')) and (buffer[1]=No[1]) and (buffer[2]=Parameter) and (buffer[8]=Ord('A')) and (buffer[9]=Ord('*'));
      if (b1 or b2) then begin

        b1 := (buffer[4]=Ord('<')) and (buffer[5]=Ord('?')) and (buffer[6]=Ord('?')) and (buffer[7]=Ord('>'));

        if b1 then
          Result := ioIllegalValue
        else begin
          Result := WestToDouble(buffer[4],Value);
        end;
      end else
        Result := ioCommError;
    end;
  finally
    SetLength(pkg.BufferToRead,0);
    SetLength(pkg.BufferToWrite,0);
    SetLength(buffer,0);
    SetLength(No,0);
    evento.Destroy;
  end;
end;

function  TWestASCIIDriver.ModifyParameter(const DeviceID:TWestAddressRange; const Parameter:BYTE; const Value:Double; const dec:BYTE):TProtocolIOResult;
var
  buffer, respprog, No:BYTES;
  flag:Boolean;
  pkg:TIOPacket;
  evento:TCrossEvent;
  i:Integer;
begin
  try
    evento := TCrossEvent.Create(nil, true, false, 'WestModifyParamValue');

    flag := true;

    SetLength(No,2);
    SetLength(buffer,20);
    SetLength(respprog,12);

    AddressToChar(DeviceID,No);
    buffer[0] := Ord('L');
    buffer[1] := No[0];
    buffer[2] := No[1];
    buffer[3] := Parameter;
    buffer[4] := Ord('#');
    DoubleToWest(buffer[5],Value,dec);
    buffer[10] := Ord('*');

    respprog[0] := Ord('L');
    respprog[1] := No[0];
    respprog[2] := No[1];
    respprog[3] := Parameter;
    DoubleToWest(respprog[4],Value,dec);
    respprog[9] := Ord('I');
    respprog[10] := Ord('*');

    if PCommPort=nil then begin
      Result := ioNullDriver;
      exit;
    end;

    evento.ResetEvent;
    PCommPort.IOCommandASync(iocWriteRead, buffer, 11, 11, DriverID, 10, CommPortCallBack, false, evento, @pkg);

    if evento.WaitFor(60000)=wrSignaled then begin
      Result := ioDriverError;
      exit;
    end;

    for i:=0 to 10 do
      flag := flag and (respprog[i]=pkg.BufferToRead[i]);

    if (not flag) then begin
      Result := ioCommError;
      exit;
    end;

    SetLength(buffer,0);
    SetLength(buffer,12);

    SetLength(pkg.BufferToRead, 0);
    SetLength(pkg.BufferToWrite,0);

    buffer[0] := Ord('L');
    buffer[1] := No[0];
    buffer[2] := No[1];
    buffer[3] := Parameter;
    buffer[4] := Ord('I');
    buffer[5] := Ord('*');

    evento.ResetEvent;
    PCommPort.IOCommandASync(iocWriteRead, buffer, 11, 6, DriverID, 10, CommPortCallBack, false, evento, @pkg);

    if evento.WaitFor(60000)<>wrSignaled then begin
      Result := ioDriverError;
      exit;
    end;

    if ((pkg.BufferToRead[8]=Ord('N')) or (pkg.BufferToRead[9]=Ord('N'))) then begin
      Result := ioIllegalFunction;
      exit;
    end;
    Result := ioOk;
  finally
    SetLength(pkg.BufferToRead, 0);
    SetLength(pkg.BufferToWrite,0);
    SetLength(No,0);
    SetLength(buffer,0);
    SetLength(respprog,0);
    evento.Destroy;
  end;
end;

function  TWestASCIIDriver.ScanTable(DeviceID:TWestAddressRange; var ScanTableValues:TScanTable):TProtocolIOResult;
var
   buffer, bufferb, No:BYTES;
   err, cd:BYTE;
   b1, b2:Boolean;
   aux:Double;
   pkg:TIOPacket;
   evento:TCrossEvent;
   OffsetSpace, OffsetNo:Integer;
begin
  try
    evento := TCrossEvent.Create(nil, true, false, 'WestModifyParamValue');

    SetLength(buffer,35);
    SetLength(bufferb,35);
    SetLength(No,2);

    AddressToChar(DeviceID,No);

    buffer[0]:=Ord('L');
    buffer[1]:=No[0];
    buffer[2]:=No[1];
    buffer[3]:=Ord(']');
    buffer[4]:=Ord('?');
    buffer[5]:=Ord('*');

    if PCommPort<>nil then begin
      Result := ioNullDriver;
      exit;
    end;

    PCommPort.Lock(DriverID);

    evento.ResetEvent;
    PCommPort.IOCommandASync(iocWriteRead, buffer, 6, 6, DriverID, 10, CommPortCallBack, false, evento, @pkg);

    buffer := pkg.BufferToRead;

    b2 := (buffer[0]=Ord('L')) and (buffer[1]=No[0]) and (buffer[2]=No[1]) and (buffer[3]=Ord(']')) and (buffer[4]=Ord('2'));
    b1 := (buffer[0]=Ord('L')) and (buffer[1]=No[1]) and (buffer[2]=Ord(']')) and (buffer[3]=Ord('2'));

    if (b1=false) and (b2=false) then begin
      Result := ioCommError;
      exit;
    end;

    //faz a leitura imediata do resto do pacote... termina em PCommPort.Unlock()

    evento.ResetEvent;
    //se respondeu o endereco com dois bytes, incrementa offset da array.
    OffsetNo:=0;
    if b2 then
      OffsetNo:=1;

    case Chr(buffer[4+OffsetNo]) of
      '0':
        PCommPort.IOCommandASync(iocRead, buffer, 21+OffsetNo, 0, DriverID, 10, CommPortCallBack, false, evento, @pkg);
      '5':
        PCommPort.IOCommandASync(iocRead, buffer, 26+OffsetNo, 0, DriverID, 10, CommPortCallBack, false, evento, @pkg);
      else begin
        Result := ioCommError;
        exit;
      end;
    end;

    if evento.WaitFor(60000)<>wrSignaled then begin
      Result := ioDriverError;
      exit;
    end;

    PCommPort.Unlock(DriverID);

    if Chr(buffer[4+OffsetNo]) in ['0','5'] then begin
      if buffer[5+OffsetNo]=Ord(' ') then
        OffsetSpace := 1
      else
        OffsetSpace := 0;

      if ((buffer[25+OffsetNo+OffsetSpace]<>Ord('A')) or (buffer[26+OffsetNo+OffsetSpace]<>Ord('*'))) then begin
        Result := ioCommError;
        exit;
      end;

      Result := WestToDouble(buffer[5+OffsetNo+OffsetSpace], ScanTableValues.SP.Value, ScanTableValues.SP.Decimal);
      if (Result=ioCommError) then
        exit;
      ScanTableValues.SP.IOResult:=Result;

      Result := WestToDouble(buffer[10+OffsetNo+OffsetSpace], ScanTableValues.PV.Value, ScanTableValues.PV.Decimal);
      if (Result=ioCommError) then
        exit;
      ScanTableValues.PV.IOResult:=Result;

      Result := WestToDouble(buffer[15+OffsetNo+OffsetSpace], ScanTableValues.Out1.Value, ScanTableValues.Out1.Decimal);
      if (Result=ioCommError) then
        exit;
      ScanTableValues.Out1.IOResult:=Result;

      if Chr(buffer[4+OffsetNo])='0' then begin
        Result := WestToDouble(buffer[20+OffsetNo+OffsetSpace], ScanTableValues.Status.Value, ScanTableValues.Status.Decimal);
        if (Result=ioCommError) then
          exit;
        ScanTableValues.Status.IOResult:=Result;
      end else begin
        Result := WestToDouble(buffer[20+OffsetNo+OffsetSpace], ScanTableValues.Out2.Value, ScanTableValues.Out2.Decimal);
        if (Result=ioCommError) then
          exit;
        ScanTableValues.Out2.IOResult:=Result;

        Result := WestToDouble(buffer[25+OffsetNo+OffsetSpace], ScanTableValues.Status.Value, ScanTableValues.Status.Decimal);
        if (Result=ioCommError) then
          exit;
        ScanTableValues.Status.IOResult:=Result;
      end;
      Result := ioOk;
    end else
      Result := ioCommError;
  finally
    if PCommPort<>nil then
      if PCommPort.LockedBy=DriverID then
         PCommPort.Unlock(DriverID);
    evento.Destroy;
  end;
end;

initialization

   //Cria a lista de Parametros Validos...
   //SetPoint
   ParameterList[$00].ParameterID := 'S';
   ParameterList[$00].FunctionAllowed :=  0;
   ParameterList[$00].ReadOnly :=  false;
   ParameterList[$00].Decimal := 255;

   //PV
   ParameterList[$01].ParameterID := 'M'; //ID do Parametro
   ParameterList[$01].FunctionAllowed :=  2 ; //Funcao q pode usar, 0 := todas as funcoes
   ParameterList[$01].ReadOnly :=  true ; //ReadOnly 1 := yes?
   ParameterList[$01].Decimal := 255; // casas decimais variaveis...

   //Power Output value
   ParameterList[$02].ParameterID := 'W';
   ParameterList[$02].FunctionAllowed :=  0;
   ParameterList[$02].ReadOnly :=  false;
   ParameterList[$02].Decimal := 255;

   //Controller status
   ParameterList[$03].ParameterID := 'L';
   ParameterList[$03].FunctionAllowed :=  2;
   ParameterList[$03].ReadOnly :=  true;
   ParameterList[$03].Decimal := 0;

   //Scale Range Max
   ParameterList[$04].ParameterID := 'G';
   ParameterList[$04].FunctionAllowed :=  0;
   ParameterList[$04].ReadOnly :=  false;
   ParameterList[$04].Decimal := 255;

   //Scale Range Min
   ParameterList[$05].ParameterID := 'H';
   ParameterList[$05].FunctionAllowed :=  0;
   ParameterList[$05].ReadOnly :=  false;
   ParameterList[$05].Decimal := 255;

   //Scale Range Dec. Point
   ParameterList[$06].ParameterID := 'Q';
   ParameterList[$06].FunctionAllowed :=  0;
   ParameterList[$06].ReadOnly :=  false;
   ParameterList[$06].Decimal := 0;

   //Input filter time constant
   ParameterList[$07].ParameterID := 'm';
   ParameterList[$07].FunctionAllowed :=  0;
   ParameterList[$07].ReadOnly :=  false;
   ParameterList[$07].Decimal := 255;

   //Output 1 Power Limit
   ParameterList[$08].ParameterID := 'B';
   ParameterList[$08].FunctionAllowed :=  0;
   ParameterList[$08].ReadOnly :=  false;
   ParameterList[$08].Decimal := 255;

   //Output 1 cycle time
   ParameterList[$09].ParameterID := 'N';
   ParameterList[$09].FunctionAllowed :=  0;
   ParameterList[$09].ReadOnly :=  false;
   ParameterList[$09].Decimal := 1;

   //Output 2 cycle time
   ParameterList[$0a].ParameterID := 'O';
   ParameterList[$0a].FunctionAllowed :=  0;
   ParameterList[$0a].ReadOnly :=  false;
   ParameterList[$0a].Decimal := 1;

   //Recorder output scale max
   ParameterList[$0b].ParameterID := '[';
   ParameterList[$0b].FunctionAllowed :=  0;
   ParameterList[$0b].ReadOnly :=  false;
   ParameterList[$0b].Decimal := 255;

   //Recorder output scale min
   ParameterList[$0c].ParameterID := '\';
   ParameterList[$0c].FunctionAllowed :=  0;
   ParameterList[$0c].ReadOnly :=  false;
   ParameterList[$0c].Decimal := 255;

   //SetPoint ramp rate
   ParameterList[$0d].ParameterID := '^';
   ParameterList[$0d].FunctionAllowed :=  0;
   ParameterList[$0d].ReadOnly :=  false;
   ParameterList[$0d].Decimal := 255;

   //Setpoint high limit
   ParameterList[$0e].ParameterID := 'A';
   ParameterList[$0e].FunctionAllowed :=  0;
   ParameterList[$0e].ReadOnly :=  false;
   ParameterList[$0e].Decimal := 255;

   //Setpoint low limit
   ParameterList[$0f].ParameterID := 'T';
   ParameterList[$0f].FunctionAllowed :=  0;
   ParameterList[$0f].ReadOnly :=  false;
   ParameterList[$0f].Decimal := 255;

   //alarm 1 value
   ParameterList[$10].ParameterID := 'C';
   ParameterList[$10].FunctionAllowed :=  0;
   ParameterList[$10].ReadOnly :=  false;
   ParameterList[$10].Decimal := 255;

   //alarm 2 value
   ParameterList[$11].ParameterID := 'E';
   ParameterList[$11].FunctionAllowed :=  0;
   ParameterList[$11].ReadOnly :=  false;
   ParameterList[$11].Decimal := 255;

   //Rate (Derivative time constant)
   ParameterList[$12].ParameterID := 'D';
   ParameterList[$12].FunctionAllowed :=  0;
   ParameterList[$12].ReadOnly :=  false;
   ParameterList[$12].Decimal := 2;

   //Reset (Integral time constant)
   ParameterList[$13].ParameterID := 'I';
   ParameterList[$13].FunctionAllowed :=  0;
   ParameterList[$13].ReadOnly :=  false;
   ParameterList[$13].Decimal := 2;

   //Manual time reset (BIAS)
   ParameterList[$14].ParameterID := 'J';
   ParameterList[$14].FunctionAllowed :=  0;
   ParameterList[$14].ReadOnly :=  false;
   ParameterList[$14].Decimal := 255;

   //ON/OFF diferential
   ParameterList[$15].ParameterID := 'F';
   ParameterList[$15].FunctionAllowed :=  0;
   ParameterList[$15].ReadOnly :=  false;
   ParameterList[$15].Decimal := 1;

   //Overlap/Deadband
   ParameterList[$16].ParameterID := 'K';
   ParameterList[$16].FunctionAllowed :=  0;
   ParameterList[$16].ReadOnly :=  false;
   ParameterList[$16].Decimal := 0;

   //Proportional band 1 value
   ParameterList[$17].ParameterID := 'P';
   ParameterList[$17].FunctionAllowed :=  0;
   ParameterList[$17].ReadOnly :=  false;
   ParameterList[$17].Decimal := 1;

   //Proportional band 2 value
   ParameterList[$18].ParameterID := 'U';
   ParameterList[$18].FunctionAllowed :=  0;
   ParameterList[$18].ReadOnly :=  false;
   ParameterList[$18].Decimal := 1;

   //PV Offset
   ParameterList[$19].ParameterID := 'v';
   ParameterList[$19].FunctionAllowed :=  0 ; //todas as funcoes podem realizar operacoes com esse parametro
   ParameterList[$19].ReadOnly :=  false ;
   ParameterList[$19].Decimal := 255;

   //Arithmetic deviation
   ParameterList[$1a].ParameterID := 'V';
   ParameterList[$1a].FunctionAllowed :=  2;
   ParameterList[$1a].ReadOnly :=  true;
   ParameterList[$1a].Decimal := 255;

   //Arithmetic deviation
   ParameterList[$1b].ParameterID := 'Z';
   ParameterList[$1b].FunctionAllowed :=  3;
   ParameterList[$1b].ReadOnly :=  false;
   ParameterList[$1b].Decimal := 0;
end.
