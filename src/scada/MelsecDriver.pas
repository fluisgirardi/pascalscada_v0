{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa a base para os drivers de protocolo ModBus RTU e ModBus TCP.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implements the base of ModBus RTU and ModBus TCP protocol drivers.)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Moved OpenTagEditor to TagBuilderAssistant to remove form dependencies
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit MelsecDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString
  {$IFNDEF FPC}, Windows{$ENDIF};

type
  TMelsecPLC = record
    Station:LongInt;
    Inputs:TPLCMemoryManager;
    OutPuts:TPLCMemoryManager;
    Registers:TPLCMemoryManager;
    AnalogReg:TPLCMemoryManager;
    Status07Value:Double;
    Status07TimeStamp:TDateTime;
    Status07LastError:TProtocolIOResult;
  end;

  TMelsecDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;

  protected
    PFirstRequestLen,
    PFuncByteOffset,
    PCRCLen:LongInt;
    POutputMaxHole:Cardinal;
    PInputMaxHole:Cardinal;
    PRegistersMaxHole:Cardinal;
    PInternalDelayBetweenCmds:Cardinal;
    PMelsecPLC:array of TMelsecPLC;
    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure SetOutputMaxHole(v:Cardinal);
    procedure SetInputMaxHole(v:Cardinal);
    procedure SetRegisterMaxHole(v:Cardinal);
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; var tr:TTagRec);

    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; virtual;
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; virtual;
    function RemainingBytes(buffer:BYTES):LongInt; virtual;
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;

    property OutputMaxHole:Cardinal read POutputMaxHole write SetOutputMaxHole default 50;
    property InputMaxHole:Cardinal read PInputMaxHole write SetInputMaxHole default 50;
    property RegisterMaxHole:Cardinal read PRegistersMaxHole write SetRegisterMaxHole default 10;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    procedure OpenTagEditor(OwnerOfNewTags: TComponent;
       InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc); override;
    function HasTabBuilderEditor: Boolean; override;
  end;

  procedure SetTagBuilderToolForMelsecProtocolFamily(TagBuilderTool:TOpenTagEditor);


implementation

uses crossdatetime, pascalScadaMTPCPU;

{ TMelsecDriver }

procedure TMelsecDriver.BuildTagRec(plc, func, startaddress, size: Integer;
  var tr: TTagRec);
begin
  with tr do begin
    Station := plc;
    Rack:=0;
    Address := startaddress;
    ReadFunction := func;
    OffSet := 0;
    Slot := 0;
    File_DB := 0;
    SubElement := 0;
    WriteFunction := 0;
    Retries := 0;
    UpdateTime := 0;
  end;
  tr.Size := size;
end;

constructor TMelsecDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProtocolReady:=false;
  POutputMaxHole := 50;
  PInputMaxHole := 50;
  PRegistersMaxHole := 50;
  //PRegistersMaxHole := 0;
  PReadSomethingAlways := true;
  PInternalDelayBetweenCmds:=5;
  SetLength(PMelsecPLC,0);
end;

function TMelsecDriver.DecodePkg(pkg: TIOPacket;
  out values: TArrayOfDouble): TProtocolIOResult;
begin
  Result:=ioDriverError
end;

destructor TMelsecDriver.Destroy;
var
  plc:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PMelsecPLC) do begin
      PMelsecPLC[plc].Inputs.Destroy;
      PMelsecPLC[plc].OutPuts.Destroy;
      PMelsecPLC[plc].Registers.Destroy;
      PMelsecPLC[plc].AnalogReg.Destroy;
  end;
  SetLength(PMelsecPLC,0);
end;

procedure TMelsecDriver.DoAddTag(TagObj: TTag; TagValid: Boolean);
var
  station, mem, size, memtype, scantime:LongInt;
  found, valido:boolean;
  plc:LongInt;
begin
  //Recupera as informações do tag;
  //retrieve informations of the tag.
  station:=0;
  mem:=0;
  size:=0;
  memtype:=0;
  scantime:=0;
  valido:=false;

  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória.
    //check if the address of the slave is valid.
    if station in [1..255] then
    begin
      found := false;
      for plc:=0 to High(PMelsecPLC) do
        if PMelsecPLC[plc].Station = station then
        begin
          found := true;
          break;
        end;
      //se nao encontrou o plc, adiciona!
      //if not found the slave, add it.
      if not found then begin
        plc:=length(PMelsecPLC);
        SetLength(PMelsecPLC,plc+1);
        PMelsecPLC[plc].Station := station;
        PMelsecPLC[plc].Inputs := TPLCMemoryManager.Create();
        PMelsecPLC[plc].Inputs.MaxBlockItems := 10;
        //PMelsecPLC[plc].Inputs.MaxBlockItems := 2000;
        PMelsecPLC[plc].Inputs.MaxHole := PInputMaxHole;
        PMelsecPLC[plc].OutPuts := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts.MaxBlockItems := 10;
        //PMelsecPLC[plc].OutPuts.MaxBlockItems := 2000;
        PMelsecPLC[plc].OutPuts.MaxHole := POutputMaxHole;
        PMelsecPLC[plc].Registers := TPLCMemoryManager.Create();
        PMelsecPLC[plc].Registers.MaxBlockItems := 10;
        //PMelsecPLC[plc].Registers.MaxBlockItems := 125;
        PMelsecPLC[plc].Registers.MaxHole := PRegistersMaxHole;
        PMelsecPLC[plc].AnalogReg := TPLCMemoryManager.Create();
        //PMelsecPLC[plc].AnalogReg.MaxBlockItems := 125;
        PMelsecPLC[plc].AnalogReg.MaxBlockItems := 10;
        PMelsecPLC[plc].AnalogReg.MaxHole := PRegistersMaxHole;
      end;

      valido := (memtype in [1..4]);

      case memtype of
        1:
          PMelsecPLC[plc].OutPuts.AddAddress(mem,size,1,scantime);
        2:
          PMelsecPLC[plc].Inputs.AddAddress(mem,size,1,scantime);
        3:
          PMelsecPLC[plc].Registers.AddAddress(mem,size,1,scantime);
        4:
          PMelsecPLC[plc].AnalogReg.AddAddress(mem,size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TMelsecDriver.DoDelTag(TagObj: TTag);
var
  station, mem, size, memtype, scantime:LongInt;
  found:boolean;
  plc:LongInt;
begin
  //Recupera as informações do tag;
  //retrieve informations about the tag.
  station:=0;
  mem:=0;
  size:=0;
  memtype:=0;
  scantime:=0;
  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória.
    //check if the slave address is valid.
    if station in [1..255] then begin
      found := false;
      for plc:=0 to High(PMelsecPLC) do
        if PMelsecPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
      //if found the slave, removes the tag.
      if found then begin
        case memtype of
          1:
            PMelsecPLC[plc].OutPuts.RemoveAddress(mem,size,1);
          2:
            PMelsecPLC[plc].Inputs.RemoveAddress(mem,size,1);
          3:
            PMelsecPLC[plc].Registers.RemoveAddress(mem,size,1);
          4:
            PMelsecPLC[plc].AnalogReg.RemoveAddress(mem,size,1);
        end;
      end;
    end;
  inherited DoDelTag(TagObj);
end;

procedure TMelsecDriver.DoGetValue(TagObj: TTagRec; var values: TScanReadRec);
var
  plc,c:LongInt;
  found:Boolean;
begin
  if Length(values.Values)<TagObj.Size then
    SetLength(values.Values,TagObj.Size);

  for c:=0 to Length(values.Values)-1 do
    values.Values[c] := 0;

  found := false;
  for plc:=0 to High(PMelsecPLC) do
    if PMelsecPLC[plc].Station = TagObj.Station then begin
      found := true;
      break;
    end;

  if not found then begin
    values.ValuesTimestamp := CrossNow;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    values.LastQueryResult := ioDriverError;
    SetLength(values.Values,0);
    exit;
  end;

  case TagObj.ReadFunction of
    $01:
      PMelsecPLC[plc].OutPuts.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $02:
      PMelsecPLC[plc].Inputs.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $03:
      PMelsecPLC[plc].Registers.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $04:
      PMelsecPLC[plc].AnalogReg.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp)
  end;

  if values.LastQueryResult=ioOk then begin
    values.ReadsOK := 1;
    values.ReadFaults := 0;
  end else begin
    values.ReadsOK := 0;
    values.ReadFaults := 1;
  end;
end;

function TMelsecDriver.DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
  Sync: Boolean): TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  FRemainingBytes:LongInt;
  pkg:BYTES;
  rl:LongInt;
  res:LongInt;
  starts, ends:TNotifyEvent;
  qttags: integer;
begin
  try
    if FMustReleaseResources then begin
      starts:=HighLatencyOperationWillBegin;
      ends  :=HighLatencyOperationWasEnded;
    end else
    begin
      starts:=nil;
      ends  :=nil;
    end;

    pkg := EncodePkg(tagrec,nil,rl);
    if PCommPort<>nil then
    begin
      PCommPort.Lock(DriverID);
      res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1,starts,ends);

      //se o resultado de leitura deu ok, le o resto do pacote.
      //if the IO result is OK, reads the remaing packet...
      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then
      begin

        //retorna o numero de bytes que está aguardando ser lido no buffer da porta de comunicação.
        //calculates the remaining package length at the communication buffer.
        FRemainingBytes:=RemainingBytes(IOResult1.BufferToRead);

        if FRemainingBytes>0 then
        begin
          qttags := tagrec.Size;
          if qttags > 1 then
            FRemainingBytes := FRemainingBytes + (qttags {tagsize} * 2) - 1;
          res := PCommPort.IOCommandSync(iocRead,0,nil,FRemainingBytes,DriverID,0,@IOResult2,starts,ends);

          if res<>0 then
          begin
            IOResult1.BufferToRead:=ConcatenateBYTES(IOResult1.BufferToRead, IOResult2.BufferToRead);
            IOResult1.Received:=IOResult1.Received + IOResult2.Received;
            if IOResult2.ReadIOResult<>iorOK then
              IOResult1.ReadIOResult:=IOResult2.ReadIOResult;
          end
          else
            Result:=ioDriverError;
        end;
        Result := DecodePkg(IOResult1,values);
      end
      else
        Result:=ioEmptyPacket;

      PCommPort.Unlock(DriverID);
    end
    else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

procedure TMelsecDriver.DoScanRead(Sender: TObject; var NeedSleep: Integer);
var
  plc,block:LongInt;
  done,first:Boolean;
  minScan:Int64;
  lastType:LongInt;
  lastBlock:TRegisterRange;
  lastPLC:LongInt;
  tr:TTagRec;
  values:TArrayOfDouble;
begin
  try
    minScan := -1;
    first:=true;
    done := false;
    if ([csDestroying]*ComponentState<>[]) then begin
      CrossThreadSwitch;
      exit;
    end;
    for plc:= 0 to High(PMelsecPLC) do begin
      for block := 0 to High(PMelsecPLC[plc].Outputs.Blocks) do
        if PMelsecPLC[plc].Outputs.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,1,PMelsecPLC[plc].Outputs.Blocks[block].AddressStart,PMelsecPLC[plc].Outputs.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else begin
          if first then begin
            lastType := 1;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Outputs.Blocks[block];
            minScan := PMelsecPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 1;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Outputs.Blocks[block];
            minScan := PMelsecPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      for block := 0 to High(PMelsecPLC[plc].Inputs.Blocks) do
        if PMelsecPLC[plc].Inputs.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,2,PMelsecPLC[plc].Inputs.Blocks[block].AddressStart,PMelsecPLC[plc].Inputs.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else begin
          if first then begin
            lastType := 2;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Inputs.Blocks[block];
            minScan := PMelsecPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 2;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Inputs.Blocks[block];
            minScan := PMelsecPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

//      for block := 0 to High(PMelsecPLC[plc].Registers.Blocks) do
      for block := 0 to High(PMelsecPLC[plc].Registers.Blocks) do
        if PMelsecPLC[plc].Registers.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,3,PMelsecPLC[plc].Registers.Blocks[block].AddressStart,PMelsecPLC[plc].Registers.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else
        begin
          if first then begin
            lastType := 3;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers.Blocks[block];
            minScan := PMelsecPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 3;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers.Blocks[block];
            minScan := PMelsecPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      for block := 0 to High(PMelsecPLC[plc].AnalogReg.Blocks) do
        if PMelsecPLC[plc].AnalogReg.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,4,PMelsecPLC[plc].AnalogReg.Blocks[block].AddressStart,PMelsecPLC[plc].AnalogReg.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else begin
          if first then begin
            lastType := 4;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].AnalogReg.Blocks[block];
            minScan := PMelsecPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 4;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].AnalogReg.Blocks[block];
            minScan := PMelsecPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;
    end;
    //se nao fez leitura de nenhum bloco
    //faz atualiza o bloco que esta quase vencendo
    //o tempo de scan...
    //
    //if does nothing, update the tag with the oldest timestamp
    if (PReadSomethingAlways) and (Length(PMelsecPLC)>0) and ((not done) and (not first)) then begin
      //compila o bloco do mais necessitado;
      //build the tagrec record.
      BuildTagRec(lastPLC,lastType,lastBlock.AddressStart,lastBlock.Size, tr);
      FMustReleaseResources:=true;
      DoRead(tr,values,false);
      FMustReleaseResources:=false;
    end else
      NeedSleep := 1;
  finally
    FProtocolReady:=true;
    SetLength(values,0);
  end;
end;

function TMelsecDriver.DoWrite(const tagrec: TTagRec;
  const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  pkg:BYTES;
  FRemainingBytes:LongInt;
  rl:LongInt;
  res:LongInt;
  tempValues:TArrayOfDouble;
begin
  try
    pkg := EncodePkg(tagrec,values,rl);
    if PCommPort<>nil then begin
      PCommPort.Lock(DriverID);
      res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1);

      //se o resultado de leitura deu ok, le o resto do pacote.
      //if the IO result is OK, reads the remaing packet...
      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin

        //retorna o numero de bytes que está aguardando ser lido no buffer da porta de comunicação.
        //calculates the remaining package length at the communication buffer.
        FRemainingBytes:=RemainingBytes(IOResult1.BufferToRead);

        //clear the remaining buffer...
(*        if (IOResult1.BufferToRead[PFuncByteOffset-1]<>pkg[PFuncByteOffset-1]) or
           ((IOResult1.BufferToRead[PFuncByteOffset]<>pkg[PFuncByteOffset]) and
            (not (IOResult1.BufferToRead[PFuncByteOffset] in [$81..$88])))then begin
           repeat
             res := PCommPort.IOCommandSync(iocRead,0,nil,1,DriverID,0,@IOResult2);
           until IOResult2.ReadIOResult=iorTimeOut;
           Result:=ioCommError;
           exit;
        end; *)

        if FRemainingBytes>0 then begin
          res := PCommPort.IOCommandSync(iocRead,0,nil,FRemainingBytes,DriverID,0,@IOResult2);

          if res<>0 then begin
            IOResult1.BufferToRead:=ConcatenateBYTES(IOResult1.BufferToRead, IOResult2.BufferToRead);
            IOResult1.Received:=IOResult1.Received + IOResult2.Received;
            if IOResult2.ReadIOResult<>iorOK then
              IOResult1.ReadIOResult:=IOResult2.ReadIOResult;
          end else
            Result:=ioDriverError;
        end;
        Result := DecodePkg(IOResult1,tempValues);
      end else
        Result:=ioEmptyPacket;

      PCommPort.Unlock(DriverID);
    end else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(tempValues,0);
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

function TMelsecDriver.EncodePkg(TagObj: TTagRec; ToWrite: TArrayOfDouble;
  var ResultLen: Integer): BYTES;
begin
  Result:=nil;
end;

function TMelsecDriver.GetTagProperts(TagObj: TTag; var Station, Address, Size,
  RegType, ScanTime: Integer): Boolean;
var
  found:Boolean;
begin
  found := false;
  Result := false;
  //PLCTagNumber
  if (not found) and (TagObj is TPLCTagNumber) then begin
    found := true;
    Station := TPLCTagNumber(TagObj).PLCStation;
    Address := TPLCTagNumber(TagObj).MemAddress;
    Size    := TPLCTagNumber(TagObj).TagSizeOnProtocol;
    RegType := TPLCTagNumber(TagObj).MemReadFunction;
    ScanTime:= TPLCTagNumber(TagObj).RefreshTime;
    Result  := found;
  end;

  //TPLCBlock and TPLCStruct
  if (not found) and (TagObj is TPLCBlock) then begin
    found   := true;
    Station := TPLCBlock(TagObj).PLCStation;
    Address := TPLCBlock(TagObj).MemAddress;
    Size    := TPLCBlock(TagObj).TagSizeOnProtocol;
    RegType := TPLCBlock(TagObj).MemReadFunction;
    ScanTime:= TPLCBlock(TagObj).RefreshTime;
    Result  := found;
  end;

  //TPLCString
  if (not found) and (TagObj is TPLCString) then begin
    found   := true;
    Station := TPLCString(TagObj).PLCStation;
    Address := TPLCString(TagObj).MemAddress;
    Size    := TPLCString(TagObj).Size;
    RegType := TPLCString(TagObj).MemReadFunction;
    ScanTime:= TPLCString(TagObj).RefreshTime;
    Result  := found;
  end;
end;

function TMelsecDriver.HasTabBuilderEditor: Boolean;
begin
  Result:=true
end;

var
  MelsecTagBuilderEditor:TOpenTagEditor = nil;


procedure TMelsecDriver.OpenTagEditor(OwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
begin
  if Assigned(MelsecTagBuilderEditor) then
    MelsecTagBuilderEditor(Self,OwnerOfNewTags,InsertHook,CreateProc)
  else
    inherited;
end;

function TMelsecDriver.RemainingBytes(buffer: BYTES): LongInt;
begin
  Result:=0;
end;

procedure TMelsecDriver.SetInputMaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = PInputMaxHole then exit;

  PInputMaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].Inputs.MaxHole := v;
end;

procedure TMelsecDriver.SetOutputMaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutputMaxHole then exit;

  POutputMaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts.MaxHole := v;
end;

procedure TMelsecDriver.SetRegisterMaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = PRegistersMaxHole then exit;

  PRegistersMaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].Registers.MaxHole := v;
end;

function TMelsecDriver.SizeOfTag(Tag: TTag; isWrite: Boolean;
  var ProtocolTagType: TProtocolTagType): BYTE;
var
  FunctionCode:Cardinal;
begin
  FunctionCode := 0;
  if (Tag is TPLCTagNumber) then begin
    if (isWrite) then
      FunctionCode := TPLCTagNumber(Tag).MemWriteFunction
    else
      FunctionCode := TPLCTagNumber(Tag).MemReadFunction;
  end;

  //TPLCBlock and TPLCStruct
  if (Tag is TPLCBlock) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(Tag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(Tag).MemReadFunction;
  end;

  //TPLCString
  if (Tag is TPLCString) then begin
    if (isWrite) then
      FunctionCode := TPLCString(Tag).MemWriteFunction
    else
      FunctionCode := TPLCString(Tag).MemReadFunction;
  end;


  //retorna o tamanho em bits dos registradores lidos/escritos por
  //cada tipo de função de leitura/escrita
  //
  //return the size in bits of the tag
  case FunctionCode of
    1,2,5,15: begin
      Result := 1;
      ProtocolTagType:=ptBit;
    end;
    3,4,6,16: begin
      Result := 16;
      ProtocolTagType:=ptWord;
    end
    else
      Result := 16;
  end;
end;

procedure SetTagBuilderToolForMelsecProtocolFamily(TagBuilderTool:TOpenTagEditor);
begin
  if assigned(MelsecTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for Modbus RTU/TCP protocol family was already assigned.')
  else
    MelsecTagBuilderEditor:=TagBuilderTool;
end;


end.
