{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Implementa a base para os drivers de protocolo MC PROTOCOL.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implements the base of MC PROTOCOL drivers.)
}

(*
As variáveis possíveis de serem utilizadas no CLP mitsubishi com protocolo "MC Protocol" são as seguintes:
Memórias outputs: M, SM, L, F, V, X, Y, B
Memórias registros: D, SD
Tabela para setar as propriedades MemReadFunction, MemWriteFunction e MemAddress

The possible variables to be used in the PLC protocol by Mitsubishi " MC Protocol" are the following
Memories outputs: M, SM, L, F, V, X, Y, B
Memories records: D, SD
Table to set MemReadFunction, MemWriteFunction and MemAddress


Variavel/variables | MemReadFunction | MemWriteFunction | MemAddress
M                  |      1          |        1         | valor/value
SM                 |      2          |        2         | valor/value
L                  |      3          |        3         | valor/value
F                  |      4          |        4         | valor/value
V                  |      5          |        5         | valor/value
X                  |      6          |        6         | valor/value
Y                  |      7          |        7         | valor/value
B                  |      8          |        8         | valor/value
D                  |      9          |        9         | valor/value
SD                 |      16         |        16        | valor/value

*)

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
    Station: LongInt;

    OutPuts_M: TPLCMemoryManager;
    OutPuts_SM: TPLCMemoryManager;
    OutPuts_L: TPLCMemoryManager;
    OutPuts_F: TPLCMemoryManager;
    OutPuts_V: TPLCMemoryManager;
    OutPuts_X: TPLCMemoryManager;
    OutPuts_Y: TPLCMemoryManager;
    OutPuts_B: TPLCMemoryManager;

    Registers_D :TPLCMemoryManager;
    Registers_SD :TPLCMemoryManager;

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

    POutput_M_MaxHole:Cardinal;
    POutput_SM_MaxHole: Cardinal;
    POutput_L_MaxHole: Cardinal;
    POutput_F_MaxHole: Cardinal;
    POutput_V_MaxHole: Cardinal;
    POutput_X_MaxHole: Cardinal;
    POutput_Y_MaxHole: Cardinal;
    POutput_B_MaxHole: Cardinal;

    PRegisters_D_MaxHole:Cardinal;
    PRegisters_SD_MaxHole:Cardinal;

    PInternalDelayBetweenCmds:Cardinal;
    PMelsecPLC:array of TMelsecPLC;
    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;

    procedure SetOutput_M_MaxHole(v:Cardinal);
    procedure SetOutput_SM_MaxHole(v:Cardinal);
    procedure SetOutput_L_MaxHole(v:Cardinal);
    procedure SetOutput_F_MaxHole(v:Cardinal);
    procedure SetOutput_V_MaxHole(v:Cardinal);
    procedure SetOutput_X_MaxHole(v:Cardinal);
    procedure SetOutput_Y_MaxHole(v:Cardinal);
    procedure SetOutput_B_MaxHole(v:Cardinal);

    procedure SetRegister_D_MaxHole(v:Cardinal);
    procedure SetRegister_SD_MaxHole(v:Cardinal);


    procedure BuildTagRec(plc,func,startaddress,size:LongInt; var tr:TTagRec);
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; virtual;
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; virtual;
    function RemainingBytesWrite(buffer:BYTES):LongInt; virtual;
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt; virtual;
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;

    function PlcDeviceType(memReadFunction: integer): integer; virtual;

    property Output_M_MaxHole:Cardinal read POutput_M_MaxHole write SetOutput_M_MaxHole default 10;
    property Output_SM_MaxHole:Cardinal read POutput_SM_MaxHole write SetOutput_SM_MaxHole default 10;
    property Output_L_MaxHole:Cardinal read POutput_L_MaxHole write SetOutput_L_MaxHole default 10;
    property Output_F_MaxHole:Cardinal read POutput_F_MaxHole write SetOutput_F_MaxHole default 10;
    property Output_V_MaxHole:Cardinal read POutput_V_MaxHole write SetOutput_V_MaxHole default 10;
    property Output_X_MaxHole:Cardinal read POutput_X_MaxHole write SetOutput_X_MaxHole default 10;
    property Output_Y_MaxHole:Cardinal read POutput_Y_MaxHole write SetOutput_Y_MaxHole default 10;
    property Output_B_MaxHole:Cardinal read POutput_B_MaxHole write SetOutput_B_MaxHole default 10;

    property Register_D_MaxHole:Cardinal read PRegisters_D_MaxHole write SetRegister_D_MaxHole default 10;
    property Register_SD_MaxHole:Cardinal read PRegisters_SD_MaxHole write SetRegister_SD_MaxHole default 10;

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

  POutput_M_MaxHole := 10;
  POutput_SM_MaxHole := 10;
  POutput_L_MaxHole:= 10;
  POutput_F_MaxHole:= 10;
  POutput_V_MaxHole:= 10;
  POutput_X_MaxHole:= 10;
  POutput_Y_MaxHole:= 10;
  POutput_B_MaxHole:= 10;

  PRegisters_D_MaxHole:= 10;
  PRegisters_SD_MaxHole:= 10;

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
  for plc:=0 to High(PMelsecPLC) do
  begin
    PMelsecPLC[plc].OutPuts_M.Destroy;
    PMelsecPLC[plc].OutPuts_SM.Destroy;
    PMelsecPLC[plc].OutPuts_L.Destroy;
    PMelsecPLC[plc].OutPuts_F.Destroy;
    PMelsecPLC[plc].OutPuts_V.Destroy;
    PMelsecPLC[plc].OutPuts_X.Destroy;
    PMelsecPLC[plc].OutPuts_Y.Destroy;
    PMelsecPLC[plc].OutPuts_B.Destroy;
    PMelsecPLC[plc].Registers_D.Destroy;
    PMelsecPLC[plc].Registers_SD.Destroy;
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
      if not found then
      begin
        plc:=length(PMelsecPLC);
        SetLength(PMelsecPLC,plc+1);
        PMelsecPLC[plc].Station := station;

        PMelsecPLC[plc].OutPuts_M := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_M.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_M.MaxHole := POutput_M_MaxHole;

        PMelsecPLC[plc].OutPuts_SM := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_SM.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_SM.MaxHole := POutput_SM_MaxHole;

        PMelsecPLC[plc].OutPuts_L := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_L.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_L.MaxHole := POutput_L_MaxHole;

        PMelsecPLC[plc].OutPuts_F := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_F.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_F.MaxHole := POutput_F_MaxHole;

        PMelsecPLC[plc].OutPuts_V := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_V.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_V.MaxHole := POutput_V_MaxHole;

        PMelsecPLC[plc].OutPuts_X := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_X.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_X.MaxHole := POutput_X_MaxHole;

        PMelsecPLC[plc].OutPuts_Y := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_Y.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_Y.MaxHole := POutput_Y_MaxHole;

        PMelsecPLC[plc].OutPuts_B := TPLCMemoryManager.Create();
        PMelsecPLC[plc].OutPuts_B.MaxBlockItems := 10;
        PMelsecPLC[plc].OutPuts_B.MaxHole := POutput_B_MaxHole;

        PMelsecPLC[plc].Registers_D := TPLCMemoryManager.Create();
        PMelsecPLC[plc].Registers_D.MaxBlockItems := 10;
        PMelsecPLC[plc].Registers_D.MaxHole := PRegisters_D_MaxHole;

        PMelsecPLC[plc].Registers_SD := TPLCMemoryManager.Create();
        PMelsecPLC[plc].Registers_SD.MaxBlockItems := 10;
        PMelsecPLC[plc].Registers_SD.MaxHole := PRegisters_SD_MaxHole;
      end;

      valido := (memtype in [1..16]);

      case memtype of
        1:
          PMelsecPLC[plc].OutPuts_M.AddAddress(mem,size,1,scantime);
        2:
          PMelsecPLC[plc].OutPuts_SM.AddAddress(mem,size,1,scantime);
        3:
          PMelsecPLC[plc].OutPuts_L.AddAddress(mem,size,1,scantime);
        4:
          PMelsecPLC[plc].OutPuts_F.AddAddress(mem,size,1,scantime);
        5:
          PMelsecPLC[plc].OutPuts_V.AddAddress(mem,size,1,scantime);
        6:
          PMelsecPLC[plc].OutPuts_X.AddAddress(mem,size,1,scantime);
        7:
          PMelsecPLC[plc].OutPuts_Y.AddAddress(mem,size,1,scantime);
        8:
          PMelsecPLC[plc].OutPuts_B.AddAddress(mem,size,1,scantime);
        9:
          PMelsecPLC[plc].Registers_D.AddAddress(mem,size,1,scantime);
        16:
          PMelsecPLC[plc].Registers_SD.AddAddress(mem,size,1,scantime);
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
            PMelsecPLC[plc].OutPuts_M.RemoveAddress(mem,size,1);
          2:
            PMelsecPLC[plc].OutPuts_SM.RemoveAddress(mem,size,1);
          3:
            PMelsecPLC[plc].OutPuts_L.RemoveAddress(mem,size,1);
          4:
            PMelsecPLC[plc].OutPuts_F.RemoveAddress(mem,size,1);
          5:
            PMelsecPLC[plc].OutPuts_V.RemoveAddress(mem,size,1);
          6:
            PMelsecPLC[plc].OutPuts_X.RemoveAddress(mem,size,1);
          7:
            PMelsecPLC[plc].OutPuts_Y.RemoveAddress(mem,size,1);
          8:
            PMelsecPLC[plc].OutPuts_B.RemoveAddress(mem,size,1);
          9:
            PMelsecPLC[plc].Registers_D.RemoveAddress(mem,size,1);
          16:
            PMelsecPLC[plc].Registers_SD.RemoveAddress(mem,size,1);
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
      PMelsecPLC[plc].OutPuts_M.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $02:
      PMelsecPLC[plc].OutPuts_SM.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $03:
      PMelsecPLC[plc].OutPuts_L.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $04:
      PMelsecPLC[plc].OutPuts_F.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $05:
      PMelsecPLC[plc].OutPuts_V.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $06:
      PMelsecPLC[plc].OutPuts_X.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $07:
      PMelsecPLC[plc].OutPuts_Y.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $08:
      PMelsecPLC[plc].OutPuts_B.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $09:
      PMelsecPLC[plc].Registers_D.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $10:
      PMelsecPLC[plc].Registers_SD.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
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
        FRemainingBytes := RemainingBytesRead(IOResult1.BufferToRead, tagrec);

        if FRemainingBytes>0 then
        begin
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
    if ([csDestroying]*ComponentState<>[]) then
    begin
      CrossThreadSwitch;
      exit;
    end;

    for plc:= 0 to High(PMelsecPLC) do
    begin

      //memory "M"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_M.Blocks) do
        if PMelsecPLC[plc].OutPuts_M.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,1,PMelsecPLC[plc].OutPuts_M.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_M.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 1;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_M.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_M.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_M.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 1;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_M.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_M.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "SM"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_SM.Blocks) do
        if PMelsecPLC[plc].OutPuts_SM.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,2,PMelsecPLC[plc].OutPuts_SM.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_SM.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 2;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_SM.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_SM.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_SM.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 2;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_SM.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_SM.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "L"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_L.Blocks) do
        if PMelsecPLC[plc].OutPuts_L.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,3,PMelsecPLC[plc].OutPuts_L.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_L.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 3;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_L.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_L.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_L.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 3;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_L.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_L.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "F"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_F.Blocks) do
        if PMelsecPLC[plc].OutPuts_F.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,4,PMelsecPLC[plc].OutPuts_F.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_F.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 4;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_F.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_F.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_F.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 4;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_F.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_F.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "V"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_V.Blocks) do
        if PMelsecPLC[plc].OutPuts_V.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,5,PMelsecPLC[plc].OutPuts_V.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_V.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 5;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_V.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_V.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_V.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 5;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_V.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_V.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "X"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_X.Blocks) do
        if PMelsecPLC[plc].OutPuts_X.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,6,PMelsecPLC[plc].OutPuts_X.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_X.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 6;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_X.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_X.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_X.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 6;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_X.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_X.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "Y"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_Y.Blocks) do
        if PMelsecPLC[plc].OutPuts_Y.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,7,PMelsecPLC[plc].OutPuts_Y.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_Y.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 7;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_Y.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_Y.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_Y.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 7;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_Y.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_Y.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "B"
      for block := 0 to High(PMelsecPLC[plc].OutPuts_B.Blocks) do
        if PMelsecPLC[plc].OutPuts_B.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,8,PMelsecPLC[plc].OutPuts_B.Blocks[block].AddressStart,PMelsecPLC[plc].OutPuts_B.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end
        else
        begin
          if first then
          begin
            lastType := 8;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_B.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_B.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].OutPuts_B.Blocks[block].MilisecondsFromLastUpdate>minScan then
          begin
            lastType := 8;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].OutPuts_B.Blocks[block];
            minScan := PMelsecPLC[plc].OutPuts_B.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;


      //memory "D"
      for block := 0 to High(PMelsecPLC[plc].Registers_D.Blocks) do
        if PMelsecPLC[plc].Registers_D.Blocks[block].NeedRefresh then
        begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,9,PMelsecPLC[plc].Registers_D.Blocks[block].AddressStart,PMelsecPLC[plc].Registers_D.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else
        begin
          if first then begin
            lastType := 9;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers_D.Blocks[block];
            minScan := PMelsecPLC[plc].Registers_D.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].Registers_D.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 9;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers_D.Blocks[block];
            minScan := PMelsecPLC[plc].Registers_D.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      //memory "SD"
      for block := 0 to High(PMelsecPLC[plc].Registers_SD.Blocks) do
        if PMelsecPLC[plc].Registers_SD.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PMelsecPLC[plc].Station,16,PMelsecPLC[plc].Registers_SD.Blocks[block].AddressStart,PMelsecPLC[plc].Registers_SD.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
        end else begin
          if first then begin
            lastType := 16;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers_SD.Blocks[block];
            minScan := PMelsecPLC[plc].Registers_SD.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PMelsecPLC[plc].Registers_SD.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 16;
            lastPLC := PMelsecPLC[plc].Station;
            lastBlock := PMelsecPLC[plc].Registers_SD.Blocks[block];
            minScan := PMelsecPLC[plc].Registers_SD.Blocks[block].MilisecondsFromLastUpdate;
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
        FRemainingBytes:=RemainingBytesWrite(IOResult1.BufferToRead);

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

function TMelsecDriver.PlcDeviceType(memReadFunction: integer): integer;
begin
  Result := 0;
end;

function TMelsecDriver.RemainingBytesWrite(buffer: BYTES): LongInt;
begin
  Result:=0;
end;

function TMelsecDriver.RemainingBytesRead(buffer: BYTES;
  TagObj: TTagRec): LongInt;
begin
  Result:=0;
end;

procedure TMelsecDriver.SetOutput_M_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_M_MaxHole then exit;

  POutput_M_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_M.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_SM_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_SM_MaxHole then exit;

  POutput_SM_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_SM.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_L_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_L_MaxHole then exit;

  POutput_L_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_L.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_F_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_F_MaxHole then exit;

  POutput_F_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_F.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_V_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_V_MaxHole then exit;

  POutput_V_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_V.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_X_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_X_MaxHole then exit;

  POutput_X_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_X.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_Y_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_Y_MaxHole then exit;

  POutput_Y_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_Y.MaxHole := v;
end;

procedure TMelsecDriver.SetOutput_B_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = POutput_B_MaxHole then exit;

  POutput_B_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].OutPuts_B.MaxHole := v;
end;

procedure TMelsecDriver.SetRegister_D_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = PRegisters_D_MaxHole then exit;

  PRegisters_D_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].Registers_D.MaxHole := v;
end;

procedure TMelsecDriver.SetRegister_SD_MaxHole(v: Cardinal);
var
  plc:LongInt;
begin
  if v = PRegisters_SD_MaxHole then exit;

  PRegisters_SD_MaxHole:=v;

  for plc:=0 to High(PMelsecPLC) do
    PMelsecPLC[plc].Registers_SD.MaxHole := v;
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
    1,2,3,4,5,6,7,8: begin
      Result := 1;
      ProtocolTagType:=ptBit;
    end;
    9,16: begin
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
