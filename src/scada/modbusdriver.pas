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
unit ModBusDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString, fgl, modbus_tagscan_req
  {$IFNDEF FPC}, Windows{$ENDIF};

type

  TReqList = specialize TFPGList<TReqItem>;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Estrutura que simula as áreas de memória de um equipamento modbus escravo.

  @member Station Armazena o endereço do equipamento modbus.
  @member Inputs Gerenciador de blocos de memórias não continuas que mapeia
          as entradas digitais do equipamento modbus.
  @member Outputs Gerenciador de blocos de memórias não continuas que mapeia
          as saidas digitais do equipamento modbus.
  @member Registers Gerenciador de blocos de memórias não continuas que mapeia
          as registradores do equipamento modbus.
  @member AnalogReg Gerenciador de blocos de memórias não continuas que mapeia
          as entradas analógicas do equipamento modbus.
  @member Status07Value Guarda o valor retornado pela função 07 do ModBus.
  @member Status07TimeStamp Guarda a data/hora do status retornado pela função
          07 do ModBus.
  @member Status07LastError Guarda o status do driver ao executar a função
          07 do ModBus.
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Record that represents a modbus slave device.

  @member Station Address of the modbus device.
  @member Inputs Memory mananger object that handles the digital inputs of
          your modbus device.
  @member Outputs Memory mananger object that handles the digital outputs of
          your modbus device (coils).
  @member Registers Memory mananger object that handles the registers of
          your modbus device.
  @member AnalogReg Memory mananger object that handles the analog registers of
          your modbus device.
  @member Status07Value Stores the value returned by the ModBus function 07.
  @member Status07TimeStamp Stores the date/time of the last action of ModBus
          function 07.
  @member Status07LastError Stores the IO result of the last ModBus function 07.
  }
  {$ENDIF}
  TModBusPLC = record
    Station:LongInt;
    Inputs:TPLCMemoryManager;
    OutPuts:TPLCMemoryManager;
    Registers:TPLCMemoryManager;
    AnalogReg:TPLCMemoryManager;
    Status07Value:Double;
    Status07TimeStamp:TDateTime;
    Status07LastError:TProtocolIOResult;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base do driver ModBus (RTU e TCP))
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Para configurar um tag para usar o ModBus, é necessário configurar as
  seguintes propriedades do tag:

  @unorderedList(
    @item(@bold(PLCStation): Endereço do equipamento modbus.)
    @item(@bold(MemAddress): Endereço da entrada/saida/registrador que se deseja
          lêr/escrever. Os endereços começam de zero)
    @item(@bold(MemReadFuntion): Função que será usada para ler o tag. Veja
          tabela abaixo.)
    @item(@bold(MemWriteFuntion): Função que será usada para escrever valores do
          tag. Veja tabela abaixo.)
  )

  Para as propriedades MemReadFunction e MemWriteFunction são aceitos os seguintes
  valores de acordo com a área de memória desejada:

  @table(
    @rowHead( @cell(Área desejada)       @cell(MemReadFunction) @cell(MemWriteFunction) )
    @row(     @cell(Entradas digitais)   @cell(2)               @cell(0) )
    @row(     @cell(Saidas digitais)     @cell(1)               @cell(5 (simples), 15 (bloco)) )
    @row(     @cell(Registradores)       @cell(3)               @cell(6 (simples), 16 (bloco)) )
    @row(     @cell(Entradas analógicas) @cell(4)               @cell(0) )
    @row(     @cell(Status equipamento)  @cell(7)               @cell(0) )
  )

  É necessário que você conheça as funções ModBus que seu equipamento suporta.)

  @seealso(TModBusRTUDriver)
  @seealso(TModBusTCPDriver)
  }
  {$ELSE}
  {:
  @abstract(Base class of ModBus protocol driver (RTU e TCP))
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  To configure your tag, you must set the following properties of your tag:

  @unorderedList(
    @item(@bold(PLCStation): Address of your modbus slave.)
    @item(@bold(MemAddress): Address of the digital input or output, register or
          analog register.)
    @item(@bold(MemReadFuntion): Modbus function that will be used to read the
          tag. See the table below.)
    @item(@bold(MemWriteFuntion): Modbus function that will be used to write the
          values of your tag on device. See the table below.)
  )

  The properties MemReadFunction and MemWriteFunction accept the following values,
  depending of the area (digital input, output, register) desejado:

  @table(
    @rowHead( @cell(Desired area)    @cell(MemReadFunction) @cell(MemWriteFunction) )
    @row(     @cell(Digital inputs)  @cell(2)               @cell(0) )
    @row(     @cell(Digital outputs) @cell(1)               @cell(5 (simple), 15 (block)) )
    @row(     @cell(Registers)       @cell(3)               @cell(6 (simple), 16 (block)) )
    @row(     @cell(Analog Inputs)   @cell(4)               @cell(0) )
    @row(     @cell(Device Status)   @cell(7)               @cell(0) )
  )

  You must know what's the supported functions of your modbus slave.

  @seealso(TModBusRTUDriver)
  @seealso(TModBusTCPDriver)
  }
  {$ENDIF}

  { TModBusDriver }

  TModBusDriver = class(TProtocolDriver)
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
    PModbusPLC:array of TModBusPLC;
    function  AllowBroadCast:Boolean; virtual;
    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure SetOutputMaxHole(v:Cardinal);
    procedure SetInputMaxHole(v:Cardinal);
    procedure SetRegisterMaxHole(v:Cardinal);
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; out tr:TTagRec);

    {$IFDEF PORTUGUES}
    //: Cria um pacote modbus
    {$ELSE}
    //: Encode a modbus packet.
    {$ENDIF}
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; virtual;

    {$IFDEF PORTUGUES}
    //: Extrai os dados de um pacote modbus
    {$ELSE}
    //: Decodes a modbus packet.
    {$ENDIF}
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; virtual;

    {$IFDEF PORTUGUES}
    //: Retorna os bytes que restaram no buffer RX da porta de comunicação.
    {$ELSE}
    //: Returns the remaing bytes on RX buffer of communication port.
    {$ENDIF}
    function RemainingBytes(buffer:BYTES):LongInt; virtual;

    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;

    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;

    //: @seealso(TProtocolDriver.DoWrite)
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;

    {$IFDEF PORTUGUES}
    {:
    Informa quantas saidas podem ficar sem serem declaradas para manter um bloco
    de saidas continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ELSE}
    {:
    How many digital outputs can be undeclared to keep the block continuous.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ENDIF}
    property OutputMaxHole:Cardinal read POutputMaxHole write SetOutputMaxHole default 50;

    {$IFDEF PORTUGUES}
    {:
    Informa quantas entradas podem ficar sem serem declaradas para manter um bloco
    continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ELSE}
    {:
    How many digital inputs can be undeclared to keep the block continuous.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ENDIF}
    property InputMaxHole:Cardinal read PInputMaxHole write SetInputMaxHole default 50;

    {$IFDEF PORTUGUES}
    {:
    Informa quantos registradores podem ficar sem serem declaradas para manter
    um bloco continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ELSE}
    {:
    How many registers can be undeclared to keep the block continuous.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ENDIF}
    property RegisterMaxHole:Cardinal read PRegistersMaxHole write SetRegisterMaxHole default 10;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: @seealso(TProtocolDriver.SizeOfTag)
    function  SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;

    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(InsertHook: TAddTagInEditorHook;
      CreateProc: TCreateTagProc); override;

    //: @seealso(TProtocolDriver.HasTabBuilderEditor)
    function HasTabBuilderEditor: Boolean; override;
  end;

  procedure SetTagBuilderToolForModBusProtocolFamily(TagBuilderTool:TOpenTagEditor);

implementation

uses crossdatetime, pascalScadaMTPCPU, math, dateutils;

function SortGenericTagList(const Item1, Item2: TReqItem): Integer;
var
  BitCombination:Integer;
  ScanPercent1, ScanPercent2:Double;
begin

  BitCombination:=ifthen(Item1.NeedUpdate,1,0)+ifthen(Item2.NeedUpdate,2,0);
  case BitCombination of
    1:
      Result:=-1;
    2:
      Result:= 1;
    0,3: begin
      ScanPercent1:=0;
      if Item1.UpdateRate<>0 then ScanPercent1:=(MilliSecondsBetween(Now,Item1.LastUpdate)/Item1.UpdateRate);

      ScanPercent2:=0;
      if Item2.UpdateRate<>0 then ScanPercent2:=(MilliSecondsBetween(Now,Item2.LastUpdate)/Item2.UpdateRate);


      if ScanPercent1=ScanPercent2 then
        Result:=0
      else begin
        if ScanPercent1>ScanPercent2 then
          Result:=-1
        else
          Result:=1;
      end;
    end;
  end;
end;

constructor TModBusDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FProtocolReady:=false;
  POutputMaxHole := 50;
  PInputMaxHole := 50;
  PRegistersMaxHole := 10;
  PReadSomethingAlways := true;
  PInternalDelayBetweenCmds:=5;
  SetLength(PModbusPLC,0);
end;

destructor TModBusDriver.Destroy;
var
  plc:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PModbusPLC) do begin
      PModbusPLC[plc].Inputs.Destroy;
      PModbusPLC[plc].OutPuts.Destroy;
      PModbusPLC[plc].Registers.Destroy;
      PModbusPLC[plc].AnalogReg.Destroy;
  end;
  SetLength(PModbusPLC,0);
end;

function TModBusDriver.AllowBroadCast: Boolean;
begin
  Result:=false;
end;

function TModBusDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
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

procedure TModBusDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
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
    if station in [1..255] then begin
      found := false;
      for plc:=0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //se nao encontrou o plc, adiciona!
      //if not found the slave, add it.
      if not found then begin
        plc:=length(PModbusPLC);
        SetLength(PModbusPLC,plc+1);
        PModbusPLC[plc].Station := station;
        PModbusPLC[plc].Inputs := TPLCMemoryManager.Create();
        PModbusPLC[plc].Inputs.MaxBlockItems := 2000;
        PModbusPLC[plc].Inputs.MaxHole := PInputMaxHole;
        PModbusPLC[plc].OutPuts := TPLCMemoryManager.Create();
        PModbusPLC[plc].OutPuts.MaxBlockItems := 2000;
        PModbusPLC[plc].OutPuts.MaxHole := POutputMaxHole;
        PModbusPLC[plc].Registers := TPLCMemoryManager.Create();
        PModbusPLC[plc].Registers.MaxBlockItems := 125;
        PModbusPLC[plc].Registers.MaxHole := PRegistersMaxHole;
        PModbusPLC[plc].AnalogReg := TPLCMemoryManager.Create();
        PModbusPLC[plc].AnalogReg.MaxBlockItems := 125;
        PModbusPLC[plc].AnalogReg.MaxHole := PRegistersMaxHole;
      end;

      valido := (memtype in [1..4]);

      case memtype of
        1:
          PModbusPLC[plc].OutPuts.AddAddress(mem,size,1,scantime);
        2:
          PModbusPLC[plc].Inputs.AddAddress(mem,size,1,scantime);
        3:
          PModbusPLC[plc].Registers.AddAddress(mem,size,1,scantime);
        4:
          PModbusPLC[plc].AnalogReg.AddAddress(mem,size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TModBusDriver.DoDelTag(TagObj:TTag);
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
      for plc:=0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
      //if found the slave, removes the tag.
      if found then begin
        case memtype of
          1:
            PModbusPLC[plc].OutPuts.RemoveAddress(mem,size,1);
          2:
            PModbusPLC[plc].Inputs.RemoveAddress(mem,size,1);
          3:
            PModbusPLC[plc].Registers.RemoveAddress(mem,size,1);
          4:
            PModbusPLC[plc].AnalogReg.RemoveAddress(mem,size,1);
        end;
      end;
    end;
  inherited DoDelTag(TagObj);
end;



function  TModBusDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
var
  FunctionCode:Cardinal;
begin
  FunctionCode := 0;
  if (aTag is TPLCTagNumber) then begin
    if (isWrite) then
      FunctionCode := TPLCTagNumber(aTag).MemWriteFunction
    else
      FunctionCode := TPLCTagNumber(aTag).MemReadFunction;
  end;

  //TPLCBlock and TPLCStruct
  if (aTag is TPLCBlock) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(aTag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(aTag).MemReadFunction;
  end;

  //TPLCString
  if (aTag is TPLCString) then begin
    if (isWrite) then
      FunctionCode := TPLCString(aTag).MemWriteFunction
    else
      FunctionCode := TPLCString(aTag).MemReadFunction;
  end;


  //retorna o tamanho em bits dos registradores lidos/escritos por
  //cada tipo de função de leitura/escrita
  //
  //return the size in bits of the atag
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

function  TModBusDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
begin
  Result:=nil;
end;

function TModBusDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=ioDriverError
end;

function  TModBusDriver.RemainingBytes(buffer:BYTES):LongInt;
begin
  Result:=0;
end;

procedure TModBusDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var
  plc,block:LongInt;
  tr:TTagRec;
  values:TArrayOfDouble;
  EntireTagList:TReqList;
  c: Integer;

  procedure AddToTagList(station,func,startaddress,size, UpdateRate:LongInt; LastUpdate:TDateTime; NeedUpdate:Boolean);
  var
    info:TReqItem;
  begin
    info.station      :=station;
    info.func         :=func;
    info.startaddress :=startaddress;
    info.size         :=size;
    info.LastUpdate   :=LastUpdate;
    info.UpdateRate   :=UpdateRate;
    info.NeedUpdate   :=NeedUpdate;
    info.Read         :=false;

    EntireTagList.add(info);
  end;
begin
  try
    if ([csDestroying]*ComponentState<>[]) then begin
      CrossThreadSwitch;
      exit;
    end;

    EntireTagList:=TReqList.Create;

    for plc:= 0 to High(PModbusPLC) do begin
      for block := 0 to High(PModBusPLC[plc].Outputs.Blocks) do begin
        AddToTagList(PModBusPLC[plc].Station,1,PModBusPLC[plc].Outputs.Blocks[block].AddressStart,PModBusPLC[plc].Outputs.Blocks[block].Size, PModBusPLC[plc].Outputs.Blocks[block].ScanTime, PModBusPLC[plc].Outputs.Blocks[block].LastUpdate,PModBusPLC[plc].Outputs.Blocks[block].NeedRefresh);
      end;

      for block := 0 to High(PModBusPLC[plc].Inputs.Blocks) do begin
        AddToTagList(PModBusPLC[plc].Station,2,PModBusPLC[plc].Inputs.Blocks[block].AddressStart,PModBusPLC[plc].Inputs.Blocks[block].Size,PModBusPLC[plc].Inputs.Blocks[block].ScanTime,PModBusPLC[plc].Inputs.Blocks[block].LastUpdate,PModBusPLC[plc].Inputs.Blocks[block].NeedRefresh);
      end;

      for block := 0 to High(PModBusPLC[plc].Registers.Blocks) do begin
        AddToTagList(PModBusPLC[plc].Station,3,PModBusPLC[plc].Registers.Blocks[block].AddressStart,PModBusPLC[plc].Registers.Blocks[block].Size,PModBusPLC[plc].Registers.Blocks[block].ScanTime,PModBusPLC[plc].Registers.Blocks[block].LastUpdate,PModBusPLC[plc].Registers.Blocks[block].NeedRefresh);
      end;

      for block := 0 to High(PModBusPLC[plc].AnalogReg.Blocks) do begin
        AddToTagList(PModBusPLC[plc].Station,4,PModBusPLC[plc].AnalogReg.Blocks[block].AddressStart,PModBusPLC[plc].AnalogReg.Blocks[block].Size,PModBusPLC[plc].AnalogReg.Blocks[block].ScanTime,PModBusPLC[plc].AnalogReg.Blocks[block].LastUpdate,PModBusPLC[plc].AnalogReg.Blocks[block].NeedRefresh);
      end;
    end;

    EntireTagList.Sort(@SortGenericTagList);

    //faz a leitura do bloco que mais precisa ser lido
    //
    //update the tag 
    if (EntireTagList.Count>0) and (EntireTagList.Items[0].NeedUpdate or PReadSomethingAlways) then begin
      //compila o bloco do mais necessitado;
      //build the tagrec record.
      BuildTagRec(EntireTagList.Items[0].station,
                  EntireTagList.Items[0].func,
                  EntireTagList.Items[0].startaddress,
                  EntireTagList.Items[0].size, tr);
      FMustReleaseResources:=true;
      DoRead(tr,values,false);
      FMustReleaseResources:=false;
    end else
      NeedSleep := 1;

    for c:=EntireTagList.Count-1 downto 0 do begin
      EntireTagList.Delete(c);
    end;
    FreeAndNil(EntireTagList);
  finally
    FProtocolReady:=true;
    SetLength(values,0);
  end;
end;

procedure TModBusDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
var
  plc,c:LongInt;
  found:Boolean;
begin
  if Length(values.Values)<TagObj.Size then
    SetLength(values.Values,TagObj.Size);

  for c:=0 to Length(values.Values)-1 do
    values.Values[c] := 0;

  found := false;
  for plc:=0 to High(PModbusPLC) do
    if PModbusPLC[plc].Station = TagObj.Station then begin
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
      PModbusPLC[plc].OutPuts.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $02:
      PModbusPLC[plc].Inputs.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $03:
      PModbusPLC[plc].Registers.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp);
    $04:
      PModbusPLC[plc].AnalogReg.GetValues(TagObj.Address,TagObj.Size,1,values.Values, values.LastQueryResult, values.ValuesTimestamp)
  end;

  if values.LastQueryResult=ioOk then begin
    values.ReadsOK := 1;
    values.ReadFaults := 0;
  end else begin
    values.ReadsOK := 0;
    values.ReadFaults := 1;
  end;
end;

function  TModBusDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
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
    if (PCommPort<>nil) and PCommPort.ReallyActive  then begin
      PCommPort.Lock(DriverID);
      try
        if AllowBroadCast and (tagrec.Station=0) then begin
          res := PCommPort.IOCommandSync(iocWrite,Length(pkg),pkg,0,DriverID,0,@IOResult1);
          case IOResult1.WriteIOResult of
            iorOK:        Result:=ioOk;
            iorTimeOut:   Result:=ioTimeOut;
            iorNotReady:  Result:=ioDriverError;
            iorNone:      Result:=ioNone;
            iorPortError: Result:=ioDriverError;
          end;
          Exit;
        end else
          res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1);

        //se o resultado de leitura deu ok, le o resto do pacote.
        //if the IO result is OK, reads the remaing packet...
        if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin

          //retorna o numero de bytes que está aguardando ser lido no buffer da porta de comunicação.
          //calculates the remaining package length at the communication buffer.
          FRemainingBytes:=RemainingBytes(IOResult1.BufferToRead);

          //clear the remaining buffer...
          if (IOResult1.BufferToRead[PFuncByteOffset-1]<>pkg[PFuncByteOffset-1]) or
             ((IOResult1.BufferToRead[PFuncByteOffset]<>pkg[PFuncByteOffset]) and
              (not (IOResult1.BufferToRead[PFuncByteOffset] in [$81..$88])))then begin
             repeat
               res := PCommPort.IOCommandSync(iocRead,0,nil,1,DriverID,0,@IOResult2);
             until IOResult2.ReadIOResult=iorTimeOut;
             Result:=ioCommError;
             exit;
          end;

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
      finally
        PCommPort.Unlock(DriverID);
      end;
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

function  TModBusDriver.DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  FRemainingBytes:LongInt;
  pkg:BYTES;
  rl:LongInt;
  res:LongInt;
  starts, ends:TNotifyEvent;
begin
  try
    if FMustReleaseResources then begin
      starts:=@HighLatencyOperationWillBegin;
      ends  :=@HighLatencyOperationWasEnded;
    end else begin
      starts:=nil;
      ends  :=nil;
    end;

    pkg := EncodePkg(tagrec,nil,rl);
    if (PCommPort<>nil) and PCommPort.ReallyActive then begin
      PCommPort.Lock(DriverID);
      res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1,starts,ends);

      //se o resultado de leitura deu ok, le o resto do pacote.
      //if the IO result is OK, reads the remaing packet...
      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin

        //retorna o numero de bytes que está aguardando ser lido no buffer da porta de comunicação.
        //calculates the remaining package length at the communication buffer.
        FRemainingBytes:=RemainingBytes(IOResult1.BufferToRead);

        //clear the remaining buffer...
        if (IOResult1.BufferToRead[PFuncByteOffset-1]<>pkg[PFuncByteOffset-1]) or
           ((IOResult1.BufferToRead[PFuncByteOffset]<>pkg[PFuncByteOffset]) and
            (not (IOResult1.BufferToRead[PFuncByteOffset] in [$81..$88])))then begin
           repeat
             res := PCommPort.IOCommandSync(iocRead,0,nil,1,DriverID,0,@IOResult2,starts,ends);
           until IOResult2.ReadIOResult=iorTimeOut;
           Result:=ioCommError;
           exit;
        end;

        if FRemainingBytes>0 then begin
          res := PCommPort.IOCommandSync(iocRead,0,nil,FRemainingBytes,DriverID,0,@IOResult2,starts,ends);

          if res<>0 then begin
            IOResult1.BufferToRead:=ConcatenateBYTES(IOResult1.BufferToRead, IOResult2.BufferToRead);
            IOResult1.Received:=IOResult1.Received + IOResult2.Received;
            if IOResult2.ReadIOResult<>iorOK then
              IOResult1.ReadIOResult:=IOResult2.ReadIOResult;
          end else
            Result:=ioDriverError;
        end;
        Result := DecodePkg(IOResult1,values);
      end else
        Result:=ioEmptyPacket;

      PCommPort.Unlock(DriverID);
    end else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

procedure TModBusDriver.SetOutputMaxHole(v:Cardinal);
var
  plc:LongInt;
begin
  if v = POutputMaxHole then exit;

  POutputMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].OutPuts.MaxHole := v;
end;

procedure TModBusDriver.SetInputMaxHole(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PInputMaxHole then exit;

  PInputMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Inputs.MaxHole := v;
end;

procedure TModBusDriver.SetRegisterMaxHole(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PRegistersMaxHole then exit;

  PRegistersMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Registers.MaxHole := v;
end;

procedure TModBusDriver.BuildTagRec(plc, func, startaddress, size: LongInt; out
  tr: TTagRec);
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

var
  ModbusTagBuilderEditor:TOpenTagEditor = nil;

procedure TModBusDriver.OpenTagEditor(InsertHook: TAddTagInEditorHook;
  CreateProc: TCreateTagProc);
begin
  if Assigned(ModbusTagBuilderEditor) then
    ModbusTagBuilderEditor(Self,Self.Owner,InsertHook,CreateProc)
  else
    inherited;
end;

function TModBusDriver.HasTabBuilderEditor: Boolean;
begin
  Result:=true
end;

procedure SetTagBuilderToolForModBusProtocolFamily(TagBuilderTool:TOpenTagEditor);
begin
  if assigned(ModbusTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for Modbus RTU/TCP protocol family was already assigned.')
  else
    ModbusTagBuilderEditor:=TagBuilderTool;
end;

end.
