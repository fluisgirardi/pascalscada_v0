{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de ...
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit s7family;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  classes, sysutils, ProtocolDriver, S7Types, Tag, ProtocolTypes, CrossEvent,
  commtypes;

type
  {: Driver IsoTCP. Baseado na biblioteca LibNodave de ...

  Para endereçar uma memória basta escrever na propriedade MemReadFunction a
  o código da área da váriavel (ver tabelas abaixo).

  Area:
  @table(
    @rowHead( @cell(Area)                       @cell(Valor) )
    @row(     @cell(Inputs, Entradas)           @cell( 1)    )
    @row(     @cell(Outputs, Saidas)            @cell( 2)    )
    @row(     @cell(Flags ou M's)               @cell( 3)    )
    @row(     @cell(DB e VM no S7-200 )         @cell( 4)    )
    @row(     @cell(Counter, S7 300/400)        @cell( 5)    )
    @row(     @cell(Timer, S7 300/400)          @cell( 6)    )

    @row(     @cell(Special Memory, SM, S7-200) @cell( 7)    )
    @row(     @cell(Entrada analógica, S7-200)  @cell( 8)    )
    @row(     @cell(Saida analógica, S7-200)    @cell( 9)    )
    @row(     @cell(Counter, S7-200)            @cell(10)    )
    @row(     @cell(Timer, S7-200)              @cell(11)    )
  )

  Logo para acessar as entradas, basta colocar na propriedade
  MemReadFunction o valor 1, para acessar a MD100 (DWord) basta
  colocar o valor 5.

  O tipo do tag fica vária com a propriedade TagType.

  }

  TSiemensProtocolFamily = class(TProtocolDriver)
  protected
    function  GetTagInfo(tagobj:TTag):TTagRec;
    function  GetByte(Ptr:PByte; idx:Integer):integer;
    procedure SetByte(Ptr:PByte; idx:Integer; value:Byte);
  protected
    PDUIn,PDUOut:Integer;
    FCPUs:TS7CPUs;
    FAdapterInitialized:Boolean;
    function  initAdapter:Boolean; virtual;
    function  disconnectAdapter:Boolean; virtual;
    function  connectPLC(var CPU:TS7CPU):Boolean; virtual;
    function  disconnectPLC(var CPU:TS7CPU):Boolean; virtual;
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; virtual;
    procedure sendMessage(var msgOut:BYTES); virtual;
    function  getResponse(var msgIn:BYTES):Integer; virtual;
    procedure listReachablePartners; virtual;
  protected
    function  SwapBytesInWord(W:Word):Word;
    procedure Send(var msg:BYTES); virtual;
    procedure PrepareToSend(var msg:BYTES); virtual;
  protected
    procedure AddParam(var MsgOut:BYTES; const param:BYTES); virtual;
    procedure InitiatePDUHeader(var MsgOut:BYTES; PDUType:Integer); virtual;
    function  NegotiatePDUSize(var CPU:TS7CPU):Boolean; virtual;
    function  SetupPDU(var msg:BYTES; MsgTypeOut:Boolean; out PDU:TPDU):Integer; virtual;
    procedure PrepareReadRequest(var msgOut:BYTES); virtual;
    procedure AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:Integer); virtual;
  protected
    procedure RunPLC(CPU:TS7CPU);
    procedure StopPLC(CPU:TS7CPU);
    procedure CopyRAMToROM(CPU:TS7CPU);
    procedure CompressMemory(CPU:TS7CPU);
  protected
    procedure UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList);
{ok}procedure DoAddTag(TagObj:TTag); override;
{ok}procedure DoDelTag(TagObj:TTag); override;
{ok}procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;
{ok}procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
{ok}procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;

    //estas funcoes ficaram apenas por motivos compatibilidade com os tags
    //e seus metodos de leitura e escrita diretas.
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    function    SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
  published
    property ReadSomethingAlways;
  end;

implementation

uses math, syncobjs, PLCTagNumber, PLCBlock, PLCString, hsstrings,
     PLCMemoryManager, hsutils, dateutils;

////////////////////////////////////////////////////////////////////////////////
// CONSTRUTORES E DESTRUTORES
////////////////////////////////////////////////////////////////////////////////

constructor TSiemensProtocolFamily.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDUIn:=0;
  PDUOut:=0;
end;

function  TSiemensProtocolFamily.SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

////////////////////////////////////////////////////////////////////////////////
// Funcoes da interface
////////////////////////////////////////////////////////////////////////////////

function  TSiemensProtocolFamily.initAdapter:Boolean;
begin
  Result := true;
end;

function  TSiemensProtocolFamily.disconnectAdapter:Boolean;
begin

end;

function  TSiemensProtocolFamily.connectPLC(var CPU:TS7CPU):Boolean;
begin

end;

function  TSiemensProtocolFamily.disconnectPLC(var CPU:TS7CPU):Boolean;
begin

end;

function TSiemensProtocolFamily.exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean;
var
  pduo:TPDU;
  res:Integer;
begin
  res := SetupPDU(msgOut, true, pduo);
  if res<>0 then  begin
    Result:=False;
    exit;
  end;
  inc(CPU.PDUId);
  PPDUHeader(pduo.header)^.number:=SwapBytesInWord(CPU.PDUId);
  Result := true;
end;

procedure TSiemensProtocolFamily.sendMessage(var msgOut:BYTES);
begin

end;

function  TSiemensProtocolFamily.getResponse(var msgIn:BYTES):Integer;
begin

end;

function  TSiemensProtocolFamily.SwapBytesInWord(W:Word):Word;
var
  bl, bh:Byte;
begin
  bl := W mod $100;
  bh := W div $100;
  Result:=(bl*$100)+bh;
end;

procedure TSiemensProtocolFamily.Send(var msg:BYTES);
begin

end;

procedure TSiemensProtocolFamily.PrepareToSend(var msg:BYTES);
begin

end;

function  TSiemensProtocolFamily.NegotiatePDUSize(var CPU:TS7CPU):Boolean;
var
  param, Msg, msgIn:BYTES;
  pdu:TPDU;
  res:Integer;
begin
  Result := false;
  SetLength(param,8);
  SetLength(msg, PDUOut+10+8);

  param[0] := $F0;
  param[1] := 0;
  param[2] := 0;
  param[3] := 1;
  param[4] := 0;
  param[5] := 1;
  param[6] := 3;
  param[7] := $C0;

  InitiatePDUHeader(msg,1);
  AddParam(Msg,param);
  if exchange(CPU,Msg,msgIn,false) then begin
    res := SetupPDU(msgIn, true, pdu);
    if res=0 then begin
      CPU.MaxPDULen:=((pdu.param+6)^)*256+((pdu.param+7)^);
      Result := true;
    end;
  end;
end;

function  TSiemensProtocolFamily.SetupPDU(var msg:BYTES; MsgTypeOut:Boolean; out PDU:TPDU):Integer;
var
  position:Integer;
begin
  if MsgTypeOut then
    position:=PDUOut
  else
    position:=PDUIn;

  Result := 0;

  PDU.header:=@msg[position];
  PDU.header_len:=10;
  if PPDUHeader(PDU.header)^.PDUHeadertype in [2,3] then begin
    PDU.header_len:=12;
    Result:=SwapBytesInWord(PPDUHeader(PDU.header)^.Error);
  end;

  PDU.param:=@msg[position+PDU.header_len];
  PDU.param_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.param_len);

  PDU.data:=@msg[position + PDU.header_len + PDU.param_len];
  PDU.data_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.data_len);

  PDU.udata:=nil;
  PDU.user_data_len:=0
end;

procedure TSiemensProtocolFamily.PrepareReadRequest(var msgOut:BYTES);
var
  param:BYTES;
begin
  SetLength(param, 2);

  param[0] := S7FuncRead;
  param[1] := 0;
  InitiatePDUHeader(msgOut,1);
  AddParam(msgOut, param);

  SetLength(param,0);
end;

procedure TSiemensProtocolFamily.AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:Integer);
var
  param:BYTES;
  p:PS7Req;
begin
  SetLength(param, 12);
  param[00] := $12;
  param[01] := $0a;
  param[02] := $10;
  param[03] := $02; //1=single bit, 2=byte, 4=word
  param[04] := $00; //comprimento do pedido
  param[05] := $00; //comprimento do pedido
  param[06] := $00; //numero Db
  param[07] := $00; //numero Db
  param[08] := $00; //area code;
  param[09] := $00; //start address in bits
  param[10] := $00; //start address in bits
  param[11] := $00; //start address in bits

  p := PS7Req(@param[00]);

  with TS7Req(p^) do begin
    header[0]:=$12;
    header[1]:=$0A;
    header[2]:=$10;

    case iArea of
      vtS7_200_AnInput, vtS7_200_AnOutput:
        WordLen:=4;

      vtS7_Counter,
      vtS7_Timer,
      vtS7_200_Counter,
      vtS7_200_Timer:
        WordLen:=iArea;
    end;

    ReqLength   :=SwapBytesInWord(iByteCount);
    DBNumber    :=SwapBytesInWord(iDBnum);
    AreaCode    :=iArea;
    StartAddress:=SwapBytesInWord(iStart);
    Bit         :=0;
  end;

  AddParam(msgOut, param);

  SetLength(param, 0);
end;

procedure TSiemensProtocolFamily.AddParam(var MsgOut:BYTES; const param:BYTES);
var
  pdu:TPDU;
  paramlen, extra:Integer;
  res:integer;
begin
  res := SetupPDU(MsgOut, true, pdu);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOut+10+extra+paramlen) then begin
    SetLength(MsgOut,(PDUOut+10+extra+paramlen));
    res := SetupPDU(MsgOut, true, pdu);
    paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  end;

  Move(param[0], (pdu.param + paramlen)^, Length(param));
  PPDUHeader(pdu.header)^.param_len:=SwapBytesInWord(paramlen + Length(param));
end;

procedure TSiemensProtocolFamily.InitiatePDUHeader(var MsgOut:BYTES; PDUType:Integer);
var
  pduh:PPDUHeader;
  extra:integer;
begin
  extra := ifthen(PDUType in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOut+10+extra) then
    SetLength(MsgOut,(PDUOut+10+extra));

  pduh:=@MsgOut[PDUOut];
  with pduh^ do begin
    P:=$32;
    PDUHeadertype:=PDUType;
    a:=0;
    b:=0;
    number:=0;
    param_len:=0;
    data_len:=0;
    //evita escrever se ão foi alocado.
    if extra=2 then begin
      Error:=0;
    end;
  end;
end;

procedure TSiemensProtocolFamily.listReachablePartners;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// FUNCOES DE MANIPULAÇAO DO DRIVER
////////////////////////////////////////////////////////////////////////////////

procedure TSiemensProtocolFamily.UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList);
var
  PDU:TPDU;
  NumResults,
  CurResult,
  DataLen,
  DataIdx,
  ResultLen,
  ResultCode,
  CurValue:Integer;

  ResultValues:TArrayOfDouble;
begin
  if writepkg then begin
    SetupPDU(pkgout, true, PDU);
    if (PDU.param+0)^<>S7FuncWrite then exit;
  end else begin
    SetupPDU(pkgin, false, PDU);
    if (PDU.param+0)^<>S7FuncRead then exit;
  end;
  NumResults:=(PDU.param+1)^;
  CurResult:=0;
  DataIdx:=0;
  DataLen:=PDU.data_len;
  while CurResult<NumResults do begin
    ResultCode:=(PDU.data+DataIdx)^;
    if (ResultCode=$FF) AND (DataLen>4) then begin
      ResultLen:=(PDU.data+DataIdx+2)^*$100 + (PDU.data+DataIdx+3)^;
      //o tamanho está em bits, precisa de ajuste.
      if (PDU.data+DataIdx+1)^=4 then
        ResultLen:=ResultLen div 8
      else begin
        //3 o restultado já está em bytes
        //e 9 o resultado está em bits, mas cada bit em um byte.
        if not ((PDU.data+DataIdx+1)^ in [3,9]) then
          exit;
      end;
    end else begin
      ResultLen:=0;
    end;

    //move os dados recebidos para as respectivas areas.
    SetLength(ResultValues,0);
    if ResultLen>0 then begin
      SetLength(ResultValues,ResultLen);
      CurValue:=0;
      while (CurValue<ResultLen) AND (CurValue<Length(ResultValues)) do begin
        ResultValues[CurValue]:=(PDU.data+DataIdx+4+CurValue)^;
        inc(CurValue);
      end;

      with ReqList[CurResult] do begin
        case ReqType of
          vtS7_DB:
             FCPUs[PLC].DBs[DB].DBArea.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_Inputs:
             FCPUs[PLC].Inputs.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_Outputs:
             FCPUs[PLC].Outputs.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_200_AnInput:
             FCPUs[PLC].AnInput.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_200_AnOutput:
             FCPUs[PLC].AnOutput.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_Timer:
             FCPUs[PLC].Timers.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_Counter:
             FCPUs[PLC].Counters.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_Flags:
             FCPUs[PLC].Flags.SetValues(StartAddress,ResultLen,1,ResultValues);
          vtS7_200_SM:
             FCPUs[PLC].SMs.SetValues(StartAddress,ResultLen,1,ResultValues);
        end;
      end;
    end else begin
      //setar a falha
    end;

    DataIdx:=ResultLen+4;
    dec(DataLen,ResultLen);

    //pelo que entendi, um resultado nunca vem com tamanho impar
    //no pacote.
    if (ResultLen mod 2)=1 then begin
      inc(DataIdx);
      dec(DataLen);
    end;

    //proximo resultado.
    inc(CurResult);
  end;
end;

procedure TSiemensProtocolFamily.DoAddTag(TagObj:TTag);
var
  plc, db:integer;
  tr:TTagRec;
  foundplc, founddb:Boolean;
begin
  tr:=GetTagInfo(TagObj);
  foundplc:=false;

  for plc := 0 to High(FCPUs) do
    if (FCPUs[plc].Slot=Tr.Slot) AND (FCPUs[plc].Rack=Tr.Hack) AND (FCPUs[plc].Station=Tr.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then begin
    plc:=Length(FCPUs);
    SetLength(FCPUs,plc+1);
    with FCPUs[plc] do begin
      Slot:=Tr.Slot;
      Rack:=Tr.Hack;
      Station :=Tr.Station;
      Inputs  :=TPLCMemoryManager.Create;
      Outputs :=TPLCMemoryManager.Create;
      AnInput :=TPLCMemoryManager.Create;
      AnOutput:=TPLCMemoryManager.Create;
      Timers  :=TPLCMemoryManager.Create;
      Counters:=TPLCMemoryManager.Create;
      Flags   :=TPLCMemoryManager.Create;
      SMs     :=TPLCMemoryManager.Create;
    end;
  end;

  case tr.ReadFunction of
    1:
      FCPUs[plc].Inputs.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    2:
      FCPUs[plc].Outputs.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    3:
      FCPUs[plc].Flags.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    4: begin
      if tr.File_DB<=0 then
        tr.File_DB:=1;

      founddb:=false;
      for db:=0 to high(FCPUs[plc].DBs) do
        if FCPUs[plc].DBs[db].DBNum=tr.File_DB then begin
          founddb:=true;
          break;
        end;

      if not founddb then begin
        db:=Length(FCPUs[plc].DBs);
        SetLength(FCPUs[plc].DBs, db+1);
        FCPUs[plc].DBs[db].DBNum:=tr.File_DB;
        FCPUs[plc].DBs[db].DBArea:=TPLCMemoryManager.Create;
      end;

      FCPUs[plc].DBs[db].DBArea.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    end;
    5,10:
      FCPUs[plc].Counters.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    6,11:
      FCPUs[plc].Timers.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    7:
      FCPUs[plc].SMs.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    8:
      FCPUs[plc].AnInput.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    9:
      FCPUs[plc].AnOutput.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
  end;

  Inherited DoAddTag(TagObj);
end;

procedure TSiemensProtocolFamily.DoDelTag(TagObj:TTag);
var
  plc, db:integer;
  tr:TTagRec;
  foundplc, founddb:Boolean;
begin
  tr:=GetTagInfo(TagObj);
  foundplc:=false;

  for plc := 0 to High(FCPUs) do
    if (FCPUs[plc].Slot=Tr.Slot) AND (FCPUs[plc].Rack=Tr.Hack) AND (FCPUs[plc].Station=Tr.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then exit;

  case tr.ReadFunction of
    1: begin
      FCPUs[plc].Inputs.RemoveAddress(tr.Address,tr.Size,1);
    end;
    2:
      FCPUs[plc].Outputs.RemoveAddress(tr.Address,tr.Size,1);
    3:
      FCPUs[plc].Flags.RemoveAddress(tr.Address,tr.Size,1);
    4: begin
      if tr.File_DB<=0 then
        tr.File_DB:=1;

      founddb:=false;
      for db:=0 to high(FCPUs[plc].DBs) do
        if FCPUs[plc].DBs[db].DBNum=tr.File_DB then begin
          founddb:=true;
          break;
        end;

      if not founddb then exit;

      FCPUs[plc].DBs[db].DBArea.RemoveAddress(tr.Address,tr.Size,1);
    end;
    5,10:
      FCPUs[plc].Counters.RemoveAddress(tr.Address,tr.Size,1);
    6,11:
      FCPUs[plc].Timers.RemoveAddress(tr.Address,tr.Size,1);
    7:
      FCPUs[plc].SMs.RemoveAddress(tr.Address,tr.Size,1);
    8:
      FCPUs[plc].AnInput.RemoveAddress(tr.Address,tr.Size,1);
    9:
      FCPUs[plc].AnOutput.RemoveAddress(tr.Address,tr.Size,1);
  end;
  Inherited DoDelTag(TagObj);
end;

procedure TSiemensProtocolFamily.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin
  DoDelTag(TagObj);
  DoAddTag(TagObj);
  inherited DoTagChange(TagObj, Change, oldValue, newValue);
end;

procedure TSiemensProtocolFamily.DoScanRead(Sender:TObject; var NeedSleep:Integer);
var
  plc, db, block, retries:integer;
  TimeElapsed:Int64;
  lastPLC, lastDB, lastType, lastStartAddress, lastSize:integer;
  msgout, msgin:BYTES;
  initialized, onereqdone:Boolean;
  anow:TDateTime;
  ReqList:TS7ReqList;

  procedure pkg_initialized;
  begin
    if not initialized then begin
      PrepareReadRequest(msgout);
      initialized:=true;
    end;
  end;

  procedure AddToReqList(iPLC, iDB, iReqType, iStartAddress, iSize:Integer);
  var
    h:Integer;
  begin
    h:=Length(ReqList);
    SetLength(ReqList,h+1);
    with ReqList[h] do begin
      PLC := iPLC;
      DB := iDB;
      ReqType := iReqType;
      StartAddress := iStartAddress;
      Size := iSize;
    end;
  end;
begin
  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    NeedSleep:=-1;
    exit;
  end;

  anow:=Now;
  TimeElapsed:=0;
  NeedSleep:=-1;
  onereqdone:=false;

  for plc:=0 to High(FCPUs) do begin
    if not FCPUs[plc].Connected then
      connectPLC(FCPUs[plc]);

    //DBs     //////////////////////////////////////////////////////////////////
    for db := 0 to high(FCPUs[plc].DBs) do
      for block := 0 to High(FCPUs[plc].DBs[db].DBArea.Blocks) do
        if FCPUs[plc].DBs[db].DBArea.Blocks[block].NeedRefresh then begin
          pkg_initialized;
          AddToReqList(plc, FCPUs[plc].DBs[db].DBNum, vtS7_DB, FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart, FCPUs[plc].DBs[db].DBArea.Blocks[block].Size);
          AddToReadRequest(msgout, vtS7_DB, FCPUs[plc].DBs[db].DBNum, FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart, FCPUs[plc].DBs[db].DBArea.Blocks[block].Size);
        end else
          if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].DBs[db].DBArea.Blocks[block].LastUpdate)>TimeElapsed) then begin
            TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].DBs[db].DBArea.Blocks[block].LastUpdate);
            lastPLC:=plc;
            lastDB:=db;
            lastType:=vtS7_DB;
            lastStartAddress:=FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart;
            lastSize:=FCPUs[plc].DBs[db].DBArea.Blocks[block].Size;
          end;

    //INPUTS////////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Inputs.Blocks) do
      if FCPUs[plc].Inputs.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Inputs, FCPUs[plc].Inputs.Blocks[block].AddressStart, FCPUs[plc].Inputs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Inputs, 0, FCPUs[plc].Inputs.Blocks[block].AddressStart, FCPUs[plc].Inputs.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Inputs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].Inputs.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_Inputs;
          lastStartAddress:=FCPUs[plc].Inputs.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].Inputs.Blocks[block].Size;
        end;

    //OUTPUTS///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Outputs.Blocks) do
      if FCPUs[plc].Outputs.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Outputs, FCPUs[plc].Outputs.Blocks[block].AddressStart, FCPUs[plc].Outputs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Outputs, 0, FCPUs[plc].Outputs.Blocks[block].AddressStart, FCPUs[plc].Outputs.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Outputs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].Outputs.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_Outputs;
          lastStartAddress:=FCPUs[plc].Outputs.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].Outputs.Blocks[block].Size;
        end;

    //AnInput///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].AnInput.Blocks) do
      if FCPUs[plc].AnInput.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_AnInput, FCPUs[plc].AnInput.Blocks[block].AddressStart, FCPUs[plc].AnInput.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_AnInput, 0, FCPUs[plc].AnInput.Blocks[block].AddressStart, FCPUs[plc].AnInput.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].AnInput.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].AnInput.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_200_AnInput;
          lastStartAddress:=FCPUs[plc].AnInput.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].AnInput.Blocks[block].Size;
        end;

    //AnOutput//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].AnOutput.Blocks) do
      if FCPUs[plc].AnOutput.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_AnOutput, FCPUs[plc].AnOutput.Blocks[block].AddressStart, FCPUs[plc].AnOutput.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_AnOutput, 0, FCPUs[plc].AnOutput.Blocks[block].AddressStart, FCPUs[plc].AnOutput.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].AnOutput.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].AnOutput.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_200_AnOutput;
          lastStartAddress:=FCPUs[plc].AnOutput.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].AnOutput.Blocks[block].Size;
        end;

    //Timers///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Timers.Blocks) do
      if FCPUs[plc].Timers.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Timer, FCPUs[plc].Timers.Blocks[block].AddressStart, FCPUs[plc].Timers.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Timer, 0, FCPUs[plc].Timers.Blocks[block].AddressStart, FCPUs[plc].Timers.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Timers.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].Timers.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_Timer;
          lastStartAddress:=FCPUs[plc].Timers.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].Timers.Blocks[block].Size;
        end;

    //Counters//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Counters.Blocks) do
      if FCPUs[plc].Counters.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Counter, FCPUs[plc].Counters.Blocks[block].AddressStart, FCPUs[plc].Counters.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Counter, 0, FCPUs[plc].Counters.Blocks[block].AddressStart, FCPUs[plc].Counters.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Counters.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].Counters.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_Counter;
          lastStartAddress:=FCPUs[plc].Counters.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].Counters.Blocks[block].Size;
        end;

    //Flags///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Flags.Blocks) do
      if FCPUs[plc].Flags.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Inputs, FCPUs[plc].Flags.Blocks[block].AddressStart, FCPUs[plc].Flags.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Flags, 0, FCPUs[plc].Flags.Blocks[block].AddressStart, FCPUs[plc].Flags.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Flags.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].Flags.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_Flags;
          lastStartAddress:=FCPUs[plc].Flags.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].Flags.Blocks[block].Size;
        end;

    //SMs//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].SMs.Blocks) do
      if FCPUs[plc].SMs.Blocks[block].NeedRefresh then begin
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Inputs, FCPUs[plc].SMs.Blocks[block].AddressStart, FCPUs[plc].SMs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_SM, 0, FCPUs[plc].SMs.Blocks[block].AddressStart, FCPUs[plc].SMs.Blocks[block].Size);
      end else
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].SMs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          TimeElapsed:=MilliSecondsBetween(anow,FCPUs[plc].SMs.Blocks[block].LastUpdate);
          lastPLC:=plc;
          lastDB:=-1;
          lastType:=vtS7_200_SM;
          lastStartAddress:=FCPUs[plc].SMs.Blocks[block].AddressStart;
          lastSize:=FCPUs[plc].SMs.Blocks[block].Size;
        end;
    if initialized then begin
      onereqdone:=true;
      NeedSleep:=0;
      if exchange(FCPUs[plc], msgout, msgin, false) then
        UpdateMemoryManager(msgin, msgout, False, ReqList);
    end;
    initialized:=false;
    setlength(msgin,0);
    setlength(msgout,0);
  end;

  if not onereqdone then begin
    if PReadSomethingAlways and (TimeElapsed>0) then begin
      NeedSleep:=0;
      pkg_initialized;
      if lastDB<>-1 then begin
        AddToReqList(plc, FCPUs[lastplc].DBs[lastDB].DBNum, lastType, lastStartAddress, lastSize);
        AddToReadRequest(msgout, lastType, FCPUs[lastplc].DBs[lastDB].DBNum, lastStartAddress, lastSize)
      end else begin
        AddToReqList(plc, 0, lastType, lastStartAddress, lastSize);
        AddToReadRequest(msgout, lastType, 0, lastStartAddress, lastSize);
      end;
      if exchange(FCPUs[plc], msgout, msgin, false) then
        UpdateMemoryManager(msgin, msgout,False, ReqList);
    end;
  end;
end;

procedure TSiemensProtocolFamily.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  plc, db:integer;
  foundplc, founddb:Boolean;
  temparea:TArrayOfDouble;
  c1, c2, lent, lend:Integer;
begin
  foundplc:=false;

  for plc := 0 to High(FCPUs) do
    if (FCPUs[plc].Slot=TagRec.Slot) AND (FCPUs[plc].Rack=TagRec.Hack) AND (FCPUs[plc].Station=TagRec.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then exit;

  SetLength(values.Values, TagRec.Size);

  case TagRec.ReadFunction of
    1:
      FCPUs[plc].Inputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    2:
      FCPUs[plc].Outputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    3:
      FCPUs[plc].Flags.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    4: begin
      if TagRec.File_DB<=0 then
        TagRec.File_DB:=1;

      founddb:=false;
      for db:=0 to high(FCPUs[plc].DBs) do
        if FCPUs[plc].DBs[db].DBNum=TagRec.File_DB then begin
          founddb:=true;
          break;
        end;

      if not founddb then exit;

      FCPUs[plc].DBs[db].DBArea.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    end;
    5,10:
      FCPUs[plc].Counters.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    6,11:
      FCPUs[plc].Timers.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    7:
      FCPUs[plc].SMs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    8:
      FCPUs[plc].AnInput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    9:
      FCPUs[plc].AnOutput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
  end;
end;

function  TSiemensProtocolFamily.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TSiemensProtocolFamily.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

procedure TSiemensProtocolFamily.RunPLC(CPU:TS7CPU);
var
  paramToRun, msgout, msgin:BYTES;
begin
  SetLength(paramToRun,20);
  paramToRun[00]:=$28;
  paramToRun[01]:=0;
  paramToRun[02]:=0;
  paramToRun[03]:=0;
  paramToRun[04]:=0;
  paramToRun[05]:=0;
  paramToRun[06]:=0;
  paramToRun[07]:=$FD;
  paramToRun[08]:=0;
  paramToRun[09]:=0;
  paramToRun[10]:=9;
  paramToRun[11]:=$50; //P
  paramToRun[12]:=$5F; //_
  paramToRun[13]:=$50; //P
  paramToRun[14]:=$52; //R
  paramToRun[15]:=$4F; //O
  paramToRun[16]:=$47; //G
  paramToRun[17]:=$52; //R
  paramToRun[18]:=$41; //A
  paramToRun[19]:=$4D; //M

  InitiatePDUHeader(msgout, 1);
  AddParam(msgout, paramToRun);

  if not exchange(CPU,msgout,msgin,false) then
    raise Exception.Create('Falha ao tentar colocar a CPU em Run!');

end;

procedure TSiemensProtocolFamily.StopPLC(CPU:TS7CPU);
begin

end;

procedure TSiemensProtocolFamily.CopyRAMToROM(CPU:TS7CPU);
begin

end;

procedure TSiemensProtocolFamily.CompressMemory(CPU:TS7CPU);
begin

end;

function  TSiemensProtocolFamily.GetTagInfo(tagobj:TTag):TTagRec;
begin
  if tagobj is TPLCTagNumber then begin
    with Result do begin
      Hack:=TPLCTagNumber(TagObj).PLCHack;
      Slot:=TPLCTagNumber(TagObj).PLCSlot;
      Station:=TPLCTagNumber(TagObj).PLCStation;
      File_DB:=TPLCTagNumber(TagObj).MemFile_DB;
      Address:=TPLCTagNumber(TagObj).MemAddress;
      SubElement:=TPLCTagNumber(TagObj).MemSubElement;
      Size:=TPLCTagNumber(TagObj).TagSizeOnProtocol;
      OffSet:=0;
      ReadFunction:=TPLCTagNumber(TagObj).MemReadFunction;
      WriteFunction:=TPLCTagNumber(TagObj).MemWriteFunction;
      ScanTime:=TPLCTagNumber(TagObj).RefreshTime;
      CallBack:=nil;
    end;
    exit;
  end;

  if tagobj is TPLCBlock then begin
    with Result do begin
      Hack:=TPLCBlock(TagObj).PLCHack;
      Slot:=TPLCBlock(TagObj).PLCSlot;
      Station:=TPLCBlock(TagObj).PLCStation;
      File_DB:=TPLCBlock(TagObj).MemFile_DB;
      Address:=TPLCBlock(TagObj).MemAddress;
      SubElement:=TPLCBlock(TagObj).MemSubElement;
      Size:=TPLCBlock(TagObj).TagSizeOnProtocol;
      OffSet:=0;
      ReadFunction:=TPLCBlock(TagObj).MemReadFunction;
      WriteFunction:=TPLCBlock(TagObj).MemWriteFunction;
      ScanTime:=TPLCBlock(TagObj).RefreshTime;
      CallBack:=nil;
    end;
    exit;
  end;

  if tagobj is TPLCString then begin
    with Result do begin
      Hack:=TPLCString(TagObj).PLCHack;
      Slot:=TPLCString(TagObj).PLCSlot;
      Station:=TPLCString(TagObj).PLCStation;
      File_DB:=TPLCString(TagObj).MemFile_DB;
      Address:=TPLCString(TagObj).MemAddress;
      SubElement:=TPLCString(TagObj).MemSubElement;
      Size:=TPLCString(TagObj).StringSize;
      OffSet:=0;
      ReadFunction:=TPLCString(TagObj).MemReadFunction;
      WriteFunction:=TPLCString(TagObj).MemWriteFunction;
      ScanTime:=TPLCString(TagObj).RefreshTime;
      CallBack:=nil;
    end;
    exit;
  end;
  raise Exception.Create(SinvalidTag);
end;

function TSiemensProtocolFamily.GetByte(Ptr:PByte; idx:Integer):Integer;
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  Result := inptr^;
end;

procedure TSiemensProtocolFamily.SetByte(Ptr:PByte; idx:Integer; value:Byte);
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  inptr^ := value;
end;

end.
