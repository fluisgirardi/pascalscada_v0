{$IFDEF PORTUGUES}
{:
  @abstract(Implementação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implementation of ISOTCP protocol.)
  This driver is based on ISOTCP implementation of LibNODAVE library of Thomas
  Hergenhahn (thomas.hergenhahn@web.de).

  This driver doesn't use the Libnodave library, it's a rewritten of it.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit s7family;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  classes, sysutils, ProtocolDriver, S7Types, Tag, ProtocolTypes,
  commtypes;

type
  {$IFDEF PORTUGUES}
  {: Familia de drivers Siemens S7. Baseado na biblioteca LibNodave
     de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Para configurar um tag, é necessário preencher as propriedade PLCStation,
  PLCRack e PLCSlot para endereçar o CLP. Para endereçar a memória dentro do CLP
  é necessário prencher as propriedades MemAddress e MemReadFunction e no caso
  de um DB preencha também a propriedade MemFile_DB com o número do DB. O tipo
  da variável é escolida na propriedade TagType.

  Caso o tipo da variável seja Word, ShortInt, Integer, DWord ou Real é
  necessário que as propriedades SwapBytes e SwapWord sejam iguais a true para
  que os valores fiquem iguais aos do CLP.

  Para escolher o tipo (área de memória) que se deseja lêr preencha a
  propriedade MemReadFunction de acordo com a tabela abaixo:

  Area:
  @table(
    @rowHead( @cell(Area)                                 @cell(Valor) @cell(Observação) )
    @row(     @cell(Inputs, Entradas)                     @cell( 1)    @cell( - ))
    @row(     @cell(Outputs, Saidas)                      @cell( 2)    @cell( - ))
    @row(     @cell(Flags ou M's)                         @cell( 3)    @cell( - ))
    @row(     @cell(DB e VM no S7-200 )                   @cell( 4)    @cell( - ))
    @row(     @cell(Counter, S7 300/400)                  @cell( 5)    @cell(Precisa que a propriedade TagType seja igual pttWord))
    @row(     @cell(Timer, S7 300/400)                    @cell( 6)    @cell(Precisa que a propriedade TagType seja igual pttWord))

    @row(     @cell(Special Memory, SM, S7-200)           @cell( 7)    @cell( - ))
    @row(     @cell(Entrada analógica, S7-200)            @cell( 8)    @cell( - ))
    @row(     @cell(Saida analógica, S7-200)              @cell( 9)    @cell( - ))
    @row(     @cell(Counter, S7-200)                      @cell(10)    @cell(Precisa que a propriedade TagType seja igual pttWord))
    @row(     @cell(Timer, S7-200)                        @cell(11)    @cell(Precisa que a propriedade TagType seja igual pttWord))

    @row(     @cell(Entrada analógica (PIW), S7 300/400)  @cell(12)    @cell(Precisa que a propriedade TagType seja igual pttWord))
  )

  Logo para acessar a IB3, basta colocar na propriedade MemReadFunction o valor
  1, MemAddress o valor 3 e na propriedade TagType o valor pttByte e para
  acessar a MD100 (DWord) basta colocar o valor 5 em MemReadFunction, 100 em
  MemAddres e pttDword em TagType.
  }
  {$ELSE}
  {: Siemens S7 protocol drivers family. Based on LibNodave library of
  Thomas Hergenhahn (thomas.hergenhahn@web.de).

  This driver doesn't use the Libnodave library, it's a rewritten of it.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  To setup a tag, you must set the properties PLCStation, PLCRack and PLCSlot
  to address your PLC. To address a memory inside the PLC, you must set the
  properties MemAddress and MemReadFunction (see the table below) and if is a DB
  that is being addressed, set the DB number on property MemFile_DB. The datatype
  of tag can be selected on property TagType.

  If the TagType is a Word, SmalltInt, Integer, DWord or Float you must set too
  the properties SwapBytes and SwapWord to @true to get the PLC values correctly.

  To choose the memory area, set the property MemReadFunction as show below:

  Area:
  @table(
    @rowHead( @cell(Area)                            @cell(Value) @cell(Observation) )
    @row(     @cell(Inputs)                          @cell( 1)    @cell( - ))
    @row(     @cell(Outputs)                         @cell( 2)    @cell( - ))
    @row(     @cell(Flags or M's)                    @cell( 3)    @cell( - ))
    @row(     @cell(DB and V no S7-200 )             @cell( 4)    @cell( - ))
    @row(     @cell(Counter, S7 300/400)             @cell( 5)    @cell(TagType property must be pttWord))
    @row(     @cell(Timer, S7 300/400)               @cell( 6)    @cell(TagType property must be pttWord))

    @row(     @cell(Special Memory, SM, S7-200)      @cell( 7)    @cell( - ))
    @row(     @cell(Analog Input, S7-200)            @cell( 8)    @cell( - ))
    @row(     @cell(Analog Output, S7-200)           @cell( 9)    @cell( - ))
    @row(     @cell(Counter, S7-200)                 @cell(10)    @cell(TagType property must be pttWord))
    @row(     @cell(Timer, S7-200)                   @cell(11)    @cell(TagType property must be pttWord))

    @row(     @cell(Analog Input (PIW), S7 300/400)  @cell(12)    @cell(TagType property must be pttWord))
  )

  So, to address the IB3, you must set the property MemReadFunction the value 1,
  the MemAddress the value 3 and the property TagType the value pttByte. To
  address the MD100 (DWord) you must set MemReadFunction to 5, the MemAddres to
  100 and pttDword on TagType.

  }
  {$ENDIF}
  TSiemensProtocolFamily = class(TProtocolDriver)
  protected
    function  GetTagInfo(tagobj:TTag):TTagRec;
    function  GetByte(Ptr:PByte; idx:Integer):integer;
    procedure SetByte(Ptr:PByte; idx:Integer; value:Byte);
    procedure SetBytes(Ptr:PByte; idx:Integer; values:BYTES);
  protected
    PDUIncoming,PDUOutgoing:Integer;
    FCPUs:TS7CPUs;
    FAdapterInitialized:Boolean;
    function  initAdapter:Boolean; virtual;
    function  disconnectAdapter:Boolean; virtual;
    function  connectPLC(var CPU:TS7CPU):Boolean; virtual;
    function  disconnectPLC(var CPU:TS7CPU):Boolean; virtual;
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; virtual;
    procedure sendMessage(var msgOut:BYTES); virtual;
    function  getResponse(var msgIn:BYTES; var BytesRead:Integer):TIOResult; virtual;
    procedure listReachablePartners; virtual;
  protected
    function  SwapBytesInWord(W:Word):Word;
    procedure Send(var msg:BYTES); virtual;
    procedure PrepareToSend(var msg:BYTES); virtual;
  protected
    procedure AddParam(var MsgOut:BYTES; const param:BYTES); virtual;
    procedure AddData(var MsgOut:BYTES; const data:BYTES); virtual;
    procedure InitiatePDUHeader(var MsgOut:BYTES; PDUType:Integer); virtual;
    function  NegotiatePDUSize(var CPU:TS7CPU):Boolean; virtual;
    function  SetupPDU(var msg:BYTES; MsgOutgoing:Boolean; out PDU:TPDU):Integer; virtual;
    procedure PrepareReadRequest(var msgOut:BYTES); virtual;
    procedure PrepareWriteRequest(var msgOut:BYTES); virtual;
    procedure PrepareReadOrWriteRequest(const WriteRequest:Boolean; var msgOut:BYTES); virtual;
    procedure AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:Integer); virtual;
    procedure AddParamToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:Integer; buffer:BYTES); virtual;
    procedure AddDataToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:Integer; buffer:BYTES); virtual;
  protected
    procedure RunPLC(CPU:TS7CPU);
    procedure StopPLC(CPU:TS7CPU);
    procedure CopyRAMToROM(CPU:TS7CPU);
    procedure CompressMemory(CPU:TS7CPU);
    function  S7ErrorCodeToProtocolErrorCode(code:Word):TProtocolIOResult;
  protected
    function  DoublesToBytes(const Values:TArrayOfDouble; Start, Len:Integer):BYTES;
    function  BytesToDoubles(const ByteSeq:BYTES; Start, Len:Integer):TArrayOfDouble;
    function  CreateCPU(iHack, iSlot, iStation:Integer):integer; virtual;
{ok}procedure UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList; var ResultValues:TArrayOfDouble);
{ok}procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
{ok}procedure DoDelTag(TagObj:TTag); override;
{ok}procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
{ok}procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;

    //estas funcoes ficaram apenas por motivos compatibilidade com os tags
    //e seus metodos de leitura e escrita diretas.
{ok}function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
{ok}function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    function    SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    procedure OpenTagEditor(OwnerOfNewTags: TComponent;
       InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc); override;
  published
    property ReadSomethingAlways;
  end;

implementation

uses PLCTagNumber, PLCBlock, PLCString, PLCStruct, hsstrings, PLCStructElement,
     PLCMemoryManager, dateutils, us7tagbuilder, Controls,
     PLCBlockElement, PLCNumber, TagBit, strutils, math;

////////////////////////////////////////////////////////////////////////////////
// CONSTRUTORES E DESTRUTORES
////////////////////////////////////////////////////////////////////////////////

constructor TSiemensProtocolFamily.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PReadSomethingAlways := true;
  FProtocolReady:=false;
  PDUIncoming:=0;
  PDUOutgoing:=0;
end;

function  TSiemensProtocolFamily.SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

procedure TSiemensProtocolFamily.OpenTagEditor(OwnerOfNewTags: TComponent;
   InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  frmS7tb:TfrmS7TagBuilder;

  curitem, curstructitem,
  curaddress, curidx,
  curTCaddress, curbit:Integer;

  block:TPLCBlock;
  item:TPLCNumber;
  bititem:TTagBit;

  started:Boolean;

  function GetCurWordSize:Integer;
  var
    ttype:TTagType;
  begin
    if frmS7tb.optplcblock.Checked then
      ttype:=frmS7tb.CurBlockType
    else
      ttype:=frmS7tb.StructItem[curstructitem].TagType;

    case ttype of
      pttDefault, pttShortInt, pttByte:
        Result := 1;
      pttSmallInt, pttWord:
        Result := 2;
      pttInteger, pttDWord, pttFloat:
        Result := 4;
    end;
  end;

  function GetValueWithZeros(value, endvalue:Integer; toFill:Boolean):String;
  var
    numdig, dig:Integer;
    strendval, fill:STring;
  begin
    strendval:=IntToStr(endvalue);

    fill:='';
    numdig:=Length(strendval);
    for dig:=1 to numdig do
      fill:=fill+'0';

    if toFill then
      Result:=RightStr(fill+IntToStr(value),numdig)
    else
      Result:=IntToStr(value);
  end;

  function GetName(namepattern:String):String;
  var
    has_atleastonereplacement:Boolean;
  begin
    {
    %a  - Endereço do item
    %i  - Numero do item comecando de 1
    %e  - Numero do item comecando de 0
    %0a - Endereço do item preenchido com zeros.
    %0i - Numero do item comecando de 1, preenchido com zeros
    %0e - Numero do item comecando de 0, preenchido com zeros
    }
    has_atleastonereplacement:=(Pos('%a',namepattern)<>0) or
                               (Pos('%i',namepattern)<>0) or
                               (Pos('%e',namepattern)<>0) or
                               (Pos('%0a',namepattern)<>0) or
                               (Pos('%0i',namepattern)<>0) or
                               (Pos('%0e',namepattern)<>0);
    if not has_atleastonereplacement then
      namepattern:=namepattern+'%i';

    Result:=namepattern;
    if frmS7tb.MemoryArea.ItemIndex in [4,9,5,10] then begin
      Result:= StringReplace(Result,'%a', IntToStr(curTCaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curTCaddress, frmS7tb.GetTheLastItemOffset div 2, true),[rfReplaceAll]);
    end else begin
      Result:= StringReplace(Result,'%a',IntToStr(curaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curaddress, frmS7tb.RealEndOffset, true),[rfReplaceAll]);
    end;
    Result:= StringReplace(Result,'%i',IntToStr(curitem),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0i',GetValueWithZeros(curitem, frmS7tb.spinNumItens.Value, true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%e',IntToStr(curitem-1),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0e',GetValueWithZeros(curitem-1, frmS7tb.spinNumItens.Value-1, true),[rfReplaceAll]);
  end;

begin
  { o que está faltando??
    NO FORMULARIO:
    ** Checagens de substituições ausentes nos nomes a fim de evitar duplicidades de nomes...

    SUBSTITUIÇÕES:

    %a  - Endereço do item
    %i  - Numero do item comecando de 1
    %e  - Numero do item comecando de 0
    %0a - Endereço do item preenchido com zeros.
    %0i - Numero do item comecando de 1, preenchido com zeros
    %0e - Numero do item comecando de 0, preenchido com zeros

  }

  frmS7tb:=TfrmS7TagBuilder.Create(nil);
  try
    with frmS7tb do begin
      if ShowModal=mrOK then begin
        //cria o bloco simples ou bloco estrutura e faz sua configuração.
        if optplcblock.Checked or optplcStruct.Checked then begin

          //cria o bloco
          if optplcblock.Checked then
            block:=TPLCBlock(CreateProc(TPLCBlock))
          else
            block:=TPLCStruct(CreateProc(TPLCStruct));

          block.PLCHack:=PLCRack.Value;
          block.PLCSlot:=PLCSlot.Value;
          block.PLCStation:=PLCStation.Value;
          block.MemReadFunction := GetTagType;
          block.Name:=BlockName.Text;
          if block.MemReadFunction=4 then
            block.MemFile_DB:=spinDBNumber.Value;
          block.MemAddress:=RealStartOffset;

          if optplcblock.Checked then begin
            block.RefreshTime:=BlockScan.Value;
            block.TagType:=CurBlockType;
            Block.SwapBytes:=BlockSwapBytes.Checked;
            block.SwapWords:=BlockSwapWords.Checked;
          end else
            block.RefreshTime:=StructScan.Value;

          block.ProtocolDriver:=Self;
          InsertHook(block);
        end;

        //comeca a criar os itens da estrutura
        curaddress:=spinStartAddress.Value;
        curTCaddress:=spinStartAddress.Value;
        curidx := 0;
        started:=false;
        for curitem:=1 to spinNumItens.Value do begin
          for curstructitem:=0 to StructItemsCount-1 do begin
            //se é para criar o tag.
            if not StructItem[curstructitem].SkipTag then begin
              started:=true;
              if optplctagnumber.Checked then begin

                item:=TPLCTagNumber(CreateProc(TPLCTagNumber));

                with TPLCTagNumber(item) do begin

                  PLCHack:=frmS7tb.PLCRack.Value;
                  PLCSlot:=frmS7tb.PLCSlot.Value;
                  PLCStation:=frmS7tb.PLCStation.Value;
                  MemReadFunction := GetTagType;
                  if MemReadFunction=4 then
                    MemFile_DB:=spinDBNumber.Value;
                  MemAddress:=curaddress;

                  RefreshTime:=StructItem[curstructitem].TagScan;
                  TagType:=StructItem[curstructitem].TagType;
                  SwapBytes:=StructItem[curstructitem].SwapBytes;
                  SwapWords:=StructItem[curstructitem].SwapWords;

                  ProtocolDriver:=Self;
                end;

              end else begin
                if optplcblock.Checked then begin
                  TPLCBlock(block).Size:=curidx+1;
                  item:=TPLCBlockElement(CreateProc(TPLCBlockElement));
                  TPLCBlockElement(item).PLCBlock:=block;
                  TPLCBlockElement(item).Index:=curidx;
                end else begin
                  item:=TPLCStructItem(CreateProc(TPLCStructItem));
                  with TPLCStructItem(item) do begin
                    PLCBlock:=TPLCStruct(block);
                    TPLCStruct(block).Size:=curidx+GetCurWordSize;
                    Index:=curidx;
                    TagType:=StructItem[curstructitem].TagType;
                    SwapBytes:=StructItem[curstructitem].SwapBytes;
                    SwapWords:=StructItem[curstructitem].SwapWords;
                  end;
                end;
              end;

              item.Name:=GetName(StructItem[curstructitem].TagName);
              InsertHook(item);

              for curbit:=0 to StructItem[curstructitem].BitCount-1 do begin
                bititem:=TTagBit(CreateProc(TTagBit));
                bititem.EndBit:=StructItem[curstructitem].Bit[curbit].EndBit;
                bititem.StartBit:=StructItem[curstructitem].Bit[curbit].StartBit;
                bititem.Name:=GetName(StructItem[curstructitem].Bit[curbit].TagName);
                bititem.PLCTag:=item;
                InsertHook(bititem);
              end;
            end;

            inc(curTCaddress);
            inc(curaddress,GetCurWordSize);
            if started then begin
              if optplcblock.Checked then
                inc(curidx)
              else
                inc(curidx, GetCurWordSize);
            end;
          end;
        end;
      end;
    end;
  finally
    frmS7tb.Destroy;
  end;
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
  Result:=false;
end;

function  TSiemensProtocolFamily.connectPLC(var CPU:TS7CPU):Boolean;
begin
  Result:=false;
end;

function  TSiemensProtocolFamily.disconnectPLC(var CPU:TS7CPU):Boolean;
begin
  Result:=false;
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

  if CPU.PDUId=$FFFF then
    CPU.PDUId:=0
  else
    inc(CPU.PDUId);

  PPDUHeader(pduo.header)^.number:=SwapBytesInWord(CPU.PDUId);
  Result := false;
end;

procedure TSiemensProtocolFamily.sendMessage(var msgOut:BYTES);
begin

end;

function  TSiemensProtocolFamily.getResponse(var msgIn:BYTES; var BytesRead:Integer):TIOResult;
begin
  Result:=iorNone;
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
  db:Integer;
begin
  Result := false;
  SetLength(param,8);
  SetLength(msg, PDUOutgoing+10+8);

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
    res := SetupPDU(msgIn, false, pdu);
    if res=0 then begin
      CPU.MaxPDULen:=GetByte(pdu.param,6)*256+GetByte(pdu.param,7);
      CPU.MaxBlockSize:=CPU.MaxPDULen-18; //10 bytes do header + 2 bytes do codigo de erro + 2 bytes da solicitação da leitura + 4 das informações do pedido.
      //ajusta o tamanho máximo dos blocos;
      with CPU do begin
        Inputs.MaxBlockItems:=MaxBlockSize;
        Outputs.MaxBlockItems:=MaxBlockSize;
        Timers.MaxBlockItems:=MaxBlockSize;
        Counters.MaxBlockItems:=MaxBlockSize;
        Flags.MaxBlockItems:=MaxBlockSize;
        PeripheralInputs.MaxBlockItems:=MaxBlockSize;

        S7200SMs.MaxBlockItems:=MaxBlockSize;
        S7200Timers.MaxBlockItems:=MaxBlockSize;
        S7200Counters.MaxBlockItems:=MaxBlockSize;
        S7200AnInput.MaxBlockItems:=MaxBlockSize;
        S7200AnOutput.MaxBlockItems:=MaxBlockSize;

        for db:=0 to High(DBs) do
          DBs[db].DBArea.MaxBlockItems:=MaxBlockSize;
      end;
      Result := true;
    end;
  end;
end;

function  TSiemensProtocolFamily.SetupPDU(var msg:BYTES; MsgOutgoing:Boolean; out PDU:TPDU):Integer;
var
  position:Integer;
begin
  if MsgOutgoing then
    position:=PDUOutgoing
  else
    position:=PDUIncoming;

  Result := 0;

  PDU.header:=@msg[position];
  PDU.header_len:=10;
  if PPDUHeader(PDU.header)^.PDUHeadertype in [2,3] then begin
    PDU.header_len:=12;
    Result:=SwapBytesInWord(PPDUHeader(PDU.header)^.Error);
  end;

  PDU.param:=@msg[position+PDU.header_len];
  PDU.param_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.param_len);

  if High(msg)>=(position + PDU.header_len + PDU.param_len) then begin
    PDU.data:=@msg[position + PDU.header_len + PDU.param_len];
    PDU.data_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.data_len);
  end else begin
    PDU.data:=nil;
    PDU.data_len:=0;
  end;
  PDU.user_data_len:=0;
  PDU.udata:=nil
end;

procedure TSiemensProtocolFamily.PrepareReadRequest(var msgOut:BYTES);
begin
  PrepareReadOrWriteRequest(false, msgOut);
end;

procedure TSiemensProtocolFamily.PrepareWriteRequest(var msgOut:BYTES);
begin
  PrepareReadOrWriteRequest(True, msgOut);
end;

procedure TSiemensProtocolFamily.PrepareReadOrWriteRequest(const WriteRequest:Boolean; var msgOut:BYTES);
var
  param:BYTES;
begin
  SetLength(param, 2);

  param[0] :=  ifthen(WriteRequest, S7FuncWrite, S7FuncRead);
  param[1] := 0;
  InitiatePDUHeader(msgOut,1);
  AddParam(msgOut, param);

  SetLength(param,0);
end;

procedure TSiemensProtocolFamily.AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:Integer);
var
  param:BYTES;
  p:PS7Req;
  PDU:TPDU;
  NumReq:Byte;
  intArray:array[0..3] of byte;
  intStart:Integer absolute intArray;
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

  with p^ do begin
    case iArea of
      vtS7_200_AnInput, vtS7_200_AnOutput:
        WordLen:=4;

      vtS7_Counter,
      vtS7_Timer,
      vtS7_200_Counter,
      vtS7_200_Timer:
        WordLen:=iArea;
      else
        intStart:=iStart*8;
    end;

    ReqLength   :=SwapBytesInWord(iByteCount);
    DBNumber    :=SwapBytesInWord(iDBnum);
    AreaCode    :=iArea;
    HiBytes     :=0;
    //StartAddress:=SwapBytesInWord(iStart);
    param[09] := intArray[2];
    param[10] := intArray[1];
    param[11] := intArray[0];
  end;

  AddParam(msgOut, param);

  SetupPDU(msgOut, true, PDU);
  NumReq:=GetByte(PDU.param,1);
  NumReq:=NumReq+1;
  SetByte(PDU.param,1,NumReq);

  SetLength(param, 0);
end;

//executa somente uma escrita por vez!!!
procedure TSiemensProtocolFamily.AddParamToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:Integer; buffer:BYTES);
var
  param:BYTES;
  bufferLen:Integer;
  p:PS7Req;
  PDU:TPDU;
  NumReq:Byte;
begin
  bufferLen:=Length(buffer);

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

  with p^ do begin
    case iArea of
      vtS7_200_AnInput, vtS7_200_AnOutput:
        begin
          WordLen:=4;
          ReqLength := SwapBytesInWord((bufferLen+1) div 2);
        end;
      vtS7_Counter,
      vtS7_Timer,
      vtS7_200_Counter,
      vtS7_200_Timer:
        begin
          WordLen:=iArea;
          ReqLength := SwapBytesInWord((bufferLen+1) div 2);
        end;
      else
        begin
          iStart:=iStart*8;
          ReqLength := SwapBytesInWord(bufferLen);
        end;
    end;

    DBNumber    :=SwapBytesInWord(iDBnum);
    AreaCode    :=iArea;
    HiBytes     :=0;
    StartAddress:=SwapBytesInWord(iStart);
  end;

  AddParam(msgOut, param);

  SetupPDU(msgOut, true, PDU);
  NumReq:=GetByte(PDU.param,1);
  NumReq:=NumReq+1;
  SetByte(PDU.param,1,NumReq);

  SetLength(param, 0);
end;

procedure TSiemensProtocolFamily.AddDataToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:Integer; buffer:BYTES);
var
  da:BYTES;
  extra:Integer;
  bufferlen:Integer;
  lastdatabyte:Integer;
begin
  bufferlen:=Length(buffer);

  extra := (bufferlen mod 2);

  SetLength(da,4+bufferlen+extra);
  da[00] := $00;
  da[01] := $04; //04 bits,
  da[02] := (bufferlen*8) div 256;
  da[03] := (bufferlen*8) mod 256;
  Move(buffer[0],da[4],Length(buffer));

  if extra=1 then begin
    lastdatabyte:=High(da);
    da[lastdatabyte]:=$80;
  end;

  AddData(msgOut, da);
end;

procedure TSiemensProtocolFamily.AddParam(var MsgOut:BYTES; const param:BYTES);
var
  pdu:TPDU;
  paramlen, extra, newparamlen:Integer;
begin
  SetupPDU(MsgOut, true, pdu);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  newparamlen := Length(param);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra+paramlen+newparamlen) then begin
    SetLength(MsgOut,(PDUOutgoing+10+extra+paramlen+newparamlen));
    SetupPDU(MsgOut, true, pdu);
    paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  end;

  SetBytes(pdu.param, paramlen, param);
  PPDUHeader(pdu.header)^.param_len:=SwapBytesInWord(paramlen + Length(param));
end;

procedure TSiemensProtocolFamily.AddData(var MsgOut:BYTES; const data:BYTES);
var
  pdu:TPDU;
  paramlen, datalen, extra, newdatalen:Integer;
begin
  SetupPDU(MsgOut, true, pdu);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  datalen  := SwapBytesInWord(PPDUHeader(pdu.header)^.data_len);
  newdatalen := Length(data);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra+paramlen+datalen+newdatalen) then begin
    SetLength(MsgOut,(PDUOutgoing+10+extra+paramlen+datalen+newdatalen));
    SetupPDU(MsgOut, true, pdu);
    paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
    datalen  := SwapBytesInWord(PPDUHeader(pdu.header)^.data_len);
  end;

  SetBytes(pdu.data, datalen, data);
  PPDUHeader(pdu.header)^.data_len:=SwapBytesInWord(datalen + Length(data));
end;

procedure TSiemensProtocolFamily.InitiatePDUHeader(var MsgOut:BYTES; PDUType:Integer);
var
  pduh:PPDUHeader;
  extra:integer;
begin
  extra := ifthen(PDUType in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra) then
    SetLength(MsgOut,(PDUOutgoing+10+extra));

  pduh:=@MsgOut[PDUOutgoing];
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

function  TSiemensProtocolFamily.DoublesToBytes(const Values:TArrayOfDouble; Start, Len:Integer):BYTES;
var
  arraylen,
  c:Integer;
begin
  arraylen:=Length(Values);
  if (start+(Len-1))>=arraylen then
    raise Exception.Create(SoutOfBounds);

  SetLength(Result,Len);
  for c:=0 to Len-1 do begin
    Result[c]:=trunc(Values[c+Start]) and $FF;
  end;
end;

function  TSiemensProtocolFamily.BytesToDoubles(const ByteSeq:BYTES; Start, Len:Integer):TArrayOfDouble;
var
  arraylen,
  c:Integer;
begin
  arraylen:=Length(ByteSeq);
  if (start+(Len-1))>=arraylen then
    raise Exception.Create(SoutOfBounds);

  SetLength(Result,Len);
  for c:=0 to Len-1 do begin
    Result[c]:=ByteSeq[c+Start];
  end;
end;

function TSiemensProtocolFamily.CreateCPU(iHack, iSlot, iStation:Integer):Integer;
begin
  Result:=Length(FCPUs);
  SetLength(FCPUs,Result+1);
  with FCPUs[Result] do begin
    MaxBlockSize:=-1;
    MaxPDULen:=0;
    Connected:=false;
    Slot:=iSlot;
    Rack:=iHack;
    Station:=iStation;

    Inputs           :=TPLCMemoryManager.Create;
    Outputs          :=TPLCMemoryManager.Create;
    PeripheralInputs :=TPLCMemoryManager.Create;
    Timers           :=TPLCMemoryManager.Create;
    Counters         :=TPLCMemoryManager.Create;
    Flags            :=TPLCMemoryManager.Create;

    S7200SMs         :=TPLCMemoryManager.Create;
    S7200Timers      :=TPLCMemoryManager.Create;
    S7200Counters    :=TPLCMemoryManager.Create;
    S7200AnInput     :=TPLCMemoryManager.Create;
    S7200AnOutput    :=TPLCMemoryManager.Create;

    Inputs.MaxBlockItems:=MaxBlockSize;
    Outputs.MaxBlockItems:=MaxBlockSize;
    PeripheralInputs.MaxBlockItems:=MaxBlockSize;
    Timers.MaxBlockItems:=MaxBlockSize;
    Counters.MaxBlockItems:=MaxBlockSize;
    Flags.MaxBlockItems:=MaxBlockSize;

    S7200SMs.MaxBlockItems:=MaxBlockSize;
    S7200Timers.MaxBlockItems:=MaxBlockSize;
    S7200Counters.MaxBlockItems:=MaxBlockSize;
    S7200AnInput.MaxBlockItems:=MaxBlockSize;
    S7200AnOutput.MaxBlockItems:=MaxBlockSize;
  end;
end;

procedure TSiemensProtocolFamily.UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList; var ResultValues:TArrayOfDouble);
var
  PDU:TPDU;
  NumResults,
  CurResult,
  DataLen,
  DataIdx,
  ResultLen,
  ResultCode,
  CurValue:Integer;
  ProtocolErrorCode:TProtocolIOResult;
begin
  if writepkg then begin
    SetupPDU(pkgout, true, PDU);
    if GetByte(PDU.param,0)<>S7FuncWrite then exit;
  end else begin
    SetupPDU(pkgin, false, PDU);
    if GetByte(PDU.param,0)<>S7FuncRead then exit;
  end;
  NumResults:=GetByte(PDU.param, 1);
  CurResult:=0;
  DataIdx:=0;
  DataLen:=PDU.data_len;
  while CurResult<NumResults do begin
    ResultCode:=GetByte(PDU.data, DataIdx);

    if writepkg and (ResultCode=0) then
      ProtocolErrorCode:=ioOk
    else
      ProtocolErrorCode:=S7ErrorCodeToProtocolErrorCode(ResultCode);

    if (writepkg or (ResultCode=$FF)) AND (DataLen>4) then begin
      ResultLen:=GetByte(PDU.data, DataIdx+2)*$100 + GetByte(PDU.data, DataIdx+3);
      //o tamanho está em bits, precisa de ajuste.
      if GetByte(PDU.data, DataIdx+1)=4 then
        ResultLen:=ResultLen div 8
      else begin
        //3 o restultado já está em bytes
        //e 9 o resultado está em bits, mas cada bit em um byte.
        if not (GetByte(PDU.data, DataIdx+1) in [3,9]) then
          exit;
      end;
    end else begin
      if ResultCode=$FF then
        ProtocolErrorCode:=ioEmptyPacket;
      ResultLen:=0;
    end;

    //move os dados recebidos para as respectivas areas.
    SetLength(ResultValues,0);
    if ResultLen>0 then begin
      SetLength(ResultValues,ResultLen);
      CurValue:=0;
      while (CurValue<ResultLen) AND (CurValue<Length(ResultValues)) do begin
        ResultValues[CurValue]:=GetByte(PDU.data, DataIdx+4+CurValue);
        inc(CurValue);
      end;

      FProtocolReady:=true;

      with ReqList[CurResult] do begin
        if (PLC>=0) and (PLC<=High(FCPUs)) then
          case ReqType of
            vtS7_DB:
              if (DB>=0) AND (DB<=High(FCPUs[PLC].DBs)) then
                FCPUs[PLC].DBs[DB].DBArea.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Inputs:
               FCPUs[PLC].Inputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Outputs:
               FCPUs[PLC].Outputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_AnInput:
               FCPUs[PLC].S7200AnInput.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_AnOutput:
               FCPUs[PLC].S7200AnOutput.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Timer:
               FCPUs[PLC].Timers.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Counter:
               FCPUs[PLC].Counters.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Flags:
               FCPUs[PLC].Flags.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_SM:
               FCPUs[PLC].S7200SMs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_Timer:
               FCPUs[PLC].S7200Timers.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_Counter:
               FCPUs[PLC].S7200Counters.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Peripheral:
               FCPUs[PLC].PeripheralInputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
          end;
      end;
    end else begin
      //seta a falha...
      with ReqList[CurResult] do begin
        if (PLC>=0) and (PLC<=High(FCPUs)) then
          case ReqType of
            vtS7_DB:
              if (DB>=0) AND (DB<=High(FCPUs[PLC].DBs)) then
                FCPUs[PLC].DBs[DB].DBArea.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Inputs:
               FCPUs[PLC].Inputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Outputs:
               FCPUs[PLC].Outputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_AnInput:
               FCPUs[PLC].S7200AnInput.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_AnOutput:
               FCPUs[PLC].S7200AnOutput.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Timer:
               FCPUs[PLC].Timers.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Counter:
               FCPUs[PLC].Counters.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Flags:
               FCPUs[PLC].Flags.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_SM:
               FCPUs[PLC].S7200SMs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_Timer:
               FCPUs[PLC].S7200Timers.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_Counter:
               FCPUs[PLC].S7200Counters.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Peripheral:
               FCPUs[PLC].PeripheralInputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
          end;
      end;
    end;

    DataIdx:=DataIdx+ResultLen+4;
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

procedure TSiemensProtocolFamily.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  plc, db:integer;
  tr:TTagRec;
  foundplc, founddb, valido:Boolean;
begin
  tr:=GetTagInfo(TagObj);
  foundplc:=false;

  valido:=true;

  for plc := 0 to High(FCPUs) do
    if (FCPUs[plc].Slot=Tr.Slot) AND (FCPUs[plc].Rack=Tr.Hack) AND (FCPUs[plc].Station=Tr.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then begin
    plc:=CreateCPU(tr.Hack, tr.Slot, tr.Station);
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
        FCPUs[plc].DBs[db].DBArea.MaxBlockItems:=FCPUs[plc].MaxBlockSize;
      end;

      FCPUs[plc].DBs[db].DBArea.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    end;
    5:
      FCPUs[plc].Counters.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    6:
      FCPUs[plc].Timers.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    7:
      FCPUs[plc].S7200SMs.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    8:
      FCPUs[plc].S7200AnInput.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    9:
      FCPUs[plc].S7200AnOutput.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    10:
      FCPUs[plc].S7200Counters.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    11:
      FCPUs[plc].S7200Timers.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    12:
      FCPUs[plc].PeripheralInputs.AddAddress(tr.Address,tr.Size,1,tr.ScanTime);
    else
      valido:=false;
  end;

  Inherited DoAddTag(TagObj, valido);
end;

procedure TSiemensProtocolFamily.DoDelTag(TagObj:TTag);
var
  plc, db:integer;
  tr:TTagRec;
  foundplc, founddb:Boolean;
begin
  try
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
      5:
        FCPUs[plc].Counters.RemoveAddress(tr.Address,tr.Size,1);
      6:
        FCPUs[plc].Timers.RemoveAddress(tr.Address,tr.Size,1);
      7:
        FCPUs[plc].S7200SMs.RemoveAddress(tr.Address,tr.Size,1);
      8:
        FCPUs[plc].S7200AnInput.RemoveAddress(tr.Address,tr.Size,1);
      9:
        FCPUs[plc].S7200AnOutput.RemoveAddress(tr.Address,tr.Size,1);
      10:
        FCPUs[plc].S7200Counters.RemoveAddress(tr.Address,tr.Size,1);
      11:
        FCPUs[plc].S7200Timers.RemoveAddress(tr.Address,tr.Size,1);
      12:
        FCPUs[plc].PeripheralInputs.RemoveAddress(tr.Address,tr.Size,1);
    end;
  finally
    Inherited DoDelTag(TagObj);
  end;
end;

procedure TSiemensProtocolFamily.DoScanRead(Sender:TObject; var NeedSleep:Integer);
var
  plc, db, block, retries, i:integer;
  TimeElapsed:Int64;
  msgout, msgin:BYTES;
  initialized, onereqdone:Boolean;
  anow:TDateTime;
  ReqList:TS7ReqList;
  ReqOutOfScan:TS7ReqList;
  MsgOutSize:Integer;
  RequestsPendding:Boolean;

  OutgoingPDUSize, IncomingPDUSize:Integer;
  OutOffScanOutgoingPDUSize, OutOffScanIncomingPDUSize:Integer;

  ivalues:TArrayOfDouble;

  procedure pkg_initialized;
  begin
    if not initialized then begin
      OutgoingPDUSize:=10+2; //10 do header + 2 do pedido de leitura;
      IncomingPDUSize:=10+2+2; //10 do header + 2 do codigo de erro + 2 do pedido de leitura;
      MsgOutSize:=PDUOutgoing+12;
      SetLength(msgout,MsgOutSize);
      PrepareReadRequest(msgout);
      initialized:=true;
    end;
  end;

  function AcceptThisRequest(CPU:TS7CPU; iSize:Integer):Boolean;
  begin
    if ((OutgoingPDUSize+12)<CPU.MaxPDULen) AND ((IncomingPDUSize+4+iSize)<CPU.MaxPDULen) then
      Result:=true
    else
      Result:=false;
  end;

  function OutOfScanAcceptThisRequest(CPU:TS7CPU; iSize:Integer):Boolean;
  begin
    if ((OutOffScanOutgoingPDUSize+12)<CPU.MaxPDULen) AND ((OutOffScanIncomingPDUSize+4+iSize)<CPU.MaxPDULen) then
      Result:=true
    else
      Result:=false;
  end;

  procedure QueueOutOfScanReq(iPLC, iDB, iReqType, iStartAddress, iSize:Integer);
  var
    h:Integer;
  begin
    h:=Length(ReqOutOfScan);
    SetLength(ReqOutOfScan,h+1);
    with ReqOutOfScan[h] do begin
      PLC := iPLC;
      DB := iDB;
      ReqType := iReqType;
      StartAddress := iStartAddress;
      Size := iSize;
    end;
    inc(OutOffScanIncomingPDUSize,4+iSize);
    if (iSize mod 2)=1 then
      inc(OutOffScanIncomingPDUSize);

    inc(OutOffScanOutgoingPDUSize,12);
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
    inc(MsgOutSize, 12);
    inc(IncomingPDUSize,4+iSize);
    if (iSize mod 2)=1 then
      inc(IncomingPDUSize);

    inc(OutgoingPDUSize,12);

    SetLength(msgout,MsgOutSize);
    RequestsPendding:=true;
  end;

  procedure Reset;
  begin
    initialized:=false;
    OutgoingPDUSize:=0;
    IncomingPDUSize:=0;
    MsgOutSize:=0;
    RequestsPendding:=false;
    SetLength(ReqList,0);
    SetLength(msgout,0);
    SetLength(msgin,0);
  end;

  procedure ReadQueuedRequests(var CPU:TS7CPU);
  begin
    if exchange(CPU, msgout, msgin, false) then begin
      UpdateMemoryManager(msgin, msgout, False, ReqList, ivalues);
      NeedSleep:=-1;
    end else
      NeedSleep:=1;
    Reset;
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
  TimeElapsed:=5;
  onereqdone:=false;
  NeedSleep:=1;

  for plc:=0 to High(FCPUs) do begin
    if not FCPUs[plc].Connected then
      if not connectPLC(FCPUs[plc]) then exit;

    Reset;
    OutOffScanOutgoingPDUSize:=0;
    OutOffScanIncomingPDUSize:=0;

    //DBs     //////////////////////////////////////////////////////////////////
    for db := 0 to high(FCPUs[plc].DBs) do begin
      for block := 0 to High(FCPUs[plc].DBs[db].DBArea.Blocks) do begin
        if FCPUs[plc].DBs[db].DBArea.Blocks[block].NeedRefresh then begin
          if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].DBs[db].DBArea.Blocks[block].Size) then begin
            onereqdone:=True;
            ReadQueuedRequests(FCPUs[plc]);
            Reset;
          end;
          pkg_initialized;
          AddToReqList(plc, db, vtS7_DB, FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart, FCPUs[plc].DBs[db].DBArea.Blocks[block].Size);
          AddToReadRequest(msgout, vtS7_DB, FCPUs[plc].DBs[db].DBNum, FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart, FCPUs[plc].DBs[db].DBArea.Blocks[block].Size);
        end else begin
          if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].DBs[db].DBArea.Blocks[block].LastUpdate)>TimeElapsed) then begin
            if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].DBs[db].DBArea.Blocks[block].Size) then begin
              QueueOutOfScanReq(plc, db, vtS7_DB, FCPUs[plc].DBs[db].DBArea.Blocks[block].AddressStart, FCPUs[plc].DBs[db].DBArea.Blocks[block].Size);
            end;
          end;
        end;
      end;
    end;

    //INPUTS////////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Inputs.Blocks) do begin
      if FCPUs[plc].Inputs.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].Inputs.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          initialized:=False;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Inputs, FCPUs[plc].Inputs.Blocks[block].AddressStart, FCPUs[plc].Inputs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Inputs, 0, FCPUs[plc].Inputs.Blocks[block].AddressStart, FCPUs[plc].Inputs.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Inputs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].Inputs.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Inputs, FCPUs[plc].Inputs.Blocks[block].AddressStart, FCPUs[plc].Inputs.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //OUTPUTS///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Outputs.Blocks) do begin
      if FCPUs[plc].Outputs.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].Outputs.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Outputs, FCPUs[plc].Outputs.Blocks[block].AddressStart, FCPUs[plc].Outputs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Outputs, 0, FCPUs[plc].Outputs.Blocks[block].AddressStart, FCPUs[plc].Outputs.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Outputs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].Outputs.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Outputs, FCPUs[plc].Outputs.Blocks[block].AddressStart, FCPUs[plc].Outputs.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //Timers///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Timers.Blocks) do begin
      if FCPUs[plc].Timers.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].Timers.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Timer, FCPUs[plc].Timers.Blocks[block].AddressStart, FCPUs[plc].Timers.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Timer, 0, FCPUs[plc].Timers.Blocks[block].AddressStart, FCPUs[plc].Timers.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Timers.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].Timers.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Timer, FCPUs[plc].Timers.Blocks[block].AddressStart, FCPUs[plc].Timers.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //Counters//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Counters.Blocks) do begin
      if FCPUs[plc].Counters.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].Counters.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Counter, FCPUs[plc].Counters.Blocks[block].AddressStart, FCPUs[plc].Counters.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Counter, 0, FCPUs[plc].Counters.Blocks[block].AddressStart, FCPUs[plc].Counters.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Counters.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].Counters.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Counter, FCPUs[plc].Counters.Blocks[block].AddressStart, FCPUs[plc].Counters.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //Flags///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].Flags.Blocks) do begin
      if FCPUs[plc].Flags.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].Flags.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Flags, FCPUs[plc].Flags.Blocks[block].AddressStart, FCPUs[plc].Flags.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Flags, 0, FCPUs[plc].Flags.Blocks[block].AddressStart, FCPUs[plc].Flags.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].Flags.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].Flags.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Flags, FCPUs[plc].Flags.Blocks[block].AddressStart, FCPUs[plc].Flags.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //PeripheralInputs///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].PeripheralInputs.Blocks) do begin
      if FCPUs[plc].PeripheralInputs.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].PeripheralInputs.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_Peripheral, FCPUs[plc].PeripheralInputs.Blocks[block].AddressStart, FCPUs[plc].PeripheralInputs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_Peripheral, 0, FCPUs[plc].PeripheralInputs.Blocks[block].AddressStart, FCPUs[plc].PeripheralInputs.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].PeripheralInputs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].PeripheralInputs.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_Peripheral, FCPUs[plc].PeripheralInputs.Blocks[block].AddressStart, FCPUs[plc].PeripheralInputs.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //S7200AnInput///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].S7200AnInput.Blocks) do begin
      if FCPUs[plc].S7200AnInput.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200AnInput.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_AnInput, FCPUs[plc].S7200AnInput.Blocks[block].AddressStart, FCPUs[plc].S7200AnInput.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_AnInput, 0, FCPUs[plc].S7200AnInput.Blocks[block].AddressStart, FCPUs[plc].S7200AnInput.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].S7200AnInput.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200AnInput.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_200_AnInput, FCPUs[plc].S7200AnInput.Blocks[block].AddressStart, FCPUs[plc].S7200AnInput.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //S7200AnOutput//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].S7200AnOutput.Blocks) do begin
      if FCPUs[plc].S7200AnOutput.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200AnOutput.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_AnOutput, FCPUs[plc].S7200AnOutput.Blocks[block].AddressStart, FCPUs[plc].S7200AnOutput.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_AnOutput, 0, FCPUs[plc].S7200AnOutput.Blocks[block].AddressStart, FCPUs[plc].S7200AnOutput.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].S7200AnOutput.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200AnOutput.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_200_AnOutput, FCPUs[plc].S7200AnOutput.Blocks[block].AddressStart, FCPUs[plc].S7200AnOutput.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //S7200Timers///////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].S7200Timers.Blocks) do begin
      if FCPUs[plc].S7200Timers.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200Timers.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_Timer, FCPUs[plc].S7200Timers.Blocks[block].AddressStart, FCPUs[plc].S7200Timers.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_Timer, 0, FCPUs[plc].S7200Timers.Blocks[block].AddressStart, FCPUs[plc].S7200Timers.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].S7200Timers.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200Timers.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_200_Timer, FCPUs[plc].S7200Timers.Blocks[block].AddressStart, FCPUs[plc].S7200Timers.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //S7200Counters//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].S7200Counters.Blocks) do begin
      if FCPUs[plc].S7200Counters.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200Counters.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_Counter, FCPUs[plc].S7200Counters.Blocks[block].AddressStart, FCPUs[plc].S7200Counters.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_Counter, 0, FCPUs[plc].S7200Counters.Blocks[block].AddressStart, FCPUs[plc].S7200Counters.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].S7200Counters.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200Counters.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_200_Counter, FCPUs[plc].S7200Counters.Blocks[block].AddressStart, FCPUs[plc].S7200Counters.Blocks[block].Size);
          end;
        end;
      end;
    end;

    //S7200SMs//////////////////////////////////////////////////////////////////
    for block := 0 to High(FCPUs[plc].S7200SMs.Blocks) do begin
      if FCPUs[plc].S7200SMs.Blocks[block].NeedRefresh then begin
        if not AcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200SMs.Blocks[block].Size) then begin
          onereqdone:=True;
          ReadQueuedRequests(FCPUs[plc]);
          Reset;
        end;
        pkg_initialized;
        AddToReqList(plc, 0, vtS7_200_SM, FCPUs[plc].S7200SMs.Blocks[block].AddressStart, FCPUs[plc].S7200SMs.Blocks[block].Size);
        AddToReadRequest(msgout, vtS7_200_SM, 0, FCPUs[plc].S7200SMs.Blocks[block].AddressStart, FCPUs[plc].S7200SMs.Blocks[block].Size);
      end else begin
        if PReadSomethingAlways and (MilliSecondsBetween(anow,FCPUs[plc].S7200SMs.Blocks[block].LastUpdate)>TimeElapsed) then begin
          if OutOfScanAcceptThisRequest(FCPUs[plc], FCPUs[plc].S7200SMs.Size) then begin
            QueueOutOfScanReq(plc, -1, vtS7_200_SM, FCPUs[plc].S7200SMs.Blocks[block].AddressStart, FCPUs[plc].S7200SMs.Blocks[block].Size);
          end;
        end;
      end;
    end;

    if RequestsPendding then begin
      onereqdone:=true;
      ReadQueuedRequests(FCPUs[plc]);
    end;
  end;

  if (not onereqdone) then begin
    if PReadSomethingAlways and (Length(ReqOutOfScan)>0) then begin
      Reset;
      pkg_initialized;
      for i:=0 to High(ReqOutOfScan) do begin
        if i=0 then
          plc:=ReqOutOfScan[i].PLC;

        if ReqOutOfScan[i].DB<>-1 then begin
          AddToReqList(ReqOutOfScan[i].PLC, ReqOutOfScan[i].DB, ReqOutOfScan[i].ReqType, ReqOutOfScan[i].StartAddress, ReqOutOfScan[i].Size);
          AddToReadRequest(msgout, ReqOutOfScan[i].ReqType, FCPUs[ReqOutOfScan[i].PLC].DBs[ReqOutOfScan[i].DB].DBNum, ReqOutOfScan[i].StartAddress, ReqOutOfScan[i].Size)
        end else begin
          AddToReqList(ReqOutOfScan[i].PLC, 0, ReqOutOfScan[i].ReqType, ReqOutOfScan[i].StartAddress, ReqOutOfScan[i].Size);
          AddToReadRequest(msgout, ReqOutOfScan[i].ReqType, 0, ReqOutOfScan[i].StartAddress, ReqOutOfScan[i].Size)
        end;
        if (i=High(ReqOutOfScan)) then
          ReadQueuedRequests(FCPUs[ReqOutOfScan[i].PLC])
        else begin
          if (i+1)<High(ReqOutOfScan) then begin
            if ReqOutOfScan[i].PLC<>ReqOutOfScan[i+1].PLC then
              ReadQueuedRequests(FCPUs[ReqOutOfScan[i].PLC])
          end;
        end;
      end;
    end;
  end;

  SetLength(ivalues,0);
  SetLength(msgin,0);
  SetLength(msgout,0);
  SetLength(ReqList,0);
end;

procedure TSiemensProtocolFamily.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  plc, db:integer;
  foundplc, founddb:Boolean;
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
    5:
      FCPUs[plc].Counters.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    6:
      FCPUs[plc].Timers.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    7:
      FCPUs[plc].S7200SMs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    8:
      FCPUs[plc].S7200AnInput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    9:
      FCPUs[plc].S7200AnOutput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    10:
      FCPUs[plc].S7200Counters.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    11:
      FCPUs[plc].S7200Timers.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    12:
      FCPUs[plc].PeripheralInputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
  end;
end;

function  TSiemensProtocolFamily.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  c,
  OutgoingPacketSize,
  MaxBytesToSend,
  retries,
  BytesToSend,
  BytesSent,
  ReqType,
  dbidx:Integer;
  foundplc,
  hasAtLeastOneSuccess:Boolean;
  PLCPtr:PS7CPU;
  msgout, msgin, BytesBuffer:BYTES;
  incomingPDU:TPDU;
  ReqList:TS7ReqList;
  ivalues:TArrayOfDouble;
begin
  PLCPtr:=nil;
  foundplc:=false;
  dbidx:=-1;
  for c:=0 to High(FCPUs) do
    if (FCPUs[c].Slot=tagrec.Slot) and (FCPUs[c].Rack=tagrec.Hack) and (FCPUs[c].Station=tagrec.Station) then begin
      PLCPtr:=@FCPUs[c];
      foundplc:=true;
      break;
    end;

  if PLCPtr=nil then begin
    c:=CreateCPU(tagrec.Hack, tagrec.Slot, tagrec.Station);
    PLCPtr:=@FCPUs[c];
  end;

  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    Result:=ioDriverError;
    exit;
  end;

  if not PLCPtr.Connected then
    if not connectPLC(PLCPtr^) then begin
      Result:=ioDriverError;
      exit;
    end;

  case tagrec.ReadFunction of
    1: begin
      Result:=ioIllegalFunction;
      exit;
    end;
    2:
      ReqType := vtS7_Outputs;
    3:
      ReqType := vtS7_Flags;
    4: begin
      ReqType := vtS7_DB;
      if foundplc then
        for dbidx:=0 to High(PLCPtr^.DBs) do
          if PLCPtr^.DBs[dbidx].DBNum=tagrec.File_DB then begin
            break;
          end;
    end;
    5:
      ReqType := vtS7_Counter;
    6:
      ReqType := vtS7_Timer;
    7:
      ReqType := vtS7_200_SM;
    8:
      ReqType := vtS7_200_AnInput;
    9:
      ReqType := vtS7_200_AnOutput;
    10:
      ReqType := vtS7_200_Counter;
    11:
      ReqType := vtS7_200_Timer;
    12:
      ReqType := vtS7_Peripheral;
  end;

  MaxBytesToSend:=PLCPtr.MaxPDULen-28;
  BytesSent:=0;
  hasAtLeastOneSuccess:=false;

  while BytesSent<Length(Values) do begin
    SetLength(msgout,0);

    BytesToSend:=Min(MaxBytesToSend, Length(Values)-BytesSent);

    OutgoingPacketSize:=PDUOutgoing+28+BytesToSend;

    SetLength(msgout,OutgoingPacketSize);

    PrepareWriteRequest(msgout);

    BytesBuffer := DoublesToBytes(Values, BytesSent, BytesToSend);

    if ReqType=vtS7_DB then begin
      if tagrec.File_DB=0 then begin
        AddParamToWriteRequest(msgout, vtS7_DB, 1, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
        AddDataToWriteRequest(msgout, vtS7_DB, 1, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      end else begin
        AddParamToWriteRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
        AddDataToWriteRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      end;
    end else begin
      AddParamToWriteRequest(msgout, ReqType, 0, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      AddDataToWriteRequest(msgout, ReqType, 0, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
    end;

    if exchange(PLCPtr^, msgout, msgin, True) then begin
      SetupPDU(msgin,false,incomingPDU);
      if (incomingPDU.data_len>0) and (GetByte(incomingPDU.data,0)=$FF) then begin
        hasAtLeastOneSuccess:=true;
        Result:=ioOk;
        if foundplc then begin
          SetLength(ReqList,1);
          ReqList[0].DB:=dbidx;
          ReqList[0].PLC:=c;
          ReqList[0].ReqType:=ReqType;
          ReqList[0].StartAddress:=tagrec.Address+BytesSent+tagrec.OffSet;
          ReqList[0].Size:=BytesToSend;
          UpdateMemoryManager(msgin, msgout, true, ReqList, ivalues);
        end;
      end else begin
        if hasAtLeastOneSuccess then begin
          Result:=ioPartialOk
        end else
          if incomingPDU.data_len>0 then begin
            Result:=S7ErrorCodeToProtocolErrorCode(GetByte(incomingPDU.data,0))
          end else
            Result := ioCommError;
        exit;
      end;
    end else begin
      if hasAtLeastOneSuccess then begin
        Result:=ioPartialOk
      end else
        Result:=ioCommError;

      exit;
    end;

    inc(BytesSent,BytesToSend);
  end;

  SetLength(ivalues, 0);
end;

function  TSiemensProtocolFamily.DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  c,
  IncomingPacketSize,
  OutgoingPacketSize,
  MaxBytesToRecv,
  retries,
  BytesToRecv,
  BytesReceived,
  ReqType,
  dbidx:Integer;
  foundplc,
  hasAtLeastOneSuccess:Boolean;
  PLCPtr:PS7CPU;
  msgout, msgin:BYTES;
  incomingPDU:TPDU;
  ReqList:TS7ReqList;
  ivalues:TArrayOfDouble;
begin
  PLCPtr:=nil;
  foundplc:=false;
  dbidx:=-1;
  for c:=0 to High(FCPUs) do
    if (FCPUs[c].Slot=tagrec.Slot) and (FCPUs[c].Rack=tagrec.Hack) and (FCPUs[c].Station=tagrec.Station) then begin
      PLCPtr:=@FCPUs[c];
      foundplc:=true;
      break;
    end;

  if PLCPtr=nil then begin
    c:=CreateCPU(tagrec.Hack, tagrec.Slot, tagrec.Station);
    PLCPtr:=@FCPUs[c];
    foundplc:=true;
  end;

  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    Result:=ioDriverError;
    exit;
  end;

  if not PLCPtr.Connected then
    if not connectPLC(PLCPtr^) then begin
      Result:=ioDriverError;
      exit;
    end;

  case tagrec.ReadFunction of
    1:
      ReqType:=vtS7_Inputs;
    2:
      ReqType := vtS7_Outputs;
    3:
      ReqType := vtS7_Flags;
    4: begin
      ReqType := vtS7_DB;
      if foundplc then
        for dbidx:=0 to High(PLCPtr^.DBs) do
          if PLCPtr^.DBs[dbidx].DBNum=tagrec.File_DB then begin
            break;
          end;
    end;
    5:
      ReqType := vtS7_Counter;
    6:
      ReqType := vtS7_Timer;
    7:
      ReqType := vtS7_200_SM;
    8:
      ReqType := vtS7_200_AnInput;
    9:
      ReqType := vtS7_200_AnOutput;
    10:
      ReqType := vtS7_200_Counter;
    11:
      ReqType := vtS7_200_Timer;
    12:
      ReqType := vtS7_Peripheral;
  end;

  MaxBytesToRecv:=PLCPtr.MaxPDULen-18; //10 do header, 2 codigo de erro, 2 pedido de leitura, 4 header do resultado.
  BytesReceived:=0;
  hasAtLeastOneSuccess:=false;

  SetLength(Values, tagrec.Size);

  while BytesReceived<tagrec.Size do begin
    SetLength(msgout,0);

    BytesToRecv:=Min(MaxBytesToRecv, tagrec.Size-BytesReceived);

    IncomingPacketSize:=PDUIncoming+18+BytesToRecv;
    OutgoingPacketSize:=PDUOutgoing+24; //10 do header, 2 pedido leitura, 12 do header do pedido de leitura.

    SetLength(msgout,OutgoingPacketSize);
    SetLength(msgin, IncomingPacketSize);

    PrepareReadRequest(msgout);

    if ReqType=vtS7_DB then begin
      if tagrec.File_DB=0 then begin
        AddToReadRequest(msgout, vtS7_DB, 1,              tagrec.Address+tagrec.OffSet+BytesReceived, tagrec.Size-BytesReceived);
      end else begin
        AddToReadRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesReceived, tagrec.Size-BytesReceived);
      end;
    end else begin
      AddToReadRequest(  msgout, ReqType, 0,              tagrec.Address+tagrec.OffSet+BytesReceived, tagrec.Size-BytesReceived);
    end;

    if exchange(PLCPtr^, msgout, msgin, false) then begin
      SetupPDU(msgin,false,incomingPDU);
      if (incomingPDU.data_len>0) and (GetByte(incomingPDU.data,0)=$FF) then begin
        hasAtLeastOneSuccess:=true;
        Result:=ioOk;
        if foundplc then begin
          SetLength(ReqList,1);
          ReqList[0].DB:=dbidx;
          ReqList[0].PLC:=c;
          ReqList[0].ReqType:=ReqType;
          ReqList[0].StartAddress:=tagrec.Address+BytesReceived+tagrec.OffSet;
          ReqList[0].Size:=BytesToRecv;
          UpdateMemoryManager(msgin, msgout, false, ReqList, ivalues);
          Move(ivalues[0],Values[BytesReceived], Length(ivalues)*sizeof(Double));
        end;
      end else begin
        if hasAtLeastOneSuccess then begin
          Result:=ioPartialOk
        end else
          if incomingPDU.data_len>0 then begin
            Result:=S7ErrorCodeToProtocolErrorCode(GetByte(incomingPDU.data,0))
          end else
            Result := ioCommError;
        exit;
      end;
    end else begin
      if hasAtLeastOneSuccess then begin
        Result:=ioPartialOk
      end else
        Result:=ioCommError;

      exit;
    end;

    inc(BytesReceived,BytesToRecv);
  end;
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

function  TSiemensProtocolFamily.S7ErrorCodeToProtocolErrorCode(code:Word):TProtocolIOResult;
begin
  case code of
    $FF: Result:=ioOk;
    $06: Result:=ioIllegalRequest;
    $0A ,$03: Result:=ioObjectNotExists;
    $05: Result:=ioIllegalMemoryAddress;
    else
      Result:=ioUnknownError;
  end;
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

procedure TSiemensProtocolFamily.SetBytes(Ptr:PByte; idx:Integer; values:BYTES);
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  Move(values[0],inptr^,Length(values));
end;

end.