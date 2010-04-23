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
  soma o tipo com a área da váriavel (ver tabelas abaixo).

  Tipo de dado:
  @table(
    @rowHead( @cell(Tipo de dado)                  @cell(Valor) )
    @row(     @cell(Byte, 8 bits, unsignaled)      @cell(1)     )
    @row(     @cell(Word, 16 bits, unsignaled)     @cell(2)     )
    @row(     @cell(ShortInt, 16 bits, signaled)   @cell(3)     )
    @row(     @cell(Integer, 32 bits, signaled)    @cell(4)     )
    @row(     @cell(DWord, 32 bits, unsignaled)    @cell(5)     )
    @row(     @cell(Float, 32 bits)                @cell(6)     )
  )

  Area:
  @table(
    @rowHead( @cell(Area)                       @cell(Valor) )
    @row(     @cell(Inputs, Entradas)           @cell(10)     )
    @row(     @cell(Outputs, Saidas)            @cell(20)     )
    @row(     @cell(Flags ou M's)               @cell(30)     )
    @row(     @cell(DB e VM no S7-200 )         @cell(40)     )
    @row(     @cell(DB instanciado)             @cell(50)     )
    @row(     @cell(Counter, S7 300/400)        @cell(60)     )
    @row(     @cell(Timer, S7 300/400)          @cell(70)     )

    @row(     @cell(Special Memory, SM, S7-200) @cell(80)     )
    @row(     @cell(Entrada analógica, S7-200)  @cell(90)     )
    @row(     @cell(Saida analógica, S7-200)    @cell(100)    )
    @row(     @cell(Counter, S7-200)            @cell(110)    )
    @row(     @cell(Timer, S7-200)              @cell(120)    )
  )

  Logo para acessar um byte das entradas, basta colocar na propriedade
  MemReadFunction o valor 10+1 = 11, para acessar a MD100 (DWord) basta
  colocar o valor 30+5 = 35.

  }

  TSiemensProtocolFamily = class(TProtocolDriver)
  protected
    PDUIn,PDUOut:Integer;
    FCPUs:TS7CPUs;
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
    function  SetupPDU(var msg:BYTES; MsgTypeOut:Boolean):TPDU; virtual;
    procedure PrepareReadRequest(var msgOut:BYTES); virtual;
    procedure AddToReadRequest(var msgOut:BYTES; Area, DBnum, Start, ByteCount:Integer); virtual;
  protected
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
  end;

implementation

uses math, syncobjs;

////////////////////////////////////////////////////////////////////////////////
// CONSTRUTORES E DESTRUTORES
////////////////////////////////////////////////////////////////////////////////

constructor TSiemensProtocolFamily.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDUIn:=0;
  PDUOut:=0;
end;

////////////////////////////////////////////////////////////////////////////////
// Funcoes da interface
////////////////////////////////////////////////////////////////////////////////

function  TSiemensProtocolFamily.initAdapter:Boolean;
begin

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
begin
  pduo := SetupPDU(msgOut,true);
  inc(CPU.PDUId);
  PPDUHeader(pduo.header)^.number:=SwapBytesInWord(CPU.PDUId);
  Result := false;
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
    pdu := SetupPDU(msgIn, true);
    CPU.MaxPDULen:=((pdu.param+6)^)*256+((pdu.param+7)^);
    Result := true;
  end;
end;

function  TSiemensProtocolFamily.SetupPDU(var msg:BYTES; MsgTypeOut:Boolean):TPDU;
var
  position:Integer;
begin
  if MsgTypeOut then
    position:=PDUOut
  else
    position:=PDUIn;

  Result.header:=@msg[position];
  Result.header_len:=10;
  if PPDUHeader(Result.header)^.PDUHeadertype in [2,3] then
    Result.header_len:=12;

  Result.param:=@msg[position+Result.header_len];
  Result.param_len:=SwapBytesInWord(PPDUHeader(Result.header)^.param_len);

  Result.data:=@msg[position + Result.header_len + Result.param_len];
  Result.data_len:=SwapBytesInWord(PPDUHeader(Result.header)^.data_len);

  Result.udata:=nil;
  Result.user_data_len:=0
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

procedure TSiemensProtocolFamily.AddToReadRequest(var msgOut:BYTES; Area, DBnum, Start, ByteCount:Integer);
var
  param:BYTES;
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



  SetLength(param, 0);
end;

procedure TSiemensProtocolFamily.AddParam(var MsgOut:BYTES; const param:BYTES);
var
  pdu:TPDU;
  paramlen, extra:Integer;
begin
  pdu := SetupPDU(MsgOut, true);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOut+10+extra+paramlen) then begin
    SetLength(MsgOut,(PDUOut+10+extra+paramlen));
    pdu := SetupPDU(MsgOut, true);
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
      result[0]:=0;
      result[1]:=0;
    end;
  end;
end;

procedure TSiemensProtocolFamily.listReachablePartners;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// FUNCOES DE MANIPULAÇAO DO DRIVER
////////////////////////////////////////////////////////////////////////////////

procedure TSiemensProtocolFamily.DoAddTag(TagObj:TTag);
var
  plc:integer;
begin
  for plc := 0 to High(FCPUs) do begin

  end;
  Inherited DoAddTag(TagObj);
end;

procedure TSiemensProtocolFamily.DoDelTag(TagObj:TTag);
begin

  Inherited DoDelTag(TagObj);
end;

procedure TSiemensProtocolFamily.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin
  DoTagChange(TagObj, Change, oldValue, newValue);
end;

procedure TSiemensProtocolFamily.DoScanRead(Sender:TObject; var NeedSleep:Integer);
begin

end;

procedure TSiemensProtocolFamily.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function  TSiemensProtocolFamily.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TSiemensProtocolFamily.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

end.
