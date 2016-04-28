{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o driver MC PROTOCOL.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements the MC PROTOCOL driver.)
}
{$ENDIF}

unit MelsecTCP;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  MelsecDriver, Tag, commtypes, Classes;

type
  TMelsecTCPDriver = class(TMelsecDriver)
  protected
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; override;
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
    function RemainingBytesWrite(buffer: BYTES): LongInt; override;
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec): LongInt; override;
    function PlcDeviceType(memReadFunction: integer): integer; override;
  public
  constructor Create(AOwner: TComponent); override;
  published
    property ReadSomethingAlways;
    property Output_M_MaxHole;
    property Output_SM_MaxHole;
    property Output_L_MaxHole;
    property Output_F_MaxHole;
    property Output_V_MaxHole;
    property Output_X_MaxHole;
    property Output_Y_MaxHole;
    property Output_B_MaxHole;
    property Register_D_MaxHole;
    property Register_SD_MaxHole;
  end;

implementation

uses Math, PLCMemoryManager, SysUtils, crossdatetime{$IFDEF FDEBUG}, LCLProc{$ENDIF};

{ TMelsecTCPDriver }

constructor TMelsecTCPDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PInternalDelayBetweenCmds:=0;
  PFirstRequestLen:=9;
  PFuncByteOffset:=11;
  PCRCLen:=0;
end;

function TMelsecTCPDriver.DecodePkg(pkg: TIOPacket;
  out values: TArrayOfDouble): TProtocolIOResult;
var
   i,c,c2,plc:LongInt;
   address, len:Cardinal;
   foundPLC:Boolean;
   aux:TPLCMemoryManager;
   {$IFDEF FDEBUG}
   debug:string;
   {$ENDIF}
begin
  //se algumas das IOs falhou,
  //if some IO fail.
  Result:=ioOk;
  case pkg.WriteIOResult of
    iorTimeOut:
      Result:=ioTimeOut;
    iorNotReady,
    iorNone:
      Result:=ioDriverError;
    iorPortError:
      Result := ioCommError;
  end;

  if (Result<>ioOk)then
    case pkg.ReadIOResult of
      iorTimeOut:
        Result:=ioTimeOut;
      iorNotReady,
      iorNone:
        Result:=ioDriverError;
      iorPortError:
        Result := ioCommError;
    end;

  //se o endereco retornado nao conferem com o selecionado...
  //if the address in the incoming packet is different of the requested
  if (Result=ioOk) AND (pkg.BufferToWrite[6]<>pkg.BufferToRead[6]) then begin
    {$IFDEF FDEBUG}
    debug := '';
    for c := 0 to High(pkg.BufferToRead) do
      debug := debug + IntToHex(pkg.BufferToRead[c], 2);
    DebugLn('Pacotes diferem no endereço. Hex do pacote recebido:');
    DebugLn(debug);
    {$ENDIF}
    Result := ioCommError;
  end;

  //procura o plc
  //search de PLC
  foundPLC := false;
  for plc := 0 to High(PMelsecPLC) do
    //if PMelsecPLC[plc].Station = pkg.BufferToWrite[6] then begin
    if PMelsecPLC[plc].Station = 1 then begin
      foundPLC := true;
      break;
    end;

  //comeca a decodificar o pacote...
  //decodes the packet.

  //se for bit
  if (pkg.BufferToWrite[13] = 1) then //byte
  begin
    if (pkg.BufferToWrite[12] = 20) then //escrita
    begin
      if Result=ioOk then
      begin
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        while (i<len) do
        begin
           values[i] := pkg.BufferToWrite[21] + pkg.BufferToWrite[22];
           inc(i);
        end;

        if Length(PMelsecPLC)>0 then
        begin
          if pkg.BufferToWrite[18] = 144 then  //M
            PMelsecPLC[0].OutPuts_M.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 145 then  //SM
            PMelsecPLC[0].OutPuts_SM.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 146 then  //L
            PMelsecPLC[0].OutPuts_L.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 147 then  //F
            PMelsecPLC[0].OutPuts_F.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 148 then  //V
            PMelsecPLC[0].OutPuts_V.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 156 then  //X
            PMelsecPLC[0].OutPuts_X.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 157 then  //Y
            PMelsecPLC[0].OutPuts_Y.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 160 then  //B
            PMelsecPLC[0].OutPuts_B.SetValues(address,len,1,Values,Result);
        end;
      end
      else
        if foundPLC then
        begin
          if pkg.BufferToWrite[18] = 144 then   //M
            PMelsecPLC[plc].OutPuts_M.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 145 then   //SM
            PMelsecPLC[plc].OutPuts_SM.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 146 then   //L
            PMelsecPLC[plc].OutPuts_L.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 147 then   //F
            PMelsecPLC[plc].OutPuts_F.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 148 then   //V
            PMelsecPLC[plc].OutPuts_V.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 156 then   //X
            PMelsecPLC[plc].OutPuts_X.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 157 then   //Y
            PMelsecPLC[plc].OutPuts_Y.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 160 then   //B
            PMelsecPLC[plc].OutPuts_B.SetFault(address,len,1,Result);
        end;
    end;

    if (pkg.BufferToWrite[12] = 4) then //leitura
    begin
      //acerta onde vao ser colocados os valores decodificados...
      //where the data decoded will be stored.
      if Result=ioOk then
      begin
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len := pkg.BufferToWrite[19];
        SetLength(Values,len);

        i := 0;

        for I := 0 to len - 1 do
        begin
          SetLength(Values,1);
          c2 := (i div 2) + 11;
          if (i mod 2 = 0) then
            Values[0]:= byte(pkg.BufferToRead[c2] shr 4)
          else
          begin
            c := LongInt(byte(pkg.BufferToRead[c2]));
            if c = 17 then
              c := 1;
            if c = 16 then
              c := 0;
            Values[0] := c;
          end;
          if Length(PMelsecPLC)>0 then
          begin
            if pkg.BufferToWrite[18] = 144 then  //M
              PMelsecPLC[0].OutPuts_M.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 145 then //SM
              PMelsecPLC[0].OutPuts_SM.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 146 then //L
              PMelsecPLC[0].OutPuts_L.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 147 then //F
              PMelsecPLC[0].OutPuts_F.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 148 then //V
              PMelsecPLC[0].OutPuts_V.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 156 then //X
              PMelsecPLC[0].OutPuts_X.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 157 then //Y
              PMelsecPLC[0].OutPuts_Y.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 160 then //B
              PMelsecPLC[0].OutPuts_B.SetValues(address,1,1,Values,Result);
          end;
          address := address + 1;
        end;
      end
      else
        if foundPLC then
        begin
          if pkg.BufferToWrite[18] = 144 then     //M
            PMelsecPLC[0].OutPuts_M.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 145 then    //SM
            PMelsecPLC[0].OutPuts_SM.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 146 then    //L
            PMelsecPLC[0].OutPuts_L.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 147 then    //F
            PMelsecPLC[0].OutPuts_F.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 148 then    //V
            PMelsecPLC[0].OutPuts_V.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 156 then    //X
            PMelsecPLC[0].OutPuts_X.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 157 then    //Y
            PMelsecPLC[0].OutPuts_Y.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 160 then    //B
            PMelsecPLC[0].OutPuts_B.SetFault(address,len,1,Result);
        end;
    end;
  end;








  ///////////////////////////////////////
  /// se for float
  if (pkg.BufferToWrite[13] = 0) then //float
  begin
    //escrita
    if (pkg.BufferToWrite[12] = 20) then //escrita
    begin
      if Result=ioOk then
      begin
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        while (i<len) do
        begin
          values[i] := pkg.BufferToWrite[21] + pkg.BufferToWrite[22];
          inc(i);
        end;

        if Length(PMelsecPLC)>0 then
        begin
          if pkg.BufferToWrite[18] = 168 then     //D
            PMelsecPLC[0].Registers_D.SetValues(address,len,1,Values,Result);
          if pkg.BufferToWrite[18] = 169 then     //SD
            PMelsecPLC[0].Registers_SD.SetValues(address,len,1,Values,Result);
        end;
      end
      else
        if foundPLC then
        begin
          if pkg.BufferToWrite[18] = 168 then     //D
            PMelsecPLC[plc].Registers_D.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 169 then     //SD
            PMelsecPLC[plc].Registers_SD.SetFault(address,len,1,Result);
        end;
    end;

    //leitura
    if (pkg.BufferToWrite[12] = 4) then //leitura
    begin
      if Result=ioOk then
      begin
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len     := pkg.BufferToWrite[19];

        SetLength(Values,len);

        for i:=0 to Len-1 do
        begin
          SetLength(Values,1);
          Values[0]:=(LongInt(pkg.BufferToRead[11+(i*2)])) + LongInt(pkg.BufferToRead[12+i*2] shl 8);

          if Length(PMelsecPLC)>0 then
          begin
            if pkg.BufferToWrite[18] = 168 then     //D
              PMelsecPLC[0].Registers_D.SetValues(address,1,1,Values,Result);
            if pkg.BufferToWrite[18] = 169 then     //SD
              PMelsecPLC[0].Registers_SD.SetValues(address,1,1,Values,Result);
          end;
          address := address + 1;
        end;

      end
      else
        if foundPLC then
        begin
          if pkg.BufferToWrite[18] = 168 then     //D
            PMelsecPLC[0].Registers_D.SetFault(address,len,1,Result);
          if pkg.BufferToWrite[18] = 169 then     //SD
            PMelsecPLC[0].Registers_SD.SetFault(address,len,1,Result);
        end;
    end;
  end;
end;

function TMelsecTCPDriver.EncodePkg(TagObj: TTagRec; ToWrite: TArrayOfDouble;
  var ResultLen: Integer): BYTES;
var
  i, c, c2, size:LongInt;
  NetworkNumber, PcNumber, IoNumber, ChannelNumber, CpuTimer,
  iMainCommand, iSubCommand, frame, dataLength: integer;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.
  //
  //Verify if is packet to write data on device that is being encoded.

  //de leitura de valores...
  //read data from slave.
  if ToWrite=nil then
  begin
    case TagObj.ReadFunction of
      $01..$08: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        //encode a packet to read input, outputs, register or analog registers
        frame         := 80;
        networkNumber := 0;
        PcNumber      := 255;
        IoNumber      := 1023;
        ChannelNumber := 0;
        dataLength    := 12;
        CpuTimer      := 16;
        iMainCommand  := 1025;
        iSubCommand   := 1;

        SetLength(Result,22);
        Result[00] := frame;
        Result[01] := frame shr 8;
        Result[02] := NetworkNumber;
        Result[03] := PcNumber;
        Result[04] := IoNumber;
        Result[05] := IoNumber shr 8;
        Result[06] := ChannelNumber;
        Result[07] := dataLength;
        Result[08] := dataLength shr 8;
        Result[09] := CpuTimer;
        Result[10] := CpuTimer shr 8;
        Result[11] := iMainCommand;
        Result[12] := iMainCommand shr 8;
        Result[13] := iSubCommand;
        Result[14] := iSubCommand shr 8;

        Result[15] := TagObj.Address;
        Result[16] := TagObj.Address shr 8;
        Result[17] := TagObj.Address shr 16;
        Result[18] := PlcDeviceType(TagObj.ReadFunction);
        Result[19] := TagObj.Size;
        Result[20] := TagObj.Size shr 8;
        Result[21] := 0;
      end;

      $09,$10: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        //
        //encode a packet to read input, outputs, register or analog registers
        //valores fixos
        frame         := 80;
        networkNumber := 0;
        PcNumber      := 255;
        IoNumber      := 1023;
        ChannelNumber := 0;
        CpuTimer      := 16;
        iMainCommand  := 1025;
        dataLength    := 12;
        iSubCommand   := 0;

        SetLength(Result,22);
        Result[00] := frame;
        Result[01] := frame shr 8;
        Result[02] := NetworkNumber;
        Result[03] := PcNumber;
        Result[04] := IoNumber; //size of the packet, bHi
        Result[05] := IoNumber shr 8; //size of the packet, bLo
        Result[06] := ChannelNumber;
        Result[07] := dataLength;
        Result[08] := dataLength shr 8;
        Result[09] := CpuTimer;
        Result[10] := CpuTimer shr 8;
        Result[11] := iMainCommand;
        Result[12] := iMainCommand shr 8;
        Result[13] := iSubCommand;
        Result[14] := iSubCommand shr 8;

        Result[15] := TagObj.Address;
        Result[16] := TagObj.Address shr 8;
        Result[17] := TagObj.Address shr 16;
        Result[18] := PlcDeviceType(TagObj.ReadFunction);
        Result[19] := TagObj.Size;
        Result[20] := TagObj.Size shr 8;
        Result[21] := 0;
      end;

      else
      begin
        SetLength(Result,0);
      end;
    end;

    // Calcula o tamanho do pacote resposta
    // computes the size of the incoming packet.
    case TagObj.ReadFunction of
      $01..$08:
        ResultLen := 9 +(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)<>0,1,0);
      $09..$10:
        ResultLen := 9+(TagObj.Size*2);
      else
      begin
        ResultLen := 0;
      end;
    end;
  end
  else
  begin
    case TagObj.WriteFunction of
      $01..$08: begin
        //escreve uma saida...
        //encodes a packet to write a single coil.
        frame         := 80;
        networkNumber := 0;
        PcNumber      := 255;
        IoNumber      := 1023;
        ChannelNumber := 0;
        CpuTimer      := 16;
        iMainCommand  := 5121;
        dataLength    := 13;
        iSubCommand   := 1;

        SetLength(Result,22);
        Result[00] := frame;
        Result[01] := frame shr 8;
        Result[02] := NetworkNumber;
        Result[03] := PcNumber;
        Result[04] := IoNumber; //size of the packet, bHi
        Result[05] := IoNumber shr 8; //size of the packet, bLo
        Result[06] := ChannelNumber;
        Result[07] := dataLength;
        Result[08] := dataLength shr 8;
        Result[09] := CpuTimer;
        Result[10] := CpuTimer shr 8;
        Result[11] := iMainCommand;
        Result[12] := iMainCommand shr 8;
        Result[13] := iSubCommand;
        Result[14] := iSubCommand shr 8;

        Result[15] := TagObj.Address;
        Result[16] := TagObj.Address shr 8;
        Result[17] := TagObj.Address shr 16;
        Result[18] := PlcDeviceType(TagObj.WriteFunction);
        Result[19] := $01;
        Result[20] := byte($00 shr 8);
        if Trunc(ToWrite[0]) = 0 then
          Result[21] := byte(0)
        else
          Result[21] := byte(16);
        Result[22] := 0;
      end;

      $09,$10: begin
        //PFirstRequestLen := 9;
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        //
        //encode a packet to read input, outputs, register or analog registers
        //valores fixos
        frame         := 80;
        networkNumber := 0;
        PcNumber      := 255;
        IoNumber      := 1023;
        ChannelNumber := 0;
        CpuTimer      := 16;
        iMainCommand  := 5121;
        dataLength    := 14;
        iSubCommand   := 0;

        SetLength(Result,23);
        Result[00] := frame;
        Result[01] := frame shr 8;
        Result[02] := NetworkNumber;
        Result[03] := PcNumber;
        Result[04] := IoNumber; //size of the packet, bHi
        Result[05] := IoNumber shr 8; //size of the packet, bLo
        Result[06] := ChannelNumber;
        Result[07] := dataLength;
        Result[08] := dataLength shr 8;
        Result[09] := CpuTimer;
        Result[10] := CpuTimer shr 8;
        Result[11] := iMainCommand;
        Result[12] := iMainCommand shr 8;
        Result[13] := iSubCommand;
        Result[14] := iSubCommand shr 8;

        Result[15] := TagObj.Address;
        Result[16] := TagObj.Address shr 8;
        Result[17] := TagObj.Address shr 16;
        Result[18] := PlcDeviceType(TagObj.WriteFunction);
        Result[19] := $01;
        Result[20] := $00;
        Result[21] := Trunc(ToWrite[0]);
        Result[22] := Trunc(ToWrite[0]) shr 8;
        Result[23] := 0;
      end;
      else
      begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    // computes the size of the incoming packet.
    case TagObj.WriteFunction of
      $01..$10:
        ResultLen := 12;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end;
end;

function TMelsecTCPDriver.PlcDeviceType(memReadFunction: integer): integer;
begin
  case memReadFunction of
    $01: Result := $90; //memory M
    $02: Result := $91; //memory SM
    $03: Result := $92; //memory L
    $04: Result := $93; //memory F
    $05: Result := $94; //memory V
    $06: Result := $9C; //memory X
    $07: Result := $9D; //memory Y
    $08: Result := $A0; //memory B
//    $09: Result := $A1; //memory SB
//    $11: Result := $A2; //memory DX
//    $12: Result := $A3; //memory DY
    $09: Result := $A8; //memory D
    $10: Result := $A9; //memory SD
  end;
end;

function TMelsecTCPDriver.RemainingBytesWrite(buffer: BYTES): LongInt;
begin
  if buffer[7] = 4 then
    Result := 4
  else
    Result := 3;
  if buffer[7] = 2 then
    Result := 0;
  if buffer[2] = 166 then
    Result := 7;
  if buffer[2] = 208 then
    Result := 6;
end;

function TMelsecTCPDriver.RemainingBytesRead(buffer: BYTES;
  TagObj: TTagRec): LongInt;
var
  qttags: integer;
begin
  if (TagObj.ReadFunction in [1,2,3,4,5,6,7,8]) then //bytes
  begin
    Result := 2;
    if buffer[2] = 208 then
      Result := 4;
    if buffer[2] = 166 then
      Result := 5;
    if ((buffer[0] = 0) and (buffer[1] = 0) and (buffer[2] = 0)) then
      Result := 6;
    qttags := TagObj.Size;
    if qttags <= 2 then
      Result := Result + 1
    else
    begin
      if (qttags mod 2 <> 0) then
        Result := Result + 1;
      Result := Result + (qttags div 2);
    end;
  end;
  if (TagObj.ReadFunction in [9,10,16]) then //float
  begin
    Result := 3;
    if buffer[2] = 208 then
      Result := 5;
    if buffer[2] = 166 then
      Result := 6;
    if buffer[7] = 2 then
      Result := 0;
    qttags := TagObj.Size;
    Result := Result + (qttags * 2) - 1
  end;
end;

end.
