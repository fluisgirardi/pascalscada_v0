{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o driver ModBus TCP.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements the ModBus TCP protocol driver.)
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
    function RemainingBytes(buffer: BYTES): LongInt; override;
  public
  constructor Create(AOwner: TComponent); override;
  published
    property ReadSomethingAlways;
    property OutputMaxHole;
    property InputMaxHole;
    property RegisterMaxHole;
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

  //leitura de bits das entradas ou saidas
  //request to read the digital input/outpus (coils)
  case pkg.BufferToRead[7] of
    $03: begin
      //acerta onde vao ser colocados os valores decodificados...
      //where the data decoded will be stored.
      if foundPLC then begin
        if pkg.BufferToWrite[7]=$01 then
          aux := PMelsecPLC[plc].OutPuts
        else
          aux := PMelsecPLC[plc].Inputs;
      end;

      if Result=ioOk then begin
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];
        SetLength(Values,len);

        i := 0;
        c := 0;
        c2:= 11;
        while (i<len) and (c2<Length(pkg.BufferToRead)) do begin
          if (c=10) then begin
            c:=0;
            inc(c2);
          end;
          //Values[i]:=IfThen(((LongInt(pkg.BufferToRead[c2]) and (1 shl c))=(1 shl c)),1,0);
          Values[i]:= 1;
          if LongInt(pkg.BufferToRead[c2]) = 0 then
            Values[i]:= 0;

          inc(i);
          inc(c);
        end;
        //if foundPLC then
          //aux.SetValues(address,len,1,Values,Result);
        if Length(PMelsecPLC)>0 then
          PMelsecPLC[0].OutPuts.SetValues(address,len,1,Values,Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    //leitura de words dos registradores ou das entradas analogicas
    //request to read registers/analog registers
    $04,$06,$08,$10,$12,$14,$16,$18,$20,$22: begin
      //acerta onde vao ser colocados os valores decodificados...
      //where the data decoded will be stored.
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
            PMelsecPLC[0].Registers.SetValues(address,1,1,Values,Result);
          address := address + 1;
        end;

      end
      else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    // decodifica a escrita de uma saida digital
    // decodes a write to a single coil
    $05: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[8] * 256) + pkg.BufferToWrite[9];
        SetLength(values,1);

        if (pkg.BufferToWrite[10]=0) and (pkg.BufferToWrite[11]=0) then
           values[0] := 0
        else
           values[0] := 1;

        if foundPLC then
          PMelsecPLC[plc].OutPuts.SetValues(address,1,1,values,Result);
      end else
        if foundPLC then
          PMelsecPLC[plc].OutPuts.SetFault(address,1,1,Result);
    end;

    

    // decodifica o status do escravo
    // decodes a the current state of the slave
    $07: begin
      if foundPLC then begin
        if Result=ioOk then begin
          PMelsecPLC[plc].Status07Value :=LongInt(pkg.BufferToRead[8]);
          PMelsecPLC[plc].Status07TimeStamp := CrossNow;
        end;
        PMelsecPLC[plc].Status07LastError := Result;
      end;
    end;

    // decodifica a escrita de multiplos saidas digitais
    // decodes a write to multiple coils.
    $0F: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[08] * 256) + pkg.BufferToWrite[09];
        len     := (pkg.BufferToWrite[10] * 256) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        c := 0;
        c2:= 13;
        while (i<len) and (c2<Length(pkg.BufferToRead)) do begin
          if (c=8) then begin
            c:=0;
            inc(c2);
          end;
          Values[i]:=IfThen(((LongInt(pkg.BufferToWrite[c2]) and (1 shl c))=(1 shl c)),1,0);
          inc(i);
          inc(c);
        end;

        if foundPLC then
          PMelsecPLC[plc].OutPuts.SetValues(address,len,1,values,Result);
      end else
        if foundPLC then
          PMelsecPLC[plc].OutPuts.SetFault(address,len,1,Result);
    end;

    // decodifica a escrita de multiplos registros
    // decodes a write to a multiple registers
    $02: begin
      if Result=ioOk then begin
        //address := (pkg.BufferToWrite[08] * 256) + pkg.BufferToWrite[09];
        //len     := (pkg.BufferToWrite[10] * 256) + pkg.BufferToWrite[11];
        address := (pkg.BufferToWrite[15]) + (pkg.BufferToWrite[16] shl 8) + (pkg.BufferToWrite[17] shl 16);
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        while (i<len) do begin
           values[i] := pkg.BufferToWrite[21] + pkg.BufferToWrite[22];
           inc(i);
        end;

        //if foundPLC then
          //PMelsecPLC[plc].Registers.SetValues(address,len,1,values,Result);
        if Length(PMelsecPLC)>0 then
          PMelsecPLC[0].OutPuts.SetValues(address,len,1,Values,Result);
      end else
        if foundPLC then
          PMelsecPLC[plc].Registers.SetFault(address,len,1,Result);
    end;
    else begin
      //tratamento de erros modbus
      //modbus error handling.
      case pkg.BufferToRead[8] of
        $01:
          Result := ioIllegalFunction;
        $02:
          Result := ioIllegalRegAddress;
        $03:
          Result := ioIllegalValue;
        $04,$05,$06,$07,$08:
          Result := ioPLCError;
        else
          Result := ioCommError;
      end;

      address := (pkg.BufferToWrite[08] shl 8) + pkg.BufferToWrite[09];
      len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];

      case pkg.BufferToWrite[7] of
        $01: begin
          if foundPLC then
            PMelsecPLC[plc].OutPuts.SetFault(address,len,1,Result);
        end;
        $02: begin
          if foundPLC then
            PMelsecPLC[plc].Inputs.SetFault(address,len,1,Result);
        end;
        $03: begin
          if foundPLC then
            PMelsecPLC[plc].Registers.SetFault(address,len,1,Result);
        end;
        $04: begin
          if foundPLC then
            PMelsecPLC[plc].AnalogReg.SetFault(address,len,1,Result);
        end;
      end;
    end;
  end;
end;

function TMelsecTCPDriver.EncodePkg(TagObj: TTagRec; ToWrite: TArrayOfDouble;
  var ResultLen: Integer): BYTES;
var
  i, c, c2, size:LongInt;

  NetworkNumber, PcNumber, IoNumber, ChannelNumber, CpuTimer,   memoriaEscre,
  iMainCommand, iSubCommand, frame, dataLength, valorProc, tamanho: integer;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.
  //
  //Verify if is packet to write data on device that is being encoded.

  //de leitura de valores...
  //read data from slave.
  if ToWrite=nil then begin
    case TagObj.ReadFunction of
      $01: begin
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
        valorProc     := TagObj.Address;
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

        Result[15] := valorProc;
        Result[16] := valorProc shr 8;
        Result[17] := valorProc shr 16;
        Result[18] := $90;
        Result[19] := $01;
        Result[20] := $00;
        Result[21] := 0;
      end;

      $03: begin
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
        valorProc     := TagObj.Address;
        //valorProc     := 305;
        iSubCommand   := 0;
        tamanho       := TagObj.Size;
        //tamanho       := 1;

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

        Result[15] := valorProc;
        Result[16] := valorProc shr 8;
        Result[17] := valorProc shr 16;
        Result[18] := $A8;
        //Result[19] := $01;
        //Result[20] := $00;
        Result[19] := tamanho;
        Result[20] := tamanho shr 8;
        Result[21] := 0;
      end;

      else begin
        SetLength(Result,0);
      end;
    end;

    // Calcula o tamanho do pacote resposta
    // computes the size of the incoming packet.
    case TagObj.ReadFunction of
      $01..$02:
        ResultLen := 9 +(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)<>0,1,0);
      $03..$04:
        ResultLen := 9+(TagObj.Size*2);
      $07:
        ResultLen := 9;
      $08:
        ResultLen := 12;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end else begin
    case TagObj.WriteFunction of
      $05: begin
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
        memoriaEscre  := TagObj.Address;
        valorProc     := Trunc(ToWrite[0]);
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

        Result[15] := memoriaEscre;
        Result[16] := memoriaEscre shr 8;
        Result[17] := memoriaEscre shr 16;
        Result[18] := $90;
        Result[19] := $01;
        Result[20] := byte($00 shr 8);
        if valorProc = 0 then
          Result[21] := byte(0)
        else
          Result[21] := byte(16);
        Result[22] := 0;
      end;

      $10: begin
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
        memoriaEscre  := TagObj.Address;
        valorProc     := Trunc(ToWrite[0]);
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

        Result[15] := memoriaEscre;
        Result[16] := memoriaEscre shr 8;
        Result[17] := memoriaEscre shr 16;
        Result[18] := $A8;
        Result[19] := $01;
        Result[20] := $00;
        Result[21] := valorProc;
        Result[22] := valorProc shr 8;
        Result[23] := 0;
      end;
      else begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    // computes the size of the incoming packet.
    case TagObj.WriteFunction of
      $05,$06,$0F,$10:
        ResultLen := 12;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end;
end;

function TMelsecTCPDriver.RemainingBytes(buffer: BYTES): LongInt;
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

end.
