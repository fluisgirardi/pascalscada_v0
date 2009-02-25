//: Implementação do protocolo ModBus RTU
unit ModBusMasterDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryMananger, crc16utils, hsutils, PLCBlock, PLCString, SyncObjs,
  CrossEvent;

type
  {:
  Estrutura que simula as áreas de memória de um equipamento modbus escravo.

  @member Station Armazena o endereço do equipamento modbus.
  @member Inputs Gerenciador de blocos de memórias não continuas que mapeia
          as entradas do equipamento modbus.
  @member Outputs Gerenciador de blocos de memórias não continuas que mapeia
          as saidas do equipamento modbus.
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
  TModBusPLC = record
    Station:Integer;
    Inputs:TPLCMemoryManager;
    OutPuts:TPLCMemoryManager;
    Registers:TPLCMemoryManager;
    AnalogReg:TPLCMemoryManager;
    Status07Value:Double;
    Status07TimeStamp:TDateTime;
    Status07LastError:TProtocolIOResult;
  end;

  {:
  Classe do driver ModBus RTU
  @author(Fabio Luis Girardi)

  Driver que implementa o protocolo ModBus RTU. Trabalha independentemente do
  driver de porta (camadas abaixo da camada de aplicação).

  Para configurar um tag para usar o ModBus RTU, é necessário configurar as
  seguintes propriedade do tag:

  @unorderedList(
    @item(@bold(PLCStation): Endereço do equipamento modbus.)
    @item(@bold(MemAddress): Endereço da entrada/saida/registrador que se deseja
          lêr/escrever.)
    @item(@bold(MemReadFuntion): Função que será usada para ler o tag. Veja
          tabela abaixo.)
    @item(@bold(MemWriteFuntion): Função que será usada para escrever valores do
          tag. Veja tabela abaixo.)
  )

  Para as propriedades MemReadFunction e MemWriteFunction são aceitos os seguintes
  valores de acordo com a área de memória desejada:

  @table(
    @rowHead( @cell(Área desejada)       @cell(MemReadFunction) @cell(MemReadFunction) )
    @row(     @cell(Entradas digitais)   @cell(2)               @cell(0) )
    @row(     @cell(Saidas digitais)     @cell(1)               @cell(5 (simples), 15 (bloco)) )
    @row(     @cell(Registradores)       @cell(3)               @cell(6 (simples), 16 (bloco)) )
    @row(     @cell(Entradas analógicas) @cell(4)               @cell(0) )
    @row(     @cell(Status equipamento)  @cell(7)               @cell(0) )
  )

  Esta tabela é apenas uma sugetão correta dos parametros para cada área de dados.
  Você pode sem problemas configura um tag dessa maneira:

  @unorderedList(
    @item(@bold(MemAddress): 0;)
    @item(@bold(MemReadFuntion): 2;)
    @item(@bold(MemWriteFuntion): 5;)
  )

  O que irá acontecer é que quando o tag for lido seu valor tem origem da
  entrada número 0, mas quando for escrito este tag irá ligar/desligar
  a saida 0 (depende do valor escrito para ligar/desligar);

  @bold(Quando for usado um valor de escrita simples(funções 5 e 6) em um tag
  bloco, e todo o bloco for escrito de uma única vez, o driver fará uma emulação
  escrevendo elemento a elemento usando a função definida, mas não é recomendado.

  É necessário que você conheças as funções ModBus que seu equipamento suporta.)

  }
  TModBusMasterDriver = class(TProtocolDriver)
  private
    //PCheckCRC:Boolean;
    PReadAnyAlways:Boolean;
    POutputMaxHole:Cardinal;
    PInputMaxHole:Cardinal;
    PRegistersMaxHole:Cardinal;
    PModbusPLC:array of TModBusPLC;
    function GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:Integer):Boolean;
    procedure SetOutputMaxHole(v:Cardinal);
    procedure SetInputMaxHole(v:Cardinal);
    procedure SetRegisterMaxHole(v:Cardinal);
    procedure BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);

    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
    function  DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult;
  protected
    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoTagChange)
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;

    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;

    //: @seealso(TProtocolDriver.DoWrite)
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: @seealso(TProtocolDriver.SizeOfTag)
    function  SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE; override;
  published
    //: Booleano que diz se o driver deve ler algum tag a todo scan.
    property ReadAnyAlways:Boolean read PReadAnyAlways write PReadAnyAlways default true;
    {:
    Informa quantas saidas podem ficar sem serem declaradas para manter um bloco
    de saidas continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    property OutputMaxHole:Cardinal read POutputMaxHole write SetOutputMaxHole default 50;
    {:
    Informa quantas entradas podem ficar sem serem declaradas para manter um bloco
    continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    property InputMaxHole:Cardinal read PInputMaxHole write SetInputMaxHole default 50;
    {:
    Informa quantos registradores podem ficar sem serem declaradas para manter
    um bloco continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    property RegisterMaxHole:Cardinal read PRegistersMaxHole write SetRegisterMaxHole default 10;
  end;

implementation

uses Math;

constructor TModBusMasterDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  POutputMaxHole := 50;
  PInputMaxHole := 50;
  PRegistersMaxHole := 10;
  PReadAnyAlways := true;
  SetLength(PModbusPLC,0);
end;

destructor TModBusMasterDriver.Destroy;
var
  plc:Integer;
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


function TModBusMasterDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:Integer):Boolean;
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
    Size := 1;
    RegType := TPLCTagNumber(TagObj).MemReadFunction;
    ScanTime := TPLCTagNumber(TagObj).RefreshTime;
    Result := found;
  end;

  //Tag Bloco
  if (not found) and ((TagObj is TPLCBlock) or (TagObj is TPLCString)) then begin
    found := true;
    Station := TPLCBlock(TagObj).PLCStation;
    Address := TPLCBlock(TagObj).MemAddress;
    Size := TPLCBlock(TagObj).Size;
    RegType := TPLCBlock(TagObj).MemReadFunction;
    ScanTime := TPLCBlock(TagObj).RefreshTime;
    Result := found;
  end;
end;

procedure TModBusMasterDriver.DoAddTag(TagObj:TTag);
var
  station, mem, size, memtype, scantime:Integer;
  found:boolean;
  plc:integer;
begin
  //Recupera as informações do tag;
  station:=0;
  mem:=0;
  size:=0;
  memtype:=0;
  scantime:=0;

  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória
    if station in [1..247] then begin
      found := false;
      for plc:=0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //precisa dizer mais??? se nao encontrou o plc, adiciona!!
      if not found then begin
        plc:=length(PModbusPLC);
        SetLength(PModbusPLC,plc+1);
        PModbusPLC[plc].Station := station;
        PModbusPLC[plc].Inputs := TPLCMemoryManager.Create(ntBinary);
        PModbusPLC[plc].Inputs.MaxBlockItems := 2016;
        PModbusPLC[plc].Inputs.MaxHole := PInputMaxHole;
        PModbusPLC[plc].OutPuts := TPLCMemoryManager.Create(ntBinary);
        PModbusPLC[plc].OutPuts.MaxBlockItems := 2016;
        PModbusPLC[plc].OutPuts.MaxHole := POutputMaxHole;
        PModbusPLC[plc].Registers := TPLCMemoryManager.Create(ntWORD);
        PModbusPLC[plc].Registers.MaxBlockItems := 100;
        PModbusPLC[plc].Registers.MaxHole := PRegistersMaxHole;
        PModbusPLC[plc].AnalogReg := TPLCMemoryManager.Create(ntWORD);
        PModbusPLC[plc].AnalogReg.MaxBlockItems := 100;
        PModbusPLC[plc].AnalogReg.MaxHole := PRegistersMaxHole;
      end;

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
  inherited DoAddTag(TagObj);
end;

procedure TModBusMasterDriver.DoDelTag(TagObj:TTag);
var
  station, mem, size, memtype, scantime:Integer;
  found:boolean;
  plc:Integer;
begin
  //Recupera as informações do tag;
  station:=0;
  mem:=0;
  size:=0;
  memtype:=0;
  scantime:=0;
  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória
    if station in [1..255] then begin
      found := false;
      for plc:=0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
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

procedure TModBusMasterDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
var
  station, mem, size, memtype, scantime:Integer;
  found:boolean;
  plc:Integer;
begin
  //Recupera as informações do tag;
  station:=0;
  mem:=0;
  size:=0;
  memtype:=0;
  scantime:=0;
  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if not found then exit;

  case Change of
    tcPLCStation: begin
      //procura pelo plc antigo
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = oldValue then begin
          found := true;
          break;
        end;

      //se essa faixa de enderecos ja estava associada a um plc
      //remove do plc antigo antes de registrar com o novo plc
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

        //se o plc nao tem mais nenhuma memoria pra ler, elimina ele do scan;
        if (Length(PModbusPLC[plc].Inputs.Blocks)=0) and
           (Length(PModbusPLC[plc].OutPuts.Blocks)=0) and
           (Length(PModbusPLC[plc].Registers.Blocks)=0) then begin
          PModbusPLC[plc] := PModbusPLC[High(PModbusPLC)];
          SetLength(PModbusPLC,High(PModbusPLC));
        end;
      end;

      //se o novo endereco o plc esta fora de faixa sai e nao adiciona ao scan
      if not newValue in [1..255] then exit;

      //procura pelo novo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = newValue then begin
          found := true;
          break;
        end;

      //se nao encontrou, adiciona!!
      if not found then begin
        plc := Length(PModbusPLC);
        SetLength(PModbusPLC,plc+1);
        PModbusPLC[plc].Station := newValue;
        PModbusPLC[plc].Inputs := TPLCMemoryManager.Create(ntBinary);
        PModbusPLC[plc].Inputs.MaxBlockItems := 2016;
        PModbusPLC[plc].Inputs.MaxHole := PInputMaxHole;
        PModbusPLC[plc].OutPuts := TPLCMemoryManager.Create(ntBinary);
        PModbusPLC[plc].OutPuts.MaxBlockItems := 2016;
        PModbusPLC[plc].OutPuts.MaxHole := POutputMaxHole;
        PModbusPLC[plc].Registers := TPLCMemoryManager.Create(ntWORD);
        PModbusPLC[plc].Registers.MaxBlockItems := 100;
        PModbusPLC[plc].Registers.MaxHole := PRegistersMaxHole;
        PModbusPLC[plc].AnalogReg := TPLCMemoryManager.Create(ntWORD);
        PModbusPLC[plc].AnalogReg.MaxBlockItems := 100;
        PModbusPLC[plc].AnalogReg.MaxHole := PRegistersMaxHole;
      end;
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
    tcMemAddress: begin
      //procura pelo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //adiciona caso encontre o plc, caso contrario faz nada...
      if found then begin
        case memtype of
          1: begin
            PModbusPLC[plc].OutPuts.AddAddress(newValue,size,1,scantime);
            PModbusPLC[plc].OutPuts.RemoveAddress(oldValue,size,1);
          end;
          2: begin
            PModbusPLC[plc].Inputs.AddAddress(newValue,size,1,scantime);
            PModbusPLC[plc].Inputs.RemoveAddress(oldValue,size,1);
          end;
          3: begin
            PModbusPLC[plc].Registers.AddAddress(newValue,size,1,scantime);
            PModbusPLC[plc].Registers.RemoveAddress(oldValue,size,1);
          end;
          4: begin
            PModbusPLC[plc].AnalogReg.AddAddress(newValue,size,1,scantime);
            PModbusPLC[plc].AnalogReg.RemoveAddress(oldValue,size,1);
          end;
        end;
      end;
    end;
    tcMemReadFunction: begin
      //procura pelo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //adiciona caso encontre o plc, caso contrario faz nada...
      if found then begin
        //remove do tipo antigo...
        case oldValue of
          1:
            PModbusPLC[plc].OutPuts.RemoveAddress(mem,size,1);
          2:
            PModbusPLC[plc].Inputs.RemoveAddress(mem,size,1);
          3:
            PModbusPLC[plc].Registers.RemoveAddress(mem,size,1);
          4:
            PModbusPLC[plc].AnalogReg.RemoveAddress(mem,size,1);
        end;
        //adiciona no novo tipo...
        case newValue of
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
    end;
    tcScanTime : begin
      //se o scantime que está setado no tag
      //é menor que o novo valor, sai, pq
      //os blocos de memoria não aceitam valores de
      //scan maiores que os que já foram setados.
      if scantime<newValue then exit;
      //procura pelo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //adiciona caso encontre o plc, caso contrario do nothing :D...
      if found then begin
        case memtype of
          1: begin
            PModbusPLC[plc].OutPuts.AddAddress(mem,size,1,newValue);
            PModbusPLC[plc].OutPuts.RemoveAddress(mem,size,1);
          end;
          2: begin
            PModbusPLC[plc].Inputs.AddAddress(mem,size,1,newValue);
            PModbusPLC[plc].Inputs.RemoveAddress(mem,size,1);
          end;
          3: begin
            PModbusPLC[plc].Registers.AddAddress(mem,size,1,newValue);
            PModbusPLC[plc].Registers.RemoveAddress(mem,size,1);
          end;
          4: begin
            PModbusPLC[plc].AnalogReg.AddAddress(mem,size,1,newValue);
            PModbusPLC[plc].AnalogReg.RemoveAddress(mem,size,1);
          end;
        end;
      end;
    end;
    tcSize: begin
      //procura pelo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //adiciona caso encontre o plc, caso contrario do nothing :D...
      if found then begin
        case memtype of
          1: begin
            PModbusPLC[plc].OutPuts.AddAddress(mem,newValue,1,scantime);
            PModbusPLC[plc].OutPuts.RemoveAddress(mem,oldValue,1);
          end;
          2: begin
            PModbusPLC[plc].Inputs.AddAddress(mem,newValue,1,scantime);
            PModbusPLC[plc].Inputs.RemoveAddress(mem,oldValue,1);
          end;
          3: begin
            PModbusPLC[plc].Registers.AddAddress(mem,newValue,1,scantime);
            PModbusPLC[plc].Registers.RemoveAddress(mem,oldValue,1);
          end;
          4: begin
            PModbusPLC[plc].AnalogReg.AddAddress(mem,newValue,1,scantime);
            PModbusPLC[plc].AnalogReg.RemoveAddress(mem,oldValue,1);
          end;
        end;
      end;
    end;
  end;
end;

function  TModBusMasterDriver.SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE;
var
  FunctionCode:Cardinal;
begin
  FunctionCode := 0;
  if (Tag is TPLCTagNumber) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(Tag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(Tag).MemReadFunction;
  end;

  //Tag Bloco
  if ((Tag is TPLCBlock) or (Tag is TPLCString)) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(Tag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(Tag).MemReadFunction;
  end;


  //retorna o tamanho em bits dos registradores lidos/escritos por
  //cada tipo de função de leitura/escrita
  case FunctionCode of
    1,2,5,15:
      Result := 1;
    3,4,6,16:
      Result := 16;
    else
      Result := 0;
  end;
end;

function  TModBusMasterDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
var
  i, c, c2:Integer;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.

  //de leitura de valores...
  if ToWrite=nil then begin
    case TagObj.ReadFunction of
      $01,$02,$03,$04: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := TagObj.ReadFunction and $FF;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (TagObj.Size and $FF00) shr 8;
        Result[5] := TagObj.Size and $FF;
        // Calcula CRC
        Calcul_crc(Result);
      end;

      $07: begin
        // Lê o Status
        SetLength(Result,4);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $07;
        // Calcula o CRC
        Calcul_crc(Result);
      end;

      $08: begin
        // Teste de Linha...
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $08;
        Result[2] := 0;
        Result[3] := 0;
        Result[4] := 0;
        Result[5] := 0;
        Calcul_crc(Result);
      end;
      else begin
        SetLength(Result,0);
      end;
    end;

    // Calcula o tamanho do pacote resposta
    case TagObj.ReadFunction of
      $01..$02:
        ResultLen := 5 +(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)<>0,1,0);
      $03..$04:
        ResultLen := 5+(TagObj.Size*2);
      $07:
        ResultLen := 5;
      $08:
        ResultLen := 8;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end else begin
    case TagObj.WriteFunction of
      $05: begin
        //escreve uma saida...
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $05;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;

        if (ToWrite[0]=0) then begin
           Result[4] := $00;
           Result[5] := $00;
        end else begin
           Result[4] := $FF;
           Result[5] := $00;
        end;
        // Calcula CRC
        Calcul_crc(Result);
      end;

      $06: begin
        // escreve 1 registro
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $06;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (FloatToInteger(ToWrite[0]) and $FF00) shr 8;
        Result[5] := FloatToInteger(ToWrite[0]) and $FF;
        // Calcula o CRC
        Calcul_crc(Result);
      end;

      $0F: begin
        //Num de saidas em bytes + 9 bytes fixos.
        SetLength(Result,(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $0F;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[4] := (Min(TagObj.Size,Length(ToWrite)) and $FF00) shr 8;
        Result[5] := Min(TagObj.Size,Length(ToWrite)) and $FF;
        Result[6] := (TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0);

        i := 0;
        c := 0;
        c2:= 7;
        Result[7] := 0;

        for c := 0 to Min(TagObj.Size,Length(ToWrite))-1 do begin
          if ToWrite[c]<>0 then begin
            Result[c2] := Result[c2]+ (1 shl i);
          end;

          inc(i);
          if i>7 then begin
            i:=0;
            inc(c2);
            Result[c2] := 0;
          end;
        end;

        // Calcula CRC
        Calcul_crc(Result);
      end;

      $10: begin
        // Escreve X bytes
        SetLength(Result,(TagObj.Size*2)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $10;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[4] := ((TagObj.Size and $FF00) shr 8);
        Result[5] := TagObj.Size and $FF;
        Result[6] := (TagObj.Size*2) and $FF;
        i := 0;
        while (i<TagObj.Size) do begin
            Result[7+i*2] := ((FloatToInteger(ToWrite[i]) and $FF00) shr 8);
            Result[8+i*2] := FloatToInteger(ToWrite[i]) and $FF;
            inc(i);
        end;
        // Calcula o CRC
        Calcul_crc(Result);
      end;
      else begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    case TagObj.WriteFunction of
      $05,$06,$0F,$10:
        ResultLen := 8;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end;
end;

function TModBusMasterDriver.DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult;
var
   i,c,c2,plc:integer;
   address, len:Cardinal;
   foundPLC:Boolean;
   aux:TPLCMemoryManager;
begin
  //se algumas das IOs falhou,
  if (pkg.ReadIOResult<>iorOK) or (pkg.WriteIOResult<>iorOK) then begin
    Result := ioCommError;
    exit;
  end;
  
  //se o endereco retornado nao conferem com o selecionado...
  if (pkg.BufferToWrite[0]<>pkg.BufferToRead[0]) then begin
    Result := ioCommError;
    exit;
  end;

  //se a checagem crc nao bate, sai
  if (not Test_crc(pkg.BufferToWrite)) or (not Test_crc(pkg.BufferToRead)) then begin
    Result := ioCommError;
    exit;
  end;

  //procura o plc
  foundPLC := false;
  for plc := 0 to High(PModbusPLC) do
    if PModbusPLC[plc].Station = pkg.BufferToWrite[0] then begin
      foundPLC := true;
      break;
    end;

  //se nao encontrou o plc nos blocos de memória sai...
  //if not found then begin
    //Result := ioDriverError;
    //exit;
  //end;

  //comeca a decodificar o pacote...

  //leitura de bits das entradas ou saidas
  case pkg.BufferToRead[1] of
    $01,$02: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$01 then
          aux := PModbusPLC[plc].OutPuts
        else
          aux := PModbusPLC[plc].Inputs;
      end;

      address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
      SetLength(Values,len);

      i := 0;
      c := 0;
      c2:= 3;
      while c2<(Length(pkg.BufferToRead)-2) do begin
        if (c=8) then begin
          c:=0;
          inc(c2);
        end;
        Values[i]:=IfThen(((Integer(pkg.BufferToRead[c2]) and (1 shl c))=(1 shl c)),1,0);
        inc(i);
        inc(c);
      end;
      if foundPLC then
        aux.SetValues(address,len,1,Values);
      Result := ioOk;
    end;

    //leitura de words dos registradores ou das entradas analogicas
    $03,$04: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$03 then
          aux := PModbusPLC[plc].Registers
        else
          aux := PModbusPLC[plc].AnalogReg;
      end;

      address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
      SetLength(Values,len);

      // data are ok
      for i:=0 to Len-1 do begin
        Values[i]:=(Integer(pkg.BufferToRead[3+(i*2)]) shl 8) + Integer(pkg.BufferToRead[4+i*2]);
      end;
      if foundPLC then
        aux.SetValues(address,len,1,Values);
      Result := ioOk;
    end;

    $05: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      SetLength(values,1);

      if (pkg.BufferToWrite[4]=0) and (pkg.BufferToWrite[5]=0) then
         values[0] := 0
      else
         values[0] := 1;

      if foundPLC then
        PModbusPLC[plc].OutPuts.SetValues(address,1,1,values);

      Result := ioOk;
    end;
    $06: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      SetLength(values,1);

      values[0] := pkg.BufferToWrite[4]*256+pkg.BufferToWrite[5];

      if foundPLC then
        PModbusPLC[plc].Registers.SetValues(address,1,1,values);

      Result := ioOk;
    end;
    $07: begin
      if foundPLC then begin
         PModbusPLC[plc].Status07Value :=Integer(pkg.BufferToRead[2]);
         PModbusPLC[plc].Status07TimeStamp := Now;
         PModbusPLC[plc].Status07LastError := ioOk;
      end;
      Result := ioOk;
    end;
    $08:
      Result := ioOk;
    $0F: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];

      SetLength(values,len);

      i := 0;
      c := 0;
      c2:= 7;
      while c2<(Length(pkg.BufferToWrite)-2) do begin
        if (c=8) then begin
          c:=0;
          inc(c2);
        end;
        Values[i]:=IfThen(((Integer(pkg.BufferToWrite[c2]) and (1 shl c))=(1 shl c)),1,0);
        inc(i);
        inc(c);
      end;

      if foundPLC then
        PModbusPLC[plc].OutPuts.SetValues(address,len,1,values);
      Result := ioOk;
    end;
    $10: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];

      SetLength(values,len);

      i := 0;
      while (i<len) do begin
         values[i] := pkg.BufferToWrite[7+i*2]*256 + pkg.BufferToWrite[8+i*2];
         inc(i);
      end;

      if foundPLC then
        PModbusPLC[plc].Registers.SetValues(address,len,1,values);
      Result := ioOk;
    end;
    else begin
      case pkg.BufferToRead[1] of
        $81:
          Result := ioIllegalFunction;
        $82:
          Result := ioIllegalRegAddress;
        $83:
          Result := ioIllegalValue;
        $84,$85,$86,$87,$88:
          Result := ioPLCError;
        else
          Result := ioCommError;
      end;
      case pkg.BufferToWrite[1] of
        $01: begin
          address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
          len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
          if foundPLC then
            PModbusPLC[plc].OutPuts.SetFault(address,len,1,Result);
        end;
        $02: begin
          address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
          len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
          if foundPLC then
            PModbusPLC[plc].Inputs.SetFault(address,len,1,Result);
        end;
        $03: begin
          address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
          len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
          if foundPLC then
            PModbusPLC[plc].Registers.SetFault(address,len,1,Result);
        end;
        $04: begin
          address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
          len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
          if foundPLC then
            PModbusPLC[plc].AnalogReg.SetFault(address,len,1,Result);
        end;
      end;
    end;
  end;
end;

procedure TModBusMasterDriver.DoScanRead(Sender:TObject);
var
  plc,block:Integer;
  done,first:Boolean;
  minScan:Int64;
  lastType:Integer;
  lastBlock:TRegisterRange;
  lastPLC:Integer;
  tr:TTagRec;
  values:TArrayOfDouble;
begin
  try
    minScan := -1;
    first:=true;
    done := false;
    if (csDesigning in ComponentState) then begin
      Sleep(1);
      exit;
    end;
    for plc:= 0 to High(PModbusPLC) do begin
      for block := 0 to High(PModBusPLC[plc].Outputs.Blocks) do
        if PModBusPLC[plc].Outputs.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PModBusPLC[plc].Station,1,PModBusPLC[plc].Outputs.Blocks[block].AddressStart,PModBusPLC[plc].Outputs.Blocks[block].Size, tr);
          DoRead(tr,values,false);
        end else begin
          if first then begin
            lastType := 1;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Outputs.Blocks[block];
            minScan := PModBusPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PModBusPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 1;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Outputs.Blocks[block];
            minScan := PModBusPLC[plc].Outputs.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      for block := 0 to High(PModBusPLC[plc].Inputs.Blocks) do
        if PModBusPLC[plc].Inputs.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PModBusPLC[plc].Station,2,PModBusPLC[plc].Inputs.Blocks[block].AddressStart,PModBusPLC[plc].Inputs.Blocks[block].Size, tr);
          DoRead(tr,values,false);
        end else begin
          if first then begin
            lastType := 2;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Inputs.Blocks[block];
            minScan := PModBusPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PModBusPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 2;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Inputs.Blocks[block];
            minScan := PModBusPLC[plc].Inputs.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      for block := 0 to High(PModBusPLC[plc].Registers.Blocks) do
        if PModBusPLC[plc].Registers.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PModBusPLC[plc].Station,3,PModBusPLC[plc].Registers.Blocks[block].AddressStart,PModBusPLC[plc].Registers.Blocks[block].Size, tr);
          DoRead(tr,values,false);
        end else begin
          if first then begin
            lastType := 3;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Registers.Blocks[block];
            minScan := PModBusPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PModBusPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 3;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].Registers.Blocks[block];
            minScan := PModBusPLC[plc].Registers.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;

      for block := 0 to High(PModBusPLC[plc].AnalogReg.Blocks) do
        if PModBusPLC[plc].AnalogReg.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PModBusPLC[plc].Station,4,PModBusPLC[plc].AnalogReg.Blocks[block].AddressStart,PModBusPLC[plc].AnalogReg.Blocks[block].Size, tr);
          DoRead(tr,values,false);
        end else begin
          if first then begin
            lastType := 4;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].AnalogReg.Blocks[block];
            minScan := PModBusPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate;
            first:=False;
          end;
          if PModBusPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate>minScan then begin
            lastType := 4;
            lastPLC := PModBusPLC[plc].Station;
            lastBlock := PModBusPLC[plc].AnalogReg.Blocks[block];
            minScan := PModBusPLC[plc].AnalogReg.Blocks[block].MilisecondsFromLastUpdate;
          end;
        end;
    end;
    //se nao fez leitura de nenhum bloco
    //faz atualiza o bloco que esta quase vencendo
    //o tempo de scan...
    if (PReadAnyAlways) and ((not done) and (not first)) then begin
      //compila o bloco do mais necessitado;
      BuildTagRec(lastPLC,lastType,lastBlock.AddressStart,lastBlock.Size, tr);
      DoRead(tr,values,false);
    end else
      Sleep(1);
  finally
    SetLength(values,0);
  end;
end;

procedure TModBusMasterDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
var
  res,plc,c:Integer;
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
    values.ValuesTimestamp := Now;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    values.LastQueryResult := ioDriverError;
    SetLength(values.Values,0);
    exit;
  end;

  case TagObj.ReadFunction of
    $01:
      res := PModbusPLC[plc].OutPuts.GetValues(TagObj.Address,TagObj.Size,1,values.Values);
    $02:
      res := PModbusPLC[plc].Inputs.GetValues(TagObj.Address,TagObj.Size,1,values.Values);
    $03:
      res := PModbusPLC[plc].Registers.GetValues(TagObj.Address,TagObj.Size,1,values.Values);
    $04:
      res := PModbusPLC[plc].AnalogReg.GetValues(TagObj.Address,TagObj.Size,1,values.Values)
  end;

  if res<0 then begin
    values.ValuesTimestamp := Now;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    values.LastQueryResult := ioDriverError;
    SetLength(values.Values,0);
    exit;
  end;

  values.ValuesTimestamp := Now;
  values.ReadsOK := 1;
  values.ReadFaults := 0;
  values.LastQueryResult := ioOk;
end;

function  TModBusMasterDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  IOResult:TIOPacket;
  pkg:BYTES;
  rl:Integer;
  res:Integer;
  tempValues:TArrayOfDouble;
begin
  try
    pkg := EncodePkg(tagrec,values,rl);
    if PCommPort<>nil then begin

      res := PCommPort.IOCommandSync(iocWriteRead,pkg,rl,Length(pkg),DriverID,5,CommPortCallBack,false,nil,@IOResult);

      if (res<>0) then
        Result := DecodePkg(IOResult,tempValues);

    end else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(tempValues,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
  end;
end;

function  TModBusMasterDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  Event:TCrossEvent;
  IOResult:TIOPacket;
  pkg:BYTES;
  rl:Integer;
  res:Integer;
begin
  try
    Event:=TCrossEvent.Create(nil,true,false,'');
    pkg := EncodePkg(tagrec,nil,rl);
    if PCommPort<>nil then begin
      Event.ResetEvent;
      if Sync then
        res := PCommPort.IOCommandSync(iocWriteRead,pkg,rl,Length(pkg),DriverID,5,CommPortCallBack,false,nil,@IOResult)
      else
        res := PCommPort.IOCommandASync(iocWriteRead,pkg,rl,Length(pkg),DriverID,5,CommPortCallBack,false,Event,@IOResult);

      if (res<>0) and (Sync or (Event.WaitFor(120000)=wrSignaled)) then
        Result := DecodePkg(IOResult,values);

    end else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
    Event.Destroy;
  end;
end;

procedure TModBusMasterDriver.SetOutputMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].OutPuts.MaxHole := v;
end;

procedure TModBusMasterDriver.SetInputMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Inputs.MaxHole := v;
end;

procedure TModBusMasterDriver.SetRegisterMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Registers.MaxHole := v;
end;

procedure TModBusMasterDriver.BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);
begin
  tr.Station := plc;
  tr.Address := startaddress;
  tr.Size := size;
  tr.ReadFunction := func;
end;

end.
