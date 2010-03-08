{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  @abstract(Implementa a base para os drivers ModBus RTU e ModBus TCP.)
}
unit ModBusDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString, SyncObjs, uTagBuilder,
  CrossEvent{$IFNDEF FPC}, Windows{$ENDIF};

type
  {:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

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
  @abstract(Classe base do driver ModBus (RTU e TCP))
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Driver que implementa a base do protocolo ModBus. Trabalha independentemente do
  driver de porta (camadas abaixo da camada de aplicação.)

  Para configurar um tag para usar o ModBus, é necessário configurar as
  seguintes propriedades do tag:

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

  Esta tabela é apenas uma sugestão correta dos parametros para cada área de dados.
  Você pode sem problemas configurar um tag dessa maneira:

  @unorderedList(
    @item(@bold(MemAddress): 0;)
    @item(@bold(MemReadFuntion): 2;)
    @item(@bold(MemWriteFuntion): 5;)
  )

  O que vai acontecer é que quando o tag for lido seu valor tem origem da
  entrada número 0, mas quando for escrito este tag irá ligar/desligar
  a saida 0 (depende do valor escrito para ligar/desligar);

  @bold(Quando for usado um valor de escrita simples(funções 5 e 6) em um tag
  bloco, e todo o bloco for escrito de uma única vez, o driver fará uma emulação
  escrevendo elemento a elemento usando a função definida, mas não é recomendado.

  É necessário que você conheça as funções ModBus que seu equipamento suporta.)

  @seealso(TModBusRTUDriver)
  @seealso(TModBusTCPDriver)
  }
  TModBusDriver = class(TProtocolDriver)
  private
    function BuildItemName(nameprefix:String; ZeroFill:Boolean; index, NumZeros:Integer):String;
    function SelectedReadFuntion(dlg:TfrmModbusTagBuilder):Integer;
    function SelectedWriteFuntion(dlg:TfrmModbusTagBuilder):Integer;
    function SeekFirstItem(LastItem:TTagNamesItemEditor):TTagNamesItemEditor;
  protected
    FReadEvent:TCrossEvent;
    POutputMaxHole:Cardinal;
    PInputMaxHole:Cardinal;
    PRegistersMaxHole:Cardinal;
    PModbusPLC:array of TModBusPLC;
    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:Integer):Boolean;
    procedure SetOutputMaxHole(v:Cardinal);
    procedure SetInputMaxHole(v:Cardinal);
    procedure SetRegisterMaxHole(v:Cardinal);
    procedure BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);

    procedure CancelPendingActions; override;

    //: Cria um pacote modbus
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES; virtual;
    //: Extrai os dados de um pacote modbus
    function  DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult; virtual; 

    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoTagChange)
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;

    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;

    //: @seealso(TProtocolDriver.DoWrite)
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
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
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
    //: @seealso(TProtocolDriver.SizeOfTag)
    function  SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE; override;
  end;

implementation

uses Dialogs, Controls, PLCBlockElement, StrUtils;


constructor TModBusDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FReadEvent := TCrossEvent.Create(nil,true,false,'');
  POutputMaxHole := 50;
  PInputMaxHole := 50;
  PRegistersMaxHole := 10;
  PReadSomethingAlways := true;
  SetLength(PModbusPLC,0);
end;

destructor TModBusDriver.Destroy;
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
  FReadEvent.Destroy;
end;


function TModBusDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:Integer):Boolean;
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
  if (not found) and (TagObj is TPLCBlock) then begin
    found := true;
    Station := TPLCBlock(TagObj).PLCStation;
    Address := TPLCBlock(TagObj).MemAddress;
    Size := TPLCBlock(TagObj).Size;
    RegType := TPLCBlock(TagObj).MemReadFunction;
    ScanTime := TPLCBlock(TagObj).RefreshTime;
    Result := found;
  end;

  //Tag Bloco
  if (not found) and (TagObj is TPLCString) then begin
    found := true;
    Station := TPLCString(TagObj).PLCStation;
    Address := TPLCString(TagObj).MemAddress;
    Size := TPLCString(TagObj).Size;
    RegType := TPLCString(TagObj).MemReadFunction;
    ScanTime := TPLCString(TagObj).RefreshTime;
    Result := found;
  end;
end;

procedure TModBusDriver.DoAddTag(TagObj:TTag);
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
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória.
    if station in [1..255] then begin
      found := false;
      for plc:=0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //se nao encontrou o plc, adiciona!
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

procedure TModBusDriver.DoDelTag(TagObj:TTag);
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
    //se o endereco do plc esta numa faixa válida procura nos blocos de memória.
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

procedure TModBusDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
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

      //se essa faixa de enderecos já estava associada a um plc
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

        //se o plc não tem mais nenhuma memoria pra ler, elimina ele do scan;
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

      //se nao encontrou, adiciona!
      if not found then begin
        plc := Length(PModbusPLC);
        SetLength(PModbusPLC,plc+1);
        PModbusPLC[plc].Station := newValue;
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
      //os blocos de memória não aceitam valores de
      //scan maiores que os que já foram setados.
      if scantime<newValue then exit;
      //procura pelo plc
      for plc:= 0 to High(PModbusPLC) do
        if PModbusPLC[plc].Station = station then begin
          found := true;
          break;
        end;
      //adiciona caso encontre o plc.
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
      //adiciona caso encontre o plc
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

procedure TModBusDriver.OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  dlg:TfrmModbusTagBuilder;
  tplc:TPLCTagNumber;
  tstr:TPLCString;
  tblk:TPLCBlock;
  c, CurMemtAdress, nameitem:Integer;
  confItem:TTagNamesItemEditor;

begin
  if [csDesigning]*ComponentState=[] then exit;
  dlg:=TfrmModbusTagBuilder.Create(nil);
  try
    if dlg.ShowModal=mrOk then begin
      if Assigned(InsertHook) and Assigned(CreateProc) then begin

        //plc number e string
        if dlg.optPLCTagNumber.Checked or dlg.optPLCString.Checked then begin
          CurMemtAdress:=dlg.FirstMemAddress.Value;
          nameitem:=1;
          confItem:=SeekFirstItem(dlg.CurItem);
          if confItem=nil then exit;

          c:=1;
          while c<=dlg.MemCount.Value do begin
            if Trim(confItem.Nome.Text)<>'' then begin
              if dlg.optPLCTagNumber.Checked then begin
                tplc := TPLCTagNumber(CreateProc(TPLCTagNumber));
                tplc.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tplc.MemAddress := CurMemtAdress;
                tplc.MemReadFunction  := SelectedReadFuntion(dlg);
                tplc.MemWriteFunction := SelectedWriteFuntion(dlg);
                tplc.PLCStation:=dlg.StationAddress.Value;
                tplc.RefreshTime:=confItem.Scan.Value;
                tplc.ProtocolDriver := Self;
                InsertHook(tplc);
                inc(CurMemtAdress);
              end else begin
                tstr := TPLCString(CreateProc(TPLCString));
                tstr.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tstr.MemAddress := CurMemtAdress;
                tstr.MemReadFunction  := SelectedReadFuntion(dlg);
                tstr.MemWriteFunction := SelectedWriteFuntion(dlg);
                tstr.PLCStation:=dlg.StationAddress.Value;
                tstr.RefreshTime:=confItem.Scan.Value;
                tstr.ProtocolDriver := Self;
                if dlg.optSTR_C.Checked then
                  tstr.StringType:=stC
                else
                  tstr.StringType:=stSIEMENS;
                tstr.ByteSize:=Byte(dlg.ByteSize.Value);
                tstr.StringSize:=dlg.MaxStringSize.Value;
                InsertHook(tstr);
                inc(CurMemtAdress,tstr.Size);
              end;
            end;

            if (Trim(confItem.Nome.Text)<>'') or confItem.CountEmpty.Checked  then
              inc(c);

            if confItem.Next=nil then begin
              confItem:=SeekFirstItem(dlg.CurItem);
              inc(nameitem);
            end else
              confItem:=TTagNamesItemEditor(confItem.Next);
          end;
        end;

        //bloco
        if dlg.optPLCBlock.Checked then begin

        end;
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;

function TModBusDriver.BuildItemName(nameprefix:String; ZeroFill:Boolean; index, NumZeros:Integer):String;
var
  idxfmt, numfmt:String;
  c:Integer;
begin
  if ZeroFill then begin
    idxfmt:='0';
    for c:= 2 to NumZeros do
      idxfmt:=idxfmt+'0';
  end
  else
    idxfmt:='#0';

  numfmt:=FormatFloat(idxfmt,index);

  if Pos('%s',nameprefix)=0 then
    Result:=nameprefix+numfmt
  else
    Result := Format(nameprefix,[numfmt]);
end;

function TModBusDriver.SelectedReadFuntion(dlg:TfrmModbusTagBuilder):Integer;
begin
  Result:=0;
  if dlg.Type1.Checked then
    Result:=1;
  if dlg.Type2.Checked then
    Result:=2;
  if dlg.Type3.Checked then
    Result:=3;
  if dlg.Type4.Checked then
    Result:=4;
end;

function TModBusDriver.SelectedWriteFuntion(dlg:TfrmModbusTagBuilder):Integer;
begin
  Result := 0;
  case SelectedReadFuntion(dlg) of
    1: begin
      if dlg.optSimpleFunctions.Checked then
        Result:=5
      else
        Result := 15;
    end;
    3: begin
      if dlg.optSimpleFunctions.Checked then
        Result := 6
      else
        Result := 16;
    end;
  end;
end;

function TModBusDriver.SeekFirstItem(LastItem:TTagNamesItemEditor):TTagNamesItemEditor;
begin
  Result := LastItem;
  while (Result<>nil) and (Result.Prior<>nil) do
    Result:=TTagNamesItemEditor(Result.Prior);
end;


function  TModBusDriver.SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE;
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

  //Tag Bloco
  if (Tag is TPLCBlock) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(Tag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(Tag).MemReadFunction;
  end;

  //tag string
  if (Tag is TPLCString) then begin
    if (isWrite) then
      FunctionCode := TPLCString(Tag).MemWriteFunction
    else
      FunctionCode := TPLCString(Tag).MemReadFunction;
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

function  TModBusDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
begin
end;

function TModBusDriver.DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=ioDriverError
end;

procedure TModBusDriver.DoScanRead(Sender:TObject; var NeedSleep:Integer);
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
    if ([csDesigning, csDestroying]*ComponentState<>[]) then begin
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF}
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
    if (PReadSomethingAlways) and (Length(PModbusPLC)>0) and ((not done) and (not first)) then begin
      //compila o bloco do mais necessitado;
      BuildTagRec(lastPLC,lastType,lastBlock.AddressStart,lastBlock.Size, tr);
      DoRead(tr,values,false);
    end else
      NeedSleep := 1;
  finally
    SetLength(values,0);
  end;
end;

procedure TModBusDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
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

function  TModBusDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
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

function  TModBusDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  IOResult:TIOPacket;
  pkg:BYTES;
  rl:Integer;
  res:Integer;
begin
  try
    pkg := EncodePkg(tagrec,nil,rl);
    if PCommPort<>nil then begin
      if Sync then
        res := PCommPort.IOCommandSync(iocWriteRead,pkg,rl,Length(pkg),DriverID,5,CommPortCallBack,false,nil,@IOResult)
      else begin
        FReadEvent.ResetEvent;
        res := PCommPort.IOCommandASync(iocWriteRead,pkg,rl,Length(pkg),DriverID,5,CommPortCallBack,false,FReadEvent,@IOResult);
      end;

      if (res<>0) and (Sync or (FReadEvent.WaitFor($FFFFFFFF)=wrSignaled)) then
        Result := DecodePkg(IOResult,values);

    end else
      Result := ioNullDriver;
  finally
    SetLength(pkg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
  end;
end;

procedure TModBusDriver.SetOutputMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  if v = POutputMaxHole then exit;

  POutputMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].OutPuts.MaxHole := v;
end;

procedure TModBusDriver.SetInputMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  if v = PInputMaxHole then exit;

  PInputMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Inputs.MaxHole := v;
end;

procedure TModBusDriver.SetRegisterMaxHole(v:Cardinal);
var
  plc:Integer;
begin
  if v = PRegistersMaxHole then exit;

  PRegistersMaxHole:=v;

  for plc:=0 to High(PModbusPLC) do
    PModbusPLC[plc].Registers.MaxHole := v;
end;

procedure TModBusDriver.CancelPendingActions;
begin
  FReadEvent.SetEvent;
end;

procedure TModBusDriver.BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);
begin
  with tr do begin
    Station := plc;
    Hack:=0;
    Address := startaddress;
    ReadFunction := func;
    OffSet := 0;
    Slot := 0;
    File_DB := 0;
    SubElement := 0;
    WriteFunction := 0;
    Retries := 0;
    ScanTime := 0;
  end;
  tr.Size := size;

end;

end.
