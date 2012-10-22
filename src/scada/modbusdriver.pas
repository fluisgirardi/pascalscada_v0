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
}
{$ENDIF}
unit ModBusDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString, uModbusTagBuilder
  {$IFNDEF FPC}, Windows{$ENDIF};

type
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
    Station:Integer;
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
  TModBusDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;
    function BuildItemName(nameprefix:String; ZeroFill:Boolean; index, NumZeros:Integer):String;
    function SelectedReadFuntion(dlg:TfrmModbusTagBuilder):Integer;
    function SelectedWriteFuntion(dlg:TfrmModbusTagBuilder):Integer;
    function SeekFirstItem(LastItem:TTagNamesItemEditor):TTagNamesItemEditor;
  protected
    PFirstRequestLen,
    PFuncByteOffset,
    PCRCLen:Integer;
    POutputMaxHole:Cardinal;
    PInputMaxHole:Cardinal;
    PRegistersMaxHole:Cardinal;
    PInternalDelayBetweenCmds:Cardinal;
    PModbusPLC:array of TModBusPLC;
    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:Integer):Boolean;
    procedure SetOutputMaxHole(v:Cardinal);
    procedure SetInputMaxHole(v:Cardinal);
    procedure SetRegisterMaxHole(v:Cardinal);
    procedure BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);

    {$IFDEF PORTUGUES}
    //: Cria um pacote modbus
    {$ELSE}
    //: Encode a modbus packet.
    {$ENDIF}
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES; virtual;

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
    function RemainingBytes(buffer:BYTES):Integer; virtual;

    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;

    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
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
    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
    //: @seealso(TProtocolDriver.SizeOfTag)
    function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
  end;

implementation

uses Dialogs, Controls, PLCBlockElement, ValueProcessor, crossdatetime,
  pascalScadaMTPCPU;


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
  station, mem, size, memtype, scantime:Integer;
  found, valido:boolean;
  plc:integer;
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
  station, mem, size, memtype, scantime:Integer;
  found:boolean;
  plc:Integer;
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

procedure TModBusDriver.OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  dlg:TfrmModbusTagBuilder;
  tplc:TPLCTagNumber;
  tstr:TPLCString;
  tblk:TPLCBlock;
  tbel:TPLCBlockElement;
  c, CurMemtAdress, nameitem, BlockNo, Element:Integer;
  confItem:TTagNamesItemEditor;
  knowstringsize:boolean;
  tstrdummy:TPLCString;
  defaultstringsize:integer;
  count:Integer;
  ItemName:Strings;
  ItemPtr:array of TComponent;
begin
  if [csDesigning]*ComponentState=[] then exit;

  count:=1;
  SetLength(ItemName,1);
  SetLength(ItemPtr, 1);
  ItemName[0]:='(none)';
  ItemPtr[0] :=nil;
  for c:=0 to Owner.ComponentCount-1 do begin
    if Owner.Components[c] is TPIPE then begin
      inc(count);
      SetLength(ItemName,count);
      SetLength(ItemPtr, count);
      ItemName[count-1]:= Owner.Components[c].Name;
      ItemPtr[count-1] := Owner.Components[c];
    end;
  end;

  dlg:=TfrmModbusTagBuilder.Create(ItemName);
  try
    if dlg.ShowModal=mrOk then begin
      if Assigned(InsertHook) and Assigned(CreateProc) then begin

        knowstringsize:=false;

        CurMemtAdress:=dlg.FirstMemAddress.Value;

        if dlg.optStartFromZero.Checked then
          nameitem:=0
        else
          nameitem:=1;

        confItem:=SeekFirstItem(dlg.CurItem);
        if confItem=nil then exit;

        ////////////////////////////////////////////////////////////////////////
        //plcnumber and string
        ////////////////////////////////////////////////////////////////////////
        if dlg.optPLCTagNumber.Checked or dlg.optPLCString.Checked then begin
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
                if confItem.PIPES.ItemIndex<>-1 then
                  tplc.ScaleProcessor:=TPIPE(ItemPtr[confItem.PIPES.ItemIndex]);
                InsertHook(tplc);
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

                if not knowstringsize then begin
                  knowstringsize:=true;
                  defaultstringsize:=tstr.Size;
                end;
              end;
            end;

            if dlg.optPLCTagNumber.Checked then
              inc(CurMemtAdress)
            else begin
              //se o tamanho do bloco da string ainda não é conhecido.
              //if the string size is unknown.
              if not knowstringsize then begin
                try
                  tstrdummy := TPLCString.Create(Nil);
                  tstrdummy.MemAddress := CurMemtAdress;
                  tstrdummy.MemReadFunction  := SelectedReadFuntion(dlg);
                  tstrdummy.MemWriteFunction := SelectedWriteFuntion(dlg);
                  tstrdummy.PLCStation:=dlg.StationAddress.Value;
                  tstrdummy.RefreshTime:=confItem.Scan.Value;
                  tstrdummy.ProtocolDriver := Self;
                  if dlg.optSTR_C.Checked then
                    tstrdummy.StringType:=stC
                  else
                    tstrdummy.StringType:=stSIEMENS;
                  tstrdummy.ByteSize:=Byte(dlg.ByteSize.Value);
                  tstrdummy.StringSize:=dlg.MaxStringSize.Value;
                  defaultstringsize:=tstrdummy.Size;
                  knowstringsize:=true;
                finally
                  tstrdummy.Destroy;
                end;
              end;
              inc(CurMemtAdress,defaultstringsize);
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

        ////////////////////////////////////////////////////////////////////////
        //TPLCBlock
        ////////////////////////////////////////////////////////////////////////
        if dlg.optPLCBlock.Checked then begin
          c:=1;
          BlockNo:=1;
          while c<=dlg.MemCount.Value do begin
            Element:=0;
            tblk := TPLCBlock(CreateProc(TPLCBlock));
            tblk.Name:=BuildItemName(dlg.NameOfEachBlock.Text, false, BlockNo, 9);
            tblk.MemAddress := CurMemtAdress;
            tblk.MemReadFunction  := SelectedReadFuntion(dlg);
            tblk.MemWriteFunction := SelectedWriteFuntion(dlg);
            tblk.PLCStation:=dlg.StationAddress.Value;
            tblk.RefreshTime:=dlg.ScanOfEachBlock.Value;
            tblk.Size := 1;
            tblk.ProtocolDriver := Self;
            InsertHook(tblk);

            //cria os elementos do bloco
            //create the block elements.
            while Element<dlg.MaxBlockSize.Value do begin
              if Trim(confItem.Nome.Text)<>'' then begin
                tbel := TPLCBlockElement(CreateProc(TPLCBlockElement));
                tbel.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tbel.PLCBlock := tblk;
                tblk.Size := Element+1;
                tbel.Index:=Element;
                if confItem.PIPES.ItemIndex<>-1 then
                  tbel.ScaleProcessor:=TPIPE(ItemPtr[confItem.PIPES.ItemIndex]);
                InsertHook(tbel);
              end;

              inc(Element);
              inc(CurMemtAdress);

              if (Trim(confItem.Nome.Text)<>'') or confItem.CountEmpty.Checked  then
                inc(c);

              if confItem.Next=nil then begin
                confItem:=SeekFirstItem(dlg.CurItem);
                inc(nameitem);
              end else
                confItem:=TTagNamesItemEditor(confItem.Next);
            end;

            //incrementa o numero do bloco.
            //inc the block number.
            inc(BlockNo);
          end;
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


function  TModBusDriver.SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
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

function  TModBusDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
begin
  Result:=nil;
end;

function TModBusDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=ioDriverError
end;

function  TModBusDriver.RemainingBytes(buffer:BYTES):Integer;
begin
  Result:=0;
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
    if ([csDestroying]*ComponentState<>[]) then begin
      CrossThreadSwitch;
      exit;
    end;
    for plc:= 0 to High(PModbusPLC) do begin
      for block := 0 to High(PModBusPLC[plc].Outputs.Blocks) do
        if PModBusPLC[plc].Outputs.Blocks[block].NeedRefresh then begin
          done := true;
          BuildTagRec(PModBusPLC[plc].Station,1,PModBusPLC[plc].Outputs.Blocks[block].AddressStart,PModBusPLC[plc].Outputs.Blocks[block].Size, tr);
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
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
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
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
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
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
          FMustReleaseResources:=true;
          DoRead(tr,values,false);
          FMustReleaseResources:=false;
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
    //
    //if does nothing, update the tag with the oldest timestamp
    if (PReadSomethingAlways) and (Length(PModbusPLC)>0) and ((not done) and (not first)) then begin
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

procedure TModBusDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
var
  plc,c:Integer;
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
  FRemainingBytes:Integer;
  rl:Integer;
  res:Integer;
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

function  TModBusDriver.DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  FRemainingBytes:Integer;
  pkg:BYTES;
  rl:Integer;
  res:Integer;
  starts, ends:TNotifyEvent;
begin
  try
    if FMustReleaseResources then begin
      starts:=HighLatencyOperationWillBegin;
      ends  :=HighLatencyOperationWasEnded;
    end else begin
      starts:=nil;
      ends  :=nil;
    end;

    pkg := EncodePkg(tagrec,nil,rl);
    if PCommPort<>nil then begin
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

procedure TModBusDriver.BuildTagRec(plc,func,startaddress,size:Integer; var tr:TTagRec);
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

end.
