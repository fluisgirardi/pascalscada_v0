{$i language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Classes para organização de blocos de memória de um CLP.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
  @abstract(Set of class to handle blocks of memory of an PLC.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit PLCMemoryManager;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses SysUtils, DateUtils, Tag, SyncObjs, Classes;

type

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Estrutura usada para cadastrar cada endereço único dentro do gerenciador de
  blocos de memória.
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Record used to handle a unique address inside the memory blocks manager.
  }
  {$ENDIF}
  TMemoryRec = record
    Address, Count, MinScan:Integer;
  end;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Classe que representa um faixa de endereços de memória continuos (bloco).

  @bold(É altamente recomendável a você que está desenvolvendo um driver de
  comunicação, utilizar a classe TPLCMemoryManager, que implementa blocos de
  memória não-continuas. Essa classe faz uso de @name e todos os seus descendentes.)

  @seealso(TPLCMemoryManager)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Continuous memory block class.

  @bold(Attention: If are you developing a protocol driver, use the class
  TPLCMemoryManager, that implements the non-continuous memory blocks. This class
  uses @name and all their descendents.)

  @seealso(TPLCMemoryManager)
  }
  {$ENDIF}
  TRegisterRange = class
  private
    FStartAddress:Integer;
    FEndAddress:Integer;
    FLastUpdate:TDateTime;
    FMinScanTime:Cardinal;
    FReadOK, FReadFault:Cardinal;
    procedure SetReadOK(value:Cardinal);
    procedure SetReadFault(value:Cardinal);
    function GetSize:Integer;
    function GetMsecLastUpdate:Int64;
  protected
    //: @exclude
    function  GetValue(index:Integer):Double;
    //: @exclude
    procedure SetValue(index:Integer; v:Double);
  public
    {$IFDEF PORTUGUES}
    //: Array que armazena os valores do bloco.
    {$ELSE}
    //: Array that stores the values of the memory block.
    {$ENDIF}
    FValues:TArrayOfDouble;
    //: Armazena o último erro ocorrido com o bloco.
    LastError:TProtocolIOResult;

    {$IFDEF PORTUGUES}
    {:
    Cria um bloco de memórias continuas.
    @param(AdrStart Cardinal. Endereço inicial do bloco.)
    @param(AdrEnd Cardinal. Endereço final do bloco.)
    AdrStart e AdrEnd devem ser passados na menor unidade de memória disponível
    no CLP. Um exemplo são os CLPs da Siemens que utilizam a mesma area
    de memória para bytes, words e DWORDs. Então para adicionar a MD0, é necessário
    passar 0 em AdrStart e 3 em AdrEnd, totalizando 4 bytes (que é o menor
    tamanho de palavra disponível no CLP) que são MB0, MB1, MB2, MB3, formando a
    MD0.
    }
    {$ELSE}
    {:
    Creates a continuous memory block
    @param(AdrStart Cardinal. Start address of the block.)
    @param(AdrEnd Cardinal. Final address of the block.)

    AdrStart and AdrEnd must be passed in the memory unit of the smaller word
    available on your PLC. A example is the Siemens PLC's, that can use on the
    same memory area, bytes, Words e DWORDs. So to add the MD0, you must pass 0
    to AdrStart and 3 to AdrEnd, totalizing 4 bytes (which is the less word size
    on this PLC) which are the MB0, MB1, MB2, MB3, forming the MD0.
    }
    {$ENDIF}
    constructor Create(AdrStart,AdrEnd:Cardinal);

    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    Lê/escreve o valor da memória especificada por Index no bloco.
    }
    {$ELSE}
    {:
    Reads/writes a value at the specified index of the memory block.
    }
    {$ENDIF}
    property Values[Index:Integer]:Double read GetValue write SetValue;

    {$IFDEF PORTUGUES}
    {:
    Use @name para dizer que os dados estão atualizados. Utilize esse método logo
    após fazer uma leitura de seu dispositivo.
    }
    {$ELSE}
    {:
    @name updates the timestamp of the block values. Call this method after read
    values of your device.
    }
    {$ENDIF}
    procedure Updated;

    {$IFDEF PORTUGUES}
    {:
    @name informa se o bloco precisa ser lido do dispositivo, devido ultrapassar
    o menor tempo de scan do bloco.
    }
    {$ELSE}
    {:
    @name tells if the memory block must be read from the device, because the
    scan time elapses (Now - time from the last update > smaller scan time of
    the block).
    }
    {$ENDIF}
    function NeedRefresh:Boolean;
  published
    {$IFDEF PORTUGUES}
    //: Informa o endereço inicial do bloco.
    {$ELSE}
    //: Start address of the memory block.
    {$ENDIF}
    property AddressStart:Integer read FStartAddress;

    {$IFDEF PORTUGUES}
    //: Informa o endereço final do bloco.
    {$ELSE}
    //: Final address of the memory block.
    {$ENDIF}
    property AddressEnd:Integer read FEndAddress;

    {$IFDEF PORTUGUES}
    //: Informa o tamanho do bloco.
    {$ELSE}
    //: Tells the size of the block.
    {$ENDIF}
    property Size:Integer read GetSize;

    {$IFDEF PORTUGUES}
    //: @name informa quando foi a última atualização dos dados do bloco.
    {$ELSE}
    //: @name tells the timestamp of the last update.
    {$ENDIF}
    property LastUpdate:TDateTime read FLastUpdate write FLastUpdate;

    {$IFDEF PORTUGUES}
    //: @name diz quantos milisegundos se passaram desde a última atualização de dados.
    {$ELSE}
    //: @name tells how many milliseconds are elapsed from the last update.
    {$ENDIF}
    property MilisecondsFromLastUpdate:Int64 read GetMsecLastUpdate;

    {$IFDEF PORTUGUES}
    //: Informa qual é o tempo de varredura desse bloco.
    {$ELSE}
    //: Tells the smaller scan time of the memory block.
    {$ENDIF}
    property ScanTime:Cardinal read FMinScanTime write FMinScanTime;

    {$IFDEF PORTUGUES}
    //: Informa quantas leituras de dados do dispositivo tiveram sucesso.
    {$ELSE}
    //: Tells how many reads was successful.
    {$ENDIF}
    property ReadSuccess:Cardinal read FReadOK write SetReadOK;

    {$IFDEF PORTUGUES}
    //: Informa quantas leituras de dados do dispositivo falharam.
    {$ELSE}
    //: Tells how many reads was not successful.
    {$ENDIF}
    property ReadFaults:Cardinal read FReadFault write SetReadFault;
  end;

  {$IFDEF PORTUGUES}
  //: Array de blocos continuos de memória.
  {$ELSE}
  //: Array of continuous memory blocks.
  {$ENDIF}
  TRegisterRangeArray = array of TRegisterRange;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  @abstract(Classe que gerência blocos de memórias não continuos (fragmentados)
            e sua melhor organização.)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  @abstract(Class that handles non-continuous memory blocks (fragmented)
            and the better organization of it.)
  }
  {$ENDIF}
  TPLCMemoryManager = class
  private
    FAddress:array of TMemoryRec;
    FMaxHole:Integer;
    FMaxBlockSize:Integer;
    //binary search of memory address
    function FindAddress(const address:Integer; var idx:Integer):Boolean;
    //segmented binary search of memory address
    function FindAddresBySegment(const address, startindex, endindex:Integer; var idx:Integer):Boolean;
    procedure AddAddress(Add,Scan:Integer); overload;
    procedure RemoveAddress(Add:Integer); overload;
    procedure SetHoleSize(size:Integer);
    procedure SetBlockSize(size:Integer);
    //rebuild the blocks
    procedure RebuildBlocks;
    //returns the size of all block fragments.
    function  GetSize:Integer;
    function  CreateRegisterRange(adrStart,adrEnd:Integer):TRegisterRange;
  public
    {$IFDEF PORTUGUES}
    //: Blocos de memória continuos.
    {$ELSE}
    //: Continous memory blocks.
    {$ENDIF}
    Blocks:TRegisterRangeArray;

    {$IFDEF PORTUGUES}
    //: Cria um gerenciador de memórias não continuas.
    {$ELSE}
    //: Creates the handler of non continuous memory block.
    {$ENDIF}
    constructor Create; virtual;

    {$IFDEF PORTUGUES}
    //: Destroi o gerenciador de blocos não continuos e todos os seus recursos.
    {$ELSE}
    //: Destroys the handler of non continuous memory block.
    {$ENDIF}
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    Adiciona uma ou mais memórias ao gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo adicionadas ao bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na área.)
    @param(Scan Cardinal. Tempo de varredura da memória.)

    Por exemplo, para adicionar as MW0, MW2 e MW4 (words) de um CLP Siemens (onde a
    menor palavra é o byte) com 1200ms de scan, você chamaria:
    @code(AddAddress(0,3,2,1200);)

    Já nos CLPs Scheneider (onde a menor palvra é de 16 bits), para endereçar
    as words W0, W1 e W2 ficaria assim:
    @code(AddAddress(0,3,1,1200);)

    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    Adds one or more memory into the manager.
    @param(Address Cardinal. Initial address of memory range.)
    @param(Size Cardinal. How many memories will be managed by the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Scan Cardinal. Scan time of your variables)

    For example, to add the MW0, MW2 and MW4 of a Siemens PLC (the smaller word
    is the byte) with 1200ms of scan into the manager, you must call:
    @code(AddAddress(0,3,2,1200);)

    However, on a Schneider PLC (the smaller word has 16 bits), to add the address
    W0, W1 and W2, you must call:
    @code(AddAddress(0,3,1,1200);)

    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    procedure AddAddress(Address,Size,RegSize,Scan:Cardinal); overload; virtual;

    {$IFDEF PORTUGUES}
    {:
    Remove uma ou mais variáveis do gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo removidas do bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na area.)

    Os parametros funcionam de maneira identica a função AddAddress.

    @seealso(AddAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    Removes one or more variables from the manager.
    @param(Address Cardinal. Initial address of memory range.)
    @param(Size Cardinal. How many memories will be removed from the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)

    These parameters works like the of function AddAddress.

    @seealso(AddAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    procedure RemoveAddress(Address,Size,RegSize:Cardinal); overload; virtual;

    {$IFDEF PORTUGUES}
    {:
    @name armazena valores em um intervalo de memórias, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a armazenar.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra de seu equipamento.)
    @param(Values TArrayOfDouble. Valores que irão ser armazenados no gerenciador de variaveis.)
    @param(LastResult TProtocolIOResult. Último resultado de E/S do conjunto de variáveis.)

    Cada valor na array Values representa a um valor da menor palavra de seu equipamento.

    Por exemplo: supondo que você esteja escrevendo em um S7-200 da Siemens, para
    escrever na VW0 você chamaria:

    @code(SetValues(0,1,2,[valor_vb0,valor_vb1]);)

    No Siemens a menor palavra é o Byte, e uma Word são dois bytes.

    Mas em um CLP Schneider ficaria:

    @code(SetValues(0,1,1,[valor_VW0]);)

    Pois o menor tamanho de palavra nesses CLPs é 16 bits.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    @name stores values in a range of memories, continuous or non continuous.

    @param(Address Cardinal. Initial address of memory range.)
    @param(Len Cardinal. How many memory will be stored on the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Values TArrayOfDouble. Values that will be stored in the memory manager.)
    @param(LastResult TProtocolIOResult. Last I/O result of the values being stored.)

    One value on Values array represents the value of the smaller word of your device.

    For example: if you are storing the value of MW0 (word) of an Siemens PLC,
    you must call:

    @code(SetValues(0,1,2,[vb0_value,vb1_value]);)

    Because on Siemens PLC's the smaller word size is the byte, so, one word are two bytes.

    But, on a Schneider PLC, you must call:

    @code(SetValues(0,1,1,[valor_VW0]);)

    Because on this PLC, the smaller word size is the Word (16bits).

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(GetValues)
    }
    {$ENDIF}
    function  SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble; LastResult:TProtocolIOResult):Integer; virtual;

    {$IFDEF PORTUGUES}
    {:
    @name lê valores armazenadas no gerenciador, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a ler.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)
    @param(Values TArrayOfDouble. Array onde serão retornados os valores armazenados no gerenciador de variaveis.)
    @param(LastResult TProtocolIOResult. Último resultado de E/S do conjunto de variáveis.)
    @param(ValueTimeStamp TDateTime. Data hora em que os valores foram atualizados no gerenciador de memórias.)

    Cada item da array retornado representa o valor da menor palavra daquela área.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    @name gets the values stored in memory manager, continuous or non-continuous.

    @param(Address Cardinal. Initial address of memory range.)
    @param(Len Cardinal. How many memories will got from the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Values TArrayOfDouble. Array that will return the values that are stored in the memory manager.)
    @param(LastResult TProtocolIOResult. Last I/O result of the memory range.)
    @param(ValueTimeStamp TDateTime. Date time of the last update of the values on the memory manager.)

    One value on Values array represents the value of the smaller word of your device.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    function  GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):Integer; virtual;

    {$IFDEF PORTUGUES}
    {:
    @name escreve o status da última leitura, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a escrever.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)
    @param(Fault TProtocolIOResult. Status da última leitura.)

    @seealso(SetValues)
    }
    {$ELSE}
    {:
    @name updates the last I/O result of a range of memories on manager.

    @param(Address Cardinal. Initial address of memory range.)
    @param(Len Cardinal. How many memories will be updated on the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Fault TProtocolIOResult. Last I/O result of the memory range.)

    @seealso(SetValues)
    }
    {$ENDIF}
    procedure SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult); virtual;
  published

    {$IFDEF PORTUGUES}
    {:
    Define quantos endereços podem ficar sem serem usados para manter a
    continuidade de um bloco. Valores grandes formam poucos grupos de tamanho
    grande, enquanto valores pequenos formam muitos grupos de tamanho pequeno.

    Digamos que sejam adicionados os endereços [0, 1] e [3 ,4] e MaxHole=0, logo
    serão formados dois blocos, o primeiro contendo os endereços [0, 1] e o
    segundo os endereços [3, 4].

    Já se for setado MaxHole=1 será criado um único grupo com os endereços
    [0,1,2,3,4] sendo o endereço 2 adicionado automaticamente para manter a
    continuidade do bloco.
    }
    {$ELSE}
    {:
    Tells how many memory address can be missing without break the block on
    two or more smaller blocks.

    For example, if are added the memory address [0, 1] and [3, 4] into the
    Manager with MaxHole=0, will be built two blocks, the first with the address
    [0, 1] and the second block with the address [3, 4].

    However, if the MaxHole is set to 1, will be built only one block with the
    address [0,1,2,3,4]. Will be included the address 2, to avoid the break the
    block on two pieces.
    }
    {$ENDIF}
    property MaxHole:Integer read FMaxHole write SetHoleSize;

    {$IFDEF PORTUGUES}
    {:
    Define qual o tamanho máximo de cada bloco continuo. Se não há limite de
    tamanho, use 0 nessa propriedade.

    Supondo que foram adicionados os endereços [0,1,2,3,4] e @name=0 será criado
    um único bloco com esses mesmos endereços. Supondo que @name=3 serão criados
    dois grupos, o primeiro com os endereços [0,1,2] e o segundo com os endereços
    [3,4].
    }
    {$ELSE}
    {:
    Tells the max size of the blocks. If has no limit, set @name to 0.

    For example, if are added the memory address [0,1,2,3,4] and  @name=0 will
    be created only one block with these address. However if @name=3, will be
    created two blocks, the first with the address [0,1,2] and the second with
    the address [3,4].
    }
    {$ENDIF}
    property MaxBlockItems:Integer read FMaxBlockSize write SetBlockSize;

    {$IFDEF PORTUGUES}
    //: Retorna a quantidade total de memórias gerenciadas pelo bloco.
    {$ELSE}
    //: How many memories are handled by the manager.
    {$ENDIF}
    property Size:Integer read GetSize;
  end;

  TPLCMemoryManagerSafe = class(TPLCMemoryManager)
  private
    FMutex:TCriticalSection;
  public
    //: @seealso(TPLCMemoryManager.Create)
    constructor Create; override;
    //: @seealso(TPLCMemoryManager.Destroy)
    destructor Destroy; override;
    //: @seealso(TPLCMemoryManager.AddAddress)
    procedure AddAddress(Address,Size,RegSize,Scan:Cardinal); override;
    //: @seealso(TPLCMemoryManager.RemoveAddress)
    procedure RemoveAddress(Address,Size,RegSize:Cardinal); override;
    //: @seealso(TPLCMemoryManager.SetValues)
    function  SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble; LastResult:TProtocolIOResult):Integer; override;
    //: @seealso(TPLCMemoryManager.GetValues)
    function  GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):Integer; override;
    //: @seealso(TPLCMemoryManager.SetFault)
    procedure SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult); override;
  end;

implementation

uses Math, hsstrings, crossdatetime;

constructor TRegisterRange.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create;
  FStartAddress := AdrStart;
  FEndAddress := AdrEnd;
  FReadOK := 0;
  FReadFault := 0;
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor TRegisterRange.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

procedure TRegisterRange.Updated;
begin
  FLastUpdate := CrossNow;
end;

function  TRegisterRange.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRange.SetValue(index:Integer; v:Double);
begin
  FValues[index] := v;
end;

function TRegisterRange.GetSize:Integer;
begin
  Result := (FEndAddress-FStartAddress)+1;
end;

function TRegisterRange.GetMsecLastUpdate:Int64;
begin
  Result := MilliSecondsBetween(CrossNow,FLastUpdate);
end;

function TRegisterRange.NeedRefresh:Boolean;
var
  aux:Int64;
begin
  aux := FMinScanTime;
  Result := GetMsecLastUpdate>=aux;
end;

procedure TRegisterRange.SetReadOK(value:Cardinal);
begin
  FReadOK := Max(FReadOK,value);
end;

procedure TRegisterRange.SetReadFault(value:Cardinal);
begin
  FReadFault := Max(FReadFault,value);
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TPLCMemoryManager
//                implementation of TPLCMemoryManager.
////////////////////////////////////////////////////////////////////////////////

constructor TPLCMemoryManager.Create;
begin
  //o bloco continua caso de enderecos seja <= 5
  //the block stay continuous if the number of missing address is <=5
  FMaxHole := 5;
  //o bloco tem seu tamanho restrito a x, 0 = sem restricao!
  //size limits of the built blocks, 0 = no size limit.
  FMaxBlockSize := 0;
end;

destructor TPLCMemoryManager.Destroy;
var
  c:integer;
begin
  for c:=0 to High(Blocks) do
    Blocks[c].Destroy;
  SetLength(Blocks,0);
end;

function TPLCMemoryManager.FindAddress(const address:Integer; var idx:Integer):Boolean;
begin
  if Length(FAddress)=0 then
    Result:=false
  else
    Result:=FindAddresBySegment(address,0,High(FAddress), idx);
end;

function TPLCMemoryManager.FindAddresBySegment(const address, startindex, endindex:Integer; var idx:Integer):Boolean;
var
  len, middle:Integer;
begin
  len:=endindex-startindex+1;

  if len=1 then begin
    if FAddress[startindex].Address=Address then begin
      Result:=true;
      idx:=startindex;
    end else
      Result:=false;
  end else begin
    middle := (len div 2) + startindex;
    if Address<FAddress[middle].Address then
      Result:=FindAddresBySegment(Address,startindex,middle-1,idx)
    else
      Result:=FindAddresBySegment(Address,middle,endindex,idx);
  end;
end;

procedure TPLCMemoryManager.AddAddress(Add, Scan:Integer);
var
  c, h:Integer;
begin
  IF Length(FAddress)=0 THEN begin
    SetLength(FAddress,1);
    FAddress[0].Address := Add;
    FAddress[0].Count := 1;
    FAddress[0].MinScan := Scan;
    exit;
  end;
  IF Length(FAddress)=1 THEN begin
    if FAddress[0].Address = add then begin
      inc(FAddress[0].Count);
      FAddress[0].MinScan := Min(FAddress[0].MinScan,Scan);
      exit;
    end else begin
      SetLength(FAddress,2);
      if FAddress[0].Address<add then begin
        FAddress[1].Address := Add;
        FAddress[1].Count := 1;
        FAddress[1].MinScan := Scan;
      end else begin
        FAddress[1].Address := FAddress[0].Address;
        FAddress[1].Count := FAddress[0].Count;
        FAddress[1].MinScan := FAddress[0].MinScan;
        FAddress[0].Address := Add;
        FAddress[0].Count := 1;
        FAddress[0].MinScan := Scan;
      end;
    end;
    exit;
  end;

  //procura e adiciona no lugar correto...
  //search and adds on right place.
  IF Length(FAddress)>=2 THEN begin
    c:=0;
    //procura...
    //search
    if not FindAddress(add,c) then begin
      c:=0;
      //if the address has not found using the binary search, try the normal
      //search to seek the nearest array index to insert the new address.
      while (c<Length(FAddress)) and (add>FAddress[c].Address) do
        inc(c);
    end;

    if (c<Length(FAddress)) and (FAddress[c].Address=add) then begin
      //se encontrou o endereco...
      //if found the address...
      inc(FAddress[c].Count);
      FAddress[c].MinScan := Min(FAddress[c].MinScan,Scan);
    end else begin
      h:=Length(FAddress);
      //adiciona mais um na array
      //adds the address
      SetLength(FAddress,h+1);
      //se se não chegou no fim, é pq é um endereco
      //que deve ficar no meio da lista para mante-la
      //ordenada
      //
      //if isn't the end of the array, is because the address must be on the middle
      //of array to keep it ordered
      if c<High(FAddress) then
         Move(FAddress[c],FAddress[c+1],(high(FAddress)-c)*sizeof(TMemoryRec));
         
      FAddress[c].Address := add;
      FAddress[c].Count := 1;
      FAddress[c].MinScan := Scan;
    end;
  end;
end;

procedure TPLCMemoryManager.RemoveAddress(Add:Integer);
var
  c:Integer;
begin
  c:=0;
  //se não encontrou cai fora...
  //if not found the addres, exit.
  if not FindAddress(add, c) then
    exit;

  dec(FAddress[c].Count);
  //caso zerou um endereco, é necessário remover ele da lista...
  //if the address isn't referenced anymore, remove it from the address list.
  if FAddress[c].Count=0 then
    if Length(FAddress)=1 then begin
        SetLength(FAddress,0);
    end else begin
      if c<High(FAddress) then
        Move(FAddress[c+1],FAddress[c],(high(FAddress)-c)*sizeof(TMemoryRec));
      SetLength(FAddress,Length(FAddress)-1);
    end;
end;

procedure TPLCMemoryManager.SetHoleSize(size:Integer);
begin
  if size=FMaxHole then exit;
  FMaxHole := size;
  RebuildBlocks;
end;

procedure TPLCMemoryManager.SetBlockSize(size:Integer);
begin
  if size=FMaxBlockSize then exit;
  FMaxBlockSize := size;
  RebuildBlocks; //rebuild the blocks.
end;

procedure TPLCMemoryManager.RebuildBlocks;
var
  c, c2, c3:integer;
  newBlocks:TRegisterRangeArray;
  adrstart, adrend,BlockItems,BlockEnd,mscan:Integer;
  BlockOldOffset, BlockNewOffset:Integer;
  BlockIndex:Integer;
  found:Boolean;
begin
  SetLength(newBlocks,0);
  adrend := 0;
  adrstart := 0;
  BlockItems := 0;
  BlockEnd := 0;
  mscan := 0;
  BlockIndex := 0;
  //refaz blocos de dados
  //rebuild the memory blocks.
  for c:=0 to High(FAddress) do begin
    if c=0 then begin
      adrstart := FAddress[0].Address;
      adrend := adrstart;
      BlockEnd := adrend + FMaxHole + 1;
      mscan := FAddress[0].MinScan;
      BlockItems := 1;
      if c<High(FAddress) then continue;
    end;

    if (FAddress[c].Address>BlockEnd) or ((FMaxBlockSize<>0) AND (BlockItems>=FMaxBlockSize)) then begin
      //bloco terminou, feche esse e inicie um novo!!
      //the block can't be extended, starts another...
      SetLength(newBlocks,Length(newBlocks)+1);

      newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
      newBlocks[BlockIndex].LastUpdate := CrossNow;
      newBlocks[BlockIndex].ScanTime := mscan;
      inc(BlockIndex);

      //pega os enderecos de onde comeca um novo bloco...
      //get the address of the new block.
      adrstart := FAddress[c].Address;
      adrend := adrstart;
      BlockEnd := adrend + FMaxHole + 1;
      mscan := FAddress[c].MinScan;
      BlockItems := 1;
    end else begin
      //bloco continua, adiciona novos enderecos.
      //the block can be extended, add the new address.
      adrend := FAddress[c].Address;
      BlockEnd := adrend + FMaxHole + 1;
      mscan := min(mscan, FAddress[c].MinScan);
      Inc(BlockItems);
    end;
    if c=High(FAddress) then begin
      SetLength(newBlocks,Length(newBlocks)+1);
      newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
      newBlocks[BlockIndex].LastUpdate := CrossNow;
      newBlocks[BlockIndex].ScanTime := mscan;
      inc(BlockIndex);
    end;
  end;

  //copia os dados que estavam nos blocos antigos...
  //baseia-se em varer a array de enderecos, verificar em que bloco ela estava
  //e para que bloco o endereco foi parar...
  //
  //copy the data of the oldest blocks to the new blocks.

  for c:=0 to High(FAddress) do begin
    found := false;
    for c2 := 0 to High(Blocks) do
      if (FAddress[c].Address>=Blocks[c2].AddressStart) and (FAddress[c].Address<=Blocks[c2].AddressEnd) then begin
        found := true;
        break;
      end;

    //se não encontrou aqui é pq o endereco foi adicionado...
    //if not found the address here, it was added.
    if not found then continue;

    found := false;
    for c3:= 0 to High(newBlocks) do
      if (FAddress[c].Address>=newBlocks[c3].AddressStart) and (FAddress[c].Address<=newBlocks[c3].AddressEnd) then begin
        found := true;
        break;
      end;

    //se não encontrou aqui é pq o endereco foi deletado...
    //if not found the address here, it was removed.
    if not found then continue;
    BlockOldOffset := FAddress[c].Address - Blocks[c2].AddressStart;
    BlockNewOffset := FAddress[c].Address - newBlocks[c3].AddressStart;
    newBlocks[c3].Values[BlockNewOffset] := Blocks[c2].Values[BlockOldOffset];

    //coloca o menor tempo de atualização para priorizar quem necessita de refresh mais urgente..
    //set the the block with the small timestamp.
    newBlocks[c3].LastUpdate := Min(newBlocks[c3].LastUpdate,Blocks[c2].LastUpdate);
  end;
  //destroi os blocos antigos
  //remove the old blocks.
  for c:=0 to High(Blocks) do
    Blocks[c].Destroy;
  SetLength(Blocks, 0);

  //copia os valores dos novos blocos para o bloco velho
  //copy the values from the new block to the old block.
  Blocks := newBlocks;

  //zera o auxiliar de blocos novos...
  //releases the memory
  SetLength(newBlocks,0);
end;

function TPLCMemoryManager.GetSize:Integer;
var
  c:Integer;
begin
  Result := 0;
  for c:=0 to High(Blocks) do
    Result := Result + Blocks[c].Size;
end;

function TPLCMemoryManager.CreateRegisterRange(adrStart,adrEnd:Integer):TRegisterRange;
begin
   Result := TRegisterRange.Create(adrStart,adrEnd);
end;

procedure TPLCMemoryManager.AddAddress(Address,Size,RegSize,Scan:Cardinal);
var
  c, items:Cardinal;
  len:Integer;
begin
  if (Size<=0) or (RegSize<=0) then
    raise Exception.Create(SsizeMustBeAtLeastOne);

  //captura o tamanho da array de enderecos...
  //gets the size of address Array.
  len := length(FAddress);

  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    AddAddress(c,Scan);
    inc(c);
  end;

  //dipara o rebuild blocks, pq foram adicionados enderecos
  //rebuild the blocks, because address are added.
  if len<>length(FAddress) then
    RebuildBlocks;
end;

procedure TPLCMemoryManager.RemoveAddress(Address,Size,RegSize:Cardinal);
var
  c, items:Cardinal;
  len:Integer;
begin
  if (Size<=0) or (RegSize=0) then
    raise Exception.Create(SsizeMustBeAtLeastOne);

  //captura o tamanho atual...
  //gets the actual size of address array.
  len := length(FAddress);
  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    RemoveAddress(c);
    inc(c);
  end;
  //dipara o rebuild blocks, pq foram adicionados enderecos
  //rebuild the blocks, because address are removed.
  if len<>length(FAddress) then
    RebuildBlocks;
end;

function TPLCMemoryManager.SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble; LastResult:TProtocolIOResult):Integer;
var
  blk, AdrEnd, LenUtil, Moved:Integer;
begin
  AdrEnd := AdrStart + Length(Values) - 1;
  Moved:=0;

  for blk := 0 to High(Blocks) do begin
    LenUtil:=0;
    if (Blocks[blk].AddressStart<=AdrStart) AND (Blocks[blk].AddressEnd>=AdrEnd) then begin
      LenUtil := (AdrEnd - AdrStart) + 1;
      Move(Values[0], Blocks[blk].FValues[AdrStart - Blocks[blk].AddressStart], LenUtil*SizeOf(Double));
      Blocks[blk].Updated;
      Blocks[blk].LastError:=LastResult;
      Blocks[blk].ReadSuccess:=Blocks[blk].ReadSuccess+1;
    end else begin
      if (Blocks[blk].AddressStart>=AdrStart) AND (Blocks[blk].AddressStart<=AdrEnd) then begin
        LenUtil := (AdrEnd - Blocks[blk].AddressStart) + 1;
        if Blocks[blk].Size<=LenUtil then
          Move(Values[Blocks[blk].AddressStart - AdrStart], Blocks[blk].FValues[0], Blocks[blk].Size*SizeOf(Double))
        else
          Move(Values[Blocks[blk].AddressStart - AdrStart], Blocks[blk].FValues[0], LenUtil*SizeOf(Double));

        Blocks[blk].Updated;
        Blocks[blk].LastError:=LastResult;
        Blocks[blk].ReadSuccess:=Blocks[blk].ReadSuccess+1;
      end else begin
        if (Blocks[blk].AddressEnd>=AdrStart) AND (Blocks[blk].AddressEnd<=AdrEnd) then begin
          LenUtil := (Blocks[blk].AddressEnd - AdrStart) + 1;
          Move(Values[0], Blocks[blk].FValues[Blocks[blk].AddressEnd - AdrStart + 1], LenUtil*SizeOf(Double));
          Blocks[blk].Updated;
          Blocks[blk].LastError:=LastResult;
          Blocks[blk].ReadSuccess:=Blocks[blk].ReadSuccess+1;
        end;
      end;
    end;
    inc(Moved, LenUtil);
    if Moved>=Length(Values) then break;
  end;
  Result:=ifthen(Moved=Length(Values),0,ifthen(Moved<Length(Values),-1,1));
end;

procedure TPLCMemoryManager.SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult);
var
  blk, AdrEnd:Integer;
begin
  AdrEnd := AdrStart + Len * RegSize - 1;

  for blk := 0 to High(Blocks) do begin
    if (Blocks[blk].AddressStart<=AdrStart) AND (Blocks[blk].AddressEnd>=AdrEnd) then begin
      Blocks[blk].ReadFaults := Blocks[blk].ReadFaults+1;
      Blocks[blk].LastError  := Fault;
    end else begin
      if (Blocks[blk].AddressStart>=AdrStart) AND (Blocks[blk].AddressStart<=AdrEnd) then begin
        Blocks[blk].ReadFaults := Blocks[blk].ReadFaults+1;
        Blocks[blk].LastError  := Fault;
      end else begin
        if (Blocks[blk].AddressEnd>=AdrStart) AND (Blocks[blk].AddressEnd<=AdrEnd) then begin
          Blocks[blk].ReadFaults := Blocks[blk].ReadFaults+1;
          Blocks[blk].LastError  := Fault;
        end;
      end;
    end;
  end;
end;

function TPLCMemoryManager.GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):Integer;
var
  blk, AdrEnd, LenUtil, Moved:Integer;
begin
  AdrEnd := AdrStart + Length(Values) - 1;
  Moved:=0;

  for blk := 0 to High(Blocks) do begin
    LenUtil:=0;
    if (Blocks[blk].AddressStart<=AdrStart) AND (Blocks[blk].AddressEnd>=AdrEnd) then begin
      LenUtil := (AdrEnd - AdrStart) + 1;
      LastResult:=Blocks[blk].LastError;
      ValueTimeStamp:=Blocks[blk].LastUpdate;
      Move(Blocks[blk].FValues[AdrStart - Blocks[blk].AddressStart], Values[0], LenUtil*SizeOf(Double))
    end else begin
      if (Blocks[blk].AddressStart>=AdrStart) AND (Blocks[blk].AddressStart<=AdrEnd) then begin
        LenUtil := (AdrEnd - Blocks[blk].AddressStart) + 1;
        LastResult:=Blocks[blk].LastError;
        ValueTimeStamp:=Blocks[blk].LastUpdate;
        if Blocks[blk].Size<=LenUtil then
          Move(Blocks[blk].FValues[0], Values[Blocks[blk].AddressStart - AdrStart], Blocks[blk].Size*SizeOf(Double))
        else
          Move(Blocks[blk].FValues[0], Values[Blocks[blk].AddressStart - AdrStart], LenUtil*SizeOf(Double));
      end else begin
        if (Blocks[blk].AddressEnd>=AdrStart) AND (Blocks[blk].AddressEnd<=AdrEnd) then begin
          LenUtil := (Blocks[blk].AddressEnd - AdrStart) + 1;
          LastResult:=Blocks[blk].LastError;
          ValueTimeStamp:=Blocks[blk].LastUpdate;
          Move(Blocks[blk].FValues[Blocks[blk].AddressEnd - AdrStart + 1], Values[0], LenUtil*SizeOf(Double));
        end
      end;
    end;
    inc(Moved, LenUtil);
    if Moved>=Length(Values) then break;
  end;
  Result:=ifthen(Moved=Length(Values),0,ifthen(Moved<Length(Values),-1,1));
end;

constructor TPLCMemoryManagerSafe.Create;
begin
  inherited Create;
  FMutex:=TCriticalSection.Create;
end;

destructor  TPLCMemoryManagerSafe.Destroy;
begin
  inherited Destroy;
  FMutex.Destroy;
end;

procedure   TPLCMemoryManagerSafe.AddAddress(Address,Size,RegSize,Scan:Cardinal);
begin
  try
    FMutex.Enter;
    inherited AddAddress(Address,Size,RegSize,Scan);
  finally
    FMutex.Leave;
  end;
end;

procedure   TPLCMemoryManagerSafe.RemoveAddress(Address,Size,RegSize:Cardinal);
begin
  try
    FMutex.Enter;
    inherited RemoveAddress(Address,Size,RegSize);
  finally
    FMutex.Leave;
  end;
end;

function    TPLCMemoryManagerSafe.SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble; LastResult:TProtocolIOResult):Integer;
begin
  try
    FMutex.Enter;
    Result := inherited SetValues(AdrStart, Len, RegSize, Values, LastResult);
  finally
    FMutex.Leave;
  end;
end;

function    TPLCMemoryManagerSafe.GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):Integer;
begin
  try
    FMutex.Enter;
    Result := inherited GetValues(AdrStart, Len, RegSize, Values, LastResult, ValueTimeStamp);
  finally
    FMutex.Leave;
  end;
end;

procedure   TPLCMemoryManagerSafe.SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult);
begin
  try
    FMutex.Enter;
    inherited SetFault(AdrStart, Len, RegSize, Fault);
  finally
    FMutex.Leave;
  end;
end;

end.
