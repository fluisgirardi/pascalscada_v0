{:
  @abstract(Classes para organização de blocos de memória de um CLP.)
  @author(Fabio Luis Girardi papelhigienico@gmail.com)
}
unit PLCMemoryMananger;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses SysUtils, DateUtils, hsutils, ProtocolTypes, SyncObjs;

type
  {:
    @abstract(Representa um bit.).
    @author(Fabio Luis Girardi papelhigienico@gmail.com)
  }
  Binary = 0..1;
  {:
    @abstract(Representa uma palavra de 4 bits.)
    @author(Fabio Luis Girardi papelhigienico@gmail.com)
  }
  Nibble  = 0..15;
  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Enumera todos os possíveis tipos de dados numéricos.
  @value(ntBinary Binário (1 bit).)
  @value(ntNibble Nibble (4 bits).)
  @value(ntByte   Byte (8 bits).)
  @value(ntWord   Word (16 bits).)
  @value(ntCardinal  Cardinal (32 bits).)
  @value(ntFloat  Ponto flutuate (até 64 bits de precisão).)
  }
  TNumType = (ntBinary, ntNibble, ntBYTE, ntWORD, ntCardinal, ntFloat);

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Estrutura usada para cadastrar cada endereço único dentro do gerenciador de
  blocos de memória.
  }
  TMemoryRec = record
    Address, Count, MinScan:Integer;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que representa um faixa de endereços de memória continuos (bloco).
  
  @bold(É altamente recomendável a você que está desenvolvendo um driver de
  comunicação, utilizar a classe TPLCMemoryManager, que implementa blocos de
  memória não-continuas. Essa classe faz uso de @name e todos os seus descendentes.)
  
  @seealso(TPLCMemoryManager)
  }
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
    function  GetValue(index:Integer):Double; virtual; abstract;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); virtual; abstract;
  public
    LastError:TProtocolIOResult;
    {:
    Cria um bloco de memórias continuas.
    @param(AdrStart Cardinal. Endereço inicial do bloco.)
    @param(AdrEnd Cardinal. Endereço final do bloco.)
    AdrStart e AdrEnd devem ser passados na menor unidade de memória disponível
    no CLP. Um exemplo são os CLPs da Siemens que utilizam a mesma area
    de memória para bytes, words e DWORDs. então para adicionar a DW0, é necessário
    passar 0 em AdrStart e 3 em AdrEnd, totalizando 4 bytes (que é o menor
    tamanho de palavra disponível no CLP) que são VB0, VB1, VB2, VB3.
    }
    constructor Create(AdrStart,AdrEnd:Cardinal); virtual;
    {:
    Lê/escreve o valor da memória especificada por Index no bloco.
    }
    property Values[Index:Integer]:Double read GetValue write SetValue;
    {:
    Use @name para dizer que os dados estão atualizados. Utilize esse método logo
    após fazer uma leitura de seu dispositivo.
    }
    procedure Updated;
    {:
    @name informa se o bloco precisa ser lido do dispositivo, devido ultrapassar
    o menor tempo de scan do bloco.
    }
    function NeedRefresh:Boolean;
  published
    //: Informa o endereço inicial do bloco.
    property AddressStart:Integer read FStartAddress;
    //: Informa o endereço final do bloco.
    property AddressEnd:Integer read FEndAddress;
    //: Informa o tamanho do bloco.
    property Size:Integer read GetSize;
    //: @name informa quando foi a última atualização dos dados do bloco.
    property LastUpdate:TDateTime read FLastUpdate write FLastUpdate;
    //: @name diz quantos milisegundos se passaram desde a última atualização de dados.
    property MilisecondsFromLastUpdate:Int64 read GetMsecLastUpdate;
    //: Informa qual é o tempo de varredura desse bloco.
    property ScanTime:Cardinal read FMinScanTime write FMinScanTime;
    //: Informa quantas leituras de dados do dispositivo tiveram sucesso.
    property ReadSuccess:Cardinal read FReadOK write SetReadOK;
    //: Informa quantas leituras de dados do dispositivo falharam.
    property ReadFaults:Cardinal read FReadFault write SetReadFault;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de dados binários continuas. Útil para o
  protocolo ModBus onde cada entrada/saida digital tem um endereço.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeBinary = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Binary;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de nibbles (4 bits) continuas.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeNibble = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Nibble;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de bytes (8 bits) continua.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeByte = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Byte;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de Words (16 bits) continua.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeWord = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Word;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de Cardinals (32 bits) continua.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeCardinal = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Cardinal;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Classe que implementa uma área de palavras de ponto flutuante (32/64 bits)
  continua.
  @seealso(TPLCMemoryManager)
  }
  TRegisterRangeFloat = class(TRegisterRange)
  protected
    //: @exclude
    FValues:array of Double;
    //: @exclude
    function  GetValue(index:Integer):Double; override;
    //: @exclude
    procedure SetValue(index:Integer; v:Double); override;
  public
    //: @exclude
    constructor Create(AdrStart,AdrEnd:Cardinal); override;
    //: @exclude
    destructor  Destroy; override;
  end;

  //: @exclude
  TRegisterRangeArray = array of TRegisterRange;
  {:
  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  @abstract(Classe que gerência blocos de memórias não continuos (fragmentados).)
  }
  TPLCMemoryManager = class
  private
    FAddress:array of TMemoryRec;
    FMaxHole:Integer;
    FMaxBlockSize:Integer;
    FMemType:TNumType;
    FCriticalSection:TCriticalSection;
    procedure AddAddress(Add,Scan:Integer); overload;
    procedure RemoveAddress(Add:Integer); overload;
    procedure SetHoleSize(size:Integer);
    procedure SetBlockSize(size:Integer);
    procedure RebuildBlocks;
    function  GetSize:Integer;
    function  CreateRegisterRange(adrStart,adrEnd:Integer):TRegisterRange;
  public
    //: Blocos de memória continuos.
    Blocks:TRegisterRangeArray;
    {:
    Cria um gerenciador de memórias não continuas.
    @param(memType TNumType. Informa qual é o tipo de dados que o bloco está
    gerenciando).
    }
    constructor Create(memType:TNumType);
    //: Destroi o gerenciador de blocos não continuos e todos os seus recursos.
    destructor Destroy; override;
    {:
    Adiciona uma ou mais memórias ao gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo adicionadas ao bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na área.)
    @param(Scan Cardinal. Tempo de varredura da memória.)
    
    Por exemplo, para adicionar as VW0, VW2 e VW4 no Siemens (onde a menor palavra
    é o byte) com 1200ms de scan, você chamaria:
    @code(AddAddress(0,3,2,1200);)
    
    Já nos CLPs Scheneider (onde a menor palvra é de 16 bits), para endereçar
    as words W0, W1 e W2 ficaria assim:
    @code(AddAddress(0,3,1,1200);)
    
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    procedure AddAddress(Address,Size,RegSize,Scan:Cardinal); overload;
    {:
    Remove uma ou mais variáveis do gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo adicionadas ao bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na area.)
    
    Os parametros funcionam de maneira identica a função AddAddress.

    @seealso(AddAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    procedure RemoveAddress(Address,Size,RegSize:Cardinal); overload;
    {:
    @name escreve valores em um intervalo de memórias, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a escrever.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)

    Cada valor representa a menor palavra do bloco.

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
    function  SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble):Integer;
    {:
    @name lê valores intervalo de memórias, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a escrever.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)

    Cada item da array retornado representa o valor da menor palavra daquela área.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    function  GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble):Integer;
    {:
    @name escreve o status da última leitura, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a escrever.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)
    @param(Fault TProtocolIOResult. Status da última leitura.)

    @seealso(SetValues)
    }
    procedure SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult);
  published
    {:
    Define quantos endereços podem ficar sem serem usados para manter a
    continuidade de um bloco. Valores grandes formam poucos grupos de tamanho
    grande, enquanto valores pequenos formam muitos grupos de tamanho pequeno.
    
    Digamos que sejam adicionados os endereços 0, 1, 3 ,4 e MaxHole=0, logo
    serão formados dois blocos, o primeiro contendo os endereços [0, 1] e o
    segundo os endereços [3, 4].
    
    Já se for setado MaxHole=1 será criado um único grupo com os endereços
    [0,1,2,3,4] sendo o endereço 2 adicionado automaticamente para manter a
    continuidade do bloco.
    }
    property MaxHole:Integer read FMaxHole write SetHoleSize;
    {:
    Define qual o tamanho máximo de cada bloco continuo. Se não há limite de
    tamanho, use -1 nessa propriedade.
    
    Supondo que foram adicionados os endereços [0,1,2,3,4] e @name=-1 será criado
    um único bloco com esses mesmos endereços. Supondo que @name=3 serão criados
    dois grupos, o primeiro com os endereços [0,1,2] e o segundo com os endereços
    [3,4].
    }
    property MaxBlockItems:Integer read FMaxBlockSize write SetBlockSize;
    //: Retorna a quantidade total de memórias gerenciadas pelo bloco.
    property Size:Integer read GetSize;
  end;


implementation

uses Math;

constructor TRegisterRange.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create;
  FStartAddress := AdrStart;
  FEndAddress := AdrEnd;
  FReadOK := 0;
  FReadFault := 0;
end;

procedure TRegisterRange.Updated;
begin
  FLastUpdate := now;
end;

function TRegisterRange.GetSize:Integer;
begin
  Result := (FEndAddress-FStartAddress)+1;
end;

function TRegisterRange.GetMsecLastUpdate:Int64;
begin
  Result := MilliSecondsBetween(Now,FLastUpdate);
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
//             inicio da implementacao do TRegisterRangeBinary
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeBinary.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeBinary.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeBinary.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRangeBinary.SetValue(index:Integer; v:Double);
begin
  if v<>0 then
    FValues[index] := 1
  else
    FValues[index] := 0
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio das implementacao do TRegisterRangeNibble
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeNibble.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeNibble.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeNibble.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRangeNibble.SetValue(index:Integer; v:Double);
begin
  FValues[index] := Nibble(FloatToInteger(v) and 15);
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TRegisterRangeByte
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeByte.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeByte.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeByte.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRangeByte.SetValue(index:Integer; v:Double);
begin
  FValues[index] := Byte(FloatToInteger(v) and $FF);
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TRegisterRangeWord
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeWord.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeWord.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeWord.GetValue(index:Integer):Double;
begin
  if (index<0) or (index>High(FValues)) then
    raise Exception.Create('fora dos limites');
  Result := FValues[index];
end;

procedure TRegisterRangeWord.SetValue(index:Integer; v:Double);
begin
  if (index<0) or (index>High(FValues)) then
    raise Exception.Create('fora dos limites');
  FValues[index] := Word(FloatToInteger(v) and $FFFF);
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TRegisterRangeCardinal
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeCardinal.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeCardinal.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeCardinal.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRangeCardinal.SetValue(index:Integer; v:Double);
begin
  FValues[index] := Cardinal(FloatToInteger(v));
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TRegisterRangeCardinal
////////////////////////////////////////////////////////////////////////////////

constructor TRegisterRangeFloat.Create(AdrStart,AdrEnd:Cardinal);
begin
  inherited Create(AdrStart,AdrEnd);
  SetLength(FValues,(AdrEnd-AdrStart)+1);
end;

destructor  TRegisterRangeFloat.Destroy;
begin
  SetLength(FValues,0);
  inherited Destroy;
end;

function  TRegisterRangeFloat.GetValue(index:Integer):Double;
begin
  Result := FValues[index];
end;

procedure TRegisterRangeFloat.SetValue(index:Integer; v:Double);
begin
  FValues[index] := v;
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TPLCMemoryManager
////////////////////////////////////////////////////////////////////////////////

constructor TPLCMemoryManager.Create(memType:TNumType);
begin
  FCriticalSection := TCriticalSection.Create;
  FMemType := memType;
  FMaxHole := 5; //o bloco continua caso de enderecos seja <= 5
  FMaxBlockSize := 0; //o bloco tem seu tamanho restrito a x, 0 = sem restricao!
end;

destructor TPLCMemoryManager.Destroy;
var
  c:integer;
begin
  for c:=0 to High(Blocks) do
    Blocks[c].Destroy;
  SetLength(Blocks,0);
  FCriticalSection.Destroy;
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
  IF Length(FAddress)>=2 THEN begin
    c:=0;
    //procura...
    while (c<Length(FAddress)) and (add>FAddress[c].Address) do
      inc(c);

    if (c<Length(FAddress)) and (FAddress[c].Address=add) then begin
      //se encontrou o endereco...
      inc(FAddress[c].Count);
      FAddress[c].MinScan := Min(FAddress[c].MinScan,Scan);
    end else begin
      h:=Length(FAddress);
      //adiciona mais um na array
      SetLength(FAddress,h+1);
      //se se não chegou no fim, é pq é um endereco
      //que deve ficar no meio da lista para mante-la
      //ordenada
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
  //esse while para quando encontra o endereco desejado ou qdo acaba a lista!!
  while (c<=high(FAddress)) and (add>=FAddress[c].Address) do
    inc(c);
  //se não encontrou cai fora...
  if (c>high(FAddress)) or (FAddress[c].Address<>Add) then exit;
  dec(FAddress[c].Count);
  //caso zerou um endereco, é necessário remover ele da lista...
  if FAddress[c].Count=0 then
    if Length(FAddress)=1 then
        SetLength(FAddress,0)
    else begin
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
  RebuildBlocks; //nao mudei enderecos.
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
      SetLength(newBlocks,Length(newBlocks)+1);

      newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
      newBlocks[BlockIndex].LastUpdate := now;
      newBlocks[BlockIndex].ScanTime := mscan;
      inc(BlockIndex);

      //pega os enderecos de onde comeca um novo bloco...
      adrstart := FAddress[c].Address;
      adrend := adrstart;
      BlockEnd := adrend + FMaxHole + 1;
      mscan := FAddress[c].MinScan;
      BlockItems := 1;
    end else begin
      //bloco continua, adiciona novos enderecos.
      adrend := FAddress[c].Address;
      BlockEnd := adrend + FMaxHole + 1;
      mscan := min(mscan, FAddress[c].MinScan);
      Inc(BlockItems);
    end;
    if c=High(FAddress) then begin
      SetLength(newBlocks,Length(newBlocks)+1);
      newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
      newBlocks[BlockIndex].LastUpdate := now;
      newBlocks[BlockIndex].ScanTime := mscan;
      inc(BlockIndex);
    end;
  end;

  //copia os dados que estavam nos blocos antigos...
  //baseia-se em varer a array de enderecos, verificar em que bloco ela estava
  //e para que bloco o endereco foi parar...

  for c:=0 to High(FAddress) do begin
    found := false;
    for c2 := 0 to High(Blocks) do
      if (FAddress[c].Address>=Blocks[c2].AddressStart) and (FAddress[c].Address<=Blocks[c2].AddressEnd) then begin
        found := true;
        break;
      end;

    //se não encontrou aqui é pq o endereco foi adicionado...
    if not found then continue;

    found := false;
    for c3:= 0 to High(newBlocks) do
      if (FAddress[c].Address>=newBlocks[c3].AddressStart) and (FAddress[c].Address<=newBlocks[c3].AddressEnd) then begin
        found := true;
        break;
      end;

    //se não encontrou aqui é pq o endereco foi deletado...
    if not found then continue;
    BlockOldOffset := FAddress[c].Address - Blocks[c2].AddressStart;
    BlockNewOffset := FAddress[c].Address - newBlocks[c3].AddressStart;
    newBlocks[c3].Values[BlockNewOffset] := Blocks[c2].Values[BlockOldOffset];

    //coloca o menor tempo de atualização para priorizar quem necessita de refresh mais urgente..
    newBlocks[c3].LastUpdate := Min(newBlocks[c3].LastUpdate,Blocks[c2].LastUpdate);
  end;
  //destroi os blocos antigos
  for c:=0 to High(Blocks) do
    Blocks[c].Destroy;
  SetLength(Blocks, 0);

  //copia os valores dos novos blocos para o bloco velho
  Blocks := newBlocks;

  //zera o auxiliar de blocos novos...
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
  case FMemType of
    ntBinary:
      Result := TRegisterRangeBinary.Create(adrStart,adrEnd);
    ntNibble:
      Result := TRegisterRangeNibble.Create(adrStart,adrEnd);
    ntBYTE:
      Result := TRegisterRangeByte.Create(adrStart,adrEnd);
    ntWORD:
      Result := TRegisterRangeWord.Create(adrStart,adrEnd);
    ntCardinal:
      Result := TRegisterRangeCardinal.Create(adrStart,adrEnd);
    else
      Result := TRegisterRangeFloat.Create(adrStart,adrEnd);
  end;
end;

procedure TPLCMemoryManager.AddAddress(Address,Size,RegSize,Scan:Cardinal);
var
  c, items:Cardinal;
  len:Integer;
begin
  FCriticalSection.Enter;
  if (Size<=0) or (RegSize<=0) then
    raise Exception.Create('Tamanho necessita ser no minimo 1!');

  //captura o tamanho da array de enderecos...
  len := length(FAddress);

  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    AddAddress(c,Scan);
    inc(c);
  end;

  //dipara o rebuild blocks, pq foram adicionados enderecos
  if len<>length(FAddress) then
    RebuildBlocks;
  FCriticalSection.Leave;
end;

procedure TPLCMemoryManager.RemoveAddress(Address,Size,RegSize:Cardinal);
var
  c, items:Cardinal;
  len:Integer;
begin
  FCriticalSection.Enter;
  if (Size<=0) or (RegSize=0) then
    raise Exception.Create('Tamanho necessita ser no minimo 1!');

  //captura o tamanho atual...
  len := length(FAddress);
  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    RemoveAddress(c);
    inc(c);
  end;
  //dipara o rebuild blocks, pq foram adicionados enderecos
  if len<>length(FAddress) then
    RebuildBlocks;
  FCriticalSection.Leave;
end;

function TPLCMemoryManager.SetValues(AdrStart,Len,RegSize:Cardinal; Values:TArrayOfDouble):Integer;
var
  count, pos, blk,items,c:Integer;
  found_add:Boolean;
begin
  FCriticalSection.Enter;
  items := Len*RegSize + AdrStart;
  c:=AdrStart;
  count := 0;

  for pos := 0 to High(Values) do begin
    found_add := false;
    for blk := 0 to High(Blocks) do
      if (c>=Blocks[blk].AddressStart) and (c<=Blocks[blk].AddressEnd) then begin
        Blocks[blk].Values[c-Blocks[blk].AddressStart] := Values[pos];
        Blocks[blk].Updated;
        Blocks[blk].ReadSuccess := Blocks[blk].ReadSuccess + 1;
        found_add := true;
        break;
      end;

    if c<(items-1) then
      inc(c)
    else
      break;

    if found_add then
      inc(count);
  end;
  Result := -1;
  if (count>0) and (count<Length(Values)) then
    Result := 0;
  if count>=Length(Values) then
    Result := 1;
  FCriticalSection.Leave;
end;

procedure TPLCMemoryManager.SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult);
var
  items,c,blk:Integer;
begin
  FCriticalSection.Enter;
  items := Len*RegSize + AdrStart;
  c:=AdrStart;

  while c<items do begin
    for blk := 0 to High(Blocks) do
      if (c>=Blocks[blk].AddressStart) and (c<=Blocks[blk].AddressEnd) then begin
        Blocks[blk].ReadFaults := Blocks[blk].ReadFaults + 1;
        Blocks[blk].LastError := Fault;
        break;
      end;
    inc(c);
  end;
  FCriticalSection.Leave;
end;

function TPLCMemoryManager.GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble):Integer;
var
  items,c,blk,pos:Integer;
  found_add:Boolean;
begin
  FCriticalSection.Enter;
  //verifica o tamanho da array de retorno
  //ajusta conforme necessario, e zera ela;
  if Integer(Len)>Length(Values) then begin
    SetLength(Values,Integer(Len));
    for c:=0 to High(Values) do
      Values[c]:=0;
  end;
  items := Len*RegSize + AdrStart;
  c:=AdrStart;
  pos:=0;
  //procura cada endereco...
  while c<items do begin
    found_add:=false;
    for blk := 0 to High(Blocks) do
      if (c>=Blocks[blk].AddressStart) and (c<=Blocks[blk].AddressEnd) then begin
        Values[pos] := Blocks[blk].Values[c-Blocks[blk].AddressStart];
        found_add := true;
        inc(pos);
        break;
      end;
    if not found_add then
      inc(pos);
    inc(c);
  end;
  Result := 1;
  FCriticalSection.Leave;
end;

end.
