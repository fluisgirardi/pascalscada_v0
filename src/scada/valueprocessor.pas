{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação de processadores de escala.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the scale processors.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ValueProcessor;

interface

uses
  SysUtils, Classes, PLCTag;

type
  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe base processadora de escalas.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    @abstract(Base class for all scale processors.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TScaleProcessor = class(TComponent)
  private
    FValueIn:Double;
    FQueueItems:array of TCollectionItem;
    procedure SetInput(value:Double);
    function  GetOutput:Double;
    procedure SetOutput(value:Double);
    procedure DoExceptionIndexOut(index:LongInt);
    function  GetProperty(index:LongInt):Double;
    procedure SetProperty(index:LongInt; Value:Double);
  protected
    {$IFDEF PORTUGUES}
    //: Array que armazena o valor das propriedades;
    {$ELSE}
    //: Store the values of all properties.
    {$ENDIF}
    FProperts:array of Double;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: Adiciona um dependente desse processador de escalas.
    {$ELSE}
    //: Adds a object to dependents list.
    {$ENDIF}
    procedure AddQueueItem(QueueItem:TCollectionItem);

    {$IFDEF PORTUGUES}
    //: Remove um dependente desse processador de escalas.
    {$ELSE}
    //: Removes a object from the dependent object list.
    {$ENDIF}
    procedure DelQueueItem(QueueItem:TCollectionItem);

    {$IFDEF PORTUGUES}
    {:
    Fornece um valor na escala de engenharia a partir de um valor puro em função
    e dos parametros da escala, se existirem.

    @bold(Geralmente é a informação no sentido Equipamento -> Usuário.)

    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Input Double: Valor de entrada.)
    @returns(Double. Valor processado para a escala de engenharia.)
    }
    {$ELSE}
    {:
    Returns a value in engineering scale based on a raw value and in the scales
    parameters, if there exists.

    @bold(Usually, this value is comming from device and going to user.)

    @param(Sender TComponent: Who is requesting this transformation.)
    @param(Input Double: Input value to be processed.)
    @returns(Double. The value tranformed to engineering scale.)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double; virtual;

    {$IFDEF PORTUGUES}
    {:
    Fornece um valor puro a partir de um valor processado em função dos
    parametros da escala, se existirem.

    @bold(Geralmente é a informação no sentido Usuário -> Equipamento.)

    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Output Double: Valor na escala de engenharia a ser transformado em um valor puro.)
    @returns(Double. Valor puro em função dos parametros da escala.)
    }
    {$ELSE}
    {:
    Returns a raw value based on a value in engineering scale and in scales
    parameters, if there exists.

    @bold(Usually, this value is comming from a user input going to device.)

    @param(Sender TComponent: Who is requesting this value transformation.)
    @param(Output Double: Value in engineering scale to be processed to a raw value.)
    @returns(Double. The tranformed raw value.)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double; virtual;

    {$IFDEF PORTUGUES}
    //: Retorna uma propriedade da escala da array de propriedades.
    {$ELSE}
    //: Returns each property of the scale processor.
    {$ENDIF}
    property Propriedade[index:LongInt]:Double read GetProperty write SetProperty;
  published

    {$IFDEF PORTUGUES}
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em OutPut.

    Se for escrito em OutPut, o valor processado será entregue em @name.

    @seealso(OutPut)
    }
    {$ELSE}
    {:
    Property to test the scale processor.

    If something is written in @name, the transformed value will be returned on OutPut property.

    If something is written in OutPut, the transformed value will be returned on @name property.

    @seealso(OutPut)
    }
    {$ENDIF}
    property Input:Double read FValueIn write SetInput Stored false;

    {$IFDEF PORTUGUES}
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em Input.

    Se for escrito em Input, o valor processado será entregue em @name.

    @seealso(OutPut)
    }
    {$ELSE}
    {:
    Property to test the scale processor.

    If something is written in @name, the transformed value will be returned on Input property.

    If something is written in Input, the transformed value will be returned on @name property.

    @seealso(Input)
    }
    {$ENDIF}
    property Output:Double read GetOutput write SetOutput Stored false;
  end;

  {$IFDEF PORTUGUES}
  //: Implementa um item de uma coleção de processadores de escala.
  {$ELSE}
  //: Implements a item of a scales processors collection.
  {$ENDIF}
  TScaleQueueItem = class(TCollectionItem)
  private
    SProcessor:TScaleProcessor;
    procedure SetScaleProcessor(SP:TScaleProcessor);
  protected
    //: @exclude
    function  GetDisplayName: AnsiString; override;
  public
    {$IFDEF PORTUGUES}
    {:
    Procedimento chamado para remover a dependencia de um objeto de escalas que
    está sendo destruido.
    }
    {$ELSE}
    {:
    Procedure called to remove a dependency with a scale processor object that
    is being destroyed.
    }
    {$ENDIF}
    procedure RemoveScaleProcessor;

    {$IFDEF PORTUGUES}
    {:
    Repassa a chamada para o método SetInGetOut do processador de escalas
    configurado em ScaleProcessor.

    @param(Sender TComponent: Objeto solicitante.)
    @param(Input Double: Valor puro que será processado pela escala.)

    @returns(O valor processado pela escala associada em ScaleProcessor. Caso
    ScaleProcessor não tenha um objeto associado, retorna o valor passado
    em Input.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ELSE}
    {:
    Calls the procedure SetInGetOut of the scale processor, if it's set.

    @param(Sender TComponent: Object that did the request.)
    @param(Input Double: Value to be processed.)

    @returns(The value processed by the scale processor. If there isn't an
    object associated, returns the value of Input.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double;

    {$IFDEF PORTUGUES}
    {:
    Repassa a chamada para o método SetOutGetIn do processador de escalas
    configurado em ScaleProcessor.

    @param(Sender TComponent: Objeto solicitante.)
    @param(Output Double: Valor processado que se deseja obter um valor puro.)

    @returns(O valor puro retornado pela escala associada em ScaleProcessor. Caso
    ScaleProcessor não tenha um objeto associado, retorna o valor passado
    em Output.)

    @seealso(TScaleProcessor.SetOutGetIn)
    }
    {$ELSE}
    {:
    Calls the procedure SetOutGetIn of the scale processor, if it's set.

    @param(Sender TComponent: Object that did the request.)
    @param(Output Double: Value to be processed.)

    @returns(The value processed by the scale processor. If there isn't an
    object associated, returns the value of Output parameter.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  published

    {$IFDEF PORTUGUES}
    //: Objeto de escalas responsável por fazer os processamentos desse item.
    {$ELSE}
    //: Scale processor object that does the values transformations of this item.
    {$ENDIF}
    property ScaleProcessor:TScaleProcessor read SProcessor write SetScaleProcessor;
  end;

  {$IFDEF PORTUGUES}
  //: Implementa uma coleção de processadores de escala.
  {$ELSE}
  //: Implements a collection of scale processors.
  {$ENDIF}
  TScaleQueue = class(TCollection)
  private
    FOwner:TPersistent;
  protected
    //: @exclude
    function GetOwner: TPersistent; override;
  public
    //: @exclude
    constructor Create(aOwner:TPersistent);

    {$IFDEF PORTUGUES}
    {:
    Adiciona um novo item de processamento de escalas a coleção.
    @returns(O novo item da coleção.)
    }
    {$ELSE}
    {:
    Adds a new item to collection.
    @returns(The new item of the collection.)
    }
    {$ENDIF}
    function Add:TScaleQueueItem;

    {$IFDEF PORTUGUES}
    {:
    Tranforma um valor puro (Entrada) em um valor processado pelas multiplas
    escalas pertencentes a coleção (Saida).

    Para isso ele passa Input para o método SetInGetOut do primeiro item da
    coleção e o resultado ele repassa como parametro do próximo item coleção,
    repetindo isso até atingir o fim da lista.

    @bold(Logo, o primeiro item da lista é primeiro a ser chamado quando o valor
    vem no sentido Equipamento -> Usuário assim como o último item da coleção é
    o primeiro a ser chamado quando o valor vai do Usuário -> Equipamento.)

    @param(Sender TComponent: Quem chamou esse processamento.)
    @param(Input Double: Valor puro a processar.)
    @returns(Retorna o valor processado em função das escalas associadas aos
             itens da coleção. Se não há itens na coleção ou se os itens dela não
             tiverem um processador de escala associado, Input é retornado.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ELSE}
    {:
    Process a raw value (Input) to a value in engineering scale, processed by
    each scale processors of the collection.

    To do this, this method passes the Input parameter to TScalePIPEItem.SetInGetOut
    of the first item of the collection, takes the result and passes it again as
    Input of TScalePIPEItem.SetInGetOut of the next item of the collection,
    until reach the end of the collection.

    @bold(So, the first item of the collection is the first that will be called
    when the value is comming from device and going to User AND so as the last
    item of the collection o is the first to be called when the value is comming
    from the user and going to device.)

    @param(Sender TComponent: Object that has requested the value transformation.)
    @param(Input Double: Value to be transformed to a value in engineering scale.)
    @returns(Returns the value processed by all items of the collection. If the
             collection is empty or all items of the collection aren't set
             correctly (a value processor isn't set), returns the value given in
             Input parameter.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double;

    {$IFDEF PORTUGUES}
    {:
    Tranforma um valor processado pelas multiplas escalas da coleção (Saida) em
    um valor puro (Entrada).

    Para isso ele passa Output para o método SetOutGetIn do último item da
    coleção e o resultado ele repassa como parametro do item que o antecede,
    repetindo isso até atingir o inicio da coleção.

    @bold(Logo, o primeiro item da lista é primeiro a ser chamado quando o valor
    vem no sentido Equipamento -> Usuário assim como o último item da coleção é
    o primeiro a ser chamado quando o valor vai do Usuário -> Equipamento.)

    @param(Sender TComponent: Quem chamou esse processamento.)
    @param(Output Double: Valor processado da qual se deseja obter um valor puro.)
    @returns(Retorna o valor puro em função das escalas associadas aos
             itens da coleção. Se não há itens na coleção ou se os itens dela não
             tiverem um processador de escala associado, Output é retornado.)
    @seealso(TScalePIPEItem.SetOutGetIn)
    }
    {$ELSE}


    {:
    Process a raw value in engineering scale to a raw value, processed by each
    scale processors of the collection.

    Para fazer isso, esse método passa o parâmetro de entrada para
    TScalePIPEItem.SetInGetOut do último item da coleção, leva o resultado e
    passa-lo novamente como entrada de TScalePIPEItem.SetInGetOut do item que
    precede o item atual da coleção, até chegar ao início da coleção.

    @bold(So, the last item of the collection is the first that will be called
          when the value is comming from the user and going to device AND so as
          the first item of the collection o is the last to be called when the
          value is comming from the device and going to user.)

    @param(Sender TComponent: Object that has requested the value transformation.)
    @param(Output Double: Value in engineering scale to be transformed to a raw value.)
    @returns(Returns the value processed by all items of the collection. If the
             collection is empty or all items of the collection aren't set
             correctly (a value processor isn't set), returns the value given in
             Outpu parameter.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  end;
  

  {$IFDEF PORTUGUES}
  //: Componente de enfileiramento de processadores de escala.
  {$ELSE}
  //: Scale processors queue.
  {$ENDIF}
  TScalesQueue = class(TScaleProcessor)
  private
    FScaleQueue:TScaleQueue;
    FTags:array of TPLCTag;
    function  GetScaleQueue:TScaleQueue;
    procedure SetScaleQueue(ScaleQueue:TScaleQueue);
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    //: @seealso(TScalePIPE.SetInGetOut)
    function SetInGetOut(Sender: TComponent; aInput: Double): Double; override;
    //: @seealso(TScalePIPE.SetOutGetIn)
    function SetOutGetIn(Sender: TComponent; aOutput: Double): Double; override;
  published

    {$IFDEF PORTUGUES}
    //: Coleção de escalas.
    {$ELSE}
    //: Collection of scale processors.
    {$ENDIF}
    property Escalas:TScaleQueue read GetScaleQueue write SetScaleQueue stored false; // to be removed after 1.0
    property ScalesQueue:TScaleQueue read GetScaleQueue write SetScaleQueue stored true;
  end;

  TPIPE = class(TScalesQueue);
    
implementation

uses PLCNumber, hsstrings;

////////////////////////////////////////////////////////////////////////////////
// TScaleQueueItem implementation
////////////////////////////////////////////////////////////////////////////////
procedure TScaleQueueItem.SetScaleProcessor(SP:TScaleProcessor);
begin
  if SP=Collection.Owner then
    raise Exception.Create(SInvalidQueueOperation);

  if sp=SProcessor then exit;

  if SProcessor<>nil then
     SProcessor.DelQueueItem(self);

  if SP<>nil then
     SP.AddQueueItem(self);

  DisplayName:=SP.Name;
  SProcessor := SP;
end;

function TScaleQueueItem.GetDisplayName: AnsiString;
begin
   if SProcessor<>nil then
      Result := SProcessor.Name
   else
      Result := SEmpty;
end;

function TScaleQueueItem.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetInGetOut(Sender,Input)
  else
     Result := Input;
end;

function TScaleQueueItem.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetOutGetIn(Sender,Output)
  else
     Result := Output;
end;

procedure TScaleQueueItem.RemoveScaleProcessor;
begin
  SProcessor := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TScalePIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TScaleQueue.Create(aOwner:TPersistent);
begin
  inherited Create(TScaleQueueItem);
  FOwner:=aOwner;
end;

function TScaleQueue.GetOwner:TPersistent;
begin
  Result:=FOwner;
end;

function TScaleQueue.Add:TScaleQueueItem;
begin
   Result := TScaleQueueItem(inherited Add)
end;

function TScaleQueue.SetInGetOut(Sender:TComponent; Input:Double):Double;
var
  c:LongInt;
begin
  Result := Input;
  for c:=0 to Count-1 do
    if GetItem(c) is TScaleQueueItem then
       Result := TScaleQueueItem(GetItem(c)).SetInGetOut(Sender,Result);
end;

function TScaleQueue.SetOutGetIn(Sender:TComponent; Output:Double):Double;
var
  c:LongInt;
begin
  Result := Output;
  for c:=(Count-1) downto 0 do
    if GetItem(c) is TScaleQueueItem then
       Result := TScaleQueueItem(GetItem(c)).SetOutGetIn(Sender,Result);
end;

////////////////////////////////////////////////////////////////////////////////
// TPIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TScalesQueue.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FScaleQueue := TScaleQueue.Create(Self);
end;

destructor  TScalesQueue.Destroy;
var
  t:LongInt;
begin
  for t:=High(FTags) downto 0 do begin
    TPLCNumber(FTags[t]).ScaleProcessor:=nil;
  end;
  FScaleQueue.Destroy;
  inherited Destroy;
end;

function  TScalesQueue.GetScaleQueue:TScaleQueue;
begin
  Result := FScaleQueue;
end;

procedure TScalesQueue.SetScaleQueue(ScaleQueue:TScaleQueue);
begin
  FScaleQueue.Assign(ScaleQueue);
end;

function TScalesQueue.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
   Result := FScaleQueue.SetInGetOut(Sender,aInput);
end;

function TScalesQueue.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
   Result := FScaleQueue.SetOutGetIn(Sender, aOutput);
end;

////////////////////////////////////////////////////////////////////////////////
// TScaleProcessor implementation
////////////////////////////////////////////////////////////////////////////////
constructor TScaleProcessor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SetLength(FProperts,8);
end;

destructor TScaleProcessor.Destroy;
var
  c:LongInt;
begin
  SetLength(FProperts,0);
  for c:=0 to High(FQueueItems) do
    TScaleQueueItem(FQueueItems[c]).RemoveScaleProcessor;
  SetLength(FQueueItems,0);
  inherited Destroy;
end;

procedure TScaleProcessor.AddQueueItem(QueueItem:TCollectionItem);
var
  found:Boolean;
  c:LongInt;
begin
  if not (QueueItem is TScaleQueueItem) then
    raise Exception.Create(SinvalidType);

  found := false;
  for c:=0 to High(FQueueItems) do
    if FQueueItems[c]=QueueItem then begin
      found := true;
      break;
    end;

  if not found  then begin
    c:=Length(FQueueItems);
    SetLength(FQueueItems,c+1);
    FQueueItems[c]:=QueueItem;
  end;
end;

procedure TScaleProcessor.DelQueueItem(QueueItem:TCollectionItem);
var
  found:Boolean;
  c,h:LongInt;
begin
  found := false;
  h:=High(FQueueItems);
  for c:=0 to h do
    if FQueueItems[c]=QueueItem then begin
      found := true;
      break;
    end;

  if found then begin
    FQueueItems[c]:=FQueueItems[h];
    SetLength(FQueueItems,h);
  end;
end;

function TScaleProcessor.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
  Result := Input;
end;

function TScaleProcessor.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
  Result := Output;
end;

function  TScaleProcessor.GetProperty(index:LongInt):Double;
begin
  DoExceptionIndexOut(index);
  Result := FProperts[index];
end;

procedure TScaleProcessor.SetInput(value:Double);
begin
  FValueIn := value;
end;

procedure TScaleProcessor.SetOutput(value:Double);
begin
  FValueIn := SetOutGetIn(self, value);
end;

function  TScaleProcessor.GetOutput:Double;
begin
  Result := SetInGetOut(self, FValueIn);
end;

procedure TScaleProcessor.SetProperty(index:LongInt; Value:Double);
begin
  DoExceptionIndexOut(index);
  FProperts[index] := Value;
end;

procedure TScaleProcessor.DoExceptionIndexOut(index:LongInt);
begin
  if (index<0) or (index>=Length(FProperts)) then
    raise Exception.Create(SoutOfBounds);
end;


end.
