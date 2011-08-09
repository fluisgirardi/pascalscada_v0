{$i language.inc}
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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

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
    FPIPEItens:array of TCollectionItem;
    procedure SetInput(value:Double);
    function  GetOutput:Double;
    procedure SetOutput(value:Double);
    procedure DoExceptionIndexOut(index:Integer);
    function  GetProperty(index:Integer):Double;
    procedure SetProperty(index:Integer; Value:Double);
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
    procedure AddPIPEItem(PIPEItem:TCollectionItem);

    {$IFDEF PORTUGUES}
    //: Remove um dependente desse processador de escalas.
    {$ELSE}
    //: Removes a object from the dependent object list.
    {$ENDIF}
    procedure DelPIPEItem(PIPEItem:TCollectionItem);

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
    property Propriedade[index:Integer]:Double read GetProperty write SetProperty;
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
  TScalePIPEItem = class(TCollectionItem)
  private
    SProcessor:TScaleProcessor;
    procedure SetScaleProcessor(SP:TScaleProcessor);
  protected
    //: @exclude
    function  GetDisplayName: string; override;
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
  TScalePIPE = class(TCollection)
  public
    //: @exclude
    constructor Create;

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
    function Add:TScalePIPEItem;

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
  TPIPE = class(TComponent)
  private
    FScalePIPE:TScalePIPE;
    FTags:array of TPLCTag;
    function  GetScalePIPE:TScalePIPE;
    procedure SetScalePIPE(ScalePIPE:TScalePIPE);
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Adiciona um tag como dependente dessa fila.
    {$ELSE}
    //: Adds a tag to the list of dependents of this scale processor set.
    {$ENDIF}
    procedure AddTag(tag:TPLCTag);

    {$IFDEF PORTUGUES}
    //: Remove um tag como dependente dessa fila.
    {$ELSE}
    //: Remove a tag from the dependent list of this scale processor set.
    {$ENDIF}
    procedure DelTag(tag:TPLCTag);
    //: @seealso(TScalePIPE.SetInGetOut)
    function SetInGetOut(Sender:TComponent; Input:Double):Double;
    //: @seealso(TScalePIPE.SetOutGetIn)
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  published

    {$IFDEF PORTUGUES}
    //: Coleção de escalas.
    {$ELSE}
    //: Collection of scale processors.
    {$ENDIF}
    property Escalas:TScalePIPE read GetScalePIPE write SetScalePIPE stored true;
  end;
    
implementation

uses PLCNumber, hsstrings;

////////////////////////////////////////////////////////////////////////////////
// TScalePIPEItem implementation
////////////////////////////////////////////////////////////////////////////////
procedure TScalePIPEItem.SetScaleProcessor(SP:TScaleProcessor);
begin
  if sp=SProcessor then exit;
  
  if SProcessor<>nil then
     SProcessor.DelPIPEItem(self);

  if SP<>nil then
     SP.AddPIPEItem(self);

  DisplayName:=SP.Name;
  SProcessor := SP;
end;

function TScalePIPEItem.GetDisplayName: string;
begin
   if SProcessor<>nil then
      Result := SProcessor.Name
   else
      Result := SEmpty;
end;

function TScalePIPEItem.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetInGetOut(Sender,Input)
  else
     Result := Input;
end;

function TScalePIPEItem.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetOutGetIn(Sender,Output)
  else
     Result := Output;
end;

procedure TScalePIPEItem.RemoveScaleProcessor;
begin
  SProcessor := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TScalePIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TScalePIPE.Create;
begin
  inherited Create(TScalePIPEItem);
end;

function TScalePIPE.Add:TScalePIPEItem;
begin
   Result := TScalePIPEItem(inherited Add)
end;

function TScalePIPE.SetInGetOut(Sender:TComponent; Input:Double):Double;
var
  c:Integer;
begin
  Result := Input;
  for c:=0 to Count-1 do
    if GetItem(c) is TScalePIPEItem then
       Result := TScalePIPEItem(GetItem(c)).SetInGetOut(Sender,Result);
end;

function TScalePIPE.SetOutGetIn(Sender:TComponent; Output:Double):Double;
var
  c:Integer;
begin
  Result := Output;
  for c:=(Count-1) downto 0 do
    if GetItem(c) is TScalePIPEItem then
       Result := TScalePIPEItem(GetItem(c)).SetOutGetIn(Sender,Result);
end;

////////////////////////////////////////////////////////////////////////////////
// TPIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TPIPE.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FScalePIPE := TScalePIPE.Create;
end;

destructor  TPIPE.Destroy;
var
  t:Integer;
begin
  for t:=High(FTags) downto 0 do begin
    TPLCNumber(FTags[t]).ScaleProcessor:=nil;
  end;
  FScalePIPE.Destroy;
  inherited Destroy;
end;

function  TPIPE.GetScalePIPE:TScalePIPE;
begin
  Result := FScalePIPE;
end;

procedure TPIPE.SetScalePIPE(ScalePIPE:TScalePIPE);
begin
  FScalePIPE.Assign(ScalePIPE);
end;

procedure TPIPE.AddTag(tag:TPLCTag);
var
  found:Boolean;
  c:Integer;
begin
  if not (tag is TPLCNumber) then
    raise Exception.Create(SinvalidTag);

  found := false;
  for c:=0 to High(FTags) do
    if FTags[c]=Tag then begin
      found := true;
      break;
    end;

  if not found  then begin
    c:=Length(FTags);
    SetLength(FTags,c+1);
    FTags[c]:=Tag;
  end;
end;

procedure TPIPE.DelTag(tag:TPLCTag);
var
  found:Boolean;
  c,h:Integer;
begin
  found := false;
  h:=High(FTags);
  for c:=0 to h do
    if FTags[c]=Tag then begin
      found := true;
      break;
    end;

  if found then begin
    FTags[c]:=FTags[h];
    SetLength(FTags,h);
  end;
end;

function TPIPE.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
   Result := FScalePIPE.SetInGetOut(Sender,Input);
end;

function TPIPE.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
   Result := FScalePIPE.SetOutGetIn(Sender, Output);
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
  c:Integer;
begin
  SetLength(FProperts,0);
  for c:=0 to High(FPIPEItens) do
    TScalePIPEItem(FPIPEItens[c]).RemoveScaleProcessor;
  SetLength(FPIPEItens,0);
  inherited Destroy;
end;

procedure TScaleProcessor.AddPIPEItem(PIPEItem:TCollectionItem);
var
  found:Boolean;
  c:Integer;
begin
  if not (PIPEItem is TScalePIPEItem) then
    raise Exception.Create(SinvalidType);

  found := false;
  for c:=0 to High(FPIPEItens) do
    if FPIPEItens[c]=PIPEItem then begin
      found := true;
      break;
    end;

  if not found  then begin
    c:=Length(FPIPEItens);
    SetLength(FPIPEItens,c+1);
    FPIPEItens[c]:=PIPEItem;
  end;
end;

procedure TScaleProcessor.DelPIPEItem(PIPEItem:TCollectionItem);
var
  found:Boolean;
  c,h:Integer;
begin
  found := false;
  h:=High(FPIPEItens);
  for c:=0 to h do
    if FPIPEItens[c]=PIPEItem then begin
      found := true;
      break;
    end;

  if found then begin
    FPIPEItens[c]:=FPIPEItens[h];
    SetLength(FPIPEItens,h);
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

function  TScaleProcessor.GetProperty(index:Integer):Double;
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

procedure TScaleProcessor.SetProperty(index:Integer; Value:Double);
begin
  DoExceptionIndexOut(index);
  FProperts[index] := Value;
end;

procedure TScaleProcessor.DoExceptionIndexOut(index:Integer);
begin
  if (index<0) or (index>=Length(FProperts)) then
    raise Exception.Create(SoutOfBounds);
end;


end.
