{:
  @abstract(Implementação de processadores de escala.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
unit ValueProcessor;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCTag;

type
  {:
    @abstract(Classe base processadora de escalas.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
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
    //: Array que armazena o valor das propriedades;
    FProperts:array of Double;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: Adiciona um dependente desse processador de escalas.
    procedure AddPIPEItem(PIPEItem:TCollectionItem);
    //: Remove um dependente desse processador de escalas.
    procedure DelPIPEItem(PIPEItem:TCollectionItem);
    {:
    Fornece um valor processado a partir de um valor puro em função dos
    parametros da escala, se existirem.
    
    @bold(Geralmente é a informação no sentido Equipamento -> Usuário.)

    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Input Double: Valor de entrada.)
    @returns(Double. Valor processado em função dos parametros da escala.)
    }
    function SetInGetOut(Sender:TComponent; Input:Double):Double; virtual;
    {:
    Fornece um valor puro a partir de um valor processado em função dos
    parametros da escala, se existirem.

    @bold(Geralmente é a informação no sentido Usuário -> Equipamento.)
    
    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Output Double: Valor processado da qual se deseja obter um valor puro.)
    @returns(Double. Valor puro em função dos parametros da escala.)
    }
    function SetOutGetIn(Sender:TComponent; Output:Double):Double; virtual;
    //: Retorna uma propriedade da escala da array de propriedades.
    property Propriedade[index:Integer]:Double read GetProperty write SetProperty;
  published
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em OutPut.

    Se for escrito em OutPut, o valor processado será entregue em @name.
    
    @seealso(OutPut)
    }
    property Input:Double read FValueIn write SetInput Stored false;
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em InPut.

    Se for escrito em InPut, o valor processado será entregue em @name.

    @seealso(Input)
    }
    property Output:Double read GetOutput write SetOutput Stored false;
  end;
  
  //: Implementa um item de uma coleção de processadores de escala.
  TScalePIPEItem = class(TCollectionItem)
  private
    SProcessor:TScaleProcessor;
    procedure SetScaleProcessor(SP:TScaleProcessor);
  protected
    //: @exclude
    function  GetDisplayName: string; override;
  public
    {:
    Procedimento chamado para remover a dependencia de um objeto de escalas que
    está sendo destruido.
    }
    procedure RemoveScaleProcessor;
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
    function SetInGetOut(Sender:TComponent; Input:Double):Double;
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
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  published
    //: Objeto de escalas responsável por fazer os processamentos desse item.
    property ScaleProcessor:TScaleProcessor read SProcessor write SetScaleProcessor;
  end;
  
  //: Implementa uma coleção de processadores de escala.
  TScalePIPE = class(TCollection)
  public
    //: @exclude
    constructor Create;
    {:
    Adiciona um novo item de processamento de escalas a coleção.
    @returns(O novo item da coleção.)
    }
    function Add:TScalePIPEItem;
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
    function SetInGetOut(Sender:TComponent; Input:Double):Double;
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
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  end;
  
  //: Componente de enfileiramento de processadores de escala.
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
    //: Adiciona um tag como dependente dessa fila.
    procedure AddTag(tag:TPLCTag);
    //: Remove um tag como dependente dessa fila.
    procedure DelTag(tag:TPLCTag);
    //: @seealso(TScalePIPE.SetInGetOut)
    function SetInGetOut(Sender:TComponent; Input:Double):Double;
    //: @seealso(TScalePIPE.SetOutGetIn)
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  published
    //: Coleção de escalas.
    property Escalas:TScalePIPE read GetScalePIPE write SetScalePIPE stored true;
  end;
    
implementation

uses PLCNumber, hsstrings;

////////////////////////////////////////////////////////////////////////////////
// implementação de TScalePIPEItem
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
// implementação de TScalePIPE
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
// implementação de TPIPE
////////////////////////////////////////////////////////////////////////////////

constructor TPIPE.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FScalePIPE := TScalePIPE.Create;
end;

destructor  TPIPE.Destroy;
begin
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
// implementação de TScaleProcessor
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
