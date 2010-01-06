{:
@abstract(Unit de coleções de zonas gráficas e de texto.)
@author(Fabio Luis Girardi)
}
unit HMIZones;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, hsutils, Controls, Graphics {$IFNDEF FPC}, StdCtrls{$ENDIF};

type
  {:
  Define a condição de seleção de uma zona.
  }
  TZoneTypes = (ztEqual, ztRange, ztBit, ztNotEqual, ztOutOfRange, ztGreaterThan, ztLessThan);

  {:
  @name é classe que implementa a base de uma zona. Estas zonas são selecionadas
  atraves do valor do tag associado ao objeto dono do conjunto de zonas aplicado
  a expressão de seleção da zona.
  @seealso(TZones)
  }
  TZone = class(TCollectionItem)
  private
     FValue1,FValue2:Double;
     FIncludeV1, FIncludeV2:Boolean;
     FBlinkTime:Cardinal;
     FBlinkWith:TZone;
     FBlinkWithIndex:Integer;
     FDefaultZone:Boolean;
     FZoneType:TZoneTypes;
     FReferencedBy:Array of TZone;
     function  GetBlinkWithZoneNumber:Integer;
     procedure SetBlinkWithZoneNumber(v:Integer);
     procedure SetV1(v:Double);
     procedure SetV2(v:Double);
     procedure SetIncV1(v:Boolean);
     procedure SetIncV2(v:Boolean);
     procedure SetBlinkTime(v:Cardinal);
     procedure SetAsDefaultZone(v:Boolean);
     procedure SetZoneType(zt:TZoneTypes);
     procedure RemoveBlinkZone;
     procedure AddReference(RefBy:TZone);
     procedure RemReference(RefBy:TZone);
  protected
     {: @exclude }
     procedure NotifyChange;
     {: @exclude }
     function  GetDisplayName: string; override;
  public
     {: @exclude }
     constructor Create(Collection: TCollection); override;
     {: @exclude }
     destructor Destroy; override;
     {: @exclude }
     procedure Loaded;
  published
     {:
     Valor principal.
     @seealso(ZoneType)
     }
     property Value1:Double read FValue1 write SetV1;
     {:
     Valor secundário.
     @seealso(ZoneType)
     }
     property Value2:Double read FValue2 write SetV2;
     {:
     Flag auxiliar da proprieade Value1.
     @seealso(ZoneType)
     }
     property IncludeValue1:Boolean read FIncludeV1 write SetIncV1;
     {:
     Flag auxiliar da proprieade Value2.
     @seealso(ZoneType)
     }
     property IncludeValue2:Boolean read FIncludeV2 write SetIncV2;
     {:
     @name informa o tempo em milisegundos que a zona ficara visivel. Após esse
     tempo chama a próxima zona definida por BlinkWith.
     @seealso(BlinkWith)
     }
     property BlinkTime:Cardinal read FBlinkTime write SetBlinkTime;
     {:
     @name informa qual será a próxima zona a ser chamada para gerar o efeito de
     pisca/animação após o tempo de exibição da zona.
     @seealso(BlinkTime)
     }
     property BlinkWith:Integer read GetBlinkWithZoneNumber write SetBlinkWithZoneNumber nodefault;
     {:
     Se @true torna a zona padrão, exibindo-a quando nenhuma zona for selecionada
     em função do valor do tag.
     @seealso(ZoneType)
     }
     property DefaultZone:Boolean read FDefaultZone write SetAsDefaultZone;
     {:
     @name define qual vai ser a condição que vai selecionar ou descartar a zona
     em questão.
     
     Seus possíveis valores são:
     
     @bold(ztEqual:)       O valor do @noAutoLink(Tag) tem que ser igual ao valor
                           da propriedade Value1 da zona para ela ser selecionada.

     @bold(ztRange:)       O valor do @noAutoLink(Tag) tem que estar entre os
                           valores das propriedades Value1 e Value2 (maior que
                           Value1 E menor que Value2) da zona para ela ser
                           selecionada. Value1 é o limite inferior e Value2 é o
                           limite superior. Para incluir Value1 ou Value2  (maior
                           ou igual que Value1 E menor ou igual que Value2) use
                           as propriedades IncludeValue1 e IncludeValue2.

     @bold(ztBit:)         Para a zona ser selecionada, o bit Valor1 da palavra
                           formada pelo @noAutoLink(tag) tem que ter seu valor
                           booleano igual ao da propriedade IncludeValue1.

     @bold(ztNotEqual:)    Para a zona ser selecionada, o valor do @noAutoLink(tag)
                           tem que ser diferente do valor presente na propriedade
                           Value1.
                          
     @bold(ztOutOfRange:)  O valor do @noAutoLink(Tag) não pode estar entre os
                           valores das propriedades Value1 e Value2 (menor que
                           Value1 OU maior que Value2) da zona para ela ser
                           selecionada. Value1 é o limite inferior e Value2 é o
                           limite superior da exclusão do range. Para incluir
                           Value1 ou Value2 (menor ou igual que Value1 OU
                           maior ou igual que Value2) use as propriedades
                           IncludeValue1 e IncludeValue2.
     
     @bold(ztGreaterThan:) Para a zona ser selecionada, o valor do @noAutoLink(tag)
                           tem que ser maior que o valor da propriedade Value1.
                           Para incluir o valor de Value1 na condição (maior ou
                           igual que) coloque o valor @true IncludeValue1.

     @bold(ztLessThan:)    Para a zona ser selecionada, o valor do @noAutoLink(tag)
                           tem que ser menor que o valor da propriedade Value1.
                           Para incluir o valor de Value1 na condição (menor ou
                           igual que) coloque o valor @true IncludeValue1.
                           
     @seealso(Value1)
     @seealso(Value2)
     @seealso(IncludeValue1)
     @seealso(IncludeValue2)
     @seealso(TZoneTypes)
     @seealso(DefaultZone)
     }
     property ZoneType:TZoneTypes read FZoneType write SetZoneType;
  end;

  {:
  Evento chamado quando é necessário saber o atual estado do componente.
  @seealso(TZones)
  }
  TNeedCompStateEvent = procedure(var CurState:TComponentState) of object;

  {:
  @name implementa a classe base para uma coleção de zonas.
  @seealso(TZone)
  }
  TZones = class(TCollection)
  private
     FOnZoneChange:TNotifyEvent;
     FOnNeedCompState:TNeedCompStateEvent;
     FComponentState:TComponentState;

  protected
     //: @exclude
     function GetComponentState:TComponentState;
     //: @exclude
     procedure NeedCurrentCompState;
  published
     {:
     @name é o evento chamado quando há alterações de alguma propriedade de
     alguma zona. Use esse evento para calcular qual é a nova zona escolhida
     em função dos novos parametros.
     }
     property OnZoneChange:TNotifyEvent read FOnZoneChange write FOnZoneChange;
     {:
     @name é o evento chamado quando uma zona ou a coleção de zonas precisa saber
     qual é o atual estado do componente. Este evento também é chamado quando o
     método NeedCurrentCompState é chamado.
     @seealso(ZonesState)
     }
     property OnNeedCompState:TNeedCompStateEvent read FOnNeedCompState write FOnNeedCompState;
  public
     //: @exclude
     constructor Create(ItemClass: TCollectionItemClass);
     {:
     Este método deve ser chamado através do método Loaded de seu componente para
     informar para as zonas que a partir de agora elas devem operar normalmente e
     não mais no modo de carga de configurações. @bold(Se este método não for
     chamado as zonas não vão se comportar da maneira esperada).
     }
     procedure Loaded;
     {:
     @name retorna uma zona em função do valor e dos critérios das zonas
     pertencentes a coleção.
     @param(v Double. Valor numérico a passar pelos critérios de seleção das
            zonas.)
    
     @return(@name retorna a primeira zona que tenha seu critério de seleção
             satisfeito, ou caso não tenha nenhuma zona escolhida, traz a zona
             marcada como padrão. Se não for escolhida nenhuma zona e não há
             nenhuma zona padrão retorna @nil).
     }
     function  GetZoneFromValue(v:Double):TZone; virtual;
     {:
     Propriedade que lê o estado do componente e o repassa para a coleção de
     zonas. Usa o evento OnNeedCompState para obter o atual estado.
     @seealso(OnNeedCompState)
     }
     property  ZonesState:TComponentState read GetComponentState;
  end;

  {:
  Implementa uma zona de texto com várias opções de formatação.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TTextZones)
  }
  TTextZone = class(TZone)
  private
     FText:TCaption;
     FColor:TColor;
     FTransparent:Boolean;
     FFont:TFont;
     FHorAlignment:TAlignment;
     FVerAlignment:TTextLayout;
     procedure FontChanges(Sender:TObject);
     procedure SetText(t:TCaption);
     procedure SetColor(c:TColor);
     procedure SetTransparent(b:Boolean);
     procedure SetFont(f:TFont);
     procedure SetHorAlignment(x:TAlignment);
     procedure SetVerAlignment(x:TTextLayout);
  public
    //: @exclude
    constructor Create(Collection: TCollection); override;
  published
     {:
     Alinhamento horizontal do texto da zona. Os valores possiveis são:

     @bold(taLeftJustify:) Alinha o texto a esquerda do componente.
     
     @bold(taRightJustify:) Alinha o texto a direita do componente.

     @bold(taCenter:) Alinha o texto no centro do componente.

     @seealso(Color)
     @seealso(Font)
     @seealso(Text)
     @seealso(Transparent)
     @seealso(VerticalAlignment)
     }
     property  HorizontalAlignment:TAlignment read FHorAlignment write SetHorAlignment default taLeftJustify;
     {:
     Alinhamento vertical do texto da zona. Os valores possíveis são:

     @bold(tlTop:) Alinha o texto no topo do componente.

     @bold(tlCenter:) Alinha o texto no centro do componente.

     @bold(tlBottom:) Alinha o texto no fundo (parte baixa) do componente.

     @seealso(Color)
     @seealso(Font)
     @seealso(Text)
     @seealso(Transparent)
     @seealso(HorizontalAlignment)
     }
     property  VerticalAlignment:TTextLayout read FVerAlignment write SetVerAlignment default tlTop;
     {:
     Texto a ser exibido na zona.
     @seealso(Color)
     @seealso(Font)
     @seealso(Transparent)
     @seealso(HorizontalAlignment)
     @seealso(VerticalAlignment)
     }
     property  Text:TCaption read FText write SetText;
     {:
     Cor que será exibida no fundo da zona.
     @seealso(Font)
     @seealso(Text)
     @seealso(Transparent)
     @seealso(HorizontalAlignment)
     @seealso(VerticalAlignment)
     }
     property  Color:TColor read FColor write SetColor;
     {:
     Caso @false, usa a cor de fundo da zona informada em Color. Caso @true
     deixa o fundo da zona transparente.
     @seealso(Color)
     @seealso(Font)
     @seealso(Text)
     @seealso(HorizontalAlignment)
     @seealso(VerticalAlignment)
     }
     property  Transparent:Boolean read FTransparent write SetTransparent;
     {:
     Fonte de formatação do texto (forma, tamanho e cor).
     @seealso(Color)
     @seealso(Text)
     @seealso(Transparent)
     @seealso(HorizontalAlignment)
     @seealso(VerticalAlignment)
     }
     property  Font:TFont read FFont write SetFont;
  end;

  {:
  Coleção de zonas de texto.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TTextZone)
  }
  TTextZones = class(TZones)
  public
    //: @exclude
    constructor Create;
    //: Adiciona uma nova zona de texto na coleção.
    function Add:TTextZone;
  end;

  {:
  Implementa uma zona gráfica.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TGraphicZones)
  }
  TGraphicZone = class(TZone)
  private
     FILIsDefault:Boolean;
     FFileName:String;
     FImageList:TImageList;
     FImageIndex:Integer;
     FColor:TColor;
     FTransparent:Boolean;
     procedure SetILAsDefault(b:Boolean);
     procedure SetFileName(fn:String);
     procedure SetImageList(il:TImageList);
     procedure SetImageIndex(index:Integer);
     procedure SetColor(c:TColor);
     procedure SetTransparent(b:Boolean);
  public
     //: @exclude
     constructor Create(Collection: TCollection); override;
  published
     {:
     Caso as propriedades FileName, ImageList e ImageIndex estejam configuradas
     corretamente, @name permite você escolher qual será o recurso primário.
     Caso @true, o recurso padrão de imagens é o ImageList, caso contrário torna
     a busca de imagens do disco como padrão.
     @seealso(FileName)
     @seealso(ImageList)
     @seealso(ImageIndex)
     }
     property ImageListAsDefault:Boolean read FILIsDefault write SetILAsDefault default true;
     {:
     Caso a zona seja escolhida, exibe a imagem apontada por esse caminho (caso exista).
     @seealso(ImageListAsDefault)
     }
     property FileName:String read FFileName write SetFileName nodefault;
     {:
     Informa qual a lista de imagens que será usada pela zona gráfica.
     @seealso(ImageListAsDefault)
     @seealso(ImageIndex)
     }
     property ImageList:TImageList read FImageList write SetImageList;
     {:
     Informa qual imagem do ImageList que será exibido pela zona gráfica.
     @seealso(ImageListAsDefault)
     @seealso(ImageList)
     }
     property ImageIndex:Integer read FImageIndex write SetImageIndex stored true nodefault;
     {:
     Informa qual cor será interpretada como transparente pela zona gráfica caso
     a propriedade Transparent esteja habilitada.
     @seealso(Transparent)
     }
     property TransparentColor:TColor read FColor write SetColor default clWhite;
     {:
     Habilita/desabilita o troca da cor TransparentColor pelo transparente.
     @seealso(TransparentColor)
     }
     property Transparent:Boolean read FTransparent write SetTransparent default true;
  end;

  {:
  Coleção de zonas gráficas.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TGraphicZone)
  }
  TGraphicZones = class(TZones)
  public
    //: @exclude
    constructor Create;
    //: Adiciona uma nova zona gráfica a coleção.
    function Add:TGraphicZone;
  end;

implementation

constructor TZone.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FBlinkWithIndex := -1;
   if Collection is TZones then
      TZones(Collection).NeedCurrentCompState;
end;

destructor TZone.Destroy;
var
   i:Integer;
begin
   for i:=0 to High(FReferencedBy) do
      FReferencedBy[i].RemoveBlinkZone;
   inherited Destroy;
end;

procedure TZone.SetV1(v:Double);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FValue1:=v;
      exit;
   end;
   if v=FValue1 then exit;
   if ZoneType=ztBit then begin
      if (v>31) or (v<0) then
         raise Exception.Create('Para o tipo de comparação bit, o valor máximo deve ser 31 e o minimo deve ser 0 (ZERO)!');
      FValue1 := Int(v);
   end else begin
      if v>FValue2 then begin
         FValue1:=FValue2;
         FValue2:=v;
      end else
         FValue1:=v;
   end;
   NotifyChange;
end;

function  TZone.GetBlinkWithZoneNumber:Integer;
begin
   if FBlinkWith<>nil then
      Result := FBlinkWith.Index
   else
      Result := -1;
end;

procedure TZone.SetBlinkWithZoneNumber(v:Integer);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FBlinkWithIndex:=v;
      exit;
   end;

   if (v>=0) and (v<Collection.Count) then begin
      if Collection.Items[v]=Self then
         raise Exception.Create('A zona escolhida para piscar não pode ser ela mesma!');
      FBlinkWith:=TZone(Collection.Items[v]);
      FBlinkWith.AddReference(Self);
   end else begin
      if v<>-1 then
         raise Exception.Create('Fora do range!');
      if FBlinkWith<>nil then
         FBlinkWith.RemReference(Self);
      FBlinkWith:=nil;
   end;
end;

procedure TZone.SetV2(v:Double);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FValue2:=v;
      exit;
   end;

   if v=FValue2 then exit;
   if v<FValue1 then begin
      FValue2:=FValue1;
      FValue1:=v;
   end else
      FValue2:=v;
   NotifyChange;
end;

procedure TZone.SetIncV1(v:Boolean);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FIncludeV1:=v;
      exit;
   end;
   
   if v=FIncludeV1 then exit;
   FIncludeV1:=v;
   NotifyChange;
end;

procedure TZone.SetIncV2(v:Boolean);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FIncludeV2:=v;
      exit;
   end;

   if v=FIncludeV2 then exit;
   FIncludeV2:=v;
   NotifyChange;
end;

procedure TZone.SetBlinkTime(v:Cardinal);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FBlinkTime:=v;
      exit;
   end;

   if v=FBlinkTime then exit;
   FBlinkTime:=v;
   NotifyChange;
end;

procedure TZone.SetAsDefaultZone(v:Boolean);
var
   c:Integer;
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FDefaultZone:=v;
      exit;
   end;

   if v=FDefaultZone then exit;
   
   if v then
      with Collection as TZones do begin
         for c:=0 to Count-1 do
            if (Items[c]<>Self) and (Items[c] is TZone) then
               TZone(Items[c]).DefaultZone := false
      end;
   FDefaultZone:=v;
   NotifyChange;
end;

procedure TZone.SetZoneType(zt:TZoneTypes);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FZoneType:=zt;
      exit;
   end;

   if zt=FZoneType then exit;
   
   if (zt=ztBit) and ((FValue1>31) or (FValue1<0)) then
      raise Exception.Create('Para a comparação do tipo ztBit o valor de Value1 precisa ser maior ou igual a 0 (Zero) e menor ou igual a 31!');
   FZoneType:=zt;
   NotifyChange;
end;

procedure TZone.RemoveBlinkZone;
begin
   FBlinkWith:=nil;
end;

procedure TZone.AddReference(RefBy:TZone);
var
   h,i:Integer;
begin
   for i:=0 to High(FReferencedBy) do
      if FReferencedBy[i]=RefBy then exit;
   h:=Length(FReferencedBy);
   SetLength(FReferencedBy, h+1);
   FReferencedBy[h]:=RefBy;
end;

procedure TZone.RemReference(RefBy:TZone);
var
   h,i,p:Integer;
   found:boolean;
begin
   found := false;
   for i:=0 to High(FReferencedBy) do
      if FReferencedBy[i]=RefBy then begin
         p:=i;
         found:=true;
      end;
   if not found then exit;
   h:=High(FReferencedBy);
   FReferencedBy[p]:=FReferencedBy[h];
   SetLength(FReferencedBy, h);
end;

procedure TZone.Loaded;
begin
   SetBlinkWithZoneNumber(FBlinkWithIndex);
end;

function  TZone.GetDisplayName: string;
begin
  case FZoneType of
    ztEqual:
       Result:='Value='+FloatToStr(Value1);

    ztRange:
    begin
       if FIncludeV1 then
          Result:='(Value>='+FloatToStr(Value1)
       else
          Result:='(Value>'+FloatToStr(Value1);

       if FIncludeV2 then
          Result:=Result + ') AND (Value<='+FloatToStr(Value2)+')'
       else
          Result:=Result + ') AND (Value<'+FloatToStr(Value2)+')';
    end;

    ztBit:
    begin
       if FIncludeV1 then
          Result := 'Value.Bit'+FormatFloat('00',FValue1)+'=ON'
       else
          Result := 'Value.Bit'+FormatFloat('00',FValue1)+'=OFF';
    end;

    ztNotEqual:
       Result:='Value<>'+FloatToStr(Value1);

    ztOutOfRange:
    begin
       if FIncludeV1 then
          Result:='(Value<='+FloatToStr(Value1)
       else
          Result:='(Value<'+FloatToStr(Value1);

       if FIncludeV2 then
          Result:=Result + ') OR (Value>='+FloatToStr(Value2)+')'
       else
          Result:=Result + ') OR (Value>'+FloatToStr(Value2)+')';
    end;
    ztGreaterThan:
    begin
       if FIncludeV1 then
          Result:='(Value>='+FloatToStr(Value1)+')'
       else
          Result:='(Value>'+FloatToStr(Value1)+')';
    end;
    ztLessThan:
    begin
       if FIncludeV1 then
          Result:='(Value<='+FloatToStr(Value1)+')'
       else
          Result:='(Value<'+FloatToStr(Value1)+')';
    end;
  end;
end;

procedure TZone.NotifyChange;
begin
   with Collection as TZones do
      if Assigned(OnZoneChange) then
         OnZoneChange(Self);
end;

//############################################################
// TZones implementation
//############################################################

constructor TZones.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

procedure TZones.Loaded;
var
   i:Integer;
begin
   for i:=0 to Count-1 do
      TZone(Items[i]).Loaded;
end;

function TZones.GetComponentState:TComponentState;
begin
   NeedCurrentCompState;
   Result := FComponentState;
end;

//seleciona a zona de acordo com seu critério de seleção
//se duas zonas responderem a um valor, ele irá retornar
//a primeira zona encontrada
function TZones.GetZoneFromValue(v:Double):TZone;
var
   c, value, bit:Integer;
   found:Boolean;
begin
   Result:=nil;
   found := false;
   for c := 0 to Count - 1 do begin

      if (not found) and TZone(Items[c]).DefaultZone then begin
         Result := TZone(Items[c]);
         found := true;
      end;

      with Items[c] as TZone do
         case ZoneType of
            ztEqual:
               if v=Value1 then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            ztRange:
               if ((v>FValue1) OR (FIncludeV1 AND (v>=FValue1))) AND ((v<FValue2) OR (FIncludeV2 AND (v<=FValue2))) then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            ztBit:
            begin
               bit := FloatToInteger(Value1);
               value := FloatToInteger(v);
               bit := Power(2,bit);
               if ((value and bit)=bit)=FIncludeV1 then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            end;
            ztNotEqual:
               if v<>Value1 then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            ztOutOfRange:
               if ((v<FValue1) OR (FIncludeV1 AND (v<=FValue1))) OR ((v>FValue2) OR (FIncludeV2 AND (v>=FValue2))) then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            ztGreaterThan:
               if ((v>FValue1) OR (FIncludeV1 AND (v>=FValue1))) then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
            ztLessThan:
               if ((v<FValue1) OR (FIncludeV1 AND (v<=FValue1))) then begin
                  Result := Self.items[c] as TZone;
                  Break;
               end;
         end; //end do case
   end; //end do for
end;

procedure TZones.NeedCurrentCompState;
begin
   if assigned(FOnNeedCompState) then
      FOnNeedCompState(FComponentState);
end;

//############################################################
// TTextZone implementation
//############################################################

constructor TTextZone.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FFont := TFont.Create;
   FFont.OnChange := FontChanges;
   FVerAlignment:=tlTop;
   FHorAlignment:=taLeftJustify;
end;

procedure TTextZone.SetText(t:TCaption);
begin
   FText:=t;
   NotifyChange;
end;

procedure TTextZone.SetHorAlignment(x:TAlignment);
begin
   FHorAlignment:=x;
   NotifyChange;
end;

procedure TTextZone.SetVerAlignment(x:TTextLayout);
begin
   FVerAlignment:=x;
   NotifyChange;
end;

procedure TTextZone.SetColor(c:TColor);
begin
   FColor:=c;
   NotifyChange;
end;

procedure TTextZone.SetTransparent(b:Boolean);
begin
   FTransparent:=b;
   NotifyChange;
end;

procedure TTextZone.SetFont(f:TFont);
begin
   FFont.Assign(f);
end;


procedure TTextZone.FontChanges(Sender:TObject);
begin
   NotifyChange;
end;

//############################################################
// TTextZones implementation
//############################################################
constructor TTextZones.Create;
begin
   inherited Create(TTextZone);
end;

function TTextZones.Add:TTextZone;
begin
   Result := TTextZone(inherited Add);
end;

//############################################################
// TGraphicZone implementation
//############################################################

constructor TGraphicZone.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FILIsDefault := true;
   FTransparent := true;
   FImageList := nil;
   FImageIndex := -1;
   FColor := clWhite;
end;

procedure TGraphicZone.SetILAsDefault(b:Boolean);
var
   notify:Boolean;
begin
   notify := (FILIsDefault<>b);
   FILIsDefault:=b;
   if notify then
      NotifyChange;
end;

procedure TGraphicZone.SetFileName(fn:String);
var
   notify:Boolean;
begin
   if not FileExists(fn) then
      raise exception.Create('Arquivo inexistente!');
      
   notify := (fn<>FFileName);
   FFileName := fn;
   if notify then
      NotifyChange;
end;

procedure TGraphicZone.SetImageList(il:TImageList);
begin
   if [csReading]*TZones(Collection).ZonesState<>[] then begin
      FImageList:=il;
      exit;
   end;
   
   if il=FImageList then exit;

   FImageList := il;

   if il=nil then
      SetImageIndex(-1)
   else
      if FImageIndex>=il.Count then
        SetImageIndex(-1);

   NotifyChange;
end;

procedure TGraphicZone.SetImageIndex(index:Integer);
var
   notify:Boolean;
begin
   if [csReading, csLoading]*TZones(Collection).ZonesState<>[] then begin
      FImageIndex:=index;
      exit;
   end;

   if FImageList<>nil then begin
      if (index>=0) and (index<=(FImageList.Count-1)) then begin
         notify := (FImageIndex<>index);
         FImageIndex:=index;
      end else begin
         notify:= FImageIndex<>-1;
         FImageIndex:=-1;
      end;
   end else begin
      notify:= FImageIndex<>-1;
      FImageIndex:=-1;
   end;

   if notify then
      NotifyChange;
end;

procedure TGraphicZone.SetColor(c:TColor);
var
   notify:Boolean;
begin
   notify := (FColor<>c);
   FColor:=c;
   if notify then
      NotifyChange;
end;

procedure TGraphicZone.SetTransparent(b:Boolean);
var
   notify:Boolean;
begin
   notify := (FTransparent<>b);
   FTransparent := b;
   if notify then
      NotifyChange;
end;

//############################################################
// TGraphicZones implementation
//############################################################
constructor TGraphicZones.Create;
begin
   inherited Create(TGraphicZone);
end;

function TGraphicZones.Add:TGraphicZone;
begin
   Result := TGraphicZone(inherited Add);
end;

end.
