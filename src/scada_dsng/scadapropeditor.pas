{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação dos editores de algumas propriedades de componentes
            do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements some property editors of PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Replaced ProtocolDriver with TagAssistant. (unit, properties and classes)
  07/2013 - Implemented Double-Click for the assistants.
  07/2013 - Replaced PlcNumber with BitMapTagAssistant. (unit, properties and classes)
  07/2013 - Replaced PlcBlock with BLockTagAssistant. (unit, properties and classes)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit scadapropeditor;

{$I ../common/delphiver.inc}

interface

uses
  Classes, SysUtils, SerialPort, PLCBlockElement, PLCStruct, Tag,
  bitmappertagassistant, blockstructtagassistant, ProtocolDriver,
  PLCNumber, plcstructstring, comptagedt, fpexprpars,

  {$IF defined(WIN32) or defined(WIN64) OR defined(WINCE)}
  Windows,
  {$ELSE}
  Unix,
  {$IFEND}
  
  {$IFDEF FPC}
    PropEdits, ComponentEditors, typinfo;
  {$ELSE}
    Types,
    //Delphi 6 ou superior
    {$IF defined(DELPHI6_UP)}
      DesignIntf, DesignEditors;
    {$ELSE}
      //demais versoes do delphi
      DsgnIntf;
    {$IFEND}
  {$ENDIF}

type
  {$IFDEF PORTUGUES}
  //: Editor da propriedade TSerialPortDriver.COMPort
  {$ELSE}
  //: Property editor of TSerialPortDriver.COMPort property.
  {$ENDIF}
  TPortPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: AnsiString); override;
    // somente para windows
    {$IFDEF MSWINDOWS}
    function  GetPortas: string;
    {$ENDIF}
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade TPLCBlockElement.Index
  {$ELSE}
  //: Property editor of TPLCBlockElement.Index property.
  {$ENDIF}

  { TIntegerExpressionPropertyEditor }

  TIntegerExpressionPropertyEditor = class(TIntegerProperty)
  private
    procedure SetValue(const index: Integer; const NewValue: Int64);
  protected
    procedure RegisterExpressionVariables(const i:Integer; var parser: TFPExpressionParser); virtual;
  public
    function  GetPropType(Index:Integer): PTypeInfo;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TElementIndexPropertyEditor }

  TElementIndexPropertyEditor = class(TIntegerExpressionPropertyEditor)
  protected
    procedure RegisterExpressionVariables(const i: Integer;
  var parser: TFPExpressionParser); override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  { TTagAddressPropertyEditor }

  TTagAddressPropertyEditor = class(TIntegerExpressionPropertyEditor)
  protected
    procedure RegisterExpressionVariables(const i: Integer;
               var parser: TFPExpressionParser); override;
  end;

  { TWinControlBoundsEditor }

  TWinControlBoundsEditor = class(TIntegerExpressionPropertyEditor)
    procedure RegisterExpressionVariables(const i: Integer;
               var parser: TFPExpressionParser); override;
  end;

  {$IFNDEF FPC}
  //: @exclude
  TDefaultComponentEditor = class(TComponentEditor);
  {$ENDIF}

  {$IFDEF PORTUGUES}
  {:
    Editor de componente base para todos os demais editores que irão inserir
    componentes na aplicação.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    Base class of Component editor for all component editors that will insert
    others componentes in application.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TInsertTagsOnFormComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddTagInEditor(Tag:TTag);
    function  CreateComponent(tagclass:TComponentClass):TComponent;
    function  GetTheOwner:TComponent; virtual;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente TagBuilder.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  TagBuilder component editor tool.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TProtocolDriverComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenTagBuilder;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function  GetVerb(Index: LongInt): AnsiString; override;
    function  GetVerbCount: LongInt; override;
    procedure Edit; override;
    function  ProtocolDriver: TProtocolDriver; virtual;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente BitMapper. Mapeia bits de um tag.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  BitMapper component editor tool. Map bits of a tag.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TTagBitMapperComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenBitMapper;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function GetVerb(Index: LongInt): AnsiString; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de componente BlockElementMapper. Mapeia elementos de um tag bloco.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ELSE}
  {:
  BlockElementMapper component editor tool. Map elements of a tag block.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TInsertTagsOnFormComponentEditor)
  }
  {$ENDIF}
  TBlockElementMapperComponentEditor = class(TInsertTagsOnFormComponentEditor)
  private
    procedure OpenElementMapper;
  protected
    function GetTheOwner: TComponent; override;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    {$if declared(has_customhints)}
    function GetCustomHint: AnsiString; override;
    {$ifend}
    function GetVerb(Index: LongInt): AnsiString; override;
    function GetVerbCount: LongInt; override;
    procedure Edit; override;
  end;

  procedure ChangeComponentTag(Sender: TObject);

implementation

uses PLCBlock, PLCTagNumber, PLCString, RtlConsts, FormEditingIntf,
  Controls {$IFDEF WINDOWS} , Registry{$ENDIF};

procedure ChangeComponentTag(Sender: TObject);
var
  aSelected: TPersistentSelectionList;
  frm: TfrmTComponentTagEditor;
begin
  aSelected:=TPersistentSelectionList.Create;
  try
    if Assigned(GlobalDesignHook) then begin
      GlobalDesignHook.GetSelection(aSelected);
      if (aSelected.Count=1) and (aSelected.Items[0] is TComponent) then begin
        frm:=TfrmTComponentTagEditor.Create(nil);
        try
          frm.Label1.Caption:=Format('Set new value %s.Tag', [TComponent(aSelected.Items[0]).Name]);
          frm.Edit1.Text:=IntToStr(TComponent(aSelected.Items[0]).Tag);
          frm.Edit1.SelectAll;
          if frm.ShowModal=mrOK then begin
            TComponent(aSelected.Items[0]).Tag:=frm.Value;
            GlobalDesignHook.Modified(aSelected.Items[0]);
          end;
        finally
          FreeAndNil(frm);
        end;
      end;
    end;
  finally
    FreeAndNil(aSelected);
  end;
end;

function  TPortPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if (GetComponent(0) is TSerialPortDriver) and (GetComponent(0) as TSerialPortDriver).AcceptAnyPortName=false then
     Result := [paValueList{$IFDEF FPC}, paPickList{$ELSE}
                {$IFDEF DELPHI2005_UP}, paReadOnly,
                paValueEditable{$ENDIF}{$ENDIF}]
   else
     Result:=inherited GetAttributes;
end;

function  TPortPropertyEditor.GetValue: AnsiString;
begin
   Result := GetStrValue;
end;

procedure TPortPropertyEditor.GetValues(Proc: TGetStrProc);
{$IF defined(WIN32) or defined(WIN64)}
var
  c:LongInt;
  dcbstring, comname:AnsiString;
  d:DCB;
  str: Tstringlist;
begin
{ com essa abordagem é possive listar as portas COM acima de 10 e tambem
  portas Virtuais que tem nomes diferentes " qualquer nome ele lista"}

  // cria a primeira porta
  Proc('(none)');
  // cria uma lista de portas "ativas" do sistema
  str:= Tstringlist.Create;
  // copia as portas da função Get portas
  str.CommaText:= GetPortas ;
  // faz uma contagem de portas ativas
  for c:=0 to (str.Count - 1) do begin
     // transfere os valores para a listagem COMport
     Proc(str.ValueFromIndex[c]);
  end;
  // libera a lista
  str.Free;
  {
  Proc('(none)');
  for c:=1 to 255 do begin
     comname := 'COM'+IntToStr(c);
     dcbstring := comname+': baud=1200 parity=N data=8 stop=1';
     if BuildCommDCB(PChar(dcbstring),d) then
        Proc(comname);
  end;
}
{$IFEND}
{$IFDEF UNIX}
var
   c, d:LongInt;
   pname:AnsiString;

   function PortDirPrefix:AnsiString;
   begin
     if Assigned(GetComponent(0)) and (GetComponent(0) is TSerialPortDriver) then
       Result:=(GetComponent(0) as TSerialPortDriver).DevDir
     else
       Result:='/dev/';
   end;

begin
   Proc('(none)');
   for d:=0 to High(PortPrefix) do
      {$IFDEF SunOS}
      for c:=Ord('a') to ord('z') do begin
         pname:=PortPrefix[d]+Char(c);
      {$ELSE}
      for c:=0 to 255 do begin
         pname:=PortPrefix[d]+IntToStr(c);
      {$ENDIF}
         if FileExists(PortDirPrefix+pname) then // Added DevDir property.
            Proc(pname);
      end;
{$ENDIF}
{$IFDEF WINCE}
begin
  //ToDo
{$ENDIF}
end;

procedure TPortPropertyEditor.SetValue(const Value: AnsiString);
begin
   SetStrValue(Value);
   if GetComponent(0) is TSerialPortDriver then
      TSerialPortDriver(GetComponent(0)).Active := false;
end;

////////////////////////////////////////////////////////////////////////////////
//TIntegerExpressionPropertyEditor
////////////////////////////////////////////////////////////////////////////////
procedure TIntegerExpressionPropertyEditor.RegisterExpressionVariables(
  const i: Integer; var parser: TFPExpressionParser);
begin
  //virtual method.
end;

function TIntegerExpressionPropertyEditor.GetPropType(Index: Integer): PTypeInfo;
begin
  Result:=GetInstProp[Index].PropInfo^.PropType;
end;

procedure TIntegerExpressionPropertyEditor.SetValue(const NewValue: ansistring);
var
  aux: Longint;
  parser: TFPExpressionParser;
  rt: TFPExpressionResult;
  i: Integer;
begin
  if (not (NewValue[1] in ['+','-','*','/'])) and TryStrToInt(NewValue,aux) then
    inherited SetValue(NewValue)
  else begin
    parser:=TFPExpressionParser.Create(nil);
    try
      parser.BuiltIns:=[bcMath];
      for i:=0 to PropCount-1 do begin
        RegisterExpressionVariables(i, parser);

        if (NewValue[1]='+') or (NewValue[1]='-') or (NewValue[1]='*') or (NewValue[1]='/')  then begin
          parser.Expression:=OrdValueToVisualValue(GetOrdValueAt(i))+NewValue
        end else
          parser.Expression:=NewValue;
        rt:=parser.Evaluate;
        case rt.ResultType of
          rtInteger: SetValue(i, parser.AsInteger);
          rtFloat:   SetValue(i, Trunc(parser.AsFloat));
        end;
      end;
    finally
      FreeAndNil(rt);
    end;
  end;
end;

procedure TIntegerExpressionPropertyEditor.SetValue(const index:Integer; const NewValue: Int64);

  procedure Error(const Args: array of const);
  begin
    raise EPropertyError.CreateResFmt(@SOutOfRange, Args);
  end;

var
  L: Int64;
begin
  L:=NewValue;
  with GetTypeData(GetPropType(index))^ do
    if OrdType = otULong then begin   // unsigned compare and reporting needed
      if (L < Cardinal(MinValue)) or (L > Cardinal(MaxValue)) then begin
        // bump up to Int64 to get past the %d in the format string
        Error([Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
        exit;
      end
    end else
      if (L < MinValue) or (L > MaxValue) then begin
        Error([MinValue, MaxValue]);
        exit;
      end;
  with GetInstProp[index] do SetOrdProp(Instance, PropInfo, NewValue);
  Modified;
end;

////////////////////////////////////////////////////////////////////////////////
//TElementIndexPropertyEditor
////////////////////////////////////////////////////////////////////////////////
function  TElementIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if (GetComponent(0) is TPLCBlockElement) or (GetComponent(0) is TPLCStructString)  then
      Result := [paValueList, paMultiSelect];
end;

procedure TElementIndexPropertyEditor.RegisterExpressionVariables(
  const i: Integer; var parser: TFPExpressionParser);
begin
  if assigned(parser) then begin
    //unregister all possible registered variables.
    parser.Identifiers.Clear;

    if (GetComponent(i) is TPLCBlockElement) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (lowercase(GetPropInfo^.Name)<>'tag') then
        parser.Identifiers.AddIntegerVariable('Tag', (GetComponent(i) as TPLCBlockElement).Tag);
    end;

    if (GetComponent(i) is TPLCStructString) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (lowercase(GetPropInfo^.Name)<>'tag') then
        parser.Identifiers.AddIntegerVariable('Tag', (GetComponent(i) as TPLCStructString).Tag);
    end;
  end;
end;

procedure TElementIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:LongInt;
begin
  if (GetComponent(0) is TPLCBlockElement) and (TPLCBlockElement(GetComponent(0)).PLCBlock <> nil) then
    for i := 0 to LongInt(TPLCBlockElement(GetComponent(0)).PLCBlock.Size)-1 do begin
      Proc(IntToStr(i));
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//TTagAddressPropertyEditor
////////////////////////////////////////////////////////////////////////////////
procedure TTagAddressPropertyEditor.RegisterExpressionVariables(
  const i: Integer; var parser: TFPExpressionParser);
var
  propertyName: String;
begin
  if assigned(parser) then begin
    //unregister all possible registered variables.
    parser.Identifiers.Clear;

    propertyName:=lowercase(GetPropInfo^.Name);

    if (GetComponent(i) is TPLCTagNumber) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (propertyName<>'plcrack') then
        parser.Identifiers.AddIntegerVariable('plcrack', (GetComponent(i) as TPLCTagNumber).plcrack);

      if (propertyName<>'plcslot') then
        parser.Identifiers.AddIntegerVariable('plcslot', (GetComponent(i) as TPLCTagNumber).plcslot);

      if (propertyName<>'plcstation') then
        parser.Identifiers.AddIntegerVariable('plcstation', (GetComponent(i) as TPLCTagNumber).plcstation);

      if (propertyName<>'memfile_db') then
        parser.Identifiers.AddIntegerVariable('memfile_db', (GetComponent(i) as TPLCTagNumber).memfile_db);

      if (propertyName<>'memaddress') then
        parser.Identifiers.AddIntegerVariable('memaddress', (GetComponent(i) as TPLCTagNumber).memaddress);

      if (propertyName<>'memsubelement') then
        parser.Identifiers.AddIntegerVariable('memsubelement', (GetComponent(i) as TPLCTagNumber).memsubelement);

      if (propertyName<>'memreadfunction') then
        parser.Identifiers.AddIntegerVariable('memreadfunction', (GetComponent(i) as TPLCTagNumber).memreadfunction);

      if (propertyName<>'memwritefunction') then
        parser.Identifiers.AddIntegerVariable('memwritefunction', (GetComponent(i) as TPLCTagNumber).memwritefunction);

      if (propertyName<>'tag') then
        parser.Identifiers.AddIntegerVariable('Tag', (GetComponent(i) as TPLCTagNumber).Tag);
    end;

    if (GetComponent(i) is TPLCBlock) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (propertyName<>'plcrack') then
        parser.Identifiers.AddIntegerVariable('plcrack', (GetComponent(i) as TPLCBlock).plcrack);

      if (propertyName<>'plcslot') then
        parser.Identifiers.AddIntegerVariable('plcslot', (GetComponent(i) as TPLCBlock).plcslot);

      if (propertyName<>'plcstation') then
        parser.Identifiers.AddIntegerVariable('plcstation', (GetComponent(i) as TPLCBlock).plcstation);

      if (propertyName<>'memfile_db') then
        parser.Identifiers.AddIntegerVariable('memfile_db', (GetComponent(i) as TPLCBlock).memfile_db);

      if (propertyName<>'memaddress') then
        parser.Identifiers.AddIntegerVariable('memaddress', (GetComponent(i) as TPLCBlock).memaddress);

      if (propertyName<>'memsubelement') then
        parser.Identifiers.AddIntegerVariable('memsubelement', (GetComponent(i) as TPLCBlock).memsubelement);

      if (propertyName<>'memreadfunction') then
        parser.Identifiers.AddIntegerVariable('memreadfunction', (GetComponent(i) as TPLCBlock).memreadfunction);

      if (propertyName<>'memwritefunction') then
        parser.Identifiers.AddIntegerVariable('memwritefunction', (GetComponent(i) as TPLCBlock).memwritefunction);

      if (propertyName<>'tag') then
        parser.Identifiers.AddIntegerVariable('Tag', (GetComponent(i) as TPLCBlock).Tag);
    end;

    if (GetComponent(i) is TPLCString) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (propertyName<>'plcrack') then
        parser.Identifiers.AddIntegerVariable('plcrack', (GetComponent(i) as TPLCString).plcrack);

      if (propertyName<>'plcslot') then
        parser.Identifiers.AddIntegerVariable('plcslot', (GetComponent(i) as TPLCString).plcslot);

      if (propertyName<>'plcstation') then
        parser.Identifiers.AddIntegerVariable('plcstation', (GetComponent(i) as TPLCString).plcstation);

      if (propertyName<>'memfile_db') then
        parser.Identifiers.AddIntegerVariable('memfile_db', (GetComponent(i) as TPLCString).memfile_db);

      if (propertyName<>'memaddress') then
        parser.Identifiers.AddIntegerVariable('memaddress', (GetComponent(i) as TPLCString).memaddress);

      if (propertyName<>'memsubelement') then
        parser.Identifiers.AddIntegerVariable('memsubelement', (GetComponent(i) as TPLCString).memsubelement);

      if (propertyName<>'memreadfunction') then
        parser.Identifiers.AddIntegerVariable('memreadfunction', (GetComponent(i) as TPLCString).memreadfunction);

      if (propertyName<>'memwritefunction') then
        parser.Identifiers.AddIntegerVariable('memwritefunction', (GetComponent(i) as TPLCString).memwritefunction);

      if (propertyName<>'tag') then
        parser.Identifiers.AddIntegerVariable('Tag', (GetComponent(i) as TPLCString).Tag);
    end;
  end;
end;

{ TWinControlBoundsEditor }

procedure TWinControlBoundsEditor.RegisterExpressionVariables(const i: Integer;
  var parser: TFPExpressionParser);
var
  propertyName: String;
begin
  if assigned(parser) then begin
    //unregister all possible registered variables.
    parser.Identifiers.Clear;

    propertyName:=lowercase(GetPropInfo^.Name);

    if (GetComponent(i) is TWinControl) then begin
      //register only if the property is not being edited,
      //to avoid circular references.
      if (propertyName<>'width') then
        parser.Identifiers.AddIntegerVariable('width', (GetComponent(i) as TWinControl).Width);

      if (propertyName<>'height') then
        parser.Identifiers.AddIntegerVariable('height', (GetComponent(i) as TWinControl).Height);

      if (propertyName<>'left') then
        parser.Identifiers.AddIntegerVariable('left', (GetComponent(i) as TWinControl).Left);

      if (propertyName<>'top') then
        parser.Identifiers.AddIntegerVariable('top', (GetComponent(i) as TWinControl).Top);

      if (propertyName<>'tag') then
              parser.Identifiers.AddIntegerVariable('tag', (GetComponent(i) as TWinControl).Tag);
    end;
  end;
end;

///////////////////////////////////////
//editor base para os demais editores.
///////////////////////////////////////
procedure TInsertTagsOnFormComponentEditor.AddTagInEditor(Tag:TTag);
{$IFDEF FPC}
var
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
{$IFDEF FPC}
  Hook:=nil;
  if not GetHook(Hook) then exit;
  Hook.PersistentAdded(Tag,false);
  Modified;
{$ELSE}
  Designer.Modified;
{$ENDIF}
end;

function  TInsertTagsOnFormComponentEditor.CreateComponent(tagclass:TComponentClass):TComponent;
begin
  {$IFDEF FPC}
    Result := tagclass.Create(GetTheOwner);
  {$ELSE}
    Result := Designer.CreateComponent(tagclass,GetTheOwner,0,0,0,0);
  {$ENDIF}
end;

function TInsertTagsOnFormComponentEditor.GetTheOwner:TComponent;
begin
  Result:=nil;
end;

///////////////////////////////////////
//editor TAG BUILDER
///////////////////////////////////////

function  TProtocolDriverComponentEditor.GetTheOwner: TComponent;
begin
  Result:=ProtocolDriver.Owner;
end;

procedure TProtocolDriverComponentEditor.OpenTagBuilder;
begin
  ProtocolDriver.OpenTagEditor(@AddTagInEditor, @CreateComponent);
end;

procedure TProtocolDriverComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenTagBuilder();
end;

function TProtocolDriverComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  if Index=0 then
    Result:='Tag Builder';
end;

function TProtocolDriverComponentEditor.GetVerbCount: LongInt;
begin
  if ProtocolDriver.HasTabBuilderEditor then
    Result:=1
  else
    Result:=0;
end;

procedure TProtocolDriverComponentEditor.Edit;
begin
  inherited Edit;
  OpenTagBuilder();
end;

function TProtocolDriverComponentEditor.ProtocolDriver: TProtocolDriver;
begin
  Result:=TProtocolDriver(GetComponent);
end;

///////////////////////////////////////////////////////////////////////////////
// BIT MAPPER
///////////////////////////////////////////////////////////////////////////////

function TTagBitMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=GetComponent().Owner;
end;

procedure TTagBitMapperComponentEditor.OpenBitMapper;
begin
  if (GetComponent is TPLCNumberMappable) then
    TPLCNumberMappable(GetComponent).OpenBitMapper(@AddTagInEditor, @CreateComponent);
end;

procedure TTagBitMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenBitMapper();
end;

function  TTagBitMapperComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  if Index=0 then
    Result:='Map bits';
end;

function  TTagBitMapperComponentEditor.GetVerbCount: LongInt;
begin
  Result:=1;
end;

procedure TTagBitMapperComponentEditor.Edit;
begin
  inherited Edit;
  OpenBitMapper();
end;

///////////////////////////////////////////////////////////////////////////////
// ELEMENT BLOCK MAPPER
///////////////////////////////////////////////////////////////////////////////

procedure TBlockElementMapperComponentEditor.OpenElementMapper;
begin
  if (GetComponent is TPLCBlock) then
    TPLCBlock(GetComponent).MapElements(@AddTagInEditor, @CreateComponent);
end;

function TBlockElementMapperComponentEditor.GetTheOwner: TComponent;
begin
  Result:=GetComponent().Owner;
end;

procedure TBlockElementMapperComponentEditor.ExecuteVerb(Index: LongInt);
begin
  if Index=0 then
    OpenElementMapper();
end;

{$if declared(has_customhints)}
function TBlockElementMapperComponentEditor.GetCustomHint: AnsiString;
begin
  if GetComponent is TPLCStruct then begin
    Result:=Result+'Structure size in bytes:'+IntToStr(TPLCStruct(GetComponent).Size);
    exit;
  end;

  if GetComponent is TPLCBlock then begin
    Result:=Result+'Number of elements: '+IntToStr(TPLCBlock(GetComponent).Size);
    exit;
  end;
end;
{$ifend}

function  TBlockElementMapperComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  Result:='Unknow option...';
  if Index=0 then begin
    if GetComponent is TPLCStruct then begin
      Result:='Map structure items...';
      exit;
    end;
    if GetComponent is TPLCBlock then begin
      Result:='Map block elements...';
      exit;
    end;
  end;
end;

function  TBlockElementMapperComponentEditor.GetVerbCount: LongInt;
begin
  Result:=0;
  if GetComponent is TPLCBlock then
   Result:=1;
end;

procedure TBlockElementMapperComponentEditor.Edit;
begin
  inherited Edit;
  OpenElementMapper();
end;

// somente para windows , pega o valor diretamente no registro do windows
// a vantagem é que é possivel listar portas acimma de COM9 e tambem portas Virtuais
// que tenha nomes diferentes (qualquer nome)
{$IFDEF MSWINDOWS}
function TPortPropertyEditor.GetPortas: string;
var
  Registro: TRegistry;
  lista: TStringList;
  valor: TStringList;
  numero: integer;
begin
  // cria lista de portas
  lista := TStringList.Create;
  // cria lista de valores
  valor := TStringList.Create;
  // cria os dados de registro a serem lidos
  Registro := TRegistry.Create;
  try
    // especifica o caminho do registro
    Registro.RootKey := HKEY_LOCAL_MACHINE;
    // abre o caminho do registro onde tem as portas seriais
    Registro.OpenKeyReadOnly('\HARDWARE\DEVICEMAP\SERIALCOMM');
    // recolhe valores do registro  ( dados no registro)
    Registro.GetValueNames(Lista);
    // conforme a quantidade de portas que estavam no registro
    for numero := 0 to lista.Count - 1 do
    // adicionar no valor os dados recolhido na string
    valor.Add(PChar(Registro.ReadString(lista[numero])));
    // o valor retornado separados em ponto e virgula
    Result := valor.CommaText;
  finally
     // finaliza a lista de registro
     Registro.Free;
     // finaliza o uso da lista
     lista.Free;
     // finaliza o uso da lista de valores
     valor.Free;
  end;
end;
{$ENDIF}
end.

