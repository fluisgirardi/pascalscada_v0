unit numexprtag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpexprpars, PLCNumber, ProtocolTypes, Tag;

type

  { TNumericExprTag }

  TNumericExprTag = class(TPLCNumberMappable, ITagInterface, ITagNumeric)
  private
    FExpr: String;
    FLastASyncWriteStatus: TProtocolIOResult;
    FLastEvalutionError: String;
    FLastSyncReadStatus: TProtocolIOResult;
    FLastSyncWriteStatus: TProtocolIOResult;
    FvarA: TPLCNumber;
    FvarB: TPLCNumber;
    FvarC: TPLCNumber;
    FvarD: TPLCNumber;
    FvarE: TPLCNumber;
    FvarF: TPLCNumber;
    FvarG: TPLCNumber;
    FvarH: TPLCNumber;
    FvarI: TPLCNumber;
    FvarJ: TPLCNumber;
    procedure CalculateValue;
    function GetValueTimestamp: TDatetime;
    function GetVariantValue: Variant;
    function IsValidValue(aValue: Variant): Boolean;
    procedure SetExpr(AValue: String);
    procedure SetVarA(AValue: TPLCNumber);
    procedure SetVarB(AValue: TPLCNumber);
    procedure SetVarC(AValue: TPLCNumber);
    procedure SetVarD(AValue: TPLCNumber);
    procedure SetVarE(AValue: TPLCNumber);
    procedure SetVarF(AValue: TPLCNumber);
    procedure SetVarG(AValue: TPLCNumber);
    procedure SetVarH(AValue: TPLCNumber);
    procedure SetVarI(AValue: TPLCNumber);
    procedure SetVariantValue(V: Variant);
    procedure SetVarJ(AValue: TPLCNumber);
    procedure TagRemovedCallback(Sender: TObject);
    procedure VarTagChanged(Sender: TObject);
  protected
    procedure SetValueRaw(aValue: Double); override;
    function GetValueRaw: Double; override;
  public
    procedure Loaded; override;
    destructor Destroy; override;
  published
    property A:TPLCNumber read FvarA write SetVarA;
    property B:TPLCNumber read FvarB write SetVarB;
    property C:TPLCNumber read FvarC write SetVarC;
    property D:TPLCNumber read FvarD write SetVarD;
    property E:TPLCNumber read FvarE write SetVarE;
    property F:TPLCNumber read FvarF write SetVarF;
    property G:TPLCNumber read FvarG write SetVarG;
    property H:TPLCNumber read FvarH write SetVarH;
    property I:TPLCNumber read FvarI write SetVarI;
    property J:TPLCNumber read FvarJ write SetVarJ;
    property Expression:String Read FExpr write SetExpr;
    property ScaleProcessor;
    property LastEvalutionError:String read FLastEvalutionError;
    property OnValueChangeFirst;
    property OnValueChangeLast;
    property OnReadFail;
    property OnWriteFail;
  end;

implementation

uses Variants, hsstrings;

{ TNumericExprTag }

function  TNumericExprTag.GetVariantValue:Variant;
begin
   Result := Value;
end;

procedure TNumericExprTag.SetVariantValue(V:Variant);
var
   aux:double;
begin
   if VarIsNumeric(v) then begin
      Value := V
   end else
      if VarIsStr(V) then begin
         if TryStrToFloat(V,aux) then
            Value := aux
         else
            raise exception.Create(SinvalidValue);
      end else
         if VarIsType(V,varboolean) then begin
            if V=true then
               Value := 1
            else
               Value := 0;
         end else
            raise exception.Create(SinvalidValue);
end;

function  TNumericExprTag.IsValidValue(aValue:Variant):Boolean;
var
   aux:Double;
   aValueStr: AnsiString;
begin
   aValueStr:=aValue;
   Result := VarIsNumeric(aValue) or
             (VarIsStr(aValue) and TryStrToFloat(aValueStr,aux)) or
             VarIsType(aValue, varboolean);
end;

function TNumericExprTag.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TNumericExprTag.CalculateValue;
var
  FParser: TFPExpressionParser;
  exprValue: TExprFloat;
begin
  if [csLoading,csReading]*ComponentState=[] then begin
    FParser := TFpExpressionParser.Create(self);
    try
      FParser.BuiltIns := [bcMath, bcBoolean];
      if Assigned(FvarA) then
        FParser.Identifiers.AddFloatVariable('A', (FvarA as ITagNumeric).GetValue);

      if Assigned(FvarB) then
        FParser.Identifiers.AddFloatVariable('B', (FvarB as ITagNumeric).GetValue);

      if Assigned(FvarC) then
        FParser.Identifiers.AddFloatVariable('C', (FvarC as ITagNumeric).GetValue);

      if Assigned(FvarD) then
        FParser.Identifiers.AddFloatVariable('D', (FvarD as ITagNumeric).GetValue);

      if Assigned(FvarE) then
        FParser.Identifiers.AddFloatVariable('E', (FvarE as ITagNumeric).GetValue);

      if Assigned(FvarF) then
        FParser.Identifiers.AddFloatVariable('F', (FvarF as ITagNumeric).GetValue);

      if Assigned(FvarG) then
        FParser.Identifiers.AddFloatVariable('G', (FvarG as ITagNumeric).GetValue);

      if Assigned(FvarH) then
        FParser.Identifiers.AddFloatVariable('H', (FvarH as ITagNumeric).GetValue);

      if Assigned(FvarI) then
        FParser.Identifiers.AddFloatVariable('I', (FvarI as ITagNumeric).GetValue);

      if Assigned(FvarJ) then
        FParser.Identifiers.AddFloatVariable('J', (FvarJ as ITagNumeric).GetValue);



      FLastEvalutionError:='OK';
      try
        FParser.Expression:=FExpr;
        exprValue:=FParser.Evaluate.ResFloat;
        if exprValue<>PValueRaw then begin
          PValueRaw:=exprValue;
          PValueTimeStamp:=Now;
          NotifyChange;
        end;
      except
        on e:Exception do begin
          FLastEvalutionError:=e.Message;
          NotifyReadFault;
        end;
      end;

    finally
      FParser.Free;
    end;
  end;
end;

procedure TNumericExprTag.SetExpr(AValue: String);
begin
  if FExpr=AValue then Exit;
  FExpr:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarA(AValue: TPLCNumber);
begin
  if FvarA=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarA) THEN begin
    FvarA.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarA:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarB(AValue: TPLCNumber);
begin
  if FvarB=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarB) THEN begin
    FvarB.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarB:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarC(AValue: TPLCNumber);
begin
  if FvarC=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarC) THEN begin
    FvarC.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarC:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarD(AValue: TPLCNumber);
begin
  if FvarD=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarD) THEN begin
    FvarD.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarD:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarE(AValue: TPLCNumber);
begin
  if FvarE=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarE) THEN begin
    FvarE.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarE:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarF(AValue: TPLCNumber);
begin
  if FvarF=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarF) THEN begin
    FvarF.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarF:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarG(AValue: TPLCNumber);
begin
  if FvarG=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarG) THEN begin
    FvarG.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarG:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarH(AValue: TPLCNumber);
begin
  if FvarH=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarH) THEN begin
    FvarH.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarH:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarI(AValue: TPLCNumber);
begin
  if FvarI=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarI) THEN begin
    FvarI.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarI:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.SetVarJ(AValue: TPLCNumber);
begin
  if FvarJ=AValue then Exit;
  if Assigned(AValue) and (not Supports(AValue,ITagNumeric)) then exit;

  if Assigned(FvarJ) THEN begin
    FvarJ.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then begin
    AValue.AddTagChangeHandler(@VarTagChanged);
    AValue.AddWriteFaultHandler(@VarTagChanged);
    AValue.AddRemoveTagHandler(@TagRemovedCallback);
  end;

  FvarJ:=AValue;

  CalculateValue;
end;

procedure TNumericExprTag.TagRemovedCallback(Sender: TObject);
begin
  IF SENDER=FvarA THEN FvarA:=NIL;
  IF SENDER=FvarB THEN FvarB:=NIL;
  IF SENDER=FvarC THEN FvarC:=NIL;
  IF SENDER=FvarD THEN FvarD:=NIL;
  IF SENDER=FvarE THEN FvarE:=NIL;
  IF SENDER=FvarF THEN FvarF:=NIL;
  IF SENDER=FvarG THEN FvarG:=NIL;
  IF SENDER=FvarH THEN FvarH:=NIL;
  IF SENDER=FvarI THEN FvarI:=NIL;
  IF SENDER=FvarJ THEN FvarJ:=NIL;
  CalculateValue;
end;

procedure TNumericExprTag.VarTagChanged(Sender: TObject);
begin
  CalculateValue;
end;

procedure TNumericExprTag.SetValueRaw(aValue: Double);
begin
  NotifyWriteFault;
end;

function TNumericExprTag.GetValueRaw: Double;
begin
  Result:=PValueRaw;
end;

procedure TNumericExprTag.Loaded;
begin
  inherited Loaded;
  CalculateValue;
end;

destructor TNumericExprTag.Destroy;
begin
  if Assigned(FvarA) THEN  FvarA.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarB) THEN  FvarB.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarC) THEN  FvarC.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarD) THEN  FvarD.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarE) THEN  FvarE.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarF) THEN  FvarF.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarG) THEN  FvarG.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarH) THEN  FvarH.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarI) THEN  FvarI.RemoveAllHandlersFromObject(Self);
  if Assigned(FvarJ) THEN  FvarJ.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

end.

