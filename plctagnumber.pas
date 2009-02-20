//: Implementa o tag PLC numérico com comunicação.
unit PLCTagNumber;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCNumber, ProtocolTypes, variants;

type
  //: Tag numérico com comunicação individual.
  TPLCTagNumber = class(TPLCNumber, ITagInterface, ITagNumeric)
  private
    function  GetValueAsText(Prefix, Sufix, Format:string):String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;
  protected
    //: @seealso(TPLCNumber.SetValueRaw)
    function  GetValueRaw:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(Value:Double); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
    //: @seealso(TTag.Size)
    property Size nodefault;
  public
    //: @seealso(TPLCTag.ScanRead)
    procedure ScanRead; override;
    //: @seealso(TPLCTag.ScanWrite)
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal); override;
    //: @seealso(TPLCTag.Read)
    procedure Read; override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); override;
  published
    //: @seealso(TTag.AutoRead)
    property AutoRead;
    //: @seealso(TTag.AutoWrite)
    property AutoWrite;
    //: @seealso(TTag.CommReadErrors)
    property CommReadErrors;
    //: @seealso(TTag.CommReadsOK)
    property CommReadsOK;
    //: @seealso(TTag.CommWriteErrors)
    property CommWriteErrors;
    //: @seealso(TTag.CommWritesOk)
    property CommWritesOk;
    //: @seealso(TTag.PLCHack)
    property PLCHack;
    //: @seealso(TTag.PLCSlot)
    property PLCSlot;
    //: @seealso(TTag.PLCStation)
    property PLCStation;
    //: @seealso(TTag.MemFile_DB)
    property MemFile_DB;
    //: @seealso(TTag.MemAddress)
    property MemAddress;
    //: @seealso(TTag.MemSubElement)
    property MemSubElement;
    //: @seealso(TTag.MemReadFunction)
    property MemReadFunction;
    //: @seealso(TTag.MemWriteFunction)
    property MemWriteFunction;
    //: @seealso(TTag.Retries)
    property Retries;
    //: @seealso(TPLCTag.ProtocolDriver)
    property ProtocolDriver;
    //: @seealso(TPLCNumber.ScaleProcessor)
    property ScaleProcessor;
    //: @seealso(TTag.RefreshTime)
    property RefreshTime;
    //: @seealso(TPLCTag.ValueTimestamp)
    property ValueTimestamp;
    //: @seealso(TTag.LongAddress)
    property LongAddress;
  end;

implementation

function TPLCTagNumber.GetValueRaw:Double;
begin
  Result := PValueRaw;
end;

function TPLCTagNumber.GetValueAsText(Prefix, Sufix, Format:string):String;
begin
   if Trim(Format)<>'' then
      Result := Prefix + FormatFloat(Format,Value)+Sufix
   else
      Result := Prefix + FloatToStr(Value)+Sufix;
end;

function  TPLCTagNumber.GetVariantValue:Variant;
begin
   Result := Value;
end;

procedure TPLCTagNumber.SetVariantValue(V:Variant);
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
            raise exception.Create('Valor inválido!');
      end else
         if VarIsType(V,varboolean) then begin
            if V=true then
               Value := 1
            else
               Value := 0;
         end else
            raise exception.Create('Valor inválido!');
end;

function  TPLCTagNumber.IsValidValue(Value:Variant):Boolean;
var
   aux:Double;
begin
   Result := VarIsNumeric(Value) or
             (VarIsStr(Value) and TryStrToFloat(Value,aux)) or
             VarIsType(Value, varboolean);
end;

function TPLCTagNumber.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TPLCTagNumber.SetValueRaw(Value:Double);
var
  towrite:TArrayOfDouble;
begin
  SetLength(towrite,1);
  towrite[0] := Value;
  ScanWrite(towrite,1,0);
  SetLength(towrite,0);
end;

procedure TPLCTagNumber.ScanRead;
var
  tr:TTagRec;
begin
  if csDesigning in ComponentState then exit;
  if (PProtocolDriver<>nil) and PAutoRead then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.ScanRead(tr);
  end;
end;

procedure TPLCTagNumber.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
begin
  if csDesigning in ComponentState then exit;
  if (PProtocolDriver<>nil) then begin
     if PAutoWrite then begin
       BuildTagRec(tr,0,0);
       PProtocolDriver.ScanWrite(tr,Values);
       //TagCommandCallBack(Values,Now,tcScanWrite,ioOk,0);
       //Dec(PCommWriteOk);
     end else begin
       TagCommandCallBack(Values,Now,tcScanWrite,ioOk,0);
       Dec(PCommWriteOk);
     end;
  end else
     TagCommandCallBack(Values, Now, tcScanWrite, ioNullDriver, Offset);
end;

procedure TPLCTagNumber.Read;
var
  tr:TTagRec;
begin
  if csDesigning in ComponentState then exit;
  if PProtocolDriver<>nil then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.Read(tr);
  end;
end;

procedure TPLCTagNumber.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
begin
  if csDesigning in ComponentState then exit;
  if (PProtocolDriver<>nil) then begin
     if PAutoWrite then begin
       BuildTagRec(tr,Count,Offset);
       PProtocolDriver.Write(tr,Values);
     end;
  end else
     TagCommandCallBack(Values, Now, tcWrite, ioNullDriver, Offset);
end;

procedure TPLCTagNumber.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
var
  notify:Boolean;
begin
  if (csDestroying in ComponentState) then exit;
  try
    notify := false;
    case TagCommand of
      tcScanRead,tcRead:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          //atualiza os dois valores (direto ou indireto) independende do caso do pedido.
          notify := (PValueRaw<>values[0]);
          PValueRaw := values[0];
          PValueTimeStamp := ValuesTimeStamp;
          if LastResult=ioOk then
             IncCommReadOK(1);
        end else
          IncCommReadFaults(1);
      end;
      tcScanWrite,tcWrite:
      begin
        if LastResult in [ioOk, ioNullDriver]then begin
          if LastResult=ioOk then
             IncCommWriteOK(1);
          notify := (PValueRaw<>values[0]);
          PValueRaw := values[0];
        end else
          IncCommWriteFaults(1);
      end;
    end;

    case TagCommand of
      tcScanRead:
        PLastASyncReadCmdResult := LastResult;
      tcScanWrite:
        PLastASyncWriteCmdResult := LastResult;
      tcRead:
        PLastSyncReadCmdResult := LastResult;
      tcWrite:
        PLastSyncWriteCmdResult := LastResult;
    end;

    if notify then
      NotifyChange;
  finally
  end;
end;

end.
 
