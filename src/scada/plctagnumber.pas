{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Implementa o tag PLC numérico com comunicação.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
  @abstract(Unit that implements a numeric tag with communication.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - TPLCTagNumber is descendant of TPLCNumberMappable
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************

}
{$ENDIF}
unit PLCTagNumber;

interface

uses
  SysUtils, Classes, Tag, PLCNumber, ProtocolTypes, variants;

type

  {$IFDEF PORTUGUES}
  {:
  @abstract(Tag numérico com comunicação individual.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ELSE}
  {:
  @abstract(Single numeric tag with communication.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ENDIF}
  TPLCTagNumber = class(TPLCNumberMappable, IScanableTagInterface, ITagInterface, ITagNumeric)
  private
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(aValue:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;
  protected
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    function  GetValueRaw:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(aValue:Double); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt); override;
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

    procedure Write(aValue:Double); overload;
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
    //: @seealso(TTag.PLCRack)
    property PLCRack;
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
    //: @seealso(TTag.ScanRate)
    property UpdateTime;
    //: @seealso(TPLCTag.ValueTimestamp)
    property ValueTimestamp;
    //: @seealso(TTag.LongAddress)
    property LongAddress;
    //: @seealso(TPLCTag.SyncWrites)
    property SyncWrites;
    //: @seealso(TPLCTag.TagType)
    property TagType;
    //: @seealso(TPLCTag.SwapBytes)
    property SwapBytes;
    //: @seealso(TPLCTag.SwapWords)
    property SwapWords;
    //: @seealso(TPLCTag.SwapDWords)
    property SwapDWords;
    //: @seealso(TPLCTag.TagSizeOnProtocol)
    property TagSizeOnProtocol;
    //: @seealso(TPLCTag.AvgUpdateRate)
    property AvgUpdateRate;
    //: @seealso(TPLCNumber.EnableMaxValue)
    property EnableMaxValue;
    //: @seealso(TPLCNumber.EnableMinValue)
    property EnableMinValue;
    //: @seealso(TPLCNumber.MaxValue)
    property MaxValue;
    //: @seealso(TPLCNumber.MinValue)
    property MinValue;
    //: @seealso(TTag.OnUpdate)
    property OnUpdate;
  end;

implementation

uses hsstrings, math, crossdatetime, dateutils;

function TPLCTagNumber.IsMyCallBack(Cback: TTagCommandCallBack): Boolean;
begin
  Result:=inherited IsMyCallBack(Cback) and (TMethod(Cback).Code=Pointer(@TPLCTagNumber.TagCommandCallBack));
end;

function TPLCTagNumber.GetValueRaw:Double;
begin
  Result := PValueRaw;
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

function  TPLCTagNumber.IsValidValue(aValue:Variant):Boolean;
var
   aux:Double;
   aValueStr: AnsiString;
begin
   aValueStr:=aValue;
   Result := VarIsNumeric(aValue) or
             (VarIsStr(aValue) and TryStrToFloat(aValueStr,aux)) or
             VarIsType(aValue, varboolean);
end;

function TPLCTagNumber.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TPLCTagNumber.SetValueRaw(aValue:Double);
var
  towrite:TArrayOfDouble;
begin
  PModified:=true;
  SetLength(towrite,1);
  towrite[0] := aValue;
  if FSyncWrites then
    Write(towrite,1,0)
  else
    ScanWrite(towrite,1,0);
  SetLength(towrite,0);
end;

procedure TPLCTagNumber.ScanRead;
var
  tr:TTagRec;
begin
  inherited ScanRead;
  if (PProtocolDriver<>nil) and PAutoRead then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.ScanRead(tr);
  end;
end;

procedure TPLCTagNumber.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
  PlcValues:TArrayOfDouble;
begin
  PlcValues:=TagValuesToPLCValues(Values, Offset);
  if (PProtocolDriver<>nil) then begin
     if PAutoWrite then begin
       BuildTagRec(tr,0,0);
       PProtocolDriver.ScanWrite(tr,PlcValues);
     end else begin
       TagCommandCallBack(PlcValues,CrossNow,tcScanWrite,ioOk,0);
       Dec(PCommWriteOk);
     end;
  end else
     TagCommandCallBack(PlcValues, CrossNow, tcScanWrite, ioNullDriver, Offset);
  SetLength(PlcValues,0);
end;

procedure TPLCTagNumber.Read;
var
  tr:TTagRec;
begin
  if PProtocolDriver<>nil then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.Read(tr);
  end;
end;

procedure TPLCTagNumber.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
  PlcValues:TArrayOfDouble;
begin
  PlcValues:=TagValuesToPLCValues(Values, Offset);
  if (PProtocolDriver<>nil) then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.Write(tr,PlcValues);
  end else
     TagCommandCallBack(PlcValues, CrossNow, tcWrite, ioNullDriver, Offset);
  SetLength(PlcValues,0);
end;

procedure TPLCTagNumber.Write(aValue: Double);
var
  x:TArrayOfDouble;
begin
  SetLength(x,1);
  try
    x[0]:=aValue;
    Write(x,1,0);
  finally
    SetLength(x,0);
  end;
end;

procedure TPLCTagNumber.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt);
var
  notify:Boolean;
  TagValues:TArrayOfDouble;
  PreviousTimestamp:TDateTime;
begin
  PreviousTimestamp:=PValueTimeStamp;
  if (csDestroying in ComponentState) then exit;
  inherited TagCommandCallBack(Values, ValuesTimeStamp, TagCommand, LastResult, Offset);
  TagValues:=PLCValuesToTagValues(Values, Offset);

  try
    notify := false;
    case TagCommand of
      tcScanRead,tcRead,tcInternalUpdate:
      begin
        if (Length(TagValues)>0) and (LastResult in [ioOk, ioNullDriver]) then begin
          notify := (PValueRaw<>TagValues[0]) OR (IsNan(TagValues[0]) and (not IsNaN(PValueRaw)));
          PValueRaw := TagValues[0];
          PValueTimeStamp := ValuesTimeStamp;
          if (TagCommand<>tcInternalUpdate) AND (LastResult=ioOk) then begin
            PModified:=False;
            IncCommReadOK(1);
          end;
        end else begin
          if (TagCommand<>tcInternalUpdate) then begin
            IncCommReadFaults(1);
          end;
        end;
      end;
      tcScanWrite,tcWrite:
      begin
        if (Length(TagValues)>0) and (LastResult in [ioOk, ioNullDriver]) then begin
          if LastResult=ioOk then begin
            PModified:=False;
            IncCommWriteOK(1);
          end;
          notify := (PValueRaw<>TagValues[0]);
          PValueRaw := TagValues[0];
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

    if notify or PFirstUpdate then begin
      if TagCommand in [tcRead,tcScanRead] then PFirstUpdate:=false;
      NotifyChange;
    end;

    if (TagCommand in [tcRead,tcScanRead]) and (LastResult=ioOk) and (PreviousTimestamp<>PValueTimeStamp) then
      NotifyUpdate;
  finally
    SetLength(TagValues,0);
  end;
end;

end.
 
