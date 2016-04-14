{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa a base para Tags Blocos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the base for all block tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit TagBlock;

interface

uses
  SysUtils, Classes, Tag, PLCTag;

type
  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base para tags blocos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Classe base para os tags TPLCBlock e TPLCString.
  }
  {$ELSE}
  {:
  @abstract(Base class for all block tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Base class for tags TPLCBlock and TPLCString.
  }
  {$ENDIF}
  TTagBlock = class(TPLCTag)
  protected
    {$IFDEF PORTUGUES}
    //: Array que armazena os valores assincronos.
    {$ELSE}
    //: Array that stores the values of the tag block.
    {$ENDIF}
    PValues:TArrayOfDouble;
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
    //: @seealso(TTag.RefreshTime)
    property RefreshTime;
    //: @seealso(TTag.ScanRate)
    property UpdateTime;
    //: @seealso(TPLCTag.ValueTimestamp)
    property ValueTimestamp;
    //: @seealso(TTag.LongAddress)
    property LongAddress;
  end;

implementation

uses crossdatetime;

procedure TTagBlock.ScanRead;
var
  tr:TTagRec;
begin
  inherited ScanRead;
  if (PProtocolDriver<>nil) and PAutoRead then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.ScanRead(tr);
  end;
end;

procedure TTagBlock.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
begin
  if Count=0 then exit;
  if (PProtocolDriver<>nil) then begin
    if PAutoWrite then begin
      BuildTagRec(tr,Count,Offset);
      PProtocolDriver.ScanWrite(tr,Values);
    end else begin
      TagCommandCallBack(Values,CrossNow,tcScanWrite,ioOk,Offset);
      Dec(PCommWriteOk);
    end;
  end else
    TagCommandCallBack(Values,CrossNow,tcScanWrite,ioNullDriver,Offset);
end;

procedure TTagBlock.Read;
var
  tr:TTagRec;
begin
  if PProtocolDriver<>nil then begin
    BuildTagRec(tr,0,0);
    PProtocolDriver.Read(tr);
  end;
end;

procedure TTagBlock.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  tr:TTagRec;
begin
  if Count=0 then exit;
  if PProtocolDriver<>nil then begin
    BuildTagRec(tr,Count,Offset);
    PProtocolDriver.Write(tr,Values);
  end else
    TagCommandCallBack(Values,CrossNow,tcWrite,ioNullDriver,Offset);
end;



end.
