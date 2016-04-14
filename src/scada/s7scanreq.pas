unit s7scanreq;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TReqItem }

  TS7ScanReqItem = record
    LastUpdate:TDateTime;
    iPLC,
    iDB,
    iDBNum,
    iReqType,
    iStartAddress,
    iSize,
    UpdateRate:LongInt;
    Read,
    NeedUpdate:Boolean;
    class operator Equal (a, b: TS7ScanReqItem) r: Boolean;
  end;
  PS7ScanReqItem = ^TS7ScanReqItem;

implementation

{ TReqItem }

class operator TS7ScanReqItem.Equal(a, b: TS7ScanReqItem)r: Boolean;
begin
  r:=(a.LastUpdate    = b.LastUpdate   ) and
     (a.iPLC          = b.iPLC         ) and
     (a.iDB           = b.iDB          ) and
     (a.iDBNum        = b.iDBNum       ) and
     (a.iReqType      = b.iReqType     ) and
     (a.iStartAddress = b.iStartAddress) and
     (a.iSize         = b.iSize        ) and
     (a.UpdateRate    = b.UpdateRate   ) and
     (a.NeedUpdate    = b.NeedUpdate);
end;

end.

