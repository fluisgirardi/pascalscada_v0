unit modbus_tagscan_req;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TReqItem = record
    LastUpdate:TDateTime;

    station,
    func,
    startaddress,
    size,

    UpdateRate:LongInt;
    Read,
    NeedUpdate:Boolean;
    class operator Equal (a, b: TReqItem) r: Boolean;
  end;
  PReqItem = ^TReqItem;

implementation

class operator TReqItem.Equal (a, b: TReqItem)r: Boolean;
begin
  r:=true;
end;

end.

