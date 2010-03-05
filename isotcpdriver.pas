{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de ...
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit ISOTCPDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  ProtocolDriver, S7Types, Tag, ProtocolTypes;

type
  TISOTCPDriver = class(TProtocolDriver)
  private
    FConnectionWay:TISOTCPConnectionWay;
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
  protected
    procedure DoAddTag(TagObj:TTag); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  published
    property ReadSomethingAlways;
    property ConnectionWay:TISOTCPConnectionWay read FConnectionWay write SetISOConnectionWay;
  end;

implementation

procedure TISOTCPDriver.SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
begin
  FConnectionWay:=NewISOConWay;
end;

procedure TISOTCPDriver.DoAddTag(TagObj:TTag);
begin

end;

procedure TISOTCPDriver.DoDelTag(TagObj:TTag);
begin

end;

procedure TISOTCPDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin

end;

procedure TISOTCPDriver.DoScanRead(Sender:TObject; var NeedSleep:Integer);
begin

end;

procedure TISOTCPDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function  TISOTCPDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TISOTCPDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

end.
