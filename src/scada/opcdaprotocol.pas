unit OPCDAProtocol;

interface

uses
  ProtocolDriver;

type

  { TOPCDAProtocol }

  TOPCDAProtocol = class(TProtocolDriver)
  private
    FHost,
    FServer:AnsiString;
    procedure SetHost(const AValue: AnsiString);
    procedure SetServer(const AValue: AnsiString);
  protected

  published
    property Host:AnsiString read FHost write SetHost;
    property Server:AnsiString read FServer write SetServer;
  end;

implementation

{ TOPCDAProtocol }

procedure TOPCDAProtocol.SetHost(const AValue: AnsiString);
begin
  if FHost=AValue then exit;
  FHost:=AValue;
end;

procedure TOPCDAProtocol.SetServer(const AValue: AnsiString);
begin
  if FServer=AValue then exit;
  FServer:=AValue;
end;

end.
