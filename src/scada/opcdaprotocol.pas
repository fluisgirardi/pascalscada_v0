unit OPCDAProtocol;

interface

uses
  ProtocolDriver;

type

  { TOPCDAProtocol }

  TOPCDAProtocol = class(TProtocolDriver)
  private
    FHost,
    FServer:String;
    procedure SetHost(const AValue: String);
    procedure SetServer(const AValue: String);
  protected

  published
    property Host:String read FHost write SetHost;
    property Server:String read FServer write SetServer;
  end;

implementation

{ TOPCDAProtocol }

procedure TOPCDAProtocol.SetHost(const AValue: String);
begin
  if FHost=AValue then exit;
  FHost:=AValue;
end;

procedure TOPCDAProtocol.SetServer(const AValue: String);
begin
  if FServer=AValue then exit;
  FServer:=AValue;
end;

end.
