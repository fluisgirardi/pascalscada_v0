{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Driver de protocolo de mentira, para os testes das portas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A porta de comunicacao guarda os drivers pendurados nela e avisa cada um
  quando muda de estado ou quando esta' sendo destruida. Para exercitar esse
  lado da porta nao interessa o que o driver faz com os bytes - interessa que
  ele seja um TProtocolDriver de verdade, que implemente a interface de aviso
  e que se registre e se desregistre como os drivers reais.
}
{$ELSE}
{:
  @abstract(A fake protocol driver, for the port tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The communication port keeps the drivers attached to it and notifies each
  one when it changes state or is being destroyed. To exercise that side of
  the port what the driver does with the bytes does not matter - what matters
  is that it is a real TProtocolDriver, implementing the notification
  interface and registering and unregistering like the real ones do.
}
{$ENDIF}
unit testsupport.fakedriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtocolDriver, ProtocolTypes, Tag, commtypes;

type

  { TFakeProtocolDriver }

  TFakeProtocolDriver = class(TProtocolDriver)
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    function  SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
  end;

implementation

constructor TFakeProtocolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
end;

procedure TFakeProtocolDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=1;
end;

procedure TFakeProtocolDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin
  SetLength(values.Values, 0);
  values.LastQueryResult:=ioOk;
end;

function TFakeProtocolDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  Result:=ioOk;
end;

function TFakeProtocolDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  SetLength(Values, 0);
  Result:=ioOk;
end;

function TFakeProtocolDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

end.
