{:
  @abstract(Implementa um controle em forma de TrackBar para a leitura/escrita
            de valores numéricos de tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
unit HMITrackBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, PLCTag, ProtocolTypes, HMITypes, Tag
  {$IFDEF FPC}, LMessages{$ENDIF};

type
  {:
  Implementa um controle em forma de TrackBar para a leitura/escrita de valores
  numéricos de tags.
  }
  THMITrackBar = class(TTrackBar, IHMIInterface, IHMITagInterface)
  private
    Ftag:TPLCTag;
    FIsEnabled:Boolean;
    FModified:Boolean;
    
    procedure RefreshTagValue;
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    function  GetPosition:Integer;
    procedure RefreshHMISecurity;
    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF FPC}
    procedure DoChange(var msg); message LM_CHANGED;
    {$ELSE}
    procedure Changed; override;
    {$ENDIF}

    //: @exclude
    procedure Loaded; override;
    //: @exclude
    procedure WriteValue;
  public
    //: @exclude
    destructor  Destroy; override;
  published
    {:
    Tag numérico que será pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    property PLCTag:TPLCTag read FTag write SetHMITag;
    //: Informa a posição atual da barra.
    Property Position:Integer read GetPosition;
    //: Diz se o valor do controle sofreu alguma alteração.
    property Modified:Boolean read FModified;
  end;

implementation

uses hsstrings, hsutils;

destructor THMITrackBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveCallBacks(self as IHMITagInterface);
  inherited Destroy;
end;

procedure THMITrackBar.Loaded;
begin
  inherited Loaded;
  RefreshTagValue;
end;

procedure THMITrackBar.RefreshTagValue;
begin
   if (FTag<>nil) AND Supports(Ftag, ITagNumeric) then
      inherited Position := FloatToInteger((Ftag as ITagNumeric).Value);
   FModified:=false;
end;

procedure THMITrackBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveCallBacks(self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddCallBacks(self as IHMITagInterface);
    FTag := t;
    RefreshTagValue;
  end;
  FTag := t;
end;

function THMITrackBar.GetHMITag;
begin
  Result:=Ftag;
end;

function THMITrackBar.GetPosition:Integer;
begin
  Result := inherited Position;
end;

procedure THMITrackBar.WriteValue;
begin
  if [csLoading,csReading]*ComponentState<>[] then exit;

  if (FTag<>nil) AND Supports(Ftag, ITagNumeric) then
    (Ftag as ITagNumeric).Value := Position;
end;

//------------------------------------------------------------------------------
// PROCESSAMENTO DE EVENTOS
//------------------------------------------------------------------------------
procedure THMITrackBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
   if Modified then
      WriteValue;
      
   inherited KeyUp(Key, Shift);
end;

procedure THMITrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Modified then
      WriteValue;
      
   inherited MouseUp(Button, Shift, X, Y);
end;

{$IFDEF FPC}
procedure THMITrackBar.DoChange(var msg);
begin
  FModified:=true;
  inherited DoChange(msg);
end;
{$ELSE}
procedure THMITrackBar.Changed;
begin
  FModified:=true;
  inherited Changed;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// FIM DO PROCESSAMENTO DE EVENTOS
//------------------------------------------------------------------------------

procedure THMITrackBar.RefreshHMISecurity;
begin

end;

procedure THMITrackBar.SetHMIEnabled(v:Boolean);
begin
   { todo: }
   inherited Enabled := v;
   FIsEnabled := v;
end;

function  THMITrackBar.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMITrackBar.NotifyReadOk;
begin

end;

procedure THMITrackBar.NotifyReadFault;
begin

end;

procedure THMITrackBar.NotifyWriteOk;
begin

end;

procedure THMITrackBar.NotifyWriteFault;
begin
  RefreshTagValue;
end;

procedure THMITrackBar.NotifyTagChange(Sender:TObject);
begin
  RefreshTagValue;
end;

procedure THMITrackBar.RemoveTag(Sender:TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
