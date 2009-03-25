{:
Implementa um controle em forma de TrackBar para a leitura/escrita de valores
numéricos de tags.
}
unit HMITrackBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, PLCTag, ProtocolTypes, HMITypes
  {$IFDEF FPC}, LMessages{$ENDIF};

type
  {:
  Implementa um controle em forma de TrackBar para a leitura/escrita de valores
  numéricos de tags.
  }
  THMITrackBar = class(TTrackBar, IHMIInterface)
  private
    Ftag:TPLCTag;
    FIsEnabled:Boolean;
    FModified:Boolean;
    
    procedure RefreshTagValue;
    procedure HMINotifyChangeCallBack(Sender:TObject);
    procedure RemoveHMITag(Sender:TObject);
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    function  GetPosition:Integer;
    procedure RefreshHMISecurity;
    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;
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
    Tag numérico que será usado pelo controle.
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

uses hsutils;

destructor THMITrackBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveChangeCallBack(HMINotifyChangeCallBack);
  inherited Destroy;
end;

procedure THMITrackBar.Loaded;
begin
  inherited Loaded;
  RefreshTagValue;
end;

procedure THMITrackBar.RemoveHMITag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

procedure THMITrackBar.RefreshTagValue;
begin
   if (Ftag as ITagNumeric)<>nil then
      inherited Position := FloatToInteger((Ftag as ITagNumeric).Value);
   FModified:=false;
end;

procedure THMITrackBar.HMINotifyChangeCallback(Sender:TObject);
begin
  RefreshTagValue;
end;

procedure THMITrackBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and ((t as ITagNumeric)=nil) then
     raise Exception.Create('Somente tags numéricos são aceitos!');

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddChangeCallBack(HMINotifyChangeCallback, RemoveHMITag);
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
  if [csLoading,csReading,csDesigning]*ComponentState<>[] then exit;

  if (Ftag as ITagNumeric)<>nil then
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

end.
