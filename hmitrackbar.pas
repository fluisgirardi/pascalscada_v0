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
  SysUtils, Classes, Controls, ComCtrls, PLCTag, ProtocolTypes, HMITypes;

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

    //eventos que serao sobrescritos
    FMouseMove:TMouseMoveEvent;
    FKeyUp:TKeyEvent;
    FKeyDown:TKeyEvent;
    FMouseUp:TMouseEvent;
    FMouseDown:TMouseEvent;
    FChange:TNotifyEvent;
    
    procedure RefreshTagValue;
    procedure HMINotifyChangeCallBack(Sender:TObject);
    procedure RemoveHMITag(Sender:TObject);
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    function  GetPosition:Integer;
    procedure RefreshHMISecurity;
    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;
    
    procedure LocalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    
    procedure LocalMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LocalMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure LocalMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    
    procedure LocalChange(Sender: TObject);
  protected
    //: @exclude
    procedure Loaded; override;
    //: @exclude
    procedure WriteValue;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
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
    
    //--------------------------------------------------------------------------
    //eventos sobrescritos
    //--------------------------------------------------------------------------
    //: @exclude
    property OnChange:TNotifyEvent read FChange write FChange;
    //atualiza o valor quando o usuário mover o mouse
    //: @exclude
    property OnMouseMove:TMouseMoveEvent read FMouseMove write FMouseMove;
    
    //: @exclude
    property OnMouseDown: TMouseEvent read FMouseDown write FMouseDown;
    //: @exclude
    property OnMouseUp: TMouseEvent read FMouseUp write FMouseUp;

    //: @exclude
    property OnKeyUp:TKeyEvent read FKeyUp write FKeyUp;
    //: @exclude
    property OnKeyDown:TKeyEvent read FKeyDown write FKeyDown;
  end;

implementation

uses hsutils;

constructor THMITrackBar.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   
   inherited OnMouseMove:=LocalMouseMove;
   inherited OnMouseDown:=LocalMouseDown;
   inherited OnMouseUp  :=LocalMouseUp;
   inherited OnKeyUp    :=LocalKeyUp;
   inherited OnKeyDown  :=LocalKeyDown;
   inherited OnChange   :=LocalChange;
end;

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
  if ((t as ITagNumeric)=nil) then
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
procedure THMITrackBar.LocalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Modified then
      WriteValue;
      
   if assigned(FKeyUp) then
      FKeyUp(Sender,Key,Shift);
end;

procedure THMITrackBar.LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if assigned(FKeyDown) then
     FKeyDown(Sender,Key,Shift);
end;

procedure THMITrackBar.LocalMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Modified then
      WriteValue;
      
   if Assigned(FMouseUp) then
      FMouseUp(Sender,Button,Shift,X,Y);
end;

procedure THMITrackBar.LocalMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Assigned(FMouseDown) then
      FMouseDown(Sender,Button,Shift,X,Y);
end;

procedure THMITrackBar.LocalMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if Assigned(FMouseMove) then
      FMouseMove(Sender,Shift,X,Y);
end;

procedure THMITrackBar.LocalChange(Sender: TObject);
begin
  FModified:=true;
  if Assigned(FChange) then
     FChange(Sender);
end;

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
