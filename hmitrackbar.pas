{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um controle em forma de TrackBar para a leitura/escrita
            de valores numéricos de tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a TrackBar control to read/write values on
            numeric tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMITrackBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, PLCTag, ProtocolTypes, HMITypes, Tag
  {$IFDEF FPC}, LMessages{$ENDIF};

type
  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe de controle TrackBar para a leitura/escrita de valores
              numéricos em tags.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    @abstract(Class of TrackBar control to read/write values in numeric tags.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  THMITrackBar = class(TTrackBar, IHMIInterface, IHMITagInterface)
  private
    Ftag:TPLCTag;
    FIsEnabled:Boolean;
    FModified:Boolean;

    //implements the IHMIInterface interface
    procedure RefreshTagValue;
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    function  GetPosition:Integer;
    procedure RefreshHMISecurity;
    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;

    //implements the IHMITagInterface interface
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

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que será pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    }
    {$ELSE}
    {:
    Numeric tag that will be linked with the control.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read FTag write SetHMITag;

    {$IFDEF PORTUGUES}
    //: Informa a posição atual da barra.
    {$ELSE}
    //: Tells the current position.
    {$ENDIF}
    Property Position:Integer read GetPosition;

    {$IFDEF PORTUGUES}
    //: Diz se o valor do controle sofreu alguma alteração.
    {$ELSE}
    //: Tells if the control has been modified.
    {$ENDIF}
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
  //
  //check if the tag is valid (only numeric tags);
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //removes the old link.
  if FTag<>nil then begin
    FTag.RemoveCallBacks(self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
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
//
// PROCESS EVENTS
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
//
// END OF PROCESSING OF EVENTS
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
