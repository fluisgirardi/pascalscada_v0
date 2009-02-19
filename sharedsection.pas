unit SharedSection;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  CrossEvent, syncobjs, SysUtils;

type
  TSharedSection = class(TObject)
  private
    EventArray:array of TCrossEvent;
    OwnersCount:array of Integer;
    fCS:TCriticalSection;
    fOwner:Integer;
    procedure ExceptOutOfBounds(i:integer);
  public
    Constructor Create(GroupCount:integer);
    destructor  Destroy; override;
    procedure   Enter(GroupID:Integer);
    procedure   Leave(GroupID:Integer);
  published
    property OwnedBy:Integer read fOwner;
  end;

implementation

Constructor TSharedSection.Create(GroupCount:integer);
var
  c:Integer;
begin
  Inherited Create;
  fcs:=TCriticalSection.Create;
  fcs.Enter;
  fOwner:=-1;
  SetLength(EventArray,GroupCount);
  SetLength(OwnersCount,GroupCount);
  for c:=0 to GroupCount-1 do begin
    EventArray[c] := TCrossEvent.Create(nil,true,false,'');
    EventArray[c].SetEvent;
    OwnersCount[c] := 0;
  end;
  fcs.Leave;
end;

destructor  TSharedSection.Destroy;
var
  c:Integer;
begin
  fcs.Destroy;

  for c:=0 to High(EventArray) do
    EventArray[c].Destroy;
  SetLength(EventArray,0);
  SetLength(OwnersCount,0);
end;

procedure TSharedSection.ExceptOutOfBounds(i:integer);
begin
  if (i<0) or (i>High(EventArray)) then
    raise Exception.Create('Fora dos limites!');
end;

procedure TSharedSection.Enter(GroupID:Integer);
var
  c:Integer;
label
  inicio;
begin
  ExceptOutOfBounds(GroupID);

inicio:

  EventArray[GroupID].WaitFor($FFFFFFFF);
  fcs.Enter;
  if fOwner<>-1 then begin
    fcs.Leave;
    goto inicio;
  end;
  if fOwner=GroupID then begin
    inc(OwnersCount[GroupID]);
    fcs.Leave;
    exit;
  end;
  fOwner:=GroupID;
  OwnersCount[GroupID] := 1;
  for c:=0 to High(EventArray) do
    if c<>GroupID then
      EventArray[c].ResetEvent;
  fcs.Leave;
end;

procedure TSharedSection.Leave(GroupID:Integer);
var
  c:Integer;
begin
  ExceptOutOfBounds(GroupID);

  fcs.Enter;
  if fOwner<>GroupID then begin
    fcs.Leave;
    exit;
  end;
  if (fOwner>=0) and (OwnersCount[GroupID]>0) then
    dec(OwnersCount[GroupID]);
  if OwnersCount[GroupID]<1 then begin
    OwnersCount[GroupID] := 0;
    fOwner:=-1;
    for c:=0 to High(EventArray) do
      EventArray[c].SetEvent;
  end;
  fcs.Leave;
end;


end.
