unit hmifaceplatecontainer;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  PLCStruct, PLCStructElement, plcstructstring;

type

  { TFaceplateFrame }

  TFaceplateFrame = class(TFrame)
  private
    ffaceplatetag: TPLCStruct;
    FOnLoaded: TNotifyEvent;
    FTransparent: Boolean;
    procedure AddBlockDelayed(Data: PtrInt);
    procedure setTransparent(AValue: Boolean);
  protected
    procedure AddDesignBlock;
    procedure setfaceplateTag(AValue: TPLCStruct);
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function CreateInstance(TheOwner: TComponent):TFaceplateFrame;
  published
    property FaceplatePLCTag:TPLCStruct read ffaceplatetag write setfaceplateTag;
    property OnLoaded:TNotifyEvent read FOnLoaded write FOnLoaded;
    property Transparent:Boolean read FTransparent write setTransparent;
  end;

  TFaceplateFormClass = class of TFaceplateFrame;

implementation

uses StdCtrls;

{ TFaceplate }

function TFaceplateFrame.CreateInstance(TheOwner: TComponent): TFaceplateFrame;
var
  frmcls: TFaceplateFormClass;
begin
  frmcls:=TFaceplateFormClass(Self.ClassType);
  if frmcls.InheritsFrom(TFaceplateFrame) then
    writeln;
  writeln(frmcls.ClassName);
  result:=frmcls.Create(self);
  if Result is frmcls then
    writeln;
end;

procedure TFaceplateFrame.AddBlockDelayed(Data: PtrInt);
var
  c: Integer;
begin
  writeln('AddBlock delayed');
  for c:=0 to ComponentCount-1 do begin
    writeln(Components[c].Name,'.SetSubComponent(True);');
    Components[c].SetSubComponent(true);
  end;
  AddDesignBlock;
end;

procedure TFaceplateFrame.setTransparent(AValue: Boolean);
begin
  if FTransparent=AValue then Exit;
  FTransparent:=AValue;
end;

procedure TFaceplateFrame.AddDesignBlock;
var
  lbl: TLabel;
begin
  exit;
  if (csDesigning in ComponentState) and Assigned(Parent) then begin
    lbl:=TLabel.Create(Self);
    lbl.Transparent:=true;
    lbl.Caption:='';
    lbl.Align:=alClient;
    lbl.Parent:=Self;
    lbl.BringToFront;
  end;
end;

procedure TFaceplateFrame.setfaceplateTag(AValue: TPLCStruct);
var
  c: Integer;
begin
  for c:=0 to ComponentCount-1 do begin
    //TODO mudar somente tags de faceplate. Change only faceplate tags
    if (Components[c] is TPLCStructItem) {and Tag.Faceplate} then begin
      (Components[c] as TPLCStructItem).PLCBlock:=aValue;
      continue;
    end;
    if (Components[c] is TPLCStructString) {and Tag.Faceplate} then begin
      (Components[c] as TPLCStructString).PLCBlock:=aValue;
      continue;
    end;
  end;
end;

procedure TFaceplateFrame.Loaded;
begin
  inherited Loaded;
  AddDesignBlock;


  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

constructor TFaceplateFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner); 
  SetSubComponent(true);
  //writeln('Create');
  //Application.QueueAsyncCall(@AddBlockDelayed, 0);
end;

end.
