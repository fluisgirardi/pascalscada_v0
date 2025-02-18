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
    procedure AddBlockDelayed(Data: PtrInt);
  protected
    procedure AddDesignBlock;
    procedure setfaceplateTag(AValue: TPLCStruct);
    procedure Loaded; override;
    procedure Updating; override;
    procedure Update; override;
    procedure AfterConstruction; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function CreateInstance(TheOwner: TComponent):TFaceplateFrame;
  published
    property FaceplatePLCTag:TPLCStruct read ffaceplatetag write setfaceplateTag;
  end;

  TFaceplateFormClass = class of TFaceplateFrame;

  { TFaceplateContainer }

  TFaceplateContainer = class(TPanel)
  private
    ffaceplate: TFaceplateFrame;
    FFaceplateInstance: TFaceplateFrame;
    ffaceplatetag: TPLCStruct;
    procedure setFaceplate(AValue: TFaceplateFrame);
    procedure setfaceplateTag(AValue: TPLCStruct);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    property FaceplateInstanc:TFaceplateFrame read FFaceplateInstance;
  published
    property Faceplate:TFaceplateFrame read ffaceplate write setFaceplate;
    property FaceplatePLCTag:TPLCStruct read ffaceplatetag write setfaceplateTag;
  end;

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
  writeln('Loaded');
  AddDesignBlock;
end;

procedure TFaceplateFrame.Updating;
begin
  inherited Updating;
  writeln('updating');
  AddDesignBlock;
end;

procedure TFaceplateFrame.Update;
begin
  inherited Update;
  writeln('Update');
  AddDesignBlock;
end;

procedure TFaceplateFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  writeln('after construction');
  AddDesignBlock
end;

constructor TFaceplateFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner); 
  SetSubComponent(true);
  writeln('Create');
  Application.QueueAsyncCall(@AddBlockDelayed, 0);

end;

{ TFaceplateContainer }

procedure TFaceplateContainer.setFaceplate(AValue: TFaceplateFrame);
begin
  if ffaceplate=AValue then Exit;
  if not (AValue is TFaceplateFrame) then exit;

  if Assigned(ffaceplate) then
    ffaceplate.RemoveFreeNotification(self);

  ffaceplate:=AValue;

  if Assigned(ffaceplate) then
    ffaceplate.FreeNotification(self);

  if Assigned(FFaceplateInstance) then
    FreeAndNil(FFaceplateInstance);

  //ffaceplate.SetFaceplateTag(ffaceplatetag);
  if {(([csDesigning]*ComponentState)=[]) and} Assigned(ffaceplate) then begin
    FFaceplateInstance:=ffaceplate.CreateInstance(Self);
    FFaceplateInstance.SetBounds(0,0,Width,Height);
    FFaceplateInstance.SetParent(self);
    FFaceplateInstance.Show;
  end;
end;

procedure TFaceplateContainer.setfaceplateTag(AValue: TPLCStruct);
begin
  if ffaceplatetag=AValue then Exit;
  ffaceplatetag:=AValue;
  if Assigned(ffaceplate) then
    ffaceplate.SetFaceplateTag(AValue);
end;

procedure TFaceplateContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=ffaceplate) then
    setFaceplate(nil);
  inherited Notification(AComponent, Operation);
end;

end.
