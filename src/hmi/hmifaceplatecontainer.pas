unit hmifaceplatecontainer;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, PLCStruct, PLCStructElement, plcstructstring, LMessages;

type

  { TFaceplateFrame }

  TFaceplateFrame = class(TFrame)
  private
    ffaceplatetag: TPLCStruct;
    FOnLoaded: TNotifyEvent;
    FTransparent: Boolean;
    procedure setTransparent(AValue: Boolean);
  protected

    function  IsControlArea(X,Y:Integer):Boolean; virtual;

    procedure setfaceplateTag(AValue: TPLCStruct);
    procedure Loaded; override;
    procedure CMHitTest(var Message: TCMHittest); message CM_HITTEST;
    //procedure CMDesigerHitTest(var Message: TCMHittest) ; message CM_DESIGNERHITTEST;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  published
    property FaceplatePLCTag:TPLCStruct read ffaceplatetag write setfaceplateTag;
    property OnLoaded:TNotifyEvent read FOnLoaded write FOnLoaded;
    property Transparent:Boolean read FTransparent write setTransparent;
  end;

  TFaceplateFormClass = class of TFaceplateFrame;

implementation

uses StdCtrls, LazRegions, LCLIntf, Math;

{ TFaceplate }

procedure TFaceplateFrame.Paint;
begin
  //EraseBackground(Canvas.Handle);
  inherited Paint;
end;

procedure TFaceplateFrame.setTransparent(AValue: Boolean);
begin
  if FTransparent=AValue then Exit;
  FTransparent:=AValue;
end;

function TFaceplateFrame.IsControlArea(X, Y: Integer): Boolean;
begin
  Result:=not FTransparent;
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
var
  c: Integer;
  rgn, rgn2: HRGN;
begin
  inherited Loaded;
  rgn:=CreateRectRgn(0,0,0,0);
  try
    for c:=0 to ControlCount-1 do begin
      try
        rgn2:=CreateRectRgn(
                            Controls[c].Left,
                            Controls[c].Top,
                            Controls[c].Left + Controls[c].Width,
                            Controls[c].Top + Controls[c].Height);
        CombineRgn(rgn,rgn,rgn2,RGN_OR);
        Controls[c].ControlStyle:=Controls[c].ControlStyle+[csNoDesignSelectable];
      finally
        DeleteObject(Rgn2);
      end;
    end;
    SetWindowRgn(Handle,rgn,true);
  finally
    DeleteObject(rgn);
  end;

  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

procedure TFaceplateFrame.CMHitTest(var Message: TCMHittest);
begin
  Message.Result:=ifthen(FTransparent,0,1);
end;

constructor TFaceplateFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csOwnedChildrenNotSelectable];// + [csOpaque];
end;

end.
