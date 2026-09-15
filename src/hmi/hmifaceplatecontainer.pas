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
    procedure StructRemoved(Sender:TObject);
    procedure ClipToChildren;
  protected

    function  IsControlArea(X,Y:Integer):Boolean; virtual;

    procedure setfaceplateTag(AValue: TPLCStruct);
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure CMHitTest(var Message: TCMHittest); message CM_HITTEST;
    //procedure CMDesigerHitTest(var Message: TCMHittest) ; message CM_DESIGNERHITTEST;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  //a estrutura fica guardada: e' o que a propriedade devolve, e o que o IDE
  //grava no .lfm da tela. Sem guardar, ler FaceplatePLCTag dava sempre nil e
  //a estrutura escolhida no projeto se perdia ao salvar.
  //the struct is kept: it is what the property gives back, and what the IDE
  //writes to the screen's .lfm. Without keeping it, reading FaceplatePLCTag
  //always gave nil and the struct chosen at design time was lost on save.
  if ffaceplatetag=AValue then exit;
  if Assigned(ffaceplatetag) then
    ffaceplatetag.RemoveAllHandlersFromObject(Self);
  ffaceplatetag:=AValue;
  if Assigned(ffaceplatetag) then
    ffaceplatetag.AddRemoveTagHandler(@StructRemoved);

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

procedure TFaceplateFrame.StructRemoved(Sender:TObject);
begin
  if Sender=ffaceplatetag then
    ffaceplatetag:=nil;
end;

procedure TFaceplateFrame.ClipToChildren;
var
  c: Integer;
  rgn, rgn2: HRGN;
begin
  //a janela do quadro fica recortada pela regiao dos seus controles: entre
  //eles o clique e a pintura vao para quem esta' atras
  //the frame's window is clipped by the region of its controls: between them
  //the click and the painting go to whoever is behind
  rgn:=CreateRectRgn(0,0,0,0);
  try
    for c:=0 to ControlCount-1 do begin
      rgn2:=CreateRectRgn(
                          Controls[c].Left,
                          Controls[c].Top,
                          Controls[c].Left + Controls[c].Width,
                          Controls[c].Top + Controls[c].Height);
      try
        CombineRgn(rgn,rgn,rgn2,RGN_OR);
      finally
        DeleteObject(Rgn2);
      end;
    end;
    SetWindowRgn(Handle,rgn,true);
  finally
    DeleteObject(rgn);
  end;
end;

procedure TFaceplateFrame.Loaded;
var
  c: Integer;
begin
  inherited Loaded;
  for c:=0 to ControlCount-1 do
    Controls[c].ControlStyle:=Controls[c].ControlStyle+[csNoDesignSelectable];
  //o recorte da janela fica para quando houver janela. O Loaded roda no meio
  //do Create - antes do Parent - num quadro criado por codigo, e pedir o
  //Handle aqui morria com "has no parent window": so' dava para usar o
  //faceplate embutido num formulario.
  //the window clipping waits until there is a window. Loaded runs in the
  //middle of Create - before Parent - on a frame created by code, and asking
  //for the Handle here died with "has no parent window": the faceplate could
  //only be used inlined in a form.
  if HandleAllocated then
    ClipToChildren;
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

procedure TFaceplateFrame.CreateWnd;
begin
  inherited CreateWnd;
  //toda vez que a janela e' criada de novo - o recorte nao sobrevive a isso
  //every time the window is created anew - the clipping does not survive it
  if [csLoading,csReading]*ComponentState=[] then
    ClipToChildren;
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

destructor TFaceplateFrame.Destroy;
begin
  if Assigned(ffaceplatetag) then
    ffaceplatetag.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

end.
