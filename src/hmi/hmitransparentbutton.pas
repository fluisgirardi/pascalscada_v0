unit HMITransparentButton;

interface

uses
  Classes, Sysutils, Controls, LMessages, hmi_draw_basiccontrol, BGRABitmap;

type

  { THMITransparentButton }

  THMITransparentButton = class(THMIBasicControl)
  private
    //procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
  published
    property OnClick;
    property OnMouseUp;
    //property on
  end;

implementation

{ THMITransparentButton }

//sem Paint proprio: o DrawControl da base ja' atribui um bitmap vazio do
//tamanho do controle - transparente, que e' todo o ponto deste botao. O
//override que existia aqui criava um bitmap por conta propria, vazava o da
//base e deixava o campo nulo depois de pintar.
//no Paint of its own: the base's DrawControl already assigns an empty bitmap
//the size of the control - transparent, which is this button's whole point.
//The override that used to be here built a bitmap of its own, leaked the
//base's one and left the field nil after painting.

procedure THMITransparentButton.WMLButtonUp(var Message: TLMLButtonUp);
begin
  //DebugLn('TControl.WMLButtonUp A ',Name,':',ClassName,' csCaptureMouse=',DbgS(csCaptureMouse in ControlStyle),' csClicked=',DbgS(csClicked in ControlState));
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    //DebugLn('TControl.WMLButtonUp B ',dbgs(ClientRect.Left),',',dbgs(ClientRect.Top),',',dbgs(ClientRect.Right),',',dbgs(ClientRect.Bottom),' ',dbgs(Message.Pos.X),',',dbgs(Message.Pos.Y));
    //if PtInRect(ClientRect, SmallPointToPoint(Message.Pos))
    //then begin
      //DebugLn('TControl.WMLButtonUp C');
      Click;
    //end;
  end;

  //DoMouseUp(Message, mbLeft);
  //DebugLn('TControl.WMLButtonUp END');
end;

//procedure THMITransparentButton.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
//var
//  P: TPoint;
//begin
//  if not (csNoStdEvents in ControlStyle) then
//    with Message do
//    begin
//      if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
//      begin
//        P := ClientToScreen(Point(XPos, YPos));
//        DragManager.MouseUp(Button, KeysToShiftState(Keys), P.X, P.Y);
//        Message.Result := 1;
//      end;
//      MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
//    end;
//end;

end.
