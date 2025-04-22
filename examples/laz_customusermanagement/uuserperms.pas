unit uuserperms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CheckLst, Buttons, ComCtrls, Menus, BCButton, fgl, ControlSecurityManager;


type
  TIntegerList = specialize TFPGList<Integer>;

  TNamedIntegerList = class(TIntegerList)
  public
    Listname : String;
  end;

  { TfrmUserPerms }

  TfrmUserPerms = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckListBox1: TTreeView;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure CheckListBox1Deletion(Sender: TObject; Node: TTreeNode);
    procedure CheckListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure CheckListKeyDownFromPopUpMenu(Sender: TObject);
    procedure PopupMenu1CloseDoNothing(Sender: TObject);
  private
    { private declarations }
    Escolha:Integer;
    popSender: TObject;
    popKey: Word;
    popShift: TShiftState;
    procedure ChangeAuth(auth: TTreeNode; Check, IncludeChildItems: Boolean);
    procedure UpdateParents(auth: TTreeNode);
    procedure UpdateNode(auth: TTreeNode);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function AddPath(aPermPath:String; PathID:Integer; checked:Boolean):TTreeNode;
  end;

var
  frmUserPerms: TfrmUserPerms;

implementation

uses Themes, Types, LCLType, math, hsutils;

{$R *.lfm}

{ TfrmUserPerms }

function TfrmUserPerms.AddPath(aPermPath: String; PathID: Integer;
  checked: Boolean): TTreeNode;
var
  path_parts: TStringArray;
  i: Integer;
  anode: TTreeNode;
begin
  Result:=nil;
  path_parts:=ExplodeString('//',aPermPath);
  for i:=0 to High(path_parts) do begin

    if i=0 then
      anode:=CheckListBox1.Items.FindNodeWithText(path_parts[i])
    else
      anode:=Result.FindNode(path_parts[i]);

    if anode=nil then begin
      if Result=nil then
        anode:=CheckListBox1.Items.Add(Result, path_parts[i])
      else
        anode:=CheckListBox1.Items.AddChild(Result, path_parts[i]);

      anode.Data:=TNamedIntegerList.Create;
      if checked then
        anode.StateIndex:=1
      else
        anode.StateIndex:=0;
    end;

    Result:=anode;
    UpdateParents(anode);
  end;
  if (Result<>nil) and (Result.Data<>nil) and (TObject(Result.Data) is TNamedIntegerList) then begin
    TNamedIntegerList(Result.Data).Listname:=aPermPath;
    if TNamedIntegerList(Result.Data).IndexOf(PathID)=-1 then
      TNamedIntegerList(Result.Data).Add(PathID);
  end;
end;

procedure TfrmUserPerms.CheckListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  hasSubItems, itemIsAAuthorization, StateToChange: Boolean;
  t: TPoint;
  i, sChecked, sUnchecked: Integer;
begin
  //verificações...
  if (Key<>VK_SPACE) or (Shift<>[]) then exit;
  if CheckListBox1.Selected=nil then exit;
  if CheckListBox1.Selected.Data=nil then exit;
  if not (TObject(CheckListBox1.Selected.Data) is TNamedIntegerList) then exit;
  if GetControlSecurityManager.CanAccess(TNamedIntegerList(CheckListBox1.Selected.Data).Listname)=False
  then raise Exception.Create('Você não pode conceder esta permissão.');

  //tudo ok, comecando as verificações de permissão
  hasSubItems:=CheckListBox1.Selected.Count>0;
  itemIsAAuthorization := TNamedIntegerList(CheckListBox1.Selected.Data).Count>0;

  if hasSubItems and itemIsAAuthorization then begin
    //um popup para escolher o que o usuário deseja fazer
    //deve ser exibido para continuar o processo de autorização
    t.x:=0;
    t.y:=0;
    if Escolha=0 then
    begin
      PopupMenu1.PopUp(CheckListBox1.ControlToScreen(t).x+CheckListBox1.Selected.DisplayRect(true).Left,
                     CheckListBox1.ControlToScreen(t).y+CheckListBox1.Selected.DisplayRect(true).Bottom);
      popSender := Sender;
      popKey := Key;
      popShift := Shift;
      PopupMenu1.OnClose := @CheckListKeyDownFromPopUpMenu;
    end else PopupMenu1.OnClose := @PopupMenu1CloseDoNothing;

    case Escolha of
      1: CheckListBox1.Selected.StateIndex:=ifthen(CheckListBox1.Selected.StateIndex=0,1,0);
      2: begin
        sChecked := 0;
        sUnchecked := 0;
        for i:=0 to CheckListBox1.Selected.Count-1 do begin
          if CheckListBox1.Selected.Items[i].StateIndex=0 then Inc(sUnchecked);
          if CheckListBox1.Selected.Items[i].StateIndex=1 then Inc(sChecked);
        end;
        StateToChange := 1=ifthen(sChecked>sUnchecked,0,1);
        for i:=0 to CheckListBox1.Selected.Count-1 do begin
          ChangeAuth(CheckListBox1.Selected.Items[i],StateToChange,hasSubItems);
        end;
      end;
      3: ChangeAuth(CheckListBox1.Selected, CheckListBox1.Selected.StateIndex=0, true);
      else exit;
    end;
  end else begin
    ChangeAuth(CheckListBox1.Selected,
               CheckListBox1.Selected.StateIndex=0,
               hasSubItems);
  end;

  if hasSubItems then UpdateNode(CheckListBox1.Selected);

  UpdateParents(CheckListBox1.Selected);
end;

procedure TfrmUserPerms.CheckListBox1Deletion(Sender: TObject; Node: TTreeNode);
begin
  if (Node.Data<>nil) and (TObject(Node.Data) is TNamedIntegerList) then begin
    TNamedIntegerList(Node.Data).Destroy;
    Node.Data:=nil;
  end;
end;

procedure TfrmUserPerms.MenuItem1Click(Sender: TObject);
begin
  Escolha:=1;
  //CheckListBox1.Selected.StateIndex:=ifthen(CheckListBox1.Selected.StateIndex=0,1,0);
  //UpdateParents(CheckListBox1.Selected);
end;

procedure TfrmUserPerms.MenuItem2Click(Sender: TObject);
var
  i: Integer;
begin
  Escolha:=2;
  //for i:=0 to CheckListBox1.Selected.Count-1 do begin
  //  ChangeAuth(CheckListBox1.Selected.Items[i],
  //  CheckListBox1.Selected.Items[0].StateIndex=0,
  //  True);
  //end;
  //UpdateParents(CheckListBox1.Selected);
end;

procedure TfrmUserPerms.MenuItem3Click(Sender: TObject);
begin
  Escolha:=3;
  //ChangeAuth(CheckListBox1.Selected, CheckListBox1.Selected.StateIndex=0, True);
  //UpdateParents(CheckListBox1.Selected);
end;

procedure TfrmUserPerms.CheckListKeyDownFromPopUpMenu(Sender: TObject);
begin
  if (Escolha in [1..3])
  then CheckListBox1KeyDown(popSender,popKey,popShift)
  else PopupMenu1.OnClose:=@PopupMenu1CloseDoNothing;
  Escolha := 0;
end;

procedure TfrmUserPerms.PopupMenu1CloseDoNothing(Sender: TObject);
begin
  exit;
end;

procedure TfrmUserPerms.ChangeAuth(auth: TTreeNode; Check, IncludeChildItems: Boolean);
var
  i: Integer;
begin
  auth.StateIndex:=ifthen( Check and ((auth.Data=Nil) or (GetControlSecurityManager.CanAccess(TNamedIntegerList(auth.Data).Listname))),1,0);
  if IncludeChildItems and (auth.Count>0) then
    for i:=0 to auth.Count-1 do
      ChangeAuth(auth.Items[i],Check,IncludeChildItems);
end;

procedure TfrmUserPerms.UpdateParents(auth: TTreeNode);
var
  CheckCount, GrayCount, UnCheckCount, i: Integer;
begin
  if auth.Parent=nil then exit;
  CheckCount := 0;
  GrayCount := 0;
  UnCheckCount := 0;
  for i:=0 to auth.Parent.Count-1 do begin
    case auth.Parent.Items[i].StateIndex of
      0: UnCheckCount+=1;
      1: CheckCount+=1;
      2: GrayCount+=1;
    end;
  end;
  if (auth.Parent.Data=nil) or (not (TObject(auth.Parent.Data) is TNamedIntegerList)) or (TNamedIntegerList(auth.Parent.Data).Count=0) then begin
    if UnCheckCount=auth.Parent.Count then
      auth.Parent.StateIndex:=0
    else begin
      if CheckCount=auth.Parent.Count then
        auth.Parent.StateIndex:=1
      else
        auth.Parent.StateIndex:=2;
    end;
  end;

  UpdateParents(auth.Parent);
end;

procedure TfrmUserPerms.UpdateNode(auth: TTreeNode);
var
  UnCheckCount, GrayCount, CheckCount, i: Integer;
begin
  CheckCount := 0;
  GrayCount := 0;
  UnCheckCount := 0;

  for i:=0 to auth.Count-1 do begin

    if auth.HasChildren then UpdateNode(auth.Items[i]);

    case auth.Items[i].StateIndex of
      0: UnCheckCount+=1;
      1: CheckCount+=1;
      2: GrayCount+=1;
    end;

  end;

  if auth.HasChildren then begin
    if UnCheckCount=auth.Count
    then auth.StateIndex:=0
    else begin
      if CheckCount=auth.Count
      then auth.StateIndex:=1
      else auth.StateIndex:=2;
    end;
  end;


end;

constructor TfrmUserPerms.Create(TheOwner: TComponent);
var
    aSize: TSize;
    aBMP: TBitmap;
    aDetails: TThemedElementDetails;
    aRect: TRect;
begin
  inherited Create(TheOwner);

  Escolha:=0;

  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  aSize:=ThemeServices.GetDetailSize(aDetails);
  ImageList1.Width:=aSize.cx;
  ImageList1.Height:=aSize.cy;
  aBMP:=TBitmap.Create;
  with aBMP do
    begin
      SetSize(aSize.cx, aSize.cy);
      Transparent:=True;
      TransparentColor:=clForm;
      Brush.Color:=TransparentColor;
      Canvas.FillRect(0,0, Width,Height);
    end;
  aRect:=Rect(0, 0, aSize.cx, aSize.cy);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  ImageList1.Add(aBMP, nil);
  aBMP.Canvas.FillRect(0,0, Width,Height);;
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  ImageList1.Add(aBMP, nil);
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxMixedNormal);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  ImageList1.Add(aBMP, nil);
  FreeAndNil(aBMP);
end;

end.

