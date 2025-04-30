unit uusermanagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ComCtrls, ExtCtrls, Buttons, Menus, ActnList, DbCtrls, StdCtrls, PairSplitter,
  ControlSecurityManager, BGRAImageList, BCButton, uaddgroup, types;

type
  TChipCardRead = class(TForm)
  public
    ChipCard:UTF8String;
  end;

  { TfrmUserManagement }

  TfrmUserManagement = class(TForm)
    ActionList1: TActionList;
    AddUserBtn: TSpeedButton;
    BCButton1: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BGRAImageList1: TBGRAImageList;
    groupList1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    EditUser: TPascalSCADASecureAction;
    ChangeUserPass: TPascalSCADASecureAction;
    AddUsers: TPascalSCADASecureAction;
    ChangeUserPerms: TPascalSCADASecureAction;
    MenuItem4: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PageControl1: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    secureRemoveChipCardFromUser: TPascalSCADASecureAction;
    secureAssignChipCardToUser: TPascalSCADASecureAction;
    RemoveUserBtn: TSpeedButton;
    secureAddUserToGroup: TPascalSCADASecureAction;
    secureRemoveGroupMember: TPascalSCADASecureAction;
    secureAddGroup: TPascalSCADASecureAction;
    secureChangeGroupPerms: TPascalSCADASecureAction;
    secureRemoveGroup: TPascalSCADASecureAction;
    PopupMenu2: TPopupMenu;
    RemoveUser: TPascalSCADASecureAction;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    userList: TListView;
    Panel1: TPanel;
    groupList: TListView;
    userMemberList: TListView;
    AvailableUserList: TListView;
    procedure AddUsersExecute(Sender: TObject);
    procedure AvailableUserListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure AvailableUserListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AvailableUserListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AvailableUserListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BCButton1Click(Sender: TObject);
    procedure ChangeUserPermsExecute(Sender: TObject);
    procedure EditUserExecute(Sender: TObject);
    procedure groupList1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure groupListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure groupListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RemoveUserExecute(Sender: TObject);
    procedure secureAddGroupExecute(Sender: TObject);
    procedure secureAddUserToGroupExecute(Sender: TObject);
    procedure secureAssignChipCardToUserExecute(Sender: TObject);
    procedure secureChangeGroupPermsExecute(Sender: TObject);
    procedure secureRemoveChipCardFromUserExecute(Sender: TObject);
    procedure secureRemoveGroupExecute(Sender: TObject);
    procedure secureRemoveGroupMemberExecute(Sender: TObject);
    procedure userMemberListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure userMemberListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure AvailableUserListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure userListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure userListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure userMemberListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure userMemberListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure userMemberListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    vids:array of Integer;
    objsqlist: String;
    separador: String;
    frmChipCardRead:TChipCardRead;
    procedure CheckIfCardHasBeenRemoved(Sender: TObject);
    procedure ChipCardReaderProc(Data: PtrInt);
    procedure FrmChipCardCanBeClosed(Sender: TObject; var CanClose: Boolean);
    procedure StartDelayedChipCardRead;
    procedure WarningCanBeClosed(Sender: TObject; var CanClose: Boolean);
  private
    { private declarations }
    procedure RefreshUserList;
    procedure RefreshGroupList;
    function ValidLogin(aStr:String):Boolean;
    procedure VerifyChilds(childNode: TTreeNode);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function SortPermissionList(Node1, Node2: TTreeNode): integer;
    procedure AddUserToGroup(const uid, gid:Integer);             
    procedure RemoveUserFromGroup(const uid, gid:Integer);
  end;

var
  frmUserManagement: TfrmUserManagement;

implementation

uses udm, uadd_edituser, md5, uuserperms, variants;

{$R *.lfm}

{ TfrmUserManagement }

procedure TfrmUserManagement.userListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and (Item<>nil) and (item.Data<>nil) then begin
    if not dm.tblUsers.Locate('id_user',PtrInt(item.Data),[]) then
      RefreshUserList;
  end;
end;

procedure TfrmUserManagement.userMemberListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and assigned(userMemberList.Selected) then begin
    userMemberList.BeginDrag(false, 10);
  end;
end;

procedure TfrmUserManagement.userMemberListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  userMemberList.EndDrag(userMemberList.Dragging);
end;

procedure TfrmUserManagement.userMemberListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  secureRemoveGroupMember.Enabled:=userMemberList.Selected<>nil;
end;

procedure TfrmUserManagement.CheckIfCardHasBeenRemoved(Sender: TObject);
begin
  if assigned(dm.CustomizedUserManagement1.ChipCardReader) and (Sender is TTimer) and (TTimer(sender).Owner is TForm) then begin
    if dm.CustomizedUserManagement1.ChipCardReader.IsEmptyChipCard then begin
      TForm(TTimer(sender).Owner).ModalResult:=mrOK;
      TTimer(sender).Enabled:=false;
    end;
  end;
end;

procedure TfrmUserManagement.WarningCanBeClosed(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(dm.CustomizedUserManagement1.ChipCardReader) then
    canClose:=dm.CustomizedUserManagement1.ChipCardReader.IsEmptyChipCard
  else
    CanClose:=true;
end;

procedure TfrmUserManagement.RefreshUserList;
var
  r: Integer;
  uobj: TListItem;
begin
  userList.Items.Clear;

  dm.tblUsers.Close;
  dm.tblUsers.Open;

  for r:=1 to dm.tblUsers.RecordCount do begin
    dm.tblUsers.RecNo:=r;
    uobj:=userList.Items.Add;
    uobj.Caption:=dm.tblUsersds_login.Value;
    if dm.tblUsersop_blocked.Value then
      uobj.ImageIndex:=1
    else
      uobj.ImageIndex:=0;
    uobj.Data:=Pointer(dm.tblUsersid_user.Value);
  end;
end;

procedure TfrmUserManagement.RefreshGroupList;
var
  r: Integer;
  uobj: TListItem;
begin


  dm.tblGroups.Close;
  dm.tblGroups.Open;
  secureAddUserToGroup.Enabled:=false;
  secureRemoveGroupMember.Enabled:=false;

  groupList.Items.Clear;
  for r:=1 to dm.tblGroups.RecordCount do begin
    dm.tblGroups.RecNo:=r;
    uobj:=groupList.Items.Add;
    uobj.Caption:=dm.tblGroupsds_groupname.Value;
    //if dm.tblUsersop_blocked.Value then
    //  uobj.ImageIndex:=1
    //else
      uobj.ImageIndex:=2;
    uobj.Data:=Pointer(dm.tblGroupsid_group.Value);
  end;

  groupList1.Items.Clear;
  for r:=1 to dm.tblGroups.RecordCount do begin
    dm.tblGroups.RecNo:=r;
    uobj:=groupList1.Items.Add;
    uobj.Caption:=dm.tblGroupsds_groupname.Value;
    //if dm.tblUsersop_blocked.Value then
    //  uobj.ImageIndex:=1
    //else
      uobj.ImageIndex:=2;
    uobj.Data:=Pointer(dm.tblGroupsid_group.Value);
  end;
end;

function TfrmUserManagement.ValidLogin(aStr: String): Boolean;
var
  c: Integer;
begin
  if aStr.Trim='' then exit(False);
  if aStr.Contains(' ') then exit(false);
  if not (aStr[1] in ['a'..'z','A'..'Z']) then exit(false);
  for c:=1 to Length(aStr) do
    if not (aStr[1] in ['a'..'z','A'..'Z','0'..'9']) then exit(false);
  exit(true);
end;

procedure TfrmUserManagement.userListContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=userList.SelCount<1;
end;

procedure TfrmUserManagement.AddUsersExecute(Sender: TObject);
var
  frm: TfrmInsertEditUser;
begin
  if (dm.tblUsers.State in dsEditModes) then
    dm.tblUsers.Cancel;

  dm.tblUsers.Append;

  frm:=TfrmInsertEditUser.Create(Self);
  frm.Caption:='Adicionar um usuário';
  try
    if frm.ShowModal=mrOK then begin
      if not ValidLogin(dm.tblUsersds_login.Value) then begin
        dm.tblUsers.Cancel;
        raise exception.Create('login é inválido! O login só pode conter letras e números e deve obrigatoriamente iniciar com uma letra!');
      end;
      if frm.edtSenha1.Modified or frm.edtSenha2.Modified then begin
        if frm.edtSenha1.Text=frm.edtSenha2.Text then begin
          dm.tblUsersds_password.Value:=MD5Print(MD5String(frm.edtSenha1.Text));
        end else begin
          dm.tblUsers.Cancel;
          raise exception.Create('A senha é diferente da contra senha. Digite a mesma senha nos "Senha" e "Confirme senha"!');
        end;
      end;
      dm.tblUsers.Post;
    end else
      dm.tblUsers.Cancel;
  finally
    FreeAndNil(frm);
  end;
  RefreshUserList;
end;

procedure TfrmUserManagement.AvailableUserListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  i: TListItem;
begin
  //estou arrastando um usuário membro para cá, ou seja, ele não é mais membro
  if source=userMemberList then begin
    if userMemberList.Selected<>nil then begin
      i:=AvailableUserList.Items.Add;
      i.Assign(userMemberList.Selected);
      userMemberList.Selected.Destroy;
    end;
  end;
end;

procedure TfrmUserManagement.AvailableUserListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and assigned(AvailableUserList.Selected) then begin
    AvailableUserList.BeginDrag(false, 10);
  end;
end;

procedure TfrmUserManagement.AvailableUserListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AvailableUserList.EndDrag(AvailableUserList.Dragging);
end;

procedure TfrmUserManagement.AvailableUserListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  secureAddUserToGroup.Enabled:=AvailableUserList.Selected<>nil;
end;

procedure TfrmUserManagement.BCButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmUserManagement.VerifyChilds(childNode:TTreeNode);
  var
    authChild: TTreeNode;
    ic: Integer;
  begin
    authChild:=childNode;
    while authChild<>nil do begin

      //se o item esta marcado, tem uma lista e tem alguma permissão na lista, adiciona estas permissões na lista
      if (authChild.StateIndex=1) and (authChild.Data<>nil) and (TObject(authChild.Data) is TIntegerList) and (TIntegerList(authChild.Data).Count>0) then begin
        for ic:=0 to  TIntegerList(authChild.Data).Count-1 do begin
          SetLength(vids,Length(vids)+1);
          vids[High(vids)]:=TIntegerList(authChild.Data).Items[ic];
          objsqlist:=objsqlist+separador+IntToStr(vids[High(vids)]);
          separador:=', ';
        end;
      end;

      //SE TEM FILHOS, VERIFICA AS PERMISSÕES DOS FILHOS TAMBÉM
      if authChild.Count>0 then begin
        VerifyChilds(authChild.GetFirstChild);
      end;

      authChild:=authChild.GetNextSibling;
    end;
  end;

procedure TfrmUserManagement.ChangeUserPermsExecute(Sender: TObject);
var
  f:TfrmUserPerms;
  i: Integer;
  r: Integer;
  sql: String;
  auth: TTreeNode;


begin
  if (userList.SelCount>0) and (userList.Selected<>nil) then begin
    dm.tblListPermsOfUser1.Close;
    dm.tblListPermsOfUser1.ParamByName('uid').Value:=dm.tblUsersid_user.Value;
    dm.tblListPermsOfUser1.Open;

    f:=TfrmUserPerms.Create(Self);
    try
      f.CheckListBox1.Items.Clear;
      for r:=1 to dm.tblListPermsOfUser1.RecordCount do begin
        dm.tblListPermsOfUser1.RecNo:=r;

        f.AddPath(dm.tblListPermsOfUser1.FieldByName('ds_object').AsString,
                     dm.tblListPermsOfUser1.FieldByName('id_object').AsInteger,
                     dm.tblListPermsOfUser1.FieldByName('AccessAllowed').AsBoolean);

      end;

      f.CheckListBox1.CustomSort(@SortPermissionList);
      f.Caption:='Alterando as permissões de "'+dm.tblUsersds_login.Value+'"';

      if f.ShowModal = mrOK then begin
        objsqlist:='';
        separador :='';
        SetLength(vids, 0);

        auth:=f.CheckListBox1.Items.GetFirstNode;
        while auth<>nil do begin

          //se o item esta marcado, tem uma lista e tem alguma permissão na lista, adiciona estas permissões na lista
          if (auth.StateIndex=1) and (auth.Data<>nil) and (TObject(auth.Data) is TIntegerList) and (TIntegerList(auth.Data).Count>0) then begin
            for i:=0 to  TIntegerList(auth.Data).Count-1 do begin
              SetLength(vids,Length(vids)+1);
              vids[High(vids)]:=TIntegerList(auth.Data).Items[i];
              objsqlist:=objsqlist+separador+IntToStr(vids[High(vids)]);
              separador:=', ';
            end;
          end;

          //SE TEM FILHOS, VERIFICA AS PERMISSÕES DOS FILHOS TAMBÉM
          if auth.Count>0 then begin
            VerifyChilds(auth.GetFirstChild);
          end;

          auth:=auth.GetNextSibling;
        end;

        sql:='DELETE FROM tbl_permission '+
             'WHERE (cd_user='+IntToStr(dm.tblUsersid_user.Value)+')';

        if Length(vids)>0 then
          sql:=sql + ' AND (NOT (cd_object IN ('+objsqlist+')))';

        dm.PGConnUsers.ExecuteDirect(sql);

        if dm.tblPermission.Active then
          dm.tblPermission.Close;

        dm.tblPermission.Open;

        for i:=0 to High(vids) do begin
          if not dm.tblPermission.Locate('cd_user;cd_object',VarArrayOf([dm.tblUsersid_user.Value,vids[i]]),[]) then begin
            if dm.tblPermission.State in dsEditModes then
              dm.tblPermission.Cancel;
            dm.tblPermission.Append;
            dm.tblPermission.FieldByName('cd_user').AsInteger:=dm.tblUsersid_user.Value;
            dm.tblPermission.FieldByName('cd_object').AsInteger:=vids[i];
            dm.tblPermission.Post;
          end;
        end;

        if dm.tblUsersid_user.Value=dm.CustomizedUserManagement1.UID then
          MessageDlg('As novas permissões só terão efeito após você efetuar Login novamente!',mtInformation,[mbok],0);
      end;
    finally
      FreeAndNil(f);
    end;
  end else
    raise Exception.Create('Para editar as permissões de acesso, selecione um usuário primeiro!');
end;

procedure TfrmUserManagement.EditUserExecute(Sender: TObject);
var
  frm: TfrmInsertEditUser;
begin
  if (userList.SelCount>0) and (userList.Selected<>nil) then begin
    if (not (dm.tblUsers.State in dsEditModes)) and (dm.tblUsers.RecNo>0) then
      dm.tblUsers.Edit;

    frm:=TfrmInsertEditUser.Create(Self);
    frm.Caption:='Editar usuário';
    try
      frm.DBEdit1.ReadOnly:=True;

      if (dm.tblUsersid_user.Value=1) then begin
        frm.DBEdit2.ReadOnly:=true;
        frm.DBCheckBox1.ReadOnly:=true;
        //frm.BitBtn1.Enabled:=false;
      end;

      if frm.ShowModal=mrOK then begin
        if frm.edtSenha1.Modified or frm.edtSenha2.Modified then begin
          if frm.edtSenha1.Text=frm.edtSenha2.Text then begin
            dm.tblUsersds_password.Value:=MD5Print(MD5String(frm.edtSenha1.Text));
          end else begin
            dm.tblUsers.Cancel;
            raise exception.Create('A senha é diferente da contra senha. Digite a mesma senha no campo "Senha" e "Confirme senha"!');
          end;
        end;
        dm.tblUsers.Post;
        RefreshUserList;
      end else
        dm.tblUsers.Cancel;
    finally
      FreeAndNil(frm);
    end;
  end else
    raise Exception.Create('Para editar, selecione um usuário primeiro!');
end;

procedure TfrmUserManagement.groupList1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  r: Integer;
  i: TListItem;
begin
  if Selected and (Item<>nil) and (item.Data<>nil) then begin
    if not dm.tblGroups.Locate('id_group',PtrInt(item.Data),[]) then begin
      RefreshGroupList;
      exit;
    end;

    AvailableUserList.Items.Clear;
    userMemberList.Items.Clear;

    dm.tblListMembersOfGroup.Close;
    dm.tblListMembersOfGroup.ParamByName('gid').AsInteger:=PtrInt(item.Data);
    dm.tblListMembersOfGroup.Open;

    for r:=1 to dm.tblListMembersOfGroup.RecordCount do begin
      dm.tblListMembersOfGroup.RecNo:=r;
      if dm.tblListMembersOfGroupmember.Value then begin
        i:=userMemberList.items.Add;
      end else begin
        i:=AvailableUserList.Items.Add;
      end;
      i.Caption:=dm.tblListMembersOfGroupds_login.Value;
      i.Data:=Pointer(PtrInt(dm.tblListMembersOfGroupid_user.Value));
      i.ImageIndex:=0;
    end;
  end;
end;

procedure TfrmUserManagement.groupListContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=groupList.SelCount<1;
end;

procedure TfrmUserManagement.groupListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and (Item<>nil) and (item.Data<>nil) then begin
    if not dm.tblGroups.Locate('id_group',PtrInt(item.Data),[]) then
      RefreshGroupList;
  end;
end;

procedure TfrmUserManagement.RemoveUserExecute(Sender: TObject);
begin
  if (userList.SelCount>0) and (userList.Selected<>nil) then begin
    if dm.tblUsersid_user.Value<>1 then begin
      if MessageDlg('Deseja mesmo remover o usuário "'+dm.tblUsersds_fullname.Value+'" do sistema?',mtConfirmation,mbYesNo,0)=mrYes then begin
        dm.PGConnUsers.ExecuteDirect('DELETE FROM public.tbl_permission WHERE cd_user = '+dm.tblUsersid_user.AsString);
        dm.PGConnUsers.ExecuteDirect('DELETE FROM public.tbl_group_member WHERE cd_user = '+dm.tblUsersid_user.AsString);
        userList.Selected.Delete;
        dm.tblUsers.Delete;
      end;
    end else
      raise Exception.Create('Você não pode apagar o usuário administrador!');
  end else
    raise Exception.Create('Para apagar, selecione um usuário primeiro!');
end;

procedure TfrmUserManagement.secureAddGroupExecute(Sender: TObject);
var
  frm: TfrmAddGroup;
begin
  if (dm.tblGroups.State in dsEditModes) then
    dm.tblGroups.Cancel;

  dm.tblGroups.Append;

  frm:=TfrmAddGroup.Create(Self);
  frm.Caption:='Adicionar novo grupo de permissões';
  try
    if frm.ShowModal=mrOK then begin
      if not ValidLogin(dm.tblGroupsds_groupname.Value) then begin
        dm.tblGroups.Cancel;
        raise exception.Create('O nome do grupo é inválido! O nome do grupo só pode conter letras e números e deve obrigatoriamente iniciar com uma letra!');
      end;
      dm.tblGroups.Post;
    end else
      dm.tblGroups.Cancel;
  finally
    FreeAndNil(frm);
  end;
  RefreshGroupList;
end;

procedure TfrmUserManagement.secureAddUserToGroupExecute(Sender: TObject);
begin
  if (groupList1.Selected<>NIL) and (AvailableUserList.Selected<>nil) then begin
    AddUserToGroup(PtrInt(AvailableUserList.Selected.Data), PtrInt(groupList1.Selected.Data));
  end;
end;

procedure TfrmUserManagement.ChipCardReaderProc(Data:PtrInt);
begin
  if assigned(dm.CustomizedUserManagement1.ChipCardReader) and Assigned(frmChipCardRead) then begin
    if dm.CustomizedUserManagement1.ChipCardReader.ChipCardRead(frmChipCardRead.ChipCard) then begin
      frmChipCardRead.ModalResult:=mrOK;
    end else
      StartDelayedChipCardRead;
  end;
end;

procedure TfrmUserManagement.FrmChipCardCanBeClosed(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=false;
  if Sender is TChipCardRead then
    CanClose:=((TChipCardRead(Sender).ModalResult=mrOK) and (TChipCardRead(Sender).ChipCard<>'')) or
               (TChipCardRead(Sender).ModalResult=mrCancel);
end;

procedure TfrmUserManagement.StartDelayedChipCardRead;
begin
  if assigned(dm.CustomizedUserManagement1.ChipCardReader) and ((Application.Flags*[AppDoNotCallAsyncQueue])=[]) then begin
    Application.QueueAsyncCall(@ChipCardReaderProc,0);
  end;
end;

procedure TfrmUserManagement.secureAssignChipCardToUserExecute(Sender: TObject);

  procedure WaitForEmptyChipCard;
  var
    frm: TForm;
    lbl: TLabel;
    tmr: TTimer;
  begin
    if Assigned(dm.CustomizedUserManagement1.ChipCardReader) then begin
      frm:=TForm.CreateNew(Self);
      try
        frm.SetBounds(0,0,320,240);
        frm.Position:=poScreenCenter;
        frm.BorderStyle:=bsNone;
        lbl:=TLabel.Create(frm);
        lbl.Align:=alClient;
        lbl.WordWrap:=true;
        lbl.Alignment:=taCenter;
        lbl.Layout:=tlCenter;
        lbl.font.Style:=[fsBold];
        lbl.font.Height:=16;
        lbl.Caption:='Remova o microchip do leitor';
        lbl.Parent:=frm;
        tmr:=TTimer.Create(frm);
        tmr.Interval:=100;
        tmr.OnTimer:=@CheckIfCardHasBeenRemoved;
        frm.OnCloseQuery:=@WarningCanBeClosed;
        repeat
          if not dm.CustomizedUserManagement1.ChipCardReader.IsEmptyChipCard then
            frm.ShowModal;
        until dm.CustomizedUserManagement1.ChipCardReader.IsEmptyChipCard;
      finally
        FreeAndNil(frm);
      end;
    end;
  end;

  procedure StopDelayedChipCardRead;
  begin
    Application.RemoveAsyncCalls(Self);
  end;
var
  res: Integer;
begin
  if Not Assigned(dm.CustomizedUserManagement1.ChipCardReader) then begin
    MessageDlg('A funcionalidade chipcard não está instalada neste sistema!',mtError,[mbOK],0);
    exit;
  end;

  try
    if dm.CustomizedUserManagement1.ChipCardReader.ChipCardReady then
      dm.CustomizedUserManagement1.ChipCardReader.InitializeChipCard;
    if dm.CustomizedUserManagement1.ChipCardReader.ChipCardReady then begin
      WaitForEmptyChipCard;
      StartDelayedChipCardRead;
    end;

    frmChipCardRead:=TChipCardRead.CreateNew(Self);
    try
      frmChipCardRead.SetBounds(0,0,320,240);
      frmChipCardRead.Position:=poScreenCenter;
      frmChipCardRead.BorderStyle:=bsNone;
      frmChipCardRead.ChipCard:='';
      with TLabel.Create(frmChipCardRead) do begin
       Align:=alClient;
       WordWrap:=true;
       Alignment:=taCenter;
       Layout:=tlCenter;
       font.Style:=[fsBold];
       font.Height:=16;
       Caption:='Aproxime o microchip do leitor para associá-lo com o usuário "'+dm.tblUsersds_fullname.Value+'"';
       Parent:=frmChipCardRead;
      end;

      with TBitBtn.Create(frmChipCardRead) do begin
        Align:=alBottom;
        Kind:=bkCancel;
        Parent:=frmChipCardRead;
      end;

      frmChipCardRead.OnCloseQuery:=@FrmChipCardCanBeClosed;
      repeat
        frmChipCardRead.ModalResult:=mrNone;
        res:=frmChipCardRead.ShowModal
      until res in [mrOK, mrCancel];

      case res of
        mrCancel: MessageDlg('Procedimento cancelado pelo usuário!',mtWarning,[mbOK],0);
        mrOK: begin
          try
            if not (dm.tblUsers.State in dsEditModes) then
              dm.tblUsers.Edit;

            dm.tblUsersds_automated_login_code.Value:=frmChipCardRead.ChipCard;
            dm.tblUsers.Post;
            MessageDlg('Operação concluída!','O microchip do usuário "'+dm.tblUsersds_fullname.Value+'" agora tem um microchip associado.',mtInformation,[mbOK],0);
          except
            on e:Exception do begin
              MessageDlg('Falha na operação','Não foi possível associar o microchip com o usuário "'+dm.tblUsersds_fullname.Value+'".'+LineEnding+'Verifique se o microchip já está associado a outro usuário.'+LineEnding+LineEnding+'Detalhes da falha:'+LineEnding+e.Message,mtError,[mbOK],0);
              if (dm.tblUsers.State in dsEditModes) then
                dm.tblUsers.Cancel;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(frmChipCardRead);
    end;

  finally
    StopDelayedChipCardRead;
    if dm.CustomizedUserManagement1.ChipCardReader.ChipCardReady then begin
      dm.CustomizedUserManagement1.ChipCardReader.FinishChipCard;
    end;
  end;
end;

procedure TfrmUserManagement.secureChangeGroupPermsExecute(Sender: TObject);
var
  f:TfrmUserPerms;
  i: Integer;
  r: Integer;
  sql: String;
  auth: TTreeNode;


begin
  if (groupList.SelCount>0) and (groupList.Selected<>nil) then begin
    dm.tblListPermsOfGroup.Close;
    dm.tblListPermsOfGroup.ParamByName('gid').Value:=dm.tblGroupsid_group.Value;
    dm.tblListPermsOfGroup.Open;

    f:=TfrmUserPerms.Create(Self);
    try
      f.CheckListBox1.Items.Clear;
      for r:=1 to dm.tblListPermsOfGroup.RecordCount do begin
        dm.tblListPermsOfGroup.RecNo:=r;

        f.AddPath(dm.tblListPermsOfGroup.FieldByName('ds_object').AsString,
                     dm.tblListPermsOfGroup.FieldByName('id_object').AsInteger,
                     dm.tblListPermsOfGroup.FieldByName('AccessAllowed').AsBoolean);

      end;

      f.CheckListBox1.CustomSort(@SortPermissionList);
      f.Caption:='Alterando as permissões de  grupo de permissões "'+dm.tblGroupsds_groupname.Value+'"';

      if f.ShowModal = mrOK then begin
        objsqlist:='';
        separador :='';
        SetLength(vids, 0);

        auth:=f.CheckListBox1.Items.GetFirstNode;
        while auth<>nil do begin

          //se o item esta marcado, tem uma lista e tem alguma permissão na lista, adiciona estas permissões na lista
          if (auth.StateIndex=1) and (auth.Data<>nil) and (TObject(auth.Data) is TIntegerList) and (TIntegerList(auth.Data).Count>0) then begin
            for i:=0 to  TIntegerList(auth.Data).Count-1 do begin
              SetLength(vids,Length(vids)+1);
              vids[High(vids)]:=TIntegerList(auth.Data).Items[i];
              objsqlist:=objsqlist+separador+IntToStr(vids[High(vids)]);
              separador:=', ';
            end;
          end;

          //SE TEM FILHOS, VERIFICA AS PERMISSÕES DOS FILHOS TAMBÉM
          if auth.Count>0 then begin
            VerifyChilds(auth.GetFirstChild);
          end;

          auth:=auth.GetNextSibling;
        end;

        sql:='DELETE FROM tbl_permission '+
             'WHERE (cd_group='+IntToStr(dm.tblGroupsid_group.Value)+')';

        if Length(vids)>0 then
          sql:=sql + ' AND (NOT (cd_object IN ('+objsqlist+')))';

        dm.PGConnUsers.ExecuteDirect(sql);

        if dm.tblPermission.Active then
          dm.tblPermission.Close;

        dm.tblPermission.Open;

        for i:=0 to High(vids) do begin
          if not dm.tblPermission.Locate('cd_group;cd_object',VarArrayOf([dm.tblGroupsid_group.Value,vids[i]]),[]) then begin
            if dm.tblPermission.State in dsEditModes then
              dm.tblPermission.Cancel;
            dm.tblPermission.Append;
            dm.tblPermission.FieldByName('cd_group').AsInteger:=dm.tblGroupsid_group.Value;
            dm.tblPermission.FieldByName('cd_object').AsInteger:=vids[i];
            dm.tblPermission.Post;
          end;
        end;
      end;
    finally
      FreeAndNil(f);
    end;
  end else
    raise Exception.Create('Para editar as permissões de acesso, selecione um grupo de permissões primeiro!');
end;

procedure TfrmUserManagement.secureRemoveChipCardFromUserExecute(Sender: TObject
  );
begin
  if (userList.SelCount>0) and (userList.Selected<>nil) then begin
    if MessageDlg('Deseja remover o microchip associado usuário "'+dm.tblUsersds_fullname.Value+'"?',mtConfirmation,mbYesNo,0)=mrYes then begin
      try
        if not (dm.tblUsers.State in dsEditModes) then
          dm.tblUsers.Edit;

        dm.tblUsersds_automated_login_code.Clear;
        dm.tblUsers.Post;
        MessageDlg('Operação concluída!','O microchip do usuário "'+dm.tblUsersds_fullname.Value+'" foi removido.',mtInformation,[mbOK],0);
      except
        on e:Exception do begin
          MessageDlg('Falha na operação','Não foi possível remover microchip do usuário "'+dm.tblUsersds_fullname.Value+'". Detalhes da falha:'+LineEnding+e.Message,mtError,[mbOK],0);
          if (dm.tblUsers.State in dsEditModes) then
            dm.tblUsers.Cancel;
        end;
      end;
    end;
  end else
    raise Exception.Create('Para remover um microchip associado, selecione um usuário primeiro!');
end;

procedure TfrmUserManagement.secureRemoveGroupExecute(Sender: TObject);
begin
  if (groupList.SelCount>0) and (groupList.Selected<>nil) then begin
    if MessageDlg('Deseja mesmo remover o grupo de permissões "'+dm.tblGroupsds_groupname.Value+'" do sistema?',mtConfirmation,mbYesNo,0)=mrYes then begin
      groupList.Selected.Delete;
      dm.PGConnUsers.ExecuteDirect('DELETE FROM public.tbl_permission WHERE cd_group = '+dm.tblGroupsid_group.AsString);
      dm.PGConnUsers.ExecuteDirect('DELETE FROM public.tbl_group_member WHERE cd_group = '+dm.tblGroupsid_group.AsString);
      dm.tblGroups.Delete;
    end;
  end else
    raise Exception.Create('Para apagar, selecione um grupo de permissões primeiro!');
end;

procedure TfrmUserManagement.secureRemoveGroupMemberExecute(Sender: TObject);
begin
  if (groupList1.Selected<>NIL) and (userMemberList.Selected<>nil) then begin
    RemoveUserFromGroup(PtrInt(userMemberList.Selected.Data), PtrInt(groupList1.Selected.Data));
  end;
end;

procedure TfrmUserManagement.userMemberListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i: TListItem;
begin
  //estou arrastando um usuário disponível para cá, logo ele será um novo membro
  if source=AvailableUserList then begin
    if AvailableUserList.Selected<>nil then begin
      i:=userMemberList.Items.Add;
      i.Assign(AvailableUserList.Selected);
      AvailableUserList.Selected.Destroy;
    end;
  end;
end;

procedure TfrmUserManagement.userMemberListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source=AvailableUserList);
end;

procedure TfrmUserManagement.AvailableUserListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source=userMemberList);
end;

constructor TfrmUserManagement.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  RefreshUserList;
  RefreshGroupList;
  PageControl1.ActivePage:=TabSheet1;
end;

destructor TfrmUserManagement.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

function TfrmUserManagement.SortPermissionList(Node1, Node2: TTreeNode
  ): integer;
begin
  if node1.Text=Node2.Text then
    Result:=0
  else begin
    if Node1.Text<Node2.Text then
      Result:=-1
    else
      Result:=1;
  end;
end;

procedure TfrmUserManagement.AddUserToGroup(const uid, gid: Integer);
begin
  try
    dm.PGConnUsers.ExecuteDirect('INSERT INTO public.tbl_group_member (cd_user, cd_group) VALUES('+uid.ToString+', '+gid.ToString+');');
  except
  end;
  groupList1SelectItem(groupList1,groupList1.Selected,True);
end;

procedure TfrmUserManagement.RemoveUserFromGroup(const uid, gid: Integer);
begin
  dm.PGConnUsers.ExecuteDirect('DELETE FROM public.tbl_group_member WHERE cd_user='+uid.ToString+' AND cd_group='+gid.ToString+';');
  groupList1SelectItem(groupList1,groupList1.Selected,True);
end;

end.

