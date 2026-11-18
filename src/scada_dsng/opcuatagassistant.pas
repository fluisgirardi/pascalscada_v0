{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Tag Builder do driver OPC UA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Navega no espaço de endereços do servidor (a pasta Objects, um nível de cada vez) e
  cria um tag para cada variável escolhida: TPLCTagNumber para escalares numéricos,
  TPLCBlock para arrays numéricos e TPLCString para textos. Tem também os botões para
  confiar no certificado do servidor (salvo em TrustListDir) e para exportar o
  certificado do cliente (que o servidor precisa aceitar quando há segurança).
}
{$ELSE}
{:
  @abstract(Tag Builder of the OPC UA driver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Browses the address space of the server (the Objects folder, one level at a time) and
  creates a tag for every variable picked: TPLCTagNumber for numeric scalars, TPLCBlock
  for numeric arrays and TPLCString for texts. It also has the buttons to trust the
  certificate of the server (saved in TrustListDir) and to export the client certificate
  (which the server has to accept when security is used).
}
{$ENDIF}
unit opcuatagassistant;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Spin, Dialogs,
  Tag, ProtocolTypes, ProtocolDriver, PLCTagNumber, PLCBlock, PLCString,
  OPCUAClient, OPCUADriver, OPCUAValues;

type
  TOPCUANodeData = class
    Item:TOPCUABrowseItem;
    Loaded:Boolean;
  end;

  { TfrmOPCUATagBuilder }

  TfrmOPCUATagBuilder = class(TForm)
  private
    FDriver:TOPCUADriver;
    FTree:TTreeView;
    FScan:TSpinEdit;
    FPrefix:TEdit;
    FUseUri:TCheckBox;
    FStatus:TLabel;
    procedure Expanding(Sender:TObject; Node:TTreeNode; var AllowExpansion:Boolean);
    procedure DeletionNode(Sender:TObject; Node:TTreeNode);
    procedure RefreshClick(Sender:TObject);
    procedure TrustServerClick(Sender:TObject);
    procedure ExportClientClick(Sender:TObject);
    procedure LoadChildren(AParent:TTreeNode);
  public
    constructor CreateFor(ADriver:TOPCUADriver);
    procedure Selected(List:TList);
    property Scan:TSpinEdit read FScan;
    property Prefix:TEdit read FPrefix;
    property UseUri:TCheckBox read FUseUri;
  end;

{ TfrmOPCUATagBuilder }

constructor TfrmOPCUATagBuilder.CreateFor(ADriver:TOPCUADriver);
var
  pTop, pBottom:TPanel;
  b:TButton;
  l:TLabel;
begin
  CreateNew(nil);
  FDriver := ADriver;
  Caption := 'OPC UA Tag Builder - '+ADriver.EndpointUrl;
  Width := 640;
  Height := 560;
  Position := poScreenCenter;

  pTop := TPanel.Create(Self);
  pTop.Parent := Self;
  pTop.Align := alTop;
  pTop.Height := 72;
  pTop.BevelOuter := bvNone;

  b := TButton.Create(Self);
  b.Parent := pTop;
  b.SetBounds(8, 8, 110, 28);
  b.Caption := 'Browse';
  b.OnClick := @RefreshClick;

  b := TButton.Create(Self);
  b.Parent := pTop;
  b.SetBounds(124, 8, 190, 28);
  b.Caption := 'Trust server certificate';
  b.Hint := 'Saves the certificate of the server in TrustListDir';
  b.ShowHint := true;
  b.OnClick := @TrustServerClick;

  b := TButton.Create(Self);
  b.Parent := pTop;
  b.SetBounds(320, 8, 190, 28);
  b.Caption := 'Export client certificate';
  b.Hint := 'Saves the client certificate, to be trusted by the server';
  b.ShowHint := true;
  b.OnClick := @ExportClientClick;

  l := TLabel.Create(Self);
  l.Parent := pTop;
  l.SetBounds(8, 46, 40, 20);
  l.Caption := 'Scan:';
  FScan := TSpinEdit.Create(Self);
  FScan.Parent := pTop;
  FScan.SetBounds(50, 42, 80, 26);
  FScan.MinValue := 0;
  FScan.MaxValue := 3600000;
  FScan.Value := 1000;

  l := TLabel.Create(Self);
  l.Parent := pTop;
  l.SetBounds(140, 46, 50, 20);
  l.Caption := 'Prefix:';
  FPrefix := TEdit.Create(Self);
  FPrefix.Parent := pTop;
  FPrefix.SetBounds(192, 42, 120, 26);
  FPrefix.Text := 'ua_';

  FUseUri := TCheckBox.Create(Self);
  FUseUri.Parent := pTop;
  FUseUri.SetBounds(324, 44, 300, 24);
  FUseUri.Caption := 'Address with the namespace URI (nsu=)';
  FUseUri.Checked := true;

  pBottom := TPanel.Create(Self);
  pBottom.Parent := Self;
  pBottom.Align := alBottom;
  pBottom.Height := 44;
  pBottom.BevelOuter := bvNone;

  FStatus := TLabel.Create(Self);
  FStatus.Parent := pBottom;
  FStatus.SetBounds(8, 14, 380, 20);
  FStatus.Caption := 'Select variables (Ctrl/Shift) and press OK.';

  b := TButton.Create(Self);
  b.Parent := pBottom;
  b.SetBounds(440, 8, 90, 28);
  b.Caption := 'OK';
  b.ModalResult := mrOK;
  b.Default := true;

  b := TButton.Create(Self);
  b.Parent := pBottom;
  b.SetBounds(538, 8, 90, 28);
  b.Caption := 'Cancel';
  b.ModalResult := mrCancel;
  b.Cancel := true;

  FTree := TTreeView.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.ReadOnly := true;
  FTree.MultiSelect := true;
  FTree.MultiSelectStyle := [msControlSelect, msShiftSelect, msVisibleOnly];
  FTree.OnExpanding := @Expanding;
  FTree.OnDeletion := @DeletionNode;
end;

procedure TfrmOPCUATagBuilder.DeletionNode(Sender:TObject; Node:TTreeNode);
begin
  TObject(Node.Data).Free;
  Node.Data := nil;
end;

procedure TfrmOPCUATagBuilder.LoadChildren(AParent:TTreeNode);
var
  items:TOPCUABrowseItemArray;
  r:TProtocolIOResult;
  i:Integer;
  d:TOPCUANodeData;
  n:TTreeNode;
  id:AnsiString;
begin
  if AParent=nil then
    id := ''
  else begin
    d := TOPCUANodeData(AParent.Data);
    if d.Loaded then exit;
    d.Loaded := true;
    id := d.Item.NodeId;
    AParent.DeleteChildren;
  end;
  Screen.Cursor := crHourGlass;
  try
    r := FDriver.Browse(id, items);
  finally
    Screen.Cursor := crDefault;
  end;
  if r<>ioOk then begin
    FStatus.Caption := 'Browse failed: '+r.ToString+' '+FDriver.LastError;
    exit;
  end;
  for i:=0 to High(items) do begin
    if not (items[i].NodeClass in [oncObject, oncVariable]) then continue;
    if (AParent=nil) and (items[i].NodeIdNs='i=2253') then continue; //Server object: diagnostics
    d := TOPCUANodeData.Create;
    d.Item := items[i];
    n := FTree.Items.AddChildObject(AParent, items[i].DisplayName, d);
    if items[i].NodeClass=oncVariable then
      n.Text := items[i].DisplayName+'  ['+items[i].NodeIdNs+']';
    //a dummy child, so that it can be expanded (loaded on Expanding)
    FTree.Items.AddChild(n, '...');
  end;
  FStatus.Caption := IntToStr(Length(items))+' nodes.';
end;

procedure TfrmOPCUATagBuilder.Expanding(Sender:TObject; Node:TTreeNode; var AllowExpansion:Boolean);
begin
  if (Node.Data<>nil) and not TOPCUANodeData(Node.Data).Loaded then
    LoadChildren(Node);
  AllowExpansion := Node.HasChildren;
end;

procedure TfrmOPCUATagBuilder.RefreshClick(Sender:TObject);
begin
  FTree.Items.Clear;
  LoadChildren(nil);
end;

procedure TfrmOPCUATagBuilder.TrustServerClick(Sender:TObject);
var
  certs:TStringList;
  r:TProtocolIOResult;
  i:Integer;
  fs:TFileStream;
  dir, fn:String;
begin
  dir := FDriver.TrustListDir;
  if dir='' then begin
    ShowMessage('Set TrustListDir first: with it empty the driver accepts any server.');
    exit;
  end;
  r := FDriver.GetServerCertificates(certs);
  try
    if (r<>ioOk) or (certs.Count=0) then begin
      ShowMessage('Could not get the server certificate: '+r.ToString);
      exit;
    end;
    ForceDirectories(dir);
    for i:=0 to certs.Count-1 do begin
      fn := IncludeTrailingPathDelimiter(dir)+'server'+IntToStr(i)+'-'+FormatDateTime('yyyymmddhhnnss', Now)+'.der';
      fs := TFileStream.Create(fn, fmCreate);
      try
        fs.WriteBuffer(certs[i][1], Length(certs[i]));
      finally
        fs.Free;
      end;
    end;
    ShowMessage(IntToStr(certs.Count)+' certificate(s) saved in '+dir);
  finally
    certs.Free;
  end;
end;

procedure TfrmOPCUATagBuilder.ExportClientClick(Sender:TObject);
var
  cert:TBytes;
  dlg:TSaveDialog;
  fs:TFileStream;
begin
  if not FDriver.GetClientCertificate(cert) then begin
    ShowMessage('No client certificate: '+FDriver.LastError);
    exit;
  end;
  dlg := TSaveDialog.Create(nil);
  try
    dlg.FileName := 'pascalscada-client.der';
    dlg.Filter := 'DER certificate|*.der|All files|*';
    if not dlg.Execute then exit;
    fs := TFileStream.Create(dlg.FileName, fmCreate);
    try
      if Length(cert)>0 then fs.WriteBuffer(cert[0], Length(cert));
    finally
      fs.Free;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmOPCUATagBuilder.Selected(List:TList);
var
  i:Integer;
  n:TTreeNode;
begin
  for i:=0 to FTree.SelectionCount-1 do begin
    n := FTree.Selections[i];
    if (n.Data<>nil) and (TOPCUANodeData(n.Data).Item.NodeClass=oncVariable) then
      List.Add(n.Data);
  end;
end;

function UniqueComponentName(aOwner:TComponent; const aBaseName:String):String;
var
  base:String;
  i, c:Integer;
begin
  base := '';
  for i:=1 to Length(aBaseName) do
    if aBaseName[i] in ['A'..'Z','a'..'z','0'..'9','_'] then
      base := base+aBaseName[i]
    else
      base := base+'_';
  if (base='') or (base[1] in ['0'..'9']) then base := 'tag_'+base;
  Result := base;
  if aOwner=nil then exit;
  c := 1;
  while aOwner.FindComponent(Result)<>nil do begin
    Inc(c);
    Result := base+IntToStr(c);
  end;
end;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
                        CreateProc:TCreateTagProc);
var
  frm:TfrmOPCUATagBuilder;
  list:TList;
  i, count, k:Integer;
  d:TOPCUANodeData;
  info:TOPCUAVariableInfo;
  addr, name:String;
  aOwner:TComponent;
  num:TPLCTagNumber;
  blk:TPLCBlock;
  str:TPLCString;
  skipped:Integer;
begin
  if not (aProtocolDriver is TOPCUADriver) then
    raise Exception.Create('The OPC UA Tag Builder works only with TOPCUADriver.');
  frm := TfrmOPCUATagBuilder.CreateFor(TOPCUADriver(aProtocolDriver));
  list := TList.Create;
  try
    frm.RefreshClick(nil);
    if frm.ShowModal<>mrOK then exit;
    frm.Selected(list);
    skipped := 0;
    for i:=0 to list.Count-1 do begin
      d := TOPCUANodeData(list[i]);
      if frm.UseUri.Checked then addr := d.Item.NodeId else addr := d.Item.NodeIdNs;
      if TOPCUADriver(aProtocolDriver).GetVariableInfo(addr, info)<>ioOk then begin
        Inc(skipped);
        continue;
      end;
      name := frm.Prefix.Text+d.Item.BrowseName;
      if OPCUAIsTextType(info.BuiltinType) then begin
        str := TPLCString(CreateProc(TPLCString));
        aOwner := str.Owner;
        if aOwner=nil then aOwner := aOwnerOfNewTags;
        str.Name := UniqueComponentName(aOwner, name);
        str.LongAddress := addr;
        str.StringType := stC;
        str.StringSize := 80;
        str.RefreshTime := frm.Scan.Value;
        str.ProtocolDriver := TProtocolDriver(aProtocolDriver);
        InsertHook(str);
      end else if (info.BuiltinType<>nil) and OPCUAIsNumericType(info.BuiltinType) then begin
        if info.ValueRank>=0 then begin
          count := 1;
          for k:=0 to High(info.ArrayDimensions) do
            if info.ArrayDimensions[k]>0 then count := count*Integer(info.ArrayDimensions[k]);
          blk := TPLCBlock(CreateProc(TPLCBlock));
          aOwner := blk.Owner;
          if aOwner=nil then aOwner := aOwnerOfNewTags;
          blk.Name := UniqueComponentName(aOwner, name);
          blk.LongAddress := addr;
          blk.Size := count;
          blk.RefreshTime := frm.Scan.Value;
          blk.ProtocolDriver := TProtocolDriver(aProtocolDriver);
          InsertHook(blk);
        end else begin
          num := TPLCTagNumber(CreateProc(TPLCTagNumber));
          aOwner := num.Owner;
          if aOwner=nil then aOwner := aOwnerOfNewTags;
          num.Name := UniqueComponentName(aOwner, name);
          num.LongAddress := addr;
          num.RefreshTime := frm.Scan.Value;
          num.ProtocolDriver := TProtocolDriver(aProtocolDriver);
          InsertHook(num);
        end;
      end else
        Inc(skipped);
    end;
    if skipped>0 then
      ShowMessage(IntToStr(skipped)+' variable(s) skipped: structures and other types without a tag equivalent.');
  finally
    list.Free;
    frm.Free;
  end;
end;

initialization
  SetTagBuilderToolForOPCUAProtocol(@OpenTagEditor);

end.
