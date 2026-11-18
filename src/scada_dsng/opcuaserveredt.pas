{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Editor de projeto do TOPCUAServer: adiciona tags do form a colecao Items.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Duplo clique no servidor (ou "Add tags..." no menu dele) lista os tags do
  form que ainda nao estao em Items; os marcados viram itens, com a opcao de
  uma pasta por driver e de deixa-los gravaveis.
}
{$ELSE}
{:
  @abstract(Design time editor of TOPCUAServer: adds tags of the form to the Items collection.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A double click on the server (or "Add tags..." in its menu) lists the tags of
  the form that are not in Items yet; the ticked ones become items, optionally
  with a folder per driver and writable.
}
{$ENDIF}
unit opcuaserveredt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, CheckLst, ButtonPanel,
  ComponentEditors, PLCTag, opcuaserver;

type

  { TOPCUAServerComponentEditor }

  TOPCUAServerComponentEditor = class(TComponentEditor)
  private
    procedure AddTags;
  public
    procedure ExecuteVerb(Index:LongInt); override;
    function  GetVerb(Index:LongInt):AnsiString; override;
    function  GetVerbCount:LongInt; override;
    procedure Edit; override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Os tags de Root que ainda nao estao em Server.Items (em ordem de criacao).
  Separado do dialogo para poder ser testado.
  }
  {$ELSE}
  {:
  The tags of Root that are not in Server.Items yet (in creation order).
  Kept apart from the dialog so it can be tested.
  }
  {$ENDIF}
  function OPCUAServerCandidateTags(Server:TOPCUAServer; Root:TComponent):TFPList;

  {$IFDEF PORTUGUES}
  {:
  Adiciona os tags a Server.Items. ByDriver poe cada um numa pasta com o nome
  do seu driver ('Memory' sem driver). Devolve quantos foram adicionados.
  }
  {$ELSE}
  {:
  Adds the tags to Server.Items. ByDriver puts each one in a folder named after
  its driver ('Memory' without a driver). Returns how many were added.
  }
  {$ENDIF}
  function OPCUAServerAddTags(Server:TOPCUAServer; Tags:TFPList; ByDriver, Writable:Boolean):Integer;

implementation

uses
  ProtocolDriver;

type
  TPLCTagAccess = class(TPLCTag);

function OPCUAServerCandidateTags(Server:TOPCUAServer; Root:TComponent):TFPList;
var
  i:Integer;
  C:TComponent;
begin
  Result := TFPList.Create;
  if Root=nil then exit;
  for i:=0 to Root.ComponentCount-1 do begin
    C := Root.Components[i];
    if (C is TPLCTag) and (Server.Items.FindByTag(TPLCTag(C))=nil) then
      Result.Add(C);
  end;
end;

function OPCUAServerAddTags(Server:TOPCUAServer; Tags:TFPList; ByDriver, Writable:Boolean):Integer;
var
  i:Integer;
  T:TPLCTag;
  it:TOPCUAServerItem;
  drv:TProtocolDriver;
begin
  Result := 0;
  Server.Items.BeginUpdate;
  try
    for i:=0 to Tags.Count-1 do begin
      T := TPLCTag(Tags[i]);
      if Server.Items.FindByTag(T)<>nil then continue;
      it := Server.Items.Add;
      it.Tag := T;
      it.Writable := Writable;
      if ByDriver then begin
        drv := TPLCTagAccess(T).ProtocolDriver;
        if drv<>nil then it.Folder := drv.Name else it.Folder := 'Memory';
      end;
      Inc(Result);
    end;
  finally
    Server.Items.EndUpdate;
  end;
end;

{ TOPCUAServerAddTagsForm }

type
  TOPCUAServerAddTagsForm = class(TForm)
  public
    List:TCheckListBox;
    ByDriver, Writable:TCheckBox;
    constructor CreateFor(Tags:TFPList; const ServerName:String);
    procedure AllClick(Sender:TObject);
    procedure NoneClick(Sender:TObject);
  end;

constructor TOPCUAServerAddTagsForm.CreateFor(Tags:TFPList; const ServerName:String);
var
  head, row:TPanel;
  all, none:TButton;
  buttons:TButtonPanel;
  i:Integer;
  T:TPLCTag;
begin
  inherited CreateNew(nil);
  Caption := 'Add tags to '+ServerName;
  Position := poScreenCenter;
  Width := 520;
  Height := 480;

  head := TPanel.Create(Self);
  head.Parent := Self;
  head.Align := alTop;
  head.AutoSize := true;
  head.BevelOuter := bvNone;
  head.BorderSpacing.Around := 6;

  row := TPanel.Create(Self);
  row.Parent := head;
  row.Align := alTop;
  row.AutoSize := true;
  row.BevelOuter := bvNone;
  all := TButton.Create(Self);
  all.Parent := row;
  all.Caption := 'All';
  all.AutoSize := true;
  all.Align := alLeft;
  all.OnClick := @AllClick;
  none := TButton.Create(Self);
  none.Parent := row;
  none.Caption := 'None';
  none.AutoSize := true;
  none.Align := alLeft;
  none.Left := 200;
  none.OnClick := @NoneClick;

  Writable := TCheckBox.Create(Self);
  Writable.Parent := head;
  Writable.Caption := 'Writable by the OPC UA clients (the writes reach the devices)';
  Writable.Align := alTop;

  ByDriver := TCheckBox.Create(Self);
  ByDriver.Parent := head;
  ByDriver.Caption := 'A folder for each driver (tags without a driver go in "Memory")';
  ByDriver.Checked := true;
  ByDriver.Align := alTop;

  List := TCheckListBox.Create(Self);
  List.Parent := Self;
  List.Align := alClient;
  List.BorderSpacing.Around := 6;
  for i:=0 to Tags.Count-1 do begin
    T := TPLCTag(Tags[i]);
    List.Items.AddObject(T.Name+'  ('+T.ClassName+')', T);
    List.Checked[i] := true;
  end;

  buttons := TButtonPanel.Create(Self);
  buttons.Parent := Self;
  buttons.ShowButtons := [pbOK, pbCancel];
end;

procedure TOPCUAServerAddTagsForm.AllClick(Sender:TObject);
begin
  List.CheckAll(cbChecked);
end;

procedure TOPCUAServerAddTagsForm.NoneClick(Sender:TObject);
begin
  List.CheckAll(cbUnchecked);
end;

{ TOPCUAServerComponentEditor }

procedure TOPCUAServerComponentEditor.AddTags;
var
  srv:TOPCUAServer;
  frm:TOPCUAServerAddTagsForm;
  tags, chosen:TFPList;
  i:Integer;
begin
  srv := Component as TOPCUAServer;
  tags := OPCUAServerCandidateTags(srv, srv.Owner);
  chosen := TFPList.Create;
  frm := TOPCUAServerAddTagsForm.CreateFor(tags, srv.Name);
  try
    if frm.ShowModal<>mrOK then exit;
    for i:=0 to frm.List.Count-1 do
      if frm.List.Checked[i] then chosen.Add(frm.List.Items.Objects[i]);
    if OPCUAServerAddTags(srv, chosen, frm.ByDriver.Checked, frm.Writable.Checked)>0 then
      Modified;
  finally
    frm.Free;
    tags.Free;
    chosen.Free;
  end;
end;

procedure TOPCUAServerComponentEditor.ExecuteVerb(Index:LongInt);
begin
  if Index=0 then AddTags;
end;

function TOPCUAServerComponentEditor.GetVerb(Index:LongInt):AnsiString;
begin
  if Index=0 then Result := 'Add tags...' else Result := '';
end;

function TOPCUAServerComponentEditor.GetVerbCount:LongInt;
begin
  Result := 1;
end;

procedure TOPCUAServerComponentEditor.Edit;
begin
  AddTags;
end;

end.
