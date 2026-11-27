{$i ../common/language.inc}
{:
  @abstract(Wizard form for importing a Codesys symbol configuration XML into
  OPC UA tags.)

  The form is built entirely in code (no .lfm resource) so it can be compiled
  into the design-time package without resource file handling. It lists the
  variables found in the XML, lets the user check the ones to import, and
  edits the OPC UA namespace / symbol prefix and the default refresh time.
}
unit ucodesysxmlbuilder;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Classes, SysUtils, Math, Forms, Controls, StdCtrls, Grids, Dialogs,
  Spin, codesysxml;

type
  { TfrmCodesysXmlBuilder }
  TfrmCodesysXmlBuilder = class(TForm)
  private
    FVars: TCodesysXmlVars;
    FChecked: array of Boolean;
    FImportedKeys: TStringList;
    FUseStateFile: string;
    FNamespace: LongInt;
    FPathPrefix: string;
    FDefaultPeriod: LongInt;
    FNamePrefix: string;
    edtFile: TEdit;
    btnBrowse: TButton;
    btnLoad: TButton;
    grid: TStringGrid;
    edtNamespace: TSpinEdit;
    edtPathPrefix: TEdit;
    edtPeriod: TSpinEdit;
    edtNamePrefix: TEdit;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblInfo: TLabel;
    procedure BrowseClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure SelectNoneClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure RefreshGrid;
    function ImportedKey(const V: TCodesysXmlVar): string;
    function UseStateKey(const V: TCodesysXmlVar): string;
    procedure ApplyUseState;
    procedure SaveUseState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: Variables selected by the user. SelectedVars() fills in the effective
       namespace, path prefix and refresh time chosen in the wizard. }
    function SelectedVars: TCodesysXmlVars;
    {: Variables unchecked ("Use" = N) in the current XML. }
    function DeselectedVars: TCodesysXmlVars;
    procedure SetVars(const AVars: TCodesysXmlVars);
    {: NodeIds of tags already on the form, so imported rows can be marked. }
    procedure SetImportedKeys(const AKeys: TStrings);
    property Namespace: LongInt read FNamespace;
    property PathPrefix: string read FPathPrefix;
    property DefaultPeriod: LongInt read FDefaultPeriod;
    property NamePrefix: string read FNamePrefix;
  end;

var
  {: Last Codesys XML used in this IDE session; loaded automatically when the
     wizard is opened again. }
  CodesysLastXMLFile: string = '';
  {: Per-file "Use" checkbox state, keyed by XML path + variable identity.
     Kept at unit scope so it survives closing and reopening the wizard.
     MUST stay UNSORTED: Values[key] := value updates via Strings[i] := ...,
     which is illegal on a sorted TStringList. }
  CodesysUseState: TStringList;

const
  CodesysLastNamespace: LongInt = 4;
  CodesysLastPathPrefix: string = '|var|';
  CodesysLastPeriod: LongInt = 100;
  CodesysLastNamePrefix: string = '';

implementation

constructor TfrmCodesysXmlBuilder.Create(AOwner: TComponent);
begin
  // Built entirely in code: CreateNew skips .lfm resource loading, which
  // Lazarus otherwise enforces through RequireDerivedFormResource.
  inherited CreateNew(AOwner, 0);
  Caption := 'Codesys tag import - symbol protocol (TCP 11740)';
  Position := poScreenCenter;
  Width := 820;
  Height := 590;
  BorderStyle := bsDialog;

  FNamespace := CodesysLastNamespace;
  FPathPrefix := CodesysLastPathPrefix;
  FDefaultPeriod := CodesysLastPeriod;
  FNamePrefix := CodesysLastNamePrefix;
  SetLength(FVars,0);
  SetLength(FChecked,0);
  FImportedKeys := TStringList.Create;
  FImportedKeys.Sorted := true;
  FImportedKeys.Duplicates := dupIgnore;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 12; Top := 12; Width := 700; Height := 18;
    Caption := 'Select the Codesys symbol configuration XML (Symbolconfiguration.xsd):';
  end;

  edtFile := TEdit.Create(Self);
  with edtFile do begin
    Parent := Self; Left := 12; Top := 34; Width := 660; Height := 23;
  end;
  btnBrowse := TButton.Create(Self);
  with btnBrowse do begin
    Parent := Self; Left := 680; Top := 33; Width := 52; Height := 24;
    Caption := '...'; OnClick := @BrowseClick;
  end;
  btnLoad := TButton.Create(Self);
  with btnLoad do begin
    Parent := Self; Left := 738; Top := 33; Width := 62; Height := 24;
    Caption := 'Load'; OnClick := @LoadClick;
  end;

  grid := TStringGrid.Create(Self);
  with grid do begin
    Parent := Self;
    Left := 12; Top := 68; Width := 794; Height := 330;
    FixedCols := 0; FixedRows := 1; RowCount := 1; ColCount := 7;
    Cells[0,0] := 'Use';
    Cells[1,0] := 'Symbol path';
    Cells[2,0] := 'Type';
    Cells[3,0] := 'Access';
    Cells[4,0] := 'Address';
    Cells[5,0] := 'Symbol path';
    Cells[6,0] := 'Imported';
    ColWidths[0] := 40;
    ColWidths[1] := 246;
    ColWidths[2] := 80;
    ColWidths[3] := 76;
    ColWidths[4] := 70;
    ColWidths[5] := 210;
    ColWidths[6] := 70;
    Options := Options + [goRowSelect];
    OnClick := @GridClick;
  end;

  btnSelectAll := TButton.Create(Self);
  with btnSelectAll do begin
    Parent := Self; Left := 12; Top := 406; Width := 84; Height := 24;
    Caption := 'Select all'; OnClick := @SelectAllClick;
  end;
  btnSelectNone := TButton.Create(Self);
  with btnSelectNone do begin
    Parent := Self; Left := 104; Top := 406; Width := 84; Height := 24;
    Caption := 'Select none'; OnClick := @SelectNoneClick;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 12; Top := 442; Width := 80; Height := 18;
    Caption := 'Namespace (OPC UA only):';
  end;
  edtNamespace := TSpinEdit.Create(Self);
  with edtNamespace do begin
    Parent := Self; Left := 92; Top := 439; Width := 56; Height := 23;
    MinValue := 0; MaxValue := 65535; Value := FNamespace;
    Enabled := false;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 158; Top := 442; Width := 96; Height := 18;
    Caption := 'Prefix (OPC UA only):';
  end;
  edtPathPrefix := TEdit.Create(Self);
  with edtPathPrefix do begin
    Parent := Self; Left := 254; Top := 439; Width := 70; Height := 23;
    Text := FPathPrefix;
    Enabled := false;
    Hint := 'The symbol protocol uses the full path from the XML directly.';
    ShowHint := true;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 334; Top := 442; Width := 84; Height := 18;
    Caption := 'Period (ms):';
  end;
  edtPeriod := TSpinEdit.Create(Self);
  with edtPeriod do begin
    Parent := Self; Left := 418; Top := 439; Width := 76; Height := 23;
    MinValue := 10; MaxValue := 7200000; Value := FDefaultPeriod;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 504; Top := 442; Width := 90; Height := 18;
    Caption := 'Name prefix:';
  end;
  edtNamePrefix := TEdit.Create(Self);
  with edtNamePrefix do begin
    Parent := Self; Left := 594; Top := 439; Width := 100; Height := 23;
    Text := FNamePrefix;
    Hint := 'Optional prefix for generated component names.';
    ShowHint := true;
  end;

  btnOK := TButton.Create(Self);
  with btnOK do begin
    Parent := Self; Left := 636; Top := 474; Width := 82; Height := 26;
    Caption := 'OK'; ModalResult := mrNone; OnClick := @OKClick;
  end;
  btnCancel := TButton.Create(Self);
  with btnCancel do begin
    Parent := Self; Left := 726; Top := 474; Width := 80; Height := 26;
    Caption := 'Cancel'; ModalResult := mrCancel; OnClick := @CancelClick;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 12; Top := 476; Width := 610; Height := 30;
    Caption := 'Select variables, then click OK. PascalSCADA creates symbol-protocol ' +
      'tags (TPLCTagNumber / TPLCString) bound to the driver. ' +
      'Unselecting a previously imported variable removes its tag.';
    WordWrap := true;
  end;

  if (CodesysLastXMLFile <> '') and FileExists(CodesysLastXMLFile) then begin
    edtFile.Text := CodesysLastXMLFile;
    LoadClick(nil);
  end;
end;

destructor TfrmCodesysXmlBuilder.Destroy;
begin
  SaveUseState;
  if Assigned(edtNamespace) then CodesysLastNamespace := edtNamespace.Value;
  if Assigned(edtPathPrefix) then CodesysLastPathPrefix := edtPathPrefix.Text;
  if Assigned(edtPeriod) then CodesysLastPeriod := edtPeriod.Value;
  if Assigned(edtNamePrefix) then CodesysLastNamePrefix := edtNamePrefix.Text;
  FImportedKeys.Free;
  inherited Destroy;
end;

procedure TfrmCodesysXmlBuilder.BrowseClick(Sender: TObject);
var Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title := 'Open Codesys symbol configuration XML';
    Dlg.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
    Dlg.Options := [ofFileMustExist];
    if Dlg.Execute then edtFile.Text := Dlg.FileName;
  finally Dlg.Free; end;
end;

procedure TfrmCodesysXmlBuilder.LoadClick(Sender: TObject);
var Err: string; Vars: TCodesysXmlVars;
begin
  if Trim(edtFile.Text) = '' then begin
    MessageDlg('Select the XML file first.',mtInformation,[mbOK],0);
    exit;
  end;
  SaveUseState;
  if not ParseCodesysSymbols(Trim(edtFile.Text),Vars,Err) then begin
    MessageDlg(Err,mtError,[mbOK],0);
    exit;
  end;
  CodesysLastXMLFile := Trim(edtFile.Text);
  FUseStateFile := CodesysLastXMLFile;
  SetVars(Vars);
end;

procedure TfrmCodesysXmlBuilder.SetVars(const AVars: TCodesysXmlVars);
var i: LongInt;
begin
  FVars := AVars;
  SetLength(FChecked,Length(FVars));
  for i:=0 to High(FVars) do FChecked[i] := true;
  ApplyUseState;
  RefreshGrid;
end;

procedure TfrmCodesysXmlBuilder.RefreshGrid;
var i: LongInt; V: TCodesysXmlVar;
begin
  grid.RowCount := Max(2,Length(FVars)+1);
  for i:=0 to High(FVars) do begin
    V := FVars[i];
    if FChecked[i] then grid.Cells[0,i+1] := 'Y' else grid.Cells[0,i+1] := 'N';
    grid.Cells[1,i+1] := V.Name;
    grid.Cells[2,i+1] := V.VarType + '/' + V.IECName;
    grid.Cells[3,i+1] := V.Access;
    if V.DirectAddress <> '' then grid.Cells[4,i+1] := V.DirectAddress
    else grid.Cells[4,i+1] := '';
    grid.Cells[5,i+1] := V.Name;
    if FImportedKeys.IndexOf(ImportedKey(V)) >= 0 then grid.Cells[6,i+1] := 'YES'
    else grid.Cells[6,i+1] := '';
  end;
  if Length(FVars) = 0 then
    for i:=0 to 6 do grid.Cells[i,1] := '';
end;

function TfrmCodesysXmlBuilder.ImportedKey(const V: TCodesysXmlVar): string;
begin
  // The symbol protocol driver addresses a tag by its full symbol path.
  Result := LowerCase(V.Name);
end;

function TfrmCodesysXmlBuilder.UseStateKey(const V: TCodesysXmlVar): string;
begin
  Result := LowerCase(FUseStateFile) + '|' + V.Name + '|' + V.VarType + '|' +
    V.TypeClass;
end;

procedure TfrmCodesysXmlBuilder.ApplyUseState;
var i: LongInt; S: string;
begin
  if CodesysUseState = nil then exit;
  for i:=0 to High(FVars) do begin
    S := CodesysUseState.Values[UseStateKey(FVars[i])];
    if S = 'Y' then FChecked[i] := true
    else if S = 'N' then FChecked[i] := false;
  end;
end;

procedure TfrmCodesysXmlBuilder.SaveUseState;
var i: LongInt;
begin
  if (CodesysUseState = nil) or (FUseStateFile = '') then exit;
  for i:=0 to High(FVars) do
    CodesysUseState.Values[UseStateKey(FVars[i])] := BoolToStr(FChecked[i],'Y','N');
end;

procedure TfrmCodesysXmlBuilder.SetImportedKeys(const AKeys: TStrings);
var i: LongInt;
begin
  FImportedKeys.Assign(AKeys);
  for i:=0 to High(FVars) do
    if FImportedKeys.IndexOf(ImportedKey(FVars[i])) >= 0 then FChecked[i] := false;
  ApplyUseState;
  RefreshGrid;
end;

procedure TfrmCodesysXmlBuilder.GridClick(Sender: TObject);
var R: LongInt;
begin
  R := grid.Row - 1;
  if (R < 0) or (R >= Length(FVars)) then exit;
  FChecked[R] := not FChecked[R];
  RefreshGrid;
  grid.Row := R + 1;
end;

procedure TfrmCodesysXmlBuilder.SelectAllClick(Sender: TObject);
var i: LongInt;
begin
  for i:=0 to High(FChecked) do FChecked[i] := true;
  RefreshGrid;
end;

procedure TfrmCodesysXmlBuilder.SelectNoneClick(Sender: TObject);
var i: LongInt;
begin
  for i:=0 to High(FChecked) do FChecked[i] := false;
  RefreshGrid;
end;

function TfrmCodesysXmlBuilder.SelectedVars: TCodesysXmlVars;
var i, Count: LongInt;
begin
  Result := nil;
  Count := 0;
  for i:=0 to High(FVars) do
    if (i < Length(FChecked)) and FChecked[i] then inc(Count);
  SetLength(Result,Count);
  Count := 0;
  for i:=0 to High(FVars) do
    if (i < Length(FChecked)) and FChecked[i] then begin
      Result[Count] := FVars[i];
      inc(Count);
    end;
  FNamespace := edtNamespace.Value;
  FPathPrefix := edtPathPrefix.Text;
  FDefaultPeriod := edtPeriod.Value;
  FNamePrefix := edtNamePrefix.Text;
end;

function TfrmCodesysXmlBuilder.DeselectedVars: TCodesysXmlVars;
var i, Count: LongInt;
begin
  Result := nil;
  Count := 0;
  for i:=0 to High(FVars) do
    if (i >= Length(FChecked)) or (not FChecked[i]) then inc(Count);
  SetLength(Result,Count);
  Count := 0;
  for i:=0 to High(FVars) do
    if (i >= Length(FChecked)) or (not FChecked[i]) then begin
      Result[Count] := FVars[i];
      inc(Count);
    end;
end;

procedure TfrmCodesysXmlBuilder.OKClick(Sender: TObject);
var Sel: TCodesysXmlVars;
begin
  Sel := SelectedVars;
  if Length(Sel) = 0 then begin
    MessageDlg('No variable selected.',mtInformation,[mbOK],0);
    exit;
  end;
  ModalResult := mrOK;
end;

procedure TfrmCodesysXmlBuilder.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

initialization
  CodesysUseState := TStringList.Create;

finalization
  CodesysUseState.Free;

end.
