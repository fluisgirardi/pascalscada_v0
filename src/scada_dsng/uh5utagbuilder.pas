{$i ../common/language.inc}
{:
  @abstract(Wizard form for importing an AutoShop variable CSV into H5U tags.)

  The form is built entirely in code (no .lfm resource) so it can be compiled
  into the design-time package without resource file handling. It shows the
  variables found in the CSV, lets the user check the ones to import, and
  edits the station number and the default refresh time.
}
unit uh5utagbuilder;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Classes, SysUtils, Math, Forms, Controls, StdCtrls, Buttons, Grids, Dialogs,
  ExtCtrls, Spin, h5utagcsv;

type
  { TfrmH5UTagBuilder }
  TfrmH5UTagBuilder = class(TForm)
  private
    FVars: TH5UTagCsvVars;
    FChecked: array of Boolean;
    FStation: LongInt;
    FDefaultPeriod: LongInt;
    FUseCsvPeriod: Boolean;
    edtFile: TEdit;
    btnBrowse: TButton;
    btnLoad: TButton;
    grid: TStringGrid;
    edtStation: TSpinEdit;
    edtPeriod: TSpinEdit;
    chkOverwritePeriod: TCheckBox;
    lblInfo: TLabel;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure BrowseClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure SelectNoneClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure RefreshGrid;
  public
    constructor Create(AOwner: TComponent); override;
    {: Station used for every generated tag. }
    property Station: LongInt read FStation write FStation;
    {: Default refresh time used when the CSV has no period. }
    property DefaultPeriod: LongInt read FDefaultPeriod write FDefaultPeriod;
    {: TRUE when the user wants the CSV period to overwrite DefaultPeriod. }
    property UseCsvPeriod: Boolean read FUseCsvPeriod write FUseCsvPeriod;
    {: Variables selected by the user. }
    function SelectedVars: TH5UTagCsvVars;
    {: Adds the variables to the dialog after it has been created. }
    procedure SetVars(const AVars: TH5UTagCsvVars);
  end;

var
  frmH5UTagBuilder: TfrmH5UTagBuilder;

implementation

constructor TfrmH5UTagBuilder.Create(AOwner: TComponent);
begin
  // This form is built entirely in code (no .lfm resource). TCustomForm.Create
  // calls InitInheritedComponent, which raises "resource not found" in the IDE
  // because Lazarus sets RequireDerivedFormResource := True at startup.
  // CreateNew skips resource loading and is the supported way for such forms.
  inherited CreateNew(AOwner, 0);
  Caption := 'H5U tag import - AutoShop CSV';
  Position := poScreenCenter;
  Width := 780;
  Height := 560;
  BorderStyle := bsDialog;

  FStation := 1;
  FDefaultPeriod := 100;
  FUseCsvPeriod := true;
  SetLength(FVars,0);
  SetLength(FChecked,0);

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self;
    Left := 12; Top := 12; Width := 560; Height := 18;
    Caption := 'Select the AutoShop "HMI monitor variables" CSV (GBK encoded):';
  end;

  edtFile := TEdit.Create(Self);
  with edtFile do begin
    Parent := Self; Left := 12; Top := 34; Width := 620; Height := 23;
    Text := '';
  end;

  btnBrowse := TButton.Create(Self);
  with btnBrowse do begin
    Parent := Self; Left := 640; Top := 33; Width := 58; Height := 24;
    Caption := '...';
    OnClick := @BrowseClick;
  end;

  btnLoad := TButton.Create(Self);
  with btnLoad do begin
    Parent := Self; Left := 704; Top := 33; Width := 62; Height := 24;
    Caption := 'Load';
    OnClick := @LoadClick;
  end;

  grid := TStringGrid.Create(Self);
  with grid do begin
    Parent := Self;
    Left := 12; Top := 68; Width := 754; Height := 360;
    FixedCols := 0;
    FixedRows := 1;
    RowCount := 1;
    ColCount := 6;
    Cells[0,0] := 'Use';
    Cells[1,0] := 'Variable name';
    Cells[2,0] := 'Type';
    Cells[3,0] := 'Device';
    Cells[4,0] := 'Address';
    Cells[5,0] := 'Period / comment';
    ColWidths[0] := 44;
    ColWidths[1] := 240;
    ColWidths[2] := 78;
    ColWidths[3] := 62;
    ColWidths[4] := 100;
    ColWidths[5] := 228;
    Options := Options + [goRowSelect];
    OnClick := @GridClick;
  end;

  btnSelectAll := TButton.Create(Self);
  with btnSelectAll do begin
    Parent := Self; Left := 12; Top := 436; Width := 80; Height := 24;
    Caption := 'Select all'; OnClick := @SelectAllClick;
  end;
  btnSelectNone := TButton.Create(Self);
  with btnSelectNone do begin
    Parent := Self; Left := 100; Top := 436; Width := 80; Height := 24;
    Caption := 'Select none'; OnClick := @SelectNoneClick;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 12; Top := 470; Width := 70; Height := 18;
    Caption := 'Station:';
  end;
  edtStation := TSpinEdit.Create(Self);
  with edtStation do begin
    Parent := Self; Left := 84; Top := 467; Width := 60; Height := 23;
    MinValue := 1; MaxValue := 255; Value := FStation;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 158; Top := 470; Width := 84; Height := 18;
    Caption := 'Period (ms):';
  end;
  edtPeriod := TSpinEdit.Create(Self);
  with edtPeriod do begin
    Parent := Self; Left := 248; Top := 467; Width := 76; Height := 23;
    MinValue := 10; MaxValue := 7200000; Value := FDefaultPeriod;
  end;

  chkOverwritePeriod := TCheckBox.Create(Self);
  with chkOverwritePeriod do begin
    Parent := Self; Left := 340; Top := 468; Width := 220; Height := 20;
    Caption := 'Use period from CSV when present';
    Checked := FUseCsvPeriod;
  end;

  btnOK := TButton.Create(Self);
  with btnOK do begin
    Parent := Self; Left := 594; Top := 464; Width := 84; Height := 26;
    Caption := 'OK'; ModalResult := mrNone; OnClick := @OKClick;
  end;
  btnCancel := TButton.Create(Self);
  with btnCancel do begin
    Parent := Self; Left := 686; Top := 464; Width := 80; Height := 26;
    Caption := 'Cancel'; ModalResult := mrCancel; OnClick := @CancelClick;
  end;

  lblInfo := TLabel.Create(Self);
  with lblInfo do begin
    Parent := Self; Left := 12; Top := 500; Width := 754; Height := 32;
    Caption := 'Select the variables to import, then click OK. PascalSCADA will ' +
      'create TPLCTagNumber / TPLCBlock components bound to the H5U driver.';
    WordWrap := true;
  end;
end;

procedure TfrmH5UTagBuilder.BrowseClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title := 'Open AutoShop variable CSV';
    Dlg.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    Dlg.Options := [ofFileMustExist];
    if Dlg.Execute then edtFile.Text := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmH5UTagBuilder.LoadClick(Sender: TObject);
var
  Err: string;
  Vars: TH5UTagCsvVars;
  i: LongInt;
begin
  if Trim(edtFile.Text) = '' then begin
    MessageDlg('Select the CSV file first.',mtInformation,[mbOK],0);
    exit;
  end;
  if not ParseAutoShopCSV(Trim(edtFile.Text),Vars,Err) then begin
    MessageDlg(Err,mtError,[mbOK],0);
    exit;
  end;
  SetVars(Vars);
end;

procedure TfrmH5UTagBuilder.SetVars(const AVars: TH5UTagCsvVars);
var
  i: LongInt;
begin
  FVars := AVars;
  SetLength(FChecked,Length(FVars));
  for i:=0 to High(FVars) do FChecked[i] := true;
  RefreshGrid;
end;

procedure TfrmH5UTagBuilder.RefreshGrid;
var
  i: LongInt;
begin
  grid.RowCount := Max(2,Length(FVars)+1);
  for i:=0 to High(FVars) do begin
    if FChecked[i] then grid.Cells[0,i+1] := 'Y' else grid.Cells[0,i+1] := 'N';
    grid.Cells[1,i+1] := FVars[i].Name;
    grid.Cells[2,i+1] := FVars[i].VarType;
    grid.Cells[3,i+1] := FVars[i].Device;
    grid.Cells[4,i+1] := '0x' + IntToHex(FVars[i].Address,8);
    if FVars[i].Comment <> '' then
      grid.Cells[5,i+1] := IntToStr(FVars[i].Period) + 'ms  ' + FVars[i].Comment
    else
      grid.Cells[5,i+1] := IntToStr(FVars[i].Period) + 'ms';
  end;
  if Length(FVars) = 0 then begin
    grid.Cells[0,1] := ''; grid.Cells[1,1] := ''; grid.Cells[2,1] := '';
    grid.Cells[3,1] := ''; grid.Cells[4,1] := ''; grid.Cells[5,1] := '';
  end;
end;

procedure TfrmH5UTagBuilder.GridClick(Sender: TObject);
var
  R: LongInt;
begin
  R := grid.Row - 1;
  if (R < 0) or (R >= Length(FVars)) then exit;
  FChecked[R] := not FChecked[R];
  RefreshGrid;
  grid.Row := R + 1;
end;

procedure TfrmH5UTagBuilder.SelectAllClick(Sender: TObject);
var
  i: LongInt;
begin
  for i:=0 to High(FChecked) do FChecked[i] := true;
  RefreshGrid;
end;

procedure TfrmH5UTagBuilder.SelectNoneClick(Sender: TObject);
var
  i: LongInt;
begin
  for i:=0 to High(FChecked) do FChecked[i] := false;
  RefreshGrid;
end;

function TfrmH5UTagBuilder.SelectedVars: TH5UTagCsvVars;
var
  i, Count: LongInt;
begin
  Count := 0;
  for i:=0 to High(FVars) do
    if (i < Length(FChecked)) and FChecked[i] then inc(Count);
  SetLength(Result,Count);
  Count := 0;
  for i:=0 to High(FVars) do
    if (i < Length(FChecked)) and FChecked[i] then begin
      Result[Count] := FVars[i];
      if FUseCsvPeriod and (FVars[i].Period > 0) then
        Result[Count].Period := FVars[i].Period
      else
        Result[Count].Period := edtPeriod.Value;
      inc(Count);
    end;
end;

procedure TfrmH5UTagBuilder.OKClick(Sender: TObject);
begin
  if Length(SelectedVars) = 0 then begin
    MessageDlg('No variable selected.',mtInformation,[mbOK],0);
    exit;
  end;
  FStation := edtStation.Value;
  FDefaultPeriod := edtPeriod.Value;
  FUseCsvPeriod := chkOverwritePeriod.Checked;
  ModalResult := mrOK;
end;

procedure TfrmH5UTagBuilder.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
