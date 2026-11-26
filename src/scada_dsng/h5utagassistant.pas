{$i ../common/language.inc}
{:
  @abstract(Design-time Tag Builder for the Inovance H5U tag area driver.)

  Implements the OpenTagEditor callback used by TH5UTagTCPDriver: opens a
  wizard that reads an AutoShop "HMI monitor variables" CSV and creates the
  selected TPLCTagNumber / TPLCBlock components bound to the driver.
}
unit h5utagassistant;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, H5UTagDriver, ProtocolTypes, uh5utagbuilder, h5utagcsv;

type
  { TH5UTagAssistant }
  TH5UTagAssistant = class
  private
    FDriver: TH5UTagDriver;
  public
    procedure OpenTagEditor(OwnerOfNewTags: TComponent;
      InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
  published
    property Driver: TH5UTagDriver read FDriver write FDriver;
  end;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);

implementation

uses
  PLCTagNumber, PLCBlock, Tag, Dialogs, Controls, TypInfo, Math;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  Wizard: TH5UTagAssistant;
begin
  if not (aProtocolDriver is TH5UTagDriver) then
    raise Exception.Create('An H5U tag driver is required as protocol.');

  Wizard := TH5UTagAssistant.Create;
  try
    Wizard.Driver := TH5UTagDriver(aProtocolDriver);
    Wizard.OpenTagEditor(aOwnerOfNewTags,InsertHook,CreateProc);
  finally
    Wizard.Free;
  end;
end;

{ TH5UTagAssistant }

procedure TH5UTagAssistant.OpenTagEditor(OwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  Dlg: TfrmH5UTagBuilder;
  Vars: TH5UTagCsvVars;
  V: TH5UTagCsvVar;
  i, FuncCode: LongInt;
  TagNum: TPLCTagNumber;
  TagBlock: TPLCBlock;
  UsedNames, ImportedKeys, TagByVarName: TStringList;
  BaseName, TagName, DeletedList: string;
  N, Skipped: LongInt;

  {: CSVVariableName of a tag component, or '' when not imported from CSV. }
  function CSVNameOf(C: TComponent): string;
  begin
    Result := '';
    if C is TPLCTagNumber then Result := TPLCTagNumber(C).CSVVariableName
    else if C is TPLCBlock then Result := TPLCBlock(C).CSVVariableName;
  end;

  {: Map CSVVariableName -> tag component for tags imported from a CSV, so a
     variable unchecked in the wizard can have its tag removed. }
  procedure BuildTagByVarNameMap;
  var
    j: LongInt;
    C: TComponent;
    Num: TPLCTagNumber;
    Blk: TPLCBlock;
    S: string;
  begin
    if not Assigned(OwnerOfNewTags) then exit;
    for j:=0 to OwnerOfNewTags.ComponentCount-1 do begin
      C := OwnerOfNewTags.Components[j];
      S := CSVNameOf(C);
      if (S = '') then continue;
      if C is TPLCTagNumber then begin
        if TPLCTagNumber(C).ProtocolDriver <> FDriver then continue;
      end else if C is TPLCBlock then begin
        if TPLCBlock(C).ProtocolDriver <> FDriver then continue;
      end else continue;
      TagByVarName.AddObject(S,C);
    end;
  end;

  {: Delete tags whose CSV variable was unchecked in this wizard run. Only
     tags from the CURRENT CSV that were un-checked are considered; tags
     created manually (no CSVVariableName) or belonging to other drivers are
     never touched. Returns the number of tags actually removed. }
  function DeleteUncheckedTags(out DeletedList: string): LongInt;
  var
    k, m: LongInt;
    DVars: TH5UTagCsvVars;
    ToDelete: TList;
    C: TComponent;
  begin
    Result := 0;
    DeletedList := '';
    DVars := Dlg.DeselectedVars;
    ToDelete := TList.Create;
    try
      for k:=0 to High(DVars) do
        for m:=0 to TagByVarName.Count-1 do
          if TagByVarName[m] = DVars[k].Name then begin
            C := TComponent(TagByVarName.Objects[m]);
            if (C <> nil) and (ToDelete.IndexOf(C) < 0) then
              ToDelete.Add(C);
          end;
      if ToDelete.Count = 0 then exit;

      for k:=0 to ToDelete.Count-1 do begin
        C := TComponent(ToDelete[k]);
        if DeletedList <> '' then DeletedList := DeletedList + #13#10;
        DeletedList := DeletedList + '  - ' + C.Name + '  (' + CSVNameOf(C) + ')';
      end;
      if MessageDlg('These tag(s) are no longer selected and will be DELETED:' +
        #13#10#13#10 + DeletedList + #13#10#13#10 + 'Continue?',
        mtConfirmation,[mbYes,mbNo],0) <> mrYes then exit;

      for k:=0 to ToDelete.Count-1 do begin
        C := TComponent(ToDelete[k]);
        try
          C.Free;
          inc(Result);
        except
          on E: Exception do
            MessageDlg('Error deleting tag: ' + E.Message,mtError,[mbOK],0);
        end;
      end;
    finally
      ToDelete.Free;
    end;
  end;

  {: Build the station|function|address|size key for a tag on the form, so the
     wizard can mark those variables as already imported. }
  function TagKey(AStation, AFunc, AAddress, ASize: LongInt): string;
  begin
    Result := IntToStr(AStation) + '|' + IntToStr(AFunc) + '|' +
      IntToStr(AAddress) + '|' + IntToStr(ASize);
  end;

  {: Collect all H5U tags already on the form into ImportedKeys. }
  procedure CollectImportedKeys;
  var
    j: LongInt;
    C: TComponent;
    Num: TPLCTagNumber;
    Blk: TPLCBlock;
  begin
    if not Assigned(OwnerOfNewTags) then exit;
    for j:=0 to OwnerOfNewTags.ComponentCount-1 do begin
      C := OwnerOfNewTags.Components[j];
      if C is TPLCTagNumber then begin
        Num := TPLCTagNumber(C);
        if Num.ProtocolDriver = FDriver then
          ImportedKeys.Add(TagKey(Num.PLCStation,Num.MemReadFunction,
            Num.MemAddress,1));
      end else if C is TPLCBlock then begin
        Blk := TPLCBlock(C);
        if Blk.ProtocolDriver = FDriver then
          ImportedKeys.Add(TagKey(Blk.PLCStation,Blk.MemReadFunction,
            Blk.MemAddress,Blk.Size));
      end;
    end;
  end;

  function MakeUniqueName(const AName: string): string;
  var
    C: LongInt;
  begin
    Result := AName;
    C := 1;
    while UsedNames.IndexOf(Result) >= 0 do begin
      inc(C);
      Result := AName + '_' + IntToStr(C);
    end;
    UsedNames.Add(Result);
  end;

  {: Ignore names already used by components on the form. Without this,
     importing the same CSV twice raises "Duplicate name: a component named
     ... already exists". }
  procedure CollectExistingNames;
  var
    i: LongInt;
  begin
    if not Assigned(OwnerOfNewTags) then exit;
    for i:=0 to OwnerOfNewTags.ComponentCount-1 do
      UsedNames.Add(OwnerOfNewTags.Components[i].Name);
  end;

  {: Returns TRUE when a tag bound to the same station / function / address
     already exists. Re-importing the same CSV then becomes a no-op instead
     of creating a second set of tags pointing at the same PLC addresses. }
  function TagAlreadyExists(AStation, AFunc, AAddress, ASize: LongInt): Boolean;
  var
    i: LongInt;
    C: TComponent;
    Num: TPLCTagNumber;
    Blk: TPLCBlock;
  begin
    Result := false;
    if not Assigned(OwnerOfNewTags) then exit;
    for i:=0 to OwnerOfNewTags.ComponentCount-1 do begin
      C := OwnerOfNewTags.Components[i];
      if C is TPLCTagNumber then begin
        Num := TPLCTagNumber(C);
        if (Num.PLCStation = AStation) and (Num.MemReadFunction = AFunc) and
           (Num.MemAddress = AAddress) then exit(true);
      end else if C is TPLCBlock then begin
        Blk := TPLCBlock(C);
        if (Blk.PLCStation = AStation) and (Blk.MemReadFunction = AFunc) and
           (Blk.MemAddress = AAddress) and (Blk.Size = ASize) then exit(true);
      end;
    end;
  end;

begin
  if not Assigned(FDriver) then begin
    MessageDlg('A protocol driver is required.',mtError,[mbOK],0);
    exit;
  end;

  ImportedKeys := TStringList.Create;
  TagByVarName := TStringList.Create;
  try
    ImportedKeys.Sorted := true;
    ImportedKeys.Duplicates := dupIgnore;
    CollectImportedKeys;
    BuildTagByVarNameMap;
    Dlg := TfrmH5UTagBuilder.Create(nil);
    try
      // Station / period are restored by the wizard itself from the last
      // session; do not overwrite them here.
      Dlg.SetImportedKeys(ImportedKeys);
      if Dlg.ShowModal = mrOK then begin
        // Remove tags for variables the user unchecked this time, before
        // creating new ones so the freed names become available again.
        N := DeleteUncheckedTags(DeletedList);
        if N > 0 then
          MessageDlg(IntToStr(N) + ' tag(s) deleted for unchecked variable(s).',
            mtInformation,[mbOK],0);

        Vars := Dlg.SelectedVars;
        UsedNames := TStringList.Create;
        try
          UsedNames.Sorted := true;
          UsedNames.Duplicates := dupIgnore;
          CollectExistingNames;
          Skipped := 0;
          try
            for i:=0 to High(Vars) do begin
              V := Vars[i];
              if V.Device = 'UB' then FuncCode := H5UTAG_FUNC_UB
              else FuncCode := H5UTAG_FUNC_UW;

              if TagAlreadyExists(Dlg.Station,FuncCode,V.Address,
                Max(1,V.Count)) then begin
                inc(Skipped);
                continue;
              end;

              BaseName := H5UIdentFromVarName(V.Name,i+1,V.Device,V.Address);
              TagName := MakeUniqueName(BaseName);

              if (V.Count > 1) and (V.Device = 'UW') then begin
                TagBlock := TPLCBlock(CreateProc(TPLCBlock));
                TagBlock.Name := TagName;
                TagBlock.PLCStation := Dlg.Station;
                TagBlock.MemReadFunction := FuncCode;
                TagBlock.MemWriteFunction := FuncCode;
                TagBlock.MemAddress := V.Address;
                TagBlock.Size := V.Count;
                TagBlock.TagType := TTagType(GetEnumValue(TypeInfo(TTagType),
                  H5UVarTypeToTagTypeName(V.VarType)));
                TagBlock.RefreshTime := V.Period;
                TagBlock.ProtocolDriver := FDriver;
                TagBlock.CSVVariableName := V.Name;
                InsertHook(TagBlock);
              end else begin
                TagNum := TPLCTagNumber(CreateProc(TPLCTagNumber));
                TagNum.Name := TagName;
                TagNum.PLCStation := Dlg.Station;
                TagNum.MemReadFunction := FuncCode;
                TagNum.MemWriteFunction := FuncCode;
                TagNum.MemAddress := V.Address;
                N := H5UVarTypeToWords(V.VarType);
                if N > 1 then TagNum.TagType := TTagType(GetEnumValue(TypeInfo(TTagType),
                  H5UVarTypeToTagTypeName(V.VarType)))
                else TagNum.TagType := pttDefault;
                TagNum.RefreshTime := V.Period;
                TagNum.ProtocolDriver := FDriver;
                TagNum.CSVVariableName := V.Name;
                InsertHook(TagNum);
              end;
            end;
          except
            on E: Exception do
              MessageDlg('Error creating tags: ' + E.Message,mtError,[mbOK],0);
          end;
          if Skipped > 0 then
            MessageDlg(IntToStr(Skipped) + ' variable(s) already have a tag at ' +
              'the same station/address and were skipped.',mtInformation,[mbOK],0);
        finally
          UsedNames.Free;
        end;
      end;
    finally
      Dlg.Free;
    end;
  finally
    TagByVarName.Free;
    ImportedKeys.Free;
  end;
end;

initialization
  SetTagBuilderToolForH5UTagProtocol(@OpenTagEditor);

end.
