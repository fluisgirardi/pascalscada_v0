{$i ../common/language.inc}
{:
  @abstract(Design-time Tag Builder for the Codesys V3 symbol protocol driver.)

  This is the Tag Builder of TCodesysSymbolDriver (proprietary TCP 11740
  symbol service). It reads a Codesys Symbolconfiguration XML and creates the
  selected TPLCTagNumber / TPLCString components with LongAddress set to the
  full symbol path, e.g. @code(Application.PLC_PRG.bBackward).
}
unit codesysxmlassistant;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, CodesysDriver, ProtocolTypes, ucodesysxmlbuilder, codesysxml;

type
  { TCodesysXmlAssistant }
  TCodesysXmlAssistant = class
  private
    FDriver: TCodesysSymbolDriver;
  public
    procedure OpenTagEditor(OwnerOfNewTags: TComponent;
      InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
  published
    property Driver: TCodesysSymbolDriver read FDriver write FDriver;
  end;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);

implementation

uses
  PLCTagNumber, PLCBlock, PLCString, Tag, TypInfo, Dialogs, Controls;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  Wizard: TCodesysXmlAssistant;
begin
  if not (aProtocolDriver is TCodesysSymbolDriver) then
    raise Exception.Create('A Codesys symbol protocol driver is required as protocol.');

  Wizard := TCodesysXmlAssistant.Create;
  try
    Wizard.Driver := TCodesysSymbolDriver(aProtocolDriver);
    Wizard.OpenTagEditor(aOwnerOfNewTags,InsertHook,CreateProc);
  finally
    Wizard.Free;
  end;
end;

{ TCodesysXmlAssistant }

procedure TCodesysXmlAssistant.OpenTagEditor(OwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  Dlg: TfrmCodesysXmlBuilder;
  Vars, DVars: TCodesysXmlVars;
  V: TCodesysXmlVar;
  i: LongInt;
  TagNum: TPLCTagNumber;
  TagStr: TPLCString;
  UsedNames, ImportedKeys: TStringList;
  BaseName, TagName, SymbolPath, TypeName, DeletedList: string;
  Skipped, Deleted, TypeValue: LongInt;
  C: TComponent;
  ToDelete: TList;

  {: SymbolPath of an existing tag component on the form, or '' when not an OPC UA tag. }
  function TagSymbolPathOf(Cmp: TComponent): string;
  begin
    Result := '';
    if Cmp is TPLCTagNumber then Result := TPLCTagNumber(Cmp).LongAddress
    else if Cmp is TPLCBlock then Result := TPLCBlock(Cmp).LongAddress
    else if Cmp is TPLCString then Result := TPLCString(Cmp).LongAddress;
  end;

  {: True when the component belongs to this driver. }
  function TagBelongsToDriver(Cmp: TComponent): Boolean;
  begin
    Result := false;
    if Cmp is TPLCTagNumber then Result := TPLCTagNumber(Cmp).ProtocolDriver = FDriver
    else if Cmp is TPLCBlock then Result := TPLCBlock(Cmp).ProtocolDriver = FDriver
    else if Cmp is TPLCString then Result := TPLCString(Cmp).ProtocolDriver = FDriver;
  end;

  {: Builds a unique component name from the symbol leaf name. }
  function MakeUniqueName(const ABase: string): string;
  var S: string; Counter: LongInt;
  begin
    S := Dlg.NamePrefix + ABase;
    if S = '' then S := 'CodesysTag';
    if not (S[1] in ['A'..'Z','a'..'z','_']) then S := 'Tag_' + S;
    Result := S;
    if not Assigned(OwnerOfNewTags) then exit;
    Counter := 1;
    while UsedNames.IndexOf(Result) >= 0 do begin
      inc(Counter);
      Result := S + '_' + IntToStr(Counter);
    end;
  end;

  {: Deletes OPC UA tags on this driver whose SymbolPath matches a variable that
     was unchecked in the wizard. Only SymbolPaths present in the current XML and
     unchecked this run are considered. }
  function DeleteUncheckedTags(out ADeletedList: string): LongInt;
  var j, m: LongInt;
  begin
    Result := 0;
    ADeletedList := '';
    if not Assigned(OwnerOfNewTags) then exit;
    DVars := Dlg.DeselectedVars;
    ToDelete := TList.Create;
    try
      for j:=0 to High(DVars) do begin
        SymbolPath := DVars[j].Name;
        for m:=0 to OwnerOfNewTags.ComponentCount-1 do begin
          C := OwnerOfNewTags.Components[m];
          if (TagSymbolPathOf(C) = SymbolPath) and TagBelongsToDriver(C) and
             (ToDelete.IndexOf(C) < 0) then
            ToDelete.Add(C);
        end;
      end;
      for j:=0 to ToDelete.Count-1 do begin
        C := TComponent(ToDelete[j]);
        if ADeletedList <> '' then ADeletedList := ADeletedList + ', ';
        ADeletedList := ADeletedList + C.Name;
        C.Free;
        inc(Result);
      end;
    finally
      ToDelete.Free;
    end;
  end;

begin
  Dlg := TfrmCodesysXmlBuilder.Create(nil);
  try
    // Collect SymbolPaths of tags already on the form so imported rows are marked.
    ImportedKeys := TStringList.Create;
    UsedNames := TStringList.Create;
    try
      ImportedKeys.Sorted := true;
      ImportedKeys.Duplicates := dupIgnore;
      UsedNames.Sorted := true;
      UsedNames.Duplicates := dupIgnore;
      if Assigned(OwnerOfNewTags) then
        for i:=0 to OwnerOfNewTags.ComponentCount-1 do begin
          C := OwnerOfNewTags.Components[i];
          if TagBelongsToDriver(C) then begin
            SymbolPath := TagSymbolPathOf(C);
            if SymbolPath <> '' then ImportedKeys.Add(LowerCase(SymbolPath));
            UsedNames.Add(C.Name);
          end;
        end;
      Dlg.SetImportedKeys(ImportedKeys);

      if Dlg.ShowModal <> mrOK then exit;

      // Remove unchecked tags first, so their names are free for reuse.
      Deleted := DeleteUncheckedTags(DeletedList);
      if Deleted > 0 then
        MessageDlg('Removed ' + IntToStr(Deleted) + ' tag(s) no longer selected: ' +
          DeletedList, mtInformation, [mbOK], 0);

      Vars := Dlg.SelectedVars;
      Skipped := 0;
      for i:=0 to High(Vars) do begin
        V := Vars[i];
        SymbolPath := V.Name;
        TypeName := CodesysVarTypeToTagTypeName(V.TypeClass,V.IECName);
        BaseName := CodesysIdentFromVarName(V.Name,i+1);
        TagName := MakeUniqueName(BaseName);
        UsedNames.Add(TagName);

        if TypeName = 'text' then begin
          TagStr := TPLCString(CreateProc(TPLCString));
          TagStr.Name := TagName;
          TagStr.LongAddress := SymbolPath;
          TagStr.StringType := stC;
          TagStr.StringSize := 80;
          TagStr.RefreshTime := Dlg.DefaultPeriod;
          TagStr.ProtocolDriver := FDriver;
          InsertHook(TagStr);
        end else begin
          // The symbol protocol has no online type discovery: the IEC type
          // comes from the XML, so set TagType explicitly. The driver uses it
          // both to decode raw bytes and to encode writes.
          TagNum := TPLCTagNumber(CreateProc(TPLCTagNumber));
          TagNum.Name := TagName;
          TagNum.LongAddress := SymbolPath;
          TypeValue := GetEnumValue(TypeInfo(TTagType),TypeName);
          if (TypeValue<0) or (TypeName='pttDefault') then
            TagNum.TagType := pttDouble
          else
            TagNum.TagType := TTagType(TypeValue);
          TagNum.RefreshTime := Dlg.DefaultPeriod;
          TagNum.ProtocolDriver := FDriver;
          InsertHook(TagNum);
        end;
      end;
      if Skipped > 0 then
        MessageDlg(IntToStr(Skipped)+' variable(s) skipped.',mtInformation,[mbOK],0);
    finally
      UsedNames.Free;
      ImportedKeys.Free;
    end;
  finally
    Dlg.Free;
  end;
end;

initialization
  SetTagBuilderToolForCodesysDriver(@OpenTagEditor);

end.
