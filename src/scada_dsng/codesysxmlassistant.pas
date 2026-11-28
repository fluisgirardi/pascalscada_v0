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
  PLCTagNumber, PLCBlock, PLCString, Tag, TypInfo, Dialogs, Controls
  {$IFDEF FPC}, PropEdits{$ENDIF};

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
  DeletionConfirmed: Boolean;
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

  {: True when a tag bound to this driver with the same symbol path already
     exists on the form. Re-opening the wizard must be a no-op for variables
     that were imported before; without this check the wizard recreates them
     with a "_2" name suffix (visible in the 20:44:46 trace). Tags queued for
     deletion are still owned at this point, so they are explicitly ignored. }
  function TagExistsForPath(const APath: string): Boolean;
  var t: LongInt;
      Cmp: TComponent;
  begin
    Result := false;
    if not Assigned(OwnerOfNewTags) then exit;
    for t:=0 to OwnerOfNewTags.ComponentCount-1 do begin
      Cmp := OwnerOfNewTags.Components[t];
      // Tags queued for deletion are still on the form while the create loop
      // runs: ignore them so a re-selected variable can be created at once.
      if (ToDelete <> nil) and (ToDelete.IndexOf(Cmp) >= 0) then continue;
      if TagBelongsToDriver(Cmp) and SameText(TagSymbolPathOf(Cmp),APath) then
        exit(true);
    end;
  end;

  {: Pointer-only test: is Cmp still listed among the owner's components?
     Used after the IDE delete hook to decide whether the component still
     needs a plain Free. Deliberately does NOT dereference Cmp. }
  function ComponentStillOwned(Cmp: TComponent): Boolean;
  var t: LongInt;
  begin
    Result := false;
    if not Assigned(OwnerOfNewTags) then exit;
    for t:=0 to OwnerOfNewTags.ComponentCount-1 do
      if OwnerOfNewTags.Components[t] = Cmp then exit(true);
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

  {: Collect the tags whose symbol path was unchecked in the wizard and ask
     for confirmation. Only paths present in the current XML and unchecked
     this run are considered.

     The actual destruction is DEFERRED until after the create loop. The
     CreateProc / InsertHook callbacks are method pointers owned by the IDE
     component editor; deleting a persistent makes Lazarus rebuild that
     editor, so a CreateProc call issued afterwards jumps into a freed object
     and raises EAccessViolation. Codesys happened to hide this bug because
     every run that deleted tags also skipped all new creations. Returns True
     when the user confirmed the deletion. }
  function PrepareTagDeletions(out ADeletedList: string): Boolean;
  var j, m: LongInt;
  begin
    Result := false;
    ADeletedList := '';
    if not Assigned(OwnerOfNewTags) then exit;
    DVars := Dlg.DeselectedVars;
    for j:=0 to High(DVars) do begin
      SymbolPath := DVars[j].Name;
      for m:=0 to OwnerOfNewTags.ComponentCount-1 do begin
        C := OwnerOfNewTags.Components[m];
        if SameText(TagSymbolPathOf(C),SymbolPath) and TagBelongsToDriver(C) and
           (ToDelete.IndexOf(C) < 0) then
          ToDelete.Add(C);
      end;
    end;
    if ToDelete.Count = 0 then begin
      exit;
    end;

    // Ask before destroying anything, and build the list while the
    // components are still alive.
    for j:=0 to ToDelete.Count-1 do begin
      C := TComponent(ToDelete[j]);
      if ADeletedList <> '' then ADeletedList := ADeletedList + ', ';
      ADeletedList := ADeletedList + C.Name;
    end;
    Result := MessageDlg('These tag(s) are no longer selected and will be DELETED:' +
      #13#10#13#10 + ADeletedList + #13#10#13#10 + 'Continue?',
      mtConfirmation,[mbYes,mbNo],0) = mrYes;
    if not Result then begin
      ToDelete.Clear;
      ADeletedList := '';
    end;
  end;

  {: Destroy the confirmed tags. Called only after ALL CreateProc / InsertHook
     calls of this wizard run have finished.

     IMPORTANT: do NOT use GlobalDesignHook.DeletePersistent here. It hands the
     component to TDesigner.DoDeletePersistent, which frees it and then
     broadcasts PersistentDeleted with the ALREADY FREED pointer, while
     TMainIDE.PropHookPersistentDeleting calls CodeToolBoss.RemovePublishedVariable
     on the form source. These tags were created with tagclass.Create(Owner) +
     PersistentAdded, NOT Designer.CreateComponent, so dragging the designer
     in corrupts IDE state: trace of 2026-11-27 20:44:46 shows DEL done ->
     CREATE count=2 -> NEW begin -> EAccessViolation. Broadcast the
     PersistentDeleting notification ourselves and free the component through
     its normal destructor instead; never send PersistentDeleted afterwards. }
  function ApplyTagDeletions(out ADeletedList: string): LongInt;
  var j, k: LongInt;
      DeletedName: string;
  begin
    Result := 0;
    ADeletedList := '';
    for j:=0 to ToDelete.Count-1 do begin
      C := TComponent(ToDelete[j]);
      // The IDE hook may remove several components at once. Skip any that
      // were already taken out of the owner list, so we never touch a
      // dangling pointer.
      if not ComponentStillOwned(C) then continue;
      DeletedName := C.Name;
      try
        {$IFDEF FPC}
        if Assigned(GlobalDesignHook) then begin
          GlobalDesignHook.PersistentDeleting(C);
        end;
        {$ENDIF}
        if ComponentStillOwned(C) then begin
          C.Free;
        end;
        // No PersistentDeleted(C): the object no longer exists.
        // Free the name too, so re-importing this variable reuses the
        // original name instead of getting a "_2" suffix.
        k := UsedNames.IndexOf(DeletedName);
        if k >= 0 then UsedNames.Delete(k);
        if ADeletedList <> '' then ADeletedList := ADeletedList + ', ';
        ADeletedList := ADeletedList + DeletedName;
        inc(Result);
      except
        on E: Exception do begin
          MessageDlg('Error deleting tag ' + DeletedName + ': ' + E.Message,
            mtError,[mbOK],0);
        end;
      end;
    end;
  end;

begin
  ToDelete := TList.Create;
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

      if Dlg.ShowModal <> mrOK then begin
        exit;
      end;

      // 1) Collect + confirm deletions, but do not destroy yet. CreateProc /
      //    InsertHook are method pointers into the IDE component editor and
      //    must be used before any persistent is deleted.
      DeletionConfirmed := PrepareTagDeletions(DeletedList);

      // 2) Create the newly selected tags while all IDE callbacks are alive.
      Vars := Dlg.SelectedVars;
      Skipped := 0;
      // Keep the wizard (and the IDE) alive if a single tag cannot be
      // created. Previously any exception here bubbled out of the modal
      // dialog and Lazarus showed its uncaught "Access violation" dialog.
      try
        for i:=0 to High(Vars) do begin
          V := Vars[i];
          SymbolPath := V.Name;
          // Never recreate a tag that is already on the form. Tags queued
          // for deletion are ignored by TagExistsForPath.
          if TagExistsForPath(SymbolPath) then begin
            inc(Skipped);
            continue;
          end;
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
            // pttDefault IS the BOOL type for the Codesys symbol protocol: the
            // XML wizard maps bool -> pttDefault and TagTypeToIEC maps it to
            // IEC $00 / 1 byte. The old OPC UA code forced pttDefault to
            // pttDouble (LREAL, 8 bytes), which made every imported BOOL tag
            // read 8 bytes and corrupted writes. Keep the wizard mapping.
            if TypeValue<0 then
              TagNum.TagType := pttDefault
            else
              TagNum.TagType := TTagType(TypeValue);
            TagNum.RefreshTime := Dlg.DefaultPeriod;
            TagNum.ProtocolDriver := FDriver;
            InsertHook(TagNum);
          end;
        end;
      except
        on E: Exception do begin
          MessageDlg('Error creating tags: ' + E.Message, mtError, [mbOK], 0);
        end;
      end;
      if Skipped > 0 then
        MessageDlg(IntToStr(Skipped)+' variable(s) skipped.',mtInformation,[mbOK],0);
      // 3) Destroy unchecked tags LAST: no CreateProc / InsertHook call
      //    happens after this point in this wizard run.
      if DeletionConfirmed then begin
        Deleted := ApplyTagDeletions(DeletedList);
        if Deleted > 0 then
          MessageDlg('Removed ' + IntToStr(Deleted) + ' tag(s) no longer selected: ' +
            DeletedList, mtInformation, [mbOK], 0);
      end;
    finally
      UsedNames.Free;
      ImportedKeys.Free;
    end;
  finally
    ToDelete.Free;
    Dlg.Free;
  end;
end;

initialization
  SetTagBuilderToolForCodesysDriver(@OpenTagEditor);

end.
