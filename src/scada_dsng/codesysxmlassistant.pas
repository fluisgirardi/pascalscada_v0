{$i ../common/language.inc}
{:
  @abstract(Design-time Tag Builder for importing Codesys symbol XML into OPC UA tags.)

  This is the SECOND Tag Builder of the OPC UA driver. It is registered in a
  separate hook (SetTagBuilderToolForCodesysXMLProtocol) so the existing
  online-browse wizard (opcuatagassistant) is left untouched. Switching the
  driver's TagBuilderMode property selects which wizard OpenTagEditor opens.

  The wizard reads a Codesys Symbolconfiguration XML, and creates the selected
  TPLCTagNumber / TPLCBlock / TPLCString components bound to the OPC UA driver,
  with LongAddress set to the OPC UA NodeId derived from the symbol path.
}
unit codesysxmlassistant;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, OPCUADriver, ProtocolTypes, ucodesysxmlbuilder, codesysxml;

type
  { TCodesysXmlAssistant }
  TCodesysXmlAssistant = class
  private
    FDriver: TOPCUADriver;
  public
    procedure OpenTagEditor(OwnerOfNewTags: TComponent;
      InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
  published
    property Driver: TOPCUADriver read FDriver write FDriver;
  end;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);

implementation

uses
  PLCTagNumber, PLCBlock, PLCString, Tag, Dialogs, Controls;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  Wizard: TCodesysXmlAssistant;
begin
  if not (aProtocolDriver is TOPCUADriver) then
    raise Exception.Create('An OPC UA driver is required as protocol.');

  Wizard := TCodesysXmlAssistant.Create;
  try
    Wizard.Driver := TOPCUADriver(aProtocolDriver);
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
  BaseName, TagName, NodeId, TypeName, DeletedList: string;
  Skipped, Deleted: LongInt;
  C: TComponent;
  ToDelete: TList;

  {: NodeId of an existing tag component on the form, or '' when not an OPC UA tag. }
  function TagNodeIdOf(Cmp: TComponent): string;
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

  {: Deletes OPC UA tags on this driver whose NodeId matches a variable that
     was unchecked in the wizard. Only NodeIds present in the current XML and
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
        NodeId := CodesysVarNodeId(DVars[j],Dlg.Namespace,Dlg.PathPrefix);
        for m:=0 to OwnerOfNewTags.ComponentCount-1 do begin
          C := OwnerOfNewTags.Components[m];
          if (TagNodeIdOf(C) = NodeId) and TagBelongsToDriver(C) and
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
    // Collect NodeIds of tags already on the form so imported rows are marked.
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
            NodeId := TagNodeIdOf(C);
            if NodeId <> '' then ImportedKeys.Add(LowerCase(Dlg.PathPrefix)+'|'+
              IntToStr(Dlg.Namespace)+'|'+Copy(NodeId,Pos(';s=',NodeId)+3,MaxInt));
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
        NodeId := CodesysVarNodeId(V,Dlg.Namespace,Dlg.PathPrefix);
        TypeName := CodesysVarTypeToTagTypeName(V.TypeClass,V.IECName);
        BaseName := CodesysIdentFromVarName(V.Name,i+1);
        TagName := MakeUniqueName(BaseName);
        UsedNames.Add(TagName);

        if TypeName = 'text' then begin
          TagStr := TPLCString(CreateProc(TPLCString));
          TagStr.Name := TagName;
          TagStr.LongAddress := NodeId;
          TagStr.StringType := stC;
          TagStr.StringSize := 80;
          TagStr.RefreshTime := Dlg.DefaultPeriod;
          TagStr.ProtocolDriver := FDriver;
          InsertHook(TagStr);
        end else begin
          // Like the OPC UA online-browse wizard: scalars become TPLCTagNumber
          // with TagType left at pttDefault so the driver keeps the data type
          // reported by the server (see TOPCUADriver.ListTags/GetVariableInfo).
          TagNum := TPLCTagNumber(CreateProc(TPLCTagNumber));
          TagNum.Name := TagName;
          TagNum.LongAddress := NodeId;
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
  SetTagBuilderToolForCodesysXMLProtocol(@OpenTagEditor);

end.
