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
  UsedNames: TStringList;
  BaseName, TagName: string;
  N, Skipped: LongInt;

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

  Dlg := TfrmH5UTagBuilder.Create(nil);
  try
    Dlg.Station := 1;
    Dlg.DefaultPeriod := 100;
    if Dlg.ShowModal = mrOK then begin
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

            BaseName := H5UIdentFromVarName(V.Name,i+1);
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
end;

initialization
  SetTagBuilderToolForH5UTagProtocol(@OpenTagEditor);

end.
