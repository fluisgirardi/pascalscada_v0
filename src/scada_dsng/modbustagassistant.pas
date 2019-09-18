{$i ../common/language.inc}
{:
  @abstract(Implementation of TagAssistant for ModBus.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
unit modbustagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, modbusdriver, ProtocolTypes, uModbusTagBuilder;

type

  { TModBusTagAssistant }

  TModBusTagAssistant = class
  private
    FDriver: TModBusDriver;
    function SelectedReadFuntion(dlg:TfrmModbusTagBuilder):LongInt;
    function SelectedWriteFuntion(dlg:TfrmModbusTagBuilder):LongInt;
    function SeekFirstItem(LastItem:TTagNamesItemEditor):TTagNamesItemEditor;
    function BuildItemName(nameprefix:AnsiString; ZeroFill:Boolean; index, NumZeros:LongInt):AnsiString;
    public
      //: Opens the Tag Builder of the ModBus protocol driver
      procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
        CreateProc:TCreateTagProc);

    published
      property Driver:TModBusDriver read FDriver write FDriver;
  end;

  procedure OpenTagEditor(aProtocolDriver,
                          aOwnerOfNewTags: TComponent;
                          InsertHook: TAddTagInEditorHook;
                          CreateProc: TCreateTagProc);

implementation

uses
  PLCBlockElement, ValueProcessor, PLCTagNumber, PLCString,
  PLCBlock, Controls, Dialogs, hsstrings, ProtocolDriver;

procedure OpenTagEditor(aProtocolDriver,
                        aOwnerOfNewTags: TComponent;
                        InsertHook: TAddTagInEditorHook;
                        CreateProc: TCreateTagProc);
var
  wizard: TModBusTagAssistant;
begin
  if not (aProtocolDriver is TModBusDriver) then
    raise exception.Create('A Modbus RTU/TCP driver required as protocol.');

  wizard:=TModBusTagAssistant.Create;
  try
    wizard.Driver:=TModBusDriver(aProtocolDriver);
    wizard.OpenTagEditor(aOwnerOfNewTags,InsertHook,CreateProc);
  finally
    FreeAndNil(wizard);
  end;
end;

{ TModBusTagAssistant }

procedure TModBusTagAssistant.OpenTagEditor(OwnerOfNewTags:TComponent;
  InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  dlg:TfrmModbusTagBuilder;
  tplc:TPLCTagNumber;
  tstr:TPLCString;
  tblk:TPLCBlock;
  tbel:TPLCBlockElement;
  c, CurMemtAdress, nameitem, BlockNo, Element:LongInt;
  confItem:TTagNamesItemEditor;
  knowstringsize:boolean;
  tstrdummy:TPLCString;
  defaultstringsize:LongInt;
  count:LongInt;
  ItemName:Strings;
  ItemPtr:array of TComponent;
begin

  if not Assigned(FDriver) then
    begin
      ShowMessage(SDriverRequired);
      Exit;
    end;

  count:=1;
  SetLength(ItemName,1);
  SetLength(ItemPtr, 1);
  ItemName[0]:='(none)';
  ItemPtr[0] :=nil;
  for c:=0 to OwnerOfNewTags.ComponentCount-1 do begin
    if OwnerOfNewTags.Components[c] is TScaleProcessor then begin
      inc(count);
      SetLength(ItemName,count);
      SetLength(ItemPtr, count);
      ItemName[count-1]:= OwnerOfNewTags.Components[c].Name;
      ItemPtr[count-1] := OwnerOfNewTags.Components[c];
    end;
  end;

  dlg:=TfrmModbusTagBuilder.Create(ItemName);
  try
    if dlg.ShowModal=mrOk then begin
      if Assigned(InsertHook) and Assigned(CreateProc) then begin

        knowstringsize:=false;

        CurMemtAdress:=dlg.FirstMemAddress.Value;

        if dlg.optStartFromZero.Checked then
          nameitem:=0
        else
          nameitem:=1;

        confItem:=SeekFirstItem(dlg.CurItem);
        if confItem=nil then exit;

        ////////////////////////////////////////////////////////////////////////
        //plcnumber and string
        ////////////////////////////////////////////////////////////////////////
        if dlg.optPLCTagNumber.Checked or dlg.optPLCString.Checked then begin
          c:=1;
          while c<=dlg.MemCount.Value do begin
            if Trim(confItem.Nome.Text)<>'' then begin
              if dlg.optPLCTagNumber.Checked then begin
                tplc := TPLCTagNumber(CreateProc(TPLCTagNumber));
                tplc.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tplc.MemAddress := CurMemtAdress;
                tplc.MemReadFunction  := SelectedReadFuntion(dlg);
                tplc.MemWriteFunction := SelectedWriteFuntion(dlg);
                tplc.PLCStation:=dlg.StationAddress.Value;
                tplc.RefreshTime:=confItem.Scan.Value;
                tplc.ProtocolDriver := FDriver;
                if confItem.PIPES.ItemIndex<>-1 then
                  tplc.ScaleProcessor:=TPIPE(ItemPtr[confItem.PIPES.ItemIndex]);
                InsertHook(tplc);
              end else begin
                tstr := TPLCString(CreateProc(TPLCString));
                tstr.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tstr.MemAddress := CurMemtAdress;
                tstr.MemReadFunction  := SelectedReadFuntion(dlg);
                tstr.MemWriteFunction := SelectedWriteFuntion(dlg);
                tstr.PLCStation:=dlg.StationAddress.Value;
                tstr.RefreshTime:=confItem.Scan.Value;
                tstr.ProtocolDriver := FDriver;
                if dlg.optSTR_C.Checked then
                  tstr.StringType:=stC
                else
                  tstr.StringType:=stSIEMENS;
                tstr.ByteSize:=Byte(dlg.ByteSize.Value);
                tstr.StringSize:=dlg.MaxStringSize.Value;
                InsertHook(tstr);

                if not knowstringsize then begin
                  knowstringsize:=true;
                  defaultstringsize:=tstr.Size;
                end;
              end;
            end;

            if dlg.optPLCTagNumber.Checked then
              inc(CurMemtAdress)
            else begin
              //se o tamanho do bloco da string ainda não é conhecido.
              //if the string size is unknown.
              if not knowstringsize then begin
                try
                  tstrdummy := TPLCString.Create(Nil);
                  tstrdummy.MemAddress := CurMemtAdress;
                  tstrdummy.MemReadFunction  := SelectedReadFuntion(dlg);
                  tstrdummy.MemWriteFunction := SelectedWriteFuntion(dlg);
                  tstrdummy.PLCStation:=dlg.StationAddress.Value;
                  tstrdummy.RefreshTime:=confItem.Scan.Value;
                  tstrdummy.ProtocolDriver := FDriver;
                  if dlg.optSTR_C.Checked then
                    tstrdummy.StringType:=stC
                  else
                    tstrdummy.StringType:=stSIEMENS;
                  tstrdummy.ByteSize:=Byte(dlg.ByteSize.Value);
                  tstrdummy.StringSize:=dlg.MaxStringSize.Value;
                  defaultstringsize:=tstrdummy.Size;
                  knowstringsize:=true;
                finally
                  tstrdummy.Destroy;
                end;
              end;
              inc(CurMemtAdress,defaultstringsize);
            end;

            if (Trim(confItem.Nome.Text)<>'') or confItem.CountEmpty.Checked  then
              inc(c);

            if confItem.Next=nil then begin
              confItem:=SeekFirstItem(dlg.CurItem);
              inc(nameitem);
            end else
              confItem:=TTagNamesItemEditor(confItem.Next);
          end;
        end;

        ////////////////////////////////////////////////////////////////////////
        //TPLCBlock
        ////////////////////////////////////////////////////////////////////////
        if dlg.optPLCBlock.Checked then begin
          c:=1;
          BlockNo:=1;
          while c<=dlg.MemCount.Value do begin
            Element:=0;
            tblk := TPLCBlock(CreateProc(TPLCBlock));
            tblk.Name:=BuildItemName(dlg.NameOfEachBlock.Text, false, BlockNo, 9);
            tblk.MemAddress := CurMemtAdress;
            tblk.MemReadFunction  := SelectedReadFuntion(dlg);
            tblk.MemWriteFunction := SelectedWriteFuntion(dlg);
            tblk.PLCStation:=dlg.StationAddress.Value;
            tblk.RefreshTime:=dlg.ScanOfEachBlock.Value;
            tblk.Size := 1;
            tblk.ProtocolDriver := FDriver;
            InsertHook(tblk);

            //cria os elementos do bloco
            //create the block elements.
            while Element<dlg.MaxBlockSize.Value do begin
              if Trim(confItem.Nome.Text)<>'' then begin
                tbel := TPLCBlockElement(CreateProc(TPLCBlockElement));
                tbel.Name:=BuildItemName(confItem.Nome.Text,confItem.ZeroFill.Checked,nameitem,confItem.QtdDigitos.Value);
                tbel.PLCBlock := tblk;
                tblk.Size := Element+1;
                tbel.Index:=Element;
                if confItem.PIPES.ItemIndex<>-1 then
                  tbel.ScaleProcessor:=TPIPE(ItemPtr[confItem.PIPES.ItemIndex]);
                InsertHook(tbel);
              end;

              inc(Element);
              inc(CurMemtAdress);

              if (Trim(confItem.Nome.Text)<>'') or confItem.CountEmpty.Checked  then
                inc(c);

              if confItem.Next=nil then begin
                confItem:=SeekFirstItem(dlg.CurItem);
                inc(nameitem);
              end else
                confItem:=TTagNamesItemEditor(confItem.Next);
            end;

            //incrementa o numero do bloco.
            //inc the block number.
            inc(BlockNo);
          end;
        end;
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;




function TModBusTagAssistant.SelectedReadFuntion(dlg:TfrmModbusTagBuilder):LongInt;
begin
  Result:=0;
  if dlg.Type1.Checked then
    Result:=1;
  if dlg.Type2.Checked then
    Result:=2;
  if dlg.Type3.Checked then
    Result:=3;
  if dlg.Type4.Checked then
    Result:=4;
end;

function TModBusTagAssistant.SelectedWriteFuntion(dlg:TfrmModbusTagBuilder):LongInt;
begin
  Result := 0;
  case SelectedReadFuntion(dlg) of
    1: begin
      if dlg.optSimpleFunctions.Checked then
        Result:=5
      else
        Result := 15;
    end;
    3: begin
      if dlg.optSimpleFunctions.Checked then
        Result := 6
      else
        Result := 16;
    end;
  end;
end;

function TModBusTagAssistant.SeekFirstItem(LastItem:TTagNamesItemEditor):TTagNamesItemEditor;
begin
  Result := LastItem;
  while (Result<>nil) and (Result.Prior<>nil) do
    Result:=TTagNamesItemEditor(Result.Prior);
end;

function TModBusTagAssistant.BuildItemName(nameprefix: AnsiString;
  ZeroFill: Boolean; index, NumZeros: LongInt): AnsiString;
var
  idxfmt, numfmt:AnsiString;
  c:LongInt;
begin
  if ZeroFill then begin
    idxfmt:='0';
    for c:= 2 to NumZeros do
      idxfmt:=idxfmt+'0';
  end
  else
    idxfmt:='#0';

  numfmt:=FormatFloat(idxfmt,index);

  if Pos('%s',nameprefix)=0 then
    Result:=nameprefix+numfmt
  else
    Result := Format(nameprefix,[numfmt]);
end;

initialization
  SetTagBuilderToolForModBusProtocolFamily(@OpenTagEditor)

end.

