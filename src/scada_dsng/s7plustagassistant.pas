{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação do Tag Builder dos CLPs S7-1200/1500 (S7CommPlus).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Lê as variáveis dos DBs diretamente do CLP e cria os tags no formulário/datamodule
  que contém o driver de protocolo.

  Como usar: com o TS7CommPlusDriver ligado a um TTCP_UDPPort @bold(ativo)
  (Active=true, o que também vale em tempo de projeto), clique com o botão direito no
  driver e escolha @italic(Tag Builder).

  Cada variável escolhida vira um TPLCTagNumber com LongAddress igual ao caminho
  simbólico, o TagType do tipo do CLP e os Swap* do big-endian (ver S7PlusTagPlan).
  O ProtocolDriver é atribuído por último, depois do endereço, para que o driver já
  enxergue o endereço do tag quando ele for registrado.

  As limitações (porta ativa, leitura bloqueante, o que não vira tag) estão
  documentadas na unit us7plustagbuilder.
}
{$ELSE}
{:
  @abstract(Tag Builder implementation of the S7-1200/1500 PLCs (S7CommPlus).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Reads the DB variables directly from the PLC and creates the tags on the
  form/datamodule that owns the protocol driver.

  How to use it: with the TS7CommPlusDriver linked to an @bold(active) TTCP_UDPPort
  (Active=true, which also applies at design time), right click the driver and choose
  @italic(Tag Builder).

  Each chosen variable becomes a TPLCTagNumber whose LongAddress is the symbolic path,
  with the TagType of the PLC type and the big-endian Swap* (see S7PlusTagPlan). The
  ProtocolDriver is assigned last, after the address, so the driver already sees the
  tag address when it gets registered.

  The limitations (active port, blocking read, what does not become a tag) are
  documented on the us7plustagbuilder unit.
}
{$ENDIF}
unit s7plustagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

implementation

uses
  Classes, SysUtils, Controls, Tag, ProtocolTypes, ProtocolDriver, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, PLCStruct, PLCStructElement, PLCStructString, PLCNumber, TagBit,
  S7PlusFamily, S7PlusTagPlan, us7plustagbuilder, us7plusbasketbuilder, hsstrings;

function UniqueComponentName(aOwner: TComponent; const aBaseName: String): String;
var
  aBase:String;
  c: LongInt;
begin
  aBase:=aBaseName;
  if not IsValidIdent(aBase) then
    aBase:='tag';

  Result:=aBase;

  if aOwner=nil then exit;

  c:=1;
  while aOwner.FindComponent(Result)<>nil do begin
    inc(c);
    Result:=aBase+IntToStr(c);
  end;
end;

//Creates the components of a plan on the form/datamodule. The plan names the byte item/element
//a TTagBit reads, so what was created is found again by the name it had in the plan (the real
//name can get a number to stay unique).
procedure CreateFromPlan(const plan:TS7PlusImportItems; aProtocolDriver, aOwnerOfNewTags:TComponent;
                         InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  c, k:LongInt;
  aOwner:TComponent;
  aNumber:TPLCTagNumber;
  aBlock:TPLCBlock;
  anElement:TPLCBlockElement;
  aString:TPLCString;
  aStruct:TPLCStruct;
  aItem:TPLCStructItem;
  aStructString:TPLCStructString;
  Made:TStringList;

  procedure CreateBits(aPlanItem:LongInt);
  var
    q, p:LongInt;
    aBit:TTagBit;
  begin
    for q:=0 to High(plan[aPlanItem].Bits) do begin
      if not Made.Find(plan[aPlanItem].Bits[q].SupportName, p) then continue;

      aBit:=TTagBit(CreateProc(TTagBit));

      aOwner:=aBit.Owner;
      if aOwner=nil then aOwner:=aOwnerOfNewTags;

      aBit.Name   := UniqueComponentName(aOwner, plan[aPlanItem].Bits[q].CompName);
      aBit.PLCTag := TPLCNumber(Made.Objects[p]);
      //EndBit first: StartBit cannot pass it
      aBit.EndBit   := plan[aPlanItem].Bits[q].BitIndex;
      aBit.StartBit := plan[aPlanItem].Bits[q].BitIndex;

      InsertHook(aBit);
    end;
  end;

begin
  Made:=TStringList.Create;
  try
    Made.CaseSensitive:=false;
    Made.Sorted:=true;
    Made.Duplicates:=dupIgnore;

    for c:=0 to High(plan) do begin
      Made.Clear;
      if plan[c].IsStruct then begin
        //an array of structures: one TPLCStruct over the whole array, with one item per member
        //of each element, each at its place in the bytes of the struct
        aStruct:=TPLCStruct(CreateProc(TPLCStruct));

        aOwner:=aStruct.Owner;
        if aOwner=nil then aOwner:=aOwnerOfNewTags;

        aStruct.Name           := UniqueComponentName(aOwner, plan[c].CompName);
        aStruct.LongAddress    := plan[c].Path;
        aStruct.Size           := plan[c].Size;
        aStruct.RefreshTime    := plan[c].Scan;
        aStruct.ProtocolDriver := TProtocolDriver(aProtocolDriver);

        InsertHook(aStruct);

        for k:=0 to High(plan[c].StructItems) do begin
          if plan[c].StructItems[k].IsString then begin
            aStructString:=TPLCStructString(CreateProc(TPLCStructString));

            aOwner:=aStructString.Owner;
            if aOwner=nil then aOwner:=aOwnerOfNewTags;

            aStructString.Name       := UniqueComponentName(aOwner, plan[c].StructItems[k].CompName);
            aStructString.PLCBlock   := aStruct;
            aStructString.Index      := plan[c].StructItems[k].Offset;
            aStructString.StringType := stSIEMENS;
            aStructString.StringSize := plan[c].StructItems[k].StringSize;

            InsertHook(aStructString);
          end else begin
            aItem:=TPLCStructItem(CreateProc(TPLCStructItem));

            aOwner:=aItem.Owner;
            if aOwner=nil then aOwner:=aOwnerOfNewTags;

            aItem.Name       := UniqueComponentName(aOwner, plan[c].StructItems[k].CompName);
            aItem.PLCBlock   := aStruct;
            aItem.Index      := plan[c].StructItems[k].Offset;
            aItem.TagType    := plan[c].StructItems[k].TagType;
            aItem.SwapBytes  := plan[c].StructItems[k].SwapBytes;
            aItem.SwapWords  := plan[c].StructItems[k].SwapWords;
            aItem.SwapDWords := plan[c].StructItems[k].SwapDWords;

            InsertHook(aItem);
            Made.AddObject(plan[c].StructItems[k].CompName, aItem);
          end;
        end;
        CreateBits(c);
        continue;
      end;

      if plan[c].IsBlock then begin
        //an array of scalars: one block of Size elements
        aBlock:=TPLCBlock(CreateProc(TPLCBlock));

        aOwner:=aBlock.Owner;
        if aOwner=nil then aOwner:=aOwnerOfNewTags;

        aBlock.Name           := UniqueComponentName(aOwner, plan[c].CompName);
        aBlock.LongAddress    := plan[c].Path;
        aBlock.TagType        := plan[c].TagType;
        aBlock.SwapBytes      := plan[c].SwapBytes;
        aBlock.SwapWords      := plan[c].SwapWords;
        aBlock.SwapDWords     := plan[c].SwapDWords;
        aBlock.Size           := plan[c].Size;
        aBlock.RefreshTime    := plan[c].Scan;
        aBlock.ProtocolDriver := TProtocolDriver(aProtocolDriver);

        InsertHook(aBlock);

        //the elements of the array, each one reading its place in the block
        for k:=0 to High(plan[c].ElementNames) do begin
          anElement:=TPLCBlockElement(CreateProc(TPLCBlockElement));

          aOwner:=anElement.Owner;
          if aOwner=nil then aOwner:=aOwnerOfNewTags;

          anElement.Name    := UniqueComponentName(aOwner, plan[c].ElementNames[k]);
          anElement.PLCBlock:= aBlock;
          anElement.Index   := k;

          InsertHook(anElement);
          Made.AddObject(plan[c].ElementNames[k], anElement);
        end;
        CreateBits(c);
        continue;
      end;

      if plan[c].IsString then begin
        //a STRING: the PLC keeps it as maximum length, current length and the characters
        aString:=TPLCString(CreateProc(TPLCString));

        aOwner:=aString.Owner;
        if aOwner=nil then aOwner:=aOwnerOfNewTags;

        aString.Name           := UniqueComponentName(aOwner, plan[c].CompName);
        aString.LongAddress    := plan[c].Path;
        aString.StringType     := stSIEMENS;
        aString.StringSize     := plan[c].StringSize;
        aString.RefreshTime    := plan[c].Scan;
        aString.ProtocolDriver := TProtocolDriver(aProtocolDriver);

        InsertHook(aString);
        continue;
      end;

      aNumber:=TPLCTagNumber(CreateProc(TPLCTagNumber));

      aOwner:=aNumber.Owner;
      if aOwner=nil then aOwner:=aOwnerOfNewTags;

      aNumber.Name           := UniqueComponentName(aOwner, plan[c].CompName);
      aNumber.LongAddress    := plan[c].Path;
      aNumber.TagType        := plan[c].TagType;
      aNumber.SwapBytes      := plan[c].SwapBytes;
      aNumber.SwapWords      := plan[c].SwapWords;
      aNumber.SwapDWords     := plan[c].SwapDWords;
      aNumber.RefreshTime    := plan[c].Scan;
      aNumber.ProtocolDriver := TProtocolDriver(aProtocolDriver);

      InsertHook(aNumber);
    end;

  finally
    Made.Free;
  end;
end;

//the classic wizard: ticks per mode
procedure OpenClassicEditor(aProtocolDriver,
                            aOwnerOfNewTags: TComponent;
                            InsertHook: TAddTagInEditorHook;
                            CreateProc: TCreateTagProc);
var
  frmTb:TfrmS7PlusTagBuilder;
begin
  frmTb:=TfrmS7PlusTagBuilder.Create(nil);
  try
    frmTb.Driver:=TS7CommPlusDriver(aProtocolDriver);

    if frmTb.ShowModal<>mrOK then exit;

    CreateFromPlan(frmTb.GetImportPlan, aProtocolDriver, aOwnerOfNewTags, InsertHook, CreateProc);
  finally
    frmTb.Destroy;
  end;
end;

//the basket wizard; its "classic wizard" button hands over to the old one
procedure OpenTagEditor(aProtocolDriver,
                        aOwnerOfNewTags: TComponent;
                        InsertHook: TAddTagInEditorHook;
                        CreateProc: TCreateTagProc);
var
  frmBk:TfrmS7PlusBasketBuilder;
  res:Integer;
begin
  if not (aProtocolDriver is TS7CommPlusDriver) then
    raise Exception.Create(SS7PTBOnlyS7PlusDriver);

  frmBk:=TfrmS7PlusBasketBuilder.Create(nil);
  try
    frmBk.Driver:=TS7CommPlusDriver(aProtocolDriver);

    res:=frmBk.ShowModal;
    if res=mrOK then
      CreateFromPlan(frmBk.GetImportPlan, aProtocolDriver, aOwnerOfNewTags, InsertHook, CreateProc);
  finally
    frmBk.Destroy;
  end;

  if res=mrRetry then
    OpenClassicEditor(aProtocolDriver, aOwnerOfNewTags, InsertHook, CreateProc);
end;

initialization
  SetTagBuilderToolForS7CommPlusProtocol(@OpenTagEditor);

end.
