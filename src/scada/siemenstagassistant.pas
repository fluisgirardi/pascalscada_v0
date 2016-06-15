{$i ../common/language.inc}
{:
  @abstract(Implementation of TagAssistant for Siemens.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  02/2014
  Changed to the old behavior (Right click on ISOTCP protocol ->
  Tag Builder to open the tag editor, whithout link with gui.
  ***********************************************************************
}
unit siemenstagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

implementation

uses
  Classes, SysUtils, ProtocolTypes, PLCTagNumber,PLCStructElement,
  us7tagbuilder, PLCBlockElement, PLCNumber, TagBit, plcblock, tag, Controls,
  PLCStruct, Dialogs, StrUtils, ProtocolDriver, s7family;

procedure OpenTagEditor(aProtocolDriver,
                        aOwnerOfNewTags: TComponent;
                        InsertHook: TAddTagInEditorHook;
                        CreateProc: TCreateTagProc);
var
  frmS7tb:TfrmS7TagBuilder;

  curitem, curstructitem,
  curaddress, curidx,
  curTCaddress, curbit, curdb:LongInt;

  block:TPLCBlock;
  item:TPLCNumber;
  bititem:TTagBit;


  morethanonedb, morethanoneitem:Boolean;

  started:Boolean;

  function GetCurWordSize:LongInt;
  var
    ttype:TTagType;
  begin
    if frmS7tb.optplcblock.Checked then
      ttype:=frmS7tb.CurBlockType
    else
      ttype:=frmS7tb.StructItem[curstructitem].TagType;

    case ttype of
      pttDefault, pttShortInt, pttByte:
        Result := 1;
      pttSmallInt, pttWord:
        Result := 2;
      pttLongInt, pttDWord, pttFloat:
        Result := 4;
    end;
  end;

  function GetValueWithZeros(value, endvalue:LongInt; toFill:Boolean):AnsiString;
  var
    numdig, dig:LongInt;
    strendval, fill:AnsiString;
  begin
    strendval:=IntToStr(endvalue);

    fill:='';
    numdig:=Length(strendval);
    for dig:=1 to numdig do
      fill:=fill+'0';

    if toFill then
      Result:=RightStr(fill+IntToStr(value),numdig)
    else
      Result:=IntToStr(value);
  end;

  function ReplaceBlockNamePattern(namepattern:AnsiString):AnsiString;
  var
    has_atleastonereplacement:Boolean;
  begin
    {$IFDEF PORTUGUES}
    {
    %db  - Numero do DB.
    %di  - Contador de DB atual, comecando de 1.
    %de  - Contador de DB atual, comecando de 0.
    %0db - Numero do DB, preenchido com zeros.
    %0di - Contador de DB atual, comecando de 1, preenchido com zeros.
    %0de - Contador de DB atual, comecando de 0, preenchido com zeros.
    }
    {$ELSE}
    {
    %db  - DB number.
    %di  - DB counter, starting from 1.
    %de  - DB counter, starting from 0.
    %0db - DB number, filled with zeroes.
    %0di - DB counter, starting from 1, filled with zeroes.
    %0de - DB counter, starting from 0, filled with zeroes.
    }
    {$ENDIF}

    has_atleastonereplacement:=(Pos('%db',namepattern)<>0) or
                               (Pos('%di',namepattern)<>0) or
                               (Pos('%de',namepattern)<>0) or
                               (Pos('%0db',namepattern)<>0) or
                               (Pos('%0di',namepattern)<>0) or
                               (Pos('%0de',namepattern)<>0);

    if (not has_atleastonereplacement) and morethanonedb then
      namepattern:=namepattern+'%di';

    Result:=namepattern;

    Result:= StringReplace(Result,'%db',IntToStr(curdb),[rfReplaceAll]);
    Result:= StringReplace(Result,'%di',IntToStr(curdb-frmS7tb.spinDBNumber.Value+1),[rfReplaceAll]);
    Result:= StringReplace(Result,'%de',IntToStr(curdb-frmS7tb.spinDBNumber.Value+0),[rfReplaceAll]);

    Result:= StringReplace(Result,'%0db',GetValueWithZeros(curdb,                              frmS7tb.spinFinalDBNumber.Value,                              true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0di',GetValueWithZeros(curdb-frmS7tb.spinDBNumber.Value+1, frmS7tb.spinFinalDBNumber.Value-frmS7tb.spinDBNumber.Value+1, true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0di',GetValueWithZeros(curdb-frmS7tb.spinDBNumber.Value,   frmS7tb.spinFinalDBNumber.Value-frmS7tb.spinDBNumber.Value  , true),[rfReplaceAll]);
  end;

  function GetItemName(namepattern:AnsiString):AnsiString;
  var
    has_atleastonereplacement,
    has_atleastoneDBreplacement:Boolean;
  begin
    {$IFDEF PORTUGUES}
    {
    %a    - Endereço do item
    %i    - Numero do item comecando de 1
    %e    - Numero do item comecando de 0
    %0a   - Endereço do item preenchido com zeros.
    %0i   - Numero do item comecando de 1, preenchido com zeros
    %0e   - Numero do item comecando de 0, preenchido com zeros
    }
    {$ELSE}
    {
    %a    - Item address
    %i    - Item number starting from 1.
    %e    - Item number starting from 0.
    %0a   - Item address filled with zeros.
    %0i   - Item number starting from 1, filled with zeroes.
    %0e   - Item number starting from 0, filled with zeroes.
    }
    {$ENDIF}
    has_atleastoneDBreplacement:=(Pos('%db',namepattern)<>0) or
                                 (Pos('%di',namepattern)<>0) or
                                 (Pos('%de',namepattern)<>0) or
                                 (Pos('%0db',namepattern)<>0) or
                                 (Pos('%0di',namepattern)<>0) or
                                 (Pos('%0de',namepattern)<>0);

    has_atleastonereplacement:=(Pos('%a',namepattern)<>0) or
                               (Pos('%i',namepattern)<>0) or
                               (Pos('%e',namepattern)<>0) or
                               (Pos('%0a',namepattern)<>0) or
                               (Pos('%0i',namepattern)<>0) or
                               (Pos('%0e',namepattern)<>0);

    if morethanonedb and (not has_atleastoneDBreplacement) then
      namepattern:=namepattern+'%di';

    if morethanoneitem and (not has_atleastonereplacement) then begin
      if morethanonedb then
        namepattern:=namepattern+'_%i'
      else
        namepattern:=namepattern+'%i';
    end;

    //replaces the block name patterns present on item names.
    Result:=ReplaceBlockNamePattern(namepattern);

    if frmS7tb.MemoryArea.ItemIndex in [4,9,5,10] then begin
      Result:= StringReplace(Result,'%a', IntToStr(curTCaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curTCaddress, frmS7tb.GetTheLastItemOffset div 2, true),[rfReplaceAll]);
    end else begin
      Result:= StringReplace(Result,'%a',IntToStr(curaddress),[rfReplaceAll]);
      Result:= StringReplace(Result,'%0a',GetValueWithZeros(curaddress, frmS7tb.RealEndOffset, true),[rfReplaceAll]);
    end;
    Result:= StringReplace(Result,'%i',IntToStr(curitem),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0i',GetValueWithZeros(curitem, frmS7tb.spinNumItens.Value, true),[rfReplaceAll]);
    Result:= StringReplace(Result,'%e',IntToStr(curitem-1),[rfReplaceAll]);
    Result:= StringReplace(Result,'%0e',GetValueWithZeros(curitem-1, frmS7tb.spinNumItens.Value-1, true),[rfReplaceAll]);
  end;

begin
  {$IFDEF PORTUGUES}
  { o que está faltando??
    NO FORMULARIO:
    ** Checagens de substituições ausentes nos nomes a fim de evitar duplicidades de nomes...

    SUBSTITUIÇÕES:

    %a  - Endereço do item
    %i  - Numero do item comecando de 1
    %e  - Numero do item comecando de 0
    %0a - Endereço do item preenchido com zeros.
    %0i - Numero do item comecando de 1, preenchido com zeros
    %0e - Numero do item comecando de 0, preenchido com zeros
  }
  {$ELSE}
  { What's missing??
    On form:
    ** Check of missing replacements to avoid name duplicity...

    REPLACEMENTS:

    %a  - Item address
    %i  - Item number starting from 1
    %e  - Item number starting from 0
    %0a - Item address filled with zeros.
    %0i - Item number starting from 1, filled with zeroes.
    %0e - Item number starting from 0, filled with zeroes.
  }
  {$ENDIF}

  if not (aProtocolDriver is TProtocolDriver) then
    raise Exception.Create('Protocol driver must be a instance of TProtocolDriver.');

  frmS7tb:=TfrmS7TagBuilder.Create(nil);
  try
    with frmS7tb do begin
      if ShowModal=mrOK then begin

        morethanonedb:=spinDBNumber.Value<>spinFinalDBNumber.Value;
        morethanoneitem:=spinNumItens.Value>1;

        for curdb:=spinDBNumber.Value to spinFinalDBNumber.Value do begin
          //cria o bloco simples ou bloco estrutura e faz sua configuração.
          //create the block or struture and configure it.
          if optplcblock.Checked or optplcStruct.Checked then begin

            //cria o bloco
            //creates the block
            if optplcblock.Checked then
              block:=TPLCBlock(CreateProc(TPLCBlock))
            else
              block:=TPLCStruct(CreateProc(TPLCStruct));

            block.PLCRack:=PLCRack.Value;
            block.PLCSlot:=PLCSlot.Value;
            block.PLCStation:=PLCStation.Value;
            block.MemReadFunction := GetTagType;
            block.Name := ReplaceBlockNamePattern(BlockName.Text);
            if block.MemReadFunction=4 then
              block.MemFile_DB:=curdb;
            block.MemAddress:=RealStartOffset;

            if optplcblock.Checked then begin
              block.RefreshTime:=BlockScan.Value;
              block.TagType:=CurBlockType;
              Block.SwapBytes:=BlockSwapBytes.Checked;
              block.SwapWords:=BlockSwapWords.Checked;
            end else
              block.RefreshTime:=StructScan.Value;

            block.ProtocolDriver:=TProtocolDriver(aProtocolDriver);
            InsertHook(block);
          end;

          //comeca a criar os itens da estrutura
          //creates the structure items
          curaddress:=spinStartAddress.Value;
          curTCaddress:=spinStartAddress.Value;
          curidx := 0;
          started:=false;
          for curitem:=1 to spinNumItens.Value do begin
            for curstructitem:=0 to StructItemsCount-1 do begin
              //se é para criar o tag.
              //if the tag must be created.
              if not StructItem[curstructitem].SkipTag then begin
                started:=true;
                if optplctagnumber.Checked then begin

                  item:=TPLCTagNumber(CreateProc(TPLCTagNumber));

                  with TPLCTagNumber(item) do begin

                    PLCRack:=frmS7tb.PLCRack.Value;
                    PLCSlot:=frmS7tb.PLCSlot.Value;
                    PLCStation:=frmS7tb.PLCStation.Value;
                    MemReadFunction := GetTagType;
                    if MemReadFunction=4 then
                      MemFile_DB:=curdb;
                    MemAddress:=curaddress;

                    RefreshTime:=StructItem[curstructitem].TagScan;
                    TagType:=StructItem[curstructitem].TagType;
                    SwapBytes:=StructItem[curstructitem].SwapBytes;
                    SwapWords:=StructItem[curstructitem].SwapWords;

                    ProtocolDriver:=TProtocolDriver(ProtocolDriver);
                  end;

                end else begin
                  if optplcblock.Checked then begin
                    TPLCBlock(block).Size:=curidx+1;
                    item:=TPLCBlockElement(CreateProc(TPLCBlockElement));
                    TPLCBlockElement(item).PLCBlock:=block;
                    TPLCBlockElement(item).Index:=curidx;
                  end else begin
                    item:=TPLCStructItem(CreateProc(TPLCStructItem));
                    TPLCStruct(block).Size:=curidx+GetCurWordSize;
                    TPLCStructItem(item).PLCBlock:=TPLCStruct(block);
                    TPLCStructItem(item).Index:=curidx;
                    TPLCStructItem(item).TagType:=StructItem[curstructitem].TagType;
                    TPLCStructItem(item).SwapBytes:=StructItem[curstructitem].SwapBytes;
                    TPLCStructItem(item).SwapWords:=StructItem[curstructitem].SwapWords;
                  end;
                end;

                item.Name:=GetItemName(StructItem[curstructitem].TagName);
                InsertHook(item);

                for curbit:=0 to StructItem[curstructitem].BitCount-1 do begin
                  bititem:=TTagBit(CreateProc(TTagBit));
                  bititem.EndBit:=StructItem[curstructitem].Bit[curbit].EndBit;
                  bititem.StartBit:=StructItem[curstructitem].Bit[curbit].StartBit;
                  bititem.Name:=GetItemName(StructItem[curstructitem].Bit[curbit].TagName);
                  bititem.PLCTag:=item;
                  InsertHook(bititem);
                end;
              end;

              inc(curTCaddress);
              inc(curaddress,GetCurWordSize);
              if started then begin
                if optplcblock.Checked then
                  inc(curidx)
                else
                  inc(curidx, GetCurWordSize);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    frmS7tb.Destroy;
  end;
end;

initialization
  SetTagBuilderToolForSiemensS7ProtocolFamily(@OpenTagEditor)

end.

