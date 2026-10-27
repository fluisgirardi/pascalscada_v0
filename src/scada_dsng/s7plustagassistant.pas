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
  PLCBlock, PLCBlockElement, S7PlusFamily, S7PlusTagPlan, us7plustagbuilder, hsstrings;

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

procedure OpenTagEditor(aProtocolDriver,
                        aOwnerOfNewTags: TComponent;
                        InsertHook: TAddTagInEditorHook;
                        CreateProc: TCreateTagProc);
var
  frmTb:TfrmS7PlusTagBuilder;
  plan:TS7PlusImportItems;
  c:LongInt;
  aOwner:TComponent;
  aNumber:TPLCTagNumber;
  aBlock:TPLCBlock;
  anElement:TPLCBlockElement;
  k:LongInt;
begin
  if not (aProtocolDriver is TS7CommPlusDriver) then
    raise Exception.Create(SS7PTBOnlyS7PlusDriver);

  frmTb:=TfrmS7PlusTagBuilder.Create(nil);
  try
    frmTb.Driver:=TS7CommPlusDriver(aProtocolDriver);

    if frmTb.ShowModal<>mrOK then exit;

    plan:=frmTb.GetImportPlan;

    for c:=0 to High(plan) do begin
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
        end;
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
    frmTb.Destroy;
  end;
end;

initialization
  SetTagBuilderToolForS7CommPlusProtocol(@OpenTagEditor);

end.
