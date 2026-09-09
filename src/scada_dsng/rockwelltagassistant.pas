{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação do Tag Builder dos CLPs Rockwell Compact/ControlLogix.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Lê a lista de tags diretamente do CLP (Ethernet/IP - CIP) e cria os tags
  correspondentes no formulário/datamodule que contém o driver de protocolo.

  Como usar: com o TLGXDriver ligado a um TTCP_UDPPort @bold(ativo)
  (Active=true, o que também vale em tempo de projeto), clique com o botão
  direito no driver e escolha @italic(Tag Builder).

  O que é criado para cada tag do CLP:

  @unorderedList(
    @item(Tag simples (DINT, REAL, BOOL, LINT, ...): um TPLCTagNumber com o
          TagType equivalente e Size=1.)
    @item(Array (@code(Receita[10])): um TPLCBlock com o TagType do elemento e
          Size igual ao número de elementos.)
    @item(STRING: um TPLCString com StringType=stC, LongAddress apontando para
          o membro DATA da estrutura e StringSize igual ao tamanho do DATA
          menos um.)
    @item(Membro de UDT: um tag individual, endereçado pelo caminho simbólico
          (@code(Tanque.nivel)).)
  )

  Em todos os casos o ProtocolDriver é atribuído por último, depois do
  LongAddress e do tamanho, para que o driver já enxergue o endereço do tag
  quando ele for registrado.

  As limitações do assistente (porta ativa, leitura bloqueante, arrays de BOOL
  e UDTs não importados, STRING lida pelo membro DATA) estão documentadas na
  unit ulgxtagbuilder.
}
{$ELSE}
{:
  @abstract(Tag Builder implementation of the Rockwell Compact/ControlLogix PLCs.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Reads the tag list directly from the PLC (Ethernet/IP - CIP) and creates the
  matching tags on the form/datamodule that owns the protocol driver.

  How to use it: with the TLGXDriver linked to an @bold(active) TTCP_UDPPort
  (Active=true, which also applies at design time), right click the driver and
  choose @italic(Tag Builder).

  What is created for each PLC tag:

  @unorderedList(
    @item(Simple tag (DINT, REAL, BOOL, LINT, ...): a TPLCTagNumber with the
          matching TagType and Size=1.)
    @item(Array (@code(Recipe[10])): a TPLCBlock with the TagType of the
          element and Size equal to the element count.)
    @item(STRING: a TPLCString with StringType=stC, LongAddress pointing to the
          DATA member of the structure and StringSize equal to the DATA size
          minus one.)
    @item(UDT member: an individual tag, addressed by its symbolic path
          (@code(Tank.level)).)
  )

  On every case the ProtocolDriver is assigned last, after the LongAddress and
  the size, so the driver already sees the tag address when it gets registered.

  The wizard limitations (active port, blocking read, BOOL arrays and UDTs not
  imported, STRING read through the DATA member) are documented on the
  ulgxtagbuilder unit.
}
{$ENDIF}
unit rockwelltagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

implementation

uses
  Classes, SysUtils, Controls, Tag, ProtocolTypes, ProtocolDriver, PLCTagNumber,
  PLCBlock, PLCString, LGXDriver, ulgxtagbuilder, hsstrings;

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
  frmLGXtb:TfrmLGXTagBuilder;
  plan:TLGXImportItems;
  c:LongInt;
  aOwner:TComponent;

  aNumber:TPLCTagNumber;
  aBlock:TPLCBlock;
  aString:TPLCString;
begin
  if not (aProtocolDriver is TLGXDriver) then
    raise Exception.Create(SLGXTBOnlyLGXDriver);

  frmLGXtb:=TfrmLGXTagBuilder.Create(nil);
  try
    frmLGXtb.Driver:=TLGXDriver(aProtocolDriver);

    if frmLGXtb.ShowModal<>mrOK then exit;

    plan:=frmLGXtb.GetImportPlan;

    for c:=0 to High(plan) do begin
      case plan[c].Kind of

        //-----------------------------------------------------------------
        //tags simples: TPLCTagNumber
        //-----------------------------------------------------------------
        lgxtkNumber: begin
          aNumber:=TPLCTagNumber(CreateProc(TPLCTagNumber));

          aOwner:=aNumber.Owner;
          if aOwner=nil then aOwner:=aOwnerOfNewTags;

          aNumber.Name           := UniqueComponentName(aOwner, plan[c].CompName);
          aNumber.LongAddress    := plan[c].TagPath;
          aNumber.TagType        := plan[c].TagType;
          aNumber.RefreshTime    := plan[c].Scan;
          aNumber.ProtocolDriver := TProtocolDriver(aProtocolDriver);

          InsertHook(aNumber);
        end;

        //-----------------------------------------------------------------
        //arrays: TPLCBlock
        //-----------------------------------------------------------------
        lgxtkBlock: begin
          aBlock:=TPLCBlock(CreateProc(TPLCBlock));

          aOwner:=aBlock.Owner;
          if aOwner=nil then aOwner:=aOwnerOfNewTags;

          aBlock.Name           := UniqueComponentName(aOwner, plan[c].CompName);
          aBlock.LongAddress    := plan[c].TagPath;
          aBlock.TagType        := plan[c].TagType;
          aBlock.Size           := plan[c].Size;
          aBlock.RefreshTime    := plan[c].Scan;
          aBlock.ProtocolDriver := TProtocolDriver(aProtocolDriver);

          InsertHook(aBlock);
        end;

        //-----------------------------------------------------------------
        //STRINGs: TPLCString lendo o membro DATA da estrutura.
        //-----------------------------------------------------------------
        lgxtkString: begin
          aString:=TPLCString(CreateProc(TPLCString));

          aOwner:=aString.Owner;
          if aOwner=nil then aOwner:=aOwnerOfNewTags;

          aString.Name           := UniqueComponentName(aOwner, plan[c].CompName);
          aString.LongAddress    := plan[c].TagPath;
          aString.StringType     := stC;
          aString.StringSize     := plan[c].Size;
          aString.RefreshTime    := plan[c].Scan;
          aString.ProtocolDriver := TProtocolDriver(aProtocolDriver);

          InsertHook(aString);
        end;
      end;
    end;
  finally
    frmLGXtb.Destroy;
  end;
end;

initialization
  SetTagBuilderToolForRockwellLogixProtocol(@OpenTagEditor);

end.
