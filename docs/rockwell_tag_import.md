# Importação de tags de CLPs Rockwell Compact/ControlLogix

Assistente de tempo de projeto que lê a lista de tags direto do CLP, por
Ethernet/IP (CIP), e cria os tags do PascalSCADA correspondentes no
formulário ou datamodule.

*Design time wizard that reads the tag list straight from the PLC over
Ethernet/IP (CIP) and creates the matching PascalSCADA tags on the form or
datamodule. This document is in Portuguese; the API documentation in
`ulgxtagbuilder.pas`, `rockwelltagassistant.pas` and `lgxdriver.pas` is
bilingual.*

## Como usar

1. Coloque no formulário um `TTCP_UDPPort` e um `TLGXDriver`, e ligue o driver
   à porta (`CommunicationPort`).
2. Configure o IP/porta do CLP e ligue a porta: **`Active := true`**.
3. Clique com o botão direito no `TLGXDriver` e escolha **Tag Builder**.
4. Clique em **Ler a lista de tags do CLP**. O CLP é navegado e a lista aparece
   na grade.
5. Marque os tags desejados, ajuste o tempo de atualização e o prefixo dos
   nomes, e clique em **Importar**.

Os componentes são criados no dono do driver (o formulário/datamodule), com
nome único e saneado, `LongAddress` com o caminho simbólico do tag no CLP e o
`ProtocolDriver` já apontando para o `TLGXDriver`.

### Filtros da tela

| Controle | Efeito |
|---|---|
| Filtro (texto) | mostra só os tags cujo nome contém o texto |
| Tipo | Todos / Numéricos / Arrays / Strings |
| Mostrar elementos de arrays | mostra também `Receita[0]`, `Receita[1]`, ... além do array inteiro |
| Mostrar membros de estruturas | mostra os membros de UDT (`Tanque.nivel`) |
| Mostrar tags de programa | mostra os tags de escopo de programa (`Program:Main.Passo`) |
| STRINGs como TPLCString | trata as estruturas STRING como texto; desligado, expõe os membros `LEN` e `DATA` crus |

Marcar/desmarcar preserva a escolha ao trocar de filtro: um tag marcado que
sai da lista por causa de um filtro continua marcado e entra na importação.
Isso permite filtrar por `motor`, marcar, filtrar por `bomba`, marcar mais, e
importar tudo de uma vez.

A exceção é **STRINGs como TPLCString**, que é uma opção de mapeamento e não um
filtro de exibição: desligada, nenhum `TPLCString` é criado, mesmo que a
estrutura tivesse sido marcada antes.

## Mapeamento aplicado

| No CLP | No projeto |
|---|---|
| escalar (`DINT`, `REAL`, `BOOL`, `LINT`, ...) | `TPLCTagNumber` com o `TagType` equivalente, `Size=1` |
| array (`REAL[10]`) | `TPLCBlock` com o `TagType` do elemento, `Size` = nº de elementos |
| `STRING` | `TPLCString`, `StringType=stC`, `LongAddress` = `<tag>.DATA`, `StringSize` = tamanho do `DATA` − 1 |
| membro de UDT | tag individual, endereçado pelo caminho simbólico (`Tanque.nivel`) |

Tipos CIP → `TTagType`: `BOOL`/`USINT` → `pttByte`, `SINT` → `pttShortInt`,
`INT` → `pttSmallInt`, `UINT` → `pttWord`, `DINT` → `pttLongInt`,
`UDINT`/`DWORD` → `pttDWord`, `LINT` → `pttInt64`, `ULINT` → `pttQWord`,
`REAL` → `pttFloat`, `LREAL` → `pttDouble`.

## Limitações conhecidas

### A porta de comunicação precisa estar ativa

A leitura é **online**: o assistente conversa com o CLP na hora. Se
`CommunicationPort` for `nil` ou estiver inativa, o assistente avisa e não lê
nada.

Isso funciona dentro da IDE porque o `TTCP_UDPPort` não é um dispositivo
exclusivo (`ExclusiveDevice=false`), e portanto `Active := true` abre a conexão
de verdade em tempo de projeto. Com o `TSerialPortDriver`, que é exclusivo,
isso não valeria — mas o driver Logix é Ethernet/IP, então não é o caso.

### A navegação é síncrona e trava a IDE

`TLGXDriver.BrowseTagList` pausa as threads de scan do driver, conversa com o
CLP e só retorna quando termina. Num CLP com muitos tags isso leva vários
segundos, e nesse tempo a IDE fica parada (o cursor vira ampulheta).

A lista fica em cache por porta de comunicação, então a segunda abertura do
assistente é instantânea. Marque **Reler do CLP** para descartar o cache e
navegar de novo — útil depois de baixar um programa novo no CLP.

*Melhoria pendente: navegar em uma thread, com progresso e cancelamento
(veja `pascalscada.todo`).*

### Arrays de BOOL não são importados

No Logix, `BOOL[32]` é armazenado empacotado em `DWORD`s, e o `TLGXDriver`
ainda não desempacota os bits na leitura. Importar geraria um `TPLCBlock` que
lê valores errados, então esses tags entram na contagem de **ignorados** em vez
de virarem tags quebrados.

`BOOL` escalar é importado normalmente (o CLP devolve 1 byte).

*Melhoria pendente: desempacotar os bits no driver e então criar um
`TPLCBlock` de `DWORD`s mais os `TTagBit` correspondentes.*

### UDTs não são importadas inteiras

Não existe hoje um tag do PascalSCADA que represente uma UDT completa lida por
caminho simbólico. A UDT em si é ignorada, e os seus membros aparecem
individualmente na lista, cada um com o seu caminho (`Tanque.nivel`,
`Tanque.valvula`). Cada membro marcado vira um tag próprio, com uma leitura
própria.

*Melhoria pendente: importar UDTs como `TPLCStruct` + `TPLCStructItem`.*

### STRING é lida pelo membro DATA

Uma `STRING` do Logix é uma estrutura com dois membros: `LEN` (um `DINT` com o
tamanho usado) e `DATA` (um `SINT[n]` com os caracteres).

O `TPLCString` tem o valor `stROCKWELL` no enum `TPLCStringTypes`, mas ele
**não está implementado**: não há codificação nem decodificação para esse
formato, e um `TPLCString` configurado com ele devolve string vazia.

Por isso o assistente aponta o `LongAddress` para o membro `DATA` e usa
`stC`: o Logix preenche o `DATA` com zeros depois do último caractere, que é
exatamente o terminador que o `stC` espera. Funciona hoje, sem depender de
código novo.

O efeito colateral é que o `LEN` não é usado — o tamanho vem do terminador. Se
a aplicação gravar bytes não-zero além do texto direto no `DATA`, a leitura vai
além do fim lógico da string.

*Melhoria pendente: implementar `stROCKWELL` no `TPLCString` e passar a apontar
o `LongAddress` para a estrutura inteira.*

### Tipos internos do CLP

Tags marcados com `TYPE_IS_SYSTEM` são descartados sem aparecer na lista.

## Usando a lista de tags em runtime

O assistente é só uma casca em cima de uma API pública do driver, que também
serve para código de aplicação:

```pascal
var
  tags:TLGXTagInfoArray;
  c:Integer;
  tt:TTagType;
begin
  if LGXDriver1.BrowseTagList(tags) then
    for c:=0 to High(tags) do
      writeln(tags[c].name, ' : ', LGXTypeName(tags[c].aType),
              ' [', tags[c].elem_count, ']');
end;
```

- `BrowseTagList(out aTagList; ForceReload)` — navega e devolve a lista.
- `ClearTagListCache` — descarta o cache da porta atual.
- `LGXTypeName`, `LGXTypeToTagType`, `LGXTypeIsStruct`, `LGXTypeIsSystem`,
  `LGXTypeDimensions`, `LGXTypeSizeInBytes` — decodificação dos tipos CIP.

## Arquivos

| Arquivo | Papel |
|---|---|
| `src/scada/lgxdriver.pas` | `BrowseTagList`, `ClearTagListCache`, helpers de tipo CIP, gancho do Tag Builder |
| `src/scada_dsng/ulgxtagbuilder.pas` / `.lfm` | a tela do assistente e a classificação dos tags |
| `src/scada_dsng/rockwelltagassistant.pas` | criação dos componentes de tag no formulário |
| `src/scada_dsng/scadareg.pas` | registro do assistente no pacote de design-time |
