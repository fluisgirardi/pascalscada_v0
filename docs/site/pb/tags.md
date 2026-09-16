##### Introdução

No PascalSCADA, um **tag** é o componente que representa uma memória (ou um conjunto de memórias) de um equipamento: uma *holding register* de um inversor Modbus, um byte de um DB de um CLP Siemens, um controller tag de um CLP Rockwell. O tag fica no meio da cadeia de comunicação:

```
Porta de comunicação  →  Driver de protocolo  →  Tag  →  Controle HMI / seu código
```

A porta transporta bytes, o driver sabe falar o protocolo do equipamento, e o **tag diz ao driver o que ler e escrever**, guarda o último valor lido e avisa quem depende dele quando esse valor muda. Os controles da biblioteca HMI (`THMIEdit`, `THMILabel`, `THMICheckBox`…) nunca falam com o driver: eles se ligam a um tag pela propriedade `PLCTag` e reagem às mudanças dele. O mesmo tag pode alimentar quantos controles você quiser.

Todos os tags ficam na paleta **PascalSCADA Tags** e descendem de `TTag` → `TPLCTag`. A partir daí eles se dividem em dois grupos:

* **Tags numéricos** (`TPLCNumber`): expõem um único valor `Double` nas propriedades `Value` e `ValueRaw`. São eles: `TPLCTagNumber`, `TPLCBlockElement`, `TPLCStructItem`, `TTagBit` e `TNumericExprTag`.
* **Tags de bloco** (`TTagBlock`): mapeiam várias memórias consecutivas em uma única requisição. São eles: `TPLCBlock`, `TPLCStruct` e `TPLCString`.

A tabela abaixo resume quando usar cada um:

| Componente | Use quando… |
|---|---|
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png) `TPLCTagNumber` | precisa de **um** valor numérico, endereçado e lido individualmente. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png) `TPLCBlock` + ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblockelement.png) `TPLCBlockElement` | precisa de **vários valores consecutivos do mesmo tipo** (ex.: 20 registradores a partir do endereço 100). O bloco faz uma requisição só; cada elemento expõe um item do bloco como tag numérico. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png) `TPLCStruct` + ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstructitem.png) `TPLCStructItem` + ![](img/TPLCStructString.png) `TPLCStructString` | precisa de **vários valores de tipos diferentes** que estão juntos na memória (uma `STRUCT`/UDT de um CLP, um DB Siemens com bytes, words e reais misturados). |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png) `TPLCString` | precisa ler ou escrever um **texto**. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/tagbit.png) `TTagBit` | precisa de **um ou mais bits** de outro tag numérico, sem gerar nova comunicação. |
| ![](img/TNumericExprTag.png) `TNumericExprTag` | precisa de um valor **calculado** a partir de outros tags, sem comunicação própria. |

##### Como um tag é lido e escrito

Cada tag de comunicação tem quatro operações, disponíveis em código:

| Método | Direção | Comportamento |
|---|---|---|
| `ScanRead` | leitura | **Assíncrona.** Enfileira uma requisição de leitura no driver e retorna imediatamente. O valor chega depois, no evento `OnValueChange`/`OnUpdate`. |
| `Read` | leitura | **Síncrona.** Bloqueia até o driver responder (ou falhar). Ao retornar, `Value` já está atualizado e `LastSyncReadStatus` diz o resultado. |
| `ScanWrite` | escrita | **Assíncrona.** Enfileira a escrita e retorna. O resultado vai para `LastASyncWriteStatus` e para os eventos `OnWriteOk`/`OnWriteFail`. |
| `Write` | escrita | **Síncrona.** Bloqueia até a escrita ser confirmada. |

Na prática você raramente chama esses métodos. O que controla o comportamento do dia a dia são três propriedades:

* **`AutoRead`** — se `True` (padrão), o driver inclui o tag no seu ciclo de varredura (*scan*) e o lê automaticamente a cada `RefreshTime` milissegundos. Se `False`, o tag só é lido quando você chamar `Read` ou `ScanRead`.
* **`RefreshTime`** — período de atualização do tag em milissegundos (padrão 1000). Tags no mesmo driver com o mesmo `RefreshTime` e endereços vizinhos são agrupados pelo driver em uma única requisição sempre que o protocolo permitir, então **não há penalidade em usar muitos tags pequenos**; use `TPLCBlock` quando quiser garantir que os dados cheguem juntos, como um único snapshot.
* **`AutoWrite`** — se `True` (padrão), toda atribuição a `Value` (por código ou por um controle HMI) gera uma escrita no equipamento. Se `False`, a atribuição só muda o valor em memória; o valor vai para o equipamento quando você chamar `WriteDirect` (bloco) ou `Write`/`ScanWrite`.
* **`SyncWrites`** — se `True`, as escritas geradas por `AutoWrite` são síncronas (`Write`) em vez de assíncronas (`ScanWrite`). Deixe `False` a não ser que o código que atribuiu o valor precise ter certeza de que ele já chegou ao equipamento.

> A regra geral é: **deixe `AutoRead` e `AutoWrite` em `True` e não chame nada** — o driver cuida de tudo. Desligue `AutoRead` só para tags que precisam ser lidos em um momento específico (por exemplo, uma receita carregada sob demanda), e chame `Read` nesse momento.

##### Endereçamento

Todo tag de comunicação tem o mesmo conjunto de propriedades de endereço. **O significado de cada uma depende do driver de protocolo**, e a página de cada protocolo explica como preenchê-las. Como referência rápida:

| Propriedade | Modbus | S7 (ISOTCP) | Melsec | West ASCII / IBox | Logix / S7CommPlus |
|---|---|---|---|---|---|
| `PLCStation` | Endereço do escravo (1–247) | – | Número da estação | Endereço do controlador | – |
| `PLCRack` / `PLCSlot` | – | Rack e slot da CPU | – | – | – |
| `MemFile_DB` | – | Número do DB (área DB) | – | – | – |
| `MemReadFunction` | Função Modbus de leitura (1, 2, 3, 4) | Área de memória (1 = I, 2 = Q, 3 = M, 4 = DB…) | Tipo de dispositivo (M, SM, L, X, Y, D…) | – | – |
| `MemWriteFunction` | Função Modbus de escrita (5, 6, 15, 16) | – | Tipo de dispositivo (mesmo código da leitura) | – | – |
| `MemAddress` | Endereço do registrador/coil | Endereço do byte | Endereço do dispositivo | Parâmetro (West) / PID (IBox) | – |
| `MemSubElement` | – | – (bits: use `TTagBit`) | – | – | – |
| `LongAddress` | – | – | – | – | Nome simbólico do tag no CLP (`Motor1.Speed`, `"DB10".Setpoint`) |
| `Retries` | Tentativas antes de declarar falha (todos os drivers) | | | | |

A forma mais rápida de acertar o endereçamento é o **Tag Builder**: clique com o botão direito no componente do driver de protocolo (`TModBusTCPDriver`, `TISOTCPDriver`…) e escolha *Tag Builder*. Ele cria os tags já configurados para aquele protocolo.

`ProtocolDriver` é a propriedade que liga o tag ao driver. Um tag sem driver não faz nada — nem gera erro.

##### Tipo de dado

A propriedade **`TagType`** diz como o driver deve interpretar os bytes lidos do equipamento:

| `TagType` | Tamanho | Interpretação |
|---|---|---|
| `pttDefault` | depende do driver | O "tamanho natural" do protocolo: 16 bits no Modbus, 8 bits no S7, etc. |
| `pttShortInt` / `pttByte` | 8 bits | Inteiro com / sem sinal |
| `pttSmallInt` / `pttWord` | 16 bits | Inteiro com / sem sinal |
| `pttLongInt` / `pttDWord` | 32 bits | Inteiro com / sem sinal |
| `pttFloat` | 32 bits | Ponto flutuante IEEE-754 (REAL) |
| `pttInt64` / `pttQWord` | 64 bits | Inteiro com / sem sinal |
| `pttDouble` | 64 bits | Ponto flutuante IEEE-754 (LREAL) |

Quando o tamanho do `TagType` é diferente do tamanho natural do protocolo, o tag ocupa mais de uma memória: um `pttFloat` em Modbus ocupa dois registradores de 16 bits consecutivos. `TagSizeOnProtocol` (somente leitura) informa quantas.

Se o valor chega "embaralhado" (um `REAL` que aparece como um número absurdo, um `DINT` que troca as metades), o equipamento usa uma ordem de bytes diferente da que o driver assume. Ajuste com:

* **`SwapBytes`** — inverte os dois bytes de cada palavra de 16 bits.
* **`SwapWords`** — inverte as duas palavras de 16 bits de um valor de 32 bits.
* **`SwapDWords`** — inverte as duas palavras de 32 bits de um valor de 64 bits.

Para Modbus, a combinação mais comum para `REAL` em equipamentos que usam a ordem "CDAB" é `SwapWords = True`.

##### Valor, escala e limites

Tags numéricos têm dois valores:

* **`ValueRaw`** — o valor exatamente como veio do equipamento.
* **`Value`** — o valor depois de passar pela cadeia de escalas ligada em `ScaleProcessor`. Se não houver escala, `Value = ValueRaw`.

Os controles HMI e o seu código normalmente usam `Value`. Para converter, por exemplo, um inteiro 0–27648 de uma entrada analógica Siemens em 0–100 %, ligue um `TLinearScaleProcessor` em `ScaleProcessor` — veja [Processadores de escala](/pb/scale-processors/). A escala funciona nos dois sentidos: escrever `50` em `Value` grava `13824` no equipamento.

**`MinValue`** / **`MaxValue`**, ativados por **`EnableMinValue`** / **`EnableMaxValue`**, limitam o que pode ser *escrito* no tag. Uma atribuição fora da faixa levanta uma exceção ("valor fora dos limites"), dispara `OnWriteFail` e o valor não vai para o equipamento. Os controles de entrada (`THMIEdit`, `THMITrackBar`, `THMIUpDown`) respeitam esses limites.

##### Eventos

Os eventos abaixo existem em todos os tags de comunicação. Todos rodam na *thread principal*, então é seguro mexer em controles visuais dentro deles.

| Evento | Quando dispara |
|---|---|
| `OnValueChangeFirst` | O valor mudou, **antes** de os controles HMI dependentes serem notificados. Use para preparar algo que os controles vão mostrar. |
| `OnValueChangeLast` (ou `OnValueChange`) | O valor mudou, **depois** de os controles serem notificados. É o evento "normal" de mudança de valor. |
| `OnUpdate` | O tag foi lido com sucesso, **mesmo que o valor não tenha mudado**. Use para "sinal de vida" ou para registrar históricos em intervalo fixo. |
| `OnReadOK` / `OnReadFail` | Resultado de cada leitura. |
| `OnWriteOk` / `OnWriteFail` | Resultado de cada escrita. |
| `OnAsyncValueChange` | Versão do `OnValueChange` chamada **na thread do driver**, sem sincronização com a thread principal. Só use se souber o que está fazendo: é mais rápida, mas não pode tocar em controles visuais. |

Para saber o que aconteceu em uma falha, leia `LastSyncReadStatus`, `LastSyncWriteStatus`, `LastASyncReadStatus` ou `LastASyncWriteStatus` (tipo `TProtocolIOResult`: `ioOk`, `ioTimeOut`, `ioIllegalFunction`, `ioIllegalRegAddress`, `ioDriverError`, `ioCommError`, …).

##### Diagnóstico

Cada tag mantém contadores que aparecem no Object Inspector em tempo de execução e ajudam a encontrar problemas de rede: `CommReadsOK`, `CommReadErrors`, `CommWritesOk`, `CommWriteErrors`. `AvgUpdateRate` mostra o intervalo médio real entre atualizações — se ele for muito maior que `RefreshTime`, o driver não está dando conta do volume de tags e você deve aumentar o `RefreshTime` dos tags menos importantes ou agrupá-los em blocos. `ClockMonotonicTimeStamp` guarda o instante da última atualização.

`TagGUID` é um identificador único gerado pelo IDE para cada tag. Não edite.

##### TPLCTagNumber {#TPLCTagNumber}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png) É o tag mais usado: uma memória, um valor. Configure `ProtocolDriver`, as propriedades de endereço e `TagType`, ligue os controles HMI e pronto.

Em código:

```pascal
// Leitura: o valor já está no tag, atualizado pelo scan.
Velocidade := PLCTagNumber1.Value;

// Escrita: com AutoWrite = True, a atribuição já grava no equipamento.
PLCTagNumber1.Value := 1500;

// Forçando uma leitura síncrona agora, independente do scan.
PLCTagNumber1.Read;
if PLCTagNumber1.LastSyncReadStatus = ioOk then
  ShowMessage(FloatToStr(PLCTagNumber1.Value));
```

##### TPLCBlock e TPLCBlockElement {#TPLCBlock}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png) `TPLCBlock` mapeia `Size` memórias consecutivas do mesmo `TagType`, começando em `MemAddress`. Todas são lidas e escritas em uma única requisição, o que garante que os valores sejam coerentes entre si (o mesmo instante) e reduz o tráfego quando o protocolo tem overhead alto por mensagem.

Os valores ficam em `ValueRaw[índice]` (de `0` a `Size-1`) ou no array `ValuesRaw`. Um `TPLCBlock` sozinho não pode ser ligado a um controle HMI — para isso existe o ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblockelement.png) **`TPLCBlockElement`**: um tag numérico cuja propriedade `PLCBlock` aponta para o bloco e `Index` diz qual elemento ele representa. O elemento tem tudo que um `TPLCTagNumber` tem (`Value`, `ScaleProcessor`, limites, eventos), mas não tem endereço próprio nem `RefreshTime`: quem comunica é o bloco.

Para não criar os elementos um a um, clique com o botão direito no bloco e escolha **Map block elements…** — o assistente cria um `TPLCBlockElement` para cada índice.

Escrita em bloco:

```pascal
// Com AutoWrite = True, cada atribuição gera uma escrita de um elemento.
PLCBlock1.ValueRaw[3] := 10;

// Para escrever o bloco inteiro de uma vez, desligue AutoWrite,
// atribua os valores e chame WriteDirect (síncrono) ou WriteByScan (assíncrono).
PLCBlock1.AutoWrite := False;
PLCBlock1.ValueRaw[0] := 1;
PLCBlock1.ValueRaw[1] := 2;
PLCBlock1.ValueRaw[2] := 3;
PLCBlock1.WriteDirect;
```

##### TPLCStruct, TPLCStructItem e TPLCStructString {#TPLCStruct}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png) `TPLCStruct` é um `TPLCBlock` cujo `TagType` é fixo em `pttByte`: ele lê `Size` **bytes** consecutivos e deixa que cada item decida como interpretá-los. É a forma de mapear uma estrutura com campos de tipos diferentes — um DB Siemens com `BOOL`, `INT`, `REAL` e `STRING` misturados, ou uma UDT.

Os campos são representados por:

* ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstructitem.png) **`TPLCStructItem`** — um campo numérico. `PLCBlock` aponta para a estrutura, `Index` é o **offset em bytes** do campo dentro dela e `TagType` diz o tipo do campo (o item ocupa o número de bytes correspondente). `SwapBytes`/`SwapWords`/`SwapDWords` valem por item.
* ![](img/TPLCStructString.png) **`TPLCStructString`** — um campo texto. `Index` é o offset, `StringSize` o tamanho máximo em caracteres, `StringType` o formato (`stC` ou `stSIEMENS`, veja `TPLCString` abaixo) e `StringEncoding` a página de código do equipamento.

Exemplo — um DB Siemens com esta estrutura:

| Offset | Tipo | Nome |
|---|---|---|
| 0 | `INT` | Contador |
| 2 | `REAL` | Temperatura |
| 6 | `STRING[20]` | Nome do produto (22 bytes com o cabeçalho Siemens) |

fica: `TPLCStruct` com `Size = 28`; `TPLCStructItem` com `Index = 0, TagType = pttSmallInt`; outro com `Index = 2, TagType = pttFloat`; e um `TPLCStructString` com `Index = 6, StringSize = 20, StringType = stSIEMENS`.

O assistente **Map structure items…** (botão direito na estrutura) cria os itens a partir de uma lista de tipos que você monta na tela, calculando os offsets automaticamente.

Assim como no bloco, `AutoWrite = False` + `WriteDirect` grava a estrutura inteira em uma única requisição.

##### TPLCString {#TPLCString}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png) Lê e escreve um texto do equipamento. O valor fica na propriedade `Value` (`UTF8String`) e os controles `THMIEdit`/`THMILabel` mostram e editam texto quando ligados a um `TPLCString`.

* **`StringSize`** — quantidade máxima de caracteres.
* **`StringType`** — como a string é armazenada no equipamento:
    * `stC` — os caracteres seguem até encontrar um byte `0` (terminador C). Use para Modbus, Melsec e para CLPs Rockwell (apontando `LongAddress` para o membro `DATA` da `STRING`, ex.: `MinhaString.DATA` — é o que o assistente de importação de tags Rockwell faz).
    * `stSIEMENS` — o formato `STRING` do S7: dois bytes de cabeçalho (tamanho máximo e tamanho usado) seguidos dos caracteres. O tag ocupa `StringSize + 2` bytes.
    * `stROCKWELL` — reservado, ainda não implementado; devolve string vazia.
* **`StringEncoding`** — página de código do equipamento (`UTF_8`, `CP1252`, `CP850`…). O tag converte de/para UTF-8 automaticamente.

`OnAsyncStringChange` é o equivalente de `OnAsyncValueChange` para strings.

##### TTagBit {#TTagBit}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/tagbit.png) Extrai um intervalo de bits de outro tag numérico (`PLCTag`), sem gerar comunicação adicional. `StartBit` é o bit menos significativo e `EndBit` o mais significativo do intervalo (ambos a partir de 0, máximo 31; `EndBit >= StartBit`). O valor do `TTagBit` é o intervalo deslocado para a direita:

| Valor do tag origem | `StartBit` | `EndBit` | Valor do `TTagBit` |
|---|---|---|---|
| 5 (`101b`) | 0 | 0 | 1 |
| 5 (`101b`) | 0 | 2 | 5 |
| 5 (`101b`) | 1 | 2 | 2 (`10b`) |
| 5 (`101b`) | 2 | 2 | 1 |

Escrever no `TTagBit` funciona: ele lê o valor atual do tag origem, altera só os bits do intervalo e escreve o resultado de volta — então com `AutoWrite = True` o bit vai para o equipamento.

`UseRawValue` diz se os bits vêm de `ValueRaw` (`True`) ou de `Value` (`False`, padrão). Ligue quando o tag origem tiver escala.

O uso típico é uma palavra de status ou de comando do CLP em que cada bit tem um significado: um `TPLCTagNumber` lê a palavra e um `TTagBit` por bit alimenta os `THMICheckBox`. O assistente **Map bits** (botão direito em qualquer tag numérico) cria os 8, 16 ou 32 `TTagBit` de uma vez. O exemplo `examples/laz_isotcp_mapping_bits_from_other_tag` mostra isso com um byte de DB Siemens.

##### TNumericExprTag {#TNumericExprTag}

![](img/TNumericExprTag.png) Um tag numérico virtual: o valor é o resultado de `Expression`, avaliada sempre que um dos tags ligados às variáveis `A` a `J` mudar. Não tem driver nem endereço.

```
Expression = 'A * B / 100'
Expression = 'ifthen(A > 80, 1, 0)'
Expression = 'sqrt(sqr(A) + sqr(B))'
```

A sintaxe é a do `TFPExpressionParser` do Free Pascal (operadores aritméticos, comparação, `and`/`or`/`not`, funções matemáticas, e a função `ifthen(condição, seVerdadeiro, seFalso)` adicionada pelo PascalSCADA). Um erro na expressão fica em `LastEvalutionError`. Pode ter `ScaleProcessor`. Veja a página [Tag de expressão numérica](/pb/numeric-expression-tag/) para mais exemplos.

##### Criando tags em tempo de execução

Tags são `TComponent`, então podem ser criados por código — útil para aplicações que carregam a configuração de um banco de dados:

```pascal
uses PLCTagNumber, ProtocolTypes;

var
  t: TPLCTagNumber;
begin
  t := TPLCTagNumber.Create(Self);
  t.Name := 'Nivel_Tanque1';
  t.MemReadFunction := 3;    // Modbus: holding register
  t.MemWriteFunction := 16;
  t.MemAddress := 100;
  t.PLCStation := 1;
  t.TagType := pttFloat;
  t.SwapWords := True;
  t.RefreshTime := 500;
  t.ProtocolDriver := ModBusTCPDriver1;   // ligue o driver por último
  t.OnValueChange := @NivelMudou;
end;
```

Ligue `ProtocolDriver` depois de configurar o endereço: ao receber o driver, o tag se registra no scan dele com o endereço que tem naquele momento. Para remover, basta `t.Free` — o tag se desregistra do driver e dos controles ligados a ele.

##### Exemplos relacionados

* `examples/TagTypes` — o mesmo bloco Modbus lido com todos os `TagType` (`pttByte` a `pttDouble`) e as combinações de `SwapBytes`/`SwapWords`, com `TPLCBlockElement` e `TPLCStructItem` lado a lado; o melhor lugar para ver o efeito de cada tipo.
* `examples/laz_isotcp_mapping_bits_from_other_tag` — um `TPLCTagNumber` `pttByte` e um `TTagBit` por bit.
* `examples/laz_numericexpr` — `TNumericExprTag` somando dois tags dirigidos por `THMITrackBar`.
* `examples/laz_modbus_tcp_example` — `TPLCBlock` + `TPLCBlockElement`, `TPLCTagNumber` e controles de entrada sobre Modbus TCP.
* `examples/laz_console_app` — tags e driver Modbus TCP em uma aplicação de console, sem LCL (um datamodule sem forms).
* `examples/hmi_bandeja` — `TPLCStruct` com `TPLCStructItem` e `TPLCStructString` mapeando um DB Siemens.
