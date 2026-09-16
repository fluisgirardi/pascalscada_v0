##### Introdução

Configurar dezenas de tags à mão — um endereço por vez, um índice por vez — é onde se perde tempo e se erram números. Para isso, várias propriedades numéricas dos componentes do PascalSCADA aceitam, no **Object Inspector**, uma **expressão** em vez de um número: você digita `memaddress+2`, `Tag*4` ou `selindex*30` e a IDE calcula o valor. Combinado com a **seleção múltipla** do Lazarus, isso configura uma centena de componentes em uma digitação.

É um recurso de **tempo de projeto**: a expressão é avaliada quando você pressiona Enter e o que fica gravado no `.lfm` é o número resultante. Em tempo de execução nada muda — para valores calculados em tempo de execução, use o [`TNumericExprTag`](/pb/tags/#TNumericExprTag).

O recurso foi anunciado no post [Expressões em todas as propriedades dos tags base](/pb/2016/11/22/expressions-on-all-properties-of-base-tags/); o vídeo abaixo mostra o uso:

https://www.youtube.com/watch?v=1E1MeYGuH8s

##### Como digitar

No Object Inspector, no campo da propriedade, digite a expressão e pressione Enter. Duas formas:

| Você digita | O que acontece |
|---|---|
| `100` | Valor normal. |
| `10*8+3` | Expressão **absoluta**: a propriedade recebe o resultado (83). |
| `+2`, `-10`, `*2`, `/2` | Expressão **relativa**: começa com um operador, e o valor atual da propriedade é colocado na frente. `+2` em um `MemAddress = 40` dá 42; `*2` dá 80. |
| `memaddress+2` | Expressão com **variáveis** (a lista de cada propriedade está abaixo). |

O resultado é sempre inteiro: divisões são truncadas (`7/2` → 3). Com a seleção múltipla, a expressão é avaliada **uma vez para cada componente selecionado**, com as variáveis daquele componente — é isso que permite `memaddress+100` deslocar dez tags de uma vez, cada um a partir do seu próprio endereço.

##### Sintaxe

O avaliador é o `TFPExpressionParser` do Free Pascal, com as funções matemáticas ligadas:

* operadores: `+ - * / ^` (potência), `mod`, parênteses;
* comparação e lógica: `= <> < > <= >=`, `and`, `or`, `not`;
* `if(condição, seVerdadeiro, seFalso)` e `case(seletor, v1, r1, v2, r2, …, padrão)`;
* funções: `abs`, `sqr`, `sqrt`, `round`, `trunc`, `int`, `frac`, `exp`, `ln`, `log`, `sin`, `cos`, `arctan`, `pi`;
* os nomes das variáveis não diferenciam maiúsculas de minúsculas (`MemAddress` e `memaddress` são a mesma coisa).

Não existe `div`: use `trunc(a/b)` (ou só `a/b`, que já é truncado ao gravar).

##### Onde funciona e quais variáveis existem

**Propriedades de endereço dos tags** — em `TPLCTagNumber`, `TPLCBlock`, `TPLCStruct` e `TPLCString`, as propriedades `PLCRack`, `PLCSlot`, `PLCStation`, `MemFile_DB`, `MemAddress`, `MemSubElement`, `MemReadFunction`, `MemWriteFunction` e, nos blocos e estruturas, `Size`. Variáveis disponíveis: **as outras propriedades dessa lista** (`plcrack`, `plcslot`, `plcstation`, `memfile_db`, `memaddress`, `memsubelement`, `memreadfunction`, `memwritefunction`) e **`Tag`** (a propriedade `Tag` do componente, um inteiro livre). A própria propriedade que está sendo editada não entra, para não haver referência circular.

**`Index`** — em `TPLCBlockElement`, `TPLCStructItem` e `TPLCStructString`. Variável: **`Tag`**. (No `TPLCBlockElement`, a lista suspensa do campo também oferece os índices válidos do bloco.)

**Posição e tamanho de qualquer controle** — `Left`, `Top`, `Width` e `Height` de todo `TControl` do form, não só dos controles do PascalSCADA. Variáveis: `left`, `top`, `width`, `height`, `tag` e **`selindex`**.

**`Tag`** — a propriedade `Tag` de qualquer `TComponent`. Variáveis: as mesmas de posição (`left`… se for um controle) e `selindex`.

**`selindex`** é o índice do componente dentro da seleção atual (0 para o primeiro selecionado, 1 para o segundo…). É a variável que transforma a seleção múltipla em numeração automática.

##### Receitas

Todas partem de uma seleção múltipla no form ou no datamodule (Shift+clique, ou arrastando uma moldura).

**Numerar os elementos de um bloco.** Selecione os 16 `TPLCBlockElement` criados à mão, na ordem desejada:

1. em `Tag`, digite `selindex` → cada um recebe 0, 1, 2… 15;
2. em `Index`, digite `Tag` → `Index` = 0, 1, 2… 15.

O mesmo vale para `TPLCStructItem` com offsets regulares: `Index` = `Tag*2` para campos `INT`, `Tag*4` para `REAL`.

**Tags consecutivos.** Dez `TPLCTagNumber` que devem apontar para D100, D101, … D109: selecione os dez, `Tag` = `selindex`, depois `MemAddress` = `100+Tag`.

**Deslocar um conjunto de endereços.** O DB mudou de lugar e todos os tags precisam avançar 50 bytes: selecione-os e digite `+50` em `MemAddress`. Trocou o DB: `MemFile_DB` = `12` na seleção inteira.

**Struct copiada.** Você copiou os itens de uma `TPLCStruct` para representar um segundo equipamento cuja estrutura fica 64 bytes adiante: selecione os itens copiados e digite `+64` em `Index`.

**Escrita coerente com a leitura.** Em tags Modbus, `MemWriteFunction` = `if(memreadfunction=1, 15, 16)` escolhe a função de escrita de bloco certa para cada tag, conforme ele lê coils ou registradores.

**Alinhar controles.** Selecione dez `THMILabel` e digite `selindex*24+8` em `Top` e `16` em `Left`: uma coluna com 24 px de passo. Uma grade de botões 4×4: `Left` = `(selindex mod 4)*90`, `Top` = `trunc(selindex/4)*40`.

**Empilhar controles de tamanhos diferentes.** `Top` = `selindex*(height+4)` só funciona quando todos têm a mesma altura; para alturas diferentes, alinhe em duas passadas ou use o *Anchor Editor* do Lazarus.

##### Limites

* Só no Object Inspector do Lazarus: não vale para código, nem para propriedades editadas em outros diálogos (Tag Builder, editores de coleção).
* As variáveis leem o valor **atual** dos outros componentes selecionados; uma expressão que dependa de uma propriedade que você ainda vai alterar precisa ser digitada depois dela.
* Só propriedades inteiras listadas acima. `RefreshTime`, `MinValue`, `Value1` das zonas e outras não aceitam expressões (por enquanto — a lista está em `src/scada_dsng/scadareg.pas` e `src/hmi/hmiregister.pas`, e estendê-la é registrar mais um `TTagAddressPropertyEditor`).
* Um erro de sintaxe é reportado pela IDE e a propriedade não muda.

##### Exemplos relacionados

* `examples/laz_isotcp_demonstration` e `examples/laz_isotcp_hourmeter` — dezenas de `TPLCBlockElement` e `TTagBit` com índices sequenciais, o cenário típico das receitas acima.
* `examples/TagTypes` — elementos e itens de estrutura com offsets calculados.
