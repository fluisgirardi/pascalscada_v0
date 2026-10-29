# DBs de teste para o driver S7CommPlus / Test DBs for the S7CommPlus driver

Fontes (formato "Gerar blocos a partir de fonte" do TIA Portal) para montar, num PLC ou no PLCSim,
os dados que os testes ao vivo do driver S7CommPlus e do Tag Builder precisam. Todo valor tem um
**valor inicial conhecido**, para que um teste possa conferir a leitura sem escrever antes.

Sources (TIA Portal "Generate blocks from source" format) to build, on a PLC or on the PLCSim,
the data the live tests of the S7CommPlus driver and of the Tag Builder need. Every value has a
**known start value**, so a test can check a read without writing first.

## Arquivos / Files

| Arquivo | O que é |
|---|---|
| `UDT_TestMotor.udt` | Tipo (UDT) usado em arrays e como membro / UDT used in arrays and as a member |
| `TestDB_Opt.db` | DB **otimizado** com todas as situações de leitura/escrita / **optimized** DB with every read/write situation |
| `TestDB_NonOpt.db` | O **mesmo conteúdo**, DB **não otimizado** (só muda `S7_Optimized_Access`) / same content, **non-optimized** (only `S7_Optimized_Access` differs) |
| `TestDB_Big.db` | Valores maiores que uma PDU (1024 bytes) / values bigger than one PDU |
| `TestDB_Dyn.db` | Valores que o próprio CLP muda e comandos que o teste escreve / values the PLC changes and commands the test writes |
| `FC_TestDynamics.scl` | Faz `TestDB_Dyn` mudar sozinho / makes `TestDB_Dyn` change by itself |
| `FC_TestPattern.scl` | Carrega os valores por elemento dos arrays de estruturas (a declaração só dá o mesmo valor a todos) / loads the per element values of the arrays of structures |

## Como importar / How to import

1. No TIA Portal: *Fontes externas* → *Adicionar nova fonte externa* → escolha os 7 arquivos. / *External source files* → *Add new external file* → pick the 7 files.
2. Botão direito em cada fonte → *Gerar blocos a partir da fonte* (o TIA ordena sozinho: UDT, DBs, FCs). / right-click each source → *Generate blocks from source* (TIA orders them: UDT, DBs, FCs).
3. No OB1 (Main) chame as duas FCs, em qualquer ordem: / In OB1 (Main) call both FCs, in any order:
   `"FC_TestPattern"();` e `"FC_TestDynamics"();`
4. Baixe para o PLC/PLCSim e coloque em RUN. / Download to the PLC/PLCSim and go RUN.
5. Os números dos DBs são os que o TIA escolher: os testes usam os **nomes simbólicos**. / The DB numbers are whatever TIA picks: the tests use the **symbolic names**.

Para repor os valores depois de um teste que escreveu: escreva `TRUE` em `"TestDB_Dyn".LoadPattern`
(o FC_TestPattern recarrega os arrays de estruturas; os demais valores o teste devolve ao que escreveu).
To restore after a test that wrote: write `TRUE` to `"TestDB_Dyn".LoadPattern`.

## Valores esperados / Expected values

### `TestDB_Opt` e `TestDB_NonOpt` (idênticos / identical)

| Situação / Situation | Variáveis / Variables | Valor inicial / Start value |
|---|---|---|
| Bool em sequência (compartilham o byte) / consecutive Bools | `Bool_1`, `Bool_2`, `Bool_3` | `true`, `false`, `true` |
| Bool depois de tipos maiores / Bool after bigger types | `Bool_4` | `true` |
| Byte / Char / SInt / USInt | `Byte_1`, `Char_1`, `SInt_1`, `USInt_1` | `16#12`, `'A'`, `-5`, `200` |
| Word / Int / UInt (2 bytes) | `Word_1`, `Int_1`, `UInt_1` | `16#1234`, `-1234`, `60000` |
| DWord / DInt / UDInt / Real (4 bytes) | `DWord_1`, `DInt_1`, `UDInt_1`, `Real_1` | `16#DEADBEEF`, `-123456`, `4000000000`, `3.14` |
| LWord / LInt / ULInt / LReal (8 bytes) | `LWord_1`, `LInt_1`, `ULInt_1`, `LReal_1` | `16#0123456789ABCDEF`, `-1234567890123`, `12345678901234`, `2.718281828` |
| Tipos que **não** viram tag / types that are **not** tags | `Time_1`, `Date_1`, `TimeOfDay_1`, `DTL_1`, `WString_1` | `T#1s500ms`, `D#2024-01-01`, `TOD#10:20:30`, `DTL#2024-05-06-07:08:09.123`, `'wide'` |
| STRING 10 / 50 / 254 / vazia | `String_Short`, `String_Mid`, `String_Default`, `String_Empty` | `'hello'`, `'middle string value'`, `'default text'`, `''` |
| Array de DInt | `Arr_DInt[0..9]` | `100..109` |
| Array de Real | `Arr_Real[0..4]` | `0.5, 1.5, 2.5, 3.5, 4.5` |
| Array de Byte | `Arr_Byte[0..15]` | `0..15` |
| Array de Bool | `Arr_Bool[0..7]` | `T,F,T,F,T,F,T,F` |
| Array com limite inferior negativo / negative lower bound | `Arr_NegLow[-2..2]` | `-20,-10,0,10,20` |
| Array de LReal | `Arr_LReal[0..1]` | `1.25, 2.5` |
| Array de 100 Words (200 bytes) | `Arr_Word100[0..99]` | `1000..1099` |
| Array de STRING (não vira tag) / array of STRINGs (not a tag) | `Arr_Str[0..2]` | `'str0'`, `'str1'`, `'str2'` |
| Array de 2 dimensões (não vira tag) / 2D array | `Arr_2D[0..2,0..3]` | `11..14, 21..24, 31..34` |
| Estrutura mista, ordem de declaração ≠ ordem real do CLP / mixed structure | `Mix.v1`, `.v2`, `.v3`, `.v4` | `16#11`, `16#2222`, `16#33333333`, `true` |
|  | `Mix.Static_1` … `.Static_4` | `1.5`, `false`, `true`, `false` |
| STRING dentro de estrutura / STRING inside a structure | `Mix.Text`, `Mix.Text_2` | `'mix text'`, `'second text'` |
| Estrutura dentro de estrutura / structure in a structure | `Mix.Inner.a`, `.b`, `.c`, `.d`, `.e` | `4444`, `55`, `6`, `true`, `'inner'` |
| Array de escalares dentro de estrutura | `Mix.Counts[0..3]` | `1,2,3,4` |
| Array de estruturas **dentro** de estrutura (o Tag Builder não importa) / nested array of structures | `Mix.Deep[0..2].x`, `.y` | `300+i`, `-i` (depois do padrão / after the pattern; antes: `7`, `8`) |
| Array de estruturas, sem texto (passo de 8 bytes no layout clássico) / array of structures | `Rec_Small[0..4].Dint1`, `.aBool`, `.aInt` | `1000+i`, `i mod 2 = 1`, `-10-i` |
| Array de estruturas, com STRING (passo de 264 bytes) | `Rec_Text[0..19].Dint1`, `.aBool`, `.aInt`, `.Descr` | `2000+i`, `i mod 3 = 0`, `100+i`, `'record'` |
| Array de UDT | `Motors[0..3].Run`, `.Speed`, `.Total`, `.Descr` | `i mod 2 = 0`, `10*i+0.5`, `5000+i`, `'motor'` |
| UDT solto / a single UDT | `Motor_One.Run`, `.Speed`, `.Total`, `.Status`, `.Descr` | `true`, `12.5`, `5000`, `16#00A0`, `'motor'` |

(`i` é o índice do elemento. Os valores por elemento dos arrays de estruturas vêm do `FC_TestPattern`.)
(`i` is the element index. The per element values of the arrays of structures come from `FC_TestPattern`.)

### `TestDB_Big`

| Situação | Variáveis | Valor |
|---|---|---|
| Array de 500 DInt (2000 bytes) em **um** item de leitura / one read item | `Arr_Big[0..499]` | `100000+i` (depois do padrão / after the pattern) |
| Array de 600 Byte | `Arr_Bytes600[0..599]` | `i mod 256` (depois do padrão) |
| Array de estruturas com STRING padrão | `Texts[0..9].Descr`, `.Value` | `'text'`, `9000+i` |

Atenção: um PLCSim derruba a sessão (evento de sistema fatal) quando a **resposta** de uma leitura em lote passa de
cerca de 990 bytes; o driver divide as leituras por tamanho. Estes valores servem para provar que isso continua certo
(e para ver o que o PLC faz com UM item maior que a PDU).
Careful: a PLCSim drops the session (fatal system event) when the **reply** of a batched read goes over about 990
bytes; the driver splits reads by size. These values prove that still holds (and show what the PLC does with ONE item
bigger than the PDU).

### `TestDB_Dyn`

| Variável | Quem escreve | Comportamento / Behaviour |
|---|---|---|
| `CountValue` (DInt) | CLP | `+ StepSize` a cada ciclo de OB1 / per OB1 cycle; zerado por `ResetCmd` |
| `Blink` (Bool) | CLP | inverte a cada 50 ciclos / flips every 50 cycles |
| `Text` (String[20]) | CLP | `'TICK'` quando `Blink`, `'TOCK'` senão / when not |
| `Ramp` (Real) | CLP | `+0.5` por ciclo, volta a `0.0` acima de `100.0` / per cycle, back to 0.0 above 100.0 |
| `Cycles` (DInt) | CLP | conta os ciclos até 50 / counts cycles up to 50 |
| `Enable` (Bool, `true`) | teste / test | `false` congela `CountValue`, `Blink`, `Text`, `Ramp` / freezes them |
| `StepSize` (DInt, `1`) | teste | passo do `CountValue` / step of `CountValue` |
| `ResetCmd` (Bool) | teste | `TRUE` zera `CountValue`; o CLP volta o bit a `false` / the PLC clears the bit |
| `Echo_In` / `Echo_Out` (DInt) | teste / CLP | `Echo_Out = Echo_In + 1` (ida e volta / round trip) |
| `LoadPattern` (Bool) | teste | `TRUE` recarrega os valores por elemento; o CLP volta o bit a `false` |
| `PatternLoaded` (Bool) | CLP | `true` depois do primeiro carregamento / after the first load |

O ciclo de OB1 em RUN é de poucos ms; para ver mudanças devagar use `Enable`/`StepSize`, ou o `Ramp`.
OB1 cycles in RUN take a few ms; to watch changes slowly use `Enable`/`StepSize`, or `Ramp`.

## Áreas de processo (I, Q, M) / Process areas

O driver lê as áreas pelo **nome** da variável da tabela de tags do PLC; não dá para importá-las por fonte.
Crie na tabela de tags padrão estas variáveis e escreva os valores à mão (ou numa tabela de monitoramento):

The driver reads the areas by the **name** of a PLC tag table variable; they cannot be imported from a source.
Create these in the default tag table and set the values by hand (or in a watch table):

| Nome / Name | Endereço / Address | Tipo | Valor para escrever / value to set |
|---|---|---|---|
| `TestM_Byte` | `%MB100` | Byte | `16#5A` |
| `TestM_Word` | `%MW102` | Word | `16#1234` |
| `TestM_DInt` | `%MD104` | DInt | `-1000` |
| `TestM_Real` | `%MD108` | Real | `2.5` |
| `TestM_Bool0` / `TestM_Bool1` | `%M112.0` / `%M112.1` | Bool | `true` / `false` |
| `TestQ_Byte` | `%QB100` | Byte | `16#A5` |
| `TestI_Byte` | `%IB100` | Byte | (PLCSim: forçar / force) |

## Situações cobertas / Situations covered

- **Leitura e escrita de escalares** de todos os tamanhos (1, 2, 4, 8 bytes), com e sem sinal, big-endian.
- **Layout otimizado × clássico:** o mesmo conteúdo nos dois DBs; `Mix` mistura tamanhos de propósito para o CLP reordenar no DB otimizado.
- **Bits compartilhando o byte** (`Bool_1..4`, `Mix.v4`, `Static_*`) e Bool depois de tipos maiores.
- **STRING** de vários tamanhos, vazia, dentro de estrutura, dentro de array de estruturas, em array (não vira tag) e `WString` (não vira tag).
- **Arrays** de escalares: 1 dimensão, limite negativo, 2 dimensões (não vira tag), de Bool, de LReal, grande (200 e 2000 bytes).
- **Estruturas:** plana, aninhada, com array, com array de estruturas dentro (não importado), array de estruturas (passo clássico de 8 e de 264 bytes) e UDT (solto e em array).
- **DB inteiro por nome** (`TestDB_Opt`, `TestDB_NonOpt`) como `TPLCStruct`, com itens de todos os tipos acima.
- **Valores que mudam** (`TestDB_Dyn`): assinatura por mudança, ciclo mínimo de 100 ms, reconexão, vigia da subscrição, escrita de comando e leitura do eco.
- **Respostas maiores que uma PDU** (`TestDB_Big`).
