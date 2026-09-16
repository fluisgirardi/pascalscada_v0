##### Introdução

O ![](img/TNumericExprTag.png) **`TNumericExprTag`** é um tag numérico **virtual**: ele não tem driver nem endereço, e o seu valor é o resultado de uma **expressão** calculada a partir de outros tags. Sempre que um dos tags de origem muda, a expressão é reavaliada e, se o resultado for diferente, o `TNumericExprTag` muda também — notificando os controles da [HCl](/pb/hmi-control-library-hcl/), os `TTagBit` e o código ligados a ele, exatamente como um tag de comunicação faria.

Serve para tudo que é *derivado* de valores do equipamento e que não vale a pena programar no CLP nem em código na aplicação:

* uma vazão total como soma de duas linhas (`A+B`);
* um percentual (`A/B*100`), uma diferença de temperatura (`A-B`), uma potência (`A*B/1000`);
* um alarme ou estado combinado (`ifthen((A>80) and (B=1), 1, 0)`);
* uma grandeza física a partir de uma medição (`sqrt(A)*12.5`).

Ele fica na paleta **PascalSCADA Tags**. Para conversões de escala simples de **um** tag, um [processador de escala](/pb/scale-processors/) é mais adequado — a expressão entra quando o resultado depende de mais de um tag ou de lógica.

##### Propriedades

| Propriedade | Descrição |
|---|---|
| `A` … `J` | Até dez tags numéricos de origem (qualquer `TPLCNumber`: `TPLCTagNumber`, `TPLCBlockElement`, `TPLCStructItem`, `TTagBit` ou outro `TNumericExprTag`). Cada um entra na expressão pela sua letra. |
| `Expression` | A expressão. Alterá-la recalcula o valor na hora. |
| `ScaleProcessor` | Escala aplicada ao resultado — `ValueRaw` é o resultado da expressão, `Value` é ele escalado. |
| `LastEvalutionError` | `OK`, ou a mensagem do último erro de avaliação (sintaxe, variável não ligada, tipo inválido). Somente leitura. |
| `Value`, `ValueRaw` | O resultado. **Somente leitura na prática**: escrever nele não faz nada além de disparar `OnWriteFail`. |
| `OnValueChangeFirst`, `OnValueChangeLast` | Disparados quando o resultado muda (antes e depois de notificar os controles). |
| `OnReadFail` | Disparado quando a avaliação falha. |
| `OnWriteFail` | Disparado quando alguém tenta escrever no tag. |

As variáveis entram na expressão com o **`Value`** do tag de origem — ou seja, já escalado. Se precisar do valor bruto, ligue um `TTagBit`/`TPLCTagNumber` sem escala ou faça a conta inversa na expressão.

##### Sintaxe da expressão

O avaliador é o `TFPExpressionParser` do Free Pascal, com as funções matemáticas e booleanas ligadas, mais uma função própria do PascalSCADA:

| Categoria | Disponível |
|---|---|
| Operadores | `+ - * / ^` (potência), `mod`, parênteses |
| Comparação | `= <> < > <= >=` |
| Lógica | `and`, `or`, `not`, `xor`; `true`, `false` |
| Bits | `shl`, `shr` |
| Condicional | **`ifthen(condição, seVerdadeiro, seFalso)`** — os dois valores devem ser numéricos |
| Matemática | `abs`, `sqr`, `sqrt`, `round`, `trunc`, `int`, `frac`, `exp`, `ln`, `log`, `sin`, `cos`, `arctan`, `pi` |

Regras que vêm do avaliador e valem a pena saber:

* **Uma comparação sozinha vale 1 ou 0**: `Expression = 'A > 5'` é um tag booleano perfeito para um `THMICheckBox` ou uma zona.
* O resultado pode ser inteiro ou real; ambos viram o `Double` do tag sem perda. Um resultado **texto** (`'"abc"'`) é erro.
* As letras das variáveis não diferenciam maiúsculas (`a+b` = `A+B`), mas prefira maiúsculas para casar com as propriedades.
* Uma variável usada na expressão sem tag ligado (`A+B` com `B = nil`) é erro de avaliação, não zero.
* Divisão por zero é **erro de avaliação** ("division by zero" em `LastEvalutionError`, valor anterior mantido) — proteja na expressão: `ifthen(B=0, 0, A/B)`.

Exemplos:

```
A + B
A / B * 100
(A - 32) * 5 / 9
ifthen(A > B, A, B)                       -- máximo
ifthen((A > 80) and (B = 1), 1, 0)        -- alarme combinado
ifthen(B = 0, 0, A / B)                   -- divisão protegida
round(A / 10) * 10                        -- arredonda para a dezena
(A shr 4) mod 16                          -- nibble alto de um byte
sqrt(sqr(A) + sqr(B))                     -- módulo de um vetor
```

##### Quando o valor é recalculado

O `TNumericExprTag` não tem `RefreshTime`: ele é **dirigido a eventos**. A expressão é avaliada:

* quando qualquer tag ligado em `A`…`J` muda de valor (`OnValueChange` da origem) ou falha uma escrita;
* quando `Expression` ou uma das variáveis é alterada;
* ao terminar de carregar o form/datamodule.

Se o resultado for igual ao anterior, nada é notificado — um `ifthen(A>80, 1, 0)` só dispara `OnValueChange` quando cruza o limite, não a cada leitura de `A`. Por isso ele é barato: dez tags de origem lidos a 100 ms custam dez avaliações por ciclo, todas na thread principal, e só as mudanças reais se propagam.

O `ClockMonotonicTimeStamp` do tag é o instante da última mudança de resultado.

##### Erros

Quando a avaliação falha — sintaxe errada, variável sem tag, resultado texto — o tag:

1. guarda a mensagem em `LastEvalutionError`;
2. **mantém o valor anterior** (não vira zero);
3. dispara `OnReadFail`, o que faz os controles ligados mostrarem o selo de falha de comunicação, como se o tag tivesse parado de comunicar.

Corrigida a expressão (ou ligada a variável que faltava), a próxima avaliação com sucesso limpa o erro (`LastEvalutionError = 'OK'`) e o selo some. Em tempo de projeto, olhe `LastEvalutionError` no Object Inspector logo depois de digitar a expressão: o erro aparece ali na hora.

Se um tag de origem for destruído, a variável correspondente volta a `nil` sozinha — e a expressão passa a falhar por variável não ligada, em vez de acessar um objeto morto.

##### Encadeando expressões

Como o `TNumericExprTag` é um `TPLCNumber`, ele pode ser variável de **outro** `TNumericExprTag`. Isso permite dividir uma conta grande em etapas nomeadas (`Vazao_Total = A+B+C`, `Percentual_Linha1 = A/B*100` com `B = Vazao_Total`) e reaproveitar o resultado intermediário em vários lugares. Evite ciclos (um tag que dependa, direta ou indiretamente, de si mesmo): a avaliação entra em loop de notificações.

Ele também pode ser origem de um [`TTagBit`](/pb/tags/#TTagBit) (com `UseRawValue` conforme o caso) e de qualquer controle da HCl.

##### Exemplo passo a passo

Mostrar a eficiência de um motor a partir de potência elétrica e mecânica lidas do CLP, com alarme abaixo de 85 %:

1. Tags de comunicação `Pot_Eletrica` e `Pot_Mecanica` (`TPLCTagNumber`, `pttFloat`).
2. Um `TNumericExprTag` chamado `Eficiencia`: `A = Pot_Mecanica`, `B = Pot_Eletrica`, `Expression = 'ifthen(B = 0, 0, A / B * 100)'`.
3. Um `THMILabel` em `Eficiencia` com `NumberFormat = '#0.0'` e `Sufix = ' %'`.
4. Outro `TNumericExprTag` chamado `Eficiencia_Baixa`: `A = Eficiencia`, `Expression = 'ifthen(A < 85, 1, 0)'`.
5. Um `THMIColorPropertyConnector` em `Eficiencia_Baixa` colorindo o painel de vermelho quando vale 1 — ou um [`THMIAlarmLogger`](/pb/event-and-alarm-loggers/) registrando o alarme.

Nenhuma linha de código, e nada foi acrescentado ao programa do CLP.

##### Exemplos relacionados

* `examples/laz_numericexpr` — dois `TPLCTagNumber` movidos por `THMITrackBar` e um `TNumericExprTag` com `Expression = 'A+B'` mostrado em `THMILabel`; o lugar para experimentar as expressões da tabela acima.
