##### Introdução

Em primeiro lugar, sinta-se livre para enviar correções de qualquer tipo, incluindo reformulações de texto.

Este artigo tem como objetivo principal entender para que servem as classes de processamento de escala e os cenários onde estas classes devem ser usadas. Um resumo das propriedades de cada uma será abordado a seguir.

##### O que são as classes processadoras de escalas

As classes processadoras de escalas (que são derivadas da classe `TScaleProcessor`) servem para fazer uma transformação numérica do valor que vem do seu equipamento (valor puro) para um valor que será exibido para o usuário do sistema de supervisão (engenharia), fazendo o caminho reverso quando o usuário altera um valor no sistema de supervisão que está na escala de engenharia, entregando um valor puro para o seu equipamento.

O equipamento raramente entrega o valor na unidade que o operador quer ver: uma entrada analógica de um S7 chega como um inteiro de 0 a 27648; um inversor manda a frequência em décimos de hertz; um transmissor de nível dá 4–20 mA convertidos em 0–4095. É esse o trabalho das escalas.

Elas se ligam aos tags numéricos pela propriedade **`ScaleProcessor`** (veja [Tags](/pb/tags/)). A partir daí:

* `ValueRaw` é o valor puro, como veio do equipamento;
* `Value` é `ValueRaw` depois da escala — o que os controles da [HCl](/pb/hmi-control-library-hcl/) mostram e o que você usa em código;
* escrever em `Value` aplica a escala inversa e grava o valor puro no equipamento.

##### Escalas implementadas no PascalSCADA atualmente

O processamento de escala mais utilizado é o processamento linear, que já está implementado no PascalSCADA na classe `TLinearScaleProcessor`. Mas caso necessite de uma escala que não está implementada ainda, você pode usar a classe `TUserScale`, que permite, através da implementação de dois eventos da instância, a sua própria escala. E para compor várias escalas em sequência existe a `TScalesQueue`. Todas ficam na paleta **PascalSCADA Utils**.

| Componente | Faz |
|---|---|
| ![](img/TLinearScaleProcessor.png) `TLinearScaleProcessor` | Conversão linear entre duas faixas (a regra de três). Cobre 90% dos casos. |
| ![](img/TUserScale.png) `TUserScale` | Conversão que você escreve em dois eventos — para qualquer fórmula. |
| ![](img/TScalesQueue.png) `TScalesQueue` | Encadeia vários processadores em sequência. |

##### Preciso criar um objeto de escala para cada tag da minha aplicação?

Não. O sistema de escalas do PascalSCADA foi desenhado para que um objeto de escala possa ser compartilhado com quantos tags existirem na sua aplicação. Ou seja, se todos os meus tags usam a mesma escala e se num determinado momento for necessário realizar uma alteração desta escala, basta mudar as propriedades de um único objeto para alterar a escala de todos os tags de sua aplicação. O processador não guarda estado por tag.

##### A classe TLinearScaleProcessor {#TLinearScaleProcessor}

A classe `TLinearScaleProcessor` faz o que o nome dela propõe: a conversão de valores através de uma função linear de primeiro grau. A imagem abaixo ilustra seu funcionamento: [![TLinearScaleProcessor](http://www.pascalscada.com/wp-content/uploads/2016/08/TLinearScaleProcessor.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/TLinearScaleProcessor.png>)

As propriedades `PLCMin` e `PLCMax` representam o intervalo de valores puros, que vem do seu equipamento. Já as propriedades `SysMin` e `SysMax` representam a faixa de valores na unidade de engenharia, ou seja, que serão exibidas ao usuário do sistema de supervisão. A conversão é feita em ambos os sentidos, do equipamento para o usuário e do usuário para o equipamento:

```
Value    = (ValueRaw - PLCMin) * (SysMax - SysMin) / (PLCMax - PLCMin) + SysMin
ValueRaw = (Value    - SysMin) * (PLCMax - PLCMin) / (SysMax - SysMin) + PLCMin
```

![Warning32](http://www.pascalscada.com/wp-content/uploads/2016/08/Warning32.png)Vale lembrar que os processadores de escala não fazem a restrição para valores na escala de engenharia fora da faixa: um valor puro fora de `PLCMin`..`PLCMax` produz um valor de engenharia fora de `SysMin`..`SysMax` — o que geralmente é o desejado, para que uma sobrefaixa apareça como tal. Para restringir a entrada de dados use as propriedades `EnableMinValue`, `EnableMaxValue`, `MinValue` e `MaxValue` de seus tags.

Basta configurar as propriedades `PLCMin`, `PLCMax`, `SysMin` e `SysMax` com os dados da sua escala. Para testar não é necessário rodar a aplicação: basta usar a propriedade `Input` para simular o valor vindo de seu CLP e a propriedade `Output` para simular os valores vindos da aplicação. Sete o valor na propriedade `Input` e veja o valor convertido na escala de engenharia na propriedade `Output`, e vice-versa. (`PLCMax = PLCMin` ou `SysMax = SysMin` não divide por zero: o divisor é tratado como 1.)

Abaixo está uma lista de alguns exemplos de conversões usando a classe `TLinearScaleProcessor`:

| Nome | PLCMin | PLCMax | SysMin | SysMax |
|---|---|---|---|---|
| Milissegundos (CLP) → segundos (Supervisão) | 0 | 1000 | 0 | 1 |
| Segundos (CLP) → minutos (Supervisão) | 0 | 60 | 0 | 1 |
| Temperatura RTD em décimos de grau (CLP) → graus (Supervisão) | 0 | 10 | 0 | 1 |
| Analógica 4 a 20 mA Siemens (CLP) → 0 a 100 % (Supervisão) | 4096 | 27648 | 0 | 100 |
| Analógica 0 a 10 V Siemens (0–27648) → 0 a 100 % | 0 | 27648 | 0 | 100 |
| Entrada 4–20 mA em cartão 0–4095 (4 mA = 819) → 0 a 10 bar | 819 | 4095 | 0 | 10 |
| Temperatura em décimos de grau, com offset (−400 = −40,0 °C) | −400 | 1500 | −40 | 150 |
| Inverter o sentido (0 = cheio, 1000 = vazio) → 100 a 0 % | 0 | 1000 | 100 | 0 |

##### A classe TUserScale {#TUserScale}

![](img/TUserScale.png) Para conversões não lineares — raiz quadrada de um transmissor de vazão por pressão diferencial, tabela de linearização de um termopar, polinômio de calibração — implemente os dois eventos:

```pascal
// Equipamento -> usuário
procedure TForm1.UserScale1PLCToUser(Sender: TObject; const Input: Double;
  var Output: Double);
begin
  Output := Sqrt(Input / 4095) * 500;      // vazão em m³/h
end;

// Usuário -> equipamento (inversa da anterior)
procedure TForm1.UserScale1UserToPLC(Sender: TObject; const Input: Double;
  var Output: Double);
begin
  Output := Sqr(Input / 500) * 4095;
end;
```

`Sender` é o tag que pediu a conversão, então um único `TUserScale` pode aplicar fórmulas diferentes conforme o tag (`if Sender = Tag_Vazao1 then …`). Se um evento não estiver atribuído, o valor passa sem alteração naquele sentido.

Os eventos rodam na thread principal, sempre que o tag é atualizado — mantenha-os rápidos e sem acesso a banco ou rede.

##### A classe TScalesQueue {#TScalesQueue}

![](img/TScalesQueue.png) Uma fila de processadores. A coleção `ScalesQueue` tem itens com a propriedade `ScaleProcessor`; o valor puro passa pelo primeiro item, o resultado pelo segundo, e assim por diante. No sentido usuário → equipamento a ordem é invertida automaticamente, do último para o primeiro.

Serve para compor conversões reutilizáveis: um `TLinearScaleProcessor` "0–27648 → 0–100 %" seguido de outro "0–100 % → 0–500 m³/h", ou uma escala linear seguida de um `TUserScale` de linearização. Um `TScalesQueue` também é um processador de escala e pode entrar na fila de outro `TScalesQueue` (só não pode conter a si mesmo).

##### Onde mais as escalas aparecem

* **[`TNumericExprTag`](/pb/tags/#TNumericExprTag)** também tem `ScaleProcessor`, aplicado ao resultado da expressão.
* **`TTagBit`** com `UseRawValue = False` mapeia os bits de `Value` (escalado) do tag origem; com `True`, de `ValueRaw`. Para palavras de status ligue `True`.
* Os controles **`THMITrackBar`**/**`THMIScrollBar`** trabalham com inteiros da LCL: para um setpoint de 0,0 a 10,0 bar, ponha a escala no tag (`SysMin = 0`, `SysMax = 100`) e o controle de 0 a 100 — ou use `THMIEdit`/`THMIUpDown`, que aceitam `Double`.
* Os **`THMIControlDislocatorAnimation`** usam internamente uma escala linear entre `ValueP0`/`ValueP1` e as posições P0/P1.

##### Exemplos relacionados

* `examples/laz_linear_scalling` — um `TScalesQueue` com um `TLinearScaleProcessor` ("1 no sistema = 100 no CLP") aplicado a elementos de um bloco Modbus TCP, com `THMIScrollBar` e `THMILabel` mostrando puro e escalado lado a lado.
* `examples/laz_numericexpr` — cálculo entre tags com `TNumericExprTag`, alternativa às escalas quando o resultado depende de mais de um tag.
