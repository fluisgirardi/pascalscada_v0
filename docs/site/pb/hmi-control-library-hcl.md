##### Introdução

A **HMI Control Library (HCl)** é o conjunto de controles visuais do PascalSCADA — a paleta **PascalSCADA HMI Controls**. Cada um deles é um controle comum da LCL (`TEdit`, `TLabel`, `TCheckBox`, `TTrackBar`…) que aprendeu a conversar com um [tag](/pb/tags/): você solta o controle no form, aponta a propriedade **`PLCTag`** para um tag e pronto — o controle passa a mostrar o valor do tag e, nos controles de entrada, a escrever nele. Não há código a escrever para o caso comum.

O que todos os controles da HCl têm em comum:

* **`PLCTag`** — o tag ao qual o controle está ligado. Um tag pode alimentar vários controles ao mesmo tempo. Se o tag for destruído, o controle se desliga sozinho.
* **`SecurityCode`** — um código de segurança (texto livre, ex.: `"setpoints"`) que integra o controle ao [sistema de segurança](/pb/security-system/): o controle só fica habilitado se o usuário logado tiver permissão para esse código. Vazio = sem restrição. O código é registrado automaticamente no gerenciador de segurança na primeira vez que aparece.
* **`Enabled`** — além do valor que você define, o controle é desabilitado automaticamente quando o usuário não tem o `SecurityCode`.
* **Selo de falha de comunicação** — quando o tag ligado ao controle para de comunicar (timeout, porta fechada, driver sem resposta), um pequeno triângulo amarelo de aviso aparece sobre o controle, e some quando a comunicação volta. Não precisa configurar nada; o exemplo `examples/laz_comm_fault_control_icon_test` mostra o efeito em cada controle.
* **`BeforeSendAValueToTag`** / **`AfterSendValueToTag`** — eventos dos controles de entrada, disparados antes e depois de o valor ir para o tag. No *Before* você pode alterar ou vetar o valor.

Os controles rodam na *thread* principal e são atualizados pelos eventos do tag — a atualização é imediata quando o valor muda no equipamento, sem timers.

##### Formatação de valores

Os controles que exibem texto (`THMIEdit`, `THMILabel`) formatam o valor do tag com três propriedades:

* **`NumberFormat`** — máscara no formato da função `FormatFloat` do Free Pascal (`#0.00`, `000`, `#,##0.0`, `0.000E+00`…). Vale só para tags numéricos.
* **`Prefix`** / **`Sufix`** — texto antes e depois do valor (`"T = "`, `" °C"`). Aparecem apenas em exibição; no `THMIEdit`, somem enquanto o controle tem o foco para o operador editar só o número.

Com um `TPLCString` o controle mostra e edita o texto do tag diretamente.

##### Zonas

Vários controles (`THMIText`, `THMIAnimation`, os conectores de propriedade e os controles de fluxo) não mostram o valor do tag, mas **escolhem uma aparência em função dele**. A escolha é feita por uma coleção de **zonas** (`Zones`, `Conditions`, `ColorAndFlowStates`): cada zona tem uma condição e um resultado (um texto, uma imagem, uma cor, um booleano). O controle percorre a coleção e usa a **primeira zona cuja condição for verdadeira**; se nenhuma for, usa a zona marcada como **`DefaultZone`**.

A condição de cada zona é definida por `ZoneType`, `Value1`, `Value2`, `IncludeValue1` e `IncludeValue2`:

| ZoneType | A zona é selecionada quando… |
|---|---|
| `ztEqual` | valor do tag = `Value1` |
| `ztNotEqual` | valor do tag ≠ `Value1` |
| `ztRange` | `Value1` < valor < `Value2` (`IncludeValue1`/`IncludeValue2` tornam os limites inclusivos) |
| `ztOutOfRange` | valor < `Value1` **ou** valor > `Value2` (idem para os limites) |
| `ztGreaterThan` | valor > `Value1` (≥ com `IncludeValue1`) |
| `ztLessThan` | valor < `Value1` (≤ com `IncludeValue1`) |
| `ztBit` | o bit número `Value1` do valor inteiro do tag é igual a `IncludeValue1` |

Zonas de animação (texto, imagem e cor) têm ainda **`BlinkTime`** e **`BlinkWith`**: passado `BlinkTime` milissegundos, o controle troca para a zona de índice `BlinkWith` — que pode apontar de volta, formando um pisca-pisca, ou para uma terceira zona, formando uma sequência de animação. `BlinkTime = 0` desliga.

`THMIText` e `THMIAnimation` têm em tempo de projeto a propriedade **`TestValue`**: um valor de teste que o controle usa no lugar do tag para você conferir as zonas no editor sem comunicação.

##### Controles de entrada

###### THMIEdit {#THMIEdit}

![](img/THMIEdit.png) Campo de edição para tags numéricos ou string. Mostra o valor formatado e, quando o operador digita um novo valor e confirma, escreve no tag.

| Propriedade | Padrão | Descrição |
|---|---|---|
| `SendValueWhen` | `[scLostFocus, scPressEnter]` | Quando o valor digitado vai para o tag: ao perder o foco, ao pressionar Enter, ao pressionar Esc, ou a cada tecla (`scAnyChange`). |
| `FreezeValueOnFocus` | `True` | Enquanto o campo tem o foco, o valor do tag não sobrescreve o que o operador está digitando. |
| `EnableMinValue` / `MinValue`, `EnableMaxValue` / `MaxValue` | | Faixa aceita para entrada. Valor fora da faixa não é enviado ao tag. |
| `NumberFormat`, `Prefix`, `Sufix` | | Formatação (veja acima). |
| `ShowFocused` | `False` | Inverte cores de fundo e fonte quando o campo tem o foco, para destacar em telas de toque. |
| `ScreenKeyboardBehavior` | `oskbDisabled` | Teclado virtual: `oskbEnabled` abre o teclado do próprio controle ao ganhar o foco (numérico ou alfanumérico conforme o tag e o `NumberFormat`); `oskbManager` delega ao `THMIKeyboardManager` do form. `ScreenNumericKBShowMinus`/`ScreenNumericKBShowDecimal` controlam as teclas do teclado numérico. Veja [Teclado virtual](/pb/keyboard-manager/). |

###### THMICheckBox e THMIRadioButton {#THMICheckBox}

![](img/THMICheckBox.png) Caixa de seleção ligada a um tag numérico. Marcada quando o tag vale `ValueTrue`, desmarcada quando vale `ValueFalse`; `OtherValuesIS` diz o que fazer com qualquer outro valor (`isChecked`, `isUnchecked`, `isGrayed`, `isNone`). Quando o operador clica, escreve `ValueTrue` ou `ValueFalse` no tag — se `WriteTrueValue`/`WriteFalseValue` estiverem em `True`, o que permite, por exemplo, um botão que só liga e nunca desliga.

O texto, a cor e a fonte podem mudar com o estado: `CaptionTrue`/`CaptionFalse`/`CaptionGrayed`, `ColorTrue`/`ColorFalse`/`ColorGrayed`, `FontTrue`/`FontFalse`/`FontGrayed`. Escrever em `Caption`, `Color` ou `Font` preenche as três variantes de uma vez.

Com um [`TTagBit`](/pb/tags/#TTagBit) como `PLCTag` (`ValueTrue = 1`, `ValueFalse = 0`) o `THMICheckBox` vira o controle padrão para um bit de comando ou status.

![](img/THMIRadioButton.png) `THMIRadioButton` é o mesmo controle com aparência de botão de rádio. Prefira `THMIRadioGroup` para escolher entre várias opções.

###### THMIRadioGroup {#THMIRadioGroup}

![](img/THMIRadioGroup.png) Grupo de opções ligado a um tag numérico: a opção de índice *n* em `Items` fica selecionada quando o tag vale *n*, e clicar em uma opção escreve seu índice no tag. `DefaultIndex` é a opção usada quando o valor do tag não corresponde a nenhum item.

###### THMIComboBox {#THMIComboBox}

![](img/THMIComboBox.png) Lista suspensa ligada a um tag numérico, com o mesmo mapeamento do `THMIRadioGroup`: índice do item = valor do tag. Para associar valores arbitrários (ex.: 10, 20, 50) aos itens, preencha `Items` por código com objetos `TComboboxItemInfo`:

```pascal
uses hmicombobox;

HMIComboBox1.Items.AddObject('Lento',  TComboboxItemInfo.Create(nil, 10));
HMIComboBox1.Items.AddObject('Normal', TComboboxItemInfo.Create(nil, 20));
HMIComboBox1.Items.AddObject('Rápido', TComboboxItemInfo.Create(nil, 50));
```

###### THMITrackBar, THMIScrollBar e THMIUpDown {#THMITrackBar}

![](img/THMITrackBar.png) `THMITrackBar` e ![](img/THMIScrollBar.png) `THMIScrollBar` movem-se com o valor do tag e escrevem nele quando o operador arrasta. No `THMIScrollBar`, `UpdateOnMove` decide se a escrita acontece durante o arrasto (`True`) ou só ao soltar (`False`). `Min`/`Max` são os da LCL, em inteiros — para tags com escala, use um `ScaleProcessor` no tag.

![](img/THMIUpDown.png) `THMIUpDown` são as setas de incremento/decremento: cada clique soma ou subtrai `Increment` (um `Double`, então serve para passos de 0,1) ao valor do tag, respeitando `Min`/`Max` quando `EnableMin`/`EnableMax` estão ligados. Costuma ser colocado ao lado de um `THMIEdit` ligado ao mesmo tag.

###### THMITransparentButton {#THMITransparentButton}

![](img/THMITransparentButton.png) Um retângulo invisível com `OnClick`/`OnMouseUp` e `SecurityCode`. Coloque-o sobre uma imagem de fundo ou sobre um desenho para criar uma área clicável com controle de acesso — o clique não acontece se o usuário não tiver o código.

##### Controles de exibição

###### THMILabel {#THMILabel}

![](img/THMILabel.png) Mostra o valor de qualquer tag (numérico ou string) com `NumberFormat`, `Prefix` e `Sufix`. É o controle de exibição mais usado. Para um tag que guarda data/hora como número, `FormatDateTimeOptions` ajusta a conversão.

###### THMIText {#THMIText}

![](img/THMIText.png) Um `THMILabel` que, em vez do valor, mostra o **texto da zona selecionada**: cada `TTextZone` em `Zones` tem `Text`, `Color`, `Transparent`, `Font` e alinhamento próprios. É a forma padrão de traduzir um código de estado em palavras — `0 → "Parado"` em cinza, `1 → "Rodando"` em verde, `2 → "Falha"` em vermelho piscando (`BlinkTime` + `BlinkWith` apontando para uma zona com outra cor).

###### THMIProgressBar {#THMIProgressBar}

![](img/THMIProgressBar.png) Barra de nível desenhada pelo próprio PascalSCADA (não depende do tema do sistema): `Min`/`Max` em `Double`, `Orientation` horizontal ou vertical, `Color` para o preenchimento, `BackgroundColor` e `BorderColor`. Serve para nível de tanque, posição, carga de motor.

###### THMIAnimation {#THMIAnimation}

![](img/THMIAnimation.png) Mostra a **imagem da zona selecionada**. Cada `TGraphicZone` em `Zones` aponta para um arquivo (`FileName`) ou para uma imagem de um `TImageList` (`ImageList` + `ImageIndex`; `ImageListAsDefault` escolhe qual dos dois tem prioridade quando ambos estão preenchidos), com `Transparent`/`TransparentColor`. Com `BlinkTime`/`BlinkWith` entre duas ou mais zonas você obtém uma animação — um motor girando, uma lâmpada piscando. O evento `ZoneChanged` avisa quando a zona muda. O post [How to build a tank level animation](/2019/05/10/how-build-a-tank-level-animation-on-pascalscada/) mostra o uso com um `TImageList`.

##### Conectores de propriedades

Os conectores não têm aparência própria: eles **alteram propriedades de outros controles** (qualquer controle da LCL, não só da HCl) em função do valor de um tag. Isso permite, por exemplo, mudar a cor de um `TShape` comum ou esconder um `TPanel` sem escrever código.

###### THMIBooleanPropertyConnector e THMIColorPropertyConnector {#THMIBooleanPropertyConnector}

* **`PLCTag`** — o tag que dirige as condições.
* **`Conditions`** — coleção de zonas (as mesmas condições da tabela acima). Cada zona tem um resultado: `ZoneResult` booleano (com `InvertResult`) no conector booleano, `ZoneResult` cor no conector de cor. Uma delas pode ser `DefaultZone`.
* **`AffectedObjects`** — coleção de pares `TargetObject` + `TargetObjectProperty` (ex.: `Shape1` + `Brush.Color`, `Panel1` + `Visible`, `Label1` + `Font.Color`). A cada mudança do tag, o conector avalia as condições e escreve o resultado em **todas** as propriedades listadas.

O editor de propriedades lista as propriedades do tipo certo (booleano ou `TColor`) do objeto escolhido, inclusive as aninhadas (`Font.Color`, `Brush.Color`).

###### THMIControlDislocatorAnimation e THMIControlDislocatorAnimation2 {#THMIControlDislocatorAnimation}

![](img/THMIControlDislocatorAnimation.png) Movem um controle (`Control`) pela tela em função de um tag: quando o tag vale `ValueP0` o controle fica em (`P0_X`, `P0_Y`); quando vale `ValueP1`, em (`P1_X`, `P1_Y`); entre os dois, interpola linearmente. `EnableXMin/XMax/YMin/YMax` com `MinXValue`… limitam o deslocamento. As propriedades `Gets_P0_Position`, `Gets_P1_Position` e `GoTo_P0_Position` são "botões" no Object Inspector: dê um duplo clique para capturar a posição atual do controle como P0/P1 ou para levá-lo de volta a P0.

![](img/THMIControlDislocatorAnimation2.png) A versão **2** usa dois tags independentes — `PLCTagX` com `ValueP0x`/`ValueP1x` e `PLCTagY` com `ValueP0y`/`ValueP1y` — para movimento em duas dimensões (a posição de um carro em uma ponte rolante, por exemplo).

##### Desenhos básicos

Formas de equipamentos desenhadas pelo PascalSCADA (via BGRABitmap, sem imagens externas), com `BodyColor`, `BorderColor`, `BorderWidth` e `SecurityCode`. Elas **não têm `PLCTag`**: são estáticas, e você as anima com os conectores de propriedade (cor) ou usa as versões *Flow* descritas em [Controles de fluxo](/pb/flow-controls/).

| Controle | Desenho |
|---|---|
| ![](img/THMIBasicValve.png) `THMIBasicValve` | Válvula; `ValveType` escolhe o atuador (`vtSimple`, `vtPneumaticOnOff`, `vtPneumaticProportional`, `vtMotorisedProportional`, `vtPneumaticDrawer`), `ValveBodyPercent` a proporção corpo/atuador, `Mirrored` espelha. |
| ![](img/THMIBasicEletricMotor.png) `THMIBasicEletricMotor` | Motor elétrico; com `DrawPump = True` desenha uma bomba acoplada. |
| ![](img/THMIFitaBasica.png) `THMIFitaBasica` | Transportador de correia (horizontal; `BodyHeight`). |
| ![](img/THMIRedlerBasico.png) `THMIRedlerBasico` | Transportador de corrente (redler). |
| ![](img/THMIRoscaBasica.png) `THMIRoscaBasica` | Transportador helicoidal (rosca). |
| ![](img/THMIElevadorBasico.png) `THMIElevadorBasico` | Elevador de canecas (`HeadAtLeft`, `HeadColor`, `FooterColor`, `BodyWidth`). |
| ![](img/THMIPolyline.png) `THMIPolyline` | Linha poligonal — tubulação, cabo, fio. Pontos em `PointCoordinates`, `LineColor`, `LineWidth`, `PenStyle`. |
| `THMIBandeja` | Painel ("bandeja") cujas cores de borda e fundo vêm de tags (`BorderColorPLCTag`, `BackgroundColorPLCTag`, valores `TColor`) e cujo texto vem de um `TPLCStructString` (`BandejaTextPLCTag`). Feito para painéis sinóticos de silos e moegas dirigidos pelo CLP. |
| `THMIBasicVectorControl` | Desenho **SVG** (`SVGContents`, colado do arquivo) com `Stretch`/`Proportional`. É a base dos controles vetoriais de fluxo, que trocam a cor de elementos do SVG por zona. |

##### Gráficos: TTagLinkedSeriesSource

`TTagLinkedSeriesSource` é uma fonte de dados do **TAChart** alimentada por um tag: ligue-o à propriedade `Source` de uma `TLineSeries` e ele acrescenta um ponto (instante, valor) a cada atualização.

* **`PLCTag`** — o tag numérico.
* **`SourceUpdateType`** — `tlTagUpdate` (um ponto a cada leitura, mesmo sem mudança), `tlTagChange` (só quando o valor muda) ou `tlCyclic` (a cada `CyclicUptimeTime` ms, com o último valor).
* **`EnableXAxisMaxInterval`** / **`XAxisMaximumInterval`** — descarta pontos mais velhos que o intervalo (em milissegundos), mantendo o gráfico deslizante.
* **`UseNowInsteadTagTimestamp`** — usa o relógio do PC em vez do timestamp do tag.
* **`YMinOffset`** / **`YMaxOffset`** — margens acrescentadas aos extremos do eixo Y.

##### Exemplo passo a passo

Uma tela mínima para um motor: estado, velocidade e comando.

1. Tags: `Motor_Status` (`TPLCTagNumber`, palavra de estado), `Motor_Ligado` (`TTagBit` sobre `Motor_Status`, bit 0), `Motor_Velocidade` (`TPLCTagNumber`, `pttFloat`) e `Motor_Setpoint`.
2. Um `THMIText` com `PLCTag = Motor_Status` e três zonas: `ztEqual 0 → "Parado"`, `ztEqual 1 → "Rodando"` (verde), `ztBit 7 → "Falha"` (vermelho, `BlinkTime = 500`, `BlinkWith` apontando para uma quarta zona idêntica em branco).
3. Um `THMILabel` com `PLCTag = Motor_Velocidade`, `NumberFormat = "#0.0"`, `Sufix = " rpm"`.
4. Um `THMIEdit` com `PLCTag = Motor_Setpoint`, `MinValue = 0`, `MaxValue = 1800`, `EnableMinValue`/`EnableMaxValue = True`, `SecurityCode = "operacao"`.
5. Um `THMICheckBox` com `PLCTag = Motor_Ligado`, `CaptionTrue = "Desligar"`, `CaptionFalse = "Ligar"`, `SecurityCode = "operacao"`.
6. Um `THMIColorPropertyConnector` com `PLCTag = Motor_Ligado`, uma condição `ztEqual 1 → clLime` e a `DefaultZone → clSilver`, afetando `Shape1.Brush.Color`.

Sem uma linha de código, a tela mostra o estado, a velocidade, aceita o setpoint dentro da faixa, liga e desliga o motor e colore a forma — e tudo isso fica desabilitado para um usuário sem o código `operacao`.


##### Exemplos relacionados

* `examples/laz_weg_twp03_full_project` — projeto completo de supervisão de um CLP WEG TPW03 por Modbus RTU: `THMIEdit`, `THMICheckBox`, `THMILabel` e blocos em telas reais.
* `examples/both_modbus_rtu_corn_weigh` — sinótico de pesagem com `THMIAnimation`, `THMIText`, `TTagBit` e blocos (Lazarus e Delphi).
* `examples/laz_modbus_tcp_example` — `THMIEdit`, `THMICheckBox`, `THMIText` e `THMILabel` sobre um bloco Modbus TCP.
* `examples/laz_comm_fault_control_icon_test` — o selo de falha de comunicação em cada tipo de controle.
* `examples/laz_isotcp_hourmeter` — `THMIAnimation` com `TImageList` e zonas piscando.
* `examples/hmi_bandeja` — `THMIBandeja` com cores e texto vindos do CLP.
* `examples/hmi_flowvectorcontrol` e `examples/laz_svg_example` — `THMIFlowVectorControl` recolorindo elementos de um SVG por zona (veja [Controles de fluxo](/pb/flow-controls/)).
* `examples/laz_numericexpr` — `THMITrackBar` e `THMILabel` em volta de um `TNumericExprTag`.

Outras páginas da HCl: [Controles de fluxo](/pb/flow-controls/), [Registradores de eventos e alarmes](/pb/event-and-alarm-loggers/), [Teclado virtual](/pb/keyboard-manager/) e [Sistema de segurança](/pb/security-system/).
