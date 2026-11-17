##### Introdução

![](img/TpSCADALogger.png) **`TpSCADALogger`** reúne o que acontece dentro do PascalSCADA — comunicação, conexões, threads de scan, tags, eventos disparados, segurança, banco de dados — e manda cada mensagem para a **tela**, para um **arquivo**, para um **evento** ou para **lugar nenhum**, conforme uma lista de **regras**. Uma regra escolhe as mensagens pela **fonte** (o nome do componente), pela **classe**, pela **categoria** e pelo **nível**. Assim você decide, por exemplo, que o tráfego de uma porta Modbus vai para um arquivo só dela, que os avisos do driver S7 vão para a tela e que todo erro também dispara um evento.

![](img/TpSCADALogMemo.png) **`TpSCADALogMemo`** é o destino de tela: um memo somente leitura que mostra as linhas que as regras mandam para a tela.

Os dois ficam na paleta **PascalSCADA Utils**. O `TpSCADALogger` está no pacote `pascalscada_common` (sem LCL: funciona também em serviços e programas de console); o `TpSCADALogMemo` está no `pascalscada_hmi`.

##### Como funciona

Todas as partes do framework publicam suas mensagens num único hub global. Cada `TpSCADALogger` ativo recebe uma cópia e a passa pelas suas regras. Pode haver vários loggers no mesmo programa; cada um recebe todas as mensagens.

O log não custa nada enquanto ninguém escuta: antes de montar uma mensagem (formatar um texto, converter um frame em hexadecimal) o código pergunta ao hub se alguma regra ativa quer aquela categoria e nível. Publicar só copia a mensagem para a fila do logger; arquivos, tela e evento são tratados pela thread do próprio logger, em lotes, e uma thread de comunicação nunca espera pelo disco ou pela tela. A tela e o evento chegam na **thread principal**.

##### Níveis e categorias

| Nível | Uso |
|---|---|
| `llTrace` | Tudo, inclusive os bytes de cada frame. Muitos dados. |
| `llDebug` | Detalhes úteis para entender um problema. |
| `llInfo` | Eventos normais: conectou, desconectou, iniciou. |
| `llWarning` | Algo deu errado e foi recuperado (timeout, reconexão). |
| `llError` | Algo falhou. |

| Categoria | Mensagens sobre |
|---|---|
| `lcComm` | bytes enviados e recebidos por uma porta |
| `lcConnection` | abertura, fechamento, reconexão, timeouts |
| `lcProtocol` | frames e requisições de um driver de protocolo |
| `lcScan` | threads de scan e de atualização, filas |
| `lcTag` | leituras, escritas e valores de tags |
| `lcEvent` | eventos disparados |
| `lcThread` | threads iniciando e terminando |
| `lcSecurity` | login, logout, autorização |
| `lcDatabase` | a camada de banco de dados |
| `lcHMI` | os controles visuais |
| `lcUser` | mensagens da sua aplicação |

##### Propriedades do TpSCADALogger

| Propriedade | Descrição |
|---|---|
| `Active` | Liga e desliga o logger (padrão `True`). Em tempo de projeto ele nunca roda. |
| `Rules` | As regras de roteamento, avaliadas em ordem (veja abaixo). |
| `Display` | O destino de tela: um `TpSCADALogMemo` ou qualquer componente que implemente `IpSCADALogDisplay`. O Object Inspector lista só esses. |
| `LineFormat` | O formato da linha. Marcadores: `%time%`, `%date%`, `%level%`, `%category%`, `%thread%` (`main`, `T1`, `T2`… na ordem em que as threads publicam pela primeira vez), `%threadid%` (o id do sistema), `%source%`, `%class%`, `%msg%`, `%data%` (os bytes em hexadecimal, vazio quando não há). Vazio volta ao padrão. |
| `OnLog` | Evento chamado, na thread principal, para cada mensagem que uma regra manda para `ltEvent`. Recebe os campos da mensagem (`Entry`) e a linha formatada (`Line`). |
| `MaxFileSize` | Tamanho máximo de um arquivo de log em bytes; acima dele o arquivo é renomeado para `.1`, `.2`… (`0` = sem limite). |
| `MaxRotatedFiles` | Quantos arquivos renomeados são mantidos (padrão 5). |
| `FileIdleCloseMs` | Um arquivo que não recebe nada por esse tempo é fechado e reaberto quando preciso (padrão 10 s). |
| `FlushIntervalMs` | De quanto em quanto tempo a thread do logger grava o que está na fila (padrão 200 ms). |
| `MaxQueue` | Mensagens que a fila comporta; acima disso as novas são descartadas e depois sai uma linha *N log messages dropped*. `Dropped` conta quantas. |

`Flush(TimeoutMs)` espera até que tudo o que foi publicado antes da chamada esteja gravado e, na thread principal, mostrado. `PSLogFlushAll` faz o mesmo para todos os loggers.

A linha padrão fica assim:

```
2026-11-17 14:03:22.417 [WARN ] [connection] T2 Form1.ModBusTCP1 (TModBusTCPDriver): timeout, reconnecting
2026-11-17 14:03:22.419 [TRACE] [comm] T2 Form1.TCP_UDPPort1 (TTCP_UDPPort): TX | 00 01 00 00 00 06 01 03 00 00 00 0A
```

##### Regras

Uma mensagem casa com uma regra quando **todas** estas condições valem:

| Propriedade | Casa quando |
|---|---|
| `Enabled` | a regra está habilitada. |
| `MinLevel` | o nível da mensagem é este ou maior (padrão `llInfo`). |
| `Categories` | a categoria da mensagem está no conjunto (padrão: todas). |
| `SourceMask` | a fonte casa com a máscara. A fonte é o caminho do componente, como `Form1.ModBusTCP1`. Aceita `*` e `?`, não diferencia maiúsculas e aceita várias máscaras separadas por `;`. Vazio significa tudo. |
| `ClassMask` | a classe da fonte **ou um ancestral dela** casa com a máscara — `TProtocolDriver` casa com todos os drivers. Mesma sintaxe da `SourceMask`. |

E então:

| Propriedade | Efeito |
|---|---|
| `Targets` | Para onde vai: `ltScreen`, `ltFile`, `ltEvent`, em qualquer combinação. **Vazio descarta a mensagem.** |
| `FileName` | O arquivo de `ltFile`. Aceita `%date%`, `%source%`, `%class%`, `%category%` e `%level%`, então uma regra pode gravar um arquivo por dia, por fonte ou por classe. Um caminho relativo é relativo à **pasta do executável**; pastas que faltam são criadas. |
| `StopProcessing` | Quando a regra casa, as regras seguintes não são avaliadas. Com `Targets` vazio vira uma **exclusão**. |

Quando várias regras casam, a mensagem vai para todos os destinos delas; duas regras no mesmo arquivo gravam a linha uma vez só.

Exemplos:

| Objetivo | Regra |
|---|---|
| Erros e avisos de tudo na tela | `MinLevel = llWarning`, `Targets = [ltScreen]` |
| Tráfego de cada porta num arquivo diário próprio | `Categories = [lcComm]`, `MinLevel = llTrace`, `Targets = [ltFile]`, `FileName = logs/%source%-%date%.log` |
| Tudo do driver S7CommPlus num arquivo | `ClassMask = TS7CommPlusDriver`, `MinLevel = llDebug`, `Targets = [ltFile]`, `FileName = logs/s7plus.log` |
| Ignorar um componente barulhento | primeira regra: `SourceMask = Form1.Debug*`, `Targets = []`, `StopProcessing = True` |

##### TpSCADALogMemo

Um memo preparado para logs: somente leitura, sem quebra de linha, com barras de rolagem. Ligue-o no `Display` do logger.

| Propriedade | Descrição |
|---|---|
| `MaxLines` | Linhas mantidas; as mais antigas saem primeiro (padrão 1000, `0` = sem limite). |
| `AutoScroll` | Mantém a última linha visível quando chegam linhas novas (padrão `True`). Desligue para ler com calma. |

Uma fonte monoespaçada deixa as colunas alinhadas. Para mostrar o log em outro lugar (uma lista, uma grade, uma barra de status) use o evento `OnLog`, ou escreva um componente que implemente `IpSCADALogDisplay` (um método, `AddLogLines(Lines: TStrings)`, chamado na thread principal).

##### O que o PascalSCADA publica

| Quem | Categoria / nível | Mensagens |
|---|---|---|
| toda porta de comunicação | `lcComm` / `llTrace` | `TX 12/12 iorOK`, `RX 10/10 iorOK`, com os bytes como dados, na ordem em que a porta os fez. |
| | `lcComm` / `llWarning` | `read failed: iorTimeOut`, `write failed: …` |
| | `lcConnection` / `llInfo` | `port started`, `port stopped` |
| | `lcConnection` / `llError` | `open failed`, `close failed`, com o erro do sistema operacional |
| | `lcConnection` / `llWarning` | `connection lost` |
| `TTCP_UDPPort` | `lcConnection` / `llInfo` | `connected`, `reconnecting` (`llDebug`: `closing the socket`) |
| `TS7CommPlusDriver` | `lcProtocol` / `llDebug` | as mesmas mensagens do `OnDebug` (inclusive as da conexão), com o driver como fonte |
| `TOPCUADriver` | `lcProtocol` / o nível da mensagem | as mesmas mensagens do `OnLog`; o `LogLevel` do driver continua decidindo o que a biblioteca informa |

As mensagens de conexão trazem a porta entre parênteses: `(192.168.0.10:502 TCP)`, `(COM1 19200 8N1)`. O `LogIOActions`/`LogFile` das portas e o `OnDebug`/`OnLog` dos drivers continuam funcionando como antes. As threads de scan, os tags, os eventos e os demais subsistemas passam a publicar nas próximas versões.

##### Publicando do seu código

```pascal
uses pscadalog;

PSLog(Self, lcUser, llInfo, 'lote iniciado');
PSLogFmt(Self, lcUser, llWarning, 'tanque %d acima de %f', [TankNo, Level]);

//monte mensagens caras só quando alguém quiser
if PSLogWants(lcComm, llTrace) then
  PSLog(Self, lcComm, llTrace, 'frame', FrameBytes);
```

`Self` (o remetente) vira a fonte e a classe da mensagem; pode ser `nil`. `PSLog` pode ser chamado de qualquer thread.

##### Programas sem a LCL

Arquivos funcionam em qualquer programa. A tela e o evento `OnLog` chegam na thread principal por `TThread.Queue`, então um programa de console ou serviço precisa chamar `CheckSynchronize` de tempos em tempos (uma aplicação LCL já faz isso).

##### Exemplos relacionados

* `examples/laz_pscadalogger` — um logger com três regras (tela, um arquivo por categoria e dia, um evento que conta os erros), um `TpSCADALogMemo`, um botão para publicar mensagens de qualquer nível e categoria e uma rajada de quatro threads publicando ao mesmo tempo.
