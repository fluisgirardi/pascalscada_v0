##### Introdução

O `TS7CommPlusDriver` fala **S7CommPlus**, o protocolo nativo do S7-1200 e do S7-1500 — o mesmo que o TIA Portal usa para programá-los. Diferente do [S7 sobre ISOTCP](/pb/s7-protocol-over-isotcp/), ele endereça as variáveis pelo **caminho simbólico** (o nome que a variável tem no programa do CLP) em vez de um número de DB mais um offset de byte, então um DB otimizado — o padrão do TIA Portal — pode continuar otimizado.

|  | S7CommPlus | ISOTCP |
|---|---|---|
| CLPs | S7-1200, S7-1500 | S7-300/400, S7-1200/1500, S7-200 atrás de um CP 243-1, LOGO! 7/8 |
| Endereçamento | simbólico: `DB4.Var26.v1`, `MArea.Clock_10Hz` | absoluto: `DB1.DBD0`, `M10.3` |
| *Acesso otimizado ao bloco* | pode ficar ligado | precisa ser desligado |
| *Permitir acesso com PUT/GET* | não precisa | precisa |
| Criptografia | TLS, ligado por padrão | nenhuma |
| Login por senha | ainda não utilizável — veja *TLS e certificados* | – |

Ele **não** conversa com S7-300, S7-400, S7-200 ou LOGO!, que não têm S7CommPlus. Para esses use o `TISOTCPDriver`, e também para um S7-1200/1500 quando você controla o projeto no TIA e prefere endereçar por byte.

> **Um driver fala com um único CLP.** O driver é dono da sessão; para falar com dois CLPs use duas portas de comunicação e dois drivers, um para cada.

##### Componentes necessários

1. Uma ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** com `Host` = IP do CLP, `Port = 102` (padrão) e `PortType = ptTCP`. Veja [Portas de comunicação](/pb/communication-ports/).
2. Um **`TS7CommPlusDriver`** (unit `S7PlusFamily`) com `CommunicationPort` apontando para a porta.
3. Os tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`, `TPLCStruct`) com `ProtocolDriver` apontando para o driver e `LongAddress` com o caminho simbólico (veja *Endereçamento*).

O jeito mais rápido de chegar ao passo 3 é o assistente **Tag Builder** (veja abaixo), que lê a lista de variáveis do CLP e cria os tags com o caminho, o tipo e o byte swapping já configurados.

##### Preparando o CLP

Nada precisa ser mudado na estrutura do programa: DBs otimizados estão liberados e o PUT/GET continua desligado. Só duas coisas importam:

1. **Endereço IP** configurado na CPU (e alcançável da máquina que roda a aplicação).
2. **Nível de proteção**, nas propriedades da CPU → *Proteção e segurança* → *Nível de acesso*:

| Nível de proteção | O que o driver consegue fazer |
|---|---|
| *Acesso completo* (sem senha) | ler e escrever |
| *Acesso de leitura* | só ler; toda escrita falha |
| *Acesso HMI* / *Sem acesso* | a conexão é recusada |

Deixe a senha de fora: uma CPU em *Acesso completo com senha* exige login, e a legitimation por senha do S7CommPlus ainda não está concluída neste driver (veja *TLS e certificados*).

##### Propriedades do driver

| Propriedade | Padrão | Descrição |
|---|---|---|
| `CommunicationPort` | – | A porta `TTCP_UDPPort` usada pelo driver. |
| `UseTLS` | `True` | Criptografa a sessão. Exigido pela maioria dos firmwares S7-1200/1500 a partir do TIA Portal V15; deixe `False` só para firmware V1 legado conhecido. |
| `ReadOnly` | `False` | Se `True`, nenhuma escrita chega ao CLP: toda escrita é respondida com `ioReadOnlyProtocol`. Para uma tela que só deve supervisionar. |
| `ReadSomethingAlways` | `True` | Uma rodada de varredura que já precisa ler algo também atualiza os tags polled que não estão vencidos, desde que caibam na requisição que já está saindo. Valores mais novos sem ida e volta extra. |
| `MaxItemsPerRequest` | 100 | Máximo de itens em uma requisição de leitura/escrita. Um lote maior é dividido em várias requisições, então isto limita a requisição, não o número de tags. Não aumente às cegas: um PLCSim recusou 101 itens. |
| `MaxTagsPerScan` | 500 | Máximo de tags atendidos em uma rodada de varredura. Os que passam disso continuam vencidos e são atendidos na rodada seguinte — nada é descartado. Um tag de DB inteiro conta como um. |
| `MaxItemsPerSubscription` | 500 | Máximo de itens em uma assinatura (limite de tamanho, não de quantidade: depende de quão longos são os caminhos LID dos tags). |
| `SubscriptionCycleMs` | 100 | De quanto em quanto tempo o CLP procura mudança nos tags com `RefreshTime = 0`. Nada abaixo de 100 é aceito (um valor menor é elevado a 100). |
| `SubscriptionCreditLimit` | 10 | Crédito de notificações dessas assinaturas; renovado automaticamente enquanto positivo. |
| `SubscriptionTimeoutMs` | 5000 | Watchdog: uma assinatura que fica em silêncio por este tempo com a sessão viva foi perdida e é criada de novo. Nunca menos de 10 ciclos; `0` desliga. |
| `ProgramCheckIntervalMs` | 10000 | De quanto em quanto tempo o driver procura um download para o CLP (lista de DBs + hora de modificação dos tipos) e resolve os endereços de novo se algo mudou. `0` desliga; abaixo de 1000 é elevado a 1000. |
| `Password` | vazio | Senha do CLP para legitimation. **Ainda não utilizável** — veja *TLS e certificados*. |
| `Username` | vazio | Nome de usuário opcional para a legitimation estilo "novo". Deixe vazio. |
| `ServerCertificate` | vazio | Vazio aceita qualquer certificado que o CLP apresente. Com valor, só aquele certificado (ou um assinado por ele) é aceito. No Inspetor de Objetos o botão "…" carrega de um arquivo `.pem/.crt/.cer/.der`. |
| `ClientCertificate` | vazio | Certificado apresentado ao CLP quando ele pede um (TLS mútuo): texto PEM, nome de arquivo PEM/PKCS#12, ou o base64 de um PKCS#12. Vazio: nenhum, o caso usual. |
| `ClientCertificatePassword` | vazio | Senha da chave privada ou do PKCS#12 de `ClientCertificate`. |

Existe também a propriedade somente leitura `PeerCertificate`: o certificado que o CLP apresentou no último handshake, em base64 do DER. Copie para `ServerCertificate` para confiar só naquele CLP.

##### Endereçamento

O `LongAddress` do tag é o caminho da variável, como escrito no TIA Portal. Duas formas chegam a um DB:

```
DB4.Var26.v1               // pelo número do DB
BlocoSimbolico_4.Var26.v1  // pelo nome simbólico do DB, como no programa
```

Os pontos fazem parte do caminho: `Var26.v1` é o membro `v1` dentro da estrutura `Var26`, exatamente como no programa do CLP. As áreas de processo se chamam `IArea`, `QArea`, `MArea`, `S7Timers` e `S7Counters`, e o nome de um tag pode ele mesmo conter pontos:

```
MArea.Clock_10Hz
QArea.Motor1_Run
S7Timers.T1
```

Que classe de tag usar:

| Variável no CLP | Classe de tag | `LongAddress` |
|---|---|---|
| Um escalar (`Bool`, `Int`, `Real`, …) | `TPLCTagNumber` | `DB4.Var26` |
| Um `STRING` | `TPLCString` | `DB4.Message` |
| Um array de tipo simples | `TPLCBlock` | `DB4.Arr` |
| Um elemento de array | `TPLCBlock` do tamanho de um elemento, ou `TPLCTagNumber` | `DB4.Arr[2]` |
| Uma estrutura | `TPLCStruct` com um `TPLCStructItem` por membro | `DB4.MyStruct` |
| Um DB inteiro | `TPLCStruct` | `DB4` |
| Um array de estruturas | `TPLCStruct` cobrindo o array, com os membros mapeados em todos os índices | `DB4.Arr` |

O **`TagType`** de um `TPLCTagNumber` precisa casar com o tipo que a variável tem no CLP, e o mesmo vale para `SwapBytes`/`SwapWords`/`SwapDWords`: o CLP é big-endian, então um valor de 2 bytes precisa de `SwapBytes`, um de 4 bytes de `SwapBytes` + `SwapWords`, um de 8 bytes dos três. O Tag Builder preenche isso a partir da informação de tipo do próprio CLP; só um tag digitado à mão pode ficar errado.

> **Um DB inteiro não são os bytes dele.** Ler a raiz de um DB devolve um objeto pequeno de metadados, não a memória do DB: um `TPLCStruct` de DB inteiro é lido membro a membro, e escrito do mesmo jeito.

##### Tag Builder

Clique com o botão direito no `TS7CommPlusDriver` → **Tag Builder** abre *S7CommPlus – importação de tags*:

1. Aperte **Ler as variáveis do CLP**. A porta precisa estar `Active = True` antes, senão o assistente avisa e não lê nada. Só a lista de DBs (mais as áreas I, Q e M, se *Incluir as áreas I, Q e M* estiver marcado) é lida neste momento; as variáveis de um DB são lidas quando ele é expandido ou marcado, e a IDE congela pelo tempo de um DB.
2. Escolha o que importar em **O que importar** — cada modo mostra só o que ele importa, e o que está marcado em cada um é guardado, então um único **Importar** cria tudo:

| Modo | O que cada item marcado vira |
|---|---|
| *Tags simples* | um `TPLCTagNumber` (um `TPLCString` para um `STRING`), lido pelo caminho simbólico |
| *Arrays de tipos simples* | um `TPLCBlock`, com a opção de criar também os `TPLCBlockElement` dos elementos |
| *Estruturas* | um `TPLCStruct`, com a opção de criar também os `TPLCStructItem` dos membros. Um DB inteiro também é uma estrutura: marcar o DB cria um `TPLCStruct` lido pelo nome dele, com um item por variável de primeiro nível |
| *Arrays de estruturas* | só o primeiro elemento aparece, com os membros; os membros marcados são mapeados (`TPLCStructItem`) em todos os índices de um `TPLCStruct` que cobre o array inteiro |

3. Ajuste o **tempo de atualização** (ms; `0` = por mudança), o **prefixo do nome** dos componentes criados e, para um bloco ou uma estrutura, se os elementos/itens são criados também. Marcar um item não marca os subitens: o menu do botão direito tem *Marcar todos os subitens* / *Desmarcar todos os subitens*.
4. **Importar**. Os componentes aparecem no form ou data module com o `ProtocolDriver` já ligado.

Uma variável que não pode virar tag é listada com o motivo (*não vira tag: estrutura (use os membros)*, *tipo não suportado*, *elemento de array*, *não foi possível ler o tamanho da STRING*) quando *Mostrar as variáveis que não viram tag* está ligado.

##### Polling ou por mudança

O `RefreshTime` do tag decide:

* **`> 0`** — polling: o driver lê o tag a cada `RefreshTime` ms, agrupando os tags vencidos na menor quantidade de requisições que `MaxItemsPerRequest` permite.
* **`= 0`** — por mudança: o driver cria uma assinatura no CLP, que passa a avisar as mudanças a cada `SubscriptionCycleMs` (mínimo 100 ms). Um valor que inverte mais rápido que metade desse ciclo pode passar despercebido — um clock de 10 Hz inverte a cada 50 ms e é sempre visto no mesmo estado — então dê a esse tag um `RefreshTime` acima de 0 e leia por polling.

As assinaturas são recriadas sozinhas depois de uma sessão perdida ou de uma assinatura derrubada (`SubscriptionTimeoutMs`), e os endereços são resolvidos de novo depois de um download para o CLP (`ProgramCheckIntervalMs`). `RefreshCaches` força isso na hora — chame depois de um download de programa novo se os tags precisam estar certos imediatamente.

##### Alarmes ativos

`ReadActiveAlarms(Languages, Alarms)` do driver devolve os alarmes que estão ativos no CLP agora (S7-1200/1500): um `TS7PlusAlarm` por alarme, com o ID do alarme na CPU, o domínio, o contador de sequência, se é *coming* (chegou) ou *going* (saiu), o horário do evento e do reconhecimento (UTC), os valores associados e os textos de cada idioma. `Languages` é a lista de IDs de idioma (`S7PlusLang_Portuguese_BR` = 1046, `S7PlusLang_English_US` = 1033...); vazia mantém todos os idiomas que o CLP envia. Os textos já trazem os valores associados no lugar das marcas `@1%d@`. É uma única requisição, então pode ser chamada quando se quer um retrato; não cria assinatura e não reconhece os alarmes.

Para ser avisado quando um alarme chega ou sai, assine: `AlarmSubscribe(Languages, SendTexts, CreditLimit)` e chame `AlarmPoll(WaitMs, Alarms)`, que espera até `WaitMs` ms (o driver fica retido nesse tempo, então use um valor curto) e devolve os alarmes que chegaram desde a última chamada; lista vazia com `ioOk` quer dizer que nenhum chegou. Com `SendTexts = True` (padrão) o CLP manda os textos junto com cada notificação; um S7-1200 sem TLS recusa isso, então use `False` nele e leia os textos com `ReadActiveAlarms`. O crédito é renovado sozinho e a assinatura é recriada depois de uma reconexão. `AlarmUnsubscribe` remove a assinatura sem mexer nas assinaturas de dados. Um alarme que já estava ativo quando se assinou não é enviado: pegue esse retrato antes com `ReadActiveAlarms`.

```pascal
var Alarms: TS7PlusAlarmArray; i: Integer;
begin
  Drv.ReadActiveAlarms([S7PlusLang_Portuguese_BR], Alarms);  // o que está ativo agora
  Drv.AlarmSubscribe([S7PlusLang_Portuguese_BR]);              // e o que muda daqui em diante
  while not Terminated do
    if Drv.AlarmPoll(500, Alarms) = ioOk then
      for i := 0 to High(Alarms) do
        WriteLn(S7PlusAlarmToString(Alarms[i], S7PlusLang_Portuguese_BR));
end;
```

A assinatura de alarmes segue o formato das bibliotecas de referência S7CommPlusDriver e s7commplus; ainda não foi validada em todos os modelos de CLP.

##### Registrando os alarmes do CLP: THMIS7PlusAlarmLogger

O `THMIS7PlusAlarmLogger` (descendente de `THMIAlarmLogger`, paleta *PascalSCADA HMI*) leva esses alarmes para o mesmo fluxo do registro de alarmes por tag: aponte `Driver` para o `TS7CommPlusDriver`, `AsyncDBConnection` para a conexão com o banco, ajuste `LanguageId` (ex.: 1046; 0 = todos os idiomas) e `Active = True`. O driver assina assim que a conexão sobe, e de novo a cada reconexão, e entrega os alarmes ao logger; não há polling no seu código. Cada alarme que chega ganha um ID novo (`OnGenerateNewAlarmID`, ou o contador interno) e dispara `OnS7PlusAlarmIncoming`; quando sai, `OnS7PlusAlarmOutgoing` com o mesmo ID. Os dois recebem o `TS7PlusAlarm` (horário da CPU convertido para a hora local, textos, valores associados) e devolvem o SQL a executar, igual a `OnIncomingAlarm`/`OnOutgoingAlarm`; `OnRefreshActiveAlarms` dispara depois. Os eventos são disparados na thread principal. Os `AlarmMessages` por tag continuam funcionando no mesmo componente.

A cada criação da assinatura o logger recebe primeiro os alarmes que já estavam ativos e os confronta com o que ele tem aberto: um alarme que chegou com a conexão caída é aberto, um que saiu nesse meio tempo é fechado (com o horário em que foi percebido). Os alarmes são identificados pelo ID da CPU; um alarme que chega duas vezes sem sair no meio é registrado uma vez. `SendTexts = False` é para o S7-1200 sem TLS (os alarmes chegam sem os textos).

Para o seu próprio código, `TS7CommPlusDriver.AddAlarmHandler(Handler)` entrega os mesmos alarmes (pela thread de scan; `IsSnapshot = True` para a lista dos que já estavam ativos) e `AlarmRequest(...)` pede a assinatura sem precisar que a conexão já esteja aberta.

##### Exemplo passo a passo

Ler a temperatura `"Data".Temperature` (`Real`) de um S7-1200 em 192.168.0.10 e escrever o setpoint `"Data".Setpoint` (`Int`):

1. No TIA Portal: nada a mudar; só anote o nome do DB (`Data`) e compile.
2. Insira um `TTCP_UDPPort`: `Host = 192.168.0.10`, `Port = 102`, `PortType = ptTCP`.
3. Insira um `TS7CommPlusDriver`: `CommunicationPort = TCP_UDPPort1`, deixe `UseTLS = True`.
4. Botão direito no driver → **Tag Builder**: *Ler as variáveis do CLP*, abra o DB `Data`, modo *Tags simples*, marque `Temperature` e `Setpoint`, tempo de atualização 500, **Importar**.
5. Ligue um `THMILabel` ao `Temperature` e um `THMIEdit` ao `Setpoint`. Faça `Port.Active := True` e rode.

O mesmo por código:

```pascal
uses tcp_udpport, S7PlusFamily, PLCTagNumber;

var
  Port: TTCP_UDPPort;
  Drv: TS7CommPlusDriver;
  Temp, Setpoint: TPLCTagNumber;
begin
  Port := TTCP_UDPPort.Create(Self);
  Port.Host := '192.168.0.10';
  Port.Port := 102;

  Drv := TS7CommPlusDriver.Create(Self);
  Drv.CommunicationPort := Port;
  Drv.UseTLS := True;

  Temp := TPLCTagNumber.Create(Self);
  Temp.LongAddress := 'Data.Temperature';  // o caminho simbólico
  Temp.TagType := pttFloat;
  Temp.SwapBytes := True;                  // CLP big-endian
  Temp.SwapWords := True;
  Temp.RefreshTime := 500;                 // 0 = por mudança

  Setpoint := TPLCTagNumber.Create(Self);
  Setpoint.LongAddress := 'Data.Setpoint';
  Setpoint.TagType := pttSmallInt;
  Setpoint.SwapBytes := True;

  Temp.ProtocolDriver := Drv;              // por último
  Setpoint.ProtocolDriver := Drv;

  Port.Active := True;
end;
```

##### TLS e certificados

`UseTLS` vem ligado por padrão e a maioria dos firmwares S7-1200/1500 a partir do V15 exige. O túnel TLS é montado pelo próprio driver, sobre OpenSSL carregado em tempo de execução: `libssl-3-x64.dll` / `libcrypto-3-x64.dll`, ou as versões 1.1, ou as antigas `ssleay32.dll`/`libeay32.dll`. Distribua essas DLLs junto do executável (ou em algum lugar do `PATH`) quando a aplicação usa TLS.

* `ServerCertificate` vazio aceita o certificado que o CLP apresentar. Preencha para fixar um CLP: o handshake então falha para qualquer outro certificado (outro CLP, um equipamento no meio), e o motivo aparece em `OnDebug`. O nome no certificado não é comparado — certificados de CLP raramente trazem o IP.
* `ClientCertificate` acrescenta TLS mútuo: o certificado que este lado apresenta, com a chave privada, como texto PEM, como nome de um arquivo PEM/PKCS#12 (lido a cada conexão, então a chave fica fora do form) ou como o base64 de um PKCS#12. Precisa de OpenSSL 1.1.1 ou mais novo; um PKCS#12 feito com algoritmos antigos (RC2) precisa também do legacy provider do OpenSSL 3.

> **A legitimation por senha não está pronta.** `Password` (e `Username`) fazem o driver fazer login logo após cada conexão nova, mas só o passo de obter o challenge está confirmado em hardware real: o CLP ainda recusa a resposta criptografada. Uma CPU em *Acesso completo com senha* portanto responde leituras e recusa escritas. Use um nível de proteção sem senha, ou aceite acesso somente leitura.

##### Problemas comuns

| Sintoma | Causa provável |
|---|---|
| A porta abre mas nenhum tag atualiza, `ioConnectPLCFailed` | IP errado, porta 102 bloqueada, ou nível de proteção da CPU em *Acesso HMI*/*Sem acesso*. |
| Todo tag falha com `ioAuthError` | A CPU protege o que a sessão precisa e o login não aconteceu — veja *A legitimation por senha* acima. |
| Um tag falha com `ioObjectNotExists` logo após um download | O endereço do programa que foi substituído continua em cache: chame `RefreshCaches` (acontece sozinho dentro de `ProgramCheckIntervalMs`). |
| Escritas falham e leituras funcionam | Nível de proteção *Acesso de leitura*, `ReadOnly = True`, ou *Acesso completo com senha* (sem login). |
| Valores de 16/32 bits absurdos (`1` vira `256`) | Falta `SwapBytes`/`SwapWords` em um tag digitado à mão. |
| Um tag em um array de estruturas lê lixo | Leia como um `TPLCStruct` cobrindo o array: ler o array inteiro devolve um valor que o driver não decodifica e ele já o lê membro a membro. |
| Um sinal rápido é sempre visto no mesmo estado | O tag está por mudança com ciclo maior que o sinal: dê a ele um `RefreshTime` acima de 0. |
| O Tag Builder avisa e não lê nada | A porta não estava `Active = True` quando o assistente foi aberto. |
| Os valores param de chegar depois de um tempo, sem erro | A assinatura ficou em silêncio: verifique `SubscriptionTimeoutMs` (o driver recria) e `OnDebug`. |

##### Exemplos relacionados

* `examples/laz_s7commplus_phase1_test` — acesso direto a área/DB (`ReadArea`, `WriteArea`, `ReadDB`, `WriteDB`) por número, sem tags.
* `examples/laz_s7commplus_phase2_test` — um `TPLCTagNumber` com `LongAddress` simbólico, atualizado por `RefreshTime`; rode como `s7commplus_tag_test <ip-do-clp> <caminho> [refresh-ms]`.
* `examples/laz_s7commplus_structitems_gui` — estruturas e struct items lidos de um DB, com um form pequeno.

As regras do plano de tags (o que vira tag, e com que tipo) ficam em `S7PlusTagPlan` e são testadas sem CLP em `tests/`.
