##### Introdução

O `TISOTCPDriver` fala o protocolo **S7comm sobre ISO-on-TCP** (RFC 1006, porta TCP 102) — o mesmo que o STEP 7, o TIA Portal e a LibNodave usam para ler e escrever memórias de um CLP Siemens. O driver é uma reescrita em Pascal do ISOTCP da LibNodave de Thomas Hergenhahn: não depende de nenhuma DLL ou biblioteca externa.

Ele conversa com:

* **S7-300, S7-400, ET200S/ET200SP** com CPU ou CP Ethernet;
* **S7-1200 e S7-1500** — depois de liberar o acesso PUT/GET e desligar o acesso otimizado dos DBs (veja *Preparando o CLP*);
* **S7-200** através de um CP 243-1 (`ConnectionWay = ISOTCP_VIA_CP243`);
* **LOGO! 0BA7/0BA8** (LOGO! 7 e 8), que expõe a área V como DB1;
* qualquer simulador ou gateway compatível com S7comm (PLCSIM com NetToPLCSim, Snap7 server, etc.).

Ele **não** conversa com o S7-1200/1500 pelo protocolo nativo deles (S7CommPlus, que é o que o TIA Portal usa para acesso simbólico) — para isso existe o [`TS7CommPlusDriver`](/pb/s7commplus/). O ISOTCP continua sendo a opção mais simples para esses CLPs quando você controla o projeto no TIA e pode desligar o acesso otimizado.

##### Componentes necessários

1. Uma ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** com `Host` = IP do CLP, `Port = 102` (padrão) e `PortType = ptTCP`. Veja [Portas de comunicação](/pb/communication-ports/).
2. Um ![](img/TISOTCPDriver.png) **`TISOTCPDriver`** com `CommunicationPort` apontando para a porta.
3. Os tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCStruct`, `TPLCString`) com `ProtocolDriver` apontando para o driver.

O jeito mais rápido de chegar ao passo 3 é clicar com o botão direito no `TISOTCPDriver` e escolher **Tag Builder** (veja abaixo).

##### Propriedades do driver

| Propriedade | Padrão | Descrição |
|---|---|---|
| `CommunicationPort` | – | A porta `TTCP_UDPPort` usada pelo driver. |
| `PLCRack` | 0 | Rack da CPU. |
| `PLCSlot` | 0 | Slot da CPU. |
| `PLCStation` | 2 | Endereço da estação. Só tem significado em redes MPI/PROFIBUS atrás de um gateway; em Ethernet deixe o padrão. |
| `ConnectionWay` | `ISOTCP` | `ISOTCP` para conexão direta à CPU/CP Ethernet; `ISOTCP_VIA_CP243` para S7-200 atrás de um CP 243-1. |
| `ISOTCPConnType` | `ctOP` | Tipo de conexão que o driver se identifica ao CLP: `ctPG` (programadora), `ctOP` (painel de operação) ou `ctBasic` (outra). O CLP reserva recursos por tipo; se todas as conexões OP estiverem em uso, troque para `ctBasic`. |
| `ForcePDUSize` | `pduAuto` | Tamanho de PDU negociado. `pduAuto` pede o maior que o CLP aceita; force `pdu240` para CLPs antigos ou LOGO!, que não negociam corretamente. |
| `ReadOnly` | `False` | Se `True`, o driver recusa qualquer escrita (`ioReadOnlyProtocol`). Útil para uma tela de supervisão que nunca deve alterar o processo. |
| `ReadSomethingAlways` | `True` | Mantém a conexão viva lendo algum tag a cada ciclo mesmo que nenhum precise ser atualizado. |

> **Uma conexão ISOTCP fala com um único CLP.** Por isso, as propriedades `PLCRack`, `PLCSlot` e `PLCStation` **dos tags são ignoradas**: o que vale é o que está configurado no driver. Para falar com dois CLPs, use duas portas e dois drivers.

Valores de rack/slot que funcionam na maioria dos casos:

| CPU | `PLCRack` | `PLCSlot` |
|---|---|---|
| S7-300 (CPU com PN integrada ou CP 343-1) | 0 | 2 |
| S7-400 | 0 | slot físico da CPU (normalmente 2 ou 3) |
| S7-1200 / S7-1500 | 0 | 0 (1 também funciona no 1200) |
| ET200S / ET200SP CPU | 0 | 2 (ET200S) / 0 (ET200SP) |
| S7-200 via CP 243-1 | 0 | 0 |
| LOGO! 7 / 8 | 0 | 0 (ver *Preparando o CLP*) |

Em código, `UpdatePLCAddress(Rack, Slot, Station)` troca os três de uma vez sem reiniciar a conexão três vezes.

##### Preparando o CLP

**S7-300 / S7-400**: nada a fazer além de ter o IP configurado. Se a CPU tiver senha de proteção contra escrita, o driver lê mas não escreve.

**S7-1200 / S7-1500** (no TIA Portal):

1. Nas propriedades da CPU → *Protection & Security* → *Connection mechanisms*: marque **Permit access with PUT/GET communication from remote partner**.
2. Em cada DB que o PascalSCADA vai acessar, desmarque **Optimized block access** (propriedades do DB → *Attributes*) e recompile. Um DB otimizado não tem endereços absolutos, e o ISOTCP só endereça por byte.
3. Se o nível de proteção da CPU for *Full access*, nada mais. Com *Read access* o driver só lê; com *HMI access* ou *No access* a conexão é recusada.

**LOGO! 7 / 8** (no LOGO!Soft Comfort): em *Tools → Ethernet connections*, adicione uma **conexão de servidor** com TSAP local `20.00` e TSAP remoto `10.00`, ou marque *Allow all connections*. A área V (VB/VW/VD) aparece para o PascalSCADA como **DB1** (`MemReadFunction = 4`, `MemFile_DB = 1`); as memórias M como área M (`MemReadFunction = 3`). O exemplo `examples/demo_logo8` mostra isso, com `ForcePDUSize = pdu240` e `PLCStation = 0`.

**S7-200**: use `ConnectionWay = ISOTCP_VIA_CP243` e configure uma conexão no CP 243-1 pelo assistente do STEP 7-Micro/WIN. A área V é `MemReadFunction = 4` com `MemFile_DB = 1`.

##### Endereçando memórias

Todo endereço S7 é composto de uma **área**, um **número de DB** (só para a área DB), um **endereço em bytes** e o **tipo** da variável. No tag isso vira:

| Propriedade do tag | Preencha com |
|---|---|
| `MemReadFunction` | Código da área (tabela abaixo). |
| `MemFile_DB` | Número do DB, quando a área for 4. Ignorado nas outras. |
| `MemAddress` | Endereço do **byte** inicial (o número depois de `IB`, `MW`, `DBD`…). |
| `TagType` | Tipo da variável (tabela mais abaixo). |
| `MemWriteFunction`, `MemSubElement`, `PLCRack/Slot/Station` | Não usados. Deixe em zero. |

Códigos de área para `MemReadFunction`:

| Código | Área | CPUs | Observação |
|---|---|---|---|
| 1 | `I` — entradas digitais (imagem de processo) | todas | |
| 2 | `Q` — saídas digitais (imagem de processo) | todas | |
| 3 | `M` — memórias/flags | todas | |
| 4 | `DB` — bloco de dados | 300/400/1200/1500; V no S7-200 e LOGO! | Preencha `MemFile_DB`. |
| 5 | `C` — contadores | 300/400 | `TagType = pttWord`. |
| 6 | `T` — temporizadores | 300/400 | `TagType = pttWord`. |
| 7 | `SM` — memória especial | S7-200 | |
| 8 | `AI` — entradas analógicas | S7-200 | `TagType = pttWord`. |
| 9 | `AQ` — saídas analógicas | S7-200 | `TagType = pttWord`. |
| 10 | `C` — contadores | S7-200 | `TagType = pttWord`. |
| 11 | `T` — temporizadores | S7-200 | `TagType = pttWord`. |
| 12 | `PIW` — periferia de entrada | 300/400 | `TagType = pttWord`. Lê o valor direto do cartão, sem passar pela imagem de processo. |

Tipos de dados S7 e o `TagType` correspondente:

| Tipo no CLP | Bytes | `TagType` | `SwapBytes` | `SwapWords` | `SwapDWords` |
|---|---|---|---|---|---|
| `BOOL` | – | `pttByte` no tag + um [`TTagBit`](/pb/tags/#TTagBit) por bit | | | |
| `BYTE`, `CHAR`, `USINT` | 1 | `pttByte` | | | |
| `SINT` | 1 | `pttShortInt` | | | |
| `WORD`, `UINT`, `S5TIME`, `DATE` | 2 | `pttWord` | ✔ | | |
| `INT` | 2 | `pttSmallInt` | ✔ | | |
| `DWORD`, `UDINT`, `TIME_OF_DAY` | 4 | `pttDWord` | ✔ | ✔ | |
| `DINT`, `TIME` | 4 | `pttLongInt` | ✔ | ✔ | |
| `REAL` | 4 | `pttFloat` | ✔ | ✔ | |
| `LINT`, `LTIME` | 8 | `pttInt64` | ✔ | ✔ | ✔ |
| `ULINT`, `LWORD` | 8 | `pttQWord` | ✔ | ✔ | ✔ |
| `LREAL` | 8 | `pttDouble` | ✔ | ✔ | ✔ |
| `STRING[n]` | n + 2 | `TPLCString` com `StringType = stSIEMENS` | | | |

Os CLPs Siemens guardam os valores em **big-endian** e o PascalSCADA monta os bytes na ordem da máquina (little-endian). Por isso tudo que tem mais de um byte precisa dos `Swap*` marcados como na tabela — sem eles, um `INT` = 1 aparece como 256 e um `REAL` vira um número absurdo. O Tag Builder já marca os swaps corretos.

Exemplos:

| Endereço S7 | `MemReadFunction` | `MemFile_DB` | `MemAddress` | `TagType` | Swaps |
|---|---|---|---|---|---|
| `IB3` | 1 | 0 | 3 | `pttByte` | – |
| `Q0.5` | 2 | 0 | 0 | `pttByte` + `TTagBit` com `StartBit = EndBit = 5` | – |
| `MW10` | 3 | 0 | 10 | `pttWord` | B |
| `MD100` (como `REAL`) | 3 | 0 | 100 | `pttFloat` | B + W |
| `DB5.DBW20` | 4 | 5 | 20 | `pttSmallInt` | B |
| `DB5.DBD24` (como `DINT`) | 4 | 5 | 24 | `pttLongInt` | B + W |
| `DB10.DBB0` até `DBB49` | 4 | 10 | 0 | `TPLCBlock` `pttByte`, `Size = 50` | – |
| `DB12` `STRING[30]` em `DBB8` | 4 | 12 | 8 | `TPLCString`, `StringSize = 30`, `stSIEMENS` | – |
| `VW6` (LOGO!/S7-200) | 4 | 1 | 6 | `pttSmallInt` | B |
| `T5` (S7-300) | 6 | 0 | 5 | `pttWord` | B |

##### Bits

O ISOTCP do PascalSCADA lê e escreve **por byte**. Para um `BOOL` (uma entrada, uma saída, um bit de um DB), crie um `TPLCTagNumber` com `TagType = pttByte` apontando para o byte que contém o bit e um [`TTagBit`](/pb/tags/#TTagBit) para cada bit de interesse (`StartBit = EndBit = número do bit`). A escrita no `TTagBit` lê o byte atual, altera só aquele bit e grava o byte de volta.

Para muitos bits no mesmo byte, clique com o botão direito no tag e escolha **Map bits**: ele cria os oito `TTagBit` de uma vez. O exemplo `examples/laz_isotcp_mapping_bits_from_other_tag` faz isso com `DB1.DBB0`.

##### Blocos, estruturas e strings

* Um **`TPLCBlock`** lê `Size` variáveis consecutivas do mesmo tipo em uma requisição — um array de `REAL` num DB, por exemplo (`TagType = pttFloat`, `Size` = número de elementos, `MemAddress` = byte inicial). Os swaps valem para o bloco inteiro.
* Um **`TPLCStruct`** lê `Size` bytes consecutivos, e cada `TPLCStructItem`/`TPLCStructString` interpreta um trecho com seu próprio tipo e swap. É a forma natural de mapear um DB com campos mistos ou uma UDT — veja o exemplo em [Tags](/pb/tags/#TPLCStruct).
* Um **`TPLCString`** com `StringType = stSIEMENS` lê uma `STRING[n]`: os dois bytes de cabeçalho (tamanho máximo e tamanho atual) são tratados pelo driver. Use `stC` só para arrays de `CHAR` terminados em zero.

O driver junta automaticamente tags vizinhos da mesma área e do mesmo `RefreshTime` em uma requisição só, respeitando o tamanho da PDU negociado. Com `pduAuto` numa CPU moderna cabem até 1920 bytes de dados por mensagem; num S7-300 antigo ou LOGO!, 240. Se o `AvgUpdateRate` dos tags ficar muito acima do `RefreshTime`, o CLP não está dando conta — aumente o `RefreshTime` dos tags menos críticos ou agrupe-os em blocos.

##### Tag Builder

Botão direito no `TISOTCPDriver` → **Tag Builder** abre o assistente *Siemens S7 Tag builder*:

1. Escolha a **área** (entradas, saídas, flags, DB, contadores, timers, áreas do S7-200…), o **tipo de tag** a criar (`TPLCTagNumber` separados, um `TPLCBlock`, ou `TPLCStruct`) e o tempo de varredura.
2. Para tags separados ou bloco: informe o tipo de dado, o endereço inicial, a quantidade e o DB. O assistente já marca `SwapBytes`/`SwapWords` conforme o tipo.
3. Para estruturas: monte a lista de itens (nome, tipo, swap, scan) na aba *Items declaration*; o assistente calcula os offsets e pode gerar N estruturas iguais em sequência (útil para arrays de UDT).

Os componentes criados aparecem no form/datamodule com `ProtocolDriver` já ligado.

##### Exemplo passo a passo

Ler a temperatura `DB1.DBD0` (`REAL`) e escrever o setpoint `DB1.DBW4` (`INT`) de um S7-1200 em 192.168.0.10:

1. No TIA Portal: libere PUT/GET, desmarque *Optimized block access* no DB1, compile e carregue.
2. Solte um `TTCP_UDPPort`: `Host = 192.168.0.10`, `Port = 102`, `Active = True`.
3. Solte um `TISOTCPDriver`: `CommunicationPort = TCP_UDPPort1`, `PLCRack = 0`, `PLCSlot = 0`.
4. Solte um `TPLCTagNumber` chamado `Temperatura`: `ProtocolDriver = ISOTCPDriver1`, `MemReadFunction = 4`, `MemFile_DB = 1`, `MemAddress = 0`, `TagType = pttFloat`, `SwapBytes = True`, `SwapWords = True`.
5. Solte outro chamado `Setpoint`: idem, com `MemAddress = 4`, `TagType = pttSmallInt`, `SwapBytes = True`.
6. Ligue um `THMILabel` em `Temperatura` e um `THMIEdit` em `Setpoint`. Rode.

O mesmo em código, para quem cria os componentes em tempo de execução:

```pascal
uses tcp_udpport, ISOTCPDriver, PLCTagNumber, ProtocolTypes;

var
  Port: TTCP_UDPPort;
  Drv: TISOTCPDriver;
  Temp: TPLCTagNumber;
begin
  Port := TTCP_UDPPort.Create(Self);
  Port.Host := '192.168.0.10';
  Port.Port := 102;

  Drv := TISOTCPDriver.Create(Self);
  Drv.CommunicationPort := Port;
  Drv.PLCRack := 0;
  Drv.PLCSlot := 0;

  Temp := TPLCTagNumber.Create(Self);
  Temp.MemReadFunction := 4;   // DB
  Temp.MemFile_DB := 1;
  Temp.MemAddress := 0;
  Temp.TagType := pttFloat;
  Temp.SwapBytes := True;
  Temp.SwapWords := True;
  Temp.RefreshTime := 500;
  Temp.ProtocolDriver := Drv;  // por último

  Port.Active := True;
end;
```

Outros exemplos no repositório: `examples/laz_isotcp_demonstration` (DB com bloco, string e bits), `examples/laz_isotcp_hourmeter` (horímetro com escrita), `examples/laz_isotcp_performance` (medição de taxa de atualização) e `examples/demo_logo8`.

##### Problemas comuns

| Sintoma | Causa provável |
|---|---|
| Porta abre (`Active = True`) mas nenhum tag atualiza; `LastSyncReadStatus = ioConnectPLCFailed` | Rack/slot errados, ou S7-1200/1500 sem PUT/GET liberado, ou nível de proteção *No access*. |
| Porta não abre (`OnCommPortOpenError`) | IP errado, porta 102 bloqueada por firewall, ou o CLP já atingiu o limite de conexões (troque `ISOTCPConnType`). |
| Tags de um DB específico dão `ioIllegalMemoryAddress` / `ioObjectNotExists` | DB não existe, é menor que o endereço pedido, ou (1200/1500) ainda está com *Optimized block access*. |
| Valores de 16/32 bits absurdos (`1` vira `256`, `REAL` vira `1E-40`) | Falta `SwapBytes`/`SwapWords`. |
| Escritas falham com `ioReadOnlyProtocol` ou `ioObjectAccessNotAllowed` | `ReadOnly = True` no driver, ou a CPU está com proteção de escrita/senha. |
| Funciona no S7-300 e não no LOGO! | Force `ForcePDUSize = pdu240` e confira o TSAP da conexão de servidor no LOGO!Soft. |
| Timers/contadores lidos como zero | `TagType` precisa ser `pttWord` nessas áreas. |
