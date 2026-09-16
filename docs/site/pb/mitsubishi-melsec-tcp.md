##### Introdução

O `TMelsecTCPDriver` fala o **MC Protocol** (MELSEC Communication Protocol) da Mitsubishi Electric sobre TCP/IP, no formato **3E frame, código binário** — o protocolo nativo das portas Ethernet dos CLPs das séries **Q**, **L**, **iQ-F (FX5)** e **iQ-R**, e dos módulos Ethernet QJ71E71/LJ71E71. Ele lê e escreve relés (bits) e registradores (words) diretamente, sem OPC nem software da Mitsubishi no meio.

O driver não fala o formato 1E (série FX3 e anteriores pelo módulo FX3U-ENET) nem o formato ASCII.

##### Componentes necessários

1. Uma ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** com `Host` = IP do CLP e `Port` = porta configurada no CLP para o MC Protocol (veja *Preparando o CLP*). Veja [Portas de comunicação](/pb/communication-ports/).
2. Um ![](img/TMelsecTCPDriver.png) **`TMelsecTCPDriver`** com `CommunicationPort` apontando para a porta.
3. Os tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`) com `ProtocolDriver` apontando para o driver.

##### Preparando o CLP

No GX Works2/GX Works3, nos parâmetros da porta Ethernet da CPU (ou do módulo E71), abra a configuração de conexões (*Open Setting* / *External Device Configuration*) e adicione uma conexão com:

* **Protocol**: TCP;
* **Open system**: MC Protocol (*SLMP* no iQ-R/iQ-F);
* **Code**: **Binary** (não ASCII);
* **Host station port No.**: a porta que vai em `TTCP_UDPPort.Port` (ex.: 5002);
* no iQ-F/iQ-R, marque também *Enable online change* se quiser escrever com a CPU em RUN.

Grave os parâmetros e reinicie a CPU. Cada conexão configurada atende um cliente; para duas estações de supervisão configure duas conexões (portas diferentes).

##### Propriedades do driver

| Propriedade | Padrão | Descrição |
|---|---|---|
| `CommunicationPort` | – | A porta `TTCP_UDPPort`. |
| `ReadSomethingAlways` | `True` | Lê algum tag a cada ciclo mesmo sem necessidade, mantendo a conexão ativa. |
| `Output_M_MaxHole`, `Output_SM_MaxHole`, `Output_L_MaxHole`, `Output_F_MaxHole`, `Output_V_MaxHole`, `Output_X_MaxHole`, `Output_Y_MaxHole`, `Output_B_MaxHole` | 10 | Para cada área de relés, quantos endereços **não mapeados** o driver aceita entre dois tags para ainda lê-los na mesma requisição. |
| `Register_D_MaxHole`, `Register_SD_MaxHole` | 10 | O mesmo para as áreas de registradores. |
| `SerieCLP` | `Serie_Q_L` | Reservada para diferenças entre séries; a versão atual do driver TCP não a usa — deixe no padrão. |

O driver agrupa tags vizinhos da mesma área em uma requisição (até 10 dispositivos por bloco). Um "buraco" maior que `*_MaxHole` entre dois tags gera duas requisições. Se você tem D100 e D125 e nada entre eles, ler os dois de uma vez (`MaxHole ≥ 25`) custa uma mensagem só.

##### Endereçando memórias

Um endereço MELSEC é o **tipo de dispositivo** (M, X, Y, D…) mais o número. No tag:

| Propriedade do tag | Preencha com |
|---|---|
| `MemReadFunction` | Código do dispositivo (tabela abaixo). |
| `MemWriteFunction` | O **mesmo** código — leitura e escrita usam a mesma tabela. |
| `MemAddress` | Número do dispositivo, **em decimal**. |
| `PLCStation` | Deixe `0` em todos os tags de um driver. O valor não vai no frame (rede 0, PC `FF`); serve só para o driver agrupar tags do mesmo CLP. |
| `MemFile_DB`, `MemSubElement`, `PLCRack`, `PLCSlot` | Não usados. |

| Código | Dispositivo | Tipo | Descrição |
|---|---|---|---|
| 1 | `M` | bit | Relé interno |
| 2 | `SM` | bit | Relé especial |
| 3 | `L` | bit | Relé de retenção (latch) |
| 4 | `F` | bit | Anunciador |
| 5 | `V` | bit | Relé de borda |
| 6 | `X` | bit | Entrada |
| 7 | `Y` | bit | Saída |
| 8 | `B` | bit | Relé de link |
| 9 | `D` | word | Registrador de dados |
| 16 | `SD` | word | Registrador especial |

Timers, contadores (`T`, `C`, `ST`), registradores de link (`W`, `R`, `ZR`) e as áreas `SB`/`DX`/`DY` não estão implementados nesta versão.

**Atenção aos endereços X e Y**: no CLP eles são numerados em **hexadecimal** (`X0`…`X1F`, `Y20`…). `MemAddress` é decimal, então `X10` (hex) é `MemAddress = 16`, e `Y2F` é `MemAddress = 47`. Os demais dispositivos (M, D, …) já são decimais no CLP.

Tipos de dados:

| No CLP | `TagType` | Observação |
|---|---|---|
| Relé (M, X, Y…) | `pttDefault` | Um dispositivo = um bit; `Value` é 0 ou 1. `TPLCBlock` com `Size = n` lê n relés consecutivos. |
| `D` de 16 bits | `pttDefault`, `pttSmallInt` ou `pttWord` | Um registrador. |
| Dois `D` como 32 bits (`DMOV`, `DINT`) | `pttLongInt` / `pttDWord` | Ocupa `D[n]` e `D[n+1]`; a ordem *low word first* do MELSEC é a que o driver assume — sem swaps. |
| Dois `D` como `REAL` (`EMOV`) | `pttFloat` | Idem. |
| Texto em `D` (`$MOV`) | `TPLCString`, `StringType = stC` | Dois caracteres por registrador, byte baixo primeiro. `StringSize` em caracteres. |

Exemplos:

| Endereço MELSEC | `MemReadFunction` / `MemWriteFunction` | `MemAddress` | `TagType` |
|---|---|---|---|
| `M100` | 1 | 100 | `pttDefault` |
| `X1A` | 6 | 26 | `pttDefault` |
| `Y0` a `Y7` | 7 | 0 | `TPLCBlock`, `Size = 8` |
| `D200` | 9 | 200 | `pttSmallInt` |
| `D300` como `REAL` | 9 | 300 | `pttFloat` |
| `D400` a `D409` | 9 | 400 | `TPLCBlock`, `Size = 10` |
| `SD210` (relógio) | 16 | 210 | `pttWord` |

Para um bit dentro de um `D`, use um [`TTagBit`](/pb/tags/#TTagBit) sobre o tag do registrador — o MC Protocol lê palavras inteiras.

##### Exemplo passo a passo

Ler o nível `D100` e comandar a bomba `M50` em um FX5U em 192.168.3.250, MC Protocol na porta 5002:

1. GX Works3 → parâmetros da CPU → *Ethernet Port* → *External Device Configuration*: adicione *SLMP Connection Module*, TCP, porta 5002, código binário. Grave e reinicie.
2. `TTCP_UDPPort`: `Host = 192.168.3.250`, `Port = 5002`, `Active = True`.
3. `TMelsecTCPDriver`: `CommunicationPort = TCP_UDPPort1`.
4. `TPLCTagNumber` `Nivel`: `ProtocolDriver = MelsecTCPDriver1`, `MemReadFunction = 9`, `MemWriteFunction = 9`, `MemAddress = 100`, `TagType = pttSmallInt`.
5. `TPLCTagNumber` `Bomba`: `MemReadFunction = 1`, `MemWriteFunction = 1`, `MemAddress = 50`.
6. Um `THMILabel` em `Nivel` e um `THMICheckBox` em `Bomba` (`ValueTrue = 1`, `ValueFalse = 0`).

##### Problemas comuns

| Sintoma | Causa provável |
|---|---|
| Porta não abre | IP/porta errados, ou a conexão MC Protocol não foi criada/gravada no CLP. |
| Porta abre mas os tags dão `ioDriverError` | Conexão configurada como **ASCII** ou como *MELSOFT connection* em vez de MC Protocol/SLMP binário. |
| `ioPLCError` em alguns tags | O CLP recusou (*end code* ≠ 0): dispositivo inexistente ou fora da faixa da CPU, ou escrita bloqueada com a CPU em RUN. |
| `X`/`Y` lendo o vizinho | `MemAddress` informado em hexadecimal; converta para decimal. |
| Um segundo supervisório não conecta | Cada conexão MC Protocol atende um cliente; crie outra conexão com outra porta. |

##### Exemplos relacionados

Não há um exemplo específico para Melsec no repositório. Os exemplos Modbus TCP (`examples/laz_modbus_tcp_example`) e ISOTCP (`examples/laz_isotcp_demonstration`) mostram a mesma estrutura porta → driver → tags → controles; só o driver e as propriedades de endereço mudam.
