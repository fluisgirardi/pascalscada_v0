# S7CommPlus Subscriptions — investigação (2026-08-21 a 2026-08-30)

## Status atual

**Subscriptions continuam bloqueadas**, mas o bloqueio de `SetupSession` na `.211` (que
travava qualquer teste nesse CLP) teve progresso real em 2026-08-30: o RST desapareceu.
O polling normal da Fase 2 (`TS7CommPlusDriver.DoScanRead`/`DoGetValue` via `Path`
simbólico) continua funcionando normalmente e não foi afetado por nada desta investigação.

## Atualização 2026-08-30 — handshake SessionKey portado, RST do SetupSession eliminado

Nesta sessão, o handshake de chave de sessão da Siemens (`OMSp_core_managed.dll`) que a
seção anterior deste documento não conseguiu decodificar foi **portado inteiramente para
Pascal nativo** a partir do projeto open-source HarpoS7 (bonk-dev/HarpoS7, MIT), via sua
adaptação em Python no `python-snap7` upstream (`s7commplus/session_auth/`) - cifra de
permutação proprietária ("Family 0"), BigInt, AES-CTR customizado, fingerprinting, tudo
validado byte a byte contra os vetores de teste reais do HarpoS7 (blob de 180 bytes E
chave de sessão derivada batendo exatamente para os dois casos de referência, S7-1500 e
S7-1200). Os 45 units (`src/scada/s7plusharpo*.pas`) já estão integrados ao
`pascalscada.lpk`.

Testando ao vivo contra a `.211` (rede/VPN precisou ser reestabelecida - ver histórico),
descobriu-se que o próprio `CreateObject` já manda os dois insumos que faltavam, direto no
formato que o código de chaves (`S7PlusHarpoKeys.ParseFingerprint`/`GetPublicKey`) espera:

- **AttrId 233** (`S7PlusObjId_ServerCertificateFingerprint` - mesmo número usado em outro
  contexto por `S7PlusIds_ObjectVariableTypeName`, ids são reaproveitados entre
  esquemas): string `"00:181B7B0847D11694"` - exatamente o fingerprint da nossa chave já
  vendorizada (família S7-1500).
- **AttrId 303** (`S7PlusObjId_ServerSessionRequest`, já existia no código com esse nome):
  array de 20 bytes - o nonce/challenge que `AuthenticateRealPlc` espera.

Isso já resolve, sozinho, a suposição errada do próprio `python-snap7` de que o handshake
só é necessário quando `ServerSessionVersion` está AUSENTE - na `.211` ele vem presente E
o handshake é exigido mesmo assim.

Implementado em `s7plusconnection.pas`:
- `ParseAttributeRawValue` (novo, em `s7pluscodec.pas`) - extrator genérico de atributo por
  id a partir de um payload de resposta, reaproveitando `DecodePValueToBytes`.
- `CreateSession` agora popula `FServerFingerprint`/`FServerChallenge` a partir dos
  atributos acima.
- `SetupSession` agora, quando os dois estão presentes, constrói o blob via
  `S7PlusHarpoLegacyAuth.AuthenticateRealPlc` e manda um SEGUNDO atributo (1830,
  `S7PlusObjId_SessionSetupLegitimation`) junto com o 306 já existente no mesmo
  `SetMultiVariables`.
- Corrigido de quebra: `S7PlusRandomBytes` (usado em toda a geração de chaves do Harpo)
  dependia do OpenSSL já estar carregado, o que só acontecia via `ActivateTLS` - como esse
  caminho roda com `UseTLS=false`, a lib nunca era carregada e as chaves saíam vazias
  silenciosamente. `SetupSession` agora chama `S7PlusSSLLoad` explicitamente antes de
  construir o blob.

**Resultado**: o `SetupSession` de 2 atributos (306+1830) já não é mais rejeitado com RST -
o CLP processa a requisição e devolve uma resposta estruturada com um `ReturnValue`
decodificável (formato documentado no dissector Wireshark `packet-s7comm_plus.c`: 64 bits
com `errorcode` Int16 nos bits 0-15, `omsline` nos bits 16-31, `errorsource` no byte
32-39, `genericerrorcode`+flag `servererror` no byte 40-47). Testado:

- 306+1830 (array de bytes plano, `EncodePValueByteArray`): `errorcode=-32`,
  `genericerrorcode=8`, `servererror=true`.
- 306+1830 (mesmo blob, mas como `S7PlusType_BLOB`/`EncodePValueBlob`): **resultado
  idêntico** - a PLC não distingue entre os dois wrappers para esse erro.
- 1830 sozinho (sem 306): `errorcode=-2`, `genericerrorcode=9`, com uma string extra
  ("ServerSession") na resposta.
- 1830 (array plano) como `S7PlusType_USINT` explícito em vez de `S7PlusType_BYTE`
  (a mesma convenção que a própria PLC usa pro nonce no atributo 303): **resultado
  idêntico** ao array `BYTE` (`errorcode=-32`, `genericerrorcode=8`).
- 1830 envelopado como `S7PlusType_STRUCT` genérico (StructId=0, 1 membro
  key=1→array de bytes, terminador key=0 - o formato "normal struct" que o próprio
  `DecodePValueToBytes` usa pra decodificar structs de leitura): **resultado idêntico**
  de novo.
- 306+1830+299 (3 atributos, ecoando o valor bruto 0x20000002/UDINT que a PLC mandou pro
  299 em CreateObject): `errorcode=-4`, `genericerrorcode=38` - **muda de novo**.

Ou seja, toda variação testada muda o código de erro (confirmando que a PLC está de fato
avaliando os atributos, não rejeitando a forma do frame) mas nenhuma convergiu pra sucesso,
e **não existe, publicamente, nenhuma tabela pra esses `errorcode`/`genericerrorcode`**
(conferido diretamente no dissector Wireshark do S7Comm-Plus, `packet-s7comm_plus.c`:
`errorcode_names` só lista `-12/-17/-134/-255`, nada de `-2/-4/-32`; não existe nenhuma
tabela pra `genericerrorcode`).

## Atualização 2026-08-30 (mais tarde) — captura TIA real decodificada byte a byte, estrutura do atributo 1830 resolvida

O usuário forneceu uma captura real (`subscription_capture2.pcapng`, TIA Portal V15 contra
a própria `.211`, texto claro) com **4 sessões independentes**, cada uma com um
`SetMultiVariables` de `SetupSession` bem-sucedido (`ReturnValue=0`, resposta de 7 bytes).
Reassemblado via scapy (streams TCP completos) e decodificado byte a byte com um parser
Python que espelha exatamente `DecodePValueToBytes`/`SkipTypedValue` do nosso próprio
`s7pluscodec.pas`. Comparando as 4 sessões entre si (mesma PLC, sessões diferentes) isolou
com certeza o que é fixo vs. o que varia por sessão - resultado completo:

- **A forma da mensagem estava errada**: os 3 `AttrId`s (1830, 306, 299) vêm **todos juntos
  no início** (`ItemCount`/`FieldCount` VLQ, depois a lista dos 3 ids), e só depois vêm os
  pares `[ItemNumber VLQ][Value]`, um por atributo, na mesma ordem - **não**
  `[AttrId,ItemNumber,Value]` intercalado por atributo, que é o que todas as tentativas
  anteriores (incluindo o código já commitado) faziam.
- **Atributo 1830 não é um array nem um BLOB solto - é um `STRUCT` (id 1800)** com 5
  membros:
  - `1801`: UDINT=0, `1802`: USINT=0 (constantes nas 4 sessões).
  - `1803`: STRUCT aninhado (id 1825) = **identidade da chave pública do servidor**:
    `1826` (ULINT) = o próprio `KeyId` do fingerprint (`"181B7B0847D11694"` parseado como
    inteiro de 64 bits) - constante nas 4 sessões porque é a mesma PLC/chave; `1827`
    (UDINT) = 16, que é exatamente `S7PlusHarpoBlobMeta.GetPublicKeyFlags(hkfS7_1500)` -
    já tínhamos essa função portada; `1828`=0.
  - `1804`: STRUCT aninhado (mesmo id 1825) = **identidade da chave simétrica do
    cliente**: `1826` (ULINT) = os mesmos 8 bytes que já escrevemos no offset 16 do blob
    (`DeriveKeyId(Key2)`), só que reinterpretados como inteiro big-endian (confirmado
    comparando com os bytes do próprio blob capturado) - por isso parecia "aleatório",
    mas é 100% determinístico a partir do `Key2` que já geramos; `1827` (UDINT) = 65537,
    fixo nas 4 sessões (não é nenhuma das flags de família já portadas); `1828`=0.
  - `1805`: BLOB = exatamente o nosso blob HarpoS7 de 180 bytes (`EncodePValueBlob`).
- **Atributo 299 não é eco de nada** - é sempre o literal `UDINT=1`, nas 4 sessões.
- **Atributo 306 continua sendo eco puro** do que a própria PLC mandou - confirmado, sem
  mudança necessária.
- O "rodapé" (`fill byte`+`ObjectQualifier`+padding final) que já mandávamos **já batia**
  byte a byte com a captura real - não precisou de nenhuma mudança.

Implementado em `s7plusconnection.pas` (`BuildAttr1830Value`, `QWordFromLEBytesReversed`,
reescrita do laço de montagem do payload em `SetupSession`) e testado ao vivo contra a
`.211` assim que a VPN voltou: **o payload enviado bate byte a byte com a captura real**
em toda a parte fixa (structs 1800/1825, flags, blob header) - só diverge exatamente onde
deveria (o `Key2`/desafio/blob, que são gerados frescos a cada sessão, e a versão do
firmware ecoada no atributo 306, que mudou de V2.6 na captura pra V2.9 na sessão ao vivo -
mesma CPU, provavelmente atualizada entre a captura e agora).

**Ainda não deu certo**: o `SetupSession` agora retorna um erro NOVO e diferente de todos
os anteriores (`errorcode=-3`, `genericerrorcode=43`), e a resposta estendida de erro
menciona explicitamente a classe `ServerSession` (`ClassId=287`, mais uma string
`"ServerSession"`) - não estava presente nos testes anteriores com a estrutura errada
(array/BLOB/struct genérico), que davam um erro mais "genérico" sem essa menção. Isso
sugere que a PLC agora consegue **desserializar** a estrutura corretamente (a forma está
certa) e falha em uma validação mais profunda, possivelmente relacionada à classe do
objeto sendo modificado ou a algo no processo de autenticação em si que ainda não foi
replicado (ex.: um passo adicional específico da versão de firmware V2.9.2, que pode ser
mais recente que a capturada). Ainda sem tabela pública pra decodificar
`errorcode=-3`/`genericerrorcode=43` especificamente.

Scripts de decodificação usados (fora do pacote, reutilizáveis se retomar):
`/tmp/harpo_capture_build/decode_pvalue.py` (decodificador genérico de PValue, espelha
`DecodePValueToBytes`), `/tmp/harpo_capture_build/parse_s7plus.py` (reassembla streams TCP
do pcap via scapy e separa frames S7+ por função).

## Atualização 2026-08-31 — regressão na `.210` encontrada e corrigida; Subscriptions testado nela

Ao testar `s7commplus_subscription_test` contra a `.210` (que já tinha sessão 100%
validada antes de qualquer coisa desta investigação de SessionKey), a legitimação passou
a ser tentada nela também - `.210` também manda fingerprint+challenge no `CreateObject`,
coisa que não estávamos extraindo até agora e passou a acionar `NeedsLegitimation`. Só que
`.210` usa TLS de verdade, e mandar o `SetupSession` com o blob de legitimação nela fez a
conexão TLS cair logo em seguida - **regressão real** num caminho que já funcionava.

Corrigido: `NeedsLegitimation` agora também exige `not FTLSActive`. Hipótese (consistente
com tudo observado até aqui): a legitimação via `SessionKey`/atributo 1830 é o substituto
específico para quando **não há TLS** - TLS já fornece autenticação de servidor via
certificado por outro canal, então a PLC não exige a legitimação adicional quando o TLS já
está ativo, mesmo mandando os mesmos campos de fingerprint/challenge no `CreateObject`.

Testado de novo contra `.210` depois do fix: `SetupSession: returnValue=0 -> True`,
`sessionSetupOK=True`, `BrowseDB` funcionando (35 variáveis em DB4) - **voltou a funcionar
exatamente como antes**, confirmando que a regressão foi resolvida sem reintroduzir o
problema original.

Com a sessão 100% saudável, testamos `SubscriptionCreate` na `.210` pela primeira vez
desde os fixes de `SymbolCrc`/`FSubscriptionsContainerObjectId` desta semana: **mesmo
erro original** - `"Download error (IDs & states [main, sub, TI, next])!"`
(`returnValue=-2161304508979544098`). Ou seja, mesmo com sessão perfeitamente válida
(sem depender de nenhuma legitimação), Subscriptions continuam bloqueadas na `.210` por
um motivo **diferente e ainda não identificado** - não relacionado ao handshake
SessionKey investigado aqui. Esse é o próximo mistério a decifrar, separado do bloqueio
de `SetupSession` na `.211`.

## Atualização 2026-08-31 (mais tarde) — "Download error" resolvido: 3 bugs reais de encoding

Testamos se as referências (python-snap7 e thomas-v2/C#) conseguem de fato fazer
Subscriptions funcionar. Não foi possível testar o C# (sem `dotnet` instalado nesta
máquina - decisão do usuário: não instalar, resultado do Python já bastava). O Python
**conseguiu, de verdade**: usando um branch local (`test-subscriptions-merged`, mesclando
`upstream/fix-v1-sessionkey-challenge-layout` + `upstream/feat/710-v1-subscription-data`,
ainda não mescladas ao `master` do próprio projeto), `create_subscription` teve sucesso
na `.210` e recebemos notificações reais com valores (`values={3: b'...', 2: b'...',
1: b'...'}`).

Isso provou que o bloqueio "Download error" **não é da PLC** - é um bug nosso. Comparando
byte a byte o payload que enviamos com o que o Python (comprovadamente funcional) envia
(usando um decodificador Python que espelha `DecodePValueToBytes`, aplicado aos dois
payloads *antes* do IntegrityId ser splicado, para comparação justa), achamos 3 bugs reais
em `s7plusconnection.pas`/`s7pluscodec.pas`, todos corrigidos e confirmados ao vivo:

1. **`TS7PlusConnection.SubscriptionCreate`**: o `InObjectId` (primeiros 4 bytes do
   payload) e o campo seguinte estavam trocados - deveria ser `[container_id]
   [RequestValue][0 fixo]`, mas estava `[session_id][RequestValue][container_id]`. Essa
   troca era a causa real do antigo "download: no parent object!" que motivou o comentário
   anterior (agora removido) culpando o `RelationId`.
2. **`EncodeValuePWString`** (`s7pluscodec.pas`): codificava WSTRING como UTF-16BE (2
   bytes por caractere), mas o comprimento declarado no wire é sempre em NÚMERO DE
   CARACTERES - então os dados reais enviados eram o dobro do que o comprimento dizia,
   desalinhando a leitura de TUDO que vinha depois desse campo. Confirmado contra o
   próprio protocolo (as strings que a PLC manda, como as de versão de firmware, já eram
   1 byte por caractere) e contra o Python funcional. Único uso dessa função no projeto
   era essa mesma linha, corrigido sem impacto em outro lugar.
3. **`EncodeValuePUInt`/`EncodeValuePInt`** (`s7pluscodec.pas`): codificavam `UINT`/`INT`
   como VLQ (1 byte para valores pequenos como 0), mas o formato real desses dois tipos é
   sempre 2 bytes fixos big-endian (confirmado em `DecodePValueToBytes`, que já lê assim).
   Usados só em `SubscriptionCreate`/`SubscriptionSetCreditLimit`, sem impacto em outro
   lugar.

Com os 3 corrigidos: `SubscriptionCreate: sucesso` e `SubscriptionDelete: sucesso`, tanto
com `RelationId` fixo (`0x7FFFC001`) quanto com `GetNewRIDOnServer` (mantido como padrão) -
confirmando que o `RelationId` nunca foi o problema.

**Resolvido também** (mesma sessão, logo em seguida): as notificações chegavam
(`Resultado=1`) mas com `Values=0` sempre - decodificando o frame bruto, os 3 itens vinham
com status `0x13` (tag de "erro por item") em vez de `0x92`/`0x9B` (tag de valor).
Reproduzido o MESMO `create_subscription` no Python de referência para essas MESMAS 3
variáveis (`BlocoSimbolico_4.Var1/2/3`, RID `0x8A0E0004`, LIDs `0xC/0xD/0xE`) - e ele
recebeu valores reais sem erro. Comparando o `SubscriptionReferenceList` byte a byte:
o Python manda **`SymbolCrc=0`** para cada item, não o CRC real da variável.

Isso contradiz uma suposição de uma investigação anterior desta mesma sessão (comentário
agora removido em `BuildSubscriptionReferenceList`) que dizia "ao contrário de leitura/
escrita simples, onde SymbolCrc=0 é aceito, a referência C# usa o CRC real aqui" - essa
suposição nunca tinha sido de fato validada contra uma subscription funcionando (só contra
o antigo bloqueio "Download error", que era causado pelos 3 bugs de encoding acima, não
pelo SymbolCrc). Corrigido: `BuildSubscriptionReferenceList` agora manda `SymbolCrc=0`
sempre, igual à leitura/escrita simples.

**Resultado final: Subscriptions funcionando de ponta a ponta na `.210`.**
`SubscriptionCreate: sucesso`, primeira notificação chega com `Values=3` (os 3 itens,
`RefId=1/2/3`, cada um com 4 bytes de dado `00 00 00 00` - valor real, batendo
exatamente com o que o Python recebeu para as mesmas variáveis, que também estão
zeradas nesse CLP de teste), notificações seguintes vêm vazias (sem mudança de valor,
comportamento esperado), e `SubscriptionDelete: sucesso`.

Resumo dos 4 bugs corrigidos nesta investigação (todos em `src/scada/`):
1. `SubscriptionCreate`: `InObjectId`/segundo campo trocados.
2. `EncodeValuePWString`: UTF-16BE em vez de 1 byte por caractere.
3. `EncodeValuePUInt`/`EncodeValuePInt`: VLQ em vez de 2 bytes fixos big-endian.
4. `BuildSubscriptionReferenceList`: `SymbolCrc` real em vez de 0.

**Ainda não testado**: o driver de alto nível (`TS7CommPlusDriver`/`DoScanRead` com tags
simbólicos usando subscriptions em vez de polling) - por enquanto só o nível baixo
(`SubscriptionCreate`/`SubscriptionWaitNotification`/`SubscriptionDelete` diretos) foi
validado.

## Atualização 2026-08-31 (mais tarde ainda) — handshake completo na `.211`: 3 de 4 passos funcionando

Testando o handshake completo (`SetupSession` → `SessionActivate` → `PostAuthLegitimation`)
contra a `.211` de verdade, usando o Python de referência (branches mescladas
`fix-v1-sessionkey-challenge-layout`+`feat/710-v1-subscription-data`, que **completa o
handshake inteiro com sucesso ao vivo**) como guia para portar os passos que faltavam:

- **`_session_activate`** portado (`TS7PlusConnection.SessionActivate`): `SET_VARIABLE`
  escrevendo `USINT(5)` no endereço 323 do objeto de sessão, usando um novo
  `EncodeObjectQualifierV1` (variante V1 do `ObjectQualifier`, com `KeyQualifier` de
  largura fixa em vez de VLQ+terminador). **Funciona** (`SessionActivate: OK`).
- **Framing V3 com HMAC-SHA256**: descoberto que, assim que `FHarpoSessionKey` é
  produzido, TODA mensagem subsequente (não só as de dados) passa a usar frame V3 com um
  HMAC-SHA256 de 32 bytes (chave = primeiros 24 bytes do SessionKey) prefixado antes do
  request - sem isso, a PLC aceitava o primeiro request (`SessionActivate`, sorte) mas
  derrubava a conexão no segundo. Implementado em `SendRequest` (envio) e no strip do
  prefixo `[hashLen][HMAC]` da resposta (caminho não-reassemble). **Funciona** (deixou de
  cair a conexão).
- **`TransportFlags` do `SetupSession`**: `0x34` fixo, não `0x36` (a captura TIA real
  usava `0x36`, mas o Python funcional usa `0x34` incondicionalmente aqui - a `.211` só
  aceitou depois dessa troca).
- **Atributo 299 removido do `SetupSession`**: o Python funcional nunca manda esse
  atributo (só `[1830,306]`, `ItemCount=2`) - a captura TIA mandava 3, mas o 299 aparenta
  ser opcional/ignorado.
- **`_post_auth_legitimation`** portado (`TS7PlusConnection.PostAuthLegitimation` +
  `S7PlusHarpoLegitimate.SolveLegitimateChallengeRealPlc`, novo unit): lê de novo o
  desafio de 20 bytes do endereço 303 via `GET_VAR_SUBSTREAMED`, resolve via HarpoS7's
  `LegitimateScheme` (SHA-1(senha)+desafio como "Key2", `DeriveLegitimationChallengeKey`
  como "Key1", reaproveitando toda a `TS7PlusHarpoAuthenticator` já portada), escreve o
  blob de 248 bytes resultante via `SET_VAR_SUBSTREAMED` no endereço 1846. A função de
  resolução (`SolveLegitimateChallengeRealPlc`) foi validada tanto contra o vetor de teste
  oficial do HarpoS7/python-snap7 (`test_session_auth_legitimate.py`) quanto, depois,
  contra uma comparação cross-validada com os dados REAIS de uma sessão .211 (mesmo
  desafio/chave de sessão/chave pública, RNG mockado igual dos dois lados): bate 100% com
  o Python nos dois casos.
  - No caminho, achado e corrigido um bug real no parsing da resposta do
    `GET_VAR_SUBSTREAMED`: o parsing ad-hoc do próprio `_post_auth_legitimation` do
    Python tem um bug (confirmado ao vivo: seu campo "length" decodifica para 79 em vez
    de 20, falha a checagem, e cai silenciosamente no fallback do desafio original de
    `CreateObject`) - usamos em vez disso o formato correto (o mesmo que a função
    "oficial" `_parse_get_var_substreamed_response` do próprio Python usa:
    `ReturnValue(VLQ)+IntegrityId(VLQ)+Flags(1)+DataType(1)+Length(VLQ)+dados`), que
    decodifica corretamente os 20 bytes reais - e confirmamos que o desafio relido é
    genuinamente diferente do desafio original do `CreateObject`.

**Ainda não resolvido**: mesmo com a estrutura do envelope do `SET_VAR_SUBSTREAMED`
batendo byte a byte com o Python (prefixo, sufixo, tamanho total - 292 bytes - tudo
idêntico) e o blob criptográfico validado como correto via a comparação cross-validada
acima, a PLC ainda rejeita com o mesmo `returnValue=0x8318890001E1FFFE`
(`errorcode=-2`, `genericerrorcode=9`) **independente do conteúdo real do blob/desafio**
testado em várias tentativas - o que sugere fortemente que a causa não é criptográfica.
Hipóteses não exploradas ainda: alguma mensagem/passo adicional entre `SessionActivate` e
`PostAuthLegitimation` que não aparece nos logs do Python (mas que talvez dependa de
timing/estado não capturado em texto), ou algo específico da versão de firmware
(V2.9.2) que diverge do que a referência Python realmente exercita com sucesso.

Resultado prático (antes da correção final abaixo): `SetupSession` e `SessionActivate`
funcionavam de verdade contra a `.211` real; `PostAuthLegitimation` seguia bloqueado.

## Atualização 2026-08-31 (última) — handshake SessionKey completo funcionando na `.211`

Bug real encontrado e corrigido: `TS7PlusConnection.SendRequest` nunca removia o
`IntegrityId` que aparece no **início** de toda resposta (não só nas requisições, onde
já era tratado) quando a sessão está no regime SessionKey/V3+HMAC - confirmado
comparando diretamente com os logs do Python (`Response IntegrityId: 17595385856 (5
bytes)` seguido de `Response payload (9 bytes): 87 c3 ff fe 05 00 00 00 00`, que
decodifica para um `ReturnValue` **positivo**, ou seja sucesso). Sem esse strip, líamos
`[IntegrityId+ReturnValue]` colados como se fossem só o `ReturnValue`, produzindo um
valor com aparência de erro (bit de sinal setado) **quase sempre**, independente do
conteúdo real - exatamente o sintoma observado (mesmo código de erro
`0x8318890001E1FFFE` repetido em toda tentativa, não importava o que mudasse no blob).

Corrigido em `SendRequest` (strip genérico, afeta toda leitura/escrita nesse regime, não
só a legitimação) e em `PostAuthLegitimation` (removida a tentativa de strip manual
duplicada que fazia ali, já redundante com a correção genérica).

**Resultado: os 3 passos do handshake (`SetupSession` → `SessionActivate` →
`PostAuthLegitimation`) funcionam de ponta a ponta contra a `.211` real** - pela
primeira vez nesta investigação inteira. `Connect` retorna com sucesso completo.

**Novo bloqueio, subsequente e diferente**: com a sessão totalmente estabelecida,
`BrowseDB`/`Explore` recebe da PLC um frame com `Version=0xFE` (`SystemEvent`) em vez de
uma resposta normal - o mesmo tipo de evento que a própria referência Python já esbarra
nesse ponto exato (`Fatal S7CommPlus SystemEvent`, documentado nas seções anteriores).
Não é um bug nosso: nem a referência sabe contornar isso ainda. Nosso código atualmente
não reconhece esse tipo de frame (trata como se fosse uma resposta normal, decodificando
lixo) - o próximo passo, se retomado, é: (a) fazer `LogicalRecv`/`SendRequest`
reconhecerem explicitamente `Version=0xFE` como SystemEvent (como o Python já faz,
mesmo sem conseguir contornar o bloqueio em si), e (b) decifrar o que esse SystemEvent
específico está reportando (guardado nos campos internos tipo `id+flags+datatype+valor`,
ainda não decodificado para este caso) - possivelmente ligado a alguma configuração de
diagnóstico/proteção específica dessa CPU (usuário se ofereceu para capturar com TIA
Portal V14/V15/V16 se for necessário, o que seria muito valioso aqui).

Arquivos de diagnóstico desta rodada (fora do pacote, em `/tmp/harpo_capture_build/`, não
versionados): `recon211.lpr` (dispara `Connect` sem TLS contra `.211` via `BrowseDB`, com
`OnDebug` acoplado a `WriteLn`) e capturas `tcpdump` correspondentes.

CLPs usados nos testes:
- `192.168.137.210` — firmware mais nova, TLS obrigatório, **já validado e funcionando**
  (sessão, browse, leitura, escrita, tudo OK). Subscriptions falham nele também, mas por
  um motivo diferente do CLP abaixo (ver "Resultado final" mais adiante).
- `192.168.137.211` — CPU S7-1513, firmware **V2.9.2** (`6ES7 513-1AL02-0AB0;V2.9`).
  Conectar nele com nosso driver expôs vários bugs reais (alguns já corrigidos, um ainda
  bloqueado). É o CLP usado pra capturar o tráfego real do TIA Portal V15.

## Bugs reais corrigidos nesta sessão

Todos em `src/scada/`, confirmados contra hardware real:

1. **`SymbolCrc` hardcoded em 0** (`s7plusconnection.pas`,
   `BuildSubscriptionReferenceList`) — a referência C# usa o CRC real da variável, não 0.
   Corrigido: `TS7PlusVarInfo` (em `s7plustypeinfo.pas`) ganhou o campo `SymbolCrc`,
   populado em `S7PlusWalk` a partir de `Node.Vte.SymbolCrc`; `TS7PlusMultiReadItem`
   ganhou o mesmo campo e `BuildSubscriptionReferenceList` passou a usá-lo.
   **Testado e confirmado correto, mas não era a causa do bloqueio.**

2. **Bug do "parent object" (`FSubscriptionsContainerObjectId`)** — `CreateSession`
   descarta o segundo `ObjectId` que o `CreateObject` de sessão retorna. A resposta
   sempre tem **dois** ids: `[0]`=a sessão, `[1]`=o container `ClassSubscriptions`
   criado junto (a própria requisição de `CreateSession` já cria um objeto filho desse
   tipo, embutido). Confirmado ao vivo: `ObjCount=2 sessionId=0x70000E41
   subscriptionsContainerObjectId=0x70000E42`. Esse segundo id é o "parent object" que
   faltava no envelope do `SubscriptionCreate` (campo antes fixo em 0) e é o que a
   referência C# chama (de forma confusa) de "SessionId2" — usado também por
   `SubscriptionDelete`, que antes usava `FSessionId` (errado).
   **Corrigido e testado, mas também não era a causa raiz do "Download error".**

3. **Subida de protocolo V1→V2 amarrada incorretamente a TLS ativo** (`Connect`,
   `s7plusconnection.pas`) — tanto nosso código quanto as duas referências (C# e Python)
   só sobem pra V2 (e habilitam `IntegrityId`) se TLS estiver ativo, abortando a conexão
   caso contrário ("V2 exige TLS"). **Provado errado** por uma captura real de tráfego do
   TIA Portal V15 conectando na `.211`: a captura é 100% texto claro (sem nenhum registro
   TLS), e mesmo assim o `SetupSession` já usa cabeçalho V2. A subida de versão está
   ligada a `CreateObject` ter tido sucesso, não a TLS. Corrigido: `FProtocolVersion :=
   S7PlusVersion_V2` agora é incondicional após `CreateSession`, e o abort
   "V2 exige TLS" foi removido (IntegrityId é só um contador de wire, não depende de
   criptografia). **Bug real, confirmado, sem relação com TLS em si.**

4. **Grupos EC do TLS reordenados** (`ActivateTLS`, `s7plusconnection.pas` +
   `S7PlusSSLCtxSetMaxProtoVersion` novo em `s7plusssl.pas`) — trocado de `'X25519'`
   sozinho para `'P-256:X25519:P-384'`, com P-256 primeiro (OpenSSL só gera a key_share
   proativa do TLS 1.3 pro primeiro grupo da lista). Motivação: o handshake TLS contra a
   `.211` é rejeitado com RST logo após o ClientHello. **Esse reordenamento sozinho não
   resolveu o RST da `.211`** (testado à exaustão, ver abaixo) — mas foi mantido porque é
   estritamente mais compatível (não quebrou nada na `.210`, testado) e não custa nada.

## Descobertas confirmadas via captura real (TIA Portal V15 → CLP .211, texto claro)

Setup: desmarcar "Only allow secure PG/PC and HMI communication" nas propriedades do CPU
no TIA **não desliga o TLS de verdade** quando o TIA Portal é recente (V17+) — só quando
se usa um **TIA Portal antigo** (V15, que antecede a exigência de TLS) é que a conexão
sai em texto claro de verdade. Confirmado byte a byte (nenhum registro `17 03 03 ...`
aparece na captura).

Com a captura em mãos (arquivo local do usuário, não versionado):

- **`RouteMode = 0x01`** (não `0x14`, que era só um chute nosso baseado na tabela do
  código-fonte C#, que nunca listava `0x01` como testado).
- **Esquema de `RelationId`**: confirmado incrementando a partir de `0x7FFFC001` por
  sessão (bate com o "guess" já documentado na referência C#), mas isolado esse valor
  correto **não** muda o resultado — já estava sendo testado e falhava igual.
- **`SetupSession` (SetMultiVariables) real do TIA escreve 3 atributos**, não 1:
  - `AttrId 306` = `ServerSessionVersion` (o mesmo que já mandamos, ecoado de volta).
  - `AttrId 299` (perto de `ServerSessionClientRID`=300, não identificado com certeza).
  - `AttrId 1830` (fora da faixa normal ~300, família `0x8Exx`) — o valor é um **struct
    aninhado** contendo, entre outras coisas, um campo de 20 bytes e dois blocos de
    ~140 bytes com cara de material criptográfico (a resposta do próprio `CreateObject`
    já manda um array de 20 bytes solto, atributo `0x822F`, que pode ser um nonce/desafio
    relacionado). **Não decodificado** — teria toda a cara de um handshake de
    autenticação mútua/chave de sessão (RSA ou ECDH), mas decompor isso exigiria mais de
    uma captura pra correlacionar o que é fixo vs. o que varia por sessão, e não há
    nenhuma referência open-source que implemente isso (ver abaixo).

## Referências verificadas (a pedido explícito) — nenhuma cobre o handshake real

- **C# (`thomas-v2/S7CommPlusDriver`)**: `Connect()` usa TLS **sempre**, incondicionalmente
  — não existe nem um caminho sem TLS pra comparar. `SetMultiVariablesRequest.
  SetSessionSetupData` é tão simples quanto o nosso: um único atributo
  (`ServerSessionVersion` ecoado), sem nada de RSA/chave/certificado.
- **Python (`python-snap7`, `s7commplus/connection.py`)**: tem um comentário explícito
  sobre um "SessionKey handshake" proprietário, mas diz que só é necessário quando
  `ServerSessionVersion` vem **ausente** (firmware bem antigo, S7-1200 < FW4.5,
  pré-TLS, que manda um `Struct(314)` em vez de escalar) — e mesmo assim está marcado
  "not implemented" lá (issue #710 do próprio projeto). No nosso caso `ServerSessionVersion`
  vem **presente** (85 bytes, escalar) — pela lógica documentada do Python isso não
  deveria precisar do handshake, mas empiricamente a `.211` exige mesmo assim.

## Resultado final / onde isso deixa cada CLP

- **`.210`**: segue 100% funcional (sessão, browse, leitura, escrita, tag polling —
  tudo revalidado depois de todos os fixes). Subscriptions falham nele com o mesmo
  "Download error (IDs & states [main, sub, TI, next])!" original, mas como esse CLP
  usa firmware mais nova e TLS "de verdade" (nunca foi possível pegar um TIA Portal
  falando texto claro com ele), não foi possível confirmar se o motivo é o mesmo handshake
  de chave de sessão da `.211` ou outra coisa — não há captura decifrável pra comparar.
- **`.211`**: `CreateSession` funciona (com ou sem TLS). `SetupSession` (nosso, de 1
  atributo) é **rejeitado com RST**, com ou sem TLS, com ou sem a correção V1→V2 — porque
  esse firmware especificamente exige o handshake de 3 atributos com material
  criptográfico que não conseguimos decodificar. Handshake TLS da `.211` também é
  rejeitado com RST logo após o ClientHello, por um motivo ainda não identificado
  (testado: grupos EC ampliados, TLS1.2-only forçado — nenhum dos dois resolveu; não
  investigado mais a fundo porque o bloqueio do `SetupSession` sem TLS já é suficiente
  pra travar qualquer teste nesse CLP).

## Arquivos de teste adicionados

Em `examples/laz_s7commplus_phase2_test/` (não fazem parte do pacote, só ferramentas de
diagnóstico manual contra CLP real):

- `s7commplus_subscription_test.lpr` — já existia; testa
  `SubscriptionCreate`/`SubscriptionWaitNotification`/`SubscriptionDelete` direto (baixo
  nível) contra `DB4.Var1/2/3`. Agora já passa `SymbolCrc` real dos itens.
- `s7commplus_subscription_sweep_test.lpr` — novo. Varre combinações de
  `RouteMode`/`RelationId`/atributo 1055/`CreditLimit`, cada uma numa sessão nova
  (aprendemos que reusar a sessão entre tentativas com `SubscriptionDelete` no meio
  destrói o container `ClassSubscriptions` e contamina as tentativas seguintes com um
  erro diferente, "no parent object"). Todas as 8 combinações testadas retornam o mesmo
  `returnValue` bit a bit idêntico — forte indício de que o bloqueio é estrutural
  (handshake de sessão), não um valor de atributo errado.

## Se for retomar

Próximos passos mais promissores, em ordem de custo/benefício:

1. **Conseguir uma segunda captura em texto claro da `.211`** (outro TIA Portal antigo,
   ou o mesmo repetido) pra comparar o bloco de ~140 bytes entre duas sessões diferentes
   — o que muda é candidato a nonce/chave de sessão; o que se repete é candidato a
   parâmetro fixo (ex. um certificado ou chave pública do CLP).
2. Investigar se existe uma forma de extrair/consultar a chave pública do CLP via TIA
   Portal (gerenciamento de certificados) pra testar hipóteses de RSA-OAEP contra os
   blobs capturados.
3. Separadamente, entender por que o handshake TLS da `.211` é rejeitado com RST (talvez
   precise de um certificado de cliente específico, já que esse CLP tem soberania de
   certificado configurada — visto no `PLC communication certificate:
   GP2E-RecebiGraos/Communication-1` da tela de propriedades do TIA).

## Atualização 2026-08-31 — `.210` funcionando 100%, `.211` decifrado até a parede final

### `.210` (TLS, S7-1500): **Subscriptions funcionam de ponta a ponta**

Confirmado ao vivo, valores reais batendo com a referência Python: `SubscriptionCreate`
(`DB4.Var1/2/3`) → `SubscriptionWaitNotification` recebendo notificações reais em ciclo.
Bugs corrigidos ao longo do caminho (todos em `s7pluscodec.pas`/`s7plusconnection.pas`):

- `EncodeValuePWString` usava UTF-16BE (2 bytes/char); o protocolo espera 1 byte/char.
  Descompassava todo campo seguinte de qualquer `SubscriptionCreate`.
- `EncodeValuePUInt`/`EncodeValuePInt` usavam VLQ; o protocolo espera 2 bytes fixos
  big-endian (igual ao que `DecodePValueToBytes` já fazia na leitura - só a escrita estava
  errada).
- `SubscriptionCreate`: campos `InObjectId`/segundo campo estavam trocados
  (`[session][valor][container]`, devia ser `[container][valor][0]`).
- `BuildSubscriptionReferenceList`: `SymbolCrc` deve ser sempre `0` no wire (a suposição
  anterior de que precisava do CRC real da variável estava errada - confirmado contra
  referência Python funcionando).

Este caminho (`BrowseDB`→`SubscriptionCreate` classe 1001) é o que deve ser usado em
produção sempre que possível — é o mecanismo documentado/genérico do protocolo, funciona
pra qualquer tipo de dado, e é o que HMIs de terceiros também usam.

### `.211` (V1-initial/SecurityKey, sem TLS, S7-1513 FW V2.9.2): sessão completa, dado da DB bloqueado

Progresso real, mas a parede final não caiu. Sequência que **funciona** (implementada em
`s7plusconnection.pas`, `Connect`/`SessionActivate`/`PostLegitimationSessionInfo`/
`CreateChangeCounterObject`/`CreateEmptySystemSubscription`):

1. `CreateSession`+`SetupSession` com o handshake HarpoS7 completo (retorna `returnValue=0`).
2. `SessionActivate` (SetVariable no endereço 323).
3. 5 leituras `GetVarSubstreamed` de atributos de sessão (endereços 300/3753/1842/300/1842),
   `ObjectQualifier` **V1 de largura fixa** (`KeyQualifier` fixo em `1`, não o formato V2
   com terminador VLQ) - **confirmado** que esse é o formato certo pro firmware V1, mas
   **não é o que bloqueia** o resto (ver abaixo).
4. `CreateObject` do objeto de sistema `SPL_PlcProgram_ChangeCounter` (classe 1001, mesma
   classe de Subscription, referência vazia específica) - **sucesso confirmado**.
2 leituras mais.
5. `CreateObject` de uma Subscription vazia (`Subscription_0`, `ReferenceList=[]`) -
   **sucesso confirmado**.

**Descoberta crítica:** `PostAuthLegitimation` (o par read(303)/write(1846) que o
`python-snap7` faz) **não deve ser chamado** - mesmo aceito pelo CLP com `returnValue`
positivo, ele deixa a sessão num estado que faz até leituras que funcionavam antes (como
o read de endereço 300) passarem a falhar com o mesmo SystemEvent fatal descrito abaixo.
TIA Portal nunca faz essa troca nesse firmware (confirmado em 3 capturas reais
diferentes) - a legitimação real deve acontecer em outro canal (certificado de engenharia
armazenado no projeto TIA), que não temos.

### A parede: qualquer acesso a dado real de DB é rejeitado com um SystemEvent fatal constante

**Toda e qualquer tentativa de ler/assinar um valor de `DB4`** (endereço bruto via
`DBRead`, LID simbólico via `SubscriptionCreate`/`ReadSymbolic`, `Explore` da DB, e até o
mecanismo interno do próprio TIA descrito abaixo) falha com o **mesmo** SystemEvent
fatal, byte a byte idêntico, member id `0x9D71` (`S7PlusSystemEventReturnValueId`) valor
`0xA2013B00029BFF88` — **sempre o mesmo valor**, independente de:

- Endereçamento (LID simbólico vs offset bruto).
- Formato do `ObjectQualifier` (V1 fixo vs V2 VLQ).
- Ter ou não passado por `PostAuthLegitimation`.
- Ter ou não criado o `ChangeCounter`/`Subscription` vazia antes.
- Sessão nova/independente (testado com 2 conexões TCP simultâneas, sessões
  `0x70000CAA` e `0x70000CAC` - mesmo erro nas duas).
- Habilitar "Permit access with PUT/GET communication from remote partner" nas
  propriedades do CLP (testado ao vivo - sem efeito).
- CLP configurado como "Full access (no protection)" - não é bloqueio de senha.

Enquanto isso, **toda** operação que não toca dado de programa (SessionActivate, os 5
reads de atributo de sessão, `CreateObject` de objetos de sistema) funciona
perfeitamente, de forma 100% repetível.

### O mecanismo real do TIA Portal: `TisMonitorJob`, não `Subscription`

Capturamos 3 vezes (Wireshark, TIA V15, ação específica "Monitor all" na tabela de
declaração da DB4, valores realmente aparecendo e variando ao vivo no TIA) o que o TIA
realmente faz pra mostrar valores ao vivo de uma DB nesse firmware. **Não é o mecanismo de
`Subscription` (classe 1001) que usamos** - é um objeto de sistema completamente diferente:

1. `GetLink(InObjectId=11, Attr=2699)` → resolve pro RID de um objeto de sistema fixo
   (ex. `0x70000cb8`).
2. `GetMultiVariables` nesse RID, campos `[9500, 9501, 9506, 9505, 9507]` → o campo 9505
   retorna um blob de ~100 bytes cujos bytes `[4:17]` (13 bytes) são a **"fingerprint"**
   viva da DB (confirmado idêntico entre essa leitura e o mesmo trecho embutido em 2
   `CreateObject`s de `TisMonitorJob` reais diferentes).
3. `CreateObject` de um objeto `"TisMonitorJob_N"` (nome livre, atributo 233) com um
   atributo BLOB (id 2693) contendo: `40 <0x28+4*N> 32 01` + fingerprint(13 bytes) +
   `80 10` + N registros de 12 bytes, um por variável monitorada:
   `[LID+0x58][04 03 F0 00 05 D0 12][countdown][80][offset]`, onde `offset` começa em 20
   e incrementa 4 por item, e `countdown = (20+4*N) - offset`. **Verificado
   matematicamente** contra 2 capturas reais com contagens de item diferentes (21 e 23
   itens) - fórmula bate exatamente nas duas.
4. Entrega de valor: ainda não confirmada - a leitura grande (~936 bytes, GetVarSubstreamed
   num RID separado) que suspeitávamos ser os valores parece ser outra coisa (dado de alta
   entropia, não bate com array de valores simples); o padrão mais provável é que os
   valores cheguem por frames de Notification assíncronos (o mesmo canal que
   `SubscriptionWaitNotification` já sabe ler no `.210`), entregues em resposta aos
   `SetMultiVariables` repetidos (~a cada 100ms) que mantêm o "crédito" da assinatura viva
   - não confirmado, não implementado.

Isso tudo está portado em `s7plusconnection.pas`: `GetLinkRaw`, `GetMultiVariablesFields`,
`GetDB4Fingerprint`, `CreateTisMonitorJob`. **Testado ao vivo: o próprio passo 1
(`GetLink(11, 2699)`) já falha com o mesmo SystemEvent fatal constante**, tanto na sessão
original quanto numa segunda sessão TCP independente recém-criada. Ou seja, mesmo
reproduzindo byte a byte o que o TIA manda nessa chamada, o CLP recusa - pra nós, mas não
pra ele.

### Conclusão

O bloqueio não parece mais ser de payload/protocolo aplicativo (já testamos todas as
variações plausíveis de endereçamento, handshake e sequência de chamadas, todas com o
mesmo erro constante). O padrão (aceita tudo que não é dado de programa, rejeita 100% do
que é, com um código fixo, independente de sessão) sugere fortemente uma credencial ou
certificado verificado numa camada abaixo do S7+ (TCP/COTP/TLS), que o TIA Portal
apresenta e um cliente terceiro não tem como replicar só analisando captura de pacotes -
não seria visível como conteúdo de protocolo decodificável.

**Não investigado ainda, possíveis próximos passos caso se queira retomar:**

- Comparar a captura completa do handshake COTP (CR/CC) e, se possível, um handshake TLS
  bem-sucedido de outra ferramenta Siemens contra esse CLP, procurando por qualquer
  extensão/certificado que só apareça quando a origem é reconhecida como TIA Portal.
- Perguntar/verificar se o CLP tem uma "PLC communication certificate" vinculada
  especificamente ao projeto TIA (visto em investigação anterior, `.211`/CLP com
  "soberania de certificado" configurada) - se for isso, pode ser uma limitação de
  hardware/licenciamento sem contorno via protocolo.
- Terminar de decifrar a entrega de valores do `TisMonitorJob` (passo 4 acima) mesmo sem
  poder testá-la ao vivo, caso surja uma forma de contornar o bloqueio do `GetLink`.

**Recomendação prática:** tratar `.210` (TLS) como o caminho suportado e validado pra
Subscriptions. `.211` (V1-initial/SecurityKey) tem toda a infraestrutura de sessão pronta
e funcionando (útil pra outros firmwares parecidos que não tenham essa restrição extra de
dado), mas leitura/assinatura de dado de programa continua bloqueada nesse CLP específico
por um motivo que parece estar fora do alcance do protocolo aplicativo S7+.
