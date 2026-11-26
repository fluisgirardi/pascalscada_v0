# Testes ao vivo do S7CommPlus / Live S7CommPlus tests

Testes que falam com um PLC (ou PLCSim) de verdade, ao contrário da suíte comum (`tests/pascalscada_tests`),
que não usa rede. Ficam num projeto à parte para o CI não depender de PLC.

Tests that talk to a real PLC (or PLCSim), unlike the common suite (`tests/pascalscada_tests`), which uses no
network. They are a project of their own so that CI does not depend on a PLC.

## Preparar o PLC / Preparing the PLC

Importe e baixe os fontes de [`../plc`](../plc/README.md) (DBs `TestDB_Opt`, `TestDB_NonOpt`, `TestDB_Big`,
`TestDB_Dyn` e as FCs `FC_TestDynamics` e `FC_TestPattern` chamadas no OB1) e deixe o PLC em RUN.

Import and download the sources of [`../plc`](../plc/README.md) (DBs `TestDB_Opt`, `TestDB_NonOpt`, `TestDB_Big`,
`TestDB_Dyn` and the FCs `FC_TestDynamics` and `FC_TestPattern` called in OB1) and keep the PLC in RUN.

## Rodar / Running

```sh
lazbuild --build-all --widgetset=nogui tests/live/pascalscada_live_tests.lpi

S7PLUS_HOST=192.168.56.2 S7PLUS_USER=User S7PLUS_PASSWORD=... \
  tests/bin/pascalscada_live_tests --all --format=plain
```

| Variável / Variable | |
|---|---|
| `S7PLUS_HOST` | IP do PLC. Sem ele todos os testes são ignorados / PLC address; without it every test is ignored |
| `S7PLUS_PORT` | porta / port (102) |
| `S7PLUS_USER`, `S7PLUS_PASSWORD` | legitimação, se o PLC pedir / login, if the PLC asks for one (nunca no repositório / never in the repository) |
| `S7PLUS_DEBUG` | `1`: mostra as mensagens do driver e o início de cada teste / shows the driver messages and the start of each test |

`--suite=TTestLiveTags` roda só um grupo; `--suite=TTestLiveTags.TheSameOnChange` só um teste.

## O que cobre / What it covers

- **Conexão:** login, `Auth`, `ListTags` (tamanho das STRINGs lido do PLC), nenhuma "mudança de programa" falsa.
- **Leituras:** todos os valores iniciais dos dois DBs (otimizado e não), o padrão do DB grande, valores que o PLC muda, 20 STRINGs numa só chamada.
- **Escritas:** escalares de todos os tamanhos (inclusive Bool), STRINGs, membros de estrutura e de array de estruturas, elementos de array, o eco e o `ResetCmd` do PLC, `Enable`.
- **Tags:** o DB inteiro como estrutura montada pelo plano do Tag Builder (por varredura e por mudança), o DB grande em várias requisições, tag numérico e de texto, tag por mudança seguindo o contador, escrita por um item da estrutura, escrita de um elemento de array, e os `TTagBit` de um DB mapeado pela cesta do Tag Builder contra os mesmos Bool lidos pelo caminho.
- **Hashes de bloco:** `TTestLiveBlockHash` lê os atributos de engenharia dos primeiros 8 DBs do CLP, quaisquer que sejam (`ReadObjectAttributes`), calcula o IfrHash (7949) e o AlarmIFRHash (7951) com `S7PlusBlockHash` e compara com os que o CLP informa. Se não bater, a mensagem traz o fluxo de bytes e os atributos lidos (tipo, tamanho): é o ponto de partida para ajustar as regras.
- **Reconexão:** a porta é fechada e reaberta sob uma subscrição.
- **No fim (`ZZ`):** `LoadPattern` recarrega os valores por elemento.

Os testes que escrevem devolvem o valor que encontraram. Se um deles for interrompido no meio, escreva `TRUE` em
`"TestDB_Dyn".LoadPattern` e confira os escalares pelo `EveryStartValueOfTheOptimizedDB`.

The tests that write put back the value they found. If one is interrupted halfway, write `TRUE` to
`"TestDB_Dyn".LoadPattern` and check the scalars with `EveryStartValueOfTheOptimizedDB`.

## OPC UA (`lt.opcua.pas`)

Os testes do driver OPC UA rodam também na suíte comum (`tests/pascalscada_tests`, suítes `TTestOPCUAValues`,
`TTestOPCUADriver`, `TTestOPCUASecurity`) contra um servidor open62541 no próprio processo; são ignorados se a
libopen62541 1.5 não estiver instalada (`OPEN62541_LIB` aponta o caminho). Os testes ao vivo falam com um servidor
de verdade (PLCSim, S7-1500, outro).

The OPC UA driver tests also run in the common suite (`tests/pascalscada_tests`, suites `TTestOPCUAValues`,
`TTestOPCUADriver`, `TTestOPCUASecurity`) against an in-process open62541 server; they are ignored when
libopen62541 1.5 is not installed (`OPEN62541_LIB` points to it). The live tests talk to a real server
(PLCSim, S7-1500, other).

```sh
OPCUA_URL=opc.tcp://192.168.56.2:4840 OPCUA_SECURITY=encrypt OPCUA_USER=... OPCUA_PASSWORD=... \
OPCUA_WRITE_NODE='ns=3;s="TestDB_Opt"."DInt_1"' OPCUA_CHANGING_NODE='ns=3;s="TestDB_Dyn"."Counter"' \
  tests/bin/pascalscada_live_tests --suite=TTestLiveOPCUA --format=plain
```

| Variável / Variable | |
|---|---|
| `OPCUA_URL` | endpoint; sem ele os testes são ignorados / without it the tests are ignored |
| `OPCUA_USER`, `OPCUA_PASSWORD` | login (nunca no repositório / never in the repository) |
| `OPCUA_SECURITY` | `none` (padrão/default), `sign`, `encrypt` |
| `OPCUA_TRUSTDIR` | certificados de servidor confiáveis; vazio aceita qualquer / trusted server certificates; empty accepts any |
| `OPCUA_WRITE_NODE` | nó numérico gravável (escreve +1 e devolve) / writable numeric node (writes +1 and restores it) |
| `OPCUA_CHANGING_NODE` | nó que muda sozinho / node that changes by itself |
| `OPCUA_DEBUG` | `1`: mensagens do driver / driver messages |

Com segurança, o servidor precisa confiar no certificado do cliente: o driver gera um em
`CertificateDir/own/pascalscada-client.der` na primeira conexão (no S7-1500/PLCSim: aceitar o certificado
rejeitado no TIA Portal / no servidor e baixar de novo). With security, the server has to trust the client
certificate: the driver generates one in `CertificateDir/own/pascalscada-client.der` on the first connection.
