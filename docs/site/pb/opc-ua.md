##### Introdução

![](img/TOPCUADriver.png) O `TOPCUADriver` é um **cliente OPC UA**. Ele lê e escreve as variáveis de qualquer servidor OPC UA: o servidor do S7-1200/1500 e do PLCSim, o servidor embutido em CLPs de outras marcas, um gateway OPC UA, um SCADA que publica os seus tags ou um servidor feito com a open62541.

O protocolo em si fica por conta da **open62541 1.5** (a biblioteca em C), através do binding Pascal [fpopen62541](https://github.com/fluisgirardi/fpopen62541), que vem junto com o PascalSCADA em `src/external/fpopen62541`. A biblioteca é carregada em tempo de execução, só na primeira conexão: um programa que não usa o driver não precisa dela.

| | |
|---|---|
| Servidores | qualquer servidor OPC UA acessível por `opc.tcp://` |
| Endereçamento | o NodeId da variável: `ns=3;s="DB1"."Speed"`, `nsu=urn:meu:servidor;i=1001` |
| Valores | por subscrição (o servidor avisa as mudanças) ou por leitura periódica em lotes |
| Segurança | None, Sign, SignAndEncrypt; anônimo ou usuário/senha |
| Tag Builder | navega pelo servidor e cria os tags |

> **Um driver fala com um servidor.** Para ler dois servidores, use dois `TOPCUADriver`.

##### Componentes necessários

1. Um **`TOPCUADriver`** (unit `OPCUADriver`) com `EndpointUrl` = endereço do servidor, por exemplo `opc.tcp://192.168.0.10:4840`. A 4840 é a porta padrão do OPC UA. Não há componente de porta de comunicação: a própria biblioteca abre a conexão.
2. Os tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`) com `ProtocolDriver` apontando para o driver e `LongAddress` com o NodeId da variável (veja *Endereçamento*).
3. A biblioteca **open62541 1.5** na máquina que roda o programa (veja a próxima seção).

O jeito mais rápido de chegar ao passo 2 é o **Tag Builder** (veja abaixo), que navega pelo servidor e cria os tags.

##### A biblioteca open62541

O driver precisa da open62541 **1.5** compilada como biblioteca compartilhada e com criptografia (`UA_ENABLE_ENCRYPTION=OPENSSL`); uma biblioteca de outra versão, ou sem criptografia, é recusada ao carregar, com uma mensagem dizendo o motivo.

* **Linux:** compile a partir dos fontes e instale no sistema:

```
git clone --branch v1.5.10 --recurse-submodules https://github.com/open62541/open62541.git
cmake -S open62541 -B open62541/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON \
      -DUA_ENABLE_ENCRYPTION=OPENSSL
cmake --build open62541/build -j
sudo cmake --install open62541/build && sudo ldconfig
```

* **Windows:** a `libopen62541.dll`, com a mesma quantidade de bits do programa, na pasta do executável (e na do `lazarus.exe` para usar o Tag Builder na IDE). O workflow `BuildOpen62541v1.5.yml` do fpopen62541 a compila para Win32 e x64 com o OpenSSL dentro dela, então nenhuma DLL do OpenSSL é necessária.

Com `LibraryName` vazio, o driver procura a biblioteca nesta ordem: o nome do sistema (`libopen62541.so` / `libopen62541.dll`), `libopen62541.so.1.5` (Linux) e os mesmos nomes na pasta do executável — assim a biblioteca pode ir junto com a aplicação. `LibraryName` com um caminho ou um nome pula a busca. Quando nada é encontrado, `LastError` lista todos os lugares tentados.

##### Preparando o servidor

Num **S7-1200/1500** (ou PLCSim Advanced), no TIA Portal:

1. Propriedades da CPU → *OPC UA* → *Server*: marque *Activate OPC UA server*. O endpoint mostrado ali é o `EndpointUrl` (porta 4840 por padrão). A CPU também precisa de uma licença de runtime OPC UA selecionada em *Runtime licenses*.
2. Só as variáveis marcadas como *Accessible from HMI/OPC UA* (e *Writable from HMI/OPC UA* para escrever) são publicadas.
3. *Server* → *Security*: as políticas de segurança que o servidor aceita e se o acesso de convidado (anônimo) está habilitado. Sem *No security* na lista, o driver precisa usar Sign ou SignAndEncrypt, e o servidor precisa confiar no certificado do cliente (veja *Segurança*).

Os NodeIds das variáveis de uma CPU Siemens têm a forma `ns=3;s="DB1"."Speed"` — com as aspas.

##### Propriedades do driver

| Propriedade | Padrão | Descrição |
|---|---|---|
| `EndpointUrl` | – | Endereço do servidor, `opc.tcp://host:porta`. |
| `Username` / `Password` | vazio | `Username` vazio: sessão anônima. |
| `SecurityMode` | `osmNone` | `osmNone`, `osmSign` (assinado) ou `osmSignAndEncrypt` (assinado e criptografado). |
| `SecurityPolicy` | `ospAuto` | `ospAuto` pega a melhor que o servidor oferece para o modo; ou uma fixa: `ospBasic256Sha256`, `ospAes128Sha256RsaOaep`, `ospAes256Sha256RsaPss`, `ospBasic256`, `ospBasic128Rsa15`. |
| `CertificateFile` / `PrivateKeyFile` | vazio | Certificado e chave do cliente (DER ou PEM). Vazio: um é gerado na primeira conexão segura e guardado em `CertificateDir`. |
| `CertificateDir` | pasta de configuração da aplicação + `opcua-pki` | Onde o certificado gerado fica: `own/pascalscada-client.der` e `private/pascalscada-client.key`. |
| `TrustListDir` | vazio | Pasta com os certificados (`.der`, `.pem`, `.crt`, `.cer`) dos servidores confiáveis. **Vazia aceita qualquer servidor.** |
| `ApplicationUri` / `ApplicationName` | `urn:pascalscada:opcua:client` / `PascalSCADA OPC UA Client` | Como o cliente se apresenta. A URI vai no certificado gerado. |
| `AllowUnencryptedPassword` | `False` | Deixa a senha passar num canal sem criptografia (`osmNone`). Mantenha desligado fora de testes. |
| `Timeout` | 5000 | Tempo limite de uma requisição, em ms. |
| `SessionTimeout` | 60000 | Tempo de vida da sessão pedido ao servidor, em ms. |
| `ReconnectInterval` | 3000 | Espera entre duas tentativas de conexão, em ms. |
| `UseSubscriptions` | `True` | Valores por subscrição; `False` lê os tags periodicamente (veja *Subscrição ou leitura periódica*). |
| `PublishingInterval` | 100 | De quanto em quanto tempo, em ms, o servidor manda as mudanças da subscrição. |
| `MaxItemsPerRequest` | 500 | Máximo de variáveis numa requisição de leitura, escrita ou de itens monitorados; mais que isso é dividido em várias requisições. |
| `LibraryName` | vazio | Nome ou caminho da biblioteca; vazio faz a busca (veja acima). |
| `LogLevel` / `OnLog` | `ollWarning` | Mensagens do driver e da biblioteca. O `OnLog` é chamado pelas threads do driver: não mexa no form dentro dele. |
| `ReadOnly` | `False` | Nenhuma escrita chega ao servidor. |

Métodos: `Connected`, `LastError`, `ListTags`, `Browse`, `GetVariableInfo`, `GetServerCertificates`, `GetClientCertificate` (veja *Navegando por código*).

##### Endereçamento

O `LongAddress` é o **NodeId** da variável, em texto:

```
ns=3;s="DB1"."Speed"                   // índice do namespace + identificador texto (Siemens)
ns=2;i=1001                            // identificador numérico
ns=2;g=09087e75-8e5e-499b-954f-f2a9603db28a   // GUID
nsu=http://www.siemens.com/simatic-s7-opcua;s="DB1"."Speed"   // URI do namespace
```

O `ns=` usa o índice do namespace no servidor; o `nsu=` usa a URI dele, que não muda se o servidor reordenar os namespaces — o Tag Builder escreve `nsu=` por padrão. O namespace 0 é o padrão do OPC UA: `i=2258` é a hora atual do servidor.

Um `|` seguido de um **IndexRange** escolhe elementos de um array: `ns=3;s="DB1"."Arr"|4` é o elemento 4, `ns=3;s="DB1"."Arr"|0:9` os dez primeiros. Os índices começam em 0, quaisquer que sejam os limites do array no programa do CLP.

##### Tags e tipos

O servidor informa o tipo de cada variável, então o próprio driver converte o valor: deixe `TagType = pttDefault`, sem swaps nem truques de escala.

| Tipo OPC UA | Tag | Valor |
|---|---|---|
| Boolean | `TPLCTagNumber` | 0 / 1; escrever qualquer valor diferente de 0 escreve `true` |
| SByte, Byte, Int16, UInt16, Int32, UInt32, Int64, UInt64 | `TPLCTagNumber` | o número. Inteiros de 64 bits acima de 2^53 perdem precisão (o valor do tag é um Double). |
| Float, Double | `TPLCTagNumber` | o número |
| DateTime | `TPLCTagNumber` | um `TDateTime` na hora local (formate com `FormatDateTime`) |
| Enumeração, StatusCode | `TPLCTagNumber` | o valor numérico |
| array de qualquer um acima | `TPLCBlock` (`Size` = número de elementos) | um valor por elemento |
| String, LocalizedText, QualifiedName, ByteString, Guid, NodeId | `TPLCString` (`StringType = stC`) | o texto em UTF-8 |

Uma escrita que não cabe no tipo da variável (300 num Byte, um texto que não é número numa variável numérica) é recusada com `ioIllegalValue`, sem chegar ao servidor. Escrever um elemento de um `TPLCBlock` escreve só aquele elemento (por IndexRange); se o servidor não aceita escrita por IndexRange, o driver lê o array, troca o elemento e escreve o array inteiro.

Estruturas (tipos de dados do usuário) não são decodificadas: enderece os membros um a um.

##### Subscrição ou leitura periódica

Com `UseSubscriptions = True` (o padrão) o driver cria uma subscrição e um item monitorado por variável: o servidor amostra a variável a cada `RefreshTime` do tag e só manda o que mudou, no máximo a cada `PublishingInterval`. Um tag com `RefreshTime = 0` pede ao servidor a amostragem mais rápida que ele tiver. Vários tags no mesmo NodeId dividem um item monitorado, com o `RefreshTime` mais rápido.

Com `UseSubscriptions = False` as variáveis são lidas periodicamente, cada uma no seu `RefreshTime`, em lotes de até `MaxItemsPerRequest`. Use com servidores que não aceitam subscrição, ou para ter cada valor lido numa taxa fixa mesmo quando não muda.

Quando a conexão cai, todos os tags recebem o erro; o driver tenta de novo a cada `ReconnectInterval` e, ao reconectar, recria a subscrição sozinho.

##### Segurança

* **None** (`osmNone`): nada é assinado nem criptografado. Uma senha só é mandada assim com `AllowUnencryptedPassword = True`.
* **Sign** / **SignAndEncrypt**: as mensagens são assinadas (e criptografadas). Os dois lados apresentam certificados e cada um precisa confiar no outro:
    * **O servidor confia no cliente.** Com `CertificateFile` vazio o driver gera um certificado autoassinado na primeira conexão segura e o guarda em `CertificateDir`. Entregue o `own/pascalscada-client.der` ao servidor como cliente confiável — no TIA Portal ele vai para os clientes confiáveis do servidor OPC UA (*Server* → *Security* → *Secure channel*), a menos que o servidor esteja configurado para aceitar certificados de cliente automaticamente; outros servidores costumam guardar um certificado recusado numa pasta *rejected*, de onde ele é movido para *trusted*. O Tag Builder tem o botão **Export client certificate** para isso. O certificado é guardado, então basta confiar nele uma vez.
    * **O cliente confia no servidor.** Com `TrustListDir` vazio, qualquer servidor é aceito. Para aceitar só servidores conhecidos, coloque os certificados deles nessa pasta — o botão **Trust server certificate** do Tag Builder salva lá o do servidor atual.
* **Usuário e senha**: `Username` e `Password`. Uma senha errada termina em `ioAuthError`.

##### Tag Builder

Clique com o botão direito no `TOPCUADriver` → **Tag Builder** abre o *OPC UA Tag Builder*.

1. Aperte **Browse**: a árvore mostra a pasta Objects do servidor; pastas e objetos são expandidos sob demanda.
2. Selecione as variáveis (Ctrl/Shift para várias). Ajuste **Scan** (o `RefreshTime` dos tags novos) e, se quiser, um **Prefix** para os nomes. *Address with the namespace URI (nsu=)* escolhe entre endereços `nsu=` (padrão) e `ns=`.
3. **OK** cria um tag por variável: `TPLCTagNumber` para um escalar numérico, `TPLCBlock` (com o `Size` já ajustado) para um array numérico e `TPLCString` para um texto. Estruturas e outros tipos sem equivalente são pulados e contados numa mensagem.

Os botões **Trust server certificate** (salva o certificado do servidor em `TrustListDir`) e **Export client certificate** ficam na mesma janela (veja *Segurança*).

##### Navegando por código

```pascal
var
  tags: TPLCTagInfoArray;
  info: TOPCUAVariableInfo;
  i: Integer;
begin
  if OPCUADriver1.ListTags(tags) = ioOk then      // todas as variáveis abaixo de Objects
    for i := 0 to High(tags) do
      Memo1.Lines.Add(tags[i].Path + '  ' + tags[i].NativeType);

  if OPCUADriver1.GetVariableInfo('ns=3;s="DB1"."Speed"', info) = ioOk then
    Memo1.Lines.Add(Format('%s rank %d', [info.DataTypeName, info.ValueRank]));
end;
```

O `ListTags` percorre a pasta Objects recursivamente, até 12 níveis e 20000 variáveis (`Path` é o NodeId com `nsu=`, pronto para o `LongAddress`); o `Browse` devolve os filhos de um nó; o `GetVariableInfo` informa o tipo, o ValueRank (-1 escalar, ≥ 0 array), as dimensões e o nível de acesso de uma variável. Eles conectam sozinhos quando precisam.

##### Exemplo passo a passo

1. Coloque um `TOPCUADriver` no form e ajuste o `EndpointUrl` (ex.: `opc.tcp://192.168.0.10:4840`).
2. Clique com o botão direito nele → **Tag Builder** → **Browse**, selecione algumas variáveis e aperte **OK**.
3. Coloque controles HMI (`THMILabel`, `THMIEdit`, `THMITrackBar`, …) e aponte o `PLCTag` deles para os tags novos.
4. Rode. Os valores aparecem; digitar num `THMIEdit` escreve de volta no servidor.

Para testar sem um servidor, o `examples/laz_opcua_hmi` sobe um pequeno servidor de teste dentro do próprio programa.

##### Erros

Os StatusCodes do OPC UA viram os `TProtocolIOResult` de sempre dos tags:

| StatusCode (exemplos) | Resultado |
|---|---|
| Good, Uncertain | `ioOk` |
| BadNodeIdUnknown, BadNodeIdInvalid, BadNotFound | `ioObjectNotExists` |
| BadUserAccessDenied, BadNotReadable, BadNotWritable | `ioObjectAccessNotAllowed` |
| BadTypeMismatch, BadOutOfRange | `ioIllegalValue` |
| BadIndexRangeInvalid, BadIndexRangeNoData | `ioIllegalMemoryAddress` |
| BadTimeout | `ioTimeOut` |
| BadIdentityTokenInvalid/Rejected, senha errada | `ioAuthError` |
| BadCertificate…, BadSecurityChecksFailed, BadConnectionRejected | `ioConnectPLCFailed` |
| conexão ou sessão perdida | `ioCommError` |

##### Problemas comuns

| Sintoma | Causa provável |
|---|---|
| `LastError`: *libopen62541 1.5 not found* | A biblioteca não está instalada nem na pasta do executável; veja *A biblioteca open62541*. |
| *is not an open62541 1.5 library* / *was built without encryption* | Uma biblioteca de outra versão, ou compilada sem `UA_ENABLE_ENCRYPTION`. |
| Todos os tags em `ioConnectPLCFailed` com Sign/SignAndEncrypt | O servidor ainda não confia no certificado do cliente: exporte-o e adicione aos confiáveis do servidor. Ou o `TrustListDir` está preenchido e não tem o certificado do servidor. |
| Todos os tags em `ioAuthError` | Usuário/senha errados, ou o servidor não permite acesso anônimo. |
| `ioObjectNotExists` numa variável Siemens | NodeId sem as aspas (`s="DB1"."Speed"`), ou a variável não está *Accessible from HMI/OPC UA*. |
| Escritas recusadas (`ioObjectAccessNotAllowed`) | A variável não está *Writable from HMI/OPC UA*, o usuário não pode escrever, ou `ReadOnly = True`. |
| Um sinal rápido é visto com atraso | Diminua o `RefreshTime` (amostragem) e o `PublishingInterval`, ou use leitura periódica. |
| Um tag passa a ler outra variável (ou nenhuma) depois que o servidor foi atualizado | O servidor reordenou os namespaces e o índice do `ns=` mudou: use endereços `nsu=`. |

##### Exemplos relacionados

* `examples/laz_opcua_client` — conecta em qualquer endpoint, lista as variáveis do servidor, monitora e escreve as escolhidas; *Servidor de teste* sobe um servidor local para testar sem CLP.
* `examples/laz_opcua_hmi` — uma tela de IHM feita só no form: `TOPCUADriver`, tags e controles HMI ajustados em tempo de projeto, com as instruções no próprio form.
