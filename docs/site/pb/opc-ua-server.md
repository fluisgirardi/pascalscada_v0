##### Introdução

![](img/TOPCUAServer.png) O `TOPCUAServer` é um **servidor OPC UA dentro da sua aplicação**. Ele publica os tags que você escolher — tags de memória, tags de expressão e os tags de qualquer driver de protocolo (Modbus, S7, Ethernet/IP, até de um cliente OPC UA) — para que qualquer cliente OPC UA possa lê-los e escrevê-los: UaExpert, um SCADA, um MES, um script Python ou outra aplicação PascalSCADA com um `TOPCUADriver`.

A aplicação continua sendo a única a conversar com os CLPs: os clientes veem os valores que a aplicação já tem, e uma escrita de um cliente passa pelo tag até o dispositivo, **com o cliente recebendo o resultado do dispositivo** (se o CLP recusar a escrita, o cliente fica sabendo).

Assim como no driver cliente, o protocolo é tratado pela **open62541 1.5** através do [fpopen62541](https://github.com/fluisgirardi/fpopen62541); veja em [Cliente OPC UA](/pb/opc-ua/) como obter a biblioteca. Ela só é carregada quando o servidor é ativado.

| | |
|---|---|
| O que é publicado | os tags listados em `Items` — nada é publicado sozinho |
| Tipos | o tipo nativo de cada tag (`TagType`), ou `Double` com a sua escala |
| Arrays e estruturas | `TPLCBlock` como array, `TPLCStruct` como objeto com uma variável por item |
| Escritas | por item (`Writable`), síncronas para o cliente, com o resultado do dispositivo |
| Segurança | None, Sign, SignAndEncrypt; anônimo, lista de usuários ou validação própria; permissão de escrita por usuário |

##### Componentes necessários

1. Um **`TOPCUAServer`** (unit `opcuaserver`, paleta *PascalSCADA Utils*).
2. Os tags a publicar, adicionados na sua coleção **`Items`**. Um **duplo clique** no servidor abre o *Add tags*: ele lista os tags do form ainda não publicados, com as opções de colocar cada um numa pasta com o nome do seu driver e de deixá-los graváveis.
3. `Active = True`. Um servidor salvo ativo num form inicia assim que o form termina de carregar.

É só isso: rode o programa e conecte um cliente em `opc.tcp://<computador>:4840`.

##### Propriedades do servidor

| Propriedade | Padrão | Descrição |
|---|---|---|
| `Active` | `False` | Inicia/para o servidor. |
| `Port` | `4840` | Porta TCP do endpoint `opc.tcp://<computador>:<Port>`. |
| `ServerName` | `PascalSCADA` | Nome da pasta, dentro de *Objects*, que contém os tags publicados. |
| `NamespaceUri` | `urn:pascalscada:tags` | Namespace das variáveis; `NamespaceIndex` (em execução) é o seu índice, o `ns=` dos NodeIds. |
| `LibraryName` | vazio | Caminho ou nome da libopen62541; vazio procura os nomes padrão e a pasta do executável. |
| `LogLevel` | `oslWarning` | Mensagens da open62541 que vão para o log. |
| `Items` | – | Os tags publicados (próxima seção). |
| `Running` / `LastError` | – | Em execução: se está servindo e, se não, por quê. |

As propriedades do servidor (porta, segurança...) só podem ser alteradas com ele parado. Os `Items` podem ser alterados a qualquer momento: itens adicionados, alterados ou removidos, e tags destruídos, são aplicados na hora num servidor em execução.

##### Itens

| Propriedade | Padrão | Descrição |
|---|---|---|
| `Tag` | – | O tag publicado. |
| `NodeName` | vazio | Nome da variável; vazio usa o nome do tag. |
| `Folder` | vazio | Pastas dentro de `ServerName`, separadas por `/`: `Linha1/Forno`. |
| `Description` | vazio | Descrição da variável para os clientes. |
| `Writable` | `False` | Se os clientes podem escrevê-la. |
| `DataType` | `odtNative` | `odtNative`: o tipo do tag; `odtScaledDouble`: o valor escalonado (`Value`) como `Double`. |

O NodeId de uma variável é `ns=<NamespaceIndex>;s=<Folder com '.'>.<NodeName>`, por exemplo `ns=1;s=Linha1.Forno.Temperatura`, então ele não muda quando o servidor reinicia.

**Tipos (`odtNative`).**

| Tag | Tipo OPC UA |
|---|---|
| `TPLCTagNumber` | o tipo do seu `TagType` (`pttByte` → Byte, `pttSmallInt` → Int16, `pttFloat` → Float...) com o valor bruto (`ValueRaw`); `pttDefault` → o tipo sem sinal da palavra do driver |
| `TTagBit` | `Boolean` para um bit (`StartBit = EndBit`); senão UInt16/32/64 pela quantidade de bits |
| `TPLCString` | `String` |
| `TPLCBlock` | um array do seu tipo, com o seu `Size` |
| `TPLCBlockElement` | o tipo do seu bloco |
| `TPLCStruct` | um objeto com uma variável por item (`TPLCStructItem` pelo seu tipo, `TPLCStructString` como String) |

**Qualidade e timestamp.** Cada valor leva o status da última leitura do tag (a leitura do tag de origem, para bits e elementos de bloco): um CLP que para de responder faz os clientes verem um status *Bad*, e não um valor congelado. Tags de memória e de expressão são sempre *Good*. O timestamp de origem é o horário da última atualização do tag.

**Arrays.** Um cliente pode ler e escrever uma parte de um array com uma faixa de índices (`2:4`, `5`).

##### Escritas

Uma escrita de um cliente num item `Writable`:

1. confere o tipo e os limites do tag (`BadOutOfRange` sem mexer no dispositivo);
2. converte o valor (escala inversa com `odtScaledDouble`, os bits de um `TTagBit` colocados na sua palavra, a codificação de uma string);
3. escreve o tag (`ScanWrite`) e **espera o dispositivo**;
4. responde ao cliente com o resultado do dispositivo.

Nada fica bloqueado enquanto o dispositivo responde: o servidor continua atendendo os outros clientes. `WriteTimeoutMs` (padrão 0 = 2 minutos) limita a espera; o cliente então recebe `BadTimeout`.

##### Segurança

| Propriedade | Padrão | Descrição |
|---|---|---|
| `Security` | `ossNone` | `ossNone`: só sem segurança; `ossNoneAndSecure`: sem e com segurança (Sign, SignAndEncrypt); `ossSecureOnly`: dados só por Sign/SignAndEncrypt. |
| `CertificateDir` | pasta de configuração da aplicação + `opcua-server-pki` | Certificado do servidor (`server_cert.der`, `server_key.der`). Criado autoassinado na primeira partida segura e mantido: os clientes confiam nele uma vez só. Substitua pelos seus, se quiser. |
| `TrustListDir` | vazio | Certificados de clientes aceitos (`.der`, `.pem`, `.crt`, `.cer`); vazio aceita qualquer cliente. |
| `AllowAnonymous` | `True` | Se um cliente pode conectar sem usuário. |
| `Users` | vazio | Usuários que podem logar, um por linha: `login=senha`. |
| `OnCheckUser` | – | Decide cada login; `Accepted` chega com o resultado de `Users`. Roda na thread principal: pode usar o gerenciador de segurança da aplicação. |
| `OnCanWrite` | – | Decide cada escrita, com o item e o usuário da sessão (vazio: anônimo). Uma escrita recusada recebe `BadUserAccessDenied`. |

* Um login com usuário exige uma política segura: o cliente criptografa a senha com o certificado do servidor. Com `Users` ou `OnCheckUser` e `Security = ossNone` o servidor não inicia, e o `LastError` diz por quê.
* Com `ossSecureOnly` o endpoint sem segurança continua lá, mas **só para a descoberta**: os clientes pegam o certificado do servidor nele, e nenhuma sessão é aberta por ele.
* Os logins (aceitos e recusados) e as escritas recusadas vão para o log; a senha, nunca.

```pascal
procedure TForm1.OPCUAServer1CheckUser(Sender: TObject; const User, Password: String;
  var Accepted: Boolean);
begin
  Accepted := Accepted or MinhaValidacao(User, Password);
end;

procedure TForm1.OPCUAServer1CanWrite(Sender: TObject; Item: TOPCUAServerItem;
  const User: String; var Allowed: Boolean);
begin
  //clientes anonimos so' leem; o operador nao muda os setpoints da Linha2
  Allowed := (User <> '') and not ((User = 'operador') and (Item.Folder = 'Linha2'));
end;
```

##### Exemplos relacionados

* `examples/laz_opcua_server` — publica um pequeno processo simulado (uma temperatura que segue um setpoint enquanto uma bomba está ligada, uma mensagem e um array de tendência) e mostra no form os valores escritos pelos clientes.
* `examples/laz_opcua_client` — um cliente para experimentar o servidor: conecte-o em `opc.tcp://localhost:4840`.
