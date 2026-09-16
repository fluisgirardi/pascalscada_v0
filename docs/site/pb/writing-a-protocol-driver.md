[toc]

##### Conhecimentos necessários

* Comunicação de dados;
* Conhecer o meio físico que seu protocolo usará (serial, TCP/IP, UDP);
* Criação de componentes em Delphi/Lazarus;
* Conhecimento do protocolo sendo implementado — com a especificação em mãos, ou pelo menos capturas de tráfego (Wireshark) de um equipamento real.

##### Definições

* **Scan de atualização**: método cíclico que atualiza o valor do tag com o último valor lido pelo driver de protocolo. Roda em uma thread própria do driver e entrega os valores aos tags na thread principal.
* **Scan de leitura**: método cíclico que varre as áreas de memória que mais precisam de atualização, codifica o pacote de dados a ser enviado ao seu equipamento, faz o envio através da porta de comunicação, espera a resposta da porta, recebe os dados, decodifica os dados recebidos e atualiza as áreas de dados internas com o valor e a data/hora da última leitura.
* **Scan de escrita**: método cíclico que atende as ordens de escrita enfileiradas pelos tags (`ScanWrite`, ou a atribuição de `Value` com `AutoWrite = True`), executando-as de maneira assíncrona — ou seja, a aplicação continua enquanto a escrita vai para o equipamento, e o resultado chega depois pelos eventos do tag.
* **Leitura síncrona**: leitura onde o scan de leitura é paralisado para liberar a porta de comunicação e a aplicação faz a leitura do tag diretamente (`Tag.Read`), bloqueando até a resposta chegar.
* **Escrita síncrona**: escrita onde o scan de leitura é paralisado da mesma forma e a aplicação escreve diretamente (`Tag.Write`), bloqueando até o equipamento confirmar.

##### Introdução

Antes de sair escrevendo códigos como um louco, vou explicar o conceito de como um driver de protocolo funciona no PascalSCADA. Um driver de protocolo é o objeto que gerencia os tags dentro de sua aplicação. Ele atualiza os tags de acordo com a sua taxa de atualização, organiza blocos e evita a duplicação de pedidos de leitura dessas memórias. Sua organização interna pode variar de acordo com o protocolo, que pode oferecer leituras em bloco ou individualizadas por memória. Tudo isso feito em nome da performance.

Os tags no PascalSCADA são **cópias dos valores de uma outra área de memória gerenciada pelo driver**. É através desta área interna que o driver gerencia seu scan de leitura. A organização dessa área de memória varia de acordo com a organização interna do equipamento. Os tags, por sua vez, podem representar um ou mais endereços de memória do seu equipamento.

O que a classe base `TProtocolDriver` já faz por você:

```
                  ┌──────────────────────── TProtocolDriver ────────────────────────┐
 tags ──AddTag──▶ │ lista de tags                                                   │
                  │                                                                 │
                  │ TScanThread  ──▶ DoScanRead ──▶ DoRead ──▶ porta ──▶ equipamento │
                  │                       (seu código: monta e decodifica frames)   │
                  │                                                                 │
                  │ TScanUpdate  ──▶ DoGetValue ──▶ Synchronize ──▶ Tag.Value       │
                  │                       (seu código: copia da área interna)       │
                  └─────────────────────────────────────────────────────────────────┘
```

* cria e mantém as duas threads (`TScanThread`, que chama `DoScanRead` em loop, e `TScanUpdate`, que chama `DoGetValue` para cada tag e entrega os valores na thread principal);
* implementa `Read`/`Write`/`ScanRead`/`ScanWrite` chamados pelos tags, pausando o scan e chamando o seu `DoRead`/`DoWrite`;
* cuida da sincronização entre as threads (seções críticas, pausa do scan) — você nunca precisa de um mutex nos métodos `Do*`;
* trata a propriedade `CommunicationPort`, o `ReadOnly`, os contadores e o `LiteralTagAddress` do Object Inspector.

O que fica para você: **sete métodos** que descrevem o seu equipamento e o seu protocolo. É o que o resto desta página mostra.

Para facilitar o entendimento, vou basear este texto num **CLP fictício**, que tem uma área de registradores de 16 bits, entradas e saídas digitais endereçadas em byte (8 bits), com todas as áreas suportando leitura/escrita em bloco (atualizar vários tags em uma única requisição), com endereçamento simples do CLP de 1 a 255 (semelhante ao Modbus).

##### Passo 1: a unit e a estrutura do equipamento

O primeiro passo é criar uma unit para abrigar seu novo protocolo e adicioná-la ao pacote do PascalSCADA (veja *Registrando o driver*, mais abaixo). Nesta unit, a representação do nosso CLP fictício seria semelhante à estrutura abaixo:

```pascal
uses ProtocolDriver, ProtocolTypes, PLCMemoryManager, Tag, PLCBlock, commtypes;

type
  TDummyPLC = record
    PLCAddress: Byte;
    Inputs:    TPLCMemoryManager;  // representada por MemReadFunction = 1 nos tags
    Outputs:   TPLCMemoryManager;  // representada por MemReadFunction = 2 nos tags
    Registers: TPLCMemoryManager;  // representada por MemReadFunction = 3 nos tags
  end;
  TDummyPLCs = array of TDummyPLC;
```

Note que foi usada uma classe chamada `TPLCMemoryManager`. O que ela faz? Ela organiza memórias de uma mesma área em blocos contínuos do maior tamanho possível, evitando a duplicação de memórias e otimizando sua comunicação. Ela tem duas propriedades que controlam todo o seu comportamento:

* **`MaxHole`**: controla quantas memórias podem faltar para manter um único bloco. Digamos que sejam adicionadas as memórias de endereços [1, 2, 5, 6]. Com `MaxHole = 0`, serão formados dois blocos, o primeiro com as memórias [1, 2] e o segundo com as memórias [5, 6]. Mas com `MaxHole = 2`, será formado um único bloco com os endereços [1, 2, 3, 4, 5, 6]. Note que os endereços 3 e 4 são adicionados para manter a continuidade do bloco, fazendo com que este seja lido com somente um pedido pelo driver de protocolo, melhorando a performance.
* **`MaxBlockItems`**: controla o tamanho máximo dos blocos. Se forem adicionados os endereços [1, 2, 3, 4, 5, 6] com `MaxBlockItems = 3`, serão formados dois blocos, o primeiro contendo os endereços [1, 2, 3] e o segundo os endereços [4, 5, 6]. Esta propriedade é útil em protocolos que limitam o tamanho máximo do pedido, como por exemplo o Modbus (125 registradores por leitura). `0` significa sem limite.

Os métodos que você vai usar nela:

| Método | Faz |
|---|---|
| `AddAddress(Address, Size, RegSize, ScanTime)` | Registra `Size` memórias a partir de `Address`, com scan de `ScanTime` ms. `RegSize` é o tamanho da variável em relação à menor palavra da área: para adicionar MW0, MW2 e MW4 de um Siemens (menor palavra = byte) use `AddAddress(0, 3, 2, 1000)`. O bloco resultante tem o **menor** `ScanTime` dos tags que o compõem. |
| `RemoveAddress(Address, Size, RegSize)` | O inverso. |
| `Blocks[i]` | Os blocos contínuos formados, cada um com `AddressStart`, `Size`, `ScanTime`, `LastUpdate` e `NeedRefresh` (verdadeiro quando passou o tempo de scan). |
| `SetValues(Address, Len, RegSize, Values, LastResult)` | Guarda os valores lidos do equipamento e marca a hora. |
| `SetFault(Address, Len, RegSize, Fault)` | Marca que a leitura falhou — os tags recebem o `TProtocolIOResult` e disparam `OnReadFail`. |
| `GetValues(Address, Len, RegSize, Values, LastResult, Timestamp)` | Copia valores, resultado e hora para entregar a um tag. |

Note também que adicionei um comentário a respeito de cada área de memória. Eu escolhi a propriedade `MemReadFunction` do tag para escolher a área de dados (entradas, saídas e registradores). O valor 1 identifica as entradas digitais, 2 as saídas digitais e 3 os registradores. Onde este mapeamento do valor da propriedade é ligado com a área de memória é um pouco mais à frente.

Feita a estrutura que representa o seu equipamento, o segundo passo é criar uma classe herdeira de `TProtocolDriver`:

```pascal
type
  TProtocoloFicticio = class(TProtocolDriver)
  private
    FCLPs: TDummyPLCs;
  protected
    procedure DoAddTag(TagObj: TTag; TagValid: Boolean); override;
    procedure DoDelTag(TagObj: TTag); override;
    procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt); override;
    procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec); override;
    function  DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult; override;
    function  DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult; override;
  public
    destructor Destroy; override;
    function SizeOfTag(aTag: TTag; isWrite: Boolean; var ProtocolTagType: TProtocolTagType): BYTE; override;
  published
    property ReadSomethingAlways;
  end;
```

##### Passo 2: adicionando tags ao scan — DoAddTag

Neste novo protocolo, você deve dizer como os tags serão adicionados ao scan do driver para serem lidos. E como faço isso? Simples. Sobrescreva o método:

```pascal
procedure DoAddTag(TagObj: TTag; TagValid: Boolean);
```

Ele irá adicionar tags à área de memória gerenciada pelo driver. Faça as devidas verificações do tag (exemplo: faixa de endereços correta, tipo correto) nesse método e, caso ele seja um tag válido, chame o método herdado com

```pascal
inherited DoAddTag(TagObj, TheTagIsValid);
```

para adicionar o tag na classe base para que este seja atualizado de acordo com a sua taxa de scan. O segundo parâmetro é importante: a classe base marca o tag como válido ou inválido, e um tag inválido fica na lista mas nunca é lido — é o que acontece quando o usuário preenche um `MemReadFunction` que o driver não conhece. Chame o `inherited` **sempre**, mesmo com `False`; se não chamar, o tag não é registrado e `DoDelTag` não vai encontrá-lo depois.

Neste exemplo, vou tratar somente tags bloco. Com o nosso CLP fictício, o método ficaria parecido com o código abaixo:

```pascal
procedure TProtocoloFicticio.DoAddTag(TagObj: TTag; TagValid: Boolean);
var
  Valido, clpencontrado: Boolean;
  clp: Integer;
begin
  Valido := False;
  clpencontrado := False;

  // como eu disse, será tratado somente tags bloco neste exemplo.
  if TagObj is TPLCBlock then
    with TagObj as TPLCBlock do begin
      // verifica se o endereço do CLP está na faixa aceita (entre 1 e 255)
      // e, se está, procura para ver se ele já não está cadastrado na
      // área de memória gerenciada pelo driver.
      if PLCStation in [1..255] then
        for clp := 0 to High(FCLPs) do
          if FCLPs[clp].PLCAddress = PLCStation then begin
            clpencontrado := True;
            Break;
          end;

      // se não encontrou o CLP, adiciona ele.
      if (not clpencontrado) and (PLCStation in [1..255]) then begin
        clp := Length(FCLPs);
        SetLength(FCLPs, clp + 1);
        FCLPs[clp].PLCAddress := PLCStation;
        FCLPs[clp].Inputs    := TPLCMemoryManager.Create;
        FCLPs[clp].Outputs   := TPLCMemoryManager.Create;
        FCLPs[clp].Registers := TPLCMemoryManager.Create;
        FCLPs[clp].Registers.MaxBlockItems := 125;   // limite do nosso protocolo
        clpencontrado := True;
      end;

      // verifica se o tag está válido (associado com alguma área)
      if clpencontrado and (MemReadFunction in [1..3]) then
        Valido := True;

      // adiciona o tag à área de memória gerenciada pelo driver de protocolo
      if Valido then
        case MemReadFunction of
          1: FCLPs[clp].Inputs.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
          2: FCLPs[clp].Outputs.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
          3: FCLPs[clp].Registers.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
        end;
    end;

  // adiciona o tag à classe base
  inherited DoAddTag(TagObj, Valido);
end;
```

`TagSizeOnProtocol` é o tamanho do tag **em palavras do protocolo** — um `TPLCBlock` de `Size = 10` com `TagType = pttFloat` sobre registradores de 16 bits ocupa 20 palavras. Ele já vem calculado pelo tag a partir do seu `SizeOfTag` (passo 7), por isso é ele, e não `Size`, que vai para o gerenciador.

Pronto, seu tag está validado e adicionado ao scan do seu driver de protocolo. Só que este método ainda não faz a leitura dos valores dos tags sozinho. Não, mas poderia — porém seria ineficiente quando se procura performance, pois como a base não conhece a estrutura do driver, a maneira de se fazer isso seria passar tag a tag solicitando-os para o dispositivo e, caso existisse um tag duplicado (mesma área de memória de um mesmo CLP), ele seria solicitado duas vezes. Para fazer a rotina de scan melhorada é necessário que sejam sobrescritos dois métodos.

##### Passo 3: entregando valores aos tags — DoGetValue

```pascal
procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
```

Lembram que os tags são cópias de uma área de memória gerenciada pelo driver? Pois é, este é o método que realiza essa cópia. Ele é chamado pela thread de atualização para cada tag, na taxa do tag. A maneira de implementá-lo é simples: basta procurar a sua memória na organização interna do protocolo. No nosso driver fictício ficaria mais ou menos assim:

```pascal
procedure TProtocoloFicticio.DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
var
  clp: Integer;
begin
  // varre os CLPs e, caso encontre o CLP procurado, solicita ao
  // gerenciador de blocos da área desejada os valores do tag.
  for clp := 0 to High(FCLPs) do
    if FCLPs[clp].PLCAddress = TagRec.Station then
      case TagRec.ReadFunction of
        1: FCLPs[clp].Inputs.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                       values.LastQueryResult, values.ClkMonotonicTStamp);
        2: FCLPs[clp].Outputs.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                        values.LastQueryResult, values.ClkMonotonicTStamp);
        3: FCLPs[clp].Registers.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                          values.LastQueryResult, values.ClkMonotonicTStamp);
      end;
end;
```

`TTagRec` é a fotografia das propriedades do tag no momento do pedido: `Station`, `Address`, `Size`, `ReadFunction`, `WriteFunction`, `File_DB`, `SubElement`, `Rack`, `Slot`, `Path` (o `LongAddress`), `UpdateTime`, `Retries` e o `CallBack` que devolve o resultado ao tag. É com ela — e não com o objeto do tag — que `DoGetValue`, `DoRead` e `DoWrite` trabalham, porque esses métodos rodam nas threads do driver.

##### Passo 4: o scan de leitura — DoScanRead

Perfeito, mas quem lê os dados do meu equipamento e atualiza os valores dessas áreas de memória? Quem faz isso é o método

```pascal
procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt);
```

Este procedimento é chamado, em loop, pela thread de scan para verificar se há algum tag necessitando ser lido. Ele faz isso varrendo a área de memória gerenciada pelo driver. Ao final, basta informar à thread de scan o que foi feito, usando a variável `NeedSleep`:

* Caso `NeedSleep = 0`, a thread segue sua execução normal. Retorne 0 sempre que o driver executar alguma ação de E/S, pois estas atuam como um "atraso natural", evitando o consumo alto de CPU.
* Caso `NeedSleep < 0`, a thread de scan irá forçar uma troca de contexto de threads (`ThreadSwitch`), retornando assim que possível. Você pode optar por retornar esse valor sempre que o seu driver não fizer nada.
* Caso `NeedSleep > 0`, a thread irá dormir `NeedSleep` milissegundos. Você pode optar por retornar esse valor sempre que o seu driver não fizer nada.

> O erro mais comum de um driver novo é retornar `0` sem ter feito E/S: a thread de scan vira um loop apertado e a CPU vai a 100 %. Se não havia nada para ler, devolva `1` (ou o tempo até o próximo bloco vencer).

A rotina de scan do nosso driver fictício ficaria parecida com esta (note que ela usa o `DoRead`, apresentado no passo seguinte):

```pascal
procedure TProtocoloFicticio.DoScanRead(Sender: TObject; var NeedSleep: LongInt);
var
  clp, bloco: Integer;
  tagrec: TTagRec;
  ReadResult: TProtocolIOResult;
  Values: TArrayOfDouble;
  fezAlgo: Boolean;
begin
  fezAlgo := False;
  NeedSleep := 0;

  // sem porta ativa não há o que fazer: durma um pouco em vez de girar
  if (PCommPort = nil) or (not PCommPort.ReallyActive) then begin
    NeedSleep := 1;
    Exit;
  end;

  // varre os CLPs
  for clp := 0 to High(FCLPs) do begin
    // varre os blocos formados das entradas digitais
    for bloco := 0 to High(FCLPs[clp].Inputs.Blocks) do
      // caso o bloco necessite ser atualizado...
      if FCLPs[clp].Inputs.Blocks[bloco].NeedRefresh then begin
        // preenche os campos da estrutura TTagRec que importam
        // para o protocolo (ou para a função DoRead)
        tagrec.Station      := FCLPs[clp].PLCAddress;
        tagrec.ReadFunction := 1;
        tagrec.Address      := FCLPs[clp].Inputs.Blocks[bloco].AddressStart;
        tagrec.Size         := FCLPs[clp].Inputs.Blocks[bloco].Size;

        // realiza a leitura usando a função DoRead
        ReadResult := DoRead(tagrec, Values, False);
        if ReadResult = ioOk then
          FCLPs[clp].Inputs.SetValues(tagrec.Address, tagrec.Size, 1, Values, ReadResult)
        else
          FCLPs[clp].Inputs.SetFault(tagrec.Address, tagrec.Size, 1, ReadResult);
        fezAlgo := True;
      end;

    // repete o procedimento acima para as saídas digitais e para os registradores...
  end;

  if not fezAlgo then
    NeedSleep := 1;
end;
```

Dois refinamentos que os drivers do PascalSCADA fazem e que valem a pena quando o número de blocos cresce:

* **Ler um bloco por chamada, o mais atrasado primeiro.** Em vez de ler todos os blocos vencidos de uma vez (o que atrasa os tags rápidos enquanto os lentos são lidos), o driver Modbus monta a lista de todos os blocos, ordena por atraso e lê só o primeiro; a thread chama `DoScanRead` de novo em seguida. Assim um tag de 100 ms não espera vinte blocos de 5 s.
* **`ReadSomethingAlways`**: quando nenhum bloco venceu, ler mesmo assim o mais antigo mantém a conexão viva e detecta a queda do equipamento cedo. Publique a propriedade herdada e respeite-a no seu scan.

##### Passo 5: lendo do equipamento — DoRead

Para não trabalhar dobrado montando pacotes de dados para enviar ao dispositivo, sobrescreva o método

```pascal
function DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
```

pois é ele quem executa as leituras síncronas (solicitadas pelo método `Read` do tag) e assíncronas (pedidos feitos pelo scan do driver de protocolo). A montagem do pacote do pedido da memória e a decodificação do pacote retornado contendo os valores das memórias deve ser feita aqui. O parâmetro `tagrec` contém as informações do tag (endereço do equipamento, endereço da memória), enquanto a variável `Values` irá receber os valores decodificados do pacote recebido. O parâmetro `Sync` não é mais usado.

O retorno é um `TProtocolIOResult`: `ioOk`, `ioTimeOut`, `ioCommError` (resposta corrompida), `ioIllegalFunction`/`ioIllegalRegAddress`/`ioIllegalValue` (o equipamento recusou), `ioPLCError`, `ioDriverError`, `ioNullDriver` (sem porta ativa)… Escolha o mais específico: ele chega ao tag em `LastSyncReadStatus` e é o que o usuário vê para diagnosticar.

É aqui que o driver conversa com a **porta de comunicação**. A porta (`TCommPortDriver`, seja `TSerialPortDriver` ou `TTCP_UDPPort`) não sabe nada do protocolo — ela só transporta bytes — e oferece uma operação síncrona:

```pascal
function IOCommandSync(Cmd: TIOCommand;             // iocWrite, iocRead ou iocWriteRead
                       BytesToWrite: Cardinal; ToWrite: BYTES;
                       BytesToRead: Cardinal;      // quantos bytes esperar de resposta
                       DriverID, DelayBetweenCmds: Cardinal;
                       pkt: PIOPacket;             // resultado
                       OnBegin: TNotifyEvent = nil; OnEnd: TNotifyEvent = nil): Cardinal;
```

O `TIOPacket` devolvido traz `WriteIOResult`/`ReadIOResult` (`iorOK`, `iorTimeOut`, `iorPortError`…), `Received` e `BufferToRead`. Como uma porta pode ser compartilhada por vários drivers, cerque a transação com `PCommPort.Lock(DriverID)` / `Unlock(DriverID)` — `DriverID` é um número único que a classe base já deu ao seu driver. O padrão que todos os drivers do PascalSCADA seguem:

```pascal
function TProtocoloFicticio.DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
  Sync: Boolean): TProtocolIOResult;
var
  pedido: BYTES;
  pkt: TIOPacket;
  faltam: LongInt;
begin
  Result := ioNullDriver;
  if (PCommPort = nil) or (not PCommPort.ReallyActive) then Exit;

  pedido := MontaPedidoDeLeitura(tagrec);          // seu protocolo: cabeçalho, função, endereço, CRC...

  PCommPort.Lock(DriverID);
  try
    // 1) envia o pedido e lê o cabeçalho da resposta (tamanho fixo)
    if PCommPort.IOCommandSync(iocWriteRead, Length(pedido), pedido, TAMANHO_CABECALHO,
                               DriverID, 0, @pkt) = 0 then begin
      Result := ioDriverError;
      Exit;
    end;
    if pkt.ReadIOResult = iorTimeOut then begin Result := ioTimeOut;  Exit; end;
    if pkt.ReadIOResult <> iorOK      then begin Result := ioCommError; Exit; end;

    // 2) o cabeçalho diz quantos bytes ainda vêm: lê o resto
    faltam := BytesRestantes(pkt.BufferToRead);
    if faltam > 0 then begin
      if PCommPort.IOCommandSync(iocRead, 0, nil, faltam, DriverID, 0, @pkt2) = 0 then ...
      pkt.BufferToRead := ConcatenateBYTES(pkt.BufferToRead, pkt2.BufferToRead);
    end;

    // 3) valida (CRC, eco da função, código de exceção) e converte para Double
    Result := DecodificaResposta(pkt.BufferToRead, tagrec, Values);
  finally
    PCommPort.Unlock(DriverID);
    SetLength(pedido, 0);
    SetLength(pkt.BufferToRead, 0);
  end;
end;
```

A leitura em duas etapas — cabeçalho de tamanho fixo e depois o resto — é o que permite ao driver saber quantos bytes esperar sem adivinhar; num protocolo de tamanho fixo basta uma chamada. Se a resposta vier de outra função ou de outra estação (lixo na linha serial), esvazie a porta com `iocRead` de 1 byte até dar timeout, como o Modbus faz, senão o pedido seguinte lê a sobra.

Os valores em `Values` são sempre `Double`, **uma palavra do protocolo por posição**: para um bloco de 4 registradores de 16 bits, `Values` tem 4 elementos com 0..65535. É o tag quem monta `pttFloat`, `pttLongInt` etc. a partir dessas palavras, usando o tamanho que você informou em `SizeOfTag` — o driver não converte tipos.

**Separe a montagem do frame do fluxo de E/S.** Os drivers Modbus e Melsec põem o fluxo acima em uma classe base da família e deixam para as subclasses só dois métodos virtuais, `EncodePkg` (TTagRec → bytes) e `DecodePkg` (bytes → valores + `SetValues`/`SetFault`). É assim que `TModBusRTUDriver` e `TModBusTCPDriver` compartilham 90 % do código diferindo só no cabeçalho e no CRC. Faça o mesmo desde o início se o seu protocolo tem variantes serial/TCP.

##### Passo 6: escrevendo no equipamento — DoWrite

Bom, já consigo ler meus tags; e como faço para escrever valores no dispositivo? O procedimento `DoWrite` existe para isso, bastando sobrescrevê-lo, montando o pacote e enviando este ao seu dispositivo:

```pascal
function DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
```

É este método quem executa as escritas de valores síncronas (usando o tag com `AutoWrite = False` e posteriormente chamando o procedimento `Write`) e assíncronas (quando é atribuído algum valor à propriedade `Value` com `AutoWrite = True`) no equipamento. O parâmetro `tagrec` contém as informações do tag (endereço do equipamento, endereço da memória — use `WriteFunction`, não `ReadFunction`), enquanto `Values` contém os valores a serem escritos. O parâmetro `Sync` não é mais usado.

O fluxo é o mesmo do `DoRead`: montar, `Lock`, `IOCommandSync`, validar a confirmação, `Unlock`. Depois de uma escrita bem-sucedida, atualize também a área interna com `SetValues` — assim os outros tags que apontam para a mesma memória mostram o valor novo antes do próximo scan. Se o driver tiver `ReadOnly = True`, a classe base já recusa a escrita com `ioReadOnlyProtocol` antes de chegar aqui.

##### Passo 7: o tamanho das palavras — SizeOfTag

Bom, mas meu equipamento tem os registradores sendo words e as entradas e saídas digitais são endereçadas como bytes. Como especifico isso? Simples. Os tags fazem uma série de conversões de tipo, mas o protocolo precisa informar que tipo de dado ele está fornecendo para o tag, para que ele possa fazer as devidas conversões. Para isso sobrescreva

```pascal
function SizeOfTag(aTag: TTag; isWrite: Boolean; var ProtocolTagType: TProtocolTagType): BYTE;
```

pois é esta a função responsável por informar o tamanho da palavra em bits que o tag está referenciando e o tipo dela (`ptBit`, `ptByte`, `ptShortInt`, `ptWord`, `ptSmallInt`, `ptDWord`, `ptLongInt`, `ptFloat`, `ptInt64`, `ptQWord`, `ptDouble`). Para retornar o tamanho da palavra use o `Result` da função e para retornar o tipo utilize a variável `ProtocolTagType`. `isWrite` permite tamanhos diferentes para leitura e escrita (no Modbus, ler com a função 3 e escrever com a 16 dá o mesmo tamanho, mas ler com a 1 e escrever com a 5 também). Em nosso driver fictício ficaria assim (mais uma vez vou tratar somente os blocos, cabe a você tratar os demais tags), assumindo que entradas e saídas digitais são bytes e registradores são words:

```pascal
function TProtocoloFicticio.SizeOfTag(aTag: TTag; isWrite: Boolean;
  var ProtocolTagType: TProtocolTagType): BYTE;
begin
  Result := 0;
  ProtocolTagType := ptUnknown;
  if aTag is TPLCBlock then
    case TPLCBlock(aTag).MemReadFunction of
      1, 2: begin
        Result := 8;
        ProtocolTagType := ptByte;
      end;
      3: begin
        Result := 16;
        ProtocolTagType := ptWord;
      end;
    end;
end;
```

Este valor é consultado pelo tag ao calcular `TagSizeOnProtocol` e ao montar os `TagType` maiores que a palavra do protocolo (dois registradores de 16 bits viram um `pttFloat`). Se você informar 16 bits mas entregar bytes em `Values`, os valores saem errados.

##### Passo 8: removendo tags — DoDelTag

Bom, já li demais. Quero apagar meus tags, fechar essa página e ir embora… Calma, para o seu tag ser apagado e removido do scan do driver sobrescreva um último método:

```pascal
procedure DoDelTag(TagObj: TTag);
```

Sobrescreva este procedimento para remover tags do scan do driver. Não esqueça de chamar o método herdado,

```pascal
inherited DoDelTag(TagObj);
```

para remover o tag da classe base. O código ficaria muito semelhante ao de `DoAddTag`, exceto pelo fato de que o tag irá ser removido da área gerenciada pelo driver (`RemoveAddress` com os mesmos parâmetros do `AddAddress`). Ele é chamado quando o tag é destruído, quando troca de `ProtocolDriver` e — importante — **quando qualquer propriedade de endereço muda**: o tag se remove com o endereço antigo e se adiciona de novo com o novo. Por isso `DoAddTag`/`DoDelTag` precisam ser simétricos.

No `destructor` do driver, libere os `TPLCMemoryManager` que você criou.

**Vale lembrar que o seu driver precisa ter uma porta de comunicação setada e ativa para os tags estarem sendo atualizados. Esta é uma condição para o driver de protocolo atualizar os tags.**

##### Registrando o driver

Com os sete métodos escritos, falta o driver aparecer na paleta:

1. **Unit no pacote**: adicione a unit ao `pascalscada.lpk` (ou ao seu próprio pacote, que dependa do `pascalscada`).
2. **Registro na paleta**: em `src/scada_dsng/scadareg.pas`, inclua a classe na chamada `RegisterComponents(strProtocolsPallete, [...])`. Num pacote próprio, crie um `procedure Register` com a mesma chamada.
3. **Ícone**: um PNG de 24×24 chamado `TProtocoloFicticio.png` em `artwork/24x24png/` (e o SVG em `artwork/scalable/`); rode `artwork/generateres.sh` para regerar o arquivo de recursos.
4. **Tag Builder (opcional)**: o assistente que cria tags pelo botão direito no driver. Como ele tem forms (LCL), ele fica no pacote de design (`scada_dsng`), não na unit do driver: o driver expõe `HasTabBuilderEditor`/`OpenTagEditor` e uma variável global `SetTagBuilderToolFor…ProtocolFamily` que o pacote de design preenche na inicialização — veja `modbustagassistant.pas` e `siemenstagassistant.pas` como modelo. Sem ele o driver funciona normalmente, só não tem o item de menu.
5. **`LiteralTagAddress`** (opcional): sobrescreva para o Object Inspector mostrar o endereço no formato do seu protocolo (`DB5.DBW20`, `40001`) na dica do tag.

##### Testando sem o equipamento

Você não precisa de um CLP para desenvolver o driver: a suíte de testes tem uma **porta falsa** (`tests/testsupport.fakeport.pas`, `TFakeCommPort`) que grava o que o driver escreveu e devolve respostas programadas. Um teste de leitura fica assim (copiado, com adaptações, de `tests/ut.modbustcp.pas`):

```pascal
// DoRead é protected: uma "sonda" o expõe ao teste e desliga a thread de
// scan, para que só o teste use a porta falsa.
type
  TProtocoloFicticioProbe = class(TProtocoloFicticio)
  protected
    procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt); override;  // NeedSleep := 1
  public
    function ReadSync(const aTag: TTagRec; out aValues: TArrayOfDouble): TProtocolIOResult;
  end;

function TProtocoloFicticioProbe.ReadSync(const aTag: TTagRec; out aValues: TArrayOfDouble): TProtocolIOResult;
begin
  Result := DoRead(aTag, aValues, True);
end;

procedure TTestProtocoloFicticio.SetUp;
begin
  FPort := TFakeCommPort.Create(nil);
  FPort.Active := True;
  FDrv := TProtocoloFicticioProbe.Create(nil);
  FDrv.CommunicationPort := FPort;
end;

procedure TTestProtocoloFicticio.UmaLeituraVaiEVoltaPelaPorta;
var
  res: TProtocolIOResult;
  vals: TArrayOfDouble;
begin
  // o que o "equipamento" responderia
  FPort.QueueResponse(BytesOf('01 03 04 00 0A 00 14'));

  res := FDrv.ReadSync(TagRecFor(1, 3, 16, 0, 2), vals);   // estação, função de leitura, de escrita, endereço, tamanho

  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertBytesEqual('o pedido que saiu', BytesOf('01 03 00 00 00 02'), FPort.LastWrittenFrame);
  AssertEquals('dois valores', 2, Length(vals));
  AssertEquals('primeiro registro', 10, vals[0], 0);
  AssertEquals('resposta consumida inteira', 0, FPort.PendingResponses);
end;
```

`TagRecFor`, `BytesOf` e `AssertBytesEqual` vêm de `tests/testsupport.protocol.pas` e `tests/testsupport.bytes.pas`. `QueueTimeout` simula o equipamento mudo, para testar o caminho de `ioTimeOut`; `WrittenFrame(i)` inspeciona cada pedido quando o driver faz várias trocas por leitura. Os testes rodam com `tests/pascalscada_tests.lpi`. Escreva um teste por função do protocolo e um por código de erro **antes** de ligar no equipamento real — quase todos os defeitos de driver (byte trocado, tamanho errado, CRC) aparecem aqui, em segundos, com o frame esperado ao lado do frame que saiu.

##### Erros comuns

| Sintoma | Causa |
|---|---|
| Tags nunca atualizam e nem marcam falha | `DoAddTag` não chamou `inherited` com `True`, ou `DoScanRead` nunca chama `SetValues`/`SetFault`. |
| CPU em 100 % | `DoScanRead` devolve `NeedSleep = 0` sem ter feito E/S. |
| Segundo driver na mesma porta trava | `Lock` sem `Unlock` num caminho de erro — use `try…finally`. |
| Valores errados só nos tipos de 32 bits | `SizeOfTag` informa um tamanho diferente do que `Values` entrega. |
| Tag some do scan ao mudar o endereço | `DoDelTag` não remove com os mesmos parâmetros que `DoAddTag` adicionou. |
| Leitura seguinte devolve lixo depois de um erro | A porta não foi esvaziada após uma resposta inesperada. |
| Falhas aparecem como `ioDriverError` genérico | `DecodePkg` não distingue timeout, CRC e exceção do equipamento. |

Espero que eu tenha conseguido repassar como os drivers funcionam no PascalSCADA. Críticas, sugestões, melhorias e principalmente correções, usem os comentários.

##### Drivers de referência no repositório

Todos em `src/scada/`, do mais simples ao mais completo:

* `iboxdriver.pas` — `TIBoxDriver`: serial, poucos registradores fixos por estação, sem gerenciador de memória; o menor exemplo completo dos sete métodos.
* `westasciidriver.pas` — `TWestASCIIDriver`: protocolo ASCII serial com checksum.
* `modbusdriver.pas` + `modbusserial.pas` + `modbustcp.pas` — a família Modbus: base com o fluxo de E/S e `TPLCMemoryManager` por área, subclasses com `EncodePkg`/`DecodePkg` para RTU e TCP. É o modelo recomendado.
* `MelsecDriver.pas` + `MelsecTCP.pas` — mesma estrutura de família, com dez áreas de memória.
* `s7family.pas` + `isotcpdriver.pas` — protocolo com conexão e negociação (PDU), várias áreas e blocos por DB.
* `lgxdriver.pas` — EtherNet/IP: endereçamento simbólico por `LongAddress`, sem gerenciador de memória por endereço.

Os testes correspondentes em `tests/ut.*.pas` mostram, frame a frame, o que cada um envia e espera.
