Saiu o PascalSCADA **0.8** — a primeira versão desde a 0.7.7, publicada no *Online Package Manager* do Lazarus em 2021. Muita coisa aconteceu nesse meio-tempo (mais de 550 commits), então aqui vai o que muda para quem vem da 0.7.7.

##### Drivers de protocolo novos

* **S7CommPlus** (`TS7CommPlusDriver`) — o protocolo nativo dos **S7-1200 e S7-1500**, o mesmo que o TIA Portal usa. Os tags são endereçados pelo **nome simbólico** (`DB4.Var1`, `"Motor".Speed`), sem precisar desligar o acesso otimizado dos blocos. Traz TLS (inclusive certificado do cliente), usuário/senha para CPUs protegidas, valores **por mudança** através de assinaturas em vez de polling, leitura e escrita de várias variáveis numa requisição só, leitura e escrita de um DB inteiro e os **alarmes do CLP** (o `THMIS7PlusAlarmLogger` os grava no mesmo fluxo dos alarmes baseados em tags). Um **Tag Builder** navega pelo CLP e cria os tags — variáveis simples, arrays, estruturas e arrays de estruturas. Veja [S7CommPlus](/pb/s7commplus/).
* **Cliente OPC UA** (`TOPCUADriver`) — lê e escreve as variáveis de qualquer servidor OPC UA (S7-1200/1500, PLCSim, outras marcas, gateways, outros SCADAs), por assinatura ou polling, com segurança None/Sign/SignAndEncrypt e login anônimo ou com usuário/senha. É construído sobre o **open62541 1.5** através do binding [fpopen62541](https://github.com/fluisgirardi/fpopen62541), que agora vem dentro do PascalSCADA. Também tem o seu Tag Builder. Veja [Cliente OPC UA](/pb/opc-ua/).
* **EtherNet/IP** (`TLGXDriver`) — **ControlLogix e CompactLogix** da Rockwell/Allen-Bradley, pelo nome do tag, com Tag Builder.
* **Xinje, Delta, Inovance e Inovance H5U** — drivers TCP para essas famílias de CLP (uma contribuição da comunidade).

Todos os drivers ganharam também uma forma comum de listar os tags de um equipamento (`TProtocolDriver.ListTags`) e de escrever vários tags numa chamada só (`MultiWrite`/`ScanMultiWrite`), que os drivers capazes de agrupar fazem numa única requisição.

##### HMI

* **`THMIButton`** — um botão de dois estados com imagens próprias (ou arquivos de imagem), um tag de retorno e quatro modos de clique, inclusive jog.
* **Registradores de alarmes e eventos** — `THMIAlarmLogger`, `THMIEventLogger` e, para S7-1200/1500, o `THMIS7PlusAlarmLogger`.
* **Novos controles desenhados** — `THMIBandeja`, controles de fluxo que acompanham o processo (válvulas, bombas, elevadores, polilinhas, `THMIFlowVectorControl` com conteúdo SVG), e o `THMIControlDislocatorAnimation2` para dois eixos independentes.
* **`TTagLinkedSeriesSource`** — os valores dos tags direto em séries do TAChart.
* Os teclados virtuais agora também digitam no **GTK3 e Qt6** (suporte inicial), além de GTK2, Qt5 e Win32/64.

##### Segurança

* **`TCentralUserManagement`** — usuários e permissões guardados num servidor e compartilhados por todas as estações (com SSL e cache de autorizações), com um exemplo de servidor.
* **Login por cartão / RFID** como alternativa à senha, com o leitor Sycreader no Linux e, agora, no Windows.

##### Banco de dados: SQLdb no lugar do ZeosLib

O `THMIDBConnection` agora usa o **SQLdb** do próprio Free Pascal: o ZeosLib deixou de ser dependência. O componente funciona do mesmo jeito (fila de comandos, thread, resultados de volta na thread principal), e a conexão pode ser renovada a cada transação. Valores de protocolo de projetos antigos com sufixo de versão (`mysql-5.7`) continuam aceitos. Veja [HMIDBConnection](/pb/hmidbconnection/).

##### Menos defeitos: uma suíte de testes de verdade

O PascalSCADA nunca teve testes automatizados. Agora tem cerca de **2.100**, cobrindo as portas, todos os drivers de protocolo, a camada de tags, os controles de HMI, a segurança e a camada de banco de dados, e eles rodam a cada mudança no **Windows 32/64, Linux 32/64 e FreeBSD**, além de **ARM (armhf e aarch64)**. Todos os exemplos também são compilados no CI, para nenhum deles ficar para trás da biblioteca de novo.

Escrever esses testes revelou e corrigiu uma longa lista de defeitos antigos — leituras Modbus que deixavam a porta travada, o scan do ISOTCP esperando para sempre por um CLP mudo, escritas West ASCII que perdiam a grandeza do valor, um checkbox que escrevia duas vezes, vazamentos de memória e corrupções de heap em pontos centrais, e muitos outros. Um tag rápido ao lado de um lento não é mais atualizado no ritmo do lento.

##### Documentação

A maior parte das páginas de "como usar" foi reescrita ou escrita pela primeira vez, em inglês e português: tags, portas, drivers, a biblioteca de HMI, segurança, processadores de escala, `THMIDBConnection`, [S7CommPlus](/pb/s7commplus/), [OPC UA](/pb/opc-ua/), expressões no Object Inspector e um tutorial sobre [como escrever um driver de protocolo](/pb/writing-a-protocol-driver/).

##### Atualizando a partir da 0.7.7

* **ZeosLib → SQLdb**: o `pascalscada_db` agora precisa do `SQLDBLaz` (que vem com o Lazarus) no lugar do `zcomponent`. Código que mexia nos objetos do Zeos dentro do `THMIDBConnection` precisa passar para os do SQLdb.
* **Dependência nova**: o pacote `pascalscada` requer o `fpopen62541`, que vem na árvore do PascalSCADA (`src/external/fpopen62541`) — abra-o uma vez antes de instalar. A biblioteca open62541 em si só é necessária em tempo de execução, nos programas que usam OPC UA.
* **S7CommPlus com TLS** precisa do OpenSSL (1.1.1 ou 3.x) em tempo de execução.
* **`TProtocolIOResult`** tem uma ordem nova (e o `ioAuthError` no fim): se você guardou os valores dele como números, mapeie-os de novo.
* Os pacotes exigem uns aos outros na **0.8** ou mais nova: atualize todos juntos.

##### Como obter

O código está no [GitHub](https://github.com/fluisgirardi/pascalscada_v0) — veja [Como instalar](/pb/how-install-pascalscada/). O pacote da 0.8 para o *Online Package Manager* do Lazarus vem em seguida, para instalar direto pela IDE.

Se algo quebrar, ou se tiver uma sugestão, abra uma issue no [GitHub](https://github.com/fluisgirardi/pascalscada_v0).
