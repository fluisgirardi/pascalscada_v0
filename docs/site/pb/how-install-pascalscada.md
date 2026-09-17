##### Introdução

Antes de mais nada, sinta-se livre para enviar correções e alternativas para este método 🙂

O objetivo principal desta página é ensinar como instalar a versão atual do PascalSCADA na versão atual do Lazarus. Instalar em versões antigas do Lazarus é assunto para outra página — ou, se quiser, sinta-se livre para escrever as instruções da IDE desejada.

**Atenção**: o PascalSCADA depende de outros pacotes, que precisam estar instalados (ou serão instalados junto):

* **BGRABitmap** — usado pelos controles gráficos da HCl. Está no *Online Package Manager* do Lazarus.
* **TAChart** — usado pelo `TTagLinkedSeriesSource`. Já vem com o Lazarus.
* **SQLDB** (`SQLDBLaz`) — usado pelo `THMIDBConnection`. Já vem com o Lazarus. *O ZeosLib não é mais necessário desde a versão 0.7.7.*

**ATENÇÃO DOBRADA:** O PascalSCADA não é como outros sistemas SCADA. Ele é um pacote, plugin ou addon (chame como quiser) que permite que o Lazarus crie aplicações HMI/SCADA. Portanto, um assistente de instalação com os botões "Next", "Next" e "Finish" não está disponível.

##### **O que preciso antes de iniciar?**

1. **Lazarus**, versão 2.0 ou mais nova, com **Free Pascal 3.0 ou mais novo** — o PascalSCADA é desenvolvido e testado com FPC 3.2.2 e a versão estável mais recente do Lazarus. Se você é usuário de Windows, você pode obter o Lazarus na página oficial: [http://www.lazarus-ide.org/](http://www.lazarus-ide.org/). Se você é usuário de Linux/FreeBSD, você pode obtê-lo a partir do repositório de software do seu sistema operacional ou, para ter a versão mais nova, com o [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe).
2. **O código do PascalSCADA**, de uma destas duas fontes:
    * **GitHub** (recomendado — é onde o desenvolvimento acontece): `git clone https://github.com/fluisgirardi/pascalscada_v0.git`, ou baixe o [ZIP do branch master](https://github.com/fluisgirardi/pascalscada_v0/archive/refs/heads/master.zip) e descompacte numa pasta de sua escolha.
    * **Online Package Manager** do Lazarus (*Package → Online Package Manager*): procure por *PascalSCADA*. É o caminho mais curto, mas o pacote publicado lá é a versão 0.7.7 (2021); o GitHub tem as correções e os drivers mais recentes.
3. Um programa para descompactar `.zip`, se não usar o `git`.

O SourceForge e o SVN citados em versões antigas desta página não são mais usados.

##### Os pacotes

Na pasta do PascalSCADA existem sete arquivos `.lpk`:

| Pacote | O que contém | Depende de |
|---|---|---|
| `pascalscada_common.lpk` | Threads, eventos, CRC, strings — base de tudo, sem LCL. | FCL |
| `pascalscada.lpk` | Portas de comunicação, drivers de protocolo, tags e escalas. Funciona sem LCL (aplicações de console e serviços). | `pascalscada_common` |
| `pascalscada_db.lpk` | `THMIDBConnection` e `TFPSBufDataSet`. | `SQLDBLaz` |
| `pascalscada_dsng.lpk` | A parte de *design-time*: registro na paleta, Tag Builders, editores de propriedade. | `pascalscada`, `IDEIntf` |
| `pascalscada_hmi.lpk` | A biblioteca de controles HMI (HCl), segurança, loggers, teclado virtual. | `pascalscada`, `pascalscada_db`, `pascalscada_dsng`, `BGRABitmapPack`, `TAChartLazarusPkg` |
| `pascalscada_full.lpk` | **Metapacote**: instala `pascalscada_dsng`, `pascalscada_hmi` e `pascalscada_db` de uma vez. É o que você instala. | os três acima |
| `pascalscada_externallibs_hmi.lpk` | **Opcional**, só Linux/FreeBSD: o leitor RFID `TSycRFIDReader`. Exige a `libhidapi` instalada no sistema (`sudo apt install libhidapi-dev`) — sem ela o Lazarus não sobe depois de instalar este pacote. Veja [Sistema de segurança](/pb/security-system/#rfid). | `pascalscada_hmi` |

##### Instalando…

Bem, a primeira coisa a fazer é instalar o Lazarus. Eu não quero explicar como instalar o Lazarus em qualquer sistema operacional/plataforma que existe neste mundo, porque instalar o Lazarus é uma tarefa muito simples, tanto em Windows (com o assistente de instalação com botões "Next", "Next" e "Finish") quanto em Linux (`sudo apt install lazarus`). Ao final do processo de instalação do Lazarus, você deve abri-lo e ver algo semelhante a isto:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051.png"><img class="aligncenter wp-image-71" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051-300x167.png" alt="Instalação limpa do Lazarus" width="1431" height="797" /></a>

Se o seu Lazarus recém instalado se parece com o da imagem acima, você está no caminho certo.

**Instale antes o BGRABitmap**: *Package → Online Package Manager*, marque *BGRABitmap* e clique em *Install*. O Lazarus vai baixar, compilar e se reconstruir. (Se você for instalar o PascalSCADA pelo OPM, pule este passo — ele resolve a dependência sozinho.)

O próximo passo é instalar o pacote do PascalSCADA. Por favor, esqueça do Lazarus por um momento. Vá para a pasta onde clonou ou descompactou o PascalSCADA.

Depois, volte para o Lazarus e acesse o menu mostrado abaixo (*Package → Open Package File (.lpk)*):

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338.png"><img class="aligncenter wp-image-72" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338-300x165.png" alt="Instalando um novo pacote no Lazarus" width="1431" height="786" /></a>

Clique neste menu e um diálogo para abrir arquivos será exibido:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052.png"><img class="aligncenter wp-image-73" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052-300x165.png" alt="Encontrando a instalação do PascalSCADA no sistema de arquivos" width="1427" height="784" /></a>

Navegue até a pasta do PascalSCADA. Nesta pasta deverão existir os sete arquivos `.lpk` da tabela acima. Se você não vê algum deles, verifique o arquivo baixado, paths, etc. Se tudo está OK, sua tela deverá ser parecida com esta:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053.png"><img class="aligncenter wp-image-75" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053-300x166.png" alt="Encontrando a instalação do PascalSCADA no sistema de arquivos (vista completa)" width="1428" height="791" /></a>

Selecione o arquivo **`pascalscada_full.lpk`** e abra-o. A janela do pacote `pascalscada_full` será mostrada:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054.png"><img class="aligncenter wp-image-76" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054-300x165.png" alt="Pacote do PascalSCADA para instalação completa aberto" width="1429" height="788" /></a>

A última etapa é instalar o pacote na sua IDE. Para fazer isto, clique no botão "Usar >>" e em seguida no menu "Instalar", como mostrado abaixo:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819.png"><img class="aligncenter wp-image-77" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819-300x167.png" alt="Instalando o PascalSCADA na IDE Lazarus" width="1427" height="794" /></a>

O pacote `pascalscada_full` é um metapacote, usado somente para fazer a instalação completa do PascalSCADA na sua IDE Lazarus. Por causa deste motivo, você será aborrecido com algumas mensagens:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055.png"><img class="aligncenter wp-image-78" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055-300x123.png" alt="Metapacote do PascalSCADA: necessário para simplificar a instalação completa" width="502" height="205" /></a>

Clique no botão "Install it, I like the fat" para continuar a instalação. A próxima tela irá mostrar uma lista com todos os pacotes que serão instalados durante o processo na sua IDE, como mostrado abaixo:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056.png"><img class="aligncenter wp-image-79" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056-300x167.png" alt="Lista dos pacotes adicionais que serão instalados" width="1421" height="790" /></a>

Uma imagem mais detalhada (a lista atual é `pascalscada_common`, `pascalscada`, `pascalscada_db`, `pascalscada_dsng` e `pascalscada_hmi`; a captura é de uma versão que ainda incluía o ZeosLib):

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057.png"><img class="aligncenter wp-image-80" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057-300x204.png" alt="Lista dos pacotes adicionais que serão instalados" width="502" height="341" /></a>

Clique no botão OK para continuar a instalação e chegar na pergunta final sobre o processo de recompilação da IDE, como pode ser vista abaixo:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058.png"><img class="aligncenter wp-image-81" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058-300x102.png" alt="Confirmando a recompilação do Lazarus" width="502" height="171" /></a>

Para finalizar, clique no botão "Sim". O processo de instalação será iniciado e sua IDE será recompilada. Ao final do processo de recompilação, o Lazarus será reiniciado. **Se você tem um projeto com alterações que não foram salvas, tenha cuidado!!**

Após reiniciar a IDE, você deverá encontrar as seguintes paletas de componentes no seu Lazarus:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059.png"><img class="aligncenter wp-image-83" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059-300x27.png" alt="PascalSCADA instalado: guia de portas de comunicação" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060.png"><img class="aligncenter wp-image-84" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060-300x27.png" alt="PascalSCADA instalado: guia com os protocolos de comunicação" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061.png"><img class="aligncenter wp-image-85" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061-300x27.png" alt="PascalSCADA instalado: guia com os componentes de utilidades" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062.png"><img class="aligncenter wp-image-86" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062-300x27.png" alt="PascalSCADA instalado: guia com todos os tags" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063.png"><img class="aligncenter wp-image-87" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063-300x27.png" alt="PascalSCADA instalado: guia com todos os controles de tela" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064.png"><img class="aligncenter wp-image-88" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064-300x27.png" alt="PascalSCADA instalado: guia com os controles de gerenciamento de usuários e segurança" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065.png"><img class="aligncenter wp-image-89" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065-300x27.png" alt="PascalSCADA instalado: guia com os componentes de banco de dados" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066.png"><img class="aligncenter wp-image-90" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066-300x27.png" alt="PascalSCADA instalado: guia com os componentes herdados do FreePascal" width="1221" height="108" /></a>

Se você consegue ver estas paletas, parabéns, você conseguiu instalar com sucesso o PascalSCADA na sua IDE Lazarus. Para conferir de vez, abra e compile um dos exemplos — `examples/laz_modbus_tcp_example` é um bom começo.

##### Instalando pela linha de comando

Para quem prefere o terminal, ou para montar uma máquina de build sem abrir a IDE, o `lazbuild` faz o mesmo em dois comandos, a partir da pasta do PascalSCADA:

```
lazbuild --add-package pascalscada_full.lpk
lazbuild --build-ide=
```

O primeiro registra o metapacote (e, por dependência, os demais) na IDE; o segundo recompila o Lazarus com eles. O BGRABitmap precisa estar registrado antes (instalado pelo OPM, ou `lazbuild --add-package caminho/para/bgrabitmappack.lpk`).

##### Linux: permissões e bibliotecas

* **Portas seriais**: o usuário que roda a aplicação precisa pertencer ao grupo dono das portas (`dialout` no Debian/Ubuntu, `uucp` em outras distribuições): `sudo usermod -aG dialout $USER` e faça login de novo. Veja [Portas de comunicação](/pb/communication-ports/).
* **Leitor RFID** (`pascalscada_externallibs_hmi`): `libhidapi-dev` e a regra udev descrita em [Sistema de segurança](/pb/security-system/#rfid).
* **Bancos de dados** (`THMIDBConnection`): a biblioteca cliente do banco escolhido (`libpq`, `libmariadb`/`libmysqlclient`, `libsqlite3`, …) precisa estar instalada — o SQLDB a carrega em tempo de execução.

##### Atualizando

Se instalou pelo GitHub: `git pull` na pasta do PascalSCADA, depois *Package → Open Package File* em `pascalscada_full.lpk` → *Usar >> → Instalar* (ou `lazbuild --build-ide=`), e a IDE recompila com o código novo. Pelo OPM, use o botão *Update* do próprio OPM.

##### E o Delphi?

O código mantém as diretivas para Delphi e há exemplos `.dpr` no repositório, mas **não há pacotes Delphi (`.dpk`) prontos** e a versão atual é desenvolvida e testada só no Lazarus/Free Pascal. Se quiser usá-lo no Delphi, conte com montar o pacote você mesmo — e mande as correções.

##### Problemas comuns

| Sintoma | Causa provável |
|---|---|
| "Package BGRABitmapPack not found" ao instalar | O BGRABitmap não foi instalado antes. Instale-o pelo OPM e repita. |
| A IDE não sobe depois de instalar `pascalscada_externallibs_hmi` | Falta a `libhidapi` no sistema (ou você está no Windows, onde este pacote não funciona). Inicie o Lazarus com `--skip-last-project`, desinstale o pacote e reconstrua. |
| `Cannot find unit tcp_udpport` (ou outra unit) ao compilar um projeto | O projeto não tem o pacote nas dependências: *Project → Project Inspector → Add → New Requirement* e adicione `pascalscada_hmi` (ou só `pascalscada`, para aplicações sem LCL). |
| Paletas aparecem, mas os componentes não têm ícone | Recursos não regerados após uma atualização — reconstrua a IDE (*Tools → Build Lazarus with Profile*). |
| Erro de compilação em versão antiga do Lazarus/FPC | Atualize: FPC 3.0 e Lazarus 2.0 são o mínimo; 3.2.2 e o Lazarus estável atual são o recomendado. |
