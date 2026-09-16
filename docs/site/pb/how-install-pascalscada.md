##### Introdução

Antes de mais nada, sinta-se livre para enviar correções e alternativas para este método 🙂

O objetivo principal desta página é ensinar como instalar o último snapshot do PascalSCADA na última versão do Lazarus. Porque eu não irei explicar como instala-lo no Delphi ou em versões passadas do Lazarus são assuntos para uma outra página or se quiser, sinta-se livre para escrever as instruções de como instalar na IDE desejada.

**Atenção** : A versão atual do pacote PascalSCADA irá instalar outros pacotes juntos com ele (que são requisitos). Estes pacotes são:

  * ZeosLib 7.2
  * BGRABitmap
  * BGRAControls

Então, se você não gosta de algum destes pacotes, sinta-se livre para criar um novo pacote do PascalSCADA sem os pacotes que você não gosta.

**ATENÇÃO DOBRADA:** O PascalSCADA não é como outros sistemas SCADA. Ele é um pacote, plugin ou addon (chame como quiser) que permite que o Lazarus crie aplicações HMI/SCADA. Portanto, um assistente de instalação com os botões “Next”, “Next” e “finish” não está disponível.

##### **O que preciso antes de iniciar?**

  1. Última versão do Lazarus. Se você é usuário de Windows, você pode obter o Lazarus na página oficial: http://www.lazarus-ide.org/. Se você é usuário de Linux/FreeBSD, você pode obtê-lo a partir do repositório de software do seu sistema operacional.
  2. O último snapshot do PascalSCADA: Você viu o link de download no topo desta página?
  3. WinRAR ou outro aplicativo que consiga descompactar arquivos .bz2.

##### Instalando…

Bem, a primeira coisa a fazer é instalar o Lazarus. Eu não quero explicar como instalar o Lazarus em qualquer sistema operacional/plataforma que existe neste mundo, porque instalar o Lazarus é uma tarefa muito simples, tanto em Windows (Com o assistente de instalação com botões “Next”, “Next” e “Finish”) quanto em Linux (Aqui com Linux Mint 17.3: sudo apt-get install…). Ao final do processo instalação do Lazarus, você deve abri-lo e ver algo semelhante a isto:

[![Instalação limpa do Lazarus](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051-300x167.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051.png>)Instalação limpa do Lazarus

Se o seu Lazarus recém instalado se parece com o da imagem acima você está no caminho certo. O próximo passo é instalar o pacote do PascalSCADA. Por favor, esqueça do Lazarus por um momento. Vá para a sua pasta de Downloads e encontre o arquivo do PascalSCADA recém e descompacte-o em uma pasta de sua escolha.

Depois de extrair o arquivo, volte para o Lazarus e acesse o menu mostrado abaixo:

[![Instalando um novo pacote no Lazarus](http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338-300x165.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338.png>)Instalando um novo pacote no Lazarus

Clique neste menu e um diálogo para abrir arquivos será exibido:

[![Encontrando a instalação do PascalSCADA no sistema de arquivos](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052-300x165.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052.png>)Encontrando a instalação do PascalSCADA no sistema de arquivos

Navegue até a pasta onde você descompactou os arquivos do PascalSCADA. Nesta pasta deverá existir os seguintes arquivos:

  * pascalscada_common.lpk
  * pascalscada_full.lpk
  * pascalscada.lpk
  * pascalscada_db.lpk
  * pascalscada_hmi.lpk

Se você não vê algum destes arquivos, verifique o arquivo baixado, paths, etc… Se tudo está OK, sua tela deverá ser parecida com esta:

[![Encontrando a instalação do PascalSCADA no sistema de arquivos \(vista completa\)](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053-300x166.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053.png>)Encontrando a instalação do PascalSCADA no sistema de arquivos (vista completa)

Selecione o arquivo “pascalscada_full.lpk” e abra-o. A janela do pacote pascalscada_full será mostrada:

[![Pacote do PascalSCADA para instalação completa aberto](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054-300x165.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054.png>)Pacote do PascalSCADA para instalação completa aberto

A última etapa é instalar o pacote na sua IDE. Para fazer isto, clique no botão “Usar &gt;&gt;” e em seguida no menu “Instalar”, como mostrado abaixo:

[![Instalando o PascalSCADA na IDE Lazarus](http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819-300x167.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819.png>)Instalando o PascalSCADA na IDE Lazarus

O pacote pascalscada_full é um metapacote, usado somente para fazer a instalação completa do PascalSCADA na sua IDE Lazarus. Por causa deste motivo, você será aborrecido com algumas mensagens:

[![Metapacote do PascalSCADA: necessário para simplificar a instalação completa](http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055-300x123.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055.png>)Metapacote do PascalSCADA: necessário para simplificar a instalação completa

Clique no botão “Install it, I like the fat” para continuar a instalação. A próxima tela irá mostrar uma lista com todos os pacotes que serão instalados durante o processo na sua IDE, como mostrado abaixo:

[![Lista dos pacotes adicionais que serão instalados](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056-300x167.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056.png>)Lista dos pacotes adicionais que serão instalados

Uma imagem mais detalhada:

[![Lista dos pacotes adicionais que serão instalados](http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057-300x204.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057.png>)Lista dos pacotes adicionais que serão instalados

Clique no botão OK para continuar a instalação e chegar na pergunta final sobre o processo de recompilação da IDE, como pode ser vista abaixo:

[![Confirmando a recompilação do Lazarus](http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058-300x102.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058.png>)Confirmando a recompilação do Lazarus

Para finalizar, clique no botão “Sim”. O processo de instalação será iniciado e sua IDE será recompilada. Ao final do processo de recompilação, o Lazarus será reiniciado. **Se você tem um projeto com alterações que não foram salvas, tenha cuidado!!**

Após reiniciar a IDE, dependendo da sua versão do PascalSCADA, você deverá encontrar as seguintes paletas de componentes no seu Lazarus:

[![PascalSCADA instalado: guia de portas de comunicação](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059.png>)PascalSCADA instalado: guia de portas de comunicação [![PascalSCADA instalado: guia com os protocolos de comunicação](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060.png>)PascalSCADA instalado: guia com os protocolos de comunicação [![PascalSCADA instalado: guia com os componentes de utilidades](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061.png>)PascalSCADA instalado: guia com os componentes de utilidades [![PascalSCADA instalado: guia com todos os tags](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062.png>)PascalSCADA instalado: guia com todos os tags [![PascalSCADA instalado: guia com todos os controles de tela](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063.png>)PascalSCADA instalado: guia com todos os controles de tela [![PascalSCADA instalado: guia com os controles de gerenciamento de usuários e segurança](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064.png>)PascalSCADA instalado: guia com os controles de gerenciamento de usuários e segurança [![PascalSCADA instalado: guia com os componentes de banco de dados](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065.png>)PascalSCADA instalado: guia com os componentes de banco de dados [![PascalSCADA instalado: guia com os componentes herdados do FreePascal](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066.png>)PascalSCADA instalado: guia com os componentes herdados do FreePascal

Se você consegue ver estas paletas, parabéns, você conseguiu instalar com sucesso o PascalSCADA na sua IDE Lazarus.
