##### Introdução

Portas de comunicação são o meio utilizado pelos drivers de protocolo para enviar e receber dados. Uma porta de comunicação pode ser usada por um ou mais drivers de protocolo, simultaneamente.

As portas de comunicação são separadas da implementação dos protocolos para permitir uma maior flexibilidade do sistema. Uma aplicação dessa real que pode ser feita com essa arquitetura, é associar uma porta TTCP_UDPPort com um COMServer (equipamento que nada mais é que um gateway RS-232/485/422 para TCP/IP), para por exemplo, comunicar com algum equipamento Modbus RTU a muitos quilômetros de distância sobre internet. Outra vantagem deste design é colocar dois ou mais protocolos usando a mesma porta de comunicação simultaneamente na mesma aplicação. Isto é possível pelo fato de todas portas contar com um mecanismo que só permite que um protocolo use a porta durante a operação de comunicação. Ou seja, uma aplicação pode comunicar com vários equipamentos, estes equipamentos com diferentes protocolos de comunicação e todos conectados na mesma rede RS-485. É possível, apesar de não ser recomendado.

Todas as portas existentes no PascalSCADA descendem da classe TCommPort, que implementa os métodos base para abrir e fechar a porta, ler e escrever dados e limpar os buffers de leitura e escrita em caso de falha de comunicação. Mas lógico que estes métodos precisam ser especializados caso uma nova porta de comunicação esteja sendo implementada.

##### ![Seleção_090](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png)TSerialPort

A classe TSerialPort, derivada da classe TCommPort, foi criada para permitir os protocolos trocar dados sobre portas seriais, independente do meio físico utilizado pela porta.

Em sistemas operacionais Windows, esta classe necessita uma porta serial com o nome entre COM1 e COM255.

Rodando sobre Linux, qualquer dispositivo que o nome comece com tty&lt;número&gt;, ttyUSB&lt;número&gt; ou ttyACM&lt;número&gt; é válida.

On FreeBSD, any device qualquer dispositivo cujo o nome comece com cuad&lt;número&gt; é aceita pela classe.

**Tanto em Linux como FreeBSD (ou outro Unix) deverá ser tomado o cuidado de adicionar o usuário que irá rodar a aplicação no grupo dono da porta serial, alem de dar permissão de leitura/escrita para o grupo sobre a porta serial desejada. O nome deste grupo varia conforme o sistema.**

Independente de sistema operacional, a classe de porta Serial conta com as seguintes propriedades que configuram o funcionamento da porta serial:

  * **COMPort:** define a porta serial a ser utilizada. Caso a aplicação seja configurada com uma porta e esta porta seja desconectada do sistema (por exemplo, um conversor USB &lt;-&gt; Serial) e a aplicação seja iniciada, uma exceção será gerada. Se esta exceção for gerada durante a carga (inicialização) da aplicação, sua aplicação não iniciará, mesmo com a propriedade Active = FALSE.
  * **Baudrate** : velocidade em bits por segundo que esta porta irá utilizar durante a comunicação. Velocidades entre 110 bps e 115200 bps são aceitas.
  * **DataBits** : Define o tamanho da palavra de dados que será trocada durante a comunicação serial, em bits. Os tamanhos aceitos são 7 ou 8 bits, sendo o padrão 8 bits. Alguns protocolos pedem 7 bits, como por exemplo o Modbus ASCII. Outros como o Modbus RTU, definem o tamanho da palavra de dados de 8 bits.
  * **StopBits** : Configura a quantidade de bits que será usada para marcar o início e o final de cada palavra transmitida na comunicação serial. O padrão é 1 stop bits.
  * **Paridade:** configura se a palavra irá contar com checagem de erros, e se em caso dela existir, se a checagem vai ser par ou impar.
  * **Timeout** : Define o tempo máximo que a porta serial irá aguardar pela leitura de uma certa quantia de dados. Este tempo é definido em milissegundos. Em casos de estouro do timeout, uma limpeza dos buffers de leitura e escrita é realizada.
  * **AcceptAnyPortName** : Esta propriedade, quando habilitada em um sistema Unix (Linux, FreeBSD, etc), faz com que qualquer nome de dispositivo seja aceito na propriedade COMPort. Muito útil quando alguns drivers criam nomes de dispositivos que não são aceitos pelo padrão descrito acima. No Windows ela não tem efeito.
  * **WriteReadDelay** : Atraso em milissegundos entre um comando de escrita e em seguida um leitura.
  * **Active:** Esta propriedade controla a abertura e fechamento da porta. A porta serial não deverá estar aberta por nenhum outro processo quando esta propriedade vai para true, caso contrário a propriedade Active irá votar para false. Em tempo de desenvolvimento, Active = true só realiza as verificações para abertura da porta mas não efetua a abertura da porta.

##### ![Seleção_091](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png)TTCP_UDPPort

A classe TTCP_UDPPort, derivada da classe TCommPort, foi criada para permitir os protocolos trocar dados sobre portas TCP (testado) ou UDP (necessita testes) sobre IPv4. independente do meio físico utilizado pela porta.

Esta classe de porta, não tem propriedades que dependentes de sistema operacional, sendo que as seguintes propriedades configuram seu funcionamento:

  * **Host** : Endereço IPv4 que a porta irá conectar. Nomes de host não são aceitos, somente endereços IP.
  * **Port** : Número da porta no host destino que a porta irá conectar. são aceitos qualquer número entre 1 e 65535.
  * **PortType** : Define se o socket que irá ser estabelecido irá ser sobre TCP ou UDP.
  * **ExclusiveDevice** : Caso true, evita que a porta seja aberta durante o tempo de desenvolvimento. Util quando o host destino tem um limite muito baixo de conexões disponíveis.
  * **EnableAutoReconnect** : Caso true e caso o socket for desconectado (problema no meio físico ou com o host) habilita o temporizador que irá ficar tentando reconectar o socket. Neste caso o socket com problemas é fechado e a propriedade Active permanece true.
  * **ReconnectRetryInterval** : Esta propriedade define o intervalo de tempo em milissegundos em que a porta tentará reconectar conexões perdidas. Esta propriedade só é valida caso EnableAutoReconnect e Active sejam true.
  * **Timeout** : Define o tempo máximo que a porta irá aguardar pela confirmação de escrita ou leitura de uma certa quantia de dados. Este tempo é definido em milissegundos. Caso alguma operação exceda este tempo, os buffers são limpos, a porta é verificada e em caso de alguma anormalidade, o socket será descartado e se EnableAutoReconnect é true uma tentativa de reconectar o socket será feito.
  * **Active:** Esta propriedade controla a abertura e fechamento do socket. Em tempo de desenvolvimento, Active = true irá abrir o socket e realizar comunicações com seu dispositivo, exceto se **ExclusiveDevice** for setado para true.

##### Exemplos

Abaixo estão listados exemplos de como trocar dados usando as portas de comunicação do PascalSCADA diretamente via código, ou seja sem usar, sem usar um driver de protocolo. Como todas as portas são descendentes da classe TCommPort, o exemplo abaixo também vale se quiser implementar algo usando uma porta TTCP_UDPPort.

  * Genius game, interface de ranqueamento: <https://sourceforge.net/p/pascalscada/code/HEAD/tree/trunk/examples/laz_serialport_genius/>
