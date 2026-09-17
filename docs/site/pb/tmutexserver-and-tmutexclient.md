##### Introdução

Em primeiro lugar, sinta-se livre para enviar correções de qualquer tipo, incluindo reformulações de texto.

Este artigo tem como objetivo principal, ensinar a usar e a entender os cenários onde as classes TMutexServer e TMutexClient devem ser usadas. Um resumo das propriedades de cada um será abordado a seguir.

##### O que são as classes TMutexServer e TMutexClient

As classes TMutexServer e TMutexClient são em resumo um [mutex, região/seção crítica](<https://pt.wikipedia.org/wiki/Regi%C3%A3o_cr%C3%ADtica>), usados para fazer a proteção de recursos exclusivos e o sincronismo de processos. Porém o detalhe que mais chama atenção nas classes TMutexServer e TMutexClient é que eles são feitos para funcionar em rede, ou seja, são feitos para sincronizar processos que estão rodando em diferentes computadores. Neste cenário, precisamos de um servidor, arbitrando quem tem a vez sobre o recurso, e dois ou mais sistemas rodando em computadores diferentes (ou duas ou mais instâncias da mesma aplicação rodando em uma ou mais estações) que concorrem a um recurso comum. A imagem abaixo ilustra esta situação:

(imagem)

Algo muito importante a se notar nestes componentes é que eles são feitos para serem chamados exclusivamente via código. Portanto, não há maneiras gráficas (ou fáceis) de proteger seus recursos que necessitam acesso exclusivo.

Por fim, para dar acesso exclusivo a um recurso ou fazer o sincronismo entre vários processos que estão rodando diferentes estações, é necessário fazer o uso da classe TMutexClient em cada uma das aplicações que concorrentes. Portanto, se uma aplicação não fizer o uso desta classe, comportamento inesperados podem acontecer.

##### Quando devo usar as classes TMutexServer e TMutexClient

É recomendado usar as classes TMutexServer e TMutexClient nas seguintes situações:

  * Quando a integridade de várias escritas devem ser garantidas, evitando que uma aplicação sobrescreva os dados enviados por outra.
  * Quando se deseja acesso exclusivo a um recurso (CLP) quando determinadas ações estejam sendo executadas em um ambiente com múltiplas aplicações.

##### Onde encontro as classes TMutexServer e TMutexClient

Ambas as classes são disponibilizadas quando é instalado o pacote pascalscada.lpk.

Uma vez instalado este pacote, dois itens na paleta “PascalSCADA utils” serão exibidos, conforme a figura abaixo:

[![Captura de tela de 2016-11-29 19:18:18](http://www.pascalscada.com/wp-content/uploads/2016/08/Captura-de-tela-de-2016-11-29-191818.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Captura-de-tela-de-2016-11-29-191818.png>)

##### Como configurar uma aplicação para usar as classes TMutexServer e TMutexClient

Esta configuração é feita em duas etapas, uma do lado servidor e outra no lado da(s) aplicação(ões) cliente(s)

###### No servidor:

Na aplicação ou serviço que irá arbitrar o sincronismo/acesso ao recurso são necessárias alguns poucos passos:

  1. Insira no seu projeto uma instancia da classe TMutexServer para cada recurso com acesso exclusivo que deseja proteger;
  2. Configure a porta TCP/IP que este servidor de mutex irá atender as requisições clientes. Este número não pode repetir caso várias instâncias da classe TMutexServer estejam inseridas e habilitadas na sua aplicação/serviço. A porta informada também deve estar disponível, pois a classe TMutexServer irá abri-lá para aceitar conexões. Portanto, modificações no seu firewall podem ser necessárias.  
[![Seleção_447](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_447.png)  
](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_447.png>)
  3. Ative o Mutex server, alterando a propriedade Active para true:[![Seleção_448](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_448.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_448.png>)
  4. Rode sua aplicação/serviço. Se estiver fazendo isto em um sistema operacional Windows, a primeira vez que rodar seu serviço, uma pergunta do firewall querendo saber se você autoriza a abertura da porta será exibida. Em sistemas derivados do Unix (e no Windows também), basta usar o utilitário _netstat_ para verificar quais portas estão abertas:  
[  
](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_449.png>)[![Seleção_449](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_449.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_449.png>)

###### Nas aplicações clientes

A configuração das aplicações clientes se resumem a duas etapas básicas, sendo elas a configuração da conexão com o servidor mutex e após isso, a alteração do código para verificação de acesso exclusivo ao recurso.

A configuração da conexão do mutex client é muito semelhante a configuração do mutex server, sendo necessário configurar duas propriedades extras: a primeira é o endereço IPv4 do mutex server. **Atenção: somente endereços IPv4 são aceitos! Não tente inserir endereços IPv6 ou nome de hosts!** A segunda configuração que precisa ser feita é dizer qual é o comportamento do Mutex Client caso este esteja tendo dificuldades em conectar no mutex server, sendo a configuração de negar acesso o seu padrão. Acompanhe as imagens que seguem:

  1. Insira na sua aplicação uma instância da classe TMutexClient. Neste exemplo, serão inseridas duas instâncias da classe para que seja possível reproduzir uma situação de concorrência.[![Seleção_451](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_451.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_451.png>)
  2. Configure o endereço dos mutex clients para que estes encontrem o mutex server, que é quem irá arbitrar quem terá acesso ao recurso exclusivo. **Lembre-se: somente IPv4 é aceito!**  
[![Seleção_452](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_452.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_452.png>)
  3. Informe a porta do mutex server para os clientes. No nosso exemplo mantivemos o padrão.  
[![Seleção_453](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_453.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_453.png>)
  4. Configure a propriedade _DefaultBehavior_ , pois é ela quem configura o comportamento padrão quando o mutex client não consegue conexão com o mutex server. Esta configuração estando com o valor _False_ , irá negar acesso ao recurso caso o mutex client esteja desconectado.Caso o esta propriedade seja _True e_ o cliente esteja desconectado do mutex server, uma tentativa de acesso feita nesse momento resultará na liberação de acesso ao recurso. **Use com cuidado!**
  5. Por último, basta ativar a conexão para que os clientes conectem no mutex server configurado.  
[![Seleção_454](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_454.png)  
](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_454.png>)
  6. Rode a aplicação e verifique se as conexões com o mutex server foram estabelecidas usando o netstat.  
[![Seleção_456](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_456.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_456.png>)

Irei neste exemplo, adicionar dois panels quer irão funcionar como botões (sim Panels, pelo simples fato que as classes de botões nativas não aceitam o valor da propriedade color), que no primeiro clique irão solicitar o recurso ao mutex server e no clique seguinte irá liberar acesso. Caso o botão clicado obtenha o recurso, ele irá trocar a cor do botão. [![Seleção_459](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_459.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_459.png>)[  
](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_457.png>)[  
](<http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_455.png>)

Dê dois cliques no primeiro botão e insira o seguinte código no evento OnClick:

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if Button1.Color = clLime then begin
    if MutexClient1.Leave then
      Button1.Color := clDefault;
  end else begin
    if MutexClient1.TryEnter then begin
      Button1.Color := clLime;
    end;
  end;
end;
```

Repita o procedimento e insira este código no evento OnClick do objeto Button2:

```pascal
procedure TForm1.Button2Click(Sender: TObject);
begin
  if Button2.Color = clLime then begin
    if MutexClient2.Leave then
      Button2.Color := clDefault;
  end else begin
    if MutexClient2.TryEnter then begin
      Button2.Color := clLime;
    end;
  end;
end;
```

O video de funcionamento deste exemplo pode ser visto aqui:

[embedyt] http://www.youtube.com/watch?v=5XoYhBTjJ1c[/embedyt]

Como visto no acima, a class TMutexClient tem duas primitivas básicas que solicitam e liberam o recurso, que são identicas ao da classe TCriticalSection:

  * **TryEnter** : Função que retorna um valor booleano _true_ caso o acesso ao recurso foi autorizado (pelo mutex server ou pela propriedade _DefaultBehavior_) ou _false_ caso contrário. Esta função não retorna a fonte do valor (mutex server ou propriedade _DefaultBehavior_).
  * **TryEnter(out PickedTheDefaultBehavior: Boolean)** : Função que retorna um valor booleano _true_ caso o acesso ao recurso foi autorizado (pelo mutex server ou pela propriedade _DefaultBehavior_) ou _false_ caso contrário. Esta função retorna outro valor booleano _true_ caso o resultado da função tenha sido obtido da propriedade _DefaultBehavior_. Ou seja, caso o valor da variável passada para o parâmetro **PickedTheDefaultBehavior** retorne com **true** , **você está desconectado do mutex server!**
  * **Leave** : função que libera o recurso, caso o respectivo mutex client tenha acesso sobre ele. Retorna true toda a vez que o mutex client tem o recurso e faz a liberação do mesmo e false caso o contrário.

##### Problemas de conexão, deadlocks, e liberação automática do mutex server

O mutex server está programado para liberar o mutex interno, caso após alguns segundos, o mutex client não responder aos comandos de ping. As requisições de ping são enviadas a cada um segundo. Após o envio o mutex server aguarda durante um segundo a resposta do mutex client. Caso ocorra uma falha no mutex server enviando ou recebendo o ping, um contador de falhas é incrementado. Quando este contador atinge um valor maior ou igual a dez, é iniciado o procedimento de desligamento deste mutex client com problemas e caso ele tenha a posse do mutex, faz a devida liberação do mesmo.

No lado cliente, uma conexão perdida é recuperada toda a vez que uma chamada as funções TryEnter são feitas.

Deadlocks no mutex server só não podem ser evitados caso o cliente não faça a devida liberação do mutex (nunca chame a função Leave), seja por erro na lógica ou congelamento da aplicação.

Tanto o mutex server quanto o mutex client iniciam threads para poder enviar e responder os comandos de ping sem sofrer ou causar interferência no loop principal da aplicação.

##### Aplicação exemplo

Este exemplo está disponível em:  
<https://sourceforge.net/p/pascalscada/code/HEAD/tree/trunk/examples/laz_networkmutex/>

Há também um outro exemplo, mais dinâmico:  
<https://sourceforge.net/p/pascalscada/code/HEAD/tree/trunk/examples/both_networkmutex/>
