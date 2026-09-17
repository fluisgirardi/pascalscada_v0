Depois de mais de 10 anos sem publicar praticamente nada por aqui, um recado rápido: o PascalSCADA está vivo. Não só vivo — nas últimas semanas o projeto recebeu mais atenção do que em qualquer período recente, então vale um resumo do que mudou.

##### Dois drivers de protocolo novos

* **S7CommPlus** — o protocolo nativo que o TIA Portal usa para falar com CLPs **S7-1200 e S7-1500**. Diferente do driver ISOTCP (que já existia), o S7CommPlus não exige desligar o acesso otimizado dos blocos de dados: você endereça o tag pelo **nome simbólico** (`DB4.Var1`, `MArea.Clock_10Hz`), como no próprio TIA. Tem suporte a usuário/senha para CPUs com proteção de acesso.
* **EtherNet/IP (CIP)** — driver novo para CLPs **Rockwell/Allen-Bradley** da família Logix (ControlLogix, CompactLogix…), também com endereçamento simbólico pelo nome do tag. Já vem com assistente de tags (Tag Builder) próprio.

##### Uma limpeza grande, com testes de verdade

Esta é a parte menos vistosa e mais importante: o PascalSCADA nunca teve uma suíte de testes automatizados de verdade. Isso mudou. Praticamente toda a biblioteca — portas de comunicação, os drivers de protocolo (Modbus RTU/TCP, Melsec, S7/ISOTCP, S7CommPlus, EtherNet/IP, West ASCII, i-Box), a camada de tags e a biblioteca de controles HMI — ganhou testes unitários, e caçar cada teste destravou uma pilha de defeitos que estavam sentados no código, alguns há anos:

* leituras Modbus que deixavam a porta travada;
* o driver ISOTCP que ficava esperando para sempre um CLP que não respondia;
* escritas do West ASCII que perdiam a grandeza do valor;
* vazamentos de memória em pontos centrais — a thread de scan, os loggers de eventos e alarmes, o gerenciador de usuários central, o servidor de mutex de rede;
* o teclado virtual no Windows, onde o `-` e o `.` do teclado numérico saíam como letra ou lixo;
* um `THMICheckBox` que às vezes escrevia duas vezes no tag por uma mudança só;
* o selo de segurança que, num caso específico, desabilitava qualquer controle criado em tempo de execução.

A lista real é bem mais longa — dá para conferir no histórico de commits do [repositório no GitHub](https://github.com/fluisgirardi/pascalscada_v0).

##### Compilando e testando em cinco plataformas

O projeto agora tem integração contínua rodando os testes em Windows 32/64, Linux 32/64 e FreeBSD 64 a cada mudança — e testes também em ARM (ARMHF/AARCH64). Isso já pegou diferenças sutis entre plataformas que passariam despercebidas antes (um `TCriticalSection` com sinalização diferente entre sistemas, caminhos de dispositivo seriais específicos do Unix, entre outras).

##### A documentação também levou uma geral

O site estava com várias páginas de "em construção" desde sempre, e outras bem desatualizadas. Reescrevemos boa parte: [Tags](/pb/tags/), [Protocolo S7 sobre ISOTCP](/pb/s7-protocol-over-isotcp/), [Mitsubishi Melsec TCP](/pb/mitsubishi-melsec-tcp/), [Biblioteca de controles HMI](/pb/hmi-control-library-hcl/), [Sistema de segurança](/pb/security-system/), [Processadores de escala](/pb/scale-processors/), [HMIDBConnection](/pb/hmidbconnection/), a visão geral de [drivers de protocolo](/pb/protocol-drivers/), e um tutorial novo, [Escrevendo um driver de protocolo](/pb/writing-a-protocol-driver/), para quem quiser adicionar suporte a outro equipamento. A página de [instalação](/pb/how-install-pascalscada/) também foi atualizada para o fluxo atual (GitHub + Lazarus atual, sem o ZeosLib).

Hoje, para instalar tudo isso, o caminho é pegar o código direto do [GitHub](https://github.com/fluisgirardi/pascalscada_v0) (veja [Como instalar](/pb/how-install-pascalscada/)) — o pacote publicado no *Online Package Manager* do Lazarus ainda é a versão 0.7.7, de 2021. Isso deve mudar nas **próximas semanas**: a ideia é publicar uma versão nova no OPM já com os dois drivers novos e toda essa limpeza, para quem prefere instalar direto pela IDE sem clonar repositório.

Se você usa o PascalSCADA, testou algo que quebrou ou tem uma sugestão, abra uma issue no [GitHub](https://github.com/fluisgirardi/pascalscada_v0) — é para lá que o projeto se mudou, e é lá que a conversa acontece agora.
