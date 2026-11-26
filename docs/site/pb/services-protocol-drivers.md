##### Drivers de protocolo e integrações

Os drivers do PascalSCADA foram escritos pelo mesmo desenvolvedor que atende os projetos. Se o seu equipamento fala um protocolo que ainda não está na biblioteca, ou se a planta precisa conversar com outros sistemas, isso pode ser desenvolvido sob encomenda.

##### Protocolos já suportados

| Fabricante / padrão | Driver |
|---|---|
| Siemens S7-300/400/1200/1500 | [S7 sobre ISOTCP](/pb/s7-protocol-over-isotcp/) e [S7CommPlus](/pb/s7commplus/) (S7-1200/1500 com acesso otimizado, TLS e senha) |
| Rockwell / Allen-Bradley | EtherNet/IP (Logix) |
| Mitsubishi | [Melsec TCP](/pb/mitsubishi-melsec-tcp/) |
| Modbus | [Modbus RTU e TCP](/pb/modbus-rtu-and-tcp/) (inversores, balanças, medidores, controladores) |
| Delta, Inovance | drivers próprios sobre serial e TCP |
| West | [West ASCII](/pb/west-ascii/) |
| OPC UA | [cliente](/pb/opc-ua/) e servidor |
| MQTT | cliente MQTT 3.1.1 |

##### O que pode ser desenvolvido

* **Um driver novo** para um CLP, inversor, balança, leitor ou instrumento, a partir do manual do protocolo e de um equipamento para teste.
* **Melhorias num driver existente**: um recurso do protocolo que ainda não é atendido, desempenho, um modelo de CLP novo.
* **Integrações**: dados da planta para bancos de dados, MES, ERP ou nuvem por OPC UA, MQTT ou SQL; gateways entre protocolos.

Um driver feito sob encomenda pode ser **incorporado à biblioteca aberta**, e passa a ser mantido junto com ela, ou ficar **exclusivo do seu projeto**. Isso é combinado na proposta.

##### O que precisamos de você

O manual ou a especificação do protocolo, o modelo e o firmware do equipamento e, durante o desenvolvimento, acesso a um equipamento ou simulador (no local, por acesso remoto ou emprestado).

##### Exemplos relacionados

* `examples/laz_s7commplus_structitems_gui`
* `examples/laz_modbus_tcp_example`
* `examples/laz_opcua_server`
* `examples/laz_mqtt_console`

[Pedir um orçamento](/pb/services-request-a-quote/)
