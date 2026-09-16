##### Introdução

Drivers de protocolo são os componentes que interpretam a configuração dos tags ligados a eles, convertem isso num pacote de dados no formato do protocolo, enviam o pacote pela porta de comunicação, recebem a resposta, decodificam e atualizam o valor dos tags — ou fazem o caminho inverso, para escrita.

Ele é o segundo componente a ser inserido numa aplicação de aquisição de dados, logo após a [porta de comunicação](/pb/communication-ports/) e antes de todos os [tags](/pb/tags/) que usarão o driver de protocolo.

##### Protocolos suportados

<table>
<tbody>
<tr>
<td><strong>Classe do protocolo</strong></td>
<td><strong>Protocolo</strong></td>
<td><strong>Tipo de protocolo</strong></td>
<td><strong>Comunica com</strong></td>
<td><strong>Classe da porta necessária</strong></td>
<td><strong>Tags suportadas</strong></td>
<td><strong>Propriedades dos tags a configurar</strong></td>
<td><strong>Observações</strong></td>
</tr>
<tr>
<td>TModBusRTUDriver<br/>
<img alt="Seleção_092" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_092.png"/></td>
<td>Modbus RTU</td>
<td>1 componente de protocolo para muitas estações</td>
<td>Qualquer equipamento que suporte Modbus RTU (endereços de estação 1 a 247)</td>
<td>
<p>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></p>
</td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>PLCStation</p>
<p>MemReadFunction</p>
<p>MemWriteFunction</p>
<p>MemAddress</p>
<p>TagType</p></td>
<td>Blocos e estruturas usam por padrão até 125 registradores por bloco de leitura/escrita (propriedades <em>HoldingRegsMaxBlockSize</em>/<em>AnalogRegsMaxBlockSize</em> do driver) e até 2000 bits para entradas/saídas digitais (<em>InputsMaxBlockSize</em>/<em>OutputsMaxBlockSize</em>) — todas configuráveis. Veja <a href="/pb/modbus-rtu-and-tcp/">Modbus RTU e TCP</a>.</td>
</tr>
<tr>
<td>TModBusTCPDriver<br/>
<img alt="modbus_tcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/modbus_tcp.png"/></td>
<td>Modbus TCP</td>
<td>1 componente de protocolo para 1 ou mais equipamentos, sobre a mesma conexão</td>
<td>Qualquer equipamento que suporte Modbus TCP</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>PLCStation</p>
<p>MemReadFunction</p>
<p>MemWriteFunction</p>
<p>MemAddress</p>
<p>TagType</p></td>
<td>Mesmos limites de bloco do Modbus RTU. Veja <a href="/pb/modbus-rtu-and-tcp/">Modbus RTU e TCP</a>.</td>
</tr>
<tr>
<td>TWestASCIIDriver<br/>
<img alt="west" src="http://www.pascalscada.com/wp-content/uploads/2016/08/west.png"/></td>
<td>West ASCII</td>
<td>1 componente de protocolo para muitas estações</td>
<td>Controladores West n6100/P6100</td>
<td>
<p>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></p>
</td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>
<p>PLCStation (1 a 99)</p>
<p>MemAddress (0 a 27 — o parâmetro West)</p></td>
<td>Todo registrador do West é ponto flutuante de 32 bits; o driver ignora o <code>TagType</code> configurado e sempre entrega um float — deixe em <code>pttDefault</code>. Suporta leitura e escrita. Tem assistente de tags próprio.</td>
</tr>
<tr>
<td>TIBoxDriver<br/>
<img alt="ibox" src="http://www.pascalscada.com/wp-content/uploads/2016/08/ibox.png"/></td>
<td>Thermo King i-Box</td>
<td>1 componente de protocolo para muitas estações</td>
<td>Controladores de frio Thermo King i-Box</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>
<p>PLCStation (0 a 255)</p>
<p>MemAddress — só os registradores (PID) 0, 96, 168, 200 a 205 e 247 são aceitos</p></td>
<td>Somente leitura: o i-Box não aceita comandos de escrita, qualquer tentativa retorna erro. Os valores já chegam prontos (percentual, horas, décimos de grau) — o <code>TagType</code> não altera o resultado.</td>
</tr>
<tr>
<td>TISOTCPDriver<br/>
<img alt="isotcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/isotcp.png"/></td>
<td>Siemens S7 (S7comm) sobre ISOTCP</td>
<td>1 componente de protocolo para 1 CLP</td>
<td>
<p>S7-200 via CP 243-1</p>
<p>S7-300 / S7-400 (Ethernet)</p>
<p>LOGO! 7 e 8 (Ethernet)</p>
<p>S7-1200 / S7-1500 (Ethernet, com PUT/GET habilitado e blocos sem acesso otimizado)</p>
<p>Simuladores/gateways compatíveis com S7comm (PLCSIM+NetToPLCSim, Snap7 server)</p></td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>PLCRack, PLCSlot, PLCStation — no driver, não no tag</p>
<p>MemReadFunction (área)</p>
<p>MemFile_DB (quando a área é DB)</p>
<p>MemAddress</p>
<p>TagType</p>
<p>SwapBytes/SwapWords/SwapDWords conforme o tipo</p></td>
<td>Não fala o protocolo nativo do S7-1200/1500 (S7CommPlus) nem faz acesso simbólico — veja <a href="/pb/s7-protocol-over-isotcp/">S7 Protocol over ISOTCP</a> para a tabela completa de áreas e tipos.</td>
</tr>
<tr>
<td>TMelsecTCPDriver<br/>
<img alt="melsec" src="http://www.pascalscada.com/wp-content/uploads/2016/08/melsec.png"/></td>
<td>Mitsubishi MC Protocol (3E frame, binário) sobre TCP</td>
<td>1 componente de protocolo para 1 CLP</td>
<td>Mitsubishi série Q, L e iQ-F/iQ-R com porta Ethernet ou módulo QJ71E71/LJ71E71, configurados para MC Protocol/SLMP binário</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemReadFunction / MemWriteFunction (código do dispositivo: M, X, Y, D…)</p>
<p>MemAddress (decimal — atenção: X/Y são numerados em hexadecimal no CLP)</p></td>
<td>Timers, contadores, registradores de link e as áreas SB/DX/DY ainda não são suportados. Veja <a href="/pb/mitsubishi-melsec-tcp/">Mitsubishi Melsec TCP</a>.</td>
</tr>
<tr>
<td><strong>TS7CommPlusDriver</strong></td>
<td>Siemens S7CommPlus</td>
<td>1 componente de protocolo para 1 CLP</td>
<td>S7-1200 e S7-1500 (protocolo nativo usado pelo TIA Portal, com blocos em acesso otimizado — não precisa desligá-lo como no ISOTCP)</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>LongAddress — caminho simbólico do tag no CLP (ex.: <code>DB4.Var1</code>, <code>MArea.Clock_10Hz</code>)</p></td>
<td>Endereça pelo nome do tag no programa do CLP, não por offset de memória. Página dedicada em preparação.</td>
</tr>
<tr>
<td><strong>TLGXDriver</strong></td>
<td>EtherNet/IP (CIP), Rockwell/Allen-Bradley</td>
<td>1 componente de protocolo para 1 CLP</td>
<td>CLPs Rockwell/Allen-Bradley da família Logix (ControlLogix, CompactLogix…) com porta EtherNet/IP</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>LongAddress — nome do tag Logix (ex.: <code>Motor1.Speed</code>, <code>MinhaString.DATA</code> para STRING)</p></td>
<td>Endereçamento simbólico, como no TIA Portal/Studio 5000: não há offset de memória a configurar. Página dedicada em preparação.</td>
</tr>
</tbody>
</table>

##### Estrutura básica de uma aplicação de aquisição de dados

A imagem abaixo resume a cadeia porta → driver → tags → controles, comum a todos os protocolos da tabela acima:

[![Estrutura básica de uma aplicação de aquisição de dados](http://www.pascalscada.com/wp-content/uploads/2016/08/g8441-300x89.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/g8441.png>)Estrutura básica de uma aplicação de aquisição de dados

##### Exemplos relacionados

* `examples/laz_modbus_tcp_example`, `examples/laz_modbus_rtu_scanner` — Modbus TCP e RTU.
* `examples/laz_isotcp_demonstration` — Siemens S7 sobre ISOTCP.
* `examples/laz_west6100_plus_demonstration` — West ASCII.
* `examples/laz_ibox` — Thermo King i-Box.
