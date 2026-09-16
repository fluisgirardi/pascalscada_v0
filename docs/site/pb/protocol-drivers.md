##### Introdução

Drivers de protocolo são componentes que interpretam as configurações dos tags ligados e ele, converte em pacotes de dados, envia este pacote para a porta de comunicação, pega resposta recebida pela porta de comunicação, decodifica a resposta e atualiza os valores dos tags.

Ele é o segundo componente a ser inserido numa aplicação de aquisição de dados, logo após a [porta de comunicação](<http://www.pascalscada.com/communication-ports/>) e antes de todos os [tags](<http://www.pascalscada.com/tags/>) que usarão o driver de protocolo.

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
<td><strong>Propriedades dos tags a configurar </strong></td>
<td><strong>Observações</strong></td>
</tr>
<tr>
<td>TModBusRTUDriver<br/>
<img alt="Seleção_092" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_092.png"/></td>
<td>Modbus RTU</td>
<td>1 componente de protocolo para muitos equipamentos</td>
<td>Qualquer equipamento que suporte Modbus RTU</td>
<td>
<p>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></p>
</td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemAddress</p>
<p>MemReadFuncion</p>
<p>MemWriteFunction</p>
<p>PLCStation</p>
<p>TagType</p></td>
<td>Tags TPLCStruct e TPLCBlock não podem exceder 125 words de tamanho quando escrevendo valores.</td>
</tr>
<tr>
<td>TModBusTCPDriver<br/>
<img alt="modbus_tcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/modbus_tcp.png"/></td>
<td>Modbus TCP</td>
<td>1 componente de protocolo para 1 equipamento</td>
<td>Qualquer equipamento que suporte Modbus TCP</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemAddress</p>
<p>MemReadFuncion</p>
<p>MemWriteFunction</p>
<p>PLCStation</p>
<p>TagType</p></td>
<td>Tags TPLCStruct e TPLCBlock não podem exceder 125 words de tamanho quando escrevendo valores.</td>
</tr>
<tr>
<td>TWestASCIIDriver<br/>
<img alt="west" src="http://www.pascalscada.com/wp-content/uploads/2016/08/west.png"/></td>
<td>West ASCII</td>
<td>1 componente de protocolo para muitos equipamentos</td>
<td>West P6100</td>
<td>
<p>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></p>
</td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td></td>
<td>TagType deve ser sempre pttDefault</td>
</tr>
<tr>
<td>TIBoxDriver<br/>
<img alt="ibox" src="http://www.pascalscada.com/wp-content/uploads/2016/08/ibox.png"/></td>
<td>Thermo King i-Box</td>
<td>1 componente de protocolo para muitos equipamentos</td>
<td>Thermo King i-Box</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>???</td>
<td></td>
</tr>
<tr>
<td>TISOTCPDriver<br/>
<img alt="isotcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/isotcp.png"/></td>
<td>Siemens S7 Protocol sobre  ISOTCP</td>
<td>1 componente de protocolo para 1 equipamento</td>
<td>
<p>S7-200 via CP-243</p>
<p>S7-300 (somente ethernet)</p>
<p>S7-400 (somente ethernet)</p>
<p>Logo 7 e 8 (somente ethernet)</p>
<p>S7-1200 (somente ethernet, sem acesso simbólico)</p>
<p>S7-1500 (somente ethernet, sem acesso simbólico)</p>
<p>WinAC 4.6</p>
<p>VIPA ?</p>
<p>Snap7 ?</p></td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemAddress</p>
<p>MemReadFuncion</p>
<p>MemFileDB (se endereçando Datablocks)</p>
<p>SwapBytes (Caso endereçando words, dwords e reais)</p>
<p>SwapBytes (Caso endereçando dwords e reais)</p>
<p>TagType</p></td>
<td></td>
</tr>
<tr>
<td>TMelsecTCPDriver<br/>
<img alt="melsec" src="http://www.pascalscada.com/wp-content/uploads/2016/08/melsec.png"/></td>
<td>Mitsubishi Melsec</td>
<td>1 componente de protocolo para 1 equipamento</td>
<td>?</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>?</td>
<td>?</td>
<td>Esclarecer com Marcelo</td>
</tr>
</tbody>
</table>

##### Estrutura básica de uma aplicação de aquisição de dados

[![Estrutura básica de uma aplicação de aquisição de dados](http://www.pascalscada.com/wp-content/uploads/2016/08/g8441-300x89.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/g8441.png>)Estrutura básica de uma aplicação de aquisição de dados
