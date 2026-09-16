As instruções a seguir servem tanto para Modbus RTU (classe TModBusRTUDriver) quanto para Modbus TCP (classe TModBusTCPDriver). Para configurar um tag+protocolo Modbus você deve:

  1. Inserir uma porta de comunicação apropriada para o protocolo Modbus escolhido: 
     * [TTCP_UDPPort](</communication-ports/#TTCP_UDPPort>) para Modbus TCP;
     * [TSerialPortDriver](</communication-ports/#TSerialPort>) para Modbus RTU;
  2. Inserir o componente de protocolo escolhido.
  3. Conectar o protocolo com a porta, através da propriedade CommunicationPort;
  4. Inserir os tags manualmente ou através da ferramenta “Tag builder”
  5. Caso inserir os tags manualmente, configurar as propriedades conforme descrito a seguir;
  6. Conectar o tag ao driver de protocolo através da propriedade **ProtocolDriver** presente em cada tag.

Ambas classes suportam os seguinte tipos de tags:

  * TPLCTagNumber![plctagnumber](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png)
  * TPLCBlock![plcblock](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png)
  * TPLCStruct![plcstruct](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png)
  * TPLCString![plcstring](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png)

Para configurar um tag para usar o ModBus, é necessário configurar as seguintes propriedades do tag:

  * **PLCStation** : Endereço do equipamento modbus. **Para Modbus TCP, esta propriedade tem seu valor fixado em 1, geralmente, depende da implementação do Modbus Server.**
  * **MemAddress** : Endereço da entrada/saida/registrador que se deseja lêr/escrever. Os endereços começam de zero, sempre. **Não use a notaçao de endereço 1xxxxx, 2xxxxx, 3xxxxx, 4xxxxx, pois ela não é suportada.**
  * **MemReadFuntion** : Função que será usada para ler o tag. Veja tabela abaixo.
  * **MemWriteFuntion** : Função que será usada para escrever valores do tag. Veja tabela abaixo.

Para as propriedades MemReadFunction e MemWriteFunction são aceitos os seguintes valores de acordo com a área de memória desejada:

<table class="table_tag" style="height: 252px;">
<tbody>
<tr>
<th>Área desejada</th>
<th>MemReadFunction</th>
<th>MemWriteFunction</th>
</tr>
<tr>
<td>Entradas digitais</td>
<td>2</td>
<td>0</td>
</tr>
<tr>
<td>Saidas digitais</td>
<td>1</td>
<td>
<p>5 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/>)</p>
<p>15 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/> <span>TPLCBlock</span><img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/> <span>TPLCStruct</span><img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/> <span>TPLCString</span><img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/><span>)</span></p></td>
</tr>
<tr>
<td>Registradores</td>
<td>3</td>
<td>
<p>6 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/>)</p>
<p>16 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/> <span>TPLCBlock</span><img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/> <span>TPLCStruct</span><img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/> <span>TPLCString</span><img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/><span>)</span></p></td>
</tr>
<tr>
<td>Entradas analógicas</td>
<td>4</td>
<td>0</td>
</tr>
<tr>
<td>Status equipamento</td>
<td>7</td>
<td>0</td>
</tr>
</tbody>
</table>

**É necessário que você conheça as funções ModBus que seu equipamento suporta.**
