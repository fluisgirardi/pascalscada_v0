##### Introduction

Protocol drivers are the components that interprets the settings of linked tags, converts into data packets, write the packet on the communication port, takes the response received by communication port, decodes the response and update the tags.

It should be the second component inserted into a data acquisition application, after the [communication port](<http://www.pascalscada.com/communication-ports/>) component and before all the [tags](<http://www.pascalscada.com/tags/>) that will use the protocol driver.

##### Supported protocols

<table>
<tbody>
<tr>
<td><strong>Protocol class</strong></td>
<td><strong>Protocol</strong></td>
<td><strong>Protocol type</strong></td>
<td><strong>Communicates with</strong></td>
<td><strong>Communication port class needed</strong></td>
<td><strong>Supported tags</strong></td>
<td><strong>Tag properties that should configured</strong></td>
<td><strong>Observations</strong></td>
</tr>
<tr>
<td>TModBusRTUDriver<img alt="Seleção_092" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_092.png"/></td>
<td>Modbus RTU</td>
<td>1 protocol instance to several devices</td>
<td>Any device that supports Modbus RTU</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
<span>TPLCString</span><img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemAddress</p>
<p>MemReadFuncion</p>
<p>MemWriteFunction</p>
<p>PLCStation</p>
<p>TagType</p></td>
<td>Tags TPLCStruct and TPLCBlock should not exceed 125 words of size when writing values to your device.</td>
</tr>
<tr>
<td>
<p>TModBusTCPDriver<br/>
<img alt="modbus_tcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/modbus_tcp.png"/></p>
</td>
<td>Modbus TCP</td>
<td>1 protocol instance to one device</td>
<td>Any device that supports Modbus TCP</td>
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
<td>Tags TPLCStruct and TPLCBlock should not exceed 125 words of size when writing values to your device.</td>
</tr>
<tr>
<td>TWestASCIIDriver<br/>
<img alt="west" src="http://www.pascalscada.com/wp-content/uploads/2016/08/west.png"/></td>
<td>West ASCII</td>
<td>1 protocol instance to several devices</td>
<td>West P6100</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>???</td>
<td>TagType should be always pttDefault</td>
</tr>
<tr>
<td>
<p>TIBoxDriver<br/>
<img alt="ibox" src="http://www.pascalscada.com/wp-content/uploads/2016/08/ibox.png"/></p>
</td>
<td>Thermo King i-Box</td>
<td>1 protocol instance to several devices</td>
<td>Thermo King i-Box</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>??</td>
<td></td>
</tr>
<tr>
<td>TISOTCPDriver<br/>
<img alt="isotcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/isotcp.png"/></td>
<td>Siemens S7 Protocol over ISOTCP</td>
<td>1 protocol instance to one device</td>
<td>S7-200 via CP-243<br/>
S7-300 (only over ethernet)<br/>
S7-400 (only over ethernet)<br/>
Logo 7 and 8 (only over ethernet)<br/>
S7-1200 (only over ethernet, without symbolic access)<br/>
S7-1500 (only over ethernet, without symbolic access)<br/>
WinAC 4.6 (only over ethernet)<br/>
VIPA ?<br/>
Snap 7 ??</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemAddress</p>
<p>MemReadFuncion</p>
<p>MemFileDB (if addressing datablocks)</p>
<p>SwapBytes (if addressing words, dwords, floats, qwords and doubles)</p>
<p>SwapWords (if addressing dwords, floats, qwords and double)</p>
<p>SwapDWords (if addressing qwords and double)</p>
<p>TagType</p></td>
<td></td>
</tr>
<tr>
<td>TMelsecTCPDriver<br/>
<img alt="melsec" src="http://www.pascalscada.com/wp-content/uploads/2016/08/melsec.png"/></td>
<td>Mitsubishi Melsec</td>
<td>1 protocol instance to one device</td>
<td>?</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>?</td>
<td>?</td>
<td>Clarify with Marcelo</td>
</tr>
</tbody>
</table>

##### Data acquisition application structure

[![Data acquisition application structure](http://www.pascalscada.com/wp-content/uploads/2016/08/g8619-300x93.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/g8619.png>)Data acquisition application structure
