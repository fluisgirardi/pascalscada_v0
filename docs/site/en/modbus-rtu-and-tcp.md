The following instructions are for both Modbus RTU (TModBusRTUDriver class) and Modbus TCP (TModBusTCPDriver class). To configure a protocol+tag connections you have to do:

  1. Insert a communication port appropriate for the chosen Modbus protocol: 
     * [TTCP_UDPPort](</communication-ports/#TTCP_UDPPort>) for Modbus TCP;
     * [TSerialPortDriver](</communication-ports/#TSerialPort>) for Modbus RTU;
  2. Insert the chosen protocol component;
  3. Connect the protocol to the port, through the “CommunicationPort” property;
  4. Insert the tags manually or through the “Tag builder” tool
  5. If you insert the tags manually, set the properties as described below;
  6. Connect the tag to the protocol driver through the **ProtocolDriver** property present in each tag.

Both protocol classes support the following tag classes:

  * TPLCTagNumber![plctagnumber](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png)
  * TPLCBlock![plcblock](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png)
  * TPLCStruct![plcstruct](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png)
  * TPLCString![plcstring](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png)

To configure a tag to use Modbus, you must configure the following tag properties:

  * **PLCStation** : Modbus equipment address. **For Modbus TCP, this property has its value set to 1, it usually depends on the Modbus Server implementation**.
  * **MemAddress** : Address of the input/output/register that you want to read/write. **The addresses start from zero, always. Do not use the address notation 1xxxxx, 2xxxxx, 3xxxxx, 4xxxxx, as it is not supported.**
  * **MemReadFuntion** : Function that will be used to read the tag. See table below.
  * **MemWriteFuntion** : Function that will be used to write tag values. See table below.

For the MemReadFunction and MemWriteFunction properties the following values are accepted according to the desired memory area:

<table class="table_tag" style="height: 252px;">
<tbody>
<tr>
<th>DESIRED AREA</th>
<th>MemReadFunction</th>
<th>MemWriteFunction</th>
</tr>
<tr>
<td>Digital inputs</td>
<td>2</td>
<td>0</td>
</tr>
<tr>
<td>Coils (digital outputs)</td>
<td>1</td>
<td>
<p>5 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/>)</p>
<p>15 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/> <span>TPLCBlock</span><img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/> <span>TPLCStruct</span><img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/> <span>TPLCString</span><img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/><span>)</span></p></td>
</tr>
<tr>
<td>Registers</td>
<td>3</td>
<td>
<p>6 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/>)</p>
<p>16 (TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/> <span>TPLCBlock</span><img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/> <span>TPLCStruct</span><img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/> <span>TPLCString</span><img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/><span>)</span></p></td>
</tr>
<tr>
<td>Analog registers (analog inputs)</td>
<td>4</td>
<td>0</td>
</tr>
<tr>
<td>Device status</td>
<td>7</td>
<td>0</td>
</tr>
</tbody>
</table>

**You need to know the ModBus functions your device supports.**
