##### Introduction

Protocol drivers are the components that interpret the configuration of the tags linked to them, turn that into a data packet in the protocol's format, send the packet through the communication port, receive the answer, decode it and update the tags' values — or the reverse path, for writes.

It is the second component to be inserted in a data-acquisition application, right after the [communication port](/communication-ports/) and before every [tag](/tags/) that will use the protocol driver.

##### Supported protocols

<table>
<tbody>
<tr>
<td><strong>Protocol class</strong></td>
<td><strong>Protocol</strong></td>
<td><strong>Protocol type</strong></td>
<td><strong>Talks to</strong></td>
<td><strong>Required port class</strong></td>
<td><strong>Supported tags</strong></td>
<td><strong>Tag properties to configure</strong></td>
<td><strong>Notes</strong></td>
</tr>
<tr>
<td>TModBusRTUDriver<br/>
<img alt="Seleção_092" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_092.png"/></td>
<td>Modbus RTU</td>
<td>1 protocol component for many stations</td>
<td>Any device supporting Modbus RTU (station addresses 1 to 247)</td>
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
<td>Blocks and structures use, by default, up to 125 registers per read/write block (driver properties <em>HoldingRegsMaxBlockSize</em>/<em>AnalogRegsMaxBlockSize</em>) and up to 2000 bits for digital inputs/outputs (<em>InputsMaxBlockSize</em>/<em>OutputsMaxBlockSize</em>) — all configurable. See <a href="/modbus-rtu-and-tcp/">Modbus RTU and TCP</a>.</td>
</tr>
<tr>
<td>TModBusTCPDriver<br/>
<img alt="modbus_tcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/modbus_tcp.png"/></td>
<td>Modbus TCP</td>
<td>1 protocol component for 1 or more devices, over the same connection</td>
<td>Any device supporting Modbus TCP</td>
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
<td>Same block limits as Modbus RTU. See <a href="/modbus-rtu-and-tcp/">Modbus RTU and TCP</a>.</td>
</tr>
<tr>
<td>TWestASCIIDriver<br/>
<img alt="west" src="http://www.pascalscada.com/wp-content/uploads/2016/08/west.png"/></td>
<td>West ASCII</td>
<td>1 protocol component for many stations</td>
<td>West n6100/P6100 controllers</td>
<td>
<p>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></p>
</td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>
<p>PLCStation (1 to 99)</p>
<p>MemAddress (0 to 27 — the West parameter)</p></td>
<td>Every West register is a 32-bit float; the driver ignores the configured <code>TagType</code> and always delivers a float — leave it at <code>pttDefault</code>. Supports reads and writes. Has its own tag wizard.</td>
</tr>
<tr>
<td>TIBoxDriver<br/>
<img alt="ibox" src="http://www.pascalscada.com/wp-content/uploads/2016/08/ibox.png"/></td>
<td>Thermo King i-Box</td>
<td>1 protocol component for many stations</td>
<td>Thermo King i-Box refrigeration controllers</td>
<td>TSerialPortDriver<br/>
<img alt="Seleção_090" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/></td>
<td>
<p>PLCStation (0 to 255)</p>
<p>MemAddress — only registers (PID) 0, 96, 168, 200 to 205 and 247 are accepted</p></td>
<td>Read-only: the i-Box refuses write commands outright, any attempt returns an error. Values already arrive ready to use (percentage, hours, tenths of a degree) — <code>TagType</code> does not change the result.</td>
</tr>
<tr>
<td>TISOTCPDriver<br/>
<img alt="isotcp" src="http://www.pascalscada.com/wp-content/uploads/2016/08/isotcp.png"/></td>
<td>Siemens S7 (S7comm) over ISOTCP</td>
<td>1 protocol component for 1 PLC</td>
<td>
<p>S7-200 via a CP 243-1</p>
<p>S7-300 / S7-400 (Ethernet)</p>
<p>LOGO! 7 and 8 (Ethernet)</p>
<p>S7-1200 / S7-1500 (Ethernet, with PUT/GET enabled and non-optimized blocks)</p>
<p>S7comm-compatible simulators/gateways (PLCSIM+NetToPLCSim, Snap7 server)</p></td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCStruct<img alt="plcstruct" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>PLCRack, PLCSlot, PLCStation — on the driver, not on the tag</p>
<p>MemReadFunction (area)</p>
<p>MemFile_DB (when the area is DB)</p>
<p>MemAddress</p>
<p>TagType</p>
<p>SwapBytes/SwapWords/SwapDWords according to the type</p></td>
<td>Does not speak the S7-1200/1500 native protocol (S7CommPlus) and does no symbolic addressing — see <a href="/s7-protocol-over-isotcp/">S7 Protocol over ISOTCP</a> for the full area and type table.</td>
</tr>
<tr>
<td>TMelsecTCPDriver<br/>
<img alt="melsec" src="http://www.pascalscada.com/wp-content/uploads/2016/08/melsec.png"/></td>
<td>Mitsubishi MC Protocol (3E frame, binary) over TCP</td>
<td>1 protocol component for 1 PLC</td>
<td>Mitsubishi Q, L and iQ-F/iQ-R series with an Ethernet port or a QJ71E71/LJ71E71 module, configured for binary MC Protocol/SLMP</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>MemReadFunction / MemWriteFunction (device code: M, X, Y, D…)</p>
<p>MemAddress (decimal — mind that X/Y are numbered in hexadecimal in the PLC)</p></td>
<td>Timers, counters, link registers and the SB/DX/DY areas are not supported yet. See <a href="/mitsubishi-melsec-tcp/">Mitsubishi Melsec TCP</a>.</td>
</tr>
<tr>
<td><strong>TS7CommPlusDriver</strong></td>
<td>Siemens S7CommPlus</td>
<td>1 protocol component for 1 PLC</td>
<td>S7-1200 and S7-1500 (the native protocol TIA Portal uses, with blocks in optimized access — no need to disable it, unlike ISOTCP)</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>LongAddress — the tag's symbolic path in the PLC (e.g. <code>DB4.Var1</code>, <code>MArea.Clock_10Hz</code>)</p></td>
<td>Addresses by the tag's name in the PLC program, not by a memory offset. Dedicated page in preparation.</td>
</tr>
<tr>
<td><strong>TLGXDriver</strong></td>
<td>EtherNet/IP (CIP), Rockwell/Allen-Bradley</td>
<td>1 protocol component for 1 PLC</td>
<td>Rockwell/Allen-Bradley Logix-family PLCs (ControlLogix, CompactLogix…) with an EtherNet/IP port</td>
<td>TTCP_UDPPort<br/>
<img alt="Seleção_091" src="http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png"/></td>
<td>TPLCTagNumber<img alt="plctagnumber" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png"/><br/>
TPLCBlock<img alt="plcblock" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png"/><br/>
TPLCString<img alt="plcstring" src="http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png"/></td>
<td>
<p>LongAddress — the Logix tag name (e.g. <code>Motor1.Speed</code>, <code>MyString.DATA</code> for a STRING)</p></td>
<td>Symbolic addressing, as in TIA Portal/Studio 5000: there is no memory offset to configure. Dedicated page in preparation.</td>
</tr>
</tbody>
</table>

##### Basic structure of a data-acquisition application

The image below summarizes the port → driver → tags → controls chain, common to every protocol in the table above:

[![Basic structure of a data-acquisition application](http://www.pascalscada.com/wp-content/uploads/2016/08/g8441-300x89.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/g8441.png>)Basic structure of a data-acquisition application

##### Related examples

* `examples/laz_modbus_tcp_example`, `examples/laz_modbus_rtu_scanner` — Modbus TCP and RTU.
* `examples/laz_isotcp_demonstration` — Siemens S7 over ISOTCP.
* `examples/laz_west6100_plus_demonstration` — West ASCII.
* `examples/laz_ibox` — Thermo King i-Box.
