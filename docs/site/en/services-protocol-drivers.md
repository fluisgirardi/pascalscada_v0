##### Protocol drivers and integrations

The PascalSCADA drivers were written by the same developer who takes on the projects. If your equipment speaks a protocol the library does not have yet, or the plant has to talk to other systems, it can be developed on demand.

##### Protocols already supported

| Vendor / standard | Driver |
|---|---|
| Siemens S7-300/400/1200/1500 | [S7 over ISOTCP](/s7-protocol-over-isotcp/) and [S7CommPlus](/s7commplus/) (S7-1200/1500 with optimized access, TLS and password) |
| Rockwell / Allen-Bradley | EtherNet/IP (Logix) |
| Mitsubishi | [Melsec TCP](/mitsubishi-melsec-tcp/) |
| Modbus | [Modbus RTU and TCP](/modbus-rtu-and-tcp/) (drives, scales, meters, controllers) |
| Delta, Inovance | dedicated drivers over serial and TCP |
| West | [West ASCII](/west-ascii/) |
| OPC UA | [client](/opc-ua/) and server |
| MQTT | MQTT 3.1.1 client |

##### What can be developed

* **A new driver** for a PLC, drive, scale, reader or instrument, from the protocol manual and a device to test with.
* **Improvements to an existing driver**: a protocol feature not covered yet, performance, a new PLC model.
* **Integrations**: plant data to databases, MES, ERP or the cloud over OPC UA, MQTT or SQL; gateways between protocols.

A driver built on demand can be **added to the open library**, and is then maintained with it, or stay **exclusive to your project**. This is agreed in the proposal.

##### What we need from you

The protocol manual or specification, the device model and firmware and, during development, access to a device or simulator (on site, remotely or on loan).

##### Related examples

* `examples/laz_s7commplus_structitems_gui`
* `examples/laz_modbus_tcp_example`
* `examples/laz_opcua_server`
* `examples/laz_mqtt_console`

[Request a quote](/services-request-a-quote/)
