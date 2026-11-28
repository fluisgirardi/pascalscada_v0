# PascalSCADA 0.x — A multiplatform SCADA framework for Lazarus

PascalSCADA is a visual SCADA/HMI framework for **Lazarus and Free Pascal**.
It lets you build a supervisory application without writing communication code:
drop a **port component** and a **protocol driver** on a form, bind **tags** to
PLC variables, and connect buttons, sliders, gauges and animations to those
tags. Everything is also available from code when you need finer control.

> **Project status.** This is the working 0.x branch, not a finished rewrite.
> A clean-room rewrite is under development at
> <https://github.com/fluisgirardi/pascalscada>. Some recently added drivers —
> in particular the CODESYS V3 symbol driver — are still marked as
> **test-stage**; see [Experimental drivers](#experimental-drivers) below.

## Features

- **Code-free setup at design time.** Ports, drivers and tags are components;
  property editors and Tag Builders generate the repetitive declarations.
- **Wide protocol coverage.** Modbus RTU/TCP, Siemens S7 (ISO-on-TCP and
  S7comm+), Mitsubishi Melsec TCP, Delta, Inovance/H5U, Xinje, LGX, IBox,
  West ASCII, OPC UA, MQTT and the CODESYS V3 symbol protocol.
- **One tag model for every protocol.** `TPLCTagNumber`, `TPLCBlock`,
  `TPLCString`, `TPLCStruct`, `TTagBit` and `TNumericExprTag` work the same way
  regardless of the underlying driver.
- **HMI component library.** Numeric and string displays, gauges, buttons,
  sliders, alarms, recipe controls and security/user management.
- **Utilities.** Linear scaling, tag queues, a protocol logger, network
  mutexes and an OPC UA server.
- **Cross-platform.** The runtime units are written in Object Pascal for FPC
  and Lazarus; individual drivers may have platform restrictions (noted in
  their documentation).

## Architecture

A PascalSCADA application has three layers:

```
  HMI controls (buttons, gauges, animations)
            |
          Tags  -- TPLCTagNumber, TPLCBlock, TPLCString, ...
            |
    Protocol driver -- Modbus, S7, MQTT, CODESYS, ...
            |
      Port component -- TSerialPortDriver, TTCP_UDPPort (when required)
            |
          PLC / device
```

A tag holds a value, an address and a refresh policy. The protocol driver
translates tag addresses into device requests and caches the answers. The port
component owns the physical connection (serial line or TCP/UDP socket) for
drivers that need one.

Some drivers manage their own connection and must **not** be connected to a
port component: `TMQTTDriver` opens a TCP connection to an MQTT broker, and
`TCodesysSymbolDriver` opens its own TCP/channel session to a CODESYS runtime.
For Modbus TCP, S7, Melsec, the H5U tag driver and others, a `TTCP_UDPPort` is
required.

## Components

### Ports

| Component | Purpose |
| --- | --- |
| `TSerialPortDriver` | Serial (RS-232/RS-485) port, used by RTU drivers |
| `TTCP_UDPPort` | TCP or UDP client socket, used by most Ethernet drivers |

### Protocol drivers

| Component | Protocol / device |
| --- | --- |
| `TModBusRTUDriver`, `TModBusTCPDriver` | Modbus RTU and Modbus TCP |
| `TWestASCIIDriver` | West instruments, ASCII protocol |
| `TIBoxDriver` | IBox controllers |
| `TISOTCPDriver` | Siemens S7 over ISO-on-TCP (RFC 1006) |
| `TS7CommPlusDriver` | Siemens S7comm+ (modern S7-1200/1500) |
| `TMelsecTCPDriver` | Mitsubishi Melsec TCP |
| `TXinJETCPDriver` | Xinje PLCs over TCP |
| `TDeltaTCPDriver` | Delta PLCs (including the AS series) over TCP |
| `TInovanceTCPDriver` | Inovance PLCs over TCP |
| `TH5UTagTCPDriver` | Inovance H5U variable-area access (port 12939) |
| `TLGXDriver` | LG PLCs over TCP |
| `TOPCUADriver` | OPC UA client |
| `TMQTTDriver` | MQTT 3.1.1 client (broker connection is built in) |
| `TCodesysSymbolDriver` | CODESYS V3 symbol protocol on TCP 11740 (test-stage) |

### Tags

`TPLCTagNumber` (single numeric value), `TPLCBlock` (array/block),
`TPLCBlockElement` (one element of a block), `TPLCString`, `TPLCStruct`,
`TPLCStructItem`, `TTagBit` (one bit of a numeric tag) and
`TNumericExprTag` (a value computed from an expression).

### Utilities and services

`TScalesQueue`, `TLinearScaleProcessor`, `TUserScale`, `TMutexServer`,
`TMutexClient`, `TpSCADALogger`, `TOPCUAServer`, plus the HMI and recipe
control libraries.

## Installation

1. Install Lazarus/Free Pascal.
2. Open `pascalscada.lpk` in Lazarus and build/install it (runtime package).
3. Open `pascalscada_dsng.lpk` and build/install it (design-time package;
   registers the components, property editors and Tag Builders).
4. Optional packages: `pascalscada_hmi.lpk` (HMI controls),
   `pascalscada_db.lpk` (database connection), `pascalscada_full.lpk`
   (everything), `pascalscada_common.lpk` and
   `pascalscada_externallibs_hmi.lpk`.
5. Rebuild the IDE if the components do not appear in the palette.

From the command line, `lazbuild pascalscada.lpk` and
`lazbuild pascalscada_dsng.lpk` compile the packages; an IDE rebuild with
`lazbuild --build-ide=` is needed after changing design-time code.

## Quick start

1. Drop a `TTCP_UDPPort` on the form, set `Host`, `Port` and `PortType`.
2. Drop a protocol driver (for example `TModBusTCPDriver`) and set its
   `CommunicationPort` to that port component.
3. Drop a `TPLCTagNumber`, set its `ProtocolDriver` to the driver and fill in
   the address required by that protocol (for example `LongAddress`,
   `MemAddress` / `MemReadFunction`, or a symbol path).
4. Set `Active := True` on the driver and `AutoRead := True` on the tag; the
   tag value now follows the PLC value.
5. Bind an HMI control to the tag, or read `Tag.Value` from code.

A minimal code-only example:

```pascal
uses PLCTagNumber, ModBusTCP, tcp_udpport, commtypes;

var
  Port: TTCP_UDPPort;
  Driver: TModBusTCPDriver;
  Tag: TPLCTagNumber;
begin
  Port := TTCP_UDPPort.Create(nil);
  Driver := TModBusTCPDriver.Create(nil);
  Tag := TPLCTagNumber.Create(nil);
  try
    Port.Host := '192.168.1.30';
    Port.Port := 502;
    Port.PortType := ptTCP;
    Driver.CommunicationPort := Port;
    Driver.Active := True;

    Tag.ProtocolDriver := Driver;   // registers the tag with the driver
    Tag.PLCStation := 1;
    Tag.MemAddress := 0;
    Tag.MemReadFunction := 3;
    Tag.UpdateTime := 500;
    Writeln(Tag.Value);
  finally
    Tag.Free;
    Driver.Free;
    Port.Free;
  end;
end;
```

## Design-time Tag Builders

Two drivers ship with a wizard that creates tags from a real PLC variable
table, avoiding manual transcription of addresses:

- **H5U Tag Builder** — right-click `TH5UTagTCPDriver`, choose *Tag Builder*,
  select the CSV exported by AutoShop (GBK encoded). The wizard reads
  variable names, addresses and types, creates `TPLCTagNumber` /
  `TPLCBlock` components and binds them to the driver. The command-line
  converter `tools/autoshop_import.py` remains available as a fallback.
- **CODESYS Tag Builder** — imports a CODESYS `Symbolconfiguration` XML and
  creates tags with full symbol paths and explicit IEC types.

## Documentation and examples

- Website and "How to use" pages: <https://www.pascalscada.com>. Markdown
  sources live in `docs/site/en/` and `docs/site/pb/`.
- A detailed **Chinese user manual** (covering the drivers, the Tag Builders
  and the CODESYS V3 protocol) is distributed separately from this repository.
- Ready-to-open Lazarus projects live in `examples/`, including
  `laz_modbus_tcp_example`, `laz_mqtt_console`, `laz_opcua_client`,
  `laz_s7commplus_structitems_gui`, `laz_linear_scalling` and many more.
- To write a new driver, start from `docs/site/en/writing-a-protocol-driver.md`.

## Experimental drivers

`TCodesysSymbolDriver` implements the private CODESYS V3 symbol protocol on
TCP port 11740 without OPC UA or a vendor DLL. It has been calibrated against
real traffic from a CODESYS Control Win V3 runtime and verified end-to-end
for BOOL, INT, REAL and LREAL, but:

- only runtimes that allow **anonymous / no-password** access are supported;
- only one runtime version has been exercised;
- LINT, ULINT, STRING and WSTRING are not yet verified.

Keep the test-stage label in mind and do not use this driver in production
until wider coverage is confirmed.

## License

PascalSCADA is distributed under a **modified LGPL**. See
`LICENSE.modifiedLGPL.txt` and `LICENSE.txt`. You may link the library into
applications for any purpose; only the included library code must remain
under the LGPL terms.

## Contributing

Improvements are welcome. Please open an issue describing the change and
attach a patch or a pull request. When adding a driver, include an example
project and documentation for the addressing model.

## Contact

Fabio Luis Girardi — fabio at pascalscada.com
