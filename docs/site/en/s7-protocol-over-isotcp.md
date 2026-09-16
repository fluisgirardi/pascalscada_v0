##### Introduction

`TISOTCPDriver` speaks **S7comm over ISO-on-TCP** (RFC 1006, TCP port 102) — the same protocol STEP 7, TIA Portal and LibNodave use to read and write the memory of a Siemens PLC. The driver is a Pascal rewrite of LibNodave's ISOTCP by Thomas Hergenhahn: it depends on no DLL or external library.

It talks to:

* **S7-300, S7-400, ET200S/ET200SP** with an Ethernet CPU or CP;
* **S7-1200 and S7-1500** — after enabling PUT/GET access and turning off optimized access on the DBs (see *Preparing the PLC*);
* **S7-200** through a CP 243-1 (`ConnectionWay = ISOTCP_VIA_CP243`);
* **LOGO! 0BA7/0BA8** (LOGO! 7 and 8), which exposes its V area as DB1;
* any S7comm-compatible simulator or gateway (PLCSIM with NetToPLCSim, Snap7 server, and so on).

It does **not** talk to the S7-1200/1500 through their native protocol (S7CommPlus, which TIA Portal uses for symbolic access) — that is what [`TS7CommPlusDriver`](/s7commplus/) is for. ISOTCP remains the simplest option for those PLCs when you control the TIA project and can turn optimized access off.

##### Required components

1. A ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** with `Host` = PLC IP address, `Port = 102` (default) and `PortType = ptTCP`. See [Communication ports](/communication-ports/).
2. A ![](img/TISOTCPDriver.png) **`TISOTCPDriver`** with `CommunicationPort` pointing to the port.
3. The tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCStruct`, `TPLCString`) with `ProtocolDriver` pointing to the driver.

The fastest way to get to step 3 is to right-click the `TISOTCPDriver` and choose **Tag Builder** (see below).

##### Driver properties

| Property | Default | Description |
|---|---|---|
| `CommunicationPort` | – | The `TTCP_UDPPort` used by the driver. |
| `PLCRack` | 0 | CPU rack. |
| `PLCSlot` | 0 | CPU slot. |
| `PLCStation` | 2 | Station address. Only meaningful on MPI/PROFIBUS networks behind a gateway; on Ethernet leave the default. |
| `ConnectionWay` | `ISOTCP` | `ISOTCP` for a direct connection to the CPU/Ethernet CP; `ISOTCP_VIA_CP243` for an S7-200 behind a CP 243-1. |
| `ISOTCPConnType` | `ctOP` | Connection type the driver announces to the PLC: `ctPG` (programming device), `ctOP` (operator panel) or `ctBasic` (other). The PLC reserves resources per type; if every OP connection is in use, switch to `ctBasic`. |
| `ForcePDUSize` | `pduAuto` | Negotiated PDU size. `pduAuto` asks for the largest the PLC accepts; force `pdu240` for old PLCs or LOGO!, which do not negotiate correctly. |
| `ReadOnly` | `False` | When `True` the driver refuses every write (`ioReadOnlyProtocol`). Handy for a monitoring screen that must never change the process. |
| `ReadSomethingAlways` | `True` | Keeps the connection alive by reading some tag every cycle even when none needs updating. |

> **One ISOTCP connection talks to one PLC.** For that reason the `PLCRack`, `PLCSlot` and `PLCStation` properties **of the tags are ignored**: what counts is the driver's configuration. To talk to two PLCs, use two ports and two drivers.

Rack/slot values that work in most cases:

| CPU | `PLCRack` | `PLCSlot` |
|---|---|---|
| S7-300 (CPU with built-in PN or CP 343-1) | 0 | 2 |
| S7-400 | 0 | physical slot of the CPU (usually 2 or 3) |
| S7-1200 / S7-1500 | 0 | 0 (1 also works on the 1200) |
| ET200S / ET200SP CPU | 0 | 2 (ET200S) / 0 (ET200SP) |
| S7-200 via CP 243-1 | 0 | 0 |
| LOGO! 7 / 8 | 0 | 0 (see *Preparing the PLC*) |

From code, `UpdatePLCAddress(Rack, Slot, Station)` changes the three at once without restarting the connection three times.

##### Preparing the PLC

**S7-300 / S7-400**: nothing beyond having the IP address configured. If the CPU has a write-protection password, the driver reads but does not write.

**S7-1200 / S7-1500** (in TIA Portal):

1. In the CPU properties → *Protection & Security* → *Connection mechanisms*: tick **Permit access with PUT/GET communication from remote partner**.
2. In every DB PascalSCADA will access, untick **Optimized block access** (DB properties → *Attributes*) and recompile. An optimized DB has no absolute addresses, and ISOTCP addresses by byte only.
3. If the CPU protection level is *Full access*, nothing else. With *Read access* the driver only reads; with *HMI access* or *No access* the connection is refused.

**LOGO! 7 / 8** (in LOGO!Soft Comfort): under *Tools → Ethernet connections*, add a **server connection** with local TSAP `20.00` and remote TSAP `10.00`, or tick *Allow all connections*. The V area (VB/VW/VD) shows up to PascalSCADA as **DB1** (`MemReadFunction = 4`, `MemFile_DB = 1`); the M memories as area M (`MemReadFunction = 3`). The `examples/demo_logo8` example shows this, with `ForcePDUSize = pdu240` and `PLCStation = 0`.

**S7-200**: use `ConnectionWay = ISOTCP_VIA_CP243` and configure a connection in the CP 243-1 with the STEP 7-Micro/WIN wizard. The V area is `MemReadFunction = 4` with `MemFile_DB = 1`.

##### Addressing memory

Every S7 address is made of an **area**, a **DB number** (DB area only), a **byte address** and the variable **type**. On the tag this becomes:

| Tag property | Fill with |
|---|---|
| `MemReadFunction` | Area code (table below). |
| `MemFile_DB` | DB number, when the area is 4. Ignored otherwise. |
| `MemAddress` | Address of the first **byte** (the number after `IB`, `MW`, `DBD`…). |
| `TagType` | Variable type (table further below). |
| `MemWriteFunction`, `MemSubElement`, `PLCRack/Slot/Station` | Not used. Leave at zero. |

Area codes for `MemReadFunction`:

| Code | Area | CPUs | Note |
|---|---|---|---|
| 1 | `I` — digital inputs (process image) | all | |
| 2 | `Q` — digital outputs (process image) | all | |
| 3 | `M` — flags/markers | all | |
| 4 | `DB` — data block | 300/400/1200/1500; V on S7-200 and LOGO! | Fill `MemFile_DB`. |
| 5 | `C` — counters | 300/400 | `TagType = pttWord`. |
| 6 | `T` — timers | 300/400 | `TagType = pttWord`. |
| 7 | `SM` — special memory | S7-200 | |
| 8 | `AI` — analog inputs | S7-200 | `TagType = pttWord`. |
| 9 | `AQ` — analog outputs | S7-200 | `TagType = pttWord`. |
| 10 | `C` — counters | S7-200 | `TagType = pttWord`. |
| 11 | `T` — timers | S7-200 | `TagType = pttWord`. |
| 12 | `PIW` — peripheral input | 300/400 | `TagType = pttWord`. Reads the value straight from the module, bypassing the process image. |

S7 data types and the matching `TagType`:

| PLC type | Bytes | `TagType` | `SwapBytes` | `SwapWords` | `SwapDWords` |
|---|---|---|---|---|---|
| `BOOL` | – | `pttByte` on the tag + one [`TTagBit`](/tags/#TTagBit) per bit | | | |
| `BYTE`, `CHAR`, `USINT` | 1 | `pttByte` | | | |
| `SINT` | 1 | `pttShortInt` | | | |
| `WORD`, `UINT`, `S5TIME`, `DATE` | 2 | `pttWord` | ✔ | | |
| `INT` | 2 | `pttSmallInt` | ✔ | | |
| `DWORD`, `UDINT`, `TIME_OF_DAY` | 4 | `pttDWord` | ✔ | ✔ | |
| `DINT`, `TIME` | 4 | `pttLongInt` | ✔ | ✔ | |
| `REAL` | 4 | `pttFloat` | ✔ | ✔ | |
| `LINT`, `LTIME` | 8 | `pttInt64` | ✔ | ✔ | ✔ |
| `ULINT`, `LWORD` | 8 | `pttQWord` | ✔ | ✔ | ✔ |
| `LREAL` | 8 | `pttDouble` | ✔ | ✔ | ✔ |
| `STRING[n]` | n + 2 | `TPLCString` with `StringType = stSIEMENS` | | | |

Siemens PLCs store values in **big-endian** and PascalSCADA assembles bytes in machine order (little-endian). So everything wider than one byte needs the `Swap*` flags set as in the table — without them an `INT` = 1 shows up as 256 and a `REAL` becomes an absurd number. The Tag Builder sets the right swaps for you.

Examples:

| S7 address | `MemReadFunction` | `MemFile_DB` | `MemAddress` | `TagType` | Swaps |
|---|---|---|---|---|---|
| `IB3` | 1 | 0 | 3 | `pttByte` | – |
| `Q0.5` | 2 | 0 | 0 | `pttByte` + `TTagBit` with `StartBit = EndBit = 5` | – |
| `MW10` | 3 | 0 | 10 | `pttWord` | B |
| `MD100` (as `REAL`) | 3 | 0 | 100 | `pttFloat` | B + W |
| `DB5.DBW20` | 4 | 5 | 20 | `pttSmallInt` | B |
| `DB5.DBD24` (as `DINT`) | 4 | 5 | 24 | `pttLongInt` | B + W |
| `DB10.DBB0` to `DBB49` | 4 | 10 | 0 | `TPLCBlock` `pttByte`, `Size = 50` | – |
| `DB12` `STRING[30]` at `DBB8` | 4 | 12 | 8 | `TPLCString`, `StringSize = 30`, `stSIEMENS` | – |
| `VW6` (LOGO!/S7-200) | 4 | 1 | 6 | `pttSmallInt` | B |
| `T5` (S7-300) | 6 | 0 | 5 | `pttWord` | B |

##### Bits

PascalSCADA's ISOTCP reads and writes **by byte**. For a `BOOL` (an input, an output, a bit of a DB), create a `TPLCTagNumber` with `TagType = pttByte` pointing to the byte that holds the bit and a [`TTagBit`](/tags/#TTagBit) for each bit of interest (`StartBit = EndBit = bit number`). Writing to the `TTagBit` reads the current byte, changes only that bit and writes the byte back.

For many bits in the same byte, right-click the tag and choose **Map bits**: it creates the eight `TTagBit`s at once. The `examples/laz_isotcp_mapping_bits_from_other_tag` example does this with `DB1.DBB0`.

##### Blocks, structures and strings

* A **`TPLCBlock`** reads `Size` consecutive variables of the same type in one request — an array of `REAL` in a DB, for instance (`TagType = pttFloat`, `Size` = number of elements, `MemAddress` = first byte). The swaps apply to the whole block.
* A **`TPLCStruct`** reads `Size` consecutive bytes, and each `TPLCStructItem`/`TPLCStructString` interprets a slice with its own type and swaps. It is the natural way to map a DB with mixed fields or a UDT — see the example in [Tags](/tags/#TPLCStruct).
* A **`TPLCString`** with `StringType = stSIEMENS` reads a `STRING[n]`: the two header bytes (maximum and current length) are handled by the driver. Use `stC` only for zero-terminated `CHAR` arrays.

The driver automatically merges neighbouring tags of the same area and `RefreshTime` into one request, within the negotiated PDU size. With `pduAuto` on a modern CPU up to 1920 data bytes fit in one message; on an old S7-300 or LOGO!, 240. If the tags' `AvgUpdateRate` stays well above `RefreshTime`, the PLC cannot keep up — raise the `RefreshTime` of the less critical tags or group them into blocks.

##### Tag Builder

Right-click the `TISOTCPDriver` → **Tag Builder** opens the *Siemens S7 Tag builder* wizard:

1. Choose the **area** (inputs, outputs, flags, DB, counters, timers, S7-200 areas…), the **kind of tag** to create (separate `TPLCTagNumber`s, one `TPLCBlock`, or `TPLCStruct`) and the scan time.
2. For separate tags or a block: give the data type, start address, count and DB. The wizard sets `SwapBytes`/`SwapWords` according to the type.
3. For structures: build the item list (name, type, swap, scan) on the *Items declaration* tab; the wizard computes the offsets and can generate N identical structures in sequence (useful for UDT arrays).

The created components appear on the form/datamodule with `ProtocolDriver` already linked.

##### Step-by-step example

Read the temperature `DB1.DBD0` (`REAL`) and write the setpoint `DB1.DBW4` (`INT`) of an S7-1200 at 192.168.0.10:

1. In TIA Portal: enable PUT/GET, untick *Optimized block access* on DB1, compile and download.
2. Drop a `TTCP_UDPPort`: `Host = 192.168.0.10`, `Port = 102`, `Active = True`.
3. Drop a `TISOTCPDriver`: `CommunicationPort = TCP_UDPPort1`, `PLCRack = 0`, `PLCSlot = 0`.
4. Drop a `TPLCTagNumber` named `Temperature`: `ProtocolDriver = ISOTCPDriver1`, `MemReadFunction = 4`, `MemFile_DB = 1`, `MemAddress = 0`, `TagType = pttFloat`, `SwapBytes = True`, `SwapWords = True`.
5. Drop another named `Setpoint`: same, with `MemAddress = 4`, `TagType = pttSmallInt`, `SwapBytes = True`.
6. Link a `THMILabel` to `Temperature` and a `THMIEdit` to `Setpoint`. Run.

The same from code, for those who create the components at run time:

```pascal
uses tcp_udpport, ISOTCPDriver, PLCTagNumber, ProtocolTypes;

var
  Port: TTCP_UDPPort;
  Drv: TISOTCPDriver;
  Temp: TPLCTagNumber;
begin
  Port := TTCP_UDPPort.Create(Self);
  Port.Host := '192.168.0.10';
  Port.Port := 102;

  Drv := TISOTCPDriver.Create(Self);
  Drv.CommunicationPort := Port;
  Drv.PLCRack := 0;
  Drv.PLCSlot := 0;

  Temp := TPLCTagNumber.Create(Self);
  Temp.MemReadFunction := 4;   // DB
  Temp.MemFile_DB := 1;
  Temp.MemAddress := 0;
  Temp.TagType := pttFloat;
  Temp.SwapBytes := True;
  Temp.SwapWords := True;
  Temp.RefreshTime := 500;
  Temp.ProtocolDriver := Drv;  // last

  Port.Active := True;
end;
```

More examples in the repository: `examples/laz_isotcp_demonstration` (DB with block, string and bits), `examples/laz_isotcp_hourmeter` (hour meter with writes), `examples/laz_isotcp_performance` (update-rate measurement) and `examples/demo_logo8`.

##### Common problems

| Symptom | Likely cause |
|---|---|
| Port opens (`Active = True`) but no tag updates; `LastSyncReadStatus = ioConnectPLCFailed` | Wrong rack/slot, or S7-1200/1500 without PUT/GET enabled, or protection level *No access*. |
| Port does not open (`OnCommPortOpenError`) | Wrong IP, port 102 blocked by a firewall, or the PLC already reached its connection limit (change `ISOTCPConnType`). |
| Tags of one specific DB return `ioIllegalMemoryAddress` / `ioObjectNotExists` | The DB does not exist, is shorter than the requested address, or (1200/1500) still has *Optimized block access* on. |
| Absurd 16/32-bit values (`1` becomes `256`, a `REAL` becomes `1E-40`) | Missing `SwapBytes`/`SwapWords`. |
| Writes fail with `ioReadOnlyProtocol` or `ioObjectAccessNotAllowed` | `ReadOnly = True` on the driver, or the CPU is write-protected/password protected. |
| Works on an S7-300 but not on a LOGO! | Force `ForcePDUSize = pdu240` and check the server connection TSAP in LOGO!Soft. |
| Timers/counters read as zero | `TagType` must be `pttWord` on those areas. |

##### Related examples

* `examples/laz_isotcp_demonstration` — a DB with `TPLCBlock`, elements, `TPLCString` and bits, with `THMIEdit`/`THMIText` (a Delphi version is in `examples/delphi_isotcp_demonstration`).
* `examples/laz_isotcp_hourmeter` — hour meters read from blocks with dozens of `TTagBit`s and `THMIAnimation`, with writes.
* `examples/laz_isotcp_mapping_bits_from_other_tag` — `DB1.DBB0` with one `TTagBit` per bit.
* `examples/laz_isotcp_performance` — thousands of block elements to measure the update rate and the effect of the PDU size.
* `examples/demo_logo8` — LOGO! 8: the V area as DB1, `ForcePDUSize = pdu240`.
* `examples/hmi_bandeja` — `TPLCStruct` over a DB, with `THMIBandeja`.
