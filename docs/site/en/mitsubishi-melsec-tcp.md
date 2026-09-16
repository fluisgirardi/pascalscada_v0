##### Introduction

`TMelsecTCPDriver` speaks Mitsubishi Electric's **MC Protocol** (MELSEC Communication Protocol) over TCP/IP, in the **3E frame, binary code** format — the native protocol of the Ethernet ports of the **Q**, **L**, **iQ-F (FX5)** and **iQ-R** series PLCs and of the QJ71E71/LJ71E71 Ethernet modules. It reads and writes relays (bits) and registers (words) directly, with no OPC or Mitsubishi software in between.

The driver does not speak the 1E frame (FX3 and older through the FX3U-ENET module) nor the ASCII format.

##### Required components

1. A ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** with `Host` = PLC IP address and `Port` = the port configured in the PLC for MC Protocol (see *Preparing the PLC*). See [Communication ports](/communication-ports/).
2. A ![](img/TMelsecTCPDriver.png) **`TMelsecTCPDriver`** with `CommunicationPort` pointing to the port.
3. The tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`) with `ProtocolDriver` pointing to the driver.

##### Preparing the PLC

In GX Works2/GX Works3, in the Ethernet port parameters of the CPU (or of the E71 module), open the connection settings (*Open Setting* / *External Device Configuration*) and add a connection with:

* **Protocol**: TCP;
* **Open system**: MC Protocol (*SLMP* on iQ-R/iQ-F);
* **Code**: **Binary** (not ASCII);
* **Host station port No.**: the port that goes in `TTCP_UDPPort.Port` (e.g. 5002);
* on iQ-F/iQ-R, also tick *Enable online change* if you want to write with the CPU in RUN.

Write the parameters and reset the CPU. Each configured connection serves one client; for two supervisory stations configure two connections (different ports).

##### Driver properties

| Property | Default | Description |
|---|---|---|
| `CommunicationPort` | – | The `TTCP_UDPPort`. |
| `ReadSomethingAlways` | `True` | Reads some tag every cycle even when not needed, keeping the connection alive. |
| `Output_M_MaxHole`, `Output_SM_MaxHole`, `Output_L_MaxHole`, `Output_F_MaxHole`, `Output_V_MaxHole`, `Output_X_MaxHole`, `Output_Y_MaxHole`, `Output_B_MaxHole` | 10 | For each relay area, how many **unmapped** addresses the driver tolerates between two tags and still reads them in one request. |
| `Register_D_MaxHole`, `Register_SD_MaxHole` | 10 | The same for the register areas. |
| `SerieCLP` | `Serie_Q_L` | Reserved for differences between series; the current TCP driver does not use it — leave the default. |

The driver groups neighbouring tags of the same area into one request (up to 10 devices per block). A "hole" wider than `*_MaxHole` between two tags produces two requests. If you have D100 and D125 and nothing in between, reading both at once (`MaxHole ≥ 25`) costs a single message.

##### Addressing memory

A MELSEC address is the **device type** (M, X, Y, D…) plus the number. On the tag:

| Tag property | Fill with |
|---|---|
| `MemReadFunction` | Device code (table below). |
| `MemWriteFunction` | The **same** code — reads and writes share the table. |
| `MemAddress` | Device number, **in decimal**. |
| `PLCStation` | Leave `0` on every tag of a driver. The value does not go into the frame (network 0, PC `FF`); it only lets the driver group tags of the same PLC. |
| `MemFile_DB`, `MemSubElement`, `PLCRack`, `PLCSlot` | Not used. |

| Code | Device | Kind | Description |
|---|---|---|---|
| 1 | `M` | bit | Internal relay |
| 2 | `SM` | bit | Special relay |
| 3 | `L` | bit | Latch relay |
| 4 | `F` | bit | Annunciator |
| 5 | `V` | bit | Edge relay |
| 6 | `X` | bit | Input |
| 7 | `Y` | bit | Output |
| 8 | `B` | bit | Link relay |
| 9 | `D` | word | Data register |
| 16 | `SD` | word | Special register |

Timers, counters (`T`, `C`, `ST`), link registers (`W`, `R`, `ZR`) and the `SB`/`DX`/`DY` areas are not implemented in this version.

**Mind the X and Y addresses**: in the PLC they are numbered in **hexadecimal** (`X0`…`X1F`, `Y20`…). `MemAddress` is decimal, so `X10` (hex) is `MemAddress = 16`, and `Y2F` is `MemAddress = 47`. The other devices (M, D, …) are already decimal in the PLC.

Data types:

| In the PLC | `TagType` | Note |
|---|---|---|
| Relay (M, X, Y…) | `pttDefault` | One device = one bit; `Value` is 0 or 1. A `TPLCBlock` with `Size = n` reads n consecutive relays. |
| 16-bit `D` | `pttDefault`, `pttSmallInt` or `pttWord` | One register. |
| Two `D` as 32 bits (`DMOV`, `DINT`) | `pttLongInt` / `pttDWord` | Spans `D[n]` and `D[n+1]`; MELSEC's *low word first* order is what the driver assumes — no swaps. |
| Two `D` as `REAL` (`EMOV`) | `pttFloat` | Same. |
| Text in `D` (`$MOV`) | `TPLCString`, `StringType = stC` | Two characters per register, low byte first. `StringSize` in characters. |

Examples:

| MELSEC address | `MemReadFunction` / `MemWriteFunction` | `MemAddress` | `TagType` |
|---|---|---|---|
| `M100` | 1 | 100 | `pttDefault` |
| `X1A` | 6 | 26 | `pttDefault` |
| `Y0` to `Y7` | 7 | 0 | `TPLCBlock`, `Size = 8` |
| `D200` | 9 | 200 | `pttSmallInt` |
| `D300` as `REAL` | 9 | 300 | `pttFloat` |
| `D400` to `D409` | 9 | 400 | `TPLCBlock`, `Size = 10` |
| `SD210` (clock) | 16 | 210 | `pttWord` |

For a bit inside a `D`, use a [`TTagBit`](/tags/#TTagBit) over the register's tag — MC Protocol reads whole words.

##### Step-by-step example

Read level `D100` and command pump `M50` on an FX5U at 192.168.3.250, MC Protocol on port 5002:

1. GX Works3 → CPU parameters → *Ethernet Port* → *External Device Configuration*: add an *SLMP Connection Module*, TCP, port 5002, binary code. Write and reset.
2. `TTCP_UDPPort`: `Host = 192.168.3.250`, `Port = 5002`, `Active = True`.
3. `TMelsecTCPDriver`: `CommunicationPort = TCP_UDPPort1`.
4. `TPLCTagNumber` `Level`: `ProtocolDriver = MelsecTCPDriver1`, `MemReadFunction = 9`, `MemWriteFunction = 9`, `MemAddress = 100`, `TagType = pttSmallInt`.
5. `TPLCTagNumber` `Pump`: `MemReadFunction = 1`, `MemWriteFunction = 1`, `MemAddress = 50`.
6. A `THMILabel` on `Level` and a `THMICheckBox` on `Pump` (`ValueTrue = 1`, `ValueFalse = 0`).

##### Common problems

| Symptom | Likely cause |
|---|---|
| Port does not open | Wrong IP/port, or the MC Protocol connection was not created/written in the PLC. |
| Port opens but tags return `ioDriverError` | Connection configured as **ASCII** or as *MELSOFT connection* instead of MC Protocol/SLMP binary. |
| `ioPLCError` on some tags | The PLC refused (*end code* ≠ 0): device does not exist or is outside the CPU range, or writing is blocked with the CPU in RUN. |
| `X`/`Y` reading the neighbour | `MemAddress` given in hexadecimal; convert to decimal. |
| A second supervisory station cannot connect | Each MC Protocol connection serves one client; create another connection on another port. |

##### Related examples

There is no Melsec-specific example in the repository. The Modbus TCP (`examples/laz_modbus_tcp_example`) and ISOTCP (`examples/laz_isotcp_demonstration`) examples show the same port → driver → tags → controls structure; only the driver and the addressing properties change.
