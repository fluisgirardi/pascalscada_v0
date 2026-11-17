PascalSCADA **0.8** is out — the first release since 0.7.7, published on Lazarus' *Online Package Manager* back in 2021. A lot happened in between (more than 550 commits), so here is what changes for anyone coming from 0.7.7.

##### New protocol drivers

* **S7CommPlus** (`TS7CommPlusDriver`) — the native protocol of the **S7-1200 and S7-1500**, the one TIA Portal uses. Tags are addressed by their **symbolic name** (`DB4.Var1`, `"Motor".Speed`), with no need to turn off optimized block access. It brings TLS (including client certificates), username/password for protected CPUs, values **on change** through subscriptions instead of polling, reading and writing several variables in one request, whole-DB reads and writes, and the **PLC alarms** (`THMIS7PlusAlarmLogger` stores them in the same flow as the tag-based alarms). A **Tag Builder** browses the PLC and creates the tags — simple variables, arrays, structures and arrays of structures. See [S7CommPlus](/s7commplus/).
* **OPC UA client** (`TOPCUADriver`) — reads and writes the variables of any OPC UA server (S7-1200/1500, PLCSim, other brands, gateways, other SCADAs), with subscriptions or polling, security None/Sign/SignAndEncrypt and anonymous or user/password login. It is built on **open62541 1.5** through the [fpopen62541](https://github.com/fluisgirardi/fpopen62541) binding, which now ships inside PascalSCADA. It has its own Tag Builder too. See [OPC UA client](/opc-ua/).
* **EtherNet/IP** (`TLGXDriver`) — Rockwell/Allen-Bradley **ControlLogix and CompactLogix**, by tag name, with a Tag Builder.
* **Xinje, Delta, Inovance and Inovance H5U** — TCP drivers for these PLC families (a community contribution).

All drivers also gained a common way to list the tags of a device (`TProtocolDriver.ListTags`) and to write several tags in one call (`MultiWrite`/`ScanMultiWrite`), which the drivers that can batch do in a single request.

##### HMI

* **`THMIButton`** — a two-state button with its own images (or image files), a feedback tag and four click modes, including jog.
* **Alarm and event loggers** — `THMIAlarmLogger`, `THMIEventLogger` and, for S7-1200/1500, `THMIS7PlusAlarmLogger`.
* **New drawn controls** — `THMIBandeja`, flow controls that follow the process (valves, pumps, elevators, polylines, `THMIFlowVectorControl` with SVG content), `THMIControlDislocatorAnimation2` for two independent axes.
* **`TTagLinkedSeriesSource`** — tag values straight into TAChart series.
* The on-screen keyboards now also type on **GTK3 and Qt6** (initial support), besides GTK2, Qt5 and Win32/64.

##### Security

* **`TCentralUserManagement`** — users and permissions kept on a server and shared by every station (with SSL and cached authorizations), with a server example.
* **Chip card / RFID login** as an alternative to the password, with the Sycreader reader on Linux and, now, on Windows.

##### Database: SQLdb instead of ZeosLib

`THMIDBConnection` now uses Free Pascal's own **SQLdb**: ZeosLib is no longer a dependency. The component works the same way (command queue, thread, results back on the main thread), and connections can be renewed per transaction. Protocol values from older projects with a version suffix (`mysql-5.7`) are still accepted. See [HMIDBConnection](/hmidbconnection/).

##### Logging everything (0.8.1)

New in 0.8.1: **`TpSCADALogger`** collects what happens inside PascalSCADA — bytes on the wire, connections and reconnections, scan threads, tag reads and writes, events fired, logins, SQL commands — and routes each message by **rules** (source, class, category, level) to the screen (**`TpSCADALogMemo`**), to files (per day, per source, per class…), to an event, or nowhere. It costs nothing while no rule asks for a message. It also gives an **audit trail**: every value an HMI control writes is logged with the user who was logged in. Exceptions that used to disappear inside the scan threads now show up in the log.

##### Fewer bugs: a real test suite

PascalSCADA never had automated tests. Now it has about **2,100**, covering the ports, every protocol driver, the tag layer, the HMI controls, security and the database layer, and they run on every change on **Windows 32/64, Linux 32/64 and FreeBSD**, plus **ARM (armhf and aarch64)**. Every example is also built on the CI, so none of them falls behind the library again.

Writing those tests uncovered and fixed a long list of old defects — Modbus reads that left the port locked, the ISOTCP scan waiting forever for a silent PLC, West ASCII writes that lost the value's magnitude, a checkbox writing twice, memory leaks and heap corruptions in central places, and many more. A fast tag next to a slow one no longer updates at the slow one's pace.

##### Documentation

Most of the "how to use" pages were rewritten or written for the first time, in English and Portuguese: tags, ports, drivers, the HMI library, security, scale processors, `THMIDBConnection`, [S7CommPlus](/s7commplus/), [OPC UA](/opc-ua/), expressions in the Object Inspector and a tutorial on [writing a protocol driver](/writing-a-protocol-driver/).

##### Upgrading from 0.7.7

* **ZeosLib → SQLdb**: `pascalscada_db` now needs `SQLDBLaz` (part of Lazarus) instead of `zcomponent`. Code that reached into the Zeos objects of `THMIDBConnection` must move to the SQLdb ones.
* **New dependency**: the `pascalscada` package requires `fpopen62541`, which comes in the PascalSCADA tree (`src/external/fpopen62541`) — open it once before installing. The open62541 library itself is only needed at run time, by programs that use OPC UA.
* **S7CommPlus over TLS** needs OpenSSL (1.1.1 or 3.x) at run time.
* **`TProtocolIOResult`** has a new order (and `ioAuthError` at the end): if you stored its values as numbers, map them again.
* The packages require each other at **0.8** or newer: update all of them together.

##### Getting it

The code is on [GitHub](https://github.com/fluisgirardi/pascalscada_v0) — see [How to install](/how-install-pascalscada/). The 0.8 package for Lazarus' *Online Package Manager* follows soon, so it can be installed straight from the IDE.

If something breaks, or you have a suggestion, open an issue on [GitHub](https://github.com/fluisgirardi/pascalscada_v0).
