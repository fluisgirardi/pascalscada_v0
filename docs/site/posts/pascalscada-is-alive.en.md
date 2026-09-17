After more than 10 years with next to nothing published here, a quick note: PascalSCADA is alive. Not just alive — the last few weeks have seen more work go into the project than any recent stretch, so a summary is in order.

##### Two new protocol drivers

* **S7CommPlus** — the native protocol TIA Portal uses to talk to **S7-1200 and S7-1500** PLCs. Unlike the existing ISOTCP driver, S7CommPlus does not require turning off optimized block access: you address the tag by its **symbolic name** (`DB4.Var1`, `MArea.Clock_10Hz`), the same way TIA does. It supports username/password for CPUs with access protection.
* **EtherNet/IP (CIP)** — a new driver for **Rockwell/Allen-Bradley** Logix-family PLCs (ControlLogix, CompactLogix…), also with symbolic addressing by tag name. It ships with its own Tag Builder wizard.

##### A big cleanup, backed by real tests

This is the less flashy but more important part: PascalSCADA never had a real automated test suite. That changed. Practically the whole library — communication ports, the protocol drivers (Modbus RTU/TCP, Melsec, S7/ISOTCP, S7CommPlus, EtherNet/IP, West ASCII, i-Box), the tag layer and the HMI control library — got unit tests, and chasing every test uncovered a pile of defects that had been sitting in the code, some for years:

* Modbus reads that left the port locked;
* the ISOTCP driver waiting forever for a PLC that never answered;
* West ASCII writes that lost the value's magnitude;
* memory leaks in central places — the scan thread, the event and alarm loggers, the central user manager, the network mutex server;
* the on-screen keyboard on Windows, where the numeric keypad's `-` and `.` came out as a letter or garbage;
* a `THMICheckBox` that sometimes wrote to its tag twice for a single change;
* the security flag that, in one specific case, disabled every control created at run time.

The real list is much longer — it's all in the commit history of the [GitHub repository](https://github.com/fluisgirardi/pascalscada_v0).

##### Building and testing on five platforms

The project now has continuous integration running the tests on Windows 32/64, Linux 32/64 and FreeBSD 64 on every change — plus tests on ARM (ARMHF/AARCH64). That already caught subtle differences between platforms that would have gone unnoticed before (a `TCriticalSection` signaling differently across systems, Unix-specific serial device paths, among others).

##### The documentation got a pass too

The site had several "under construction" pages that had been sitting untouched forever, and others that were badly out of date. Most of it was rewritten: [Tags](/tags/), [S7 Protocol over ISOTCP](/s7-protocol-over-isotcp/), [Mitsubishi Melsec TCP](/mitsubishi-melsec-tcp/), [HMI Control Library](/hmi-control-library-hcl/), [Security system](/security-system/), [Scale processors](/scale-processors/), [HMIDBConnection](/hmidbconnection/), the [protocol drivers](/protocol-drivers/) overview, and a new tutorial, [Writing a protocol driver](/writing-a-protocol-driver/), for anyone who wants to add support for another device. The [installation](/how-install-pascalscada/) page was also brought up to date for the current workflow (GitHub + current Lazarus, no more ZeosLib).

Right now, installing all of this means getting the code straight from [GitHub](https://github.com/fluisgirardi/pascalscada_v0) (see [How to install](/how-install-pascalscada/)) — the package published on Lazarus' *Online Package Manager* is still version 0.7.7, from 2021. That should change in the **coming weeks**: the plan is to publish a new OPM release with both new drivers and all of this cleanup, for anyone who'd rather install straight from the IDE without cloning a repository.

If you use PascalSCADA, ran into something broken, or have a suggestion, open an issue on [GitHub](https://github.com/fluisgirardi/pascalscada_v0) — that's where the project moved to, and that's where the conversation happens now.
