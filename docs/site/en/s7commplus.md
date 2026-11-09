##### Introduction

`TS7CommPlusDriver` speaks **S7CommPlus**, the native protocol of the S7-1200 and S7-1500 — the same one TIA Portal uses to program them. Unlike [S7 over ISOTCP](/s7-protocol-over-isotcp/), it addresses variables by their **symbolic path** (the name the variable has in the PLC program) instead of a DB number plus a byte offset, so an optimized DB — the default in TIA Portal — can stay optimized.

|  | S7CommPlus | ISOTCP |
|---|---|---|
| PLCs | S7-1200, S7-1500 | S7-300/400, S7-1200/1500, S7-200 behind a CP 243-1, LOGO! 7/8 |
| Addressing | symbolic: `DB4.Var26.v1`, `MArea.Clock_10Hz` | absolute: `DB1.DBD0`, `M10.3` |
| *Optimized block access* | may stay on | must be turned off |
| *Permit access with PUT/GET* | not needed | needed |
| Encryption | TLS, on by default | none |
| Password login | not usable yet — see *TLS and certificates* | – |

It does **not** talk to the S7-300, S7-400, S7-200 or LOGO!, which have no S7CommPlus. Use `TISOTCPDriver` for those, and also for an S7-1200/1500 when you control the TIA project and would rather address by byte.

> **One driver talks to one PLC.** The driver owns the session; to talk to two PLCs use two communication ports and two drivers, one each.

##### Required components

1. A ![](img/TTCP_UDPPort.png) **`TTCP_UDPPort`** with `Host` = PLC IP address, `Port = 102` (default) and `PortType = ptTCP`. See [Communication ports](/communication-ports/).
2. A **`TS7CommPlusDriver`** (unit `S7PlusFamily`) with `CommunicationPort` pointing to the port.
3. The tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`, `TPLCStruct`) with `ProtocolDriver` pointing to the driver and `LongAddress` holding the symbolic path (see *Addressing*).

The fastest way to get to step 3 is the **Tag Builder** wizard (see below), which reads the variable list from the PLC and creates the tags with the path, the type and the byte swapping already set.

##### Preparing the PLC

Nothing has to be changed in the program structure: optimized DBs are fine, and PUT/GET stays off. Only two things matter:

1. **IP address** configured on the CPU (and reachable from the machine running the application).
2. **Protection level**, in the CPU properties → *Protection & Security* → *Access level*:

| Protection level | What the driver can do |
|---|---|
| *Full access* (no password) | read and write |
| *Read access* | read only; every write fails |
| *HMI access* / *No access* | the connection is refused |

Leave the password out: a CPU set to *Full access with password* needs a login, and the password legitimation of S7CommPlus is not finished in this driver yet (see *TLS and certificates*).

##### Driver properties

| Property | Default | Description |
|---|---|---|
| `CommunicationPort` | – | The `TTCP_UDPPort` used by the driver. |
| `UseTLS` | `True` | Encrypts the session. Required by most S7-1200/1500 firmware from TIA Portal V15 on; set it to `False` only for known legacy V1 firmware. |
| `ReadOnly` | `False` | When `True` no write reaches the PLC: every write is answered with `ioReadOnlyProtocol`. For a screen that must only watch. |
| `ReadSomethingAlways` | `True` | A scan round that has to read something also refreshes the polled tags that are not due, as long as they fit in the request already going out. Fresh values for no extra round trip. |
| `MaxItemsPerRequest` | 100 | Most items in one read/write request. A bigger batch is split into several requests, so this bounds the request, not the number of tags. Do not raise it blindly: a PLCSim refused 101 items. |
| `MaxTagsPerScan` | 500 | Most tags served in one scan round. Tags beyond it stay due and are served by the next round — nothing is dropped. A whole-DB tag counts as one. |
| `MaxItemsPerSubscription` | 500 | Most items in one subscription (a limit of size, not of count: it depends on how long the LID paths of the tags are). |
| `SubscriptionCycleMs` | 100 | How often the PLC looks for a change on the tags with `RefreshTime = 0`. Nothing below 100 is accepted (a shorter one is raised to 100). |
| `SubscriptionCreditLimit` | 10 | Notification credit of those subscriptions; renewed automatically while positive. |
| `SubscriptionTimeoutMs` | 5000 | Watchdog: a subscription that stays silent for this long while the session is up was lost and is created again. Never less than 10 cycles; `0` turns it off. |
| `ProgramCheckIntervalMs` | 10000 | How often the driver looks for a download to the PLC (DB list + type modification time) and re-resolves the addresses if it changed. `0` turns it off; below 1000 is raised to 1000. |
| `Password` | empty | PLC password for legitimation. **Not usable yet** — see *TLS and certificates*. |
| `Username` | empty | Optional user name for the "new" style legitimation. Leave empty. |
| `ServerCertificate` | empty | Empty accepts whatever certificate the PLC presents. With a value, only that certificate (or one signed by it) is accepted. In the Object Inspector the "…" button loads it from a `.pem/.crt/.cer/.der` file. |
| `ClientCertificate` | empty | Certificate presented to the PLC when it asks for one (mutual TLS): PEM text, a PEM/PKCS#12 file name, or the base64 of a PKCS#12. Empty: none, the usual case. |
| `ClientCertificatePassword` | empty | Password of the private key or of the PKCS#12 of `ClientCertificate`. |

There is also a read-only `PeerCertificate` property: the certificate the PLC presented in the last handshake, as base64 of its DER. Copy it into `ServerCertificate` to trust only that PLC.

##### Addressing

`LongAddress` of the tag is the path of the variable, as written in TIA Portal. Two forms reach a DB:

```
DB4.Var26.v1               // by DB number
BlocoSimbolico_4.Var26.v1  // by the symbolic name of the DB, as in the program
```

The dots are part of the path: `Var26.v1` is a member `v1` inside the structure `Var26`, exactly as in the PLC program. The process areas are named `IArea`, `QArea`, `MArea`, `S7Timers` and `S7Counters`, and a tag name may itself contain dots:

```
MArea.Clock_10Hz
QArea.Motor1_Run
S7Timers.T1
```

Which tag class to use:

| Variable in the PLC | Tag class | `LongAddress` |
|---|---|---|
| A scalar (`Bool`, `Int`, `Real`, …) | `TPLCTagNumber` | `DB4.Var26` |
| A `STRING` | `TPLCString` | `DB4.Message` |
| An array of a simple type | `TPLCBlock` | `DB4.Arr` |
| One element of an array | `TPLCBlock` sized to one element, or `TPLCTagNumber` | `DB4.Arr[2]` |
| A structure | `TPLCStruct` with one `TPLCStructItem` per member | `DB4.MyStruct` |
| A whole DB | `TPLCStruct` | `DB4` |
| An array of structures | `TPLCStruct` covering the array, members mapped to every index | `DB4.Arr` |

The **`TagType`** of a `TPLCTagNumber` must match the type the variable has in the PLC, and so must `SwapBytes`/`SwapWords`/`SwapDWords`: the PLC is big-endian, so a 2-byte value needs `SwapBytes`, a 4-byte one `SwapBytes` + `SwapWords`, an 8-byte one all three. The Tag Builder fills these in from the PLC's own type information; only a tag typed by hand can get them wrong.

> **A whole DB is not its bytes.** Reading the root of a DB returns a small metadata object, not the memory of the DB: a whole-DB `TPLCStruct` is read member by member, and written the same way.

##### Tag Builder

Right-click the `TS7CommPlusDriver` → **Tag Builder** opens *S7CommPlus – tag import*:

1. Press **Read the variables from the PLC**. The port must be `Active = True` beforehand, otherwise the wizard warns and reads nothing. Only the DB list (plus the I, Q and M areas, if *Include the I, Q and M areas* is ticked) is read at this point; the variables of a DB are read when it is expanded or ticked, and the IDE freezes for the time of one DB.
2. Choose what to import in **What to import** — each mode shows only what it imports, and what is ticked in each one is kept, so a single **Import** creates everything:

| Mode | What each ticked item becomes |
|---|---|
| *Simple tags* | a `TPLCTagNumber` (a `TPLCString` for a `STRING`), read by its symbolic path |
| *Arrays of simple types* | a `TPLCBlock`, with the option of also creating the `TPLCBlockElement` of its elements |
| *Structures* | a `TPLCStruct`, with the option of also creating the `TPLCStructItem` of its members. A whole DB is a structure too: ticking the DB creates a `TPLCStruct` read by its name, with an item per top-level variable |
| *Arrays of structures* | only the first element is shown, with its members; the ticked members are mapped (`TPLCStructItem`) to every index of a `TPLCStruct` that covers the whole array |

3. Set the **scan time** (ms; `0` = on change), the **name prefix** of the created components and, for a block or a structure, whether the elements/items are created too. Ticking an item does not tick its sub-items: the right-button menu has *Check all the sub-items* / *Uncheck all*.
4. **Import**. The components appear on the form or data module with `ProtocolDriver` already linked.

A variable that cannot become a tag is listed with the reason (*not a tag: structure (use the members)*, *type not supported*, *array element*, *could not read the STRING length*) when *Show the variables that do not become tags* is on.

##### Polling or on change

`RefreshTime` of the tag decides:

* **`> 0`** — polling: the driver reads the tag every `RefreshTime` ms, batching the tags that are due into as few requests as `MaxItemsPerRequest` allows.
* **`= 0`** — on change: the driver creates a subscription on the PLC, which then reports the changes every `SubscriptionCycleMs` (100 ms minimum). A value that flips faster than half of that cycle can go unnoticed — a 10 Hz clock flips every 50 ms and is always seen in the same state — so give such a tag a `RefreshTime` above 0 and poll it.

Subscriptions are recreated by themselves after a lost session or a dropped subscription (`SubscriptionTimeoutMs`), and the addresses are resolved again after a download to the PLC (`ProgramCheckIntervalMs`). `RefreshCaches` forces that immediately — call it after a download of a new program if the tags must be right at once.

##### Active alarms

`ReadActiveAlarms(Languages, Alarms)` of the driver returns the alarms that are active in the PLC now (S7-1200/1500): one `TS7PlusAlarm` per alarm, with the CPU alarm ID, the domain, the sequence counter, whether it is *coming* or *going*, the time of the event and of the acknowledgement (UTC), the associated values and the texts of each language. `Languages` is a list of language IDs (`S7PlusLang_Portuguese_BR` = 1046, `S7PlusLang_English_US` = 1033...); empty keeps every language the PLC sends. The texts already have the associated values in place of the `@1%d@` marks. It is a single request, so it can be called whenever a snapshot is wanted; no subscription is created and the alarms are not acknowledged.

To be told when an alarm comes or goes, subscribe: `AlarmSubscribe(Languages, SendTexts, CreditLimit)` and then call `AlarmPoll(WaitMs, Alarms)`, which waits up to `WaitMs` ms (the driver is held meanwhile, so keep it short) and returns the alarms that arrived since the last call; empty with `ioOk` means none did. `SendTexts = True` (default) makes the PLC send the texts with each notification; an S7-1200 without TLS refuses that, so use `False` there and read the texts with `ReadActiveAlarms`. The credit is renewed on its own and the subscription is created again after a reconnect. `AlarmUnsubscribe` removes it without touching the data subscriptions. An alarm that was already active when subscribing is not sent: take that snapshot with `ReadActiveAlarms` first.

```pascal
var Alarms: TS7PlusAlarmArray; i: Integer;
begin
  Drv.ReadActiveAlarms([S7PlusLang_English_US], Alarms);  // what is active now
  Drv.AlarmSubscribe([S7PlusLang_English_US]);              // and what changes from now on
  while not Terminated do
    if Drv.AlarmPoll(500, Alarms) = ioOk then
      for i := 0 to High(Alarms) do
        WriteLn(S7PlusAlarmToString(Alarms[i], S7PlusLang_English_US));
end;
```

The alarm subscription follows the format of the S7CommPlusDriver and s7commplus reference libraries; it is not yet validated against every PLC model.

##### Step-by-step example

Read the temperature `"Data".Temperature` (`Real`) of an S7-1200 at 192.168.0.10 and write the setpoint `"Data".Setpoint` (`Int`):

1. In TIA Portal: nothing to change; just note the name of the DB (`Data`) and compile.
2. Drop a `TTCP_UDPPort`: `Host = 192.168.0.10`, `Port = 102`, `PortType = ptTCP`.
3. Drop a `TS7CommPlusDriver`: `CommunicationPort = TCP_UDPPort1`, leave `UseTLS = True`.
4. Right-click the driver → **Tag Builder**: *Read the variables from the PLC*, open the DB `Data`, mode *Simple tags*, tick `Temperature` and `Setpoint`, scan time 500, **Import**.
5. Link a `THMILabel` to `Temperature` and a `THMIEdit` to `Setpoint`. Set `Port.Active := True` and run.

The same from code:

```pascal
uses tcp_udpport, S7PlusFamily, PLCTagNumber;

var
  Port: TTCP_UDPPort;
  Drv: TS7CommPlusDriver;
  Temp, Setpoint: TPLCTagNumber;
begin
  Port := TTCP_UDPPort.Create(Self);
  Port.Host := '192.168.0.10';
  Port.Port := 102;

  Drv := TS7CommPlusDriver.Create(Self);
  Drv.CommunicationPort := Port;
  Drv.UseTLS := True;

  Temp := TPLCTagNumber.Create(Self);
  Temp.LongAddress := 'Data.Temperature';  // the symbolic path
  Temp.TagType := pttFloat;
  Temp.SwapBytes := True;                  // big-endian PLC
  Temp.SwapWords := True;
  Temp.RefreshTime := 500;                 // 0 = on change

  Setpoint := TPLCTagNumber.Create(Self);
  Setpoint.LongAddress := 'Data.Setpoint';
  Setpoint.TagType := pttSmallInt;
  Setpoint.SwapBytes := True;

  Temp.ProtocolDriver := Drv;              // last
  Setpoint.ProtocolDriver := Drv;

  Port.Active := True;
end;
```

##### TLS and certificates

`UseTLS` is on by default and most S7-1200/1500 firmware from V15 on requires it. The TLS tunnel is built by the driver itself, over OpenSSL loaded at run time: `libssl-3-x64.dll` / `libcrypto-3-x64.dll`, or the 1.1 versions, or the legacy `ssleay32.dll`/`libeay32.dll`. Ship those DLLs next to the executable (or anywhere on the `PATH`) when the application uses TLS.

* `ServerCertificate` empty accepts the certificate the PLC presents. Fill it to pin one PLC: the handshake then fails for any other certificate (another PLC, a device in between), and the reason shows on `OnDebug`. The name in the certificate is not compared — PLC certificates rarely carry their IP address.
* `ClientCertificate` adds mutual TLS: the certificate this side presents, with its private key, as PEM text, as the name of a PEM/PKCS#12 file (read at each connection, so the key stays out of the form) or as the base64 of a PKCS#12. Needs OpenSSL 1.1.1 or newer; a PKCS#12 made with old algorithms (RC2) also needs the legacy provider of OpenSSL 3.

> **Password legitimation is not finished.** `Password` (and `Username`) makes the driver log in right after each new connection, but only the challenge-retrieval step is confirmed against real hardware: the PLC still rejects the encrypted response. A CPU at *Full access with password* therefore answers reads and refuses writes. Use a protection level without password, or accept read-only access.

##### Common problems

| Symptom | Likely cause |
|---|---|
| Port opens but no tag updates, `ioConnectPLCFailed` | Wrong IP, port 102 blocked, or the CPU protection level is *HMI access*/*No access*. |
| Every tag fails with `ioAuthError` | The CPU protects what the session needs and the login did not happen — see *Password legitimation* above. |
| A tag fails with `ioObjectNotExists` right after a download | The address of the program that was replaced is still cached: call `RefreshCaches` (it happens by itself within `ProgramCheckIntervalMs`). |
| Writes fail while reads work | Protection level *Read access*, `ReadOnly = True`, or *Full access with password* (no login). |
| Absurd 16/32-bit values (`1` becomes `256`) | Missing `SwapBytes`/`SwapWords` on a tag typed by hand. |
| A tag on an array of structures reads garbage | Read it as a `TPLCStruct` covering the array: reading the whole array returns a value the driver does not decode and it already reads it member by member. |
| A fast signal is always seen in the same state | The tag is on change with a cycle longer than the signal: give it a `RefreshTime` above 0. |
| The Tag Builder warns and reads nothing | The port was not `Active = True` when the wizard was opened. |
| Values stop arriving after a while, without an error | The subscription went silent: check `SubscriptionTimeoutMs` (the driver recreates it) and `OnDebug`. |

##### Related examples

* `examples/laz_s7commplus_phase1_test` — direct area/DB access (`ReadArea`, `WriteArea`, `ReadDB`, `WriteDB`) by number, no tags.
* `examples/laz_s7commplus_phase2_test` — one `TPLCTagNumber` with a symbolic `LongAddress`, updated by `RefreshTime`; run it as `s7commplus_tag_test <plc-ip> <path> [refresh-ms]`.
* `examples/laz_s7commplus_structitems_gui` — structures and structures items read from a DB, with a small form.

The tag plan rules (what becomes a tag, and with which type) live in `S7PlusTagPlan` and are tested without a PLC in `tests/`.
