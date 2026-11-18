##### Introduction

![](img/TOPCUAServer.png) `TOPCUAServer` is an **OPC UA server inside your application**. It publishes the tags you choose — memory tags, expression tags and the tags of any protocol driver (Modbus, S7, Ethernet/IP, even an OPC UA client) — so that any OPC UA client can read and write them: UaExpert, a SCADA, an MES, a Python script, or another PascalSCADA application with a `TOPCUADriver`.

The application stays the only one talking to the PLCs: the clients see the values the application already has, and a write of a client goes through the tag to the device, **the client getting the device's result** (if the PLC refuses the write, the client is told so).

Like the client driver, the protocol is handled by **open62541 1.5** through [fpopen62541](https://github.com/fluisgirardi/fpopen62541); see [OPC UA client](/opc-ua/) for how to get the library. It is loaded only when the server is activated.

| | |
|---|---|
| What is published | the tags listed in `Items` — nothing is published by itself |
| Types | the native type of each tag (`TagType`), or `Double` with its scale |
| Arrays and structures | `TPLCBlock` as an array, `TPLCStruct` as an object with one variable per item |
| Writes | per item (`Writable`), synchronous for the client, with the device's result |
| Security | None, Sign, SignAndEncrypt; anonymous, user list or your own check; write permission per user |

##### Required components

1. A **`TOPCUAServer`** (unit `opcuaserver`, palette *PascalSCADA Utils*).
2. The tags to publish, added to its **`Items`** collection. A **double click** on the server opens *Add tags*: it lists the tags of the form not yet published, with the options to put each one in a folder named after its driver and to make them writable.
3. `Active = True`. A server stored active on a form starts once the form is loaded.

That is all: run the program and connect a client to `opc.tcp://<computer>:4840`.

##### Server properties

| Property | Default | Description |
|---|---|---|
| `Active` | `False` | Starts/stops the server. |
| `Port` | `4840` | TCP port of the endpoint `opc.tcp://<computer>:<Port>`. |
| `ServerName` | `PascalSCADA` | Name of the folder, under *Objects*, that holds the published tags. |
| `NamespaceUri` | `urn:pascalscada:tags` | Namespace of the variables; `NamespaceIndex` (run time) is its index, the `ns=` of the NodeIds. |
| `LibraryName` | empty | Path or name of libopen62541; empty searches the default names and the folder of the executable. |
| `LogLevel` | `oslWarning` | Messages of open62541 that go to the log. |
| `Items` | – | The published tags (next section). |
| `Running` / `LastError` | – | Run time: whether it is serving, and why not. |

The properties of the server (port, security...) can only be changed while it is stopped. `Items` can be changed at any time: added, changed or removed items, and destroyed tags, are applied at once on a running server.

##### Items

| Property | Default | Description |
|---|---|---|
| `Tag` | – | The tag published. |
| `NodeName` | empty | Name of the variable; empty uses the name of the tag. |
| `Folder` | empty | Folders under `ServerName`, separated by `/`: `Line1/Oven`. |
| `Description` | empty | Description of the variable for the clients. |
| `Writable` | `False` | Whether the clients may write it. |
| `DataType` | `odtNative` | `odtNative`: the type of the tag; `odtScaledDouble`: the scaled value (`Value`) as `Double`. |

The NodeId of a variable is `ns=<NamespaceIndex>;s=<Folder with '.'>.<NodeName>`, for example `ns=1;s=Line1.Oven.Temperature`, so it does not change when the server restarts.

**Types (`odtNative`).**

| Tag | OPC UA type |
|---|---|
| `TPLCTagNumber` | the type of its `TagType` (`pttByte` → Byte, `pttSmallInt` → Int16, `pttFloat` → Float...) with the raw value (`ValueRaw`); `pttDefault` → the unsigned type of the driver word |
| `TTagBit` | `Boolean` for one bit (`StartBit = EndBit`); otherwise UInt16/32/64 by the number of bits |
| `TPLCString` | `String` |
| `TPLCBlock` | an array of its type, of its `Size` |
| `TPLCBlockElement` | the type of its block |
| `TPLCStruct` | an object with one variable per item (`TPLCStructItem` by its type, `TPLCStructString` as String) |

**Quality and timestamp.** Each value carries the status of the last read of the tag (the read of the source tag for bits and block elements): a PLC that stops answering makes the clients see a *Bad* status, not a frozen value. Memory and expression tags are always *Good*. The source timestamp is the time of the last update of the tag.

**Arrays.** A client can read and write a part of an array with an index range (`2:4`, `5`).

##### Writes

A write of a client to a `Writable` item:

1. checks the type and the limits of the tag (`BadOutOfRange` without touching the device);
2. converts the value (inverse scale with `odtScaledDouble`, the bits of a `TTagBit` put in its word, the encoding of a string);
3. writes the tag (`ScanWrite`) and **waits for the device**;
4. answers the client with the device's result.

Nothing blocks while the device answers: the server keeps serving the other clients. `WriteTimeoutMs` (default 0 = 2 minutes) limits the wait; the client then gets `BadTimeout`.

##### Security

| Property | Default | Description |
|---|---|---|
| `Security` | `ossNone` | `ossNone`: only without security; `ossNoneAndSecure`: without and with security (Sign, SignAndEncrypt); `ossSecureOnly`: data only over Sign/SignAndEncrypt. |
| `CertificateDir` | config folder of the app + `opcua-server-pki` | Certificate of the server (`server_cert.der`, `server_key.der`). Created self-signed on the first secure start and kept: the clients trust it once. Replace them by your own if you wish. |
| `TrustListDir` | empty | Client certificates accepted (`.der`, `.pem`, `.crt`, `.cer`); empty accepts any client. |
| `AllowAnonymous` | `True` | Whether a client may connect without a user. |
| `Users` | empty | Users that may log in, one per line: `login=password`. |
| `OnCheckUser` | – | Decides each login; `Accepted` arrives with the result of `Users`. Runs in the main thread: it may use the security manager of the application. |
| `OnCanWrite` | – | Decides each write, with the item and the user of the session (empty: anonymous). A refused write gets `BadUserAccessDenied`. |

* A user login needs a secure policy: the client encrypts the password with the server certificate. With `Users` or `OnCheckUser` and `Security = ossNone` the server does not start, and `LastError` says why.
* With `ossSecureOnly` the endpoint without security is still there, but **for discovery only**: the clients get the certificate of the server from it, and no session is opened on it.
* Logins (accepted and refused) and refused writes are logged; the password never is.

```pascal
procedure TForm1.OPCUAServer1CheckUser(Sender: TObject; const User, Password: String;
  var Accepted: Boolean);
begin
  Accepted := Accepted or MyUserCheck(User, Password);
end;

procedure TForm1.OPCUAServer1CanWrite(Sender: TObject; Item: TOPCUAServerItem;
  const User: String; var Allowed: Boolean);
begin
  //anonymous clients only read; operators do not change the setpoints of Line2
  Allowed := (User <> '') and not ((User = 'operator') and (Item.Folder = 'Line2'));
end;
```

##### Related examples

* `examples/laz_opcua_server` — publishes a small simulated process (a temperature that follows a setpoint while a pump is on, a message and a trend array) and shows on the form the values written by the clients.
* `examples/laz_opcua_client` — a client to try the server: connect it to `opc.tcp://localhost:4840`.
