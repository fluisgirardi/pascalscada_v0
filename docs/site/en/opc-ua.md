##### Introduction

![](img/TOPCUADriver.png) `TOPCUADriver` is an **OPC UA client**. It reads and writes the variables of any OPC UA server: the S7-1200/1500 and PLCSim servers, the server built into other PLC brands, an OPC UA gateway, a SCADA that exposes its tags, or a server written with open62541.

The protocol itself is handled by **open62541 1.5** (the C library), through the Pascal binding [fpopen62541](https://github.com/fluisgirardi/fpopen62541), which ships with PascalSCADA in `src/external/fpopen62541`. The library is loaded at run time, on the first connection only: a program that does not use the driver does not need it.

| | |
|---|---|
| Servers | any OPC UA server reachable by `opc.tcp://` |
| Addressing | the NodeId of the variable: `ns=3;s="DB1"."Speed"`, `nsu=urn:my:server;i=1001` |
| Values | subscriptions (the server reports the changes) or polling in batches |
| Security | None, Sign, SignAndEncrypt; anonymous or user/password |
| Tag Builder | browses the server and creates the tags |

> **One driver talks to one server.** To read two servers, use two `TOPCUADriver`.

##### Required components

1. A **`TOPCUADriver`** (unit `OPCUADriver`) with `EndpointUrl` = the server address, for example `opc.tcp://192.168.0.10:4840`. 4840 is the standard OPC UA port. There is no communication port component: the library opens the connection itself.
2. The tags (`TPLCTagNumber`, `TPLCBlock`, `TPLCString`) with `ProtocolDriver` pointing to the driver and `LongAddress` holding the NodeId of the variable (see *Addressing*).
3. The **open62541 1.5** library on the machine that runs the program (see the next section).

The fastest way to get to step 2 is the **Tag Builder** (see below), which browses the server and creates the tags.

##### The open62541 library

The driver needs open62541 **1.5** built as a shared library with encryption (`UA_ENABLE_ENCRYPTION=OPENSSL`); a library of another version, or without encryption, is refused at load time with a message saying why.

* **Linux:** build it from the sources and install it in the system:

```
git clone --branch v1.5.10 --recurse-submodules https://github.com/open62541/open62541.git
cmake -S open62541 -B open62541/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON \
      -DUA_ENABLE_ENCRYPTION=OPENSSL
cmake --build open62541/build -j
sudo cmake --install open62541/build && sudo ldconfig
```

* **Windows:** `libopen62541.dll`, of the same bitness as the program, next to the executable (and next to `lazarus.exe` to use the Tag Builder in the IDE). The workflow `BuildOpen62541v1.5.yml` of fpopen62541 builds it for Win32 and x64 with OpenSSL linked inside, so no OpenSSL DLL is needed.

With `LibraryName` empty, the driver looks for the library in this order: the system name (`libopen62541.so` / `libopen62541.dll`), `libopen62541.so.1.5` (Linux), and the same names in the folder of the executable — so the library can ship with the application. `LibraryName` with a path or a name skips the search. When nothing is found, `LastError` lists every place tried.

##### Preparing the server

On an **S7-1200/1500** (or PLCSim Advanced), in TIA Portal:

1. CPU properties → *OPC UA* → *Server*: tick *Activate OPC UA server*. The endpoint shown there is the `EndpointUrl` (port 4840 by default). The CPU also needs an OPC UA runtime license selected under *Runtime licenses*.
2. Only the variables marked *Accessible from HMI/OPC UA* (and *Writable from HMI/OPC UA* to write) are published.
3. *Server* → *Security*: the security policies the server accepts and whether guest (anonymous) access is enabled. Without *No security* in the list, the driver must use Sign or SignAndEncrypt and the server must trust the client certificate (see *Security*).

The NodeIds of the variables of a Siemens CPU look like `ns=3;s="DB1"."Speed"` — with the quotation marks.

##### Driver properties

| Property | Default | Description |
|---|---|---|
| `EndpointUrl` | – | Server address, `opc.tcp://host:port`. |
| `Username` / `Password` | empty | Empty `Username`: anonymous session. |
| `SecurityMode` | `osmNone` | `osmNone`, `osmSign` (signed) or `osmSignAndEncrypt` (signed and encrypted). |
| `SecurityPolicy` | `ospAuto` | `ospAuto` takes the best one the server offers for the mode; or a fixed one: `ospBasic256Sha256`, `ospAes128Sha256RsaOaep`, `ospAes256Sha256RsaPss`, `ospBasic256`, `ospBasic128Rsa15`. |
| `CertificateFile` / `PrivateKeyFile` | empty | Client certificate and key (DER or PEM). Empty: one is generated on the first secure connection and kept in `CertificateDir`. |
| `CertificateDir` | config folder of the app + `opcua-pki` | Where the generated certificate is kept: `own/pascalscada-client.der` and `private/pascalscada-client.key`. |
| `TrustListDir` | empty | Folder with the certificates (`.der`, `.pem`, `.crt`, `.cer`) of the trusted servers. **Empty accepts any server.** |
| `ApplicationUri` / `ApplicationName` | `urn:pascalscada:opcua:client` / `PascalSCADA OPC UA Client` | How the client introduces itself. The URI goes in the generated certificate. |
| `AllowUnencryptedPassword` | `False` | Lets the password travel on a channel without encryption (`osmNone`). Keep it off outside of tests. |
| `Timeout` | 5000 | Timeout of a request, in ms. |
| `SessionTimeout` | 60000 | Session lifetime asked to the server, in ms. |
| `ReconnectInterval` | 3000 | Wait between two connection attempts, in ms. |
| `UseSubscriptions` | `True` | Values by subscription; `False` reads the tags periodically (see *Subscriptions or polling*). |
| `PublishingInterval` | 100 | How often, in ms, the server sends the changes of the subscription. |
| `MaxItemsPerRequest` | 500 | Most variables in one read, write or monitored item request; more are split in several requests. |
| `LibraryName` | empty | Name or path of the library; empty searches it (see above). |
| `LogLevel` / `OnLog` | `ollWarning` | Messages of the driver and of the library. `OnLog` is called from the driver threads: do not touch the form inside it. |
| `ReadOnly` | `False` | No write reaches the server. |

Methods: `Connected`, `LastError`, `ListTags`, `Browse`, `GetVariableInfo`, `GetServerCertificates`, `GetClientCertificate` (see *Browsing from code*).

##### Addressing

`LongAddress` is the **NodeId** of the variable, as text:

```
ns=3;s="DB1"."Speed"                   // namespace index + string identifier (Siemens)
ns=2;i=1001                            // numeric identifier
ns=2;g=09087e75-8e5e-499b-954f-f2a9603db28a   // GUID
nsu=http://www.siemens.com/simatic-s7-opcua;s="DB1"."Speed"   // namespace URI
```

`ns=` uses the index of the namespace in the server; `nsu=` uses its URI, which does not change if the server reorders its namespaces — the Tag Builder writes `nsu=` by default. Namespace 0 is the standard one: `i=2258` is the current time of the server.

A `|` followed by an **IndexRange** picks elements of an array: `ns=3;s="DB1"."Arr"|4` is element 4, `ns=3;s="DB1"."Arr"|0:9` the first ten. Indexes start at 0, whatever the array bounds are in the PLC program.

##### Tags and types

The server tells the type of each variable, so the driver converts the value itself: keep `TagType = pttDefault`, with no swaps and no scale tricks.

| OPC UA type | Tag | Value |
|---|---|---|
| Boolean | `TPLCTagNumber` | 0 / 1; writing any value other than 0 writes `true` |
| SByte, Byte, Int16, UInt16, Int32, UInt32, Int64, UInt64 | `TPLCTagNumber` | the number. 64-bit integers above 2^53 lose precision (the tag value is a Double). |
| Float, Double | `TPLCTagNumber` | the number |
| DateTime | `TPLCTagNumber` | a `TDateTime` in local time (format it with `FormatDateTime`) |
| Enumeration, StatusCode | `TPLCTagNumber` | its numeric value |
| array of any of the above | `TPLCBlock` (`Size` = number of elements) | one value per element |
| String, LocalizedText, QualifiedName, ByteString, Guid, NodeId | `TPLCString` (`StringType = stC`) | the text in UTF-8 |

A write that does not fit the type of the variable (300 into a Byte, a text that is not a number into a numeric variable) is refused with `ioIllegalValue`, without reaching the server. Writing one element of a `TPLCBlock` writes only that element (by IndexRange); if the server does not accept an IndexRange write, the driver reads the array, changes the element and writes it whole.

Structures (user data types) are not decoded: address their members one by one.

##### Subscriptions or polling

With `UseSubscriptions = True` (the default) the driver creates one subscription and a monitored item per variable: the server samples the variable every `RefreshTime` of the tag and sends only what changed, at most every `PublishingInterval`. A tag with `RefreshTime = 0` asks the server for its fastest sampling. Several tags on the same NodeId share one monitored item with the fastest `RefreshTime`.

With `UseSubscriptions = False` the variables are read periodically, each at its `RefreshTime`, in batches of up to `MaxItemsPerRequest`. Use it with servers that do not support subscriptions, or to have every value read at a fixed rate even when it does not change.

When the connection drops, every tag gets the error; the driver tries again every `ReconnectInterval` and, once connected, recreates the subscription by itself.

##### Security

* **None** (`osmNone`): nothing is signed or encrypted. A password is only sent this way with `AllowUnencryptedPassword = True`.
* **Sign** / **SignAndEncrypt**: the messages are signed (and encrypted). Both sides present certificates and each one has to trust the other:
    * **The server trusts the client.** With `CertificateFile` empty the driver generates a self-signed certificate on the first secure connection and keeps it in `CertificateDir`. Give `own/pascalscada-client.der` to the server as a trusted client — in TIA Portal it goes to the trusted clients of the OPC UA server (*Server* → *Security* → *Secure channel*), unless the server is set to accept client certificates automatically; other servers usually keep a refused certificate in a *rejected* folder, to be moved to *trusted*. The Tag Builder has an **Export client certificate** button for this. The certificate is kept, so it is trusted once.
    * **The client trusts the server.** With `TrustListDir` empty, any server is accepted. To accept only known servers, put their certificates in that folder — the **Trust server certificate** button of the Tag Builder saves the one of the current server there.
* **User and password**: `Username` and `Password`. A wrong password ends with `ioAuthError`.

##### Tag Builder

Right-click the `TOPCUADriver` → **Tag Builder** opens *OPC UA Tag Builder*.

1. Press **Browse**: the tree shows the Objects folder of the server; folders and objects expand on demand.
2. Select the variables (Ctrl/Shift for several). Set **Scan** (the `RefreshTime` of the new tags) and an optional name **Prefix**. *Address with the namespace URI (nsu=)* chooses between `nsu=` (default) and `ns=` addresses.
3. **OK** creates one tag per variable: `TPLCTagNumber` for a numeric scalar, `TPLCBlock` (with `Size` already set) for a numeric array and `TPLCString` for a text. Structures and other types without an equivalent are skipped and counted in a message.

The buttons **Trust server certificate** (saves the server certificate in `TrustListDir`) and **Export client certificate** are in the same window (see *Security*).

##### Browsing from code

```pascal
var
  tags: TPLCTagInfoArray;
  info: TOPCUAVariableInfo;
  i: Integer;
begin
  if OPCUADriver1.ListTags(tags) = ioOk then      // every variable under Objects
    for i := 0 to High(tags) do
      Memo1.Lines.Add(tags[i].Path + '  ' + tags[i].NativeType);

  if OPCUADriver1.GetVariableInfo('ns=3;s="DB1"."Speed"', info) = ioOk then
    Memo1.Lines.Add(Format('%s rank %d', [info.DataTypeName, info.ValueRank]));
end;
```

`ListTags` walks the Objects folder recursively, up to 12 levels and 20000 variables (`Path` is the NodeId with `nsu=`, ready for `LongAddress`); `Browse` returns the children of one node; `GetVariableInfo` gives the type, the ValueRank (-1 scalar, ≥ 0 array), the dimensions and the access level of a variable. They connect by themselves when needed.

##### Step-by-step example

1. Drop a `TOPCUADriver` and set `EndpointUrl` (e.g. `opc.tcp://192.168.0.10:4840`).
2. Right-click it → **Tag Builder** → **Browse**, select some variables and press **OK**.
3. Drop HMI controls (`THMILabel`, `THMIEdit`, `THMITrackBar`, …) and point their `PLCTag` at the new tags.
4. Run. The values show up; typing in a `THMIEdit` writes back to the server.

To try it without a server, `examples/laz_opcua_hmi` starts a small test server inside the program.

##### Errors

The OPC UA status codes become the usual `TProtocolIOResult` of the tags:

| Status code (examples) | Result |
|---|---|
| Good, Uncertain | `ioOk` |
| BadNodeIdUnknown, BadNodeIdInvalid, BadNotFound | `ioObjectNotExists` |
| BadUserAccessDenied, BadNotReadable, BadNotWritable | `ioObjectAccessNotAllowed` |
| BadTypeMismatch, BadOutOfRange | `ioIllegalValue` |
| BadIndexRangeInvalid, BadIndexRangeNoData | `ioIllegalMemoryAddress` |
| BadTimeout | `ioTimeOut` |
| BadIdentityTokenInvalid/Rejected, wrong password | `ioAuthError` |
| BadCertificate…, BadSecurityChecksFailed, BadConnectionRejected | `ioConnectPLCFailed` |
| connection or session lost | `ioCommError` |

##### Common problems

| Symptom | Likely cause |
|---|---|
| `LastError`: *libopen62541 1.5 not found* | The library is not installed or not next to the executable; see *The open62541 library*. |
| *is not an open62541 1.5 library* / *was built without encryption* | A library of another version, or built without `UA_ENABLE_ENCRYPTION`. |
| Every tag `ioConnectPLCFailed` with Sign/SignAndEncrypt | The server does not trust the client certificate yet: export it and add it to the server's trusted list. Or `TrustListDir` is set and does not hold the server certificate. |
| Every tag `ioAuthError` | Wrong user/password, or the server does not allow anonymous access. |
| `ioObjectNotExists` on a Siemens variable | NodeId without the quotation marks (`s="DB1"."Speed"`), or the variable is not *Accessible from HMI/OPC UA*. |
| Writes refused (`ioObjectAccessNotAllowed`) | The variable is not *Writable from HMI/OPC UA*, the user may not write, or `ReadOnly = True`. |
| A fast signal is seen late | Lower `RefreshTime` (sampling) and `PublishingInterval`, or use polling. |
| A tag reads another variable (or none) after the server was updated | The server reordered its namespaces and the `ns=` index changed: use `nsu=` addresses. |

##### Related examples

* `examples/laz_opcua_client` — connects to any endpoint, lists the variables of the server, monitors and writes the ones picked; *Test server* starts a local server to try it without a PLC.
* `examples/laz_opcua_hmi` — an HMI screen made only on the form: `TOPCUADriver`, tags and HMI controls set at design time, with the instructions on the form.
