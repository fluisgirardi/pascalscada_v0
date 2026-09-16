[toc]

##### Required knowledge

* Data communication;
* The physical medium your protocol will use (serial, TCP/IP, UDP);
* Component development in Delphi/Lazarus;
* The protocol being implemented — with the specification at hand, or at least traffic captures (Wireshark) from a real device.

##### Definitions

* **Update scan**: cyclic method that updates the tag value with the last value read by the protocol driver. It runs on a thread of the driver and delivers the values to the tags on the main thread.
* **Read scan**: cyclic method that sweeps the memory areas most in need of an update, encodes the data packet to send to your device, sends it through the communication port, waits for the port's answer, receives the data, decodes it and updates the internal data areas with the value and the date/time of the last read.
* **Write scan**: cyclic method that serves the write orders queued by the tags (`ScanWrite`, or an assignment to `Value` with `AutoWrite = True`), executing them asynchronously — the application goes on while the write travels to the device, and the result comes later through the tag events.
* **Synchronous read**: a read where the read scan is paused to free the communication port and the application reads the tag directly (`Tag.Read`), blocking until the answer arrives.
* **Synchronous write**: a write where the read scan is paused the same way and the application writes directly (`Tag.Write`), blocking until the device confirms.

##### Introduction

Firstly, I'll explain my idea of a protocol driver and how I implemented it, with small pieces of code as an example of each part. A protocol driver is the object that manages the tags in your application. It updates the tags respecting their scan rate, organizes them in blocks and avoids a memory being read twice (or more) in a single cycle. Its internal organization can vary from one protocol to another, which may read a single tag or a block of tags per request. Everything in the name of performance.

The tags in PascalSCADA are **copies of values stored in another area, managed by the driver**. It is through this internal area that the driver manages its read scan. The organization of this area varies with the device's memory organization. The tags, in turn, may represent one or more memory addresses of your device.

What the base class `TProtocolDriver` already does for you:

```
                  ┌──────────────────────── TProtocolDriver ────────────────────────┐
 tags ──AddTag──▶ │ tag list                                                        │
                  │                                                                 │
                  │ TScanThread  ──▶ DoScanRead ──▶ DoRead ──▶ port ──▶ device      │
                  │                       (your code: builds and decodes frames)    │
                  │                                                                 │
                  │ TScanUpdate  ──▶ DoGetValue ──▶ Synchronize ──▶ Tag.Value       │
                  │                       (your code: copies from the internal area)│
                  └─────────────────────────────────────────────────────────────────┘
```

* creates and keeps the two threads (`TScanThread`, which calls `DoScanRead` in a loop, and `TScanUpdate`, which calls `DoGetValue` for each tag and delivers the values on the main thread);
* implements the `Read`/`Write`/`ScanRead`/`ScanWrite` called by the tags, pausing the scan and calling your `DoRead`/`DoWrite`;
* handles the synchronization between the threads (critical sections, scan pause) — you never need a mutex inside the `Do*` methods;
* handles the `CommunicationPort` property, `ReadOnly`, the counters and the Object Inspector's `LiteralTagAddress`.

What is left for you: **seven methods** describing your device and your protocol. That is what the rest of this page shows.

To demonstrate, I'll use a **fictitious PLC** with a 16-bit register area and digital inputs and outputs addressed in bytes (8 bits), all areas supporting block read/write (update several tags in a single request), and simple PLC addressing from 1 to 255 (like Modbus).

##### Step 1: the unit and the device structure

The first step is to create a unit to hold your new protocol and add it to the PascalSCADA package (see *Registering the driver* below). In that unit, the representation of our fictitious PLC would be something like this:

```pascal
uses ProtocolDriver, ProtocolTypes, PLCMemoryManager, Tag, PLCBlock, commtypes;

type
  TDummyPLC = record
    PLCAddress: Byte;
    Inputs:    TPLCMemoryManager;  // represented by MemReadFunction = 1 in our tags
    Outputs:   TPLCMemoryManager;  // represented by MemReadFunction = 2 in our tags
    Registers: TPLCMemoryManager;  // represented by MemReadFunction = 3 in our tags
  end;
  TDummyPLCs = array of TDummyPLC;
```

Note the class `TPLCMemoryManager`. What does it do? It organizes memories of the same area into continuous blocks as large as possible, avoiding duplicated memories and optimizing your communication. Two properties control its whole behaviour:

* **`MaxHole`**: how many memories may be missing and still keep a single block. Say the addresses [1, 2, 5, 6] are added. With `MaxHole = 0` two blocks are formed, [1, 2] and [5, 6]. With `MaxHole = 2` a single block [1, 2, 3, 4, 5, 6] is formed. Addresses 3 and 4 are added to keep the block continuous, so it is read with a single request by the driver, improving performance.
* **`MaxBlockItems`**: the maximum block size. If [1, 2, 3, 4, 5, 6] are added with `MaxBlockItems = 3`, two blocks are formed, [1, 2, 3] and [4, 5, 6]. Useful for protocols that limit the request size, such as Modbus (125 registers per read). `0` means no limit.

The methods you will use on it:

| Method | Does |
|---|---|
| `AddAddress(Address, Size, RegSize, ScanTime)` | Registers `Size` memories starting at `Address`, with a scan of `ScanTime` ms. `RegSize` is the variable size relative to the smallest word of the area: to add MW0, MW2 and MW4 of a Siemens (smallest word = byte) use `AddAddress(0, 3, 2, 1000)`. The resulting block gets the **smallest** `ScanTime` of the tags composing it. |
| `RemoveAddress(Address, Size, RegSize)` | The inverse. |
| `Blocks[i]` | The continuous blocks formed, each with `AddressStart`, `Size`, `ScanTime`, `LastUpdate` and `NeedRefresh` (true once the scan time has elapsed). |
| `SetValues(Address, Len, RegSize, Values, LastResult)` | Stores the values read from the device and stamps the time. |
| `SetFault(Address, Len, RegSize, Fault)` | Marks that the read failed — the tags receive the `TProtocolIOResult` and fire `OnReadFail`. |
| `GetValues(Address, Len, RegSize, Values, LastResult, Timestamp)` | Copies values, result and time to hand to a tag. |

Note the comment on each memory area. I chose the tag's `MemReadFunction` property to select the data area (inputs, outputs and registers): 1 identifies the digital inputs, 2 the digital outputs and 3 the registers. Where this mapping is tied to the memory area comes a bit further on.

With the structure representing your device done, the second step is to create a class derived from `TProtocolDriver`:

```pascal
type
  TDummyProtocol = class(TProtocolDriver)
  private
    FPLCs: TDummyPLCs;
  protected
    procedure DoAddTag(TagObj: TTag; TagValid: Boolean); override;
    procedure DoDelTag(TagObj: TTag); override;
    procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt); override;
    procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec); override;
    function  DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult; override;
    function  DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult; override;
  public
    destructor Destroy; override;
    function SizeOfTag(aTag: TTag; isWrite: Boolean; var ProtocolTagType: TProtocolTagType): BYTE; override;
  published
    property ReadSomethingAlways;
  end;
```

##### Step 2: adding tags to the scan — DoAddTag

In this new protocol you must say how tags are added to the driver's scan to be read. How? Simple. Override the method:

```pascal
procedure DoAddTag(TagObj: TTag; TagValid: Boolean);
```

It adds tags to the memory area managed by the driver. Do the tag checks here (address range, tag class) and, if the tag is valid, call the inherited method with

```pascal
inherited DoAddTag(TagObj, TheTagIsValid);
```

to add the tag to the base class so it gets updated at its scan rate. The second parameter matters: the base class marks the tag as valid or invalid, and an invalid tag stays in the list but is never read — that is what happens when the user fills in a `MemReadFunction` the driver does not know. Call `inherited` **always**, even with `False`; if you don't, the tag is not registered and `DoDelTag` will not find it later.

In this example I'll handle block tags only. With our fictitious PLC the method looks like this:

```pascal
procedure TDummyProtocol.DoAddTag(TagObj: TTag; TagValid: Boolean);
var
  Valid, plcFound: Boolean;
  plc: Integer;
begin
  Valid := False;
  plcFound := False;

  // as I said, only block tags are handled in this example.
  if TagObj is TPLCBlock then
    with TagObj as TPLCBlock do begin
      // check whether the PLC address is in the accepted range (1 to 255)
      // and, if so, whether that PLC is already registered in the
      // memory area managed by the driver.
      if PLCStation in [1..255] then
        for plc := 0 to High(FPLCs) do
          if FPLCs[plc].PLCAddress = PLCStation then begin
            plcFound := True;
            Break;
          end;

      // PLC not found: add it.
      if (not plcFound) and (PLCStation in [1..255]) then begin
        plc := Length(FPLCs);
        SetLength(FPLCs, plc + 1);
        FPLCs[plc].PLCAddress := PLCStation;
        FPLCs[plc].Inputs    := TPLCMemoryManager.Create;
        FPLCs[plc].Outputs   := TPLCMemoryManager.Create;
        FPLCs[plc].Registers := TPLCMemoryManager.Create;
        FPLCs[plc].Registers.MaxBlockItems := 125;   // our protocol's limit
        plcFound := True;
      end;

      // is the tag valid (bound to some area)?
      if plcFound and (MemReadFunction in [1..3]) then
        Valid := True;

      // add the tag to the memory area managed by the driver
      if Valid then
        case MemReadFunction of
          1: FPLCs[plc].Inputs.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
          2: FPLCs[plc].Outputs.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
          3: FPLCs[plc].Registers.AddAddress(MemAddress, TagSizeOnProtocol, 1, RefreshTime);
        end;
    end;

  // add the tag to the base class
  inherited DoAddTag(TagObj, Valid);
end;
```

`TagSizeOnProtocol` is the tag size **in protocol words** — a `TPLCBlock` with `Size = 10` and `TagType = pttFloat` over 16-bit registers spans 20 words. The tag computes it from your `SizeOfTag` (step 7), which is why it, and not `Size`, goes to the manager.

Done: your tag is validated and added to the scan of your protocol driver. But this method does not read the tag values by itself. It could — but it would be inefficient when performance is the goal, because the base class does not know the driver structure: it would have to request tag by tag, and a duplicated tag (same memory of the same PLC) would be requested twice. For an improved scan routine, two more methods must be overridden.

##### Step 3: handing values to the tags — DoGetValue

```pascal
procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
```

Remember that tags are copies of a memory area managed by the driver? This is the method that makes the copy. It is called by the update thread for each tag, at the tag's rate. Implementing it is simple: look up the memory in the protocol's internal organization. For our fictitious driver:

```pascal
procedure TDummyProtocol.DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
var
  plc: Integer;
begin
  // sweep the PLCs and, on the one requested, ask the block manager
  // of the desired area for the tag values.
  for plc := 0 to High(FPLCs) do
    if FPLCs[plc].PLCAddress = TagRec.Station then
      case TagRec.ReadFunction of
        1: FPLCs[plc].Inputs.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                       values.LastQueryResult, values.ClkMonotonicTStamp);
        2: FPLCs[plc].Outputs.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                        values.LastQueryResult, values.ClkMonotonicTStamp);
        3: FPLCs[plc].Registers.GetValues(TagRec.Address, TagRec.Size, 1, values.Values,
                                          values.LastQueryResult, values.ClkMonotonicTStamp);
      end;
end;
```

`TTagRec` is a snapshot of the tag properties at request time: `Station`, `Address`, `Size`, `ReadFunction`, `WriteFunction`, `File_DB`, `SubElement`, `Rack`, `Slot`, `Path` (the `LongAddress`), `UpdateTime`, `Retries` and the `CallBack` that returns the result to the tag. `DoGetValue`, `DoRead` and `DoWrite` work with it — not with the tag object — because they run on the driver threads.

##### Step 4: the read scan — DoScanRead

Fine, but who reads the data from my device and updates those memory areas? This method does:

```pascal
procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt);
```

It is called in a loop by the scan thread to check whether some tag needs to be read, sweeping the memory area managed by the driver. At the end, tell the scan thread what was done through `NeedSleep`:

* `NeedSleep = 0`: the thread carries on normally. Return 0 whenever the driver performed some I/O, since I/O acts as a "natural delay" and avoids high CPU usage.
* `NeedSleep < 0`: the scan thread forces a thread context switch (`ThreadSwitch`) and returns as soon as possible. You may return this whenever your driver did nothing.
* `NeedSleep > 0`: the thread sleeps `NeedSleep` milliseconds. You may return this whenever your driver did nothing.

> The most common mistake of a new driver is returning `0` without having done any I/O: the scan thread becomes a tight loop and the CPU goes to 100 %. If there was nothing to read, return `1` (or the time until the next block is due).

The scan routine of our fictitious driver looks like this (note it uses `DoRead`, presented in the next step):

```pascal
procedure TDummyProtocol.DoScanRead(Sender: TObject; var NeedSleep: LongInt);
var
  plc, block: Integer;
  tagrec: TTagRec;
  ReadResult: TProtocolIOResult;
  Values: TArrayOfDouble;
  didSomething: Boolean;
begin
  didSomething := False;
  NeedSleep := 0;

  // without an active port there is nothing to do: sleep a bit instead of spinning
  if (PCommPort = nil) or (not PCommPort.ReallyActive) then begin
    NeedSleep := 1;
    Exit;
  end;

  // sweep the PLCs
  for plc := 0 to High(FPLCs) do begin
    // sweep the blocks formed from the digital inputs
    for block := 0 to High(FPLCs[plc].Inputs.Blocks) do
      // if the block needs an update...
      if FPLCs[plc].Inputs.Blocks[block].NeedRefresh then begin
        // fill the TTagRec fields that matter to the protocol (or to DoRead)
        tagrec.Station      := FPLCs[plc].PLCAddress;
        tagrec.ReadFunction := 1;
        tagrec.Address      := FPLCs[plc].Inputs.Blocks[block].AddressStart;
        tagrec.Size         := FPLCs[plc].Inputs.Blocks[block].Size;

        // read through DoRead
        ReadResult := DoRead(tagrec, Values, False);
        if ReadResult = ioOk then
          FPLCs[plc].Inputs.SetValues(tagrec.Address, tagrec.Size, 1, Values, ReadResult)
        else
          FPLCs[plc].Inputs.SetFault(tagrec.Address, tagrec.Size, 1, ReadResult);
        didSomething := True;
      end;

    // repeat the above for the digital outputs and the registers...
  end;

  if not didSomething then
    NeedSleep := 1;
end;
```

Two refinements the PascalSCADA drivers make, worth it once the number of blocks grows:

* **Read one block per call, the most overdue first.** Instead of reading every due block at once (which delays the fast tags while the slow ones are read), the Modbus driver builds the list of all blocks, sorts it by lateness and reads only the first; the thread calls `DoScanRead` again right after. A 100 ms tag then never waits for twenty 5 s blocks.
* **`ReadSomethingAlways`**: when no block is due, reading the oldest one anyway keeps the connection alive and detects a dead device early. Publish the inherited property and honour it in your scan.

##### Step 5: reading from the device — DoRead

To avoid building request packets twice, override

```pascal
function DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
```

because it performs the synchronous reads (requested by the tag's `Read`) and the asynchronous ones (requested by the driver's scan). Building the request packet and decoding the received packet with the memory values happens here. `tagrec` carries the tag information (device address, memory address), and `Values` receives the decoded values. The `Sync` parameter is no longer used.

The return is a `TProtocolIOResult`: `ioOk`, `ioTimeOut`, `ioCommError` (corrupted answer), `ioIllegalFunction`/`ioIllegalRegAddress`/`ioIllegalValue` (the device refused), `ioPLCError`, `ioDriverError`, `ioNullDriver` (no active port)… Pick the most specific one: it reaches the tag in `LastSyncReadStatus` and is what the user sees when diagnosing.

This is where the driver talks to the **communication port**. The port (`TCommPortDriver`, be it `TSerialPortDriver` or `TTCP_UDPPort`) knows nothing about the protocol — it only carries bytes — and offers a synchronous operation:

```pascal
function IOCommandSync(Cmd: TIOCommand;             // iocWrite, iocRead or iocWriteRead
                       BytesToWrite: Cardinal; ToWrite: BYTES;
                       BytesToRead: Cardinal;      // how many bytes of answer to expect
                       DriverID, DelayBetweenCmds: Cardinal;
                       pkt: PIOPacket;             // result
                       OnBegin: TNotifyEvent = nil; OnEnd: TNotifyEvent = nil): Cardinal;
```

The returned `TIOPacket` carries `WriteIOResult`/`ReadIOResult` (`iorOK`, `iorTimeOut`, `iorPortError`…), `Received` and `BufferToRead`. Since a port may be shared by several drivers, wrap the transaction in `PCommPort.Lock(DriverID)` / `Unlock(DriverID)` — `DriverID` is a unique number the base class already gave your driver. The pattern every PascalSCADA driver follows:

```pascal
function TDummyProtocol.DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
  Sync: Boolean): TProtocolIOResult;
var
  request: BYTES;
  pkt: TIOPacket;
  remaining: LongInt;
begin
  Result := ioNullDriver;
  if (PCommPort = nil) or (not PCommPort.ReallyActive) then Exit;

  request := BuildReadRequest(tagrec);            // your protocol: header, function, address, CRC...

  PCommPort.Lock(DriverID);
  try
    // 1) send the request and read the answer header (fixed size)
    if PCommPort.IOCommandSync(iocWriteRead, Length(request), request, HEADER_SIZE,
                               DriverID, 0, @pkt) = 0 then begin
      Result := ioDriverError;
      Exit;
    end;
    if pkt.ReadIOResult = iorTimeOut then begin Result := ioTimeOut;  Exit; end;
    if pkt.ReadIOResult <> iorOK      then begin Result := ioCommError; Exit; end;

    // 2) the header says how many bytes are still coming: read the rest
    remaining := RemainingBytes(pkt.BufferToRead);
    if remaining > 0 then begin
      if PCommPort.IOCommandSync(iocRead, 0, nil, remaining, DriverID, 0, @pkt2) = 0 then ...
      pkt.BufferToRead := ConcatenateBYTES(pkt.BufferToRead, pkt2.BufferToRead);
    end;

    // 3) validate (CRC, function echo, exception code) and convert to Double
    Result := DecodeAnswer(pkt.BufferToRead, tagrec, Values);
  finally
    PCommPort.Unlock(DriverID);
    SetLength(request, 0);
    SetLength(pkt.BufferToRead, 0);
  end;
end;
```

The two-step read — fixed-size header, then the rest — is what lets the driver know how many bytes to expect without guessing; a fixed-size protocol needs a single call. If the answer comes from another function or another station (noise on a serial line), drain the port with 1-byte `iocRead`s until a timeout, as Modbus does, otherwise the next request reads the leftovers.

The values in `Values` are always `Double`, **one protocol word per position**: for a block of 4 16-bit registers, `Values` has 4 elements in 0..65535. The tag is the one assembling `pttFloat`, `pttLongInt` etc. from those words, using the size you reported in `SizeOfTag` — the driver does not convert types.

**Separate frame building from the I/O flow.** The Modbus and Melsec drivers put the flow above in a family base class and leave only two virtual methods to the subclasses, `EncodePkg` (TTagRec → bytes) and `DecodePkg` (bytes → values + `SetValues`/`SetFault`). That is how `TModBusRTUDriver` and `TModBusTCPDriver` share 90 % of the code and differ only in header and CRC. Do the same from the start if your protocol has serial/TCP variants.

##### Step 6: writing to the device — DoWrite

I can read my tags; how do I write values to the device? `DoWrite` is for that — override it, build the packet and send it:

```pascal
function DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
```

This is the method performing the synchronous writes (tag with `AutoWrite = False` and a later call to `Write`) and the asynchronous ones (a value assigned to `Value` with `AutoWrite = True`). `tagrec` carries the tag information (device address, memory address — use `WriteFunction`, not `ReadFunction`), and `Values` the values to write. The `Sync` parameter is no longer used.

The flow is the same as `DoRead`: build, `Lock`, `IOCommandSync`, validate the confirmation, `Unlock`. After a successful write, also update the internal area with `SetValues` — so other tags pointing to the same memory show the new value before the next scan. If the driver has `ReadOnly = True`, the base class already refuses the write with `ioReadOnlyProtocol` before it gets here.

##### Step 7: word sizes — SizeOfTag

But my device has registers as words while digital inputs and outputs are addressed as bytes. How do I say that? Simple. Tags do a lot of type conversions, but the protocol must tell what kind of data it is providing to the tag so it can convert. Override

```pascal
function SizeOfTag(aTag: TTag; isWrite: Boolean; var ProtocolTagType: TProtocolTagType): BYTE;
```

which reports the size in bits of the word the tag references and its type (`ptBit`, `ptByte`, `ptShortInt`, `ptWord`, `ptSmallInt`, `ptDWord`, `ptLongInt`, `ptFloat`, `ptInt64`, `ptQWord`, `ptDouble`). Return the size in `Result` and the type in `ProtocolTagType`. `isWrite` allows different sizes for reads and writes. In our fictitious driver (again block tags only; the other tag classes are up to you), assuming digital inputs and outputs are bytes and registers are words:

```pascal
function TDummyProtocol.SizeOfTag(aTag: TTag; isWrite: Boolean;
  var ProtocolTagType: TProtocolTagType): BYTE;
begin
  Result := 0;
  ProtocolTagType := ptUnknown;
  if aTag is TPLCBlock then
    case TPLCBlock(aTag).MemReadFunction of
      1, 2: begin
        Result := 8;
        ProtocolTagType := ptByte;
      end;
      3: begin
        Result := 16;
        ProtocolTagType := ptWord;
      end;
    end;
end;
```

The tag consults this value when computing `TagSizeOnProtocol` and when assembling `TagType`s larger than the protocol word (two 16-bit registers become one `pttFloat`). If you report 16 bits but deliver bytes in `Values`, the values come out wrong.

##### Step 8: removing tags — DoDelTag

I've read enough. I want to delete my tags, close this page and leave… Easy: for your tag to be deleted and removed from the driver's scan, override one last method:

```pascal
procedure DoDelTag(TagObj: TTag);
```

Override it to remove tags from the driver's scan. Don't forget to call the inherited method,

```pascal
inherited DoDelTag(TagObj);
```

to remove the tag from the base class. The code is very similar to `DoAddTag`, except that the tag is removed from the area managed by the driver (`RemoveAddress` with the same parameters as `AddAddress`). It is called when the tag is destroyed, when it changes `ProtocolDriver` and — importantly — **whenever any addressing property changes**: the tag removes itself with the old address and adds itself again with the new one. That is why `DoAddTag`/`DoDelTag` must be symmetrical.

In the driver's `destructor`, free the `TPLCMemoryManager`s you created.

**Keep in mind that your driver needs a communication port set and active for the tags to be updated. This is a condition for the protocol driver to update the tags.**

##### Registering the driver

With the seven methods written, the driver still has to show up on the palette:

1. **Unit in the package**: add the unit to `pascalscada.lpk` (or to your own package depending on `pascalscada`).
2. **Palette registration**: in `src/scada_dsng/scadareg.pas`, include the class in the `RegisterComponents(strProtocolsPallete, [...])` call. In your own package, write a `procedure Register` with the same call.
3. **Icon**: a 24×24 PNG named `TDummyProtocol.png` in `artwork/24x24png/` (and the SVG in `artwork/scalable/`); run `artwork/generateres.sh` to regenerate the resource file.
4. **Tag Builder (optional)**: the wizard that creates tags from the driver's context menu. Since it has forms (LCL), it lives in the design package (`scada_dsng`), not in the driver unit: the driver exposes `HasTabBuilderEditor`/`OpenTagEditor` and a global `SetTagBuilderToolFor…ProtocolFamily` that the design package fills at initialization — see `modbustagassistant.pas` and `siemenstagassistant.pas` as models. Without it the driver works normally, it just lacks the menu item.
5. **`LiteralTagAddress`** (optional): override it so the Object Inspector shows the address in your protocol's notation (`DB5.DBW20`, `40001`) in the tag hint.

##### Testing without the device

You don't need a PLC to develop the driver: the test suite has a **fake port** (`tests/testsupport.fakeport.pas`, `TFakeCommPort`) that records what the driver wrote and replays queued answers. A read test looks like this (adapted from `tests/ut.modbustcp.pas`):

```pascal
// DoRead is protected: a "probe" exposes it to the test and disables the
// scan thread, so that only the test uses the fake port.
type
  TDummyProtocolProbe = class(TDummyProtocol)
  protected
    procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt); override;  // NeedSleep := 1
  public
    function ReadSync(const aTag: TTagRec; out aValues: TArrayOfDouble): TProtocolIOResult;
  end;

function TDummyProtocolProbe.ReadSync(const aTag: TTagRec; out aValues: TArrayOfDouble): TProtocolIOResult;
begin
  Result := DoRead(aTag, aValues, True);
end;

procedure TTestDummyProtocol.SetUp;
begin
  FPort := TFakeCommPort.Create(nil);
  FPort.Active := True;
  FDrv := TDummyProtocolProbe.Create(nil);
  FDrv.CommunicationPort := FPort;
end;

procedure TTestDummyProtocol.AReadGoesOutAndBackThroughThePort;
var
  res: TProtocolIOResult;
  vals: TArrayOfDouble;
begin
  // what the "device" would answer
  FPort.QueueResponse(BytesOf('01 03 04 00 0A 00 14'));

  res := FDrv.ReadSync(TagRecFor(1, 3, 16, 0, 2), vals);   // station, read function, write function, address, size

  AssertEquals('result', Ord(ioOk), Ord(res));
  AssertBytesEqual('the request that went out', BytesOf('01 03 00 00 00 02'), FPort.LastWrittenFrame);
  AssertEquals('two values', 2, Length(vals));
  AssertEquals('first register', 10, vals[0], 0);
  AssertEquals('answer fully consumed', 0, FPort.PendingResponses);
end;
```

`TagRecFor`, `BytesOf` and `AssertBytesEqual` come from `tests/testsupport.protocol.pas` and `tests/testsupport.bytes.pas`. `QueueTimeout` simulates a silent device, to test the `ioTimeOut` path; `WrittenFrame(i)` inspects each request when the driver makes several exchanges per read. The tests run with `tests/pascalscada_tests.lpi`. Write one test per protocol function and one per error code **before** connecting to the real device — almost every driver defect (swapped byte, wrong size, CRC) shows up here, in seconds, with the expected frame next to the one that went out.

##### Common mistakes

| Symptom | Cause |
|---|---|
| Tags never update and never flag a fault | `DoAddTag` did not call `inherited` with `True`, or `DoScanRead` never calls `SetValues`/`SetFault`. |
| CPU at 100 % | `DoScanRead` returns `NeedSleep = 0` without having done I/O. |
| A second driver on the same port hangs | `Lock` without `Unlock` on an error path — use `try…finally`. |
| Wrong values only on 32-bit types | `SizeOfTag` reports a size different from what `Values` delivers. |
| Tag drops out of the scan when its address changes | `DoDelTag` does not remove with the same parameters `DoAddTag` added. |
| The next read returns garbage after an error | The port was not drained after an unexpected answer. |
| Failures show up as a generic `ioDriverError` | `DecodePkg` does not tell timeout, CRC and device exception apart. |

I hope I managed to explain how drivers work in PascalSCADA. Criticism, suggestions, improvements and above all corrections: use the comments.

##### Reference drivers in the repository

All in `src/scada/`, from the simplest to the most complete:

* `iboxdriver.pas` — `TIBoxDriver`: serial, a few fixed registers per station, no memory manager; the smallest complete example of the seven methods.
* `westasciidriver.pas` — `TWestASCIIDriver`: a serial ASCII protocol with checksum.
* `modbusdriver.pas` + `modbusserial.pas` + `modbustcp.pas` — the Modbus family: base with the I/O flow and one `TPLCMemoryManager` per area, subclasses with `EncodePkg`/`DecodePkg` for RTU and TCP. The recommended model.
* `MelsecDriver.pas` + `MelsecTCP.pas` — the same family structure, with ten memory areas.
* `s7family.pas` + `isotcpdriver.pas` — a protocol with connection and negotiation (PDU), several areas and per-DB blocks.
* `lgxdriver.pas` — EtherNet/IP: symbolic addressing through `LongAddress`, no per-address memory manager.

The matching tests in `tests/ut.*.pas` show, frame by frame, what each one sends and expects.
