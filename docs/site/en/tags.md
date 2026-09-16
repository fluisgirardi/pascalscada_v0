##### Introduction

In PascalSCADA a **tag** is the component that represents a memory (or a set of memories) of a device: a holding register of a Modbus drive, a byte of a DB in a Siemens PLC, a controller tag in a Rockwell PLC. The tag sits in the middle of the communication chain:

```
Communication port  →  Protocol driver  →  Tag  →  HMI control / your code
```

The port carries bytes, the driver speaks the device's protocol, and the **tag tells the driver what to read and write**, keeps the last value read and notifies whoever depends on it when that value changes. The HMI controls (`THMIEdit`, `THMILabel`, `THMICheckBox`…) never talk to the driver: they are linked to a tag through their `PLCTag` property and react to its changes. One tag can feed as many controls as you want.

All tags live on the **PascalSCADA Tags** palette and descend from `TTag` → `TPLCTag`. From there they split into two groups:

* **Numeric tags** (`TPLCNumber`): expose a single `Double` value in the `Value` and `ValueRaw` properties. These are `TPLCTagNumber`, `TPLCBlockElement`, `TPLCStructItem`, `TTagBit` and `TNumericExprTag`.
* **Block tags** (`TTagBlock`): map several consecutive memories in a single request. These are `TPLCBlock`, `TPLCStruct` and `TPLCString`.

The table below summarizes when to use each one:

| Component | Use it when… |
|---|---|
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png) `TPLCTagNumber` | you need **one** numeric value, addressed and read on its own. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png) `TPLCBlock` + ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblockelement.png) `TPLCBlockElement` | you need **several consecutive values of the same type** (e.g. 20 registers starting at address 100). The block issues a single request; each element exposes one item of the block as a numeric tag. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png) `TPLCStruct` + ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstructitem.png) `TPLCStructItem` + ![](img/TPLCStructString.png) `TPLCStructString` | you need **several values of different types** that sit together in memory (a PLC `STRUCT`/UDT, a Siemens DB mixing bytes, words and reals). |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png) `TPLCString` | you need to read or write a **text**. |
| ![](http://www.pascalscada.com/wp-content/uploads/2016/08/tagbit.png) `TTagBit` | you need **one or more bits** of another numeric tag, without extra communication. |
| ![](img/TNumericExprTag.png) `TNumericExprTag` | you need a value **computed** from other tags, with no communication of its own. |

##### How a tag is read and written

Every communication tag has four operations available from code:

| Method | Direction | Behaviour |
|---|---|---|
| `ScanRead` | read | **Asynchronous.** Queues a read request on the driver and returns at once. The value arrives later, in the `OnValueChange`/`OnUpdate` events. |
| `Read` | read | **Synchronous.** Blocks until the driver answers (or fails). On return `Value` is already updated and `LastSyncReadStatus` tells the result. |
| `ScanWrite` | write | **Asynchronous.** Queues the write and returns. The result goes to `LastASyncWriteStatus` and to the `OnWriteOk`/`OnWriteFail` events. |
| `Write` | write | **Synchronous.** Blocks until the write is confirmed. |

In practice you rarely call these methods. Day-to-day behaviour is governed by a few properties:

* **`AutoRead`** — when `True` (default) the driver includes the tag in its scan cycle and reads it automatically every `RefreshTime` milliseconds. When `False` the tag is only read when you call `Read` or `ScanRead`.
* **`RefreshTime`** — update period of the tag, in milliseconds (default 1000). Tags on the same driver with the same `RefreshTime` and neighbouring addresses are grouped by the driver into a single request whenever the protocol allows it, so **there is no penalty in using many small tags**; use `TPLCBlock` when you want to guarantee that the data arrives together, as a single snapshot.
* **`AutoWrite`** — when `True` (default) every assignment to `Value` (from code or from an HMI control) produces a write to the device. When `False` the assignment only changes the value in memory; it goes to the device when you call `WriteDirect` (blocks) or `Write`/`ScanWrite`.
* **`SyncWrites`** — when `True`, writes triggered by `AutoWrite` are synchronous (`Write`) instead of asynchronous (`ScanWrite`). Leave it `False` unless the code that assigned the value must be sure it has already reached the device.

> The rule of thumb is: **leave `AutoRead` and `AutoWrite` at `True` and call nothing** — the driver does the work. Turn `AutoRead` off only for tags that must be read at a specific moment (a recipe loaded on demand, for instance) and call `Read` at that moment.

##### Addressing

Every communication tag has the same set of addressing properties. **The meaning of each one depends on the protocol driver**, and the page of each protocol explains how to fill them. As a quick reference:

| Property | Modbus | S7 (ISOTCP) | Melsec | West ASCII / IBox | Logix / S7CommPlus |
|---|---|---|---|---|---|
| `PLCStation` | Slave address (1–247) | – | Station number | Controller address | – |
| `PLCRack` / `PLCSlot` | – | CPU rack and slot | – | – | – |
| `MemFile_DB` | – | DB number (DB area) | – | – | – |
| `MemReadFunction` | Modbus read function (1, 2, 3, 4) | Memory area (1 = I, 2 = Q, 3 = M, 4 = DB…) | Device type (M, SM, L, X, Y, D…) | – | – |
| `MemWriteFunction` | Modbus write function (5, 6, 15, 16) | – | Device type (same code as read) | – | – |
| `MemAddress` | Register/coil address | Byte address | Device address | Parameter (West) / PID (IBox) | – |
| `MemSubElement` | – | – (bits: use `TTagBit`) | – | – | – |
| `LongAddress` | – | – | – | – | Symbolic tag name in the PLC (`Motor1.Speed`, `"DB10".Setpoint`) |
| `Retries` | Attempts before declaring a failure (all drivers) | | | | |

The fastest way to get addressing right is the **Tag Builder**: right-click the protocol driver component (`TModBusTCPDriver`, `TISOTCPDriver`…) and choose *Tag Builder*. It creates tags already configured for that protocol.

`ProtocolDriver` is the property that links the tag to the driver. A tag without a driver does nothing — and raises no error.

##### Data type

The **`TagType`** property tells the driver how to interpret the bytes read from the device:

| `TagType` | Size | Interpretation |
|---|---|---|
| `pttDefault` | driver dependent | The protocol's "natural size": 16 bits on Modbus, 8 bits on S7, and so on. |
| `pttShortInt` / `pttByte` | 8 bits | Signed / unsigned integer |
| `pttSmallInt` / `pttWord` | 16 bits | Signed / unsigned integer |
| `pttLongInt` / `pttDWord` | 32 bits | Signed / unsigned integer |
| `pttFloat` | 32 bits | IEEE-754 floating point (REAL) |
| `pttInt64` / `pttQWord` | 64 bits | Signed / unsigned integer |
| `pttDouble` | 64 bits | IEEE-754 floating point (LREAL) |

When the size of `TagType` differs from the protocol's natural size the tag spans more than one memory: a `pttFloat` on Modbus takes two consecutive 16-bit registers. `TagSizeOnProtocol` (read-only) tells how many.

If the value arrives "scrambled" (a `REAL` that shows up as an absurd number, a `DINT` with its halves swapped) the device uses a byte order different from the one the driver assumes. Fix it with:

* **`SwapBytes`** — swaps the two bytes of each 16-bit word.
* **`SwapWords`** — swaps the two 16-bit words of a 32-bit value.
* **`SwapDWords`** — swaps the two 32-bit words of a 64-bit value.

For Modbus, the most common setting for `REAL` on devices that use the "CDAB" order is `SwapWords = True`.

##### Value, scaling and limits

Numeric tags have two values:

* **`ValueRaw`** — the value exactly as it came from the device.
* **`Value`** — the value after going through the chain of scales linked in `ScaleProcessor`. With no scale, `Value = ValueRaw`.

HMI controls and your code normally use `Value`. To convert, say, the 0–27648 integer of a Siemens analog input into 0–100 %, link a `TLinearScaleProcessor` in `ScaleProcessor` — see [Scale processors](/scale-processors/). Scaling works both ways: writing `50` to `Value` stores `13824` in the device.

**`MinValue`** / **`MaxValue`**, enabled by **`EnableMinValue`** / **`EnableMaxValue`**, bound what can be *written* to the tag. An assignment outside the range raises an exception ("value out of bounds"), fires `OnWriteFail`, and the value does not reach the device. The input controls (`THMIEdit`, `THMITrackBar`, `THMIUpDown`) honour these limits.

##### Events

The events below exist on every communication tag. All of them run on the *main thread*, so it is safe to touch visual controls inside them.

| Event | When it fires |
|---|---|
| `OnValueChangeFirst` | The value changed, **before** the dependent HMI controls are notified. Use it to prepare something the controls are about to show. |
| `OnValueChangeLast` (or `OnValueChange`) | The value changed, **after** the controls were notified. This is the "normal" value-change event. |
| `OnUpdate` | The tag was read successfully, **even if the value did not change**. Use it as a heartbeat or to log history at a fixed interval. |
| `OnReadOK` / `OnReadFail` | Result of each read. |
| `OnWriteOk` / `OnWriteFail` | Result of each write. |
| `OnAsyncValueChange` | A version of `OnValueChange` called **on a driver-side thread**, with no synchronization with the main thread. Use it only if you know what you are doing: it is faster, but it must not touch visual controls. |

To find out what went wrong on a failure, read `LastSyncReadStatus`, `LastSyncWriteStatus`, `LastASyncReadStatus` or `LastASyncWriteStatus` (type `TProtocolIOResult`: `ioOk`, `ioTimeOut`, `ioIllegalFunction`, `ioIllegalRegAddress`, `ioDriverError`, `ioCommError`, …).

##### Diagnostics

Each tag keeps counters that show in the Object Inspector at run time and help track down network problems: `CommReadsOK`, `CommReadErrors`, `CommWritesOk`, `CommWriteErrors`. `AvgUpdateRate` shows the real average interval between updates — if it is much larger than `RefreshTime`, the driver cannot keep up with the tag load and you should raise the `RefreshTime` of the less important tags or group them into blocks. `ClockMonotonicTimeStamp` holds the instant of the last update.

`TagGUID` is a unique identifier generated by the IDE for each tag. Do not edit it.

##### TPLCTagNumber {#TPLCTagNumber}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plctagnumber.png) The most used tag: one memory, one value. Set `ProtocolDriver`, the addressing properties and `TagType`, link the HMI controls and you are done.

From code:

```pascal
// Reading: the value is already in the tag, kept fresh by the scan.
Speed := PLCTagNumber1.Value;

// Writing: with AutoWrite = True the assignment already writes to the device.
PLCTagNumber1.Value := 1500;

// Forcing a synchronous read right now, regardless of the scan.
PLCTagNumber1.Read;
if PLCTagNumber1.LastSyncReadStatus = ioOk then
  ShowMessage(FloatToStr(PLCTagNumber1.Value));
```

##### TPLCBlock and TPLCBlockElement {#TPLCBlock}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblock.png) `TPLCBlock` maps `Size` consecutive memories of the same `TagType`, starting at `MemAddress`. All of them are read and written in a single request, which guarantees the values are consistent with each other (same instant) and cuts traffic when the protocol has a high per-message overhead.

The values live in `ValueRaw[index]` (from `0` to `Size-1`) or in the `ValuesRaw` array. A `TPLCBlock` alone cannot be linked to an HMI control — that is what ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcblockelement.png) **`TPLCBlockElement`** is for: a numeric tag whose `PLCBlock` property points to the block and whose `Index` says which element it represents. The element has everything a `TPLCTagNumber` has (`Value`, `ScaleProcessor`, limits, events) but no address of its own and no `RefreshTime`: the block does the communication.

To avoid creating the elements one by one, right-click the block and choose **Map block elements…** — the wizard creates one `TPLCBlockElement` per index.

Writing a block:

```pascal
// With AutoWrite = True each assignment produces a write of one element.
PLCBlock1.ValueRaw[3] := 10;

// To write the whole block at once, turn AutoWrite off,
// assign the values and call WriteDirect (synchronous) or WriteByScan (asynchronous).
PLCBlock1.AutoWrite := False;
PLCBlock1.ValueRaw[0] := 1;
PLCBlock1.ValueRaw[1] := 2;
PLCBlock1.ValueRaw[2] := 3;
PLCBlock1.WriteDirect;
```

##### TPLCStruct, TPLCStructItem and TPLCStructString {#TPLCStruct}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstruct.png) `TPLCStruct` is a `TPLCBlock` whose `TagType` is fixed at `pttByte`: it reads `Size` consecutive **bytes** and lets each item decide how to interpret them. It is the way to map a structure with fields of different types — a Siemens DB mixing `BOOL`, `INT`, `REAL` and `STRING`, or a UDT.

The fields are represented by:

* ![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstructitem.png) **`TPLCStructItem`** — a numeric field. `PLCBlock` points to the structure, `Index` is the **byte offset** of the field inside it and `TagType` gives the field type (the item spans the corresponding number of bytes). `SwapBytes`/`SwapWords`/`SwapDWords` apply per item.
* ![](img/TPLCStructString.png) **`TPLCStructString`** — a text field. `Index` is the offset, `StringSize` the maximum length in characters, `StringType` the format (`stC` or `stSIEMENS`, see `TPLCString` below) and `StringEncoding` the device's code page.

Example — a Siemens DB with this layout:

| Offset | Type | Name |
|---|---|---|
| 0 | `INT` | Counter |
| 2 | `REAL` | Temperature |
| 6 | `STRING[20]` | Product name (22 bytes including the Siemens header) |

becomes: a `TPLCStruct` with `Size = 28`; a `TPLCStructItem` with `Index = 0, TagType = pttSmallInt`; another with `Index = 2, TagType = pttFloat`; and a `TPLCStructString` with `Index = 6, StringSize = 20, StringType = stSIEMENS`.

The **Map structure items…** wizard (right-click the structure) creates the items from a list of types you assemble on screen, computing the offsets automatically.

As with blocks, `AutoWrite = False` + `WriteDirect` writes the whole structure in a single request.

##### TPLCString {#TPLCString}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/plcstring.png) Reads and writes a text from the device. The value is in the `Value` property (`UTF8String`), and `THMIEdit`/`THMILabel` show and edit text when linked to a `TPLCString`.

* **`StringSize`** — maximum number of characters.
* **`StringType`** — how the string is stored in the device:
    * `stC` — the characters run until a `0` byte is found (C terminator). Use it for Modbus, Melsec and Rockwell PLCs (pointing `LongAddress` to the `DATA` member of the `STRING`, e.g. `MyString.DATA` — which is what the Rockwell tag import wizard does).
    * `stSIEMENS` — the S7 `STRING` format: two header bytes (maximum length and used length) followed by the characters. The tag spans `StringSize + 2` bytes.
    * `stROCKWELL` — reserved, not implemented yet; returns an empty string.
* **`StringEncoding`** — the device's code page (`UTF_8`, `CP1252`, `CP850`…). The tag converts to/from UTF-8 automatically.

`OnAsyncStringChange` is the string counterpart of `OnAsyncValueChange`.

##### TTagBit {#TTagBit}

![](http://www.pascalscada.com/wp-content/uploads/2016/08/tagbit.png) Extracts a range of bits from another numeric tag (`PLCTag`), with no extra communication. `StartBit` is the least significant and `EndBit` the most significant bit of the range (both from 0, at most 31; `EndBit >= StartBit`). The `TTagBit` value is the range shifted right:

| Source tag value | `StartBit` | `EndBit` | `TTagBit` value |
|---|---|---|---|
| 5 (`101b`) | 0 | 0 | 1 |
| 5 (`101b`) | 0 | 2 | 5 |
| 5 (`101b`) | 1 | 2 | 2 (`10b`) |
| 5 (`101b`) | 2 | 2 | 1 |

Writing to a `TTagBit` works: it takes the current value of the source tag, changes only the bits in the range and writes the result back — so with `AutoWrite = True` the bit reaches the device.

`UseRawValue` selects whether the bits come from `ValueRaw` (`True`) or from `Value` (`False`, default). Turn it on when the source tag has a scale.

The typical use is a PLC status or command word where each bit has a meaning: one `TPLCTagNumber` reads the word and one `TTagBit` per bit feeds the `THMICheckBox`es. The **Map bits** wizard (right-click any numeric tag) creates the 8, 16 or 32 `TTagBit`s at once. The `examples/laz_isotcp_mapping_bits_from_other_tag` example shows this with a byte of a Siemens DB.

##### TNumericExprTag {#TNumericExprTag}

![](img/TNumericExprTag.png) A virtual numeric tag: its value is the result of `Expression`, evaluated whenever one of the tags linked to the variables `A` to `J` changes. It has no driver and no address.

```
Expression = 'A * B / 100'
Expression = 'ifthen(A > 80, 1, 0)'
Expression = 'sqrt(sqr(A) + sqr(B))'
```

The syntax is that of Free Pascal's `TFPExpressionParser` (arithmetic and comparison operators, `and`/`or`/`not`, math functions, plus the `ifthen(condition, ifTrue, ifFalse)` function added by PascalSCADA). An error in the expression is reported in `LastEvalutionError`. It can have a `ScaleProcessor`. See the [Numeric expression tag](/numeric-expression-tag/) page for more examples.

##### Creating tags at run time

Tags are `TComponent`s, so they can be created from code — handy for applications that load their configuration from a database:

```pascal
uses PLCTagNumber, ProtocolTypes;

var
  t: TPLCTagNumber;
begin
  t := TPLCTagNumber.Create(Self);
  t.Name := 'Tank1_Level';
  t.MemReadFunction := 3;    // Modbus: holding register
  t.MemWriteFunction := 16;
  t.MemAddress := 100;
  t.PLCStation := 1;
  t.TagType := pttFloat;
  t.SwapWords := True;
  t.RefreshTime := 500;
  t.ProtocolDriver := ModBusTCPDriver1;   // link the driver last
  t.OnValueChange := @LevelChanged;
end;
```

Link `ProtocolDriver` after setting the address: when it receives the driver, the tag registers itself in the driver's scan with the address it has at that moment. To remove it, just `t.Free` — the tag unregisters from the driver and from the controls linked to it.

##### Related examples

* `examples/TagTypes` — the same Modbus block read with every `TagType` (`pttByte` to `pttDouble`) and the `SwapBytes`/`SwapWords` combinations, with `TPLCBlockElement` and `TPLCStructItem` side by side; the best place to see the effect of each type.
* `examples/laz_isotcp_mapping_bits_from_other_tag` — one `pttByte` `TPLCTagNumber` and one `TTagBit` per bit.
* `examples/laz_numericexpr` — `TNumericExprTag` adding two tags driven by `THMITrackBar`s.
* `examples/laz_modbus_tcp_example` — `TPLCBlock` + `TPLCBlockElement`, `TPLCTagNumber` and input controls over Modbus TCP.
* `examples/laz_console_app` — tags and a Modbus TCP driver in a console application, without the LCL (a datamodule and no forms).
* `examples/hmi_bandeja` — `TPLCStruct` with `TPLCStructItem` and `TPLCStructString` mapping a Siemens DB.
