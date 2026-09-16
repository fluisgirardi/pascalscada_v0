##### Introduction

First of all, feel free to send corrections of any kind, including text reformulations.

The main goal of this article is to understand what the scale-processing classes are for and the scenarios where they should be used. A summary of the properties of each one follows.

##### What the scale processor classes are

The scale processor classes (derived from `TScaleProcessor`) perform a numeric transformation of the value that comes from your device (raw value) into a value shown to the user of the supervisory system (engineering), and the reverse path when the user changes a value in the supervisory system, which is in the engineering scale, delivering a raw value to your device.

The device rarely delivers the value in the unit the operator wants to see: an S7 analog input arrives as an integer from 0 to 27648; a drive sends its frequency in tenths of a hertz; a level transmitter gives 4–20 mA converted to 0–4095. That is the job of the scales.

They attach to numeric tags through the **`ScaleProcessor`** property (see [Tags](/tags/)). From then on:

* `ValueRaw` is the raw value, as it came from the device;
* `Value` is `ValueRaw` after scaling — what the [HCl](/hmi-control-library-hcl/) controls show and what you use in code;
* writing to `Value` applies the inverse scale and stores the raw value in the device.

##### Scales currently implemented in PascalSCADA

The most used scale processing is the linear one, already implemented in PascalSCADA in the `TLinearScaleProcessor` class. If you need a scale not implemented yet, you can use the `TUserScale` class, which lets you write your own scale by implementing two events of the instance. And to compose several scales in sequence there is `TScalesQueue`. All of them live on the **PascalSCADA Utils** palette.

| Component | Does |
|---|---|
| ![](img/TLinearScaleProcessor.png) `TLinearScaleProcessor` | Linear conversion between two ranges (the rule of three). Covers 90% of the cases. |
| ![](img/TUserScale.png) `TUserScale` | A conversion you write in two events — for any formula. |
| ![](img/TScalesQueue.png) `TScalesQueue` | Chains several processors in sequence. |

##### Do I need one scale object per tag in my application?

No. PascalSCADA's scale system was designed so that one scale object can be shared by as many tags as exist in your application. That is, if all my tags use the same scale and at some point that scale must change, changing the properties of a single object changes the scale of every tag in the application. The processor keeps no per-tag state.

##### The TLinearScaleProcessor class {#TLinearScaleProcessor}

The `TLinearScaleProcessor` class does what its name says: converts values through a first-degree linear function. The image below illustrates it: [![TLinearScaleProcessor](http://www.pascalscada.com/wp-content/uploads/2016/08/TLinearScaleProcessor.png)](<http://www.pascalscada.com/wp-content/uploads/2016/08/TLinearScaleProcessor.png>)

The `PLCMin` and `PLCMax` properties represent the range of raw values coming from your device. The `SysMin` and `SysMax` properties represent the range in engineering units, i.e. what is shown to the user of the supervisory system. The conversion works both ways, device to user and user to device:

```
Value    = (ValueRaw - PLCMin) * (SysMax - SysMin) / (PLCMax - PLCMin) + SysMin
ValueRaw = (Value    - SysMin) * (PLCMax - PLCMin) / (SysMax - SysMin) + PLCMin
```

![Warning32](http://www.pascalscada.com/wp-content/uploads/2016/08/Warning32.png)Keep in mind that scale processors do not restrict engineering values outside the range: a raw value outside `PLCMin`..`PLCMax` yields an engineering value outside `SysMin`..`SysMax` — usually what you want, so that an over-range shows as such. To restrict data entry use the `EnableMinValue`, `EnableMaxValue`, `MinValue` and `MaxValue` properties of your tags.

Just set `PLCMin`, `PLCMax`, `SysMin` and `SysMax` with your scale data. To test it you do not need to run the application: use the `Input` property to simulate the value coming from your PLC and the `Output` property to simulate values coming from the application. Set a value in `Input` and read the converted engineering value in `Output`, and vice versa. (`PLCMax = PLCMin` or `SysMax = SysMin` does not divide by zero: the divisor is treated as 1.)

Below is a list of some conversions using `TLinearScaleProcessor`:

| Name | `PLCMin` | `PLCMax` | `SysMin` | `SysMax` |
|---|---|---|---|---|
| Milliseconds (PLC) → seconds (Supervisory) | 0 | 1000 | 0 | 1 |
| Seconds (PLC) → minutes (Supervisory) | 0 | 60 | 0 | 1 |
| RTD temperature in tenths of a degree (PLC) → degrees (Supervisory) | 0 | 10 | 0 | 1 |
| Siemens 4 to 20 mA analog (PLC) → 0 to 100 % (Supervisory) | 4096 | 27648 | 0 | 100 |
| Siemens 0 to 10 V analog (0–27648) → 0 to 100 % | 0 | 27648 | 0 | 100 |
| 4–20 mA input on a 0–4095 card (4 mA = 819) → 0 to 10 bar | 819 | 4095 | 0 | 10 |
| Temperature in tenths of a degree, with offset (−400 = −40.0 °C) | −400 | 1500 | −40 | 150 |
| Reverse direction (0 = full, 1000 = empty) → 100 to 0 % | 0 | 1000 | 100 | 0 |

##### The TUserScale class {#TUserScale}

![](img/TUserScale.png) For non-linear conversions — the square root of a differential-pressure flow transmitter, a thermocouple linearization table, a calibration polynomial — implement the two events:

```pascal
// Device -> user
procedure TForm1.UserScale1PLCToUser(Sender: TObject; const Input: Double;
  var Output: Double);
begin
  Output := Sqrt(Input / 4095) * 500;      // flow in m³/h
end;

// User -> device (inverse of the above)
procedure TForm1.UserScale1UserToPLC(Sender: TObject; const Input: Double;
  var Output: Double);
begin
  Output := Sqr(Input / 500) * 4095;
end;
```

`Sender` is the tag that requested the conversion, so a single `TUserScale` can apply different formulas per tag (`if Sender = Flow1_Tag then …`). If an event is not assigned, the value passes through unchanged in that direction.

The events run on the main thread, whenever the tag is updated — keep them fast and free of database or network access.

##### The TScalesQueue class {#TScalesQueue}

![](img/TScalesQueue.png) A queue of processors. The `ScalesQueue` collection has items with a `ScaleProcessor` property; the raw value goes through the first item, its result through the second, and so on. In the user → device direction the order is reversed automatically, from the last to the first.

It is for composing reusable conversions: a `TLinearScaleProcessor` "0–27648 → 0–100 %" followed by another "0–100 % → 0–500 m³/h", or a linear scale followed by a linearizing `TUserScale`. A `TScalesQueue` is itself a scale processor and can be an item of another `TScalesQueue` (it just cannot contain itself).

##### Where else scales show up

* **[`TNumericExprTag`](/tags/#TNumericExprTag)** also has a `ScaleProcessor`, applied to the expression result.
* **`TTagBit`** with `UseRawValue = False` maps the bits of the source tag's `Value` (scaled); with `True`, of `ValueRaw`. For status words turn it on.
* The **`THMITrackBar`**/**`THMIScrollBar`** controls work with LCL integers: for a 0.0–10.0 bar setpoint, put the scale on the tag (`SysMin = 0`, `SysMax = 100`) and the control from 0 to 100 — or use `THMIEdit`/`THMIUpDown`, which accept `Double`.
* The **`THMIControlDislocatorAnimation`** components use a linear scale internally between `ValueP0`/`ValueP1` and the P0/P1 positions.

##### Related examples

* `examples/laz_linear_scalling` — a `TScalesQueue` with one `TLinearScaleProcessor` ("1 in the system = 100 in the PLC") applied to elements of a Modbus TCP block, with `THMIScrollBar` and `THMILabel` showing raw and scaled side by side.
* `examples/laz_numericexpr` — computing between tags with `TNumericExprTag`, the alternative to scales when the result depends on more than one tag.
