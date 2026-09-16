##### Introduction

The **HMI Control Library (HCl)** is PascalSCADA's set of visual controls — the **PascalSCADA HMI Controls** palette. Each of them is an ordinary LCL control (`TEdit`, `TLabel`, `TCheckBox`, `TTrackBar`…) that learned to talk to a [tag](/tags/): drop the control on the form, point its **`PLCTag`** property to a tag and you are done — the control shows the tag value and, on input controls, writes to it. No code is needed for the common case.

What every HCl control has in common:

* **`PLCTag`** — the tag the control is linked to. One tag can feed many controls at once. If the tag is destroyed, the control unlinks itself.
* **`SecurityCode`** — a security code (free text, e.g. `"setpoints"`) that plugs the control into the [security system](/security-system/): the control is enabled only if the logged-in user is allowed that code. Empty = no restriction. The code is registered automatically with the security manager the first time it appears.
* **`Enabled`** — besides the value you set, the control is disabled automatically when the user lacks the `SecurityCode`.
* **Communication-fault badge** — when the tag linked to the control stops communicating (timeout, closed port, unresponsive driver), a small yellow warning triangle appears over the control and goes away when communication resumes. Nothing to configure; the `examples/laz_comm_fault_control_icon_test` example shows the effect on each control.
* **`BeforeSendAValueToTag`** / **`AfterSendValueToTag`** — events of the input controls, fired before and after the value goes to the tag. In *Before* you can change or veto the value.

Controls run on the main thread and are updated by the tag's events — the update is immediate when the value changes in the device, with no timers.

##### Value formatting

The text-displaying controls (`THMIEdit`, `THMILabel`) format the tag value with three properties:

* **`NumberFormat`** — a mask in the format of Free Pascal's `FormatFloat` function (`#0.00`, `000`, `#,##0.0`, `0.000E+00`…). Numeric tags only.
* **`Prefix`** / **`Sufix`** — text before and after the value (`"T = "`, `" °C"`). Shown only for display; in `THMIEdit` they disappear while the control has focus so the operator edits just the number.

With a `TPLCString` the control shows and edits the tag text directly.

##### Zones

Several controls (`THMIText`, `THMIAnimation`, the property connectors and the flow controls) do not show the tag value but **pick an appearance depending on it**. The choice is made by a collection of **zones** (`Zones`, `Conditions`, `ColorAndFlowStates`): each zone has a condition and a result (a text, an image, a colour, a boolean). The control walks the collection and uses the **first zone whose condition is true**; if none is, it uses the zone marked **`DefaultZone`**.

Each zone's condition is defined by `ZoneType`, `Value1`, `Value2`, `IncludeValue1` and `IncludeValue2`:

| ZoneType | The zone is selected when… |
|---|---|
| `ztEqual` | tag value = `Value1` |
| `ztNotEqual` | tag value ≠ `Value1` |
| `ztRange` | `Value1` < value < `Value2` (`IncludeValue1`/`IncludeValue2` make the bounds inclusive) |
| `ztOutOfRange` | value < `Value1` **or** value > `Value2` (same for the bounds) |
| `ztGreaterThan` | value > `Value1` (≥ with `IncludeValue1`) |
| `ztLessThan` | value < `Value1` (≤ with `IncludeValue1`) |
| `ztBit` | bit number `Value1` of the tag's integer value equals `IncludeValue1` |

Animation zones (text, image and colour) also have **`BlinkTime`** and **`BlinkWith`**: after `BlinkTime` milliseconds the control switches to the zone at index `BlinkWith` — which can point back, making a blink, or to a third zone, making an animation sequence. `BlinkTime = 0` turns it off.

`THMIText` and `THMIAnimation` have a design-time property **`TestValue`**: a test value the control uses instead of the tag so you can check the zones in the editor without communication.

##### Input controls

###### THMIEdit {#THMIEdit}

![](img/THMIEdit.png) Edit box for numeric or string tags. Shows the formatted value and, when the operator types a new value and confirms it, writes it to the tag.

| Property | Default | Description |
|---|---|---|
| `SendValueWhen` | `[scLostFocus, scPressEnter]` | When the typed value goes to the tag: on losing focus, on Enter, on Esc, or on every keystroke (`scAnyChange`). |
| `FreezeValueOnFocus` | `True` | While the box has focus, the tag value does not overwrite what the operator is typing. |
| `EnableMinValue` / `MinValue`, `EnableMaxValue` / `MaxValue` | | Accepted input range. A value outside it is not sent to the tag. |
| `NumberFormat`, `Prefix`, `Sufix` | | Formatting (see above). |
| `ShowFocused` | `False` | Swaps background and font colours while focused, to stand out on touch screens. |
| `ScreenKeyboardBehavior` | `oskbDisabled` | On-screen keyboard: `oskbEnabled` opens the control's own keyboard on focus (numeric or alphanumeric depending on the tag and `NumberFormat`); `oskbManager` delegates to the form's `THMIKeyboardManager`. `ScreenNumericKBShowMinus`/`ScreenNumericKBShowDecimal` control the numeric keyboard's keys. See [On-screen keyboard](/keyboard-manager/). |

###### THMICheckBox and THMIRadioButton {#THMICheckBox}

![](img/THMICheckBox.png) Check box linked to a numeric tag. Checked when the tag equals `ValueTrue`, unchecked when it equals `ValueFalse`; `OtherValuesIS` says what to do with any other value (`isChecked`, `isUnchecked`, `isGrayed`, `isNone`). When the operator clicks, it writes `ValueTrue` or `ValueFalse` to the tag — if `WriteTrueValue`/`WriteFalseValue` are `True`, which allows, for instance, a button that only switches on and never off.

Text, colour and font can follow the state: `CaptionTrue`/`CaptionFalse`/`CaptionGrayed`, `ColorTrue`/`ColorFalse`/`ColorGrayed`, `FontTrue`/`FontFalse`/`FontGrayed`. Writing to `Caption`, `Color` or `Font` fills the three variants at once.

With a [`TTagBit`](/tags/#TTagBit) as `PLCTag` (`ValueTrue = 1`, `ValueFalse = 0`) the `THMICheckBox` becomes the standard control for a command or status bit.

![](img/THMIRadioButton.png) `THMIRadioButton` is the same control with a radio-button look. Prefer `THMIRadioGroup` to choose among several options.

###### THMIRadioGroup {#THMIRadioGroup}

![](img/THMIRadioGroup.png) Option group linked to a numeric tag: the option at index *n* in `Items` is selected when the tag equals *n*, and clicking an option writes its index to the tag. `DefaultIndex` is the option used when the tag value matches no item.

###### THMIComboBox {#THMIComboBox}

![](img/THMIComboBox.png) Drop-down list linked to a numeric tag, with the same mapping as `THMIRadioGroup`: item index = tag value. To associate arbitrary values (e.g. 10, 20, 50) with the items, fill `Items` from code with `TComboboxItemInfo` objects:

```pascal
uses hmicombobox;

HMIComboBox1.Items.AddObject('Slow',   TComboboxItemInfo.Create(nil, 10));
HMIComboBox1.Items.AddObject('Normal', TComboboxItemInfo.Create(nil, 20));
HMIComboBox1.Items.AddObject('Fast',   TComboboxItemInfo.Create(nil, 50));
```

###### THMITrackBar, THMIScrollBar and THMIUpDown {#THMITrackBar}

![](img/THMITrackBar.png) `THMITrackBar` and ![](img/THMIScrollBar.png) `THMIScrollBar` follow the tag value and write to it when the operator drags. On `THMIScrollBar`, `UpdateOnMove` decides whether the write happens while dragging (`True`) or only on release (`False`). `Min`/`Max` are the LCL's, in integers — for scaled tags, use a `ScaleProcessor` on the tag.

![](img/THMIUpDown.png) `THMIUpDown` is the increment/decrement arrow pair: each click adds or subtracts `Increment` (a `Double`, so steps of 0.1 work) to the tag value, honouring `Min`/`Max` when `EnableMin`/`EnableMax` are on. Usually placed next to a `THMIEdit` linked to the same tag.

###### THMITransparentButton {#THMITransparentButton}

![](img/THMITransparentButton.png) An invisible rectangle with `OnClick`/`OnMouseUp` and `SecurityCode`. Put it over a background image or a drawing to create a clickable area with access control — the click does not happen if the user lacks the code.

##### Display controls

###### THMILabel {#THMILabel}

![](img/THMILabel.png) Shows the value of any tag (numeric or string) with `NumberFormat`, `Prefix` and `Sufix`. It is the most used display control. For a tag holding a date/time as a number, `FormatDateTimeOptions` tunes the conversion.

###### THMIText {#THMIText}

![](img/THMIText.png) A `THMILabel` that, instead of the value, shows the **text of the selected zone**: each `TTextZone` in `Zones` has its own `Text`, `Color`, `Transparent`, `Font` and alignment. It is the standard way to turn a state code into words — `0 → "Stopped"` in grey, `1 → "Running"` in green, `2 → "Fault"` in blinking red (`BlinkTime` + `BlinkWith` pointing to a zone with another colour).

###### THMIProgressBar {#THMIProgressBar}

![](img/THMIProgressBar.png) Level bar drawn by PascalSCADA itself (independent of the system theme): `Min`/`Max` as `Double`, horizontal or vertical `Orientation`, `Color` for the fill, `BackgroundColor` and `BorderColor`. Good for tank level, position, motor load.

###### THMIAnimation {#THMIAnimation}

![](img/THMIAnimation.png) Shows the **image of the selected zone**. Each `TGraphicZone` in `Zones` points to a file (`FileName`) or to an image in a `TImageList` (`ImageList` + `ImageIndex`; `ImageListAsDefault` chooses which one wins when both are set), with `Transparent`/`TransparentColor`. With `BlinkTime`/`BlinkWith` across two or more zones you get an animation — a spinning motor, a blinking lamp. The `ZoneChanged` event tells when the zone changes. The post [How to build a tank level animation](/2019/05/10/how-build-a-tank-level-animation-on-pascalscada/) shows the use with a `TImageList`.

##### Property connectors

Connectors have no look of their own: they **change properties of other controls** (any LCL control, not just HCl ones) depending on a tag value. That lets you, for instance, change the colour of a plain `TShape` or hide a `TPanel` without writing code.

###### THMIBooleanPropertyConnector and THMIColorPropertyConnector {#THMIBooleanPropertyConnector}

* **`PLCTag`** — the tag that drives the conditions.
* **`Conditions`** — collection of zones (the same conditions as in the table above). Each zone has a result: a boolean `ZoneResult` (with `InvertResult`) on the boolean connector, a colour `ZoneResult` on the colour connector. One of them can be the `DefaultZone`.
* **`AffectedObjects`** — collection of `TargetObject` + `TargetObjectProperty` pairs (e.g. `Shape1` + `Brush.Color`, `Panel1` + `Visible`, `Label1` + `Font.Color`). On every tag change, the connector evaluates the conditions and writes the result to **all** the listed properties.

The property editor lists the properties of the right type (boolean or `TColor`) of the chosen object, nested ones included (`Font.Color`, `Brush.Color`).

###### THMIControlDislocatorAnimation and THMIControlDislocatorAnimation2 {#THMIControlDislocatorAnimation}

![](img/THMIControlDislocatorAnimation.png) Move a control (`Control`) across the screen according to a tag: when the tag equals `ValueP0` the control sits at (`P0_X`, `P0_Y`); when it equals `ValueP1`, at (`P1_X`, `P1_Y`); in between, the position is interpolated linearly. `EnableXMin/XMax/YMin/YMax` with `MinXValue`… bound the movement. The `Gets_P0_Position`, `Gets_P1_Position` and `GoTo_P0_Position` properties are "buttons" in the Object Inspector: double-click them to capture the control's current position as P0/P1 or to send it back to P0.

![](img/THMIControlDislocatorAnimation2.png) Version **2** uses two independent tags — `PLCTagX` with `ValueP0x`/`ValueP1x` and `PLCTagY` with `ValueP0y`/`ValueP1y` — for two-dimensional movement (the position of a crane trolley, for instance).

##### Basic drawings

Equipment shapes drawn by PascalSCADA (through BGRABitmap, no external images), with `BodyColor`, `BorderColor`, `BorderWidth` and `SecurityCode`. They **have no `PLCTag`**: they are static, and you animate them with the property connectors (colour) or use the *Flow* versions described in [Flow controls](/flow-controls/).

| Control | Drawing |
|---|---|
| ![](img/THMIBasicValve.png) `THMIBasicValve` | Valve; `ValveType` picks the actuator (`vtSimple`, `vtPneumaticOnOff`, `vtPneumaticProportional`, `vtMotorisedProportional`, `vtPneumaticDrawer`), `ValveBodyPercent` the body/actuator ratio, `Mirrored` flips it. |
| ![](img/THMIBasicEletricMotor.png) `THMIBasicEletricMotor` | Electric motor; with `DrawPump = True` it draws a coupled pump. |
| ![](img/THMIFitaBasica.png) `THMIFitaBasica` | Belt conveyor (horizontal; `BodyHeight`). |
| ![](img/THMIRedlerBasico.png) `THMIRedlerBasico` | Chain conveyor (redler). |
| ![](img/THMIRoscaBasica.png) `THMIRoscaBasica` | Screw conveyor. |
| ![](img/THMIElevadorBasico.png) `THMIElevadorBasico` | Bucket elevator (`HeadAtLeft`, `HeadColor`, `FooterColor`, `BodyWidth`). |
| ![](img/THMIPolyline.png) `THMIPolyline` | Polyline — pipe, cable, wire. Points in `PointCoordinates`, `LineColor`, `LineWidth`, `PenStyle`. |
| `THMIBandeja` | Panel ("tray") whose border and background colours come from tags (`BorderColorPLCTag`, `BackgroundColorPLCTag`, `TColor` values) and whose text comes from a `TPLCStructString` (`BandejaTextPLCTag`). Made for silo and pit mimic panels driven by the PLC. |
| `THMIBasicVectorControl` | **SVG** drawing (`SVGContents`, pasted from the file) with `Stretch`/`Proportional`. It is the base of the vector flow controls, which recolour SVG elements per zone. |

##### Charts: TTagLinkedSeriesSource

`TTagLinkedSeriesSource` is a **TAChart** data source fed by a tag: link it to the `Source` property of a `TLineSeries` and it appends a (time, value) point on every update.

* **`PLCTag`** — the numeric tag.
* **`SourceUpdateType`** — `tlTagUpdate` (one point per read, even without change), `tlTagChange` (only when the value changes) or `tlCyclic` (every `CyclicUptimeTime` ms, with the last value).
* **`EnableXAxisMaxInterval`** / **`XAxisMaximumInterval`** — drops points older than the interval (in milliseconds), keeping a sliding chart.
* **`UseNowInsteadTagTimestamp`** — uses the PC clock instead of the tag timestamp.
* **`YMinOffset`** / **`YMaxOffset`** — margins added to the Y-axis extremes.

##### Step-by-step example

A minimal screen for a motor: state, speed and command.

1. Tags: `Motor_Status` (`TPLCTagNumber`, status word), `Motor_On` (`TTagBit` over `Motor_Status`, bit 0), `Motor_Speed` (`TPLCTagNumber`, `pttFloat`) and `Motor_Setpoint`.
2. A `THMIText` with `PLCTag = Motor_Status` and three zones: `ztEqual 0 → "Stopped"`, `ztEqual 1 → "Running"` (green), `ztBit 7 → "Fault"` (red, `BlinkTime = 500`, `BlinkWith` pointing to a fourth identical zone in white).
3. A `THMILabel` with `PLCTag = Motor_Speed`, `NumberFormat = "#0.0"`, `Sufix = " rpm"`.
4. A `THMIEdit` with `PLCTag = Motor_Setpoint`, `MinValue = 0`, `MaxValue = 1800`, `EnableMinValue`/`EnableMaxValue = True`, `SecurityCode = "operation"`.
5. A `THMICheckBox` with `PLCTag = Motor_On`, `CaptionTrue = "Switch off"`, `CaptionFalse = "Switch on"`, `SecurityCode = "operation"`.
6. A `THMIColorPropertyConnector` with `PLCTag = Motor_On`, one condition `ztEqual 1 → clLime` and the `DefaultZone → clSilver`, affecting `Shape1.Brush.Color`.

Without a line of code the screen shows the state and the speed, accepts the setpoint within range, switches the motor on and off and colours the shape — and all of it is disabled for a user without the `operation` code.


##### Related examples

* `examples/laz_weg_twp03_full_project` — a complete supervisory project for a WEG TPW03 PLC over Modbus RTU: `THMIEdit`, `THMICheckBox`, `THMILabel` and blocks on real screens.
* `examples/both_modbus_rtu_corn_weigh` — a weighing mimic with `THMIAnimation`, `THMIText`, `TTagBit` and blocks (Lazarus and Delphi).
* `examples/laz_modbus_tcp_example` — `THMIEdit`, `THMICheckBox`, `THMIText` and `THMILabel` over a Modbus TCP block.
* `examples/laz_comm_fault_control_icon_test` — the communication-fault badge on every kind of control.
* `examples/laz_isotcp_hourmeter` — `THMIAnimation` with a `TImageList` and blinking zones.
* `examples/hmi_bandeja` — `THMIBandeja` with colours and text coming from the PLC.
* `examples/hmi_flowvectorcontrol` and `examples/laz_svg_example` — `THMIFlowVectorControl` recolouring SVG elements per zone (see [Flow controls](/flow-controls/)).
* `examples/laz_numericexpr` — `THMITrackBar` and `THMILabel` around a `TNumericExprTag`.

Other HCl pages: [Flow controls](/flow-controls/), [Event and alarm loggers](/event-and-alarm-loggers/), [On-screen keyboard](/keyboard-manager/) and [Security system](/security-system/).
