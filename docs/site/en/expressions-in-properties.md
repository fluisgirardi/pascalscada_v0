##### Introduction

Configuring dozens of tags by hand — one address at a time, one index at a time — is where time is lost and numbers get mistyped. For that, several numeric properties of PascalSCADA components accept, in the **Object Inspector**, an **expression** instead of a number: you type `memaddress+2`, `Tag*4` or `selindex*30` and the IDE computes the value. Combined with Lazarus' **multiple selection**, this configures a hundred components in one keystroke.

It is a **design-time** feature: the expression is evaluated when you press Enter and what gets stored in the `.lfm` is the resulting number. Nothing changes at run time — for values computed at run time, use [`TNumericExprTag`](/tags/#TNumericExprTag).

The feature was announced in the post [Expressions on all properties of base tags](/2016/11/22/expressions-on-all-properties-of-base-tags/); the video below shows it in use:

https://www.youtube.com/watch?v=1E1MeYGuH8s

##### How to type it

In the Object Inspector, in the property field, type the expression and press Enter. Two forms:

| You type | What happens |
|---|---|
| `100` | A normal value. |
| `10*8+3` | **Absolute** expression: the property receives the result (83). |
| `+2`, `-10`, `*2`, `/2` | **Relative** expression: it starts with an operator, and the property's current value is put in front. `+2` on a `MemAddress = 40` gives 42; `*2` gives 80. |
| `memaddress+2` | Expression with **variables** (each property's list is below). |

The result is always an integer: divisions are truncated (`7/2` → 3). With a multiple selection the expression is evaluated **once for each selected component**, with that component's variables — which is what lets `memaddress+100` shift ten tags at once, each from its own address.

##### Syntax

The evaluator is Free Pascal's `TFPExpressionParser`, with the math functions enabled:

* operators: `+ - * / ^` (power), `mod`, parentheses;
* comparison and logic: `= <> < > <= >=`, `and`, `or`, `not`;
* `if(condition, ifTrue, ifFalse)` and `case(selector, v1, r1, v2, r2, …, default)`;
* functions: `abs`, `sqr`, `sqrt`, `round`, `trunc`, `int`, `frac`, `exp`, `ln`, `log`, `sin`, `cos`, `arctan`, `pi`;
* variable names are case-insensitive (`MemAddress` and `memaddress` are the same).

There is no `div`: use `trunc(a/b)` (or just `a/b`, already truncated when stored).

##### Where it works and which variables exist

**Tag addressing properties** — on `TPLCTagNumber`, `TPLCBlock`, `TPLCStruct` and `TPLCString`, the properties `PLCRack`, `PLCSlot`, `PLCStation`, `MemFile_DB`, `MemAddress`, `MemSubElement`, `MemReadFunction`, `MemWriteFunction` and, on blocks and structures, `Size`. Available variables: **the other properties of that list** (`plcrack`, `plcslot`, `plcstation`, `memfile_db`, `memaddress`, `memsubelement`, `memreadfunction`, `memwritefunction`) and **`Tag`** (the component's `Tag` property, a free integer). The property being edited is left out, to avoid a circular reference.

**`Index`** — on `TPLCBlockElement`, `TPLCStructItem` and `TPLCStructString`. Variable: **`Tag`**. (On `TPLCBlockElement` the field's drop-down also offers the block's valid indexes.)

**Position and size of any control** — `Left`, `Top`, `Width` and `Height` of every `TControl` on the form, not just PascalSCADA controls. Variables: `left`, `top`, `width`, `height`, `tag` and **`selindex`**.

**`Tag`** — the `Tag` property of any `TComponent`. Variables: the same position ones (`left`… when it is a control) and `selindex`.

**`selindex`** is the component's index within the current selection (0 for the first selected, 1 for the second…). It is the variable that turns a multiple selection into automatic numbering.

##### Recipes

All start from a multiple selection on the form or datamodule (Shift+click, or dragging a frame).

**Number the elements of a block.** Select the 16 hand-made `TPLCBlockElement`s in the desired order:

1. in `Tag`, type `selindex` → each gets 0, 1, 2… 15;
2. in `Index`, type `Tag` → `Index` = 0, 1, 2… 15.

The same goes for `TPLCStructItem` with regular offsets: `Index` = `Tag*2` for `INT` fields, `Tag*4` for `REAL`.

**Consecutive tags.** Ten `TPLCTagNumber`s that must point to D100, D101, … D109: select the ten, `Tag` = `selindex`, then `MemAddress` = `100+Tag`.

**Shift a set of addresses.** The DB moved and every tag must advance 50 bytes: select them and type `+50` in `MemAddress`. Changed DB: `MemFile_DB` = `12` on the whole selection.

**Copied struct.** You copied the items of a `TPLCStruct` to represent a second device whose structure sits 64 bytes further: select the copied items and type `+64` in `Index`.

**Write function consistent with the read one.** On Modbus tags, `MemWriteFunction` = `if(memreadfunction=1, 15, 16)` picks the right block write function for each tag, according to whether it reads coils or registers.

**Align controls.** Select ten `THMILabel`s and type `selindex*24+8` in `Top` and `16` in `Left`: a column with a 24 px pitch. A 4×4 button grid: `Left` = `(selindex mod 4)*90`, `Top` = `trunc(selindex/4)*40`.

**Stack controls of different sizes.** `Top` = `selindex*(height+4)` only works when all have the same height; for different heights, align in two passes or use Lazarus' *Anchor Editor*.

##### Limits

* Object Inspector only: not from code, nor in properties edited in other dialogs (Tag Builder, collection editors).
* Variables read the **current** value of the other selected components; an expression depending on a property you are still going to change must be typed after it.
* Only the integer properties listed above. `RefreshTime`, `MinValue`, the zones' `Value1` and others do not accept expressions (for now — the list is in `src/scada_dsng/scadareg.pas` and `src/hmi/hmiregister.pas`, and extending it means registering one more `TTagAddressPropertyEditor`).
* A syntax error is reported by the IDE and the property is left unchanged.

##### Related examples

* `examples/laz_isotcp_demonstration` and `examples/laz_isotcp_hourmeter` — dozens of `TPLCBlockElement`s and `TTagBit`s with sequential indexes, the typical scenario of the recipes above.
* `examples/TagTypes` — block elements and structure items with computed offsets.
