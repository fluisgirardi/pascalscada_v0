##### Introduction

![](img/TNumericExprTag.png) **`TNumericExprTag`** is a **virtual** numeric tag: it has no driver and no address, and its value is the result of an **expression** computed from other tags. Whenever one of the source tags changes, the expression is re-evaluated and, if the result differs, the `TNumericExprTag` changes too — notifying the [HCl](/hmi-control-library-hcl/) controls, the `TTagBit`s and the code linked to it, exactly as a communication tag would.

It is for everything *derived* from device values that is not worth programming in the PLC or in application code:

* a total flow as the sum of two lines (`A+B`);
* a percentage (`A/B*100`), a temperature difference (`A-B`), a power (`A*B/1000`);
* a combined alarm or state (`ifthen((A>80) and (B=1), 1, 0)`);
* a physical quantity from a measurement (`sqrt(A)*12.5`).

It lives on the **PascalSCADA Tags** palette. For plain scale conversions of **one** tag a [scale processor](/scale-processors/) is the better fit — the expression comes in when the result depends on more than one tag or on logic.

##### Properties

| Property | Description |
|---|---|
| `A` … `J` | Up to ten numeric source tags (any `TPLCNumber`: `TPLCTagNumber`, `TPLCBlockElement`, `TPLCStructItem`, `TTagBit` or another `TNumericExprTag`). Each enters the expression through its letter. |
| `Expression` | The expression. Changing it recomputes the value at once. |
| `ScaleProcessor` | Scale applied to the result — `ValueRaw` is the expression result, `Value` is it scaled. |
| `LastEvalutionError` | `OK`, or the message of the last evaluation error (syntax, unbound variable, invalid type). Read-only. |
| `Value`, `ValueRaw` | The result. **Read-only in practice**: writing to it does nothing besides firing `OnWriteFail`. |
| `OnValueChangeFirst`, `OnValueChangeLast` | Fired when the result changes (before and after notifying the controls). |
| `OnReadFail` | Fired when the evaluation fails. |
| `OnWriteFail` | Fired when someone tries to write to the tag. |

Variables enter the expression with the source tag's **`Value`** — that is, already scaled. If you need the raw value, link an unscaled `TTagBit`/`TPLCTagNumber` or undo the scale in the expression.

##### Expression syntax

The evaluator is Free Pascal's `TFPExpressionParser`, with the math and boolean functions enabled, plus one function of PascalSCADA's own:

| Category | Available |
|---|---|
| Operators | `+ - * / ^` (power), `mod`, parentheses |
| Comparison | `= <> < > <= >=` |
| Logic | `and`, `or`, `not`, `xor`; `true`, `false` |
| Bits | `shl`, `shr` |
| Conditional | **`ifthen(condition, ifTrue, ifFalse)`** — both values must be numeric |
| Math | `abs`, `sqr`, `sqrt`, `round`, `trunc`, `int`, `frac`, `exp`, `ln`, `log`, `sin`, `cos`, `arctan`, `pi` |

Rules that come from the evaluator and are worth knowing:

* **A bare comparison is worth 1 or 0**: `Expression = 'A > 5'` is a perfect boolean tag for a `THMICheckBox` or a zone.
* The result may be integer or real; both become the tag's `Double` without loss. A **text** result (`'"abc"'`) is an error.
* Variable letters are case-insensitive (`a+b` = `A+B`), but prefer upper case to match the properties.
* A variable used in the expression with no tag linked (`A+B` with `B = nil`) is an evaluation error, not zero.
* Division by zero is an **evaluation error** ("division by zero" in `LastEvalutionError`, previous value kept) — guard it in the expression: `ifthen(B=0, 0, A/B)`.

Examples:

```
A + B
A / B * 100
(A - 32) * 5 / 9
ifthen(A > B, A, B)                       -- maximum
ifthen((A > 80) and (B = 1), 1, 0)        -- combined alarm
ifthen(B = 0, 0, A / B)                   -- guarded division
round(A / 10) * 10                        -- round to tens
(A shr 4) mod 16                          -- high nibble of a byte
sqrt(sqr(A) + sqr(B))                     -- vector magnitude
```

##### When the value is recomputed

`TNumericExprTag` has no `RefreshTime`: it is **event driven**. The expression is evaluated:

* when any tag linked in `A`…`J` changes value (the source's `OnValueChange`) or fails a write;
* when `Expression` or one of the variables is changed;
* when the form/datamodule finishes loading.

If the result equals the previous one, nothing is notified — an `ifthen(A>80, 1, 0)` fires `OnValueChange` only when it crosses the threshold, not on every read of `A`. That is why it is cheap: ten source tags read every 100 ms cost ten evaluations per cycle, all on the main thread, and only real changes propagate.

The tag's `ClockMonotonicTimeStamp` is the instant of the last result change.

##### Errors

When the evaluation fails — wrong syntax, unbound variable, text result — the tag:

1. stores the message in `LastEvalutionError`;
2. **keeps the previous value** (it does not become zero);
3. fires `OnReadFail`, which makes the linked controls show the communication-fault badge, as if the tag had stopped communicating.

Once the expression is fixed (or the missing variable linked), the next successful evaluation clears the error (`LastEvalutionError = 'OK'`) and the badge goes away. At design time, look at `LastEvalutionError` in the Object Inspector right after typing the expression: the error shows there immediately.

If a source tag is destroyed, the matching variable goes back to `nil` by itself — and the expression starts failing with an unbound variable, instead of touching a dead object.

##### Chaining expressions

Since `TNumericExprTag` is a `TPLCNumber`, it can be a variable of **another** `TNumericExprTag`. That lets you split a big computation into named steps (`Total_Flow = A+B+C`, `Line1_Percent = A/B*100` with `B = Total_Flow`) and reuse the intermediate result in several places. Avoid cycles (a tag depending, directly or indirectly, on itself): the evaluation goes into a notification loop.

It can also be the source of a [`TTagBit`](/tags/#TTagBit) (with `UseRawValue` as needed) and of any HCl control.

##### Step-by-step example

Show a motor's efficiency from electrical and mechanical power read from the PLC, with an alarm below 85 %:

1. Communication tags `Electrical_Power` and `Mechanical_Power` (`TPLCTagNumber`, `pttFloat`).
2. A `TNumericExprTag` named `Efficiency`: `A = Mechanical_Power`, `B = Electrical_Power`, `Expression = 'ifthen(B = 0, 0, A / B * 100)'`.
3. A `THMILabel` on `Efficiency` with `NumberFormat = '#0.0'` and `Sufix = ' %'`.
4. Another `TNumericExprTag` named `Low_Efficiency`: `A = Efficiency`, `Expression = 'ifthen(A < 85, 1, 0)'`.
5. A `THMIColorPropertyConnector` on `Low_Efficiency` painting the panel red when it is 1 — or a [`THMIAlarmLogger`](/event-and-alarm-loggers/) recording the alarm.

No line of code, and nothing added to the PLC program.

##### Related examples

* `examples/laz_numericexpr` — two `TPLCTagNumber`s driven by `THMITrackBar`s and a `TNumericExprTag` with `Expression = 'A+B'` shown in a `THMILabel`; the place to try the expressions of the table above.
