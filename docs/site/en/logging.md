##### Introduction

![](img/TpSCADALogger.png) **`TpSCADALogger`** collects what happens inside PascalSCADA — communication, connections, scan threads, tags, events fired, security, database — and sends each message to the **screen**, to a **file**, to an **event**, or **nowhere**, according to a list of **rules**. A rule picks messages by **source** (the component name), **class**, **category** and **level**, so you decide, for example, that the traffic of one Modbus port goes to its own file, the S7 driver warnings go to the screen and every error also fires an event.

![](img/TpSCADALogMemo.png) **`TpSCADALogMemo`** is the screen target: a read-only memo that shows the lines the rules send to the screen.

Both live on the **PascalSCADA Utils** palette. `TpSCADALogger` is in the `pascalscada_common` package (no LCL: it also works in services and console programs); `TpSCADALogMemo` is in `pascalscada_hmi`.

##### How it works

Every part of the framework posts its messages to a single, global hub. Each active `TpSCADALogger` gets a copy and routes it through its rules. Several loggers may exist in the same program; each one gets every message.

Logging costs nothing while nobody listens: before building a message (formatting a text, dumping a frame as hex) the code asks the hub whether some active rule wants that category and level. Posting only copies the message into the logger's queue; the files, the screen and the event are handled by the logger's own thread, in batches, so a communication thread never waits for the disk or the screen. Screen and event are delivered in the **main thread**.

##### Levels and categories

| Level | Use |
|---|---|
| `llTrace` | Everything, including the bytes of each frame. A lot of data. |
| `llDebug` | Details useful to understand a problem. |
| `llInfo` | Normal events: connected, disconnected, started. |
| `llWarning` | Something went wrong and was recovered (timeout, reconnection). |
| `llError` | Something failed. |

| Category | Messages about |
|---|---|
| `lcComm` | bytes sent and received by a port |
| `lcConnection` | opening, closing, reconnecting, timeouts |
| `lcProtocol` | frames and requests of a protocol driver |
| `lcScan` | scan and update threads, queues |
| `lcTag` | tag reads, writes and values |
| `lcEvent` | events fired |
| `lcThread` | threads starting and stopping |
| `lcSecurity` | login, logout, authorization |
| `lcDatabase` | the database layer |
| `lcHMI` | the visual controls |
| `lcUser` | messages of your application |

##### TpSCADALogger properties

| Property | Description |
|---|---|
| `Active` | Turns the logger on and off (default `True`). At design time it never runs. |
| `Rules` | The routing rules, evaluated in order (see below). |
| `Display` | The screen target: a `TpSCADALogMemo` or any component that implements `IpSCADALogDisplay`. The Object Inspector lists only those. |
| `LineFormat` | How a line looks. Tokens: `%time%`, `%date%`, `%level%`, `%category%`, `%thread%` (`main`, `T1`, `T2`… in the order the threads first post), `%threadid%` (the system id), `%source%`, `%class%`, `%msg%`, `%data%` (the payload in hex, empty when there is none). Empty restores the default. |
| `OnLog` | Event fired, in the main thread, for each message a rule sends to `ltEvent`. Gets the message fields (`Entry`) and the formatted `Line`. |
| `MaxFileSize` | Size limit of a log file in bytes; over it the file is renamed to `.1`, `.2`… (`0` = no limit). |
| `MaxRotatedFiles` | How many renamed files are kept (default 5). |
| `FileIdleCloseMs` | A file that got nothing for this long is closed and reopened when needed (default 10 s). |
| `FlushIntervalMs` | How often the logger's thread writes what is queued (default 200 ms). |
| `MaxQueue` | Messages the queue holds; beyond it new messages are dropped and a line *N log messages dropped* is logged later. `Dropped` counts them. |

`Flush(TimeoutMs)` waits until everything posted before the call has been written and, in the main thread, shown. `PSLogFlushAll` does the same for every logger.

The default line looks like this:

```
2026-11-17 14:03:22.417 [WARN ] [connection] T2 Form1.ModBusTCP1 (TModBusTCPDriver): timeout, reconnecting
2026-11-17 14:03:22.419 [TRACE] [comm] T2 Form1.TCP_UDPPort1 (TTCP_UDPPort): TX | 00 01 00 00 00 06 01 03 00 00 00 0A
```

##### Rules

A message matches a rule when **all** of these hold:

| Property | Matches when |
|---|---|
| `Enabled` | the rule is enabled. |
| `MinLevel` | the message level is this one or higher (default `llInfo`). |
| `Categories` | the message category is in the set (default: all). |
| `SourceMask` | the source matches the mask. The source is the component path, like `Form1.ModBusTCP1`. Accepts `*` and `?`, ignores case, and takes several masks separated by `;`. Empty means everything. |
| `ClassMask` | the class of the source **or one of its ancestors** matches the mask — `TProtocolDriver` matches every driver. Same syntax as `SourceMask`. |

And then:

| Property | Effect |
|---|---|
| `Targets` | Where it goes: `ltScreen`, `ltFile`, `ltEvent`, any combination. **Empty drops the message.** |
| `FileName` | The file for `ltFile`. Accepts `%date%`, `%source%`, `%class%`, `%category%` and `%level%`, so one rule can write one file per day, per source or per class. A relative path is relative to the **executable's folder**; missing folders are created. |
| `StopProcessing` | When the rule matches, the following rules are not evaluated. With empty `Targets` it makes an **exclusion**. |

When several rules match, the message goes to all their targets; two rules on the same file write the line once.

Examples:

| Goal | Rule |
|---|---|
| Errors and warnings of everything on the screen | `MinLevel = llWarning`, `Targets = [ltScreen]` |
| Traffic of each port in its own daily file | `Categories = [lcComm]`, `MinLevel = llTrace`, `Targets = [ltFile]`, `FileName = logs/%source%-%date%.log` |
| Everything from the S7CommPlus driver in one file | `ClassMask = TS7CommPlusDriver`, `MinLevel = llDebug`, `Targets = [ltFile]`, `FileName = logs/s7plus.log` |
| Ignore one noisy component | first rule: `SourceMask = Form1.Debug*`, `Targets = []`, `StopProcessing = True` |

##### TpSCADALogMemo

A memo set up for logs: read-only, no word wrap, scroll bars. Link it to the logger's `Display`.

| Property | Description |
|---|---|
| `MaxLines` | Lines kept; the oldest go first (default 1000, `0` = no limit). |
| `AutoScroll` | Keeps the last line in view when new lines come (default `True`). Turn it off to read calmly. |

A monospaced font keeps the columns aligned. To show the log somewhere else (a list, a grid, a status bar) use the `OnLog` event, or write a component that implements `IpSCADALogDisplay` (one method, `AddLogLines(Lines: TStrings)`, called in the main thread).

##### Logging from your code

```pascal
uses pscadalog;

PSLog(Self, lcUser, llInfo, 'batch started');
PSLogFmt(Self, lcUser, llWarning, 'tank %d above %f', [TankNo, Level]);

//build expensive messages only when someone wants them
if PSLogWants(lcComm, llTrace) then
  PSLog(Self, lcComm, llTrace, 'frame', FrameBytes);
```

`Self` (the sender) becomes the source and the class of the message; it may be `nil`. `PSLog` may be called from any thread.

##### Programs without the LCL

Files work in any program. The screen and the `OnLog` event reach the main thread through `TThread.Queue`, so a console program or service must call `CheckSynchronize` from time to time (an LCL application already does it).

##### Related examples

* `examples/laz_pscadalogger` — a logger with three rules (screen, one file per category and day, an event counting the errors), a `TpSCADALogMemo`, a button to post messages of any level and category and a burst of four threads posting at the same time.
