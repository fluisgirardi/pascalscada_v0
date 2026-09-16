##### Introduction

![](img/THMIDBConnection.png) `THMIDBConnection` is PascalSCADA's database access component: it connects to a DBMS and runs SQL commands **asynchronously**, without freezing the interface while the query runs. It lives in the `pascalscada_db` package (which does not depend on the LCL, so it also works in console applications) and is the base of components such as [`THMIEventLogger` and `THMIAlarmLogger`](/event-and-alarm-loggers/), besides serving your own screens — user management, recipes, history.

Underneath, it uses Free Pascal's **SQLdb** (no longer ZeosLib, dropped as a dependency from version 0.7.7 on — see [How to install](/how-install-pascalscada/)). That means `THMIDBConnection` is ready for whatever database SQLdb has a connector for; it is not an ORM and does not hide SQL from you — you write the command and it takes care of the queue, the thread and delivering the result back.

##### Supported databases

The **`Protocol`** property picks the database. In the Object Inspector it comes with a fixed drop-down list:

| Protocol | SQLdb connector used | Note |
|---|---|---|
| postgresql | PostgreSQL | |
| sqlite | SQLite3 | `Database` is the path of the `.db`/`.sqlite` file. |
| mysql | MySQL 5.7 | Only the MySQL 5.7 wire protocol driver is supported for now — it still works against newer servers, through their compatibility protocol. |
| firebird | Firebird | |

Values inherited from older installations with a version suffix (e.g. `mysql-5.7`, the format ZeosLib used) still work: only the part before the dash is used to pick the connector.

##### Connection properties

| Property | Description |
|---|---|
| Protocol | One of the four protocols in the table above. |
| HostName | Address or name of the database machine. Not used with SQLite. |
| Port | Connection port. `0` leaves the connector's default port. |
| Database | Database name (Postgres/MySQL/Firebird) or file path (SQLite). |
| User, Password | Access credentials. |
| Catalog | Only meaningful on databases whose SQLdb connector has a catalog concept; ignored on most. |
| Properties | A list of `key=value` pairs passed straight to SQLdb's connection parameters (e.g. connector-specific charset or SSL options). |
| LibraryLocation | Path to a native client library (`libpq.so`, `libmysqlclient.so`…) when it is not on the system's default path. |
| Connected | Opens or closes the connection. |
| ReadOnly | See its own section below. |

##### How commands are executed

`THMIDBConnection` does not expose a live-bound `TDataset`: it queues SQL commands on its own thread (`TProcessSQLCommandThread`), which runs them **in order**, one at a time, over the same connection. That avoids two screens racing for the same SQLdb connection (which is not thread-safe) and keeps the interface responsive even with a slow query.

The two methods that put work on the queue:

```pascal
procedure ExecSQL(sql: UTF8String; ReturnDatasetCallback: TReturnDataSetProc;
                   ReturnSync: Boolean = true; NewConnection: Boolean = false);

procedure ExecTransaction(statements: THMIDBConnectionStatementList;
                   ReturnTransactionResult: TReturnTransactionStatementsProc;
                   FreeStatemensAfterExecute: Boolean;
                   ReturnSync: Boolean = true; NewConnection: Boolean = false);
```

* **`ExecSQL`** runs a single command. If it starts with `SELECT`, the result arrives ready-made — a `TFPSBufDataSet` (an in-memory dataset, see below) — in `ReturnDatasetCallback`; for `INSERT`/`UPDATE`/`DELETE`/DDL, pass `nil` as the callback or ignore the dataset parameter. Each `ExecSQL` runs inside its own transaction, committed automatically at the end if there was no error.
* **`ExecTransaction`** runs a **list** of commands as a single transaction: if any of them fails, all are rolled back. `FreeStatemensAfterExecute` decides whether the string list is freed by the component when done. The callback reports success/failure, which line in the list it stopped at (`LineOfError`) and the exception, if there was one.
* **`ReturnSync`** decides **on which thread** your callback runs: `True` (default) brings it back to the main thread through `Synchronize` — safe to touch visual controls; `False` calls the callback **on the database thread itself**, faster but only safe for code that does not touch the UI.
* **`NewConnection`**: when `True`, forces the connection closed and reopened before running the command. Useful to isolate a command from a suspect connection state, at the cost of the reconnect time.
* **`GetPendingSQLCommands`** reports how many commands are still queued — usable as a visual "queue full" indicator on a screen that fires many writes.

The dataset that arrives in `ReturnDatasetCallback` (signature `procedure(Sender: TObject; DS: TFPSBufDataSet; error: Exception)`) is created exclusively for that call — you own it from there on: bind it to a `TDataSource`/`TDBGrid` and free it (`DS.Free`) once you are done with it. On error, `DS` comes back `nil` and `error` carries the original SQLdb exception.

##### Read-only access

With **`ReadOnly = True`**, every command that is not a `SELECT` is refused before it reaches the database — the callback gets an error, nothing is executed. Use it on a query-only screen, or on a second connection instance that must only display data, never change it.

##### Direct synchronous access

For the cases where you want to use SQLdb data components (`TSQLQuery`, `TDataSource`) bound directly to the connection — an editable `DBGrid`, for instance — instead of going through the asynchronous queue, `THMIDBConnection` exposes the internal connection:

```pascal
function  GetSyncConnection: TSQLConnector;
procedure LockSyncConnection;
procedure UnlockSyncConnection;
```

`GetSyncConnection` returns the `TSQLConnector` used internally. Since it is shared with the asynchronous thread, wrap any direct use in `LockSyncConnection`/`UnlockSyncConnection` (the same critical section the queue uses) so it does not collide with an `ExecSQL` in flight.

##### Formatting SQL literals

Building SQL commands by string concatenation is the classic door into SQL injection and into locale bugs (a `FloatToStr` that comes out with a decimal comma in a pt-BR locale and breaks the `INSERT`). For that, `THMIDBConnection` has four class functions — callable without an instance:

| Function | Does |
|---|---|
| FormatSQLString(str, EmptyIsNull=False) | Wraps the text in single quotes and **doubles** every internal single quote (`O'Brien` becomes `'O''Brien'`), neutralizing injection attempts. With `EmptyIsNull = True`, an empty string becomes the `NULL` literal instead of `''`. |
| FormatSQLNumber(number, decimalPlaces=0) | Always formats with a **dot** as the decimal separator, regardless of the operating system's locale. |
| FormatPGDatetime(dateTime) | Formats as `'YYYY-MM-DD HH:NN:SS.ZZZ'`, quoted — the timestamp literal accepted by PostgreSQL (and most databases), with milliseconds, independent of locale. |
| FormatSQLUUID(uuid) | Formats a `TGuid` as a quoted text literal, with no braces — the format Postgres and most databases expect for UUID columns. |

```pascal
SQL := 'INSERT INTO events (message, value, at, id) VALUES (' +
        THMIDBConnection.FormatSQLString(Message) + ', ' +
        THMIDBConnection.FormatSQLNumber(Value, 2) + ', ' +
        THMIDBConnection.FormatPGDatetime(Now) + ', ' +
        THMIDBConnection.FormatSQLUUID(NewGuid) + ')';
DBConn.ExecSQL(SQL, nil, false);
```

##### Step-by-step example

Record an event asynchronously, without freezing the screen:

1. Drop a `THMIDBConnection`: `Protocol = 'postgresql'`, `HostName`, `Database`, `User`, `Password`, `Connected = True`.
2. On a button click (or any event):

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  SQL: String;
begin
  SQL := 'INSERT INTO events (message, at) VALUES (' +
          THMIDBConnection.FormatSQLString(EdMessage.Text) + ', ' +
          THMIDBConnection.FormatPGDatetime(Now) + ')';
  HMIDBConnection1.ExecSQL(SQL, nil, false);
end;
```

3. For a query that returns data, implement the callback:

```pascal
procedure TForm1.LoadEvents;
begin
  HMIDBConnection1.ExecSQL('SELECT * FROM events ORDER BY at DESC LIMIT 100',
                            @ShowEvents, true);
end;

procedure TForm1.ShowEvents(Sender: TObject; DS: TFPSBufDataSet; error: Exception);
begin
  if Assigned(error) then begin
    ShowMessage('Error: ' + error.Message);
    Exit;
  end;
  DataSource1.DataSet := DS;   // DBGrid1.DataSource = DataSource1
end;
```

Since `ReturnSync = true`, the callback already arrives on the main thread — you can bind `DS` straight to a `TDataSource` with no extra `Synchronize`.

##### Related examples

* `examples/laz_hmialarms` — `THMIDBConnection` feeding a `THMIAlarmLogger`, with a `DBGrid` showing the active alarms.
* `examples/laz_hmieventlogger` — `THMIDBConnection` with `THMIEventLogger`, recording the event history.
