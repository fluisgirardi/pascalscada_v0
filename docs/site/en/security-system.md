##### Introduction

PascalSCADA's security system answers one simple question: **may the logged-in user touch this control?** It has three parts:

1. **Security codes** (`SecurityCode`) — free-text strings you assign to the [HCl](/hmi-control-library-hcl/) controls and to the security *actions*: `"setpoints"`, `"maintenance"`, `"recipes"`. A control without a code is free for everyone.
2. **The security manager** (`TControlSecurityManager`) — a single object per application, created automatically (reach it with `GetControlSecurityManager`). Every control with a `SecurityCode` registers with it; on each login and logout it asks, for each code, whether the current user has access and enables or disables the controls.
3. **A user-management component** (the **PascalSCADA User Management** palette) — the one that actually knows which users exist, validates passwords and tells which codes each user may access. There can be only **one** per application (creating a second raises an exception). There are three implementations, described below.

Without a user-management component the security manager answers *yes* to everything: `SecurityCode`s have no effect and nothing is disabled. This lets you build the screens first and wire security in later.

##### How it works

```
THMIEdit.SecurityCode = "setpoints"
        │ registers the code
        ▼
TControlSecurityManager ──CanAccess("setpoints")?──▶ TxxxUserManagement
        │                                                    │
        ◀──────────────── True / False ──────────────────────┘
        ▼
THMIEdit.Enabled := (answer) and (the Enabled you set)
```

* When a `SecurityCode` is assigned, the control calls `ValidateSecurityCode` (the user manager may reject unknown codes by raising an exception) and `RegisterSecurityCode` (so the manager can list the codes used by the application, which the user managers use to build their permission screens).
* **Login**: `GetControlSecurityManager.Login` opens the standard authentication dialog (user and password, or an RFID card if a reader is attached). If the password is right, the manager re-evaluates every control (`UpdateControls`). After `LoginRetries` wrong attempts the dialog freezes for `LoginFrozenTime` milliseconds. `Login(user, password, uid)` does the same without a dialog.
* **Logout**: `GetControlSecurityManager.Logout` — every control with a code is disabled again.
* From code, `CanAccess(code)` returns whether the current user has access, and `TryAccess(code)` raises an exception if not — handy at the top of a procedure that must not run without permission.

Properties and events available on every user manager:

| Member | Description |
|---|---|
| `UserLogged`, `CurrentUserLogin`, `CurrentUserName`, `UID`, `LoggedSince` | State of the current login (read-only). |
| `LoginRetries` | Password attempts before the dialog freezes (0 = never freezes). |
| `LoginFrozenTime` | Freeze time, in ms. |
| `ChipCardReader` | Card/RFID reader for password-less login — see [RFID reader login](/rfid-reader/). |
| `SuccessfulLogin`, `FailureLogin` | Events for successful and failed logins (for auditing, e.g. with the [event logger](/event-and-alarm-loggers/)). |
| `UserChanged` | `(Sender, OldUsername, NewUserName)` event fired on login and logout. |

##### Choosing the user manager

| Component | Where the users live | Use it when… |
|---|---|---|
| ![](img/TCustomizedUserManagement.png) `TCustomizedUserManagement` | Wherever you want: you answer events | you have your own user database, an LDAP, a file — anything. The most used one. |
| ![](img/TCentralUserManagement.png) `TCentralUserManagement` | On an HTTP/JSON server on the network | several stations must share the same users and permissions, with a login reflected on all of them. |
| ![](img/TWinCCUserManagement.png) `TWinCCUserManagement` | In the SIMATIC WinCC *User Administrator* (Windows only) | the application runs next to a WinCC and must use the same users and authorizations. |

##### TCustomizedUserManagement {#TCustomizedUserManagement}

![](img/TCustomizedUserManagement.png) Knows nothing about users: it delegates everything to events you implement. Two are mandatory:

```pascal
// Validates user and password. Return the UID (any integer identifying the user)
// and ValidUser. LoginAction is True when the call comes from the login dialog and
// False when it comes from a "signature" check (TPascalSCADACheckSpecialTokenAction).
procedure TForm1.UserManagementCheckUserAndPass(user, pass: UTF8String;
  out aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean);
begin
  ValidUser := Users.CheckPassword(user, pass, aUID);
end;

// Tells whether the logged-in user may access the security code.
procedure TForm1.UserManagementCanAccess(securityCode: UTF8String;
  var CanAccess: Boolean);
begin
  CanAccess := Users.HasPermission(UserManagement.UID, securityCode);
end;
```

The other events:

| Event | Purpose |
|---|---|
| `OnUIDCanAccess(aUID, securityCode, var CanAccess)` | Like `OnCanAccess`, but for a specific user — used by the signature check, when the one authorizing is not the one logged in. |
| `OnGetUserName`, `OnGetUserLogin` | Return the current user's name and login, shown in `CurrentUserName`/`CurrentUserLogin`. |
| `OnLogout` | Clear whatever you kept about the user. |
| `OnManageUsersAndGroups` | Open your own user-management screen (what `Manage` and `TPascalSCADAManageUsersAction` call). |
| `OnValidadeSecurityCode(code)` | Raise an exception if the code does not exist in your records — protects against typos in the controls' `SecurityCode`. |
| `OnRegisterSecurityCode(code)` | Called for each new code that shows up in the application; store it so it appears on the permissions screen. |
| `OnCheckUserChipCard` | Validates the code of an RFID card read by the `ChipCardReader`. |

The `examples/laz_custom_user_management` example implements this with three hard-coded users — good for understanding the mechanism. The `examples/laz_customusermanagement` example is a complete implementation with **users, groups and permissions in PostgreSQL** (through Zeos), including the user, group and permission screens and MD5 password hashing — the database model is in `docs/db_user_groups_modelagem.xml`. Use it as the starting point for your own.

##### TCentralUserManagement {#TCentralUserManagement}

![](img/TCentralUserManagement.png) Client of an HTTP **authentication server**: every password and permission check becomes a JSON call to the server, and a thread watches the `userchanged` endpoint so that a login made on one station shows up on the others.

| Property | Description |
|---|---|
| `AuthServer`, `AuthServerPort`, `UseSSL` | Server address. |
| `UseCachedAuthorizations` | Keeps permission answers locally so the server is not queried on every `CanAccess`; `CachedUpdateCount` shows how many times the cache was refreshed. |
| `UseCentralUserAsLocalUser` | When `True`, the user logged in on the server (from another station) becomes this station's user too — single sign-on across the plant. |
| `RaiseExceptOnConnFailure` | Raises an exception when the server does not answer; when `False`, it fails silently by denying access. |
| `OnUserChanged` | User-change notification coming from the server. |

The protocol is simple — a `POST` of a JSON object to `/checkuserpwd`, `/uidcanaccess`, `/validadesecuritycode`, `/registersecuritycode`, `/enumsecuritycodes` and `/userchanged`. The **`examples/laz_security_webserver_WinCC`** example is a ready-made server that exposes one machine's `TWinCCUserManagement` to the network — so every PascalSCADA station uses the WinCC users — and serves as a template for writing a server over your own user database.

##### TWinCCUserManagement {#TWinCCUserManagement}

![](img/TWinCCUserManagement.png) Windows only, with SIMATIC WinCC installed on the machine: it uses WinCC's *UseAdmin* DLL to authenticate and query authorizations. The login dialog is PascalSCADA's standard one, but user and password are validated by WinCC; `Manage` opens WinCC's *User Administrator*.

Since WinCC identifies authorizations by number (1 = *User administration*, 2 = *Value input*, 1000 = *System change*…), the **`AuthorizationList`** property maps each PascalSCADA security code to a number, one line per code, in the form `number:code`:

```
2:setpoints
1000:maintenance
1001:recipes
```

A WinCC user holding authorization 2 has access to every control with `SecurityCode = "setpoints"`. Codes not in the list are rejected.

##### Security actions

On the **PascalSCADA User Management** tab of the `TActionList` editor (*New standard action*) there are six ready-made actions to hook to buttons, menu items and toolbars:

| Action | What it does |
|---|---|
| `TPascalSCADALoginAction` | Opens the login dialog. |
| `TPascalSCADALogoutAction` | Logs out. Enabled only while someone is logged in. |
| `TPascalSCADALogin_LogoutAction` | Both in one button: `WithoutUserLoggedInCaption`/`Hint`/`ImageIndex` when nobody is logged in and `WithUserLoggedInCaption`/`Hint`/`ImageIndex` when there is a user — the button swaps text and icon by itself. |
| `TPascalSCADAManageUsersAction` | Opens the user-management screen (`Manage`). |
| `TPascalSCADASecureAction` | An ordinary action with a `SecurityCode`: use it on any button or menu that must require permission. With `DisableIfNotAuthorized = True` (default) it is disabled for whoever lacks the code; with `False` it stays enabled, but `Execute` does not run `OnExecute` without permission. |
| `TPascalSCADACheckSpecialTokenAction` | **Supervisor signature**: on execution, if the logged-in user lacks the `SecurityCode` (or always, with `RequireLoginAlways = True`), it opens a dialog asking for the user and password of **another** user who has the code. Only then does it fire `OnExecute`; the login of whoever authorized it is left in `AuthorizedBy` for you to record in the log. The logged-in user does not change. |

The action's `Hint` is shown in the signature dialog as the explanation of what is being authorized.

##### Step by step

1. Drop a `TCustomizedUserManagement` on the main form (or on a datamodule created before the forms). Implement `OnCheckUserAndPass` and `OnCanAccess`.
2. Drop a `TActionList`, add a `TPascalSCADALogin_LogoutAction` and hook it to a toolbar button.
3. On the controls that need protection, fill `SecurityCode` — the same code on many controls is the norm (`"operation"` on every command, `"setpoints"` on every tuning `THMIEdit`).
4. On ordinary menus and buttons, use `TPascalSCADASecureAction` with the appropriate code.
5. For critical operations (deleting a recipe, forcing an output), use `TPascalSCADACheckSpecialTokenAction` with `SecurityCode = "supervisor"`.
6. Run: everything with a code starts disabled; after logging in with a user holding the codes, the controls enable; logging out disables them again.

From code, when needed:

```pascal
uses ControlSecurityManager;

if GetControlSecurityManager.CanAccess('recipes') then
  LoadRecipe;

GetControlSecurityManager.TryAccess('maintenance');  // exception if not allowed
```
