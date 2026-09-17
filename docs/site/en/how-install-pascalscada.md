##### Introduction

First of all, feel free to send corrections or alternative ways to this method 🙂

The main goal of this page is to show how to install the current PascalSCADA on the current **Lazarus** version. Installing it on outdated Lazarus versions is a matter for another page — or feel free to write the instructions for the IDE you want.

**Warning**: PascalSCADA depends on other packages, which must be installed (or will be installed along with it):

* **BGRABitmap** — used by the HCl drawn controls. Available in Lazarus' *Online Package Manager*.
* **TAChart** — used by `TTagLinkedSeriesSource`. Ships with Lazarus.
* **SQLDB** (`SQLDBLaz`) — used by `THMIDBConnection`. Ships with Lazarus. *ZeosLib is no longer required since version 0.7.7.*

**DOUBLE WARNING:** PascalSCADA is not like other SCADA systems. It is a package, plugin or add-on (call it what you like) that lets Lazarus build HMI/SCADA applications. So an installer with "Next", "Next" and "Finish" buttons is not available.

##### **What do I need before starting?**

1. **Lazarus** 2.0 or newer, with **Free Pascal 3.0 or newer** — PascalSCADA is developed and tested with FPC 3.2.2 and the latest stable Lazarus. On Windows, get Lazarus from the official page: [http://www.lazarus-ide.org/](http://www.lazarus-ide.org/). On Linux/FreeBSD, get it from your distribution's software repository or, for the newest version, with [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe).
2. **The PascalSCADA code**, from one of two sources:
    * **GitHub** (recommended — it is where development happens): `git clone https://github.com/fluisgirardi/pascalscada_v0.git`, or download the [master branch ZIP](https://github.com/fluisgirardi/pascalscada_v0/archive/refs/heads/master.zip) and unpack it in a folder of your choice.
    * Lazarus' **Online Package Manager** (*Package → Online Package Manager*): search for *PascalSCADA*. It is the shortest path, but the package published there is version 0.7.7 (2021); GitHub has the latest fixes and drivers.
3. A program to unpack `.zip` files, if you don't use `git`.

The SourceForge and SVN mentioned in older versions of this page are no longer used.

##### The packages

The PascalSCADA folder holds seven `.lpk` files:

| Package | Contains | Depends on |
|---|---|---|
| `pascalscada_common.lpk` | Threads, events, CRC, strings — the base of everything, no LCL. | FCL |
| `pascalscada.lpk` | Communication ports, protocol drivers, tags and scales. Works without the LCL (console applications and services). | `pascalscada_common` |
| `pascalscada_db.lpk` | `THMIDBConnection` and `TFPSBufDataSet`. | `SQLDBLaz` |
| `pascalscada_dsng.lpk` | The design-time part: palette registration, Tag Builders, property editors. | `pascalscada`, `IDEIntf` |
| `pascalscada_hmi.lpk` | The HMI control library (HCl), security, loggers, on-screen keyboard. | `pascalscada`, `pascalscada_db`, `pascalscada_dsng`, `BGRABitmapPack`, `TAChartLazarusPkg` |
| `pascalscada_full.lpk` | **Meta-package**: installs `pascalscada_dsng`, `pascalscada_hmi` and `pascalscada_db` at once. The one you install. | the three above |
| `pascalscada_externallibs_hmi.lpk` | **Optional**, Linux/FreeBSD only: the `TSycRFIDReader` RFID reader. Requires `libhidapi` on the system (`sudo apt install libhidapi-dev`) — without it Lazarus does not start after installing this package. See [Security system](/security-system/#rfid). | `pascalscada_hmi` |

##### Installing…

Well, the first thing to do is to install Lazarus. I don't want to explain how to install Lazarus on every operating system/platform in this world, because installing Lazarus is a very simple task, both on Windows (with the "Next", "Next", "Finish" installer) and on Linux (`sudo apt install lazarus`). At the end of the Lazarus installation, open it and you should see something like this:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051.png"><img class="aligncenter wp-image-71" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_051-300x167.png" alt="Clean Lazarus installation" width="1431" height="797" /></a>

If your freshly installed Lazarus looks like the picture above, you are on the right track.

**Install BGRABitmap first**: *Package → Online Package Manager*, tick *BGRABitmap* and click *Install*. Lazarus downloads, compiles and rebuilds itself. (If you are going to install PascalSCADA through the OPM, skip this step — it resolves the dependency by itself.)

The next step is to install the PascalSCADA package. Please forget Lazarus for a moment. Go to the folder where you cloned or unpacked PascalSCADA.

Then, back in Lazarus, open the menu shown below (*Package → Open Package File (.lpk)*):

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338.png"><img class="aligncenter wp-image-72" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-000338-300x165.png" alt="Installing a new package in Lazarus" width="1431" height="786" /></a>

Click it and a file dialog shows up:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052.png"><img class="aligncenter wp-image-73" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_052-300x165.png" alt="Finding the PascalSCADA files" width="1427" height="784" /></a>

Browse to the PascalSCADA folder. It must contain the seven `.lpk` files of the table above. If you don't see one of them, check the downloaded file, paths, etc. If everything is OK, your screen should look like this:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053.png"><img class="aligncenter wp-image-75" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_053-300x166.png" alt="Finding the PascalSCADA files (full view)" width="1428" height="791" /></a>

Select **`pascalscada_full.lpk`** and open it. The `pascalscada_full` package window shows up:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054.png"><img class="aligncenter wp-image-76" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_054-300x165.png" alt="PascalSCADA full-install package opened" width="1429" height="788" /></a>

The last step is to install the package into your IDE. Click the "Use >>" button and then the "Install" menu, as shown below:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819.png"><img class="aligncenter wp-image-77" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-16-001819-300x167.png" alt="Installing PascalSCADA into the Lazarus IDE" width="1427" height="794" /></a>

`pascalscada_full` is a meta-package, used only to make the full installation of PascalSCADA into your Lazarus IDE simple. Because of that, you will be bothered by a few messages:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055.png"><img class="aligncenter wp-image-78" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Não-é-um-pacote-de-instalação_055-300x123.png" alt="PascalSCADA meta-package: needed to simplify the full installation" width="502" height="205" /></a>

Click "Install it, I like the fat" to carry on. The next screen lists every package that will be installed into your IDE during the process:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056.png"><img class="aligncenter wp-image-79" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_056-300x167.png" alt="List of the additional packages that will be installed" width="1421" height="790" /></a>

A closer picture (the current list is `pascalscada_common`, `pascalscada`, `pascalscada_db`, `pascalscada_dsng` and `pascalscada_hmi`; the screenshot is from a version that still included ZeosLib):

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057.png"><img class="aligncenter wp-image-80" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Pacotes-instalados-automaticamente_057-300x204.png" alt="List of the additional packages that will be installed" width="502" height="341" /></a>

Click OK to continue and reach the final question about rebuilding the IDE:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058.png"><img class="aligncenter wp-image-81" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Reconstruir-Lazarus_058-300x102.png" alt="Confirming the Lazarus rebuild" width="502" height="171" /></a>

To finish, click "Yes". The installation starts and your IDE is rebuilt. When the rebuild ends, Lazarus restarts. **If you have a project with unsaved changes, be careful!!**

After the IDE restarts, you should find the following component palettes in your Lazarus:

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059.png"><img class="aligncenter wp-image-83" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_059-300x27.png" alt="PascalSCADA installed: communication ports tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060.png"><img class="aligncenter wp-image-84" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_060-300x27.png" alt="PascalSCADA installed: protocols tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061.png"><img class="aligncenter wp-image-85" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_061-300x27.png" alt="PascalSCADA installed: utilities tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062.png"><img class="aligncenter wp-image-86" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_062-300x27.png" alt="PascalSCADA installed: tags tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063.png"><img class="aligncenter wp-image-87" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_063-300x27.png" alt="PascalSCADA installed: screen controls tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064.png"><img class="aligncenter wp-image-88" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_064-300x27.png" alt="PascalSCADA installed: user management and security tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065.png"><img class="aligncenter wp-image-89" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_065-300x27.png" alt="PascalSCADA installed: database tab" width="1221" height="108" /></a>

<a href="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066.png"><img class="aligncenter wp-image-90" src="http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_066-300x27.png" alt="PascalSCADA installed: FreePascal inherited components tab" width="1221" height="108" /></a>

If you can see these palettes, congratulations, you have successfully installed PascalSCADA into your Lazarus IDE. To be sure, open and build one of the examples — `examples/laz_modbus_tcp_example` is a good start.

##### Installing from the command line

For those who prefer the terminal, or to set up a build machine without opening the IDE, `lazbuild` does the same in two commands, from the PascalSCADA folder:

```
lazbuild --add-package pascalscada_full.lpk
lazbuild --build-ide=
```

The first registers the meta-package (and, through dependencies, the others) with the IDE; the second rebuilds Lazarus with them. BGRABitmap must be registered beforehand (installed through the OPM, or `lazbuild --add-package path/to/bgrabitmappack.lpk`).

##### Linux: permissions and libraries

* **Serial ports**: the user running the application must belong to the group owning the ports (`dialout` on Debian/Ubuntu, `uucp` on other distributions): `sudo usermod -aG dialout $USER`, then log in again. See [Communication ports](/communication-ports/).
* **RFID reader** (`pascalscada_externallibs_hmi`): `libhidapi-dev` and the udev rule described in [Security system](/security-system/#rfid).
* **Databases** (`THMIDBConnection`): the client library of the chosen database (`libpq`, `libmariadb`/`libmysqlclient`, `libsqlite3`, …) must be installed — SQLDB loads it at run time.

##### Updating

If you installed from GitHub: `git pull` in the PascalSCADA folder, then *Package → Open Package File* on `pascalscada_full.lpk` → *Use >> → Install* (or `lazbuild --build-ide=`), and the IDE rebuilds with the new code. Through the OPM, use the OPM's own *Update* button.

##### What about Delphi?

The code keeps its Delphi directives and there are `.dpr` examples in the repository, but **there are no ready-made Delphi packages (`.dpk`)**, and the current version is developed and tested on Lazarus/Free Pascal only. To use it on Delphi, expect to assemble the package yourself — and send the fixes.

##### Common problems

| Symptom | Likely cause |
|---|---|
| "Package BGRABitmapPack not found" while installing | BGRABitmap was not installed first. Install it through the OPM and try again. |
| The IDE does not start after installing `pascalscada_externallibs_hmi` | `libhidapi` is missing on the system (or you are on Windows, where this package does not work). Start Lazarus with `--skip-last-project`, uninstall the package and rebuild. |
| `Cannot find unit tcp_udpport` (or another unit) when building a project | The project lacks the package in its dependencies: *Project → Project Inspector → Add → New Requirement* and add `pascalscada_hmi` (or just `pascalscada`, for applications without the LCL). |
| Palettes show up but the components have no icon | Resources not regenerated after an update — rebuild the IDE (*Tools → Build Lazarus with Profile*). |
| Compilation error on an old Lazarus/FPC | Update: FPC 3.0 and Lazarus 2.0 are the minimum; 3.2.2 and the current stable Lazarus are recommended. |
