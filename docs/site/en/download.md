##### Where to get PascalSCADA

Development happens on **GitHub** these days — that's where the current version, the new drivers and the latest fixes are (see the recent announcement on the blog). SourceForge and SVN, mentioned in older versions of this page, are no longer used.

> **Recommended: install through Lazarus' Online Package Manager (OPM).** *Package → Online Package Manager*, search for *PascalSCADA*, tick it and click *Install* — the OPM downloads PascalSCADA **0.8.0** and its dependencies (BGRABitmap included) and rebuilds the IDE by itself ([see how, with a video](/2017/04/04/pascalscada-on-opm-repository/)). The manual installation from GitHub described below is for those who want the development code.

**GitHub (development code)**

```
git clone https://github.com/fluisgirardi/pascalscada_v0.git
```

Or, without `git`, download the [master branch ZIP](https://github.com/fluisgirardi/pascalscada_v0/archive/refs/heads/master.zip) and unpack it in a folder of your choice.

**Note**: installing from GitHub, some dependencies must be downloaded and installed **before** PascalSCADA — most importantly **BGRABitmap**, used by the HCl's drawn controls (through Lazarus' *Online Package Manager*). Without it installed first, the PascalSCADA package will not compile. See the full dependency list in [How to install PascalSCADA](/how-install-pascalscada/#packages).

**Lazarus' Online Package Manager (recommended)**

Through Lazarus' *Package → Online Package Manager*, search for *PascalSCADA* and install it straight from the IDE, with nothing to download by hand. The package published there is version **0.8.0**, with the new drivers and the recent fixes.

Once you have it (either way), follow the walkthrough in [How to install PascalSCADA](/how-install-pascalscada/).
