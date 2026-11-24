##### Introduction

A **recipe** is a named set of values that the operator sends to the machine as one unit: the parameters of a product, the setpoints of an oven, the dosage of a mixer. The `pascalscada_hmi` package ships two components that reproduce the recipe manager of a standalone HMI — the library lives **on the PC**, not in the PLC:

1. **`THMIRecipeGrid`** (unit `HMIRecipe`) — the recipe table: one row per recipe, one column per parameter, editable in place, saved to a plain text library file.
2. **`THMIRecipeButton`** (unit `HMIRecipeButton`) — one button per role, linked to the grid: add recipe, delete recipe, save the library, **download** the selected recipe to the PLC tags and **upload** the tag values back into a recipe.

Both speak plain text: the library file is human readable, versionable and editable with any editor.

##### The recipe table

Drop a `THMIRecipeGrid` on the form. Each entry of the **`Fields`** collection becomes one parameter column; the name column stays on the left and, by default, a read-only **sequence column** (`#`, 1, 2, 3…) comes in front of it.

| Property | Default | Description |
|---|---|---|
| `Fields` | empty | The parameter columns. One `TRecipeField` per column: `Name` (column header and library key), `PLCTag` (the tag that receives the value on download and provides it on upload), `Decimals` (display rounding, default 2), `CheckRange` + `MinValue`/`MaxValue` (clamps the typed value and reports through `OnError`), `DefaultValue` (value of a new recipe). |
| `RecipeFile` | `recipes.ini` | The library file. A relative path is resolved against the directory of the executable. |
| `AutoLoad` | `True` | Loads the library when the application starts. Turn it off to control the moment yourself (`LoadLibrary`). |
| `ShowRowNumbers` | `True` | The read-only sequence column in front of the name column; the numbers follow the library order and are rebuilt after a delete. |
| `RowNumberTitle` | `#` | Header of the sequence column. |
| `RowNumberWidth` | 40 | Width of the sequence column. |
| `NameColumnTitle` | `Recipe` | Header of the name column. |
| `OnError` | – | Called for every rejected input: empty or duplicated name, invalid number, out of range. |
| `OnRecipeChanged` | – | Called after a name, a value, an add or a delete. |

Editing is in place: the name column renames the recipe (empty and duplicate names are refused through `OnError`), each parameter column takes a number (point or comma). A value out of the `CheckRange` window is clamped to the nearest limit and reported.

> **`Columns` stays empty.** The columns come from `Fields`; the LCL column mode locks the column count and breaks the table. A `Columns` collection filled by mistake is dropped by the grid instead of crashing — but keep it empty in the Object Inspector.

##### The library file

```
# PascalSCADA recipe library
Version=1

[Recipe 1]
Field 0=1.000000
Field 1=2.000000
$0=1.000000
```

One `[section]` per recipe, one `key=value` per parameter — the key is the field `Name` (`$n` keeps the value of a field that was renamed or removed, so nothing is lost when the field list evolves). `LoadLibrary` and `SaveLibrary` read and write it; a missing file is not an error, it starts an empty library.

##### The command buttons

Drop a `THMIRecipeButton` for each role and point `RecipeGrid` to the table. The `Command` property selects the role — and the caption follows it while you have not typed one of your own:

| Command | What the click does |
|---|---|
| `rcAddRecipe` | Adds a recipe (`RecipeName` fixed, or the next free name when empty). |
| `rcDeleteRecipe` | Deletes the recipe (`RecipeName`, or the selected row when empty). |
| `rcSaveLibrary` | Writes the library file. |
| `rcDownload` | Writes the values of the **selected row** into the `PLCTag` of each field. |
| `rcUpload` | Reads every `PLCTag` and stores the values in the **selected row**. |

| Property | Default | Description |
|---|---|---|
| `RecipeGrid` | – | The table this button commands. |
| `Command` | `rcAddRecipe` | The role, from the table above. |
| `RecipeName` | empty | Fixed recipe name for add/delete; empty uses the selection. |
| `AutoSave` | `True` | Saves the library after add, delete and upload. |
| `GlyphUpFile` / `GlyphDownFile` | empty | Image of the released / pressed state (PNG with transparency, BMP, JPG). A relative path is resolved against the directory of the executable; leave `GlyphDownFile` empty to reuse the released image. |
| `GlyphUp` / `GlyphDown` | – | The same images as bitmaps, pasted in the Object Inspector. |
| `OnError` / `OnAfterCommand` | – | Called when the command cannot run (no grid attached, no row selected) and after it finished. |

Clicking a button without a grid attached reports through `OnError` instead of raising; in the designer the buttons do nothing.

##### Step by step

The [recipe library example](https://github.com/fluisgirardi/pascalscada_v0/tree/master/examples/laz_recipe_library) shows the whole assembly:

1. Put the `TPLCTagNumber` tags on the form — one per parameter the machine takes.
2. Drop the `THMIRecipeGrid` and add one `Fields` item per tag, binding `PLCTag` and setting `Name`, `Decimals` and the range.
3. Drop four or five `THMIRecipeButton`s, point each one to the grid and choose the `Command` (`Add recipe`, `Delete recipe`, `Save library`, `Download`).
4. Show the tag values with `THMILabel`s so the operator sees what arrived.
5. Run: edit the table, save, select a row and download.

##### FAQ

* **The grid has one column and reading `Cells[1, x]` raises "Index out of range".** Old builds only: the grid now keeps at least two columns and two rows in every state. Update the package.
* **"Invalid value for property" while reading `ColWidths` on startup.** Old builds only: it happened with exactly four fields, when the designer omitted the default `ColCount` from the .lfm. Update the package.
* **Where are the recipes stored?** In `RecipeFile`, next to the executable by default. It is plain text — inspect, edit or put it under version control as you like.
* **Can several screens share one library?** Yes, but one component must own the file at a time; give each screen its own `RecipeFile` or route every save through one grid.
