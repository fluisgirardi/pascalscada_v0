# Recipe library example (THMIRecipeGrid + THMIRecipeButton)

Demonstrates the recipe controls shipped in the `pascalscada_hmi` package:

* `THMIRecipeGrid` — the recipe table with the library file on the PC
  (`recipes.ini`, next to the executable, loaded automatically by `AutoLoad`).
* `THMIRecipeButton` — one button per role, linked to the grid through
  `RecipeGrid`: add recipe, delete recipe, save library, download to the tags
  and upload from the tags.

The form has three `TPLCTagNumber` tags bound to the three fields of the grid
(`Fields` collection, one item per tag) and three `THMILabel` components that
show the current value of each tag, so a download writes the selected recipe
row into the tags and the labels mirror what arrived.

## Files

| File | What it is |
|---|---|
| `unit1.pas` / `unit1.lfm` | The form: the grid, four command buttons, the tags and the labels. |
| `project1.lpr` / `project1.lpi` | The Lazarus project. |
| `recipes.ini.sample` | A library file saved by the application itself — rename to `recipes.ini` next to the executable to start from it. |

## Notes

* The tags have no protocol driver on purpose: the example runs anywhere and
  the download/upload exercise the tag write/read paths without a PLC. Attach
  a driver and a communication port to the tags to talk to a real device.
* `RowNumberTitle` is set to the Portuguese/Chinese style header `序号`
  (sequence number) to show that the title of the read-only sequence column
  is configurable; set it back to `#` for the default look.
* The library file is resolved relative to the directory of the executable,
  so run the application from its own folder to keep the file where you
  expect it.
