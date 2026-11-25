unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  PLCTagNumber, HMIRecipe, HMIRecipeButton, HMIRecipeActions, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIRecipeButton1: THMIRecipeButton;
    HMIRecipeButton2: THMIRecipeButton;
    HMIRecipeButton3: THMIRecipeButton;
    HMIRecipeButton4: THMIRecipeButton;
    HMIRecipeButton5: THMIRecipeButton;
    HMIRecipeGrid1: THMIRecipeGrid;
    MainMenu1: TMainMenu;
    RecipeAddAction: TPascalSCADAAddRecipeAction;
    RecipeAddItem: TMenuItem;
    RecipeDeleteAction: TPascalSCADADeleteRecipeAction;
    RecipeDeleteItem: TMenuItem;
    RecipeDownloadAction: TPascalSCADADownloadRecipeAction;
    RecipeDownloadItem: TMenuItem;
    RecipeSaveAction: TPascalSCADASaveRecipeLibraryAction;
    RecipeSaveItem: TMenuItem;
    RecipesMenu: TMenuItem;
    RecipesSep1: TMenuItem;
    RecipeUploadAction: TPascalSCADAUploadRecipeAction;
    RecipeUploadItem: TMenuItem;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    PLCTagNumber3: TPLCTagNumber;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

