unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PLCTagNumber,
  HMIRecipe, HMIRecipeButton, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIRecipeButton1: THMIRecipeButton;
    HMIRecipeButton2: THMIRecipeButton;
    HMIRecipeButton3: THMIRecipeButton;
    HMIRecipeButton4: THMIRecipeButton;
    HMIRecipeGrid1: THMIRecipeGrid;
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

