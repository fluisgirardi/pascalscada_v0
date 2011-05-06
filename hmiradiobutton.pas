{:
  ATUALMENTE ESTE CONTROLE não funciona no Lazarus.
  @abstract(Implementa um controle um controle em forma de RadioButton.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}

unit HMIRadioButton;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  HMICheckBox, Classes, Controls, {$IFDEF FPC}LCLIntf, LCLType,
  WSLCLClasses,{$ELSE}Windows,{$ENDIF} StdCtrls, HMITypes;

type
  {:
  Define um controle booleando em forma de RadioButton.
  @seealso(THMICheckBox)
  }
  THMIRadioButton = class(THMICheckBox)
  protected
  {$IFDEF FPC}
    //: @exclude
    procedure Click; override;
  {$ELSE}
    //: @exclude
    procedure CreateParams(var Params: TCreateParams); override;
  {$ENDIF}
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    //: @exclude
    property OtherValuesIS default IsUnchecked;
  end;

implementation

constructor THMIRadioButton.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF FPC}
  fCompStyle := csRadioButton;
  {$ENDIF}
  OtherValuesIS := isUnchecked;
end;

destructor THMIRadioButton.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF FPC}
procedure THMIRadioButton.Click;
begin
  if State<>cbChecked then
     TCheckBox(self).State := cbChecked;
  Inherited DoOnChange;
end;

{$ELSE}
procedure THMIRadioButton.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TLeftRight] of Cardinal =
    ((BS_LEFTTEXT, 0), (0, BS_LEFTTEXT));
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  with Params do begin
    Style := Style xor BS_3STATE or BS_RADIOBUTTON or Alignments[UseRightToLeftAlignment, Alignment];
    WindowClass.style := WindowClass.style and (CS_HREDRAW or CS_VREDRAW);        
  end;
end;
{$ENDIF}

procedure THMIRadioButton.Loaded;
begin
  inherited Loaded;
end;

{$IFDEF FPC}
initialization
  //RegisterWSComponent(THMIRadioButton, FindWSComponentClass(TRadioButton));
{$ENDIF}

end.
