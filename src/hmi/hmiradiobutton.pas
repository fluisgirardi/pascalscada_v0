{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  ATUALMENTE ESTE CONTROLE não funciona no Lazarus.
  @abstract(Implementa um controle um controle em forma de RadioButton.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  Currently this control doesn't works on Lazarus IDE.
  @abstract(Unit that implements a RadioButton control.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIRadioButton;

interface

uses
  HMICheckBox, Classes, Controls, {$IFDEF FPC}LCLIntf, LCLType,
  WSLCLClasses,{$ELSE}Windows,{$ENDIF} StdCtrls, HMITypes;

type
  {$IFDEF PORTUGUES}
  {:
  Define um controle booleando em forma de RadioButton.
  @bold(Atualmente este controle não funciona no Lazarus.)
  @seealso(THMICheckBox)
  }
  {$ELSE}
  {:
  Implements a RadioButton Control.
  @bold(Currently this control doesn't works on Lazarus IDE.)
  @seealso(THMICheckBox)
  }
  {$ENDIF}
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

uses ControlSecurityManager;

constructor THMIRadioButton.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF FPC}
  fCompStyle := csRadioButton;
  {$ENDIF}
  OtherValuesIS := isUnchecked;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIRadioButton.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
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
