{$IFDEF PORTUGUES}
{:
  @abstract(Define tipos usuais em controles de telas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Define types commonly used on controls.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMITypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  PLCTag;

type
  {$IFDEF PORTUGUES}
  {:
  Define quando um valor deve ser escrito no Tag por um controle.
  @value(scLostFocus Quando o controle perde o foco.)
  @value(scPressEnter Quando é pressionado a tecla enter.)
  @value(scPressESC Quando é pressionado a tecla ESC.)
  @value(scAnyChange O valor é escrito no tag após qualquer alteração.)
  }
  {$ELSE}
  {:
  Defines when a value must be written on tag by a control.
  @value(scLostFocus When the control lost the focus.)
  @value(scPressEnter When Enter is pressed.)
  @value(scPressESC When Esc is pressed.)
  @value(scAnyChange After any changes.)
  }
  {$ENDIF}
  TSendKind = (scLostFocus, scPressEnter, scPressESC, scAnyChange);

  {$IFDEF PORTUGUES}
  {:
  Define o conjunto de ações de escrita de valor de um controle no seu tag.
  @seealso(TSendKind)
  }
  {$ELSE}
  {:
  Defines the set of actions that will do a control write its value on tag.
  }
  {$ENDIF}
  TSendChange = set of TSendKind;

  {$IFDEF PORTUGUES}
  {:
  Define como um controle booleano (CheckBox, RadioButton) deve interpretar
  valores que não são equivalentes ao valor falso e ao valor verdadeiro.

  @value(isChecked o componente irá aparecer marcado caso o valor do tag seja
  diferente de ValueFalse e ValueTrue.)

  @value(isUnchecked o componente irá aparecer desmarcado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)

  @value(isNone o componente não irá mudar o seu estado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)

  @value(IsGrayed o componente irá aparecer acinzentado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)
  }
  {$ELSE}
  {:
  Defines how a boolean control(CheckBox, RadioButton) must interpret values
  that are different of the true value and false value.

  @value(isChecked the control will be checked if the tag value is different of
  ValueFalse and ValueTrue properties.)

  @value(isUnchecked the control will be unchecked if the tag value is different
  of ValueFalse and ValueTrue properties.)

  @value(isNone the control keeps unchanged if the value of the tag is different
  of ValueFalse and ValueTrue properties.)

  @value(IsGrayed the control will be grayed if the tag value is different of
  ValueFalse and ValueTrue properties.)
  }
  {$ENDIF}
  TOtherValues = (isChecked, isUnchecked, isNone, IsGrayed);

  {$IFDEF PORTUGUES}
  {:
  Define os possíveis tipos de botões.

  @value(btJog o botão irá ficar pressionado enquanto ele for mantido pressionado.)

  @value(btOnOff o botão fica pressionado com um clique e com outro ele é liberado.)

  @value(btMomentary O botão fica pressionado por alguns instantes e logo
  em seguinda liberado, mesmo que ele seja mantido pressionado.)

  @value(btToogle O botão irá inverter o valor do tag e manter a aparência
  solta (não pressionado).)
  }
  {$ELSE}
  {:
  Define the button kind.

  @value(btJog the button will be pressed while it is held down.)

  @value(btOnOff the button will be pressed with one click and released with other click.)

  @value(btMomentary The button is pressed for a few moments and then following
         it's released, even it is  keeps pressed.)

  @value(btToogle The button will invert the value of the tag and keep the
         released appearence.)
  }
  {$ENDIF}
  TButtonType = (btJog, btOnOff, btMomentary, btToogle);


  {$IFDEF PORTUGUES}
  //: @name define a interface comum a todos os objetos de tela.
  {$ELSE}
  //: @name defines a interface to interact with all HMI window controls.
  {$ENDIF}
  IHMIInterface = interface
    ['{62FF1979-FA70-4965-B24F-347A69AC2EB1}']

    {$IFDEF PORTUGUES}
    {:
    Procedimento que atualiza o componente de acordo com as regras atuais de
    segurança. É chamado quando um usuário faz login ou alguma regra de segurança
    é alterada.
    }
    {$ELSE}
    {:
    Procedure that updates the control with the new security rules. Is called every
    time that a user does a log-in or log-off or if a security rule changes.
    }
    {$ENDIF}
    procedure RefreshHMISecurity;

    {$IFDEF PORTUGUES}
    //: Atualiza a referencia de tag do controle.
    {$ELSE}
    //: Updates the link of the control with a tag
    {$ENDIF}
    procedure SetHMITag(t:TPLCTag);

    {$IFDEF PORTUGUES}
    //: Retorna o tag em uso pelo controle.
    {$ELSE}
    //: Returns the tag linked with the control.
    {$ENDIF}
    function  GetHMITag:TPLCTag;

    {$IFDEF PORTUGUES}
    //: Informa se o controle está habilitado. @seealso(Enabled)
    {$ELSE}
    //: Tells if the control is enabled. @seealso(Enabled)
    {$ENDIF}
    function  GetHMIEnabled:Boolean;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o controle. @seealso(Enabled)
    {$ELSE}
    //: Enables/disables the control. @seealso(Enabled)
    {$ENDIF}
    procedure SetHMIEnabled(v:Boolean);

    {$IFDEF PORTUGUES}
    //: Propriedade criada para informar/setar o estado do controle através da interface.
    {$ELSE}
    //: Property to get/set the state of the control through the interface.
    {$ENDIF}
    property  Enabled:Boolean read GetHMIEnabled write SetHMIEnabled;

    {$IFDEF PORTUGUES}
    //: Propriedade criada para informar/setar o tag do controle através da interface.
    {$ELSE}
    //: Property to get/set the tag of the control through the interface.
    {$ENDIF}
    property  Tag:TPLCTag read GetHMITag write SetHMITag;
  end;

implementation

end.
