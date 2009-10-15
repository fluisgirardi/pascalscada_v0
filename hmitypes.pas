//: Define tipos usuais em controles de telas.
unit HMITypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  PLCTag;

type
  {:
  Define quando um valor deve ser escrito no Tag por um controle.
  @value(scLostFocus Quando o controle perde o foco.)
  @value(scPressEnter Quando é precionado a tecla enter.)
  @value(scPressESC Quando é precionado a tecla ESC.)
  @value(scAnyChange O valor é escrito no tag após qualquer alteração.)
  }
  TSendKind = (scLostFocus, scPressEnter, scPressESC, scAnyChange);
  {:
  Define o conjunto de ações de escrita de valor de um controle no seu tag.
  @seealso(TSendKind)
  }
  TSendChange = set of TSendKind;
  {:
  Define como um controle booleano (CheckBox, RadioButton) deve interpretar
  valores que são verdadeiros e nem falsos.
  
  @value(isChecked o componente irá aparecer marcado caso o valor do tag seja
  diferente de ValueFalse e ValueTrue.)

  @value(isUnchecked o componente irá aparecer desmarcado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)

  @value(isNone o componente não irá mudar o seu estado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)

  @value(IsGrayed o componente irá aparecer acinzentado caso o valor do tag
  seja diferente de ValueFalse e ValueTrue.)
  }
  TOtherValues = (isChecked, isUnchecked, isNone, IsGrayed);

  {:
  Define os possíveis tipos de botões.

  @value(btJog o botão irá ficar precionado enquanto ele estiver precionado.)

  @value(btOnOff o botão fica precionado com um clique e com outro ele é
  liberado.)

  @value(btMomentary O botão fica precionado por alguns instantes e logo
  em seguinda liberado, mesmo que ele seja mantido precionado.)
  
  @value(btToogle O botão irá inverter o valor do tag e manter a aparencia
  solta (não precionado).)
  }
  TButtonType = (btJog, btOnOff, btMomentary, btToogle);

  //: @name define a interface comum a todos os objetos de tela.
  IHMIInterface = interface
    ['{62FF1979-FA70-4965-B24F-347A69AC2EB1}']
    //: Procedimento que o TPLCTag irá chamar para informar alterações em seu valor.
    procedure HMINotifyChangeCallback(Sender:TObject);
    {:
    Procedimento que atualiza o componente de acordo com as regras atuais de
    segurança. É chamado quando um usuário faz login ou alguma regra de segurança
    é alterada.
    }
    procedure RefreshHMISecurity;
    //: Força a remoção da referência ao TPLCTag. Chamado quando o Tag está sendo destruido.
    procedure RemoveHMITag(Sender:TObject);
    //: Seta o tag do controle, criando uma referência. @seealso(Tag)
    procedure SetHMITag(t:TPLCTag);
    //: Remove o tag do controle, eliminando a referência. @seealso(Tag)
    function  GetHMITag:TPLCTag;
    //: Informa se o controle está habilitado. @seealso(Enabled)
    function  GetHMIEnabled:Boolean;
    //: Habilita/desabilita o controle. @seealso(Enabled)
    procedure SetHMIEnabled(v:Boolean);
    //: Propriedade criada para informar/setar o estado do controle através da interface.
    property  Enabled:Boolean read GetHMIEnabled write SetHMIEnabled;
    //: Propriedade criada para informar/setar o tag do controle através da interface.
    property  Tag:TPLCTag read GetHMITag write SetHMITag;
  end;

  IHMITagInterface = interface
    ['{4301B240-79D9-41F9-A814-68CFEFD032B8}']
    //: Chama o evento quando uma letura tem exito.
    procedure NotifyReadOk;
    //: Chama o evento quando uma leitura falha.
    procedure NotifyReadFault;
    //: Chama o evento quando uma escrita tem sucesso.
    procedure NotifyWriteOk;
    //: Chama o evento quando uma escrita do tag falha.
    procedure NotifyWriteFault;
    //: Chama o evento quando o valor do tag muda.
    procedure NotifyChange;
  end;

implementation

end.
