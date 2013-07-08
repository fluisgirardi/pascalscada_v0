{$i ../common/language.inc}
{:
  @abstract(Implementation of TCommonTagAssistant.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)

  This is the base class for TCommonTagAssistant descendents

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo@aplicacionesjjmb.com>)
  ***********************************************************************
}
unit commontagassistant;
{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

{
  ***********************************************************************
  07/2013 - Interfaces unit are required in order to Initialize WidgetSet
  for Assistants in TDaemon Projects
  @author(Juanjo Montero <juanjo@aplicacionesjjmb.com>)
  ***********************************************************************
}

uses
  Classes, SysUtils, ProtocolTypes{$IFDEF FPC}, Interfaces{$ENDIF};


type
  TCommonTagAssistant = class(TComponent)
    private

    public
    {$IFDEF PORTUGUES}
    //: Chama o editor de tags do driver.
    {$ELSE}
    //: Opens the Tag Builder of the protocol driver (if exists)
    {$ENDIF}
    procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
      CreateProc:TCreateTagProc); virtual; abstract;


end;



implementation



end.

