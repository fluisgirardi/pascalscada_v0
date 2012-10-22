{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Unit que melhora a classe de coleções.)
@author(Fabio Luis Girardi)
}
{$ELSE}
{:
@abstract(Unit that improves the collection classes.)
@author(Fabio Luis Girardi)
}
{$ENDIF}
unit collections;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  {$IFDEF PORTUGUES}
  {:
  Evento chamado quando é necessário saber o atual estado do componente.
  @seealso(TZones)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Procedure used to return the current state of the owner component.
  @seealso(TZones)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TNeedCompStateEvent = procedure(var CurState:TComponentState) of object;

implementation

end.

