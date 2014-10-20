unit pscommontypes;

{$mode objfpc}{$H+}

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

