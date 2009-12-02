{:
  @abstract(Unit de tradução do PascalSCADA.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit hsstrings;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

resourcestring
  //////////////////////////////////////////////////////////////////////////////
  // PALHETAS DE COMPONENTES
  //////////////////////////////////////////////////////////////////////////////
  
  strPortsPallete       = 'PascalSCADA Ports';
  strProtocolsPallete   = 'PascalSCADA Protocols';
  strTagsPallete        = 'PascalSCADA Tags';
  strUtilsPallete       = 'PascalSCADA Utils';
  strControlsPallete    = 'PascalSCADA HCl';
  
  //////////////////////////////////////////////////////////////////////////////
  // Mensagens de exceptions.
  //////////////////////////////////////////////////////////////////////////////
  
  strUpdateThreadWinit  = 'A thread não respondeu ao commando INIT';
  strCompIsntADriver    = 'O componente não é um driver de protocolo válido';
  strThreadSuspended    = 'A thread está suspensa?';

implementation

end.
 
