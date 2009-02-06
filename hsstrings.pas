//: @exclude
unit hsstrings;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

resourcestring
  //////////////////////////////////////////////////////////////////////////////
  // PALHETAS DE COMPONENTES
  //////////////////////////////////////////////////////////////////////////////
  
  strPortsPallete       = 'FLG SCADA Ports';
  strProtocolsPallete   = 'FLG SCADA Protocols';
  strTagsPallete        = 'FLG SCADA Tags';
  strUtilsPallete       = 'FLG SCADA Utils';
  strControlsPallete    = 'HCl - Acid Controls';
  
  //////////////////////////////////////////////////////////////////////////////
  // Mensagens de exceptions.
  //////////////////////////////////////////////////////////////////////////////
  
  strUpdateThreadWinit  = 'A thread não respondeu ao commando INIT';
  strCompIsntADriver    = 'O componente não é um driver de protocolo válido';
  strThreadSuspended    = 'A thread está suspensa?';

implementation

end.
 
