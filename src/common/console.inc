{
****************************** History  *******************************
***********************************************************************
07/2013 - Define CONSOLEPASCALSCADA if fpc >= 2.7.1
@author(Juanjo Montero <juanjo.montero@gmail.com>)
***********************************************************************
}


// Compile directive that allows to use PascalSCADA in Console Mode
// This only is usefull for Linux that can runs without X enviroment
{$IFDEF UNIX}
  {$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 7) and (FPC_PATCH >= 1))}
    {$DEFINE CONSOLEPASCALSCADA}
  {$IFEND}
{$ENDIF}

