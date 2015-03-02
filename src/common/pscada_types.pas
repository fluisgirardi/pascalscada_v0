{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit pSCADA_Types;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$MACRO ON}
{$ELSE}
  {$ERROR This unit should be used with FPC + Lazarus, never with Delphi!}
{$ENDIF}

interface

uses
  Classes, SysUtils
  {$IFDEF FPC}
  , LCLVersion;
  {$ENDIF};

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
  THasCustomHint = (hchNo,hchYes);

  const
    pSCADAFullVersion = 000070300;
    pSCADAVersion = '0.7.3';
    {$IFNDEF FPC}
    pSCADALineEnding = #13#10;
    {$ENDIF}


    {$IFDEF PORTUGUES}
    {:
      Calcula a constante lcl_fullversion em versões do lazarus
      que não tem ela disponível.
      Verifica também se existe o recurso de hints customizaveis (na IDE).
      @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    }
    {$ELSE}
    {:
      Calculates the lcl_fullversion constant in old Lazarus versions.
      Check too if the custom hints (on IDE) are available.
      @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    }
    {$ENDIF}
    {$IF declared(lcl_fullversion)}
      const pSCADA_LCL_FullVersion = lcl_fullversion;
    {$ELSE}
      {$IF declared(lcl_major) and declared(lcl_minor) and declared(lcl_release)}
        {$WARNING pslcl_fullversion calculated!}
        const pSCADA_LCL_FullVersion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100;
      {$ELSE}
        {$ERROR Your Lazarus is too old or it has something wrong!}
      {$ENDIF}
    {$ifend}

    {$if declared(pSCADA_LCL_FullVersion)}
      {$if pSCADA_LCL_FullVersion>=093000}
        const has_customhints = hchYes;
      {$ifend}
    {$ifend}

implementation

end.

