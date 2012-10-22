{$i ../common/language.inc}
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
unit lazlclversion;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

{$IFDEF FPC}
uses
  LCLVersion;
{$ENDIF}

type

  THasCustomHint = (hchNo,hchYes);

  {$if declared(lcl_version)}
    {$IF declared(lcl_fullversion)}
      const pslcl_fullversion = lcl_fullversion;
    {$ELSE}
      {$WARNING lcl_fullversion CALCULADO!}
      const pslcl_fullversion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100;
    {$ifend}
  {$else}
    {$WARNING lcl_fullversion ZERADO!}
    const pslcl_fullversion = 0;
  {$ifend}

  {$if declared(pslcl_fullversion)}
    {$if pslcl_fullversion>=093000}
      const has_customhints = hchYes;
    {$ifend}
  {$ifend}

implementation

end.

