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
  {$MACRO ON}
{$ELSE}
  {$ERROR This unit should be used with FPC + Lazarus, never with Delphi!}
{$ENDIF}

interface

{$IFDEF FPC}
uses
  LCLVersion;
{$ENDIF}

type

  THasCustomHint = (hchNo,hchYes);

  {$IF declared(lcl_fullversion)}
    const pslcl_fullversion = lcl_fullversion;
  {$ELSE}
    {$IF declared(lcl_major) and declared(lcl_minor) and declared(lcl_release)}
      {$WARNING pslcl_fullversion calculated!}
      const pslcl_fullversion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100;
    {$ELSE}
      {$ERROR Your Lazarus is too old or it has something wrong!}  
    {$ENDIF}
  {$ifend}

  {$if declared(pslcl_fullversion)}
    {$if pslcl_fullversion>=093000}
      const has_customhints = hchYes;
    {$ifend}
  {$ifend}

implementation

end.

