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
    {$IF not declared(lcl_fullversion)}
      {$WARNING lcl_fullversion CALCULADO!}
      const lcl_fullversion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100;
    {$ifend}
  {$else}
    {$WARNING lcl_fullversion ZERADO!}
    const lcl_fullversion = 0;
  {$ifend}

  {$if declared(lcl_fullversion)}
    {$if lcl_fullversion>=092900}
      const has_customhints = hchYes;
    {$ifend}
  {$ifend}

implementation

end.

