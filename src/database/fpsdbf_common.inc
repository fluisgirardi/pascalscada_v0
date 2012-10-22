// define this if you need more SPEEEEEDDDD!!!
// useful if you index dbf files over a network

{.$define USE_CACHE}

// define the following if you want support for 65535 length character
// fields for all dbase files (and not only foxpro); if you define this, 
// you will not be able to read MS Excel generated .dbf files!

{.$define USE_LONG_CHAR_FIELDS}

// modifies unix unit dbf_wtil to use hungarian encodings (hack)

{.$define HUNGARIAN}

// enables assembler routines, 486+ only

{$define USE_ASSEMBLER_486_UP}

// test compatibility

{.$define TDBF_UPDATE_FIRSTLAST_NODE}

// use this directive to suppress math exceptions,
// instead NAN is returned.
// Using this directive is slightly less efficient
//
// used in Dbf_PrsDef
//
// NAN support needs to be rewritten and is currently absent

{.$define NAN}

//------------------------------------------------------
//--- Define all SUPPORT_xxx; undef if not supported ---
//------------------------------------------------------


//------------------------------------------------------
//--- Delphi versions                                ---
//------------------------------------------------------

{$I ../common/delphiver.inc}

{$ifndef DELPHI2009_UP}          //	Delphi 1.0
  #ERROR tDbf needs Delphi 2009 minimum.
{$endif}

//-------------------------------------------------------
//--- Conclude supported features from delphi version ---
//-------------------------------------------------------

{$ifdef DELPHI2009_UP}

  {$define SUPPORT_VARIANTS}
  {$define WINDOWS}
  {$define ENDIAN_LITTLE}

//{$ifdef DELPHI_4}

  {$define SUPPORT_DEFCHANGED}
  {$define SUPPORT_DEFAULT_PARAMS}
  {$define SUPPORT_OVERLOAD}
  {$define SUPPORT_NEW_TRANSLATE}
  {$define SUPPORT_INT64}
  {$define SUPPORT_REINTRODUCE}
  {$define SUPPORT_FIELDDEFS_UPDATED}
  {$define SUPPORT_FIELDDEF_ATTRIBUTES}
  {$define SUPPORT_FIELDDEF_TPERSISTENT}
  {$define SUPPORT_FIELDDEF_INDEX}
  {$define SUPPORT_FIELDTYPES_V4}
  {$define SUPPORT_UINT32_CARDINAL}
  {$define SUPPORT_MATH_UNIT}

//{$ifdef DELPHI_5}

  {$define SUPPORT_BACKWARD_FIELDDATA}
  {$define SUPPORT_INITDEFSFROMFIELDS}
  {$define SUPPORT_REFRESHEVENTS}
  {$define SUPPORT_DEF_DELETE}
  {$define SUPPORT_FREEANDNIL}

//{$ifdef DELPHI_6}

  {$define SUPPORT_PATHDELIM}
  {$define SUPPORT_SEPARATE_VARIANTS_UNIT}

//{$endif}
//{$endif}
//{$endif}
{$endif}

//------------------------------------------------------
//--- Conclude supported features in FreePascal      ---
//------------------------------------------------------

{$ifdef FPC_VERSION}

  {$mode delphi}
  {$h+}

{$ifndef CPUI386}
  {$undef USE_ASSEMBLER_486_UP}
{$endif}

{$ifdef USE_ASSEMBLER_486_UP}
  {$asmmode intel}
{$endif}

  {$define SUPPORT_INT64}
  {$define SUPPORT_DEFAULT_PARAMS}
  {$define SUPPORT_OVERLOAD}
  {$define SUPPORT_NEW_TRANSLATE}
  {$define SUPPORT_FIELDDEF_TPERSISTENT}
  {$define SUPPORT_FIELDTYPES_V4}
  {$define SUPPORT_UINT32_CARDINAL}
  {$define SUPPORT_REINTRODUCE}
  {$define SUPPORT_MATH_UNIT}
  {$define SUPPORT_VARIANTS}
  {$define SUPPORT_SEPARATE_VARIANTS_UNIT}
  {$define SUPPORT_REFRESHEVENTS}

  // FPC 2.0.x improvements
  {$ifdef VER2}
    {$ifndef VER2_0_0}
      {$define SUPPORT_BACKWARD_FIELDDATA}
    {$endif}
  {$endif}

  // FPC 1.0.x exceptions: no 0/0 support
  {$ifdef VER1_0}
    {$undef NAN}
    {$undef SUPPORT_DEFAULT_PARAMS}
    {$undef SUPPORT_NEW_TRANSLATE}

    {$message error TDbf needs fpc 1.9 minimum.}

  {$endif}

{$endif}

//----------------------------------------------------------
//--- Conclude supported features in non-Windows platforms ---
//----------------------------------------------------------

{$ifndef WINDOWS}

    {$define SUPPORT_PATHDELIM}
    {$define SUPPORT_INCLUDETRAILPATHDELIM}
    {$define SUPPORT_INCLUDETRAILBACKSLASH}

{$endif}

{$ifndef ENDIAN_LITTLE}
{$ifndef ENDIAN_BIG}
{$error Neither ENDIAN_LITTLE nor ENDIAN_BIG defined!}
{$endif}
{$endif}

