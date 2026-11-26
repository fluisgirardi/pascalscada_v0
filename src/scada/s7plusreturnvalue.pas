{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Decodificação estruturada do ReturnValue de 64 bits do S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Toda resposta S7CommPlus começa por um ReturnValue (VLQ de 64 bits). Em vez de olhar
  só o bit de sinal (falhou/ok), este módulo decompõe os campos conforme o layout do
  OMS+ (portado de gijzelaerr/s7commplus protocol.ReturnValue, #154) e produz um texto
  legível para os logs: "SERVICE_EXECUTED_WITH_PARTIAL_ERROR (19)", "OBJECT_NOT_FOUND
  (-12), generic INVALID_ARGUMENT_VALUE (43)", etc.

  Layout de bits (raw = valor de 64 bits):
    bit 63     : failed (falhou)
    bit 62     : has_extension (traz ResponseExtension)
    bits 61..48: interno do firmware (ignorado)
    bit 47     : from_responder
    bits 46..40: generic  = (raw shr 40) and $7F
    bits 39..32: source   = (raw shr 32) and $FF
    bits 31..0 : code - se source=0, os 16 bits baixos como signed16; senão os 32 bits
                 baixos como signed32.
}
{$ELSE}
{:
  @abstract(Structured decode of the S7CommPlus 64-bit ReturnValue.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Every S7CommPlus reply begins with a 64-bit VLQ ReturnValue. Instead of only testing
  the sign bit (failed/ok), this splits it into the OMS+ fields (ported from
  gijzelaerr/s7commplus protocol.ReturnValue, #154) and renders a readable string for
  logs. Bit layout: see the Portuguese doc above.
}
{$ENDIF}
unit S7PlusReturnValue;

{$mode Delphi}{$H+}

interface

uses
  SysUtils;

type
  //: The S7CommPlus ReturnValue broken into its OMS+ fields.
  TS7PlusReturnValue = record
    Raw:QWord;
    Failed:Boolean;
    HasExtension:Boolean;
    FromResponder:Boolean;
    Generic:Byte;    //: generic error category (bits 46..40)
    Source:Byte;     //: subsystem/source (bits 39..32)
    Code:LongInt;    //: signed service result (source=0) or subsystem code (source<>0)
  end;

//: True when the ReturnValue's failure bit (bit 63) is set.
function S7PlusReturnValueFailed(Raw:QWord):Boolean;
//: Splits a 64-bit ReturnValue into its fields.
function S7PlusDecodeReturnValue(Raw:QWord):TS7PlusReturnValue;
//: One-line human-readable rendering for logs (names the few codes we actually see).
function S7PlusDescribeReturnValue(Raw:QWord):String;

implementation

function S7PlusReturnValueFailed(Raw:QWord):Boolean;
begin
  Result := (Raw shr 63)<>0;
end;

function S7PlusDecodeReturnValue(Raw:QWord):TS7PlusReturnValue;
var
  Low16:Word;
  Low32:Cardinal;
begin
  Result.Raw := Raw;
  Result.Failed := (Raw shr 63)<>0;
  Result.HasExtension := ((Raw shr 62) and 1)<>0;
  Result.FromResponder := ((Raw shr 47) and 1)<>0;
  Result.Generic := Byte((Raw shr 40) and $7F);
  Result.Source := Byte((Raw shr 32) and $FF);
  if Result.Source=0 then begin
    //source 0: the service result is in the low 16 bits (bits 31..16 are firmware-internal)
    Low16 := Word(Raw and $FFFF);
    if (Low16 and $8000)<>0 then
      Result.Code := LongInt(Low16)-$10000
    else
      Result.Code := LongInt(Low16);
  end else begin
    //source <> 0: the subsystem code is the full signed 32-bit low word
    Low32 := Cardinal(Raw and $FFFFFFFF);
    Result.Code := LongInt(Low32);
  end;
end;

//Only the handful of names that actually show up in our logs; everything else falls
//back to the number. (The reference's full ServiceResult/ErrorSource/GenericError
//tables are ~700 lines and diagnostics-only - deliberately not ported, #154.)
function ServiceResultName(Code:LongInt):String;
begin
  case Code of
    0:   Result := 'OK';
    7:   Result := 'SERVICE_NOT_SUPPORTED';
    17:  Result := 'SERVICE_EXECUTED_WITH_WARNING';
    19:  Result := 'SERVICE_EXECUTED_WITH_PARTIAL_ERROR';
    -1:  Result := 'ERROR';
    -4:  Result := 'INVALID_SERVICE';
    -12: Result := 'OBJECT_NOT_FOUND';
  else
    Result := '';
  end;
end;

function GenericName(Generic:Byte):String;
begin
  case Generic of
    0:  Result := '';
    43: Result := 'INVALID_ARGUMENT_VALUE';
  else
    Result := '';
  end;
end;

function S7PlusDescribeReturnValue(Raw:QWord):String;
var
  rv:TS7PlusReturnValue;
  nm, gnm:String;
begin
  if Raw=0 then begin
    Result := 'OK';
    exit;
  end;
  rv := S7PlusDecodeReturnValue(Raw);
  if rv.Source=0 then begin
    nm := ServiceResultName(rv.Code);
    if nm<>'' then
      Result := Format('%s (%d)',[nm, rv.Code])
    else
      Result := Format('service result %d',[rv.Code]);
  end else
    Result := Format('subsystem 0x%.2x code 0x%.8x',[rv.Source, Cardinal(rv.Code)]);

  gnm := GenericName(rv.Generic);
  if gnm<>'' then
    Result := Result+Format(', generic %s (%d)',[gnm, rv.Generic])
  else if rv.Generic<>0 then
    Result := Result+Format(', generic %d',[rv.Generic]);

  if rv.Failed then
    Result := Result+' [failed]';
  Result := Result+Format(' (0x%.16x)',[Raw]);
end;

end.
