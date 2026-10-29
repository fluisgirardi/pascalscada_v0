{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Certificados do CLP guardados numa string: arquivo PEM/DER <-> base64.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A propriedade @code(ServerCertificate) do TS7CommPlusDriver guarda o(s) certificado(s) em
  que o driver confia como UMA string base64 do DER (vários certificados, se forem vários,
  vão um depois do outro antes do base64). Assim ela vai no .lfm junto com o resto do
  formulário. Esta unit converte o que o usuário escolhe num arquivo (PEM de texto, com um ou
  mais blocos CERTIFICATE, ou DER binário) nessa string, e a string de volta em DER para o
  OpenSSL. Não usa OpenSSL nem tem dependência de rede: é testável sozinha.

  Chaves privadas que estejam no mesmo arquivo PEM são IGNORADAS e nunca vão para a string.
}
{$ELSE}
{:
  @abstract(The certificates of the PLC kept in a string: PEM/DER file <-> base64.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The @code(ServerCertificate) property of TS7CommPlusDriver holds the certificate(s) the
  driver trusts as ONE base64 string of the DER (several certificates, if there are several,
  go one after the other before the base64). It travels in the .lfm with the rest of the form.
  This unit turns what the user picks in a file (a text PEM with one or more CERTIFICATE
  blocks, or a binary DER) into that string, and the string back into DER for OpenSSL. It
  needs no OpenSSL and no network: it can be tested on its own.

  Private keys that happen to be in the same PEM file are IGNORED and never reach the string.
}
{$ENDIF}
unit S7PlusCertificate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TS7PlusCertDERArray = array of TBytes;

{$IFDEF PORTUGUES}
//: O conteúdo de um arquivo (PEM ou DER) como string base64 do(s) certificado(s) em DER. Count
//: diz quantos certificados foram achados. '' (e Count=0) se o arquivo não tem nenhum certificado.
{$ELSE}
//: The content of a file (PEM or DER) as the base64 string of the certificate(s) in DER. Count
//: says how many were found. '' (and Count=0) when the file has no certificate.
{$ENDIF}
function S7PlusCertFileToBase64(const FileData:TBytes; out Count:Integer):String;

{$IFDEF PORTUGUES}
//: A string da propriedade de volta em certificados DER, um por item. False se não for base64
//: válido ou se o que há dentro não for uma sequência de certificados DER inteiros.
{$ELSE}
//: The string of the property back as DER certificates, one per item. False when it is not
//: valid base64 or what is inside is not a run of whole DER certificates.
{$ENDIF}
function S7PlusCertBase64ToDER(const B64:String; out Certs:TS7PlusCertDERArray):Boolean;

{$IFDEF PORTUGUES}
//: Os bytes DER (de um ou mais certificados) como a string base64 da propriedade; '' se vazio.
{$ELSE}
//: The DER bytes (of one or more certificates) as the base64 string of the property; '' if empty.
{$ENDIF}
function S7PlusDerToBase64(const Der:TBytes):String;

{$IFDEF PORTUGUES}
//: O tamanho total (cabeçalho + conteúdo) do elemento DER que começa em Offset de Data, ou -1
//: se não for uma SEQUENCE bem formada que cabe nos bytes.
{$ELSE}
//: The total size (header + content) of the DER element that starts at Offset of Data, or -1
//: when it is not a well formed SEQUENCE that fits in the bytes.
{$ENDIF}
function S7PlusDerSequenceLength(const Data:TBytes; Offset:Integer):Integer;

implementation

uses
  base64;

const
  PemBegin = '-----BEGIN CERTIFICATE-----';
  PemEnd   = '-----END CERTIFICATE-----';

function BytesToText(const B:TBytes):String;
begin
  SetLength(Result, Length(B));
  if Length(B)>0 then Move(B[0], Result[1], Length(B));
end;

function TextToBytes(const S:String):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

function DecodeBase64Text(const Text:String; out Bytes:TBytes):Boolean;
var
  Clean:String;
  i:Integer;
  Decoded:String;
begin
  Result := false;
  SetLength(Bytes, 0);
  Clean := '';
  for i:=1 to Length(Text) do
    if Text[i] in ['A'..'Z','a'..'z','0'..'9','+','/','='] then
      Clean := Clean+Text[i]
    else if not (Text[i] in [#9,#10,#13,' ']) then
      exit;                                    //something that is not base64
  if (Clean='') or (Length(Clean) mod 4<>0) then exit;
  try
    Decoded := DecodeStringBase64(Clean, true);
  except
    exit;
  end;
  Bytes := TextToBytes(Decoded);
  Result := Length(Bytes)>0;
end;

function S7PlusDerSequenceLength(const Data:TBytes; Offset:Integer):Integer;
var
  LenByte, n, i, Header, Content:Integer;
begin
  Result := -1;
  if (Offset<0) or (Offset+2>Length(Data)) then exit;
  if Data[Offset]<>$30 then exit;                   //SEQUENCE
  LenByte := Data[Offset+1];
  if LenByte<$80 then begin
    Header := 2;
    Content := LenByte;
  end else begin
    n := LenByte and $7F;
    if (n=0) or (n>4) or (Offset+2+n>Length(Data)) then exit;   //indefinite or absurd length
    Content := 0;
    for i:=1 to n do
      Content := (Content shl 8) or Data[Offset+1+i];
    Header := 2+n;
    if Content<0 then exit;
  end;
  if (Content<=0) or (Offset+Header+Content>Length(Data)) then exit;
  Result := Header+Content;
end;

//Every whole DER SEQUENCE in Data, back to back; false when something is left over or malformed.
function SplitDER(const Data:TBytes; out Certs:TS7PlusCertDERArray):Boolean;
var
  Offset, L:Integer;
begin
  SetLength(Certs, 0);
  Result := false;
  Offset := 0;
  while Offset<Length(Data) do begin
    L := S7PlusDerSequenceLength(Data, Offset);
    if L<0 then begin
      SetLength(Certs, 0);
      exit;
    end;
    SetLength(Certs, Length(Certs)+1);
    SetLength(Certs[High(Certs)], L);
    Move(Data[Offset], Certs[High(Certs)][0], L);
    Inc(Offset, L);
  end;
  Result := Length(Certs)>0;
end;

function S7PlusCertFileToBase64(const FileData:TBytes; out Count:Integer):String;
var
  Text, Block:String;
  P, Q:Integer;
  All, One:TBytes;
  Certs:TS7PlusCertDERArray;
begin
  Result := '';
  Count := 0;
  if Length(FileData)=0 then exit;
  SetLength(All, 0);

  Text := BytesToText(FileData);
  if Pos(PemBegin, Text)>0 then begin
    //PEM: each CERTIFICATE block; any other block (a private key) is not looked at
    P := 1;
    repeat
      Q := Pos(PemBegin, Copy(Text, P, MaxInt));
      if Q=0 then break;                        //no more certificates
      P := P+Q-1;
      Q := Pos(PemEnd, Copy(Text, P, MaxInt));
      if Q=0 then begin
        Count := 0;
        exit;                                   //a block that never ends
      end;
      Block := Copy(Text, P+Length(PemBegin), Q-1-Length(PemBegin));
      if not DecodeBase64Text(Block, One) then begin
        Count := 0;
        exit;
      end;
      if S7PlusDerSequenceLength(One, 0)<>Length(One) then begin
        Count := 0;
        exit;                                   //not one whole certificate
      end;
      All := Concat(All, One);
      Inc(Count);
      P := P+Q-1+Length(PemEnd);
    until P>Length(Text);
  end else begin
    //DER: one or more certificates, back to back
    if not SplitDER(FileData, Certs) then exit;
    All := Copy(FileData, 0, Length(FileData));
    Count := Length(Certs);
  end;

  if (Count=0) or (Length(All)=0) then begin
    Count := 0;
    exit;
  end;
  Result := EncodeStringBase64(BytesToText(All));
end;

function S7PlusDerToBase64(const Der:TBytes):String;
begin
  Result := '';
  if Length(Der)>0 then
    Result := EncodeStringBase64(BytesToText(Der));
end;

function S7PlusCertBase64ToDER(const B64:String; out Certs:TS7PlusCertDERArray):Boolean;
var
  Bytes:TBytes;
begin
  SetLength(Certs, 0);
  Result := false;
  if not DecodeBase64Text(B64, Bytes) then exit;
  Result := SplitDER(Bytes, Certs);
end;

end.
