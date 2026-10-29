{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da conversao dos certificados do CLP (arquivo PEM/DER <-> string base64).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os certificados sao de teste (EC P-256, gerados so para isto); nao ha chave privada valida aqui.
}
{$ELSE}
{:
  @abstract(Tests of the conversion of the PLC certificates (PEM/DER file <-> base64 string).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The certificates are test ones (EC P-256, made just for this); no usable private key is here.
}
{$ENDIF}
unit ut.s7pluscertificate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testutils, S7PlusCertificate;

type

  { TTestS7PlusCertificate }

  TTestS7PlusCertificate = class(TTestCase)
  published
    procedure TestPemOne;
    procedure TestPemChainAndKeyIgnored;
    procedure TestDerFile;
    procedure TestDerTwo;
    procedure TestRoundTrip;
    procedure TestInvalid;
    procedure TestSequenceLength;
  end;

implementation

const
  CertA_B64 = 'MIIBkjCCATegAwIBAgIUXTjn7/RYAQNJ4hqUnwU2ws+6iIQwCgYIKoZIzj0E'
    + 'AwIwHTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1hMCAXDTI2MTAyOTAy'
    + 'NDQyMloYDzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2Fk'
    + 'YS10ZXN0LWEwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARAMC4iVD68Ix9p'
    + 'zwdadSuFBgqkn4bfN1WeaPCsXohFMmVNzp6uFx7tFV4OqTA2jZV+FMkS6vc0'
    + 'Rbgm5dUeYDj2o1MwUTAdBgNVHQ4EFgQUZMqMD/lLd9x/Q39HNvAQgGiZgNcw'
    + 'HwYDVR0jBBgwFoAUZMqMD/lLd9x/Q39HNvAQgGiZgNcwDwYDVR0TAQH/BAUw'
    + 'AwEB/zAKBggqhkjOPQQDAgNJADBGAiEA+hgLYFAOFteHY5CSPeuJSTBAEy01'
    + 'AK6LDenFNOscgNgCIQCCgqnMUHl05ES/UaxAmPuX5vy4r/+6CYuSQQsfHbAA'
    + 'EA==';
  CertB_B64 = 'MIIBkTCCATegAwIBAgIUBvIA5qkHpiHCUMF1pz+r3nLw3kAwCgYIKoZIzj0E'
    + 'AwIwHTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1iMCAXDTI2MTAyOTAy'
    + 'NDQyMloYDzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2Fk'
    + 'YS10ZXN0LWIwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARarWggb14Yj/ZW'
    + 'fpHUHTnqIO2ojpAQanMu1I4+YvQp6vMSvaS7+YgyQ/FzQs7cgkevHEiRi3j3'
    + '0SaF5BKYGLQ3o1MwUTAdBgNVHQ4EFgQU5xpuwbieTXFz3CNa18WrECp/+Www'
    + 'HwYDVR0jBBgwFoAU5xpuwbieTXFz3CNa18WrECp/+WwwDwYDVR0TAQH/BAUw'
    + 'AwEB/zAKBggqhkjOPQQDAgNIADBFAiEAqgwkYNzHz+G4hgm/5rmVicjtBI1U'
    + 'BLSgrcYdznYaH0sCIBnsoUR7DeoBueLOw7G1jkrFPXfWTkjwhR7ocVj9zmVk';
  CertAB_B64 = 'MIIBkjCCATegAwIBAgIUXTjn7/RYAQNJ4hqUnwU2ws+6iIQwCgYIKoZIzj0E'
    + 'AwIwHTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1hMCAXDTI2MTAyOTAy'
    + 'NDQyMloYDzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2Fk'
    + 'YS10ZXN0LWEwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARAMC4iVD68Ix9p'
    + 'zwdadSuFBgqkn4bfN1WeaPCsXohFMmVNzp6uFx7tFV4OqTA2jZV+FMkS6vc0'
    + 'Rbgm5dUeYDj2o1MwUTAdBgNVHQ4EFgQUZMqMD/lLd9x/Q39HNvAQgGiZgNcw'
    + 'HwYDVR0jBBgwFoAUZMqMD/lLd9x/Q39HNvAQgGiZgNcwDwYDVR0TAQH/BAUw'
    + 'AwEB/zAKBggqhkjOPQQDAgNJADBGAiEA+hgLYFAOFteHY5CSPeuJSTBAEy01'
    + 'AK6LDenFNOscgNgCIQCCgqnMUHl05ES/UaxAmPuX5vy4r/+6CYuSQQsfHbAA'
    + 'EDCCAZEwggE3oAMCAQICFAbyAOapB6YhwlDBdac/q95y8N5AMAoGCCqGSM49'
    + 'BAMCMB0xGzAZBgNVBAMMEnBhc2NhbHNjYWRhLXRlc3QtYjAgFw0yNjEwMjkw'
    + 'MjQ0MjJaGA8yMTI2MTAwNTAyNDQyMlowHTEbMBkGA1UEAwwScGFzY2Fsc2Nh'
    + 'ZGEtdGVzdC1iMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEWq1oIG9eGI/2'
    + 'Vn6R1B056iDtqI6QEGpzLtSOPmL0KerzEr2ku/mIMkPxc0LO3IJHrxxIkYt4'
    + '99EmheQSmBi0N6NTMFEwHQYDVR0OBBYEFOcabsG4nk1xc9wjWtfFqxAqf/ls'
    + 'MB8GA1UdIwQYMBaAFOcabsG4nk1xc9wjWtfFqxAqf/lsMA8GA1UdEwEB/wQF'
    + 'MAMBAf8wCgYIKoZIzj0EAwIDSAAwRQIhAKoMJGDcx8/huIYJv+a5lYnI7QSN'
    + 'VAS0oK3GHc52Gh9LAiAZ7KFEew3qAbnizsOxtY5KxT131k5I8IUe6HFY/c5l'
    + 'ZA==';

  PemA =
    '-----BEGIN CERTIFICATE-----'+#10
    + 'MIIBkjCCATegAwIBAgIUXTjn7/RYAQNJ4hqUnwU2ws+6iIQwCgYIKoZIzj0EAwIw'+#10
    + 'HTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1hMCAXDTI2MTAyOTAyNDQyMloY'+#10
    + 'DzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2FkYS10ZXN0LWEw'+#10
    + 'WTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARAMC4iVD68Ix9pzwdadSuFBgqkn4bf'+#10
    + 'N1WeaPCsXohFMmVNzp6uFx7tFV4OqTA2jZV+FMkS6vc0Rbgm5dUeYDj2o1MwUTAd'+#10
    + 'BgNVHQ4EFgQUZMqMD/lLd9x/Q39HNvAQgGiZgNcwHwYDVR0jBBgwFoAUZMqMD/lL'+#10
    + 'd9x/Q39HNvAQgGiZgNcwDwYDVR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNJADBG'+#10
    + 'AiEA+hgLYFAOFteHY5CSPeuJSTBAEy01AK6LDenFNOscgNgCIQCCgqnMUHl05ES/'+#10
    + 'UaxAmPuX5vy4r/+6CYuSQQsfHbAAEA=='+#10
    + '-----END CERTIFICATE-----'+#10;
  PemB =
    '-----BEGIN CERTIFICATE-----'+#10
    + 'MIIBkTCCATegAwIBAgIUBvIA5qkHpiHCUMF1pz+r3nLw3kAwCgYIKoZIzj0EAwIw'+#10
    + 'HTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1iMCAXDTI2MTAyOTAyNDQyMloY'+#10
    + 'DzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2FkYS10ZXN0LWIw'+#10
    + 'WTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARarWggb14Yj/ZWfpHUHTnqIO2ojpAQ'+#10
    + 'anMu1I4+YvQp6vMSvaS7+YgyQ/FzQs7cgkevHEiRi3j30SaF5BKYGLQ3o1MwUTAd'+#10
    + 'BgNVHQ4EFgQU5xpuwbieTXFz3CNa18WrECp/+WwwHwYDVR0jBBgwFoAU5xpuwbie'+#10
    + 'TXFz3CNa18WrECp/+WwwDwYDVR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNIADBF'+#10
    + 'AiEAqgwkYNzHz+G4hgm/5rmVicjtBI1UBLSgrcYdznYaH0sCIBnsoUR7DeoBueLO'+#10
    + 'w7G1jkrFPXfWTkjwhR7ocVj9zmVk'+#10
    + '-----END CERTIFICATE-----'+#10;
  KeyBlock =
    '-----BEGIN PRIVATE KEY-----'+#10
    + 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'+#10
    + '-----END PRIVATE KEY-----'+#10;

function B64ToBytes(const S:String):TBytes;
var
  Certs:TS7PlusCertDERArray;
  i:Integer;
begin
  SetLength(Result, 0);
  if not S7PlusCertBase64ToDER(S, Certs) then raise Exception.Create('bad test constant');
  for i:=0 to High(Certs) do
    Result := Concat(Result, Certs[i]);
end;

function TextBytes(const S:String):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

procedure TTestS7PlusCertificate.TestPemOne;
var
  n:Integer;
begin
  AssertEquals('b64', CertA_B64, S7PlusCertFileToBase64(TextBytes(PemA), n));
  AssertEquals('count', 1, n);
end;

procedure TTestS7PlusCertificate.TestPemChainAndKeyIgnored;
var
  n:Integer;
  S:String;
begin
  S := S7PlusCertFileToBase64(TextBytes(KeyBlock+PemA+PemB), n);
  AssertEquals('count', 2, n);
  AssertEquals('b64', CertAB_B64, S);
end;

procedure TTestS7PlusCertificate.TestDerFile;
var
  n:Integer;
begin
  AssertEquals('b64', CertB_B64, S7PlusCertFileToBase64(B64ToBytes(CertB_B64), n));
  AssertEquals('count', 1, n);
end;

procedure TTestS7PlusCertificate.TestDerTwo;
var
  n:Integer;
begin
  AssertEquals('b64', CertAB_B64, S7PlusCertFileToBase64(B64ToBytes(CertAB_B64), n));
  AssertEquals('count', 2, n);
end;

procedure TTestS7PlusCertificate.TestRoundTrip;
var
  Certs:TS7PlusCertDERArray;
begin
  AssertTrue('split', S7PlusCertBase64ToDER(CertAB_B64, Certs));
  AssertEquals('two', 2, Length(Certs));
  AssertEquals('first size', 406, Length(Certs[0]));
  AssertEquals('second size', 405, Length(Certs[1]));
  AssertEquals('again', CertAB_B64, S7PlusDerToBase64(Concat(Certs[0], Certs[1])));
  AssertEquals('empty', '', S7PlusDerToBase64(nil));
end;

procedure TTestS7PlusCertificate.TestInvalid;
var
  n:Integer;
  Certs:TS7PlusCertDERArray;
  Cut:TBytes;
begin
  AssertEquals('empty file', '', S7PlusCertFileToBase64(nil, n));
  AssertEquals('empty file count', 0, n);
  AssertEquals('text', '', S7PlusCertFileToBase64(TextBytes('hello world'), n));
  AssertEquals('text count', 0, n);
  AssertEquals('key only', '', S7PlusCertFileToBase64(TextBytes(KeyBlock), n));
  AssertEquals('block without end', '',
    S7PlusCertFileToBase64(TextBytes('-----BEGIN CERTIFICATE-----'+#10+'MIIB'+#10), n));
  AssertEquals('block with garbage', '',
    S7PlusCertFileToBase64(TextBytes('-----BEGIN CERTIFICATE-----'+#10+'!!!!'+#10+'-----END CERTIFICATE-----'+#10), n));
  Cut := B64ToBytes(CertA_B64);
  SetLength(Cut, Length(Cut)-5);
  AssertEquals('truncated DER', '', S7PlusCertFileToBase64(Cut, n));

  AssertFalse('empty property', S7PlusCertBase64ToDER('', Certs));
  AssertFalse('not base64', S7PlusCertBase64ToDER('this is not base64!', Certs));
  AssertFalse('base64 of text', S7PlusCertBase64ToDER('aGVsbG8gd29ybGQ=', Certs));
  AssertFalse('bad length', S7PlusCertBase64ToDER('MIIB', Certs));
  AssertEquals('no items on failure', 0, Length(Certs));
end;

procedure TTestS7PlusCertificate.TestSequenceLength;
var
  Der:TBytes;
begin
  Der := B64ToBytes(CertA_B64);
  AssertEquals('whole', 406, S7PlusDerSequenceLength(Der, 0));
  AssertEquals('not a sequence', -1, S7PlusDerSequenceLength(Der, 1));
  AssertEquals('offset out', -1, S7PlusDerSequenceLength(Der, 500));
  AssertEquals('negative', -1, S7PlusDerSequenceLength(Der, -1));
  AssertEquals('indefinite', -1, S7PlusDerSequenceLength(TextBytes(#$30#$80#0#0), 0));
end;

initialization
  RegisterTest(TTestS7PlusCertificate);
end.
