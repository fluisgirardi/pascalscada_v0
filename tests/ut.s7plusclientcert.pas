{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do certificado de CLIENTE do TLS do S7CommPlus (TLS mutuo).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os certificados e chaves abaixo sao DESCARTAVEIS, gerados so para estes testes (EC P-256, senha
  "segredo"); nenhum serve para nada fora daqui. O handshake mutuo roda dentro do processo, com dois
  SSL ligados por BIOs de memoria (o mesmo jeito que o driver usa), sem rede e sem CLP.
}
{$ELSE}
{:
  @abstract(Tests of the CLIENT certificate of the S7CommPlus TLS (mutual TLS).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The certificates and keys below are THROWAWAY ones, made just for these tests (EC P-256, password
  "segredo"); none is good for anything outside here. The mutual handshake runs in-process, with two
  SSL objects joined by memory BIOs (the way the driver does it), with no network and no PLC.
}
{$ENDIF}
unit ut.s7plusclientcert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, ctypes, fpcunit, testregistry, S7PlusSSL, S7PlusCertificate;

type

  { TTestS7PlusClientCertificate }

  TTestS7PlusClientCertificate = class(TTestCase)
  private
    function CtxWith(const Cred:String; const Pass:AnsiString; out Err:String):Boolean;
    function Handshake(const ClientCred:String; const ClientPass:AnsiString; out ServerSawDER:TBytes):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //the property <-> bytes conversions (no OpenSSL)
    procedure APemTextIsKeptAsItIs;
    procedure APemWithoutAKeyIsReported;
    procedure APkcs12IsKeptAsBase64;
    procedure SomethingElseIsNotACredential;
    procedure TheBytesComeFromTextBase64OrAFile;
    procedure NonsenseIsRefused;
    //loading into an SSL context (needs OpenSSL)
    procedure TheCertificateAndTheKeyInAnyOrderLoad;
    procedure AnEncryptedKeyNeedsItsPassword;
    procedure APkcs12LoadsWithItsPassword;
    procedure TheKeyOfAnotherCertificateIsRefused;
    procedure ACertificateWithoutKeyIsRefused;
    procedure GarbageIsRefused;
    //the handshake (needs OpenSSL)
    procedure AServerThatAsksGetsTheClientCertificate;
    procedure AServerThatAsksRefusesAClientWithout;
  end;


implementation

type
  TFn_TLS_server_method = function:Pointer; cdecl;
  TFn_SSL_set_accept_state = procedure(assl:PSSL); cdecl;

var
  LibSSL, LibCrypto:TLibHandle;
  TLS_server_method:TFn_TLS_server_method;
  SSL_set_accept_state:TFn_SSL_set_accept_state;

function TryNames(const Names:array of String):TLibHandle;
var
  i:Integer;
begin
  Result := NilHandle;
  for i:=0 to High(Names) do begin
    Result := LoadLibrary(Names[i]);
    if Result<>NilHandle then exit;
  end;
end;

//the server side of the handshake is not part of the driver: bound here, from the same libraries
procedure BindServerSide;
begin
  if Assigned(TLS_server_method) then exit;
  LibSSL := TryNames(['libssl.so.3', 'libssl.so.1.1', 'libssl.so', 'libssl-3-x64.dll', 'libssl-3.dll',
                      'libssl-1_1-x64.dll', 'libssl-1_1.dll', 'libssl.3.dylib', 'libssl.dylib']);
  if LibSSL=NilHandle then exit;
  TLS_server_method := TFn_TLS_server_method(GetProcedureAddress(LibSSL, 'TLS_server_method'));
  SSL_set_accept_state := TFn_SSL_set_accept_state(GetProcedureAddress(LibSSL, 'SSL_set_accept_state'));
end;

const
  TestPass = 'segredo';

{$I s7plusclientcert_fixtures.inc}

function DERs(const PemText:String):TS7PlusCertDERArray;
var
  Data:TBytes;
  Count:Integer;
  B64:String;
begin
  SetLength(Data, Length(PemText));
  Move(PemText[1], Data[0], Length(PemText));
  B64 := S7PlusCertFileToBase64(Data, Count);
  S7PlusCertBase64ToDER(B64, Result);
end;

{ TTestS7PlusClientCertificate }

procedure TTestS7PlusClientCertificate.SetUp;
begin
  inherited SetUp;
  if not S7PlusSSLLoad then
    Ignore('OpenSSL is not available here: '+S7PlusSSLLoadError);
end;

procedure TTestS7PlusClientCertificate.TearDown;
begin
  S7PlusSSLUnload;
  inherited TearDown;
end;

//a fresh context loaded with a credential given the way the property gives it
function TTestS7PlusClientCertificate.CtxWith(const Cred:String; const Pass:AnsiString; out Err:String):Boolean;
var
  Ctx:PSSLCTX;
  Data:TBytes;
begin
  Result := false;
  if not S7PlusClientCredentialToBytes(Cred, Data, Err) then exit;
  Ctx := SSL_CTX_new(TLS_client_method());
  AssertTrue('a context', Ctx<>nil);
  try
    Result := S7PlusSSLCtxUseClientCertificate(Ctx, Data, Pass, Err);
  finally
    SSL_CTX_free(Ctx);
  end;
end;

procedure TTestS7PlusClientCertificate.APemTextIsKeptAsItIs;
var
  HasKey:Boolean;
  Bytes:TBytes;
  V:String;
begin
  V := ClientCrtPEM+LineEnding+ClientKeyPEM;
  SetLength(Bytes, Length(V));
  Move(V[1], Bytes[0], Length(V));
  AssertEquals(V, S7PlusClientCredentialFromFile(Bytes, HasKey));
  AssertTrue('and it has the key', HasKey);
end;

procedure TTestS7PlusClientCertificate.APemWithoutAKeyIsReported;
var
  HasKey:Boolean;
  Bytes:TBytes;
begin
  SetLength(Bytes, Length(ClientCrtPEM));
  Move(ClientCrtPEM[1], Bytes[0], Length(ClientCrtPEM));
  AssertEquals(ClientCrtPEM, S7PlusClientCredentialFromFile(Bytes, HasKey));
  AssertFalse('no key in it', HasKey);
end;

procedure TTestS7PlusClientCertificate.APkcs12IsKeptAsBase64;
var
  HasKey:Boolean;
  Bytes:TBytes;
  Back:TBytes;
  Err:String;
begin
  AssertTrue(S7PlusClientCredentialToBytes(ClientP12_B64, Bytes, Err));
  AssertEquals('as base64 again', ClientP12_B64, S7PlusClientCredentialFromFile(Bytes, HasKey));
  AssertTrue('the key is inside a PKCS#12', HasKey);
  AssertTrue(S7PlusClientCredentialToBytes(S7PlusClientCredentialFromFile(Bytes, HasKey), Back, Err));
  AssertEquals(Length(Bytes), Length(Back));
end;

procedure TTestS7PlusClientCertificate.SomethingElseIsNotACredential;
var
  HasKey:Boolean;
  Bytes:TBytes;
begin
  SetLength(Bytes, 5);
  Bytes[0] := 1; Bytes[1] := 2; Bytes[2] := 3; Bytes[3] := 4; Bytes[4] := 5;
  AssertEquals('binary that is not a DER sequence', '', S7PlusClientCredentialFromFile(Bytes, HasKey));
  SetLength(Bytes, 0);
  AssertEquals('empty', '', S7PlusClientCredentialFromFile(Bytes, HasKey));
  Bytes := nil;
  SetLength(Bytes, Length(ClientKeyPEM));
  Move(ClientKeyPEM[1], Bytes[0], Length(ClientKeyPEM));
  AssertEquals('a key alone has no certificate', '', S7PlusClientCredentialFromFile(Bytes, HasKey));
end;

procedure TTestS7PlusClientCertificate.TheBytesComeFromTextBase64OrAFile;
var
  Data:TBytes;
  Err, Name:String;
  FS:TFileStream;
begin
  AssertTrue('text', S7PlusClientCredentialToBytes(ClientCrtPEM, Data, Err));
  AssertEquals(Length(ClientCrtPEM), Length(Data));
  AssertTrue('base64', S7PlusClientCredentialToBytes(ClientP12_B64, Data, Err));
  AssertTrue(Length(Data)>100);
  Name := GetTempFileName(GetTempDir, 'cc');
  FS := TFileStream.Create(Name, fmCreate);
  try
    FS.WriteBuffer(ClientCrtPEM[1], Length(ClientCrtPEM));
  finally
    FS.Free;
  end;
  try
    AssertTrue('a file name', S7PlusClientCredentialToBytes(Name, Data, Err));
    AssertEquals(Length(ClientCrtPEM), Length(Data));
  finally
    DeleteFile(Name);
  end;
end;

procedure TTestS7PlusClientCertificate.NonsenseIsRefused;
var
  Data:TBytes;
  Err:String;
begin
  AssertFalse('empty', S7PlusClientCredentialToBytes('   ', Data, Err));
  AssertFalse('not base64, not a file', S7PlusClientCredentialToBytes('isto nao e nada!', Data, Err));
  AssertTrue('and says why', Err<>'');
end;

procedure TTestS7PlusClientCertificate.TheCertificateAndTheKeyInAnyOrderLoad;
var
  Err:String;
begin
  AssertTrue('certificate then key: '+Err, CtxWith(ClientCrtPEM+LineEnding+ClientKeyPEM, '', Err));
  AssertTrue('key then certificate: '+Err, CtxWith(ClientKeyPEM+LineEnding+ClientCrtPEM, '', Err));
end;

procedure TTestS7PlusClientCertificate.AnEncryptedKeyNeedsItsPassword;
var
  Err:String;
begin
  AssertTrue('right password: '+Err, CtxWith(ClientCrtPEM+LineEnding+ClientEncKeyPEM, TestPass, Err));
  AssertFalse('wrong password', CtxWith(ClientCrtPEM+LineEnding+ClientEncKeyPEM, 'errada', Err));
  AssertTrue('with a reason', Err<>'');
  AssertFalse('no password (and no question on the console)', CtxWith(ClientCrtPEM+LineEnding+ClientEncKeyPEM, '', Err));
end;

procedure TTestS7PlusClientCertificate.APkcs12LoadsWithItsPassword;
var
  Err:String;
begin
  AssertTrue('right password: '+Err, CtxWith(ClientP12_B64, TestPass, Err));
  AssertFalse('wrong password', CtxWith(ClientP12_B64, 'errada', Err));
  AssertTrue('with a reason', Err<>'');
end;

procedure TTestS7PlusClientCertificate.TheKeyOfAnotherCertificateIsRefused;
var
  Err:String;
begin
  AssertFalse(CtxWith(ClientCrtPEM+LineEnding+OtherKeyPEM, '', Err));
  AssertTrue('says it is not the key of the certificate: '+Err, Pos('nao e a do certificado', Err)>0);
end;

procedure TTestS7PlusClientCertificate.ACertificateWithoutKeyIsRefused;
var
  Err:String;
begin
  AssertFalse(CtxWith(ClientCrtPEM, '', Err));
  AssertTrue('and says why', Err<>'');
end;

procedure TTestS7PlusClientCertificate.GarbageIsRefused;
var
  Err:String;
  Data:TBytes;
  Ctx:PSSLCTX;
begin
  SetLength(Data, 40);
  FillChar(Data[0], 40, 7);
  Ctx := SSL_CTX_new(TLS_client_method());
  try
    AssertFalse(S7PlusSSLCtxUseClientCertificate(Ctx, Data, '', Err));
    AssertTrue('and says why', Err<>'');
    SetLength(Data, 0);
    AssertFalse('empty', S7PlusSSLCtxUseClientCertificate(Ctx, Data, '', Err));
  finally
    SSL_CTX_free(Ctx);
  end;
end;

//Two SSL objects, a client and a server that REQUIRES a client certificate (and trusts only the
//test one), joined by memory BIOs. ClientCred '' = a client with no certificate.
function TTestS7PlusClientCertificate.Handshake(const ClientCred:String; const ClientPass:AnsiString;
  out ServerSawDER:TBytes):Boolean;
var
  SCtx, CCtx:PSSLCTX;
  S, C:PSSL;
  SR, SW, CR, CW:PBIO;
  Data:TBytes;
  Err:String;
  Round:Integer;
  CDone, SDone, Failed:Boolean;
  Trust:TS7PlusCertDERArray;
  ServerCred:String;

  function Step(ASSL:PSSL; var Done:Boolean):Boolean;
  var
    Ret, E:cint;
  begin
    Result := true;
    if Done then exit;
    Ret := SSL_do_handshake(ASSL);
    if Ret=1 then
      Done := true
    else begin
      E := SSL_get_error(ASSL, Ret);
      Result := (E=SSL_ERROR_WANT_READ) or (E=SSL_ERROR_WANT_WRITE);
    end;
  end;

  procedure Pump(FromBio, ToBio:PBIO);
  var
    Buf:array[0..4095] of Byte;
    N:cint;
  begin
    while S7PlusBIOCtrlPending(FromBio)>0 do begin
      N := BIO_read(FromBio, @Buf[0], SizeOf(Buf));
      if N<=0 then break;
      BIO_write(ToBio, @Buf[0], N);
    end;
  end;

begin
  Result := false;
  SetLength(ServerSawDER, 0);
  BindServerSide;
  if not (Assigned(TLS_server_method) and Assigned(SSL_set_accept_state)) then
    Ignore('the server side of OpenSSL could not be bound');

  SCtx := SSL_CTX_new(TLS_server_method());
  CCtx := SSL_CTX_new(TLS_client_method());
  AssertTrue(SCtx<>nil);
  AssertTrue(CCtx<>nil);
  try
    ServerCred := ServerCrtPEM+LineEnding+ServerKeyPEM;
    SetLength(Data, Length(ServerCred));
    Move(ServerCred[1], Data[0], Length(ServerCred));
    AssertTrue('the server has its own certificate: '+Err, S7PlusSSLCtxUseClientCertificate(SCtx, Data, '', Err));
    //it asks for a certificate, fails without one and trusts only the test client
    Trust := DERs(ClientCrtPEM);
    AssertEquals(1, S7PlusSSLCtxTrustCertificates(SCtx, [Trust[0]]));
    SSL_CTX_set_verify(SCtx, SSL_VERIFY_PEER or 2, nil);        //(2 = fail if no peer certificate)

    if ClientCred<>'' then begin
      AssertTrue('the client credential: '+Err, S7PlusClientCredentialToBytes(ClientCred, Data, Err));
      AssertTrue('the client certificate: '+Err, S7PlusSSLCtxUseClientCertificate(CCtx, Data, ClientPass, Err));
    end;

    S := SSL_new(SCtx);
    C := SSL_new(CCtx);
    SR := BIO_new(BIO_s_mem()); SW := BIO_new(BIO_s_mem());
    CR := BIO_new(BIO_s_mem()); CW := BIO_new(BIO_s_mem());
    SSL_set_bio(S, SR, SW);
    SSL_set_bio(C, CR, CW);
    SSL_set_accept_state(S);
    SSL_set_connect_state(C);

    CDone := false; SDone := false; Failed := false;
    for Round:=1 to 30 do begin
      if not Step(C, CDone) then begin Failed := true; break; end;
      Pump(CW, SR);
      if not Step(S, SDone) then begin Failed := true; break; end;
      Pump(SW, CR);
      if CDone and SDone then break;
    end;
    Result := (not Failed) and CDone and SDone;
    if Result then
      ServerSawDER := S7PlusSSLPeerCertificateDER(S);
    SSL_free(S);
    SSL_free(C);
  finally
    SSL_CTX_free(SCtx);
    SSL_CTX_free(CCtx);
  end;
end;

procedure TTestS7PlusClientCertificate.AServerThatAsksGetsTheClientCertificate;
var
  Seen:TBytes;
  Mine:TS7PlusCertDERArray;
  ok:Boolean;
begin
  ok := Handshake(ClientCrtPEM+LineEnding+ClientKeyPEM, '', Seen);
  AssertTrue('the handshake with the certificate', ok);
  Mine := DERs(ClientCrtPEM);
  AssertEquals('the server saw our certificate', Length(Mine[0]), Length(Seen));
  AssertTrue('the very same bytes', CompareMem(@Mine[0][0], @Seen[0], Length(Seen)));
  //(and the same with the PKCS#12 and the encrypted key)
  ok := Handshake(ClientP12_B64, TestPass, Seen);
  AssertTrue('the handshake with the PKCS#12', ok);
  AssertEquals(Length(Mine[0]), Length(Seen));
  ok := Handshake(ClientCrtPEM+LineEnding+ClientEncKeyPEM, TestPass, Seen);
  AssertTrue('the handshake with the encrypted key', ok);
end;

procedure TTestS7PlusClientCertificate.AServerThatAsksRefusesAClientWithout;
var
  Seen:TBytes;
begin
  AssertFalse('no client certificate: the server refuses', Handshake('', '', Seen));
end;

initialization
  RegisterTest(TTestS7PlusClientCertificate);
end.
