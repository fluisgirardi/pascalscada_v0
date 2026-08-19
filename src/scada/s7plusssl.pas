{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Bindings dinâmicos mínimos de OpenSSL (libssl/libcrypto) para o túnel TLS do S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Expõe só o necessário para um handshake TLS 1.2/1.3 baseado em par de BIOs de memória
  (sem tocar no socket TCP diretamente - o S7CommPlus tuneliza os registros TLS dentro dos
  frames de dados do COTP, então quem envia/recebe bytes é @link(S7PlusConnection), não
  esta unit). Carrega libssl/libcrypto dinamicamente em tempo de execução (várias tentativas
  de nome, por plataforma/versão do OpenSSL), sem criar dependência de pacote de
  desenvolvimento - só exige a lib do sistema operacional presente.
}
{$ELSE}
{:
  @abstract(Minimal dynamic OpenSSL (libssl/libcrypto) bindings for the S7CommPlus TLS tunnel.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Exposes only what's needed for a TLS 1.2/1.3 handshake driven by a memory-BIO pair
  (never touches the TCP socket directly - S7CommPlus tunnels TLS records inside COTP
  data frames, so the actual byte I/O is done by @link(S7PlusConnection), not this unit).
  Loads libssl/libcrypto dynamically at runtime (several candidate names, per platform/
  OpenSSL version), so it doesn't add a build-time dependency - only requires the OS
  library to be present.
}
{$ENDIF}
unit S7PlusSSL;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, ctypes;

type
  PSSLCTX = Pointer;
  PSSL    = Pointer;
  PBIO    = Pointer;

const
  //-- SSL_get_error() results.
  SSL_ERROR_NONE             = 0;
  SSL_ERROR_SSL              = 1;
  SSL_ERROR_WANT_READ        = 2;
  SSL_ERROR_WANT_WRITE       = 3;
  SSL_ERROR_SYSCALL          = 5;
  SSL_ERROR_ZERO_RETURN      = 6;

  //-- Certificate verification modes.
  SSL_VERIFY_NONE            = $00;

  //-- SSL_CTX_ctrl()/SSL_ctrl() command codes actually used here.
  SSL_CTRL_SET_MIN_PROTO_VERSION = 123;
  SSL_CTRL_SET_MAX_PROTO_VERSION = 124;
  SSL_CTRL_SET_GROUPS_LIST       = 92;

  //-- Protocol version numbers (as used by SSL_CTX_ctrl SET_MIN/MAX_PROTO_VERSION).
  TLS1_2_VERSION = $0303;
  TLS1_3_VERSION = $0304;

  //-- SSL_CTX_set_options() bits used to match Siemens PLC expectations.
  SSL_OP_NO_EXTENDED_MASTER_SECRET = UInt64(1) shl 0;
  SSL_OP_NO_TICKET                 = UInt64(1) shl 14;
  SSL_OP_NO_ENCRYPT_THEN_MAC       = UInt64(1) shl 19;

  //-- BIO_ctrl() command code used to query pending bytes (BIO_ctrl_pending macro).
  BIO_CTRL_PENDING = 10;

//: Loads libssl/libcrypto (tries several candidate names/versions). Safe to call more
//: than once (reference-counted). Returns @true if both libraries and every required
//: symbol were resolved.
function S7PlusSSLLoad:Boolean;
//: Releases the libraries loaded by @link(S7PlusSSLLoad), once the last caller releases it.
procedure S7PlusSSLUnload;
//: Tells if @link(S7PlusSSLLoad) has succeeded and the bindings below are usable.
function S7PlusSSLLoaded:Boolean;
//: Human-readable reason @link(S7PlusSSLLoad) failed, for diagnostics.
function S7PlusSSLLoadError:String;

type
  //-- Named procedural types, one per bound function - assigning a raw Pointer to an
  //-- anonymous "function(...):T; cdecl;" variable via Pointer(Var):=... is ambiguous
  //-- in FPC (it tries to CALL the variable instead of casting it), so every function
  //-- pointer below needs its own named type to cast through.
  TFn_TLS_client_method = function:Pointer; cdecl;
  TFn_SSL_CTX_new = function(meth:Pointer):PSSLCTX; cdecl;
  TFn_SSL_CTX_free = procedure(ctx:PSSLCTX); cdecl;
  TFn_SSL_CTX_set_cipher_list = function(ctx:PSSLCTX; const AStr:PAnsiChar):cint; cdecl;
  TFn_SSL_CTX_set_options = function(ctx:PSSLCTX; op:UInt64):UInt64; cdecl;
  TFn_SSL_CTX_ctrl = function(ctx:PSSLCTX; cmd:cint; larg:clong; parg:Pointer):clong; cdecl;
  TFn_SSL_CTX_set_verify = procedure(ctx:PSSLCTX; mode:cint; callback:Pointer); cdecl;
  TFn_SSL_new = function(ctx:PSSLCTX):PSSL; cdecl;
  TFn_SSL_free = procedure(assl:PSSL); cdecl;
  TFn_SSL_set_bio = procedure(assl:PSSL; rbio, wbio:PBIO); cdecl;
  TFn_SSL_set_connect_state = procedure(assl:PSSL); cdecl;
  TFn_SSL_do_handshake = function(assl:PSSL):cint; cdecl;
  TFn_SSL_read = function(assl:PSSL; buf:Pointer; num:cint):cint; cdecl;
  TFn_SSL_write = function(assl:PSSL; const buf:Pointer; num:cint):cint; cdecl;
  TFn_SSL_get_error = function(assl:PSSL; ret:cint):cint; cdecl;
  TFn_SSL_export_keying_material = function(assl:PSSL; outbuf:Pointer; olen:csize_t;
                                             const alabel:PAnsiChar; llen:csize_t;
                                             context:Pointer; contextlen:csize_t;
                                             use_context:cint):cint; cdecl;
  TFn_SSL_get_version = function(assl:PSSL):PAnsiChar; cdecl;
  TFn_SSL_get_current_cipher = function(assl:PSSL):Pointer; cdecl;
  TFn_SSL_CIPHER_get_name = function(cipher:Pointer):PAnsiChar; cdecl;

  TFn_BIO_new = function(AType:Pointer):PBIO; cdecl;
  TFn_BIO_s_mem = function:Pointer; cdecl;
  TFn_BIO_read = function(b:PBIO; data:Pointer; len:cint):cint; cdecl;
  TFn_BIO_write = function(b:PBIO; const data:Pointer; len:cint):cint; cdecl;
  TFn_BIO_ctrl = function(bp:PBIO; cmd:cint; larg:clong; parg:Pointer):clong; cdecl;
  TFn_BIO_free = function(b:PBIO):cint; cdecl;

  //-- libcrypto - EVP digest/cipher, used for S7CommPlus legitimation (password
  //-- authentication): SHA-1/SHA-256 for password hashing/key derivation, AES-256-CBC for
  //-- the "new"-style encrypted challenge response. See S7PlusSHA1/S7PlusSHA256/
  //-- S7PlusAES256CBCEncrypt below.
  TFn_EVP_sha1 = function:Pointer; cdecl;
  TFn_EVP_sha256 = function:Pointer; cdecl;
  TFn_EVP_Digest = function(const data:Pointer; count:csize_t; md:PByte; size:pcuint;
                             mdtype:Pointer; impl_:Pointer):cint; cdecl;
  TFn_EVP_CIPHER_CTX_new = function:Pointer; cdecl;
  TFn_EVP_CIPHER_CTX_free = procedure(ctx:Pointer); cdecl;
  TFn_EVP_aes_256_cbc = function:Pointer; cdecl;
  TFn_EVP_EncryptInit_ex = function(ctx:Pointer; cipher:Pointer; impl_:Pointer;
                                     const key:PByte; const iv:PByte):cint; cdecl;
  TFn_EVP_EncryptUpdate = function(ctx:Pointer; outb:PByte; outl:pcint;
                                    const inb:PByte; inl:cint):cint; cdecl;
  TFn_EVP_EncryptFinal_ex = function(ctx:Pointer; outb:PByte; outl:pcint):cint; cdecl;

var
  //-- libssl
  TLS_client_method:TFn_TLS_client_method;
  SSL_CTX_new:TFn_SSL_CTX_new;
  SSL_CTX_free:TFn_SSL_CTX_free;
  SSL_CTX_set_cipher_list:TFn_SSL_CTX_set_cipher_list;
  SSL_CTX_set_options:TFn_SSL_CTX_set_options;
  SSL_CTX_ctrl:TFn_SSL_CTX_ctrl;
  SSL_CTX_set_verify:TFn_SSL_CTX_set_verify;
  SSL_new:TFn_SSL_new;
  SSL_free:TFn_SSL_free;
  SSL_set_bio:TFn_SSL_set_bio;
  SSL_set_connect_state:TFn_SSL_set_connect_state;
  SSL_do_handshake:TFn_SSL_do_handshake;
  SSL_read:TFn_SSL_read;
  SSL_write:TFn_SSL_write;
  SSL_get_error:TFn_SSL_get_error;
  SSL_export_keying_material:TFn_SSL_export_keying_material;
  SSL_get_version:TFn_SSL_get_version;
  SSL_get_current_cipher:TFn_SSL_get_current_cipher;
  SSL_CIPHER_get_name:TFn_SSL_CIPHER_get_name;

  //-- libcrypto
  BIO_new:TFn_BIO_new;
  BIO_s_mem:TFn_BIO_s_mem;
  BIO_read:TFn_BIO_read;
  BIO_write:TFn_BIO_write;
  BIO_ctrl:TFn_BIO_ctrl;
  BIO_free:TFn_BIO_free;
  EVP_sha1:TFn_EVP_sha1;
  EVP_sha256:TFn_EVP_sha256;
  EVP_Digest:TFn_EVP_Digest;
  EVP_CIPHER_CTX_new:TFn_EVP_CIPHER_CTX_new;
  EVP_CIPHER_CTX_free:TFn_EVP_CIPHER_CTX_free;
  EVP_aes_256_cbc:TFn_EVP_aes_256_cbc;
  EVP_EncryptInit_ex:TFn_EVP_EncryptInit_ex;
  EVP_EncryptUpdate:TFn_EVP_EncryptUpdate;
  EVP_EncryptFinal_ex:TFn_EVP_EncryptFinal_ex;

//: BIO_ctrl_pending() macro equivalent: how many bytes are buffered and ready to read.
function S7PlusBIOCtrlPending(b:PBIO):clong;
//: SSL_CTX_set_min_proto_version() macro equivalent.
function S7PlusSSLCtxSetMinProtoVersion(ctx:PSSLCTX; version:cint):clong;
//: SSL_CTX_set1_groups_list() macro equivalent (restricts the offered EC groups).
function S7PlusSSLCtxSet1GroupsList(ctx:PSSLCTX; const AStr:PAnsiChar):clong;

//: SHA-1 digest of Data (20 bytes), via EVP_Digest. Empty result on any failure/unloaded lib.
function S7PlusSHA1(const Data:TBytes):TBytes;
//: SHA-256 digest of Data (32 bytes), via EVP_Digest. Empty result on any failure/unloaded lib.
function S7PlusSHA256(const Data:TBytes):TBytes;
//: AES-256-CBC encryption of Plaintext (Key must be 32 bytes, IV at least 16 - only the
//: first 16 are used), with PKCS7 padding (EVP's default). Empty result on any failure.
function S7PlusAES256CBCEncrypt(const Key, IV, Plaintext:TBytes):TBytes;

implementation

var
  SSLLib, CryptoLib:TLibHandle;
  RefCount:Integer;
  LoadError:String;

{$IFDEF WINDOWS}
const
  SSLNames:array[0..4] of String = ('libssl-3-x64.dll','libssl-3.dll','libssl-1_1-x64.dll','libssl-1_1.dll','ssleay32.dll');
  CryptoNames:array[0..4] of String = ('libcrypto-3-x64.dll','libcrypto-3.dll','libcrypto-1_1-x64.dll','libcrypto-1_1.dll','libeay32.dll');
{$ELSE}
  {$IFDEF DARWIN}
const
  SSLNames:array[0..2] of String = ('libssl.3.dylib','libssl.1.1.dylib','libssl.dylib');
  CryptoNames:array[0..2] of String = ('libcrypto.3.dylib','libcrypto.1.1.dylib','libcrypto.dylib');
  {$ELSE}
const
  SSLNames:array[0..2] of String = ('libssl.so.3','libssl.so.1.1','libssl.so');
  CryptoNames:array[0..2] of String = ('libcrypto.so.3','libcrypto.so.1.1','libcrypto.so');
  {$ENDIF}
{$ENDIF}

function TryLoad(const Names:array of String):TLibHandle;
var
  i:Integer;
begin
  Result := NilHandle;
  for i:=0 to High(Names) do begin
    Result := LoadLibrary(Names[i]);
    if Result<>NilHandle then exit;
  end;
end;

function Bind(Lib:TLibHandle; const AName:String; out Missing:String):Pointer;
begin
  Result := GetProcedureAddress(Lib, AName);
  if (Result=nil) and (Missing='') then
    Missing := AName;
end;

function S7PlusSSLLoad:Boolean;
var
  Missing:String;
begin
  if RefCount>0 then begin
    Inc(RefCount);
    Result := (SSLLib<>NilHandle) and (CryptoLib<>NilHandle) and (LoadError='');
    exit;
  end;

  Missing := '';
  SSLLib    := TryLoad(SSLNames);
  CryptoLib := TryLoad(CryptoNames);

  if SSLLib=NilHandle then Missing := 'libssl';
  if (Missing='') and (CryptoLib=NilHandle) then Missing := 'libcrypto';

  if Missing<>'' then begin
    LoadError := 'Nao foi possivel carregar '+Missing+' (necessario para TLS/S7CommPlus V2+).';
    Result := false;
    exit;
  end;

  TLS_client_method          := TFn_TLS_client_method(Bind(SSLLib, 'TLS_client_method', Missing));
  SSL_CTX_new                := TFn_SSL_CTX_new(Bind(SSLLib, 'SSL_CTX_new', Missing));
  SSL_CTX_free                := TFn_SSL_CTX_free(Bind(SSLLib, 'SSL_CTX_free', Missing));
  SSL_CTX_set_cipher_list    := TFn_SSL_CTX_set_cipher_list(Bind(SSLLib, 'SSL_CTX_set_cipher_list', Missing));
  SSL_CTX_set_options        := TFn_SSL_CTX_set_options(Bind(SSLLib, 'SSL_CTX_set_options', Missing));
  SSL_CTX_ctrl               := TFn_SSL_CTX_ctrl(Bind(SSLLib, 'SSL_CTX_ctrl', Missing));
  SSL_CTX_set_verify          := TFn_SSL_CTX_set_verify(Bind(SSLLib, 'SSL_CTX_set_verify', Missing));
  SSL_new                    := TFn_SSL_new(Bind(SSLLib, 'SSL_new', Missing));
  SSL_free                    := TFn_SSL_free(Bind(SSLLib, 'SSL_free', Missing));
  SSL_set_bio                 := TFn_SSL_set_bio(Bind(SSLLib, 'SSL_set_bio', Missing));
  SSL_set_connect_state       := TFn_SSL_set_connect_state(Bind(SSLLib, 'SSL_set_connect_state', Missing));
  SSL_do_handshake           := TFn_SSL_do_handshake(Bind(SSLLib, 'SSL_do_handshake', Missing));
  SSL_read                   := TFn_SSL_read(Bind(SSLLib, 'SSL_read', Missing));
  SSL_write                  := TFn_SSL_write(Bind(SSLLib, 'SSL_write', Missing));
  SSL_get_error              := TFn_SSL_get_error(Bind(SSLLib, 'SSL_get_error', Missing));
  SSL_export_keying_material := TFn_SSL_export_keying_material(Bind(SSLLib, 'SSL_export_keying_material', Missing));
  SSL_get_version            := TFn_SSL_get_version(Bind(SSLLib, 'SSL_get_version', Missing));
  SSL_get_current_cipher     := TFn_SSL_get_current_cipher(Bind(SSLLib, 'SSL_get_current_cipher', Missing));
  SSL_CIPHER_get_name        := TFn_SSL_CIPHER_get_name(Bind(SSLLib, 'SSL_CIPHER_get_name', Missing));

  BIO_new    := TFn_BIO_new(Bind(CryptoLib, 'BIO_new', Missing));
  BIO_s_mem  := TFn_BIO_s_mem(Bind(CryptoLib, 'BIO_s_mem', Missing));
  BIO_read   := TFn_BIO_read(Bind(CryptoLib, 'BIO_read', Missing));
  BIO_write  := TFn_BIO_write(Bind(CryptoLib, 'BIO_write', Missing));
  BIO_ctrl   := TFn_BIO_ctrl(Bind(CryptoLib, 'BIO_ctrl', Missing));
  BIO_free   := TFn_BIO_free(Bind(CryptoLib, 'BIO_free', Missing));

  EVP_sha1             := TFn_EVP_sha1(Bind(CryptoLib, 'EVP_sha1', Missing));
  EVP_sha256           := TFn_EVP_sha256(Bind(CryptoLib, 'EVP_sha256', Missing));
  EVP_Digest           := TFn_EVP_Digest(Bind(CryptoLib, 'EVP_Digest', Missing));
  EVP_CIPHER_CTX_new   := TFn_EVP_CIPHER_CTX_new(Bind(CryptoLib, 'EVP_CIPHER_CTX_new', Missing));
  EVP_CIPHER_CTX_free  := TFn_EVP_CIPHER_CTX_free(Bind(CryptoLib, 'EVP_CIPHER_CTX_free', Missing));
  EVP_aes_256_cbc      := TFn_EVP_aes_256_cbc(Bind(CryptoLib, 'EVP_aes_256_cbc', Missing));
  EVP_EncryptInit_ex   := TFn_EVP_EncryptInit_ex(Bind(CryptoLib, 'EVP_EncryptInit_ex', Missing));
  EVP_EncryptUpdate    := TFn_EVP_EncryptUpdate(Bind(CryptoLib, 'EVP_EncryptUpdate', Missing));
  EVP_EncryptFinal_ex  := TFn_EVP_EncryptFinal_ex(Bind(CryptoLib, 'EVP_EncryptFinal_ex', Missing));

  if Missing<>'' then begin
    LoadError := 'Simbolo OpenSSL nao encontrado: '+Missing+'.';
    FreeLibrary(SSLLib); SSLLib := NilHandle;
    FreeLibrary(CryptoLib); CryptoLib := NilHandle;
    Result := false;
    exit;
  end;

  LoadError := '';
  Inc(RefCount);
  Result := true;
end;

procedure S7PlusSSLUnload;
begin
  if RefCount<=0 then exit;
  Dec(RefCount);
  if RefCount=0 then begin
    if SSLLib<>NilHandle then begin FreeLibrary(SSLLib); SSLLib := NilHandle; end;
    if CryptoLib<>NilHandle then begin FreeLibrary(CryptoLib); CryptoLib := NilHandle; end;
  end;
end;

function S7PlusSSLLoaded:Boolean;
begin
  Result := (RefCount>0) and (SSLLib<>NilHandle) and (CryptoLib<>NilHandle) and (LoadError='');
end;

function S7PlusSSLLoadError:String;
begin
  Result := LoadError;
end;

function S7PlusBIOCtrlPending(b:PBIO):clong;
begin
  Result := BIO_ctrl(b, BIO_CTRL_PENDING, 0, nil);
end;

function S7PlusSSLCtxSetMinProtoVersion(ctx:PSSLCTX; version:cint):clong;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, version, nil);
end;

function S7PlusSSLCtxSet1GroupsList(ctx:PSSLCTX; const AStr:PAnsiChar):clong;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_GROUPS_LIST, 0, AStr);
end;

function S7PlusSHA1(const Data:TBytes):TBytes;
var
  Buf:array[0..63] of Byte; //EVP_MAX_MD_SIZE=64, SHA-1 only fills the first 20
  Size:cuint;
  DataPtr:Pointer;
begin
  SetLength(Result, 0);
  if not S7PlusSSLLoaded then exit;
  if Length(Data)>0 then DataPtr := @Data[0] else DataPtr := nil;
  Size := 0;
  if EVP_Digest(DataPtr, Length(Data), @Buf[0], @Size, EVP_sha1(), nil)<>1 then exit;
  SetLength(Result, Size);
  if Size>0 then Move(Buf[0], Result[0], Size);
end;

function S7PlusSHA256(const Data:TBytes):TBytes;
var
  Buf:array[0..63] of Byte;
  Size:cuint;
  DataPtr:Pointer;
begin
  SetLength(Result, 0);
  if not S7PlusSSLLoaded then exit;
  if Length(Data)>0 then DataPtr := @Data[0] else DataPtr := nil;
  Size := 0;
  if EVP_Digest(DataPtr, Length(Data), @Buf[0], @Size, EVP_sha256(), nil)<>1 then exit;
  SetLength(Result, Size);
  if Size>0 then Move(Buf[0], Result[0], Size);
end;

function S7PlusAES256CBCEncrypt(const Key, IV, Plaintext:TBytes):TBytes;
var
  Ctx:Pointer;
  OutBuf:array of Byte;
  OutLen1, OutLen2:cint;
  InPtr:PByte;
begin
  SetLength(Result, 0);
  if (Length(Key)<>32) or (Length(IV)<16) then exit;
  if not S7PlusSSLLoaded then exit;

  Ctx := EVP_CIPHER_CTX_new();
  if Ctx=nil then exit;
  try
    if EVP_EncryptInit_ex(Ctx, EVP_aes_256_cbc(), nil, @Key[0], @IV[0])<>1 then exit;

    SetLength(OutBuf, Length(Plaintext)+16); //PKCS7 padding adds at most one full block
    OutLen1 := 0;
    if Length(Plaintext)>0 then InPtr := @Plaintext[0] else InPtr := nil;
    if EVP_EncryptUpdate(Ctx, @OutBuf[0], @OutLen1, InPtr, Length(Plaintext))<>1 then exit;

    OutLen2 := 0;
    if EVP_EncryptFinal_ex(Ctx, @OutBuf[OutLen1], @OutLen2)<>1 then exit;

    SetLength(Result, OutLen1+OutLen2);
    if Length(Result)>0 then Move(OutBuf[0], Result[0], Length(Result));
  finally
    EVP_CIPHER_CTX_free(Ctx);
  end;
end;

initialization
  RefCount := 0;
  SSLLib := NilHandle;
  CryptoLib := NilHandle;
  LoadError := '';
finalization
  while RefCount>0 do
    S7PlusSSLUnload;
end.
