unit S7PlusHarpoRandom;

{$mode objfpc}{$H+}

//: Indirection point for "random" bytes used by the SessionKey handshake (client-
//: generated key material) - defaults to OpenSSL's RAND_bytes, but can be overridden
//: to a deterministic mock for testing against HarpoS7's own fixed-fill-sequence test
//: vectors (mirrors the Python port's `patch("os.urandom", mock_urandom)`).

interface

uses SysUtils, S7PlusSSL;

type
  THarpoRandomBytesFunc = function(Count:Integer):TBytes;

var
  HarpoRandomBytes:THarpoRandomBytesFunc;

implementation

initialization
  HarpoRandomBytes := @S7PlusRandomBytes;

end.
