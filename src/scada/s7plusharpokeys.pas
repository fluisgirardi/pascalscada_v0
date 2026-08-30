unit S7PlusHarpoKeys;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/keys.py. Public-key store for the S7CommPlus session-key handshake:
//: the PLC advertises which key to use via ObjectVariableTypeName (attribute 233) in
//: its CreateObject response - a "FF:HHHHHHHHHHHHHHHH" string (family:fingerprint).
//: Only families 0 (S7-1500), 1 (S7-1200), 3 (PlcSim) are supported, matching
//: upstream HarpoS7 1.1.0.

interface

uses SysUtils;

const
  PUBLIC_KEY_LENGTH_REAL_PLC = 40;
  PUBLIC_KEY_LENGTH_PLCSIM = 64;

type
  TS7PlusHarpoKeyFamily = (hkfS7_1500=0, hkfS7_1200=1, hkfPLCSIM=3);

  EUnknownPublicKeyError = class(Exception);

//: Parses a "FF:HHHHHHHHHHHHHHHH" fingerprint string (as advertised by the PLC).
//: Raises Exception if malformed or the family isn't supported.
function ParseFingerprint(const Fingerprint:String; out Family:TS7PlusHarpoKeyFamily; out KeyId:String):Boolean;
//: Looks up the public-key bytes for a fingerprint string. Raises
//: EUnknownPublicKeyError if the family is supported but the id isn't bundled.
function GetPublicKey(const Fingerprint:String):TBytes;

implementation

function HexToBytes(const H:String):TBytes;
var
  i:Integer;
begin
  SetLength(Result, Length(H) div 2);
  for i:=0 to High(Result) do
    Result[i] := StrToInt('$'+Copy(H, i*2+1, 2));
end;

function ParseFingerprint(const Fingerprint:String; out Family:TS7PlusHarpoKeyFamily; out KeyId:String):Boolean;
var
  FamilyValue:Integer;
begin
  Result := false;
  KeyId := '';
  if (Length(Fingerprint)<>19) or (Fingerprint[3]<>':') then
    raise Exception.CreateFmt('Invalid fingerprint shape: %s', [Fingerprint]);
  try
    FamilyValue := StrToInt('$'+Copy(Fingerprint,1,2));
  except
    raise Exception.CreateFmt('Invalid family in fingerprint: %s', [Fingerprint]);
  end;
  case FamilyValue of
    0: Family := hkfS7_1500;
    1: Family := hkfS7_1200;
    3: Family := hkfPLCSIM;
  else
    raise Exception.CreateFmt('Unsupported public-key family 0x%.2X in fingerprint: %s', [FamilyValue, Fingerprint]);
  end;
  KeyId := UpperCase(Copy(Fingerprint, 4, 16));
  Result := true;
end;

function GetPublicKey(const Fingerprint:String):TBytes;
var
  Family:TS7PlusHarpoKeyFamily;
  KeyId:String;
  Result_:TBytes;

  procedure TryKey(F:TS7PlusHarpoKeyFamily; const Id, HexKey:String);
  begin
    if (F=Family) and (Id=KeyId) then
      Result_ := HexToBytes(HexKey);
  end;

begin
  ParseFingerprint(Fingerprint, Family, KeyId);
  SetLength(Result_, 0);

  //Family 0 - S7-1500 (40-byte keys)
  TryKey(hkfS7_1500, '0448ACCBD5A0BFD2', '9808369442d4f3b63f9aa40856ddf798579966bc2a9c382c3fbf13a4d00aaa98ffefc9ab38da5537');
  TryKey(hkfS7_1500, '181B7B0847D11694', '8456a26996122216c921c571ff11e0befafdb1d70b5d4bc8390f5b0cc273ec142a03f2a04e6f1593');
  TryKey(hkfS7_1500, '1B580465BB0551B2', 'd1c451071e18c526a423a9cd21349f01b6ccd9f2dda9e4dc2ff11356c9116b6dba611dc02995c097');
  TryKey(hkfS7_1500, '2C1DD211E529278D', '7de32536eac89a3558b82ee98f963bda6e7c179fddb3eeed2e30e446686eced971bb5478d112a3da');
  TryKey(hkfS7_1500, '580B8A122D42D1C0', '6ca1bbad4697ed515424db6b897e9d0c33e2ead0bce5566f257c1bd1dc714bad41f14ed9536cfda2');
  TryKey(hkfS7_1500, '60CDAAA33E0B5D20', 'a21639ced1531ae99d8229a2d5b44c3c5243d73ea4a07099db98ef82c0fd7f5a21ffdce5adc77a3b');
  TryKey(hkfS7_1500, '65227E2580029B7F', 'c249c5135787859bc1e0f475798f134dec36a3df81cdc47d07d1f03060ab0311547f4ba090d7a487');
  TryKey(hkfS7_1500, '6BA412F7F1D965AA', '576516d41c3254e9bb392215cad08f23b3ffb6bc1c588edbb424c44397b068af316f34ad26bf13fc');
  TryKey(hkfS7_1500, '99E4632334CC7993', '3078a83781ca537d29476e26dccd3ab8fb707348c5af98054b4432c89f3601e4be66be574c875967');
  TryKey(hkfS7_1500, 'ACD68E9BF9901F8B', '4c7546b7f80fa1938f22db3e90df57a3cad9369afdbd7f51464b95cf62b8e7ce0aeb0cef1681110f');
  TryKey(hkfS7_1500, 'C4F47B876DA76D52', '67afc52ad2b7e8d81fd5d56b603ff64cb7973904775f5b0f1a8baa6e3b632274809e51a35e67aba0');
  TryKey(hkfS7_1500, 'D3F9CD55A57FE4EB', 'cea2b01f2ec599f21bd602edfd168291df355e1b127c4b1313c2d601ab0c0f4f0e65119996643084');
  TryKey(hkfS7_1500, 'E69E7A996524AFAC', 'f0ee32aa65f7169c09df3f75cea9bcdb8e1b0f0c90fac0ab19c3dfc1c08bb85ec60773cdef8f524e');

  //Family 1 - S7-1200 (40-byte keys)
  TryKey(hkfS7_1200, 'A95850575DF7B3DE', 'dead01d78404e753f0804d1038ebd16cef9788ccc3c78a972c978ce7efd3baf5002cb03e4fce1128');
  TryKey(hkfS7_1200, 'AC9BE476CB324E65', '6c71ed8fcc1ae94172040a14b8b1af7501b81e4cb99937491691075109300866557136c8ae848cad');
  TryKey(hkfS7_1200, 'BD426B091F08731A', 'e0e1f04a5ca3f90148178689bd0c930ab9db867b4f0ab109623959aa32316b7880ed1b4f9a9b189f');

  //Family 3 - PlcSim (64-byte keys)
  TryKey(hkfPLCSIM, '09013727CCBFBF3C', '2b5d9a1aec74c6cefa5fab75edd0ea202d5bff1341de87c1b6f066d291ba08714d6689ab229e147556eeac59a8a157f7b1e7c1d5c12e4eb03cb11a810757f644');
  TryKey(hkfPLCSIM, '4964E2F77A386F64', '5ede4c30e9bd71c9c416bc43aeda7ad4e8e64a19a061847e7f6f74524fe9aaf917a8c4476085dc0af4ac2f291cd83ce422f1409ee8c23a95524e3ec344caf0b6');
  TryKey(hkfPLCSIM, '5A9B6B015F48D284', 'eca6d799ddf03eaadd16b5d7245331e426c9e6ba8997877a7394f3286532a6b053e4229818085223432483fba4d5c43bd6c354c10febc903908ed271697f39e9');
  TryKey(hkfPLCSIM, '950840E428F7C7FA', '50f39e36c958ea5ec4982a62337c7f7319eb5c0ef501ac18a28416de86882e21640faf05304dc3b99016f2bc98fa732c2e48e7d3be380d11bf292cdf1691bc93');
  TryKey(hkfPLCSIM, 'AE5147B429BBA96C', 'ab7f65871080201263d7925c34a8224136ee01164efd5b79f3ca8e12b1a881c17e3264728c49c11b82a62e03b9f309f6ba054048fe9d5d33e2201b59dbefcc5a');
  TryKey(hkfPLCSIM, 'B07654AC9CAA4ACA', '4700db8fa25d791c2a77eec9795d66e3b5f2ba9a59508add510ca9fe8762aa081dff80ea8f730ad4caa0bca7ba92892c691984338eec2047681d958dc5c5086a');
  TryKey(hkfPLCSIM, 'CE331E08538A26B9', 'd2a736be8e86ad4d22a43a43fbc531cb1f30f8e35dbe4d934c8cbaeb66db96db2f1a52c59b81ff2845228e1487f02687da14229c0840b1d611cbce698eb5ece3');
  TryKey(hkfPLCSIM, 'D61B355B59D711A9', '166f0ae5b303d53bbadbb2a6268ecb1a4fcdec74f9a05f6596a6add71256b10867aee0214f05ea714f73c06ec0b71445b3eb39c8306f233756cf53653a12e924');
  TryKey(hkfPLCSIM, 'E90B455D3CC46013', '0fb15661f5953df692f3ce717f3a5b6cb70ff7da11ecdd8a27ff38958601292738f24a30c7381cf392ef2492863ee0b383ca816bd46b18e0e82a30b47a414d19');

  if Length(Result_)=0 then
    raise EUnknownPublicKeyError.CreateFmt('No public key for fingerprint %s', [Fingerprint]);
  Result := Result_;
end;

end.
