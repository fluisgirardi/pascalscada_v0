unit ut.hsutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  hsutils;

type

  { TTestHsUtils }

  TTestHsUtils = class(TTestCase)
  published
    procedure ExplodeSeparaTodosOsCampos;
    procedure ExplodeRespeitaOLimite;
    procedure ExplodeSemDelimitadorRetornaAStringInteira;
    procedure PotenciaDeInteiros;
  end;

implementation

procedure TTestHsUtils.ExplodeSeparaTodosOsCampos;
var
  r: TStringArray;
begin
  r := ExplodeString('.', '192.168.0.1');
  AssertEquals('quantidade de campos', 4, Length(r));
  AssertEquals('192', r[0]);
  AssertEquals('1', r[3]);
end;

procedure TTestHsUtils.ExplodeRespeitaOLimite;
var
  r: TStringArray;
begin
  r := ExplodeString('/', 'a/b/c/d', 2);
  AssertEquals('quantidade de campos', 2, Length(r));
  AssertEquals('a', r[0]);
  AssertEquals('b/c/d', r[1]);
end;

procedure TTestHsUtils.ExplodeSemDelimitadorRetornaAStringInteira;
var
  r: TStringArray;
begin
  r := ExplodeString('', 'texto');
  AssertEquals(1, Length(r));
  AssertEquals('texto', r[0]);
end;

procedure TTestHsUtils.PotenciaDeInteiros;
begin
  AssertEquals('2^0',  1, Power(2, 0));
  AssertEquals('2^8',  256, Power(2, 8));
  AssertEquals('2^16', 65536, Power(2, 16));
  AssertEquals('10^3', 1000, Power(10, 3));
end;

initialization
  RegisterTest(TTestHsUtils);

end.
