unit ut.crc16utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, crc16utils;

type

  { TTestCRC16Utils }

  TTestCRC16Utils = class(TTestCase)
  published
    procedure CalculaCRCDeUmaLeituraModbus;
    procedure CRCFechaComOProprioCalculo;
    procedure CRCIndependeDaArquitetura;
    procedure CRC8MaximSegueOPadrao;
  end;

implementation

{ Frame Modbus RTU: escravo 1, funcao 3, endereco 0, 10 registradores.
  CRC esperado (padrao Modbus): $CDC5, transmitido byte baixo primeiro. }
function FrameLeitura: BYTES;
begin
  Result := nil;
  SetLength(Result, 8);
  Result[0] := $01; Result[1] := $03;
  Result[2] := $00; Result[3] := $00;
  Result[4] := $00; Result[5] := $0A;
  Result[6] := $00; Result[7] := $00;
end;

procedure TTestCRC16Utils.CalculaCRCDeUmaLeituraModbus;
var
  pkg: BYTES;
  crc: Cardinal;
begin
  pkg := FrameLeitura;
  crc := Calcul_crc(pkg);
  AssertEquals('valor do CRC16', $CDC5, crc);
  AssertEquals('byte baixo do CRC', $C5, pkg[6]);
  AssertEquals('byte alto do CRC',  $CD, pkg[7]);
end;

procedure TTestCRC16Utils.CRCFechaComOProprioCalculo;
var
  pkg: BYTES;
begin
  pkg := FrameLeitura;
  Calcul_crc(pkg);
  AssertTrue('Test_crc deve aceitar um frame com CRC valido', Test_crc(pkg));
end;

procedure TTestCRC16Utils.CRCIndependeDaArquitetura;
var
  pkg: BYTES;
  i: Integer;
  crc: Cardinal;
begin
  pkg := nil;
  SetLength(pkg, 258);
  for i := 0 to 255 do
    pkg[i] := i;
  pkg[256] := 0; pkg[257] := 0;
  crc := Calcul_crc(pkg);
  { valor fixo: se mudar entre 32 e 64 bits, ha problema de tipo/shift }
  AssertEquals('CRC de 0..255', $DE6C, crc);
end;

procedure TTestCRC16Utils.CRC8MaximSegueOPadrao;
var
  data: BYTES;
  i: Integer;
const
  VETOR = '123456789';
begin
  data := nil;
  SetLength(data, Length(VETOR));
  for i := 1 to Length(VETOR) do
    data[i-1] := Ord(VETOR[i]);

  { valor de conferencia catalogado para CRC-8/MAXIM (poly $31 refletido) }
  AssertEquals('CRC-8/MAXIM de "123456789"', $A1, CRC8_Maxim(data, 0));

  { a tabela nao refletida dava $A2 aqui - e o que este teste impede de voltar }
  AssertFalse('nao pode ser o resultado da tabela nao refletida',
              CRC8_Maxim(data, 0)=$A2);
end;

initialization
  RegisterTest(TTestCRC16Utils);

end.
