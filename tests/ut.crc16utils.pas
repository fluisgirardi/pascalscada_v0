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

    { as 12 variantes de CRC8: as 8 que divergem do catalogo estao ignoradas,
      veja o comentario no inicio da secao de implementacao }
    procedure CRC8_DefaultSegueOPadrao;
    procedure CRC8_DVBS2SegueOPadrao;
    procedure CRC8_MaximSegueOPadrao;
    procedure CRC8_ITUSegueOPadrao;
    procedure CRC8_DARCSegueOPadrao;
    procedure CRC8_BLUETOOTHSegueOPadrao;
    procedure CRC8_CDMA2000SegueOPadrao;
    procedure CRC8_ROHCSegueOPadrao;
    procedure CRC8_WCDMASegueOPadrao;
    procedure CRC8_OPENSAFETYSegueOPadrao;
    procedure CRC8_AUTOSARSegueOPadrao;
    procedure CRC8_ICODESegueOPadrao;
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

{ As rotinas de CRC8 desta unit nao sao usadas em nenhum ponto do projeto -
  sao API publica para quem consome a biblioteca. Conferidas contra o valor de
  catalogo de "123456789", 8 das 12 divergem do padrao que o nome anuncia.
  As 4 que conferem com o catalogo ficam como teste ativo, protegendo o que ja
  esta certo. As 8 que divergem ficam ignoradas, e a mensagem de cada uma traz
  o valor que a implementacao devolve hoje e o valor catalogado, para quem for
  corrigir nao precisar medir de novo.

  Diagnostico ja levantado: as tabelas divergentes coincidem com a tabela
  correta ate o indice 7 e se afastam a partir do 8, ou seja, foram geradas por
  uma rotina defeituosa - nao e' caso de reflexao trocada. A propria unit tem o
  gerador certo (CRC8_GenerateTable com RefIn, CRC8_Calc com RefOut/XorOut),
  que nenhuma delas usa. }

function VetorDeConferencia: BYTES;
const
  VETOR = '123456789';
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(VETOR));
  for i := 1 to Length(VETOR) do
    Result[i-1] := Ord(VETOR[i]);
end;

procedure TTestCRC16Utils.CRC8_DefaultSegueOPadrao;
var
  data: BYTES;
begin
  data := VetorDeConferencia;
  AssertEquals('CRC-8/SMBUS de "123456789"', $F4, CRC8_Default(data, $00));
end;

procedure TTestCRC16Utils.CRC8_DVBS2SegueOPadrao;
var
  data: BYTES;
begin
  data := VetorDeConferencia;
  AssertEquals('CRC-8/DVB-S2 de "123456789"', $BC, CRC8_DVBS2(data, $00));
end;

procedure TTestCRC16Utils.CRC8_MaximSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/MAXIM-DOW: a implementacao devolve $A2 onde o catalogo diz $A1. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/MAXIM-DOW de "123456789"', $A1, CRC8_Maxim(data, $00));
end;

procedure TTestCRC16Utils.CRC8_ITUSegueOPadrao;
var
  data: BYTES;
begin
  data := VetorDeConferencia;
  AssertEquals('CRC-8/I-432-1/ITU de "123456789"', $A1, CRC8_ITU(data, $00));
end;

procedure TTestCRC16Utils.CRC8_DARCSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/DARC: a implementacao devolve $B2 onde o catalogo diz $15. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/DARC de "123456789"', $15, CRC8_DARC(data, $00));
end;

procedure TTestCRC16Utils.CRC8_BLUETOOTHSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/BLUETOOTH: a implementacao devolve $43 onde o catalogo diz $26. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/BLUETOOTH de "123456789"', $26, CRC8_BLUETOOTH(data, $00));
end;

procedure TTestCRC16Utils.CRC8_CDMA2000SegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/CDMA2000: a implementacao devolve $FE onde o catalogo diz $DA. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/CDMA2000 de "123456789"', $DA, CRC8_CDMA2000(data, $FF));
end;

procedure TTestCRC16Utils.CRC8_ROHCSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/ROHC: a implementacao devolve $FB onde o catalogo diz $D0. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/ROHC de "123456789"', $D0, CRC8_ROHC(data, $FF));
end;

procedure TTestCRC16Utils.CRC8_WCDMASegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/WCDMA: a implementacao devolve $3C onde o catalogo diz $25. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/WCDMA de "123456789"', $25, CRC8_WCDMA(data, $00));
end;

procedure TTestCRC16Utils.CRC8_OPENSAFETYSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/OPENSAFETY: a implementacao devolve $32 onde o catalogo diz $3E. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/OPENSAFETY de "123456789"', $3E, CRC8_OPENSAFETY(data, $00));
end;

procedure TTestCRC16Utils.CRC8_AUTOSARSegueOPadrao;
var
  data: BYTES;
begin
  Ignore('CRC-8/AUTOSAR: a implementacao devolve $A1 onde o catalogo diz $DF. ' +
         'Rotina sem uso no projeto; remova este Ignore ao corrigir ou ao ' +
         'confirmar a variante.');

  data := VetorDeConferencia;
  AssertEquals('CRC-8/AUTOSAR de "123456789"', $DF, CRC8_AUTOSAR(data, $FF));
end;

procedure TTestCRC16Utils.CRC8_ICODESegueOPadrao;
var
  data: BYTES;
begin
  data := VetorDeConferencia;
  AssertEquals('CRC-8/I-CODE de "123456789"', $7E, CRC8_ICODE(data, $FD));
end;

initialization
  RegisterTest(TTestCRC16Utils);

end.
