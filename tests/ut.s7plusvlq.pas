{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do S7PlusVLQ: inteiros de tamanho variavel do S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' a base de todo o resto do protocolo - um erro de um byte aqui desalinha
  a mensagem inteira dali pra frente, e o sintoma aparece longe da causa.
  Por isso os valores de fronteira (127/128, 16383/16384, o maior uint32) tem
  o frame esperado escrito literalmente, e nao so ida e volta.
}
{$ELSE}
{:
  @abstract(S7PlusVLQ tests: the S7CommPlus variable length integers.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the base of everything else in the protocol - a one byte mistake
  here misaligns the whole message from that point on, and the symptom shows
  up far from the cause. That is why the boundary values (127/128,
  16383/16384, the largest uint32) have their expected frame spelled out
  literally, not just a round trip.
}
{$ENDIF}
unit ut.s7plusvlq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  S7PlusVLQ,
  testsupport.bytes;

type

  { TTestS7PlusVLQ }

  TTestS7PlusVLQ = class(TTestCase)
  published
    //uint32
    procedure ZeroFitsInOneByte;
    procedure OneHundredAndTwentySevenIsTheLargestInOneByte;
    procedure OneHundredAndTwentyEightMovesToTwoBytes;
    procedure ThreeHundredFollowsBase128BigEndian;
    procedure SixteenThousandThreeHundredAndEightyThreeIsTheLargestInTwoBytes;
    procedure TheLargestUInt32UsesFiveBytes;
    procedure UInt32GoesOutAndComesBack;
    procedure DecodeReportsHowManyBytesItConsumed;
    procedure DecodeWithNotEnoughDataComplains;

    //int32 / int64 / uint64
    procedure Int32GoesOutAndComesBackNegativesIncluded;
    procedure UInt64GoesOutAndComesBack;
    procedure Int64GoesOutAndComesBackNegativesIncluded;
  end;

implementation

procedure TTestS7PlusVLQ.ZeroFitsInOneByte;
begin
  AssertBytesEqual('zero', BytesOf('00'), EncodeUInt32VLQ(0));
end;

procedure TTestS7PlusVLQ.OneHundredAndTwentySevenIsTheLargestInOneByte;
begin
  AssertBytesEqual('127', BytesOf('7F'), EncodeUInt32VLQ(127));
end;

procedure TTestS7PlusVLQ.OneHundredAndTwentyEightMovesToTwoBytes;
begin
  //o bit $80 do primeiro byte diz "tem mais"
  AssertBytesEqual('128', BytesOf('81 00'), EncodeUInt32VLQ(128));
end;

procedure TTestS7PlusVLQ.ThreeHundredFollowsBase128BigEndian;
begin
  //300 = 10 0101100 -> grupo alto 2, grupo baixo $2C
  AssertBytesEqual('300', BytesOf('82 2C'), EncodeUInt32VLQ(300));
end;

procedure TTestS7PlusVLQ.SixteenThousandThreeHundredAndEightyThreeIsTheLargestInTwoBytes;
begin
  AssertBytesEqual('16383', BytesOf('FF 7F'), EncodeUInt32VLQ(16383));
  AssertBytesEqual('16384', BytesOf('81 80 00'), EncodeUInt32VLQ(16384));
end;

procedure TTestS7PlusVLQ.TheLargestUInt32UsesFiveBytes;
begin
  //32 bits nao cabem em 4 grupos de 7, entao o maior valor gasta 5 bytes
  AssertBytesEqual('$FFFFFFFF', BytesOf('8F FF FF FF 7F'), EncodeUInt32VLQ($FFFFFFFF));
end;

procedure TTestS7PlusVLQ.UInt32GoesOutAndComesBack;
const
  VALORES:array[0..8] of Cardinal = (0, 1, 127, 128, 300, 16383, 16384, 2097152, $FFFFFFFF);
var
  i, consumido:Integer;
begin
  for i:=0 to High(VALORES) do
    AssertEquals('round trip of '+IntToStr(VALORES[i]),
                 Int64(VALORES[i]),
                 Int64(DecodeUInt32VLQ(EncodeUInt32VLQ(VALORES[i]), 0, consumido)));
end;

procedure TTestS7PlusVLQ.DecodeReportsHowManyBytesItConsumed;
var
  consumido:Integer;
begin
  //o decodificador precisa dizer onde termina o valor: e' assim que quem
  //chama anda pelo resto da mensagem.
  DecodeUInt32VLQ(BytesOf('82 2C FF FF'), 0, consumido);
  AssertEquals('bytes consumed', 2, consumido);

  DecodeUInt32VLQ(BytesOf('7F FF FF'), 0, consumido);
  AssertEquals('value in a single byte', 1, consumido);
end;

procedure TTestS7PlusVLQ.DecodeWithNotEnoughDataComplains;
var
  consumido:Integer;
  reclamou:Boolean;
begin
  //todos os bytes pedem continuacao e o dado acaba: nao pode devolver lixo
  reclamou:=false;
  try
    DecodeUInt32VLQ(BytesOf('82'), 0, consumido);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('a truncated VLQ must raise an exception', reclamou);
end;

procedure TTestS7PlusVLQ.Int32GoesOutAndComesBackNegativesIncluded;
const
  VALORES:array[0..9] of LongInt = (0, 1, -1, 63, -64, 64, 1000, -1000, 2147483647, -2147483647);
var
  i, consumido:Integer;
begin
  for i:=0 to High(VALORES) do
    AssertEquals('round trip of '+IntToStr(VALORES[i]),
                 Int64(VALORES[i]),
                 Int64(DecodeInt32VLQ(EncodeInt32VLQ(VALORES[i]), 0, consumido)));
end;

procedure TTestS7PlusVLQ.UInt64GoesOutAndComesBack;
const
  VALORES:array[0..5] of QWord = (0, 1, 127, 128, QWord(4294967296), QWord($FFFFFFFFFFFFFFFF));
var
  i, consumido:Integer;
begin
  for i:=0 to High(VALORES) do
    AssertEquals('round trip of '+IntToStr(VALORES[i]),
                 VALORES[i],
                 DecodeUInt64VLQ(EncodeUInt64VLQ(VALORES[i]), 0, consumido));
end;

procedure TTestS7PlusVLQ.Int64GoesOutAndComesBackNegativesIncluded;
const
  VALORES:array[0..7] of Int64 = (0, 1, -1, 63, -64, 1000000000000, -1000000000000, -9223372036854775807);
var
  i, consumido:Integer;
begin
  for i:=0 to High(VALORES) do
    AssertEquals('round trip of '+IntToStr(VALORES[i]),
                 VALORES[i],
                 DecodeInt64VLQ(EncodeInt64VLQ(VALORES[i]), 0, consumido));
end;

initialization
  RegisterTest(TTestS7PlusVLQ);

end.
