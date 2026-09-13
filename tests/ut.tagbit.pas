{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TTagBit: recorte de bits de um tag numerico.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O tag de bits nao fala com CLP nenhum: ele se pendura num outro tag numerico
  e mostra um pedaco dele. StartBit e' o bit menos significativo do recorte,
  EndBit o mais significativo, e o valor sai deslocado para a direita - o bit
  3 sozinho vale 1, nao 8. Escrever faz o caminho de volta: monta o valor no
  lugar certo e devolve ao tag de origem sem tocar no resto dos bits.

  O tag de origem aqui e' um TFakeNumber, que so' guarda um valor e avisa
  quando ele muda.
}
{$ELSE}
{:
  @abstract(TTagBit tests: slicing bits out of a numeric tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The bit tag talks to no PLC: it hangs on another numeric tag and shows a
  slice of it. StartBit is the least significant bit of the slice, EndBit the
  most significant, and the value comes out shifted right - bit 3 alone is
  worth 1, not 8. Writing goes the other way: it puts the value back in place
  and hands it to the source tag without touching the other bits.

  The source tag here is a TFakeNumber, which only holds a value and tells
  when it changes.
}
{$ENDIF}
unit ut.tagbit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ProtocolTypes, Tag, TagBit,
  testsupport.faketag;

type

  { TTestTagBit }

  TTestTagBit = class(TTestCase)
  private
    FOrigem:TFakeNumber;
    FBit:TTagBit;
    FAvisos:LongInt;
    procedure CountNotification(Sender:TObject);
    //: prepara o recorte e poe um valor na origem
    procedure SliceOf(aInicio, aFim:LongInt; aValorDaOrigem:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //leitura / reading
    procedure ASingleBitComesOutAsZeroOrOne;
    procedure ABitRangeComesOutShiftedRight;
    procedure ExamplesFromTheDocumentation;
    procedure TheHighBitsOfTheSixtyFourBitWord;

    //escrita / writing
    procedure AWriteGoesBackToTheSourceTag;
    procedure AWriteDoesNotTouchTheOtherBits;
    procedure ARangeWriteGoesInShifted;
    procedure AWriteBiggerThanTheRangeDoesNotOverflow;

    //sem tag de origem / with no source tag
    procedure WithNoSourceItKeepsTheValueLocally;
    procedure WithNoSourceTheReadStatusSaysThereIsNoBlock;
    procedure WithNoSourceTheAsyncWriteStatusSaysThereIsNoBlock;

    //avisos de mudanca / change notifications
    procedure AChangeInTheWatchedBitsNotifies;
    procedure AChangeOutsideTheWatchedBitsDoesNotNotify;
    procedure LinkingTheSourceUpdatesTheValueKept;
    procedure ADestroyedSourceBreaksTheLink;

    //comandos de leitura e escrita / read and write commands
    procedure AReadCommandGoesToTheSource;
    procedure WithNoSourceTheCommandsDoNothing;
    procedure AWriteCommandGoesToTheSource;
    procedure AScanWriteCommandGoesToTheSource;
  end;

implementation

procedure TTestTagBit.SetUp;
begin
  FOrigem:=TFakeNumber.Create(nil);
  FBit:=TTagBit.Create(nil);
  FAvisos:=0;
end;

procedure TTestTagBit.TearDown;
begin
  FreeAndNil(FBit);
  FreeAndNil(FOrigem);
end;

procedure TTestTagBit.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestTagBit.SliceOf(aInicio, aFim:LongInt; aValorDaOrigem:Double);
begin
  //o fim primeiro: a mascara e' refeita a cada atribuicao, e uma faixa com o
  //inicio depois do fim nao cobre bit nenhum
  FBit.EndBit  :=aFim;
  FBit.StartBit:=aInicio;
  FOrigem.ChegouDoCLP(aValorDaOrigem);
  FBit.PLCTag  :=FOrigem;
end;

procedure TTestTagBit.ASingleBitComesOutAsZeroOrOne;
begin
  //5 = 101: os bits 0 e 2 estao ligados, o 1 nao
  SliceOf(0, 0, 5);
  AssertEquals('bit 0 of 5', 1, FBit.Value, 0);

  SliceOf(1, 1, 5);
  AssertEquals('bit 1 of 5', 0, FBit.Value, 0);

  SliceOf(2, 2, 5);
  AssertEquals('bit 2 of 5', 1, FBit.Value, 0);
end;

procedure TTestTagBit.ABitRangeComesOutShiftedRight;
begin
  //o valor $F0 = 11110000: a faixa 4..7 vale 15, nao 240
  SliceOf(4, 7, $F0);
  AssertEquals('high nibble of $F0', 15, FBit.Value, 0);

  //e a faixa 8..15 do valor $1234 e' $12
  SliceOf(8, 15, $1234);
  AssertEquals('high byte of $1234', $12, FBit.Value, 0);
end;

procedure TTestTagBit.ExamplesFromTheDocumentation;
begin
  //os tres exemplos escritos no cabecalho da unit, com o tag valendo 5
  SliceOf(1, 2, 5);
  AssertEquals('1..2 of 5', 2, FBit.Value, 0);

  SliceOf(0, 2, 5);
  AssertEquals('0..2 of 5', 5, FBit.Value, 0);

  SliceOf(0, 1, 5);
  AssertEquals('0..1 of 5', 1, FBit.Value, 0);
end;

procedure TTestTagBit.TheHighBitsOfTheSixtyFourBitWord;
begin
  //a mascara e' de 64 bits: o bit 32 tem que ser alcancavel
  SliceOf(32, 32, 4294967296.0);
  AssertEquals('bit 32', 1, FBit.Value, 0);

  SliceOf(32, 35, 64424509440.0);   //$F00000000
  AssertEquals('range 32..35', 15, FBit.Value, 0);
end;

procedure TTestTagBit.AWriteGoesBackToTheSourceTag;
begin
  //ligar o bit 1 de um valor 5 da' 7
  SliceOf(1, 1, 5);
  FBit.Value:=1;
  AssertEquals('source after setting bit 1', 7, FOrigem.Value, 0);

  //e desligar devolve o 5
  FBit.Value:=0;
  AssertEquals('source after clearing it', 5, FOrigem.Value, 0);
end;

procedure TTestTagBit.AWriteDoesNotTouchTheOtherBits;
begin
  //o valor $FF com o bit 3 desligado vira $F7
  SliceOf(3, 3, $FF);
  FBit.Value:=0;
  AssertEquals('only bit 3 changed', $F7, FOrigem.Value, 0);
end;

procedure TTestTagBit.ARangeWriteGoesInShifted;
begin
  //escrever 5 na faixa 4..7 de um valor zerado poe $50
  SliceOf(4, 7, 0);
  FBit.Value:=5;
  AssertEquals('5 shifted into the high nibble', $50, FOrigem.Value, 0);
end;

procedure TTestTagBit.AWriteBiggerThanTheRangeDoesNotOverflow;
begin
  //a faixa 0..1 so' guarda dois bits: escrever 7 nao pode vazar para o bit 2
  SliceOf(0, 1, 0);
  FBit.Value:=7;
  AssertEquals('what does not fit is left out', 3, FOrigem.Value, 0);
end;

procedure TTestTagBit.WithNoSourceItKeepsTheValueLocally;
begin
  //sem tag de origem o tag de bits ainda e' um tag: guarda o que escrevem
  FBit.StartBit:=0;
  FBit.EndBit  :=3;
  FBit.Value   :=9;
  AssertEquals('value kept', 9, FBit.Value, 0);
end;

procedure TTestTagBit.WithNoSourceTheReadStatusSaysThereIsNoBlock;
begin
  AssertEquals('asynchronous read',  Ord(ioNullTagBlock), Ord(FBit.LastASyncReadStatus));
  AssertEquals('synchronous read',    Ord(ioNullTagBlock), Ord(FBit.LastSyncReadStatus));
  AssertEquals('synchronous write',    Ord(ioNullTagBlock), Ord(FBit.LastSyncWriteStatus));
end;

procedure TTestTagBit.WithNoSourceTheAsyncWriteStatusSaysThereIsNoBlock;
begin
  AssertEquals('asynchronous write', Ord(ioNullTagBlock), Ord(FBit.LastASyncWriteStatus));
end;

procedure TTestTagBit.AChangeInTheWatchedBitsNotifies;
begin
  SliceOf(1, 1, 5);
  FBit.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  //5 -> 7 liga o bit 1
  FOrigem.ChegouDoCLP(7);
  AssertTrue('a change in the watched bit must notify', FAvisos>0);
end;

procedure TTestTagBit.AChangeOutsideTheWatchedBitsDoesNotNotify;
begin
  SliceOf(1, 1, 5);
  FBit.AddTagChangeHandler(@CountNotification);

  //a primeira atualizacao sempre avisa; o que interessa aqui e' a seguinte
  FOrigem.ChegouDoCLP(5);
  FAvisos:=0;

  //5 -> 13: mudou o bit 3, o bit 1 continua desligado
  FOrigem.ChegouDoCLP(13);
  AssertEquals('the watched bit did not change', 0, FAvisos);
end;

procedure TTestTagBit.LinkingTheSourceUpdatesTheValueKept;
begin
  FBit.EndBit  :=0;
  FBit.StartBit:=0;
  FOrigem.ChegouDoCLP(5);

  FBit.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;
  FBit.PLCTag:=FOrigem;

  AssertTrue('linking the source must notify the listener', FAvisos>0);
end;

procedure TTestTagBit.ADestroyedSourceBreaksTheLink;
var
  origem:TFakeNumber;
begin
  origem:=TFakeNumber.Create(nil);
  origem.ChegouDoCLP(5);
  FBit.EndBit  :=0;
  FBit.StartBit:=0;
  FBit.PLCTag  :=origem;
  AssertEquals('reading from the source', 1, FBit.Value, 0);

  FreeAndNil(origem);

  //o tag de bits tem que ter largado o vinculo, nao guardado um ponteiro morto
  AssertTrue('the link must have been broken', FBit.PLCTag=nil);
end;

procedure TTestTagBit.AReadCommandGoesToTheSource;
begin
  //quem manda ler um tag de bits esta' mandando ler o tag de origem: e' o
  //unico dos dois que fala com o equipamento
  SliceOf(1, 1, 5);

  FBit.Read;
  AssertEquals('the source got the read request', 1, FOrigem.Leituras);
end;

procedure TTestTagBit.WithNoSourceTheCommandsDoNothing;
var
  valores:TArrayOfDouble;
begin
  //sem origem nao ha para onde mandar, mas tambem nao pode estourar
  SetLength(valores, 1);
  valores[0]:=1;

  FBit.Read;
  FBit.Write(valores, 1, 0);
  AssertEquals('scan with no source', -1, FBit.ScanRead);
  AssertEquals('scan write with no source', -1, FBit.ScanWrite(valores, 1, 0));
end;

procedure TTestTagBit.AWriteCommandGoesToTheSource;
begin
  //Write sem parametros e' o que a interface grafica chama para mandar o valor
  //ao equipamento. Em TPLCNumber ele repassa para Write(valores,1,0), que o
  //TTagBit nao implementa - o TPLCBlockElement, que e' o mesmo arranjo para
  //tags de bloco, repassa Read, Write, ScanRead e ScanWrite ao tag pai
  SliceOf(1, 1, 5);
  FBit.Value:=1;

  FBit.Write;
  AssertEquals('the source got the write', 7, FOrigem.Value, 0);
end;

procedure TTestTagBit.AScanWriteCommandGoesToTheSource;
begin
  SliceOf(1, 1, 5);
  FBit.Value:=1;

  FBit.ScanWrite;
  AssertEquals('the source got the scan write', 7, FOrigem.Value, 0);
end;

initialization
  RegisterTest(TTestTagBit);

end.
