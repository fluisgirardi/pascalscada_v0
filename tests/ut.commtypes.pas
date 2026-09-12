{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do commtypes: a juncao de buffers de bytes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A unit e' quase toda declaracao de tipo; a unica funcao e' o
  ConcatenateBYTES, que o s7family usa para emendar as duas etapas de uma
  leitura (cabecalho e resto do pacote). Um erro de um byte ali corrompe todo
  valor lido, e o sintoma aparece longe da causa.
}
{$ELSE}
{:
  @abstract(commtypes tests: joining byte buffers.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The unit is almost all type declarations; its only function is
  ConcatenateBYTES, which s7family uses to splice the two stages of a read
  (header and the rest of the packet). A one byte mistake there corrupts every
  value read, and the symptom shows up far from the cause.
}
{$ENDIF}
unit ut.commtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes,
  testsupport.bytes;

type

  { TTestCommTypes }

  TTestCommTypes = class(TTestCase)
  published
    procedure JuncaoPreservaAOrdemDosDois;
    procedure JuncaoComOPrimeiroVazioDevolveOSegundo;
    procedure JuncaoComOSegundoVazioDevolveOPrimeiro;
    procedure JuncaoDeDoisVaziosDaVazio;
    procedure TamanhoEhASomaDosDois;
    procedure ResultadoNaoCompartilhaMemoriaComAsFontes;
  end;

implementation

procedure TTestCommTypes.JuncaoPreservaAOrdemDosDois;
begin
  //o segundo pedaco entra depois do primeiro, sem embaralhar
  AssertBytesEqual('juncao', BytesOf('01 02 03 AA BB'),
                   ConcatenateBYTES(BytesOf('01 02 03'), BytesOf('AA BB')));
end;

procedure TTestCommTypes.JuncaoComOPrimeiroVazioDevolveOSegundo;
begin
  //acontece de verdade: a primeira etapa da leitura pode voltar vazia
  AssertBytesEqual('primeiro vazio', BytesOf('AA BB'),
                   ConcatenateBYTES(nil, BytesOf('AA BB')));
end;

procedure TTestCommTypes.JuncaoComOSegundoVazioDevolveOPrimeiro;
begin
  //e a segunda tambem, quando nao falta nada para ler
  AssertBytesEqual('segundo vazio', BytesOf('01 02 03'),
                   ConcatenateBYTES(BytesOf('01 02 03'), nil));
end;

procedure TTestCommTypes.JuncaoDeDoisVaziosDaVazio;
begin
  AssertEquals('nada com nada', 0, Length(ConcatenateBYTES(nil, nil)));
end;

procedure TTestCommTypes.TamanhoEhASomaDosDois;
begin
  AssertEquals('3 + 2', 5, Length(ConcatenateBYTES(BytesOf('01 02 03'), BytesOf('AA BB'))));
  AssertEquals('0 + 2', 2, Length(ConcatenateBYTES(nil, BytesOf('AA BB'))));
  AssertEquals('3 + 0', 3, Length(ConcatenateBYTES(BytesOf('01 02 03'), nil)));
end;

procedure TTestCommTypes.ResultadoNaoCompartilhaMemoriaComAsFontes;
var
  primeiro, segundo, juntos:BYTES;
begin
  //vetores dinamicos sao referencias: se a juncao devolvesse uma delas em vez
  //de copiar, mexer no resultado estragaria o buffer de quem chamou.
  primeiro:=BytesOf('01 02');
  segundo :=BytesOf('AA BB');
  juntos  :=ConcatenateBYTES(primeiro, segundo);

  juntos[0]:=$FF;
  juntos[3]:=$FF;

  AssertBytesEqual('a primeira fonte fica intacta', BytesOf('01 02'), primeiro);
  AssertBytesEqual('a segunda fonte fica intacta',  BytesOf('AA BB'), segundo);
end;

initialization
  RegisterTest(TTestCommTypes);

end.
