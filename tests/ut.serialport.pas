{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TSerialPortDriver: configuracao da porta serial.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A porta serial e' a unica da biblioteca presa a um dispositivo do sistema
  operacional, e por isso a que menos se deixa exercitar. O que da' para medir
  sem hardware e' a metade de cima: os valores de fabrica, a normalizacao do
  diretorio de dispositivos, o que ela aceita como nome de porta e o que
  recusa. O caminho de abrir, ler e escrever fica de fora deste arquivo.
}
{$ELSE}
{:
  @abstract(TSerialPortDriver tests: serial port configuration.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The serial port is the only one in the library tied to an operating system
  device, and for that reason the hardest to exercise. What can be measured
  with no hardware is the upper half: the factory values, the device directory
  normalization, what it accepts as a port name and what it refuses. Opening,
  reading and writing are out of this file.
}
{$ENDIF}
unit ut.serialport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, CommPort, serialport;

type

  { TSerialProbe }

  TSerialProbe = class(TSerialPortDriver)
  public
    //: ComSettingsOK e' protegido: e' a porta concreta que se declara pronta
    function  ConfiguracaoAceitavel:Boolean;
    //: SetDesigning e' protegido em TComponent
    procedure MarcarComoEmProjeto;
  end;

  { TTestSerialPort }

  TTestSerialPort = class(TTestCase)
  private
    FPorta:TSerialProbe;
    //: o numero da porta fica nos quatro bytes baixos do identificador
    function  NumeroDoId(aId:TPortUniqueID):LongWord;
    //: e o tipo da porta, com a marca de incompleta, no byte mais alto
    function  ByteDeTipo(aId:TPortUniqueID):Byte;
    function  IdDe(const aNome:String):TPortUniqueID;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //valores de fabrica / factory values
    procedure VelocidadeEFormatoDeFabrica;
    procedure EsperaEntreEscritaELeituraDeFabrica;
    procedure TempoLimiteDeFabricaConfereComOQueAPropriedadeDeclara;

    //diretorio de dispositivos / device directory
    procedure DiretorioDeDispositivosDeFabrica;
    procedure DiretorioVazioVoltaAoPadrao;
    procedure DiretorioNenhumVoltaAoPadrao;
    procedure DiretorioEscolhidoEhGuardado;

    //nome da porta / port name
    procedure NomeDePortaDesconhecidoNaoEhAceito;
    procedure NomeDePortaDesconhecidoNaoApagaOAnterior;
    procedure AceitarQualquerNomeAbreMaoDaConferencia;
    procedure NomeVazioLimpaAPorta;

    //estado / state
    procedure PortaNovaEstaFechada;
    procedure PortaSerialEhDispositivoExclusivo;
    procedure SemNomeDePortaAConfiguracaoNaoServe;

    //identificacao / identification
    procedure PortasDiferentesTemIdentificadoresDiferentes;
    procedure OIdentificadorCarregaONumeroDaPorta;
    procedure MesmoNumeroComPrefixoDiferenteNaoSeConfunde;
    procedure PortaSemNomeEhMarcadaComoIncompleta;
    procedure NomeSemNumeroEhMarcadoComoIncompleto;
    procedure IdentificadorEhEstavelParaAMesmaPorta;
  end;

implementation

{ TSerialProbe }

function TSerialProbe.ConfiguracaoAceitavel:Boolean;
begin
  Result:=ComSettingsOK;
end;

procedure TSerialProbe.MarcarComoEmProjeto;
begin
  SetDesigning(true, false);
end;

{ TTestSerialPort }

procedure TTestSerialPort.SetUp;
begin
  FPorta:=TSerialProbe.Create(nil);
end;

procedure TTestSerialPort.TearDown;
begin
  FreeAndNil(FPorta);
end;

function TTestSerialPort.NumeroDoId(aId:TPortUniqueID):LongWord;
begin
  Result:=LongWord(QWord(aId) and $FFFFFFFF);
end;

function TTestSerialPort.ByteDeTipo(aId:TPortUniqueID):Byte;
begin
  Result:=(QWord(aId) shr 56) and $FF;
end;

function TTestSerialPort.IdDe(const aNome:String):TPortUniqueID;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:=aNome;
  Result:=FPorta.getPortId;
end;

procedure TTestSerialPort.VelocidadeEFormatoDeFabrica;
begin
  //19200 8N1, que e' o que a maioria dos equipamentos usa
  AssertEquals('velocidade',      Ord(br19200), Ord(FPorta.BaudRate));
  AssertEquals('bits de dados',   Ord(db8),     Ord(FPorta.DataBits));
  AssertEquals('paridade',        Ord(spNone),  Ord(FPorta.Paridade));
  AssertEquals('bits de parada',  Ord(sb1),     Ord(FPorta.StopBits));
end;

procedure TTestSerialPort.EsperaEntreEscritaELeituraDeFabrica;
begin
  AssertEquals('espera entre escrever e ler', 20, FPorta.WriteReadDelay);
end;

procedure TTestSerialPort.TempoLimiteDeFabricaConfereComOQueAPropriedadeDeclara;
begin
  //o valor declarado na propriedade e' o que o mecanismo de gravacao usa para
  //decidir o que precisa ir para o arquivo de formulario: o que for igual a
  //ele nao e' gravado. Se o construtor comeca com outro valor, quem escolher
  //justamente o valor declarado perde a escolha na proxima abertura
  AssertEquals('tempo limite', 100, FPorta.Timeout);
end;

procedure TTestSerialPort.DiretorioDeDispositivosDeFabrica;
begin
  AssertEquals('diretorio de dispositivos', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.DiretorioVazioVoltaAoPadrao;
begin
  FPorta.DevDir:='/tmp/';
  FPorta.DevDir:='';
  AssertEquals('vazio volta ao padrao', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.DiretorioNenhumVoltaAoPadrao;
begin
  //"(none)" e' o que o editor de propriedades entrega quando nada foi escolhido
  FPorta.DevDir:='/tmp/';
  FPorta.DevDir:='(none)';
  AssertEquals('nenhum volta ao padrao', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.DiretorioEscolhidoEhGuardado;
begin
  //existe para quem usa emulador de porta serial fora de /dev
  FPorta.DevDir:='/tmp/portas/';
  AssertEquals('diretorio escolhido', '/tmp/portas/', FPorta.DevDir);
end;

procedure TTestSerialPort.NomeDePortaDesconhecidoNaoEhAceito;
begin
  FPorta.COMPort:='naoexisteestaporta';
  AssertEquals('nome desconhecido nao entra', '', FPorta.COMPort);
end;

procedure TTestSerialPort.NomeDePortaDesconhecidoNaoApagaOAnterior;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='ttyS0';
  FPorta.AcceptAnyPortName:=false;

  FPorta.COMPort:='naoexisteestaporta';
  AssertEquals('o nome anterior fica de pe', 'ttyS0', FPorta.COMPort);
end;

procedure TTestSerialPort.AceitarQualquerNomeAbreMaoDaConferencia;
begin
  //e' assim que se usa um emulador de porta, ou um dispositivo com nome fora
  //dos prefixos que a unit conhece
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='umnomequalquer';

  AssertEquals('nome aceito sem conferencia', 'umnomequalquer', FPorta.COMPort);
end;

procedure TTestSerialPort.NomeVazioLimpaAPorta;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='ttyS0';
  FPorta.AcceptAnyPortName:=false;

  FPorta.COMPort:='';
  AssertEquals('nome vazio limpa', '', FPorta.COMPort);
end;

procedure TTestSerialPort.PortaNovaEstaFechada;
begin
  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestSerialPort.PortaSerialEhDispositivoExclusivo;
begin
  //dispositivo exclusivo nao e' aberto em tempo de projeto: abrir tomaria o
  //equipamento de quem esta' rodando
  FPorta.MarcarComoEmProjeto;
  FPorta.Active:=true;

  AssertFalse('em projeto, nao abre de fato', FPorta.ReallyActive);
end;

procedure TTestSerialPort.SemNomeDePortaAConfiguracaoNaoServe;
begin
  AssertFalse('sem nome de porta nao ha o que abrir', FPorta.ConfiguracaoAceitavel);
end;

procedure TTestSerialPort.PortasDiferentesTemIdentificadoresDiferentes;
var
  outra:TSerialProbe;
begin
  //o identificador separa os dados de cada porta em quem indexa por ele - o
  //LGXDriver usa isso como chave do mapa de CLPs
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='ttyS0';

  outra:=TSerialProbe.Create(nil);
  try
    outra.AcceptAnyPortName:=true;
    outra.COMPort:='ttyS1';

    AssertTrue('duas portas seriais distintas', FPorta.getPortId<>outra.getPortId);
  finally
    outra.Free;
  end;
end;

procedure TTestSerialPort.OIdentificadorCarregaONumeroDaPorta;
begin
  //o numero da porta e' o que vem depois das letras: COM1 da' 1, ttyUSB0 da' 0
  AssertEquals('COM1',     1,  NumeroDoId(IdDe('COM1')));
  AssertEquals('COM2',     2,  NumeroDoId(IdDe('COM2')));
  AssertEquals('COM12',    12, NumeroDoId(IdDe('COM12')));
  AssertEquals('ttyUSB0',  0,  NumeroDoId(IdDe('ttyUSB0')));
  AssertEquals('ttyS3',    3,  NumeroDoId(IdDe('ttyS3')));
  AssertEquals('cuau1',    1,  NumeroDoId(IdDe('cuau1')));

  AssertEquals('e a marca de porta serial', 1, ByteDeTipo(IdDe('COM1')));
end;

procedure TTestSerialPort.MesmoNumeroComPrefixoDiferenteNaoSeConfunde;
var
  umUsb:TPortUniqueID;
begin
  //ttyS0 e ttyUSB0 sao duas portas, com o mesmo numero: o identificador tem
  //que separa-las, senao quem indexa por ele mistura as duas
  umUsb:=IdDe('ttyUSB0');

  AssertTrue('ttyUSB0 e ttyS0',   umUsb<>IdDe('ttyS0'));
  AssertTrue('ttyUSB0 e ttyADV0', umUsb<>IdDe('ttyADV0'));
end;

procedure TTestSerialPort.PortaSemNomeEhMarcadaComoIncompleta;
begin
  //sem nome de porta o identificador leva o bit alto ligado, como na porta de
  //rede sem endereco
  AssertEquals('serial com a marca de incompleta', $81, ByteDeTipo(FPorta.getPortId));
end;

procedure TTestSerialPort.NomeSemNumeroEhMarcadoComoIncompleto;
begin
  //um nome sem digito nenhum nao identifica porta alguma
  AssertEquals('nome sem numero', $81, ByteDeTipo(IdDe('umnomesemnumero')));
end;

procedure TTestSerialPort.IdentificadorEhEstavelParaAMesmaPorta;
var
  primeiro:TPortUniqueID;
begin
  //e' chave de mapa: ler duas vezes tem que dar o mesmo valor
  primeiro:=IdDe('ttyUSB0');
  AssertTrue('mesma leitura',      primeiro=FPorta.getPortId);
  AssertTrue('mesma configuracao', primeiro=IdDe('ttyUSB0'));
end;

initialization
  RegisterTest(TTestSerialPort);

end.
