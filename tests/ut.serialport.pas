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
  Classes, SysUtils, fpcunit, testregistry, CommPort, commtypes, serialport,
  testsupport.bytes, testsupport.fakeserial{$IFDEF UNIX}, termio{$ENDIF};

type

  { TSerialProbe }

  TSerialProbe = class(TSerialPortDriver)
  public
    //: ComSettingsOK e' protegido: e' a porta concreta que se declara pronta
    function  SettingsAreAcceptable:Boolean;
    //: SetDesigning e' protegido em TComponent
    procedure MarkAsDesignTime;
  end;

  { TTestSerialPort }

  TTestSerialPort = class(TTestCase)
  private
    FPorta:TSerialProbe;
    //: o numero da porta fica nos quatro bytes baixos do identificador
    function  NumberOfTheId(aId:TPortUniqueID):LongWord;
    //: e o tipo da porta, com a marca de incompleta, no byte mais alto
    function  TypeByteOf(aId:TPortUniqueID):Byte;
    function  IdOf(const aNome:String):TPortUniqueID;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //valores de fabrica / factory values
    procedure SpeedAndFormatOutOfTheBox;
    procedure DelayBetweenWriteAndReadOutOfTheBox;
    procedure TheDefaultTimeoutAgreesWithWhatThePropertyDeclares;

    {$IFDEF UNIX}
    //diretorio de dispositivos / device directory
    //
    //So' existe em Unix: no Windows a porta se chama COM1, sem diretorio, e o
    //construtor nem inicializa o prefixo - a propria documentacao da
    //propriedade diz que ali ela nao serve para nada.
    //Unix only: on Windows the port is called COM1, with no directory, and the
    //constructor does not even initialize the prefix - the property's own
    //documentation says it is useless there.
    procedure TheDeviceDirectoryOutOfTheBox;
    procedure AnEmptyDirectoryGoesBackToTheDefault;
    procedure NoDirectoryAtAllGoesBackToTheDefault;
    procedure TheChosenDirectoryIsKept;
    {$ENDIF}

    //nome da porta / port name
    procedure AnUnknownPortNameIsNotAccepted;
    procedure AnUnknownPortNameDoesNotEraseThePreviousOne;
    procedure AcceptingAnyNameGivesUpTheCheck;
    procedure AnEmptyNameClearsThePort;

    //estado / state
    procedure ANewPortIsClosed;
    procedure ASerialPortIsAnExclusiveDevice;
    procedure WithNoPortNameTheSettingsAreNotGoodEnough;

    //identificacao / identification
    procedure DifferentPortsHaveDifferentIdentifiers;
    procedure TheIdentifierCarriesThePortNumber;
    procedure TheSameNumberWithADifferentPrefixIsNotConfused;
    procedure APortWithNoNameIsMarkedIncomplete;
    procedure ANameWithNoNumberIsMarkedIncomplete;
    procedure TheIdentifierIsStableForTheSamePort;
  end;

  {$IFDEF UNIX}
  {$IFDEF PORTUGUES}
  {:
  A mesma porta, agora aberta de verdade sobre um par de pseudo-terminais. E'
  a unica forma de exercitar o que so' existe quando ha' um dispositivo do
  outro lado: abrir, escrever, ler e fechar.
  }
  {$ELSE}
  {:
  The same port, now really opened over a pseudo terminal pair. It is the only
  way to exercise what only exists when there is a device on the other side:
  opening, writing, reading and closing.
  }
  {$ENDIF}

  { TTestSerialPortOverADevice }

  TTestSerialPortOverADevice = class(TTestCase)
  private
    FDispositivo:TSerialDeMentira;
    FPorta:TSerialProbe;
    procedure PointThePortAtTheDevice;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OpeningThePortOverTheDevice;
    procedure WhatTheDriverWritesReachesTheDevice;
    procedure WhatTheDeviceSendsIsReadByTheDriver;
    procedure WithNoAnswerTheResultIsTimeout;
    procedure ClosingThePortStopsItBeingActive;

    //a configuracao chegando ao dispositivo / settings reaching the device
    procedure TheChosenSpeedReachesTheDevice;
    procedure ChangingTheSpeedChangesTheDevice;
    procedure TheChosenStopBitsReachTheDevice;
  end;
  {$ENDIF}

implementation

{ TSerialProbe }

function TSerialProbe.SettingsAreAcceptable:Boolean;
begin
  Result:=ComSettingsOK;
end;

procedure TSerialProbe.MarkAsDesignTime;
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

function TTestSerialPort.NumberOfTheId(aId:TPortUniqueID):LongWord;
begin
  Result:=LongWord(QWord(aId) and $FFFFFFFF);
end;

function TTestSerialPort.TypeByteOf(aId:TPortUniqueID):Byte;
begin
  Result:=(QWord(aId) shr 56) and $FF;
end;

function TTestSerialPort.IdOf(const aNome:String):TPortUniqueID;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:=aNome;
  Result:=FPorta.getPortId;
end;

procedure TTestSerialPort.SpeedAndFormatOutOfTheBox;
begin
  //19200 8N1, que e' o que a maioria dos equipamentos usa
  AssertEquals('speed',      Ord(br19200), Ord(FPorta.BaudRate));
  AssertEquals('data bits',   Ord(db8),     Ord(FPorta.DataBits));
  AssertEquals('parity',        Ord(spNone),  Ord(FPorta.Paridade));
  AssertEquals('stop bits',  Ord(sb1),     Ord(FPorta.StopBits));
end;

procedure TTestSerialPort.DelayBetweenWriteAndReadOutOfTheBox;
begin
  AssertEquals('delay between writing and reading', 20, FPorta.WriteReadDelay);
end;

procedure TTestSerialPort.TheDefaultTimeoutAgreesWithWhatThePropertyDeclares;
begin
  //o valor declarado na propriedade e' o que o mecanismo de gravacao usa para
  //decidir o que precisa ir para o arquivo de formulario: o que for igual a
  //ele nao e' gravado. Se o construtor comeca com outro valor, quem escolher
  //justamente o valor declarado perde a escolha na proxima abertura
  AssertEquals('timeout', 100, FPorta.Timeout);
end;

{$IFDEF UNIX}
procedure TTestSerialPort.TheDeviceDirectoryOutOfTheBox;
begin
  AssertEquals('device directory', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.AnEmptyDirectoryGoesBackToTheDefault;
begin
  FPorta.DevDir:='/tmp/';
  FPorta.DevDir:='';
  AssertEquals('empty goes back to the default', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.NoDirectoryAtAllGoesBackToTheDefault;
begin
  //"(none)" e' o que o editor de propriedades entrega quando nada foi escolhido
  FPorta.DevDir:='/tmp/';
  FPorta.DevDir:='(none)';
  AssertEquals('none goes back to the default', '/dev/', FPorta.DevDir);
end;

procedure TTestSerialPort.TheChosenDirectoryIsKept;
begin
  //existe para quem usa emulador de porta serial fora de /dev
  FPorta.DevDir:='/tmp/portas/';
  AssertEquals('chosen directory', '/tmp/portas/', FPorta.DevDir);
end;
{$ENDIF}

procedure TTestSerialPort.AnUnknownPortNameIsNotAccepted;
begin
  FPorta.COMPort:='naoexisteestaporta';
  AssertEquals('an unknown name does not get in', '', FPorta.COMPort);
end;

procedure TTestSerialPort.AnUnknownPortNameDoesNotEraseThePreviousOne;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='ttyS0';
  FPorta.AcceptAnyPortName:=false;

  FPorta.COMPort:='naoexisteestaporta';
  AssertEquals('the previous name stands', 'ttyS0', FPorta.COMPort);
end;

procedure TTestSerialPort.AcceptingAnyNameGivesUpTheCheck;
begin
  //e' assim que se usa um emulador de porta, ou um dispositivo com nome fora
  //dos prefixos que a unit conhece
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='umnomequalquer';

  AssertEquals('name accepted without checking', 'umnomequalquer', FPorta.COMPort);
end;

procedure TTestSerialPort.AnEmptyNameClearsThePort;
begin
  FPorta.AcceptAnyPortName:=true;
  FPorta.COMPort:='ttyS0';
  FPorta.AcceptAnyPortName:=false;

  FPorta.COMPort:='';
  AssertEquals('an empty name clears it', '', FPorta.COMPort);
end;

procedure TTestSerialPort.ANewPortIsClosed;
begin
  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestSerialPort.ASerialPortIsAnExclusiveDevice;
begin
  //dispositivo exclusivo nao e' aberto em tempo de projeto: abrir tomaria o
  //equipamento de quem esta' rodando
  FPorta.MarkAsDesignTime;
  FPorta.Active:=true;

  AssertFalse('at design time it does not really open', FPorta.ReallyActive);
end;

procedure TTestSerialPort.WithNoPortNameTheSettingsAreNotGoodEnough;
begin
  AssertFalse('with no port name there is nothing to open', FPorta.SettingsAreAcceptable);
end;

procedure TTestSerialPort.DifferentPortsHaveDifferentIdentifiers;
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

    AssertTrue('two distinct serial ports', FPorta.getPortId<>outra.getPortId);
  finally
    outra.Free;
  end;
end;

procedure TTestSerialPort.TheIdentifierCarriesThePortNumber;
begin
  //o numero da porta e' o que vem depois das letras: COM1 da' 1, ttyUSB0 da' 0
  AssertEquals('COM1',     1,  NumberOfTheId(IdOf('COM1')));
  AssertEquals('COM2',     2,  NumberOfTheId(IdOf('COM2')));
  AssertEquals('COM12',    12, NumberOfTheId(IdOf('COM12')));
  AssertEquals('ttyUSB0',  0,  NumberOfTheId(IdOf('ttyUSB0')));
  AssertEquals('ttyS3',    3,  NumberOfTheId(IdOf('ttyS3')));
  AssertEquals('cuau1',    1,  NumberOfTheId(IdOf('cuau1')));

  AssertEquals('and the serial port mark', 1, TypeByteOf(IdOf('COM1')));
end;

procedure TTestSerialPort.TheSameNumberWithADifferentPrefixIsNotConfused;
var
  umUsb:TPortUniqueID;
begin
  //ttyS0 e ttyUSB0 sao duas portas, com o mesmo numero: o identificador tem
  //que separa-las, senao quem indexa por ele mistura as duas
  umUsb:=IdOf('ttyUSB0');

  AssertTrue('ttyUSB0 and ttyS0',   umUsb<>IdOf('ttyS0'));
  AssertTrue('ttyUSB0 and ttyADV0', umUsb<>IdOf('ttyADV0'));
end;

procedure TTestSerialPort.APortWithNoNameIsMarkedIncomplete;
begin
  //sem nome de porta o identificador leva o bit alto ligado, como na porta de
  //rede sem endereco
  AssertEquals('serial with the incomplete mark', $81, TypeByteOf(FPorta.getPortId));
end;

procedure TTestSerialPort.ANameWithNoNumberIsMarkedIncomplete;
begin
  //um nome sem digito nenhum nao identifica porta alguma
  AssertEquals('name with no number', $81, TypeByteOf(IdOf('umnomesemnumero')));
end;

procedure TTestSerialPort.TheIdentifierIsStableForTheSamePort;
var
  primeiro:TPortUniqueID;
begin
  //e' chave de mapa: ler duas vezes tem que dar o mesmo valor
  primeiro:=IdOf('ttyUSB0');
  AssertTrue('same reading',      primeiro=FPorta.getPortId);
  AssertTrue('same settings', primeiro=IdOf('ttyUSB0'));
end;

{$IFDEF UNIX}
{ TTestSerialPortOverADevice }

const
  DRIVER_DE_TESTE = 91;

procedure TTestSerialPortOverADevice.SetUp;
begin
  FDispositivo:=TSerialDeMentira.Create;

  FPorta:=TSerialProbe.Create(nil);
  FPorta.AcceptAnyPortName:=true;
  FPorta.Timeout:=300;
end;

procedure TTestSerialPortOverADevice.TearDown;
begin
  FreeAndNil(FPorta);
  FreeAndNil(FDispositivo);
end;

procedure TTestSerialPortOverADevice.PointThePortAtTheDevice;
begin
  //o lado escravo do par aparece em /dev/pts: e' para isso que DevDir existe
  FPorta.DevDir :=FDispositivo.Diretorio;
  FPorta.COMPort:=FDispositivo.NomeDoDispositivo;
end;

procedure TTestSerialPortOverADevice.OpeningThePortOverTheDevice;
begin
  PointThePortAtTheDevice;
  FPorta.Active:=true;

  AssertTrue('the port must open', FPorta.ReallyActive);
end;

procedure TTestSerialPortOverADevice.WhatTheDriverWritesReachesTheDevice;
var
  pkg:TIOPacket;
begin
  PointThePortAtTheDevice;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  FPorta.IOCommandSync(iocWrite, 4, BytesOf('01 02 03 04'), 0, DRIVER_DE_TESTE, 0, @pkg);

  AssertBytesEqual('what went out on the wire', BytesOf('01 02 03 04'),
                   FDispositivo.LerOQueFoiEscrito(4, 1000));
end;

procedure TTestSerialPortOverADevice.WhatTheDeviceSendsIsReadByTheDriver;
var
  pkg:TIOPacket;
begin
  PointThePortAtTheDevice;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  FDispositivo.Responder(BytesOf('AA BB CC'));

  FPorta.IOCommandSync(iocRead, 0, nil, 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('read ok',   Ord(iorOK), Ord(pkg.ReadIOResult));
  AssertEquals('three bytes',   3, pkg.Received);
  AssertBytesEqual('what came back', BytesOf('AA BB CC'), pkg.BufferToRead);
end;

procedure TTestSerialPortOverADevice.WithNoAnswerTheResultIsTimeout;
var
  pkg:TIOPacket;
begin
  //o equipamento nao mandou nada: o driver tem que desistir no prazo
  PointThePortAtTheDevice;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  FPorta.IOCommandSync(iocRead, 0, nil, 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('no answer', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
  AssertEquals('nothing read',    0, pkg.Received);
end;

procedure TTestSerialPortOverADevice.ClosingThePortStopsItBeingActive;
begin
  PointThePortAtTheDevice;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  FPorta.Active:=false;
  AssertFalse('closed', FPorta.ReallyActive);
end;

procedure TTestSerialPortOverADevice.TheChosenSpeedReachesTheDevice;
begin
  //guardar o valor na propriedade nao basta: ele tem que virar ajuste do
  //dispositivo, senao o equipamento conversa numa velocidade e o driver noutra
  PointThePortAtTheDevice;
  FPorta.BaudRate:=br9600;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  AssertEquals('9600 on the device', B9600, FDispositivo.CodigoDeVelocidade);
end;

procedure TTestSerialPortOverADevice.ChangingTheSpeedChangesTheDevice;
begin
  //duas velocidades diferentes tem que dar dois ajustes diferentes no
  //dispositivo, senao a escolha nao esta' indo a lugar nenhum
  PointThePortAtTheDevice;
  FPorta.BaudRate:=br19200;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  AssertEquals('19200 on the device', B19200, FDispositivo.CodigoDeVelocidade);
  AssertTrue  ('and it differs from 9600', B9600<>FDispositivo.CodigoDeVelocidade);
end;

procedure TTestSerialPortOverADevice.TheChosenStopBitsReachTheDevice;
begin
  PointThePortAtTheDevice;
  FPorta.StopBits:=sb2;
  FPorta.Active:=true;
  AssertTrue('opened', FPorta.ReallyActive);

  AssertTrue('two stop bits', FDispositivo.TemDoisBitsDeParada);
end;

{$ENDIF}

initialization
  RegisterTest(TTestSerialPort);
  {$IFDEF UNIX}
  //par de pseudo-terminais so' existe em Unix
  RegisterTest(TTestSerialPortOverADevice);
  {$ENDIF}

end.
