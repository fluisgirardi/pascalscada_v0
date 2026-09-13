{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TMelsecTCPDriver: montagem do quadro 3E binario do
            protocolo MC da Mitsubishi.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O cabecalho e' fixo - subcabecalho $5000, rede 0, CLP $FF, modulo $03FF,
  estacao 0, tamanho, temporizador da CPU e o comando $0401 (leitura em
  bloco). O que muda por pedido e' o subcomando (bit ou palavra), o endereco
  em tres bytes, o codigo do dispositivo e a quantidade de pontos.

  Como no Modbus, EncodePkg e DecodePkg sao protegidos e virtuais, entao uma
  subclasse-sonda os expoe sem precisar de porta nenhuma.
}
{$ELSE}
{:
  @abstract(TMelsecTCPDriver tests: building Mitsubishi's MC protocol 3E
            binary frame.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The header is fixed - $5000 subheader, network 0, PLC $FF, module $03FF,
  station 0, length, CPU timer and command $0401 (batch read). What changes
  per request is the subcommand (bit or word), the three byte address, the
  device code and the point count.

  As with Modbus, EncodePkg and DecodePkg are protected and virtual, so a
  probe subclass exposes them with no port involved.
}
{$ENDIF}
unit ut.melsectcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, MelsecTCP,
  testsupport.bytes, testsupport.protocol;

type

  { TMelsecProbe }

  TMelsecProbe = class(TMelsecTCPDriver)
  public
    function Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
  end;

  { TTestMelsecTCP }

  TTestMelsecTCP = class(TTestCase)
  private
    FDrv:TMelsecProbe;
    //: byte do codigo de dispositivo dentro do quadro
    function  DeviceCodeOf(aFuncaoDeLeitura:LongInt):Byte;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //quadro de leitura / read frame
    procedure ARegisterReadBuildsTheFrame;
    procedure ABitReadUsesAnotherSubcommand;
    procedure TheAddressGoesInThreeBytes;
    procedure ThePointCountGoesInTwoBytes;

    //codigos de dispositivo / device codes
    procedure EveryAreaHasItsOwnDeviceCode;

    //tamanho previsto da resposta / expected response size
    procedure ARegisterAnswerHasTwoBytesPerPoint;
    procedure ABitAnswerPacksEightPerByte;

    //funcao desconhecida / unknown function
    procedure AnUnknownFunctionBuildsNoFrame;
  end;

implementation

{ TMelsecProbe }

function TMelsecProbe.Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
begin
  Result:=EncodePkg(aTag, aToWrite, aResultLen);
end;

{ TTestMelsecTCP }

//Criar o driver sobe as threads de varredura, e destrui-lo espera por elas.
//Os metodos exercitados aqui recebem tudo por parametro e nao guardam estado,
//entao uma instancia atende a classe toda.
var
  DriverCompartilhado:TMelsecProbe = nil;

procedure TTestMelsecTCP.SetUp;
begin
  if DriverCompartilhado=nil then
    DriverCompartilhado:=TMelsecProbe.Create(nil);
  FDrv:=DriverCompartilhado;
end;

procedure TTestMelsecTCP.TearDown;
begin
  FDrv:=nil;
end;

function TTestMelsecTCP.DeviceCodeOf(aFuncaoDeLeitura:LongInt):Byte;
var
  len:LongInt;
  quadro:BYTES;
begin
  quadro:=FDrv.Encode(TagRecFor(1, aFuncaoDeLeitura, 0, 0, 1), nil, len);
  Result:=quadro[18];
end;

procedure TTestMelsecTCP.ARegisterReadBuildsTheFrame;
var
  len:LongInt;
begin
  //funcao $09 = registradores D; 2 palavras a partir de D100
  AssertBytesEqual('read of D100, 2 words',
                   BytesOf('50 00' +          //subcabecalho
                           '00' +             //numero da rede
                           'FF' +             //numero do CLP
                           'FF 03' +          //modulo de E/S
                           '00' +             //estacao
                           '0C 00' +          //tamanho dos dados
                           '10 00' +          //temporizador da CPU
                           '01 04' +          //comando: leitura em bloco
                           '00 00' +          //subcomando: por palavra
                           '64 00 00' +       //endereco 100
                           'A8' +             //dispositivo D
                           '02 00' +          //dois pontos
                           '00'),
                   FDrv.Encode(TagRecFor(1, $09, 0, 100, 2), nil, len));
end;

procedure TTestMelsecTCP.ABitReadUsesAnotherSubcommand;
var
  len:LongInt;
begin
  //funcao $01 = memoria M (bits): o subcomando muda para 1
  AssertBytesEqual('read of 16 bits from M0',
                   BytesOf('50 00 00 FF FF 03 00 0C 00 10 00 01 04' +
                           '01 00' +          //subcomando: por bit
                           '00 00 00' +       //endereco 0
                           '90' +             //dispositivo M
                           '10 00' +          //16 pontos
                           '00'),
                   FDrv.Encode(TagRecFor(1, $01, 0, 0, 16), nil, len));
end;

procedure TTestMelsecTCP.TheAddressGoesInThreeBytes;
var
  len:LongInt;
  quadro:BYTES;
begin
  //endereco $012345 ocupa os tres bytes, do menos para o mais significativo
  quadro:=FDrv.Encode(TagRecFor(1, $09, 0, $012345, 1), nil, len);

  AssertEquals('low byte',  $45, quadro[15]);
  AssertEquals('middle byte',  $23, quadro[16]);
  AssertEquals('high byte',   $01, quadro[17]);
end;

procedure TTestMelsecTCP.ThePointCountGoesInTwoBytes;
var
  len:LongInt;
  quadro:BYTES;
begin
  //300 pontos = $012C
  quadro:=FDrv.Encode(TagRecFor(1, $09, 0, 0, 300), nil, len);

  AssertEquals('low byte', $2C, quadro[19]);
  AssertEquals('high byte',  $01, quadro[20]);
end;

procedure TTestMelsecTCP.EveryAreaHasItsOwnDeviceCode;
begin
  //os codigos do MC protocol, um por area de memoria
  AssertEquals('M',  $90, DeviceCodeOf($01));
  AssertEquals('SM', $91, DeviceCodeOf($02));
  AssertEquals('L',  $92, DeviceCodeOf($03));
  AssertEquals('F',  $93, DeviceCodeOf($04));
  AssertEquals('V',  $94, DeviceCodeOf($05));
  AssertEquals('X',  $9C, DeviceCodeOf($06));
  AssertEquals('Y',  $9D, DeviceCodeOf($07));
  AssertEquals('B',  $A0, DeviceCodeOf($08));
  AssertEquals('D',  $A8, DeviceCodeOf($09));
  AssertEquals('SD', $A9, DeviceCodeOf($10));
end;

procedure TTestMelsecTCP.ARegisterAnswerHasTwoBytesPerPoint;
var
  len:LongInt;
begin
  //9 bytes de cabecalho mais duas palavras
  FDrv.Encode(TagRecFor(1, $09, 0, 0, 2), nil, len);
  AssertEquals('2 registers', 13, len);

  FDrv.Encode(TagRecFor(1, $09, 0, 0, 10), nil, len);
  AssertEquals('10 registers', 29, len);
end;

procedure TTestMelsecTCP.ABitAnswerPacksEightPerByte;
var
  len:LongInt;
begin
  //16 bits cabem em 2 bytes
  FDrv.Encode(TagRecFor(1, $01, 0, 0, 16), nil, len);
  AssertEquals('16 bits', 11, len);

  //e 17 precisam de 3, arredondando para cima
  FDrv.Encode(TagRecFor(1, $01, 0, 0, 17), nil, len);
  AssertEquals('17 bits', 12, len);
end;

procedure TTestMelsecTCP.AnUnknownFunctionBuildsNoFrame;
var
  len:LongInt;
  quadro:BYTES;
begin
  //funcao fora da tabela: nada e' montado, e nada se espera de volta
  quadro:=FDrv.Encode(TagRecFor(1, $42, 0, 0, 1), nil, len);

  AssertEquals('empty frame', 0, Length(quadro));
  AssertEquals('nothing to receive', 0, len);
end;

initialization
  RegisterTest(TTestMelsecTCP);

finalization
  FreeAndNil(DriverCompartilhado);

end.
