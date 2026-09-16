{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do LGXDriver: caminho simbolico CIP e classificacao dos
            tipos do ControlLogix.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  No protocolo dos CLPs Rockwell nao se le por endereco: pede-se pelo NOME do
  tag, codificado em segmentos CIP. O codificador e' funcao interna da unit,
  entao os testes chegam nele pela porta publica - atribuindo RequestPath num
  pedido e lendo o ReqPathData que sai.

  Os frames esperados seguem a especificacao CIP: $91 abre um segmento
  simbolico ANSI (tamanho, caracteres, e um byte de enchimento quando o nome
  tem tamanho impar); $28 e' indice de ate um byte e $29 indice de dois.
}
{$ELSE}
{:
  @abstract(LGXDriver tests: CIP symbolic path and ControlLogix type
            classification.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On Rockwell PLCs you do not read by address: you ask by the tag NAME, encoded
  as CIP segments. The encoder is internal to the unit, so the tests reach it
  through the public door - setting RequestPath on a request and reading the
  ReqPathData that comes out.

  The expected frames follow the CIP specification: $91 opens an ANSI symbolic
  segment (length, characters, and a pad byte when the name has an odd length);
  $28 is a one byte index and $29 a two byte one.
}
{$ENDIF}
unit ut.lgxdriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, PLCTagNumber, LGXDriver,
  testsupport.bytes, testsupport.fakeport;

type


  { TRawCIP }

  //um PDU CIP que devolve os bytes que se der a ele: e' o que preenche o lugar
  //dos pacotes de resposta CIP, que a unidade so' sabe decodificar
  //a CIP PDU that gives back whatever bytes it is handed: it fills the place
  //of the CIP reply packets, which the unit only knows how to decode
  TRawCIP = class(TCIPPDU)
  public
    Raw:BYTES;
    function getPacket:BYTES; override;
  end;

  { TLGXProbe }

  //a varredura do LGX e' uma thread enorme; a sonda a desliga e abre DoRead e
  //DoWrite, que a base chama de dentro da varredura
  //the LGX scan is a huge thread; the probe turns it off and opens DoRead and
  //DoWrite, which the base calls from inside the scan
  TLGXProbe = class(TLGXDriver)
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function ReadNow(const aTag:AnsiString; aSize:LongInt; out aValues:TArrayOfDouble):TProtocolIOResult;
    function WriteNow(const aTag:AnsiString; const aValues:array of Double):TProtocolIOResult;
  end;

  { TTestLGXOverAPort }

  //o caminho inteiro pela porta de mentira: abrir sessao (RegisterSession),
  //abrir a conexao (ForwardOpen) e ler/escrever tags por SendUnitData
  //the whole path through the fake port: opening the session
  //(RegisterSession), the connection (ForwardOpen) and reading/writing tags
  //through SendUnitData
  TTestLGXOverAPort = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TLGXProbe;
    procedure QueueRegSession(aHandle:LongWord);
    procedure QueueForwardOpen(aConnID:LongWord);
    procedure QueueShortForwardOpen;
    procedure QueueRegAndFO;
    procedure QueueReadReply(aStatus:Byte; aDataType:Word; const aData:array of Byte);
    procedure QueueWriteFragReply(aStatus:Byte);
    procedure QueueWriteReply(aStatus:Byte);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a leitura / the read
    procedure AReadOpensSessionAndConnectionThenReturnsTheValue;
    procedure TheSessionAndConnectionAreOpenedOnlyOnce;
    procedure TheReadRequestCarriesTheServiceAndThePath;
    procedure AFragmentedReadIsStitchedFromTwoReplies;
    procedure ACIPErrorStatusBecomesTheMappedResult;
    procedure ARefusedSessionMakesTheReadFail;
    procedure ARefusedConnectionMakesTheReadFail;

    //a escrita / the write
    procedure AWriteDiscoversTheTypeThenWrites;
    procedure ABoolWriteUsesTheWriteTagService;
    procedure AWriteErrorStatusBecomesTheMappedResult;
  end;

  { TTestLGXDriver }

  TTestLGXDriver = class(TTestCase)
  private
    FPedido:TCIPReadTagFragReq;
    //: codifica o caminho pela API publica e devolve os bytes gerados
    function  PathOf(const aTag:String):BYTES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //caminho simbolico / symbolic path
    procedure APlainNameBecomesAnAnsiSegment;
    procedure AnEvenLengthNameTakesNoPadding;
    procedure AStructureMemberGivesOneSegmentPerPart;
    procedure AnIndexUpToTwoHundredAndFiftyFiveFitsInOneByte;
    procedure ABiggerIndexUsesATwoByteSegment;
    procedure AMultiDimensionalArrayGivesOneIndexPerDimension;
    procedure AProgramTagIsASingleSegment;
    procedure ThePaddingIsZeroWhenThePreviousOneIsSmaller;
    procedure APathThatIsTooLongIsRefused;

    //classificacao de tipos CIP / CIP type classification
    procedure AStructureIsRecognisedByTheHighBit;
    procedure ASystemTypeIsRecognised;
    procedure TheDimensionsComeFromTheTwoBitsAboveTheType;
    procedure TheTypeNameFollowsTheStudio;
    procedure ACipTypeBecomesAPascalSCADATagType;
    procedure AStructureHasNoMatchingTagType;
    procedure SizeInBytesPerType;

    procedure TheSegmentPaddingIsAlwaysZero;

    //lacuna conhecida / known gap
    procedure AnEncodedPathShouldBeReadableBack;
    procedure EveryKindOfPathComesBackAsItWent;
    procedure ATruncatedPathDecodesWhatIsWhole;
  end;

implementation

procedure TTestLGXDriver.SetUp;
begin
  FPedido:=TCIPReadTagFragReq.Create;
end;

procedure TTestLGXDriver.TearDown;
begin
  FreeAndNil(FPedido);
end;

function TTestLGXDriver.PathOf(const aTag:String):BYTES;
begin
  FPedido.RequestPath:=aTag;
  Result:=FPedido.ReqPathData;
end;

procedure TTestLGXDriver.APlainNameBecomesAnAnsiSegment;
begin
  //$91, tamanho 5, os caracteres de "MyTag", e um byte de enchimento porque
  //o segmento tem que terminar em fronteira de palavra
  AssertBytesEqual('MyTag', BytesOf('91 05 4D 79 54 61 67 00'), PathOf('MyTag'));
end;

procedure TTestLGXDriver.AnEvenLengthNameTakesNoPadding;
begin
  AssertBytesEqual('Ab', BytesOf('91 02 41 62'), PathOf('Ab'));
end;

procedure TTestLGXDriver.AStructureMemberGivesOneSegmentPerPart;
begin
  //cada parte separada por ponto vira o seu proprio segmento
  AssertBytesEqual('A.B', BytesOf('91 01 41 00 91 01 42 00'), PathOf('A.B'));
end;

procedure TTestLGXDriver.AnIndexUpToTwoHundredAndFiftyFiveFitsInOneByte;
begin
  //$28 = indice de um byte
  AssertBytesEqual('Tag[5]', BytesOf('91 03 54 61 67 00 28 05'), PathOf('Tag[5]'));
  AssertBytesEqual('Tag[255]', BytesOf('91 03 54 61 67 00 28 FF'), PathOf('Tag[255]'));
end;

procedure TTestLGXDriver.ABiggerIndexUsesATwoByteSegment;
begin
  //$29 = indice de dois bytes, e o valor vai em little-endian (300 = $012C)
  AssertBytesEqual('Tag[300]', BytesOf('91 03 54 61 67 00 29 00 2C 01'), PathOf('Tag[300]'));
  AssertBytesEqual('Tag[256]', BytesOf('91 03 54 61 67 00 29 00 00 01'), PathOf('Tag[256]'));
end;

procedure TTestLGXDriver.AMultiDimensionalArrayGivesOneIndexPerDimension;
begin
  //array de duas dimensoes: um segmento de indice para cada
  AssertBytesEqual('Tag[1,2]', BytesOf('91 03 54 61 67 00 28 01 28 02'), PathOf('Tag[1,2]'));
end;

procedure TTestLGXDriver.AProgramTagIsASingleSegment;
begin
  //"Program:MainProgram" tem 19 caracteres e vale como um nome unico - os dois
  //pontos nao separam nada aqui, so o ponto separa.
  AssertBytesEqual('program tag',
                   BytesOf('91 13' +                                   //segmento de 19 caracteres
                           '50 72 6F 67 72 61 6D 3A' +                 //"Program:"
                           '4D 61 69 6E 50 72 6F 67 72 61 6D' +        //"MainProgram"
                           '00' +                                      //enchimento
                           '91 02 41 62'),                             //"Ab"
                   PathOf('Program:MainProgram.Ab'));
end;

procedure TTestLGXDriver.ThePaddingIsZeroWhenThePreviousOneIsSmaller;
begin
  //o lado que funciona: com o segmento anterior menor, o enchimento sai zerado
  AssertBytesEqual('A.Tag', BytesOf('91 01 41 00 91 03 54 61 67 00'), PathOf('A.Tag'));
end;

procedure TTestLGXDriver.APathThatIsTooLongIsRefused;
var
  recusou:Boolean;
begin
  //acima de 200 caracteres o codificador recusa em vez de gerar lixo
  recusou:=false;
  try
    PathOf(StringOfChar('A', 250));
  except
    on E:Exception do
      recusou:=true;
  end;
  AssertTrue('a path over the limit must be refused', recusou);
end;

procedure TTestLGXDriver.AStructureIsRecognisedByTheHighBit;
begin
  //bit $8000 ligado = estrutura/UDT
  AssertTrue ('with the structure bit', LGXTypeIsStruct($8FCE));
  AssertFalse('DINT is not a structure',   LGXTypeIsStruct($00C4));
end;

procedure TTestLGXDriver.ASystemTypeIsRecognised;
begin
  //bit $1000 = tipo interno do CLP, que o construtor de tags ignora
  AssertTrue ('with the system bit', LGXTypeIsSystem($10C4));
  AssertFalse('plain DINT',           LGXTypeIsSystem($00C4));
end;

procedure TTestLGXDriver.TheDimensionsComeFromTheTwoBitsAboveTheType;
begin
  //bits 13 e 14 guardam de 0 a 3 dimensoes
  AssertEquals('scalar',      0, LGXTypeDimensions($00C4));
  AssertEquals('one dimension', 1, LGXTypeDimensions($20C4));
  AssertEquals('two',         2, LGXTypeDimensions($40C4));
  AssertEquals('three',         3, LGXTypeDimensions($60C4));
end;

procedure TTestLGXDriver.TheTypeNameFollowsTheStudio;
begin
  AssertEquals('bool',  'BOOL',  LGXTypeName($00C1));
  AssertEquals('dint',  'DINT',  LGXTypeName($00C4));
  AssertEquals('real',  'REAL',  LGXTypeName($00CA));
  AssertEquals('lreal', 'LREAL', LGXTypeName($00CB));

  //estrutura tem nome proprio, independente do resto do codigo
  AssertEquals('structure', 'STRUCT/UDT', LGXTypeName($8FCE));

  //tipo que o driver nao conhece aparece em hexa, para nao mentir um nome
  AssertEquals('unknown', '0x00EE', LGXTypeName($00EE));
end;

procedure TTestLGXDriver.ACipTypeBecomesAPascalSCADATagType;
var
  tipo:TTagType;
begin
  AssertTrue  ('dint converted',  LGXTypeToTagType($00C4, tipo));
  AssertEquals('dint', Ord(pttLongInt), Ord(tipo));

  AssertTrue  ('real converted',  LGXTypeToTagType($00CA, tipo));
  AssertEquals('real', Ord(pttFloat), Ord(tipo));

  AssertTrue  ('lint converted',  LGXTypeToTagType($00C5, tipo));
  AssertEquals('lint', Ord(pttInt64), Ord(tipo));

  //as dimensoes nao atrapalham: o tipo continua sendo lido dos 8 bits baixos
  AssertTrue  ('dint in an array',    LGXTypeToTagType($20C4, tipo));
  AssertEquals('dint in an array', Ord(pttLongInt), Ord(tipo));
end;

procedure TTestLGXDriver.AStructureHasNoMatchingTagType;
var
  tipo:TTagType;
begin
  //estrutura nao tem valor numerico unico: quem chama precisa saber disso
  AssertFalse('structure', LGXTypeToTagType($8FCE, tipo));
end;

procedure TTestLGXDriver.SizeInBytesPerType;
begin
  AssertEquals('bool',  1, LGXTypeSizeInBytes($00C1));
  AssertEquals('sint',  1, LGXTypeSizeInBytes($00C2));
  AssertEquals('int',   2, LGXTypeSizeInBytes($00C3));
  AssertEquals('dint',  4, LGXTypeSizeInBytes($00C4));
  AssertEquals('real',  4, LGXTypeSizeInBytes($00CA));
  AssertEquals('lint',  8, LGXTypeSizeInBytes($00C5));
  AssertEquals('lreal', 8, LGXTypeSizeInBytes($00CB));

  //tipo desconhecido cai em 1 byte - inclusive STRING, que na pratica ocupa
  //bem mais; quem trata string nao passa por aqui
  AssertEquals('unknown', 1, LGXTypeSizeInBytes($00EE));
end;

procedure TTestLGXDriver.TheSegmentPaddingIsAlwaysZero;
begin
  //nome impar depois de um nome maior: o enchimento nao pode herdar nada do
  //segmento anterior
  AssertBytesEqual('AAAA.B', BytesOf('91 04 41 41 41 41 91 01 42 00'), PathOf('AAAA.B'));
  AssertBytesEqual('program tag with an odd member',
                   BytesOf('91 13 50 72 6F 67 72 61 6D 3A 4D 61 69 6E 50 72 6F 67 72 61 6D 00' +
                           '91 03 54 61 67 00'),
                   PathOf('Program:MainProgram.Tag'));
end;

procedure TTestLGXDriver.AnEncodedPathShouldBeReadableBack;
begin
  //e' o caminho que o IDE e o depurador leem de volta dos pedidos CIP
  //it is the path the IDE and the debugger read back from the CIP requests
  FPedido.RequestPath:='MyTag';
  AssertEquals('round trip of the path', 'MyTag', FPedido.RequestPath);
end;

procedure TTestLGXDriver.EveryKindOfPathComesBackAsItWent;
const
  Caminhos: array[0..5] of String = ('A.B', 'Tag[5]', 'Tag[300]', 'Tag[256]', 'Tag[1,2]', 'Program:MainProgram.Motor.Speed[3]');
var
  c:Integer;
begin
  for c:=0 to High(Caminhos) do begin
    FPedido.RequestPath:=Caminhos[c];
    AssertEquals(Caminhos[c], Caminhos[c], FPedido.RequestPath);
  end;
end;

procedure TTestLGXDriver.ATruncatedPathDecodesWhatIsWhole;
begin
  //um segmento cortado no meio nao pode ler alem do buffer: para no que
  //estava inteiro
  //a segment cut in the middle must not read past the buffer: it stops at
  //what was whole
  FPedido.ReqPathData:=BytesOf('91 01 41 00 91 05 42');
  AssertEquals('so o primeiro', 'A', FPedido.RequestPath);
end;


{ TRawCIP }

function TRawCIP.getPacket:BYTES;
begin
  Result:=Raw;
end;

{ TLGXProbe }

procedure TLGXProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TLGXProbe.ReadNow(const aTag:AnsiString; aSize:LongInt; out aValues:TArrayOfDouble):TProtocolIOResult;
var
  tr:TTagRec;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path:=aTag;
  tr.Size:=aSize;
  Result:=DoRead(tr, aValues, true);
end;

function TLGXProbe.WriteNow(const aTag:AnsiString; const aValues:array of Double):TProtocolIOResult;
var
  tr:TTagRec;
  v:TArrayOfDouble;
  i:Integer;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path:=aTag;
  tr.Size:=Length(aValues);
  SetLength(v, Length(aValues));
  for i:=0 to High(aValues) do v[i]:=aValues[i];
  Result:=DoWrite(tr, v, true);
end;

{ helpers that build the reply frames with the driver's own encoders }

//: uma resposta de SendRRData (ForwardOpen), com o CIP dado no lugar do pacote
//: a SendRRData (ForwardOpen) reply, with the given CIP in the packet's place
function FrameSendRR(const aCIP:BYTES):BYTES;
var
  rr:TEIPSendRRDataPDU;
  raw:TRawCIP;
  un:TCPFUnconnectedDataItem;
begin
  rr:=TEIPSendRRDataPDU.Create;
  try
    rr.SessionHandler:=$01020304;
    rr.SendRRDataCmdData.EncapsuledCPF.Add(TCPFNullAddressItem.Create);
    un:=TCPFUnconnectedDataItem.Create;
    raw:=TRawCIP.Create;
    raw.Raw:=aCIP;
    un.Add(raw);
    rr.SendRRDataCmdData.EncapsuledCPF.Add(un);
    Result:=rr.getPacket;
  finally
    rr.Free;
  end;
end;

//: uma resposta de SendUnitData, com o CIP dado no item de dados conectado
//: a SendUnitData reply, with the given CIP in the connected data item
function FrameSendUnit(const aCIP:BYTES):BYTES;
var
  ud:TEIPSendUnitDataPDU;
  raw:TRawCIP;
  addr:TCPFAddressItem;
  cdi:TCPFConnectedDataItem;
begin
  ud:=TEIPSendUnitDataPDU.Create;
  try
    ud.SessionHandler:=$01020304;
    addr:=TCPFAddressItem.Create;
    addr.ConnectionID:=$11223344;
    ud.SendUnitDataCmdData.EncapsuledCPF.Add(addr);
    cdi:=TCPFConnectedDataItem.Create;
    raw:=TRawCIP.Create;
    raw.Raw:=aCIP;
    cdi.Add(raw);
    ud.SendUnitDataCmdData.EncapsuledCPF.Add(cdi);
    Result:=ud.getPacket;
  finally
    ud.Free;
  end;
end;

function ForwardOpenCIP(aConnID:LongWord):BYTES;
begin
  //servico $d4, status 0, e a resposta de sucesso: OT_ConnID e o resto zerado
  //service $d4, status 0, and the success reply: OT_ConnID and the rest zeroed
  SetLength(Result, 4+26);
  FillChar(Result[0], Length(Result), 0);
  Result[0]:=$d4;
  PLongWord(@Result[4])^:=aConnID; //OT_ConnID
end;

{ TTestLGXOverAPort }

procedure TTestLGXOverAPort.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;
  FDrv:=TLGXProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
end;

procedure TTestLGXOverAPort.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

procedure TTestLGXOverAPort.QueueRegSession(aHandle:LongWord);
var
  reg:TEIPRegSessionPDU;
begin
  //o proprio pacote de RegisterSession, com o handle que o "CLP" devolveria
  //the RegisterSession packet itself, with the handle the "PLC" would give back
  reg:=TEIPRegSessionPDU.Create;
  try
    reg.SessionHandler:=aHandle;
    FPort.QueueResponse(reg.getPacket);
  finally
    reg.Free;
  end;
end;

procedure TTestLGXOverAPort.QueueForwardOpen(aConnID:LongWord);
begin
  FPort.QueueResponse(FrameSendRR(ForwardOpenCIP(aConnID)));
end;

procedure TTestLGXOverAPort.QueueShortForwardOpen;
begin
  //um quadro curto demais: o driver espera 70 bytes e desiste da conexao
  //a frame too short: the driver expects 70 bytes and gives up on the connection
  FPort.QueueResponse(BytesOf('6F 00 00 00 04 03 02 01 00 00'));
end;

procedure TTestLGXOverAPort.QueueRegAndFO;
begin
  QueueRegSession($01020304);
  QueueForwardOpen($11223344);
end;

procedure TTestLGXOverAPort.QueueReadReply(aStatus:Byte; aDataType:Word; const aData:array of Byte);
var
  cip:BYTES;
  i:Integer;
begin
  SetLength(cip, 6+Length(aData));
  cip[0]:=$d2; cip[1]:=0; cip[2]:=aStatus; cip[3]:=0;
  PWord(@cip[4])^:=aDataType;
  for i:=0 to High(aData) do cip[6+i]:=aData[i];
  FPort.QueueResponse(FrameSendUnit(cip));
end;

procedure TTestLGXOverAPort.QueueWriteFragReply(aStatus:Byte);
begin
  FPort.QueueResponse(FrameSendUnit(BytesOf('D3 00 '+IntToHex(aStatus,2)+' 00')));
end;

procedure TTestLGXOverAPort.QueueWriteReply(aStatus:Byte);
begin
  FPort.QueueResponse(FrameSendUnit(BytesOf('CD 00 '+IntToHex(aStatus,2)+' 00')));
end;

procedure TTestLGXOverAPort.AReadOpensSessionAndConnectionThenReturnsTheValue;
var
  v:TArrayOfDouble;
begin
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_DINT, [42,0,0,0]);

  AssertEquals('leu', Ord(ioOk), Ord(FDrv.ReadNow('MyTag', 4, v)));
  AssertEquals('quatro bytes',  4,  Length(v));
  AssertEquals('o valor',       42, v[0], 0);
  AssertEquals('tudo consumido', 0, FPort.PendingResponses);
end;

procedure TTestLGXOverAPort.TheSessionAndConnectionAreOpenedOnlyOnce;
var
  v:TArrayOfDouble;
begin
  //RegisterSession e ForwardOpen sao guardados: a segunda leitura so' manda o
  //SendUnitData
  //RegisterSession and ForwardOpen are cached: the second read only sends the
  //SendUnitData
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_DINT, [1,0,0,0]);
  FDrv.ReadNow('MyTag', 4, v);
  AssertEquals('tres idas na primeira: reg, fo, leitura', 3, FPort.WriteCount);

  QueueReadReply(0, TAG_CIP_TYPE_DINT, [2,0,0,0]);
  FDrv.ReadNow('MyTag', 4, v);

  AssertEquals('mais uma so',   4, FPort.WriteCount);
  AssertEquals('o novo valor',  2, v[0], 0);
end;

procedure TTestLGXOverAPort.TheReadRequestCarriesTheServiceAndThePath;
var
  v:TArrayOfDouble;
  req:BYTES;
  comoTexto:AnsiString;
begin
  //o SendUnitData que sai leva o servico de leitura fragmentada ($52) e o
  //caminho do tag no CIP; e' a terceira escrita (reg, fo, leitura)
  //the SendUnitData that goes out carries the fragmented read service ($52)
  //and the tag path in the CIP; it is the third write (reg, fo, read)
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_DINT, [42,0,0,0]);
  FDrv.ReadNow('MyTag', 4, v);

  req:=FPort.WrittenFrame(2);
  //o CIP comeca depois do cabecalho (24) + SendUnitData (6) + CPF (2) +
  //item de endereco (8) + cabecalho do item conectado (6) = 46
  //the CIP starts after header (24) + SendUnitData (6) + CPF (2) + address
  //item (8) + connected item header (6) = 46
  AssertEquals('servico read tag fragmented', $52, req[46]);
  SetString(comoTexto, PAnsiChar(@req[0]), Length(req));
  AssertTrue  ('o nome do tag no pedido', Pos('MyTag', comoTexto)>0);
end;

procedure TTestLGXOverAPort.AFragmentedReadIsStitchedFromTwoReplies;
var
  v:TArrayOfDouble;
begin
  //o CLP responde em dois pedacos: status $06 "ha' mais", depois $00; o driver
  //pede de novo e junta
  //the PLC answers in two pieces: status $06 "more to come", then $00; the
  //driver asks again and stitches
  QueueRegAndFO;
  QueueReadReply($06, TAG_CIP_TYPE_DINT, [10,20]);
  QueueReadReply($00, TAG_CIP_TYPE_DINT, [30,40]);

  AssertEquals('leu', Ord(ioOk), Ord(FDrv.ReadNow('MyTag', 4, v)));
  AssertEquals('quatro bytes juntados', 4, Length(v));
  AssertEquals('do primeiro pedaco', 10, v[0], 0);
  AssertEquals('do segundo pedaco',  30, v[2], 0);
end;

procedure TTestLGXOverAPort.ACIPErrorStatusBecomesTheMappedResult;
var
  v:TArrayOfDouble;
begin
  //$05 e' "objeto inexistente" - o tag nao existe no CLP
  //$05 is "object does not exist" - the tag is not in the PLC
  QueueRegAndFO;
  QueueReadReply($05, 0, []);

  AssertEquals(Ord(ioObjectNotExists), Ord(FDrv.ReadNow('MyTag', 4, v)));
end;

procedure TTestLGXOverAPort.ARefusedSessionMakesTheReadFail;
var
  v:TArrayOfDouble;
begin
  //handle zero: o CLP recusou a sessao, nao ha' como ler
  //handle zero: the PLC refused the session, there is no way to read
  QueueRegSession(0);

  AssertTrue('nao leu', FDrv.ReadNow('MyTag', 4, v)<>ioOk);
end;

procedure TTestLGXOverAPort.ARefusedConnectionMakesTheReadFail;
var
  v:TArrayOfDouble;
begin
  //sessao aberta, mas o ForwardOpen nao veio inteiro: sem conexao, sem leitura
  //session open, but the ForwardOpen did not come whole: no connection, no read
  QueueRegSession($01020304);
  QueueShortForwardOpen;

  AssertTrue('nao leu', FDrv.ReadNow('MyTag', 4, v)<>ioOk);
end;

procedure TTestLGXOverAPort.AWriteDiscoversTheTypeThenWrites;
begin
  //o tag e' desconhecido: o driver le uma vez para descobrir o tipo e so'
  //depois escreve
  //the tag is unknown: the driver reads once to discover the type and only
  //then writes
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_DINT, [0,0,0,0]); //descoberta do tipo / type discovery
  QueueWriteFragReply(0);

  AssertEquals('escreveu', Ord(ioOk), Ord(FDrv.WriteNow('MyTag', [42,0,0,0])));
  AssertEquals('reg, fo, descoberta, escrita', 4, FPort.WriteCount);
end;

procedure TTestLGXOverAPort.ABoolWriteUsesTheWriteTagService;
begin
  //descoberto como BOOL, a escrita usa o servico Write Tag ($4d), respondido
  //com $cd
  //discovered as BOOL, the write uses the Write Tag service ($4d), answered
  //with $cd
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_BOOL, [0]);
  QueueWriteReply(0);

  AssertEquals('escreveu', Ord(ioOk), Ord(FDrv.WriteNow('MyBool', [1])));
end;

procedure TTestLGXOverAPort.AWriteErrorStatusBecomesTheMappedResult;
begin
  QueueRegAndFO;
  QueueReadReply(0, TAG_CIP_TYPE_DINT, [0,0,0,0]);
  QueueWriteFragReply($05);

  AssertEquals(Ord(ioObjectNotExists), Ord(FDrv.WriteNow('MyTag', [42,0,0,0])));
end;

initialization
  RegisterTest(TTestLGXDriver);
  RegisterTest(TTestLGXOverAPort);

end.
