{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes de HMIDBConnection: a formatacao de valores para SQL e o
            componente de conexao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  As funcoes de classe FormatSQL* montam os literais que vao dentro dos comandos
  SQL - e' onde mora a fuga de aspas (injecao), a independencia de locale dos
  numeros e datas, e o formato do UUID que cada banco aceita. O componente em si
  guarda a configuracao da conexao e a fila de comandos; aqui so' o que da' para
  exercitar sem um banco de verdade: a ida e volta das propriedades e o
  construir/destruir limpo.
}
{$ELSE}
{:
  @abstract(HMIDBConnection tests: the formatting of values for SQL and the
            connection component.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The FormatSQL* class functions build the literals that go inside SQL commands
  - that is where quote escaping (injection), the locale independence of numbers
  and dates, and the UUID shape each engine accepts all live. The component
  itself holds the connection configuration and the command queue; here only
  what can be exercised without a real database: the round trip of the
  properties and a clean construct/destroy.
}
{$ENDIF}
unit ut.hmidbconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, HMIDBConnection;

type

  { TTestSQLFormatting }

  TTestSQLFormatting = class(TTestCase)
  private
    FSaved:TFormatSettings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //FormatSQLString
    procedure APlainStringIsWrappedInQuotes;
    procedure ASingleQuoteIsDoubled;
    procedure AnInjectionAttemptIsNeutralised;
    procedure AnEmptyStringBecomesTwoQuotes;
    procedure AnEmptyStringCanBecomeNull;
    procedure ANonEmptyStringIsNeverNull;
    procedure AStringOfOnlyOneQuote;
    //FormatSQLNumber
    procedure WithNoDecimalsItIsAnInteger;
    procedure ANumberIsRoundedToTheDecimals;
    procedure ANumberBelowHalfIsRoundedDown;
    procedure WithDecimalsTheIntegerPartKeepsOneDigit;
    procedure ANegativeNumberKeepsItsSign;
    procedure ZeroWithDecimalsIsPadded;
    procedure TheDecimalSeparatorIsAlwaysADot;
    //FormatPGDatetime
    procedure TheTimestampFollowsIso;
    procedure TheTimestampIsWrappedInQuotes;
    procedure TheTimestampKeepsMilliseconds;
    procedure TheTimestampDoesNotFollowTheLocale;
    //FormatSQLUUID
    procedure TheUuidIsWrappedInQuotes;
    procedure TheUuidHasNoBraces;
    procedure TheUuidBodyIsThirtySixChars;
    procedure TheUuidHexIsPreserved;
  end;

  { TTestDBConnectionComponent }

  TTestDBConnectionComponent = class(TTestCase)
  private
    FConn:THMIDBConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewConnectionHasNoPendingCommands;
    procedure ANewConnectionIsNotConnected;
    procedure TheProtocolRoundTrips;
    procedure AnUnknownProtocolIsRejected;
    procedure AVersionedProtocolIsKeptWhole;
    procedure ThePortRoundTrips;
    procedure TheHostNameRoundTrips;
    procedure TheDatabaseRoundTrips;
    procedure TheUserRoundTrips;
    procedure ThePasswordRoundTrips;
    procedure TheCatalogRoundTrips;
    procedure ReadOnlyRoundTrips;
    procedure ThePropertiesCanBeAssigned;
  end;

implementation

const
  //UUID de teste; GUIDToString o devolveria como {...} em maiusculas
  //test UUID; GUIDToString would return it as {...} in upper case
  SAMPLE_UUID = '{12345678-9abc-def0-1234-56789abcdef0}';

{ TTestSQLFormatting }

procedure TTestSQLFormatting.SetUp;
begin
  //guarda o locale para que os testes que o alteram nao vazem para os demais
  //keep the locale so the tests that change it do not leak into the others
  FSaved:=DefaultFormatSettings;
end;

procedure TTestSQLFormatting.TearDown;
begin
  DefaultFormatSettings:=FSaved;
end;

procedure TTestSQLFormatting.APlainStringIsWrappedInQuotes;
begin
  AssertEquals('aspas ao redor', '''abc''', THMIDBConnection.FormatSQLString('abc'));
end;

procedure TTestSQLFormatting.ASingleQuoteIsDoubled;
begin
  //O'Brien -> 'O''Brien'
  AssertEquals('aspa dobrada', '''O''''Brien''', THMIDBConnection.FormatSQLString('O''Brien'));
end;

procedure TTestSQLFormatting.AnInjectionAttemptIsNeutralised;
begin
  //toda aspa vira duas, entao a string continua sendo um unico literal
  //every quote becomes two, so the string stays a single literal
  AssertEquals('injecao neutralizada',
    '''x''''; DROP TABLE t; --''',
    THMIDBConnection.FormatSQLString('x''; DROP TABLE t; --'));
end;

procedure TTestSQLFormatting.AnEmptyStringBecomesTwoQuotes;
begin
  AssertEquals('string vazia', '''''', THMIDBConnection.FormatSQLString(''));
end;

procedure TTestSQLFormatting.AnEmptyStringCanBecomeNull;
begin
  AssertEquals('vazia vira NULL', 'NULL', THMIDBConnection.FormatSQLString('', true));
end;

procedure TTestSQLFormatting.ANonEmptyStringIsNeverNull;
begin
  AssertEquals('nao vazia nunca e NULL', '''x''', THMIDBConnection.FormatSQLString('x', true));
end;

procedure TTestSQLFormatting.AStringOfOnlyOneQuote;
begin
  //uma aspa -> quatro aspas ('' entre '')
  //one quote -> four quotes ('' inside '')
  AssertEquals('so uma aspa', '''''''''', THMIDBConnection.FormatSQLString(''''));
end;

procedure TTestSQLFormatting.WithNoDecimalsItIsAnInteger;
begin
  AssertEquals('sem casas', '1234', THMIDBConnection.FormatSQLNumber(1234.0));
end;

procedure TTestSQLFormatting.ANumberIsRoundedToTheDecimals;
begin
  AssertEquals('arredonda pra cima', '3.15', THMIDBConnection.FormatSQLNumber(3.146, 2));
end;

procedure TTestSQLFormatting.ANumberBelowHalfIsRoundedDown;
begin
  AssertEquals('arredonda pra baixo', '3.14', THMIDBConnection.FormatSQLNumber(3.144, 2));
end;

procedure TTestSQLFormatting.WithDecimalsTheIntegerPartKeepsOneDigit;
begin
  //0.5 com duas casas nao pode virar '.50'
  //0.5 with two decimals must not become '.50'
  AssertEquals('zero antes do ponto', '0.50', THMIDBConnection.FormatSQLNumber(0.5, 2));
end;

procedure TTestSQLFormatting.ANegativeNumberKeepsItsSign;
begin
  AssertEquals('sinal negativo', '-2.5', THMIDBConnection.FormatSQLNumber(-2.5, 1));
end;

procedure TTestSQLFormatting.ZeroWithDecimalsIsPadded;
begin
  AssertEquals('zero com casas', '0.00', THMIDBConnection.FormatSQLNumber(0, 2));
end;

procedure TTestSQLFormatting.TheDecimalSeparatorIsAlwaysADot;
var
  f:TFormatSettings;
begin
  //mesmo numa maquina cujo separador decimal e' virgula, o SQL leva ponto
  //even on a machine whose decimal separator is a comma, the SQL carries a dot
  f:=DefaultFormatSettings;
  f.DecimalSeparator:=',';
  f.ThousandSeparator:='.';
  DefaultFormatSettings:=f;
  AssertEquals('sempre ponto', '3.14', THMIDBConnection.FormatSQLNumber(3.14159, 2));
end;

procedure TTestSQLFormatting.TheTimestampFollowsIso;
var
  dt:TDateTime;
begin
  dt:=EncodeDate(2020, 1, 2)+EncodeTime(3, 4, 5, 6);
  AssertEquals('formato ISO', '''2020-01-02 03:04:05.006''',
    THMIDBConnection.FormatPGDatetime(dt));
end;

procedure TTestSQLFormatting.TheTimestampIsWrappedInQuotes;
var
  s:String;
begin
  s:=THMIDBConnection.FormatPGDatetime(Now);
  AssertTrue('comeca com aspa', (Length(s)>0) and (s[1]=''''));
  AssertTrue('termina com aspa', (Length(s)>0) and (s[Length(s)]=''''));
end;

procedure TTestSQLFormatting.TheTimestampKeepsMilliseconds;
var
  dt:TDateTime;
begin
  dt:=EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 58, 123);
  AssertEquals('milissegundos', '''1999-12-31 23:59:58.123''',
    THMIDBConnection.FormatPGDatetime(dt));
end;

procedure TTestSQLFormatting.TheTimestampDoesNotFollowTheLocale;
var
  f:TFormatSettings;
  dt:TDateTime;
begin
  //numa maquina cujo separador de hora nao e' ':' o literal tem que continuar
  //correto - antes saia algo como '03.04.05', invalido para o banco
  //on a machine whose time separator is not ':' the literal must stay correct
  //- it used to come out as '03.04.05', invalid for the database
  f:=DefaultFormatSettings;
  f.TimeSeparator:='.';
  f.DateSeparator:='/';
  f.DecimalSeparator:=',';
  DefaultFormatSettings:=f;
  dt:=EncodeDate(2020, 1, 2)+EncodeTime(3, 4, 5, 6);
  AssertEquals('independe do locale', '''2020-01-02 03:04:05.006''',
    THMIDBConnection.FormatPGDatetime(dt));
end;

procedure TTestSQLFormatting.TheUuidIsWrappedInQuotes;
var
  s:String;
begin
  s:=THMIDBConnection.FormatSQLUUID(StringToGUID(SAMPLE_UUID));
  AssertTrue('comeca com aspa', s[1]='''');
  AssertTrue('termina com aspa', s[Length(s)]='''');
end;

procedure TTestSQLFormatting.TheUuidHasNoBraces;
var
  s:String;
begin
  //GUIDToString devolve entre chaves; a maioria dos bancos rejeita as chaves
  //GUIDToString returns it wrapped in braces; most engines reject the braces
  s:=THMIDBConnection.FormatSQLUUID(StringToGUID(SAMPLE_UUID));
  AssertEquals('sem chave de abertura', 0, Pos('{', s));
  AssertEquals('sem chave de fechamento', 0, Pos('}', s));
end;

procedure TTestSQLFormatting.TheUuidBodyIsThirtySixChars;
var
  s:String;
begin
  //8-4-4-4-12 = 36, mais as duas aspas = 38
  //8-4-4-4-12 = 36, plus the two quotes = 38
  s:=THMIDBConnection.FormatSQLUUID(StringToGUID(SAMPLE_UUID));
  AssertEquals('36 caracteres entre aspas', 38, Length(s));
end;

procedure TTestSQLFormatting.TheUuidHexIsPreserved;
var
  s:String;
begin
  s:=UpperCase(THMIDBConnection.FormatSQLUUID(StringToGUID(SAMPLE_UUID)));
  AssertEquals('mesmo UUID', '''12345678-9ABC-DEF0-1234-56789ABCDEF0''', s);
end;

{ TTestDBConnectionComponent }

procedure TTestDBConnectionComponent.SetUp;
begin
  FConn:=THMIDBConnection.Create(nil);
end;

procedure TTestDBConnectionComponent.TearDown;
begin
  FreeAndNil(FConn);
end;

procedure TTestDBConnectionComponent.ANewConnectionHasNoPendingCommands;
begin
  AssertEquals('fila vazia', 0, FConn.GetPendingSQLCommands);
end;

procedure TTestDBConnectionComponent.ANewConnectionIsNotConnected;
begin
  AssertFalse('nao conectado', FConn.Connected);
end;

procedure TTestDBConnectionComponent.TheProtocolRoundTrips;
begin
  FConn.Protocol:='postgresql';
  AssertEquals('protocolo', 'postgresql', FConn.Protocol);
end;

procedure TTestDBConnectionComponent.AnUnknownProtocolIsRejected;
var
  raised:Boolean;
begin
  //os quatro conectores sao ligados estaticamente; um protocolo que nao mapeia
  //para nenhum deles e' recusado na hora (falha cedo) em vez de aceito calado
  //the four connectors are linked statically; a protocol that maps to none of
  //them is refused right away (fail fast) instead of being silently accepted
  raised:=false;
  try
    FConn.Protocol:='weirddb';
  except
    on E:Exception do
      raised:=true;
  end;
  AssertTrue('protocolo desconhecido e recusado', raised);
end;

procedure TTestDBConnectionComponent.AVersionedProtocolIsKeptWhole;
begin
  //o valor cru guarda a versao; so' o conector interno olha antes do '-'
  //the raw value keeps the version; only the internal connector looks before '-'
  FConn.Protocol:='mysql-5.7';
  AssertEquals('protocolo com versao', 'mysql-5.7', FConn.Protocol);
end;

procedure TTestDBConnectionComponent.ThePortRoundTrips;
begin
  FConn.Port:=5432;
  AssertEquals('porta', 5432, FConn.Port);
end;

procedure TTestDBConnectionComponent.TheHostNameRoundTrips;
begin
  FConn.HostName:='dbserver';
  AssertEquals('host', 'dbserver', FConn.HostName);
end;

procedure TTestDBConnectionComponent.TheDatabaseRoundTrips;
begin
  FConn.Database:='mydb';
  AssertEquals('banco', 'mydb', FConn.Database);
end;

procedure TTestDBConnectionComponent.TheUserRoundTrips;
begin
  FConn.User:='operator';
  AssertEquals('usuario', 'operator', FConn.User);
end;

procedure TTestDBConnectionComponent.ThePasswordRoundTrips;
begin
  FConn.Password:='s3cret';
  AssertEquals('senha', 's3cret', FConn.Password);
end;

procedure TTestDBConnectionComponent.TheCatalogRoundTrips;
begin
  FConn.Catalog:='public';
  AssertEquals('catalogo', 'public', FConn.Catalog);
end;

procedure TTestDBConnectionComponent.ReadOnlyRoundTrips;
begin
  AssertFalse('comeca liberado', FConn.ReadOnly);
  FConn.ReadOnly:=true;
  AssertTrue('somente leitura', FConn.ReadOnly);
end;

procedure TTestDBConnectionComponent.ThePropertiesCanBeAssigned;
var
  props:TStringList;
begin
  props:=TStringList.Create;
  try
    props.Values['ApplicationName']:='pascalscada';
    FConn.Properties:=props;
    AssertEquals('propriedade guardada', 'pascalscada',
      FConn.Properties.Values['ApplicationName']);
  finally
    props.Free;
  end;
end;

initialization
  RegisterTest(TTestSQLFormatting);
  RegisterTest(TTestDBConnectionComponent);

end.
