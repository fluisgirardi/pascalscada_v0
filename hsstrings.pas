{:
  @abstract(Unit de tradução do PascalSCADA.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit hsstrings;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

resourcestring
  //////////////////////////////////////////////////////////////////////////////
  // PALHETAS DE COMPONENTES
  //////////////////////////////////////////////////////////////////////////////
  
  strPortsPallete       = 'PascalSCADA Ports';
  strProtocolsPallete   = 'PascalSCADA Protocols';
  strTagsPallete        = 'PascalSCADA Tags';
  strUtilsPallete       = 'PascalSCADA Utils';
  strControlsPallete    = 'PascalSCADA HCl';
  
  //////////////////////////////////////////////////////////////////////////////
  // Mensagens de exceptions.
  //////////////////////////////////////////////////////////////////////////////

  SUpdateThreadWinit = 'A thread não respondeu ao commando INIT';
  SCompIsntADriver = 'O componente não é um driver de protocolo válido';
  SthreadSuspended ='A thread está suspensa?';
  ScannotBlinkWithItSelf = 'A zona escolhida para piscar não pode ser ela mesma!';
  SfileNotFound = 'Arquivo inexistente!';
  SPLCMinPLCMaxMustBeDifferent = 'As propriedades PLCMin e PLCMax tem de ser obrigatoriamente diferentes!';
  SsysMinSysMaxMustBeDifferent = 'As propriedades SysMin e SysMax tem de ser obrigatoriamente diferentes!';
  SportNumberRangeError = 'A porta deve estar entre 1 e 65535!';
  SDBConnectionRequired = 'É necessário estar conectado com o banco de dados!';
  SonlyPLCTagNumber = 'Este driver suporta somente tags PLC simples. Tags Bloco e String não são suportados!';
  STagAlreadyRegiteredWithThisDriver = 'Este Tag já esta registrado com este driver!';
  SerrorInitializingWinsock = 'Falha inicializando WinSock!';
  SoutOfBounds = 'Fora do range!';
  SimpossibleToChangeWhenActive = 'Impossível mudar propriedades de comunicação quando ativo!';
  SimpossibleToRemoveWhenBusy = 'Impossível remover conexão enquanto ela estiver em uso!';
  SincrementMustBeGreaterThanZero = 'Incremento deve ser um valor maior que zero!';
  SinvalidInterface = 'Interface inválida!';
  SoutOfMemory = 'Memória insuficiente!';
  SinvalidMode = 'Modo inválido!!';
  SsizeMustBe7or8 = 'O tamanho do byte pode ser 7 ou 8 somente!';
  SmaxMustBeGreaterThanMin = 'O valor máximo precisa ser maior que o minimo!';
  SminMustBeLessThanMax = 'O valor minimo precisa ser menor que o máximo!';
  StheValueMustBeDifferentOfValueFalseProperty = 'O valor precisa ser diferente do valor FALSO!';
  StheValueMustBeDifferentOfValueTrueProperty = 'O valor precisa ser diferente do valor VERDADEIRO!';
  SonlyMySQL_SQLite_PostgresSupported = 'Os banco de dados suportados são: MySQL, SQLite e PostgreSQL';
  SztBitcomparationValue1MustBeBetween0And31 = 'Para a comparação do tipo ztBit o valor de Value1 precisa ser maior ou igual a 0 (Zero) e menor ou igual a 31!';
  SserialPortNotExist = 'Porta serial inexistente!!';
  SwithoutDBConnection = 'Sem conexão com banco de dados!';
  SonlyNumericTags = 'Somente tags numéricos são aceitos!';
  SuserTableCorrupted = 'Tabela de de integrantes dos grupos não existe! Impossível continuar!';
  SgroupsTableNotExist = 'Tabela de grupos não existe! Impossível continuar!';
  SusersTableNotExist = 'Tabela de usuários não existe! Impossível continuar!';
  StablesCorrupt = 'Tabelas corrompidas! Impossível continuar!';
  SinvalidTag = 'Tag Inválido!';
  SsizeMustBeAtLeastOne = 'Tamanho necessita ser no minimo 1!';
  SstringSizeOutOfBounds = 'Tamanho máximo da string fora dos limites!';
  SinvalidType = 'Tipo inválido!';
  SinvalidValue = 'Valor das propriedades inválido!';
  SinvalidWinSockVersion = 'Versao incorreta da WinSock. Requerida versao 2.0 ou superior';

implementation

end.
 
