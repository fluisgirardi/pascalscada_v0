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

  SUpdateThreadWinit = 'A thread não respondeu ao commando INIT';  //ok
  SCompIsntADriver = 'O componente não é um driver de protocolo válido';    //ok
  SthreadSuspended ='A thread está suspensa?'; //ok
  ScannotBlinkWithItSelf = 'A zona escolhida para piscar não pode ser ela mesma!';  //ok
  SfileNotFound = 'Arquivo inexistente!'; //ok
  SPLCMinPLCMaxMustBeDifferent = 'As propriedades PLCMin e PLCMax tem de ser obrigatoriamente diferentes!';  //ok
  SsysMinSysMaxMustBeDifferent = 'As propriedades SysMin e SysMax tem de ser obrigatoriamente diferentes!';  //ok
  SportNumberRangeError = 'A porta deve estar entre 1 e 65535!'; //ok
  SDBConnectionRequired = 'É necessário estar conectado com o banco de dados!'; //ok
  SonlyPLCTagNumber = 'Este driver suporta somente tags PLC simples. Tags Bloco e String não são suportados!'; //ok
  STagAlreadyRegiteredWithThisDriver = 'Este Tag já esta registrado com este driver!'; //ok
  SerrorInitializingWinsock = 'Falha inicializando WinSock!';  //ok
  SoutOfBounds = 'Fora dos limites!'; //ok
  SimpossibleToChangeWhenActive = 'Impossível mudar propriedades de comunicação quando ativo!'; //ok
  SimpossibleToRemoveWhenBusy = 'Impossível remover conexão enquanto ela estiver em uso!';      //ok
  SincrementMustBeGreaterThanZero = 'Incremento deve ser um valor maior que zero!'; //ok
  SinvalidInterface = 'Interface inválida!';  //ok
  SoutOfMemory = 'Memória insuficiente!';  //ok
  SinvalidMode = 'Modo inválido!';  //ok
  SsizeMustBe7or8 = 'O tamanho do byte pode ser 7 ou 8 somente!';
  SmaxMustBeGreaterThanMin = 'O valor máximo precisa ser maior que o minimo!';
  SminMustBeLessThanMax = 'O valor minimo precisa ser menor que o máximo!'; //ok
  StheValueMustBeDifferentOfValueFalseProperty = 'O valor precisa ser diferente do valor FALSO!';
  StheValueMustBeDifferentOfValueTrueProperty = 'O valor precisa ser diferente do valor VERDADEIRO!';
  SonlyMySQL_SQLite_PostgresSupported = 'Os banco de dados suportados são: MySQL, SQLite e PostgreSQL';  //ok
  SztBitcomparationValue1MustBeBetween0And31 = 'Para a comparação do tipo ztBit o valor de Value1 precisa ser maior ou igual a 0 (Zero) e menor ou igual a 31!';
  SserialPortNotExist = 'Porta serial inexistente!!';
  SwithoutDBConnection = 'Sem conexão com banco de dados!';
  SonlyNumericTags = 'Somente tags numéricos são aceitos!';
  SuserTableCorrupted = 'Tabela de de integrantes dos grupos não existe! Impossível continuar!';
  SgroupsTableNotExist = 'Tabela de grupos não existe! Impossível continuar!';  //ok
  SusersTableNotExist = 'Tabela de usuários não existe! Impossível continuar!'; //ok
  StablesCorrupt = 'Tabelas corrompidas! Impossível continuar!';
  SinvalidTag = 'Tag Inválido!';
  SsizeMustBeAtLeastOne = 'Tamanho necessita ser no minimo 1!'; //ok
  SstringSizeOutOfBounds = 'Tamanho máximo da string fora dos limites!';
  SinvalidType = 'Tipo inválido!'; //ok
  SinvalidValue = 'Valor inválido!';
  SinvalidWinSockVersion = 'Versao incorreta da WinSock. Requerida versao 2.0 ou superior';
  SFaultGettingLastOSError = 'Falha buscando a mensagem de erro do sistema operacional';
  SWithoutTag = 'SEM TAG!';
  SEmpty      = 'Vazio';
  SWithoutTagBuilder = 'O driver não suporta a ferramenta Tag Builder';

implementation

end.
 
