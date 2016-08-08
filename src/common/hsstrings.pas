{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit de tradução do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of translation of PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New resourcestrings
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit hsstrings;

interface

resourcestring
  //////////////////////////////////////////////////////////////////////////////
  // PALHETAS DE COMPONENTES
  //
  // COMPONENT PALETTE
  //////////////////////////////////////////////////////////////////////////////
  
  strPortsPallete     = 'PascalSCADA Ports';
  strProtocolsPallete = 'PascalSCADA Protocols';
  strTagsPallete      = 'PascalSCADA Tags';
  strUtilsPallete     = 'PascalSCADA Utils';
  strControlsPallete  = 'PascalSCADA HCl';
  strDatabasePallete  = 'PascalSCADA Database';
  strUserManagement   = 'PascalSCADA User Management';
  strFPCPallete       = 'PascalSCADA - FreePascal';
  strAssistants       = 'PascalSCADA - Assistants';
  
  //////////////////////////////////////////////////////////////////////////////
  // Mensagens de exceptions.
  //
  // Exception messages.
  //////////////////////////////////////////////////////////////////////////////

  {$IFDEF PORTUGUES}
  SUpdateThreadWinit = 'A thread não respondeu ao comando INIT';  //ok
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
  SInvalidTagNameInTagBuilder = 'Nome inválido!';
  SWithoutAtLeastOneValidName = 'É necessário que pelo menos um nome válido esteja presente ou um item tenha a opção "Contar Vazios" marcado!';
  SInvalidBlockName           = 'Nome do(s) bloco(s) inválido(s)!';
  SCannotDestroyBecauseTagsStillManaged= 'O gerenciador de tags não pode ser destruido por ainda estar gerenciando tags!';
  SCannotRebuildTagID = 'Impossível refazer a identificação do tag (TagID).';
  SItemOutOfStructure = 'O tamanho do item excede a área de dados da estrutura.';
  SGetP0 = 'Gets initial position';
  SGetP1 = 'Gets final position';
  SGotoP0 = 'Controls go to P0 position';
  SScanableNotSupported = 'Interface IScanableTagInterface não é suportada!';
  SCheckAtLeastOneVariable = 'Marque pelo menos uma variável!';
  STheOwnerMustBeAProtocolDriver = 'O componente dono da thread de atualização precisa ser um driver de protocolo!';
  SYouMustHaveAtLeastOneStructureItem = 'É necessário pelo menos um item de estrutura!';
  SWhyMapBitsFromOtherBits = 'Porque mapear bits de outros bits?';
  SStartMustBeLessThanEndIndex = 'O indice inicial precisa ser menor que o inicial';
  SDoYouWantDeleteThisItem = 'Deseja mesmo deletar o item?';
  SDeleteTheItem = 'Deletar o item "';
  SDigitalInputInitialByte  = 'Byte inicial da entrada digital';
  SDigitalOutputInitialByte = 'Byte inicial da saida digital';
  SFlagInitialAddress       = 'End. inicial da Flag(M)';
  SInitialAddressInsideDB   = 'End. inicial dentro da DB';
  SCounterInitialAddress    = 'Contador inicial';
  STimerInitialAddress      = 'Temporizador inicial';
  SSMInitialByte            = 'Byte inicial da SM';
  SAIWInitialAddress        = 'End. inicial da AIW';
  SAQWInitialAddress        = 'End. inicial da AQW';
  SPIWInitialAddress        = 'End. inicial da PIW';
  SVInitialAddress          = 'End. inicial da V';
  SRemoveaStructItemCalled  = 'Remover o item da estrutura chamado "';
  SSecurityControlBusy      = 'Gerenciador de segurança ainda em uso.';
  SUserManagementIsSet      = 'Já existe um gerenciador de usuários ativo na aplicação.';
  SInvalidUserManager       = 'Gerenciador de usuarios invalido!';

  us7tb_tabsheet1_caption       = 'Area e tipo';
  us7tb_plcaddres_caption       = 'Endereco do CLP';
  us7tb_memoryarea_caption      = 'Qual e a area de memoria que deseja enderecar';
  us7tb_grptagtype_caption      = 'Qual o tipo do tag que deseja usar para enderecar a(s) memoria(s) selecionada(s)';
  us7tb_optplctagnumber_caption = 'PLCTagNumber (tags separados)';
  us7tb_optplcblock_caption     = 'PLCBlock (tag agrupados do mesmo tipo de dados)';
  us7tb_optplcstruct_caption    = 'PLCStruck (tags agrupados de diferentes tipos de dados)';
  us7tb_lblblocktype_caption    = 'Tipo de dado:';
  us7tb_blockswapbytes_caption  = 'Inverter bytes';
  us7tb_blockswapwords_caption  = 'Inverter words';
  us7tb_blockscan_caption       = 'Varredura (ms):';
  us7tb_structscan_caption      = 'Varredura (ms):';

  us7tb_tabsheet4_caption       = 'Declaração dos items';
  us7tb_lblnumitems_caption     = 'Numero de estruturas a criar';
  us7tb_startaddress_caption    = 'Endereço inicial';
  us7tb_lblblockname_caption    = 'Nome do bloco';
  us7tb_lblDBNumber_caption     = 'A partir do DB';
  us7tb_lblDBNumber1_caption    = 'ate o DB';
  us7tb_label28_caption         = 'Nome do item';
  us7tb_label29_caption         = 'Tipo do item';
  us7tb_label30_caption         = 'Varredura';
  us7tb_label31_caption         = 'Inverter';
  us7tb_label32_caption         = 'Pular';

  SInvalidDatabaseName          = 'Nome inválido!';
  STableAlreadyexistsOnMetadata = 'A tabela já existe no metadata';
  SInvalidQueueOperation        = 'Impossível adicionar a fila como um item da mesma!';
  SInvalidOwnerClass            = 'O dono do objeto precisa ser da classe THMIDBConnection';
  SInvalidClass                 = 'Classe inválida!';

  SSecurityCodeBusyWantRemove   = 'O codigo de segurança ainda está sendo usado por alguns controles, deseja remover o a ligação com eles?';
  SSecurityCodeStillBusy        = 'Remova o codigo dos controles que o estão usando!';
  SWinCCAreInstalled            = 'WinCC está instalado?';
  SUseTheWinCCUserManager       = 'Cadastre a informação de segurança usando o Security Manager do WinCC!';
  SWCCNotSupportCheckUserAuth   = 'Este metodo não é suportado pelo gerenciador de segurança do WinCC';
  SLoginCaptionToken            = 'Informe um usuário que tenha autorização para "%s".';
  SAccessDenied                 = 'Acesso negado!';

  SMBTBStatiomAddress           = 'Qual o endereço (nó) da estação (CLP) que contem as memórias que se deseja mapear?';
  SMBTBTagType                  = 'Qual o tipo de memória você deseja criar como tag?';
  SMBTBMemCount                 = 'Quantas memórias você deseja mapear?';
  SMBTBStartFromZero            = 'Iniciar a contagem a partir do zero? (nomes apenas)';
  SMBTBFirstMemAddress          = 'Qual o endereço da primeira memória que se deseja mapear?';
  SMBTBTabSheet1                = 'Inicio';
  SMBTBTabSheet2                = 'Tipo de tag usado';
  SMBTBTabSheet3                = 'Estrutura de nomeacao';
  SMBTBCancel                   = 'Cancelar';
  SMBTBPrior                    = 'Antrerior';
  SMBTBNext                     = 'Proxima';
  SMBTBFinish                   = 'Concluir';
  SMBTBLabel2                   = 'Qual o tipo de tag deseja usar para mapear essas memórias?';
  SMBTBSimpleFunctions          = 'Usar as funções de escrita simples';
  SMBTBMaxBlockSize             = 'Qual o tamanho máximo do bloco';
  SMBTBScanOfEachBlock          = 'Scan de cada bloco?';
  SMBTBNameOfEachBlock          = 'Nome de cada bloco';
  SMBTBMaxStringSize            = 'Qual o tamanho máximo da string';
  SMBTBStringFormat             = 'Qual o formato da string';
  SMBTBStringByteSize           = 'Tamanho de cada byte da string';
  SMBTBLabel1                   = 'Nome do Tag';
  SMBTBLabel3                   = 'Contar Vazio';
  SMBTBLabel4                   = 'Scan (ms)';
  SMBTBLabel5                   = 'Zero Fill';
  SMBTBLabel6                   = 'Qtd. de digitos';
  SMBTBLabel7                   = 'Escala';
  STagIdxMoreSizeExceedStructLen= 'Indice do item da estrutura mais o seu tamanho excedem o tamanho da área de dados da estrutura';
  {$ELSE}
  SUpdateThreadWinit = 'The thread does not respond to the INIT command';  //ok
  SCompIsntADriver = 'The component is not a valid protocol driver';    //ok
  SthreadSuspended ='The thread is suspended?'; //ok
  ScannotBlinkWithItSelf = 'The animation zone can''t blink with it self!';  //ok
  SfileNotFound = 'File not found!'; //ok
  SPLCMinPLCMaxMustBeDifferent = 'The properties PLCMin and PLCMax must be different!';  //ok
  SsysMinSysMaxMustBeDifferent = 'The properties SysMin and SysMax must be different!';  //ok
  SportNumberRangeError = 'The port value must be between 1 and 65535!'; //ok
  SDBConnectionRequired = 'You must be logged into the database!'; //ok
  SonlyPLCTagNumber = 'This driver supports only TPLCTagNumber. TPLCBlock and TPLCString aren''t supported!'; //ok
  STagAlreadyRegiteredWithThisDriver = 'This Tag already linked with this protocol driver!'; //ok
  SerrorInitializingWinsock = 'Error initializing the WinSock!';  //ok
  SoutOfBounds = 'Out of bounds!'; //ok
  SimpossibleToChangeWhenActive = 'Cannot change the communication port properties while it''s active!'; //ok
  SimpossibleToRemoveWhenBusy = 'Cannot remove the the connection while it''s being used!';      //ok
  SincrementMustBeGreaterThanZero = 'The increment must be a value greater than zero!'; //ok
  SinvalidInterface = 'Invalid interface!';  //ok
  SoutOfMemory = 'Out of memory!';  //ok
  SinvalidMode = 'Invalid mode!';  //ok
  SsizeMustBe7or8 = 'The byte size can be 7 or 8 only!';
  SmaxMustBeGreaterThanMin = 'The maximum value must be greater than minimum!';
  SminMustBeLessThanMax = 'The minimum value must be less than maximum!'; //ok
  StheValueMustBeDifferentOfValueFalseProperty = 'The value must be different of the ValueFalse property!';
  StheValueMustBeDifferentOfValueTrueProperty = 'The value must be different of the ValueTrue property!';
  SonlyMySQL_SQLite_PostgresSupported = 'The supported databases are: MySQL, SQLite and PostgreSQL';  //ok
  SztBitcomparationValue1MustBeBetween0And31 = 'To use the ztBit selection criteria, the value of Value1 must be greater or equal than 0 and less or equal than 31!';
  SserialPortNotExist = 'Serial port does not exists!';
  SwithoutDBConnection = 'Without a database connection!';
  SonlyNumericTags = 'Only numeric tags are acceptable!';
  SuserTableCorrupted = 'Group members table does not exists!';
  SgroupsTableNotExist = 'Group table does not exists!';  //ok
  SusersTableNotExist = 'Users table does not exists!'; //ok
  StablesCorrupt = 'Database tables corrupted!';
  SinvalidTag = 'Invalid Tag!';
  SsizeMustBeAtLeastOne = 'The size must be at least 1!'; //ok
  SstringSizeOutOfBounds = 'String size out of bounds!';
  SinvalidType = 'Invalid type!'; //ok
  SinvalidValue = 'Invalid value!';
  SinvalidWinSockVersion = 'Wrong version of the Winsock. Requires version 2.0 or later!';
  SFaultGettingLastOSError = 'Error getting the error messege from operating system';
  SWithoutTag = 'WITHOUT TAG!';
  SEmpty      = 'Empty';
  SWithoutTagBuilder = 'The protocol driver does not support the Tag Builder tool';
  SInvalidTagNameInTagBuilder = 'Invalid name!';
  SWithoutAtLeastOneValidName = 'You must have at least on item with a valid name or the option "Count empty" must be checked!';
  SInvalidBlockName           = 'Invalid block name!';
  SCannotDestroyBecauseTagsStillManaged= 'The tag manager cannot be destroyed while it''s in use!';
  SCannotRebuildTagID = 'Cannot rebuild the tag identification (TagID).';
  SItemOutOfStructure = 'The item size exceeds the area of structure.';
  SGetP0 = 'Gets initial position';
  SGetP1 = 'Gets final position';
  SGotoP0 = 'Controls go to P0 position';
  SScanableNotSupported = 'The IScanableTagInterface Interface isn''t supported!';
  SCheckAtLeastOneVariable = 'Check at least one variable!';
  STheOwnerMustBeAProtocolDriver = 'The component owner of the update thread must be a protocol driver!';
  SYouMustHaveAtLeastOneStructureItem = 'You must have at least one structure item!';
  SWhyMapBitsFromOtherBits = 'Why to map bits from other bits?';
  SStartMustBeLessThanEndIndex = 'Start index must be less or equal than end index';
  SDoYouWantDeleteThisItem = 'Do you want to delete this item?';
  SDeleteTheItem = 'Delete item "';
  SDigitalInputInitialByte  = 'Initial byte of digital input';
  SDigitalOutputInitialByte = 'Initial byte of digital ouput';
  SFlagInitialAddress       = 'Initial address of Flags(M)';
  SInitialAddressInsideDB   = 'Start address inside of DB';
  SCounterInitialAddress    = 'Initial address of Counter';
  STimerInitialAddress      = 'Initial address of Timer';
  SSMInitialByte            = 'Initial address of SM';
  SAIWInitialAddress        = 'Initial address of AIW';
  SAQWInitialAddress        = 'Initial address of AQW';
  SPIWInitialAddress        = 'Initial address of PIW';
  SVInitialAddress          = 'Initial address of V';
  SRemoveaStructItemCalled  = 'Remove the structure item called "';
  SSecurityControlBusy      = 'The security manager is busy.';
  SUserManagementIsSet      = 'There are already a user management on your application.';
  SInvalidUserManager       = 'Invalid user manager!';
  SDriverRequired           = 'You must assign one driver to build tags';
  SBlockRequired            = 'A block or structure tag is required to map their elements';
  SNumberTagRequired        = 'A numeric tag is required to build bits';

  us7tb_tabsheet1_caption       = 'Area and type';
  us7tb_plcaddres_caption       = 'PLC Address';
  us7tb_memoryarea_caption      = 'What''s the memory area to be mapped';
  us7tb_grptagtype_caption      = 'What''s the kind of tag that will be used to represent the selected memory area';
  us7tb_optplctagnumber_caption = 'PLCTagNumber (single tags)';
  us7tb_optplcblock_caption     = 'PLCBlock (grouped tags of the same data type)';
  us7tb_optplcstruct_caption    = 'PLCStruck (grouped tags of many data types)';
  us7tb_lblblocktype_caption    = 'Data type:';
  us7tb_blockswapbytes_caption  = 'Swap bytes';
  us7tb_blockswapwords_caption  = 'Swap words';
  us7tb_blockscan_caption       = 'Scan (ms):';
  us7tb_structscan_caption      = 'Scan (ms):';

  us7tb_tabsheet4_caption       = 'Item declaration';
  us7tb_lblnumitems_caption     = 'Number of structures to create';
  us7tb_startaddress_caption    = 'Initial address';
  us7tb_lblblockname_caption    = 'Block name';
  us7tb_lblDBNumber_caption     = 'From DB number';
  us7tb_lblDBNumber1_caption    = 'to DB';
  us7tb_label28_caption         = 'Item name';
  us7tb_label29_caption         = 'Item type';
  us7tb_label30_caption         = 'SCAN';
  us7tb_label31_caption         = 'SWAP';
  us7tb_label32_caption         = 'SKIP';

  SInvalidDatabaseName          = 'Invalid name!';
  STableAlreadyexistsOnMetadata = 'The table already exists on metadata.';
  SInvalidQueueOperation        = 'Impossible add the queue as a queue item!';
  SInvalidOwnerClass            = 'The owner must be a object of THMIDBConnection class';
  SInvalidClass                 = 'Invalid class!';

  SSecurityCodeBusyWantRemove   = 'The Security Code still being used by some controls. Do you want remove the link between the security code and these controls?';
  SSecurityCodeStillBusy        = 'The SecurityCode still being used by some controls!';
  SWinCCAreInstalled            = 'WinCC is installed?';
  SUseTheWinCCUserManager       = 'Use the WinCC Security Manager to insert a new Security Code/Authorization!';
  SWCCNotSupportCheckUserAuth   = 'This method is not supported by WinCC security Manager';
  SLoginCaptionToken            = 'Enter a user that can access the "%s" token.';
  SAccessDenied                 = 'Access denied!';

  SMBTBStatiomAddress           = 'What are the address of your Modbus Device?';
  SMBTBTagType                  = 'What are the tag type you want use in your application?';
  SMBTBMemCount                 = 'How many memories do you want map?';
  SMBTBStartFromZero            = 'Start the count from zero? (only names)';
  SMBTBFirstMemAddress          = 'What´s the address of the first memory to map?';
  SMBTBTabSheet1                = 'Start from';
  SMBTBTabSheet2                = 'Tag type used';
  SMBTBTabSheet3                = 'Name struct';
  SMBTBCancel                   = 'Cancel';
  SMBTBPrior                    = 'Prior';
  SMBTBNext                     = 'Next';
  SMBTBFinish                   = 'Finish';
  SMBTBLabel2                   = 'What´s the tag kind that will be used to map these variables?';
  SMBTBSimpleFunctions          = 'Use the single write functions';
  SMBTBMaxBlockSize             = 'What´s the maximum size of the block';
  SMBTBScanOfEachBlock          = 'Update rate of each block?';
  SMBTBNameOfEachBlock          = 'Name of each block';
  SMBTBMaxStringSize            = 'Maximum string size';
  SMBTBStringFormat             = 'What´s the string format';
  SMBTBStringByteSize           = 'Whats the byte size of string';
  SMBTBLabel1                   = 'Tag name';
  SMBTBLabel3                   = 'Count empty';
  SMBTBLabel4                   = 'Scan (ms)';
  SMBTBLabel5                   = 'Zero Fill';
  SMBTBLabel6                   = 'Qtd. de digitos';
  SMBTBLabel7                   = 'Scale';
  STagIdxMoreSizeExceedStructLen= 'Struct item index more struct item size exceeds the data length of the struct';
  {$ENDIF}

{$IFNDEF FPC}
const
  LineEnding=#13#10;
{$ENDIF}

implementation

end.
 
