object DM: TDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 476
  Width = 651
  object SerialPortDriver1: TSerialPortDriver
    Timeout = 100
    Left = 40
    Top = 8
  end
  object ModBusRTUDriver1: TModBusRTUDriver
    CommunicationPort = SerialPortDriver1
    Left = 40
    Top = 56
  end
  object MySQLConnection: TZConnection
    Protocol = 'mysql-5'
    HostName = 'localhost'
    Database = 'dbtemperaturas'
    User = 'root'
    Password = 'cetis'
    Connected = True
    Left = 188
    Top = 8
  end
  object PesoBalancaW1W2: TPLCBlock
    Tag = 1
    TagGUID = '{C4D2B4AD-4DBE-4C9C-BA28-8389BD06C9B9}'
    AutoWrite = False
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 500
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 16
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 500
    Size = 2
    OnValueChange = PesoBalancaW1W2ValueChange
    Left = 40
    Top = 104
  end
  object PesoProdutoW1W2: TPLCBlock
    Tag = 2
    TagGUID = '{87E2AA23-71BB-41D9-8E48-939678E782E6}'
    AutoWrite = False
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 254
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 16
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 500
    Size = 2
    OnValueChange = PesoBalancaW1W2ValueChange
    Left = 40
    Top = 152
  end
  object PesoACCAtualW1W2: TPLCBlock
    Tag = 3
    TagGUID = '{B980A271-FB7E-4000-9D7A-EAEE7560732D}'
    AutoWrite = False
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 250
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 16
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 10000
    Size = 2
    OnValueChange = PesoBalancaW1W2ValueChange
    Left = 40
    Top = 200
  end
  object FlagsGravar: TPLCTagNumber
    TagGUID = '{C5D7FDCB-881A-4709-802A-51F509A12200}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 261
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 6
    ProtocolDriver = ModBusTCPDriver1
    SyncWrites = True
    Left = 40
    Top = 248
  end
  object Gravar: TTagBit
    TagGUID = '{E54C2B60-AE59-4EA0-BEB6-DF1F96BB0BD8}'
    OnValueChange = GravarValueChange
    PLCTag = FlagsGravar
    UseRawValue = False
    StartBit = 8
    EndBit = 8
    Left = 40
    Top = 304
  end
  object Gravado: TTagBit
    TagGUID = '{72448B6A-FA5C-49D4-A077-6DDEA6060888}'
    PLCTag = FlagsGravar
    UseRawValue = False
    StartBit = 9
    EndBit = 9
    Left = 104
    Top = 304
  end
  object PesoBalanca: TPLCTagNumber
    TagGUID = '{645E967F-9A8C-4B23-A626-AE54BC7AC12A}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 0
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 0
    MemWriteFunction = 0
    Left = 184
    Top = 104
  end
  object PesoProduto: TPLCTagNumber
    TagGUID = '{EB87259D-59A9-4409-BD67-B8692851297B}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 0
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 0
    MemWriteFunction = 0
    Left = 184
    Top = 152
  end
  object PesoACCAtual: TPLCTagNumber
    TagGUID = '{30EC4F27-A1E1-4AA4-BE92-AD1CD1ADA56F}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 0
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 0
    MemWriteFunction = 0
    Left = 184
    Top = 200
  end
  object Saidas: TPLCBlock
    TagGUID = '{2B122DCB-DD15-4A6A-BB2F-831624794F91}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 1
    MemWriteFunction = 15
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 500
    Size = 7
    Left = 248
    Top = 8
  end
  object Q0_0: TPLCBlockElement
    TagGUID = '{6B7786DD-0062-4CEA-AC7C-C2CAECC83DA6}'
    PLCBlock = Saidas
    Index = 0
    Left = 248
    Top = 56
  end
  object Q0_1: TPLCBlockElement
    TagGUID = '{6B7786DD-0062-4CEA-AC7C-C2CAECC83DA6}'
    PLCBlock = Saidas
    Index = 1
    Left = 248
    Top = 104
  end
  object Q0_2: TPLCBlockElement
    TagGUID = '{6B7786DD-0062-4CEA-AC7C-C2CAECC83DA6}'
    OnValueChange = Q0_2ValueChange
    PLCBlock = Saidas
    Index = 2
    Left = 248
    Top = 152
  end
  object Q0_3: TPLCBlockElement
    TagGUID = '{6B7786DD-0062-4CEA-AC7C-C2CAECC83DA6}'
    OnValueChange = Q0_3ValueChange
    PLCBlock = Saidas
    Index = 3
    Left = 248
    Top = 200
  end
  object Entradas: TPLCBlock
    TagGUID = '{B8620C20-2F6B-4A57-8826-0598187B7CF1}'
    AutoWrite = False
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 2
    MemWriteFunction = 0
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 500
    Size = 8
    Left = 328
    Top = 8
  end
  object I0_0: TPLCBlockElement
    TagGUID = '{743B5931-6CAF-42CB-A36E-FCA6D2ACA061}'
    PLCBlock = Entradas
    Index = 0
    Left = 328
    Top = 56
  end
  object I0_1: TPLCBlockElement
    TagGUID = '{598F803F-F457-4E72-AECB-20E19127DA90}'
    PLCBlock = Entradas
    Index = 1
    Left = 328
    Top = 104
  end
  object I0_2: TPLCBlockElement
    TagGUID = '{F8FA5D84-A033-4A14-BEB0-2DFC56885AD3}'
    PLCBlock = Entradas
    Index = 2
    Left = 328
    Top = 152
  end
  object I0_3: TPLCBlockElement
    TagGUID = '{123CC66D-DD62-4176-9DDD-B6598AF7444D}'
    OnValueChange = Q0_2ValueChange
    PLCBlock = Entradas
    Index = 3
    Left = 328
    Top = 200
  end
  object I0_4: TPLCBlockElement
    TagGUID = '{2F430921-37B0-44A2-8586-30B46214261B}'
    OnValueChange = Q0_2ValueChange
    PLCBlock = Entradas
    Index = 4
    Left = 328
    Top = 248
  end
  object I0_5: TPLCBlockElement
    TagGUID = '{3655E52E-3E14-44F7-ADC8-046FB480AAFD}'
    OnValueChange = Q0_3ValueChange
    PLCBlock = Entradas
    Index = 5
    Left = 328
    Top = 296
  end
  object I0_6: TPLCBlockElement
    TagGUID = '{BD1CC53C-23F7-490D-AC8B-11A36AEACBFB}'
    OnValueChange = Q0_3ValueChange
    PLCBlock = Entradas
    Index = 6
    Left = 328
    Top = 344
  end
  object I0_7: TPLCBlockElement
    TagGUID = '{1DC81068-240B-4AE2-89B7-2872AEF063AD}'
    PLCBlock = Entradas
    Index = 7
    Left = 328
    Top = 392
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 40
    Top = 360
  end
  object Flags520: TPLCTagNumber
    TagGUID = '{868EAEC9-6FB7-436B-9D83-566C154247B2}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 260
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 16
    ProtocolDriver = ModBusTCPDriver1
    Left = 400
    Top = 8
  end
  object ala1: TTagBit
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 8
    EndBit = 8
    Left = 400
    Top = 56
  end
  object ala2: TTagBit
    Tag = 1
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 9
    EndBit = 9
    Left = 400
    Top = 104
  end
  object ala3: TTagBit
    Tag = 2
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 10
    EndBit = 10
    Left = 400
    Top = 152
  end
  object ala4: TTagBit
    Tag = 3
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 11
    EndBit = 11
    Left = 400
    Top = 200
  end
  object ala5: TTagBit
    Tag = 4
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 12
    EndBit = 12
    Left = 400
    Top = 248
  end
  object ala6: TTagBit
    Tag = 5
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 13
    EndBit = 13
    Left = 400
    Top = 296
  end
  object ala7: TTagBit
    Tag = 6
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 14
    EndBit = 14
    Left = 400
    Top = 344
  end
  object ala8: TTagBit
    Tag = 7
    TagGUID = '{99A9CE57-4EEA-4ED0-B3B3-819D31C497CD}'
    OnValueChange = ala1ValueChange
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 15
    EndBit = 15
    Left = 400
    Top = 392
  end
  object AlarmesCorrentes: TZQuery
    Connection = MySQLConnection
    Active = True
    ReadOnly = True
    SQL.Strings = (
      'SELECT * FROM alarmes WHERE dt_saida IS NULL')
    Params = <>
    Left = 40
    Top = 424
    object AlarmesCorrentesid: TLargeintField
      FieldName = 'id'
      Required = True
    end
    object AlarmesCorrentesdt_entrada: TDateTimeField
      FieldName = 'dt_entrada'
      Required = True
    end
    object AlarmesCorrentesdt_saida: TDateTimeField
      FieldName = 'dt_saida'
    end
    object AlarmesCorrentestagid: TLargeintField
      FieldName = 'tagid'
      Required = True
    end
    object AlarmesCorrentesmensagem: TStringField
      FieldName = 'mensagem'
      Required = True
      Size = 100
    end
  end
  object dsAlarmesCorrentes: TDataSource
    DataSet = AlarmesCorrentes
    Left = 128
    Top = 424
  end
  object ValvulaBalanca: TPLCTagNumber
    TagGUID = '{DF71347C-C82D-4219-B179-8799F0701F6A}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 0
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 0
    MemWriteFunction = 0
    Left = 536
    Top = 8
  end
  object ValvulaDeposito: TPLCTagNumber
    TagGUID = '{DF71347C-C82D-4219-B179-8799F0701F6A}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 0
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 0
    MemWriteFunction = 0
    Left = 536
    Top = 56
  end
  object v521_1: TTagBit
    TagGUID = '{B6607A8F-3410-4FB9-A2FA-B80495B0436C}'
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 1
    EndBit = 1
    Left = 456
    Top = 56
  end
  object v521_2: TTagBit
    TagGUID = '{B6607A8F-3410-4FB9-A2FA-B80495B0436C}'
    PLCTag = Flags520
    UseRawValue = False
    StartBit = 2
    EndBit = 2
    Left = 456
    Top = 104
  end
  object ZQuery1: TZQuery
    Connection = MySQLConnection
    Params = <>
    Left = 128
    Top = 368
  end
  object TCP_UDPPort1: TTCP_UDPPort
    Active = True
    Host = '192.168.45.130'
    Port = 502
    Timeout = 0
    Left = 112
    Top = 8
  end
  object ModBusTCPDriver1: TModBusTCPDriver
    CommunicationPort = TCP_UDPPort1
    Left = 112
    Top = 56
  end
end
