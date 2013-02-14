object DataModule2: TDataModule2
  OldCreateOrder = False
  Left = 482
  Top = 104
  Height = 415
  Width = 239
  object TCP_UDPPort1: TTCP_UDPPort
    Active = True
    LogFile = 'c:\isotcp.log'
    Host = '192.168.2.185'
    Timeout = 50
    ExclusiveDevice = True
    Left = 24
    Top = 8
  end
  object ISOTCPDriver1: TISOTCPDriver
    CommunicationPort = TCP_UDPPort1
    ConnectionWay = ISOTCP
    Left = 24
    Top = 40
  end
  object DB1: TPLCBlock
    TagGUID = '{B0747848-93A1-11DF-AA46-001BFC644612}'
    AutoRead = False
    AutoWrite = False
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 2
    MemFile_DB = 1
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 4
    MemWriteFunction = 0
    ProtocolDriver = ISOTCPDriver1
    RefreshTime = 8
    Size = 40
    TagType = pttDWord
    SwapBytes = True
    SwapWords = True
    Left = 56
    Top = 8
  end
  object MD0_MD40: TPLCBlock
    TagGUID = '{2358DCFB-93A2-11DF-AA46-001BFC644612}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 2
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 0
    ProtocolDriver = ISOTCPDriver1
    RefreshTime = 8
    Size = 11
    TagType = pttDWord
    SwapBytes = True
    SwapWords = True
    Left = 88
    Top = 8
  end
  object DB1_DBD00: TPLCBlockElement
    TagGUID = '{090B09C0-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 0
    Left = 56
    Top = 40
  end
  object DB1_DBD04: TPLCBlockElement
    TagGUID = '{3F377D89-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 1
    Left = 56
    Top = 72
  end
  object DB1_DBD08: TPLCBlockElement
    TagGUID = '{C79046CB-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 2
    Left = 56
    Top = 104
  end
  object DB1_DBD12: TPLCBlockElement
    TagGUID = '{CF12E2C9-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 3
    Left = 56
    Top = 136
  end
  object DB1_DBD16: TPLCBlockElement
    TagGUID = '{D4AAC635-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 4
    Left = 56
    Top = 168
  end
  object DB1_DBD20: TPLCBlockElement
    TagGUID = '{D6C01DD6-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 5
    Left = 56
    Top = 200
  end
  object DB1_DBD24: TPLCBlockElement
    TagGUID = '{DA2F20C5-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 6
    Left = 56
    Top = 232
  end
  object DB1_DBD28: TPLCBlockElement
    TagGUID = '{DC2268E1-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 7
    Left = 56
    Top = 264
  end
  object DB1_DBD32: TPLCBlockElement
    TagGUID = '{DDE364DE-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 8
    Left = 56
    Top = 296
  end
  object DB1_DBD36: TPLCBlockElement
    TagGUID = '{DFA13295-93A2-11DF-AA46-001BFC644612}'
    PLCBlock = DB1
    Index = 9
    Left = 56
    Top = 328
  end
  object MD00: TPLCBlockElement
    TagGUID = '{29D96AAE-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 0
    Left = 88
    Top = 40
  end
  object MD04: TPLCBlockElement
    TagGUID = '{29DA4BDA-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 1
    Left = 88
    Top = 72
  end
  object MD08: TPLCBlockElement
    TagGUID = '{29DB2E57-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 2
    Left = 88
    Top = 104
  end
  object MD12: TPLCBlockElement
    TagGUID = '{29DC2BA7-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 3
    Left = 88
    Top = 136
  end
  object MD16: TPLCBlockElement
    TagGUID = '{29DD124B-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 4
    Left = 88
    Top = 168
  end
  object MD20: TPLCBlockElement
    TagGUID = '{29DE01ED-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 5
    Left = 88
    Top = 200
  end
  object MD24: TPLCBlockElement
    TagGUID = '{29DF0D21-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 6
    Left = 88
    Top = 232
  end
  object MD28: TPLCBlockElement
    TagGUID = '{29E00568-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 7
    Left = 88
    Top = 264
  end
  object MD32: TPLCBlockElement
    TagGUID = '{29E0FD75-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 8
    Left = 88
    Top = 296
  end
  object MD36: TPLCBlockElement
    TagGUID = '{29E21914-93A3-11DF-AA46-001BFC644612}'
    PLCBlock = MD0_MD40
    Index = 9
    Left = 88
    Top = 328
  end
  object InputsBYTE_01: TPLCTagNumber
    TagGUID = '{A5A6E944-93A8-11DF-AA46-001BFC644612}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 2
    MemFile_DB = 0
    MemAddress = 3
    MemSubElement = 0
    MemReadFunction = 1
    MemWriteFunction = 0
    ProtocolDriver = ISOTCPDriver1
    RefreshTime = 8
    TagType = pttByte
    Left = 120
    Top = 8
  end
  object I0_0: TTagBit
    TagGUID = '{FBF4B42B-93A8-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 0
    EndBit = 0
    Left = 120
    Top = 40
  end
  object I0_1: TTagBit
    TagGUID = '{EA286CED-7B0F-4D86-988C-DEE992FBBCB2}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 1
    EndBit = 1
    Left = 120
    Top = 72
  end
  object I0_2: TTagBit
    TagGUID = '{1A49923F-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 2
    EndBit = 2
    Left = 120
    Top = 104
  end
  object I0_3: TTagBit
    TagGUID = '{1BA09E17-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 3
    EndBit = 3
    Left = 120
    Top = 136
  end
  object I0_4: TTagBit
    TagGUID = '{1D9ED8BA-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 4
    EndBit = 4
    Left = 120
    Top = 168
  end
  object I0_5: TTagBit
    TagGUID = '{1EE73D23-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 5
    EndBit = 5
    Left = 120
    Top = 200
  end
  object I0_6: TTagBit
    TagGUID = '{20F2E595-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 6
    EndBit = 6
    Left = 120
    Top = 232
  end
  object I0_7: TTagBit
    TagGUID = '{22D0366D-93A9-11DF-AA46-001BFC644612}'
    PLCTag = InputsBYTE_01
    UseRawValue = False
    StartBit = 7
    EndBit = 7
    Left = 120
    Top = 264
  end
  object OutputsBYTE_01: TPLCTagNumber
    TagGUID = '{477E97A0-2322-4E1F-B909-96D70F2950F5}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 2
    MemFile_DB = 0
    MemAddress = 2
    MemSubElement = 0
    MemReadFunction = 2
    MemWriteFunction = 0
    ProtocolDriver = ISOTCPDriver1
    RefreshTime = 8
    TagType = pttByte
    Left = 152
    Top = 8
  end
  object Q0_0: TTagBit
    TagGUID = '{6A0638CB-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 0
    EndBit = 0
    Left = 152
    Top = 40
  end
  object Q0_1: TTagBit
    TagGUID = '{6A07739D-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 1
    EndBit = 1
    Left = 152
    Top = 72
  end
  object Q0_2: TTagBit
    TagGUID = '{6A08AE9F-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 2
    EndBit = 2
    Left = 152
    Top = 104
  end
  object Q0_3: TTagBit
    TagGUID = '{6A0A19E2-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 3
    EndBit = 3
    Left = 152
    Top = 136
  end
  object Q0_4: TTagBit
    TagGUID = '{6A0B9F42-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 4
    EndBit = 4
    Left = 152
    Top = 168
  end
  object Q0_5: TTagBit
    TagGUID = '{6A0D006F-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 5
    EndBit = 5
    Left = 152
    Top = 200
  end
  object Q0_6: TTagBit
    TagGUID = '{6A0E70F4-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 6
    EndBit = 6
    Left = 152
    Top = 232
  end
  object Q0_7: TTagBit
    TagGUID = '{6A0FC4E7-93AA-11DF-AA46-001BFC644612}'
    PLCTag = OutputsBYTE_01
    UseRawValue = False
    StartBit = 7
    EndBit = 7
    Left = 152
    Top = 264
  end
  object Counters: TPLCBlock
    TagGUID = '{72AFD62A-73A8-41C5-BE62-23F8617ED7E0}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 2
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 5
    MemWriteFunction = 0
    ProtocolDriver = ISOTCPDriver1
    RefreshTime = 8
    Size = 10
    TagType = pttWord
    SwapBytes = True
    Left = 184
    Top = 8
  end
  object C0: TPLCBlockElement
    TagGUID = '{3E3A7078-DEA6-4E0F-A828-8CE7D1EE5C1D}'
    PLCBlock = Counters
    Index = 0
    Left = 184
    Top = 40
  end
  object C1: TPLCBlockElement
    TagGUID = '{8A89DCF5-2915-406A-93E7-9FBCB13B73F1}'
    PLCBlock = Counters
    Index = 1
    Left = 184
    Top = 72
  end
  object C2: TPLCBlockElement
    TagGUID = '{AB16A901-DBDB-466A-9C32-BEBB7838F5B3}'
    PLCBlock = Counters
    Index = 2
    Left = 184
    Top = 104
  end
  object C3: TPLCBlockElement
    TagGUID = '{1A8DC46B-99C9-4761-AC48-E4FB99044627}'
    PLCBlock = Counters
    Index = 3
    Left = 184
    Top = 136
  end
  object C4: TPLCBlockElement
    TagGUID = '{19A280ED-CDE1-49B9-91F2-379D4E414920}'
    PLCBlock = Counters
    Index = 4
    Left = 184
    Top = 168
  end
  object C5: TPLCBlockElement
    TagGUID = '{4121C494-628D-4555-A2E6-C916E06D0865}'
    PLCBlock = Counters
    Index = 5
    Left = 184
    Top = 200
  end
  object C6: TPLCBlockElement
    TagGUID = '{720392A2-3256-4F51-B810-3EC5DC06A82D}'
    PLCBlock = Counters
    Index = 6
    Left = 184
    Top = 232
  end
  object C7: TPLCBlockElement
    TagGUID = '{D56E507F-1DA5-4685-AF22-9C09122BD790}'
    PLCBlock = Counters
    Index = 7
    Left = 184
    Top = 264
  end
  object C8: TPLCBlockElement
    TagGUID = '{2EE10182-3ACC-40B5-8E03-C607794B9766}'
    PLCBlock = Counters
    Index = 8
    Left = 184
    Top = 296
  end
  object C9: TPLCBlockElement
    TagGUID = '{86F8C967-D69D-42E6-A56F-D63680655D0C}'
    PLCBlock = Counters
    Index = 9
    Left = 184
    Top = 328
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 24
    Top = 72
  end
end
