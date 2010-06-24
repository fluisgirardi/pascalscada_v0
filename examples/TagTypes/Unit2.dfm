object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 249
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object HMIEdit1: THMIEdit
    Left = 16
    Top = 40
    Width = 121
    Height = 21
    Color = clWindow
    TabOrder = 0
    NumberFormat = '#0.0'
    PLCTag = PLCBlockElement1
  end
  object HMIEdit2: THMIEdit
    Left = 16
    Top = 67
    Width = 121
    Height = 21
    Color = clWindow
    TabOrder = 1
    NumberFormat = '#0.0'
    PLCTag = PLCBlockElement2
  end
  object TCP_UDPPort1: TTCP_UDPPort
    Active = True
    Host = '10.0.2.2'
    Port = 1502
    Timeout = 50
    Left = 16
    Top = 8
  end
  object ModBusTCPDriver1: TModBusTCPDriver
    CommunicationPort = TCP_UDPPort1
    Left = 48
    Top = 8
  end
  object PLCBlock1: TPLCBlock
    TagGUID = '{A0D02639-8C46-4EA2-807E-B35BB719D05D}'
    PLCHack = 0
    PLCSlot = 0
    PLCStation = 1
    MemFile_DB = 0
    MemAddress = 0
    MemSubElement = 0
    MemReadFunction = 3
    MemWriteFunction = 16
    ProtocolDriver = ModBusTCPDriver1
    RefreshTime = 200
    Size = 2
    TagType = pttByte
    Left = 80
    Top = 8
  end
  object PLCBlockElement1: TPLCBlockElement
    TagGUID = '{0F5D393C-860C-413B-B5B5-4CD4BE2EC064}'
    PLCBlock = PLCBlock1
    Index = 0
    Left = 112
    Top = 8
  end
  object PLCBlockElement2: TPLCBlockElement
    TagGUID = '{0F5D393C-860C-413B-B5B5-4CD4BE2EC064}'
    PLCBlock = PLCBlock1
    Index = 1
    Left = 144
    Top = 8
  end
end
