object Form1: TForm1
  Left = 191
  Top = 106
  Width = 628
  Height = 411
  Caption = 'IsoTCP Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 429
    Height = 15
    Caption = 
      'Crie no seu PLC o DB1 com 40 bytes de tamanho (10 DWORDs ou  10 ' +
      'Floats)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 412
    Height = 15
    Caption = 
      'Create the DB1 in your PLC with 40 bytes sized (10 DWORDs or 10 ' +
      'Floats)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 80
    Top = 70
    Width = 112
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'DB1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 0
    Top = 88
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label5: TLabel
    Left = 0
    Top = 112
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 4'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label6: TLabel
    Left = 0
    Top = 136
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 8'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label7: TLabel
    Left = 0
    Top = 160
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 12'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label8: TLabel
    Left = 0
    Top = 184
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 16'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label9: TLabel
    Left = 0
    Top = 208
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 20'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label10: TLabel
    Left = 0
    Top = 232
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 24'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label11: TLabel
    Left = 0
    Top = 256
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 28'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label12: TLabel
    Left = 0
    Top = 280
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 32'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label13: TLabel
    Left = 0
    Top = 304
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 36'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label24: TLabel
    Left = 280
    Top = 70
    Width = 112
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Flags (M)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label25: TLabel
    Left = 8
    Top = 45
    Width = 167
    Height = 13
    Caption = 'TCP_UDP Port Commands/second'
    Color = clBtnFace
    ParentColor = False
  end
  object Label26: TLabel
    Left = 214
    Top = 45
    Width = 56
    Height = 14
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label27: TLabel
    Left = 273
    Top = 45
    Width = 30
    Height = 13
    Caption = 'cmd/s'
    Color = clBtnFace
    ParentColor = False
  end
  object HMIText1: THMIText
    Left = 552
    Top = 0
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_0
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText2: THMIText
    Left = 552
    Top = 24
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_1
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText3: THMIText
    Left = 552
    Top = 48
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_2
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText4: THMIText
    Left = 552
    Top = 72
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_3
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText5: THMIText
    Left = 552
    Top = 94
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_4
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText6: THMIText
    Left = 552
    Top = 118
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_5
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText7: THMIText
    Left = 552
    Top = 144
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_6
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText8: THMIText
    Left = 552
    Top = 168
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.I0_7
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object Label28: TLabel
    Left = 576
    Top = 0
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_0'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label29: TLabel
    Left = 576
    Top = 24
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_1'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label30: TLabel
    Left = 576
    Top = 48
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_2'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label31: TLabel
    Left = 576
    Top = 72
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_3'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label32: TLabel
    Left = 576
    Top = 94
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_4'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label33: TLabel
    Left = 576
    Top = 118
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_5'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label34: TLabel
    Left = 576
    Top = 144
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I3_6'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label35: TLabel
    Left = 576
    Top = 168
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'I7_7'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object HMIText9: THMIText
    Left = 552
    Top = 192
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_0
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText10: THMIText
    Left = 552
    Top = 216
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_1
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText11: THMIText
    Left = 552
    Top = 242
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_2
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText12: THMIText
    Left = 552
    Top = 266
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_3
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText13: THMIText
    Left = 552
    Top = 288
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_4
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText14: THMIText
    Left = 552
    Top = 312
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_5
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText15: THMIText
    Left = 552
    Top = 338
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_6
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object HMIText16: THMIText
    Left = 552
    Top = 362
    Width = 17
    Height = 17
    Color = clSilver
    ParentColor = False
    Transparent = False
    NumberFormat = '#0.0'
    PLCTag = DataModule2.Q0_7
    Zones = <
      item
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clSilver
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Value1 = 1.000000000000000000
        Value2 = 1.000000000000000000
        IncludeValue1 = False
        IncludeValue2 = False
        BlinkTime = 0
        BlinkWith = -1
        DefaultZone = False
        ZoneType = ztEqual
        Color = clLime
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
  end
  object Label36: TLabel
    Left = 576
    Top = 192
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_0'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label37: TLabel
    Left = 576
    Top = 216
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_1'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label38: TLabel
    Left = 576
    Top = 242
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_2'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label39: TLabel
    Left = 576
    Top = 266
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_3'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label40: TLabel
    Left = 576
    Top = 288
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_4'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label41: TLabel
    Left = 576
    Top = 312
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_5'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label42: TLabel
    Left = 576
    Top = 338
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_6'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label43: TLabel
    Left = 576
    Top = 362
    Width = 32
    Height = 17
    AutoSize = False
    Caption = 'Q7_7'
    Color = clBtnFace
    ParentColor = False
    Layout = tlCenter
  end
  object Label44: TLabel
    Left = 398
    Top = 88
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label45: TLabel
    Left = 398
    Top = 112
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label46: TLabel
    Left = 398
    Top = 136
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C2'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label47: TLabel
    Left = 398
    Top = 160
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C3'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label48: TLabel
    Left = 398
    Top = 184
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C4'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label49: TLabel
    Left = 398
    Top = 208
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C5'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label50: TLabel
    Left = 398
    Top = 232
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C6'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label51: TLabel
    Left = 398
    Top = 256
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C7'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label52: TLabel
    Left = 398
    Top = 280
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C8'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label53: TLabel
    Left = 398
    Top = 304
    Width = 35
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'C9'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label54: TLabel
    Left = 433
    Top = 70
    Width = 112
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Counters (C)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label14: TLabel
    Left = 200
    Top = 88
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label15: TLabel
    Left = 200
    Top = 112
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 4'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label16: TLabel
    Left = 200
    Top = 136
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 8'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label17: TLabel
    Left = 200
    Top = 160
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 12'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label18: TLabel
    Left = 200
    Top = 184
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 16'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label19: TLabel
    Left = 200
    Top = 208
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 20'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label20: TLabel
    Left = 200
    Top = 232
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 24'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label21: TLabel
    Left = 200
    Top = 256
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 28'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label22: TLabel
    Left = 200
    Top = 280
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 32'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label23: TLabel
    Left = 200
    Top = 304
    Width = 80
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'DWORD 36'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object HMIEdit1: THMIEdit
    Left = 80
    Top = 88
    Width = 112
    Height = 21
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD00
    FreezeValueOnFocus = False
  end
  object HMIEdit2: THMIEdit
    Left = 80
    Top = 112
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 1
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD04
    FreezeValueOnFocus = False
  end
  object HMIEdit3: THMIEdit
    Left = 80
    Top = 136
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 2
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD08
    FreezeValueOnFocus = False
  end
  object HMIEdit4: THMIEdit
    Left = 80
    Top = 160
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 3
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD12
    FreezeValueOnFocus = False
  end
  object HMIEdit5: THMIEdit
    Left = 80
    Top = 184
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 4
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD16
    FreezeValueOnFocus = False
  end
  object HMIEdit6: THMIEdit
    Left = 80
    Top = 208
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 5
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD20
    FreezeValueOnFocus = False
  end
  object HMIEdit7: THMIEdit
    Left = 80
    Top = 232
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 6
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD24
    FreezeValueOnFocus = False
  end
  object HMIEdit8: THMIEdit
    Left = 80
    Top = 256
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 7
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD28
    FreezeValueOnFocus = False
  end
  object HMIEdit9: THMIEdit
    Left = 80
    Top = 280
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 8
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD32
    FreezeValueOnFocus = False
  end
  object HMIEdit10: THMIEdit
    Left = 80
    Top = 304
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 9
    NumberFormat = '#0.0'
    PLCTag = DataModule2.DB1_DBD36
    FreezeValueOnFocus = False
  end
  object HMIEdit11: THMIEdit
    Left = 280
    Top = 88
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 10
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD00
    FreezeValueOnFocus = False
  end
  object HMIEdit12: THMIEdit
    Left = 280
    Top = 112
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 11
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD04
    FreezeValueOnFocus = False
  end
  object HMIEdit13: THMIEdit
    Left = 280
    Top = 136
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 12
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD08
    FreezeValueOnFocus = False
  end
  object HMIEdit14: THMIEdit
    Left = 280
    Top = 160
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 13
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD12
    FreezeValueOnFocus = False
  end
  object HMIEdit15: THMIEdit
    Left = 280
    Top = 184
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 14
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD16
    FreezeValueOnFocus = False
  end
  object HMIEdit16: THMIEdit
    Left = 280
    Top = 208
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 15
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD20
    FreezeValueOnFocus = False
  end
  object HMIEdit17: THMIEdit
    Left = 280
    Top = 232
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 16
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD24
    FreezeValueOnFocus = False
  end
  object HMIEdit18: THMIEdit
    Left = 280
    Top = 256
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 17
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD28
    FreezeValueOnFocus = False
  end
  object HMIEdit19: THMIEdit
    Left = 280
    Top = 280
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 18
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD32
    FreezeValueOnFocus = False
  end
  object HMIEdit20: THMIEdit
    Left = 280
    Top = 304
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 19
    NumberFormat = '#0.0'
    PLCTag = DataModule2.MD36
    FreezeValueOnFocus = False
  end
  object HMIEdit21: THMIEdit
    Left = 433
    Top = 88
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 20
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C0
    FreezeValueOnFocus = False
  end
  object HMIEdit22: THMIEdit
    Left = 433
    Top = 112
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 21
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C1
    FreezeValueOnFocus = False
  end
  object HMIEdit23: THMIEdit
    Left = 433
    Top = 136
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 22
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C2
    FreezeValueOnFocus = False
  end
  object HMIEdit24: THMIEdit
    Left = 433
    Top = 160
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 23
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C3
    FreezeValueOnFocus = False
  end
  object HMIEdit25: THMIEdit
    Left = 433
    Top = 184
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 24
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C4
    FreezeValueOnFocus = False
  end
  object HMIEdit26: THMIEdit
    Left = 433
    Top = 208
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 25
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C5
    FreezeValueOnFocus = False
  end
  object HMIEdit27: THMIEdit
    Left = 433
    Top = 232
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 26
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C6
    FreezeValueOnFocus = False
  end
  object HMIEdit28: THMIEdit
    Left = 433
    Top = 256
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 27
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C7
    FreezeValueOnFocus = False
  end
  object HMIEdit29: THMIEdit
    Left = 433
    Top = 280
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 28
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C8
    FreezeValueOnFocus = False
  end
  object HMIEdit30: THMIEdit
    Left = 433
    Top = 304
    Width = 112
    Height = 21
    Color = clWindow
    TabOrder = 29
    NumberFormat = '#0.0'
    PLCTag = DataModule2.C9
    FreezeValueOnFocus = False
  end
  object Button1: TButton
    Left = 48
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 30
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 520
    Top = 32
  end
  object HMIControlDislocatorAnimation1: THMIControlDislocatorAnimation
    P0_X = 48
    P0_Y = 344
    P1_X = 440
    P1_Y = 344
    ValueP1 = 100000.000000000000000000
    PLCTag = DataModule2.DB1_DBD00
    Control = Button1
    EnableXMin = True
    EnableXMax = True
    EnableYMin = False
    EnableYMax = False
    MinXValue = 2
    MaxXValue = 440
    MinYValue = 0
    MaxYValue = 0
    Gets_P0_Position = 'Gets initial position'
    Gets_P1_Position = 'Gets final position'
    GoTo_P0_Position = 'Controls go to P0 position'
    Left = 104
    Top = 344
  end
end
