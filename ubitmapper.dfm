object Form1: TfrmBitMapper
  Left = 192
  Top = 107
  Width = 688
  Height = 155
  Caption = 'Map bits'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 20
    Height = 13
    Caption = 'Bits:'
  end
  object Label2: TLabel
    Left = 0
    Top = 48
    Width = 108
    Height = 13
    Caption = 'New tag name pattern:'
  end
  object Label3: TLabel
    Left = 352
    Top = 40
    Width = 81
    Height = 13
    Caption = 'Name pattern:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 360
    Top = 56
    Width = 75
    Height = 13
    Caption = '%b - bit number.'
  end
  object Label5: TLabel
    Left = 360
    Top = 73
    Width = 86
    Height = 13
    Caption = '%B - Byte number.'
  end
  object Label6: TLabel
    Left = 360
    Top = 89
    Width = 92
    Height = 13
    Caption = '%w - Word number.'
  end
  object Label7: TLabel
    Left = 360
    Top = 105
    Width = 98
    Height = 13
    Caption = '%t - Owner tag name'
  end
  object StringGrid1: TStringGrid
    Left = 1
    Top = 16
    Width = 675
    Height = 20
    ColCount = 32
    DefaultColWidth = 20
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
    ScrollBars = ssNone
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 0
    Top = 104
    Width = 167
    Height = 17
    Caption = 'Map each bit as individual tag.'
    TabOrder = 1
  end
  object edtNamepattern: TEdit
    Left = 0
    Top = 64
    Width = 161
    Height = 21
    TabOrder = 2
    Text = '%t_bit%b'
  end
  object CheckBox2: TCheckBox
    Left = 176
    Top = 48
    Width = 153
    Height = 17
    Caption = 'Bit name starts from 1'
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 176
    Top = 72
    Width = 153
    Height = 17
    Caption = 'Byte name starts from 1'
    TabOrder = 4
  end
  object CheckBox4: TCheckBox
    Left = 176
    Top = 96
    Width = 153
    Height = 17
    Caption = 'Word name starts from 1'
    TabOrder = 5
  end
  object Button1: TButton
    Left = 600
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 517
    Top = 95
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
    OnClick = Button2Click
  end
end
