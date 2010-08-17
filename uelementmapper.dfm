object frmMapElements: TfrmMapElements
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Map block elements...'
  ClientHeight = 154
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 8
    Width = 117
    Height = 13
    Caption = '&Map elements from index'
    FocusControl = startindex
  end
  object Label2: TLabel
    Left = 112
    Top = 24
    Width = 25
    Height = 22
    Alignment = taCenter
    AutoSize = False
    Caption = '&TO'
    FocusControl = endindex
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 0
    Top = 56
    Width = 113
    Height = 13
    Caption = '&Names of new elements'
    FocusControl = elementnames
  end
  object startindex: TSpinEdit
    Left = 0
    Top = 24
    Width = 113
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object endindex: TSpinEdit
    Left = 136
    Top = 24
    Width = 113
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object elementnames: TEdit
    Left = 0
    Top = 72
    Width = 145
    Height = 21
    TabOrder = 2
    Text = '%t_e%e'
  end
  object GroupBox1: TGroupBox
    Left = 152
    Top = 56
    Width = 105
    Height = 57
    Caption = 'Name pattern'
    TabOrder = 3
    object Label4: TLabel
      Left = 2
      Top = 16
      Width = 76
      Height = 13
      Caption = '%t - Block name'
    end
    object Label5: TLabel
      Left = 2
      Top = 32
      Width = 99
      Height = 13
      Caption = '%e - Element number'
    end
  end
  object ElementsStartFromOne: TCheckBox
    Left = 0
    Top = 97
    Width = 151
    Height = 17
    Caption = '&Element number start from 1'
    TabOrder = 4
    WordWrap = True
  end
  object Button1: TButton
    Left = 104
    Top = 128
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 184
    Top = 128
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = Button2Click
  end
end
