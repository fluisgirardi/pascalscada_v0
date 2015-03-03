object frmUserAuthentication: TfrmUserAuthentication
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'User Authentication'
  ClientHeight = 146
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = '&Username'
    FocusControl = edtusername
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 46
    Height = 13
    Caption = '&Password'
    FocusControl = edtPassword
  end
  object edtusername: TEdit
    Left = 8
    Top = 24
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object edtPassword: TEdit
    Left = 8
    Top = 72
    Width = 185
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 111
    Width = 200
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel2: TPanel
      Left = 15
      Top = 0
      Width = 185
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOk: TBitBtn
        Left = 16
        Top = 6
        Width = 81
        Height = 25
        TabOrder = 0
        Kind = bkOK
      end
      object btnCancel: TBitBtn
        Left = 104
        Top = 6
        Width = 75
        Height = 25
        TabOrder = 1
        Kind = bkCancel
      end
    end
  end
end
