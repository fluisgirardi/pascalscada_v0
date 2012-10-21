object WestTagBuilder: TWestTagBuilder
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'West 6100+ Tag Builder'
  ClientHeight = 448
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 564
    Height = 43
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 13
      Width = 200
      Height = 13
      Caption = 'Mapear variaveis do controlador endere'#231'o'
    end
    object Label2: TLabel
      Left = 271
      Top = 13
      Width = 15
      Height = 13
      Caption = 'at'#233
    end
    object AdrStart: TSpinEdit
      Left = 216
      Top = 10
      Width = 49
      Height = 22
      MaxValue = 99
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object AdrEnd: TSpinEdit
      Left = 292
      Top = 10
      Width = 49
      Height = 22
      MaxValue = 99
      MinValue = 1
      TabOrder = 1
      Value = 1
    end
    object ZeroFill: TCheckBox
      Left = 376
      Top = 1
      Width = 177
      Height = 39
      Caption = 
        'Preencher com zeros o endere'#231'o do controlador nas variaveis cria' +
        'das'
      TabOrder = 2
      WordWrap = True
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 63
    Width = 564
    Height = 350
    Cursor = crArrow
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = True
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 413
    Width = 564
    Height = 35
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      Left = 484
      Top = 6
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 402
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancelar'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 43
    Width = 564
    Height = 20
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label6: TLabel
      Left = 5
      Top = 6
      Width = 47
      Height = 13
      Caption = 'Vari'#225'vel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 196
      Top = 6
      Width = 73
      Height = 13
      Caption = 'Nome do tag'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 402
      Top = 6
      Width = 56
      Height = 13
      Caption = 'Varredura'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
