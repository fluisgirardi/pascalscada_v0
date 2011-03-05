object frmStructureEditor: TfrmStructureEditor
  Left = 190
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Structure editor'
  ClientHeight = 379
  ClientWidth = 712
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 712
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 145
      Height = 22
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'N'#250'mero de estruturas a criar'
      Layout = tlCenter
    end
    object SpinEdit1: TSpinEdit
      Left = 156
      Top = 8
      Width = 121
      Height = 22
      MaxValue = 2147483647
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object Button1: TButton
      Left = 293
      Top = 6
      Width = 116
      Height = 26
      Caption = '&Adicionar um item'
      TabOrder = 1
      OnClick = Button1Click
    end
    object BitBtn1: TBitBtn
      Left = 613
      Top = 6
      Width = 92
      Height = 26
      Caption = 'C&onstruir'
      TabOrder = 3
      OnClick = BitBtn1Click
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 496
      Top = 6
      Width = 92
      Height = 26
      Caption = '&Cancelar'
      TabOrder = 2
      OnClick = BitBtn2Click
      Kind = bkCancel
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 40
    Width = 712
    Height = 339
    Align = alClient
    TabOrder = 1
  end
  object Timer1: TTimer
    Left = 432
    Top = 8
  end
end
