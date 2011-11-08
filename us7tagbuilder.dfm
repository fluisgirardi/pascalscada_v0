object frmS7TagBuilder: TfrmS7TagBuilder
  Left = 285
  Top = 113
  BorderStyle = bsDialog
  Caption = 'Siemens S7 Tag builder..'
  ClientHeight = 441
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 756
    Height = 404
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 756
      Height = 404
      ActivePage = TabSheet4
      Align = alClient
      TabOrder = 0
      OnChanging = PageControl1Changing
      object TabSheet1: TTabSheet
        Caption = 'Area and type'
        OnShow = TabSheet1Show
        object MemoryArea: TRadioGroup
          Left = 0
          Top = 57
          Width = 748
          Height = 144
          Align = alTop
          Caption = 'Qual '#233' a '#225'rea de mem'#243'ria que deseja endere'#231'ar'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Digital Inputs, S7 200/300/400/1200'
            'Digital Outputs, S7 200/300/400/1200'
            'Flags, M'#39's, S7 200/300/400/1200'
            'DB'#39's, S7 300/400/1200'
            'Counter, S7 300/400/1200'
            'Timer, S7 300/400/1200'
            'Special Memory, SM, S7-200'
            'Analog Input, S7-200'
            'Analog output, S7-200'
            'Counter, S7-200'
            'Timer, S7-200                                   '
            'Analog Input (PIW), S7-300/400/1200'
            'VB, VW, VD, S7-200')
          TabOrder = 1
          OnClick = MemoryAreaClick
        end
        object Panel3: TPanel
          Left = 0
          Top = 201
          Width = 748
          Height = 6
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 748
          Height = 57
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object PLCAddress: TGroupBox
            Left = 0
            Top = 0
            Width = 748
            Height = 53
            Align = alTop
            Caption = 'Endere'#231'o do CLP'
            TabOrder = 0
            object Label1: TLabel
              Left = 8
              Top = 20
              Width = 61
              Height = 22
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'PLCStation'
              Layout = tlCenter
            end
            object Label2: TLabel
              Left = 168
              Top = 20
              Width = 53
              Height = 22
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'PLCStlot'
              Layout = tlCenter
            end
            object Label3: TLabel
              Left = 320
              Top = 20
              Width = 62
              Height = 22
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'PLCRack'
              Layout = tlCenter
            end
            object PLCStation: TSpinEdit
              Left = 73
              Top = 20
              Width = 73
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 0
              Value = 2
            end
            object PLCSlot: TSpinEdit
              Left = 225
              Top = 20
              Width = 73
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object PLCRack: TSpinEdit
              Left = 385
              Top = 20
              Width = 73
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
          end
        end
        object grptagtype: TGroupBox
          Left = 0
          Top = 207
          Width = 748
          Height = 169
          Align = alClient
          Caption = 
            'Qual o tipo do tag que deseja usar para endere'#231'ar a(s) memoria(s' +
            ') selecionada(s)'
          TabOrder = 3
          object lblBlockType: TLabel
            Left = 56
            Top = 72
            Width = 66
            Height = 21
            AutoSize = False
            Caption = 'Tipo de dado:'
            Layout = tlCenter
          end
          object lblBlockScan: TLabel
            Left = 365
            Top = 72
            Width = 66
            Height = 21
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Scan (ms):'
            Transparent = True
            Layout = tlCenter
          end
          object lblStructScan: TLabel
            Left = 30
            Top = 140
            Width = 66
            Height = 21
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Scan (ms):'
            Transparent = True
            Layout = tlCenter
          end
          object optplctagnumber: TRadioButton
            Left = 16
            Top = 24
            Width = 177
            Height = 17
            Caption = 'PLCTagNumber (tags separados)'
            TabOrder = 0
            OnClick = optplcblockClick
          end
          object optplcblock: TRadioButton
            Left = 16
            Top = 48
            Width = 265
            Height = 17
            Caption = 'PLCBlock (tag agrupados do mesmo tipo de dados)'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = optplcblockClick
          end
          object optplcStruct: TRadioButton
            Left = 16
            Top = 120
            Width = 291
            Height = 17
            Caption = 'PLCStruck (tags agrupados de diferentes tipos de dados)'
            TabOrder = 3
            OnClick = optplcblockClick
          end
          object BlockType: TComboBox
            Left = 128
            Top = 72
            Width = 225
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 2
            Text = 'pttDefault,  Protocol defined word size'
            OnChange = BlockTypeChange
            Items.Strings = (
              'pttDefault,  Protocol defined word size'
              'pttShortInt, Integer, 8 bits, signaled'
              'pttByte,      Integer, 8 bits, unsignaled'
              'pttSmallInt, Integer, 16 bits, signaled'
              'pttWord,     Integer, 16 bits, unsignaled'
              'pttInteger,   Integer, 32 bits, signaled'
              'pttDWord,   Integer, 32 bits, unsignaled'
              'pttFloat       32 bits real')
          end
          object BlockScan: TSpinEdit
            Left = 432
            Top = 72
            Width = 81
            Height = 22
            MaxValue = 2147483647
            MinValue = 1
            TabOrder = 4
            Value = 1000
          end
          object BlockSwapBytes: TCheckBox
            Left = 128
            Top = 98
            Width = 97
            Height = 17
            Caption = 'Swap bytes'
            Checked = True
            State = cbChecked
            TabOrder = 5
          end
          object BlockSwapWords: TCheckBox
            Left = 232
            Top = 98
            Width = 97
            Height = 17
            Caption = 'Swap words'
            Checked = True
            State = cbChecked
            TabOrder = 6
          end
          object StructScan: TSpinEdit
            Left = 97
            Top = 140
            Width = 81
            Height = 22
            MaxValue = 2147483647
            MinValue = 1
            TabOrder = 7
            Value = 1000
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Items declaration'
        ImageIndex = 3
        OnShow = TabSheet4Show
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 748
          Height = 71
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblNumItems: TLabel
            Left = 24
            Top = 2
            Width = 167
            Height = 22
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'N'#250'mero de estruturas a criar'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object lblStartAddress: TLabel
            Left = -1
            Top = 26
            Width = 191
            Height = 22
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Endere'#231'o inicial do primeiro item'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object lblDBNumber: TLabel
            Left = 271
            Top = 26
            Width = 102
            Height = 22
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'N'#250'mero do DB'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Label28: TLabel
            Left = 3
            Top = 56
            Width = 152
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'Item Name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label29: TLabel
            Left = 155
            Top = 56
            Width = 78
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'Item Type'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label30: TLabel
            Left = 239
            Top = 56
            Width = 54
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'Scan'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label31: TLabel
            Left = 298
            Top = 56
            Width = 101
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'SWAP'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label32: TLabel
            Left = 397
            Top = 56
            Width = 45
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'SKIP'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblBlockName: TLabel
            Left = 275
            Top = 2
            Width = 97
            Height = 22
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Nome do bloco'
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
          object lblDBNumber1: TLabel
            Left = 464
            Top = 26
            Width = 97
            Height = 22
            Alignment = taCenter
            AutoSize = False
            Caption = 'to DB number'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object spinNumItens: TSpinEdit
            Left = 192
            Top = 2
            Width = 73
            Height = 22
            MaxValue = 100000000
            MinValue = 1
            TabOrder = 0
            Value = 1
            OnChange = spinStartAddressChange
          end
          object spinStartAddress: TSpinEdit
            Left = 192
            Top = 26
            Width = 73
            Height = 22
            MaxValue = 100000000
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnChange = spinStartAddressChange
          end
          object spinDBNumber: TSpinEdit
            Left = 375
            Top = 26
            Width = 90
            Height = 22
            MaxValue = 65535
            MinValue = 1
            TabOrder = 3
            Value = 1
            OnChange = spinDBNumberChange
          end
          object Button1: TButton
            Left = 656
            Top = 8
            Width = 88
            Height = 49
            Caption = '&Add structitem'
            TabOrder = 4
            WordWrap = True
            OnClick = Button1Click
          end
          object BlockName: TEdit
            Left = 375
            Top = 2
            Width = 274
            Height = 21
            TabOrder = 2
            Text = 'BlockName'
          end
          object spinFinalDBNumber: TSpinEdit
            Left = 559
            Top = 26
            Width = 90
            Height = 22
            MaxValue = 65535
            MinValue = 1
            TabOrder = 5
            Value = 1
            OnChange = spinFinalDBNumberChange
          end
        end
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 71
          Width = 748
          Height = 305
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 404
    Width = 756
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TButton
      Left = 416
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnBack: TButton
      Left = 496
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Back'
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 576
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnFinish: TButton
      Left = 656
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Finish'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5
    OnTimer = Timer1Timer
    Left = 276
    Top = 40
  end
end
