object frmS7TagBuilder: TfrmS7TagBuilder
  Left = 191
  Top = 104
  BorderStyle = bsDialog
  Caption = 'Siemens S7 Tag builder..'
  ClientHeight = 425
  ClientWidth = 595
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 595
    Height = 388
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label24: TLabel
      Left = 296
      Top = 8
      Width = 38
      Height = 13
      Caption = 'Label24'
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 595
      Height = 388
      ActivePage = TabSheet4
      Align = alClient
      TabOrder = 0
      OnChange = PageControl1Change
      OnChanging = PageControl1Changing
      object TabSheet1: TTabSheet
        Caption = 'Area e Tipo'
        OnShow = TabSheet1Show
        object MemoryArea: TRadioGroup
          Left = 0
          Top = 67
          Width = 587
          Height = 104
          Align = alTop
          Caption = 'Qual '#233' a '#225'rea de mem'#243'ria que deseja endere'#231'ar'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'Inputs, Entradas'
            'Outputs, Saidas'
            'Flags ou M'#39's'
            'DB'#180's'
            'Counter, S7 300/400'
            'Timer, S7 300/400')
          TabOrder = 1
          OnClick = MemoryAreaClick
        end
        object Panel3: TPanel
          Left = 0
          Top = 171
          Width = 587
          Height = 11
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 587
          Height = 67
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object PLCAddress: TGroupBox
            Left = 0
            Top = 0
            Width = 587
            Height = 60
            Align = alTop
            Caption = 'Endere'#231'o do CLP'
            TabOrder = 0
            object Label1: TLabel
              Left = 8
              Top = 24
              Width = 53
              Height = 22
              AutoSize = False
              Caption = 'PLCStation'
              Layout = tlCenter
            end
            object Label2: TLabel
              Left = 176
              Top = 24
              Width = 45
              Height = 22
              AutoSize = False
              Caption = 'PLCStlot'
              Layout = tlCenter
            end
            object Label3: TLabel
              Left = 333
              Top = 24
              Width = 49
              Height = 22
              AutoSize = False
              Caption = 'PLCRack'
              Layout = tlCenter
            end
            object PLCStation: TSpinEdit
              Left = 65
              Top = 24
              Width = 73
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 0
              Value = 2
            end
            object PLCSlot: TSpinEdit
              Left = 225
              Top = 24
              Width = 73
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object PLCRack: TSpinEdit
              Left = 385
              Top = 24
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
          Top = 182
          Width = 587
          Height = 178
          Align = alClient
          Caption = 
            'Qual o tipo do tag que deseja usar para endere'#231'ar a(s) memoria(s' +
            ') selecionada(s)'
          TabOrder = 3
          object Label25: TLabel
            Left = 56
            Top = 72
            Width = 66
            Height = 21
            AutoSize = False
            Caption = 'Tipo de dado:'
            Layout = tlCenter
          end
          object Label26: TLabel
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
          object Label27: TLabel
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
      object TabSheet2: TTabSheet
        Caption = 'Inputs and Outputs'
        ImageIndex = 1
        OnShow = TabSheet2Show
        object Label4: TLabel
          Left = 8
          Top = 8
          Width = 43
          Height = 13
          Caption = 'Mapear'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 32
          Top = 40
          Width = 76
          Height = 13
          Caption = 'Da entrada byte'
        end
        object Label6: TLabel
          Left = 200
          Top = 40
          Width = 87
          Height = 13
          Caption = 'At'#233' a entrada byte'
        end
        object Label8: TLabel
          Left = 32
          Top = 88
          Width = 17
          Height = 13
          Caption = 'Bits'
        end
        object Label9: TLabel
          Left = 32
          Top = 184
          Width = 81
          Height = 21
          AutoSize = False
          Caption = 'Nome do bloco'
          Layout = tlCenter
        end
        object Label10: TLabel
          Left = 32
          Top = 208
          Width = 81
          Height = 21
          AutoSize = False
          Caption = 'Nome dos bytes'
          Layout = tlCenter
        end
        object Label11: TLabel
          Left = 32
          Top = 232
          Width = 81
          Height = 21
          AutoSize = False
          Caption = 'Nome dos bits'
          Layout = tlCenter
        end
        object Label12: TLabel
          Left = 48
          Top = 268
          Width = 211
          Height = 13
          Caption = '&Preencher com zeros o n'#250'mero do byte atual'
          FocusControl = IOByteNumberZeroFill
        end
        object Label13: TLabel
          Left = 48
          Top = 284
          Width = 171
          Height = 13
          Caption = '&Actual byte number filled with zeroes'
          FocusControl = IOByteNumberZeroFill
        end
        object IOStartByte: TSpinEdit
          Left = 32
          Top = 56
          Width = 121
          Height = 22
          MaxValue = 10000
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = IOStartByteChange
        end
        object IOEndByte: TSpinEdit
          Left = 200
          Top = 56
          Width = 121
          Height = 22
          MaxValue = 10000
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = IOStartByteChange
        end
        object BitList: TCheckListBox
          Left = 32
          Top = 104
          Width = 289
          Height = 65
          OnClickCheck = IOStartByteChange
          Columns = 2
          ItemHeight = 13
          Items.Strings = (
            'Bit 0'
            'Bit 1'
            'Bit 2'
            'Bit 3'
            'Bit 4'
            'Bit 5'
            'Bit 6'
            'Bit 7')
          TabOrder = 2
          OnClick = IOStartByteChange
        end
        object IOBlockName: TEdit
          Left = 112
          Top = 184
          Width = 121
          Height = 21
          TabOrder = 6
          Text = 'Block_%sb_to_%eb'
        end
        object IOByteNames: TEdit
          Left = 112
          Top = 208
          Width = 121
          Height = 21
          TabOrder = 7
          Text = 'I%B'
        end
        object IOBitNames: TEdit
          Left = 112
          Top = 232
          Width = 121
          Height = 21
          TabOrder = 8
          Text = 'I%B_%b'
        end
        object Memo1: TMemo
          Left = 240
          Top = 179
          Width = 273
          Height = 90
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = False
          Lines.Strings = (
            'Padr'#245'es de nomes/Name patterns'
            ''
            '%sb - Byte Inicial/Start byte'
            '%eb - Byte Final/Final byte'
            '%B - N'#250'mero do byte atual/Actual byte number'
            '%b - N'#250'mero do bit atual/Actual bit number')
          ParentCtl3D = False
          ReadOnly = True
          TabOrder = 9
        end
        object IOByteNumberZeroFill: TCheckBox
          Left = 32
          Top = 275
          Width = 17
          Height = 17
          TabOrder = 10
        end
        object btnSelectAll: TButton
          Left = 328
          Top = 104
          Width = 81
          Height = 25
          Caption = 'Select All'
          TabOrder = 3
          OnClick = btnSelectAllClick
        end
        object btnUnselectAll: TButton
          Left = 416
          Top = 104
          Width = 81
          Height = 25
          Caption = 'Unselect All'
          TabOrder = 4
          OnClick = btnUnselectAllClick
        end
        object btnInvertSelection: TButton
          Left = 328
          Top = 136
          Width = 169
          Height = 25
          Caption = 'Invert selection'
          TabOrder = 5
          OnClick = btnInvertSelectionClick
        end
        object IOStatus: TMemo
          Left = 0
          Top = 310
          Width = 587
          Height = 50
          TabStop = False
          Align = alBottom
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Padr'#245'es de nomes/Name patterns'
            ''
            '%sb - Byte Inicial/Start byte'
            '%eb - Byte Final/Final byte'
            '%B - N'#250'mero do byte atual/Actual byte number'
            '%b - N'#250'mero do bit atual/Actual bit number')
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          TabOrder = 11
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Counters and Timers'
        ImageIndex = 2
        OnShow = TabSheet3Show
        object Label14: TLabel
          Left = 8
          Top = 40
          Width = 65
          Height = 13
          Caption = 'Mapear da '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 32
          Top = 72
          Width = 53
          Height = 13
          Caption = 'Do counter'
        end
        object Label16: TLabel
          Left = 200
          Top = 72
          Width = 65
          Height = 13
          Caption = 'At'#233' o Counter'
        end
        object Label17: TLabel
          Left = 160
          Top = 88
          Width = 32
          Height = 22
          Alignment = taCenter
          AutoSize = False
          Caption = 'at'#233
          Layout = tlCenter
        end
        object Label18: TLabel
          Left = 32
          Top = 176
          Width = 81
          Height = 21
          AutoSize = False
          Caption = 'Nome do bloco'
          Layout = tlCenter
        end
        object Label19: TLabel
          Left = 32
          Top = 200
          Width = 81
          Height = 21
          AutoSize = False
          Caption = 'Nome dos bytes'
          Layout = tlCenter
        end
        object Label20: TLabel
          Left = 48
          Top = 260
          Width = 211
          Height = 13
          Caption = '&Preencher com zeros o n'#250'mero do byte atual'
          FocusControl = CTZeroFill
        end
        object Label21: TLabel
          Left = 48
          Top = 276
          Width = 171
          Height = 13
          Caption = '&Actual byte number filled with zeroes'
          FocusControl = CTZeroFill
        end
        object CTStartAddress: TSpinEdit
          Left = 32
          Top = 88
          Width = 121
          Height = 22
          MaxValue = 10000
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = CTStartAddressChange
        end
        object CTEndAddress: TSpinEdit
          Left = 200
          Top = 88
          Width = 121
          Height = 22
          MaxValue = 10000
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = CTStartAddressChange
        end
        object CTBlockName: TEdit
          Left = 112
          Top = 176
          Width = 121
          Height = 21
          TabOrder = 2
          Text = 'Block_%si_to_%ei'
        end
        object CTNames: TEdit
          Left = 112
          Top = 200
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'C%I'
        end
        object Memo2: TMemo
          Left = 240
          Top = 171
          Width = 273
          Height = 78
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = False
          Lines.Strings = (
            'Padr'#245'es de nomes/Name patterns'
            ''
            '%si - Counter ou Timer Inicial/Start Counter or Timer'
            '%ei - Counter ou Timer Final/Final Counter or Timer'
            '%I - Counter ou Timer Atual/Actual Counter or Timer ')
          ParentCtl3D = False
          ReadOnly = True
          TabOrder = 4
        end
        object CTZeroFill: TCheckBox
          Left = 32
          Top = 267
          Width = 17
          Height = 17
          TabOrder = 5
        end
        object CTStatus: TMemo
          Left = 0
          Top = 310
          Width = 587
          Height = 50
          TabStop = False
          Align = alBottom
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Padr'#245'es de nomes/Name patterns'
            ''
            '%sb - Byte Inicial/Start byte'
            '%eb - Byte Final/Final byte'
            '%B - N'#250'mero do byte atual/Actual byte number'
            '%b - N'#250'mero do bit atual/Actual bit number')
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          TabOrder = 6
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'DB'#180's and Flags'
        ImageIndex = 3
        OnShow = TabSheet4Show
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 587
          Height = 71
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label7: TLabel
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
          object Label22: TLabel
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
          object Label23: TLabel
            Left = 288
            Top = 2
            Width = 102
            Height = 22
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Endere'#231'o do DB'
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
          object spinNumStructs: TSpinEdit
            Left = 192
            Top = 2
            Width = 73
            Height = 22
            MaxValue = 100000000
            MinValue = 1
            TabOrder = 0
            Value = 1
          end
          object spinStructStartAddress: TSpinEdit
            Left = 192
            Top = 26
            Width = 73
            Height = 22
            MaxValue = 100000000
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object spinDBNum: TSpinEdit
            Left = 391
            Top = 2
            Width = 73
            Height = 22
            MaxValue = 100000000
            MinValue = 1
            TabOrder = 2
            Value = 1
          end
          object Button1: TButton
            Left = 487
            Top = 6
            Width = 60
            Height = 49
            Caption = '&Add structitem'
            TabOrder = 3
            WordWrap = True
            OnClick = Button1Click
          end
        end
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 71
          Width = 587
          Height = 289
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
    Top = 388
    Width = 595
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TButton
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnBack: TButton
      Left = 352
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Back'
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 432
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnFinish: TButton
      Left = 512
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
    Interval = 10
    OnTimer = Timer1Timer
    Left = 444
    Top = 56
  end
end
