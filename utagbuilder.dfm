object frmModbusTagBuilder: TfrmModbusTagBuilder
  Left = 181
  Top = 109
  BorderStyle = bsDialog
  Caption = 'Modbus Tag Builder'
  ClientHeight = 380
  ClientWidth = 593
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 593
    Height = 333
    ActivePage = TabSheet2
    Align = alClient
    MultiLine = True
    ParentShowHint = False
    RaggedRight = True
    ShowHint = True
    TabOrder = 0
    TabPosition = tpBottom
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Inicio'
      object txtTagType: TLabel
        Left = 8
        Top = 72
        Width = 409
        Height = 13
        AutoSize = False
        Caption = 'Qual o tipo de mem'#243'ria voc'#234' deseja criar como tag?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtMemCount: TLabel
        Left = 8
        Top = 200
        Width = 185
        Height = 13
        AutoSize = False
        Caption = 'Quantos tags deseja criar?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtFirstMemAddress: TLabel
        Left = 8
        Top = 256
        Width = 409
        Height = 13
        AutoSize = False
        Caption = 'Qual o endere'#231'o da primeira mem'#243'ria que se deseja mapear?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtStationAddress: TLabel
        Left = 8
        Top = 8
        Width = 529
        Height = 33
        AutoSize = False
        Caption = 
          'Qual o endere'#231'o (n'#243') da esta'#231#227'o (CLP) que contem as mem'#243'rias que' +
          ' se deseja mapear?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Type1: TRadioButton
        Tag = 2000
        Left = 12
        Top = 96
        Width = 113
        Height = 17
        Caption = 'Coils (Outputs)'
        TabOrder = 0
      end
      object Type2: TRadioButton
        Tag = 2000
        Left = 12
        Top = 120
        Width = 113
        Height = 17
        Caption = 'Inputs'
        TabOrder = 1
      end
      object Type3: TRadioButton
        Tag = 125
        Left = 12
        Top = 144
        Width = 113
        Height = 17
        Caption = 'Holding Registers'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object Type4: TRadioButton
        Tag = 125
        Left = 12
        Top = 168
        Width = 113
        Height = 17
        Caption = 'Input Registers'
        TabOrder = 3
      end
      object MemCount: TSpinEdit
        Left = 8
        Top = 224
        Width = 121
        Height = 22
        MaxValue = 10000
        MinValue = 1
        TabOrder = 4
        Value = 1
      end
      object FirstMemAddress: TSpinEdit
        Left = 8
        Top = 280
        Width = 121
        Height = 22
        MaxValue = 65535
        MinValue = 0
        TabOrder = 5
        Value = 1
      end
      object StationAddress: TSpinEdit
        Left = 8
        Top = 40
        Width = 121
        Height = 22
        MaxValue = 255
        MinValue = 1
        TabOrder = 6
        Value = 1
      end
      object optStartFromZero: TCheckBox
        Left = 144
        Top = 226
        Width = 185
        Height = 17
        Caption = 'Iniciar a contagem a partir do zero'
        TabOrder = 7
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Tipo de tag usado'
      ImageIndex = 1
      object Label2: TLabel
        Left = 8
        Top = 16
        Width = 409
        Height = 13
        AutoSize = False
        Caption = 'Qual o tipo de tag deseja usar para mapear essas mem'#243'rias?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtMaxStringSize: TLabel
        Left = 24
        Top = 200
        Width = 201
        Height = 13
        AutoSize = False
        Caption = 'Qual o tamanho m'#225'ximo da string'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtMaxBlockSize: TLabel
        Left = 24
        Top = 120
        Width = 193
        Height = 13
        AutoSize = False
        Caption = 'Qual o tamanho m'#225'ximo do bloco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtStringFormat: TLabel
        Left = 24
        Top = 248
        Width = 143
        Height = 13
        AutoSize = False
        Caption = 'Qual o formato da string'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtStringByteSize: TLabel
        Left = 224
        Top = 248
        Width = 201
        Height = 13
        AutoSize = False
        Caption = 'Tamanho de cada byte da string'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object txtScanOfEachBlock: TLabel
        Left = 240
        Top = 120
        Width = 193
        Height = 13
        AutoSize = False
        Caption = 'Qual o Scan de cada bloco?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object optPLCTagNumber: TRadioButton
        Left = 12
        Top = 40
        Width = 113
        Height = 17
        Caption = 'TPLCTagNumber'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = optPLCTagNumberClick
      end
      object optPLCBlock: TRadioButton
        Left = 12
        Top = 96
        Width = 113
        Height = 17
        Caption = 'TPLCBlock'
        TabOrder = 1
        OnClick = optPLCTagNumberClick
      end
      object optPLCString: TRadioButton
        Left = 12
        Top = 176
        Width = 113
        Height = 17
        Caption = 'TPLCString'
        TabOrder = 2
        OnClick = optPLCTagNumberClick
      end
      object optSimpleFunctions: TCheckBox
        Left = 32
        Top = 60
        Width = 289
        Height = 17
        Caption = 'Usar as fun'#231#245'es de escrita simples'
        TabOrder = 3
      end
      object MaxStringSize: TSpinEdit
        Left = 24
        Top = 216
        Width = 121
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 4
        Value = 1
      end
      object MaxBlockSize: TSpinEdit
        Left = 24
        Top = 136
        Width = 121
        Height = 22
        MaxValue = 125
        MinValue = 1
        TabOrder = 5
        Value = 1
      end
      object Panel2: TPanel
        Left = 16
        Top = 264
        Width = 198
        Height = 30
        BevelOuter = bvNone
        TabOrder = 6
        object optSTR_C: TRadioButton
          Left = 16
          Top = 8
          Width = 65
          Height = 17
          Caption = 'String C'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object optSTR_SIEMENS: TRadioButton
          Left = 96
          Top = 8
          Width = 105
          Height = 17
          Caption = 'String SIEMENS'
          TabOrder = 1
        end
      end
      object ByteSize: TSpinEdit
        Left = 224
        Top = 270
        Width = 121
        Height = 22
        MaxValue = 8
        MinValue = 7
        TabOrder = 7
        Value = 8
      end
      object ScanOfEachBlock: TSpinEdit
        Left = 240
        Top = 136
        Width = 121
        Height = 22
        MaxValue = 125
        MinValue = 7200000
        TabOrder = 8
        Value = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Estrutura de nomeacao'
      ImageIndex = 2
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 585
        Height = 307
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 585
          Height = 29
          Align = alTop
          BevelOuter = bvNone
          Color = clAppWorkSpace
          TabOrder = 0
          object Label1: TLabel
            Left = 0
            Top = 0
            Width = 160
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'Nome do Tag'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Label3: TLabel
            Left = 160
            Top = 0
            Width = 41
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'Contar Vazio'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Label4: TLabel
            Left = 201
            Top = 0
            Width = 51
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'Scan (ms)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Label5: TLabel
            Left = 252
            Top = 0
            Width = 43
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'Zero Fill'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Label6: TLabel
            Left = 295
            Top = 0
            Width = 55
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'Qtd. de digitos'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Label7: TLabel
            Left = 350
            Top = 0
            Width = 127
            Height = 29
            Align = alLeft
            Alignment = taCenter
            AutoSize = False
            Caption = 'PIPE de escalas'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            WordWrap = True
          end
          object Button1: TButton
            Left = 512
            Top = 2
            Width = 50
            Height = 25
            Caption = 'Add'
            TabOrder = 0
            OnClick = Button1Click
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 333
    Width = 593
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 33
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnPrior: TButton
      Left = 332
      Top = 8
      Width = 75
      Height = 33
      Cancel = True
      Caption = 'Anterior'
      Enabled = False
      TabOrder = 1
      OnClick = btnPriorClick
    end
    object btnNext: TButton
      Left = 412
      Top = 8
      Width = 75
      Height = 33
      Cancel = True
      Caption = 'Pr'#243'xima'
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnFinish: TButton
      Left = 492
      Top = 8
      Width = 75
      Height = 33
      Cancel = True
      Caption = 'Concluir'
      Enabled = False
      ModalResult = 1
      TabOrder = 3
      OnClick = btnFinishClick
    end
  end
end
