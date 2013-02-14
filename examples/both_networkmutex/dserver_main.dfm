object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Network Mutex server'
  ClientHeight = 212
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 147
    Height = 195
    Caption = 
      'This application does nothing, '#13#10'it only serve as Mutex Serve.'#13#10 +
      #13#10'To test it, start this application'#13#10'and start how many instanc' +
      'es'#13#10'of the client you want and try'#13#10'use the mutex. Only one '#13#10'ap' +
      'plication will use the mutex.'#13#10#13#10'To test this over the network,'#13 +
      #10'change the property "Host"'#13#10'of the MutexClient1 of the '#13#10'Client' +
      ' application to point to'#13#10'where the server application'#13#10'will run' +
      '.'
    Color = clBtnFace
    ParentColor = False
  end
  object MutexServer1: TMutexServer
    Active = True
    Port = 51342
    Left = 48
    Top = 32
  end
end
