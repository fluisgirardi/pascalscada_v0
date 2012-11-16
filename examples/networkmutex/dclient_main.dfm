object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Network mutex client'
  ClientHeight = 293
  ClientWidth = 319
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
    Left = 144
    Top = 8
    Width = 165
    Height = 221
    Caption = 
      'This application works as mutex,'#13#10'client, it needs another appli' +
      'cation'#13#10'(networkmutex_server) '#13#10'running first.'#13#10#13#10'To test it, st' +
      'art this application'#13#10'and start how many instances'#13#10'of the clien' +
      't you want and try'#13#10'use the mutex. Only one '#13#10'application will u' +
      'se the mutex.'#13#10#13#10'To test this over the network,'#13#10'change the prop' +
      'erty "Host"'#13#10'of the MutexClient1 of this '#13#10'application to point ' +
      'to'#13#10'where the server application'#13#10'will run.'
    Color = clBtnFace
    ParentColor = False
  end
  object Button1: TButton
    Left = 0
    Top = 48
    Width = 136
    Height = 25
    Caption = 'Try enter on mutex'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 0
    Top = 80
    Width = 136
    Height = 25
    Caption = 'Leave the mutex'
    TabOrder = 1
    OnClick = Button2Click
  end
  object MutexClient1: TMutexClient
    Active = True
    Host = '127.0.0.1'
    Port = 51342
    Left = 56
    Top = 8
  end
end
