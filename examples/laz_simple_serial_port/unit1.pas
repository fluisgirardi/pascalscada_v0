unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SerialPort,
  commtypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SerialPortDriver1: TSerialPortDriver;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Buffer:String;
    procedure VerificaDadosPortaSerial(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //setup da sua porta serial aqui!
  SerialPortDriver1.AcceptAnyPortName:=false;  //habilita  a validação de porta serial válida (existente no computador)
  try
    SerialPortDriver1.COMPort:='ttyUSB0'; //troque conforme necessário
  except
    showmessage('Porta não existe, abortando!');
    exit;
  end;

  SerialPortDriver1.BaudRate:=br19200;  //troque conforme necessário
  SerialPortDriver1.StopBits:=sb1;      //troque conforme necessário
  SerialPortDriver1.DataBits:=db8;      //troque conforme necessário
  SerialPortDriver1.Paridade:=spEven;   //troque conforme necessário
  SerialPortDriver1.Timeout:=0;         //deixe zero para não ficar esperando...
  //SerialPortDriver1.LogIOActions:=true;  //habilite se quiser fazer um log das transações de IO executadas pela porta
  //SerialPortDriver1.LogFile:='/tmp/serialport.txt';  //arquivo que armazenará o log de transações de IO...

  try
    SerialPortDriver1.Active:=true;  //ative
  except
    showmessage('Falha abrindo a porta serial! Provavelmente ela está em uso por outro processo');
    exit;
  end;

  if (Application.Flags*[AppDoNotCallAsyncQueue])=[] then begin
    Application.QueueAsyncCall(@VerificaDadosPortaSerial, 0);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('teste de latencia! Se este dialogo demorar para abrir, é pq vc deve ter setado um timeout > 0 na sua porta serial');
end;

procedure TForm1.VerificaDadosPortaSerial(Data: PtrInt);
var
  pkg: TIOPacket;
  c: Integer;
begin
  try
    SerialPortDriver1.IOCommandSync(iocRead, //iremos somente ler bytes da porta serial
                                    0,       //não iremos escrever nenhum byte
                                    nil,     //não iremos escrever nada
                                    1,       //vamos ler um byte de cada vez
                                    0,       //driver ID, vc não irá precisar,
                                    0,       //delay entre leitura => escrita e escrita => leitura
                                    @pkg);

    if (pkg.ReadIOResult=iorOK) and (pkg.Received>=1) and (Length(pkg.BufferToRead)>0) then begin
      for c:=0 to High(pkg.BufferToRead) do begin
        Buffer:=Buffer+Char(pkg.BufferToRead[c]);
      end;
      if RightStr(Buffer,2)='#3' then begin
        Caption:=Buffer; //chame aqui sua rotina de processamento do valor de pesagem.
        Buffer:='';
      end;
    end;
  finally
    //enfileira a proxima leitura para ser executda no proximo ciclo de exceução do programa.
    if (Application.Flags*[AppDoNotCallAsyncQueue])=[] then
      Application.QueueAsyncCall(@VerificaDadosPortaSerial, 0);
  end;
end;

end.

