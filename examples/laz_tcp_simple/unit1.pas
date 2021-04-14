unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SerialPort,
  commtypes, tcp_udpport, Strutils;


type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    TCP_UDPPort1: TTCP_UDPPort;
    procedure FormCreate(Sender: TObject);
  private
    Buffer:String;
    FTempStr,FQtde: String;
    FTempPeso,FPeso : real;

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
  TCP_UDPPort1.Host:='192.168.0.1';               //troque conforme necessário
  TCP_UDPPort1.Port:=100;                         //troque conforme necessário
  TCP_UDPPort1.Timeout:=0;                        //deixe zero para não ficar esperando...
  //TCP_UDPPort1.LogIOActions:=true;              //habilite se quiser fazer um log
                                                  //das transações de IO executadas pela porta
  //TCP_UDPPort1.LogFile:='c:\tcpbalanca.txt';    //arquivo que armazenará o log de transações de IO...

  TCP_UDPPort1.Active:=True;  //ative


  if (Application.Flags*[AppDoNotCallAsyncQueue])=[] then begin
    Application.QueueAsyncCall(@VerificaDadosPortaSerial, 0);
  end;
end;

procedure TForm1.VerificaDadosPortaSerial(Data: PtrInt);
var
  pkg: TIOPacket;
  c: Integer;
begin
  try
    TCP_UDPPort1.IOCommandSync(iocRead,  //iremos somente ler bytes da porta serial
                                    0,   //não iremos escrever nenhum byte
                                    nil, //não iremos escrever nada
                                    1,   //vamos ler um byte de cada vez
                                    0,   //driver ID, vc não irá precisar,
                                    0,   //delay entre leitura => escrita e escrita => leitura
                                    @pkg);

    if (pkg.ReadIOResult=iorOK) and (pkg.Received>=1) and (Length(pkg.BufferToRead)>0) then begin
      for c:=0 to High(pkg.BufferToRead) do begin
        if pkg.BufferToRead[c] = $0d then begin
          Edit1.Text:=Buffer; //chame aqui sua rotina de processamento do valor de pesagem.
          case buffer[1] of
            'D': Label1.Caption:='Peso estabilizado';
            'F': Label1.Caption:='Balanca vazia';
            '@': Label1.Caption:='Peso instável/balança em movimento';
          end;
          Buffer:='';
        end else
          Buffer:=Buffer+Char(pkg.BufferToRead[c]);
      end;
    end;
  finally
    Edit1.Text:=Trim(Buffer);
    //enfileira a proxima leitura para ser executda no proximo ciclo de exceução do programa.
    if (Application.Flags*[AppDoNotCallAsyncQueue])=[] then
      Application.QueueAsyncCall(@VerificaDadosPortaSerial, 0);
  end;
end;


end.

