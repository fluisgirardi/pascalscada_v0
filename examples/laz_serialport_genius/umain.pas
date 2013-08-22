{
  This program communicates with a game called "Genius" recording the name
  of each player and their punctuation on this game. The game is a third
  part hardware, that sends the latest game number/punctuation every second.

  To see the serial communication, see  TCommThread.Execute on this file.

  YOU WILL NEED THE SQLITE3 LIBRARIES INSTALLED ON YOUR SYSTEM!!!!!

  Author: Fabio Luis Girardi.
}

unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, DbCtrls, StdCtrls, ZConnection, ZDataset, HMILabel, HMIText,
  SerialPort, CrossEvent, syncobjs, commtypes;

type

  TCommState = (csWaitingFirstZero, csWaitingSecondZero, csWaitingValueB1, csWaitingValueB2, csWaitingValueB3, csWaitingValueB4);

  { TCommThread }
  TCommThread = class(TThread)
  protected
    last_game_number,
    cur_game_number,
    cur_game_punctuation: Integer;
    FSerialPort:TSerialPortDriver;
    FEnd:TCrossEvent;
    procedure Execute; override;
    procedure InsertRecord;
  public
    constructor Create(CreateSuspended: Boolean;
                       aSerialPort:TSerialPortDriver;
                       const StackSize: SizeUInt=DefaultStackSize
                      );
    procedure WaitEnd;
  end;

  { TfrmRank }

  TfrmRank = class(TForm)
    DBEdit1: TDBEdit;
    DBGrid1: TDBGrid;
    DBText1: TDBText;
    DBText2: TDBText;
    dsRank: TDatasource;
    HMIText1: THMIText;
    Panel1: TPanel;
    Panel2: TPanel;
    SerialPortDriver1: TSerialPortDriver;
    SQLiteConn: TZConnection;
    tblRank: TZTable;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  protected
    commthread:TCommThread;
  public
    { public declarations }
  end;

var
  frmRank: TfrmRank;

implementation

uses IniFiles;

{$R *.lfm}

{ TCommThread }

procedure TCommThread.Execute;
var
  bytes:array[1..4] of byte;
  Values:array[1..2] of word absolute bytes;
  pkg:TIOPacket;
  clear_buffer: Boolean;
  state: TCommState;
begin

  {
  This thread expects the folowing data on serial port:

  +--------+--------+--------+--------+--------+--------+
  | Byte 1 | Byte 2 | Byte 3 | Byte 4 | Byte 5 | Byte 6 |
  +--------+--------+--------+--------+--------+--------+
  |      Word 1     |      Word 2     |      Word 3     |
  +--------+--------+--------+--------+--------+--------+
  |                 |                 |     Always 0    |
  |   Game number   |   Punctuation   |    Packet end   |
  |                 |                 |    0        0   |
  +--------+--------+--------+--------+--------+--------+
  }

  last_game_number:=0;
  cur_game_number:=0;
  cur_game_punctuation:=0;

  //starts with a empty input buffer...
  clear_buffer:=true;
  state:=csWaitingFirstZero;
  while not Terminated do begin
    if (FSerialPort<>nil) and FSerialPort.Active then begin
      //clears the input buffer...
      if clear_buffer then begin
        repeat
          FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
        until (pkg.ReadIOResult=iorTimeOut) or Terminated; //evita o programa congelar...
        clear_buffer:=false;
        state:=csWaitingFirstZero;
      end else begin
        //finds values and packet end...
        case state of
          //finds the first zero of end mark.
          csWaitingFirstZero: begin
            cur_game_number:=0;
            cur_game_punctuation:=0;
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              if (pkg.BufferToRead[0]=0) then
                state:=csWaitingSecondZero
              else
                clear_buffer:=true;
            end;
          end;

          //finds the second zero of end mark.
          csWaitingSecondZero: begin
            cur_game_number:=0;
            cur_game_punctuation:=0;
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              if (pkg.BufferToRead[0]=0) then
                state:=csWaitingValueB1
              else
                clear_buffer:=true;
            end;
          end;

          csWaitingValueB1: begin
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              bytes[1]:=pkg.BufferToRead[0];
              state:=csWaitingValueB2;
            end;
          end;
          csWaitingValueB2: begin
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              bytes[2]:=pkg.BufferToRead[0];
              if (bytes[1]=0) and (bytes[2]=0) then begin
                clear_buffer:=true;
                Continue;
              end;
              state:=csWaitingValueB3;
            end;
          end;
          csWaitingValueB3: begin
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              bytes[3]:=pkg.BufferToRead[0];
              if (bytes[2]=0) and (bytes[3]=0) then begin
                clear_buffer:=true;
                Continue;
              end;
              state:=csWaitingValueB4;
            end;
          end;
          csWaitingValueB4: begin
            FSerialPort.IOCommandSync(iocRead,0,nil,1,0,0,@pkg,nil,nil);
            if (pkg.ReadIOResult=iorOK) and (Length(pkg.BufferToRead)>0) then begin
              bytes[4]:=pkg.BufferToRead[0];
              if (bytes[3]=0) and (bytes[4]=0) then begin
                clear_buffer:=true;
                Continue;
              end;
              state:=csWaitingFirstZero;
              cur_game_number:=Values[1];
              cur_game_punctuation:=Values[2];
              if cur_game_number<>last_game_number then begin
                Synchronize(@InsertRecord);
                last_game_number:=cur_game_number;
              end;
            end;
          end;
        end;
      end;
      SetLength(pkg.BufferToRead,0);
      Sleep(5);
    end else
      Sleep(1);
  end;
  FEnd.SetEvent;
end;

procedure TCommThread.InsertRecord;
begin
  if frmRank=nil then exit;
  try
    frmRank.tblRank.DisableControls;

    //if the game number is already registered, exit...
    if frmRank.tblRank.Locate('id_jogo',cur_game_number,[]) then exit;

    //only append if not in edit mode.
    if not (frmRank.tblRank.State in dsEditModes) then
      frmRank.tblRank.Append;

    frmRank.tblRank.FieldByName('id_jogo').AsInteger:=cur_game_number;
    frmRank.tblRank.FieldByName('nr_pontos').AsInteger:=cur_game_punctuation;

    frmRank.Panel1.Visible:=true;
  finally
    frmRank.tblRank.EnableControls;
  end;
end;

constructor TCommThread.Create(CreateSuspended: Boolean;
  aSerialPort: TSerialPortDriver; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FSerialPort:=aSerialPort;
  FEnd:=TCrossEvent.Create(nil,true,false,'');
end;

procedure TCommThread.WaitEnd;
begin
  if FEnd<>nil then begin
    while FEnd.WaitFor(1)<>wrSignaled do
      CheckSynchronize(1);
    FreeAndNil(FEnd);
  end;
end;

{ TfrmRank }

procedure TfrmRank.FormCreate(Sender: TObject);
var
  conf:TIniFile;
  ativo: Boolean;
begin
  //conecta no arquivo SQLite e cria a tabela caso ela não exista...
  //connect to SQLite3 file and creates the table if it don't exists...
  SQLiteConn.Disconnect;
  SQLiteConn.Database:=ExtractFilePath(Application.ExeName)+'ranking.db3';
  SQLiteConn.Connect;
  try
    SQLiteConn.ExecuteDirect(
                             'CREATE TABLE tbl_rank ( ' +
                             '    id_jogo            INTEGER        PRIMARY KEY, '+  //game number
                             '    nr_pontos          INTEGER, '+                     //punctuation
                             '    ds_nome_recordista VARCHAR( 50 ) '+                //player name
                             ');  '
                            );
  except
  end;
  tblRank.Open;

  //carrega as configurações da porta serial e ativa ela.
  //load and setup the serial port.
  conf:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'comm.ini');
  try
    case conf.ReadInteger('communication','databits',8) of
      5:
        SerialPortDriver1.DataBits:=db5;
      6:
        SerialPortDriver1.DataBits:=db6;
      7:
        SerialPortDriver1.DataBits:=db7;
      else
        SerialPortDriver1.DataBits:=db8;
    end;

    if conf.ReadInteger('communication','stopbits',1)=1 then
      SerialPortDriver1.StopBits:=sb1
    else
      SerialPortDriver1.StopBits:=sb2;

    case conf.ReadInteger('communication','parity',0) of
      1:
        SerialPortDriver1.Paridade:=spOdd;
      2:
        SerialPortDriver1.Paridade:=spEven;
      else
        SerialPortDriver1.Paridade:=spNone;
    end;

    case conf.ReadInteger('communication','baudrate',9600) of
      110   : SerialPortDriver1.BaudRate:=br110;
      300   : SerialPortDriver1.BaudRate:=br300;
      600   : SerialPortDriver1.BaudRate:=br600;
      1200  : SerialPortDriver1.BaudRate:=br1200;
      2400  : SerialPortDriver1.BaudRate:=br2400;
      4800  : SerialPortDriver1.BaudRate:=br4800;
      19200 : SerialPortDriver1.BaudRate:=br19200;
      38400 : SerialPortDriver1.BaudRate:=br38400;
      57600 : SerialPortDriver1.BaudRate:=br57600;
      115200: SerialPortDriver1.BaudRate:=br115200;
      else    SerialPortDriver1.BaudRate:=br9600;
    end;

    SerialPortDriver1.Timeout:=conf.ReadInteger('communication','Timeout',100);

    ativo := conf.ReadBool('communication','active',false);
    try
      SerialPortDriver1.COMPort:=conf.ReadString('communication', 'comport', '');
      SerialPortDriver1.Active:=ativo;
    except
    end;
  finally
    FreeAndNil(conf);
  end;

  //se ativou a porta serial, cria a thread de leitura...
  //if serial port has been activated, creates the thread to read data from it.
  if SerialPortDriver1.Active then begin
    commthread:=TCommThread.Create(true,SerialPortDriver1);
    commthread.Start;
  end;
end;

procedure TfrmRank.FormDestroy(Sender: TObject);
begin
  if commthread<>nil then begin
    commthread.Terminate;
    commthread.WaitEnd;
  end;
end;

procedure TfrmRank.Panel2Click(Sender: TObject);
begin

  if tblRank.State in dsEditModes then begin
    if tblRank.FieldByName('ds_nome_recordista').IsNull then
      tblRank.FieldByName('ds_nome_recordista').AsString:='Unnamed player';
    tblRank.Post;
  end;
  Panel1.Visible:=false;
end;

end.

