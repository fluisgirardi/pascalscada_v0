unit uDM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, SerialPort, ModBusSerial,
  PLCTagNumber, PLCBlock, PLCBlockElement, commtypes;

type

  { TDM }

  TDM = class(TDataModule)
    D0: TPLCTagNumber;
    D100: TPLCTagNumber;
    D200: TPLCTagNumber;
    D300: TPLCTagNumber;
    C251: TPLCTagNumber;
    Entradas: TPLCBlock;
    M11: TPLCTagNumber;
    MDB: TModBusRTUDriver;
    D8436: TPLCTagNumber;
    Saidas: TPLCBlock;
    SPD: TSerialPortDriver;
    M10: TPLCTagNumber;
    D8013: TPLCTagNumber;
    D8014: TPLCTagNumber;
    D8015: TPLCTagNumber;
    D8016: TPLCTagNumber;
    D8017: TPLCTagNumber;
    D8018: TPLCTagNumber;
    D8019: TPLCTagNumber;
    X0: TPLCBlockElement;
    X1: TPLCBlockElement;
    X2: TPLCBlockElement;
    X3: TPLCBlockElement;
    Y0: TPLCBlockElement;
    Y1: TPLCBlockElement;
    Y2: TPLCBlockElement;
    Y3: TPLCBlockElement;
    ZTable1idtpw03: TLargeintField;
    ZTable1y0: TLargeintField;
    ZTable1y1: TLargeintField;
    procedure DataModuleCreate(Sender: TObject);
    procedure SPDCommErrorReading(Error: TIOResult);
    procedure SPDCommErrorWriting(Error: TIOResult);
    procedure SPDCommPortOpened(Sender: TObject);
    procedure SPDCommPortOpenError(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DM: TDM;
  i:integer;

implementation

uses uPrincipal;

{$R *.lfm}

{ TDM }

procedure TDM.SPDCommErrorReading(Error: TIOResult);
begin
    i:=i+1;
    fPrincipal.Memo1.Lines.Add(IntToStr(i)+' - Erro de comunicação na Leitura. ->'+IntToStr(SPD.LastOSErrorNumber)+'-'+SPD.LastOSErrorMessage);
end;

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  SPD.COMPort:='your serial port name here';
  SPD.Active:=true;
end;

procedure TDM.SPDCommErrorWriting(Error: TIOResult);
begin
  i:=i+1;
  fPrincipal.Memo1.Lines.Add(IntToStr(i)+' - Erro de comunicação na Escrita.');
end;

procedure TDM.SPDCommPortOpened(Sender: TObject);
begin
  i:=i+1;
  fPrincipal.Memo1.Lines.Add(IntToStr(i)+' - Porta de comunicação Aberta.');
end;

procedure TDM.SPDCommPortOpenError(Sender: TObject);
begin
  i:=i+1;
  fPrincipal.Memo1.Lines.Add(IntToStr(i)+' - Erro na abertura da porta.');
end;

end.

