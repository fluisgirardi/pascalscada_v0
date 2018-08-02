unit uDM;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommPort, SerialPort, ProtocolDriver, ModBusDriver,
  ModBusSerial, ZConnection, Tag, PLCTag, TagBlock, PLCBlock, TagBit, PLCNumber,
  PLCTagNumber, PLCBlockElement, tcp_udpport, ModBusTCP, ExtCtrls,
  {$IFDEF FPC}LResources,{$ENDIF} DB, ZAbstractRODataset, ZAbstractDataset,
  ZDataset;

type

  { TDM }

  TDM = class(TDataModule)
    SerialPortDriver1: TSerialPortDriver;
    ModBusRTUDriver1: TModBusRTUDriver;
    PesoBalancaW1W2: TPLCBlock;
    PesoProdutoW1W2: TPLCBlock;
    PesoACCAtualW1W2: TPLCBlock;
    FlagsGravar: TPLCTagNumber;
    Gravar: TTagBit;
    Gravado: TTagBit;
    PesoBalanca: TPLCTagNumber;
    PesoProduto: TPLCTagNumber;
    PesoACCAtual: TPLCTagNumber;
    Saidas: TPLCBlock;
    Q0_0: TPLCBlockElement;
    Q0_1: TPLCBlockElement;
    Q0_2: TPLCBlockElement;
    Q0_3: TPLCBlockElement;
    Entradas: TPLCBlock;
    I0_0: TPLCBlockElement;
    I0_1: TPLCBlockElement;
    I0_2: TPLCBlockElement;
    I0_3: TPLCBlockElement;
    I0_4: TPLCBlockElement;
    I0_5: TPLCBlockElement;
    I0_6: TPLCBlockElement;
    I0_7: TPLCBlockElement;
    Timer1: TTimer;
    Flags520: TPLCTagNumber;
    ala1: TTagBit;
    ala2: TTagBit;
    ala3: TTagBit;
    ala4: TTagBit;
    ala5: TTagBit;
    ala6: TTagBit;
    ala7: TTagBit;
    ala8: TTagBit;
    AlarmesCorrentesid: TLargeintField;
    AlarmesCorrentesdt_entrada: TDateTimeField;
    AlarmesCorrentestagid: TLargeintField;
    AlarmesCorrentesmensagem: TStringField;
    AlarmesCorrentesdt_saida: TDateTimeField;
    ValvulaBalanca: TPLCTagNumber;
    ValvulaDeposito: TPLCTagNumber;
    v521_1: TTagBit;
    v521_2: TTagBit;
    procedure Q0_3ValueChange(Sender: TObject);
    procedure Q0_2ValueChange(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure ala1ValueChange(Sender: TObject);
    procedure GravarValueChange(Sender: TObject);
    procedure PesoBalancaW1W2ValueChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function S7200Float(W1,W2:Double):Single;
    function FloatToSQLNumer(Number:Double):string;
  end;

var
  DM: TDM;

implementation

uses hsutils, ProtocolTypes, Math;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  //MySQLConnection.ExecuteDirect('UPDATE alarmes Set dt_saida=dt_entrada WHERE dt_saida IS NULL')
end;

procedure TDM.ala1ValueChange(Sender: TObject);
var
  msgs:array[0..7] of string;
  id:integer;
  state:double;
  sql:String;
begin
  msgs[0]:='Chupin desligado';
  msgs[1]:='Deposito superior não abriu';
  msgs[2]:='Deposito superior não fechou';
  msgs[3]:='Balanca não abriu';
  msgs[4]:='Balanca não fechou';
  msgs[5]:='Balanca não descarregou';
  msgs[6]:='Tempo do deposito inferior';
  msgs[7]:='Alarme de pesagem';

  if sender is TTagBit then
    with sender as TTagBit do begin
      id := Tag;
      state := Value;
    end;

  if state=1 then
    sql := 'INSERT INTO alarmes(dt_entrada,tagid,mensagem) VALUES(Now(),'+IntToStr(id)+','''+msgs[id]+''');'
  else
    sql := 'UPDATE alarmes Set dt_saida=Now() WHERE dt_saida is null and tagid='+IntToStr(id);

  //MySQLConnection.ExecuteDirect(sql);
  //AlarmesCorrentes.Open;
  //AlarmesCorrentes.Refresh;
end;

procedure TDM.GravarValueChange(Sender: TObject);
begin
  if Gravar.Value=1 then
    Timer1.Enabled := true;
end;

procedure TDM.PesoBalancaW1W2ValueChange(Sender: TObject);
begin
  if Sender is TPLCBlock then
    with sender as TPLCBlock do begin
      case Tag of
        1:
          PesoBalanca.Value  := S7200Float(ValueRaw[1],ValueRaw[0]);
        2:
          PesoProduto.Value  := S7200Float(ValueRaw[1],ValueRaw[0]);
        3:
          PesoACCAtual.Value := S7200Float(ValueRaw[1],ValueRaw[0]);
      end;
    end;
end;

procedure TDM.Q0_2ValueChange(Sender: TObject);
begin
  ValvulaDeposito.Value := IfThen(Q0_2.Value=1,IfThen(I0_3.Value=1,IfThen(I0_4.Value=0,1,3),0),IfThen(I0_4.Value=1,IfThen(I0_3.Value=0,2,3),0))
end;

procedure TDM.Q0_3ValueChange(Sender: TObject);
begin
  ValvulaBalanca.Value := IfThen(Q0_3.Value=1,IfThen(I0_5.Value=1,IfThen(I0_6.Value=0,1,3),0),IfThen(I0_6.Value=1,IfThen(I0_5.Value=0,2,3),0))
end;

function TDM.S7200Float(W1,W2:Double):Single;
var
  mW1,mW2:Word;
  resultado:single;
  paux:PWord;
begin
  mW1 := Trunc(W1) and $FFFF;
  mW2 := Trunc(W2) and $FFFF;
  paux := PWord(@resultado);
  resultado := 0;
  Paux^ := mW1;
  inc(Paux);
  Paux^ := mW2;
  result := resultado;
end;

function TDM.FloatToSQLNumer(Number:Double):string;
var
  olddec:char;
begin
  olddec:=DecimalSeparator;
  DecimalSeparator:='.';
  try
    Result := FormatFloat('#0.00000',Number);
  finally
    DecimalSeparator:=olddec
  end;
end;

procedure TDM.Timer1Timer(Sender: TObject);
var
  pesoprod:Double;
  tentativas:integer;
  sql:String;
begin
  tentativas := 0;
  PesoProdutoW1W2.Read;
  repeat
    if PesoProdutoW1W2.LastSyncReadStatus=ioOk then begin
      pesoprod := PesoProduto.Value;
      //sql := 'SELECT InsereTemp(NOW(), '+FloatToSQLNumer(pesoprod)+', 0, 0, 0, 0, 1, ''Balança Milho'',''Pesagens'','''','' Kg'', 1, 1);';
      //MySQLConnection.ExecuteDirect(sql);
    end;
    inc(tentativas)
  until (PesoProdutoW1W2.LastSyncReadStatus=ioOk) or (tentativas=3);
  Gravado.Value := 1;
  Timer1.Enabled := false
end;

{$IFDEF FPC}
initialization
  {$i uDM.lrs}
{$ENDIF}  

end.
