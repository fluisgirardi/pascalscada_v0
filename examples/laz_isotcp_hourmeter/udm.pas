unit udm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, tcp_udpport, ISOTCPDriver, PLCBlock,
  PLCBlockElement, PLCTagNumber, TagBit;

type

  { Tdmtags }

  Tdmtags = class(TDataModule)
    Alarme1_1: TTagBit;
    Alarme1_2: TTagBit;
    Alarme1_3: TTagBit;
    Alarme1_4: TTagBit;
    Alarme1_5: TTagBit;
    Alarme1_6: TTagBit;
    Alarme1_7: TTagBit;
    Alarme1_8: TTagBit;
    Alarmes1: TPLCTagNumber;
    EquipLigado: TPLCBlock;
    HorasAcumuladas: TPLCBlock;
    Horimetros_PV_e_SP: TPLCBlock;
    ISOTCPDriver: TISOTCPDriver;
    MB0: TPLCBlockElement;
    MB0_0: TTagBit;
    MB0_1: TTagBit;
    MB0_2: TTagBit;
    MB0_3: TTagBit;
    MB0_4: TTagBit;
    MB0_5: TTagBit;
    MB0_6: TTagBit;
    MB0_7: TTagBit;
    MB1: TPLCBlockElement;
    MB1_0: TTagBit;
    MB1_1: TTagBit;
    MB1_2: TTagBit;
    MB1_3: TTagBit;
    MB1_4: TTagBit;
    MB1_5: TTagBit;
    MB1_6: TTagBit;
    MB1_7: TTagBit;
    MB2: TPLCBlockElement;
    MB2_0: TTagBit;
    MB2_1: TTagBit;
    MB2_2: TTagBit;
    MB2_3: TTagBit;
    MB2_4: TTagBit;
    MB2_5: TTagBit;
    MB2_6: TTagBit;
    MB2_7: TTagBit;
    ResetAlarme1_1: TTagBit;
    ResetAlarme1_2: TTagBit;
    ResetAlarme1_3: TTagBit;
    ResetAlarme1_4: TTagBit;
    ResetAlarme1_5: TTagBit;
    ResetAlarme1_6: TTagBit;
    ResetAlarme1_7: TTagBit;
    ResetAlarme1_8: TTagBit;
    ResetAlarmes1: TPLCTagNumber;
    Resets: TPLCTagNumber;
    Reset_1: TTagBit;
    Reset_2: TTagBit;
    Reset_3: TTagBit;
    Reset_4: TTagBit;
    Reset_5: TTagBit;
    Reset_6: TTagBit;
    Reset_7: TTagBit;
    Reset_8: TTagBit;
    S7200Connection: TTCP_UDPPort;
    SPHorimetro1: TPLCBlockElement;
    SPHorimetro2: TPLCBlockElement;
    SPHorimetro3: TPLCBlockElement;
    SPHorimetro4: TPLCBlockElement;
    SPHorimetro5: TPLCBlockElement;
    SPHorimetro6: TPLCBlockElement;
    SPHorimetro7: TPLCBlockElement;
    SPHorimetro8: TPLCBlockElement;
    TAcumulado1: TPLCBlockElement;
    TAcumulado2: TPLCBlockElement;
    TAcumulado3: TPLCBlockElement;
    TAcumulado4: TPLCBlockElement;
    TAcumulado5: TPLCBlockElement;
    TAcumulado6: TPLCBlockElement;
    TAcumulado7: TPLCBlockElement;
    TAcumulado8: TPLCBlockElement;
    TDecorrido1: TPLCBlockElement;
    TDecorrido2: TPLCBlockElement;
    TDecorrido3: TPLCBlockElement;
    TDecorrido4: TPLCBlockElement;
    TDecorrido5: TPLCBlockElement;
    TDecorrido6: TPLCBlockElement;
    TDecorrido7: TPLCBlockElement;
    TDecorrido8: TPLCBlockElement;
    procedure Alarmes1ValueChangeLast(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure RegistraAcionamento(Sender: TObject);
    procedure RegistraAlarme(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dmtags: Tdmtags;

implementation

uses udmdb;

{$R *.lfm}

{ Tdmtags }

procedure Tdmtags.DataModuleCreate(Sender: TObject);
begin

end;

procedure Tdmtags.Alarmes1ValueChangeLast(Sender: TObject);
begin

end;

procedure Tdmtags.RegistraAcionamento(Sender: TObject);
var
  id, agora:String;
  state:Double;
begin
  if Sender is TTagBit then
    with TTagBit(Sender) do begin
      id:= IntToStr(Tag);
      state:=Value;
    end
  else
    exit;

  agora:=FormatDateTime('yyyy-mm-dd hh:nn:ss',Now);
  with dmdb.SQLiteConnection do begin
    if state=1 then
      ExecuteDirect('INSERT INTO tbl_acionamento (dt_inicio, cd_equipamento) VALUES ('''+agora+''', '+id+');')
    else
      ExecuteDirect('UPDATE tbl_acionamento SET dt_fim='''+agora+''' WHERE dt_fim IS NULL AND cd_equipamento='+id);
  end;
end;

procedure Tdmtags.RegistraAlarme(Sender: TObject);
var
  id, agora:String;
  state:Double;
begin
  if Sender is TTagBit then
    with TTagBit(Sender) do begin
      id:= IntToStr(Tag);
      state:=Value;
    end
  else
    exit;

  agora:=FormatDateTime('yyyy-mm-dd hh:nn:ss',Now);
  with dmdb.SQLiteConnection do begin
    if state=1 then
      ExecuteDirect('INSERT INTO tbl_alarme (dt_inicio, cd_equipamento, ds_mensagem) VALUES ('''+agora+''', '+id+', ''Estouro do horimetro'');')
    else
      ExecuteDirect('UPDATE tbl_alarme SET dt_fim='''+agora+''' WHERE dt_fim IS NULL AND cd_equipamento='+id);
  end;
  dmdb.AlarmesAtivos.Refresh;
end;

end.

