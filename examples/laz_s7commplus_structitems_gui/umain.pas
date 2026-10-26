unit umain;

{: Le um DB inteiro com um TPLCStruct e cada membro por um TPLCStructItem.

   O TPLCStruct (LongAddress = nome do DB, sem ponto) traz o DB todo numa leitura so. Cada
   TPLCStructItem aponta para ele (PLCBlock), diz em que byte comeca (Index) e de que tipo e'
   (TagType), e passa a valer o que a struct leu - sem pedir nada mais ao CLP.

   Os membros sao DInt de 4 bytes em sequencia (Var<n> no byte (n-1)*4), como no DB4 de teste.
   O CLP guarda em big-endian, por isso os itens usam SwapBytes e SwapWords. A coluna "Confere"
   compara o valor do item com os mesmos 4 bytes da struct montados a mao.

   Uso: s7commplus_structitems_gui [ip [DB]] - com o ip, ja conecta ao abrir.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids,
  Tag, PLCStruct, PLCStructElement, tcp_udpport, S7PlusFamily;

const
  MemberSize = 4;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    EditDb: TEdit;
    EditHost: TEdit;
    EditMembers: TEdit;
    EditRefresh: TEdit;
    Grid: TStringGrid;
    LabelDb: TLabel;
    LabelHost: TLabel;
    LabelMembers: TLabel;
    LabelRefresh: TLabel;
    LabelStatus: TLabel;
    PanelTop: TPanel;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FPort:TTCP_UDPPort;
    FDriver:TS7CommPlusDriver;
    FStruct:TPLCStruct;
    FItems:array of TPLCStructItem;
    FUpdates:Integer;
    FMismatches:Integer;
    function Connected:Boolean;
    procedure Connect;
    procedure Disconnect;
    procedure StructUpdated(Sender:TObject);
    function ExpectedOf(N:Integer):LongInt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Grid.Cells[0, 0] := 'Item';
  Grid.Cells[1, 0] := 'Byte';
  Grid.Cells[2, 0] := 'Valor (TPLCStructItem)';
  Grid.Cells[3, 0] := 'Bytes da struct';
  Grid.Cells[4, 0] := 'Confere';

  //s7commplus_structitems_gui <ip> [DB]: preenche e ja conecta
  if ParamCount>=1 then begin
    EditHost.Text := ParamStr(1);
    if ParamCount>=2 then EditDb.Text := ParamStr(2);
    Connect;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Disconnect;
end;

function TFormMain.Connected:Boolean;
begin
  Result := FStruct<>nil;
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
begin
  if Connected then
    Disconnect
  else
    Connect;
end;

procedure TFormMain.Connect;
var
  Members, RefreshMs, n:Integer;
begin
  Members := StrToIntDef(EditMembers.Text, 0);
  RefreshMs := StrToIntDef(EditRefresh.Text, 0);
  if (Members<1) or (RefreshMs<1) then begin
    ShowMessage('Membros e Refresh precisam ser numeros maiores que zero.');
    exit;
  end;

  FUpdates := 0;
  FMismatches := 0;

  FPort := TTCP_UDPPort.Create(nil);
  FPort.Host := Trim(EditHost.Text);
  FPort.Port := 102;
  FPort.Timeout := 8000;
  FPort.EnableAutoReconnect := true;
  FDriver := TS7CommPlusDriver.Create(nil);
  FDriver.CommunicationPort := FPort;

  //a struct cobre os membros lidos (Size trunca o DB ao inicio dele)
  FStruct := TPLCStruct.Create(nil);
  FStruct.Name := 'Struct_'+Trim(EditDb.Text);
  FStruct.Size := Members*MemberSize;
  FStruct.OnUpdate := @StructUpdated;
  FStruct.RefreshTime := RefreshMs;
  FStruct.LongAddress := Trim(EditDb.Text);

  Grid.RowCount := Members+1;
  SetLength(FItems, Members);
  for n:=1 to Members do begin
    FItems[n-1] := TPLCStructItem.Create(nil);
    FItems[n-1].Name := Format('Var%d', [n]);
    FItems[n-1].PLCBlock := FStruct;
    FItems[n-1].TagType := pttLongInt;  //DInt
    FItems[n-1].SwapBytes := true;      //big-endian do CLP
    FItems[n-1].SwapWords := true;
    FItems[n-1].Index := (n-1)*MemberSize;
    Grid.Cells[0, n] := FItems[n-1].Name;
    Grid.Cells[1, n] := IntToStr(FItems[n-1].Index);
    Grid.Cells[2, n] := '';
    Grid.Cells[3, n] := '';
    Grid.Cells[4, n] := '';
  end;

  FStruct.ProtocolDriver := FDriver;
  FPort.Active := true;

  ButtonConnect.Caption := 'Desconectar';
  EditHost.Enabled := false;
  EditDb.Enabled := false;
  EditMembers.Enabled := false;
  EditRefresh.Enabled := false;
  LabelStatus.Caption := Format('Conectando a %s ...', [FPort.Host]);
end;

procedure TFormMain.Disconnect;
var
  n:Integer;
begin
  for n:=0 to High(FItems) do
    FreeAndNil(FItems[n]);
  SetLength(FItems, 0);
  FreeAndNil(FStruct);
  FreeAndNil(FDriver);
  FreeAndNil(FPort);

  ButtonConnect.Caption := 'Conectar';
  EditHost.Enabled := true;
  EditDb.Enabled := true;
  EditMembers.Enabled := true;
  EditRefresh.Enabled := true;
  LabelStatus.Caption := 'Desconectado.';
end;

function TFormMain.ExpectedOf(N:Integer):LongInt;
var
  Off:Integer;
begin
  Off := (N-1)*MemberSize;
  Result := LongInt((Cardinal(FStruct.GetByte(Off)) shl 24) or (Cardinal(FStruct.GetByte(Off+1)) shl 16) or
                    (Cardinal(FStruct.GetByte(Off+2)) shl 8) or Cardinal(FStruct.GetByte(Off+3)));
end;

procedure TFormMain.StructUpdated(Sender:TObject);
var
  n, b:Integer;
  s:String;
  Ok:Boolean;
begin
  if not Connected then exit;
  Inc(FUpdates);
  for n:=1 to Length(FItems) do begin
    s := '';
    for b:=0 to MemberSize-1 do
      s := s + IntToHex(FStruct.GetByte((n-1)*MemberSize+b), 2) + ' ';
    Ok := Trunc(FItems[n-1].Value)=ExpectedOf(n);
    if not Ok then Inc(FMismatches);
    Grid.Cells[2, n] := IntToStr(Trunc(FItems[n-1].Value));
    Grid.Cells[3, n] := Trim(s);
    Grid.Cells[4, n] := BoolToStr(Ok, 'sim', 'NAO');
  end;
  LabelStatus.Caption := Format('%s - %d bytes, %d atualizacoes, %d divergencias',
                                [FStruct.LongAddress, FStruct.Size, FUpdates, FMismatches]);
end;

end.
