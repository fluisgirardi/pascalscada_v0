unit umain;

{: Le um DB inteiro com um TPLCStruct e cada membro por um TPLCStructItem.

   O TPLCStruct (LongAddress = nome do DB, sem ponto) traz o DB todo numa leitura so. Cada
   TPLCStructItem aponta para ele (PLCBlock), diz em que byte comeca (Index) e de que tipo e'
   (TagType), e passa a valer o que a struct leu - sem pedir nada mais ao CLP.

   Os membros sao os do DB4 de teste: Var1..VarN, DInt de 4 bytes em sequencia (Var<n> no byte
   (n-1)*4) e, com "Var26" marcado, os subitens da struct Var26 (byte 100, 15 bytes).
   ATENCAO: o DB4 tem acesso otimizado, e o CLP monta uma struct otimizada ordenada por tamanho,
   nao pela ordem da declaracao. Os 15 bytes da Var26 chegam assim (offsets a partir do byte 100):
     v3 DWord +0, Static_1 Real +4, v2 Word +8, v1 Byte +10, v4 +11, Static_2 +12, Static_3 +13,
     Static_4 +14 (cada BOOL ocupa um byte). Sao os mesmos numeros do OptAddr de cada membro
     (TS7PlusVarInfo.ImageOffset), nao os do layout classico (v1 +0, v2 +2, v3 +4 ...).
   Um BOOL e' um TTagBit (bit 0) ligado ao TPLCStructItem do byte dele (PLCTag, StartBit = EndBit).
   O CLP guarda em big-endian, por isso os itens usam SwapBytes e SwapWords. A coluna "Confere"
   compara o valor do item com os mesmos bytes da struct montados a mao.

   Uso: s7commplus_structitems_gui [ip [DB]] - com o ip, ja conecta ao abrir.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids,
  Tag, TagBit, PLCStruct, PLCStructElement, tcp_udpport, S7PlusFamily;

const
  MemberSize = 4;      //Var1..VarN: DInt
  Var26Offset = 100;
  Var26Size = 15;

type
  //one TPLCStructItem: where it starts, how wide it is and what it holds
  TMemberSpec = record
    Name:String;
    Offset:Integer;
    Size:Integer;     //1, 2 or 4
    TagType:TTagType;
    IsFloat:Boolean;
    Bit:Integer;      //-1: a TPLCStructItem; 0..7: a TTagBit on the item Parent
    Parent:Integer;   //index in FSpecs of the item that carries the bit
  end;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    CheckVar26: TCheckBox;
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
    FSpecs:array of TMemberSpec;
    FItems:array of TPLCStructItem;  //nil where the spec is a bit
    FBits:array of TTagBit;          //nil where the spec is an item
    FUpdates:Integer;
    FMismatches:Integer;
    function Connected:Boolean;
    procedure Connect;
    procedure Disconnect;
    procedure StructUpdated(Sender:TObject);
    procedure AddSpec(const AName:String; AOffset, ASize:Integer; AType:TTagType; AIsFloat:Boolean = false);
    procedure AddBit(const AName:String; AParent, ABit:Integer);
    function ExpectedOf(N:Integer):Double;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

//the grid shows "Var26 byte 108", a component name cannot hold spaces or dots
function ComponentNameOf(const S:String):String;
var
  i:Integer;
begin
  Result := S;
  for i:=1 to Length(Result) do
    if not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9']) then
      Result[i] := '_';
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Grid.Cells[0, 0] := 'Item';
  Grid.Cells[1, 0] := 'Byte[.bit]';
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
  Members, RefreshMs, n, StructSize:Integer;
begin
  Members := StrToIntDef(EditMembers.Text, 0);
  RefreshMs := StrToIntDef(EditRefresh.Text, 0);
  if (Members<1) or (RefreshMs<1) then begin
    ShowMessage('Membros e Refresh precisam ser numeros maiores que zero.');
    exit;
  end;

  FUpdates := 0;
  FMismatches := 0;

  SetLength(FSpecs, 0);
  for n:=1 to Members do
    AddSpec(Format('Var%d', [n]), (n-1)*MemberSize, MemberSize, pttLongInt);
  StructSize := Members*MemberSize;
  if CheckVar26.Checked then begin
    //DB4.Var26 is a struct at byte 100; its members are read out of the same struct tag
    //offsets in the bytes the PLC returns for the optimized struct (see the header)
    AddSpec('Var26.v3', Var26Offset, 4, pttDWord);
    AddSpec('Var26.Static_1', Var26Offset+4, 4, pttFloat, true);
    AddSpec('Var26.v2', Var26Offset+8, 2, pttWord);
    AddSpec('Var26.v1', Var26Offset+10, 1, pttByte);
    //the BOOLs: one byte each, read as an item; the BOOL is a TTagBit on it
    AddSpec('Var26.v4 (byte)', Var26Offset+11, 1, pttByte);
    AddBit('Var26.v4', High(FSpecs), 0);
    AddSpec('Var26.Static_2 (byte)', Var26Offset+12, 1, pttByte);
    AddBit('Var26.Static_2', High(FSpecs), 0);
    AddSpec('Var26.Static_3 (byte)', Var26Offset+13, 1, pttByte);
    AddBit('Var26.Static_3', High(FSpecs), 0);
    AddSpec('Var26.Static_4 (byte)', Var26Offset+14, 1, pttByte);
    AddBit('Var26.Static_4', High(FSpecs), 0);
    if StructSize<Var26Offset+Var26Size then StructSize := Var26Offset+Var26Size;
  end;

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
  FStruct.Size := StructSize;
  FStruct.OnUpdate := @StructUpdated;
  FStruct.RefreshTime := RefreshMs;
  FStruct.LongAddress := Trim(EditDb.Text);

  Grid.RowCount := Length(FSpecs)+1;
  //the window grows to show every item (up to what the screen allows)
  n := PanelTop.Height + Grid.RowCount*(Grid.DefaultRowHeight+Grid.GridLineWidth) + 4;
  if n>Screen.WorkAreaHeight-80 then n := Screen.WorkAreaHeight-80;
  if ClientHeight<n then ClientHeight := n;
  SetLength(FItems, Length(FSpecs));
  SetLength(FBits, Length(FSpecs));
  for n:=1 to Length(FSpecs) do begin
    if FSpecs[n-1].Bit>=0 then begin
      FBits[n-1] := TTagBit.Create(nil);
      FBits[n-1].Name := ComponentNameOf(FSpecs[n-1].Name);
      FBits[n-1].PLCTag := FItems[FSpecs[n-1].Parent];
      FBits[n-1].UseRawValue := true;
      FBits[n-1].EndBit := FSpecs[n-1].Bit;    //first: EndBit cannot be below StartBit
      FBits[n-1].StartBit := FSpecs[n-1].Bit;
      Grid.Cells[0, n] := FSpecs[n-1].Name;
      Grid.Cells[1, n] := Format('%d.%d', [FSpecs[n-1].Offset, FSpecs[n-1].Bit]);
    end else begin
      FItems[n-1] := TPLCStructItem.Create(nil);
      FItems[n-1].Name := ComponentNameOf(FSpecs[n-1].Name);
      FItems[n-1].PLCBlock := FStruct;
      FItems[n-1].TagType := FSpecs[n-1].TagType;
      FItems[n-1].SwapBytes := FSpecs[n-1].Size>=2;  //big-endian do CLP
      FItems[n-1].SwapWords := FSpecs[n-1].Size>=4;
      FItems[n-1].Index := FSpecs[n-1].Offset;
      Grid.Cells[0, n] := FSpecs[n-1].Name;
      Grid.Cells[1, n] := IntToStr(FSpecs[n-1].Offset);
    end;
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
  CheckVar26.Enabled := false;
  LabelStatus.Caption := Format('Conectando a %s ...', [FPort.Host]);
end;

procedure TFormMain.Disconnect;
var
  n:Integer;
begin
  for n:=0 to High(FBits) do
    FreeAndNil(FBits[n]);
  SetLength(FBits, 0);
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
  CheckVar26.Enabled := true;
  LabelStatus.Caption := 'Desconectado.';
end;

procedure TFormMain.AddSpec(const AName:String; AOffset, ASize:Integer; AType:TTagType; AIsFloat:Boolean);
var
  n:Integer;
begin
  n := Length(FSpecs);
  SetLength(FSpecs, n+1);
  FSpecs[n].Name := AName;
  FSpecs[n].Offset := AOffset;
  FSpecs[n].Size := ASize;
  FSpecs[n].TagType := AType;
  FSpecs[n].IsFloat := AIsFloat;
  FSpecs[n].Bit := -1;
  FSpecs[n].Parent := -1;
end;

procedure TFormMain.AddBit(const AName:String; AParent, ABit:Integer);
var
  n:Integer;
begin
  AddSpec(AName, FSpecs[AParent].Offset, 1, pttByte);
  n := High(FSpecs);
  FSpecs[n].Bit := ABit;
  FSpecs[n].Parent := AParent;
end;

//the item's bytes taken from the struct and put together by hand (big-endian)
function TFormMain.ExpectedOf(N:Integer):Double;
var
  b:Integer;
  u:Cardinal;
  f:Single;
begin
  u := 0;
  for b:=0 to FSpecs[N-1].Size-1 do
    u := (u shl 8) or FStruct.GetByte(FSpecs[N-1].Offset+b);
  if FSpecs[N-1].Bit>=0 then begin
    Result := (u shr FSpecs[N-1].Bit) and 1;
    exit;
  end;
  if FSpecs[N-1].IsFloat then begin
    Move(u, f, SizeOf(f));
    Result := f;
  end else if FSpecs[N-1].TagType=pttLongInt then
    Result := LongInt(u)
  else
    Result := u;
end;

procedure TFormMain.StructUpdated(Sender:TObject);
var
  n, b:Integer;
  s:String;
  Ok:Boolean;
  Val:Double;
begin
  if not Connected then exit;
  Inc(FUpdates);
  for n:=1 to Length(FItems) do begin
    s := '';
    for b:=0 to FSpecs[n-1].Size-1 do
      s := s + IntToHex(FStruct.GetByte(FSpecs[n-1].Offset+b), 2) + ' ';
    if FSpecs[n-1].Bit>=0 then
      Val := FBits[n-1].Value
    else
      Val := FItems[n-1].Value;
    Ok := Val=ExpectedOf(n);
    if not Ok then Inc(FMismatches);
    if FSpecs[n-1].IsFloat then
      Grid.Cells[2, n] := FloatToStr(Val)
    else
      Grid.Cells[2, n] := IntToStr(Trunc(Val));
    Grid.Cells[3, n] := Trim(s);
    Grid.Cells[4, n] := BoolToStr(Ok, 'sim', 'NAO');
  end;
  LabelStatus.Caption := Format('%s - %d bytes, %d atualizacoes, %d divergencias',
                                [FStruct.LongAddress, FStruct.Size, FUpdates, FMismatches]);
end;

end.
