unit umain;

{: Cliente OPC UA com o TOPCUADriver.

   1. Informe o endpoint (opc.tcp://host:4840), usuario/senha se o servidor pedir e a seguranca.
      Sem um servidor a mao, "Servidor de teste" sobe um servidor open62541 neste mesmo processo
      (o de tests/testsupport.opcuaserver.pas, variaveis Test.*, com o Test.Counter mudando
      sozinho a cada 50 ms) e preenche o endpoint.
   2. "Conectar e listar" cria o driver e lista as variaveis do servidor (TOPCUADriver.ListTags).
   3. Duplo clique (ou "Monitorar") numa variavel cria o tag que combina com ela, como o Tag
      Builder faz: TPLCTagNumber para um escalar numerico, TPLCBlock para um array e TPLCString
      para um texto. O LongAddress de cada um e' o NodeId da variavel.
   4. Escolha um tag monitorado, digite o valor e "Gravar" (num array, os valores separados por ";").

   Com seguranca Sign/SignAndEncrypt o driver gera um certificado de cliente na primeira conexao
   (pasta CertificateDir) - no PLCSim/S7-1500 ele precisa ser aceito no TIA Portal.
   A libopen62541 1.5 tem de estar no caminho de bibliotecas, ou na variavel OPEN62541_LIB.

   Uso: laz_opcua_client [endpoint] - com o endpoint, ja conecta e lista ao abrir.
        laz_opcua_client --teste    - sobe o servidor de teste, conecta e monitora tudo.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, SyncObjs,
  Tag, ProtocolTypes, PLCTag, PLCTagNumber, PLCBlock, PLCString, OPCUAClient, OPCUADriver,
  testsupport.opcuaserver;

type
  TMonitored = record
    Info:TPLCTagInfo;
    Tag:TPLCTag;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    ButtonMonitor: TButton;
    ButtonRemove: TButton;
    ButtonTestServer: TButton;
    ButtonWrite: TButton;
    ComboSecurity: TComboBox;
    EditPassword: TEdit;
    EditScan: TEdit;
    EditUrl: TEdit;
    EditUser: TEdit;
    EditWrite: TEdit;
    GridTags: TStringGrid;
    GridVars: TStringGrid;
    LabelPassword: TLabel;
    LabelScan: TLabel;
    LabelSecurity: TLabel;
    LabelStatus: TLabel;
    LabelUrl: TLabel;
    LabelUser: TLabel;
    LabelVars: TLabel;
    LabelTags: TLabel;
    LabelWrite: TLabel;
    MemoLog: TMemo;
    PanelTags: TPanel;
    PanelTop: TPanel;
    PanelVars: TPanel;
    PanelWrite: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Timer: TTimer;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonMonitorClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonTestServerClick(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FDriver:TOPCUADriver;
    FServer:TOPCUATestServer;
    FVars:TPLCTagInfoArray;
    FTags:array of TMonitored;
    FLogLock:TCriticalSection;
    FPendingLog:TStringList;
    procedure DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
    procedure AddLog(const Msg:String);
    procedure FreeTags;
    procedure FreeDriver;
    procedure Monitor(const Info:TPLCTagInfo);
    function ValueText(const M:TMonitored):String;
    procedure ShowVars;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

type
  //RefreshTime, LongAddress and ProtocolDriver are published by each tag class, not by TPLCTag
  TPLCTagAccess = class(TPLCTag);

const
  KindName:array[TPLCTagKind] of String = ('escalar', 'array', 'struct', 'texto', 'nao suportado');

function ParseNumber(S:String):Double;
var
  fs:TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  S := StringReplace(Trim(S), ',', '.', [rfReplaceAll]);
  Result := StrToFloat(S, fs);
end;

function NumberText(V:Double):String;
var
  fs:TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := FloatToStr(V, fs);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FLogLock := TCriticalSection.Create;
  FPendingLog := TStringList.Create;

  GridVars.Cells[0, 0] := 'Nome';
  GridVars.Cells[1, 0] := 'Tipo';
  GridVars.Cells[2, 0] := 'Forma';
  GridVars.Cells[3, 0] := 'NodeId';
  GridTags.Cells[0, 0] := 'Nome';
  GridTags.Cells[1, 0] := 'Valor';
  GridTags.Cells[2, 0] := 'Status';
  GridTags.Cells[3, 0] := 'Tag';

  //laz_opcua_client <endpoint>: preenche e ja conecta
  //laz_opcua_client --teste: sobe o servidor de teste, conecta e monitora todas as variaveis
  if ParamCount>=1 then begin
    if ParamStr(1)='--teste' then begin
      ButtonTestServerClick(nil);
      if FServer=nil then exit;
    end else
      EditUrl.Text := ParamStr(1);
    ButtonConnectClick(nil);
    if (ParamStr(1)='--teste') and (Length(FVars)>0) then begin
      GridVars.Selection := TGridRect(Rect(0, 1, GridVars.ColCount-1, Length(FVars)));
      ButtonMonitorClick(nil);
    end;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := false;
  FreeDriver;
  FreeAndNil(FServer);
  FreeAndNil(FPendingLog);
  FreeAndNil(FLogLock);
end;

//called from the driver threads too: the message waits in FPendingLog for the timer
procedure TFormMain.DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
begin
  AddLog(Msg);
end;

procedure TFormMain.AddLog(const Msg:String);
begin
  if FLogLock=nil then exit;
  FLogLock.Enter;
  try
    FPendingLog.Add(FormatDateTime('hh:nn:ss.zzz ', Now)+Msg);
  finally
    FLogLock.Leave;
  end;
end;

procedure TFormMain.FreeTags;
var
  i:Integer;
begin
  for i:=0 to High(FTags) do
    FTags[i].Tag.Free;
  SetLength(FTags, 0);
  GridTags.RowCount := 1;
end;

procedure TFormMain.FreeDriver;
begin
  FreeTags; //the tags first: a tag unregisters itself from its driver
  FreeAndNil(FDriver);
end;

procedure TFormMain.ButtonTestServerClick(Sender: TObject);
var
  reason:String;
  opt:TOPCUATestServerOptions;
begin
  if FServer<>nil then exit;
  if not OPCUATestLibraryAvailable(reason) then begin
    ShowMessage(reason);
    exit;
  end;
  opt := OPCUATestDefaultOptions;
  opt.Port := 48400; //not 4840: a real server may be listening on this computer
  try
    FServer := TOPCUATestServer.Create(opt);
  except
    on e:Exception do begin
      ShowMessage(e.Message);
      exit;
    end;
  end;
  EditUrl.Text := FServer.Url;
  ButtonTestServer.Enabled := false;
  AddLog('Servidor de teste em '+FServer.Url+', namespace '+OPCUATestNamespaceUri);
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
var
  r:TProtocolIOResult;
begin
  //a new driver for every connection: the settings below are read when it connects
  FreeDriver;
  FDriver := TOPCUADriver.Create(Self);
  FDriver.EndpointUrl := Trim(EditUrl.Text);
  FDriver.Username := EditUser.Text;
  FDriver.Password := EditPassword.Text;
  case ComboSecurity.ItemIndex of
    1: FDriver.SecurityMode := osmSign;
    2: FDriver.SecurityMode := osmSignAndEncrypt;
    else FDriver.SecurityMode := osmNone;
  end;
  //where the generated client certificate is kept (Sign/SignAndEncrypt)
  FDriver.CertificateDir := ExtractFilePath(Application.ExeName)+'pki';
  FDriver.LibraryName := GetEnvironmentVariable('OPEN62541_LIB');
  FDriver.LogLevel := ollInfo;
  FDriver.OnLog := @DriverLog;

  Screen.Cursor := crHourGlass;
  try
    r := FDriver.ListTags(FVars);
  finally
    Screen.Cursor := crDefault;
  end;
  if r<>ioOk then begin
    SetLength(FVars, 0);
    ShowVars;
    AddLog('ListTags: '+r.ToString+' '+FDriver.LastError);
    ShowMessage('Nao conectou ('+r.ToString+'): '+FDriver.LastError);
    exit;
  end;
  ShowVars;
  AddLog(IntToStr(Length(FVars))+' variaveis listadas');
end;

procedure TFormMain.ShowVars;
var
  i:Integer;
begin
  GridVars.RowCount := Length(FVars)+1;
  for i:=0 to High(FVars) do begin
    GridVars.Cells[0, i+1] := FVars[i].Name;
    GridVars.Cells[1, i+1] := FVars[i].NativeType;
    if FVars[i].Kind=tkArray then
      GridVars.Cells[2, i+1] := KindName[FVars[i].Kind]+'['+IntToStr(FVars[i].ElementCount)+']'
    else
      GridVars.Cells[2, i+1] := KindName[FVars[i].Kind];
    GridVars.Cells[3, i+1] := FVars[i].Path;
  end;
end;

procedure TFormMain.Monitor(const Info:TPLCTagInfo);
var
  t:TPLCTag;
  n:Integer;
begin
  if FDriver=nil then exit;
  case Info.Kind of
    tkScalar:
      t := TPLCTagNumber.Create(Self);
    tkArray: begin
      t := TPLCBlock.Create(Self);
      TPLCBlock(t).Size := Info.ElementCount;
    end;
    tkString: begin
      t := TPLCString.Create(Self);
      TPLCString(t).StringType := stC;
      TPLCString(t).StringSize := 80;
    end;
    else begin
      AddLog(Info.Name+': '+Info.NativeType+' nao tem tag equivalente');
      exit;
    end;
  end;
  TPLCTagAccess(t).RefreshTime := StrToIntDef(EditScan.Text, 500); //sampling interval of the monitored item
  TPLCTagAccess(t).LongAddress := Info.Path;
  TPLCTagAccess(t).ProtocolDriver := FDriver;
  n := Length(FTags);
  SetLength(FTags, n+1);
  FTags[n].Info := Info;
  FTags[n].Tag := t;
  GridTags.RowCount := n+2;
  GridTags.Cells[0, n+1] := Info.Name;
  GridTags.Cells[3, n+1] := t.ClassName;
end;

procedure TFormMain.ButtonMonitorClick(Sender: TObject);
var
  r:Integer;
begin
  for r:=GridVars.Selection.Top to GridVars.Selection.Bottom do
    if (r>=1) and (r<=Length(FVars)) then
      Monitor(FVars[r-1]);
end;

procedure TFormMain.ButtonRemoveClick(Sender: TObject);
var
  r, i:Integer;
begin
  r := GridTags.Row-1;
  if (r<0) or (r>High(FTags)) then exit;
  FTags[r].Tag.Free;
  for i:=r to High(FTags)-1 do
    FTags[i] := FTags[i+1];
  SetLength(FTags, Length(FTags)-1);
  GridTags.DeleteRow(r+1);
end;

procedure TFormMain.ButtonWriteClick(Sender: TObject);
var
  r, i:Integer;
  t:TPLCTag;
  parts:TStringArray;
  values:TArrayOfDouble;
begin
  r := GridTags.Row-1;
  if (r<0) or (r>High(FTags)) then exit;
  t := FTags[r].Tag;
  try
    //SyncWrites: the write is done now and its result is in LastSyncWriteStatus
    if t is TPLCTagNumber then begin
      TPLCTagNumber(t).SyncWrites := true;
      TPLCTagNumber(t).Value := ParseNumber(EditWrite.Text);
    end else if t is TPLCString then begin
      TPLCString(t).SyncWrites := true;
      TPLCString(t).Value := EditWrite.Text;
    end else if t is TPLCBlock then begin
      TPLCBlock(t).SyncWrites := true;
      parts := SplitString(EditWrite.Text, ';');
      SetLength(values, Length(parts));
      for i:=0 to High(parts) do
        values[i] := ParseNumber(parts[i]);
      //from element 0; fewer values than the array size write only those elements
      TPLCBlock(t).Write(values, Length(values), 0);
    end;
  except
    on e:Exception do begin
      ShowMessage('Valor invalido: '+e.Message);
      exit;
    end;
  end;
  AddLog('Gravar '+FTags[r].Info.Name+' = '+EditWrite.Text+': '+t.LastSyncWriteStatus.ToString);
end;

function TFormMain.ValueText(const M:TMonitored):String;
var
  v:TArrayOfDouble;
  i:Integer;
begin
  if (M.Tag is TPLCTagNumber) and SameText(M.Info.NativeType, 'DateTime') then
    //a DateTime variable arrives as a TDateTime, in local time
    Result := FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz', TPLCTagNumber(M.Tag).Value)
  else if M.Tag is TPLCTagNumber then
    Result := NumberText(TPLCTagNumber(M.Tag).Value)
  else if M.Tag is TPLCString then
    Result := TPLCString(M.Tag).Value
  else if M.Tag is TPLCBlock then begin
    v := TPLCBlock(M.Tag).ValuesRaw;
    Result := '';
    for i:=0 to High(v) do begin
      if i>0 then Result := Result+'; ';
      Result := Result+NumberText(v[i]);
    end;
  end else
    Result := '';
end;

procedure TFormMain.TimerTimer(Sender: TObject);
var
  i:Integer;
  lines:TStringList;
begin
  for i:=0 to High(FTags) do begin
    GridTags.Cells[1, i+1] := ValueText(FTags[i]);
    GridTags.Cells[2, i+1] := FTags[i].Tag.LastASyncReadStatus.ToString;
  end;

  if FDriver=nil then
    LabelStatus.Caption := 'Sem driver'
  else if FDriver.Connected then
    LabelStatus.Caption := 'Conectado a '+FDriver.EndpointUrl
  else
    LabelStatus.Caption := 'Desconectado '+FDriver.LastError;

  lines := nil;
  FLogLock.Enter;
  try
    if FPendingLog.Count>0 then begin
      lines := TStringList.Create;
      lines.Assign(FPendingLog);
      FPendingLog.Clear;
    end;
  finally
    FLogLock.Leave;
  end;
  if lines<>nil then
    try
      MemoLog.Lines.AddStrings(lines);
      while MemoLog.Lines.Count>500 do
        MemoLog.Lines.Delete(0);
      MemoLog.SelStart := Length(MemoLog.Text);
    finally
      lines.Free;
    end;
end;

end.
