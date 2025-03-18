unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  ExtCtrls, udmdb, IniFiles, syncobjs, RTTICtrls, WinCCUserManagement, HMIEdit,
  ControlSecurityManager, HMILabel, PLCTagNumber, fphttpserver, fpjson,
  jsonparser;

type

  { TSecWSThread }

  TSecWSThread = class(TThread)
  private
    FUser, FPass, FSecurityCode:String;
    FLoginOk:Boolean;
    ws:TFPHttpServer;
    cs:TCriticalSection;
    procedure AcceptIdle(Sender: TObject);
    procedure LeSCNoArquivoIni(aJArray: TJSONArray);
    function  SecurityCodeReplaced(aOriginalSecCode:String; var aReplacedSecurityCode:String):Boolean;
    procedure RequestError(Sender: TObject; E: Exception);
    procedure WSRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure ChecarUsuarioNaAplicacaoComWinCC;
  protected
    function RegistraSCNoArquivoIni(aSecurityCode:UTF8String):Boolean;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
  DefaultStackSize);
    destructor Destroy; override;
  end;


  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    HMIEdit1: THMIEdit;
    HMILabel1: THMILabel;
    Label1: TLabel;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    PLCTagNumber1: TPLCTagNumber;
    Timer1: TTimer;
    WinCCUserManagement1: TWinCCUserManagement;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    wst:TSecWSThread;
    procedure WSTTerminated(Sender: TObject);
    procedure IniciaWSThread;
  public

  end;

var
  Form1: TForm1;

const
  SectionName = 'Securty Override';
  IniFileName = 'securitycodeoverrides.ini';

implementation

{$R *.lfm}

{ TSecWSThread }

procedure TSecWSThread.AcceptIdle(Sender: TObject);
begin
  if Terminated and Assigned(ws) then
    ws.Active:=false;
end;

procedure TSecWSThread.RequestError(Sender: TObject; E: Exception);
begin
  //nao faz nada.
end;

procedure TSecWSThread.WSRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  auxS1, auxS2, newSecCode: String;
  logindata: TJSONData = nil;
  username, password, securitycode:TJSONString;
  dm: Tdmdb;
  uid: TJSONNumber;
  AsString: TJSONStringType;
  a, b, c: Boolean;
  r: Integer;
  ja: TJSONArray;
  ms: TStringStream;
begin
  auxS1 := ARequest.URL;
  auxS2 := ARequest.URI;

  try
    auxS1:=ARequest.Method;
    if ARequest.Method<>'POST' then exit;

    AResponse.ContentType:='application/json';
    AResponse.AcceptCharset:='utf-8';
    case ARequest.URL of
      '/checkuserpwd': begin
        try
          logindata := GetJSON(ARequest.Content);
        except
          AResponse.Content := 'Requisicao invalida';
          AResponse.Code    := 500;
          exit;
        end;
        if Assigned(logindata) and
           (logindata is TJSONObject) and
           TJSONObject(logindata).Find('user',username) and
           TJSONObject(logindata).Find('password',password)
        then begin
          FLoginOk:=false;
          FUser:=username.AsString;
          FPass:=password.AsString;
          Synchronize(@ChecarUsuarioNaAplicacaoComWinCC);

          if FLoginOk then begin;
            try
              dm:=Tdmdb.Create(nil);
              try
                dm.BuscaUserName.Close;
                dm.BuscaUserName.ParamByName('username').AsString:=FUser;
                dm.BuscaUserName.Open;
                if dm.BuscaUserName.RecordCount=1 then begin
                  AResponse.Content := '{ "UID": '+dm.BuscaUserNameID.AsString+' }';
                  AResponse.Code    := 200;
                end else begin
                  AResponse.Content := '{"error": "Tabela de usuarios retornou mais de um usuario com mesmo nome" }';
                  AResponse.Code    := 500;
                end;
              finally
                FreeAndNil(dm);
              end;
            except
              on e:Exception do begin
                AResponse.Content := '{"error": "Erro criando conexao com SQLServer WinCC" }';
                AResponse.Code    := 500;
              end;
            end;

          end else begin
            AResponse.Code    := 401;
          end;
        end else begin
          AResponse.Content := 'Requisicao JSON mal formada';
          AResponse.Code    := 500;
          exit;
        end;
      end;
      '/checkuserchipcard': begin
        AResponse.Content := '{"error:" "Login por chipcard/autenticacao via RFID nao suportada" }';
        AResponse.Code    := 501;
      end;
      '/uidcanaccess': begin
        try
          logindata := GetJSON(ARequest.Content);
        except
          AResponse.Content := 'Requisicao invalida';
          AResponse.Code    := 500;
          exit;
        end;
        if Assigned(logindata) and
           (logindata is TJSONObject) and
           TJSONObject(logindata).Find('uid',uid) and
           TJSONObject(logindata).Find('securitycode',SecurityCode)
        then begin

          try
            dm:=Tdmdb.Create(nil);
            try
              dm.UIDCanAccessTable.Close;
              dm.UIDCanAccessTable.ParamByName('uid').AsInteger:=uid.AsInteger;
              dm.UIDCanAccessTable.ParamByName('securitycode').AsString:=SecurityCode.AsString;
              dm.UIDCanAccessTable.Open;
              if dm.UIDCanAccessTable.RecordCount=1 then begin
                AResponse.Code := 200; //o UID X TEM acesso ao security code ORIGINAL
              end else begin

                if SecurityCodeReplaced(SecurityCode.AsString, newSecCode) then begin
                  dm.UIDCanAccessTable.Close;
                  dm.UIDCanAccessTable.ParamByName('uid').AsInteger:=uid.AsInteger;
                  dm.UIDCanAccessTable.ParamByName('securitycode').AsString:=newSecCode;
                  dm.UIDCanAccessTable.Open;

                  if dm.UIDCanAccessTable.RecordCount=1 then begin
                    AResponse.Code    := 200; //o UID X TEM acesso ao security code substitudo
                  end else
                    AResponse.Code    := 403; //o UID X NAO TEM acesso ao security code substitudo
                end else
                  AResponse.Code    := 403; //security code sem substituto
              end;
            finally
              FreeAndNil(dm);
            end;
          except
            on e:Exception do begin
              AResponse.Content := '{"error": "Erro criando conexao com SQLServer WinCC" }';
              AResponse.Code    := 500;
            end;
          end;
        end else begin
          AResponse.Content := 'Requisicao JSON mal formada';
          AResponse.Code    := 500;
          exit;
        end;
      end;
      '/validadesecuritycode': begin
        AResponse.Code:=200;
      end;
      '/registersecuritycode': begin
        try
          logindata := GetJSON(ARequest.Content);
        except
          AResponse.Content := 'Requisicao invalida';
          AResponse.Code    := 500;
          exit;
        end;
        a:=Assigned(logindata);
        b:=(logindata is TJSONObject);
        if a and b then
          c:=TJSONObject(logindata).Find('securitycode',securitycode);
        if a and b and c then begin
          try
            dm:=Tdmdb.Create(nil);
            try
              dm.REGISTERsecuritycode.Close;
              dm.REGISTERsecuritycode.ParamByName('tipobusca').AsInteger   := 0;
              dm.REGISTERsecuritycode.ParamByName('securitycode').AsString := securitycode.AsString;
              dm.REGISTERsecuritycode.Open;
              if dm.REGISTERsecuritycode.RecordCount=1 then begin
                AResponse.Code    := 200;
              end else begin
                if RegistraSCNoArquivoIni(securitycode.AsString) then
                  AResponse.Code    := 200
                else
                  AResponse.Code    := 404;
              end;
            finally
              FreeAndNil(dm);
            end;
          except
            on e:Exception do begin
              AResponse.Content := '{"error": "Erro não encontrado" }';
              AResponse.Code    := 404;
            end;
          end;
        end else begin
          AResponse.Content := 'Requisicao JSON mal formada';
          AResponse.Code    := 500;
          exit;
        end;
      end;
      '/enumsecuritycodes': begin
        dm:=Tdmdb.Create(nil);
        try
          dm.REGISTERsecuritycode.Close;
          dm.REGISTERsecuritycode.ParamByName('tipobusca').AsInteger   := 1;
          dm.REGISTERsecuritycode.ParamByName('securitycode').AsString := '';
          dm.REGISTERsecuritycode.Open;

          ja:=TJSONArray.Create;
          try
            for r:=1 to dm.REGISTERsecuritycode.RecordCount do begin
              dm.REGISTERsecuritycode.RecNo:=r;
              ja.Add(dm.REGISTERsecuritycodeL1033.AsString);
            end;

            LeSCNoArquivoIni(ja);

            ms:=TStringStream.Create(ja.AsJSON);
            try
              ms.Position:=0;
              AResponse.ContentStream:=ms;
              AResponse.ContentLength:=ms.Size;
              AResponse.Code:=200;
              AResponse.SendContent;
            finally
              FreeAndNil(ms);
            end;
          finally
            FreeAndNil(ja);
          end;
        finally
          dm.SQLServerWINCC.Disconnect;
          FreeAndNil(dm);
        end;
      end;

    end;

  finally
    if AResponse.Code=0 then
      AResponse.Code:=404;
    if not AResponse.ContentSent then
      AResponse.SendContent;
  end;
end;

procedure TSecWSThread.ChecarUsuarioNaAplicacaoComWinCC;
var
  aux:Integer;
begin
  Sleep(1000);
  if Assigned(Form1) and Assigned(Form1.WinCCUserManagement1) then begin
    FLoginOk:=Form1.WinCCUserManagement1.Login(FUser,FPass,aux);
  end else
    FLoginOk:=false;
end;

function TSecWSThread.RegistraSCNoArquivoIni(aSecurityCode: UTF8String
  ): Boolean;
var
  inif: TIniFile;

begin
  cs.Enter;
  try
    inif:=TIniFile.Create(ExtractFilePath(ParamStr(0))+IniFileName);
    try
      if not inif.ValueExists(SectionName, aSecurityCode) then begin
        inif.WriteString(SectionName,aSecurityCode,'');
        exit(false);
      end else begin
        exit(inif.ReadString(SectionName, aSecurityCode,'').Trim.IsEmpty=false);
      end;
    finally
      FreeAndNil(inif);
    end;
  finally
    cs.Leave;
  end;
end;

procedure TSecWSThread.LeSCNoArquivoIni(aJArray: TJSONArray);
var
  inif: TIniFile;
  values: TStringList;
  i: Integer;
begin
  if not Assigned(aJArray) then exit;

  cs.Enter;
  try
    inif:=TIniFile.Create(ExtractFilePath(ParamStr(0))+IniFileName);
    try
      values:=TStringList.Create;
      try
        inif.ReadSection(SectionName,values);
        for i:=0 to values.Count-1 do begin
          if not inif.ReadString(SectionName, values.Strings[i],'').Trim.IsEmpty then
            aJArray.Add(values.Strings[i]);
        end;
      finally
        FreeAndNil(values);
      end;
    finally
      FreeAndNil(inif);
    end;
  finally
    cs.Leave;
  end;
end;

function TSecWSThread.SecurityCodeReplaced(aOriginalSecCode: String;
  var aReplacedSecurityCode: String): Boolean;
var
  inif: TIniFile;
  auxS: String;
begin
  Result:=false;
  cs.Enter;
  try
    inif:=TIniFile.Create(ExtractFilePath(ParamStr(0))+IniFileName);
    try
      Result:=inif.ValueExists(SectionName, aOriginalSecCode);
      if Result then begin

        auxS:=inif.ReadString(SectionName, aOriginalSecCode,'');

        if not auxS.Trim.IsEmpty then begin
          aReplacedSecurityCode:=auxS;
          exit(true);
        end;
      end;
    finally
      FreeAndNil(inif);
    end;
  finally
    cs.Leave;
  end;
end;

procedure TSecWSThread.Execute;
begin
  while not Terminated do begin
    ws:=TFPHttpServer.Create(nil);
    try
      ws.AdminName:='GP2e';
      ws.AdminMail:='coordenacao@gp2e.com.br';
      ws.AcceptIdleTimeout:=50;
      ws.Port          :=7561;
      ws.OnAcceptIdle  :=@AcceptIdle;
      ws.OnRequest     :=@WSRequest;
      ws.OnRequestError:=@RequestError;
      ws.Active:=true;
    finally
      if Assigned(ws) then
        FreeAndNil(ws);
    end;
  end;
  if Assigned(cs) then FreeAndNil(cs);
end;

constructor TSecWSThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  cs:=TCriticalSection.Create;
end;

destructor TSecWSThread.Destroy;
begin
  inherited Destroy;
  if Assigned(cs) then FreeAndNil(cs);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  IniciaWSThread;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := WinCCUserManagement1.CurrentUserLogin;
end;

procedure TForm1.WSTTerminated(Sender: TObject);
begin
  if (Sender=wst) then
    wst:=nil;

  if (not Application.Terminated) and (wst=nil) then begin
    IniciaWSThread;
  end;
end;

procedure TForm1.IniciaWSThread;
begin
  wst:=TSecWSThread.Create(true);
  wst.FreeOnTerminate:=true;
  wst.OnTerminate:=@WSTTerminated;
  wst.Start;
end;

end.

