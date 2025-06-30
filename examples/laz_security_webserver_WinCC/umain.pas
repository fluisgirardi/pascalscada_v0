unit umain;

{$mode objfpc}{$H+}

interface

uses
  sslbase, sslsockets, fpopenssl, openssl, opensslsockets, Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls, ExtCtrls, DBGrids,
  udmdb, IniFiles, syncobjs, eventlog, RTTICtrls, WinCCUserManagement, HMIEdit,
  ControlSecurityManager, HMILabel, PLCTagNumber, PLCString, tcp_udpport,
  ISOTCPDriver, fphttpserver, fpjson, jsonparser, ActiveX, Sockets, StrUtils,
  CrossEvent;

type
  TJSONObjectExt = class helper for TJSONObject
    function AddOrSet(const AName: TJSONStringType; AValue: TJSONData): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; AValue: Boolean): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; AValue: TJSONFloat): Integer; overload;
    function AddOrSet(const AName, AValue: TJSONStringType): Integer; overload;
    function AddOrSet(const AName : String; AValue: TJSONUnicodeStringType): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; Avalue: Int64): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; Avalue: QWord): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; Avalue: Integer): Integer; overload;
    function AddOrSet(const AName: TJSONStringType): Integer; overload;
    function AddOrSet(const AName: TJSONStringType; AValue : TJSONArray): Integer; overload;
  end;

  TLogEntry = record
    EvtType:TEventType;
    EvtMsg, From:String;
  end;
  PLogEntry = ^TLogEntry;

  THTTPServer = class(TFPHttpServer)
  public
    property ConnectionCount;
  end;

  { TSecWSThread }

  TSecWSThread = class(TThread)
  private
    FUser, FPass,
    FCurrUserLogin:String;
    FLoginOk:Boolean;
    UserChanged: TCrossEvent;
    ws:THTTPServer;
    cs:TCriticalSection;
    QueuedClients: cardinal;
    //log:TEventLog;
    //FEvtType:TEventType;
    //FEvtMsg:String;
    msgList:TThreadList;
    procedure AcceptIdle(Sender: TObject);
    procedure LeSCNoArquivoIni(aJArray: TJSONArray);
    procedure GetAuthorizedReplacedSC(var aOriginalAuths:TJSONObject);
    function  SecurityCodeReplaced(aOriginalSecCode:String; var aReplacedSecurityCode:String):Boolean;
    procedure RequestError(Sender: TObject; E: Exception);
    procedure WSRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure ChecarUsuarioNaAplicacaoComWinCC;
    procedure SyncLog;
    procedure GetLoggedWinCCUser;
  protected
    function IPToString(aIP:in_addr):String;
    procedure AddToLog(EvtType: TEventType; aFrom, aMsg: String);
    function RegistraSCNoArquivoIni(aSecurityCode:UTF8String):Boolean;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
  DefaultStackSize);
    destructor Destroy; override;
    function SetUserChanged: String;
    procedure Terminate2;

  end;


  { TForm1 }

  TForm1 = class(TForm)
    EventLog1: TEventLog;
    Memo1: TMemo;
    S7_CLP1: TISOTCPDriver;
    TCP_UDPPort1: TTCP_UDPPort;
    Timer1: TTimer;
    UserName: TPLCString;
    WinCCUserManagement1: TWinCCUserManagement;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    wst:TSecWSThread;
    procedure WSTTerminated(Sender: TObject);
    procedure IniciaWSThread;
  public

  end;

var
  Form1: TForm1;
  FLastUserloggedIn: String;

const
  SectionName = 'Securty Override';
  IniFileName = 'securitycodeoverrides.ini';

implementation

{$R *.lfm}

{ TJSONObjectExt }

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType; AValue: TJSONData
  ): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType; AValue: Boolean
  ): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType;
  AValue: TJSONFloat): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName, AValue: TJSONStringType): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: String;
  AValue: TJSONUnicodeStringType): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType; Avalue: Int64
  ): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType; Avalue: QWord
  ): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType; Avalue: Integer
  ): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName));
end;

function TJSONObjectExt.AddOrSet(const AName: TJSONStringType;
  AValue: TJSONArray): Integer;
var
  aux: TJSONData;
begin
  aux:=Find(AName);
  if assigned(aux) then begin
    Remove(aux);
  end;
  exit(Add(AName, AValue));
end;

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
  newSecCode, cliIPAddr, aCurrUserLogin: String;
  logindata: TJSONData = nil;
  username, password, securitycode:TJSONString;
  jUseCentralUserAsLocalUser:TJSONBoolean;
  dm: Tdmdb;
  uid: TJSONNumber;
  jBool:TJSONBoolean;
  UseCentralUserAsLocalUser, IncDec: Boolean;
  r, aUID: Integer;
  ja: TJSONArray;
  ms: TStringStream;
  jobj, jAuth: TJSONObject;
  jsonStr: TJSONStringType;
  aux: Cardinal;
  wr:TWaitResult;
  reqData: TJSONData;

  procedure CentralUserData(aObj:TJSONObject);
  const
    bVal:array[low(boolean)..high(Boolean)] of string = ('false', 'true');
  begin
    //Result:='';
    if UseCentralUserAsLocalUser and Assigned(aObj) then begin
      aObj.AddOrSet('UseCentralUserAsLocalUser', UseCentralUserAsLocalUser);
      aObj.AddOrSet('uid', aUID);
      aObj.AddOrSet('login',aCurrUserLogin);
      //Result:=aPrefix+'"UseCentralUserAsLocalUser": '+bVal[UseCentralUserAsLocalUser]+', "uid": '+aUID.ToString+', "login": "'+aCurrUserLogin+'"';
    end;
  end;

begin
  if Terminated then begin
    AResponse.Code:=405;
    exit;
  end;

  CoInitialize(nil);
  try
    try
      cliIPAddr:=IPToString(ARequest.Connection.Socket.RemoteAddress.sin_addr);
      AddToLog(etInfo,  cliIPAddr, ARequest.Method.ToUpper.Trim+' '+ARequest.URL);
      AddToLog(etDebug, cliIPAddr, 'Request content: '+ARequest.Content);
      if ARequest.Method.ToUpper.Trim<>'POST' then begin
        AResponse.Code:=405;
        exit;
      end;


      AResponse.ContentType:='application/json';
      AResponse.AcceptCharset:='utf-8';
      UseCentralUserAsLocalUser := false;
      case ARequest.URL of
        '/checkuserpwd': begin
          try
            logindata := GetJSON(ARequest.Content);
          except
            AResponse.Content := '{"error": "Requisicao invalida"}';
            AResponse.Code    := 500;
            exit;
          end;
          try
            if Assigned(logindata) and
               (logindata is TJSONObject) and
               TJSONObject(logindata).Find('user',username) and
               TJSONObject(logindata).Find('password',password)
            then begin
              FLoginOk:=false;
              FUser:=username.AsString;
              FPass:=password.AsString;
              Synchronize(@ChecarUsuarioNaAplicacaoComWinCC);

              if FLoginOk then begin
                try
                  dm:=Tdmdb.Create(nil);
                  try
                    dm.SQLServerWINCC.Connect;
                    dm.BuscaUserName.Close;
                      dm.BuscaUserName.ParamByName('username').Value:=FUser;
                      dm.BuscaUserName.Open;
                      if dm.BuscaUserName.RecordCount=1 then begin
                        jobj:=TJSONObject.Create;
                        try
                          jobj.add('uid',dm.BuscaUserNameID.Value);
                          dm.AuthorizationsFromUser.Close;
                          dm.AuthorizationsFromUser.ParamByName('uid').AsInteger:=dm.BuscaUserNameID.Value;
                          dm.AuthorizationsFromUser.Open;
                          jAuth:=TJSONObject.Create;
                          for r:=1 to dm.AuthorizationsFromUser.RecordCount do begin
                            dm.AuthorizationsFromUser.RecNo:=r;
                            jAuth.AddOrSet(dm.AuthorizationsFromUserL1046.AsString, true);
                          end;
                          GetAuthorizedReplacedSC(jAuth);
                          jobj.Add('authorizations',jAuth);
                          AResponse.Content := jobj.AsJSON;
                          AResponse.Code    := 200;
                        finally
                          FreeAndNil(jobj);
                        end;
                      end else begin
                        AResponse.Content := '{"error": "Tabela de usuarios retornou zero ou mais de um usuario com mesmo nome" }';
                        AResponse.Code    := 500;
                      end;
                  finally
                    FreeAndNil(dm);
                  end;
                except
                  on e:Exception do begin
                    jobj:=TJSONObject.Create();
                    try
                      jobj.AddOrSet('error', e.Message);
                      AResponse.Content := jobj.AsJSON;
                      AResponse.Code    := 500;
                    finally
                      FreeAndNil(jobj);
                    end;
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
          finally
            if Assigned(logindata) then
              FreeAndNil(logindata);
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
            AResponse.Content := '{"error": "Requisicao invalida"}';
            AResponse.Code    := 500;
            exit;
          end;
          try
            if Assigned(logindata) and
               (logindata is TJSONObject) and
               TJSONObject(logindata).Find('uid',uid) and
               TJSONObject(logindata).Find('securitycode',SecurityCode)
            then begin
              aUID:=uid.AsInteger;
              if TJSONObject(logindata).Find('UseCentralUserAsLocalUser',jUseCentralUserAsLocalUser) and jUseCentralUserAsLocalUser.AsBoolean then begin
                Synchronize(@GetLoggedWinCCUser);
                aCurrUserLogin:=FCurrUserLogin;
                dm:=Tdmdb.Create(nil);
                try
                  dm.SQLServerWINCC.Connect;
                  dm.BuscaUserName.Close;
                  dm.BuscaUserName.ParamByName('username').Value:=aCurrUserLogin;
                  dm.BuscaUserName.Open;
                  if dm.BuscaUserName.RecordCount=1 then begin
                    UseCentralUserAsLocalUser := true;
                    aUID := dm.BuscaUserNameID.Value;
                  end;
                  dm.SQLServerWINCC.Disconnect;
                finally
                  FreeAndNil(dm);
                end;
              end;


              try
                dm:=Tdmdb.Create(nil);
                try
                  dm.SQLServerWINCC.Connect;
                  jobj:=TJSONObject.Create;
                  try
                    if uid.AsInt64<>aUID then begin
                      dm.AuthorizationsFromUser.Close;
                      dm.AuthorizationsFromUser.ParamByName('uid').AsInteger:=aUID;
                      dm.AuthorizationsFromUser.Open;
                      jAuth:=TJSONObject.Create;
                      for r:=1 to dm.AuthorizationsFromUser.RecordCount do begin
                        dm.AuthorizationsFromUser.RecNo:=r;
                        jAuth.AddOrSet(dm.AuthorizationsFromUserL1046.AsString, true);
                      end;
                      GetAuthorizedReplacedSC(jAuth);
                      jobj.Add('authorizations',jAuth);
                    end;

                    CentralUserData(jobj);

                    dm.UIDCanAccessTable.Close;
                    dm.UIDCanAccessTable.ParamByName('uid').AsInteger:=aUID;
                    dm.UIDCanAccessTable.ParamByName('securitycode').Value:=securitycode.AsString;
                    dm.UIDCanAccessTable.Open;
                    if dm.UIDCanAccessTable.RecordCount>=1 then begin
                      jobj.AddOrSet('WinCCAuthCount', dm.UIDCanAccessTable.RecordCount);
                      AResponse.Content:= jobj.AsJSON;
                      AResponse.Code := 200; //o UID X TEM acesso ao security code ORIGINAL
                    end else begin

                      if SecurityCodeReplaced(SecurityCode.AsString, newSecCode) then begin
                        dm.UIDCanAccessTable.Close;
                        dm.UIDCanAccessTable.ParamByName('uid').AsInteger:=uid.AsInteger;
                        dm.UIDCanAccessTable.ParamByName('securitycode').AsString:=UTF8Encode(newSecCode);
                        dm.UIDCanAccessTable.Open;

                        if dm.UIDCanAccessTable.RecordCount=1 then begin
                          jobj.AddOrSet('WinCCReplacedAuthCount', dm.UIDCanAccessTable.RecordCount);
                          AResponse.Content:=jobj.AsJSON;
                          AResponse.Code    := 200; //o UID X TEM acesso ao security code substitudo
                        end else begin
                          jsonStr:=jobj.AsJSON;
                          AResponse.Content:=jobj.AsJSON;
                          AResponse.Code    := 403; //o UID X NAO TEM acesso ao security code substitudo
                        end;
                      end else begin
                        jsonStr:=jobj.AsJSON;
                        AResponse.Content:=jobj.AsJSON;
                        AResponse.Code    := 403; //security code sem substituto
                      end;
                    end;
                  finally
                    FreeAndNil(jobj);
                  end;
                finally
                  FreeAndNil(dm);
                end;
              except
                on e:Exception do begin
                  jobj:=TJSONObject.Create();
                  try
                    jobj.AddOrSet('error', e.Message);
                    AResponse.Content := jobj.AsJSON;
                    AResponse.Code    := 500;
                  finally
                    FreeAndNil(jobj);
                  end;
                end;
              end;
            end else begin
              AResponse.Content := '{"error": "Requisicao mal formada"}';
              AResponse.Code    := 500;
              exit;
            end;
          finally
            if Assigned(logindata) then
              FreeAndNil(logindata);
          end;
        end;
        '/validadesecuritycode': begin
          AResponse.Code:=200;
        end;
        '/registersecuritycode': begin
          try
            logindata := GetJSON(ARequest.Content);
          except
            AResponse.Content := '{"error": "Requisicao invalida"}';
            AResponse.Code    := 500;
            exit;
          end;
          try
            if Assigned(logindata) and (logindata is TJSONObject) and TJSONObject(logindata).Find('securitycode',securitycode) then begin
              try
                dm:=Tdmdb.Create(nil);
                try
                  dm.SQLServerWINCC.Connect;
                  dm.REGISTERsecuritycode.Close;
                  dm.REGISTERsecuritycode.ParamByName('tipobusca').AsInteger   := 0;
                  dm.REGISTERsecuritycode.ParamByName('securitycode').Value := securitycode.AsString;
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
                  jobj:=TJSONObject.Create();
                  try
                    jobj.AddOrSet('error', e.Message);
                    AResponse.Content := jobj.AsJSON;
                    AResponse.Code    := 500;
                  finally
                    FreeAndNil(jobj);
                  end;
                end;
              end;
            end else begin
              AResponse.Content := '{"error": "JSON mal formado"}';
              AResponse.Code    := 500;
              exit;
            end;
          finally
            if Assigned(logindata) then
              FreeAndNil(logindata);
          end;
        end;
        '/enumsecuritycodes': begin
          dm:=Tdmdb.Create(nil);
          try
            dm.SQLServerWINCC.Connect;
            dm.REGISTERsecuritycode.Close;
            dm.REGISTERsecuritycode.ParamByName('tipobusca').AsInteger := 1;
            dm.REGISTERsecuritycode.ParamByName('securitycode').Value  := '';
            dm.REGISTERsecuritycode.Open;

            ja:=TJSONArray.Create;
            try
              for r:=1 to dm.REGISTERsecuritycode.RecordCount do begin
                dm.REGISTERsecuritycode.RecNo:=r;
                ja.Add(dm.REGISTERsecuritycodeL1046.AsString);
              end;

              LeSCNoArquivoIni(ja);

              ms:=TStringStream.Create(ja.AsJSON);
              try
                ms.Position:=0;
                AResponse.ContentStream:=ms;
                AResponse.ContentLength:=ms.Size;
                AResponse.Code:=200;
                AResponse.SendContent;
                AddToLog(etInfo, cliIPAddr, 'Response: '+AResponse.Code.ToString+' '+ms.DataString);
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
        '/userchanged': begin
          try
            reqData := GetJSON(ARequest.Content);
          except
            AResponse.Content := '{"error": "Requisicao invalida"}';
            AResponse.Code    := 500;
            exit;
          end;

          try
            if (reqData is TJSONObject) and TJSONObject(reqData).Find('wait', jBool) and (jBool.AsBoolean=false) then begin
              IncDec:=false;
              wr:=wrSignaled
            end else begin
              IncDec:=true;
              InterlockedIncrement(QueuedClients);
              //wait 120s for a user change
              wr:=UserChanged.WaitFor(120000);
            end;

            case wr of
              wrSignaled: begin
                try
                  dm:=Tdmdb.Create(nil);
                  try
                    dm.SQLServerWINCC.Connect;
                    dm.BuscaUserName.Close;
                    dm.BuscaUserName.ParamByName('username').Value:=FLastUserLoggedIn;
                    dm.BuscaUserName.Open;
                    case dm.BuscaUserName.RecordCount of
                      0: ;
                      1: begin
                        jobj:=TJSONObject.Create;
                        try
                          jobj.AddOrSet('uid',dm.BuscaUserNameID.Value);
                          jobj.AddOrSet('username',FLastUserLoggedIn);
                          CentralUserData(jobj);
                          dm.AuthorizationsFromUser.Close;
                          dm.AuthorizationsFromUser.ParamByName('uid').AsInteger:=dm.BuscaUserNameID.Value;
                          dm.AuthorizationsFromUser.Open;
                          jAuth:=TJSONObject.Create;
                          for r:=1 to dm.AuthorizationsFromUser.RecordCount do begin
                            dm.AuthorizationsFromUser.RecNo:=r;
                            jAuth.AddOrSet(dm.AuthorizationsFromUserL1046.AsString, true);
                          end;
                          GetAuthorizedReplacedSC(jAuth);
                          jobj.Add('authorizations',jAuth);
                          AResponse.Content := jobj.AsJSON;
                          AResponse.Code    := 200;
                        finally
                          FreeAndNil(jobj);
                        end;
                      end
                      else begin
                        AResponse.Content := '{"error": "Tabela de usuarios retornou mais de um usuario com mesmo nome" }';
                        AResponse.Code    := 500;
                      end;
                    end;
                  finally
                    FreeAndNil(dm);
                  end;
                except
                  on e:Exception do begin
                    jobj:=TJSONObject.Create();
                    try
                      jobj.AddOrSet('error', e.Message);
                      AResponse.Content := jobj.AsJSON;
                      AResponse.Code    := 500;
                    finally
                      FreeAndNil(jobj);
                    end;
                  end;
                end;
              end; //end of wrSignaled
              else begin
                AResponse.Code:=408;
                AResponse.SendContent;
              end;
            end; //case end
          finally
            if Assigned(reqData) then
              FreeAndNil(reqData);
            if IncDec then  begin
              aux := InterlockedDecrement(QueuedClients);
              if aux<=0 then begin
                if aux<0 then
                  InterlockedExchange(QueuedClients, 0);
                UserChanged.ResetEvent;
              end;
            end;
          end;
          exit;
        end;
      end;

    finally
      if not Assigned(AResponse.ContentStream) then
        AddToLog(etInfo, cliIPAddr, 'Response: '+AResponse.Code.ToString+' '+AResponse.Content);

      if AResponse.Code=0 then
        AResponse.Code:=404;
      if not AResponse.ContentSent then
        AResponse.SendContent;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TSecWSThread.ChecarUsuarioNaAplicacaoComWinCC;
var
  aux:Integer;
begin
  Sleep(100);
  if Assigned(Form1) and Assigned(Form1.WinCCUserManagement1) then begin
    FLoginOk:=Form1.WinCCUserManagement1.Login(FUser,FPass,aux);
  end else
    FLoginOk:=false;
end;

procedure TSecWSThread.SyncLog;
var
  QueueList, intList: TList;
  i: Integer;
  logEntry: PLogEntry;
begin
  intList:=TList.Create;
  try
    QueueList:=msgList.LockList;
    try
      intList.Assign(QueueList);
      QueueList.Clear;
      //RemoveQueuedEvents(Self);
    finally
      msgList.UnlockList;
    end;

    if Assigned(Form1) and Assigned(Form1.Memo1) and Assigned(form1.EventLog1) then begin
      for i:=0 to intList.Count-1 do begin
        logEntry:=PLogEntry(intList.Items[i]);
        if not Assigned(logEntry) then continue;
        try
          logEntry^.EvtMsg:=PadLeft(logEntry^.From,15)+' '+logEntry^.EvtMsg;
          logEntry^.EvtMsg:=logEntry^.EvtMsg.Replace(#10,' ', [rfIgnoreCase,rfReplaceAll]);
          logEntry^.EvtMsg:=logEntry^.EvtMsg.Replace(#13,' ', [rfIgnoreCase,rfReplaceAll]);

          form1.EventLog1.Log(logEntry^.EvtType, logEntry^.EvtMsg);
          {$IFNDEF RELEASENOMEMOLOG}
          Form1.Memo1.Lines.Insert(0,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+' '+PadLeft(Form1.EventLog1.EventTypeToString(logEntry^.EvtType).ToUpper,15)+' '+logEntry^.EvtMsg);
          while Form1.Memo1.Lines.Count>5 do
            Form1.Memo1.Lines.Delete(5);
          {$ENDIF}

        finally
          if Assigned(logEntry) then
            Dispose(logEntry);
        end;
      end;
    end;
  finally
    FreeAndNil(intList);
  end;
end;

function TSecWSThread.SetUserChanged:String;
begin
  if Assigned(UserChanged) then begin
    UserChanged.SetEvent;
  end;
end;

procedure TSecWSThread.Terminate2;
begin
  if Assigned(UserChanged) then
    while ws.ConnectionCount>0 do
      UserChanged.SetEvent;

  Terminate;

  if Assigned(UserChanged) then
    while ws.ConnectionCount>0 do
      UserChanged.SetEvent;
end;

procedure TSecWSThread.GetLoggedWinCCUser;
begin
  if Assigned(Form1) and Assigned(Form1.WinCCUserManagement1) then
    FCurrUserLogin:=Form1.WinCCUserManagement1.CurrentUserLogin;
end;

function TSecWSThread.IPToString(aIP: in_addr): String;
begin
  //Result:=PadLeft(aIP.s_bytes[1].ToString,3)+'.'+
  //        PadLeft(aIP.s_bytes[2].ToString,3)+'.'+
  //        PadLeft(aIP.s_bytes[3].ToString,3)+'.'+
  //        PadLeft(aIP.s_bytes[4].ToString,3);

  Result:=PadLeft(aIP.s_bytes[1].ToString+'.'+
                  aIP.s_bytes[2].ToString+'.'+
                  aIP.s_bytes[3].ToString+'.'+
                  aIP.s_bytes[4].ToString,15);
end;

procedure TSecWSThread.AddToLog(EvtType: TEventType; aFrom, aMsg: String);
var
  logEntry:PLogEntry;
  qlist: TList;
begin
  //try
  //  if Assigned(log) and log.Active then begin
  //    log.Log(EvtType, aMsg);
  //  end;
  //except
  //end;

  qlist:=msgList.LockList;
  try
    new(logEntry);
    logEntry^.EvtMsg:=aMsg;
    logEntry^.EvtType:=EvtType;
    logEntry^.From:=aFrom;

    qlist.Add(logEntry);
    Queue(@SyncLog);
  finally
    msgList.UnlockList;
  end;
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

procedure TSecWSThread.GetAuthorizedReplacedSC(var aOriginalAuths: TJSONObject);
var
  inif: TIniFile;
  auxS, RealAuth, SecCode: String;
  sl: TStringList;
  i: Integer;
  jBool:TJSONBoolean;
begin
  if not Assigned(aOriginalAuths) then exit;
  cs.Enter;
  try
    inif:=TIniFile.Create(ExtractFilePath(ParamStr(0))+IniFileName);
    try
      sl:=TStringList.Create;
      try
        inif.ReadSectionValues(SectionName,sl);

        for i:=0 to sl.Count-1 do begin
          SecCode :=sl.Names[i];
          RealAuth:=sl.ValueFromIndex[i];
          if (not RealAuth.IsEmpty) and aOriginalAuths.Find(RealAuth, jBool) then
            aOriginalAuths.AddOrSet(SecCode,true);
        end;
      finally
        FreeAndNil(sl);
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
  CoInitialize(nil);
  try
    while not Terminated do begin
      ws:=THttpServer.Create(nil);
      try
        ws.AdminName:='GP2e';
        ws.AdminMail:='coordenacao@gp2e.com.br';
        ws.AcceptIdleTimeout :=1000;
        ws.OnAcceptIdle      :=@AcceptIdle;
        ws.OnRequest         :=@WSRequest;
        ws.OnRequestError    :=@RequestError;
        ws.QueueSize         :=100;
        ws.LookupHostNames   :=false;
        ws.Threaded          :=true;
        {$IF FPC_FULLVERSION > 30300}
        ws.Port              :=8443;
        ws.UseSSL            :=true;
        {$ELSE}
        ws.Port              :=7561;
        ws.UseSSL            :=false;
        {$ENDIF}
        ws.Active            :=true;
        while ws.ConnectionCount>0 do
          Sleep(1000);
      finally
        if Assigned(ws) then begin
          FreeAndNil(ws);
        end;
      end;
    end;

    if Assigned(cs)  then FreeAndNil(cs);
    //if Assigned(log) then FreeAndNil(log);
    if Assigned(msgList) then FreeAndNil(msgList);
  finally
    CoUninitialize;
  end;
end;

constructor TSecWSThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  cs:=TCriticalSection.Create;

  msgList:=TThreadList.Create;
  msgList.Duplicates:=dupAccept;

  UserChanged:=TCrossEvent.Create(true, false);
end;

destructor TSecWSThread.Destroy;
begin
  inherited Destroy;
  if Assigned(UserChanged) then FreeAndNil(UserChanged);
  if Assigned(cs) then FreeAndNil(cs);
  if Assigned(msgList) then FreeAndNil(msgList);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    EventLog1.Identification:='Prime-Wincc Security server';
    EventLog1.RaiseExceptionOnError:=false;
    EventLog1.AppendContent:=true;
    EventLog1.LogType:=ltFile;
    EventLog1.FileName:=ExtractFilePath(Application.ExeName)+'security.log';
    EventLog1.Active:=true;
  except
    if Assigned(EventLog1) then
      FreeAndNil(EventLog1);
  end;

  IniciaWSThread;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  wst.Terminate2;
  while assigned(wst) do begin
    CheckSynchronize(10);
    Application.ProcessMessages;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if FLastUserloggedIn<>UserName.Value then begin
    wst.SetUserChanged;
    FLastUserloggedIn:=UserName.Value;
  end;
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

