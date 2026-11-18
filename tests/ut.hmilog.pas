{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do log de seguranca, das escritas feitas pela HMI e do banco de dados.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Seguranca: login, falha de login, logout e acesso concedido - e nunca a
  senha. HMI: o controle que escreveu, o tag, o valor e o usuario logado.
  Banco: o comando recusado em ReadOnly e o resultado de uma transacao.
}
{$ELSE}
{:
  @abstract(Logging tests of security, HMI writes and the database.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Security: login, failed login, logout and access granted - and never the
  password. HMI: the control that wrote, the tag, the value and the logged
  user. Database: the command refused in ReadOnly and the result of a
  transaction.
}
{$ENDIF}
unit ut.hmilog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CustomizedUserManagement, HMICheckBox, HMIDBConnection, pscadalog,
  testsupport.faketag, testsupport.logcollector, ut.hmidbconnection;

type

  { TTestSecurityLog }

  TTestSecurityLog = class(TTestCase)
  private
    FUsers:TCustomizedUserManagement;
    FLog:TLogCollector;
    procedure CheckUser(user, pass:UTF8String; out aUID:Integer; var ValidUser:Boolean; LoginAction:Boolean);
    procedure AllowEverything(securityCode:UTF8String; var CanAccess:Boolean);
    procedure CheckNoPassword;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ALoginIsPostedWithTheUser;
    procedure AFailedLoginIsAWarning;
    procedure ALogoutIsPosted;
    procedure AccessGrantedToTheLoggedUserIsPosted;
  end;

  { TTestHMIWriteLog }

  TTestHMIWriteLog = class(TTestCase)
  private
    FBox:THMICheckBox;
    FTag:TFakeNumber;
    FUsers:TCustomizedUserManagement;
    FLog:TLogCollector;
    procedure CheckUser(user, pass:UTF8String; out aUID:Integer; var ValidUser:Boolean; LoginAction:Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AControlWritePostsTheTagTheValueAndTheUser;
    procedure WithAUserLoggedInTheWriteNamesIt;
    procedure ARefusedWriteIsAWarning;
  end;

  { TTestDatabaseLog }

  TTestDatabaseLog = class(TTestCase)
  private
    FLog:TLogCollector;
    function WaitForEntry(const Prefix:String; Ms:Integer = 3000):Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ACommandRefusedInReadOnlyIsAWarning;
    procedure ACommittedTransactionIsPosted;
    procedure AFailedCommitIsAnError;
  end;

implementation

type
  //the limits are protected in TPLCNumber (each tag class publishes them)
  TFakeNumberAccess = class(TFakeNumber);

{ TTestSecurityLog }

procedure TTestSecurityLog.SetUp;
begin
  FLog := TLogCollector.Create([lcSecurity], llDebug);
  FUsers := TCustomizedUserManagement.Create(nil);
  FUsers.Name := 'Users';
  FUsers.OnCheckUserAndPass := @CheckUser;
  FUsers.OnCanAccess := @AllowEverything;
end;

procedure TTestSecurityLog.TearDown;
begin
  FreeAndNil(FUsers);
  FreeAndNil(FLog);
end;

procedure TTestSecurityLog.CheckUser(user, pass:UTF8String; out aUID:Integer;
  var ValidUser:Boolean; LoginAction:Boolean);
begin
  ValidUser := (user='fabio') and (pass='segredo');
  if ValidUser then aUID := 7 else aUID := -1;
end;

procedure TTestSecurityLog.AllowEverything(securityCode:UTF8String; var CanAccess:Boolean);
begin
  CanAccess := true;
end;

procedure TTestSecurityLog.CheckNoPassword;
var
  i:Integer;
begin
  for i:=0 to FLog.Count-1 do begin
    AssertEquals('no password in: '+FLog[i].Msg, 0, Pos('segredo', FLog[i].Msg));
    AssertEquals('no attempted password in: '+FLog[i].Msg, 0, Pos('chute', FLog[i].Msg));
  end;
end;

procedure TTestSecurityLog.ALoginIsPostedWithTheUser;
var
  uid:Integer;
  i:Integer;
begin
  AssertTrue('login', FUsers.Login('fabio', 'segredo', uid));
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcSecurity, 'login: ');
  AssertTrue('posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'login: fabio (password)', FLog[i].Msg);
  AssertEquals('info', PSLogLevelName(llInfo), PSLogLevelName(FLog[i].Level));
  AssertEquals('source', 'Users', FLog[i].Source);
  CheckNoPassword;
end;

procedure TTestSecurityLog.AFailedLoginIsAWarning;
var
  uid:Integer;
  i:Integer;
begin
  AssertFalse('refused', FUsers.Login('fabio', 'chute', uid));
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcSecurity, 'login failed');
  AssertTrue('posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'login failed: fabio (password)', FLog[i].Msg);
  AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[i].Level));
  CheckNoPassword;
end;

procedure TTestSecurityLog.ALogoutIsPosted;
var
  uid:Integer;
begin
  FUsers.Login('fabio', 'segredo', uid);
  FUsers.Logout;
  FUsers.Logout; //nobody logged in: nothing more
  AssertTrue('flush', FLog.Flush);
  AssertTrue('posted'+FLog.Dump, FLog.IndexOf(lcSecurity, 'logout: fabio')>=0);
  AssertEquals('only once'+FLog.Dump, -1, FLog.IndexOf(lcSecurity, 'logout', FLog.IndexOf(lcSecurity, 'logout')+1));
end;

procedure TTestSecurityLog.AccessGrantedToTheLoggedUserIsPosted;
var
  uid:Integer;
  who:UTF8String;
  i:Integer;
begin
  FUsers.Login('fabio', 'segredo', uid);
  who := '';
  AssertTrue('allowed', FUsers.CheckIfUserIsAllowed('valves', false, who, ''));
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcSecurity, 'access to');
  AssertTrue('posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'access to "valves" granted to fabio (logged in)', FLog[i].Msg);
  AssertEquals('debug', PSLogLevelName(llDebug), PSLogLevelName(FLog[i].Level));
end;

{ TTestHMIWriteLog }

procedure TTestHMIWriteLog.SetUp;
begin
  FLog := TLogCollector.Create([lcHMI], llInfo);
  FBox := THMICheckBox.Create(nil);
  FBox.Name := 'ValveBox';
  FBox.ValueTrue := 1;
  FBox.ValueFalse := 0;
  FTag := TFakeNumber.Create(nil);
  FTag.Name := 'Valve1';
  FBox.PLCTag := FTag;
end;

procedure TTestHMIWriteLog.TearDown;
begin
  FreeAndNil(FBox);
  FreeAndNil(FTag);
  FreeAndNil(FUsers);
  FreeAndNil(FLog);
end;

procedure TTestHMIWriteLog.CheckUser(user, pass:UTF8String; out aUID:Integer;
  var ValidUser:Boolean; LoginAction:Boolean);
begin
  ValidUser := true;
  aUID := 3;
end;

procedure TTestHMIWriteLog.AControlWritePostsTheTagTheValueAndTheUser;
var
  i:Integer;
begin
  FBox.Checked := true;
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcHMI, 'wrote');
  AssertTrue('posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'wrote 1 to Valve1 (no user logged in)', FLog[i].Msg);
  AssertEquals('the control is the source', 'ValveBox', FLog[i].Source);
  AssertEquals('info', PSLogLevelName(llInfo), PSLogLevelName(FLog[i].Level));
end;

procedure TTestHMIWriteLog.WithAUserLoggedInTheWriteNamesIt;
var
  uid:Integer;
begin
  FUsers := TCustomizedUserManagement.Create(nil);
  FUsers.OnCheckUserAndPass := @CheckUser;
  AssertTrue('login', FUsers.Login('operator', 'x', uid));
  FBox.Checked := true;
  AssertTrue('flush', FLog.Flush);
  AssertTrue('names the user'+FLog.Dump, FLog.IndexOf(lcHMI, 'wrote 1 to Valve1 (user operator)')>=0);
end;

procedure TTestHMIWriteLog.ARefusedWriteIsAWarning;
var
  i:Integer;
begin
  TFakeNumberAccess(FTag).EnableMaxValue := true;
  TFakeNumberAccess(FTag).MaxValue := 0.5;
  try
    FBox.Checked := true;
  except
    //the refusal goes on to whoever wrote, as before
  end;
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcHMI, 'write of 1 to Valve1 refused');
  AssertTrue('posted'+FLog.Dump, i>=0);
  AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[i].Level));
  AssertEquals('no "wrote" for it', -1, FLog.IndexOf(lcHMI, 'wrote'));
end;

{ TTestDatabaseLog }

procedure TTestDatabaseLog.SetUp;
begin
  FLog := TLogCollector.Create([lcDatabase], llDebug);
end;

procedure TTestDatabaseLog.TearDown;
begin
  FreeAndNil(FLog);
end;

function TTestDatabaseLog.WaitForEntry(const Prefix:String; Ms:Integer):Integer;
var
  start:QWord;
begin
  start := GetTickCount64;
  repeat
    CheckSynchronize(5);
    FLog.Flush;
    Result := FLog.IndexOf(lcDatabase, Prefix);
    if Result>=0 then exit;
  until GetTickCount64-start>=QWord(Ms);
end;

procedure TTestDatabaseLog.ACommandRefusedInReadOnlyIsAWarning;
var
  C:THMIDBConnection;
  i:Integer;
begin
  C := THMIDBConnection.Create(nil);
  try
    C.Name := 'HistoryDB';
    C.ReadOnly := true;
    C.ExecSQL('insert into t values (1)', nil, false);
    i := WaitForEntry('SQL refused');
    AssertTrue('posted'+FLog.Dump, i>=0);
    AssertEquals('message', 'SQL refused (ReadOnly): insert into t values (1)', FLog[i].Msg);
    AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[i].Level));
    AssertEquals('source', 'HistoryDB', FLog[i].Source);
  finally
    C.Free;
  end;
end;

procedure TTestDatabaseLog.ACommittedTransactionIsPosted;
var
  B:TFakeDBBackend;
  T:TProcessSQLCommandThread;
  S:THMIDBConnectionStatementList;
  i:Integer;
begin
  B := TFakeDBBackend.Create;
  T := TProcessSQLCommandThread.Create(true, @B.ExecSQL, @B.StartTransaction,
                                       @B.CommitTransaction, @B.RollbackTransaction);
  try
    T.WakeUp;
    S := THMIDBConnectionStatementList.Create;
    S.Add('insert into t values (1)');
    S.Add('insert into t values (2)');
    T.ExecTransaction(S, @B.ReturnResult, true, false, false);
    i := WaitForEntry('transaction committed');
    AssertTrue('posted'+FLog.Dump, i>=0);
    AssertEquals('message', 'transaction committed (2 statements)', FLog[i].Msg);
  finally
    T.Terminate;
    T.WaitEnd(3000);
    T.Free;
    B.Free;
  end;
end;

procedure TTestDatabaseLog.AFailedCommitIsAnError;
var
  B:TFakeDBBackend;
  T:TProcessSQLCommandThread;
  S:THMIDBConnectionStatementList;
  i:Integer;
begin
  B := TFakeDBBackend.Create;
  B.RaiseOnCommit := true;
  T := TProcessSQLCommandThread.Create(true, @B.ExecSQL, @B.StartTransaction,
                                       @B.CommitTransaction, @B.RollbackTransaction);
  try
    T.WakeUp;
    S := THMIDBConnectionStatementList.Create;
    S.Add('insert into t values (1)');
    T.ExecTransaction(S, @B.ReturnResult, true, false, false);
    i := WaitForEntry('commit/rollback failed');
    AssertTrue('posted'+FLog.Dump, i>=0);
    AssertEquals('error', PSLogLevelName(llError), PSLogLevelName(FLog[i].Level));
    AssertEquals('not reported as committed', -1, FLog.IndexOf(lcDatabase, 'transaction committed'));
  finally
    T.Terminate;
    T.WaitEnd(3000);
    T.Free;
    B.Free;
  end;
end;

initialization
  RegisterTest(TTestSecurityLog);
  RegisterTest(TTestHMIWriteLog);
  RegisterTest(TTestDatabaseLog);

end.
