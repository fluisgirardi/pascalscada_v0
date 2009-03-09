//: Implementação da base de todos os tags.
unit Tag;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs;

type
  //: Estrutura de procedimentos internos dos tags.
  TTagProcedures = record
    ChangeCallBack:TNotifyEvent;
    RemoveTag:TNotifyEvent;
  end;

  //: Classe base para todos os tags.
  TTag = class(TComponent)
  protected
    //: Booleano que armazena se o tag vai ser lido automaticamente.
    PAutoRead:Boolean;
    //: Booleano que armazena se o tag vai ter seu valor escrito automaticamente.
    PAutoWrite:Boolean;
    //: Conta os erros de leitura.
    PCommReadErrors:Cardinal;
    //: Conta as leituras com exito.
    PCommReadOK:Cardinal;
    //: Conta os erros de escritas do tag.
    PCommWriteErrors:Cardinal;
    //: Conta as escritas com sucesso do tag.
    PCommWriteOk:Cardinal;
    //: Armazena o Hack do equipamento da memória que está sendo mapeada.
    PHack:Cardinal;
    //: Armazena o Slot do equipamento da memória que está sendo mapeada.
    PSlot:Cardinal;
    //: Armazena o endereço da estação da memória que está sendo mapeada.
    PStation:Cardinal;
    //: Armazena o Arquivo/DB dentro do equipamento da memória que está sendo mapeada.
    PFile_DB:Cardinal;
    //: Armazena o endereço da memória no equipamento que está sendo mapeada.
    PAddress:Cardinal;
    //: Armazena o subendereço da memória no equipamento que está sendo mapeada.
    PSubElement:Cardinal;
    //: Armazena o número de memórias que estão mapeadas.
    PSize:Cardinal;
    //: Armazena o endereço completo da memória em formato texto.
    PPath:String;
    //: Armazena a função usada para leitura da memória.
    PReadFunction:Cardinal;
    //: Armazena a função usada para escrita da memória
    PWriteFunction:Cardinal;
    //: Armazena o número de tentivas de leitura/escrita da memória.
    PRetries:Cardinal;
    //: Armazena o tempo de varredura a memória.
    PScanTime:Cardinal;
    //: Armazena o evento chamado pelo quando uma leitura do tag tem sucesso.
    POnReadOk:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma leitura do tag falha.
    POnReadFail:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma escrita tem sucesso.
    POnWriteOk:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma escrita falha.
    POnWriteFail:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando o seu valor se altera.
    POnValueChange:TNotifyEvent;
    //: Armazena os procedimentos que o tag deve chamar quando o seu valor altera.
    PChangeCallBacks:array of TTagProcedures;
    //: Conta os callbacks que dependem desse tag.
    PChangeCallBackCount:integer;
    //: Armazena a zona critica dos callbacks.
    //PChangeCallBackCS:TCriticalSection;

    //: Armazena o identificador desse tag. GUID
    PGUID:String;

    //: Chama o evento quando uma letura tem exito.
    procedure NotifyReadOk;
    //: Chama o evento quando uma leitura falha.
    procedure NotifyReadFault;
    //: Chama o evento quando uma escrita tem sucesso.
    procedure NotifyWriteOk;
    //: Chama o evento quando uma escrita do tag falha.
    procedure NotifyWriteFault;
    //: Chama o evento quando o valor do tag muda.
    procedure NotifyChange;

    //: Incrementa o contador de leituras com sucesso.
    procedure IncCommReadOK(value:Cardinal);
    //: Incrementa o contador de leituras com falha do tag.
    procedure IncCommReadFaults(value:Cardinal);
    //: Incrementa o contador de escritas com exito do tag.
    procedure IncCommWriteOK(value:Cardinal);
    //: Incrementa o contador de falhas de escrita do tag.
    procedure IncCommWriteFaults(value:Cardinal);

    //: Caso @true, o tag será lido automaticamente.
    property AutoRead:Boolean read PAutoRead;
    {:
    Caso @true, toda a vez que ocorrerem escritas no tag,
    ele irá escrever o valor no equipamento.
    }
    property AutoWrite:Boolean read PAutoWrite;
    //: Informa o total de erros de leitura do tag.
    property CommReadErrors:Cardinal read PCommReadErrors;
    //: Informa o total de leituras com exito do tag.
    property CommReadsOK:Cardinal read PCommReadOK;
    //: Informa o total de erros de escrita do tag.
    property CommWriteErrors:Cardinal read PCommWriteErrors;
    //: Informa o total de escritas com sucesso do tag.
    property CommWritesOk:Cardinal read PCommWriteOk;
    //: Hack do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property PLCHack:Cardinal read PHack;
    //: Slot do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property PLCSlot:Cardinal read PSlot;
    //: Endereço da estação que contem a memória que está sendo mapeada, se aplicável.
    property PLCStation:Cardinal read PStation;
    //: Arquivo/DB dentro do equipamento que contem a memórias que está sendo mapeada, se aplicável.
    property MemFile_DB:Cardinal read PFile_DB;
    //: Endereço da memória que está sendo mapeada.
    property MemAddress:Cardinal read PAddress;
    //: Subendereço da memória que está sendo mapeada, se aplicável.
    property MemSubElement:Cardinal read PSubElement;
    //: Função do driver responsável por realizar a leitura desse memória.
    property MemReadFunction:Cardinal read PReadFunction;
    //: Função do driver responsável por realizar a escrita de valores dessa memória.
    property MemWriteFunction:Cardinal read PWriteFunction;
    //: Número tentivas de leitura/escrita dessa memória.
    property Retries:Cardinal read PRetries;
    //: Tempo de varredura (atualização) dessa memória em milisegundos.
    property RefreshTime:Cardinal read PScanTime;
    //: Número de memórias que serão mapeadas, se aplicável.
    property Size:Cardinal read PSize;
    //: Endereço longo (texto), se aplicável ao driver.
    property LongAddress:String read PPath;

    //: Evento chamado quando uma leitura do tag tem exito.
    property OnReadOK:TNotifyEvent      read POnReadOk       write POnReadOk;
    //: Evento chamado quando uma leitura do tag falha.
    property OnReadFail:TNotifyEvent    read POnReadFail     write POnReadFail;
    //: Evento chamado quando uma escrita de valor do tag tem exito.
    property OnWriteOk:TNotifyEvent     read POnWriteOk      write POnWriteOk;
    //: Evento chamado quando uma escrita do tag falha.
    property OnWriteFail:TNotifyEvent   read POnWriteFail    write POnWriteFail;
    //: Evento chamado quando o valor do tag sofre alguma mudança.
    property OnValueChange:TNotifyEvent read POnValueChange  write POnValueChange;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    //: Adiciona um conjunto de notificação de alteracão para o tag.
    procedure AddChangeCallBack(cback,remTag:TNotifyEvent);
    //: Remove um conjunto de notificação de mundanças do tag.
    procedure RemoveChangeCallBack(cback:TNotifyEvent);
  end;


implementation


constructor TTag.Create(AOwner:TComponent);
var
  x:TGuid;
begin
  inherited Create(AOwner);
  PChangeCallBackCount := 0;
  PCommReadErrors := 0;
  PCommReadOK := 0;
  PCommWriteErrors := 0;
  PCommWriteOk := 0;

  if ComponentState*[csReading, csLoading]=[] then begin
    CreateGUID(x);
    PGUID:=GUIDToString(x);
  end;

end;

destructor TTag.Destroy;
var
  c:Integer;
begin
  for c := 0 to High(PChangeCallBacks) do
    if Assigned(PChangeCallBacks[c].RemoveTag) then
      PChangeCallBacks[c].RemoveTag(Self);
  inherited Destroy;
end;

procedure TTag.AddChangeCallBack(cback, remTag:TNotifyEvent);
//var
//  c:integer;
begin
  if (Not Assigned(cback)) or (not Assigned(remTag)) then
    raise Exception.Create('Nenhum dos notificadores pode ser nulo!');
  
  inc(PChangeCallBackCount);
  SetLength(PChangeCallBacks, PChangeCallBackCount);
  PChangeCallBacks[PChangeCallBackCount-1].ChangeCallBack := cback;
  PChangeCallBacks[PChangeCallBackCount-1].RemoveTag := remTag;
end;

procedure TTag.RemoveChangeCallBack(cback:TNotifyEvent);
var
  c,h:Integer;
  found:Boolean;
begin
  found:=false;
  h := High(PChangeCallbacks);
  for c:=0 to h do
    if (@cback)=(@PChangeCallBacks[c]) then begin
      found := true;
      break;
    end;
  if found then begin
    PChangeCallBacks[c] := PChangeCallbacks[h];
    PChangeCallBackCount := PChangeCallBackCount - 1;
    SetLength(PChangeCallBacks, PChangeCallBackCount);
  end;
end;

procedure TTag.NotifyChange;
var
  c:Integer;
begin
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].ChangeCallBack(self);
    except
    end;
  if Assigned(POnValueChange) then
    POnValueChange(Self);

end;

procedure TTag.NotifyReadOk;
begin
  if Assigned(POnReadOk) then
    POnReadOk(self)
end;

procedure TTag.NotifyReadFault;
begin
  if Assigned(POnReadFail) then
    POnReadFail(self)
end;

procedure TTag.NotifyWriteOk;
begin
  if Assigned(POnWriteOk) then
    POnWriteOk(self)
end;

procedure TTag.NotifyWriteFault;
begin
  if Assigned(POnWriteFail) then
    POnWriteFail(self)
end;

procedure TTag.IncCommReadOK(value:Cardinal);
begin
  inc(PCommReadOK,value);
  if value>0 then
    NotifyReadOk;
end;

procedure TTag.IncCommReadFaults(value:Cardinal);
begin
  inc(PCommReadErrors,value);
  if value>0 then
    NotifyReadFault;
end;

procedure TTag.IncCommWriteOK(value:Cardinal);
begin
  inc(PCommWriteOk,value);
  if value>0 then
    NotifyWriteOk;
end;

procedure TTag.IncCommWriteFaults(value:Cardinal);
begin
  inc(PCommWriteErrors,value);
  if value>0 then
    NotifyWriteFault;
end;

end.
