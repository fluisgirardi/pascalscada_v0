{:
@abstract(Implementação da base de um driver de protocolo.)
@author(Fabio Luis Girardi papelhigienico@gmail.com)
}
unit ProtocolDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommPort, CommTypes, ProtocolTypes, protscanupdate,
  protscan, CrossEvent, Tag, syncobjs {$IFNDEF FPC}, Windows{$ENDIF};

type
  {:
  Classe base para drivers de protocolo.

  @author(Fabio Luis Girardi papelhigienico@gmail.com)

  Para você criar um novo driver, basta sobrescrever alguns métodos e funções,
  de acordo com as necessidades de seu driver de protocolo. São eles:
  
  @code(procedure DoAddTag(TagObj:TTag);)
  Procedimento por adicionar tags ao scan do driver. Não esqueça de chamar os
  método herdado com @code(inherited DoAddTag(TagObj:TTag)).
  
  @code(procedure DoDelTag(TagObj:TTag);)
  Procedimento por remover tags do scan do driver. Não esqueça de chamar os
  método herdado com @code(inherited DoDelTag(TagObj:TTag)).
  
  @code(procedure DoScanRead(Sender:TObject);)
  Prodimento chamado para verificar se há algum tag necessitando ser lido.
  
  @code(procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec);)
  Procedimento chamado pelo driver para retornar os valores lidos que estão
  em algum gerenciador de memória para os tags.
  
  @code(procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);)
  Procedimento usado para atualizar as informações dos tags dependentes do driver.
  Caso alguma alteração torne o tag inconsistente, gere um Exception para abortar
  a mudança.
  
  @code(function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;)
  Função que codifica pedidos de leitura escrita dos tags em pacotes de dados
  do protocolo para serem escritos em um driver de porta.
  
  @code(function  DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult;)
  Função que decodifica um pacote de dados retornado pelo driver de porta e retira
  os valores presentes nesse pacote.
  
  @code(function  SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE; )
  Função responsável por informar o tamanho das palavras de dados em bits
  que o tag está referenciando.
  
  
  Sobrescrevendo esses métodos e rotinas, seu driver estará pronto.
  }

  TProtocolDriver = class(TComponent)
  private
    //Array de tags associados ao driver.
    PTags:array of TTag;

    //thread de execução do scan dos tags
    PScanReadThread:TScanThread;
    //Thread de execução de escritas
    PScanWriteThread:TScanThread;
    //thread de atualização dos pedidos dos tags
    PScanUpdateThread:TScanUpdate;

    //excessao caso o index to tag esteja fora dos limites
    procedure DoExceptionIndexOut(index:integer);

    //metodos para manipulação da lista de tags
    function  GetTagCount:Integer;
    function  GetTag(index:integer):TTag;
    function  GetTagName(index:integer):String;
    function  GetTagByName(Nome:String):TTag;

    //metodos chamados pelas threads
    procedure SafeScanRead(Sender:TObject; var NeedSleep:Integer);
    function  SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
    procedure SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
  protected
    //: Flag que informa ao driver se ao menos uma variavel deve ser lida a cada ciclo de scan do driver.
    PReadSomethingAlways:Boolean;
    //: Armazena a ID (número único) do driver.
    PDriverID:Cardinal;
    //: Armazena o driver de porta associado a esse driver de protocolo.
    PCommPort:TCommPortDriver;
    //: Armazena o ID (número único) desses pedidos.
    FScanReadID, FScanWriteID, FReadID, FWriteID:Cardinal;
    //: Armazena a evento usado para parar as threads do driver de protocolo.
    FCritical:TCriticalSection;
    //: Forca a suspensão das threads.
    FPause:TCrossEvent;
    //: Armazena a seção crítica que protege areas comuns a muitas threads.
    PCallersCS:TCriticalSection;

    //: Secao critica de acoes pendentes.
    FPendingActionsCS:TCriticalSection;

    //: Array de acoes pendentes.
    FPendingActions:TArrayOfObject;

    {:
    Cancela ações pendentes do driver que possam demorar.
    Chamado quando o driver está sendo destruido.
    }
    procedure CancelPendingActions; virtual;

    {:
    Adiciona uma ação a lista de espera do driver.
    }
    procedure AddPendingAction(const Obj:TObject); virtual;

    {:
    Adiciona uma ação a lista de espera do driver.
    }
    function RemovePendingAction(const Obj:TObject):boolean; virtual;

    //: Configura a porta de comunicação que será usada pelo driver.
    procedure SetCommPort(CommPort:TCommPortDriver);
    {:
    Copia uma estrutura TIOPacket para outra.
    @param(Source TIOPacket. Estrutura de origem dos dados.)
    @param(Dest TIOPacket. Estrutura para onde os dados serão copiados.)
    }
    procedure CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
    {:
    Callback @bold(assincrono) que o driver de porta (TCommPortDriver) irá chamar para
    retornar os resultados de I/O.
    }
    procedure CommPortCallBack(var Result:TIOPacket); virtual;

    {:
    Método chamado pelo driver de protocolo para adicionar um tag ao scan driver.
    @param(TagObj TTag. Tag a adicionar como dependente do driver.)
    @seealso(AddTag)
    }
    procedure DoAddTag(TagObj:TTag); virtual;
    {:
    Método chamado pelo driver de protocolo para remover um tag do scan do driver.
    @param(TagObj TTag. Tag dependente para remover do driver.)
    @seealso(RemoveTag)
    }
    procedure DoDelTag(TagObj:TTag); virtual;
    {:
    Método chamado pelo driver de protocolo para atualizar as informações a
    respeito de um tag quando este tem alguma propriedade alterada.

    @param(TagObj TTag. Tag quem sofreu a mudança.)
    @param(Change TChangeType. Que propriedade sofreu a alteração.)
    @param(oldValue Cardinal. Valor antigo da propriedade.)
    @param(newValue Cardinal. Novo valor da propriedade.)
    @seealso(TagChanges)
    }
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); virtual; abstract; //realiza uma mundanca em alguma propriedade do tag;


    {:
    Método chamado pelas threads do driver de protocolo para realizar leitura dos
    tags a ele associado.
    }
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); virtual; abstract;
    {:
    Método chamado pelas threads do driver de protocolo para atualizar os valores
    dos tags.
    @param(TagRec TTagRec. Estrutura com informações do tag.)
    @param(values TArrayOfDouble. Armazena os valores que serão enviados ao tag.)
    }
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); virtual; abstract;
    {: todo }

    //funcoes que realmente executam a leitura/escrita de valores dos tags.
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;
    function DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;

    //: Booleano que diz se o driver deve ler algum tag a todo scan.
    property ReadSomethingAlways:Boolean read PReadSomethingAlways write PReadSomethingAlways default true;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    {:
    Adiciona um tag ao scan do driver.
    @param(Tag TTag. Tag a adicionar no scan do driver.)
    @raises(Exception caso alguma configuração esteja errada.)
    }
    procedure AddTag(TagObj:TTag);
    {:
    Remove um tag do scan do driver.
    @param(Tag TTag. Tag a remover do scan do driver.)
    }
    procedure RemoveTag(TagObj:TTag);
    {:
    Atualiza as informações do driver a respeito do tag. Chamado quando alguma
    propriedade de um tag sofre alguma mudança.
    @param(TagObj TTag. Tag quem sofreu a mudança.)
    @param(Change TChangeType. Que propriedade sofreu a alteração.)
    @param(oldValue Cardinal. Valor antigo da propriedade.)
    @param(newValue Cardinal. Novo valor da propriedade.)
    @seealso(TProtocolDriver.TagChanges)
    }
    procedure TagChanges(TagObj:TTag; Change:TChangeType; oldValue, newValue:Cardinal);
    {:
    Função que informa se o Tag está associado ao driver.
    @param(TagObj TTag. Tag que deseja saber se está associado ao driver.)
    @returns(@true caso o tag esteja associado ao driver. @false caso contrário.)
    }
    function  IsMyTag(TagObj:TTag):Boolean;
    {:
    Função que retorna o tamanho em bits do registrador mapeado pelo tag.
    @param(Tag TTag. Tag que se deseja saber o tamanho do registrador.)
    @param(isWrite Boolean. Caso @true, informa o tamanho em bits usando as
           funções de escrita.)
    @returns(Tamanho em bits do registrador associado ou 0 (zero) caso falhe.
    }
    function  SizeOfTag(Tag:TTag; isWrite:Boolean):BYTE; virtual; abstract; 

    {:
    Solicita a leitura por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    @returns(Cardinal. Número único do pedido de leitura por scan.)
    }
    function  ScanRead(const tagrec:TTagRec):Cardinal;
    {:
    Solicita a escrita por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    @returns(Cardinal. Número único do pedido de escrita por scan.)
    }
    function  ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;
    {:
    Solicita a leitura (sincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    }
    procedure Read(const tagrec:TTagRec);
    {:
    Solicita uma escrita (sincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    }
    procedure Write(const tagrec:TTagRec; const Values:TArrayOfDouble);

    //: Conta os tags dependentes desse driver de protocolo.
    property TagCount:Integer read GetTagCount;
    //: Lista cada tag dependente desse driver.
    property Tag[index:integer]:TTag read GetTag;
    //: Lista o nome de cada tag dependente desse driver.
    property TagName[index:integer]:String read GetTagName;
    //: Lista cada tag dependente desse driver usando o nome do tag como indice.
    property TagByName[Nome:String]:TTag read GetTagByName;

  published
    {:
    Driver de porta que será usado para realizar as operações de comunicação
    do protoloco.
    @seealso(TCommPortDriver)
    }
    property CommunicationPort:TCommPortDriver read PCommPort write SetCommPort nodefault;
    //: Identificação (número único) do driver.
    property DriverID:Cardinal read PDriverID;
  end;

var
   {:
   Contador de drivers criados, usado para gerar nomes únicos dos eventos
   seções criticas e semaforos em ambiente Windows.

   @bold(Não altere o valor dessa variável.)
   }
   DriverCount:Cardinal;

implementation

uses PLCTag;

////////////////////////////////////////////////////////////////////////////////
//             inicio das declarações do TProtocolDriver
////////////////////////////////////////////////////////////////////////////////

constructor TProtocolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDriverID := DriverCount;
  inc(DriverCount);

  FCritical := TCriticalSection.Create;

  FPendingActionsCS := TCriticalSection.Create;

  FPause := TCrossEvent.Create(nil,true,true,'');

  PCallersCS := TCriticalSection.Create;

  if ComponentState*[csDesigning]=[] then begin

    PScanUpdateThread := TScanUpdate.Create(true);
    PScanUpdateThread.Priority:=tpTimeCritical;
    PScanUpdateThread.OnGetValue := SafeGetValue;

    PScanReadThread := TScanThread.Create(true, PScanUpdateThread);
    //PScanReadThread.Priority:=tpHighest;
    PScanReadThread.OnDoScanRead := SafeScanRead;
    PScanReadThread.OnDoScanWrite := nil;

    PScanWriteThread := TScanThread.Create(true, PScanUpdateThread);
    PScanWriteThread.Priority:=tpTimeCritical;
    PScanWriteThread.OnDoScanRead := nil;
    PScanWriteThread.OnDoScanWrite := SafeScanWrite;

    PScanUpdateThread.Resume;

    PScanReadThread.Resume;
    PScanReadThread.WaitInit;

    PScanWriteThread.Resume;
    PScanWriteThread.WaitInit;
  end;
end;

destructor TProtocolDriver.Destroy;
var
  c:Integer;
begin
  CancelPendingActions;
  if ComponentState*[csDesigning]=[] then begin
    PScanReadThread.Destroy;
    PScanWriteThread.Destroy;

    PScanUpdateThread.Terminate;
    PScanUpdateThread.WaitFor;
  end;

  for c:=0 to High(PTags) do
    TPLCTag(PTags[c]).RemoveDriver;

  SetCommPort(nil);

  FCritical.Destroy;

  FPendingActionsCS.Destroy;

  FPause.Destroy;
  
  SetLength(PTags,0);
  SetLength(FPendingActions,0);
  PCallersCS.Destroy;
  inherited Destroy;
end;

procedure TProtocolDriver.CancelPendingActions;
var
  c:Integer;
begin
  FPendingActionsCS.Enter;
  try
    for c:=0 to High(FPendingActions) do begin
      if ((FPendingActions[c]<>nil) and (FPendingActions[c] is TCrossEvent)) then
        TCrossEvent(FPendingActions[c]).Destroy
      else begin
        if ((FPendingActions[c]<>nil) and (FPendingActions[c] is TObject)) then
          TObject(FPendingActions[c]).Destroy;
      end;
    end;
  finally
    FPendingActionsCS.Leave;
  end;
end;

procedure TProtocolDriver.AddPendingAction(const Obj:TObject);
var
  c,h:Integer;
  found:Boolean;
begin
  FPendingActionsCS.Enter;
  try
    found := false;
    for c:=0 to High(FPendingActions) do
      if (FPendingActions[c]=Obj) then begin
        found:=true;
        break;
      end;

    if not found then begin
      h := Length(FPendingActions);
      SetLength(FPendingActions,h+1);
      FPendingActions[h]:=Obj;
    end;
  finally
    FPendingActionsCS.Leave;
  end;
end;

function  TProtocolDriver.RemovePendingAction(const Obj:TObject):boolean;
var
  c,h:Integer;
  found:Boolean;
begin
  FPendingActionsCS.Enter;
  try
    Result:=false;
    found := false;
    for c:=0 to High(FPendingActions) do
      if (FPendingActions[c]=Obj) then begin
        found:=true;
        break;
      end;

    if found then begin
      Result := true;
      h := High(FPendingActions);
      FPendingActions[c]:=FPendingActions[h];
      SetLength(FPendingActions,h);
    end;
  finally
    FPendingActionsCS.Leave;
  end;
end;

procedure TProtocolDriver.SetCommPort(CommPort:TCommPortDriver);
begin
  try
    FPause.ResetEvent;
    FCritical.Enter;
    //se for a mesma porta cai fora...
    if CommPort=PCommPort then exit;

    if PCommPort<>nil then begin
      if PCommPort.LockedBy=PDriverID then
        PCommPort.Unlock(PDriverID);
      PCommPort.CancelCallBack(CommPortCallBack);
      PCommPort.DelProtocol(Self);
    end;

    if CommPort<>nil then begin
      CommPort.ResumeCallBack(CommPortCallBack);
      CommPort.AddProtocol(Self);
    end;
    PCommPort := CommPort;
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoAddTag(TagObj:TTag);
var
  c:integer;
begin
  for c:=0 to High(PTags) do
    if PTags[c]=TagObj then
      raise Exception.Create('Este Tag já esta registrado com este driver!');

  c:=Length(Ptags);
  SetLength(PTags,c+1);
  PTags[c] := TagObj;
end;

procedure TProtocolDriver.DoDelTag(TagObj:TTag);
var
  c:Integer;
  h:integer;
  found:boolean;
begin
  if Length(PTags)<=0 then exit;

  h:=High(PTags);
  found := false;
  for c:=0 to h do
    if PTags[c]=TagObj then begin
      found := true;
      break;
    end;
  if found then begin
    PTags[c] := PTags[h];
    SetLength(PTags,h);
  end;
end;

procedure TProtocolDriver.AddTag(TagObj:TTag);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;

  try
    FPause.ResetEvent;
    FCritical.Enter;
    DoAddTag(TagObj);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.RemoveTag(TagObj:TTag);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;
  try
    FPause.ResetEvent;
    FCritical.Enter;
    DoDelTag(TagObj);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoExceptionIndexOut(index:integer);
begin
  if (index>high(PTags)) then
    raise Exception.Create('Indice fora dos limites!');
end;

function TProtocolDriver.GetTagCount;
begin
  Result := Length(PTags);
end;

function TProtocolDriver.GetTag(index:integer):TTag;
begin
  DoExceptionIndexOut(index);
  result:=PTags[index];
end;

function TProtocolDriver.GetTagName(index:integer):String;
begin
  Result:='';
  DoExceptionIndexOut(index);
  result:=PTags[index].Name;
end;

function TProtocolDriver.GetTagByName(Nome:String):TTag;
var
  c:Integer;
begin
  Result := nil;
  for c:=0 to High(PTags) do
    if PTags[c].Name = Nome then begin
      Result := PTags[c];
      break;
    end;
end;

function TProtocolDriver.IsMyTag(TagObj:TTag):Boolean;
var
  c:integer;
begin
  Result := false;
  for c:=0 to High(PTags) do
    if TagObj=PTags[c] then begin
      Result := true;
      break;
    end;
end;

procedure TProtocolDriver.TagChanges(TagObj:TTag; Change:TChangeType; oldValue, newValue:Cardinal);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;
  try
    FPause.ResetEvent;
    FCritical.Enter;
    DoTagChange(TagObj,Change,oldValue,newValue);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

function TProtocolDriver.ScanRead(const tagrec:TTagRec):Cardinal;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDesigning in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de scanReads
    //zera o contador para evitar overflow;
    if FScanReadID=$FFFFFFFF then
       FScanReadID := 0
    else
       inc(FScanReadID);

    //posta uma mensagem de Leitura por Scan
    if (ComponentState*[csDesigning]=[]) and (PScanUpdateThread<>nil) then
      PScanUpdateThread.ScanRead(tagrec);

    Result := FScanReadID;

  finally
    PCallersCS.Leave;
  end;
end;

function TProtocolDriver.ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;
var
   pkg:PScanWriteRec;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDesigning in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de ScanWrites
    //zera o contador para evitar overflow;
    if FScanWriteID=4294967295 then
       FScanWriteID := 0
    else
       inc(FScanWriteID);
       
    //cria um pacote de escrita por scan
    New(pkg);
    pkg^.SWID:=FScanReadID;
    //copia o TagRec
    Move(tagrec, pkg^.Tag, sizeof(TTagRec));
    //copia os valores
    pkg^.ValuesToWrite := Values;
    
    pkg^.WriteResult:=ioNone;

    //posta uma mensagem de Escrita por Scan
    if (ComponentState*[csDesigning]=[]) and (PScanWriteThread<>nil) then begin
      PScanWriteThread.ScanWrite(pkg);
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      SwitchToThread;
      {$ENDIF}
    end;

    Result := FScanWriteID;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.Read(const tagrec:TTagRec);
var
  res:TProtocolIOResult;
  Values:TArrayOfDouble;
begin
  try
    FPause.ResetEvent;
    FCritical.Enter;
    res := DoRead(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,Now,tcRead,res,tagrec.OffSet);
  finally
    FCritical.Leave;
    FPause.SetEvent;
    SetLength(Values,0);
  end;
end;

procedure TProtocolDriver.Write(const tagrec:TTagRec; const Values:TArrayOfDouble);
var
  res:TProtocolIOResult;
begin
  try
    FPause.ResetEvent;
    FCritical.Enter;
    res := DoWrite(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,Now,tcWrite,res,tagrec.OffSet);
  finally
    FCritical.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.CommPortCallBack(var Result:TIOPacket);
begin
  if Result.Res2<>nil then
     CopyIOPacket(Result,PIOPacket(Result.Res2)^);
  if Result.res1 is TCrossEvent then
     TCrossEvent(Result.res1).SetEvent;
end;

procedure TProtocolDriver.CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
begin
  Dest.PacketID := Source.PacketID;
  Dest.WriteIOResult := Source.WriteIOResult;
  Dest.ToWrite := Source.ToWrite;
  Dest.Wrote := Source.Wrote;
  Dest.WriteRetries := Source.WriteRetries;
  Dest.DelayBetweenCommand := Source.DelayBetweenCommand;
  Dest.ReadIOResult := Source.ReadIOResult;
  Dest.ToRead := Source.ToRead;
  Dest.Received := Source.Received;
  Dest.ReadRetries := Source.ReadRetries;
  SetLength(Dest.BufferToRead, 0);
  SetLength(Dest.BufferToWrite, 0);
  Dest.BufferToRead := Source.BufferToRead;
  Dest.BufferToWrite:= Source.BufferToWrite;
  Dest.Res1 := Source.Res1;
  Dest.Res2 := Source.Res2;
end;

procedure TProtocolDriver.SafeScanRead(Sender:TObject; var NeedSleep:Integer);
begin
   try
      FPause.WaitFor($FFFFFFFF);
      FCritical.Enter;
      DoScanRead(Sender, NeedSleep);
   finally
      FCritical.Leave;
   end;
end;

function  TProtocolDriver.SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
begin
   try
      FPause.ResetEvent;
      FCritical.Enter;
      Result := DoWrite(TagRec,values,false)
   finally
      FCritical.Leave;
      FPause.SetEvent;
   end;
end;

procedure TProtocolDriver.SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
begin
   try
      FPause.ResetEvent;
      FCritical.Enter;
      DoGetValue(TagRec,values);
   finally
      FCritical.Leave;
      FPause.SetEvent;
   end;
end;

end.
