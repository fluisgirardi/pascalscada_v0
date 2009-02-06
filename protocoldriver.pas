//: Implementação de um driver de protocolo.
unit ProtocolDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommPort, CommTypes, ProtocolTypes, Tag, MessageSpool,
  Forms, CrossEvent, SyncObjs;

type

  {:
  Classe de thread reponsável por atualizar os tags após o driver processar
  leituras/escritas por scan. Usado por TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  TScanUpdate = class(TCrossThread)
  private
    FSUTInitEventHandle:TCrossEvent;
    FDoSomethingEventHandle:TCrossEvent;
    FEnd:TCrossEvent;
    Ferro:Exception;
    TagCBack:TTagCommandCallBack;
    FTagRec:PTagRec;
    Fvalues:TScanReadRec;
    FCmd:TTagCommand;
    PGetValues:TGetValues;
    FSpool:TMessageSpool;
    procedure SyncCallBack;
    procedure SyncException;
    procedure DoSomething;
    procedure WaitToDoSomething;
    procedure CheckScanWrite;
    function  WaitEnd(timeout:Cardinal):TWaitResult;
  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean);
    //: @exclude
    destructor Destroy; override;
    //: Ao chamar @name, espera a thread sinalizar a sua inicialização.
    procedure WaitInit;
    //: Sinaliza para thread Terminar.
    procedure Terminate;
    {:
    Faz a atualização de um tag que solicitou uma LEITURA por scan.

    Retorna principalmente valores e resultados das leituras do driver.
    @param(Tag TTagRec. Estrutura com as informações do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanRead(Tag:TTagRec);
    {:
    Faz a atualização de um tag que solicitou uma ESCRITA por scan.

    Retorna principalmente valores e resultados da escrita do driver.
    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanWriteCallBack(SWPkg:PScanWriteRec);
  published
    //: Evento chamado pela thread para executar uma leitura por scan.
    property OnGetValue:TGetValues read PGetValues write PGetValues;
  end;

  {:
  Classe de thread reponsável processar as escritas por scan e por manter os
  tags com seus valores atualizados o mais rápido possível. Usado por
  TProtocolDriver.
  @seealso(TProtocolDriver)
  }
  TScanThread = class(TCrossThread)
  private
    FSTInitEventHandle:TCrossEvent;
    FDoScanRead:TNotifyEvent;
    FDoScanWrite:TScanWriteProc;
    FMinScan:DWORD;
    erro:Exception;
    FSpool:TMessageSpool;
    PScanUpdater:TScanUpdate;
    procedure CheckScanWriteCmd;
    procedure SyncException;
  protected
    //: @exclude
    procedure Execute; override;
  public
    //: @exclude
    constructor Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
    //: @exclude
    destructor Destroy; override;
    //: Ao chamar @name, espera a thread sinalizar a sua inicialização.
    procedure WaitInit;
    {:
    Solicita uma escrita de valores por scan para a thread do driver de protocolo.

    @param(SWPkg PScanWriteRec. Ponteiro para estrutura com as informações
           da escrita por scan do tag.)
    @raises(Exception caso a thread esteja suspensa ou não sinalize a sua
            inicialização em 5 segundos.)
    }
    procedure ScanWrite(SWPkg:PScanWriteRec);
    {:
    Atualiza as informações do driver a respeito dos tags dependentes. Chamado
    quando alguma propriedade de um tag sofre alguma mudança.
    @param(Tag TTag. Tag quem sofreu a mudança.)
    @param(Change TChangeType. Que propriedade sofreu a alteração.)
    @param(oldValue DWORD. Valor antigo da propriedade.)
    @param(newValue DWORD. Novo valor da propriedade.)
    @seealso(TProtocolDriver.TagChanges)
    }
    procedure TagChanges(Tag:TTag; Change:TChangeType; oldValue, newValue:DWORD);
    //: Adiciona um tag como dependente do driver.
    procedure AddTag(Tag:TTag);
    //: Remove um tag dependente do driver.
    procedure RemoveTag(Tag:TTag);
  published
    {:
    Diz quantos milisegundos o driver esperar caso não seja feita nenhuma
    operação de scan, a fim de evitar alto consumo de processador inutilmente.
    }
    property MinTimeOfScan:DWORD read FMinScan write FMinScan nodefault;
    //: Evento chamado para realizar a atualização do valores dos tags.
    property OnDoScanRead:TNotifyEvent read FDoScanRead write FDoScanRead;
    {:
    Evento chamado para executar uma escrita por scan.
    @seealso(TScanWriteProc)
    }
    property OnDoScanWrite:TScanWriteProc read FDoScanWrite write FDoScanWrite;
  end;

  {:
  Classe base para drivers de protocolo.
  @author(Fabio Luis Girardi)
  
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
    PSyncEventHandle:TCrossEvent;
    PASyncEventHandle:TCrossEvent;
    PAcceptUnsolicitedMsgs:Boolean;
    PTagCount:Cardinal;
    PTags:array of TTag;
    PScanThread:TScanThread;
    PScanUpdateThread:TScanUpdate;
    procedure SetCommPort(CommPort:TCommPortDriver);
    procedure DoExceptionIndexOut(index:integer);
    function  GetTag(index:integer):TTag;
    function  GetTagName(index:integer):String;
    function  GetTagByName(Nome:String):TTag;
    procedure InternalDoScanRead(Sender:TObject);
    function  InternalDoScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
    procedure InternalDoGetValue(const TagRec:TTagRec; var values:TScanReadRec);
  protected
    //: Armazena o tamanho máximo dos blocos.
    PNewBlockOn:Cardinal;
    //: Armazena a ID (número único) do driver.
    PDriverID:Cardinal;
    //: Armazena o driver de porta associado a esse driver de protocolo.
    PCommPort:TCommPortDriver;
    {:
    Estrutura que armazena os resultados de IO assincronos do driver de porta
    (TCommPortDriver).
    }
    ResultAsync:TIOPacket;
    {:
    Estrutura que armazena os resultados de IO sincronos do driver de porta
    (TCommPortDriver).
    }
    ResultSync:TIOPacket;
    //: Armazena o ID (número único) da próxima leitura por scan.
    FScanReadID:DWORD;
    //: Armazena o ID (número único) da próxima escrita por scan.
    FScanWriteID:DWORD;
    //: Armazena a evento usado para parar as threads do driver de protocolo.
    PPause:TCrossEvent;
    //: Armazena a seção crítica para proteger partes comuns ao driver e ao usuário.
    PProtocolCS:TCriticalSection;
    //: Armazena a seção crítica que protege areas comuns a muitas threads.
    PCallersCS:TCriticalSection;
    {:
    Reinicializa o evento de operações assincronas. Deve ser chamado sempre antes
    de enviar um pedido assincrono para o driver de porta (TCommPortDriver).
    }
    procedure ResetASyncEvent;
    {:
    Sinaliza o evento de operações assincronas. Chamado automáticamente quando
    o driver de porta (TCommPortDriver) retornar os resultados da I/O.
    }
    procedure SetASyncEvent;
    //: @exclude
    procedure Loaded; override;
    {:
    Espera um tempo pelo retorno dos resultados do driver de porta (TCommPortDriver).
    @param(miliseconds Integer. Tempo máximo que devo aguardar pelo retorno dos
           dados do driver de porta (TCommPortDriver).)
    @returns(Consulte a documentação de TEvent.WaitFor da seu ambiente de
             desenvolvimento.)
    }
    function  WaitASyncEvent(miliseconds:Integer):TWaitResult;
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
    procedure CommPortAsyncCallBack(var Result:TIOPacket);
    {:
    Callback @bold(sincrono) que o driver de porta (TCommPortDriver) irá chamar para
    retornar os resultados de I/O.
    }
    procedure CommPortSyncCallBack(var Result:TIOPacket);
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
    Método chamado pelas threads do driver de protocolo para realizar leitura dos
    tags a ele associado.
    }
    procedure DoScanRead(Sender:TObject); virtual; abstract; //realiza o scan (leitura) dos tags (blocos)
    {:
    Função chamada pelas threads do driver de protocolo para executar as escritas
    por scan.
    @param(TagRec TTagRec. Informações do tag a ser escrito.)
    @param(values TArrayOfDouble. Valores a serem escritos.)
    }
    function  DoScanWrite(TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult; virtual;
    {:
    Método chamado pelas threads do driver de protocolo para atualizar os valores
    dos tags.
    @param(TagRec TTagRec. Estrutura com informações do tag.)
    @param(values TArrayOfDouble. Armazena os valores que serão enviados ao tag.)
    }
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); virtual; abstract; //metodo que retorna os valores do Memory Blocks Manager
    {:
    Método chamado pelo driver de protocolo para atualizar as informações a
    respeito de um tag quando este tem alguma propriedade alterada.

    @param(TagObj TTag. Tag quem sofreu a mudança.)
    @param(Change TChangeType. Que propriedade sofreu a alteração.)
    @param(oldValue DWORD. Valor antigo da propriedade.)
    @param(newValue DWORD. Novo valor da propriedade.)
    @seealso(TagChanges)
    }
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); virtual; abstract; //realiza uma mundanca em alguma propriedade do tag;
    {:
    Codifica um pedido de leitura/escrita de um tag e um conjunto de valores
    (caso for uma ação de escrita de valores) em um pacote de dados (sequencia
    de bytes) para ser enviado ao equipamento pelo driver de porta.
    @param(tagrec TTagRec. Estrutura com as informações do tag.)
    @param(ToWrite TArrayOfDouble. Conjunto de valores a codificar.)
    @param(ResultLen Integer. Caso o pacote retornado gere um outro pacote de
           dados ao ser enviado para o equipamento, ResultLen diz qual será
           o tamanho desse pacote retornado.)
    @returns(BYTES. Sequência de bytes do pacote gerado.)
    }
    function  EncodePkg(tagrec:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES; virtual; abstract; //codifica um pedido e retorna o pacote da solicitação
    {:
    Decodifica um pacote (seqüência de bytes) vindo do equipamento e retira os
    valores dele caso existirem e atualiza os tags relacionados.
    @param(pkg TIOPacket. Estrutura retornada pelo driver de porta
           (TCommPortDriver) com o pacote a decodificar.)
    @param(values TArrayOfDouble. Valores que foram extraidos do pacote.)
    @returns(Veja TProtocolIOResult.)
    }
    function  DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult; virtual; abstract; //decodifica um pacote de dados;

    //: @exclude
    property  NewBlockOn:Cardinal read PNewBlockOn;  //informa ao driver qtas memorias poderam existir vazias para o bloco nào ser quebrado.
    //: @exclude
    property  AcceptUnsolicitedMsgs:Boolean read PAcceptUnsolicitedMsgs write PAcceptUnsolicitedMsgs default true;
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
    @param(oldValue DWORD. Valor antigo da propriedade.)
    @param(newValue DWORD. Novo valor da propriedade.)
    @seealso(TProtocolDriver.TagChanges)
    }
    procedure TagChanges(TagObj:TTag; Change:TChangeType; oldValue, newValue:DWORD);
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
    @returns(DWORD. Número único do pedido de leitura por scan.)
    }
    function  ScanRead(const tagrec:TTagRec):DWORD;
    {:
    Solicita a escrita por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    @returns(DWORD. Número único do pedido de escrita por scan.)
    }
    function  ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):DWORD;
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
    property TagCount:Cardinal read PTagCount;
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
//                   inicio das declarações da TScanUpdate
////////////////////////////////////////////////////////////////////////////////
constructor TScanUpdate.Create(StartSuspended:Boolean);
begin
  inherited Create(StartSuspended);
  FEnd := TCrossEvent.Create(nil, true, false, 'EndOfThread'+IntToStr(UniqueID));
  FEnd.ResetEvent;
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  FSUTInitEventHandle := TCrossEvent.Create(nil,true,false,'ScanUpdateThreadInit'+IntToStr(UniqueID));
  FDoSomethingEventHandle := TCrossEvent.Create(nil,true,false,'ScanUpdateThreadDoSomething'+IntToStr(UniqueID));
end;

destructor TScanUpdate.Destroy;
begin
  inherited Destroy;
  FDoSomethingEventHandle.Destroy;
  FSUTInitEventHandle.Destroy;
  FSpool.Destroy;
  FEnd.Destroy;
end;

procedure TScanUpdate.Terminate;
begin
  TCrossThread(self).Terminate;
  DoSomething;
  repeat
     Application.ProcessMessages;
  until WaitEnd(1)=wrSignaled;
end;

function  TScanUpdate.WaitEnd(timeout:Cardinal):TWaitResult;
begin
   Result := FEnd.WaitFor(timeout);
end;

procedure TScanUpdate.Execute;
var
  PMsg:TMsg;
begin
   //sinaliza q a fila de mensagens esta criada
   FSUTInitEventHandle.SetEvent;
   while not Terminated do begin
      WaitToDoSomething;
      while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANREAD,WM_TAGSCANREAD,true) do begin
         try
            CheckScanWrite;
        
            FTagRec := PTagRec(PMsg.wParam);
            TagCBack:=FTagRec^.CallBack;
            Fvalues.Offset:=FTagRec^.OffSet;

            if Assigned(PGetValues) then begin
               if not Terminated then
                  PGetValues(FTagRec^, Fvalues)
            end else
               Fvalues.LastQueryResult := ioDriverError;

            FCmd:=tcScanRead;
        
            Synchronize(SyncCallBack);
            
            //libera a memoria ocupada pelos pacotes
            SetLength(Fvalues.Values, 0);
            Dispose(FTagRec);
            TagCBack:=nil;
         except
            on E: Exception do begin
               Ferro := E;
               Synchronize(SyncException);
            end;
         end;
      end;
   end;
   FEnd.SetEvent;
end;

procedure TScanUpdate.WaitInit;
begin
  while FSUTInitEventHandle.WaitFor($FFFFFFFF)<>wrSignaled do ;
end;

procedure TScanUpdate.ScanRead(Tag:TTagRec);
var
  tagpkg:PTagRec;
begin
  if FSUTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  New(tagpkg);
  Move(tag,tagpkg^,sizeof(TTagRec));
  FSpool.PostMessage(WM_TAGSCANREAD,tagpkg,nil,false);
  DoSomething;
end;

procedure TScanUpdate.ScanWriteCallBack(SWPkg:PScanWriteRec);
begin
  if FSUTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

   FSpool.PostMessage(WM_TAGSCANWRITE,SWPkg,nil,true);
   DoSomething;
end;

procedure TScanUpdate.SyncException;
begin
  try
    Application.ShowException(Ferro);
  except
  end;
end;

procedure TScanUpdate.DoSomething;
begin
  FDoSomethingEventHandle.SetEvent;
end;

procedure TScanUpdate.WaitToDoSomething;
begin
  FDoSomethingEventHandle.ResetEvent;
  FDoSomethingEventHandle.WaitFor(1);
end;

procedure TScanUpdate.CheckScanWrite;
var
  x:PScanWriteRec;
  PMsg:TMsg;
begin
   while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANWRITE,WM_TAGSCANWRITE,true) do begin
      //recupera o pacote
      x := PScanWriteRec(PMsg.wParam);

      TagCBack                := x^.Tag.CallBack;
      Fvalues.Values          := x^.ValuesToWrite;
      Fvalues.Offset          := x^.Tag.OffSet;
      Fvalues.ValuesTimestamp := x^.ValueTimeStamp;
      Fvalues.LastQueryResult := x^.WriteResult;
      FCmd:=tcScanWrite;
      
      //sincroniza com o tag.
      Synchronize(SyncCallBack);
      
      //libera a memoria ocupada
      //pelo pacote
      SetLength(x^.ValuesToWrite,0);
      Dispose(x);
      TagCBack:=nil;
   end;
end;

procedure TScanUpdate.SyncCallBack;
begin
  if Terminated then exit;
  try
    if Assigned(TagCBack) then
      TagCBack(Fvalues.Values,Fvalues.ValuesTimestamp,FCmd,Fvalues.LastQueryResult, Fvalues.Offset);
  except
    on erro:Exception do begin
      Ferro:=erro;
      SyncException;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                   inicio das declarações da TScanThread
////////////////////////////////////////////////////////////////////////////////

constructor TScanThread.Create(StartSuspended:Boolean; ScanUpdater:TScanUpdate);
begin
  inherited Create(StartSuspended);
  Priority := tpHighest;
  FSpool := TMessageSpool.Create;
  PScanUpdater       := ScanUpdater;
  FSTInitEventHandle := TCrossEvent.Create(nil,true,false,'ScanThreadInit'+IntToStr(UniqueID));
  FMinScan := 0;
end;

destructor TScanThread.Destroy;
begin
  Terminate;
  FSTInitEventHandle.Destroy;
  FSpool.Destroy;
  inherited Destroy;
end;

procedure TScanThread.Execute;
begin
  //sinaliza q a fila de mensagens esta criada
  FSTInitEventHandle.SetEvent;
  while not Terminated do begin
    CheckScanWriteCmd;
    if Assigned(FDoScanRead) then
      try
        FDoScanRead(Self);
      except
        on E: Exception do begin
          erro := E;
          Synchronize(SyncException);
        end;
      end;
    //FSTSuspendEventHandle.ResetEvent;
    if FMinScan>0 then
      Sleep(FMinScan);
  end;
end;

procedure TScanThread.CheckScanWriteCmd;
var
  PMsg:TMsg;
  pkg:PScanWriteRec;
begin
   //verifica se exite algum comando de escrita...
   while (not Terminated) and FSpool.PeekMessage(PMsg,WM_TAGSCANWRITE,WM_TAGSCANWRITE,true) do begin
      pkg := PScanWriteRec(PMsg.wParam);

      if Assigned(FDoScanWrite) then
         pkg^.WriteResult := FDoScanWrite(pkg^.Tag,pkg^.ValuesToWrite)
      else
         pkg^.WriteResult := ioDriverError;

      if PScanUpdater<>nil then
         PScanUpdater.ScanWriteCallBack(pkg);
   end;
end;

procedure TScanThread.SyncException;
begin
  try
    Application.ShowException(erro);
  except
  end;
end;

procedure TScanThread.WaitInit;
begin
  while FSTInitEventHandle.WaitFor($FFFFFFFF)<>wrSignaled do ;
end;

procedure TScanThread.ScanWrite(SWPkg:PScanWriteRec);
begin
  if FSTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  //envia a mensagem
  FSpool.PostMessage(WM_TAGSCANWRITE,SWPkg,nil,true);
end;

procedure TScanThread.TagChanges(Tag:TTag; Change:TChangeType; oldValue, newValue:DWORD);
var
  pkg:PTagChangeRec;
begin
  if FSTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  pkg := AllocMem(sizeof(TTagChangeRec));
  pkg^.Tag := Tag;
  pkg^.Change := Change;
  pkg^.OldValue := oldValue;
  pkg^.NewValue := newValue;
  FSpool.PostMessage(WM_TAGCHANGE,pkg,nil,true);
end;

procedure TScanThread.AddTag(Tag:TTag);
begin
  if FSTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');
  FSpool.PostMessage(WM_ADDTAG,Tag,nil,true);
end;

procedure TScanThread.RemoveTag(Tag:TTag);
begin
  if FSTInitEventHandle.WaitFor(50000)<>wrSignaled then
    raise Exception.Create('A thread está suspensa?');

  FSpool.PostMessage(WM_DELTAG,Tag,nil,true);
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio das declarações do TProtocolDriver
////////////////////////////////////////////////////////////////////////////////

constructor TProtocolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDriverID := DriverCount;
  inc(DriverCount);

  PTagCount:=0;

  PPause := TCrossEvent.Create(nil,true,true,'ProtocolPause'+IntToStr(PDriverID));
  PProtocolCS := TCriticalSection.Create;
  PCallersCS := TCriticalSection.Create;

  PSyncEventHandle := TCrossEvent.Create(nil,true,false,'IOCallBackSync'+Name+IntToStr(PDriverID));
  PASyncEventHandle := TCrossEvent.Create(nil,true,false,'IOCallBackASync'+Name+IntToStr(PDriverID));

  PScanUpdateThread := TScanUpdate.Create(true);
  PScanUpdateThread.Priority:=tpHighest;
  PScanUpdateThread.OnGetValue := InternalDoGetValue;

  PScanThread := TScanThread.Create(true, PScanUpdateThread);
  PScanThread.Priority:=tpHighest;
  PScanThread.OnDoScanRead := InternalDoScanRead;
  PScanThread.OnDoScanWrite := InternalDoScanWrite;

  //if not (csDesigning in ComponentState) then
  PScanUpdateThread.Resume;

  //if not (csDesigning in ComponentState) then begin
  PScanThread.Resume;
  PScanThread.WaitInit;
  //end;
end;

destructor TProtocolDriver.Destroy;
var
  c:Integer;
begin
  for c:=0 to High(PTags) do
    TPLCTag(PTags[c]).RemoveDriver;

  SetCommPort(nil);

  //PScanThread.Terminate;
  PScanThread.Destroy;
  
  PScanUpdateThread.Terminate;
  PScanUpdateThread.WaitFor;

  PASyncEventHandle.Destroy;
  PSyncEventHandle.Destroy;
  
  SetLength(PTags,0);
  PProtocolCS.Destroy;  
  PCallersCS.Destroy;
  PPause.Destroy;
  inherited Destroy;
end;

procedure TProtocolDriver.SetCommPort(CommPort:TCommPortDriver);
begin
  try
    PPause.ResetEvent;
    PProtocolCS.Enter;
    //se for a mesma porta cai fora...
    if CommPort=PCommPort then exit;

    if PCommPort<>nil then begin
      if PCommPort.LockedBy=PDriverID then
        PCommPort.Unlock(PDriverID);
      PCommPort.CancelCallBack(CommPortAsyncCallBack);
      PCommPort.CancelCallBack(CommPortSyncCallBack);
      PCommPort.DelProtocol(Self);
    end;

    if CommPort<>nil then begin
      CommPort.ResumeCallBack(CommPortAsyncCallBack);
      CommPort.ResumeCallBack(CommPortSyncCallBack);
      CommPort.AddProtocol(Self);
    end;
    PCommPort := CommPort;
  finally
    PProtocolCS.Leave;
    PPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoAddTag(TagObj:TTag);
var
  c:integer;
begin
  for c:=0 to High(PTags) do
    if PTags[c]=TagObj then
      raise Exception.Create('Este Tag já esta registrado com este driver!');
  inc(PTagCount);
  SetLength(PTags,PTagCount);
  PTags[PTagCount-1] := TagObj;
end;

procedure TProtocolDriver.DoDelTag(TagObj:TTag);
var
  c:Integer;
  h:integer;
  found:boolean;
begin
  if PTagCount<=0 then exit;

  h:=High(PTags);
  found := false;
  for c:=0 to h do
    if PTags[c]=TagObj then begin
      found := true;
      break;
    end;
  if found then begin
    PTags[c] := PTags[h];
    PTagCount := PTagCount - 1;
    SetLength(PTags,PTagCount);
  end;
end;

procedure TProtocolDriver.AddTag(TagObj:TTag);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;

  try
    PPause.ResetEvent;
    PProtocolCS.Enter;
    DoAddTag(TagObj);
  finally
    PProtocolCS.Leave;
    PPause.SetEvent;    
  end;
end;

procedure TProtocolDriver.RemoveTag(TagObj:TTag);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;
  try
    PPause.ResetEvent;
    PProtocolCS.Enter;
    DoDelTag(TagObj);
  finally
    PProtocolCS.Leave;
    PPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoExceptionIndexOut(index:integer);
begin
  try
    PProtocolCS.Enter;
    if (index>high(PTags)) then
      raise Exception.Create('Indice fora dos limites!');
  finally
    PProtocolCS.Leave;
  end;
end;

function TProtocolDriver.GetTag(index:integer):TTag;
begin
  try
    PProtocolCS.Enter;
    DoExceptionIndexOut(index);
    result:=PTags[index];
  finally
    PProtocolCS.Leave;
  end;
end;

function TProtocolDriver.GetTagName(index:integer):String;
begin
  try
    PProtocolCS.Enter;
    Result:='';
    DoExceptionIndexOut(index);
    result:=PTags[index].Name;
  finally
    PProtocolCS.Leave;
  end;

end;

function TProtocolDriver.GetTagByName(Nome:String):TTag;
var
  c:Integer;
begin
  try
    PProtocolCS.Enter;
    Result := nil;
    for c:=0 to High(PTags) do
      if PTags[c].Name = Nome then begin
        Result := PTags[c];
        break;
      end;
  finally
    PProtocolCS.Leave;
  end;
end;

function TProtocolDriver.IsMyTag(TagObj:TTag):Boolean;
var
  c:integer;
begin
  try
    PPause.ResetEvent;
    PProtocolCS.Enter;
    Result := false;
    for c:=0 to High(PTags) do
      if TagObj=PTags[c] then begin
        Result := true;
        break;
      end;
  finally
    PProtocolCS.Leave;
    PPause.SetEvent;
  end;
end;

procedure TProtocolDriver.TagChanges(TagObj:TTag; Change:TChangeType; oldValue, newValue:DWORD);
begin
  if (csReading in TagObj.ComponentState) or (csDesigning in TagObj.ComponentState) then
    exit;
  try
    PPause.ResetEvent;
    PProtocolCS.Enter;
    DoTagChange(TagObj,Change,oldValue,newValue);
  finally
    PProtocolCS.Leave;
    PPause.SetEvent;
  end;
end;

function TProtocolDriver.ScanRead(const tagrec:TTagRec):DWORD;
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
    if PScanUpdateThread<>nil then
      PScanUpdateThread.ScanRead(tagrec);

    Result := FScanReadID;

  finally
    PCallersCS.Leave;
  end;
end;

function TProtocolDriver.ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):DWORD;
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
    if PScanThread<>nil then
      PScanThread.ScanWrite(pkg);

    Result := FScanWriteID;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.Read(const tagrec:TTagRec);
var
  pkg:BYTES;
  ResultLen:Integer;
  values:TArrayOfDouble;
  cmd:TIOCommand;
begin
  try
    PCallersCS.Enter;
    
    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDesigning in ComponentState) or
       (csDestroying in ComponentState) then
      exit;

    
    ResultLen := 0;
    if PCommPort=nil then begin
      if Assigned(tagrec.CallBack) then
        tagrec.CallBack(nil,Now,tcRead,ioDriverError,tagrec.OffSet);
    end else begin
      pkg := EncodePkg(tagrec,nil,ResultLen);

      if Length(pkg)>0 then
        cmd := iocWriteRead
      else
        cmd := iocRead;

      PCommPort.IOCommandSync(cmd,pkg,ResultLen,Length(pkg),PDriverID,0,CommPortSyncCallBack,false);
      if ResultSync.ReadIOResult = iorOK then begin
        if DecodePkg(ResultSync,values)=ioOk then begin
          tagrec.CallBack(values,Now,tcRead,ioOk,tagrec.OffSet);
        end else begin
          tagrec.CallBack(values,Now,tcRead,ioCommError,tagrec.OffSet);
        end;
        SetLength(values,0);
      end else begin
        case ResultSync.ReadIOResult of
          iorTimeOut:
            tagrec.CallBack(nil,Now,tcRead,ioTimeOut,tagrec.OffSet);
          iorNotReady, iorPortError, iorNone:
            tagrec.CallBack(nil,Now,tcRead,ioDriverError,tagrec.OffSet);
        end;
      end;
    end;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.Write(const tagrec:TTagRec; const Values:TArrayOfDouble);
var
  pkg:BYTES;
  ResultLen:Integer;
  Aux:TArrayOfDouble;
begin
  try
    PCallersCS.Enter;

    //verifica se esta em edição, caso positivo evita o comando.
    if (csReading in ComponentState) or
       (csDesigning in ComponentState) or
       (csDestroying in ComponentState) then
      exit;

    ResultLen := 0;
    if PCommPort=nil then begin
      if Assigned(tagrec.CallBack) then
        tagrec.CallBack(nil,now,tcWrite,ioDriverError,tagrec.OffSet);
    end else begin
      pkg := EncodePkg(tagrec,Values,ResultLen);
      if ResultLen=0 then begin
        PCommPort.IOCommandSync(iocWrite,pkg,0,Length(pkg),PDriverID,0,CommPortSyncCallBack,true);
        if ResultSync.WriteIOResult = iorOK then begin
          if Assigned(tagrec.CallBack) then
            tagrec.CallBack(values,Now,tcWrite,ioOk,tagrec.OffSet)
        end else
          case ResultSync.ReadIOResult of
            iorTimeOut:
              if Assigned(tagrec.CallBack) then
                tagrec.CallBack(nil,Now,tcWrite,ioTimeOut,tagrec.OffSet);
            iorNotReady, iorPortError, iorNone:
              if Assigned(tagrec.CallBack) then
                tagrec.CallBack(nil,Now,tcWrite,ioDriverError,tagrec.OffSet);
          end;
      end else begin
        PCommPort.IOCommandSync(iocWriteRead,pkg,ResultLen,Length(pkg),PDriverID,0,CommPortSyncCallBack,true);
        if ResultSync.ReadIOResult = iorOK then begin
          if DecodePkg(ResultSync,Aux)=ioOk then begin
            if Assigned(tagrec.CallBack) then
              tagrec.CallBack(values,Now,tcWrite,ioOk,tagrec.OffSet);
          end else
            if Assigned(tagrec.CallBack) then
              tagrec.CallBack(nil,Now,tcWrite,ioCommError,tagrec.OffSet);
        end else begin
          case ResultSync.ReadIOResult of
            iorTimeOut:
              if Assigned(tagrec.CallBack) then
                tagrec.CallBack(nil,Now,tcWrite,ioTimeOut,tagrec.OffSet);
            iorNotReady, iorPortError, iorNone:
              if Assigned(tagrec.CallBack) then
                tagrec.CallBack(nil,Now,tcWrite,ioDriverError,tagrec.OffSet);
          end;
        end;
      end;
    end;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.ResetASyncEvent;
begin
  PASyncEventHandle.ResetEvent;
end;

procedure TProtocolDriver.SetASyncEvent;
begin
  PASyncEventHandle.SetEvent;
end;

procedure TProtocolDriver.Loaded;
begin
  inherited Loaded;
end;

function TProtocolDriver.DoScanWrite(TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
var
  pkg:BYTES;
  ResultLen:Integer;
  Aux:TArrayOfDouble;
  pkgid:DWORD;
begin
  Result := ioDriverError;
  if PCommPort=nil then begin
    Result := ioDriverError;
  end else begin
    pkg := EncodePkg(tagrec,Values,ResultLen);
    if ResultLen=0 then begin
      ResetASyncEvent;
      pkgid := PCommPort.IOCommandASync(iocWrite,pkg,0,Length(pkg),PDriverID,0,CommPortASyncCallBack,true);
      if pkgid<>0 then begin
        repeat
          WaitASyncEvent(60000);
        until pkgid=ResultAsync.PacketID;
        if ResultASync.WriteIOResult = iorOK then
           Result := ioOk
        else
           case ResultASync.ReadIOResult of
             iorTimeOut:
               Result := ioTimeOut;
             iorNotReady, iorPortError, iorNone:
               Result := ioDriverError;
           end;
      end else
        Result := ioDriverError;
    end else begin
      ResetASyncEvent;
      pkgid := PCommPort.IOCommandASync(iocWriteRead,pkg,ResultLen,Length(pkg),PDriverID,0,CommPortASyncCallBack,true);
      if pkgid<>0 then begin
        repeat
          WaitASyncEvent(60000);
        until pkgid=ResultAsync.PacketID;
        if ResultASync.ReadIOResult = iorOK then begin
          if DecodePkg(ResultASync,aux)=ioOk then
            Result := ioOk
          else
            Result := ioCommError;
          SetLength(Aux,0);
        end else begin
          case ResultASync.ReadIOResult of
            iorTimeOut:
              Result := ioTimeOut;
            iorNotReady, iorPortError, iorNone:
              Result := ioDriverError;
          end;
        end;
      end else
        Result := ioDriverError;
    end;
  end;
end;

function TProtocolDriver.WaitASyncEvent(miliseconds:Integer):TWaitResult;
begin
  Result := PASyncEventHandle.WaitFor(miliseconds)
end;

procedure TProtocolDriver.CommPortAsyncCallBack(var Result:TIOPacket);
begin
  CopyIOPacket(Result,ResultAsync);
  PASyncEventHandle.SetEvent;
end;

procedure TProtocolDriver.CommPortSyncCallBack(var Result:TIOPacket);
begin
  CopyIOPacket(Result,ResultSync);
end;

procedure TProtocolDriver.CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
//var
//  c:Integer;
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
end;

procedure TProtocolDriver.InternalDoScanRead(Sender:TObject);
begin
   try
      PPause.WaitFor($FFFFFFFF);            
      PProtocolCS.Enter;
      DoScanRead(Sender);
   finally
      PProtocolCS.Leave;
   end;
end;

function  TProtocolDriver.InternalDoScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
begin
   try
      PPause.WaitFor($FFFFFFFF);   
      PProtocolCS.Enter;
      Result := DoScanWrite(TagRec,values)
   finally
      PProtocolCS.Leave;
   end;
end;

procedure TProtocolDriver.InternalDoGetValue(const TagRec:TTagRec; var values:TScanReadRec);
begin
   try
      PPause.ResetEvent;
      PProtocolCS.Enter;
      DoGetValue(TagRec,values);
   finally
      PProtocolCS.Leave;
      PPause.SetEvent;      
   end;
end;

end.

