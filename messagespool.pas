{:
 @abstract(Unit que implementa o enfileiramento de mensagens internas do programa,
           multiplataforma.)
 
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
 
 Feito para suprir a necessidade de comunicação inter-processos (threads) de
 forma independente de sistema operacional.

 Sua implementação dos métodos é semelhante ao sistema de mensagens do windows,
 porem com algumas melhorias, tais como prioridade de mensagens (mensagens que
 devem ser processadas antes de outras).
}
unit MessageSpool;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, CrossEvent, SyncObjs;

type

  {:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Estrutura que identifica uma mensagem.
  @member MsgID Classifica a mensagem.
  @member lParam Ponteiro para dados (caso necessário).
  @member wParam Ponteiro para dados (caso necessário).
  @member Priority Identifica se é uma mensagem com alta prioridade.  
  }
  TMSMsg=record
    MsgID:Cardinal;
    lParam:Pointer;
    wParam:Pointer;
    Priority:Boolean;
  end;
  //: Aponta para uma mensagem
  PMSMsg = ^TMSMsg;

  {:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Estrutura usada para montar uma fila de mensagens.
  @member Msg Mensagem do elemento da fila.
  @member NextMsg Aponta para o próximo elemento da fila de mensagens. Tem o valor @code(nil) caso esse elemento seja o último.
  @member PriorMsg Aponta para o elemento anterior da fila de mensagens. Tem o valor @code(nil) caso esse elemento seja o primeiro.
  }
  TMsgPkg = record
    Msg:TMSMsg;
    NextMsg:Pointer;
    PriorMsg:Pointer;
  end;
  //: Ponteiro de um elemento da fila de mensagens.
  PMsgPkg = ^TMsgPkg;

  {:
  @abstract(Classe de enfileiramento de mensagens internas do programa)

  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Classe criada para suprir a necessidade de comunicação inter-processos (threads)
  de forma independente de sistema operacional.

  Sua Implementação dos métodos é semelhante ao sistema de mensagens do Windows,
  porem com algumas melhorias, tais como prioridade de mensagens (mensagens que
  devem ser processadas antes que outras).
  }
  TMessageSpool = class(TObject)
  private
    FCs:TCriticalSection;
    Finit,FHasMsg:TCrossEvent;
    FirstMessage:PMsgPkg;
    NormalMsgs:PMsgPkg;
    LastMsg:PMsgPkg;
    FMsgCount:Integer;
  public
    //: Cria um objeto de enfileiramento de mensagens.
    constructor Create;
    //: Destroi um objeto de enfileiramento de mensagens.
    destructor  Destroy; override;
    {:
    Retira uma mensagem da fila. O critério de seleção é que o MsgID seja maior
    ou igual a uFilterMinMsg e menor ou igual a uFilterMaxMsg.
    @bold(Caso a mensagem solicitada não esteja na fila, aguarda ela chegar.)

    @param(Msg TMsg. Variável onde é retornada a mensagem da fila.)
    @param(uFilterMinMsg Cardinal. Filtra mensagem com MsgID maiores ou iguais aos
           valores passados nesse parametro.)
    @param(uFilterMaxMsg Cardinal. Filtra mensagem com MsgID menores ou iguais aos
           valores passados nesse parametro.)

    @bold(Caso uFilterMinMsg e uFilterMaxMsg sejam iguais, procura as mensagens
          cujo o MsgID seja igual a esses dois parametros.)

    @return(@true caso consiga pegar uma mensagem.)

    @seealso(PeekMessage)
    @seealso(PostMessage)
    }
    function  GetMessage (var Msg:TMSMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal):Boolean;

    {:
    Procura uma mensagem podendo retirá-la ou não da fila. O critério de seleção
    é que o MsgID seja maior ou igual a uFilterMinMsg e menor ou igual a
    uFilterMaxMsg. @bold(Caso não existam mensagens na fila com os critérios
    informados, retorna imediatamente, ao contrário de GetMessage, que espera
    uma mensagem chegar que bata com os seus critérios.)

    @param(Msg TMsg. Variável onde é retornada a mensagem da fila.)
    @param(uFilterMinMsg Cardinal. Filtra mensagem com MsgID maiores ou iguais aos
           valores passados nesse parametro.)
    @param(uFilterMaxMsg Cardinal. Filtra mensagem com MsgID menores ou iguais aos
           valores passados nesse parametro.)
    @param(Remove Boolean. @true se a mensagem encontrada deve ser removida
           da fila.)

    @bold(Caso uFilterMinMsg e uFilterMaxMsg sejam iguais, procura as mensagens
          cujo o MsgID seja igual a esses dois parametros.)

    @return(Retorna imediatamente retornando @true caso encontre alguma mensagem
    com os critérios informados ou @false caso não encontre nenhuma mensagem.) 

    @seealso(GetMessage)
    @seealso(PostMessage)
    }
    function  PeekMessage(var Msg:TMSMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal; Remove:Boolean):Boolean;

    {:
    Insere uma mensagem na Fila.

    @param(MsgID Cardinal. Número de classificação da mensagem.)
    @param(wParam Pointer Ponteiro de dados.)
    @param(lParam Pointer Ponteiro de dados.)
    @param(Priority Boolean. @true se a mensagem encontrada deve ser colocada no
           inicio da fila (Com Prioridade).)

    @seealso(GetMessage)
    @seealso(PeekMessage)
    }
    procedure PostMessage(MsgID:Cardinal; wParam, lParam:Pointer; Priority:Boolean);
  end;

var
  MSCount:Integer = 0;

implementation

constructor TMessageSpool.Create;
begin
  Finit := TCrossEvent.Create(nil,true,false,'MessageSpool'+IntToStr(MSCount));
  FHasMsg  := TCrossEvent.Create(nil,true,false,'HasMessage'+IntToStr(MSCount));
  FCs := TCriticalSection.Create;
  FirstMessage := nil;
  NormalMsgs := nil;
  LastMsg := nil;
  FMsgCount := 0;
  Finit.SetEvent;
  MSCount := MSCount + 1;
end;

destructor TMessageSpool.Destroy;
var
  msg, nextmsg:PMsgPkg;
begin
  FCs.Acquire;
  msg := FirstMessage;
  while (msg<>nil) do begin
    nextmsg := (msg^).NextMsg;
    FreeMem(msg);
    msg := nextmsg;
  end;
  FCs.Release;
  Finit.destroy;
  FHasMsg.Destroy;
  FCs.Destroy;
  FCs := nil;;
  MSCount := MSCount - 1;
end;

function TMessageSpool.GetMessage(var Msg:TMSMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal):Boolean;
begin
  if FMsgCount=0 then begin
    FHasMsg.ResetEvent;
    //espera até que uma mensagem chegue;
    while FHasMsg.WaitFor($FFFFFFFF)<>wrSignaled do ;
    Result := PeekMessage(Msg,uFilterMinMsg,uFilterMaxMsg,true);
  end else
    Result := PeekMessage(Msg,uFilterMinMsg,uFilterMaxMsg,true);
end;

function TMessageSpool.PeekMessage(var Msg:TMSMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal; Remove:Boolean):Boolean;
var
  curmsg, aux:PMsgPkg;
  found:Boolean;
begin
  if FCs=nil then begin
     Result := false;
     exit;
  end;
  FCs.Acquire;
  Result := false;
  found := false;
  curmsg := FirstMessage;
  while (curmsg<>nil) do begin
    if (uFilterMinMsg<>0) and (uFilterMaxMsg<>0) then begin
      if (curmsg^.Msg.MsgID>=uFilterMinMsg) and (curmsg^.Msg.MsgID<=uFilterMaxMsg) then begin
        found := true;
        break;
      end;
    end else begin
      found := true;
      break;
    end;
    curmsg := curmsg^.NextMsg;
  end;
  
  if found then begin
    Msg := curmsg^.Msg;
    //se a mensagens encontrada é a primeira,
    //diz q a próxima msg é a primeira
    if Remove and (curmsg = FirstMessage) then
      FirstMessage := curmsg^.NextMsg;

    //se a mensagens encontrada é a ultima,
    //diz q a msg anterior é a ultima
    if Remove and (curmsg = LastMsg) then
      LastMsg := curmsg^.PriorMsg;

    //se a mensagem encontrada é a primeira normal
    //reaponta mensagens
    if Remove and (curmsg = NormalMsgs) then begin
      NormalMsgs := curmsg^.NextMsg;
      if NormalMsgs<>nil then
        NormalMsgs^.PriorMsg := curmsg^.PriorMsg;
    end;

    //se existe mensagens antes dessa
    if Remove and (curmsg^.PriorMsg<>nil) then begin
      aux := curmsg^.PriorMsg;
      aux^.NextMsg := curmsg^.NextMsg
    end;

    //se existem mensagens após essa msg
    if Remove and (curmsg^.NextMsg<>nil) then begin
      aux := curmsg^.NextMsg;
      aux^.PriorMsg := curmsg^.PriorMsg
    end;

    //libera a memória usada pela mensagem
    //caso a ordem seja para remover...
    if Remove then begin
      Dispose(curmsg);
      FMsgCount := FMsgCount - 1;
    end;

    Result := true;
  end;
  FCs.Release;
end;

procedure TMessageSpool.PostMessage(MsgID:Cardinal; wParam, lParam:Pointer; Priority:Boolean);
var
  msg, aux:PMsgPkg;
  err:boolean;
begin
  //se esta sendo destroido o objeto, sai.
  if FCs=nil then
     exit;

  //adiquire direiro sobre a fila.
  FCs.Acquire;

  //cria uma mensagem na memória
  err := false;
  try
     New(msg);
     if msg=nil then
        err := true;
  except
     err:=true;
  end;
  if err then
     raise Exception.Create('Memória insuficiente!');
     
  msg^.Msg.MsgID := MsgID;
  msg^.Msg.lParam := lParam;
  msg^.Msg.wParam := wParam;
  msg^.Msg.Priority := Priority;
  msg^.NextMsg := nil;
  msg^.PriorMsg := nil;

  //se a primeira msg é nula,a fila esta vazia.
  if FirstMessage=nil then begin
    FirstMessage := msg;
    LastMsg := msg;
    if not Priority then
      NormalMsgs := msg;
  end else begin
    //se existem msgs na fila.

    //e se existe só uma msg
    if FirstMessage = LastMsg then begin
      FirstMessage^.NextMsg := msg;
      msg^.PriorMsg := FirstMessage;
      LastMsg := msg;
      if (not Priority) and (NormalMsgs=nil) then
        NormalMsgs := msg;
    end else begin
      //se existirem duas ou mais mensagens na fila...
      if Priority then
        if NormalMsgs=nil then begin
          LastMsg^.NextMsg := msg;
          msg^.PriorMsg := LastMsg;
          LastMsg := msg;
        end else begin
          aux := NormalMsgs^.PriorMsg;
          //se é nulo, é a primeira msg
          if aux=nil then
            FirstMessage := msg
          else
            aux^.NextMsg :=  msg;
          msg^.PriorMsg := aux;
          msg^.NextMsg := NormalMsgs;
          NormalMsgs^.PriorMsg := msg;
        end
      else begin
        LastMsg^.NextMsg := msg;
        msg^.PriorMsg := LastMsg;
        LastMsg := msg;
        if (NormalMsgs=nil) then
          NormalMsgs := msg;
      end;
    end;
  end;

  //sinaliza que há mensagens na fila.
  FMsgCount := FMsgCount + 1;
  FHasMsg.SetEvent;

  //libera a fila.
  FCs.Release;
end;

end.

