unit hmi_animation_timers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fgl;

type
  TCallbackList = array of TNotifyEvent;

  { TTimerEntry }

  TTimerEntry = class(TObject)
  private
    FInterval:LongWord;
    FID:LongInt;
    FTimer:TTimer;
    FCallbackList:TCallbackList;
    function  IndexOfCallback(const aCallBack:TNotifyEvent):Integer;
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(Interval:LongWord; aCallBack:TNotifyEvent); overload;
    destructor Destroy; override;
    procedure AddTimerCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallbacksFromObject(aObject:TObject);
    //: quantos ainda querem ser avisados neste intervalo
    //: how many still want to be told on this interval
    function  CallbackCount:Integer;
  end;

  TTimerList = specialize TFPGMap<LongWord,TTimerEntry>;

  { TTimerManager }

  TTimerManager = class(TObject)
  private
    fTimerList:TTimerList;
    //: solta as entradas que ficaram sem ninguem para avisar
    //: drops the entries left with nobody to tell
    procedure DropEmptyEntries;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTimerCallback(Interval:LongWord; aCallBack:TNotifyEvent);
    procedure RemoveTimerCallback(Interval:LongWord; aCallBack:TNotifyEvent);
    procedure RemoveCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallbacksFromObject(aObject:TObject);
    //: quantos intervalos tem temporizador vivo
    //: how many intervals have a live timer
    function  TimerCount:Integer;
  end;

  function GetAnimationTimer:TTimerManager;

implementation

{ TTimerEntry }

procedure TTimerEntry.OnTimer(Sender: TObject);
var
  i: Integer;
  aRodada: TCallbackList;
begin
  //sobre uma copia: o limite do for e' calculado uma unica vez, na entrada, e
  //os avisados removem a si mesmos de dentro da propria chamada - e' o que um
  //controle faz quando para de piscar. O RemoveCallback joga o ultimo da lista
  //na vaga liberada e encurta o vetor, entao a volta passava do fim e chamava
  //ou quem ja' tinha saido, ou lixo. O except vazio abaixo fazia o estrago
  //sumir sem deixar rastro.
  //over a copy: the for limit is evaluated once, on entry, and the listeners
  //remove themselves from inside their own call - which is what a control does
  //when it stops blinking. RemoveCallback drops the last one into the freed
  //slot and shrinks the array, so the round ran past the end and called either
  //someone already gone or garbage. The empty except below made the damage
  //vanish without a trace.
  //a copia e' explicita de proposito: atribuir o vetor direto tambem
  //funcionaria, porque o FPC copia na escrita quando o SetLength encontra mais
  //de uma referencia - mas correcao que depende desse detalhe da linguagem
  //some no dia em que alguem trocar o tipo da lista.
  //the copy is explicit on purpose: assigning the array directly would work
  //too, because FPC copies on write when SetLength finds more than one
  //reference - but a fix that leans on that language detail disappears the day
  //someone changes the list's type.
  aRodada:=Copy(FCallbackList, 0, Length(FCallbackList));
  for i:=0 to High(aRodada) do
    //e nao basta a copia: quem saiu no meio da volta - porque o controle foi
    //destruido e o destrutor soltou os avisos dele - nao pode ser chamado
    //depois de sair. A copia resolve o estouro; esta conferencia resolve o
    //ponteiro morto.
    //and the copy alone is not enough: whoever left mid-round - because the
    //control was destroyed and its destructor dropped its callbacks - must not
    //be called after leaving. The copy solves the overrun; this check solves
    //the dead pointer.
    if IndexOfCallback(aRodada[i])>=0 then
      try
        aRodada[i](Sender);
      except
      end;
end;

var
  TimerCount:Integer = 0;

constructor TTimerEntry.Create(Interval: LongWord; aCallBack: TNotifyEvent);
begin
  inherited Create;
  FInterval:=Interval;
  FID:=TimerCount;
  Inc(TimerCount);
  SetLength(FCallbackList, 0);
  FTimer:=TTimer.Create(nil);
  FTimer.Interval := Interval;
  FTimer.OnTimer  := @OnTimer;
  FTimer.Enabled  := true;
  AddTimerCallback(aCallBack);
end;

destructor TTimerEntry.Destroy;
begin
  FreeAndNil(FTimer);
  SetLength(FCallbackList,0);
  inherited Destroy;
end;

function TTimerEntry.IndexOfCallback(const aCallBack: TNotifyEvent):Integer;
var
  i: Integer;
begin
  //um aviso e' o par objeto+metodo; comparar os dois e' o que distingue o
  //mesmo metodo de dois controles diferentes.
  //a callback is the object+method pair; comparing both is what tells the same
  //method of two different controls apart.
  Result:=-1;
  for i:=0 to High(FCallbackList) do
    if (TMethod(FCallbackList[i]).Data=TMethod(aCallBack).Data) and
       (TMethod(FCallbackList[i]).Code=TMethod(aCallBack).Code) then begin
      Result:=i;
      exit;
    end;
end;

procedure TTimerEntry.AddTimerCallback(aCallBack: TNotifyEvent);
var
  i: Integer;
begin
  if IndexOfCallback(aCallBack)>=0 then exit;

  i:=Length(FCallbackList);
  SetLength(FCallbackList,i+1);
  FCallbackList[i]:=aCallBack;
end;

procedure TTimerEntry.RemoveCallback(aCallBack: TNotifyEvent);
var
  i: Integer;
begin
  i:=IndexOfCallback(aCallBack);
  if i<0 then exit;

  FCallbackList[i]:=FCallbackList[High(FCallbackList)];
  SetLength(FCallbackList,High(FCallbackList));
end;

function TTimerEntry.CallbackCount:Integer;
begin
  Result:=Length(FCallbackList);
end;

procedure TTimerEntry.RemoveCallbacksFromObject(aObject: TObject);
var
  i: Integer;
begin
  for i:=high(FCallbackList) downto 0 do
    if TMethod(FCallbackList[i]).Data=Pointer(aObject) then begin
      FCallbackList[i]:=FCallbackList[High(FCallbackList)];
      SetLength(FCallbackList,High(FCallbackList));
    end;

end;

{ TTimerManager }

constructor TTimerManager.Create;
begin
  fTimerList:=TTimerList.Create;
end;

destructor TTimerManager.Destroy;
var
  i: Integer;
begin

  for i:=fTimerList.Count-1 downto 0 do begin
     fTimerList.KeyData[fTimerList.Keys[i]].Free;
     fTimerList.Delete(i);
  end;
  FreeAndNil(fTimerList);
  inherited Destroy;
end;

procedure TTimerManager.AddTimerCallback(Interval: LongWord;
  aCallBack: TNotifyEvent);
var
  idx: LongInt;
begin
  idx:=fTimerList.IndexOf(Interval);
  if idx=-1 then
    fTimerList.Add(Interval,TTimerEntry.Create(Interval, aCallBack))
  else
    fTimerList.KeyData[Interval].AddTimerCallback(aCallBack);
end;

procedure TTimerManager.RemoveTimerCallback(Interval: LongWord;
  aCallBack: TNotifyEvent);
var
  idx: LongInt;
begin
  idx:=fTimerList.IndexOf(Interval);
  if idx<>-1 then begin
    fTimerList.KeyData[Interval].RemoveCallback(aCallBack);
    DropEmptyEntries;
  end;
end;

procedure TTimerManager.RemoveCallback(aCallBack: TNotifyEvent);
var
  k: Integer;
begin
  for k:=0 to fTimerList.Count-1 do
    fTimerList.KeyData[fTimerList.Keys[k]].RemoveCallback(aCallBack);
  DropEmptyEntries;
end;

procedure TTimerManager.RemoveCallbacksFromObject(aObject: TObject);
var
  k: Integer;
begin
  for k:=0 to fTimerList.Count-1 do
    fTimerList.KeyData[fTimerList.Keys[k]].RemoveCallbacksFromObject(aObject);
  DropEmptyEntries;
end;

procedure TTimerManager.DropEmptyEntries;
var
  k: Integer;
begin
  //sem ninguem para avisar, a entrada continuava na lista com o TTimer ligado:
  //uma tela que teve alarmes piscando a cada 300ms seguia acordando o programa
  //tres vezes por segundo para percorrer uma lista vazia, ate' o fim do
  //processo.
  //with nobody to tell, the entry stayed on the list with its TTimer running: a
  //screen that had alarms blinking every 300ms went on waking the program three
  //times a second to walk an empty list, until the process ended.
  for k:=fTimerList.Count-1 downto 0 do
    if fTimerList.KeyData[fTimerList.Keys[k]].CallbackCount=0 then begin
      fTimerList.KeyData[fTimerList.Keys[k]].Free;
      fTimerList.Delete(k);
    end;
end;

function TTimerManager.TimerCount:Integer;
begin
  Result:=fTimerList.Count;
end;

var
  FAnimationTimerManager:TTimerManager;

function GetAnimationTimer:TTimerManager;
begin
  Result:=FAnimationTimerManager;
end;

initialization
  FAnimationTimerManager:=TTimerManager.Create;

finalization
  FreeAndNil(FAnimationTimerManager);


end.

