{$i ../common/pscada_settings.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit com os tipos que são usados frequentemente em drivers de portas de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit with types/definitions commonly used on communication ports.)
}
{$ENDIF}

unit commtypes;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses Classes, pSCADA_Types;

type

  {$IFDEF PORTUGUES}
  {:
    Interface de notificações de eventos para os drivers de protocolo.
  }
  {$ELSE}
  {:
    Event notification interface for protocol drivers.
  }
  {$ENDIF}
  IPortDriverEventNotification = interface
  ['{26B0F551-5B46-49D9-BCA1-AD621B3775CF}']
    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é aberta.
    {$ELSE}
    //: Returns the event to be called when communication port opens.
    {$ENDIF}
    function  GetPortOpenedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é fechada.
    {$ELSE}
    //: Returns the event to be called when communication port closed.
    {$ENDIF}
    function  GetPortClosedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Retorna o evento a ser chamado quando a porta é disconectada.
    {$ELSE}
    //: Returns the event to be called when communication port is disconnected.
    {$ENDIF}
    function  GetPortDisconnectedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    {:
    Conjunto de eventos da porta de comunicação que o protocolo deseja ser
    notificado.
    @seealso(TPortEvents)
    @seealso(TNotifyThisEvents)
    }
    {$ELSE}
    {:
    Set of events that the protocol driver wants be notified.
    @seealso(TPortEvents)
    @seealso(TNotifyThisEvents)
    }
    {$ENDIF}
    function  NotifyThisEvents:TNotifyThisEvents;

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for aberta.
    {$ELSE}
    //: Procedure called when the communication port opens.
    {$ENDIF}
    procedure DoPortOpened(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for fechada.
    {$ELSE}
    //: Procedure called when the communication port was closed.
    {$ENDIF}
    procedure DoPortClosed(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for desconectada.
    {$ELSE}
    //: Procedure called when the communication port was disconnected.
    {$ENDIF}
    procedure DoPortDisconnected(Sender: TObject);

    {$IFDEF PORTUGUES}
    //: Método que vai ser chamado quando a porta for destruida.
    {$ELSE}
    //: Procedure called when the communication port has been destroied.
    {$ENDIF}
    procedure DoPortRemoved(Sender:TObject);
  end;

  IPortDriverEventNotificationArray = array of IPortDriverEventNotification;

implementation

end.
