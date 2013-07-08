{$i ../common/language.inc}
{$I ../common/delphiver.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação da base de todos os tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the base class of tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Avoid the use of Linux-Widget if fpc >= 2.7.1 (CONSOLEPASCALSCADA)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}

{$ENDIF}
unit Tag;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes

  {$IFDEF FPC}

    {$IFNDEF CONSOLEPASCALSCADA}
       ,LCLIntf, LCLType, Interfaces, Forms
    {$ENDIF}

  {$ELSE}
  ,Windows, Messages
  {$ENDIF}
  ;

{$IFNDEF FPC}
const
  PM_ASYNCVALUECHANGE = WM_USER + $0123;
{$ENDIF}


type
  {$IFDEF PORTUGUES}
  {:
  Define o intervalo de atualização dos tags.
  }
  {$ELSE}
  {:
  Defines the the update rate range of tags.
  }
  {$ENDIF}
  TRefreshTime = 1..$7FFFFFFF;

  {$IFDEF PORTUGUES}
  //: Array dinamico de valores flutuantes.
  {$ELSE}
  //: Dynamic array of double.
  {$ENDIF}
  TArrayOfDouble = array of double;

  {$IFDEF PORTUGUES}
  //: Ponteiro para um array dinamico de pontos flutuantes.
  {$ELSE}
    //: Points to a array of doubles.
  {$ENDIF}
  PArrayOfDouble = ^TArrayOfDouble;

  {$IFDEF PORTUGUES}
  {:
  Enumera todos os possíveis tamanhos de palavras dos tags.
  @value(pttDefault  Tamanho e tipo de dados dependentes das configuração do tag.)
  @value(pttShortInt Inteiro de 8 bits COM sinal.)
  @value(pttByte     Inteiro de 8 bits sem sinal.)
  @value(pttSmallInt Inteiro de 16 bits COM sinal.)
  @value(pttWord,    Inteiro de 16 bits SEM sinal.)
  @value(pttInteger  Inteiro de 32 bits COM sinal.)
  @value(pttDWord,   Inteiro de 32 bits SEM sinal.)
  @value(pttFloat    Flutuante de 32 bits.)
  @value(pttInt64    Inteiro de 64 bits COM sinal)
  @value(pttQWord    Inteiro de 64 bits SEM sinal)
  @value(pttDouble   Flutuante de 64 bits.)
  }
  {$ELSE}
  {:
  Enumerates all tag data types.
  @value(pttDefault  Word length and data type variable.)
  @value(pttShortInt Signed Integer, 8 bits sized.)
  @value(pttByte     Unsigned Integer, 8 bits sized.)
  @value(pttSmallInt Signed Integer, 16 bits sized.)
  @value(pttWord,    Unsigned Integer, 16 bits sized.)
  @value(pttInteger  Signed Integer, 32 bits sized.)
  @value(pttDWord,   Unsigned Integer, 32 bits sized.)
  @value(pttFloat    Float, 32 bits sized.)
  @value(pttInt64    Signed Integer, 64 bits sized)
  @value(pttQWord    Unsigned Integer, 64 bits sized)
  @value(pttDouble   Float, 64 bits sized.)
  }
  {$ENDIF}
  TTagType = (pttDefault,                      //size variable
              pttShortInt, pttByte,            //8 bits
              pttSmallInt, pttWord,            //16 bits
              pttInteger,  pttDWord, pttFloat, //32 bits
              pttInt64,    pttQWord, pttDouble //64 bits
             );

  {$IFDEF PORTUGUES}
  {:
  Enumera os possíveis tipos de comandos aceitos pelo tag.

  @value(tcScanRead        Leitura de valor através do scan do driver de protocolo (assincrona).)
  @value(tcScanWrite       Escrita de valor através do scan do driver de protocolo (assincrona).)
  @value(tcRead            Leitura de valor direta (sincrona).)
  @value(tcWrite           Escrita de valor direta (sincrona).)
  @value(tcInternalUpdate  Comando interno de atualização.)
  }
  {$ELSE}
  {:
  Enumerates all commands accept by the tag.

  @value(tcScanRead        Values are read using the driver scan.)
  @value(tcScanWrite       Values are write using the driver scan.)
  @value(tcRead            Values are read synchronous (without driver scan).)
  @value(tcWrite           Values are write synchronous (without driver scan).)
  @value(tcInternalUpdate  Internal tag update command.)
  }
  {$ENDIF}
  TTagCommand = (tcScanRead, tcScanWrite, tcRead, tcWrite, tcInternalUpdate);

  {$IFDEF PORTUGUES}
  {:
  Enumera todos os possíveis resultados de um pedido de leitura/escrita de um
  tag para um driver de protocolo (TProtocolDriver).

  @value(ioDriverError            Erro interno do driver.)
  @value(ioCommError              Erro de comunicação.)
  @value(ioOk                     Comando com sucesso.)
  @value(ioTimeout                Timeout de comunicação.)
  @value(ioIllegalFunction        Função de IO inválida.)
  @value(ioIllegalRegAddress      O endereco da memória é inválido.)
  @value(ioIllegalValue           O valor é inválido.)
  @value(ioPLCError               Erro no equipamento.)
  @value(ioTagError               Erro interno do Tag.)
  @value(ioNullDriver             Tag SEM DRIVER.)
  @value(ioIllegalStationAddress  Endereço do equipamento é inválido.)
  @value(ioIllegalRequest         Requisição inválida ou não suportada.)
  @value(ioObjectNotExists        O objeto requisitado não existe.)
  @value(ioIllegalMemoryAddress   O inicio ou o fim da requisição estão fora do espaço de endereços do equipamento.)
  @value(ioUnknownError           Um código de erro desconhecido foi retornado.)
  @value(ioEmptyPacket            Um pacote vazio (sem dados) foi retornado.)
  @value(ioPartialOk              Uma ação teve sucesso parcial.)
  }
  {$ELSE}
  {:
  Enumerates all results that can be returned by the protocol driver to a
  read/write request of a tag.

  @value(ioDriverError            Internal driver error.)
  @value(ioCommError              Communication error.)
  @value(ioOk                     Sucessfull request.)
  @value(ioTimeout                Communication timeout.)
  @value(ioIllegalFunction        Invalid IO function.)
  @value(ioIllegalRegAddress      Invalid memory address.)
  @value(ioIllegalValue           Invalid value.)
  @value(ioPLCError               Device error.)
  @value(ioTagError               Internal tag error.)
  @value(ioNullDriver             Tag without a driver.)
  @value(ioIllegalStationAddress  Invalid device address.)
  @value(ioIllegalRequest         The request is invalid or not supported.)
  @value(ioObjectNotExists        The requested object doesn't exists.)
  @value(ioIllegalMemoryAddress   The request is out of bound of the memory space of device.)
  @value(ioUnknownError           A invalid error code was returned.)
  @value(ioEmptyPacket            A empty packet was returned.)
  @value(ioPartialOk              An action was partially successful.)
  }
  {$ENDIF}
  TProtocolIOResult = (ioNone, ioDriverError, ioCommError, ioOk, ioTimeOut,
                       ioIllegalFunction, ioIllegalRegAddress,ioIllegalValue,
                       ioPLCError, ioTagError, ioNullDriver, ioIllegalRequest,
                       ioIllegalStationAddress, ioObjectNotExists,
                       ioIllegalMemoryAddress, ioUnknownError, ioEmptyPacket,
                       ioPartialOk);

  {$IFDEF PORTUGUES}
  {:
  Callback chamado pelo driver de protocolo (TProtocolDriver) para retornar o
  resultado de uma solicitação e os respectivos valores.
  @param(Values TArrayOfDouble: Array com os valores lidos/escritos.)
  @param(ValuesTimeStamp TDateTime: Data/Hora em que esses valores foram lidos/escritos.)
  @param(TagCommand TTagCommand: Tipo de comando.)
  @param(LastResult TProtocolIOResult: Resultado do driver ao processar o pedido.)
  @param(Offset Cardinal: Posição dentro do bloco onde os valores começam.)
  }
  {$ELSE}
  {:
  Callback called by the protocol driver (TProtocolDriver) to return the result
  of an request and theirs values.
  @param(Values TArrayOfDouble: Array with the values read/written.)
  @param(ValuesTimeStamp TDateTime: Date/Time when these values are read/written.)
  @param(TagCommand TTagCommand: Command type.)
  @param(LastResult TProtocolIOResult: I/O result after process this request.)
  @param(Offset Cardinal: Block Offset.)
  }
  {$ENDIF}
  TTagCommandCallBack = procedure(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:Integer) of object;

  {$IFDEF PORTUGUES}
  {:
  Estrutura usada internamente pelos drivers de protocolo (TProtocolDriver) para
  realizar leituras e escritas por Scan. Representa a configuração do tag que
  está sendo tratado.

  @member Hack Valor da propriedade PLCHack.
  @member Slot Valor da propriedade PLCSlot.
  @member Station Valor da propriedade PLCStation.
  @member File_DB Valor da propriedade MemFile_DB.
  @member Address Valor da propriedade MemAddress.
  @member SubElement Valor da propriedade MemSubElement.
  @member Size Valor da propriedade Size (Tags Blocos).
  @member OffSet Indice do bloco (Tags Blocos).
  @member Path Valor da propriedade LongAddress.
  @member ReadFunction Valor da propriedade MemReadFunction.
  @member WriteFunction Valor da propriedade MemWriteFunction.
  @member Retries Valor da propriedade Retries.
  @member ScanTime Valor da propriedade RefreshTime.
  @member CallBack Procedimento que será chamado quando o comando for completado.
  }
  {$ELSE}
  {:
  Struture used internaly by the protocolo driver (TProtocolDriver) to process
  read and write requests. Represents the configuration of the tag being
  processed.

  @member Rack Value of PLCRack property.
  @member Slot Value of PLCSlot property.
  @member Station Value of PLCStation property.
  @member File_DB Value of MemFile_DB property.
  @member Address Value of MemAddress property.
  @member SubElement Value of MemSubElement property.
  @member Size Value of Size (Block tags) property.
  @member OffSet Index inside the block (Block Tags).
  @member Path Value of LongAddress property.
  @member ReadFunction Value of MemReadFunction property.
  @member WriteFunction Value of MemWriteFunction property.
  @member Retries Value of Retries property.
  @member UpdateTime Value of UpdateTime property.
  @member CallBack Procedure called when the request is done to return the data of the request.
  }
  {$ENDIF}
  TTagRec = record
    Rack:Integer;
    Slot:Integer;
    Station:Integer;
    File_DB:Integer;
    Address:Integer;
    SubElement:Integer;
    Size:Integer;
    OffSet:Integer;
    RealOffset:Integer;
    Path:String;
    ReadFunction:Integer;
    WriteFunction:Integer;
    Retries:Integer;
    UpdateTime:Integer;
    CallBack:TTagCommandCallBack;
  end;

  {$IFDEF PORTUGUES}
  //: Aponta para uma estrutura de Tag.
  {$ELSE}
  //: Points to a tag structure
  {$ENDIF}
  PTagRec = ^TTagRec;

  {$IFDEF PORTUGUES}
  {:
  Interface de notificação de eventos do tag. Usado para notificar os controles
  ligados ao tag sobre o que ocorre com o tag.
  }
  {$ELSE}
  {:
  Tag events notification interface. Used to notifies the linked controls about
  any event that occurs with the tag.
  }
  {$ENDIF}
  IHMITagInterface = interface
    ['{4301B240-79D9-41F9-A814-68CFEFD032B8}']
    {$IFDEF PORTUGUES}
    //: Chama o evento quando o tag tem uma letura com exito.
    {$ELSE}
    //: Called when the tag has a successful read.
    {$ENDIF}
    procedure NotifyReadOk;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma leitura falha.
    {$ELSE}
    //: Called when the tag has a fault read.
    {$ENDIF}
    procedure NotifyReadFault;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma escrita tem sucesso.
    {$ELSE}
    //: Called when the tag has a successful write.
    {$ENDIF}
    procedure NotifyWriteOk;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma escrita do tag falha.
    {$ELSE}
    //: Called when the tag has a fault write.
    {$ENDIF}
    procedure NotifyWriteFault;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando o valor do tag muda.
    {$ELSE}
    //: Called when the tag value changes.
    {$ENDIF}
    procedure NotifyTagChange(Sender:TObject);

    {$IFDEF PORTUGUES}
    //: Notifica o objeto dependente que o tag será removido.
    {$ELSE}
    //: Notifies the control about the destruction of the tag.
    {$ENDIF}
    procedure RemoveTag(Sender:TObject);
  end;

  {$IFDEF PORTUGUES}
  //: Interface de gerenciamento de identificação do tag.
  {$ELSE}
  //: Interface of management of unique tag identification.
  {$ENDIF}
  IManagedTagInterface = interface
    ['{5CC728FD-B75F-475E-BDE7-07A862B6B2B6}']
    {$IFDEF PORTUGUES}
    //: Ordena o tag refazer sua identificação a fim de torná-la única.
    {$ELSE}
    //: Forces the tag to rebuild their identification to make it unique.
    {$ENDIF}
    procedure RebuildTagGUID;
  end;

  {$IFDEF PORTUGUES}
  {:
  Interface de atualização do tag. Interface usada pelo driver de protocolo
  para saber quando o tag deve ser atualizado.
  }
  {$ELSE}
  {:
  Tag scan interface. Used by the protocol driver to know when a tag must be
  updated.
  }
  {$ENDIF}
  IScanableTagInterface = interface
    ['{6D57805C-D779-4607-BDA5-DF8A68F49973}']
    {$IFDEF PORTUGUES}
    //: Informa quanto tempo se passou desde a última atualização do valor do tag.
    {$ELSE}
    //: Tells how many time has elapsed from the last update of tag value.
    {$ENDIF}
    function RemainingMiliseconds:Int64;

    {$IFDEF PORTUGUES}
    //: Informa quanto tempo se passou desde a última verificação do tag.
    {$ELSE}
    //: Tells how many time has elapsed from the last scan of tag.
    {$ENDIF}
    function RemainingMilisecondsForNextScan:Int64;

    {$IFDEF PORTUGUES}
    //: Diz se o tag está configurado corretamente.
    {$ELSE}
    //: Tells if the tag is set properly.
    {$ENDIF}
    function IsValidTag:Boolean;

    {$IFDEF PORTUGUES}
    //: Informa se o callback pertence ao tag.
    {$ELSE}
    //: Tells if callback belongs to the tag
    {$ENDIF}
    function IsMyCallBack(Cback:TTagCommandCallBack):Boolean;

    {$IFDEF PORTUGUES}
    //: Seta o tag como válido ou não.
    {$ELSE}
    //: Sets the tag as valid or not.
    {$ENDIF}
    procedure SetTagValidity(TagValidity:Boolean);

    {$IFDEF PORTUGUES}
    //: Obtem a estrutura com as informações do tag.
    {$ELSE}
    //: Gets a structure with informations about the tag.
    {$ENDIF}
    procedure BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);
  end;

  TASyncValueChangeNotify = procedure(Sender:TObject; const Value:TArrayOfDouble) of object;
  TASyncStringValueChange = procedure(Sender:TObject; const Value:String) of object;

  {$IFNDEF FPC}
  TLMessage = TMessage;
  PtrInt = Integer;
  {$ENDIF}

  {$IFDEF PORTUGUES}
  //: Classe base para todos os tags.
  {$ELSE}
  //: Base class for all tags.
  {$ENDIF}
  TTag = class(TComponent)
  private
    {$IFDEF FPC}
      {$IFDEF CONSOLEPASCALSCADA}
        FUserData:Pointer;
      {$ENDIF}
    {$ENDIF}

    {$IFNDEF FPC}
      fHandle:HWND;
         {$IFDEF PORTUGUES}
         //: Processa as mensagens do tag.
         {$ELSE}
         //: Processes the tag messages.
         {$ENDIF}
      procedure wndMethod(var Msg:TLMessage); virtual;
    {$ELSE}
      {$IFNDEF CONSOLEPASCALSCADA}
        procedure ASyncMethod(Data: PtrInt); virtual;
      {$ELSE}
        procedure ASyncMethod(); virtual;
      {$ENDIF}
    {$ENDIF}
  protected
    {$IFDEF PORTUGUES}
    //: Realiza a chamado do evento assincrono informando a mudança do valor do tag.
    {$ELSE}
    //: Call the assynchronous tag value change.
    {$ENDIF}    
    procedure AsyncNotifyChange(data:Pointer); virtual;

    {$IFDEF PORTUGUES}
    //: Retorna uma cópia do valor do tag para que ele não se perca entre as chamadas de evento assincronas.
    {$ELSE}
    //: Return a copy of the Tag value for each assynchronous event calls.
    {$ENDIF} 
    function  GetValueChangeData:Pointer; virtual;

    {$IFDEF PORTUGUES}
    //: Libera a cópia do valor do tag de uma determinada chamada asincrona.
    {$ELSE}
    //: Release a copy of a tag value of one assynchronous event call.
    {$ENDIF} 
    procedure ReleaseChangeData(data:Pointer); virtual;
  protected
    {$IFDEF PORTUGUES}
    //: Booleano que armazena se o tag vai ser lido automaticamente.
    {$ELSE}
    //: Stores if the tag will be updated automatically.
    {$ENDIF}
    PAutoRead:Boolean;

    {$IFDEF PORTUGUES}
    //: Booleano que armazena se o tag vai ter seu valor escrito automaticamente.
    {$ELSE}
    //: Stores if the tag will write their value automatically.
    {$ENDIF}
    PAutoWrite:Boolean;

    {$IFDEF PORTUGUES}
    //: Armazena a contagem de erros de leitura.
    {$ELSE}
    //: Stores the counter of failed reads.
    {$ENDIF}
    PCommReadErrors:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena a contagem de leituras bem sucedidas.
    {$ELSE}
    //: Stores the counter of successful reads.
    {$ENDIF}
    PCommReadOK:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena a contagem de erros de escrita.
    {$ELSE}
    //: Stores the counter of failed writes.
    {$ENDIF}
    PCommWriteErrors:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena a contagem de escritas bem sucedidas.
    {$ELSE}
    //: Stores the counter of successful writes.
    {$ENDIF}
    PCommWriteOk:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o Rack do equipamento da memória que está sendo mapeada.
    {$ELSE}
    //: Stores the device Rack number of the tag.
    {$ENDIF}
    PRack:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o Slot do equipamento da memória que está sendo mapeada.
    {$ELSE}
    //: Stores the device Slot number of the tag.
    {$ENDIF}
    PSlot:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o endereço da estação da memória que está sendo mapeada.
    {$ELSE}
    //: Stores the device Station address of the tag.
    {$ENDIF}
    PStation:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o Arquivo/DB dentro do equipamento da memória que está sendo mapeada.
    {$ELSE}
    //: Stores the File/DB number of the tag.
    {$ENDIF}
    PFile_DB:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o endereço da memória no equipamento que está sendo mapeada.
    {$ELSE}
    //: Stores the device memory address of the tag.
    {$ENDIF}
    PAddress:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o subendereço da memória no equipamento que está sendo mapeada.
    {$ELSE}
    //: Stores the device sub-memory address of the tag.
    {$ENDIF}
    PSubElement:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o número de memórias que estão mapeadas.
    {$ELSE}
    //: Stores how many memories are being mapped.
    {$ENDIF}
    PSize:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o endereço completo da memória em formato texto.
    {$ELSE}
    //: Stores the textual memory address of the tag.
    {$ENDIF}
    PPath:String;

    {$IFDEF PORTUGUES}
    //: Armazena a função usada para leitura da memória.
    {$ELSE}
    //: Stores the function code to read the values of the tag.
    {$ENDIF}
    PReadFunction:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena a função usada para escrita da memória.
    {$ELSE}
    //: Stores the function code to write the values of the tag.
    {$ENDIF}
    PWriteFunction:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o número de tentivas de leitura/escrita da memória.
    {$ELSE}
    //: Stores the
    {$ENDIF}
    PRetries:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o tempo de atualização do tag.
    {$ELSE}
    //: Stores the update time of the tag.
    {$ENDIF}
    PUpdateTime:TRefreshTime;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo quando uma leitura do tag tem sucesso.
    {$ELSE}
    //: Stores the event to be called when a read has success.
    {$ENDIF}
    POnReadOk:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag quando uma leitura do tag falha.
    {$ELSE}
    //: Stores the event to be called when a read fail occurs.
    {$ENDIF}
    POnReadFail:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag quando um valor é escrito com sucesso no dispositivo.
    {$ELSE}
    //: Stores the event to be called when a value is written successfully on device.
    {$ENDIF}
    POnWriteOk:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag quando uma escrita de valores do tag no dispositivo falha.
    {$ELSE}
    //: Stores the event called when a write of tag value has a failed.
    {$ENDIF}
    POnWriteFail:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag quando o valor da tag se altera, ANTES de notificar seus dependetes.
    {$ELSE}
    //: Stores the event called when the tag value changes, BEFORE notify the dependents of the tag.
    {$ENDIF}
    POnValueChangeFirst:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag quando o valor da tag se altera, APÓS notificar seus dependetes.
    {$ELSE}
    //: Stores the event called when the tag value changes, AFTER notify the dependents of the tag.
    {$ENDIF}
    POnValueChangeLast:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena o evento assincrono que notifica uma mudança de valor do tag.
    {$ELSE}
    //: Stores asynchronous event that notifies when the tag value changes.
    {$ENDIF}
    POnAsyncValueChange:TASyncValueChangeNotify;

    {$IFDEF PORTUGUES}
    //: Armazena o evento chamado pelo tag toda vez que ele for atualizado.
    {$ELSE}
    //: Stores the event called when the tag value was updated.
    {$ENDIF}
    POnUpdate:TNotifyEvent;

    {$IFDEF PORTUGUES}
    //: Armazena a interface de notificação de eventos de todos os objetos dependentes.
    {$ELSE}
    //: Stores the notification event interface of all dependent objects.
    {$ENDIF}
    PNotificationInterfaces:array of IHMITagInterface;

    {$IFDEF PORTUGUES}
    //: Conta os objetos que dependem desse tag.
    {$ELSE}
    //: Number of dependent objects.
    {$ENDIF}
    PNotificationInterfacesCount:integer;

    {$IFDEF PORTUGUES}
    //: Armazena o identificador desse tag. GUID
    {$ELSE}
    //: Stores the unique tag identification.
    {$ENDIF}
    PGUID:String;

    {$IFDEF PORTUGUES}
    //: Notifica quando uma letura tem exito.
    {$ELSE}
    //: Notifies when a successful read occurs.
    {$ENDIF}
    procedure NotifyReadOk;

    {$IFDEF PORTUGUES}
    //: Notifica quando uma falha de leitura.
    {$ELSE}
    //: Notifies when a read fault occurs.
    {$ENDIF}
    procedure NotifyReadFault;

    {$IFDEF PORTUGUES}
    //: Notifica quando o valor do tag é atualizado.
    {$ELSE}
    //: Notifies when the tag value was updated.
    {$ENDIF}
    procedure NotifyUpdate;

    {$IFDEF PORTUGUES}
    //: Notifica quando uma escrita tem sucesso.
    {$ELSE}
    //: Notifies when a successful write occurs.
    {$ENDIF}
    procedure NotifyWriteOk;

    {$IFDEF PORTUGUES}
    //: Notifica quando uma falha de escrita ocorre.
    {$ELSE}
    //: Notifies when a write fault occurs.
    {$ENDIF}
    procedure NotifyWriteFault;

    {$IFDEF PORTUGUES}
    //: Notifica quando o valor do tag muda.
    {$ELSE}
    //: Notifies when the tag value changes.
    {$ENDIF}
    procedure NotifyChange;


    {$IFDEF PORTUGUES}
    //: Incrementa o contador de leituras com sucesso.
    {$ELSE}
    //: Increments the counter of successful reads.
    {$ENDIF}
    procedure IncCommReadOK(value:Cardinal);

    {$IFDEF PORTUGUES}
    //: Incrementa o contador de leituras com falha do tag.
    {$ELSE}
    //: Increments the counter of faulted reads.
    {$ENDIF}
    procedure IncCommReadFaults(value:Cardinal);

    {$IFDEF PORTUGUES}
    //: Incrementa o contador de escritas com exito do tag.
    {$ELSE}
    //: Increments the counter of successful writes.
    {$ENDIF}
    procedure IncCommWriteOK(value:Cardinal);

    {$IFDEF PORTUGUES}
    //: Incrementa o contador de falhas de escrita do tag.
    {$ELSE}
    //: Increments the counter of faulted writes.
    {$ENDIF}
    procedure IncCommWriteFaults(value:Cardinal);

    {$IFDEF PORTUGUES}
    //: Caso @true, o tag será lido automaticamente.
    {$ELSE}
    //: If @true, the tag will be updated automaticaly.
    {$ENDIF}
    property AutoRead:Boolean read PAutoRead;

    {$IFDEF PORTUGUES}
    {:
    Caso @true, toda a vez que ocorrerem escritas no tag,
    ele irá escrever o valor no equipamento.
    }
    {$ELSE}
    {:
    If @true, all values written on tags will be automaticaly written on the
    the memory of the linked device.
    }
    {$ENDIF}
    property AutoWrite:Boolean read PAutoWrite;

    {$IFDEF PORTUGUES}
    //: Informa o total de erros de leitura do tag.
    {$ELSE}
    //: Tell how many read errors occurred.
    {$ENDIF}
    property CommReadErrors:Cardinal read PCommReadErrors;

    {$IFDEF PORTUGUES}
    //: Informa o total de leituras com exito do tag.
    {$ELSE}
    //: Tells the count of successful reads.
    {$ENDIF}
    property CommReadsOK:Cardinal read PCommReadOK;

    {$IFDEF PORTUGUES}
    //: Informa o total de erros de escrita do tag.
    {$ELSE}
    //: Tell how many write errors occurred.
    {$ENDIF}
    property CommWriteErrors:Cardinal read PCommWriteErrors;

    {$IFDEF PORTUGUES}
    //: Informa o total de escritas com exito do tag.
    {$ELSE}
    //: Tells the count of successful writes.
    {$ENDIF}
    property CommWritesOk:Cardinal read PCommWriteOk;

    {$IFDEF PORTUGUES}
    //: Rack do equipamento que contem a memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: Device Rack that contains the memory being mapped, if applicable.
    {$ENDIF}
    property PLCHack:Cardinal read PRack stored false;

    {$IFDEF PORTUGUES}
    //: Rack do equipamento que contem a memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: Device Rack that contains the memory being mapped, if applicable.
    {$ENDIF}
    property PLCRack:Cardinal read PRack;

    {$IFDEF PORTUGUES}
    //: Slot do equipamento que contem a memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: Device Slot that contains the memory being mapped, if applicable.
    {$ENDIF}
    property PLCSlot:Cardinal read PSlot;

    {$IFDEF PORTUGUES}
    //: Endereço da estação que contem a memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: Device address that contains the memory being mapped, if applicable.
    {$ENDIF}
    property PLCStation:Cardinal read PStation;

    {$IFDEF PORTUGUES}
     //: Arquivo/DB dentro do equipamento que contem a memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: Device File/DB that contains the memory being mapped, if applicable.
    {$ENDIF}
    property MemFile_DB:Cardinal read PFile_DB;

    {$IFDEF PORTUGUES}
    //: Endereço da memória que está sendo mapeada.
    {$ELSE}
    //: The address of the memory being mapped.
    {$ENDIF}
    property MemAddress:Cardinal read PAddress;

    {$IFDEF PORTUGUES}
    //: Subendereço da memória que está sendo mapeada, se aplicável.
    {$ELSE}
    //: The sub-memory address, if applicable.
    {$ENDIF}
    property MemSubElement:Cardinal read PSubElement;

    {$IFDEF PORTUGUES}
    //: Função do driver responsável por realizar a leitura dessa memória.
    {$ELSE}
    //: Protocol driver function that will read the memory being mapped.
    {$ENDIF}
    property MemReadFunction:Cardinal read PReadFunction;

    {$IFDEF PORTUGUES}
    //: Função do driver responsável por realizar a escrita de valores dessa memória.
    {$ELSE}
    //: Protocol driver function that will write values in the memory being mapped.
    {$ENDIF}
    property MemWriteFunction:Cardinal read PWriteFunction;

    {$IFDEF PORTUGUES}
    //: Número tentivas de leitura/escrita dessa memória.
    {$ELSE}
    //: Number of retries of read/write of this memory.
    {$ENDIF}
    property Retries:Cardinal read PRetries;

    {$IFDEF PORTUGUES}
    //: Tempo de atualização do tag, em milisegundos.
    {$ELSE}
    //: Update time of the tag, in milliseconds.
    {$ENDIF}
    property RefreshTime:TRefreshTime read PUpdateTime stored false;

    {$IFDEF PORTUGUES}
    //: Tempo de atualização do tag, em milisegundos.
    {$ELSE}
    //: Update time of the tag, in milliseconds.
    {$ENDIF}
    property UpdateTime:TRefreshTime read PUpdateTime;

    {$IFDEF PORTUGUES}
    //: Número de memórias que serão mapeadas, se aplicável.
    {$ELSE}
    //: Number of memories being mapped, if applicable.
    {$ENDIF}
    property Size:Cardinal read PSize;

    {$IFDEF PORTUGUES}
    //: Endereço longo (texto), se suportado pelo driver de protocolo.
    {$ELSE}
    //: Long address (text), if supported by the protocol driver.
    {$ENDIF}
    property LongAddress:String read PPath;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando uma leitura do tag tem exito.
    {$ELSE}
    //: Event called to notify when a successful read occurs.
    {$ENDIF}
    property OnReadOK:TNotifyEvent      read POnReadOk       write POnReadOk;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando uma leitura do tag falha.
    {$ELSE}
    //: Event called when a read fault occurs.
    {$ENDIF}
    property OnReadFail:TNotifyEvent    read POnReadFail     write POnReadFail;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando uma escrita de valor do tag tem exito.
    {$ELSE}
    //: Event called to notify when a write of the tag value has success.
    {$ENDIF}
    property OnWriteOk:TNotifyEvent     read POnWriteOk      write POnWriteOk;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando uma escrita do tag falha.
    {$ELSE}
    //: Event called when a write fault occurs.
    {$ENDIF}
    property OnWriteFail:TNotifyEvent   read POnWriteFail    write POnWriteFail;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando o valor do tag sofre alguma mudança, após notificar todos os dependentes do tag.
    {$ELSE}
    //: Event called when the tag value changes, AFTER notify all tag dependents.
    {$ENDIF}
    property OnValueChange:TNotifyEvent read POnValueChangeLast  write POnValueChangeLast stored false;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando o valor do tag sofre alguma mudança, APÓS notificar todos os dependentes do tag.
    {$ELSE}
    //: Event called when the tag value changes, AFTER notify all tag dependents.
    {$ENDIF}
    property OnValueChangeLast:TNotifyEvent read POnValueChangeLast  write POnValueChangeLast;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando o valor do tag sofre alguma mudança, ANTES DE notificar todos os dependentes do tag.
    {$ELSE}
    //: Event called when the tag value changes, BEFORE notify all tag dependents.
    {$ENDIF}
    property OnValueChangeFirst:TNotifyEvent read POnValueChangeFirst  write POnValueChangeFirst;

    {$IFDEF PORTUGUES}
    //: Evento chamado quando o valor do tag é atualizado.
    {$ELSE}
    //: Event called when the tag value was updated.
    {$ENDIF}
    property OnUpdate:TNotifyEvent read POnUpdate  write POnUpdate;

    {$IFDEF PORTUGUES}
    //: Evento assincrono chamado quando o valor do tag sofre uma alteração.
    {$ELSE}
    //: Asynchronous event called when the tag value changes.
    {$ENDIF}
    property OnAsyncValueChange:TASyncValueChangeNotify read POnAsyncValueChange  write POnAsyncValueChange;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Adiciona uma nova interface para ser notificada dos eventos do tag.
    {$ELSE}
    //: Adds a new notification interface to the tag notification list.
    {$ENDIF}
    procedure AddCallBacks(ITag:IHMITagInterface);

    {$IFDEF PORTUGUES}
    //: Remove uma interface da lista de notificações do tag.
    {$ELSE}
    //: Remove a interface from the tag notification list.
    {$ENDIF}
    procedure RemoveCallBacks(ITag:IHMITagInterface);

    {$IFNDEF FPC}
    {$IFDEF PORTUGUES}
    //: Retorna o manipulador do tag.
    {$ELSE}
    //: Returns the tag handle.
    {$ENDIF}
    property Handle:HWND read fHandle;
    {$ENDIF}
  end;

  TScanUpdateRec = record
    Owner:TTag;
    CallBack:TTagCommandCallBack;
    ValueTimeStamp:TDateTime;
    LastResult:TProtocolIOResult;
    Values:TArrayOfDouble;
  end;

  TArrayOfScanUpdateRec = array of TScanUpdateRec;


implementation

uses hsstrings;


constructor TTag.Create(AOwner:TComponent);
var
  x:TGuid;
begin
  inherited Create(AOwner);
  PNotificationInterfacesCount := 0;
  PCommReadErrors := 0;
  PCommReadOK := 0;
  PCommWriteErrors := 0;
  PCommWriteOk := 0;
  PUpdateTime:=1000;

  {$IFNDEF FPC}
  fHandle:=AllocateHWnd(WndMethod);
  {$ENDIF}

  if ComponentState*[csReading, csLoading]=[] then begin
    CreateGUID(x);
    PGUID:=GUIDToString(x);
  end;
end;

destructor TTag.Destroy;
var
  c:Integer;
begin
  for c := 0 to High(PNotificationInterfaces) do
    PNotificationInterfaces[c].RemoveTag(Self);
  {$IFNDEF FPC}
  DeallocateHWnd(fHandle);
  {$ENDIF}
  inherited Destroy;
end;

procedure TTag.AddCallBacks(ITag:IHMITagInterface);
begin
  if (ITag<>nil) and ((ITag as IHMITagInterface)=nil) then
    raise Exception.Create(SinvalidInterface);
  
  inc(PNotificationInterfacesCount);
  SetLength(PNotificationInterfaces, PNotificationInterfacesCount);
  PNotificationInterfaces[PNotificationInterfacesCount-1]:=ITag;
end;

procedure TTag.RemoveCallBacks(ITag:IHMITagInterface);
var
  c,h:Integer;
  found:Boolean;
begin
  found:=false;
  h := High(PNotificationInterfaces);
  for c:=0 to h do
    if (ITag)=(PNotificationInterfaces[c]) then begin
      found := true;
      break;
    end;
  if found then begin
    PNotificationInterfaces[c] := PNotificationInterfaces[h];
    dec(PNotificationInterfacesCount);
    SetLength(PNotificationInterfaces, PNotificationInterfacesCount);
  end;
end;

procedure TTag.NotifyChange;
var
  c:Integer;
{$IFNDEF CONSOLEPASCALSCADA}
  x:Pointer;
{$ENDIF}
begin
  //notifica a mudanca antes de notificar os
  //demais controles.
  //
  // Notify the change before notify the dependent objects.
  try
    if Assigned(POnValueChangeFirst) then
      POnValueChangeFirst(Self);
  except
  end;

  //notifica controles e objetos dependentes
  //
  //Notify the dependent objects.
  for c:=0 to High(PNotificationInterfaces) do
    try
      PNotificationInterfaces[c].NotifyTagChange(self);
    except
    end;

  //notificação de mudanca após notificar os controles.
  //
  //Notify the change after notify the dependent objects.
  try
    if Assigned(POnValueChangeLast) then
      POnValueChangeLast(Self);
  except
  end;

  {$IFNDEF CONSOLEPASCALSCADA}
    x:=GetValueChangeData;
  {$ELSE}
    FUserData:=GetValueChangeData;
  {$ENDIF}


  {$IFDEF FPC}

    {$IFNDEF CONSOLEPASCALSCADA}
      Application.QueueAsyncCall(ASyncMethod,PtrInt(x));
    {$ELSE}
      TThread.Queue(nil, ASyncMethod);
    {$ENDIF}

  {$ELSE}
  PostMessage(fHandle,PM_ASYNCVALUECHANGE,PtrInt(x),0);
  {$ENDIF}
end;

{$IFNDEF FPC}
procedure TTag.wndMethod(var Msg: TLMessage);
var
  handled:Boolean;
  FData:Pointer;
begin
  handled:=true;
  case Msg.msg of
    PM_ASYNCVALUECHANGE: begin
      FData:=Pointer(Msg.wParam);
      AsyncNotifyChange(FData);
      ReleaseChangeData(FData);
    end
    else
      handled:=false;
  end;

  if handled then
    msg.Result:=0
  else
    inherited Dispatch(Msg);
end;
{$ELSE}
  {$IFNDEF CONSOLEPASCALSCADA}
    procedure TTag.ASyncMethod(Data: PtrInt);
    begin
      AsyncNotifyChange(Pointer(Data));
      ReleaseChangeData(Pointer(Data));
    end;
  {$ELSE}
    procedure TTag.ASyncMethod();
    begin
      AsyncNotifyChange(Pointer(FUserData));
      ReleaseChangeData(Pointer(FUserData));
    end;
  {$ENDIF}

{$ENDIF}

procedure TTag.AsyncNotifyChange(data:Pointer);
begin
  //does nothing.
end;

function TTag.GetValueChangeData: Pointer;
begin
  Result:=nil;
end;

procedure TTag.ReleaseChangeData(data: Pointer);
begin
  //does nothing.
end;

procedure TTag.NotifyReadOk;
var
  c:Integer;
begin
  for c:=0 to High(PNotificationInterfaces) do
    try
      PNotificationInterfaces[c].NotifyReadOk;
    except
    end;

  if Assigned(POnReadOk) then
    POnReadOk(self)
end;

procedure TTag.NotifyReadFault;
var
  c:Integer;
begin
  for c:=0 to High(PNotificationInterfaces) do
    try
      PNotificationInterfaces[c].NotifyReadFault;
    except
    end;

  if Assigned(POnReadFail) then
    POnReadFail(self)
end;

procedure TTag.NotifyUpdate;
begin
  try
    if Assigned(POnUpdate) then
      POnUpdate(self)
  finally
  end;
end;

procedure TTag.NotifyWriteOk;
var
  c:Integer;
begin
  for c:=0 to High(PNotificationInterfaces) do
    try
      PNotificationInterfaces[c].NotifyWriteOk;
    except
    end;

  if Assigned(POnWriteOk) then
    POnWriteOk(self)
end;

procedure TTag.NotifyWriteFault;
var
  c:Integer;
begin
  for c:=0 to High(PNotificationInterfaces) do
    try
      PNotificationInterfaces[c].NotifyWriteFault;
    except
    end;

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