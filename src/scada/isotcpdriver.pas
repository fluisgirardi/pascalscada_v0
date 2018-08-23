{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the ISOTCP protocol.)
  This driver is based on ISOTCP of LibNODAVE library of
  Thomas Hergenhahn (thomas.hergenhahn@web.de).

  This driver does not uses LibNodave, it's a rewritten of it.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ISOTCPDriver;

interface

uses
  classes, sysutils, S7Types, commtypes, s7family, Tag, ProtocolTypes;

type

  {$IFDEF PORTUGUES}
  {: Driver IsoTCP. Baseado na biblioteca LibNodave de
     Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Para endereçar uma memória, veja a documentação da classe
  TSiemensProtocolFamily.

  @bold(Devido ao ISOTCP permitir conectar a somente um CLP atraves de uma
        conexao TCP/IP, as propriedades TTag.PLCStation, TTag.PLCSlot e
        TTag.PLCRack dos tags nao tem efeito. Portanto estas informacoes devem
        ser configuradas atraves das propriedades PLCStation, PLCSlot e PLCRack
        de cada instancia deste protocolo.)

  @seealso(TSiemensProtocolFamily).
  }
  {$ELSE}
  {: ISOTCP protocol driver. Based on LibNODAVE libray of
     Thomas Hergenhahn (thomas.hergenhahn@web.de).

  To address your tags, see the documentation of the class
  TSiemensProtocolFamily.

  @bold(Due to ISOTCP allow connect to a single PLC through a TCP/IP connection,
        the properties TTag.PLCStation, TTag.PLCSlot and TTag.PLCRack has no
        effect. Therefore these informations must be set through properties
        PLCStation, PLCSlot and PLCRack of each instance of this protocol.)

  @seealso(TSiemensProtocolFamily).
  }
  {$ENDIF}

  { TISOTCPDriver }

  TISOTCPDriver = class(TSiemensProtocolFamily)
  private
    procedure SetPLCRack(AValue: longint);
    procedure SetPLCSlot(AValue: LongInt);
    procedure SetPLCStation(AValue: LongInt);
    procedure UpdatePLCs;
  protected
    FPLCRack,
    FPLCSlot,
    FPLCStation:LongInt;
    FConnectionWay:TISOTCPConnectionWay;

    {$IFDEF PORTUGUES}
    //: Define o meio de conexão com o CLP.
    {$ELSE}
    //: Defines the way to connect into the PLC.
    {$ENDIF}
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);

    //: seealso(TSiemensProtocolFamily.GetTagInfo)
    function GetTagInfo(tagobj: TTag): TTagRec; override;

    //: seealso(TProtocolDriver.NotifyThisEvents)
    function NotifyThisEvents: TNotifyThisEvents; override;
    //: seealso(TProtocolDriver.PortClosed)
    procedure PortClosed(Sender: TObject); override;
    //: seealso(TProtocolDriver.PortDisconnected)
    procedure PortDisconnected(Sender: TObject); override;
  protected
    //: seealso(TSiemensProtocolFamily.connectPLC)
    function  connectPLC(var CPU:TS7CPU):Boolean; override;
    //: seealso(TSiemensProtocolFamily.exchange)
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; override;
    //: seealso(TSiemensProtocolFamily.getResponse)
    function  getResponse(var msgIn:BYTES; var BytesRead:LongInt):TIOResult; override;
    //: seealso(TSiemensProtocolFamily.PrepareToSend)
    procedure PrepareToSend(var msg: BYTES); override;
    //: @exclude
    procedure Loaded; override;

    //: seealso(TSiemensProtocolFamily.doRead)
    function DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
      Sync: Boolean): TProtocolIOResult; override;
    procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec); override;
    //: seealso(TSiemensProtocolFamily.doWrite)
    function DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble;
      Sync: Boolean): TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;

    {$IFDEF PORTUGUES}
    {:
    Atualiza de uma so vez, Rack, Slot e Station da lista de CPUs, evitando
    overhead.
    }
    {$ELSE}
    {:
    Updates in a single call, Rack, Slot and Station, avoiding overhead.
    }
    {$ENDIF}
    procedure UpdatePLCAddress(Rack, Slot, Station:LongInt);
  published
    //: @seealso(TSiemensProtocolFamily.ReadSomethingAlways)
    property ReadSomethingAlways;

    {$IFDEF PORTUGUES}
    {:
    Define o meio de conexão com o CLP.
    @seealso(TISOTCPConnectionWay)
    }
    {$ELSE}
    {:
    Defines the way to connect into the PLC.
    @seealso(TISOTCPConnectionWay)
    }
    {$ENDIF}
    property ConnectionWay:TISOTCPConnectionWay read FConnectionWay write SetISOConnectionWay;

    {$IFDEF PORTUGUES}
    {:
      Sobrescreve o valor da propriedade PLCRack do tag pelo valor configurado
      aqui.

      @bold(Devido ao ISOTCP permitir conectar a somente um CLP atraves de uma
            conexao TCP/IP, a propriedade TTag.PLCRack do tag nao tem efeito.
            Portanto esta informacao deve ser configurada atraves da
            propriedade PLCRack de cada instancia deste protocolo.)

      @seealso(TTag.PLCRack)
      @seealso(TISOTCPDriver)
    }
    {$ELSE}
    {:
      Override the value TTag.PLCRack property by the value set here.

      @bold(Due to ISOTCP allow connect to a single PLC through a TCP/IP
            connection, the property TTag.PLCRack has no effect. Therefore
            these information must be set through property PLCRack of each
            instance of this protocol.)

      @seealso(TTag.PLCRack)
      @seealso(TISOTCPDriver)
    }
    {$ENDIF}
    property PLCRack:longint    read FPLCRack    write SetPLCRack default 0;

    {$IFDEF PORTUGUES}
    {:
      Sobrescreve o valor da propriedade PLCSlot do tag pelo valor
      configurado aqui.

      @bold(Devido ao ISOTCP permitir conectar a somente um CLP atraves de uma
            conexao TCP/IP, a propriedade TTag.PLCSlot do tag nao tem efeito.
            Portanto esta informacao deve ser configurada atraves da
            propriedade PLCSlot de cada instancia deste protocolo.)

      @seealso(TTag.PLCSlot)
      @seealso(TISOTCPDriver)
    }
    {$ELSE}
    {:
      Override the value TTag.PLCSlot property by the value set here.

      @bold(Due to ISOTCP allow connect to a single PLC through a TCP/IP
            connection, the property TTag.PLCSlot has no effect. Therefore
            these information must be set through property PLCSlot of each
            instance of this protocol.)

      @seealso(TTag.PLCSlot)
      @seealso(TISOTCPDriver)
    }
    {$ENDIF}
    property PLCSlot:LongInt    read FPLCSlot    write SetPLCSlot default 0;

    {$IFDEF PORTUGUES}
    {:
      Sobrescreve o valor da propriedade PLCStation do tag pelo valor
      configurado aqui.

      @bold(Devido ao ISOTCP permitir conectar a somente um CLP atraves de uma
            conexao TCP/IP, a propriedade TTag.PLCStation do tag nao tem efeito.
            Portanto esta informacao deve ser configurada atraves da
            propriedade PLCStation de cada instancia deste protocolo.)

      @seealso(TTag.PLCStation)
      @seealso(TISOTCPDriver)
    }
    {$ELSE}
    {:
      Override the value TTag.PLCStation property by the value set here.

      @bold(Due to ISOTCP allow connect to a single PLC through a TCP/IP
            connection, the property TTag.PLCStation has no effect. Therefore
            these information must be set through property PLCStation of each
            instance of this protocol.)

      @seealso(TTag.PLCStation)
      @seealso(TISOTCPDriver)
    }
    {$ENDIF}
    property PLCStation:LongInt read FPLCStation write SetPLCStation default 2;

    property ReadOnly;
  end;

const
  ISOTCPMinPacketLen = 16;

implementation

uses math, pascalScadaMTPCPU;

constructor TISOTCPDriver.Create(AOwner:TComponent);
begin
  Inherited Create(AOwner);
  FPLCRack   :=0;
  FPLCSlot   :=0;
  FPLCStation:=2;

  PDUIncoming:=7;
  PDUOutgoing:=7;
end;

procedure TISOTCPDriver.UpdatePLCAddress(Rack, Slot, Station: LongInt);
begin
  FPLCRack   :=Rack;
  FPLCSlot   :=Slot;
  FPLCStation:=Station;
  UpdatePLCs;
end;

function TISOTCPDriver.connectPLC(var CPU: TS7CPU): Boolean;
var
  IOResult:TIOPacket;
  msg:BYTES;
  res:LongInt;
  len:Cardinal;
  retries:LongInt;
begin
  CPU.Connected:=false;
  Result:=false;
  if (PCommPort=nil) or (PCommPort.ReallyActive=false) then exit;

  //incializa conexao
  //
  //initiates the connection.
  SetLength(msg,22);
  msg[04] := $11;  // $11,
  msg[05] := $E0;  // $E0,
  msg[06] := 0;    // 0,
  msg[07] := 0;    // 0,
  msg[08] := 0;    // 0,
  msg[09] := 1;    // 1,
  msg[10] := 0;    // 0,
  msg[11] := $C1;  // $C1,
  msg[12] := 2;    // 2,
  msg[13] := ifthen(FConnectionWay=ISOTCP, 1, $4D);    //'M',
  msg[14] := ifthen(FConnectionWay=ISOTCP, 0, $57);    //'W',
  msg[15] := $C2;  // $C2,
  msg[16] := 2;
  msg[17] := ifthen(FConnectionWay=ISOTCP, 2{1=PG, 2=OP, 3=S7 Basic Comm?}, $4D);
  msg[18] := ifthen(FConnectionWay=ISOTCP, CPU.Slot or CPU.Rack shl 5,      $57);
  msg[19] := $C0;  // $C0,
  msg[20] := 1;    // 1,
  msg[21] := 9;    // 9;
  PrepareToSend(msg);

  try
    res := PCommPort.IOCommandSync(iocWriteRead,22,msg,4,DriverID,ifthen(FConnectionWay=ISOTCP_VIA_CP243,1000,0),@IOResult);
    if (res=0) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

    len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

    res := PCommPort.IOCommandSync(iocRead,0,nil,len-4,DriverID,0,@IOResult);
    if (res=0) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) then exit;

    retries := 1;
    while (len<>22) and (retries<3) do begin
      res := PCommPort.IOCommandSync(iocRead,0,nil,4,DriverID,0,@IOResult);
      if (res=0) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

      len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

      res := PCommPort.IOCommandSync(iocRead,0,nil,len-4,DriverID,0,@IOResult);
      if (res=0) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) then exit;
    end;

    //negocia o tamanho da pdu
    //
    //negotiates the PDU size
    if len=22 then
      CPU.Connected := NegotiatePDUSize(CPU);
  finally
    SetLength(msg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
    Result:=CPU.Connected;
  end;
end;

function TISOTCPDriver.exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean;
var
  res:LongInt;
  retries, BytesRead:LongInt;
  resget:TIOResult;
begin
  if (PCommPort=nil) or (PCommPort.ReallyActive=false) then exit;

  Result := Inherited exchange(CPU, msgOut, msgIn, IsWrite);
  Result := false;

  if Length(msgOut)<7 then
    SetLength(msgOut, 7);
  msgOut[04] := $02;
  msgOut[05] := $F0;
  msgOut[06] := $80;

  PrepareToSend(msgOut);

  HighLatencyOperationWillBegin(nil);
  try
    res:=PCommPort.IOCommandSync(iocWrite,Length(msgOut),msgOut,0,DriverID,0,nil);
    if res=0 then begin
      SetLength(msgIn,0);
      SetLength(msgOut,0);
      Result:=false;
      exit;
    end;
    retries:=0;

    BytesRead:=0;
    resget := getResponse(msgIn, BytesRead);
    while (resget<>iorOk) and (retries<3) do begin

      if resget<>iorTimeOut then
        Inc(retries)
      else
        Sleep(5);

      resget := getResponse(msgIn, BytesRead);
    end;

    Result:= BytesRead>ISOTCPMinPacketLen;
  finally
    HighLatencyOperationWasEnded(nil);
  end;
end;

function  TISOTCPDriver.getResponse(var msgIn:BYTES; var BytesRead:LongInt):TIOResult;
var
  res, len:LongInt;
  IOResult1, IOResult2:TIOPacket;
begin
  Result:=iorNotReady;

  try
    res := PCommPort.IOCommandSync(iocRead,0,nil,7,DriverID,0,@IOResult1);
    if (res=0) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;

    if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>7) then begin
      BytesRead:=IOResult1.Received;
      Result:=IOResult1.ReadIOResult;
      exit;
    end;

    len := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    //As vezes o CLP manda um pacote de
    //7 bytes que não serve para nada
    //ou se serve pra algo, eu não sei.
    //
    //Sometimes the PLC sends a useless
    //packet, with 7 bytes of len.
    while len = 7 do begin
      //le novamente...
      //reads again.
      res := PCommPort.IOCommandSync(iocRead,0,nil,7,DriverID,0,@IOResult1);
      if (res=0) then begin
        BytesRead:=0;
        Result:=iorNotReady;
        exit;
      end;

      if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>7) then begin
        BytesRead:=IOResult1.Received;
        Result:= IOResult1.ReadIOResult;
        exit;
      end;
      //calcula o tamanho do pacote recebido.
      //
      //calculate the size of the packet
      len:= IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    end;

    res := PCommPort.IOCommandSync(iocRead,0,nil,len-7,DriverID,0,@IOResult2);
    if (res=0) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;
    //se resultado nao der ok,
    //ou não fechar com o numero de bytes a ler
    //e não ter o comprimento minimo do ISOTCP sai.
    //
    //if the IO result aren't ok or the packet has less bytes than minimum size.
    //exit...
    if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>(len-7)) then begin
      BytesRead:=IOResult2.Received;
      Result:= IOResult2.ReadIOResult;
      exit;
    end;

    SetLength(msgIn,IOResult1.ToRead + IOResult2.ToRead);

    Move(IOResult1.BufferToRead[0], msgIn[0], IOResult1.ToRead);
    Move(IOResult2.BufferToRead[0], msgIn[IOResult1.ToRead],Length(IOResult2.BufferToRead));

    BytesRead := IOResult1.Received + IOResult2.Received;
    Result:=iorOK;
  finally
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

procedure TISOTCPDriver.PrepareToSend(var msg:BYTES);
var
  len:LongInt;
begin
  len := Length(msg);
  if len<4 then
    SetLength(msg, 4);
  msg[00] := 3;
  msg[01] := 0;
  msg[02] := len div $100;
  msg[03] := len mod $100;
end;

procedure TISOTCPDriver.Loaded;
begin
  inherited Loaded;
  UpdatePLCs;
end;

function TISOTCPDriver.DoRead(const tagrec: TTagRec; out
  Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
var
  atagrec: TTagRec;
begin
  atagrec:=tagrec;
  atagrec.Rack:=FPLCRack;
  atagrec.Slot:=FPLCSlot;
  atagrec.Station:=FPLCStation;
  Result:=inherited DoRead(atagrec, Values, Sync);
end;

procedure TISOTCPDriver.DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
begin
  TagRec.Station:=FPLCStation;
  TagRec.Slot:=FPLCSlot;
  TagRec.Rack:=FPLCRack;
  inherited DoGetValue(TagRec, values);
end;

function TISOTCPDriver.DoWrite(const tagrec: TTagRec;
  const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
var
  atagrec: TTagRec;
begin
  atagrec:=tagrec;
  atagrec.Rack:=FPLCRack;
  atagrec.Slot:=FPLCSlot;
  atagrec.Station:=FPLCStation;
  Result:=inherited DoWrite(atagrec, Values, Sync);
end;

procedure TISOTCPDriver.SetPLCRack(AValue: longint);
begin
  if FPLCRack=AValue then Exit;
  FPLCRack:=AValue;
  if [csLoading]*ComponentState=[] then
    UpdatePLCs;
end;

procedure TISOTCPDriver.SetPLCSlot(AValue: LongInt);
begin
  if FPLCSlot=AValue then Exit;
  FPLCSlot:=AValue;
  if [csLoading]*ComponentState=[] then
    UpdatePLCs;
end;

procedure TISOTCPDriver.SetPLCStation(AValue: LongInt);
begin
  if FPLCStation=AValue then Exit;
  FPLCStation:=AValue;
  if [csLoading]*ComponentState=[] then
    UpdatePLCs;
end;

procedure TISOTCPDriver.UpdatePLCs;
var
  StillConnected: Boolean;
  t: Integer;
  CurTag: TTag;
  TagList: TList;

begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    case Length(FPLCs) of
      0: begin
        //does nothing...;
      end;
      1:
        with FPLCs[0] do begin
          StillConnected:=Connected and (Rack=FPLCRack) AND (Slot=FPLCSlot) AND (Station=FPLCStation);
          Rack     :=FPLCRack;
          Slot     :=FPLCSlot;
          Station  :=FPLCStation;
          Connected:=StillConnected;
          if not StillConnected then
            PDUId:=0;
        end;
      else begin
        TagList:=TList.Create;
        try
          for t:=TagCount-1 downto 0 do begin
            CurTag:=Tag[t];
            TagList.Add(CurTag);
            DoDelTag(CurTag);
          end;

          if Length(FPLCs)>0 then
            raise Exception.Create('Something went wrong. At this point the '+
                                   'size of FPLCs must be zero. Please inform '+
                                   'this error to the PascalSCADA developer.');

          for t:=0 to TagList.Count-1 do begin
            DoAddTag(TTag(TagList.Items[t]), false);
          end;
        finally
          TagList.Destroy;
        end;
      end;
    end;

  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TISOTCPDriver.SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
begin
  FConnectionWay:=NewISOConWay;
end;

function TISOTCPDriver.GetTagInfo(tagobj: TTag): TTagRec;
begin
  Result:=inherited GetTagInfo(tagobj);
  //iso on TCP allows one PLC connection per TCP/IP connection
  //so, it allow only one Rack, Slot, and Station linked with
  //this connection. To avoid settings mistakes on user aplication
  //these method override the Tag settings (Rack, Slot,
  //and Station) with the settings of current instance of ISO on TCP protocol.
  Result.Slot   :=FPLCSlot;
  Result.Station:=FPLCStation;
  Result.Rack   :=FPLCRack;
end;

function TISOTCPDriver.NotifyThisEvents: TNotifyThisEvents;
begin
  Result:=[ntePortClosed, ntePortDisconnected];
end;

procedure TISOTCPDriver.PortClosed(Sender: TObject);
begin
  PortDisconnected(Self);
end;

procedure TISOTCPDriver.PortDisconnected(Sender: TObject);
var
  plc:LongInt;
begin
  for plc := 0 to High(FPLCs) do
    FPLCs[plc].Connected:=false;
end;

end.
