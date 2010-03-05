{:
  @abstract(Tipos comuns aos CLP's da familia Siemens.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit S7Types;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  PLCMemoryManager;

type
  //: Identifica um DB da familia S7-300/S7-400
  TS7DB = Record
    DBNum:Cardinal;
    DBArea:TPLCMemoryManager;
  end;
  //: Identifica um conjunto de DB's da familia S7-300/S7-400
  TS7DBs = array of TS7DB;

  //: Representação de um CLP S7-200/300/400 da Siemens.
  TS7CPU=record
    Station,
    Rack,
    Slot:Integer;
    Connected:Boolean;
    Inputs:TPLCMemoryManager;
    Outputs:TPLCMemoryManager;
    VMs:TPLCMemoryManager;
    DBs:TS7DBs;
    Timers:TPLCMemoryManager;
    Counters:TPLCMemoryManager;
    Memorys:TPLCMemoryManager;
    SMs:TPLCMemoryManager;
  end;

  //: Representação de um conjunto de CLP's S7-200/300/400 da Siemens.
  TS7CPUs = array of TS7CPU;

  //: Identifica o meio de conexão com o CLP.
  TISOTCPConnectionWay = (ISOTCP,ISOTCP_VIA_CP243);

implementation

end.

