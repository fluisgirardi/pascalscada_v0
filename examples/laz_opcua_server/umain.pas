unit umain;

{
  TOPCUAServer demonstration: the tags of this form, read and written by any
  OPC UA client (UaExpert, a SCADA, another PascalSCADA application with a
  TOPCUADriver...).

  The tags are memory tags (no driver) moved by a timer:
    Process/Temperature   Float, follows the setpoint slowly       read only
    Process/Setpoint      Word                                     writable
    Process/PumpOn        bit 0 of Status (Boolean)                writable
    Process/Message       String                                   writable
    Process/Trend         array of 10 Words, the last temperatures read only

  Everything is set on the form: see OPCUAServer1.Items (double click on the
  server adds the tags of a form). A tag with a protocol driver is published
  the same way - its value is what the device has, and a write by a client
  goes to the device, the client getting the device's result.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Tag, PLCTagNumber, PLCString, PLCBlock, TagBit, HMILabel, HMIEdit, opcuaserver;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelTemperatureCaption: TLabel;
    LabelSetpointCaption: TLabel;
    LabelPumpCaption: TLabel;
    LabelMessageCaption: TLabel;
    LabelStatus: TLabel;
    MemoHelp: TMemo;
    Temperature: TPLCTagNumber;
    Setpoint: TPLCTagNumber;
    Status: TPLCTagNumber;
    PumpOn: TTagBit;
    Message: TPLCString;
    Trend: TPLCBlock;
    OPCUAServer1: TOPCUAServer;
    ShowTemperature: THMILabel;
    EditSetpoint: THMIEdit;
    ShowPump: THMILabel;
    ShowMessage: THMILabel;
    Simulation: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure SimulationTimer(Sender: TObject);
  private
    procedure ShowServerState;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Temperature.Value := 20;
  Setpoint.Value := 25;
  Message.Value := 'hello from PascalSCADA';
end;

procedure TFormMain.SimulationTimer(Sender: TObject);
var
  t:Double;
  vals:TArrayOfDouble;
  i:Integer;
begin
  //the pump only heats while it is on
  t := Temperature.Value;
  if PumpOn.Value<>0 then
    t := t+(Setpoint.Value-t)*0.1
  else
    t := t-0.05;
  Temperature.Value := t+(Random-0.5)*0.05;

  //the last temperatures, oldest first
  vals := Trend.ValuesRaw;
  for i:=0 to High(vals)-1 do vals[i] := vals[i+1];
  if Length(vals)>0 then vals[High(vals)] := Round(Temperature.Value);
  Trend.ValuesRaw := vals;

  ShowServerState;
end;

procedure TFormMain.ShowServerState;
begin
  //the server starts once the form is loaded (it may take a moment)
  if OPCUAServer1.Running then
    LabelStatus.Caption := Format('Running: opc.tcp://<this computer>:%d  (namespace index %d)',
                                  [OPCUAServer1.Port, OPCUAServer1.NamespaceIndex])
  else if OPCUAServer1.LastError<>'' then
    LabelStatus.Caption := 'Not running: '+OPCUAServer1.LastError
  else
    LabelStatus.Caption := 'Starting...';
end;

end.
