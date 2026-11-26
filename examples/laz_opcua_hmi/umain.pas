unit umain;

{: Tela de IHM ligada a tags OPC UA, montada toda no form (umain.lfm).

   No form estao, como componentes: o OPCUADriver1 (TOPCUADriver), os tags (TPLCTagNumber e
   TPLCString, LongAddress = NodeId) e os controles HMI ligados a eles pela propriedade PLCTag.
   Nenhum desses objetos e' criado por codigo - abra o form no Lazarus para ver as propriedades.

   Os tags apontam para variaveis ficticias do servidor de teste do PascalSCADA
   (tests/testsupport.opcuaserver.pas), que este programa sobe em opc.tcp://localhost:48401
   antes de abrir o form. O unico codigo aqui e' o desse servidor e o do status.
   Para usar um servidor de verdade: troque OPCUADriver1.EndpointUrl e o LongAddress dos tags
   (o Tag Builder do driver lista as variaveis) e tire StartLocalServer do .lpr.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Tag, PLCTagNumber, PLCString, OPCUAClient, OPCUADriver, HMILabel, HMIEdit, HMITrackBar,
  HMICheckBox, HMIProgressBar, testsupport.opcuaserver;

type

  { TFormMain }

  TFormMain = class(TForm)
    CheckServer: TCheckBox;
    GroupBool: TGroupBox;
    GroupCounter: TGroupBox;
    GroupDouble: TGroupBox;
    GroupInt: TGroupBox;
    GroupText: TGroupBox;
    HMICheckBoxBool: THMICheckBox;
    HMIEditDouble: THMIEdit;
    HMIEditText: THMIEdit;
    HMILabelCounter: THMILabel;
    HMILabelDouble: THMILabel;
    HMILabelInt: THMILabel;
    HMILabelText: THMILabel;
    HMIProgressBarInt: THMIProgressBar;
    HMITrackBarInt: THMITrackBar;
    LabelInstructions: TLabel;
    LabelStatus: TLabel;
    OPCUADriver1: TOPCUADriver;
    OPCUADriver2: TOPCUADriver;
    PanelStatus: TPanel;
    TagBool: TPLCTagNumber;
    TagCounter: TPLCTagNumber;
    TagDouble: TPLCTagNumber;
    TagInt: TPLCTagNumber;
    TagText: TPLCString;
    TimerStatus: TTimer;
    procedure CheckServerChange(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
  end;

const
  LocalServerPort = 48401;

//: Starts the local test server (the variables of the tags of the form). False if it could not.
function StartLocalServer:Boolean;
procedure StopLocalServer;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

var
  GServer:TOPCUATestServer = nil;
  GServerError:String = '';

function StartLocalServer:Boolean;
var
  opt:TOPCUATestServerOptions;
begin
  if GServer=nil then begin
    GServerError := '';
    //the same library search as the driver (system, versioned name, folder of the executable)
    if not OPCUALoadLibrary(GetEnvironmentVariable('OPEN62541_LIB'), GServerError) then
      exit(false);
    opt := OPCUATestDefaultOptions;
    opt.Port := LocalServerPort;
    try
      GServer := TOPCUATestServer.Create(opt);
    except
      on e:Exception do
        GServerError := e.Message;
    end;
  end;
  Result := GServer<>nil;
end;

procedure StopLocalServer;
begin
  FreeAndNil(GServer);
end;

{ TFormMain }

procedure TFormMain.CheckServerChange(Sender: TObject);
begin
  //turning the server off shows the tags falling into error and coming back when it is on again
  if CheckServer.Checked then begin
    if not StartLocalServer then
      ShowMessage('O servidor de teste nao subiu: '+GServerError);
  end else
    StopLocalServer;
end;

procedure TFormMain.TimerStatusTimer(Sender: TObject);
var
  s:String;
begin
  CheckServer.OnChange := nil;
  CheckServer.Checked := GServer<>nil;
  CheckServer.OnChange := @CheckServerChange;

  if OPCUADriver1.Connected then
    s := 'Conectado a '+OPCUADriver1.EndpointUrl
  else if OPCUADriver1.LastError<>'' then
    s := 'Desconectado: '+OPCUADriver1.LastError
  else
    s := 'Conectando a '+OPCUADriver1.EndpointUrl+'...';
  if (GServer=nil) and (GServerError<>'') then
    s := s+'   |   servidor de teste: '+GServerError;
  //the first line only
  if Pos(LineEnding, s)>0 then
    s := Copy(s, 1, Pos(LineEnding, s)-1);
  LabelStatus.Caption := s+'   |   Contador: '+TagCounter.LastASyncReadStatus.ToString;
end;

end.
