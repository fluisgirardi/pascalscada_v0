unit umain;

{: Lista os tags de um CLP com o metodo COMUM TProtocolDriver.ListTags.

   O driver (Siemens S7CommPlus ou Rockwell) so e' escolhido na hora de CRIA-LO; dali em
   diante o programa so enxerga um TProtocolDriver - SupportsListTags, ListTags e o registro
   TPLCTagInfo sao os mesmos para os dois. Para um terceiro driver que saiba listar, basta
   acrescentar uma linha em CreateDriver.

   Uso: listtags_generic [s7|rockwell [ip [porta]]]  - com argumentos ja conecta ao abrir.
   Portas: S7CommPlus 102, Rockwell (EtherNet/IP) 44818.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Tag, ProtocolTypes, ProtocolDriver, tcp_udpport, S7PlusFamily, LGXDriver;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonList: TButton;
    CheckOnlyTags: TCheckBox;
    ComboDriver: TComboBox;
    EditFilter: TEdit;
    EditHost: TEdit;
    EditPort: TEdit;
    LabelDriver: TLabel;
    LabelFilter: TLabel;
    LabelHost: TLabel;
    LabelPort: TLabel;
    ListTagsView: TListView;
    PanelTop: TPanel;
    StatusBar: TStatusBar;
    procedure ButtonListClick(Sender: TObject);
    procedure ComboDriverChange(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPort:TTCP_UDPPort;
    FDriver:TProtocolDriver;     //the only thing the listing code knows about the driver
    FTags:TPLCTagInfoArray;
    procedure Disconnect;
    function  CreateDriver:TProtocolDriver;
    procedure FillList;
    function  ResultName(aResult:TProtocolIOResult):String;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  DRIVER_S7PLUS   = 0;
  DRIVER_ROCKWELL = 1;

  KindNames:array[TPLCTagKind] of String =
    ('scalar', 'array', 'estrutura', 'string', 'sem suporte');

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ComboDriver.Items.Add('Siemens S7-1200/1500 (S7CommPlus)');
  ComboDriver.Items.Add('Rockwell Compact/ControlLogix (EtherNet/IP)');
  ComboDriver.ItemIndex:=DRIVER_S7PLUS;

  //listtags_generic [s7|rockwell [ip [porta]]]
  if ParamCount>=1 then begin
    if SameText(ParamStr(1), 'rockwell') then ComboDriver.ItemIndex:=DRIVER_ROCKWELL;
    ComboDriverChange(nil);
    if ParamCount>=2 then EditHost.Text:=ParamStr(2);
    if ParamCount>=3 then EditPort.Text:=ParamStr(3);
    ButtonListClick(nil);
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Disconnect;
end;

//each driver has its own default port
procedure TFormMain.ComboDriverChange(Sender: TObject);
begin
  if ComboDriver.ItemIndex=DRIVER_ROCKWELL then EditPort.Text:='44818'
  else EditPort.Text:='102';
end;

procedure TFormMain.Disconnect;
begin
  FreeAndNil(FDriver);
  FreeAndNil(FPort);
end;

//the one place that knows which drivers exist
function TFormMain.CreateDriver:TProtocolDriver;
begin
  case ComboDriver.ItemIndex of
    DRIVER_ROCKWELL: Result:=TLGXDriver.Create(nil);
  else
    Result:=TS7CommPlusDriver.Create(nil);
  end;
end;

function TFormMain.ResultName(aResult:TProtocolIOResult):String;
begin
  Result:=GetEnumName(TypeInfo(TProtocolIOResult), Ord(aResult));
end;

procedure TFormMain.ButtonListClick(Sender: TObject);
var
  res:TProtocolIOResult;
  waited:LongInt;
  oldCursor:TCursor;
begin
  Disconnect;
  SetLength(FTags, 0);
  ListTagsView.Items.Clear;

  FPort:=TTCP_UDPPort.Create(nil);
  FPort.Host:=Trim(EditHost.Text);
  FPort.Port:=StrToIntDef(EditPort.Text, 0);
  FPort.Timeout:=8000;
  FPort.EnableAutoReconnect:=false;

  FDriver:=CreateDriver;
  FDriver.CommunicationPort:=FPort;
  FPort.Active:=true;

  StatusBar.SimpleText:='Conectando...';
  StatusBar.Update;
  waited:=0;
  while (not FPort.ReallyActive) and (waited<5000) do begin
    Sleep(50);
    Application.ProcessMessages;
    inc(waited, 50);
  end;
  if not FPort.ReallyActive then begin
    StatusBar.SimpleText:='Nao conectou a '+FPort.Host+':'+IntToStr(FPort.Port);
    exit;
  end;

  //from here on only the base class is used
  if not FDriver.SupportsListTags then begin
    StatusBar.SimpleText:='Este driver nao sabe listar os tags.';
    exit;
  end;

  oldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  try
    StatusBar.SimpleText:='Lendo a lista de tags do CLP (pode demorar)...';
    StatusBar.Update;
    res:=FDriver.ListTags(FTags);
  finally
    Screen.Cursor:=oldCursor;
  end;

  FillList;
  StatusBar.SimpleText:=Format('ListTags = %s - %d tag(s) listado(s), %d exibido(s).',
                              [ResultName(res), Length(FTags), ListTagsView.Items.Count]);
end;

procedure TFormMain.FilterChanged(Sender: TObject);
begin
  FillList;
end;

procedure TFormMain.FillList;
var
  c:LongInt;
  filter, swap:String;
  item:TListItem;
begin
  filter:=LowerCase(Trim(EditFilter.Text));

  ListTagsView.Items.BeginUpdate;
  try
    ListTagsView.Items.Clear;
    for c:=0 to High(FTags) do begin
      if CheckOnlyTags.Checked and (FTags[c].Kind in [tkStructure, tkUnsupported]) then continue;
      if (filter<>'') and (Pos(filter, LowerCase(FTags[c].Path))=0) then continue;

      swap:='';
      if FTags[c].SwapBytes then swap:=swap+'B';
      if FTags[c].SwapWords then swap:=swap+'W';
      if FTags[c].SwapDWords then swap:=swap+'D';

      item:=ListTagsView.Items.Add;
      item.Caption:=FTags[c].Path;
      item.SubItems.Add(KindNames[FTags[c].Kind]);
      item.SubItems.Add(FTags[c].NativeType);
      if FTags[c].Kind in [tkScalar, tkArray] then
        item.SubItems.Add(GetEnumName(TypeInfo(TTagType), Ord(FTags[c].TagType)))
      else
        item.SubItems.Add('');
      item.SubItems.Add(IntToStr(FTags[c].ElementCount));
      item.SubItems.Add(IntToStr(FTags[c].SizeInBytes));
      item.SubItems.Add(swap);
      item.SubItems.Add(FTags[c].Group);
    end;
  finally
    ListTagsView.Items.EndUpdate;
  end;

  if Length(FTags)>0 then
    StatusBar.SimpleText:=Format('%d tag(s) listado(s), %d exibido(s).',
                                [Length(FTags), ListTagsView.Items.Count]);
end;

end.
