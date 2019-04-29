unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, ActnList, HMIAnimation, PLCTagNumber,
  HMIEdit, HMILabel, HMICheckBox, HMIText, ControlSecurityManager;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    DBGrid1: TDBGrid;
    anForno: THMIAnimation;
    anRotativa: THMIAnimation;
    anAtomizador: THMIAnimation;
    anZaranda: THMIAnimation;
    anBomba_de_Produto: THMIAnimation;
    anVentilador: THMIAnimation;
    HMIAnimation2: THMIAnimation;
    HMICheckBox1: THMICheckBox;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    HMIText1: THMIText;
    anFiltro_Manga: THMIText;
    HMIText2: THMIText;
    HMIText3: THMIText;
    HMIText4: THMIText;
    HMIText5: THMIText;
    HMIText6: THMIText;
    HMIText7: THMIText;
    anMartelo: THMIText;
    HMIText9: THMIText;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    fire: TImageList;
    ImageList4: TImageList;
    motorh: TImageList;
    motorv: TImageList;
    ShowTags: TPLCTagNumber;
    LeftToolbar: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    PascalSCADAManageUsersAction1: TPascalSCADAManageUsersAction;
    secureGraphics: TPascalSCADASecureAction;
    secureRelatorioAcionamento: TPascalSCADASecureAction;
    secureAlarmes: TPascalSCADASecureAction;
    Timer1: TTimer;
    Timer2: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure anBomba_de_ProdutoClick(Sender: TObject);
    procedure anFiltro_MangaClick(Sender: TObject);
    procedure anMarteloClick(Sender: TObject);
    procedure anRotativaClick(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormCreate(Sender: TObject);
    procedure anFornoClick(Sender: TObject);
    procedure anAtomizadorClick(Sender: TObject);
    procedure anZarandaClick(Sender: TObject);
    procedure anVentiladorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HMILabel7Click(Sender: TObject);
    procedure HMILabel8Click(Sender: TObject);
    procedure HMIText7Click(Sender: TObject);
    procedure HMIText9Click(Sender: TObject);
    procedure secureAlarmesExecute(Sender: TObject);
    procedure secureGraphicsExecute(Sender: TObject);
    procedure secureRelatorioAcionamentoExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses udmdb, udm, upopuphorimetro, ugraficos, ufiltro;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ShowTags.Value:=1;
  WindowState:=wsMaximized;
end;

procedure TfrmMain.anBomba_de_ProdutoClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido4,dmtags.SPHorimetro4,dmtags.TAcumulado4,dmtags.Reset_4,dmtags.MB1_2,dmtags.ResetAlarme1_4) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anFiltro_MangaClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido7,dmtags.SPHorimetro7,dmtags.TAcumulado7,dmtags.Reset_7,dmtags.MB1_3,dmtags.ResetAlarme1_7) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anMarteloClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido8,dmtags.SPHorimetro8,dmtags.TAcumulado8,dmtags.Reset_8,dmtags.MB0_1,dmtags.ResetAlarme1_8) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anRotativaClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido5,dmtags.SPHorimetro5,dmtags.TAcumulado5,dmtags.Reset_5,dmtags.MB1_1,dmtags.ResetAlarme1_5) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.ApplicationProperties1Exception(Sender: TObject; E: Exception
  );
begin
  MessageDlg(e.Message,mtError,[mbOK],0);
end;

procedure TfrmMain.anFornoClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido1,dmtags.SPHorimetro1,dmtags.TAcumulado1,dmtags.Reset_1,dmtags.MB2_0,dmtags.ResetAlarme1_1) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anAtomizadorClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido3,dmtags.SPHorimetro3,dmtags.TAcumulado3,dmtags.Reset_3,dmtags.MB0_7,dmtags.ResetAlarme1_3) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anZarandaClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido6,dmtags.SPHorimetro6,dmtags.TAcumulado6,dmtags.Reset_6,dmtags.MB1_0,dmtags.ResetAlarme1_6) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.anVentiladorClick(Sender: TObject);
begin
  //GetControlSecurityManager.TryAccess('5');
  with TfrmPopup.Create(nil,dmtags.TDecorrido2,dmtags.SPHorimetro2,dmtags.TAcumulado2,dmtags.Reset_2,dmtags.MB0_6,dmtags.ResetAlarme1_2) do begin
    Caption:=RightStr(TComponent(sender).Name,Length(TComponent(sender).Name)-2);
    Show;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LeftToolbar.Left:=-57;
end;

procedure TfrmMain.HMILabel7Click(Sender: TObject);
begin
  anMarteloClick(anMartelo);
end;

procedure TfrmMain.HMILabel8Click(Sender: TObject);
begin
  anFiltro_MangaClick(anFiltro_Manga);
end;

procedure TfrmMain.HMIText7Click(Sender: TObject);
begin
  anMarteloClick(anMartelo);
end;

procedure TfrmMain.HMIText9Click(Sender: TObject);
begin
  anFiltro_MangaClick(anFiltro_Manga);
end;

procedure TfrmMain.secureAlarmesExecute(Sender: TObject);
//var
//  filtro:TfrmFiltro;
begin
  //filtro:=TfrmFiltro.Create(self);
  //try
  //  if filtro.ShowModal=mrOK then begin
  //    with dmdb do begin
  //      AlarmesQuery.Close;
  //      AlarmesQuery.ParamByName('datai').AsDateTime:=filtro.DateInicial.Date;
  //      AlarmesQuery.ParamByName('dataf').AsDateTime:=filtro.DateFinal.Date;
  //      AlarmesQuery.Open;
  //
  //      with frmRelatorios do begin
  //        RLReport1.DataSource:=AlarmeDatasource;
  //        RLDBText1.DataSource:=AlarmeDatasource;
  //        RLDBText2.DataSource:=AlarmeDatasource;
  //        RLDBText3.DataSource:=AlarmeDatasource;
  //        RLDBText4.DataSource:=AlarmeDatasource;
  //
  //        RLLabel2.Caption:='Iniciado em';
  //        RLLabel3.Caption:='Finalizado em';
  //        RLLabel4.Caption:='Duração';
  //        RLLabel5.Caption:='Equipamento';
  //
  //        RLLabel1.Caption:='Relatório de Alarmes de Horimetro';
  //
  //        RLLabel6.Caption:='Intervalo de '+DateToStr(filtro.DateInicial.Date)+' até '+DateToStr(filtro.DateFinal.Date);
  //
  //        RLReport1.Preview;
  //      end;
  //    end;
  //  end;
  //finally
  //  dmdb.AlarmesQuery.Close;
  //  filtro.Destroy;
  //end;
end;

procedure TfrmMain.secureGraphicsExecute(Sender: TObject);
var
  x:TfrmGraficos;
begin
  x:=TfrmGraficos.Create(self);
  try
    x.ShowModal;
  finally
    x.Destroy;
  end;
end;

procedure TfrmMain.secureRelatorioAcionamentoExecute(Sender: TObject);
//var
//  filtro:TfrmFiltro;
begin
  //filtro:=TfrmFiltro.Create(self);
  //try
  //  if filtro.ShowModal=mrOK then begin
  //    with dmdb do begin
  //      AcionamentoQuery.Close;
  //      AcionamentoQuery.ParamByName('datai').AsDateTime:=filtro.DateInicial.Date;
  //      AcionamentoQuery.ParamByName('dataf').AsDateTime:=filtro.DateFinal.Date;
  //      AcionamentoQuery.Open;
  //
  //      with frmRelatorios do begin
  //        RLReport1.DataSource:=AcionamentoDatasource;
  //        RLDBText1.DataSource:=AcionamentoDatasource;
  //        RLDBText2.DataSource:=AcionamentoDatasource;
  //        RLDBText3.DataSource:=AcionamentoDatasource;
  //        RLDBText4.DataSource:=AcionamentoDatasource;
  //
  //        RLLabel2.Caption:='Ligado em';
  //        RLLabel3.Caption:='Desligado em';
  //        RLLabel4.Caption:='Duração';
  //        RLLabel5.Caption:='Equipamento';
  //
  //        RLLabel1.Caption:='Relatório de acionamentos';
  //
  //        RLLabel6.Caption:='Intervalo de '+DateToStr(filtro.DateInicial.Date)+' até '+DateToStr(filtro.DateFinal.Date);
  //
  //        RLReport1.Preview;
  //      end;
  //    end;
  //  end;
  //finally
  //  dmdb.AcionamentoQuery.Close;
  //  filtro.Destroy;
  //end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if Timer1.Tag>0 then begin
    if (LeftToolbar.Left<0) then
      LeftToolbar.Left:=LeftToolbar.Left+Timer1.Tag
    else begin
      timer1.Enabled:=false;
      timer1.Tag:=0;
      timer2.Enabled:=true;
    end;
  end;

  if Timer1.Tag<0 then begin
    if (LeftToolbar.Left>-57) then
      LeftToolbar.Left:=LeftToolbar.Left+Timer1.Tag
    else begin
      timer1.Enabled:=false;
      timer1.Tag:=0;
      timer2.Enabled:=true;
    end;
  end;
end;

procedure TfrmMain.Timer2Timer(Sender: TObject);
begin
  if (mouse.CursorPos.X<=33) and (timer1.tag=0) then begin
    Timer1.Tag:=1;
    Timer1.Enabled:=true;
    timer2.Enabled:=false;
  end;
  if (mouse.CursorPos.X>80) and (timer1.tag=0) then begin
    Timer1.Tag:=-1;
    Timer1.Enabled:=true;
    timer2.Enabled:=false;
  end;
end;

end.

