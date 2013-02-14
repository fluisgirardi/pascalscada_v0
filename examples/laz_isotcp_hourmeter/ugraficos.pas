unit ugraficos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, Buttons, TAGraph, TADbSource, TASeries, TASources, TAChartAxis;

type

  { TfrmGraficos }

  TfrmGraficos = class(TForm)
    BitBtn1: TBitBtn;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart2: TChart;
    Chart2PieSeries1: TPieSeries;
    DateInicial: TDateEdit;
    DateFinal: TDateEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure Chart1AxisList0MarkToText(var AText: String; AMark: Double);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmGraficos: TfrmGraficos;

implementation

uses udmdb;

{ TfrmGraficos }

procedure TfrmGraficos.BitBtn1Click(Sender: TObject);
var
  r, horas:Integer;
  datai,dataf, atext:string;
  minutos:Double;
  olddecseparator:Char;
begin
  datai:=FormatDateTime('yyyy-mm-dd',DateInicial.Date);
  dataf:=FormatDateTime('yyyy-mm-dd',DateFinal.Date);
  with dmdb.ZQuery1 do begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT SUM((strftime(''%s'',a.dt_fim) - strftime(''%s'',a.dt_inicio)) / 3600.0) as horas, e.ds_equipamento from tbl_acionamento a '+
            'LEFT JOIN tbl_equipamento e ON a.cd_equipamento=e.id_equipamento '+
            'WHERE a.dt_inicio>=strftime(''%Y-%m-%d'','''+datai+''') AND a.dt_inicio<=strftime(''%Y-%m-%d %H:%M:%S'','''+dataf+' 23:59:59'') '+
            'GROUP BY a.cd_equipamento');
    Open;

    ListChartSource1.Clear;
    olddecseparator:=DecimalSeparator;
    DecimalSeparator:='.';
    try
      for r:=1 to RecordCount do begin
        RecNo:=r;

        horas:=trunc(FieldByName('horas').AsFloat);
        minutos:=(FieldByName('horas').AsFloat-horas)*60;
        AText:=IntToStr(horas)+':'+FormatFloat('00',minutos);

        ListChartSource1.Add(r,FieldByName('horas').AsFloat,FieldByName('ds_equipamento').AsString+' - '+atext);
      end;
    finally
      DecimalSeparator:=olddecseparator;
    end;
  end;
end;

procedure TfrmGraficos.Chart1AxisList0MarkToText(var AText: String;
  AMark: Double);
var
  horas:Integer;
  minutos:Double;
begin
  horas:=trunc(AMark);
  minutos:=(AMark-horas)*60;
  AText:=IntToStr(horas)+':'+FormatFloat('00',minutos);
end;

procedure TfrmGraficos.FormShow(Sender: TObject);
begin
  DateInicial.Date:=Now;
  DateFinal.Date:=now;
end;

{$R *.lfm}

end.

