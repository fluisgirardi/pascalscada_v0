unit TagLinkedSeriesSource;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, DateUtils, TASources, TACustomSource, TAChartUtils, ExtCtrls,
  PLCNumber, ProtocolTypes, TAGraph;

type
  TTagLinkUpdateType = (tlCyclic, tlTagUpdate, tlTagChange);

  { TTagLinkedSeriesSource }

  TTagLinkedSeriesSource = class(TListChartSource)
  private
    FCyclicUptimeTime: LongWord;
    FEnableXAxisMaxInterval: Boolean;
    FPLCTag: TPLCNumber;
    FSourceUpdateType: TTagLinkUpdateType;
    FUseNowInsteadTagTimestamp: Boolean;
    FXAxisMaximumInterval: LongWord;
    FUpdateTimer: TTimer;
    function GetParentChart: TChart;
    procedure SetCyclicUptimeTime(AValue: LongWord);
    procedure SetEnableXAxisMaxInterval(AValue: Boolean);
    procedure SetPLCTag(AValue: TPLCNumber);
    procedure SetSourceUpdateType(AValue: TTagLinkUpdateType);
    procedure SetUseNowInsteadTagTimestamp(AValue: Boolean);
    procedure SetXAxisMaximumInterval(AValue: LongWord);
    procedure SetXAxisTimeFormat(Chart: TChart);
    procedure FormatAxisXLabel(Sender: TObject; var AText: AnsiString; AMark: Double);
    procedure TagChanged(Sender: TObject);
    procedure TimerCyclic(Sender: TObject);
    procedure DoAddDataPoint;
  protected
    procedure Loaded; override;
    procedure DeleteOldData;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SourceUpdateType: TTagLinkUpdateType read FSourceUpdateType write SetSourceUpdateType;
    property PLCTag: TPLCNumber read FPLCTag write SetPLCTag;
    property CyclicUptimeTime: LongWord read FCyclicUptimeTime write SetCyclicUptimeTime;
    property EnableXAxisMaxInterval: Boolean read FEnableXAxisMaxInterval write SetEnableXAxisMaxInterval;
    property XAxisMaximumInterval: LongWord read FXAxisMaximumInterval write SetXAxisMaximumInterval;
    property UseNowInsteadTagTimestamp: Boolean read FUseNowInsteadTagTimestamp write SetUseNowInsteadTagTimestamp;
  end;

implementation

uses
  TASeries, TAChartAxis;

{ TTagLinkedSeriesSource }

procedure TTagLinkedSeriesSource.SetPLCTag(AValue: TPLCNumber);
begin
  if FPLCTag = AValue then Exit;

  if Assigned(FPLCTag) then
  begin
    FPLCTag.RemoveFreeNotification(Self);
    FPLCTag.RemoveAllHandlersFromObject(Self);
  end;

  if Assigned(AValue) then
  begin
    AValue.FreeNotification(Self);
    AValue.AddTagChangeHandler(@TagChanged);
  end;

  FPLCTag := AValue;
end;

procedure TTagLinkedSeriesSource.SetCyclicUptimeTime(AValue: LongWord);
begin
  if FCyclicUptimeTime = AValue then Exit;
  FCyclicUptimeTime := AValue;
  FUpdateTimer.Interval := AValue;
end;

procedure TTagLinkedSeriesSource.SetEnableXAxisMaxInterval(AValue: Boolean);
begin
  if FEnableXAxisMaxInterval = AValue then Exit;
  FEnableXAxisMaxInterval := AValue;
  DeleteOldData;
end;

procedure TTagLinkedSeriesSource.SetSourceUpdateType(AValue: TTagLinkUpdateType);
begin
  if FSourceUpdateType = AValue then Exit;
  FUpdateTimer.Enabled := (AValue = tlCyclic);
  FSourceUpdateType := AValue;
end;

procedure TTagLinkedSeriesSource.SetUseNowInsteadTagTimestamp(AValue: Boolean);
begin
  if FUseNowInsteadTagTimestamp = AValue then Exit;
  FUseNowInsteadTagTimestamp := AValue;
end;

procedure TTagLinkedSeriesSource.SetXAxisMaximumInterval(AValue: LongWord);
begin
  if FXAxisMaximumInterval = AValue then Exit;
  FXAxisMaximumInterval := AValue;
  DeleteOldData;
end;

procedure TTagLinkedSeriesSource.SetXAxisTimeFormat(Chart: TChart);
begin
  if Assigned(Chart) and not Assigned(Chart.BottomAxis.OnGetMarkText) then
  begin
    Chart.BottomAxis.OnGetMarkText := @FormatAxisXLabel;
    Chart.BottomAxis.Marks.Style := smsValue;
    Chart.BottomAxis.Marks.Distance := 15; // aumenta o espaço entre rótulos para evitar sobreposição
  end;
end;

procedure TTagLinkedSeriesSource.FormatAxisXLabel(Sender: TObject; var AText: AnsiString; AMark: Double);
begin
  AText := FormatDateTime('hh:nn:ss', AMark);
end;

procedure TTagLinkedSeriesSource.TagChanged(Sender: TObject);
begin
  if (FSourceUpdateType = tlTagChange) then
    DoAddDataPoint;
end;

procedure TTagLinkedSeriesSource.TimerCyclic(Sender: TObject);
var
  chart: TChart;
  x, minX: TDateTime;
begin
  if FSourceUpdateType = tlCyclic then
    DoAddDataPoint;

  if FEnableXAxisMaxInterval then
  begin
    chart := GetParentChart;
    if Assigned(chart) then
    begin
      if FUseNowInsteadTagTimestamp then
        x := Now
      else if Assigned(FPLCTag) and Supports(FPLCTag, ITagNumeric) then
        x := (FPLCTag as ITagNumeric).GetValueTimestamp
      else
        Exit;

      minX := IncMilliSecond(x, -Int64(FXAxisMaximumInterval));

      chart.DisableRedrawing;
      try
        chart.BottomAxis.Range.UseMin := True;
        chart.BottomAxis.Range.UseMax := True;
        chart.BottomAxis.Range.Min := minX;
        chart.BottomAxis.Range.Max := x;

        chart.Extent.UseXMin := True;
        chart.Extent.UseXMax := True;
        chart.Extent.XMin := minX;
        chart.Extent.XMax := x;
      finally
        chart.EnableRedrawing;
      end;

      chart.Invalidate;
    end;
  end;
end;

procedure TTagLinkedSeriesSource.DoAddDataPoint;
var
  x, minX: TDateTime;
  chart: TChart;
begin
  if Supports(FPLCTag, ITagNumeric) then
  begin
    if FUseNowInsteadTagTimestamp then
      x := Now
    else
      x := (FPLCTag as ITagNumeric).GetValueTimestamp;

    Add(x, (FPLCTag as ITagNumeric).Value);
    DeleteOldData;

    chart := GetParentChart;
    if Assigned(chart) then
    begin
      SetXAxisTimeFormat(chart);
      if FEnableXAxisMaxInterval then
      begin
        minX := IncMilliSecond(x, -Int64(FXAxisMaximumInterval));
        chart.BottomAxis.Range.UseMin := True;
        chart.BottomAxis.Range.UseMax := True;
        chart.BottomAxis.Range.Min := minX;
        chart.BottomAxis.Range.Max := x;
        chart.Invalidate;
      end;
    end;

    Notify;
  end;
end;

procedure TTagLinkedSeriesSource.DeleteOldData;
var
  LimitTime: TDateTime;
  ChartItem: PChartDataItem;
begin
  if not FEnableXAxisMaxInterval then Exit;
  if FXAxisMaximumInterval = 0 then Exit;

  if FUseNowInsteadTagTimestamp then
    LimitTime := IncMilliSecond(Now, -Int64(FXAxisMaximumInterval))
  else if Assigned(FPLCTag) and Supports(FPLCTag, ITagNumeric) then
    LimitTime := IncMilliSecond((FPLCTag as ITagNumeric).GetValueTimestamp, -Int64(FXAxisMaximumInterval))
  else
    Exit;

  while (FData.Count > 0) do
  begin
    ChartItem := PChartDataItem(FData.Items[0]);
    if ChartItem^.X >= LimitTime then
      Break;
    FData.Delete(0);
  end;

  Notify;
end;

function TTagLinkedSeriesSource.GetParentChart: TChart;
var
  i: Integer;
  s: TBasicChartSeries;
  chart: TChart;
begin
  Result := nil;
  if not Assigned(Owner) then Exit;

  for i := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[i] is TChart then
    begin
      chart := TChart(Owner.Components[i]);
      for s in chart.Series do
      begin
        if (s is TLineSeries) and (TLineSeries(s).Source = Self) then
        begin
          Result := chart;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TTagLinkedSeriesSource.Loaded;
begin
  inherited Loaded;
end;

procedure TTagLinkedSeriesSource.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FPLCTag) then
    SetPLCTag(nil);
end;

constructor TTagLinkedSeriesSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.OnTimer := @TimerCyclic;
end;

destructor TTagLinkedSeriesSource.Destroy;
begin
  FreeAndNil(FUpdateTimer);
  inherited Destroy;
end;

end.

