unit hmi_tachart_tag_source_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, TACustomSource;

type

  { THMITaChartTagValuesSource }

  THMITaChartTagValuesSource = class(TCustomChartSource)
  private
    FData: TFPList;
    procedure ClearCaches;
    function NewItem: PChartDataItem;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    //procedure SetXCount(AValue: Cardinal); override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ THMITaChartTagValuesSource }

function THMITaChartTagValuesSource.NewItem: PChartDataItem;
begin
  New(Result);
  //SetLength(Result^.XList, Max(XCount - 1, 0));
  SetLength(Result^.YList, Max(YCount - 1, 0));
end;

function THMITaChartTagValuesSource.GetCount: Integer;
begin
  Result := FData.Count;
end;

function THMITaChartTagValuesSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Result := PChartDataItem(FData.Items[AIndex]);
end;

//procedure THMITaChartTagValuesSource.SetXCount(AValue: Cardinal);
//var
//  i: Integer;
//begin
//  if AValue = FXCount then exit;
//  FXCount := AValue;
//  for i := 0 to Count - 1 do
//    SetLength(Item[i]^.XList, Max(FXCount - 1, 0));
//end;

procedure THMITaChartTagValuesSource.SetYCount(AValue: Cardinal);
var
  i: Integer;
begin
  if AValue = FYCount then exit;
  FYCount := AValue;
  for i := 0 to Count - 1 do
    SetLength(Item[i]^.YList, Max(FYCount - 1, 0));
end;

procedure THMITaChartTagValuesSource.Clear;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Dispose(Item[i]);
  FData.Clear;
  ClearCaches;
  Notify;
end;

procedure THMITaChartTagValuesSource.ClearCaches;
begin
  //FExtent := EmptyExtent;
  //FExtentIsValid := true;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
end;

constructor THMITaChartTagValuesSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData:=TFPList.Create;
end;

destructor THMITaChartTagValuesSource.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  inherited Destroy;
end;

end.

