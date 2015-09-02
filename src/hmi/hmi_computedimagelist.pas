unit HMI_ComputedImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRAImageList, hmibasiccolletion;

type

  { TColorCollectionitem }

  TColorCollectionitem = class(THMIBasicColletionItem)
  private
    FColor: TColor;
    procedure SetColor(AValue: TColor);
  published
    property Color:TColor read FColor write SetColor;
  end;

  TColorCollection = class(THMIBasicColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add: TColorCollectionitem;
  end;

  { TComputedImageList }

  TComputedImageList = class(TBGRAImageList)
  private
    FBaseImage: TBitmap;
    FImageColors: TColorCollection;
    procedure SetBaseImage(AValue: TBitmap);
    procedure SetImageColors(AValue: TColorCollection);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BaseImage:TBitmap read FBaseImage write SetBaseImage;
    property ImageColors:TColorCollection read FImageColors write SetImageColors;
  end;

implementation

{ TComputedImageList }

procedure TComputedImageList.SetBaseImage(AValue: TBitmap);
begin
  if FBaseImage=AValue then Exit;
  if Assigned(FBaseImage) then FBaseImage.Assign(AValue);
end;

procedure TComputedImageList.SetImageColors(AValue: TColorCollection);
begin
  if FImageColors=AValue then Exit;
  if Assigned(FImageColors) then FImageColors.Assign(AValue);
end;

constructor TComputedImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBaseImage:=TBitmap.Create;
end;

{ TColorCollection }

constructor TColorCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TColorCollectionitem);
end;

function TColorCollection.Add: TColorCollectionitem;
begin
  Result:=TColorCollectionitem(inherited Add);
end;

{ TColorCollectionitem }

procedure TColorCollectionitem.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  if (Collection is THMIBasicColletion) and
     (THMIBasicColletion(Collection).GetComponentState*[csReading, csLoading]=[]) then
    NotifyChange;
end;

end.
