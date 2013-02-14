unit uDMImg;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, ImgList, Controls{$IFDEF FPC}, LResources{$ENDIF};

type

  { TImagens }

  TImagens = class(TDataModule)
    MotorD: TImageList;
    Silos: TImageList;
    Rosca: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Imagens: TImagens;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
initialization
  {$i uDMImg.lrs}
{$ENDIF}
end.
