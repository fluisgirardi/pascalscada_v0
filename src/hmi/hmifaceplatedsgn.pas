unit hmifaceplatedsgn;

{$mode objfpc}{$H+}

interface

uses ProjectIntf, PropEdits;

type

  { TFaceplateFrame }

  TFaceplateFrameFileDescriptor = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

implementation

uses hmifaceplatecontainer;

{ TFaceplateFrame }

constructor TFaceplateFrameFileDescriptor.Create;
begin
  inherited Create;
  Name:='FaceplateFrame'; // do not translate this
  ResourceClass:=TFaceplateFrame;
  RequiredPackages:='pascalscada_hmi';
  UseCreateFormStatements:=True;
end;

function TFaceplateFrameFileDescriptor.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, Forms, HMIFaceplateContainer';
end;

function TFaceplateFrameFileDescriptor.GetLocalizedName: string;
begin
  Result:='FaceplateFrame';
end;

function TFaceplateFrameFileDescriptor.GetLocalizedDescription: string;
begin
  Result:='Create a new TFaceplate form for object customization';
end;

end.

