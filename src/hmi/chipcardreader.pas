unit ChipCardReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TChipCardReader }

  TChipCardReader = class(TComponent)
  public
    function InitializeChipCard:Boolean; virtual;
    function ChipCardReady:Boolean; virtual;
    function IsEmptyChipCard:Boolean; virtual;
    function ChipCardRead(var aChipCardCode:UTF8String):Boolean; virtual;
    function FinishChipCard:Boolean; Virtual;
  end;

implementation

{ TChipCardReader }

function TChipCardReader.InitializeChipCard: Boolean;
begin
  Exit(false);
end;

function TChipCardReader.ChipCardReady: Boolean;
begin
  Exit(false);
end;

function TChipCardReader.IsEmptyChipCard: Boolean;
begin
  Result:=true;
end;

function TChipCardReader.ChipCardRead(var aChipCardCode: UTF8String): Boolean;
begin
  Result:=false;
end;

function TChipCardReader.FinishChipCard: Boolean;
begin
  Exit(false);
end;

end.
