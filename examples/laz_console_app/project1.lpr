program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, udatamod, Crt
  { you can add units after this };

type

  { TSamplePSCADAApp }

  TSamplePSCADAApp = class(TCustomApplication)
  protected
    dm:TDataModule1;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSamplePSCADAApp }

procedure TSamplePSCADAApp.DoRun;
var
  i: Integer;
begin
  ClrScr;
  WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss',Now)+'------------------------------------------------------');
  for i:=0 to dm.PLCBlock1.Size-1 do
    WriteLn(Format('PLCBlock1[%d]=%.1f',[i,dm.PLCBlock1.ValueRaw[i]]));

  WriteLn('PLCBlock1_e4.Value=',dm.PLCBlock1_e4.Value);
  CheckSynchronize(2); //<<<<<==== this line does the work... DONT REMOVE!!
  Sleep(100);
end;

constructor TSamplePSCADAApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  dm:=TDataModule1.Create(Self);
end;

destructor TSamplePSCADAApp.Destroy;
begin
  inherited Destroy;
  FreeAndNil(dm);
end;

var
  Application: TSamplePSCADAApp;
begin
  Application:=TSamplePSCADAApp.Create(nil);
  Application.Title:='Sample PascalSCADA App';
  Application.Run;
  Application.Free;
end.

