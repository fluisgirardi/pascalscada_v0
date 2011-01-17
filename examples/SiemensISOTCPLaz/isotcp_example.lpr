program isotcp_example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, LResources, runtimetypeinfocontrols, dm, pascalscada
  { you can add units after this };

{$IFDEF WINDOWS}{$R isotcp_example.rc}{$ENDIF}

begin
  //{$I isotcp_example.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.

