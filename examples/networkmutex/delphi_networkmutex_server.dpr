program delphi_networkmutex_server;

uses
  Forms,
  dserver_main in 'dserver_main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
