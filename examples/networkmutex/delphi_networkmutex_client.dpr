program delphi_networkmutex_client;

uses
  Forms,
  dclient_main in 'dclient_main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
