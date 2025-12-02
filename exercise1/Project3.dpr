program Project3;

uses
  System.StartUpCopy,
  FMX.Forms,
  Exercise1 in 'Exercise1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
