program Project2;

uses
  System.StartUpCopy,
  FMX.Forms,
  配置 in '配置.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
