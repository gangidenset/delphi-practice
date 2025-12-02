program Project1_animation;

uses
  System.StartUpCopy,
  FMX.Forms,
  animation in 'animation.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
