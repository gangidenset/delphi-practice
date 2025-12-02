unit animation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Ani,
  FMX.Filter.Effects;

type
  TForm1 = class(TForm)
    Switch1: TSwitch;
    GlowEffect1: TGlowEffect;
    Image1: TImage;
    Button1: TButton;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    RippleEffect1: TRippleEffect;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    procedure FloatAnimation2Finish(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation2Finish(Sender: TObject);
begin
  Button1.Position.X := 50;
  Button1.Opacity := 1;
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  FloatAnimation3.Enabled := Switch1.IsChecked;
end;

end.
