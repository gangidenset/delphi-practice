unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Effects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Rectangle_background1: TRectangle;
    Rectangle_background2: TRectangle;
    FloatAnimation_background1: TFloatAnimation;
    FloatAnimation_background2: TFloatAnimation;
    Rectangle_player: TRectangle;
    Rectangle_missile: TRectangle;
    GlowEffect_player: TGlowEffect;
    GlowEffect_missile: TGlowEffect;
    FloatAnimation_player_y: TFloatAnimation;
    FloatAnimation_missile: TFloatAnimation;
    Button_up: TButton;
    Button_down: TButton;
    Button_missile: TButton;
    FloatAnimation_player_x: TFloatAnimation;
    Rectangle_Enm1: TRectangle;
    BitmapAnimation_Enm1: TBitmapAnimation;
    GlowEffect_Enm1: TGlowEffect;
    Rectangle_Enm2: TRectangle;
    BitmapAnimation_Enm2: TBitmapAnimation;
    GlowEffect_Enm2: TGlowEffect;
    Rectangle_Enm3: TRectangle;
    BitmapAnimation_Enm3: TBitmapAnimation;
    GlowEffect_Enm3: TGlowEffect;
    FloatAnimation_Enm1: TFloatAnimation;
    Timer_Enms: TTimer;
    FloatAnimation_Enm2: TFloatAnimation;
    FloatAnimation_Enm3: TFloatAnimation;
    Timer1: TTimer;
    Rectangle_startscene: TRectangle;
    Button1: TButton;
    ColorAnimation3: TColorAnimation;
    Label2: TLabel;
    ColorAnimation4: TColorAnimation;
    procedure FloatAnimation_background2Finish(Sender: TObject);
    procedure FloatAnimation_background1Finish(Sender: TObject);
    procedure Button_upClick(Sender: TObject);
    procedure Button_downClick(Sender: TObject);
    procedure Button_missileClick(Sender: TObject);
    procedure FloatAnimation_missileFinish(Sender: TObject);
    procedure Button_startClick(Sender: TObject);
    procedure FloatAnimation_Enm1Finish(Sender: TObject);
    procedure Timer_EnmsTimer(Sender: TObject);
    procedure FloatAnimation_Enm2Finish(Sender: TObject);
    procedure FloatAnimation_Enm3Finish(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.iPhone4in.fmx IOS}
{$R *.Surface.fmx MSWINDOWS}
{$R *.iPhone55in.fmx IOS}
{$R *.GGlass.fmx ANDROID}

procedure TForm1.Button_startClick(Sender: TObject);
begin
  Rectangle_player.Visible := true;
  Button_up.BringToFront;
  Button_down.BringToFront;
  Button_missile.BringToFront;
  Rectangle_missile.Visible := False;
  Rectangle_startscene.Visible := False;
  Rectangle_Enm1.Visible := False;
  Rectangle_Enm2.Visible := False;
  Rectangle_Enm3.visible := false;
  FloatAnimation_player_x.Start;
end;

procedure TForm1.Button_downClick(Sender: TObject);
begin
  FloatAnimation_player_y.StartValue := Rectangle_player.Position.Y;
  if Rectangle_player.Position.Y + 50 < (Self.Height- Rectangle_player.Height -50) then
    FloatAnimation_player_y.StopValue := Rectangle_player.Position.Y + 50
  else
    FloatAnimation_player_y.StopValue := Self.Height- Rectangle_player.Height -50;
  FloatAnimation_player_y.Start;
end;

procedure TForm1.Button_missileClick(Sender: TObject);
begin
  Button_missile.Enabled := False;
  FloatAnimation_missile.StartValue := Rectangle_player.Position.X + 20;
  Rectangle_missile.Position.Y := Rectangle_player.Position.Y +10;
  Rectangle_missile.Visible := True;
  FloatAnimation_missile.Start;
end;

procedure TForm1.Button_upClick(Sender: TObject);
begin
  FloatAnimation_player_y.StartValue := Rectangle_player.Position.Y;
  if Rectangle_player.Position.Y - 50 > 0 then
    FloatAnimation_player_y.StopValue := Rectangle_player.Position.Y - 50
  else
    FloatAnimation_player_y.StopValue := 0;
  FloatAnimation_player_y.Start;
end;

procedure TForm1.FloatAnimation_background1Finish(Sender: TObject);
begin
  case Round(Rectangle_background1.Position.X) of
    0: begin
      FloatAnimation_background1.StartValue := 0;
      FloatAnimation_background1.StopValue  := -self.width;
    end;
  else
  begin
    FloatAnimation_background1.StartValue := self.Width;
    FloatAnimation_background1.StopValue  := 0;
  end;
  end;
  FloatAnimation_background1.Start;
end;

procedure TForm1.FloatAnimation_background2Finish(Sender: TObject);
begin
  case Round(Rectangle_background2.Position.X) of 0:
  begin
      FloatAnimation_background2.StartValue := 0;
      FloatAnimation_background2.StopValue  := -self.width;
    end;
  else
  begin
    FloatAnimation_background2.StartValue := self.Width;
    FloatAnimation_background2.StopValue  := 0;
  end;
  end;
  FloatAnimation_background2.Start;
end;

procedure TForm1.FloatAnimation_Enm1Finish(Sender: TObject);
begin
  Rectangle_Enm1.Visible := false;
end;

procedure TForm1.FloatAnimation_Enm2Finish(Sender: TObject);
begin
  Rectangle_Enm2.Visible := false;
end;

procedure TForm1.FloatAnimation_Enm3Finish(Sender: TObject);
begin
  Rectangle_Enm3.Visible := false;
end;

procedure TForm1.FloatAnimation_missileFinish(Sender: TObject);
begin
  Button_missile.Enabled := True;
  Rectangle_missile.Visible := false;
end;

procedure TForm1.Timer_EnmsTimer(Sender: TObject);
var
  iEnm: Integer;
  iEnm_y: Integer;

  procedure enm_start(enm: TRectangle; ani: TFloatAnimation; bani: TBitmapAnimation);
  begin
    if not enm.Visible then
    begin
      enm.Position.Y := iEnm_y;
      enm.Visible := true;

      ani.StartValue := Self.Width + 10;
      ani.StopValue  := -enm.Width - 10;
      ani.Start;

      bani.Start;
    end;
  end;

begin
  iEnm := Random(5);
  iEnm_y := Random(Self.Height - 100);

  case iEnm of
    1: enm_start(Rectangle_Enm1, FloatAnimation_Enm1, BitmapAnimation_Enm1);
    2: enm_start(Rectangle_Enm2, FloatAnimation_Enm2, BitmapAnimation_Enm2);
    3: enm_start(Rectangle_Enm3, FloatAnimation_Enm3, BitmapAnimation_Enm3);
  end;
end;


end.

