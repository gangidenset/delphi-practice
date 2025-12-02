unit Exercise1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button1DblClick(Sender: TObject);
    procedure Button1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { private 宣言 }
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Insert(0,'ボタンを押しました');
end;

procedure TForm1.Button1DblClick(Sender: TObject);
begin
  Memo1.Lines.Insert(0,'ダブルクリックしました');
end;

procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Memo1.Lines.Insert(0,'マウスボタンを押しています');
end;

procedure TForm1.Button1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Memo1.Lines.Insert(0,'マウスのボタンを離しました');
end;

end.
