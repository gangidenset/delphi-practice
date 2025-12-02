unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('アプリが起動します。')
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ShowMessage('アプリが終了します')
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Memo1.Lines.Insert(0, DateTimeToStr(Now()));
end;

end.
