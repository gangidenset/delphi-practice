unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
   Dicevalue: integer;
begin
   Dicevalue := Random(6) + 1;
   Memo1.Lines.Insert(0, Dicevalue.ToString);

   if Dicevalue =1 then
    Memo1.Lines.Insert(0,'1Ç™èoÇ‹ÇµÇΩ');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Dicevalue: Integer;
begin
  Memo1.Lines.Insert(0,'');
  Dicevalue := Random(6) + 1;

  while Dicevalue <> 1 do
  begin
    Memo1.Lines.Insert(0, Dicevalue.ToString + 'ÇÕÇ∏ÇÍ');

    Dicevalue := Random(6) + 1;
  end;

  Memo1.Lines.Insert(0, Dicevalue.ToString + 'Ç†ÇΩÇË');
end;

end.
