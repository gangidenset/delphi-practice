unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  System.JSON, System.IOUtils, System.StrUtils, Vcl.ComCtrls, VclTee.TeeGDIPlus,
  VCLTee.TeEngine, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart;

type
  TCheckItem = record
    ID: Integer;
    Text: string;
    Checked: Boolean;
    priority: Integer;
    Category: String;
    Deadline: TDatetime;
  end;

  TForm1 = class(TForm)
    Edit_add: TEdit;
    Button_add: TButton;
    Button_delete: TButton;
    CheckListBox1: TCheckListBox;
    Edit_search: TEdit;
    Button_reset: TButton;
    Button_path: TButton;
    Label_search: TLabel;
    Label_add: TLabel;
    Label_checklist: TLabel;
    Label_priority: TLabel;
    Label_category: TLabel;
    ComboBox_Priority: TComboBox;
    ComboBox_Category: TComboBox;
    dtp_Deadline: TDateTimePicker;
    Label1: TLabel;
    Memo_Completed: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_addClick(Sender: TObject);
    procedure Button_deleteClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Edit_searchChange(Sender: TObject);
    procedure Button_resetClick(Sender: TObject);
    procedure Button_pathClick(Sender: TObject);
    procedure Edit_addKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox_PriorityKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox_CategoryKeyPress(Sender: TObject; var Key: Char);
    procedure dtp_DeadlineKeyPress(Sender: TObject; var Key: Char);
    procedure Memo_CompletedDblClick(Sender: TObject);
  private
    ItemsBackup: TArray<TCheckItem>;
    procedure RefreshList;
    procedure SaveData;
    procedure LoadData;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetJsonPath: string;
begin
  Result := TPath.Combine(GetHomePath, 'CheckList.json');
end;

function PrettyJSON(const S: string): string;
var
  i, indent: Integer;
  c: Char;
  inString: Boolean;
begin
  Result := '';
  indent := 0;
  inString := False;

  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '"':
        begin
          Result := Result + c;
          if (i = 1) or (S[i - 1] <> '\') then
            inString := not inString;
        end;

      '{', '[':
        begin
          Result := Result + c;
          if not inString then
          begin
            Inc(indent);
            Result := Result + sLineBreak + StringOfChar(' ', indent * 2);
          end;
        end;

      '}', ']':
        begin
          if not inString then
          begin
            Dec(indent);
            Result := Result + sLineBreak + StringOfChar(' ', indent * 2) + c;
          end
          else
            Result := Result + c;
        end;

      ',':
        begin
          Result := Result + c;
          if not inString then
            Result := Result + sLineBreak + StringOfChar(' ', indent * 2);
        end;

      else
        Result := Result + c;
    end;
  end;
end;

procedure TForm1.RefreshList;
var
  i: Integer;
begin
  CheckListBox1.Items.BeginUpdate;
  try
    CheckListBox1.Items.Clear;
    for i := 0 to High(ItemsBackup) do
    begin
    CheckListBox1.Items.Add(
      Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
        [ItemsBackup[i].Text,
         ItemsBackup[i].Priority,
         ItemsBackup[i].Category,
         DateToStr(ItemsBackup[i].Deadline)])
    );
    CheckListBox1.Checked[i] := ItemsBackup[i].Checked;
    end;
  finally
    CheckListBox1.Items.EndUpdate;
  end;
end;

procedure TForm1.Button_resetClick(Sender: TObject);
var
  J: TJSONObject;
begin
  J := TJSONObject.Create;
  try
    J.AddPair('items', TJSONArray.Create);
    TFile.WriteAllText(GetJsonPath, PrettyJSON(J.ToString), TEncoding.UTF8);
    ShowMessage('JSON 初期化完了！');
  finally
    J.Free;
  end;

  SetLength(ItemsBackup, 0);
  RefreshList;
end;

procedure TForm1.Button_pathClick(Sender: TObject);
begin
  ShowMessage('保存パス: ' + GetJsonPath);
end;

procedure TForm1.Button_addClick(Sender: TObject);
var
  txt: string;
  idx, id, i: Integer;
begin
  txt := Trim(Edit_add.Text);
  if txt = '' then Exit;

// 編集モード
if CheckListBox1.ItemIndex >= 0 then
begin
  id := ItemsBackup[CheckListBox1.ItemIndex].ID;

  ItemsBackup[CheckListBox1.ItemIndex].Text :=
    txt;
  ItemsBackup[CheckListBox1.ItemIndex].Priority :=
    StrToIntDef(ComboBox_Priority.Text, 1);
  ItemsBackup[CheckListBox1.ItemIndex].Category :=
    ComboBox_Category.Text;
  ItemsBackup[CheckListBox1.ItemIndex].Deadline :=
    dtp_Deadline.DAte;
  RefreshList;
end

  else
  begin
    // 新規追加
    id := TThread.GetTickCount;

    idx := Length(ItemsBackup);
    SetLength(ItemsBackup, idx + 1);

    ItemsBackup[idx].ID := id;
    ItemsBackup[idx].Text := txt;
    ItemsBackup[idx].Checked := False;
    ItemsBackup[idx].Priority := StrToIntDef(ComboBox_Priority.Text, 1);
    ItemsBackup[idx].Category := ComboBox_Category.Text;
    ItemsBackup[idx].Deadline := dtp_Deadline.Date;

    RefreshList;
  end;

  Edit_add.Clear;
  CheckListBox1.ItemIndex := -1;
end;

procedure TForm1.Button_deleteClick(Sender: TObject);
var
  i, sel, p: Integer;
  tmp: TArray<TCheckItem>;
begin
  sel := CheckListBox1.ItemIndex;
  if sel < 0 then Exit;

  SetLength(tmp, 0);
  p := 0;

  for i := 0 to High(ItemsBackup) do
    if i <> sel then
    begin
      SetLength(tmp, p + 1);
      tmp[p] := ItemsBackup[i];
      Inc(p);
    end;

  ItemsBackup := tmp;
  RefreshList;
end;

procedure TForm1.CheckListBox1Click(Sender: TObject);
var
  idx: Integer;
begin
  idx := CheckListBox1.ItemIndex;
  if idx < 0 then Exit;

  // テキスト
  Edit_add.Text := ItemsBackup[idx].Text;

  // 優先度
  ComboBox_Priority.ItemIndex :=
    ComboBox_Priority.Items.IndexOf(IntToStr(ItemsBackup[idx].Priority));

  // カテゴリ
  ComboBox_Category.ItemIndex :=
    ComboBox_Category.Items.IndexOf(ItemsBackup[idx].Category);

  // 期限
  dtp_Deadline.Date := ItemsBackup[idx].Deadline;

end;


procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
  idx: Integer;
  item: TCheckItem;
begin
  idx := CheckListBox1.ItemIndex;
  if idx < 0 then Exit;

  // チェックされたタスクを TMemo に移動
  if CheckListBox1.Checked[idx] then
  begin
    item := ItemsBackup[idx];

    // Memo に追加
    Memo_Completed.Lines.Add(
      Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
        [item.Text, item.Priority, item.Category, DateToStr(item.Deadline)])
    );

    // チェックリストから削除
    Delete(ItemsBackup, idx, 1);
    RefreshList;
  end;
end;




procedure TForm1.ComboBox_CategoryKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0; // Enterの「ピンッ」という音防止
    Button_addClick(nil);
  end;
end;

procedure TForm1.ComboBox_PriorityKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0; // Enterの「ピンッ」という音防止
    Button_addClick(nil);
  end;
end;

procedure TForm1.dtp_DeadlineKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0; // Enterの「ピンッ」という音防止
    Button_addClick(nil);
  end;
end;

procedure TForm1.Edit_addKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Button_addClick(nil);
  end;
end;

procedure TForm1.Edit_searchChange(Sender: TObject);
var
  s: string;
  i: Integer;
  displayText: string;
begin
  s := Trim(Edit_search.Text);

  CheckListBox1.Items.BeginUpdate;
  try
    CheckListBox1.Items.Clear;

    for i := 0 to High(ItemsBackup) do
    begin
      // --- 検索対象を Text / Category / Priority に拡張 ---
      if (s = '') or
         ContainsText(ItemsBackup[i].Text, s) or
         ContainsText(ItemsBackup[i].Category, s) or
         ContainsText(IntToStr(ItemsBackup[i].Priority), s) or
         ContainsText(DateToStr(ItemsBackup[i].Deadline), s) then
      begin
        // --- RefreshList と同じ形式で表示 ---
        displayText := Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
          [ItemsBackup[i].Text,
           ItemsBackup[i].Priority,
           ItemsBackup[i].Category,
           DateToStr(ItemsBackup[i].Deadline)]);

        CheckListBox1.Items.Add(displayText);
        CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := ItemsBackup[i].Checked;
      end;
    end;
  finally
    CheckListBox1.Items.EndUpdate;
  end;
end;


procedure TForm1.SaveData;
var
  Obj, It: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
begin
  Obj := TJSONObject.Create;
  try
    Arr := TJSONArray.Create;

    for i := 0 to High(ItemsBackup) do
    begin
      It := TJSONObject.Create;
      It.AddPair('ID', TJSONNumber.Create(ItemsBackup[i].ID));
      It.AddPair('Text', ItemsBackup[i].Text);
      It.AddPair('Checked', TJSONBool.Create(ItemsBackup[i].Checked));
      It.AddPair('Priority', TJSONNumber.Create(ItemsBackup[i].Priority));
      It.AddPair('Category', ItemsBackup[i].Category);
      It.AddPair('Deadline', DateToStr(ItemsBackup[i].Deadline));
      Arr.AddElement(It);
    end;

    Obj.AddPair('items', Arr);
    TFile.WriteAllText(GetJsonPath, PrettyJSON(Obj.ToString), TEncoding.UTF8);
  finally
    Obj.Free;
  end;
end;

procedure TForm1.LoadData;
var
  Obj, It: TJSONObject;
  Arr: TJSONArray;
  Json: string;
  i, idx: Integer;
begin
  if not TFile.Exists(GetJsonPath) then Exit;

  Json := TFile.ReadAllText(GetJsonPath, TEncoding.UTF8);
  Obj := TJSONObject.ParseJSONValue(Json) as TJSONObject;

  if Obj = nil then Exit;

  try
    Arr := Obj.GetValue('items') as TJSONArray;
    SetLength(ItemsBackup, 0); // 最初は空にして、チェック済みは TMemo に移す

    for i := 0 to Arr.Count - 1 do
    begin
      It := Arr.Items[i] as TJSONObject;

      if (It.GetValue('Checked') as TJSONBool).AsBoolean then
      begin
        // 完了タスク → TMemo に追加
        Memo_Completed.Lines.Add(
          Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
            [It.GetValue('Text').Value,
             (It.GetValue('Priority') as TJSONNumber).AsInt,
             It.GetValue('Category').Value,
             It.GetValue('Deadline').Value])
        );
      end
      else
      begin
        // 未完了タスク → ItemsBackup に追加
        idx := Length(ItemsBackup);
        SetLength(ItemsBackup, idx +1);
        ItemsBackup[i].ID := (It.GetValue('ID') as TJSONNumber).AsInt;
        ItemsBackup[i].Text := It.GetValue('Text').Value;
        ItemsBackup[i].Checked := False;
        ItemsBackup[i].Priority := (It.GetValue('Priority') as TJSONNumber).AsInt;
        ItemsBackup[i].Category := It.GetValue('Category').Value;
        ItemsBackup[i].Deadline := StrToDate(It.GetValue('Deadline').Value);
      end;
    end;
  finally
    Obj.Free;
  end;

  RefreshList;
end;

procedure TForm1.Memo_CompletedDblClick(Sender: TObject);
var
  line: string;
  newItemIdx: Integer;
  item: TCheckItem;
begin
  if Memo_Completed.SelText = '' then Exit;

  line := Memo_Completed.Lines[Memo_Completed.CaretPos.Y];

  // TMemo の文字列からタスク情報を復元
  // フォーマット: 'タスク名 [優先度:x, カテゴリ:y, 期限:z]'
  item.Text := Copy(line, 1, Pos(' [', line)-1);

  item.Priority := StrToIntDef(
    Copy(line, Pos('優先度:', line)+4, Pos(', カテゴリ:', line)-Pos('優先度:', line)-4), 1);

  item.Category := Copy(line, Pos('カテゴリ:', line)+5, Pos(', 期限:', line)-Pos('カテゴリ:', line)-5);

  item.Deadline := StrToDateDef(
    Copy(line, Pos('期限:', line)+3, Length(line)), Date);

  item.Checked := False;

  // ItemsBackup に戻す
  newItemIdx := Length(ItemsBackup);
  SetLength(ItemsBackup, newItemIdx+1);
  ItemsBackup[newItemIdx] := item;

  // TMemo から削除
  Memo_Completed.Lines.Delete(Memo_Completed.CaretPos.Y);

  // リストを更新
  RefreshList;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadData;
  ComboBox_Priority.Items.Clear;
  ComboBox_Priority.Items.Add('1');
  ComboBox_Priority.Items.Add('2');
  ComboBox_Priority.Items.Add('3');
  ComboBox_Priority.Items.Add('4');
  ComboBox_Priority.Items.Add('5');
  ComboBox_Priority.ItemIndex := 0; // デフォルト1
  ComboBox_Category.Items.Clear;
  ComboBox_Category.Items.Add('仕事系');
  ComboBox_Category.Items.Add('プライベート系');
  ComboBox_Category.Items.Add('学習/自己啓発系');
  ComboBox_Category.Items.Add('その他');
  ComboBox_Category.ItemIndex := 0; // デフォルト1
  Memo_Completed.Visible := True;
  Memo_Completed.ReadOnly := True;
  Memo_Completed.ScrollBars := ssVertical;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveData;
end;

end.

