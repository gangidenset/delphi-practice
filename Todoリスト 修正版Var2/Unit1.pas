unit Unit1;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  System.JSON, System.IOUtils, System.StrUtils, Vcl.ComCtrls;

type
  TCheckItem = record
    ID: Integer;
    Text: string;
    Checked: Boolean;
    Priority: Integer;
    Category: string;
    Deadline: TDateTime;
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
    Label2: TLabel;
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
    LastSelectedIndex: Integer;
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
  Result := TPath.Combine(TPath.GetHomePath, 'CheckList.json');
end;

// Improved PrettyJSON: line-buffer approach + correct escape handling
function PrettyJSON(const S: string): string;
var
  i, indent: Integer;
  c: Char;
  inString: Boolean;
  Line: string;
  SL: TStringList;

  function CountPrecedingBackslashes(pos: Integer): Integer;
  var
    k, cnt: Integer;
  begin
    cnt := 0;
    k := pos - 1;
    while (k >= 1) and (S[k] = '\') do
    begin
      Inc(cnt);
      Dec(k);
    end;
    Result := cnt;
  end;

  procedure FlushLine;
  begin
    if Trim(Line) <> '' then
      SL.Add(Line);
    Line := '';
  end;

begin
  SL := TStringList.Create;
  try
    indent := 0;
    inString := False;
    Line := '';

    for i := 1 to Length(S) do
    begin
      c := S[i];
      case c of
        '"':
          begin
            Line := Line + c;
            if (CountPrecedingBackslashes(i) mod 2) = 0 then
              inString := not inString;
          end;

        '{', '[':
          begin
            Line := Line + c;
            if not inString then
            begin
              FlushLine;
              Inc(indent);
              Line := StringOfChar(' ', indent * 2);
            end;
          end;

        '}', ']':
          begin
            if not inString then
            begin
              FlushLine;
              Dec(indent);
              Line := StringOfChar(' ', indent * 2) + c;
            end
            else
              Line := Line + c;
          end;

        ',':
          begin
            Line := Line + c;
            if not inString then
            begin
              FlushLine;
              Line := StringOfChar(' ', indent * 2);
            end;
          end

        else
          Line := Line + c;
      end;
    end;

    FlushLine;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TForm1.RefreshList;
var
  i: Integer;
  displayText: string;
begin
  CheckListBox1.Items.BeginUpdate;
  try
    CheckListBox1.Items.Clear;
    for i := 0 to High(ItemsBackup) do
    begin
      displayText := Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
        [ItemsBackup[i].Text,
         ItemsBackup[i].Priority,
         ItemsBackup[i].Category,
         DateToStr(ItemsBackup[i].Deadline)]);
      CheckListBox1.Items.Add(displayText);
      CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := ItemsBackup[i].Checked;
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
    // create both arrays to keep format stable
    J.AddPair('items', TJSONArray.Create);
    J.AddPair('completed', TJSONArray.Create);
    TFile.WriteAllText(GetJsonPath, PrettyJSON(J.ToString), TEncoding.UTF8);
    ShowMessage('JSON 初期化完了！');
  finally
    J.Free;
  end;

  SetLength(ItemsBackup, 0);
  Memo_Completed.Clear;
  RefreshList;
end;

procedure TForm1.Button_pathClick(Sender: TObject);
begin
  ShowMessage('保存パス: ' + GetJsonPath);
end;

procedure TForm1.Button_addClick(Sender: TObject);
var
  txt: string;
  idx, id: Integer;
begin
  txt := Trim(Edit_add.Text);
  if txt = '' then Exit;

  // 編集モード
  if CheckListBox1.ItemIndex >= 0 then
  begin
    idx := CheckListBox1.ItemIndex;
    ItemsBackup[idx].Text := txt;
    ItemsBackup[idx].Priority := StrToIntDef(ComboBox_Priority.Text, 1);
    ItemsBackup[idx].Category := ComboBox_Category.Text;
    ItemsBackup[idx].Deadline := dtp_Deadline.Date;
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

  // ▼ 同じ項目をもう一度クリックしたら選択解除
  if (idx = LastSelectedIndex) then
  begin
    CheckListBox1.ItemIndex := -1;
    LastSelectedIndex := -1;

    // 入力欄リセット → 新規追加モードへ戻る
    Edit_add.Clear;
    ComboBox_Priority.ItemIndex := 0;
    ComboBox_Category.ItemIndex := 0;
    dtp_Deadline.Date := Date;
    Exit;
  end;

  // ▼ 通常選択（編集モード）
  LastSelectedIndex := idx;

  if idx < 0 then Exit;

  Edit_add.Text := ItemsBackup[idx].Text;
  ComboBox_Priority.ItemIndex :=
    ComboBox_Priority.Items.IndexOf(IntToStr(ItemsBackup[idx].Priority));
  ComboBox_Category.ItemIndex :=
    ComboBox_Category.Items.IndexOf(ItemsBackup[idx].Category);
  dtp_Deadline.Date := ItemsBackup[idx].Deadline;
end;


procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
  idx: Integer;
  item: TCheckItem;
  memoLine: string;
begin
  idx := CheckListBox1.ItemIndex;
  if idx < 0 then Exit;

  if CheckListBox1.Checked[idx] then
  begin
    item := ItemsBackup[idx];

    memoLine := Format('%s [優先度:%d, カテゴリ:%s, 期限:%s]',
      [item.Text, item.Priority, item.Category, DateToStr(item.Deadline)]);

    Memo_Completed.Lines.Add(memoLine);

    // 未完了リストから削除
    Delete(ItemsBackup, idx, 1);
    RefreshList;
  end;
end;

procedure TForm1.ComboBox_CategoryKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Button_addClick(nil);
  end;
end;

procedure TForm1.ComboBox_PriorityKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Button_addClick(nil);
  end;
end;

procedure TForm1.dtp_DeadlineKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
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
      if (s = '') or
         ContainsText(ItemsBackup[i].Text, s) or
         ContainsText(ItemsBackup[i].Category, s) or
         ContainsText(IntToStr(ItemsBackup[i].Priority), s) or
         ContainsText(DateToStr(ItemsBackup[i].Deadline), s) then
      begin
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
  ArrItems, ArrCompleted: TJSONArray;
  i: Integer;
begin
  Obj := TJSONObject.Create;
  try
    ArrItems := TJSONArray.Create;
    ArrCompleted := TJSONArray.Create;

    // 未完了タスクを構造化して保存
    for i := 0 to High(ItemsBackup) do
    begin
      It := TJSONObject.Create;
      It.AddPair('ID', TJSONNumber.Create(ItemsBackup[i].ID));
      It.AddPair('Text', ItemsBackup[i].Text);
      It.AddPair('Checked', TJSONBool.Create(ItemsBackup[i].Checked));
      It.AddPair('Priority', TJSONNumber.Create(ItemsBackup[i].Priority));
      It.AddPair('Category', ItemsBackup[i].Category);
      It.AddPair('Deadline', DateToStr(ItemsBackup[i].Deadline));
      ArrItems.AddElement(It);
    end;

    // 完了タスクは Memo の各行をそのまま文字列で保存（簡潔）
    for i := 0 to Memo_Completed.Lines.Count - 1 do
    begin
      It := TJSONObject.Create;
      It.AddPair('Text', Memo_Completed.Lines[i]);
      ArrCompleted.AddElement(It);
    end;

    Obj.AddPair('items', ArrItems);
    Obj.AddPair('completed', ArrCompleted);

    TFile.WriteAllText(GetJsonPath, PrettyJSON(Obj.ToString), TEncoding.UTF8);
  finally
    Obj.Free;
  end;
end;

procedure TForm1.LoadData;
var
  Obj, It: TJSONObject;
  ArrItems, ArrCompleted: TJSONArray;
  Json: string;
  i, idx: Integer;
  tmpDate: TDateTime;
begin
  if not TFile.Exists(GetJsonPath) then Exit;

  Json := TFile.ReadAllText(GetJsonPath, TEncoding.UTF8);
  Obj := TJSONObject.ParseJSONValue(Json) as TJSONObject;

  if Obj = nil then Exit;

  try
    // 未完了読み込み
    if Obj.TryGetValue('items', ArrItems) then
    begin
      SetLength(ItemsBackup, 0);
      for i := 0 to ArrItems.Count - 1 do
      begin
        It := ArrItems.Items[i] as TJSONObject;

        if (It.GetValue('Checked') as TJSONBool).AsBoolean then
        begin
          // もし JSON 側に間違って完了を items に突っ込んでいたら Memo に回す
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
          idx := Length(ItemsBackup);
          SetLength(ItemsBackup, idx + 1);

          ItemsBackup[idx].ID := (It.GetValue('ID') as TJSONNumber).AsInt;
          ItemsBackup[idx].Text := It.GetValue('Text').Value;
          ItemsBackup[idx].Checked := False;
          ItemsBackup[idx].Priority := (It.GetValue('Priority') as TJSONNumber).AsInt;
          ItemsBackup[idx].Category := It.GetValue('Category').Value;

          if TryStrToDate(It.GetValue('Deadline').Value, tmpDate) then
            ItemsBackup[idx].Deadline := tmpDate
          else
            ItemsBackup[idx].Deadline := Date;
        end;
      end;
    end;

    // 完了タスク読み込み（completed 配列）
    if Obj.TryGetValue('completed', ArrCompleted) then
    begin
      Memo_Completed.Clear;
      for i := 0 to ArrCompleted.Count - 1 do
      begin
        It := ArrCompleted.Items[i] as TJSONObject;
        Memo_Completed.Lines.Add(It.GetValue('Text').Value);
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
  ln: Integer;
  p1, p2: Integer;
  sPriority, sCategory, sDeadline: string;
begin
  // CaretPos.Y が行のインデックス。念のため範囲チェック
  ln := Memo_Completed.CaretPos.Y;
  if (ln < 0) or (ln >= Memo_Completed.Lines.Count) then Exit;

  line := Memo_Completed.Lines[ln];

  // フォーマット: 'タスク名 [優先度:x, カテゴリ:y, 期限:z]'
  p1 := Pos(' [', line);
  if p1 = 0 then Exit;

  item.Text := Copy(line, 1, p1 - 1);

  // 抜き出しは安全側で処理
  p2 := Pos('優先度:', line);
  if p2 > 0 then
  begin
    sPriority := Copy(line, p2 + Length('優先度:'), Pos(', カテゴリ:', line) - (p2 + Length('優先度:')));
    item.Priority := StrToIntDef(Trim(sPriority), 1);
  end
  else
    item.Priority := 1;

  p2 := Pos('カテゴリ:', line);
  if p2 > 0 then
    sCategory := Copy(line, p2 + Length('カテゴリ:'), Pos(', 期限:', line) - (p2 + Length('カテゴリ:')))
  else
    sCategory := '';
  item.Category := Trim(sCategory);

  p2 := Pos('期限:', line);
  if p2 > 0 then
  begin
    sDeadline := Copy(line, p2 + Length('期限:'), Length(line));
    // 余計な ] を削除
    if sDeadline.EndsWith(']') then
      Delete(sDeadline, Length(sDeadline), 1);
    sDeadline := Trim(sDeadline);
  end
  else
    sDeadline := '';


  if not TryStrToDate(sDeadline, item.Deadline) then
    item.Deadline := Date;

  item.Checked := False;

  newItemIdx := Length(ItemsBackup);
  SetLength(ItemsBackup, newItemIdx + 1);
  ItemsBackup[newItemIdx] := item;

  Memo_Completed.Lines.Delete(ln);
  RefreshList;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // UI 初期化
  ComboBox_Priority.Items.Clear;
  ComboBox_Priority.Items.Add('1');
  ComboBox_Priority.Items.Add('2');
  ComboBox_Priority.Items.Add('3');
  ComboBox_Priority.Items.Add('4');
  ComboBox_Priority.Items.Add('5');
  ComboBox_Priority.ItemIndex := 0;

  ComboBox_Category.Items.Clear;
  ComboBox_Category.Items.Add('仕事系');
  ComboBox_Category.Items.Add('プライベート系');
  ComboBox_Category.Items.Add('学習/自己啓発系');
  ComboBox_Category.Items.Add('その他');
  ComboBox_Category.ItemIndex := 0;

  Memo_Completed.ReadOnly := True;

  LoadData;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveData;
end;

end.

