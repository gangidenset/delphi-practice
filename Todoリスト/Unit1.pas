unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  System.JSON, System.IOUtils, System.StrUtils, Vcl.ExtCtrls;

type
  TCheckItem = record
    ID: integer;
    Text: string;
    Checked: Boolean;
  end;

type
  TFadeItem = record
    Index: Integer;
    Alpha: Byte;  // 255 → 0 に下げていく
  end;

  TForm1 = class(TForm)
    ListBox1: TListBox;
    Edit_add: TEdit;
    Button_add: TButton;
    Button_delete: TButton;
    CheckListBox1: TCheckListBox;
    Button_path: TButton;
    Edit_search: TEdit;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    Button_reset: TButton;
    procedure Button_addClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button_deleteClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_pathClick(Sender: TObject);
    procedure Edit_searchChange(Sender: TObject);
    procedure Edit_addKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_searchKeyPress(Sender: TObject; var Key: Char);
    procedure Button_resetClick(Sender: TObject);
  private
    procedure SaveDate;
    procedure LoadDate;
    procedure RefreshLists; // バックアップから表示を更新
  private
    ListBoxBackup: TStringList;
    CheckListBoxBackup: TArray<TCheckItem>;
    FadingItems: array of TFadeItem;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetIniFullPath: string;
begin
  Result := TPath.Combine(GetHomePath, 'ListBox.json');
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


procedure TForm1.Button_pathClick(Sender: TObject);
begin
  ShowMessage('保存ファイルのパス: ' + GetIniFullPath);
end;

procedure TForm1.RefreshLists;
var
  i: Integer;
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
    for i := 0 to ListBoxBackup.Count - 1 do
      ListBox1.Items.AddObject(ListBoxBackup[i], ListBoxBackup.Objects[i]);
  finally
    ListBox1.Items.EndUpdate;
  end;

  CheckListBox1.Items.BeginUpdate;
  try
    CheckListBox1.Items.Clear;
    for i := 0 to Length(CheckListBoxBackup) - 1 do
    begin
      CheckListBox1.Items.Add('');
      CheckListBox1.Checked[i] := CheckListBoxBackup[i].Checked;
    end;
  finally
    CheckListBox1.Items.EndUpdate;
  end;
end;

procedure TForm1.Button_resetClick(Sender: TObject);
  var
  JsonObj: TJSONObject;
  FilePath: string;
begin
  // 保存先パス
  FilePath := GetIniFullPath;

  // 新しい JSON を作る
  JsonObj := TJSONObject.Create;
  try
    // 必要なら初期値を入れる
    JsonObj.AddPair('version', '1.0');
    JsonObj.AddPair('items', TJSONArray.Create);

    // ファイルとして保存
    TFile.WriteAllText(FilePath, PrettyJSON(JsonObj.ToString), TEncoding.UTF8);

    ShowMessage('JSON を初期化！');
  finally
    JsonObj.Free;
  end;
end;

procedure TForm1.Button_addClick(Sender: TObject);
var
  oldText, newText: string;
  idx, id, i: Integer;
begin
  newText := Trim(Edit_add.Text);
  if newText = '' then Exit;

  // 編集モード
  if (ListBox1.ItemIndex <> -1) or (CheckListBox1.ItemIndex <> -1) then
  begin
    // 配列が空なら編集不可
    if Length(CheckListBoxBackup) = 0 then Exit;

    // 編集対象
    if ListBox1.ItemIndex <> -1 then
      id := Integer(ListBox1.Items.Objects[ListBox1.ItemIndex])
    else
      id := CheckListBoxBackup[CheckListBox1.ItemIndex].ID;

    // 表示リスト更新
    if ListBox1.ItemIndex <> -1 then
      ListBox1.Items[ListBox1.ItemIndex] := newText;
    if CheckListBox1.ItemIndex <> -1 then
      CheckListBox1.Items[CheckListBox1.ItemIndex] := '';

    // ListBoxBackup 更新
    for i := 0 to ListBoxBackup.Count - 1 do
      if Integer(ListBoxBackup.Objects[i]) = id then
        ListBoxBackup[i] := newText;

    // CheckListBoxBackup 更新
    for i := 0 to High(CheckListBoxBackup) do
      if CheckListBoxBackup[i].ID = id then
        CheckListBoxBackup[i].Text := newText;
  end
  else
  begin
    // 新規追加
    // 新規IDを発行
    id := TThread.GetTickCount; // か、連番管理でもOK

    ListBoxBackup.AddObject(newText, TObject(id));

    idx := Length(CheckListBoxBackup);
    SetLength(CheckListBoxBackup, idx + 1);
    CheckListBoxBackup[idx].ID := id;
    CheckListBoxBackup[idx].Text := newText;
    CheckListBoxBackup[idx].Checked := False;

    // 表示リストも更新
    RefreshLists;
  end;

  Edit_add.Clear;
  ListBox1.ItemIndex := -1;
  CheckListBox1.ItemIndex := -1;
end;

procedure TForm1.Button_deleteClick(Sender: TObject);
var
  s: string;
  i: Integer;
  NewCheckList: TArray<TCheckItem>;
  j: Integer;
begin
  if ListBox1.ItemIndex > -1 then
    s := ListBox1.Items[ListBox1.ItemIndex]
  else if CheckListBox1.ItemIndex > -1 then
    s := CheckListBox1.Items[CheckListBox1.ItemIndex]
  else Exit;

  // バックアップから削除
  i := ListBoxBackup.IndexOf(s);
  if i >= 0 then
    ListBoxBackup.Delete(i);

  SetLength(NewCheckList, 0);
  for i := 0 to High(CheckListBoxBackup) do
  begin
    if CheckListBoxBackup[i].Text <> s then
    begin
      j := Length(NewCheckList);
      SetLength(NewCheckList, j + 1);
      NewCheckList[j] := CheckListBoxBackup[i];
    end;
  end;
  CheckListBoxBackup := Copy(NewCheckList, 0, Length(NewCheckList));

  // 表示リスト更新
  RefreshLists;
end;

procedure TForm1.CheckListBox1Click(Sender: TObject);
begin
  if CheckListBox1.ItemIndex > -1 then
    Edit_add.Text := CheckListBox1.Items[CheckListBox1.ItemIndex];
end;

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
  txt: string;
  i: Integer;
begin
  if CheckListBox1.ItemIndex < 0 then Exit;

  txt := CheckListBox1.Items[CheckListBox1.ItemIndex];

  // Backup を Text で探す
  for i := 0 to High(CheckListBoxBackup) do
    if CheckListBoxBackup[i].Text = txt then
    begin
      CheckListBoxBackup[i].Checked := CheckListBox1.Checked[CheckListBox1.ItemIndex];
      Break;
    end;

  // ← あなたのフェードアウト処理はここに付けるのでOK
end;



procedure TForm1.CheckListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LB: TCheckListBox;
begin
  LB := Control as TCheckListBox;
  LB.Canvas.FillRect(Rect);
  if LB.Checked[Index] then
    LB.Canvas.Font.Color := clGrayText
  else
    LB.Canvas.Font.Color := clWindowText;

  LB.Canvas.TextOut(Rect.Left + 20, Rect.Top, LB.Items[Index]);
end;

procedure TForm1.Edit_addKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Enterキー
  begin
    Key := #0;       // ビープ音防止
    Button_add.Click; // 追加処理を呼ぶ
  end;
end;

procedure TForm1.Edit_searchChange(Sender: TObject);
var
  searchText: string;
  i: Integer;
begin
  searchText := Trim(Edit_search.Text);

  // ListBox
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;

    if searchText = '' then
    begin
      // 検索文字が空なら全部表示
      for i := 0 to ListBoxBackup.Count - 1 do
        ListBox1.Items.Add(ListBoxBackup[i]);
    end
    else
    begin
      for i := 0 to ListBoxBackup.Count - 1 do
        if ContainsText(ListBoxBackup[i], searchText) then
          ListBox1.Items.Add(ListBoxBackup[i]);
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;

  // CheckListBox
  CheckListBox1.Items.BeginUpdate;
  try
    CheckListBox1.Items.Clear;

    if searchText = '' then
    begin
      for i := 0 to Length(CheckListBoxBackup) - 1 do
      begin
        CheckListBox1.Items.Add(CheckListBoxBackup[i].Text);
        CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := CheckListBoxBackup[i].Checked;
      end;
    end
    else
    begin
      for i := 0 to Length(CheckListBoxBackup) - 1 do
        if ContainsText(CheckListBoxBackup[i].Text, searchText) then
        begin
          CheckListBox1.Items.Add(CheckListBoxBackup[i].Text);
          CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := CheckListBoxBackup[i].Checked;
        end;
    end;
  finally
    CheckListBox1.Items.EndUpdate;
  end;
end;


procedure TForm1.Edit_searchKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then  // Enterキー
  begin
    Key := #0;       // ビープ音防止
    Edit_searchChange(Sender);  // 検索処理を呼び出す
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
    Edit_add.Text := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TForm1.SaveDate;
var
  JSONObject, CheckObj, ItemObj: TJSONObject;
  ListArray, CheckArray: TJSONArray;
  filename: string;
  i: Integer;
begin
  filename := GetIniFullPath;
  JSONObject := TJSONObject.Create;
  try
    ListArray := TJSONArray.Create;
    for i := 0 to ListBoxBackup.Count - 1 do
    begin
      ItemObj := TJSONObject.Create;
      ItemObj.AddPair('ID', TJSONNumber.Create(Integer(ListBoxBackup.Objects[i])));
      ItemObj.AddPair('Text', ListBoxBackup[i]);
      ListArray.AddElement(ItemObj);
    end;
    JSONObject.AddPair('ListBox', ListArray);

    CheckArray := TJSONArray.Create;
    for i := 0 to Length(CheckListBoxBackup) - 1 do
    begin
      CheckObj := TJSONObject.Create;
      CheckObj.AddPair('ID', TJSONNumber.Create(CheckListBoxBackup[i].ID));
      CheckObj.AddPair('Text', CheckListBoxBackup[i].Text);
      CheckObj.AddPair('Checked', TJSONBool.Create(CheckListBoxBackup[i].Checked));
      CheckArray.AddElement(CheckObj);
    end;
    JSONObject.AddPair('CheckListBox', CheckArray);

    TFile.WriteAllText(filename, PrettyJSON(JSONObject.ToString), TEncoding.UTF8);
  finally
    JSONObject.Free;
  end;
end;

procedure TForm1.LoadDate;
var
  JSONObject, CheckObj, ItemObj: TJSONObject;
  JSONText: string;
  ListArray, CheckArray: TJSONArray;
  filename: string;
  i, id: Integer;
begin
  filename := GetIniFullPath;
  if not TFile.Exists(filename) then Exit;

  JSONText := TFile.ReadAllText(filename, TEncoding.UTF8);
  JSONObject := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;

  if JSONObject = nil then
  begin
    ShowMessage('JSON が壊れてたから初期化するで！');
    Button_resetClick(nil);
    Exit;
  end;

  try
    //ListBox読込
    ListArray := JSONObject.GetValue('ListBox') as TJSONArray;
    ListBoxBackup := TStringList.Create;

    for i := 0 to ListArray.Count - 1 do
    begin
      ItemObj := ListArray.Items[i] as TJSONObject;
      id := (ItemObj.GetValue('ID') as TJSONNumber).AsInt;

      ListBoxBackup.AddObject(
        ItemObj.GetValue('Text').Value,
        TObject(id)
      );
    end;

    //CheckListBox読込
    CheckArray := JSONObject.GetValue('CheckListBox') as TJSONArray;
    SetLength(CheckListBoxBackup, CheckArray.Count);

    for i := 0 to CheckArray.Count - 1 do
    begin
      CheckObj := CheckArray.Items[i] as TJSONObject;
      CheckListBoxBackup[i].ID      := (CheckObj.GetValue('ID') as TJSONNumber).AsInt;
      CheckListBoxBackup[i].Text    := CheckObj.GetValue('Text').Value;
      CheckListBoxBackup[i].Checked := (CheckObj.GetValue('Checked') as TJSONBool).AsBoolean;
    end;

  finally
    JSONObject.Free;
  end;

  RefreshLists;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadDate;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveDate;
  ListBoxBackup.Free;
end;

end.

