unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.Controls,
  KP.ListString; // ← 追加

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    ListStringQ, ListStringA: TListString;
    procedure ListStringToMemo1;

    // ★ Ollama 呼び出し関数
    function RunCmdAndGetOutput(const ACmd: string; const AWorkDir: string = ''): string;
    function AskOllama(const Prompt: string): string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{---------------------------------------------}
{   チャット履歴を Memo1 に表示               }
{---------------------------------------------}
procedure TForm1.ListStringToMemo1;
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Text := ChatQAToString(ListStringQ, ListStringA);
  Memo1.Lines.EndUpdate;

  // 最下行を表示
  Memo1.Perform(EM_LINESCROLL, 0, Memo1.Lines.Count);
end;

{---------------------------------------------}
{   起動時                                     }
{---------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo2.Lines.TrailingLineBreak := False;
  ListStringQ := TListString.Create;
  ListStringA := TListString.Create;
end;

{---------------------------------------------}
{   終了時                                     }
{---------------------------------------------}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ListStringQ.Free;
  ListStringA.Free;
end;

{---------------------------------------------}
{   Button1 → Ollama に質問送信                }
{---------------------------------------------}
procedure TForm1.Button1Click(Sender: TObject);
var
  Q, A: string;
begin
  Q := Memo2.Text;
  if Q = '' then Exit;

  // 質問を追加
  ListStringQ.Add('質問:' + Q);

  // Ollama に送信
  A := AskOllama(Q);

  // 回答追加
  ListStringA.Add(A);

  // 画面へ反映
  ListStringToMemo1;

  Memo2.Clear;
end;

{---------------------------------------------}
{   ★ 外部コマンド実行（標準出力取得）          }
{---------------------------------------------}
function TForm1.RunCmdAndGetOutput(const ACmd: string; const AWorkDir: string = ''): string;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Buffer: array[0..2047] of AnsiChar;
  BytesRead: DWORD;
  WorkDir: PChar;
begin
  Result := '';

  Security.nLength := SizeOf(Security);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;

  if AWorkDir = '' then
    WorkDir := nil
  else
    WorkDir := PChar(AWorkDir);

  CreatePipe(ReadPipe, WritePipe, @Security, 0);
  try
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.hStdOutput := WritePipe;
    StartupInfo.hStdError := WritePipe;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES;

    if CreateProcess(
      nil,
      PChar(ACmd),
      nil, nil,
      True,
      CREATE_NO_WINDOW,
      nil,
      WorkDir,
      StartupInfo,
      ProcessInfo
    ) then
    begin
      CloseHandle(WritePipe);

      while ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil)
        and (BytesRead > 0) do
        Result := Result + String(UTF8String(Copy(Buffer, 1, BytesRead)));

      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    CloseHandle(ReadPipe);
  end;
end;


{---------------------------------------------}
{   ★ Ollama 実行                              }
{---------------------------------------------}
function TForm1.AskOllama(const Prompt: string): string;
var
  ExePath, Cmd: string;
begin
  // アプリと同階層の ollama.exe を探す
  ExePath := ExtractFilePath(ParamStr(0)) + 'ollama.exe';

  if not FileExists(ExePath) then
    Exit('Error: ollama.exe が見つかりません');

  // llama3.1 を例として使用
  Cmd := Format('"%s" run llama3.1 "%s"', [ExePath, Prompt]);

  Result := RunCmdAndGetOutput(Cmd);
  Result := Trim(Result);
end;

end.

