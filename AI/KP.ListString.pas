        unit KP.ListString;
(*
TListString: 改行を含む文字列を管理するTList<string>を内包するクラス

LICENSE
Copyright (c) 2025 Yuzuru Kato
Released under the MIT license
http://opensource.org/licenses/mit-license.php

procedure ListStringCopy(FromListString, ToListString: TListString); // TListStringのコピー
procedure ListStringQAMarge(ATextQ, ATextA, LText: TListString);     // 質問と回答を交互に並べて一つにまとめる
function ChatQAToString(ATextQA: TListString): string;               // まとめられた質問と回答を文字列に出力
function ChatQAToString(ATextQ, ATextA: TListString): string;        // 個別の質問と回答を文字列に出力
function ChatQAToHTML(ATextQ, ATextA: TListString): string;          // 個別の質問と回答をチャットバブル風のHTML文字列に出力
*)

interface

uses
  System.Generics.Collections;

type
  TListString = class(TObject)
  private
    FStrings:TList<String>;
    function StringsGet(AIndex: Integer): string;
    procedure StringsPut(AIndex: Integer; const Value: string);
    function CountGet: integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AText:string);
    procedure Delete(AIndex: integer);
    property Strings[AIndex: integer]: string read StringsGet write StringsPut; default;
    property Count: integer read CountGet;
  end;

procedure ListStringCopy(FromListString, ToListString: TListString);    // TListStringのコピー
procedure ListStringQAMarge(ATextQ, ATextA, LText: TListString);        // 質問と回答を交互に並べて一つにまとめる
function ChatQAToString(ATextQA: TListString): string; overload;        // まとめられた質問と回答を文字列に出力
function ChatQAToString(ATextQ, ATextA: TListString): string; overload; // 個別の質問と回答を文字列に出力
function ChatQAToHTML(ATextQ, ATextA: TListString): string;             // 個別の質問と回答をチャットバブル風のHTML文字列に出力

implementation

uses
  System.Classes, System.SysUtils,
  System.RegularExpressions;

{ TListString }

constructor TListString.Create;
begin
  FStrings:=TList<String>.Create;
end;

destructor TListString.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TListString.Clear;
begin
  FStrings.Clear;
end;

function TListString.CountGet: integer;
begin
  Result:=FStrings.Count;
end;

function TListString.StringsGet(AIndex: Integer): string;
begin
  if (0<=AIndex)and(AIndex<FStrings.Count) then begin
    Result:=FStrings[AIndex];
  end else begin
    Result:='';
  end;
end;

procedure TListString.StringsPut(AIndex: Integer; const Value: string);
begin
  if (0<=AIndex)and(AIndex<FStrings.Count) then begin
    FStrings[AIndex]:=Value;
  end;
end;

procedure TListString.Add(AText: string);
begin
  FStrings.Add(AText);
end;

procedure TListString.Delete(AIndex: integer);
begin
  if (0<=AIndex)and(AIndex<FStrings.Count) then begin
    FStrings.Delete(AIndex);
  end;
end;

// TListStringのサポート関数

procedure ListStringCopy(FromListString, ToListString: TListString);
var
  s:string;
begin
  ToListString.Clear;
  for s in FromListString.FStrings do begin
    ToListString.FStrings.Add(s);
  end;
end;

procedure ListStringQAMarge(ATextQ, ATextA, LText: TListString);
var
  i,cq,ca:integer;
begin
  cq:=ATextQ.Count;
  ca:=ATextA.Count;
  LText.Clear;
  if (cq>0)and(cq>ca) then begin
    for i:=0 to ca-1 do begin
      LText.Add(ATextQ[i]);
      LText.Add(ATextA[i]);
    end;
    LText.Add(ATextQ[ca]);
  end;
end;

function ChatQAToString(ATextQA: TListString): string; overload;
var
  st:TStringList;
  i,c:integer;
begin
  st:=TStringList.Create;
  c:=ATextQA.Count;
  for i:=0 to c-1 do begin
    if i mod 2=0 then begin
      st.Add('Q ---')
    end else begin
      st.Add('A ---')
    end;
    st.Add(ATextQA[i]);
  end;
  Result:=st.Text;
  st.Free;
end;

function ChatQAToString(ATextQ, ATextA: TListString): string; overload;
var
  st:TStringList;
  i,cq,ca:integer;
begin
  st:=TStringList.Create;
  cq:=ATextQ.Count;
  ca:=ATextA.Count;
  for i:=0 to cq-1 do begin
    st.Add('Q ---');
    st.Add(ATextQ[i]);
    if i<ca then begin
      st.Add('A ---');
      st.Add(ATextA[i]);
    end;
  end;
  Result:=st.Text;
  st.Free;
end;

function CRLFToHTMLBR(AString:string):string;
begin
  Result := StringReplace(AString,
    {$IFDEF MSWINDOWS} #13#10 {$ELSE} #10{$ENDIF},  // WindowsならCRLF，それ以外はLF
    '<br>', [rfReplaceAll]);
end;

function ChatQAToHTML(ATextQ, ATextA: TListString): string; overload;
var
  st:TStringList;
  i,cq,ca:integer;
begin
  st:=TStringList.Create;
  st.Add('''
<html lang="ja">
<head>
<meta charset="UTF-8">
<style type="text/css">
/* 全体 */
html {
  font-size: 62.5%;
  background-color: #fff;
}
.sb-box {
  position: relative;
  overflow: hidden;
}
/* 吹き出し */
.sb-side-left {
  position: relative;
  float: left;
  margin: 0 10.5rem 2.0rem 2.0rem; /* 吹き出しの上下左右の余白 */
}
.sb-side-right {
  position: relative;
  float: right;
  margin: 0 2.0rem 2.0rem 10.5rem; /* 吹き出しの上下左右の余白 */
}
/* 吹き出し内のテキスト */
.sb-txt {
  position: relative;
  border: 0.2rem solid #eee; /* 吹き出しの縁取りの太さとカラー */
  border-radius: 0.6rem; /* 吹き出しを角丸に */
  background: #ccc; /* 吹き出しの背景色 */
  color: #333; /* 吹き出し内のテキストのカラー */
  font-size: 1.5rem; /* 吹き出し内のフォントサイズ */
  line-height: 1.7; /* 吹き出し内のテキストが2行以上になった時の行間 */
  padding: 1.8rem; /* 吹き出し内の上下左右の余白 */
}
.sb-txt > p:last-of-type {
  padding-bottom: 0; /* 吹き出し内のテキストを改行した場合、最後のpタグにpadding-bottomをつけない */
  margin-bottom: 0; /* 吹き出し内のテキストを改行した場合、最後のpタグにmargin-bottomをつけない */
}
/* 吹き出しの三角 */
.sb-txt:before {
  content: "";
  position: absolute;
  border-style: solid;
  top: 1.6rem; /* 吹き出し内の三角の位置 */
  z-index: 3;
}
.sb-txt:after {
  content: "";
  position: absolute;
  border-style: solid;
  top: 1.5rem; /* beforeより-0.1rem */
  z-index: 2; /* beforeより-1 */
}
/* 吹き出しの三角（左） */
.sb-txt-left:before {
  left: -0.7rem;
  border-width: 0.7rem 1.0rem 0.7rem 0;
  border-color: transparent #ccc transparent transparent; /* 背景色と同じカラーに */
}
.sb-txt-left:after {
  left: -1.0rem; /* beforeより-3rem */
  border-width: 0.8rem 1.0rem 0.8rem 0; /* beforeより上下+0.1rem */
  border-color: transparent #eee transparent transparent; /* 縁取りと同じカラーに */
}
/* 吹き出しの三角（右） */
.sb-txt-right:before {
  right: -0.7rem;
  border-width: 0.7rem 0 0.7rem 1.0rem;
  border-color: transparent transparent transparent #ccc; /* 背景色と同じカラーに */
}
.sb-txt-right:after {
  right: -1.0rem; /* beforeより-3rem */
  border-width: 0.8rem 0 0.8rem 1.0rem; /* beforeより上下+1rem */
  border-color: transparent transparent transparent #eee; /* 縁取りと同じカラーに */
}

/* 40.0rem 以下 */
@media (max-width: 40.0rem) {
  /* 吹き出し（左） */
  .sb-side-left {
    margin: 0 7.8rem 1.5rem 0; /* 吹き出し（左）の上下左右の余白を狭く */
  }
  /* 吹き出し（右） */
  .sb-side-right {
    margin: 0 0 1.5rem 7.8rem; /* 吹き出し（右）の上下左右の余白を狭く */
  }
  /* 吹き出し内のテキスト */
  .sb-txt {
    padding: 1.2rem; /* 吹き出し内の上下左右の余白を-0.6rem */
  }
}
</style>
</head>
<body>
''');
  cq:=ATextQ.Count;
  ca:=ATextA.Count;
  for i:=0 to cq-1 do begin
    st.Add('<div class="sb-box"><div class="sb-side-right"><div class="sb-txt sb-txt-right">');
    st.Add(CRLFToHTMLBR(ATextQ[i]));
    st.Add('</div></div></div>');
    if i<ca then begin
      st.Add('<div class="sb-box"><div class="sb-side-left"><div class="sb-txt sb-txt-left">');
      st.Add(CRLFToHTMLBR(ATextA[i]));
      st.Add('</div></div></div>');
    end;
  end;
  st.Add('''
</body>
</html>
''');
  Result:=st.Text;
  st.Free;
end;

end.

