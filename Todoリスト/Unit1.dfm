object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object ListBox1: TListBox
    Left = 24
    Top = 104
    Width = 275
    Height = 298
    ItemHeight = 15
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Edit_add: TEdit
    Left = 120
    Top = 39
    Width = 475
    Height = 23
    TabOrder = 1
    OnKeyPress = Edit_addKeyPress
  end
  object Button_add: TButton
    Left = 424
    Top = 408
    Width = 75
    Height = 25
    Caption = #36861#21152
    TabOrder = 2
    OnClick = Button_addClick
  end
  object Button_delete: TButton
    Left = 520
    Top = 408
    Width = 75
    Height = 25
    Caption = #21066#38500
    TabOrder = 3
    OnClick = Button_deleteClick
  end
  object CheckListBox1: TCheckListBox
    Left = 305
    Top = 104
    Width = 290
    Height = 298
    ItemHeight = 15
    TabOrder = 4
    OnClick = CheckListBox1Click
    OnClickCheck = CheckListBox1ClickCheck
    OnDrawItem = CheckListBox1DrawItem
  end
  object Button_path: TButton
    Left = 272
    Top = 408
    Width = 131
    Height = 25
    Caption = #20445#23384#12501#12449#12452#12523#12398#12497#12473#30906#35469
    TabOrder = 5
    OnClick = Button_pathClick
  end
  object Edit_search: TEdit
    Left = 120
    Top = 10
    Width = 475
    Height = 23
    TabOrder = 6
    OnChange = Edit_searchChange
    OnKeyPress = Edit_searchKeyPress
  end
  object StaticText1: TStaticText
    Left = 54
    Top = 14
    Width = 43
    Height = 19
    Caption = #26908#32034#65306
    TabOrder = 7
  end
  object StaticText2: TStaticText
    Left = 38
    Top = 43
    Width = 76
    Height = 19
    Caption = #36861#21152#12539#32232#38598#65306
    TabOrder = 8
  end
  object StaticText3: TStaticText
    Left = 305
    Top = 79
    Width = 77
    Height = 19
    Caption = #12481#12455#12483#12463#12508#12483#12463#12473
    TabOrder = 9
  end
  object StaticText4: TStaticText
    Left = 24
    Top = 79
    Width = 59
    Height = 19
    Caption = 'ToDo'#12522#12473#12488
    TabOrder = 10
  end
  object Button_reset: TButton
    Left = 176
    Top = 408
    Width = 75
    Height = 25
    Caption = #21021#26399#21270
    TabOrder = 11
    OnClick = Button_resetClick
  end
end
