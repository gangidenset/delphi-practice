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
  object Label_search: TLabel
    Left = 75
    Top = 11
    Width = 39
    Height = 15
    Caption = #26908#32034#65306
  end
  object Label_add: TLabel
    Left = 42
    Top = 42
    Width = 72
    Height = 15
    Caption = #36861#21152#12539#32232#38598#65306
  end
  object Label_checklist: TLabel
    Left = 24
    Top = 163
    Width = 55
    Height = 15
    Caption = 'ToDo'#12522#12473#12488
  end
  object Label_priority: TLabel
    Left = 62
    Top = 71
    Width = 52
    Height = 15
    Caption = #20778#20808#24230#65306
  end
  object Label_category: TLabel
    Left = 64
    Top = 100
    Width = 50
    Height = 15
    Caption = #12459#12486#12468#12522#65306
  end
  object Label1: TLabel
    Left = 75
    Top = 126
    Width = 39
    Height = 15
    Caption = #26399#38480#65306
  end
  object Label2: TLabel
    Left = 264
    Top = 68
    Width = 310
    Height = 15
    Caption = #23436#20102#12479#12473#12463#19968#35239#65288#12480#12502#12523#12463#12522#12483#12463#12391'ToDo'#12522#12473#12488#12408#12398#24489#20803#21487#33021#65289
  end
  object Edit_add: TEdit
    Left = 120
    Top = 39
    Width = 475
    Height = 23
    TabOrder = 0
    OnKeyPress = Edit_addKeyPress
  end
  object Button_add: TButton
    Left = 520
    Top = 346
    Width = 75
    Height = 25
    Caption = #36861#21152
    TabOrder = 1
    OnClick = Button_addClick
  end
  object Button_delete: TButton
    Left = 520
    Top = 377
    Width = 75
    Height = 25
    Caption = #21066#38500
    TabOrder = 2
    OnClick = Button_deleteClick
  end
  object CheckListBox1: TCheckListBox
    Left = 24
    Top = 184
    Width = 490
    Height = 249
    ItemHeight = 15
    TabOrder = 3
    OnClick = CheckListBox1Click
    OnClickCheck = CheckListBox1ClickCheck
  end
  object Button_path: TButton
    Left = 520
    Top = 315
    Width = 75
    Height = 25
    Caption = #12497#12473#30906#35469
    TabOrder = 4
    OnClick = Button_pathClick
  end
  object Edit_search: TEdit
    Left = 120
    Top = 10
    Width = 475
    Height = 23
    TabOrder = 5
    OnChange = Edit_searchChange
  end
  object Button_reset: TButton
    Left = 520
    Top = 408
    Width = 75
    Height = 25
    Caption = #21021#26399#21270
    TabOrder = 6
    OnClick = Button_resetClick
  end
  object ComboBox_Priority: TComboBox
    Left = 120
    Top = 68
    Width = 121
    Height = 23
    TabOrder = 7
    OnKeyPress = ComboBox_PriorityKeyPress
  end
  object ComboBox_Category: TComboBox
    Left = 120
    Top = 97
    Width = 121
    Height = 23
    TabOrder = 8
    OnKeyPress = ComboBox_CategoryKeyPress
  end
  object dtp_Deadline: TDateTimePicker
    Left = 120
    Top = 123
    Width = 121
    Height = 23
    Date = 45999.000000000000000000
    Time = 0.395179085651761900
    TabOrder = 9
    OnKeyPress = dtp_DeadlineKeyPress
  end
  object Memo_Completed: TMemo
    Left = 264
    Top = 89
    Width = 313
    Height = 57
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 10
    WordWrap = False
    OnDblClick = Memo_CompletedDblClick
  end
end
