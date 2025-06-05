object FormButtonGroupTest: TFormButtonGroupTest
  Left = 0
  Top = 0
  Caption = 'TANDMR_CButtonGroup Test'
  ClientHeight = 600
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDesc: TLabel
    Left = 8
    Top = 8
    Width = 680
    Height = 26
    AutoSize = False
    Caption = 'Description of manual test controls and purpose of run tests button.'
    WordWrap = True
  end
  object GroupForManualTest: TANDMR_CButtonGroup
    Left = 8
    Top = 40
    Width = 680
    Height = 45
    TabOrder = 0
    SelectedItemIndex = -1
    OnSelectionChanged = GroupForManualTestSelectionChanged
    // Example of setting some custom colors if desired for manual test
    // SelectedItemBackgroundColor = claGreen
    // UnselectedItemFontColor = claDarkGray
  end
  object LabelAddItem: TLabel
    Left = 8
    Top = 100
    Width = 80
    Height = 13
    Caption = 'Item Caption:'
  end
  object EditAddItemCaption: TEdit
    Left = 96
    Top = 96
    Width = 180
    Height = 21
    TabOrder = 1
    Text = 'New Item'
  end
  object ButtonAddItem: TButton
    Left = 288
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Add Item'
    TabOrder = 2
    OnClick = ButtonAddItemClick
  end
  object LabelItemIndex: TLabel
    Left = 8
    Top = 132
    Width = 80
    Height = 13
    Caption = 'Selected Index:'
  end
  object SpinEditItemIndex: TSpinEdit
    Left = 96
    Top = 128
    Width = 70
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object ButtonRemoveItem: TButton
    Left = 176
    Top = 126
    Width = 90
    Height = 25
    Caption = 'Remove Item'
    TabOrder = 4
    OnClick = ButtonRemoveItemClick
  end
  object ButtonToggleItemEnabled: TButton
    Left = 272
    Top = 126
    Width = 120
    Height = 25
    Caption = 'Toggle Enabled'
    TabOrder = 5
    OnClick = ButtonToggleItemEnabledClick
  end
  object ButtonToggleItemVisible: TButton
    Left = 400
    Top = 126
    Width = 120
    Height = 25
    Caption = 'Toggle Visible'
    TabOrder = 6
    OnClick = ButtonToggleItemVisibleClick
  end
  object ButtonRunTests: TButton
    Left = 8
    Top = 160
    Width = 680
    Height = 33
    Caption = 'Run All Programmatic Tests'
    Font.Style = [fsBold]
    TabOrder = 7
    OnClick = ButtonRunTestsClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 200
    Width = 680
    Height = 390
    ScrollBars = ssVertical
    TabOrder = 8
    ReadOnly = True
  end
end
