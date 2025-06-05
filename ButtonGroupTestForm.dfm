object FormButtonGroupTest: TFormButtonGroupTest
  Left = 0
  Top = 0
  Caption = 'Button Group Test'
  ClientHeight = 450
  ClientWidth = 600
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
  object bgHorizontal: TANDMR_CButtonGroup
    Left = 24
    Top = 24
    Width = 300
    Height = 75
    ActiveButtonColor = clHighlight
    InactiveButtonColor = clBtnFace
    ActiveButtonFontColor = clHighlightText
    InactiveButtonFontColor = clWindowText
    Orientation = bgoHorizontal
    ButtonSpacing = 4
    ActiveButtonIndex = -1
    TabOrder = 0
    object btnH1: TANDMR_CButton
      Left = 0
      Top = 0
      Width = 97
      Height = 75
      Caption = 'Button 1'
      TabOrder = 0
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
    object btnH2: TANDMR_CButton
      Left = 101
      Top = 0
      Width = 97
      Height = 75
      Caption = 'Button 2'
      TabOrder = 1
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
    object btnH3: TANDMR_CButton
      Left = 202
      Top = 0
      Width = 97
      Height = 75
      Caption = 'Button 3'
      TabOrder = 2
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
  end
  object bgVertical: TANDMR_CButtonGroup
    Left = 24
    Top = 120
    Width = 150
    Height = 250
    ActiveButtonColor = clHighlight
    InactiveButtonColor = clBtnFace
    ActiveButtonFontColor = clHighlightText
    InactiveButtonFontColor = clWindowText
    Orientation = bgoVertical
    ButtonSpacing = 4
    ActiveButtonIndex = -1
    TabOrder = 1
    object btnV1: TANDMR_CButton
      Left = 0
      Top = 0
      Width = 150
      Height = 80
      Caption = 'Option A'
      TabOrder = 0
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
    object btnV2: TANDMR_CButton
      Left = 0
      Top = 84
      Width = 150
      Height = 80
      Caption = 'Option B'
      TabOrder = 1
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
    object btnV3: TANDMR_CButton
      Left = 0
      Top = 168
      Width = 150
      Height = 80
      Caption = 'Option C'
      TabOrder = 2
      ActiveColor = clBtnFace
      TitleFont.Color = clWindowText
      OnClick = nil // Will be assigned by ButtonGroup
    end
  end
end
