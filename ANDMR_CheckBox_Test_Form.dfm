object FormTestCheckBox: TFormTestCheckBox
  Left = 0
  Top = 0
  Caption = 'TANDMR_CCheckBox Test'
  ClientHeight = 450
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cbxLightDefault: TANDMR_CCheckBox
    Left = 24
    Top = 24
    Width = 150
    Height = 25
    Caption = 'Light Default'
    TabOrder = 0
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object cbxDarkSolid: TANDMR_CCheckBox
    Left = 24
    Top = 64
    Width = 150
    Height = 25
    Caption = 'Dark Solid'
    Style = cbsDark
    ElementStyle = cbeseSolid
    ElementCornerRadius = 4
    Color = clBlack // Example dark theme background
    OverallBorderColor = clGray
    CheckMarkColor = clWhite
    ElementBoxColor = 4210752 // Dark Gray for element box
    Font.Color = clWhite // For caption
    TabOrder = 1
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object cbxMaterialBordered: TANDMR_CCheckBox
    Left = 24
    Top = 104
    Width = 180
    Height = 25
    Caption = 'Material Bordered'
    Style = cbsMaterial
    ElementStyle = cbeseBordered // Will be mostly overridden by Material's own logic
    ElementBoxColor = clRed // Accent color for Material checked
    CheckMarkColor = clWhite
    TabOrder = 2
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object cbxFlatChecked: TANDMR_CCheckBox
    Left = 24
    Top = 144
    Width = 150
    Height = 25
    Caption = 'Flat Checked'
    Checked = True
    Style = cbsFlat
    TabOrder = 3
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object cbxModernWithImage: TANDMR_CCheckBox
    Left = 220
    Top = 24
    Width = 200
    Height = 30
    Caption = 'Modern Image'
    Style = cbsModern
    ElementCornerRadius = 5
    TabOrder = 4
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
    // Image property would be set in Object Inspector or code if a default image was available
  end
  object cbxIOS_Styled: TANDMR_CCheckBox
    Left = 220
    Top = 64
    Width = 150
    Height = 28
    Caption = 'iOS Style'
    Style = cbsIOS
    ElementBoxColor = 35222 // Green for iOS checked
    TabOrder = 5
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object cbxWin11_Disabled: TANDMR_CCheckBox
    Left = 220
    Top = 104
    Width = 160
    Height = 25
    Caption = 'Win11 Disabled'
    Checked = True
    Enabled = False
    Style = cbsWin11
    ElementBoxColor = clBlue // Accent for Win11
    TabOrder = 6
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object MemoLog: TMemo
    Left = 24
    Top = 200
    Width = 585
    Height = 220
    Lines.Strings = (
      'Event Log:')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object cbxTransparent: TANDMR_CCheckBox
    Left = 24
    Top = 170  // Adjusted top
    Width = 150
    Height = 25
    Caption = 'Transparent'
    Transparent = True
    TabOrder = 8
    OnClick = GeneralCheckBoxClick
    OnChange = GeneralCheckBoxChange
  end
  object MyRadioGroup: TANDMR_CRadioGroup
    Left = 450
    Top = 24
    Width = 180
    Height = 105
    Caption = 'My Radio Group'
    Columns = 1
    Items.Strings = (
      'Option Alpha'
      'Option Beta'
      'Option Gamma')
    ItemIndex = 0
    TabOrder = 9
    OnChange = MyRadioGroupChange
  end
end
