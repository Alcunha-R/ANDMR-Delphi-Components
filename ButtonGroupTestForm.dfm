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
  object bgHorizontal: TANDMR_CButtonGroup // Ensure this is the new component
    Left = 24
    Top = 24
    Width = 550
    Height = 60
    TabOrder = 0
    Orientation = bgoHorizontal
    ButtonSpacing = 5
    ActiveButtonColor = clBlue // Example initial active color
    InactiveButtonColor = clWhite
    ActiveButtonFontColor = clWhite
    InactiveButtonFontColor = clBlack
    BorderSettings.CornerRadius = 8
    BorderSettings.Color = clGray
    BorderSettings.Thickness = 1
    SeparatorSettings.Visible = True
    SeparatorSettings.Color = clMedGray
    SeparatorSettings.Thickness = 1
    SeparatorSettings.Padding = 2
  end
  object bgVertical: TANDMR_CButtonGroup // Ensure this is the new component
    Left = 24
    Top = 100
    Width = 150
    Height = 300
    TabOrder = 1
    Orientation = bgoVertical
    ButtonSpacing = 3
    ActiveButtonColor = clGreen
    InactiveButtonColor = clSilver
    ActiveButtonFontColor = clWhite
    InactiveButtonFontColor = clBlack
    BorderSettings.CornerRadius = 6
    BorderSettings.Color = clGray
    BorderSettings.Thickness = 1
    SeparatorSettings.Visible = True
    SeparatorSettings.Thickness = 2
    SeparatorSettings.Padding = 3
  end
  // Add VCL Buttons for testing
  object btnAddH: TButton
    Left = 24
    Top = 400
    Width = 100
    Height = 25
    Caption = 'Add to Horz'
    TabOrder = 2
    OnClick = btnAddHClick
  end
  object btnAddV: TButton
    Left = 136
    Top = 400
    Width = 100
    Height = 25
    Caption = 'Add to Vert'
    TabOrder = 3
    OnClick = btnAddVClick
  end
  object btnClearH: TButton
    Left = 248
    Top = 400
    Width = 100
    Height = 25
    Caption = 'Clear Horz'
    TabOrder = 4
    OnClick = btnClearHClick
  end
  object btnClearV: TButton
    Left = 360
    Top = 400
    Width = 100
    Height = 25
    Caption = 'Clear Vert'
    TabOrder = 5
    OnClick = btnClearVClick
  end
  object chkHorzOrientation: TCheckBox
    Left = 470
    Top = 400
    Width = 120
    Height = 17
    Caption = 'Vertical Horz Grp'
    TabOrder = 6
    OnClick = chkHorzOrientationClick
  end
  object lblSelectedH: TLabel
    Left = 24
    Top = 88
    Width = 300
    Height = 13
    Caption = 'Selected H:'
  end
  object lblSelectedV: TLabel
    Left = 180
    Top = 88
    Width = 300
    Height = 13
    Caption = 'Selected V:'
  end
end
