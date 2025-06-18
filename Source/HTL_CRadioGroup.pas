unit HTL_CRadioGroup;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Controls, HTL_CPanel, HTL_CRadioBox, HTL_ComponentUtils,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages; // Ensure Vcl.Graphics is here for TFont, TColor

type
  TRadioBoxItemSettings = class(TPersistent)
  private
    FFont: TFont;
    FColorUnchecked: TColor;
    FColorChecked: TColor;
    FMarkColor: TColor;
    FTransparent: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetColorUnchecked(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetMarkColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Font: TFont read FFont write SetFont;
    property ColorUnchecked: TColor read FColorUnchecked write SetColorUnchecked default clNone; // Modernized
    property ColorChecked: TColor read FColorChecked write SetColorChecked default clHighlight; // Modernized
    property MarkColor: TColor read FMarkColor write SetMarkColor default clWhite; // Modernized
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THTL_CRadioGroup = class(THTL_CPanel)
  private
    FItems: TStringList;
    FItemIndex: Integer;
    FColumns: Integer;
    FRadioButtons: TList<THTL_CRadioBox>;
    FOnChange: TNotifyEvent;
    FItemSettings: TRadioBoxItemSettings;

    procedure ItemsChanged(Sender: TObject);
    procedure ItemSettingsChanged(Sender: TObject);
    procedure ApplyRadioBoxStylesToChildren;

    function GetItems: TStringList;
    procedure SetItems(const Value: TStringList);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetColumns: Integer;
    procedure SetColumns(const Value: Integer);
    procedure SetEnabled(Value: Boolean); override;

    procedure SetItemSettings(const Value: TRadioBoxItemSettings);

    procedure ClearRadioButtons;
    procedure CreateRadioButtonsFromItems;
    procedure RearrangeRadioButtons;
    procedure RadioButtonClick(Sender: TObject);
  protected
    procedure Resize; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStringList read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property Columns: Integer read GetColumns write SetColumns default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property ItemSettings: TRadioBoxItemSettings read FItemSettings write SetItemSettings;
  end;

procedure Register;

implementation

uses System.Math;

{ TRadioBoxItemSettings }

constructor TRadioBoxItemSettings.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.Name := 'Segoe UI'; // Modernized
  FFont.Size := 9;          // Modernized
  FFont.Color := clWindowText; // Modernized (standard text color)
  FFont.Style := [];        // Modernized (plain for items)

  FColorUnchecked := clNone; // Modernized
  FColorChecked := clHighlight; // Modernized
  FMarkColor := clWhite; // Modernized
  FTransparent := False;
  FOnChange := nil;
end;

destructor TRadioBoxItemSettings.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TRadioBoxItemSettings.Assign(Source: TPersistent);
begin
  if Source is TRadioBoxItemSettings then
  begin
    SetFont(TRadioBoxItemSettings(Source).Font);
    FColorUnchecked := TRadioBoxItemSettings(Source).ColorUnchecked;
    FColorChecked := TRadioBoxItemSettings(Source).ColorChecked;
    FMarkColor := TRadioBoxItemSettings(Source).MarkColor;
    FTransparent := TRadioBoxItemSettings(Source).Transparent;
  end
  else
    inherited Assign(Source);
  Changed;
end;

procedure TRadioBoxItemSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRadioBoxItemSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TRadioBoxItemSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TRadioBoxItemSettings.SetColorUnchecked(const Value: TColor);
begin
  if FColorUnchecked <> Value then
  begin
    FColorUnchecked := Value;
    Changed;
  end;
end;

procedure TRadioBoxItemSettings.SetColorChecked(const Value: TColor);
begin
  if FColorChecked <> Value then
  begin
    FColorChecked := Value;
    Changed;
  end;
end;

procedure TRadioBoxItemSettings.SetMarkColor(const Value: TColor);
begin
  if FMarkColor <> Value then
  begin
    FMarkColor := Value;
    Changed;
  end;
end;

procedure TRadioBoxItemSettings.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;


{ THTL_CRadioGroup }

constructor THTL_CRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // This calls THTL_CPanel.Create, which sets modern panel defaults

  // Default caption for the group itself (from THTL_CPanel's FCaptionSettings)
  Self.CaptionSettings.Text := Self.Name; // Group Name as default caption
  Self.CaptionSettings.Alignment := taLeftJustify;
  Self.CaptionSettings.Font.Style := [fsBold];
  Self.CaptionSettings.Offset := Point(2,0); // Small offset for group caption from top-left

  // Specific RadioGroup initializations
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChanged;
  FRadioButtons := TList<THTL_CRadioBox>.Create;
  FItemIndex := -1;
  FColumns := 1;
  FOnChange := nil;

  FItemSettings := TRadioBoxItemSettings.Create; // This now gets modernized defaults
  FItemSettings.OnChange := ItemSettingsChanged;
end;

destructor THTL_CRadioGroup.Destroy;
begin
  FItemSettings.Free;
  FItems.Free;
  ClearRadioButtons; // Ensure radio buttons are freed
  FRadioButtons.Free;
  inherited Destroy;
end;

procedure THTL_CRadioGroup.ItemSettingsChanged(Sender: TObject);
begin
  ApplyRadioBoxStylesToChildren;
end;

procedure THTL_CRadioGroup.ApplyRadioBoxStylesToChildren;
var
  RadioButton: THTL_CRadioBox;
begin
  for RadioButton in FRadioButtons do
  begin
    RadioButton.CaptionSettings.Font.Assign(FItemSettings.Font);
    // RadioIndicatorBorder colors are not directly part of ItemSettings,
    // but RadioColorUnchecked/Checked/MarkColor are.
    // The RadioIndicatorBorder.Color (the circle itself) for new radio boxes
    // will come from ANDMR_CRadioBox's own constructor default (clGrayText).
    // This is good, as ItemSettings focuses on the fill/mark.
    RadioButton.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    RadioButton.RadioColorChecked := FItemSettings.ColorChecked;
    RadioButton.MarkColor := FItemSettings.MarkColor;
    RadioButton.Transparent := FItemSettings.Transparent; // This applies to the RadioBox's own background
  end;
  Invalidate;
end;

procedure THTL_CRadioGroup.ItemsChanged(Sender: TObject);
begin
  ClearRadioButtons;
  CreateRadioButtonsFromItems;
  RearrangeRadioButtons;
  Invalidate;
end;

function THTL_CRadioGroup.GetItems: TStringList;
begin
  Result := FItems;
end;

procedure THTL_CRadioGroup.SetItems(const Value: TStringList);
begin
  FItems.OnChange := nil;
  FItems.Assign(Value);
  if Assigned(FItems) then
    FItems.OnChange := ItemsChanged;

  ClearRadioButtons;
  CreateRadioButtonsFromItems;
  RearrangeRadioButtons;
  Invalidate;
end;

function THTL_CRadioGroup.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure THTL_CRadioGroup.SetItemIndex(const Value: Integer);
var
  PrevIndex: Integer;
  I: Integer;
begin
  if Value = FItemIndex then
    Exit;

  PrevIndex := FItemIndex;

  for I := 0 to FRadioButtons.Count - 1 do
  begin
    FRadioButtons[I].Checked := False;
  end;

  if (Value >= 0) and (Value < FRadioButtons.Count) then
  begin
    FItemIndex := Value;
    FRadioButtons[FItemIndex].Checked := True;
  end
  else
  begin
    FItemIndex := -1;
  end;

  if FItemIndex <> PrevIndex then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  Invalidate;
end;

function THTL_CRadioGroup.GetColumns: Integer;
begin
  Result := FColumns;
end;

procedure THTL_CRadioGroup.SetColumns(const Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    RearrangeRadioButtons;
    Invalidate;
  end;
end;

procedure THTL_CRadioGroup.SetEnabled(Value: Boolean);
var
  RadioButton: THTL_CRadioBox;
begin
  if Enabled <> Value then
  begin
    inherited SetEnabled(Value);
    for RadioButton in FRadioButtons do
      RadioButton.Enabled := Value;
    Invalidate;
  end;
end;

procedure THTL_CRadioGroup.ClearRadioButtons;
var
  RadioButton: THTL_CRadioBox;
begin
  while FRadioButtons.Count > 0 do
  begin
    RadioButton := FRadioButtons[0];
    FRadioButtons.Remove(RadioButton);
    RadioButton.Free;
  end;
  FItemIndex := -1;
end;

procedure THTL_CRadioGroup.CreateRadioButtonsFromItems;
var
  i: Integer;
  NewRadioBox: THTL_CRadioBox;
begin
  if FItems = nil then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    NewRadioBox := THTL_CRadioBox.Create(Self);
    NewRadioBox.Parent := Self;
    NewRadioBox.Caption := FItems[i];
    NewRadioBox.OnClick := RadioButtonClick;
    NewRadioBox.Tag := i;

    // Apply initial styles from FItemSettings
    NewRadioBox.CaptionSettings.Font.Assign(FItemSettings.Font);
    NewRadioBox.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    NewRadioBox.RadioColorChecked := FItemSettings.ColorChecked;
    NewRadioBox.MarkColor := FItemSettings.MarkColor;
    NewRadioBox.Transparent := FItemSettings.Transparent;
    // The ANDMR_CRadioBox constructor already sets its own RadioIndicatorBorder defaults (e.g., color clGrayText)

    FRadioButtons.Add(NewRadioBox);
  end;

  if (FItemIndex >= 0) and (FItemIndex < FRadioButtons.Count) then
    FRadioButtons[FItemIndex].Checked := True
  else
    FItemIndex := -1; // Ensure consistency if previous index is now invalid
end;

procedure THTL_CRadioGroup.RearrangeRadioButtons;
var
  i: Integer;
  ColumnWidth, X_Pos, Y_Pos: Double;
  ItemsPerColumn: Integer;
  CurrentColumn, CurrentRowInColumn: Integer;
  LBorderThickness, LCornerRadius: Integer;
  ActualPaddingX, ActualPaddingY, TopOffsetForCaption: Integer;
  AvailableWidth, AvailableHeight: Double;
  BasePadding: Integer;
  RadioButtonSpacing: Integer;
  DefaultRadioButtonHeight: Integer;
begin
  BasePadding := 4;
  RadioButtonSpacing := 4;
  DefaultRadioButtonHeight := 24;

  if (FRadioButtons.Count = 0) or (FColumns <= 0) then
  begin
    Invalidate;
    Exit;
  end;

  LBorderThickness := Self.BorderSettings.Thickness;
  LCornerRadius := Self.BorderSettings.CornerRadius;

  ActualPaddingX := BasePadding + LBorderThickness;
  ActualPaddingY := BasePadding + LBorderThickness;

  TopOffsetForCaption := 0;
  if Self.CaptionSettings.Visible and (Self.CaptionSettings.Text <> '') then
  begin
    var TempCanvas: TControlCanvas;
    var CaptionHeight: Integer;
    TempCanvas := TControlCanvas.Create;
    try
      TempCanvas.Control := Self;
      TempCanvas.Font.Assign(Self.CaptionSettings.Font);
      CaptionHeight := TempCanvas.TextHeight(Self.CaptionSettings.Text);
      // Assuming caption is at the top for this calculation.
      // If VerticalAlignment can be cvaBottom or cvaCenter for group caption, this needs adjustment.
      TopOffsetForCaption := CaptionHeight + BasePadding + Self.CaptionSettings.Offset.Y;
    finally
      TempCanvas.Free;
    end;
  end;
  ActualPaddingY := ActualPaddingY + TopOffsetForCaption;


  ItemsPerColumn := Ceil(FRadioButtons.Count / FColumns);
  if (ItemsPerColumn = 0) and (FRadioButtons.Count > 0) then ItemsPerColumn := 1;

  AvailableWidth := Self.ClientWidth - (ActualPaddingX * 2);
  AvailableHeight := Self.ClientHeight - ActualPaddingY - BasePadding; // BasePadding for bottom margin

  if FColumns > 1 then
    ColumnWidth := (AvailableWidth - (RadioButtonSpacing * (FColumns - 1))) / FColumns
  else
    ColumnWidth := AvailableWidth;

  if ColumnWidth < (DefaultRadioButtonHeight * 2) then // Min width for a radio button
      ColumnWidth := DefaultRadioButtonHeight * 2;
  if (AvailableWidth > 0) and (ColumnWidth > AvailableWidth) then
      ColumnWidth := AvailableWidth;

  for i := 0 to FRadioButtons.Count - 1 do
  begin
    CurrentColumn := i div ItemsPerColumn;
    CurrentRowInColumn := i mod ItemsPerColumn;

    X_Pos := ActualPaddingX + CurrentColumn * (ColumnWidth + RadioButtonSpacing);
    Y_Pos := ActualPaddingY + CurrentRowInColumn * (DefaultRadioButtonHeight + RadioButtonSpacing);

    FRadioButtons[i].SetBounds(Round(X_Pos), Round(Y_Pos), Round(ColumnWidth), DefaultRadioButtonHeight);
  end;
  Invalidate;
end;

procedure THTL_CRadioGroup.RadioButtonClick(Sender: TObject);
begin
  if Sender is THTL_CRadioBox then
  begin
    Self.ItemIndex := (Sender as THTL_CRadioBox).Tag;
  end;
end;

procedure THTL_CRadioGroup.Resize;
begin
  inherited Resize;
  RearrangeRadioButtons;
end;

procedure THTL_CRadioGroup.SetItemSettings(const Value: TRadioBoxItemSettings);
begin
  FItemSettings.Assign(Value);
  ApplyRadioBoxStylesToChildren;
end;

procedure THTL_CRadioGroup.CMMouseEnter(var Message: TMessage);
var
  RadioButton: THTL_CRadioBox;
begin
  inherited;
  if Self.HoverSettings.Enabled and Self.Enabled then // Check if group itself should react
  begin
    // This part is for the CPanel's hover effect (border, background)
    // The IsGroupHovered logic below is for children
  end;

  for RadioButton in FRadioButtons do
  begin
    RadioButton.IsGroupHovered := True; // Signal to children that group is hovered
    // RadioButton.GroupHoverCaptionBackgroundColor := Self.HoverSettings.BackgroundColor; // Example
  end;
end;

procedure THTL_CRadioGroup.CMMouseLeave(var Message: TMessage);
var
  RadioButton: THTL_CRadioBox;
begin
  inherited;
  for RadioButton in FRadioButtons do
  begin
    RadioButton.IsGroupHovered := False;
  end;
end;

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CRadioGroup]);
end;

end.
