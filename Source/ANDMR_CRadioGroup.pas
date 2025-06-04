unit ANDMR_CRadioGroup;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Controls, ANDMR_CPanel, ANDMR_CRadioBox, ANDMR_ComponentUtils,
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
    property ColorUnchecked: TColor read FColorUnchecked write SetColorUnchecked default clWindow;
    property ColorChecked: TColor read FColorChecked write SetColorChecked default clActiveCaption;
    property MarkColor: TColor read FMarkColor write SetMarkColor default clWindowText;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TANDMR_CRadioGroup = class(TANDMR_CPanel)
  private
    FItems: TStringList;
    FItemIndex: Integer;
    FColumns: Integer;
    FRadioButtons: TList<TANDMR_CRadioBox>;
    FOnChange: TNotifyEvent;
    FItemSettings: TRadioBoxItemSettings; // New settings class field

    procedure ItemsChanged(Sender: TObject);
    procedure ItemSettingsChanged(Sender: TObject); // New handler
    procedure ApplyRadioBoxStylesToChildren;

    function GetItems: TStringList;
    procedure SetItems(const Value: TStringList);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetColumns: Integer;
    procedure SetColumns(const Value: Integer);
    procedure SetEnabled(Value: Boolean); override;

    procedure SetItemSettings(const Value: TRadioBoxItemSettings); // Setter for new property

    procedure ClearRadioButtons;
    procedure CreateRadioButtonsFromItems;
    procedure RearrangeRadioButtons;
    procedure RadioButtonClick(Sender: TObject);
  protected
    procedure Resize; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER; // New
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE; // New
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStringList read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property Columns: Integer read GetColumns write SetColumns default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // Enabled property is inherited and will be handled by overriding SetEnabled

    property ItemSettings: TRadioBoxItemSettings read FItemSettings write SetItemSettings;
  end;

procedure Register;

implementation

uses System.Math; // Winapi.Windows and Vcl.Graphics already added above

// const
//   ControlPadding = 4; // Will be BasePadding now
//   RadioButtonSpacing = 4; // Still used
//   DefaultRadioButtonHeight = 24; // Still used

{ TANDMR_CRadioGroup }

constructor TANDMR_CRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChanged;
  FRadioButtons := TList<TANDMR_CRadioBox>.Create;
  FItemIndex := -1;
  FColumns := 1;
  FOnChange := nil;

  FItemSettings := TRadioBoxItemSettings.Create;
  FItemSettings.OnChange := ItemSettingsChanged;
end;

destructor TANDMR_CRadioGroup.Destroy;
begin
  FItemSettings.Free;
  FItems.Free;
  FRadioButtons.Free;
  inherited Destroy;
end;

procedure TANDMR_CRadioGroup.ItemSettingsChanged(Sender: TObject);
begin
  ApplyRadioBoxStylesToChildren;
end;

procedure TANDMR_CRadioGroup.ApplyRadioBoxStylesToChildren;
var
  RadioButton: TANDMR_CRadioBox;
begin
  // Consider if FApplyingRadioBoxStyle logic is needed if ItemSettings.OnChange can cause re-entrancy
  for RadioButton in FRadioButtons do
  begin
    RadioButton.CaptionSettings.Font.Assign(FItemSettings.Font);
    RadioButton.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    RadioButton.RadioColorChecked := FItemSettings.ColorChecked;
    RadioButton.MarkColor := FItemSettings.MarkColor;
    RadioButton.Transparent := FItemSettings.Transparent;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioGroup.ItemsChanged(Sender: TObject);
begin
  ClearRadioButtons;
  CreateRadioButtonsFromItems;
  RearrangeRadioButtons;
  Invalidate;
end;

function TANDMR_CRadioGroup.GetItems: TStringList;
begin
  Result := FItems;
end;

procedure TANDMR_CRadioGroup.SetItems(const Value: TStringList);
begin
  FItems.OnChange := nil; // Temporarily remove handler
  FItems.Assign(Value);   // Assign new items
  if Assigned(FItems) then // FItems is always assigned after create
    FItems.OnChange := ItemsChanged; // Re-assign handler

  ClearRadioButtons; // Clear existing visual radio buttons
  CreateRadioButtonsFromItems; // Create new ones
  RearrangeRadioButtons; // Position them
  Invalidate;
end;

function TANDMR_CRadioGroup.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TANDMR_CRadioGroup.SetItemIndex(const Value: Integer);
var
  PrevIndex: Integer;
  I: Integer;
begin
  if Value = FItemIndex then // If new index is same as current, do nothing
    Exit;

  PrevIndex := FItemIndex; // Store previous index for OnChange check

  // Uncheck all radio buttons first
  for I := 0 to FRadioButtons.Count - 1 do
  begin
    FRadioButtons[I].Checked := False;
  end;

  // Check the new one if it's valid
  if (Value >= 0) and (Value < FRadioButtons.Count) then
  begin
    FItemIndex := Value;
    FRadioButtons[FItemIndex].Checked := True;
  end
  else // If Value is invalid (e.g., -1 or out of bounds)
  begin
    FItemIndex := -1; // Set to no selection
  end;

  // Trigger OnChange only if the effective ItemIndex has changed
  if FItemIndex <> PrevIndex then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;

  Invalidate; // Repaint the control
end;

function TANDMR_CRadioGroup.GetColumns: Integer;
begin
  Result := FColumns;
end;

procedure TANDMR_CRadioGroup.SetColumns(const Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    RearrangeRadioButtons;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioGroup.SetEnabled(Value: Boolean);
var
  RadioButton: TANDMR_CRadioBox;
begin
  if Enabled <> Value then
  begin
    inherited SetEnabled(Value);
    for RadioButton in FRadioButtons do
      RadioButton.Enabled := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioGroup.ClearRadioButtons;
var
  RadioButton: TANDMR_CRadioBox;
begin
  // Free and remove from Controls list
  while FRadioButtons.Count > 0 do
  begin
    RadioButton := FRadioButtons[0];
    FRadioButtons.Remove(RadioButton); // Remove from our list
    RadioButton.Free; // This also removes it from Parent.Controls
  end;
  // FRadioButtons.Clear; // Already cleared by removing items one by one
  FItemIndex := -1; // Reset item index when clearing
end;

procedure TANDMR_CRadioGroup.CreateRadioButtonsFromItems;
var
  i: Integer;
  Radio: TANDMR_CRadioBox; // Changed variable name for clarity
begin
  if FItems = nil then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    Radio := TANDMR_CRadioBox.Create(Self); // Owner is Self
    Radio.Parent := Self; // Parent is Self, adds to Controls list
    Radio.Caption := FItems[i];
    Radio.OnClick := RadioButtonClick;
    Radio.Tag := i; // Store original index in Tag

    // Apply current group styles to the new radio button
    Radio.CaptionSettings.Font.Assign(FItemSettings.Font);
    Radio.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    Radio.RadioColorChecked := FItemSettings.ColorChecked;
    Radio.MarkColor := FItemSettings.MarkColor;
    Radio.Transparent := FItemSettings.Transparent;

    FRadioButtons.Add(Radio); // Add to our tracking list
  end;

  // After creating, if ItemIndex was previously set and is valid, apply it
  if (FItemIndex >= 0) and (FItemIndex < FRadioButtons.Count) then
    FRadioButtons[FItemIndex].Checked := True;
  // No default selection if FItemIndex is not valid, SetItemIndex(-1) already handled this.
end;

procedure TANDMR_CRadioGroup.RearrangeRadioButtons;
var
  i: Integer;
  ColumnWidth, X_Pos, Y_Pos: Double; // Use Double for calculations, then Round
  ItemsPerColumn: Integer;
  CurrentColumn, CurrentRowInColumn: Integer;
  LBorderThickness, LCornerRadius: Integer;
  ActualPaddingX, ActualPaddingY: Integer;
  AvailableWidth: Double;
  // Constants (can be defined globally or locally if preferred)
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

  if LCornerRadius > 4 then // Only add extra if radius is somewhat significant
  begin
    ActualPaddingX := ActualPaddingX + LCornerRadius div 3; // Heuristic
    ActualPaddingY := ActualPaddingY + LCornerRadius div 3; // Heuristic
  end;

  // Account for the group's own caption
  if Self.CaptionSettings.Visible then
  begin
    var EstimatedCaptionHeight: Integer;
    // Estimate caption height: font height + a small margin
    EstimatedCaptionHeight := Abs(Self.CaptionSettings.Font.Height) + 4; // 4px for top/bottom margin around text

    if (Self.CaptionSettings.VerticalAlignment = ANDMR_ComponentUtils.cvaTop) then
    begin
      ActualPaddingY := ActualPaddingY + EstimatedCaptionHeight;
    end;
  end;

  ItemsPerColumn := Ceil(FRadioButtons.Count / FColumns);
  if (ItemsPerColumn = 0) and (FRadioButtons.Count > 0) then ItemsPerColumn := 1;

  AvailableWidth := Self.ClientWidth - (ActualPaddingX * 2);

  if FColumns > 1 then
    ColumnWidth := (AvailableWidth - (RadioButtonSpacing * (FColumns - 1))) / FColumns
  else
    ColumnWidth := AvailableWidth;

  // Ensure a minimum sensible width if calculated width is too small
  // Also ensure it doesn't try to be wider than available space if ActualPaddingX is large
  if ColumnWidth < (DefaultRadioButtonHeight * 2) then
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

procedure TANDMR_CRadioGroup.RadioButtonClick(Sender: TObject);
begin
  if Sender is TANDMR_CRadioBox then
  begin
    // SetItemIndex will handle unchecking others, FItemIndex update, and calling OnChange
    Self.ItemIndex := (Sender as TANDMR_CRadioBox).Tag;
  end;
end;

procedure TANDMR_CRadioGroup.Resize;
begin
  inherited Resize;
  RearrangeRadioButtons;
end;

procedure TANDMR_CRadioGroup.SetItemSettings(const Value: TRadioBoxItemSettings);
begin
  FItemSettings.Assign(Value);
  // FItemSettings.OnChange will trigger ItemSettingsChanged, which calls ApplyRadioBoxStylesToChildren
  // However, direct assignment might not trigger OnChange if internal values are same.
  // Explicitly call ApplyRadioBoxStylesToChildren to ensure styles are applied.
  ApplyRadioBoxStylesToChildren;
end;

procedure TANDMR_CRadioGroup.CMMouseEnter(var Message: TMessage);
var
  RadioButton: TANDMR_CRadioBox;
begin
  inherited; // Calls TANDMR_CPanel's CMMouseEnter

  if Self.HoverSettings.Enabled then
  begin
    for RadioButton in FRadioButtons do
    begin
      RadioButton.IsGroupHovered := True;
      RadioButton.GroupHoverCaptionBackgroundColor := Self.HoverSettings.BackgroundColor;
    end;
  end;
end;

procedure TANDMR_CRadioGroup.CMMouseLeave(var Message: TMessage);
var
  RadioButton: TANDMR_CRadioBox;
begin
  inherited; // Calls TANDMR_CPanel's CMMouseLeave

  for RadioButton in FRadioButtons do
  begin
    RadioButton.IsGroupHovered := False;
    // Optionally reset: RadioButton.GroupHoverCaptionBackgroundColor := clNone;
  end;
end;

{ TRadioBoxItemSettings }

constructor TRadioBoxItemSettings.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColorUnchecked := clWindow;
  FColorChecked := clActiveCaption;
  FMarkColor := clWindowText;
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
    SetFont(TRadioBoxItemSettings(Source).Font); // Use setter to ensure OnChange for font is handled if necessary
    FColorUnchecked := TRadioBoxItemSettings(Source).ColorUnchecked;
    FColorChecked := TRadioBoxItemSettings(Source).ColorChecked;
    FMarkColor := TRadioBoxItemSettings(Source).MarkColor;
    FTransparent := TRadioBoxItemSettings(Source).Transparent;
    // Do not copy FOnChange
  end
  else
    inherited Assign(Source);
  Changed; // Notify after assigning all properties
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
  // FFont.OnChange will call FontChanged, which calls Changed.
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

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CRadioGroup]);
end;

end.
