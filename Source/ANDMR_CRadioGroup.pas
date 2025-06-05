unit ANDMR_CRadioGroup;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Controls, ANDMR_CPanel, ANDMR_CRadioBox, ANDMR_ComponentUtils,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages;

type
  TRadioBoxItemSettings = class(TPersistent)
  private
    FFont: TFont;
    FColorUnchecked: TColor;
    FColorChecked: TColor;
    FMarkColor: TColor;
    FTransparent: Boolean;
    FStyle: TANDMR_CRadioBoxStyle; // New field for style
    FOnChange: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetColorUnchecked(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetMarkColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetStyle(const Value: TANDMR_CRadioBoxStyle); // Setter for Style
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
    property Style: TANDMR_CRadioBoxStyle read FStyle write SetStyle default crsDefault; // Published Style
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TANDMR_CRadioGroup = class(TANDMR_CPanel)
  private
    FItems: TStringList;
    FItemIndex: Integer;
    FColumns: Integer;
    FRadioButtons: TList<TANDMR_CRadioBox>;
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
  FColorUnchecked := clWindow;
  FColorChecked := clActiveCaption;
  FMarkColor := clWindowText;
  FTransparent := False;
  FStyle := crsDefault; // Initialize default style
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
    SetStyle(TRadioBoxItemSettings(Source).Style); // Assign style using setter
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
  Changed; // Font itself will call its OnChange, but we call Changed for the settings object
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

procedure TRadioBoxItemSettings.SetStyle(const Value: TANDMR_CRadioBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

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
  ClearRadioButtons; // Ensure radio buttons are freed
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
  for RadioButton in FRadioButtons do
  begin
    // Apply the style first. This will set defaults.
    RadioButton.CurrentStyle := FItemSettings.Style;

    // If the chosen style is custom, then apply individual properties.
    // Otherwise, the style itself has defined these.
    // However, users might want to override parts of a non-custom style via ItemSettings,
    // so we apply them after setting the style. The RadioBox's internal logic
    // (FUserOverrides) should ideally handle this, but explicit assignment ensures it.

    RadioButton.CaptionSettings.Font.Assign(FItemSettings.Font);
    RadioButton.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    RadioButton.RadioColorChecked := FItemSettings.ColorChecked;
    RadioButton.MarkColor := FItemSettings.MarkColor;
    RadioButton.Transparent := FItemSettings.Transparent;
    // Note: If RadioButton.CurrentStyle was set to a non-custom style,
    // these direct assignments will make its style become crsCustom again
    // if the values differ from the applied style's defaults. This is generally desired.
  end;
  Invalidate; // Invalidate the group panel
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
  FItems.OnChange := nil;
  FItems.Assign(Value);
  if Assigned(FItems) then
    FItems.OnChange := ItemsChanged;

  ClearRadioButtons;
  CreateRadioButtonsFromItems;
  RearrangeRadioButtons;
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
  I: Integer;
begin
  for I := FRadioButtons.Count - 1 downto 0 do
  begin
    FRadioButtons[I].Free; // Free the control
  end;
  FRadioButtons.Clear; // Clear the list
  FItemIndex := -1;
end;

procedure TANDMR_CRadioGroup.CreateRadioButtonsFromItems;
var
  i: Integer;
  Radio: TANDMR_CRadioBox;
begin
  if FItems = nil then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    Radio := TANDMR_CRadioBox.Create(Self);
    Radio.Parent := Self;
    Radio.Caption := FItems[i];
    Radio.OnClick := RadioButtonClick;
    Radio.Tag := i;

    // Apply current group styles to the new radio button
    // Set the style first
    Radio.CurrentStyle := FItemSettings.Style;

    // Then apply specific properties from ItemSettings.
    // If FItemSettings.Style is crsCustom, these will be the primary values.
    // If FItemSettings.Style is a predefined one, these assignments might override
    // some of that style's defaults, effectively making the radiobox's style custom again.
    Radio.CaptionSettings.Font.Assign(FItemSettings.Font);
    Radio.RadioColorUnchecked := FItemSettings.ColorUnchecked;
    Radio.RadioColorChecked := FItemSettings.ColorChecked;
    Radio.MarkColor := FItemSettings.MarkColor;
    Radio.Transparent := FItemSettings.Transparent;

    Radio.Enabled := Self.Enabled; // Ensure new radio matches group's enabled state

    FRadioButtons.Add(Radio);
  end;

  if (FItemIndex >= 0) and (FItemIndex < FRadioButtons.Count) then
    FRadioButtons[FItemIndex].Checked := True
  else
    FItemIndex := -1; // Ensure item index is -1 if no item is selected
end;

procedure TANDMR_CRadioGroup.RearrangeRadioButtons;
var
  i: Integer;
  ColumnWidth, X_Pos, Y_Pos: Double;
  ItemsPerColumn: Integer;
  CurrentColumn, CurrentRowInColumn: Integer;
  LBorderThickness, LCornerRadius: Integer;
  ActualPaddingX, ActualPaddingY: Integer;
  AvailableWidth: Double;
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

  if LCornerRadius > 4 then
  begin
    ActualPaddingX := ActualPaddingX + LCornerRadius div 3;
    ActualPaddingY := ActualPaddingY + LCornerRadius div 3;
  end;

  if Self.CaptionSettings.Visible then
  begin
    var EstimatedCaptionHeight: Integer;
    EstimatedCaptionHeight := Abs(Self.CaptionSettings.Font.Height) + 4;
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
  ApplyRadioBoxStylesToChildren; // Ensure this is called
end;

procedure TANDMR_CRadioGroup.CMMouseEnter(var Message: TMessage);
var
  RadioButton: TANDMR_CRadioBox;
begin
  inherited;
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
  inherited;
  for RadioButton in FRadioButtons do
  begin
    RadioButton.IsGroupHovered := False;
  end;
end;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CRadioGroup]);
end;

end.
