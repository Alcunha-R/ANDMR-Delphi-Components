unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, System.Types,
  ANDMR_ComponentUtils;

type
  TANDMR_CButtonGroupItem = class(TCollectionItem)
  private
    FCaption: string;
    FTag: NativeInt;
    FEnabled: Boolean;
    FVisible: Boolean;
    // Future: FImage: TPicture;

    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetTag(const Value: NativeInt);

  protected
    procedure Changed; // Notify collection of change

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Tag: NativeInt read FTag write SetTag default 0;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TANDMR_CButtonGroupItems = class(TCollection)
  private
    FOwnerControl: TWinControl;
    function GetItem(Index: Integer): TANDMR_CButtonGroupItem;
    procedure SetItem(Index: Integer; Value: TANDMR_CButtonGroupItem);
  protected
    procedure Update(Item: TCollectionItem); override; // To handle item changes
  public
    constructor Create(AOwnerControl: TWinControl);
    procedure ItemChanged(AItem: TANDMR_CButtonGroupItem); // New public method
    function Add: TANDMR_CButtonGroupItem;
    property Items[Index: Integer]: TANDMR_CButtonGroupItem read GetItem write SetItem; default;
  end;

  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FItems: TANDMR_CButtonGroupItems;
    FBorderSettings: TBorderSettings;
    FCaptionSettings: TCaptionSettings; // For common text properties
    FHoverSettings: THoverSettings;
    FSeparatorSettings: TSeparatorSettings;

    FSelectedItemBackgroundColor: TColor;
    FSelectedItemFontColor: TColor;
    FUnselectedItemBackgroundColor: TColor;
    FUnselectedItemFontColor: TColor;
    FHoverFontColor: TColor; // New
    FDisabledItemBackgroundColor: TColor; // New

    FSelectedItemIndex: Integer;
    FHoveredItemIndex: Integer;

    FOnSelectionChanged: TNotifyEvent;
    FOnClick: TNotifyEvent; // Standard OnClick

    function GetSelectedText: string; // Getter for SelectedText

    procedure SetItems(const Value: TANDMR_CButtonGroupItems);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);

    procedure SetSelectedItemBackgroundColor(const Value: TColor);
    procedure SetSelectedItemFontColor(const Value: TColor);
    procedure SetUnselectedItemBackgroundColor(const Value: TColor);
    procedure SetUnselectedItemFontColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor); // New
    procedure SetDisabledItemBackgroundColor(const Value: TColor); // New

    procedure SetSelectedItemIndex(const Value: Integer);

    procedure SettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

    function GetButtonRect(Index: Integer): TRect;
    function GetItemAt(X, Y: Integer): Integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items: TANDMR_CButtonGroupItems read FItems write SetItems;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings; // Common font settings
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;

    property SelectedItemBackgroundColor: TColor read FSelectedItemBackgroundColor write SetSelectedItemBackgroundColor default TColor($00D19A6C);
    property SelectedItemFontColor: TColor read FSelectedItemFontColor write SetSelectedItemFontColor default clWhite;
    property UnselectedItemBackgroundColor: TColor read FUnselectedItemBackgroundColor write SetUnselectedItemBackgroundColor default clWindow;
    property UnselectedItemFontColor: TColor read FUnselectedItemFontColor write SetUnselectedItemFontColor default clBlack;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor default clNone; // New
    property DisabledItemBackgroundColor: TColor read FDisabledItemBackgroundColor write SetDisabledItemBackgroundColor default TColor($00F0F0F0); // New

    property SelectedItemIndex: Integer read FSelectedItemIndex write SetSelectedItemIndex default -1;
    property SelectedText: string read GetSelectedText; // Read-only

    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnClick; // Publish standard OnClick

    // Standard properties
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font; // Will be managed by CaptionSettings mostly
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    // Add other relevant standard events as needed
  end;

procedure Register;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButtonGroup]);
end;

{ TANDMR_CButtonGroupItem }

constructor TANDMR_CButtonGroupItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FVisible := True;
end;

destructor TANDMR_CButtonGroupItem.Destroy;
begin
  inherited Destroy;
end;

procedure TANDMR_CButtonGroupItem.Assign(Source: TPersistent);
begin
  if Source is TANDMR_CButtonGroupItem then
  begin
    SetCaption(TANDMR_CButtonGroupItem(Source).FCaption);
    SetTag(TANDMR_CButtonGroupItem(Source).FTag);
    SetEnabled(TANDMR_CButtonGroupItem(Source).FEnabled);
    SetVisible(TANDMR_CButtonGroupItem(Source).FVisible);
  end
  else
    inherited Assign(Source);
end;

procedure TANDMR_CButtonGroupItem.Changed;
begin
  if Assigned(Collection) and (Collection is TANDMR_CButtonGroupItems) then
    TANDMR_CButtonGroupItems(Collection).ItemChanged(Self);
end;

procedure TANDMR_CButtonGroupItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TANDMR_CButtonGroupItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TANDMR_CButtonGroupItem.SetTag(const Value: NativeInt);
begin
  if FTag <> Value then
  begin
    FTag := Value;
    Changed; // Tag changes might be visually relevant in some cases or for owner logic
  end;
end;

procedure TANDMR_CButtonGroupItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TANDMR_CButtonGroupItems }

constructor TANDMR_CButtonGroupItems.Create(AOwnerControl: TWinControl);
begin
  inherited Create(TANDMR_CButtonGroupItem);
  FOwnerControl := AOwnerControl;
end;

function TANDMR_CButtonGroupItems.Add: TANDMR_CButtonGroupItem;
begin
  Result := TANDMR_CButtonGroupItem(inherited Add);
  // No Invalidate here, TCollection.Changed (called by Item.Changed) will handle it via Update
end;

function TANDMR_CButtonGroupItems.GetItem(Index: Integer): TANDMR_CButtonGroupItem;
begin
  Result := TANDMR_CButtonGroupItem(inherited GetItem(Index));
end;

procedure TANDMR_CButtonGroupItems.SetItem(Index: Integer; Value: TANDMR_CButtonGroupItem);
begin
  inherited SetItem(Index, Value);
  // No Invalidate here, TCollection.Changed will handle it via Update
end;

procedure TANDMR_CButtonGroupItems.Update(Item: TCollectionItem);
var
  OwnerGroup: TANDMR_CButtonGroup;
  OldSelectedIndex: Integer;
  CurrentItemAtIndex: TANDMR_CButtonGroupItem;
begin
  // Guard clause for FOwnerControl
  if not Assigned(FOwnerControl) or not (FOwnerControl is TANDMR_CButtonGroup) then
  begin
    inherited Update(Item); // Call inherited if owner is not what we expect
    Exit;
  end;

  OwnerGroup := TANDMR_CButtonGroup(FOwnerControl);
  OldSelectedIndex := OwnerGroup.SelectedItemIndex;

  inherited Update(Item); // Call inherited TCollection.Update to let it do its work

  // After inherited call, the collection is in its new state.
  // Check if the previously selected index is still valid.
  if (OldSelectedIndex >= 0) then // Only proceed if there was a selection
  begin
    if (OldSelectedIndex < OwnerGroup.Items.Count) then
    begin
      // The item at the previously selected index still exists.
      // Now check if it's still visible and enabled.
      // Ensure the item is TANDMR_CButtonGroupItem before casting
      if (OwnerGroup.Items[OldSelectedIndex] is TANDMR_CButtonGroupItem) then
      begin
        CurrentItemAtIndex := OwnerGroup.Items[OldSelectedIndex] as TANDMR_CButtonGroupItem;
        if not CurrentItemAtIndex.Visible or not CurrentItemAtIndex.Enabled then
        begin
          OwnerGroup.SetSelectedItemIndex(-1); // Deselect because it became invalid
        end;
      end
      else
      begin
        // Should not happen if collection only contains TANDMR_CButtonGroupItem
        OwnerGroup.SetSelectedItemIndex(-1);
      end;
      // If it's still valid and of correct type, the selection remains.
    end
    else // Index is now out of bounds (item was likely deleted, or collection cleared)
    begin
      OwnerGroup.SetSelectedItemIndex(-1); // Deselect
    end;
  end;

  // Always invalidate, as TCollection.Update implies something changed structurally.
  OwnerGroup.Invalidate;
end;

procedure TANDMR_CButtonGroupItems.ItemChanged(AItem: TANDMR_CButtonGroupItem);
var
  OwnerGroup: TANDMR_CButtonGroup;
  SelectionWasChanged: Boolean;
begin
  SelectionWasChanged := False;
  if Assigned(FOwnerControl) and (FOwnerControl is TANDMR_CButtonGroup) then
  begin
    OwnerGroup := TANDMR_CButtonGroup(FOwnerControl);
    if Assigned(AItem) then
    begin
      // If the changed item was selected and is now invisible or disabled, deselect it.
      if (OwnerGroup.SelectedItemIndex = AItem.Index) and
         (not AItem.Visible or not AItem.Enabled) then
      begin
        OwnerGroup.SetSelectedItemIndex(-1); // This calls Invalidate
        SelectionWasChanged := True;
      end;
    end;

    if not SelectionWasChanged then
    begin
      // If selection didn't change (which would have invalidated),
      // but an item's property (like Caption/Tag) changed, we still need to repaint.
      OwnerGroup.Invalidate;
    end;
  end;
end;

{ TANDMR_CButtonGroup }

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
var TempCaptionFont: TFont;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csAcceptsControls, csNeedsBorderPaint];
  Width := 200;
  Height := 30;
  TabStop := True;
  DoubleBuffered := True;
  Cursor := crHandPoint;

  FItems := TANDMR_CButtonGroupItems.Create(Self);

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.Color := clGray;
  FBorderSettings.BackgroundColor := clNone; // Group background usually transparent or handled by theme
  FBorderSettings.Thickness := 1;

  TempCaptionFont := TFont.Create;
  try
    TempCaptionFont.Name := 'Segoe UI';
    TempCaptionFont.Size := 9;

    FCaptionSettings := TCaptionSettings.Create(Self);
    FCaptionSettings.OnChange := SettingsChanged;
    FCaptionSettings.Font.Assign(TempCaptionFont);
    FCaptionSettings.Alignment := taCenter;
    FCaptionSettings.VerticalAlignment := cvaCenter;
  finally
    TempCaptionFont.Free;
  end;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHoverSettings.BackgroundColor := BlendColors(clWindow, clBlack, 0.1); // Subtle hover
  FHoverSettings.BorderColor := clNone; // No border change on hover for individual items by default
  FHoverSettings.FontColor := clNone;   // No font color change on hover by default
  FHoverSettings.Enabled := True;

  FSeparatorSettings := TSeparatorSettings.Create;
  FSeparatorSettings.OnChange := SettingsChanged;
  FSeparatorSettings.Visible := True;
  FSeparatorSettings.Color := clSilver;
  FSeparatorSettings.Thickness := 1;
  FSeparatorSettings.Padding := 0; // No extra padding for separator line itself

  FSelectedItemBackgroundColor := TColor($00D19A6C); // Changed default
  FSelectedItemFontColor := clWhite;
  FUnselectedItemBackgroundColor := clWindow;
  FUnselectedItemFontColor := clBlack;
  FHoverFontColor := clNone; // Default for new property
  FDisabledItemBackgroundColor := TColor($00F0F0F0); // Default for new property

  FSelectedItemIndex := -1;
  FHoveredItemIndex := -1;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FItems.Free;
  FBorderSettings.Free;
  FCaptionSettings.Free;
  FHoverSettings.Free;
  FSeparatorSettings.Free;
  inherited Destroy;
end;

procedure TANDMR_CButtonGroup.Loaded;
begin
  inherited Loaded;
  // Additional setup after properties are loaded
end;

procedure TANDMR_CButtonGroup.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CButtonGroup.HoverSettingsChanged(Sender: TObject);
begin
  Invalidate; // Usually handled by THoverSettings itself if owner control is assigned
end;

procedure TANDMR_CButtonGroup.SetItems(const Value: TANDMR_CButtonGroupItems);
begin
  FItems.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetSeparatorSettings(const Value: TSeparatorSettings);
begin
  FSeparatorSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetSelectedItemBackgroundColor(const Value: TColor);
begin
  if FSelectedItemBackgroundColor <> Value then
  begin
    FSelectedItemBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetSelectedItemFontColor(const Value: TColor);
begin
  if FSelectedItemFontColor <> Value then
  begin
    FSelectedItemFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetUnselectedItemBackgroundColor(const Value: TColor);
begin
  if FUnselectedItemBackgroundColor <> Value then
  begin
    FUnselectedItemBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetUnselectedItemFontColor(const Value: TColor);
begin
  if FUnselectedItemFontColor <> Value then
  begin
    FUnselectedItemFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetHoverFontColor(const Value: TColor);
begin
  if FHoverFontColor <> Value then
  begin
    FHoverFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetDisabledItemBackgroundColor(const Value: TColor);
begin
  if FDisabledItemBackgroundColor <> Value then
  begin
    FDisabledItemBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetSelectedItemIndex(const Value: Integer);
var
  OldIndex: Integer;
  NewIndex: Integer;
begin
  OldIndex := FSelectedItemIndex;

  if Value < -1 then NewIndex := -1
  else if Value >= FItems.Count then NewIndex := OldIndex // Or -1, or FItems.Count -1 if that's preferred for out of bounds
  else NewIndex := Value;

  // Prevent selection of a non-visible or disabled item
  if (NewIndex >= 0) and (NewIndex < FItems.Count) then
  begin
    if not FItems[NewIndex].FVisible or not FItems[NewIndex].FEnabled then
    begin
      if OldIndex = NewIndex then // Tried to set to itself but it's invalid
         NewIndex := -1 // Deselect
      else
         NewIndex := OldIndex; // Revert to old index if attempted selection is invalid
    end;
  end;

  if FSelectedItemIndex <> NewIndex then
  begin
    FSelectedItemIndex := NewIndex;
    Invalidate;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

function TANDMR_CButtonGroup.GetSelectedText: string;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex < FItems.Count) then
  begin
    if FItems[FSelectedItemIndex].FVisible then // Only return text if item is visible
      Result := FItems[FSelectedItemIndex].Caption
    else
      Result := '';
  end
  else
    Result := '';
end;

function TANDMR_CButtonGroup.GetItemAt(X, Y: Integer): Integer;
var
  I: Integer;
  BtnRect: TRect;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
  begin
    if not FItems[I].FVisible then Continue;
    BtnRect := GetButtonRect(I);
    if PtInRect(BtnRect, Point(X, Y)) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TANDMR_CButtonGroup.GetButtonRect(Index: Integer): TRect;
var
  VisibleItemCount: Integer;
  LVisibleIndex: Integer;
  I: Integer;
  ButtonWidth, StartX, TotalWidth: Integer;
  Item: TANDMR_CButtonGroupItem;
begin
  Result := System.Types.Rect(0,0,0,0);
  if (Index < 0) or (Index >= FItems.Count) then Exit;

  Item := FItems[Index];
  if not Item.FVisible then Exit; // Should not happen if called for visible items

  VisibleItemCount := 0;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].FVisible then Inc(VisibleItemCount);

  if VisibleItemCount = 0 then Exit;

  // Calculate which visible item this is
  LVisibleIndex := -1;
  // Note: The redundant checks for Index bounds and FItems[Index].FVisible
  // and the redundant loop for VisibleItemCount have been removed.
  // VisibleItemCount is already correctly calculated at the start of the function.

  // Calculate which visible item this is (LVisibleIndex)
  for I := 0 to Index do
  begin
    if FItems[I].FVisible then Inc(LVisibleIndex);
  end;
  if LVisibleIndex = -1 then Exit; // Should not happen if Index item is visible

  // Use Scaled values for border thickness
  // EffectiveBorderThickness is already calculated in Paint, but if Paint is not called yet, scale here.
  // For consistency, let's assume Paint has set a class field or we pass scaled values.
  // Or, more robustly, always scale here if needed.
  // For now, assume GetButtonRect is called from Paint or similar context where scaling is handled.
  // Let's refine this to ensure it uses scaled values directly.
  var scaledBorderThickness: Integer; // Changed Dim to var
  scaledBorderThickness := Self.Scaled(FBorderSettings.Thickness);
  if not FBorderSettings.Visible then scaledBorderThickness := 0;


  TotalWidth := Max(0, ClientWidth - scaledBorderThickness * 2);
  if VisibleItemCount = 0 then ButtonWidth := TotalWidth
  else ButtonWidth := TotalWidth div VisibleItemCount;

  Result.Top := scaledBorderThickness;
  Result.Bottom := Max(Result.Top, ClientHeight - scaledBorderThickness);
  Result.Left := scaledBorderThickness + (LVisibleIndex * ButtonWidth);
  Result.Right := Result.Left + ButtonWidth;

  // Adjust for last button to fill remaining space due to integer division
  if LVisibleIndex = VisibleItemCount - 1 then
    Result.Right := Max(Result.Left, ClientWidth - scaledBorderThickness);
end;

procedure TANDMR_CButtonGroup.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedItemIndex: Integer;
begin
  inherited;
  if not Enabled then Exit;
  if Button = mbLeft then
  begin
    ClickedItemIndex := GetItemAt(X,Y);
    if (ClickedItemIndex <> -1) and FItems[ClickedItemIndex].FEnabled then
    begin
      SetSelectedItemIndex(ClickedItemIndex);
      if Assigned(FOnClick) then FOnClick(Self); // Trigger general OnClick
    end;
  end;
end;

procedure TANDMR_CButtonGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CurrentHoverItem: Integer;
begin
  inherited;
  if not Enabled then
  begin
    if FHoveredItemIndex <> -1 then
    begin
      FHoveredItemIndex := -1;
      FHoverSettings.StartAnimation(False); // Turn off hover if mouse leaves while disabled
      Invalidate;
    end;
    Exit;
  end;

  CurrentHoverItem := GetItemAt(X,Y);
  if CurrentHoverItem <> FHoveredItemIndex then
  begin
    if FHoveredItemIndex <> -1 then // Mouse moved off a previous item
    begin
       FHoverSettings.StartAnimation(False); // Stop animation for previous item
    end;

    FHoveredItemIndex := CurrentHoverItem;

    if FHoveredItemIndex <> -1 then // Mouse moved onto a new item
    begin
      if FItems[FHoveredItemIndex].FEnabled then
        FHoverSettings.StartAnimation(True); // Start animation for new item
      else
        FHoverSettings.StartAnimation(False); // Ensure no animation for disabled item
    end
    else // Mouse moved into empty space
    begin
       FHoverSettings.StartAnimation(False);
    end;
    Invalidate; // Repaint to reflect hover change
  end
  else if FHoveredItemIndex <> -1 then // Still hovering over the same item
  begin
     if FItems[FHoveredItemIndex].FEnabled and not FHoverSettings.FAnimationTimer.Enabled and FHoverSettings.CurrentAnimationValue = 0 then
     begin
        // This case might happen if animation finished but mouse is still there. Re-trigger if needed.
        // Or rely on StartAnimation(True) to handle this.
        // For simplicity, if it's the same item and enabled, assume hover is active or starting.
     end
     else if not FItems[FHoveredItemIndex].FEnabled and FHoverSettings.CurrentAnimationValue > 0 then
     begin
        FHoverSettings.StartAnimation(False); // Stop animation if item became disabled while hovered
        Invalidate;
     end;
  end;
end;

procedure TANDMR_CButtonGroup.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoveredItemIndex <> -1 then
  begin
    FHoveredItemIndex := -1;
    FHoverSettings.StartAnimation(False);
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FHoveredItemIndex := -1;
    FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPBrush;
  LGPPen: TGPPen;
  I: Integer;
  BtnRect, SepRect, TextRect, ClipRect, ContentRectForPaint: TRect; // Added ContentRectForPaint
  BtnFont: TFont;
  BtnTextColor, BtnBgColor: TColor;
  LHoverProgress: Single;
  EffectiveBorderThickness: Integer;
  ScaledCornerRadius: Integer; // Changed from ActualCornerRadius
  ActualRoundCornerType: TRoundCornerType;
  ActualBorderColor: TColor;
  ActualBackgroundColor: TColor;
  PathRectF: TGPRectF;
  ScaledSeparatorThickness, ScaledSeparatorPadding: Integer;
  ScaledCaptionMarginsLeftRight, ScaledCaptionMarginsTopBottom: Integer;
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf); // For sharper lines

    // --- 1. Calculate Scaled Dimensions ---
    EffectiveBorderThickness := IfThen(FBorderSettings.Visible, Self.Scaled(FBorderSettings.Thickness), 0);
    ScaledCornerRadius := Self.Scaled(FBorderSettings.CornerRadius); // Changed name
    ActualRoundCornerType := FBorderSettings.RoundCornerType;
    ActualBorderColor := FBorderSettings.Color;
    ActualBackgroundColor := FBorderSettings.BackgroundColor; // This is for the group's background

    ScaledSeparatorThickness := IfThen(FSeparatorSettings.Visible, Self.Scaled(FSeparatorSettings.Thickness), 0);
    ScaledSeparatorPadding := Self.Scaled(FSeparatorSettings.Padding);

    // Scale caption margins
    ScaledCaptionMarginsLeftRight := Self.Scaled(FCaptionSettings.Margins.Left) + Self.Scaled(FCaptionSettings.Margins.Right);
    ScaledCaptionMarginsTopBottom := Self.Scaled(FCaptionSettings.Margins.Top) + Self.Scaled(FCaptionSettings.Margins.Bottom);


    if not Enabled then
    begin
      ActualBorderColor := BlendColors(ActualBorderColor, clGray, 0.6);
      if ActualBackgroundColor <> clNone then
        ActualBackgroundColor := BlendColors(ActualBackgroundColor, clGray, 0.8);
    end;

    ClipRect := ClientRect; // The full component rectangle

    // --- 2. Draw Overall Group Background ---
    if ActualBackgroundColor <> clNone then
    begin
        LGPBrush := TGPSolidBrush.Create(ColorToARGB(ActualBackgroundColor, 255));
        try
          PathRectF := MakeRect(ClipRect.Left, ClipRect.Top, ClipRect.Width, ClipRect.Height);
          LGPPath := TGPGraphicsPath.Create;
          try
            CreateGPRoundedPath(LGPPath, PathRectF, ScaledCornerRadius, ActualRoundCornerType);
            LG.FillPath(LGPBrush, LGPPath);
          finally
            LGPPath.Free;
          end;
        finally
          LGPBrush.Free;
        end;
    end;

    // Define the content area (inside the main border)
    ContentRectForPaint := ClipRect;
    InflateRect(ContentRectForPaint, -EffectiveBorderThickness, -EffectiveBorderThickness);


    // --- 3. Draw Buttons and Separators ---
    BtnFont := TFont.Create;
    try
      // Font size in FCaptionSettings.Font is automatically scaled by VCL if ParentFont=False
      BtnFont.Assign(FCaptionSettings.Font);

      for I := 0 to FItems.Count - 1 do
      begin
        if not FItems[I].Visible then Continue;

        BtnRect := GetButtonRect(I); // This now uses Scaled border thickness internally
        if (BtnRect.Width <= 0) or (BtnRect.Height <= 0) then Continue;

        // Determine button style based on state
        if I = FSelectedItemIndex then
        begin
          BtnBgColor := FSelectedItemBackgroundColor;
          BtnTextColor := FSelectedItemFontColor;
          BtnFont.Style := BtnFont.Style + [fsBold];
        end
        else
        begin
          BtnBgColor := FUnselectedItemBackgroundColor;
          BtnTextColor := FUnselectedItemFontColor;
          BtnFont.Style := BtnFont.Style - [fsBold];
        end;

        // Apply hover effect
        LHoverProgress := 0;
        if Enabled and FItems[I].FEnabled and (I = FHoveredItemIndex) and (FHoverSettings.CurrentAnimationValue > 0) then
        begin
          LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
          if FHoverSettings.BackgroundColor <> clNone then
             BtnBgColor := BlendColors(BtnBgColor, FHoverSettings.BackgroundColor, LHoverProgress);
          // Use new HoverFontColor if specified, otherwise default hover logic (which might be FHoverSettings.FontColor if it exists)
          if FHoverFontColor <> clNone then
             BtnTextColor := BlendColors(BtnTextColor, FHoverFontColor, LHoverProgress)
          else if FHoverSettings.FontColor <> clNone then // Fallback to THoverSettings.FontColor if our specific one isn't set
             BtnTextColor := BlendColors(BtnTextColor, FHoverSettings.FontColor, LHoverProgress);
        end;

        if not Enabled or not FItems[I].FEnabled then
        begin
          BtnBgColor := FDisabledItemBackgroundColor; // Use new DisabledItemBackgroundColor
          // Font color for disabled items will be handled by FCaptionSettings.DisabledColor
          // Ensure BtnFont.Color is set appropriately before DrawComponentCaption
          BtnTextColor := FCaptionSettings.DisabledColor; // Directly use the disabled color
          BtnFont.Style := BtnFont.Style - [fsBold];
        end;

        // Draw button background
        // Create a path for the button that respects the main border's curves for first/last items
        LGPPath := TGPGraphicsPath.Create;
        try
          var ItemRectF: TGPRectF;
          ItemRectF := MakeRect(BtnRect.Left, BtnRect.Top, BtnRect.Width, BtnRect.Height);

          // Adjust path for first and last visible items to match outer rounded corners
          var CurrentItemIsFirstVisible, CurrentItemIsLastVisible: Boolean;
          var FirstVisibleIdx, LastVisibleIdx, CurrentVisibleIdx: Integer;
          var J: Integer;

          FirstVisibleIdx := -1; LastVisibleIdx := -1; CurrentVisibleIdx := -1;
          for J := 0 to FItems.Count - 1 do
          begin
            if FItems[J].Visible then
            begin
              if FirstVisibleIdx = -1 then FirstVisibleIdx := J;
              LastVisibleIdx := J;
              if J = I then CurrentVisibleIdx := FirstVisibleIdx; // Simplified: just need to know if it *is* first/last
            end;
          end;
          CurrentItemIsFirstVisible := (I = FirstVisibleIdx);
          CurrentItemIsLastVisible := (I = LastVisibleIdx);

          if ScaledCornerRadius > 0 then
          begin
            // Simplified: Individual buttons are rectangular.
            // The main group border provides the overall rounded shape.
            // Advanced clipping for first/last buttons to match outer curve is complex and deferred.
            LGPPath.AddRectangle(ItemRectF);

          if BtnBgColor <> clNone then
          begin
            LGPBrush := TGPSolidBrush.Create(ColorToARGB(BtnBgColor, 255));
            try
              LG.FillPath(LGPBrush, LGPPath);
            finally
              LGPBrush.Free;
            end;
          end;
        finally
          LGPPath.Free;
        end;


        // Draw button text
        TextRect := BtnRect;
        // Use scaled margins for text inflation
        InflateRect(TextRect, -(Self.Scaled(FCaptionSettings.Margins.Left) + Self.Scaled(FCaptionSettings.Margins.Right)) div 2,
                               -(Self.Scaled(FCaptionSettings.Margins.Top) + Self.Scaled(FCaptionSettings.Margins.Bottom)) div 2);


        if (TextRect.Width > 0) and (TextRect.Height > 0) then
        begin
          DrawComponentCaption(Self.Canvas, TextRect, FItems[I].Caption, BtnFont, BtnTextColor,
                               FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment,
                               False, 255);
        end;

        // Draw separator
        var IsLastVisibleButton: Boolean = True;
        for J := I + 1 to FItems.Count - 1 do
        begin
            if FItems[J].FVisible then
            begin
                IsLastVisibleButton := False;
                Break;
            end;
        end;

        if FSeparatorSettings.Visible and (ScaledSeparatorThickness > 0) and not IsLastVisibleButton then
        begin
          SepRect.Top := BtnRect.Top + ScaledSeparatorPadding;
          SepRect.Bottom := BtnRect.Bottom - ScaledSeparatorPadding;
          SepRect.Left := BtnRect.Right - (ScaledSeparatorThickness div 2);
          SepRect.Right := SepRect.Left + ScaledSeparatorThickness;

          if (SepRect.Width > 0) and (SepRect.Height > 0) then
          begin
            var SepColor: TColor;
            SepColor := FSeparatorSettings.Color;
            if not Enabled then SepColor := BlendColors(SepColor, clGray, 0.6);

            LGPBrush := TGPSolidBrush.Create(ColorToARGB(SepColor, 255));
            try
              LG.FillRectangle(LGPBrush, MakeRect(SepRect.Left, SepRect.Top, SepRect.Width, SepRect.Height));
            finally
              LGPBrush.Free;
            end;
          end;
        end;
      end; // for loop
    finally
      BtnFont.Free;
    end;

    // --- 4. Draw Overall Group Border (on top of buttons) ---
    if EffectiveBorderThickness > 0 then
    begin
      // Path for border should be inset by half its thickness
      PathRectF := MakeRect(ClipRect.Left + EffectiveBorderThickness / 2.0,
                            ClipRect.Top + EffectiveBorderThickness / 2.0,
                            ClipRect.Width - EffectiveBorderThickness,
                            ClipRect.Height - EffectiveBorderThickness);
      PathRectF.Width := Max(0, PathRectF.Width);
      PathRectF.Height := Max(0, PathRectF.Height);

      LGPPath := TGPGraphicsPath.Create;
      try
        // Corner radius for the border path itself should be adjusted if border is thick
        var BorderPathRadius: Single;
        BorderPathRadius := ScaledCornerRadius - EffectiveBorderThickness / 2.0;
        CreateGPRoundedPath(LGPPath, PathRectF, Max(0, BorderPathRadius), ActualRoundCornerType);

        if LGPPath.GetPointCount > 0 then
        begin
            LGPPen := TGPPen.Create(ColorToARGB(ActualBorderColor, 255), EffectiveBorderThickness);
            try
              LG.DrawPath(LGPPen, LGPPath);
            finally
              LGPPen.Free;
            end;
        end;
      finally
        LGPPath.Free;
      end;
    end;

    // --- 5. Draw Focus Rect ---
    if Focused and TabStop and Enabled and (FSelectedItemIndex <> -1) then
    begin
        BtnRect := GetButtonRect(FSelectedItemIndex);
        InflateRect(BtnRect, -Self.Scaled(2), -Self.Scaled(2)); // Scale the inset for focus rect
        if (BtnRect.Width > 0) and (BtnRect.Height > 0) then
        begin
            // Consider a GDI+ focus rect for consistency if default DrawFocusRect is not ideal
            DrawFocusRect(Canvas.Handle, BtnRect);
        end;
    end;

  finally
    LG.Free;
  end;
end;

end.
