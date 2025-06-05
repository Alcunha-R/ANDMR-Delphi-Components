unit ANDMR_CControlTab;

//------------------------------------------------------------------------------
// Unit Name: ANDMR_CControlTab
// Purpose:   Defines a custom tab control (TANDMR_CControlTab) and its
//            associated tab sheet (TANDMR_CTabSheet). This provides a
//            flexible, stylable alternative to standard VCL tab controls.
//------------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, ANDMR_CPanel, ANDMR_ComponentUtils, System.Contnrs,
  System.Types, // Added for TRect, TPoint
  Winapi.Messages, // Added for CM_MOUSEENTER, CM_MOUSELEAVE
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math; // Added for GDI+

type
  { TANDMR_CTabSheet }
  // Represents a single page or tab within a TANDMR_CControlTab.
  // It inherits from TANDMR_CPanel, allowing it to act as a container
  // for other VCL controls. Its appearance is largely determined by the
  // parent TANDMR_CControlTab, but it can host its own child controls.
  TANDMR_CTabSheet = class(TANDMR_CPanel)
  private
    FCaption: string;
    FIcon: TPicture;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The text displayed on the tab button for this sheet.
    property Caption: string read FCaption write FCaption;
    // The icon displayed on the tab button for this sheet (optional).
    property Icon: TPicture read FIcon write FIcon;
  end;

  // Defines the possible positions for the tab buttons within the TANDMR_CControlTab.
  TANDMR_CTabSheetPosition = (tpTop, tpBottom, tpLeft, tpRight);

  { TANDMR_CControlTab }
  // A tabbed control, similar in function to TPageControl, that manages a
  // collection of TANDMR_CTabSheet instances. It provides customizable
  // appearance for tab buttons and the content area, and handles the
  // display and activation of individual tab sheets.
  TANDMR_CControlTab = class(TCustomControl)
  private
    FTabSheets: TObjectList; // Internal list of TANDMR_CTabSheet instances.
    FActiveTabSheetIndex: Integer; // Index of the currently visible tab sheet.
    FTabPosition: TANDMR_CTabSheetPosition; // Position of the tab buttons.
    FTabButtonHeight: Integer; // Height of tab buttons when TabPosition is Top or Bottom.
    FTabButtonWidth: Integer;  // Width of tab buttons when TabPosition is Left or Right; also min width for Top/Bottom.
    FTabButtonPadding: TANDMR_Margins; // Padding within each tab button, around text and icon.
    FTabButtonSpacing: Integer; // Spacing between adjacent tab buttons.

    // Styling Properties
    FInactiveTabStyle: TBorderSettings; // Visual style for inactive tab buttons.
    FActiveTabStyle: TBorderSettings;   // Visual style for the active tab button.
    FHoverTabStyle: TBorderSettings;    // Visual style for a tab button when the mouse is hovering over it.
    FTabCaptionSettings: TCaptionSettings; // Text style for captions on inactive/hovered tab buttons.
    FActiveTabCaptionSettings: TCaptionSettings; // Text style for the caption on the active tab button.

    FHoveredTabIndex: Integer; // Index of the tab button currently under the mouse cursor (-1 if none).
    FTabButtonRects: array of TRect; // Array storing the calculated screen rectangles for each tab button. Used for hit-testing and painting.

    // Property Setters
    procedure SetTabPosition(Value: TANDMR_CTabSheetPosition);
    function GetActiveTabSheet: TANDMR_CTabSheet; // Retrieves the currently active TANDMR_CTabSheet instance.
    procedure SetTabButtonHeight(Value: Integer);
    procedure SetTabButtonWidth(Value: Integer);
    procedure SetTabButtonPadding(Value: TANDMR_Margins);
    procedure SetTabButtonSpacing(Value: Integer);
    procedure TabButtonPaddingChanged(Sender: TObject); // Handles OnChange event from FTabButtonPadding.

    // Setters for style properties
    procedure SetInactiveTabStyle(Value: TBorderSettings);
    procedure SetActiveTabStyle(Value: TBorderSettings);
    procedure SetHoverTabStyle(Value: TBorderSettings);
    procedure SetTabCaptionSettings(Value: TCaptionSettings);
    procedure SetActiveTabCaptionSettings(Value: TCaptionSettings);
    procedure StyleChanged(Sender: TObject); // Common OnChange handler for TBorderSettings and TCaptionSettings style objects.
    procedure SetActiveTabSheetIndex(Value: Integer); // Sets the currently active/visible tab sheet by its index.

  protected
    // Handles all custom painting for the control, including tab buttons and content area borders.
    procedure Paint; override;
    // Called when the control is resized; recalculates layout and invalidates.
    procedure Resize; override;
    // Handles mouse entering the control area, used for hover effects.
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    // Handles mouse leaving the control area, used for hover effects.
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    // Tracks mouse movement to update hover effects on tab buttons.
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // Handles mouse clicks to activate tab sheets.
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // Determines which tab button (if any) is at the given screen coordinates. Returns tab index or -1.
    function GetTabButtonAt(X, Y: Integer): Integer;
    // Calculates the bounding rectangles for the tab button area and the content display area based on TabPosition and metrics.
    procedure CalculateLayout(var TabButtonsRegion: TRect; var ContentRegion: TRect); virtual;
    // Updates the visibility and bounds of all managed TANDMR_CTabSheet instances based on the active tab.
    procedure UpdateTabSheetBounds; virtual;
    // Updates the stored FTabButtonRects for each tab button, typically called after layout-affecting changes.
    procedure RecalculateTabButtonRects;
    // Handles being notified when components (specifically TANDMR_CTabSheet) are added or removed from this control
    // (e.g., at design-time or runtime by changing Parent property).
    // This method manages the internal FTabSheets list accordingly.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Programmatically creates a new TANDMR_CTabSheet with the given caption, adds it to the control, and parents it.
    function AddTabSheet(ACaption: string): TANDMR_CTabSheet;
    // Removes and frees the specified TANDMR_CTabSheet instance from the control.
    procedure RemoveTabSheet(ATabSheet: TANDMR_CTabSheet);
    // Removes and frees the tab sheet at the specified index.
    procedure RemoveTabSheetByIndex(AIndex: Integer);
    // Removes and frees all tab sheets from the control.
    procedure ClearTabSheets;
  published
    // Provides read-only access to the currently active TANDMR_CTabSheet instance.
    property ActiveTabSheet: TANDMR_CTabSheet read GetActiveTabSheet;
    // Provides read-only access to the internal list of TANDMR_CTabSheet instances.
    // Note: Modifying this list directly is not recommended; use AddTabSheet, RemoveTabSheet, etc.
    property TabSheets: TObjectList read FTabSheets;
    // Determines where the tab buttons are displayed (top, bottom, left, or right).
    property TabPosition: TANDMR_CTabSheetPosition read FTabPosition write SetTabPosition default tpTop;
    // Specifies the height of tab buttons when TabPosition is Top or Bottom.
    // For Left/Right positions, this also influences the calculation of individual tab button heights if they contain icons/multi-line text.
    property TabButtonHeight: Integer read FTabButtonHeight write SetTabButtonHeight default 30;
    // Specifies the width of tab buttons when TabPosition is Left or Right.
    // For Top/Bottom positions, this acts as a minimum width for each tab button.
    property TabButtonWidth: Integer read FTabButtonWidth write SetTabButtonWidth default 100;
    // Defines the internal padding for each tab button, affecting the space around its caption and icon.
    property TabButtonPadding: TANDMR_Margins read FTabButtonPadding write SetTabButtonPadding;
    // Specifies the spacing in pixels between adjacent tab buttons.
    property TabButtonSpacing: Integer read FTabButtonSpacing write SetTabButtonSpacing default 4;

    // The zero-based index of the currently visible/active tab sheet.
    // Setting this property changes the active tab.
    property ActiveTabSheetIndex: Integer read FActiveTabSheetIndex write SetActiveTabSheetIndex;

    // Defines the visual style (border, background, corners) for inactive tab buttons.
    property InactiveTabStyle: TBorderSettings read FInactiveTabStyle write SetInactiveTabStyle;
    // Defines the visual style for the currently active tab button.
    property ActiveTabStyle: TBorderSettings read FActiveTabStyle write SetActiveTabStyle;
    // Defines the visual style for a tab button when the mouse cursor is hovering over it.
    property HoverTabStyle: TBorderSettings read FHoverTabStyle write SetHoverTabStyle;
    // Defines the text appearance (font, color, alignment) for captions on inactive and hovered tab buttons.
    property TabCaptionSettings: TCaptionSettings read FTabCaptionSettings write SetTabCaptionSettings;
    // Defines the text appearance for the caption on the currently active tab button.
    property ActiveTabCaptionSettings: TCaptionSettings read FActiveTabCaptionSettings write SetActiveTabCaptionSettings;
  end;

procedure Register;

implementation

uses Vcl.Themes, // For StyleServices (themed text drawing)
     Vcl.Imaging.jpeg; // Example: if TJPEGImage might be used in TPicture for icons

{ TANDMR_CTabSheet }

constructor TANDMR_CTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := TPicture.Create;
  // Default to transparent background for tab sheets initially.
  // The actual content area background will be painted by this panel.
  Self.ControlStyle := Self.ControlStyle + [csOpaque]; // Ensures it paints itself
  Self.Color := clNone; // Default to no specific background color, relying on inherited or theme.
  Self.BorderSettings.Visible := False; // Typically, individual sheets don't show borders; the CControlTab might draw one around the content area.
end;

destructor TANDMR_CTabSheet.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

{ TANDMR_CControlTab }

constructor TANDMR_CControlTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Standard control styles for mouse interaction, opacity handling, and theming.
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csCaptureMouse, csClickEvents];
  FTabSheets := TObjectList.Create(True); // True: TObjectList owns (and frees) the TANDMR_CTabSheet objects it holds.
  FActiveTabSheetIndex := -1; // No tab active initially.
  FTabPosition := tpTop; // Default tab position.

  // Default metrics for tab buttons
  FTabButtonHeight := 30;
  FTabButtonWidth := 100;
  FTabButtonPadding := TANDMR_Margins.Create;
  FTabButtonPadding.OnChange := TabButtonPaddingChanged; // Hook up OnChange for padding.
//  FTabButtonPadding.SetBounds(4, 4, 4, 4);
  FTabButtonSpacing := 4;

  // Initialize Styling Properties with default values
  FInactiveTabStyle := TBorderSettings.Create;
  FInactiveTabStyle.OnChange := StyleChanged;
  FInactiveTabStyle.BackgroundColor := clBtnFace;
  FInactiveTabStyle.Color := clGrayText;
  FInactiveTabStyle.CornerRadius := 3;
  FInactiveTabStyle.RoundCornerType := rctTop;

  FActiveTabStyle := TBorderSettings.Create;
  FActiveTabStyle.OnChange := StyleChanged;
  FActiveTabStyle.BackgroundColor := clWindow;
  FActiveTabStyle.Color := clHighlight;
  FActiveTabStyle.CornerRadius := 3;
  FActiveTabStyle.RoundCornerType := rctTop;

  FHoverTabStyle := TBorderSettings.Create;
  FHoverTabStyle.OnChange := StyleChanged;
  FHoverTabStyle.BackgroundColor := LighterColor(clBtnFace, 10);
  FHoverTabStyle.Color := clRed;
  FHoverTabStyle.CornerRadius := 3;
  FHoverTabStyle.RoundCornerType := rctTop;

  FTabCaptionSettings := TCaptionSettings.Create(Self); // Owner is Self for resource management.
  FTabCaptionSettings.OnChange := StyleChanged;
  FTabCaptionSettings.Font.Color := clBtnText;
  FTabCaptionSettings.Alignment := taCenter;
  FTabCaptionSettings.VerticalAlignment := cvaCenter;

  FActiveTabCaptionSettings := TCaptionSettings.Create(Self);
  FActiveTabCaptionSettings.OnChange := StyleChanged;
  FActiveTabCaptionSettings.Font.Color := clHighlightText;
  FActiveTabCaptionSettings.Alignment := taCenter;
  FActiveTabCaptionSettings.VerticalAlignment := cvaCenter;

  FHoveredTabIndex := -1; // No tab hovered initially.

  Width := 300; // Default control width.
  Height := 200; // Default control height.
end;

destructor TANDMR_CControlTab.Destroy;
begin
  FTabButtonPadding.OnChange := nil; // Unhook event before freeing.
  FTabButtonPadding.Free;

  FInactiveTabStyle.OnChange := nil;
  FInactiveTabStyle.Free;
  FActiveTabStyle.OnChange := nil;
  FActiveTabStyle.Free;
  FHoverTabStyle.OnChange := nil;
  FHoverTabStyle.Free;
  FTabCaptionSettings.OnChange := nil;
  FTabCaptionSettings.Free;
  FActiveTabCaptionSettings.OnChange := nil;
  FActiveTabCaptionSettings.Free;

  FTabSheets.Free; // Frees the list and all TANDMR_CTabSheet instances it owns.
  SetLength(FTabButtonRects, 0); // Clear the array of button rectangles.
  inherited Destroy;
end;

// Common OnChange handler for TBorderSettings and TCaptionSettings style objects.
// Triggers recalculation of tab button layouts and repaints the control.
procedure TANDMR_CControlTab.StyleChanged(Sender: TObject);
begin
  // If the sender is one of the caption settings, or any other setting that might affect geometry.
  // Font changes in caption settings or thickness changes in border settings can affect layout.
  if (Sender = FTabCaptionSettings) or (Sender = FActiveTabCaptionSettings) or
     (Sender = FInactiveTabStyle) or (Sender = FActiveTabStyle) or (Sender = FHoverTabStyle) then
  begin
    RecalculateTabButtonRects;
  end;
  Invalidate; // Always repaint when a style changes.
end;

// Handles OnChange event from FTabButtonPadding.
// Recalculates button rectangles and repaints the control.
procedure TANDMR_CControlTab.TabButtonPaddingChanged(Sender: TObject);
begin
  RecalculateTabButtonRects;
  Invalidate;
end;

procedure TANDMR_CControlTab.SetTabButtonHeight(Value: Integer);
begin
  if FTabButtonHeight <> Value then
  begin
    FTabButtonHeight := Value;
    RecalculateTabButtonRects;
    Invalidate;
  end;
end;

procedure TANDMR_CControlTab.SetTabButtonWidth(Value: Integer);
begin
  if FTabButtonWidth <> Value then
  begin
    FTabButtonWidth := Value;
    RecalculateTabButtonRects;
    Invalidate;
  end;
end;

procedure TANDMR_CControlTab.SetTabButtonPadding(Value: TANDMR_Margins);
begin
  FTabButtonPadding.Assign(Value);
  // OnChange event (TabButtonPaddingChanged) will trigger RecalculateTabButtonRects and Invalidate.
end;

procedure TANDMR_CControlTab.SetTabButtonSpacing(Value: Integer);
begin
  if FTabButtonSpacing <> Value then
  begin
    FTabButtonSpacing := Value;
    RecalculateTabButtonRects;
    Invalidate;
  end;
end;

// Setters for style properties that are objects (TBorderSettings, TCaptionSettings).
// They use Assign to copy values and rely on the OnChange mechanism for updates.
procedure TANDMR_CControlTab.SetInactiveTabStyle(Value: TBorderSettings);
begin
  FInactiveTabStyle.Assign(Value);
end;

procedure TANDMR_CControlTab.SetActiveTabStyle(Value: TBorderSettings);
begin
  FActiveTabStyle.Assign(Value);
end;

procedure TANDMR_CControlTab.SetHoverTabStyle(Value: TBorderSettings);
begin
  FHoverTabStyle.Assign(Value);
end;

procedure TANDMR_CControlTab.SetTabCaptionSettings(Value: TCaptionSettings);
begin
  FTabCaptionSettings.Assign(Value);
end;

procedure TANDMR_CControlTab.SetActiveTabCaptionSettings(Value: TCaptionSettings);
begin
  FActiveTabCaptionSettings.Assign(Value);
  // OnChange event (StyleChanged) will trigger RecalculateTabButtonRects and Invalidate.
end;

// Sets the currently active/visible tab sheet by its index.
// Updates UI and visibility accordingly.
procedure TANDMR_CControlTab.SetActiveTabSheetIndex(Value: Integer);
begin
  // Validate index: allow -1 to deselect all, or must be within current tab sheet range.
  if (Value >= -1) and (Value < FTabSheets.Count) then
  begin
    if FActiveTabSheetIndex <> Value then
    begin
      FActiveTabSheetIndex := Value;
      UpdateTabSheetBounds; // Update visibility and bounds of tab sheets.
      Invalidate; // Repaint to reflect changes in tab button styles (active vs. inactive).
      // TODO: Consider adding an OnChange event here for external notification.
    end;
  end
  // Special case: if there are no tabs, ensure index is -1.
  else if (FTabSheets.Count = 0) and (Value = -1) then
  begin
     if FActiveTabSheetIndex <> Value then // Though it should already be -1 if count is 0 due to RemoveTab logic
     begin
        FActiveTabSheetIndex := Value;
        UpdateTabSheetBounds; // Should effectively do nothing but good for consistency.
        Invalidate;
     end;
  end;
end;

procedure TANDMR_CControlTab.SetTabPosition(Value: TANDMR_CTabSheetPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    // Update RoundCornerType defaults based on new TabPosition for a more intuitive look.
    // Users can override these manually if desired.
    case Value of
      tpTop: begin
        FInactiveTabStyle.RoundCornerType := rctTop;
        FActiveTabStyle.RoundCornerType := rctTop;
        FHoverTabStyle.RoundCornerType := rctTop;
      end;
      tpBottom: begin
        FInactiveTabStyle.RoundCornerType := rctBottom;
        FActiveTabStyle.RoundCornerType := rctBottom;
        FHoverTabStyle.RoundCornerType := rctBottom;
      end;
      tpLeft: begin
        FInactiveTabStyle.RoundCornerType := rctLeft;
        FActiveTabStyle.RoundCornerType := rctLeft;
        FHoverTabStyle.RoundCornerType := rctLeft;
      end;
      tpRight: begin
        FInactiveTabStyle.RoundCornerType := rctRight;
        FActiveTabStyle.RoundCornerType := rctRight;
        FHoverTabStyle.RoundCornerType := rctRight;
      end;
    end;
    RecalculateTabButtonRects; // Tab position drastically changes layout.
    UpdateTabSheetBounds;    // Content area also changes.
    Invalidate;
  end;
end;

// Retrieves the currently active TANDMR_CTabSheet instance.
// Returns nil if no tab is active or index is out of bounds.
function TANDMR_CControlTab.GetActiveTabSheet: TANDMR_CTabSheet;
begin
  if (FActiveTabSheetIndex >= 0) and (FActiveTabSheetIndex < FTabSheets.Count) then
    Result := TANDMR_CTabSheet(FTabSheets[FActiveTabSheetIndex])
  else
    Result := nil;
end;

// Programmatically creates a new TANDMR_CTabSheet with the given caption,
// adds it to the control by setting its parent, which triggers the Notification mechanism.
function TANDMR_CControlTab.AddTabSheet(ACaption: string): TANDMR_CTabSheet;
var
  NewSheet: TANDMR_CTabSheet;
begin
  NewSheet := TANDMR_CTabSheet.Create(Self); // Owner is Self for lifetime management.
  NewSheet.Caption := ACaption;
  NewSheet.Name := 'TabSheet' + IntToStr(FTabSheets.Count); // Suggest a unique name.
  NewSheet.Parent := Self; // This assignment triggers the Notification(opInsert) logic.
  Result := NewSheet;
end;

// Removes and frees the specified TANDMR_CTabSheet instance from the control.
// Relies on the sheet's Free method to trigger Notification for list management.
procedure TANDMR_CControlTab.RemoveTabSheet(ATabSheet: TANDMR_CTabSheet);
var
  idx: Integer;
begin
  if ATabSheet = nil then Exit;
  // Check if the sheet is actually managed by this control.
  idx := FTabSheets.IndexOf(ATabSheet);
  if idx <> -1 then
  begin
    ATabSheet.Free; // Freeing the component will trigger Notification(opRemove).
  end
  else
  begin
    // Optionally raise an exception or log if the sheet is not part of this control.
    // For now, do nothing if not found in the internal list, as it might have been
    // parented to something else or already freed.
  end;
end;

// Removes and frees the tab sheet at the specified index.
// Relies on the sheet's Free method to trigger Notification.
procedure TANDMR_CControlTab.RemoveTabSheetByIndex(AIndex: Integer);
var
  Sheet: TANDMR_CTabSheet;
begin
  if (AIndex >= 0) and (AIndex < FTabSheets.Count) then
  begin
    Sheet := TANDMR_CTabSheet(FTabSheets[AIndex]);
    Sheet.Free; // This will trigger Notification(opRemove).
  end
  else
  begin
    // Optionally raise an exception for invalid index (e.g., EListError.Create('Index out of bounds')).
  end;
end;

// Removes and frees all tab sheets from the control.
// Iteratively frees sheets, allowing Notification to handle each removal and UI update.
procedure TANDMR_CControlTab.ClearTabSheets;
begin
  // Iterate by always freeing the first sheet until none are left.
  // This is safer than a typical for-loop when the list is modified during iteration (by Notification).
  while FTabSheets.Count > 0 do
  begin
    TANDMR_CTabSheet(FTabSheets[0]).Free;
  end;

  // FActiveTabSheetIndex should be -1 after all sheets are removed due to Notification logic.
  // This is a safeguard. SetActiveTabSheetIndex handles Invalidate and UpdateTabSheetBounds.
  if FActiveTabSheetIndex <> -1 then
     SetActiveTabSheetIndex(-1);

  // Explicitly recalculate and invalidate in case no tabs were present to begin with
  // or if SetActiveTabSheetIndex(-1) didn't trigger it because it was already -1.
  RecalculateTabButtonRects;
  Invalidate;
end;

// Handles being notified when components (specifically TANDMR_CTabSheet) are added or removed from this control
// (e.g., at design-time or runtime by changing Parent property).
// This method manages the internal FTabSheets list accordingly.
procedure TANDMR_CControlTab.Notification(AComponent: TComponent; Operation: TOperation);
var
  Sheet: TANDMR_CTabSheet;
  RemovedIndex: Integer;
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TANDMR_CTabSheet then
  begin
    Sheet := TANDMR_CTabSheet(AComponent);
    case Operation of
      opInsert: // A component is being added (e.g., Parent property set to Self)
        begin
          if FTabSheets.IndexOf(Sheet) = -1 then // Only add if not already in our list
          begin
            // Sheet.Parent should already be Self if opInsert is for this control.
            // If Parent is not Self, it's an error or unexpected state.
            // For robustness, one might check Sheet.Parent = Self here.
            FTabSheets.Add(Sheet); // Add to our internal list of managed sheets.

            // If it's the first tab added, or no tab is currently active, make this new one active.
            if (FTabSheets.Count = 1) or (FActiveTabSheetIndex = -1) then
            begin
              SetActiveTabSheetIndex(FTabSheets.Count - 1); // Activates and makes visible.
            end
            else // Otherwise, the new tab is added but not active by default.
            begin
              Sheet.Visible := False; // Ensure it's not visible if not active.
            end;
            RecalculateTabButtonRects; // Update button layout.
            Invalidate; // Repaint the control.
          end;
        end;
      opRemove: // A component is being removed (e.g., being freed or Parent changed)
        begin
          RemovedIndex := FTabSheets.IndexOf(Sheet);
          if RemovedIndex <> -1 then // Only act if it was in our list
          begin
            FTabSheets.Remove(Sheet); // Remove from our internal list.

            if FActiveTabSheetIndex = RemovedIndex then // If the active tab was removed
            begin
              if FTabSheets.Count > 0 then
                SetActiveTabSheetIndex(0) // Make the first tab active (index 0).
              else
                SetActiveTabSheetIndex(-1); // No tabs left, set index to -1.
            end
            else if FActiveTabSheetIndex > RemovedIndex then // If a tab before the active one was removed
            begin
               // The active tab's index has shifted down by one.
               SetActiveTabSheetIndex(FActiveTabSheetIndex - 1);
            end;
            // If a tab after the active one was removed, FActiveTabSheetIndex remains correct.

            RecalculateTabButtonRects; // Update button layout.
            Invalidate; // Repaint the control.
          end;
        end;
    end;
  end;
end;

// Calculates the bounding rectangles for the tab button area and the content display area
// based on the current TabPosition and various button metric properties.
procedure TANDMR_CControlTab.CalculateLayout(var TabButtonsRegion: TRect; var ContentRegion: TRect);
var
  EffectiveTabButtonSize: Integer;
begin
  TabButtonsRegion := ClientRect; // Start with the whole client area.
  ContentRegion := ClientRect;    // Same for content initially.

  // Adjust regions based on tab position.
  case FTabPosition of
    tpTop:
      begin
        EffectiveTabButtonSize := FTabButtonHeight + FTabButtonPadding.Top + FTabButtonPadding.Bottom;
        TabButtonsRegion.Bottom := TabButtonsRegion.Top + EffectiveTabButtonSize;
        ContentRegion.Top := TabButtonsRegion.Bottom; // Content area is below tab buttons.
      end;
    tpBottom:
      begin
        EffectiveTabButtonSize := FTabButtonHeight + FTabButtonPadding.Top + FTabButtonPadding.Bottom;
        TabButtonsRegion.Top := TabButtonsRegion.Bottom - EffectiveTabButtonSize;
        ContentRegion.Bottom := TabButtonsRegion.Top; // Content area is above tab buttons.
      end;
    tpLeft:
      begin
        EffectiveTabButtonSize := FTabButtonWidth + FTabButtonPadding.Left + FTabButtonPadding.Right;
        TabButtonsRegion.Right := TabButtonsRegion.Left + EffectiveTabButtonSize;
        ContentRegion.Left := TabButtonsRegion.Right; // Content area is to the right of tab buttons.
      end;
    tpRight:
      begin
        EffectiveTabButtonSize := FTabButtonWidth + FTabButtonPadding.Left + FTabButtonPadding.Right;
        TabButtonsRegion.Left := TabButtonsRegion.Right - EffectiveTabButtonSize;
        ContentRegion.Right := TabButtonsRegion.Left; // Content area is to the left of tab buttons.
      end;
  end;
end;

// Updates the stored FTabButtonRects for each tab button.
// This is crucial for hit-testing (mouse clicks) and for painting each button in the correct location.
// It should be called whenever the control is resized, tabs are added/removed,
// or any property affecting tab button size or layout changes (e.g., padding, spacing, font).
procedure TANDMR_CControlTab.RecalculateTabButtonRects;
var
  TabButtonsRegion, ContentRegion: TRect;
  I: Integer;
  CurrentX, CurrentY: Integer; // Current drawing origin for the next tab button.
  Sheet: TANDMR_CTabSheet;
  ButtonTextWidth, ButtonIconWidth, ButtonWidth, ButtonHeight: Integer;
  TempRect: TRect;
begin
  CalculateLayout(TabButtonsRegion, ContentRegion); // First, get the overall region for tab buttons.
  SetLength(FTabButtonRects, FTabSheets.Count); // Ensure array is sized correctly.

  // Initialize drawing position based on TabPosition
  CurrentX := TabButtonsRegion.Left;
  CurrentY := TabButtonsRegion.Top;
  if FTabPosition in [tpTop, tpBottom] then
    CurrentX := CurrentX + FTabButtonPadding.Left // Start with left padding for horizontal tabs.
  else // tpLeft, tpRight
    CurrentY := CurrentY + FTabButtonPadding.Top;  // Start with top padding for vertical tabs.

  for I := 0 to FTabSheets.Count - 1 do
  begin
    Sheet := TANDMR_CTabSheet(FTabSheets[I]);
    ButtonIconWidth := 0; // Reset for current tab.

    // Calculate width needed for the icon, if present.
    if Assigned(Sheet.Icon) and (Sheet.Icon.Graphic <> nil) and (Sheet.Icon.Width > 0) and (Sheet.Icon.Height > 0) then
    begin
      var MaxIconDim, IconSourceWidth, IconSourceHeight: Integer;
      IconSourceWidth := Sheet.Icon.Width;
      IconSourceHeight := Sheet.Icon.Height;

      if FTabPosition in [tpTop, tpBottom] then // Icon height is constrained by tab button height.
      begin
        MaxIconDim := Self.FTabButtonHeight - FTabButtonPadding.Top - FTabButtonPadding.Bottom;
        if (IconSourceHeight > 0) and (MaxIconDim > 0) then
          ButtonIconWidth := MulDiv(IconSourceWidth, MaxIconDim, IconSourceHeight)
        else
          ButtonIconWidth := IconSourceWidth; // Fallback if calculation isn't possible.
      end
      else // tpLeft, tpRight. Icon width is constrained by overall tab button width.
      begin
         MaxIconDim := Self.FTabButtonHeight - FTabButtonPadding.Top - FTabButtonPadding.Bottom; // Icon height constraint
         if (IconSourceHeight > 0) and (MaxIconDim > 0) then
             ButtonIconWidth := MulDiv(IconSourceWidth, MaxIconDim, IconSourceHeight)
         else ButtonIconWidth := IconSourceWidth;
      end;
      ButtonIconWidth := ButtonIconWidth + FTabButtonPadding.Left; // Add left padding for icon within button.
    end;

    // Calculate width needed for the caption text.
    Canvas.Font.Assign(FTabCaptionSettings.Font); // Use base caption style for measurement.
    ButtonTextWidth := Canvas.TextWidth(Sheet.Caption);

    // Calculate actual button dimensions based on content and TabPosition.
    case FTabPosition of
      tpTop, tpBottom:
        begin
          ButtonWidth := ButtonTextWidth + ButtonIconWidth + FTabButtonPadding.Left + FTabButtonPadding.Right;
          ButtonWidth := Max(ButtonWidth, Self.FTabButtonWidth); // Ensure minimum width.
          ButtonHeight := Self.FTabButtonHeight; // Height is fixed by property.
          TempRect := System.Types.Rect(CurrentX, TabButtonsRegion.Top + FTabButtonPadding.Top, CurrentX + ButtonWidth, TabButtonsRegion.Top + FTabButtonPadding.Top + ButtonHeight);
          CurrentX := TempRect.Right + FTabButtonSpacing; // Advance for next button.
        end;
      tpLeft, tpRight:
        begin
          Canvas.Font.Assign(FTabCaptionSettings.Font); // Set font for accurate measurement
          ButtonHeight := Canvas.TextHeight('Wg');     // Correctly call TextHeight with a string

          // Consider icon height contribution
          var IconActualHeight: Integer := 0;
          if ButtonIconWidth > 0 then // ButtonIconWidth implies an icon is present and its potential width was calculated earlier
          begin
            // For L/R tabs, an icon usually sits beside or above/below text.
            // The FTabButtonHeight property sets a general height for the items in L/R mode.
            // IconActualHeight represents the usable space for an icon within that FTabButtonHeight, after padding.
            IconActualHeight := Self.FTabButtonHeight - FTabButtonPadding.Top - FTabButtonPadding.Bottom;
            IconActualHeight := Max(0, IconActualHeight); // Ensure non-negative
          end;

          ButtonHeight := Max(ButtonHeight, IconActualHeight); // Max of text height or potential icon height area

          ButtonHeight := ButtonHeight + FTabButtonPadding.Top + FTabButtonPadding.Bottom; // Add vertical padding
          ButtonHeight := Max(ButtonHeight, Self.FTabButtonHeight); // Ensure minimum overall height for the tab item

          ButtonWidth := Self.FTabButtonWidth; // Width is fixed by property for L/R tabs
          TempRect := System.Types.Rect(TabButtonsRegion.Left + FTabButtonPadding.Left, CurrentY, TabButtonsRegion.Left + FTabButtonPadding.Left + ButtonWidth, CurrentY + ButtonHeight);
          CurrentY := TempRect.Bottom + FTabButtonSpacing; // Advance for next button.
        end;
    else // Should not happen
      TempRect := System.Types.Rect(0,0,0,0);
    end;
    FTabButtonRects[I] := TempRect; // Store calculated rectangle.
  end;
end;

// Updates the visibility and bounds of all managed TANDMR_CTabSheet instances.
// Only the active tab sheet is made visible and sized to the content region.
procedure TANDMR_CControlTab.UpdateTabSheetBounds;
var
  TabButtonsRegion, ContentRegion: TRect;
  I: Integer;
  Sheet: TANDMR_CTabSheet;
begin
  CalculateLayout(TabButtonsRegion, ContentRegion); // Determine content area first.

  for I := 0 to FTabSheets.Count - 1 do
  begin
    Sheet := TANDMR_CTabSheet(FTabSheets[I]);
    if I = FActiveTabSheetIndex then // If this is the active sheet
    begin
      Sheet.BoundsRect := ContentRegion; // Set its bounds to the calculated content region.
      Sheet.Visible := True;             // Make it visible.
      if Sheet.Parent = Self then        // Ensure it's our direct child before BringToFront.
         Sheet.BringToFront;             // Bring to front to ensure it's not obscured.
    end
    else // For all other (inactive) sheets
    begin
      Sheet.Visible := False; // Make them invisible.
      // Optionally, move inactive tabs off-screen, though Visible := False is usually sufficient
      // and more performant than changing bounds frequently.
      // Sheet.SetBounds(-Sheet.Width-10, -Sheet.Height-10, Sheet.Width, Sheet.Height);
    end;
  end;
end;

// Handles mouse entering the control area.
// Currently used to trigger hover animations/styles if implemented (not fully in this version for CMMouseEnter itself).
// Actual hover index update is primarily done in MouseMove.
procedure TANDMR_CControlTab.CMMouseEnter(var Message: TMessage);
begin
  // inherited; // TCustomControl doesn't have CMMouseEnter/Leave by default.
  // MouseMove will handle setting FHoveredTabIndex.
end;

// Handles mouse leaving the control area.
// Resets the hovered tab index and invalidates the control to remove hover styling.
procedure TANDMR_CControlTab.CMMouseLeave(var Message: TMessage);
var
  OldHoveredTabIndex: Integer;
begin
  // inherited;
  OldHoveredTabIndex := FHoveredTabIndex;
  FHoveredTabIndex := -1; // Mouse left the control, so no tab is hovered.
  if OldHoveredTabIndex <> FHoveredTabIndex then // Repaint only if hover state changed.
    Invalidate;
end;

// Tracks mouse movement to update hover effects on tab buttons.
// Determines which tab is under the cursor and invalidates if hover state changes.
procedure TANDMR_CControlTab.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHoveredTabIndex: Integer;
begin
  inherited; // Important for general mouse processing like hints, drag operations.
  OldHoveredTabIndex := FHoveredTabIndex;
  FHoveredTabIndex := GetTabButtonAt(X,Y); // Determine which tab (if any) is under cursor.
  if OldHoveredTabIndex <> FHoveredTabIndex then // Repaint only if hover state changed.
    Invalidate;
end;

// Determines which tab button (if any) is at the given client coordinates.
// Returns the index of the tab sheet or -1 if no button is at these coordinates.
function TANDMR_CControlTab.GetTabButtonAt(X, Y: Integer): Integer;
var
  I: Integer;
  P: TPoint;
begin
  Result := -1; // Default to no tab found.
  P := Point(X,Y);

  // Ensure FTabButtonRects is up-to-date before hit-testing.
  // This is a defensive call; RecalculateTabButtonRects should ideally be called
  // by methods that change layout, making this check rarely necessary.
  if (Length(FTabButtonRects) <> FTabSheets.Count) and (FTabSheets.Count > 0) then
     RecalculateTabButtonRects;

  if FTabSheets.Count = 0 then Exit; // No tabs, no buttons to find.

  // Iterate through pre-calculated button rectangles.
  for I := 0 to High(FTabButtonRects) do // Use High for safety; FTabButtonRects should be sized correctly.
  begin
    if PtInRect(FTabButtonRects[I], P) then // Check if point is within the current button's rectangle.
    begin
      Result := I; // Tab button found.
      Exit;      // Exit immediately.
    end;
  end;
end;

// Handles all custom painting for the TANDMR_CControlTab.
// This includes drawing the tab bar background, individual tab buttons (with icons and text),
// and the border around the content area where the active tab sheet is displayed.
procedure TANDMR_CControlTab.Paint;
var
  TabButtonsRegion, ContentRegion: TRect; // Rectangles for tab bar and content display area.
  I: Integer;
  ButtonRect, TextRect, IconRect: TRect; // Rectangles for individual button, text, and icon.
  Sheet: TANDMR_CTabSheet;
  CurrentStyle: TBorderSettings;       // Holds style for the current button being drawn.
  CurrentCaptionStyle: TCaptionSettings; // Holds caption style for the current button.
  LGraphics: TGPGraphics;              // GDI+ graphics object for advanced drawing.
  IconWidth, IconHeight, ActualIconWidth: Integer; // Variables for icon dimension calculations.
begin
  inherited Paint; // Call ancestor's Paint method.
  LGraphics := TGPGraphics.Create(Canvas.Handle); // Create GDI+ graphics object.
  try
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias); // Enable anti-aliasing for smoother drawing.
    LGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic); // Use high-quality image scaling.

    CalculateLayout(TabButtonsRegion, ContentRegion); // Determine layout areas.

    // Ensure FTabButtonRects is valid and sized.
    // It should be calculated by RecalculateTabButtonRects before Paint is called if layout changed.
    if (Length(FTabButtonRects) <> FTabSheets.Count) then
    begin
      if FTabSheets.Count = 0 then
        SetLength(FTabButtonRects, 0) // Clear rects if no tabs
      else
        RecalculateTabButtonRects; // Recalculate if mismatched and tabs exist
    end;

    // Draw TabButtonsRegion background (can be styled further if desired)
    Canvas.Brush.Color := Color; // Use control's base color or a specific theme color
    if Parent <> nil then Canvas.Brush.Color := Parent.Color; // Basic theming attempt
    if csOpaque in ControlStyle then Canvas.FillRect(ClientRect); // Fill entire control if opaque

    // More specific background for the tab buttons area
    Canvas.Brush.Color := clBtnFace; // Example, could be from a style property
    Canvas.FillRect(TabButtonsRegion);

    // Draw ContentRegion border
    // Uses FInactiveTabStyle for the content area border by default, or a dedicated style could be added.
    if FInactiveTabStyle.Visible then
    begin
       DrawEditBox(LGraphics, ContentRegion, FInactiveTabStyle.BackgroundColor, FInactiveTabStyle.Color,
                   FInactiveTabStyle.Thickness, FInactiveTabStyle.Style, 0, rctNone, 255); // No radius for content area border
    end else
    begin // Fallback simple border if InactiveTabStyle is not visible
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := clGray;
        Canvas.Rectangle(ContentRegion);
        Canvas.Brush.Style := bsSolid;
    end;

    // Draw each tab button
    for I := 0 to FTabSheets.Count - 1 do
    begin
      if I >= Length(FTabButtonRects) then continue; // Safety: Should not happen if RecalculateTabButtonRects is correct

      Sheet := TANDMR_CTabSheet(FTabSheets[I]);
      ButtonRect := FTabButtonRects[I]; // Use pre-calculated rectangle for the current button.

      // Determine current style based on state (active, hover, or inactive)
      if I = FActiveTabSheetIndex then
      begin
        CurrentStyle := FActiveTabStyle;
        CurrentCaptionStyle := FActiveTabCaptionSettings;
      end
      else if I = FHoveredTabIndex then
      begin
        CurrentStyle := FHoverTabStyle;
        CurrentCaptionStyle := FTabCaptionSettings; // Could also have a specific FHoverCaptionSettings
      end
      else
      begin
        CurrentStyle := FInactiveTabStyle;
        CurrentCaptionStyle := FTabCaptionSettings;
      end;

      // Draw the tab button's background and border using DrawEditBox utility function.
      DrawEditBox(LGraphics, ButtonRect, CurrentStyle.BackgroundColor, CurrentStyle.Color,
                  CurrentStyle.Thickness, CurrentStyle.Style, CurrentStyle.CornerRadius,
                  CurrentStyle.RoundCornerType, 255); // Assuming full opacity (255).

      // Prepare text rectangle, adjusted for button padding.
      TextRect := ButtonRect;
      InflateRect(TextRect, -FTabButtonPadding.Left, -FTabButtonPadding.Right,
                              -FTabButtonPadding.Top, -FTabButtonPadding.Bottom);

      IconWidth := 0; // Reset for each button
      IconHeight := 0;
      ActualIconWidth := 0;

      // Draw Icon if present and valid.
      if Assigned(Sheet.Icon) and (Sheet.Icon.Graphic <> nil) and (Sheet.Icon.Width > 0) and (Sheet.Icon.Height > 0) then
      begin
          // Calculate available height for icon within the padded TextRect.
          IconHeight := Max(0, TextRect.Height - CurrentCaptionStyle.Margins.Top - CurrentCaptionStyle.Margins.Bottom);
          if IconHeight > 0 then // Only proceed if there's vertical space for the icon.
          begin
            ActualIconWidth := MulDiv(Sheet.Icon.Width, IconHeight, Sheet.Icon.Height); // Calculate width maintaining aspect ratio.
            ActualIconWidth := Min(ActualIconWidth, TextRect.Width div 2); // Limit icon width to half of available text area.

            if ActualIconWidth > 0 then // Only draw if calculated width is positive.
            begin
              // Position icon (e.g., left of text, vertically centered within available icon space).
              IconRect := System.Types.Rect(
                TextRect.Left + CurrentCaptionStyle.Margins.Left,
                TextRect.Top + (TextRect.Height - IconHeight) div 2, // Center vertically in the space made for text
                TextRect.Left + CurrentCaptionStyle.Margins.Left + ActualIconWidth,
                TextRect.Top + (TextRect.Height - IconHeight) div 2 + IconHeight);

              // Draw the icon (handle different graphic types).
              if Sheet.Icon.Graphic is TBitmap then
                 DrawNonPNGImageWithCanvas(Canvas, Sheet.Icon.Graphic, IconRect, idmProportional)
              else if Sheet.Icon.Graphic is TPNGImage then
                 DrawPNGImageWithGDI(LGraphics, TPNGImage(Sheet.Icon.Graphic), IconRect, idmProportional)
              else if Sheet.Icon.Graphic is TJPEGImage then // Example for another type
                 DrawNonPNGImageWithCanvas(Canvas, Sheet.Icon.Graphic, IconRect, idmProportional);
              // TODO: Add support for other TGraphic descendants as needed.

              // Adjust TextRect to make space for the icon and its right margin.
              IconWidth := ActualIconWidth + CurrentCaptionStyle.Margins.Right + CurrentCaptionStyle.Margins.Left; // Total horizontal space used by icon and its margins.
              TextRect.Left := TextRect.Left + IconWidth;
            end;
          end;
      end;

      // Draw Caption Text, if space remains and caption is visible and not empty.
      if Assigned(CurrentCaptionStyle) and CurrentCaptionStyle.Visible and (Sheet.Caption <> '') and (TextRect.Width > 0) then
      begin
        DrawComponentCaption(Canvas, TextRect, Sheet.Caption, CurrentCaptionStyle.Font,
                             CurrentCaptionStyle.Color, CurrentCaptionStyle.Alignment,
                             CurrentCaptionStyle.VerticalAlignment, CurrentCaptionStyle.WordWrap, 255); // Full opacity.
      end;
    end;
  finally
    LGraphics.Free; // Free the GDI+ graphics object.
  end;

  UpdateTabSheetBounds; // Final adjustment of active sheet visibility and bounds.
end;

// Handles mouse clicks to activate tab sheets.
procedure TANDMR_CControlTab.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedTabIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y); // Call ancestor's method.

  if Button = mbLeft then // Process only left mouse button clicks.
  begin
    ClickedTabIndex := GetTabButtonAt(X, Y); // Determine which tab button was clicked.
    // If a valid tab button was clicked and it's not already the active one:
    if (ClickedTabIndex <> -1) and (ClickedTabIndex <> FActiveTabSheetIndex) then
    begin
      SetActiveTabSheetIndex(ClickedTabIndex); // Activate the clicked tab.
      // FHoveredTabIndex might also need to be updated here if the mouse remains over the clicked tab.
      // However, SetActiveTabSheetIndex calls Invalidate, which will repaint with the new active style.
      // MouseMove will eventually correct FHoveredTabIndex if needed and mouse is still moving.
    end;
  end;
end;

// Called when the control is resized.
// Recalculates layout of tab buttons and content area, then repaints.
procedure TANDMR_CControlTab.Resize;
begin
  inherited Resize;
  RecalculateTabButtonRects; // Recalculate positions and sizes of tab buttons.
  UpdateTabSheetBounds;    // Update bounds of the active tab sheet.
  Invalidate;              // Force a repaint of the control.
end;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CControlTab, TANDMR_CTabSheet]);
end;

end.
