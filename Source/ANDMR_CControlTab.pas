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
    FContentBorder: TBorderSettings; // Style for the border around the tab content area.

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
    procedure SetContentBorder(Value: TBorderSettings); // Setter for FContentBorder
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
    // Defines the visual style for the border around the content area.
    property ContentBorder: TBorderSettings read FContentBorder write SetContentBorder;
  end;

procedure Register;

implementation

uses Vcl.Themes,      // For StyleServices (themed text drawing)
     Vcl.Imaging.jpeg,  // For TJPEGImage support
     Vcl.Imaging.pngimage; // For TPNGImage support

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
  FTabButtonPadding.Left := 8;
  FTabButtonPadding.Top := 6;
  FTabButtonPadding.Right := 8;
  FTabButtonPadding.Bottom := 6;
  FTabButtonSpacing := 4;

  // Initialize Styling Properties with default values
  FInactiveTabStyle := TBorderSettings.Create;
  FInactiveTabStyle.OnChange := StyleChanged;
  FInactiveTabStyle.BackgroundColor := TColor($00F0F0F0); // Modernized: light gray
  FInactiveTabStyle.Color := TColor($00D0D0D0);           // Modernized: lighter gray border
  FInactiveTabStyle.CornerRadius := 3;
  FInactiveTabStyle.RoundCornerType := rctTop;

  FActiveTabStyle := TBorderSettings.Create;
  FActiveTabStyle.OnChange := StyleChanged;
  FActiveTabStyle.BackgroundColor := clWindow;
  FActiveTabStyle.Color := TColor($00AECAF0); // Modernized: softer blue border
  FActiveTabStyle.CornerRadius := 3;
  FActiveTabStyle.RoundCornerType := rctTop;

  FHoverTabStyle := TBorderSettings.Create;
  FHoverTabStyle.OnChange := StyleChanged;
  FHoverTabStyle.BackgroundColor := LighterColor(FInactiveTabStyle.BackgroundColor, 15); // Modernized & slightly more blend
  FHoverTabStyle.Color := FActiveTabStyle.Color; // Modernized: Use active tab's border color
  FHoverTabStyle.CornerRadius := 3;
  FHoverTabStyle.RoundCornerType := rctTop;

  FTabCaptionSettings := TCaptionSettings.Create(Self); // Owner is Self for resource management.
  FTabCaptionSettings.OnChange := StyleChanged;
  FTabCaptionSettings.Font.Color := clBtnText;
  FTabCaptionSettings.Alignment := taCenter;
  FTabCaptionSettings.VerticalAlignment := cvaCenter;

  FActiveTabCaptionSettings := TCaptionSettings.Create(Self);
  FActiveTabCaptionSettings.OnChange := StyleChanged;
  FActiveTabCaptionSettings.Font.Color := clWindowText; // Modernized for better contrast with clWindow background
  FActiveTabCaptionSettings.Alignment := taCenter;
  FActiveTabCaptionSettings.VerticalAlignment := cvaCenter;

  // [FIX] Initialize the FContentBorder property
  FContentBorder := TBorderSettings.Create;
  FContentBorder.OnChange := StyleChanged;
  FContentBorder.Assign(FInactiveTabStyle); // Start with inactive style as a base
  FContentBorder.RoundCornerType := rctNone;  // But no rounded corners for the content panel itself
  FContentBorder.CornerRadius := 0;

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

  // [FIX] Unhook and free the FContentBorder property
  FContentBorder.OnChange := nil;
  FContentBorder.Free;

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
end;

// [FIX] Implement the setter for the ContentBorder property
procedure TANDMR_CControlTab.SetContentBorder(Value: TBorderSettings);
begin
    FContentBorder.Assign(Value);
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
    end;
  end
  // Special case: if there are no tabs, ensure index is -1.
  else if (FTabSheets.Count = 0) and (FActiveTabSheetIndex <> -1) then
  begin
      FActiveTabSheetIndex := -1;
      UpdateTabSheetBounds;
      Invalidate;
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
    UpdateTabSheetBounds;     // Content area also changes.
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

  if FActiveTabSheetIndex <> -1 then
    SetActiveTabSheetIndex(-1);

  RecalculateTabButtonRects;
  Invalidate;
end;

// Manages the internal FTabSheets list when TANDMR_CTabSheet components are added or removed.
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
      opInsert:
        begin
          if FTabSheets.IndexOf(Sheet) = -1 then
          begin
            FTabSheets.Add(Sheet);
            if (FTabSheets.Count = 1) or (FActiveTabSheetIndex = -1) then
            begin
              SetActiveTabSheetIndex(FTabSheets.Count - 1);
            end
            else
            begin
              Sheet.Visible := False;
            end;
            RecalculateTabButtonRects;
            Invalidate;
          end;
        end;
      opRemove:
        begin
          RemovedIndex := FTabSheets.IndexOf(Sheet);
          if RemovedIndex <> -1 then
          begin
            FTabSheets.Remove(Sheet);

            if FActiveTabSheetIndex = RemovedIndex then
            begin
              if FTabSheets.Count > 0 then
                // Adjust index to stay valid, clamping at the new last index
                SetActiveTabSheetIndex(Min(RemovedIndex, FTabSheets.Count - 1))
              else
                SetActiveTabSheetIndex(-1);
            end
            else if FActiveTabSheetIndex > RemovedIndex then
            begin
              SetActiveTabSheetIndex(FActiveTabSheetIndex - 1);
            end;

            RecalculateTabButtonRects;
            Invalidate;
          end;
        end;
    end;
  end;
end;

// Calculates the bounding rectangles for the tab button area and the content display area.
procedure TANDMR_CControlTab.CalculateLayout(var TabButtonsRegion: TRect; var ContentRegion: TRect);
begin
  TabButtonsRegion := ClientRect;
  ContentRegion := ClientRect;

  case FTabPosition of
    tpTop:
      begin
        TabButtonsRegion.Bottom := TabButtonsRegion.Top + FTabButtonHeight;
        ContentRegion.Top := TabButtonsRegion.Bottom;
      end;
    tpBottom:
      begin
        TabButtonsRegion.Top := TabButtonsRegion.Bottom - FTabButtonHeight;
        ContentRegion.Bottom := TabButtonsRegion.Top;
      end;
    tpLeft:
      begin
        TabButtonsRegion.Right := TabButtonsRegion.Left + FTabButtonWidth;
        ContentRegion.Left := TabButtonsRegion.Right;
      end;
    tpRight:
      begin
        TabButtonsRegion.Left := TabButtonsRegion.Right - FTabButtonWidth;
        ContentRegion.Right := TabButtonsRegion.Left;
      end;
  end;
end;

// Updates the stored FTabButtonRects for each tab button.
procedure TANDMR_CControlTab.RecalculateTabButtonRects;
var
  TabButtonsRegion, ContentRegion: TRect;
  I: Integer;
  CurrentX, CurrentY: Integer;
  Sheet: TANDMR_CTabSheet;
  ButtonTextWidth, ButtonIconWidth, ButtonWidth, ButtonHeight: Integer;
  TempRect: TRect;
begin
  CalculateLayout(TabButtonsRegion, ContentRegion);
  SetLength(FTabButtonRects, FTabSheets.Count);

  CurrentX := TabButtonsRegion.Left;
  CurrentY := TabButtonsRegion.Top;

  for I := 0 to FTabSheets.Count - 1 do
  begin
    Sheet := TANDMR_CTabSheet(FTabSheets[I]);
    ButtonIconWidth := 0;

    if Assigned(Sheet.Icon) and (Sheet.Icon.Graphic <> nil) and not Sheet.Icon.Graphic.Empty then
    begin
        var IconAvailableSpace := 0;
        if FTabPosition in [tpTop, tpBottom] then
            IconAvailableSpace := FTabButtonHeight - FTabButtonPadding.Top - FTabButtonPadding.Bottom
        else
            IconAvailableSpace := FTabButtonWidth - FTabButtonPadding.Left - FTabButtonPadding.Right;

        if (Sheet.Icon.Height > 0) and (IconAvailableSpace > 0) then
            ButtonIconWidth := MulDiv(Sheet.Icon.Width, IconAvailableSpace, Sheet.Icon.Height)
        else
            ButtonIconWidth := Sheet.Icon.Width;

        ButtonIconWidth := ButtonIconWidth + 4; // Add spacing between icon and text
    end;

    Canvas.Font.Assign(FTabCaptionSettings.Font);
    ButtonTextWidth := Canvas.TextWidth(Sheet.Caption);

    case FTabPosition of
      tpTop, tpBottom:
        begin
          ButtonWidth := ButtonTextWidth + ButtonIconWidth + FTabButtonPadding.Left + FTabButtonPadding.Right;
          ButtonWidth := Max(ButtonWidth, Self.FTabButtonWidth);
          ButtonHeight := Self.FTabButtonHeight;
          TempRect := System.Types.Rect(CurrentX, TabButtonsRegion.Top, CurrentX + ButtonWidth, TabButtonsRegion.Top + ButtonHeight);
          CurrentX := TempRect.Right + FTabButtonSpacing;
        end;
      tpLeft, tpRight:
        begin
          ButtonWidth := Self.FTabButtonWidth;
          ButtonHeight := Canvas.TextHeight('Wg') + FTabButtonPadding.Top + FTabButtonPadding.Bottom;
          ButtonHeight := Max(ButtonHeight, Self.FTabButtonHeight);
          TempRect := System.Types.Rect(TabButtonsRegion.Left, CurrentY, TabButtonsRegion.Left + ButtonWidth, CurrentY + ButtonHeight);
          CurrentY := TempRect.Bottom + FTabButtonSpacing;
        end;
    else
      TempRect := System.Types.Rect(0,0,0,0);
    end;
    FTabButtonRects[I] := TempRect;
  end;
end;

// Updates the visibility and bounds of all managed TANDMR_CTabSheet instances.
procedure TANDMR_CControlTab.UpdateTabSheetBounds;
var
  TabButtonsRegion, ContentRegion: TRect;
  I: Integer;
  Sheet: TANDMR_CTabSheet;
begin
  if not (csDesigning in ComponentState) then
    CalculateLayout(TabButtonsRegion, ContentRegion)
  else
    ContentRegion := ClientRect; // In designer, just use client rect to avoid hiding sheets

  for I := 0 to FTabSheets.Count - 1 do
  begin
    Sheet := TANDMR_CTabSheet(FTabSheets[I]);
    if I = FActiveTabSheetIndex then
    begin
      Sheet.BoundsRect := ContentRegion;
      Sheet.Visible := True;
      if Sheet.Parent = Self then
        Sheet.BringToFront;
    end
    else
    begin
      Sheet.Visible := False;
    end;
  end;
end;

procedure TANDMR_CControlTab.CMMouseEnter(var Message: TMessage);
begin
  // MouseMove will handle setting FHoveredTabIndex.
end;

procedure TANDMR_CControlTab.CMMouseLeave(var Message: TMessage);
var
  OldHoveredTabIndex: Integer;
begin
  OldHoveredTabIndex := FHoveredTabIndex;
  FHoveredTabIndex := -1;
  if OldHoveredTabIndex <> FHoveredTabIndex then
    Invalidate;
end;

procedure TANDMR_CControlTab.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHoveredTabIndex: Integer;
begin
  inherited;
  OldHoveredTabIndex := FHoveredTabIndex;
  FHoveredTabIndex := GetTabButtonAt(X,Y);
  if OldHoveredTabIndex <> FHoveredTabIndex then
    Invalidate;
end;

// Determines which tab button is at the given client coordinates.
function TANDMR_CControlTab.GetTabButtonAt(X, Y: Integer): Integer;
var
  I: Integer;
  P: TPoint;
begin
  Result := -1;
  P := Point(X,Y);

  if (Length(FTabButtonRects) <> FTabSheets.Count) and (FTabSheets.Count > 0) then
    RecalculateTabButtonRects;

  if FTabSheets.Count = 0 then Exit;

  for I := 0 to High(FTabButtonRects) do
  begin
    if PtInRect(FTabButtonRects[I], P) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

// Handles all custom painting for the TANDMR_CControlTab.
procedure TANDMR_CControlTab.Paint;
var
  TabButtonsRegion, ContentRegion: TRect;
  I: Integer;
  ButtonRect, TextRect, IconRect: TRect;
  Sheet: TANDMR_CTabSheet;
  CurrentStyle: TBorderSettings;
  CurrentCaptionStyle: TCaptionSettings;
  LGraphics: TGPGraphics;
  IconWidth, IconHeight, ActualIconWidth: Integer;
begin
  inherited Paint;
  LGraphics := TGPGraphics.Create(Canvas.Handle);
  try
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    LGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);

    CalculateLayout(TabButtonsRegion, ContentRegion);

    if (Length(FTabButtonRects) <> FTabSheets.Count) then
    begin
      if FTabSheets.Count = 0 then
        SetLength(FTabButtonRects, 0)
      else
        RecalculateTabButtonRects;
    end;

    if csOpaque in ControlStyle then Canvas.FillRect(ClientRect);

    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(TabButtonsRegion);

    // [FIX] Use the dedicated FContentBorder property to draw the content area border
    if FContentBorder.Visible then
    begin
      DrawEditBox(LGraphics, ContentRegion, FContentBorder.BackgroundColor, FContentBorder.Color,
        FContentBorder.Thickness, FContentBorder.Style, FContentBorder.CornerRadius,
        FContentBorder.RoundCornerType, 255);
    end;

    for I := 0 to FTabSheets.Count - 1 do
    begin
      if I >= Length(FTabButtonRects) then continue;

      Sheet := TANDMR_CTabSheet(FTabSheets[I]);
      ButtonRect := FTabButtonRects[I];

      if I = FActiveTabSheetIndex then
      begin
        CurrentStyle := FActiveTabStyle;
        CurrentCaptionStyle := FActiveTabCaptionSettings;
      end
      else if I = FHoveredTabIndex then
      begin
        CurrentStyle := FHoverTabStyle;
        CurrentCaptionStyle := FTabCaptionSettings;
      end
      else
      begin
        CurrentStyle := FInactiveTabStyle;
        CurrentCaptionStyle := FTabCaptionSettings;
      end;

      DrawEditBox(LGraphics, ButtonRect, CurrentStyle.BackgroundColor, CurrentStyle.Color,
                  CurrentStyle.Thickness, CurrentStyle.Style, CurrentStyle.CornerRadius,
                  CurrentStyle.RoundCornerType, 255);

      TextRect := ButtonRect;
      InflateRect(TextRect, -FTabButtonPadding.Left, -FTabButtonPadding.Top);

      ActualIconWidth := 0;

      if Assigned(Sheet.Icon) and (Sheet.Icon.Graphic <> nil) and not Sheet.Icon.Graphic.Empty then
      begin
        IconHeight := Max(0, TextRect.Height);
        if IconHeight > 0 then
        begin
          ActualIconWidth := MulDiv(Sheet.Icon.Width, IconHeight, Sheet.Icon.Height);
          if ActualIconWidth > 0 then
          begin
            IconRect := System.Types.Rect(
              TextRect.Left,
              TextRect.Top + (TextRect.Height - IconHeight) div 2,
              TextRect.Left + ActualIconWidth,
              TextRect.Top + (TextRect.Height - IconHeight) div 2 + IconHeight);

            // [FIX] Use fully qualified class names to resolve ambiguity and prevent E2015 error.
            if (Sheet.Icon.Graphic is Vcl.Graphics.TBitmap) then
                DrawNonPNGImageWithCanvas(Canvas, Sheet.Icon.Graphic, IconRect, idmProportional)
            else if (Sheet.Icon.Graphic is Vcl.Imaging.pngimage.TPNGImage) then
                DrawPNGImageWithGDI(LGraphics, Vcl.Imaging.pngimage.TPNGImage(Sheet.Icon.Graphic), IconRect, idmProportional)
            else if (Sheet.Icon.Graphic is Vcl.Imaging.jpeg.TJPEGImage) then
                DrawNonPNGImageWithCanvas(Canvas, Sheet.Icon.Graphic, IconRect, idmProportional);

            TextRect.Left := TextRect.Left + ActualIconWidth + 4; // Add 4px spacing between icon and text
          end;
        end;
      end;

      if Assigned(CurrentCaptionStyle) and CurrentCaptionStyle.Visible and (Sheet.Caption <> '') and (TextRect.Width > 0) then
      begin
        DrawComponentCaption(Canvas, TextRect, Sheet.Caption, CurrentCaptionStyle.Font,
                             CurrentCaptionStyle.Color, CurrentCaptionStyle.Alignment,
                             CurrentCaptionStyle.VerticalAlignment, CurrentCaptionStyle.WordWrap, 255);
      end;
    end;
  finally
    LGraphics.Free;
  end;
end;


procedure TANDMR_CControlTab.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedTabIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    ClickedTabIndex := GetTabButtonAt(X, Y);
    if (ClickedTabIndex <> -1) and (ClickedTabIndex <> FActiveTabSheetIndex) then
    begin
      SetActiveTabSheetIndex(ClickedTabIndex);
    end;
  end;
end;

procedure TANDMR_CControlTab.Resize;
begin
  inherited Resize;
  RecalculateTabButtonRects;
  UpdateTabSheetBounds;
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CControlTab, TANDMR_CTabSheet]);
end;

end.
