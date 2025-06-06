unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, // Added System.Contnrs
  Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.StdCtrls, System.Types, System.UITypes,
  System.Math, Winapi.GDIPOBJ, Winapi.GDIPAPI,
  ANDMR_CButton, ANDMR_ComponentUtils;

type
  TANDMR_ButtonGroupOrientation = (bgoHorizontal, bgoVertical);

  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FButtons: TObjectList;
    FOrientation: TANDMR_ButtonGroupOrientation;
    FButtonSpacing: Integer;
    FSelectedButton: TANDMR_CButton;
    FBorderSettings: TBorderSettings;
    FHoverSettings: THoverSettings;
    FSeparatorSettings: TSeparatorSettings;
    FAllowMultiSelect: Boolean; // Added
    FActiveButtonColor: TColor;
    FInactiveButtonColor: TColor;
    FActiveButtonFontColor: TColor;
    FInactiveButtonFontColor: TColor;
    FTransparent: Boolean;


    procedure SetOrientation(const Value: TANDMR_ButtonGroupOrientation);
    procedure SetButtonSpacing(const Value: Integer);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);
    procedure SetAllowMultiSelect(const Value: Boolean); // Added
    procedure SetActiveButtonColor(const Value: TColor);
    procedure SetInactiveButtonColor(const Value: TColor);
    procedure SetActiveButtonFontColor(const Value: TColor);
    procedure SetInactiveButtonFontColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);


    procedure ButtonClickHandler(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure UpdateButtonsStyle;


  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddButton(ACaption: string = ''; AImage: TPicture = nil): TANDMR_CButton;
    procedure RemoveButton(AButton: TANDMR_CButton);
    procedure ClearButtons;
    procedure SelectButton(AButton: TANDMR_CButton);
    // Add public methods as needed

  published
    property Orientation: TANDMR_ButtonGroupOrientation read FOrientation write SetOrientation default bgoHorizontal;
    property ButtonSpacing: Integer read FButtonSpacing write SetButtonSpacing default 5;
    property SelectedButton: TANDMR_CButton read FSelectedButton write SelectButton;

    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings; // Group hover, not button hover
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;
    property AllowMultiSelect: Boolean read FAllowMultiSelect write SetAllowMultiSelect default False; // Added

    property ActiveButtonColor: TColor read FActiveButtonColor write SetActiveButtonColor default clHighlight;
    property InactiveButtonColor: TColor read FInactiveButtonColor write SetInactiveButtonColor default clBtnFace;
    property ActiveButtonFontColor: TColor read FActiveButtonFontColor write SetActiveButtonFontColor default clWhite;
    property InactiveButtonFontColor: TColor read FInactiveButtonFontColor write SetInactiveButtonFontColor default clBlack;

    property Align;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButtonGroup]);
end;

{ TANDMR_CButtonGroup }

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks, csReplicatable];
  Width := 200;
  Height := 50;
  TabStop := True;

  FButtons := TObjectList.Create(True);
  FTransparent := False; // Default value
  ControlStyle := ControlStyle - [csParentBackground];
  FOrientation := bgoHorizontal;
  FButtonSpacing := 5;
  FSelectedButton := nil;
  FAllowMultiSelect := False; // Initialize new field

  FActiveButtonColor := clHighlight;
  FInactiveButtonColor := clBtnFace;
  FActiveButtonFontColor := clWhite;
  FInactiveButtonFontColor := clBlack;


  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.BackgroundColor := clNone; // Group background should be transparent or none
  FBorderSettings.Color := clGray;
  FBorderSettings.Thickness := 1;

  // Group's own hover settings, not for individual buttons directly unless explicitly propagated
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := SettingsChanged;
  FHoverSettings.Enabled := False; // Typically, hover is on buttons, not the group itself

  FSeparatorSettings := TSeparatorSettings.Create;
  FSeparatorSettings.OnChange := SettingsChanged;
  FSeparatorSettings.Visible := True;
  FSeparatorSettings.Color := clMedGray;
  FSeparatorSettings.Thickness := 1;
  FSeparatorSettings.Padding := 0; // Separators are between buttons

  Font.Name := 'Segoe UI';
  Font.Size := 9;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FButtons.Free;
  FBorderSettings.Free;
  FHoverSettings.Free;
  FSeparatorSettings.Free;
  inherited Destroy;
end;

procedure TANDMR_CButtonGroup.Paint;
var
  i: Integer;
  ButtonRect: TRect;
  CurrentPos: Integer;
  Btn: TANDMR_CButton;
  SepRect: TRect;
  Path: TGPGraphicsPath;
  LRectF: TGPRectF;
  LRadiusValue : Single;
  LG: TGPGraphics;
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LRectF.X := 0.0;
    LRectF.Y := 0.0;
    LRectF.Width := Self.Width; // Self.Width is Integer, implicitly converted to Single
    LRectF.Height := Self.Height; // Self.Height is Integer, implicitly converted to Single
    if FBorderSettings.Thickness > 0 then
    begin
        LRectF.X := FBorderSettings.Thickness / 2;
        LRectF.Y := FBorderSettings.Thickness / 2;
        LRectF.Width := Width - FBorderSettings.Thickness;
        LRectF.Height := Height - FBorderSettings.Thickness;
    end;
    LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LRectF.Width, LRectF.Height) / 2.0);
    LRadiusValue := Max(0, LRadiusValue);

    Path := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(Path, LRectF, LRadiusValue, FBorderSettings.RoundCornerType);
      // Fill background of the group if specified
      if FBorderSettings.BackgroundColor <> clNone then
      begin
        var FillBrush: TGPSolidBrush;
        FillBrush := TGPSolidBrush.Create(ColorToARGB(FBorderSettings.BackgroundColor, IfThen(Self.Transparent, 0, 255)));
        try
          LG.FillPath(FillBrush, Path);
        finally
          FillBrush.Free;
        end;
      end;
      // Draw group border
      if FBorderSettings.Visible and (FBorderSettings.Thickness > 0) then
      begin
        var Pen: TGPPen;
        Pen := TGPPen.Create(ColorToARGB(FBorderSettings.Color), FBorderSettings.Thickness);
        try
          LG.DrawPath(Pen, Path);
        finally
          Pen.Free;
        end;
      end;
    finally
      Path.Free;
    end;

    // Draw Separators
    if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) and (FButtons.Count > 1) then
    begin
      // Local variables for separator drawing
      var   SepPenObj: TGPPen;
      var   BtnBefore: TANDMR_CButton;
      var   o: Integer;
      var   SepX, ActualSepTop, ActualSepBottom: Single; // For Horizontal orientation
      var   SepY, ActualSepLeft, ActualSepRight: Single; // For Vertical orientation

      SepPenObj := TGPPen.Create(ColorToARGB(FSeparatorSettings.Color, 255), FSeparatorSettings.Thickness);
      try
        for o := 0 to FButtons.Count - 2 do // Iterate up to the second-to-last button
        begin
          BtnBefore := TANDMR_CButton(FButtons[o]);

          if FOrientation = bgoHorizontal then
          begin
            SepX := BtnBefore.Left + BtnBefore.Width + (FButtonSpacing / 2.0);
            ActualSepTop := BtnBefore.Top + FSeparatorSettings.Padding;
            ActualSepBottom := BtnBefore.Top + BtnBefore.Height - FSeparatorSettings.Padding;

            if ActualSepBottom > ActualSepTop then
            begin
              LG.DrawLine(SepPenObj, SepX, ActualSepTop, SepX, ActualSepBottom);
            end;
          end
          else // bgoVertical
          begin
            SepY := BtnBefore.Top + BtnBefore.Height + (FButtonSpacing / 2.0);
            ActualSepLeft := BtnBefore.Left + FSeparatorSettings.Padding;
            ActualSepRight := BtnBefore.Left + BtnBefore.Width - FSeparatorSettings.Padding;

            if ActualSepRight > ActualSepLeft then
            begin
              LG.DrawLine(SepPenObj, ActualSepLeft, SepY, ActualSepRight, SepY);
            end;
          end;
        end;
      finally
        SepPenObj.Free;
      end;
    end;

  finally
    LG.Free;
  end;


  // Button layout and separator drawing logic will be called from Resize or when buttons change
  // For now, this Paint focuses on the group's own border/background
end;

procedure TANDMR_CButtonGroup.Loaded;
begin
  inherited Loaded;
  UpdateButtonsStyle;
  Resize; // Initial layout
end;

procedure TANDMR_CButtonGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TANDMR_CButton) then
  begin
    if FButtons.IndexOf(AComponent as TANDMR_CButton) <> -1 then
      RemoveButton(AComponent as TANDMR_CButton); // This will also update layout via Resize
  end;
end;

procedure TANDMR_CButtonGroup.Resize;
var
  i: Integer;
  CurrentPos, TotalButtonSize, TotalSpacing, ButtonSize, StartPos: Integer;
  Btn: TANDMR_CButton;
begin
  inherited Resize;

  if FButtons.Count = 0 then Exit;

  TotalSpacing := FButtonSpacing * (FButtons.Count - 1);
  if TotalSpacing < 0 then TotalSpacing := 0;

  if FOrientation = bgoHorizontal then
  begin
    TotalButtonSize := Width - TotalSpacing - FBorderSettings.Thickness * 2 - FSeparatorSettings.Padding * 2;
    if TotalButtonSize < 0 then TotalButtonSize := 0;
    ButtonSize := IfThen(FButtons.Count > 0, TotalButtonSize div FButtons.Count, 0);
    StartPos := FBorderSettings.Thickness + FSeparatorSettings.Padding;
    CurrentPos := StartPos;

    for i := 0 to FButtons.Count - 1 do
    begin
      Btn := TANDMR_CButton(FButtons[i]);
      Btn.SetBounds(CurrentPos, FBorderSettings.Thickness + FSeparatorSettings.Padding, ButtonSize, Height - (FBorderSettings.Thickness * 2) - (FSeparatorSettings.Padding * 2));
      CurrentPos := CurrentPos + ButtonSize + FButtonSpacing;
    end;
  end
  else // bgoVertical
  begin
    TotalButtonSize := Height - TotalSpacing - FBorderSettings.Thickness * 2 - FSeparatorSettings.Padding * 2;
    if TotalButtonSize < 0 then TotalButtonSize := 0;
    ButtonSize := IfThen(FButtons.Count > 0, TotalButtonSize div FButtons.Count, 0);
    StartPos := FBorderSettings.Thickness + FSeparatorSettings.Padding;
    CurrentPos := StartPos;

    for i := 0 to FButtons.Count - 1 do
    begin
      Btn := TANDMR_CButton(FButtons[i]);
      Btn.SetBounds(FBorderSettings.Thickness + FSeparatorSettings.Padding, CurrentPos, Width - (FBorderSettings.Thickness * 2) - (FSeparatorSettings.Padding * 2), ButtonSize);
      CurrentPos := CurrentPos + ButtonSize + FButtonSpacing;
    end;
  end;
  UpdateButtonsStyle; // Ensure styles are reapplied after resize/reposition
  Invalidate;
end;


procedure TANDMR_CButtonGroup.SetOrientation(const Value: TANDMR_ButtonGroupOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Resize; // Re-layout buttons
  end;
end;

procedure TANDMR_CButtonGroup.SetButtonSpacing(const Value: Integer);
begin
  if FButtonSpacing <> Max(0, Value) then
  begin
    FButtonSpacing := Max(0, Value);
    Resize; // Re-layout buttons
  end;
end;

procedure TANDMR_CButtonGroup.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  Resize; // Border thickness might change layout
  Repaint;
end;

procedure TANDMR_CButtonGroup.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value); // Group hover, not directly applied to buttons here
  Repaint;
end;

procedure TANDMR_CButtonGroup.SetSeparatorSettings(const Value: TSeparatorSettings);
begin
  FSeparatorSettings.Assign(Value);
  Resize; // Separator padding might change layout
  Repaint;
end;

procedure TANDMR_CButtonGroup.SetAllowMultiSelect(const Value: Boolean);
begin
  if FAllowMultiSelect <> Value then
  begin
    FAllowMultiSelect := Value;
    if not FAllowMultiSelect and Assigned(FSelectedButton) then
    begin
      // If switching to single select, ensure only FSelectedButton is "active"
      // No specific action needed here if SelectButton already handles single selection logic
    end;
    UpdateButtonsStyle; // Re-apply styles based on new selection mode
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.SetActiveButtonColor(const Value: TColor);
begin
  if FActiveButtonColor <> Value then
  begin
    FActiveButtonColor := Value;
    UpdateButtonsStyle;
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.SetInactiveButtonColor(const Value: TColor);
begin
  if FInactiveButtonColor <> Value then
  begin
    FInactiveButtonColor := Value;
    UpdateButtonsStyle;
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.SetActiveButtonFontColor(const Value: TColor);
begin
  if FActiveButtonFontColor <> Value then
  begin
    FActiveButtonFontColor := Value;
    UpdateButtonsStyle;
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.SetInactiveButtonFontColor(const Value: TColor);
begin
  if FInactiveButtonFontColor <> Value then
  begin
    FInactiveButtonFontColor := Value;
    UpdateButtonsStyle;
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.SettingsChanged(Sender: TObject);
begin
  Resize; // Some settings might affect layout
  Repaint;
end;

procedure TANDMR_CButtonGroup.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButtonGroup.ButtonClickHandler(Sender: TObject);
var
  ClickedButton: TANDMR_CButton;
begin
  if Sender is TANDMR_CButton then
  begin
    ClickedButton := TANDMR_CButton(Sender);
    if FAllowMultiSelect then
    begin
      // Toggle selection for multi-select mode (not fully implemented yet)
      // For now, just call SelectButton which will behave as single select
      SelectButton(ClickedButton);
    end
    else
    begin
      SelectButton(ClickedButton);
    end;

    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

function TANDMR_CButtonGroup.AddButton(ACaption: string = ''; AImage: TPicture = nil): TANDMR_CButton;
var
  NewButton: TANDMR_CButton;
begin
  NewButton := TANDMR_CButton.Create(Self);
  NewButton.Parent := Self;
  NewButton.Caption := ACaption;
  if AImage <> nil then
    NewButton.ImageSettings.Picture.Assign(AImage);

  NewButton.ControlStyle := NewButton.ControlStyle - [csSetCaption]; // Group manages caption implications
  NewButton.OnClick := ButtonClickHandler;
  NewButton.Visible := True;
  NewButton.TabStop := False; // Group is the TabStop

  // Initial styling - will be overridden by UpdateButtonsStyle
  NewButton.BorderSettings.CornerRadius := Max(0, FBorderSettings.CornerRadius - 1); // Group radius is outer
  NewButton.BorderSettings.Color := FBorderSettings.Color; // Default border
  NewButton.BorderSettings.Thickness := 1; // Standard button border
  NewButton.BorderSettings.BackgroundColor := FInactiveButtonColor;
  NewButton.CaptionSettings.Font.Color := FInactiveButtonFontColor;

  // Assign group's hover settings to the button for consistent hover effect
  NewButton.HoverSettings.Assign(Self.FHoverSettings);
  // More specific button hover styling if needed:
  NewButton.HoverSettings.BackgroundColor := LighterColor(FInactiveButtonColor, 15);
  NewButton.HoverSettings.BorderColor := FActiveButtonColor;
  NewButton.HoverSettings.FontColor := FActiveButtonFontColor;
  NewButton.HoverSettings.Enabled := True; // Enable hover for buttons

  FButtons.Add(NewButton);
  UpdateButtonsStyle; // Apply consistent styling
  Resize; // Re-layout
  Result := NewButton;
end;

procedure TANDMR_CButtonGroup.RemoveButton(AButton: TANDMR_CButton);
var
  idx: Integer;
begin
  idx := FButtons.IndexOf(AButton);
  if idx <> -1 then
  begin
    if FSelectedButton = AButton then
      FSelectedButton := nil;
    FButtons.Remove(AButton); // TObjectList frees if it owns
    // AButton.Free; // No, TObjectList(True) handles freeing
    if FButtons.Count = 0 then FSelectedButton := nil;
    Resize; // Re-layout
  end;
end;

procedure TANDMR_CButtonGroup.ClearButtons;
begin
  FSelectedButton := nil; // Clear selection first
  // TObjectList owns the buttons, so clearing it will free them.
  FButtons.Clear;
  Resize; // Re-layout
end;

procedure TANDMR_CButtonGroup.SelectButton(AButton: TANDMR_CButton);
begin
  if not Enabled then Exit;
  if FAllowMultiSelect then
  begin
    // Multi-select logic (not the primary requirement for now)
    // For now, even in multi-select mode, we'll just mark AButton as the "last selected" or "primary selected".
    // A full multi-select would involve a list of selected buttons and checking if AButton is in it.
    // If AButton is in FButtons list:
    if FButtons.IndexOf(AButton) <> -1 then
    begin
        if FSelectedButton <> AButton then // If it's a new button to "select"
        begin
            FSelectedButton := AButton; // Update the primary selected button
        end
        else // If clicking the already primary selected button in multi-select (optional: deselect)
        begin
             // FSelectedButton := nil; // Optional: allow deselecting the primary in multi-mode
        end;
    end else
    begin
        FSelectedButton := nil; // Button not in group
    end;
  end
  else // Single selection mode
  begin
    if FSelectedButton = AButton then
    begin
        // Optional: Allow deselecting by clicking the selected button again
        // FSelectedButton := nil;
    end
    else
    begin
      FSelectedButton := nil; // Clear previous before searching
      if FButtons.IndexOf(AButton) <> -1 then
      begin
        FSelectedButton := AButton;
      end;
    end;
  end;

  UpdateButtonsStyle;
  Repaint; // Repaint the group and its buttons
end;

procedure TANDMR_CButtonGroup.UpdateButtonsStyle;
var
  i: Integer;
  Btn: TANDMR_CButton;
  IsSelected: Boolean;
  IsFirst, IsLast, IsMiddle: Boolean;
  EffectiveRadius: Integer;
begin
  EffectiveRadius := Max(0, FBorderSettings.CornerRadius -1); // Inner buttons have slightly less or specific radius handling

  for i := 0 to FButtons.Count - 1 do
  begin
    Btn := TANDMR_CButton(FButtons[i]);
    IsSelected := (Btn = FSelectedButton); // Simplified for single selection focus

    if IsSelected then
    begin
      Btn.BorderSettings.BackgroundColor := FActiveButtonColor;
      Btn.CaptionSettings.Font.Color := FActiveButtonFontColor;
      // Potentially different border for selected
      Btn.BorderSettings.Color := DarkerColor(FActiveButtonColor, 20);
    end
    else
    begin
      Btn.BorderSettings.BackgroundColor := FInactiveButtonColor;
      Btn.CaptionSettings.Font.Color := FInactiveButtonFontColor;
      Btn.BorderSettings.Color := FBorderSettings.Color; // Default border color
    end;

    // Adjust corner radius based on position for a continuous look
    IsFirst := (i = 0);
    IsLast := (i = FButtons.Count - 1);
    IsMiddle := not IsFirst and not IsLast;

    Btn.BorderSettings.RoundCornerType := rctNone; // Default for middle buttons

    if FButtons.Count = 1 then // Single button takes full group radius
    begin
        Btn.BorderSettings.RoundCornerType := FBorderSettings.RoundCornerType;
        Btn.BorderSettings.CornerRadius := FBorderSettings.CornerRadius;
    end
    else if FOrientation = bgoHorizontal then
    begin
      if IsFirst then
      begin
        Btn.BorderSettings.CornerRadius := EffectiveRadius;
        Btn.BorderSettings.RoundCornerType := rctLeft;
        if FBorderSettings.RoundCornerType = rctAll then Btn.BorderSettings.RoundCornerType := rctLeft
        else if FBorderSettings.RoundCornerType = rctTopLeft then Btn.BorderSettings.RoundCornerType := rctTopLeft
        else if FBorderSettings.RoundCornerType = rctBottomLeft then Btn.BorderSettings.RoundCornerType := rctBottomLeft
        else if FBorderSettings.RoundCornerType = rctTop then Btn.BorderSettings.RoundCornerType := rctTopLeft
        else if FBorderSettings.RoundCornerType = rctBottom then Btn.BorderSettings.RoundCornerType := rctBottomLeft;

      end
      else if IsLast then
      begin
        Btn.BorderSettings.CornerRadius := EffectiveRadius;
        Btn.BorderSettings.RoundCornerType := rctRight;
        if FBorderSettings.RoundCornerType = rctAll then Btn.BorderSettings.RoundCornerType := rctRight
        else if FBorderSettings.RoundCornerType = rctTopRight then Btn.BorderSettings.RoundCornerType := rctTopRight
        else if FBorderSettings.RoundCornerType = rctBottomRight then Btn.BorderSettings.RoundCornerType := rctBottomRight
        else if FBorderSettings.RoundCornerType = rctTop then Btn.BorderSettings.RoundCornerType := rctTopRight
        else if FBorderSettings.RoundCornerType = rctBottom then Btn.BorderSettings.RoundCornerType := rctBottomRight;
      end
      else // Middle buttons
      begin
        Btn.BorderSettings.CornerRadius := 0; // No radius for middle buttons in horizontal
      end;
    end
    else // bgoVertical
    begin
      if IsFirst then
      begin
        Btn.BorderSettings.CornerRadius := EffectiveRadius;
        Btn.BorderSettings.RoundCornerType := rctTop;
         if FBorderSettings.RoundCornerType = rctAll then Btn.BorderSettings.RoundCornerType := rctTop
        else if FBorderSettings.RoundCornerType = rctTopLeft then Btn.BorderSettings.RoundCornerType := rctTopLeft
        else if FBorderSettings.RoundCornerType = rctTopRight then Btn.BorderSettings.RoundCornerType := rctTopRight
        else if FBorderSettings.RoundCornerType = rctLeft then Btn.BorderSettings.RoundCornerType := rctTopLeft
        else if FBorderSettings.RoundCornerType = rctRight then Btn.BorderSettings.RoundCornerType := rctTopRight;
      end
      else if IsLast then
      begin
        Btn.BorderSettings.CornerRadius := EffectiveRadius;
        Btn.BorderSettings.RoundCornerType := rctBottom;
        if FBorderSettings.RoundCornerType = rctAll then Btn.BorderSettings.RoundCornerType := rctBottom
        else if FBorderSettings.RoundCornerType = rctBottomLeft then Btn.BorderSettings.RoundCornerType := rctBottomLeft
        else if FBorderSettings.RoundCornerType = rctBottomRight then Btn.BorderSettings.RoundCornerType := rctBottomRight
        else if FBorderSettings.RoundCornerType = rctLeft then Btn.BorderSettings.RoundCornerType := rctBottomLeft
        else if FBorderSettings.RoundCornerType = rctRight then Btn.BorderSettings.RoundCornerType := rctBottomRight;
      end
      else // Middle buttons
      begin
        Btn.BorderSettings.CornerRadius := 0; // No radius for middle buttons in vertical
      end;
    end;
    Btn.Repaint;
  end;
  Repaint; // Group itself might need repaint due to selection changes affecting borders/separators
end;

end.
