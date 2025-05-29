procedure TANDMR_CEdit.Paint;
var
  LG: TGPGraphics;
  TextToDisplay: string;
  TextFlags: Cardinal;
  imgR, txtR, sepR: TRect;
  //OverallBGColor: TColor; // Superseded by Actual* logic
  RectToDrawEditBoxIn: TRect; // Used for FocusUnderline, should be the EditBoxDrawingRect
  PaddedTextDrawArea: TRect;
  InternalTextPaddingX, InternalTextPaddingY: Integer;
  FullClientRect: TRect;

  // Variables for determined colors based on state
  ActualEditBGColor, ActualEditBorderColor, ActualEditTextColor, ActualCaptionTextColor: TColor;
  // Variables for the specific area to draw the edit box and its background
  EditBoxDrawingRect: TRect; // The rect for the DrawEditBox call
  BGForDrawEditBox: TColor;  // The background color for DrawEditBox
begin
  FullClientRect := Self.ClientRect; // Cache client rect
  CalculateLayout(imgR, txtR, sepR); // This sets FCaptionRect and out params for edit area

  InternalTextPaddingX := 4;
  InternalTextPaddingY := 2;

  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      // --- Determine current state colors ---
      // 1. Initialize with base colors (not focused, not hovered)
      ActualEditBorderColor := FBorderColor;
      ActualEditTextColor := Self.Font.Color;

      if FCaptionSettings.Color = clDefault then
        ActualCaptionTextColor := Self.Font.Color
      else
        ActualCaptionTextColor := FCaptionSettings.Color;

      if FImagePlacement = iplInsideBounds then
        ActualEditBGColor := FInactiveColor
      else // iplOutsideBounds, text field has its own typical background
        ActualEditBGColor := clWindow;

      // 2. Apply Hover Settings if enabled and hovered (overrides base)
      if FHovered and FHoverSettings.Enabled then
      begin
        if FHoverSettings.BackgroundColor <> clNone then ActualEditBGColor := FHoverSettings.BackgroundColor;
        if FHoverSettings.BorderColor <> clNone then ActualEditBorderColor := FHoverSettings.BorderColor;
        if FHoverSettings.FontColor <> clNone then ActualEditTextColor := FHoverSettings.FontColor;
        if FHoverSettings.CaptionFontColor <> clNone then ActualCaptionTextColor := FHoverSettings.CaptionFontColor;
      end;

      // 3. Apply Focus Settings (can override Hover or Base colors)
      if Self.Focused then
      begin
        // Focused Border Color: Specific focus color takes precedence, then ActiveColor.
        if FFocusBorderColorVisible and (FFocusBorderColor <> clNone) then
          ActualEditBorderColor := FFocusBorderColor
        else
          ActualEditBorderColor := FActiveColor; // Default focus border

        // Focused Background Color: Specific focus BG takes precedence.
        // If no specific focus BG, hover BG is allowed to show through.
        // If neither specific focus BG nor hover BG is active, use clWindow default.
        if FFocusBackgroundColorVisible and (FFocusBackgroundColor <> clNone) then
          ActualEditBGColor := FFocusBackgroundColor
        else if not (FHovered and FHoverSettings.Enabled and (FHoverSettings.BackgroundColor <> clNone)) then
        begin // Not overridden by specific Focus BG, AND not overridden by Hover BG
            if FImagePlacement = iplInsideBounds then ActualEditBGColor := clWindow
            else ActualEditBGColor := clWindow;
        end;
        // ActualEditTextColor and ActualCaptionTextColor retain their current values (base or hover)
        // unless specific focus font color properties were to be added and checked here.
      end;

      // --- Painting ---
      // 1. Paint component's main background (entire ClientRect)
      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(FullClientRect);
      end; // If FOpacity < 255, parent's background shows through due to csOpaque removal.

      // 2. Determine the rectangle for the main edit box drawing operations
      //    and the background color to pass to DrawEditBox.
      if FImagePlacement = iplInsideBounds then
      begin
        // The "edit box" visual is the entire component area, adjusted for caption.
        EditBoxDrawingRect := FullClientRect;
        if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
        begin
            case FCaptionSettings.Position of
              cpAbove: EditBoxDrawingRect.Top := FCaptionRect.Bottom + FCaptionSettings.Offset;
              cpBelow: EditBoxDrawingRect.Bottom := FCaptionRect.Top - FCaptionSettings.Offset;
              cpLeft:  EditBoxDrawingRect.Left := FCaptionRect.Right + FCaptionSettings.Offset;
              cpRight: EditBoxDrawingRect.Right := FCaptionRect.Left - FCaptionSettings.Offset;
            end;
        end;
        BGForDrawEditBox := ActualEditBGColor; // This is the background for the main component shape.
      end
      else // iplOutsideBounds
      begin
        // The "edit box" visual is only the text input area (txtR).
        // The area around it (part of FullClientRect) was filled by Self.Color or is transparent.
        EditBoxDrawingRect := txtR; // txtR is already adjusted for caption and image.
        BGForDrawEditBox := ActualEditBGColor; // This is the background specifically for the text field.
      end;
      RectToDrawEditBoxIn := EditBoxDrawingRect; // For focus underline, it should relate to what was just drawn by DrawEditBox.

      // 3. Draw the main edit box (background and border)
      DrawEditBox(EditBoxDrawingRect, LG, BGForDrawEditBox, ActualEditBorderColor);

      // 4. Draw Image
      // For iplInsideBounds, image is drawn on top of BGForDrawEditBox.
      // For iplOutsideBounds, image is drawn on Self.Color (already painted or transparent)
      if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
      begin
        if (FImage.Graphic is TPNGImage) then
          Self.DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode)
        else
          Self.DrawNonPNGImageWithCanvas(Canvas, FImage.Graphic, imgR, FImageDrawMode);
      end;
      
      // 5. Draw Separator (drawn on top of whatever is beneath it)
      if FSeparatorVisible and (FSeparatorThickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
        Self.DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorColor, FSeparatorThickness);

      // 6. Draw Focus Underline
      if Self.Focused and FFocusUnderlineVisible and (FFocusUnderlineThickness > 0) then
      begin
        var UnderlineY: Integer;
        var UnderlinePen: TGPPen;
        
        // Underline should be relative to the main edit box area that was just drawn.
        if FBorderThickness > 0 then
          UnderlineY := EditBoxDrawingRect.Bottom - FBorderThickness - (FFocusUnderlineThickness div 2)
        else
          UnderlineY := EditBoxDrawingRect.Bottom - (FFocusUnderlineThickness div 2);
        UnderlineY := Min(UnderlineY, EditBoxDrawingRect.Bottom - FFocusUnderlineThickness);

        UnderlinePen := TGPPen.Create(ColorToARGB(FFocusUnderlineColor, Self.FOpacity), FFocusUnderlineThickness);
        try
          case FFocusUnderlineStyle of
            psSolid: UnderlinePen.SetDashStyle(DashStyleSolid);
            psDash: UnderlinePen.SetDashStyle(DashStyleDash);
            psDot: UnderlinePen.SetDashStyle(DashStyleDot);
            psDashDot: UnderlinePen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: UnderlinePen.SetDashStyle(DashStyleDashDotDot);
            else UnderlinePen.SetDashStyle(DashStyleSolid);
          end;
          LG.DrawLine(UnderlinePen, EditBoxDrawingRect.Left + FBorderThickness, UnderlineY, EditBoxDrawingRect.Right - FBorderThickness, UnderlineY);
        finally
          UnderlinePen.Free;
        end;
      end;

    finally
      LG.Free;
    end; // End of GDI+ operations

    // --- Text Rendering (using Canvas, relative to txtR) ---
    PaddedTextDrawArea := txtR; 
    PaddedTextDrawArea.Left := txtR.Left + InternalTextPaddingX;
    PaddedTextDrawArea.Top := txtR.Top + InternalTextPaddingY;
    PaddedTextDrawArea.Right := txtR.Right - InternalTextPaddingX;
    PaddedTextDrawArea.Bottom := txtR.Bottom - InternalTextPaddingY;

    if PaddedTextDrawArea.Right < PaddedTextDrawArea.Left then PaddedTextDrawArea.Right := PaddedTextDrawArea.Left;
    if PaddedTextDrawArea.Bottom < PaddedTextDrawArea.Top then PaddedTextDrawArea.Bottom := PaddedTextDrawArea.Top;
    
    if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
      TextToDisplay := StringOfChar(FPasswordChar, Length(FText))
    else
      TextToDisplay := FText; // FText is the masked text if mask is used

    Canvas.Font.Assign(Self.Font);
    Canvas.Font.Color := ActualEditTextColor; // Use the determined color
    Canvas.Brush.Style := bsClear;
    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if Length(TextToDisplay) > 0 and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), PaddedTextDrawArea, TextFlags);

    // --- Caret Drawing (relative to PaddedTextDrawArea) ---
    if Self.Focused and FCaretVisible and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
    begin
      var CaretXBase, CaretTop, CaretHeight, CaretXOffset: Integer;
      var TextBeforeCaretVisible: string;
      if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
        TextBeforeCaretVisible := StringOfChar(FPasswordChar, FCaretPosition)
      else
        TextBeforeCaretVisible := Copy(FText, 1, FCaretPosition); 

      CaretXBase := PaddedTextDrawArea.Left;
      CaretHeight := Canvas.TextHeight('Tg'); 
      CaretTop := PaddedTextDrawArea.Top + (PaddedTextDrawArea.Height - CaretHeight) div 2;
      CaretXOffset := Canvas.TextWidth(TextBeforeCaretVisible);
      Canvas.Pen.Color := ActualEditTextColor; 
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(CaretXBase + CaretXOffset, CaretTop);
      Canvas.LineTo(CaretXBase + CaretXOffset, CaretTop + CaretHeight);
    end;

    // --- Draw Caption ---
    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionRect.Width > 0) and (FCaptionRect.Height > 0) then
    begin
      Canvas.Font.Assign(FCaptionSettings.Font);
      Canvas.Font.Color := ActualCaptionTextColor; // Use the determined color
      Canvas.Brush.Style := bsClear;

      var CaptionDrawFlags: Cardinal;
      CaptionDrawFlags := DT_NOPREFIX; 
      if FCaptionSettings.WordWrap then
        CaptionDrawFlags := CaptionDrawFlags or DT_WORDBREAK
      else
        CaptionDrawFlags := CaptionDrawFlags or DT_SINGLELINE;
      
      case FCaptionSettings.Alignment of
        taLeftJustify: CaptionDrawFlags := CaptionDrawFlags or DT_LEFT;
        taCenter: CaptionDrawFlags := CaptionDrawFlags or DT_CENTER;
        taRightJustify: CaptionDrawFlags := CaptionDrawFlags or DT_RIGHT;
      end;
      
      if FCaptionSettings.Position in [cpLeft, cpRight] then
         CaptionDrawFlags := CaptionDrawFlags or DT_VCENTER 
      else 
         CaptionDrawFlags := CaptionDrawFlags or DT_TOP;

      var TempCaptionDrawRect := FCaptionRect;
      DrawText(Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempCaptionDrawRect, CaptionDrawFlags);
    end;

  finally
    Canvas.Unlock;
  end;
end;
