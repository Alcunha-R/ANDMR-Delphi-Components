unit ANDMR_CPanel;

interface

uses
  Vcl.Controls, // Moved to top
  // Core System units
  System.SysUtils, System.Classes, System.Types, System.Math,
  // Windows API
  Winapi.Windows, Winapi.Messages,
  // GDI+
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  // VCL core (remaining)
  Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls,
  // Custom utilities
  ANDMR_ComponentUtils;

type
  TCaptionVerticalAlignment = (cvaTop, cvaCenter, cvaBottom); // New enum for vertical alignment

  TANDMR_CPanel = class(TCustomControl)
  private
    // Private fields for properties
    FColor: TColor;
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FCaption: TCaption; // VCL's TCaption type
    FFont: TFont;
    FDropShadowEnabled: Boolean;
    FDropShadowColor: TColor;
    FDropShadowOffset: TPoint; // Or separate X/Y offsets
    FDropShadowBlurRadius: Integer; // For a softer shadow effect
    FOpacity: Byte; // Added for opacity control
    FCaptionAlignment: System.Classes.TAlignment; // Added for caption alignment
    FCaptionVerticalAlignment: TCaptionVerticalAlignment; // New field for vertical alignment
    FCaptionWordWrap: Boolean; // New field for WordWrap
    FCaptionOffsetX: Integer; // New field for Caption X Offset
    FCaptionOffsetY: Integer; // New field for Caption Y Offset
    FDisabledFontColor: TColor; // New field for Disabled Font Color
    FTransparentChildren: Boolean; // New field for TransparentChildren
    FWindowRegion: HRGN; // Field to store the handle to the window region

    // Property Setters
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Integer);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetCaption(const Value: TCaption);
    procedure SetComponentFont(const Value: TFont); // Renamed to avoid conflict with inherited Font
    procedure SetDropShadowEnabled(const Value: Boolean);
    procedure SetDropShadowColor(const Value: TColor);
    procedure SetDropShadowOffset(const Value: TPoint);
    procedure SetDropShadowBlurRadius(const Value: Integer);
    procedure SetOpacity(const Value: Byte); // Added for opacity control
    procedure SetCaptionAlignment(const Value: System.Classes.TAlignment); // Added for caption alignment
    procedure SetCaptionVerticalAlignment(const Value: TCaptionVerticalAlignment); // New setter
    procedure SetCaptionWordWrap(const Value: Boolean); // New setter for WordWrap
    procedure SetCaptionOffsetX(const Value: Integer); // New setter for X Offset
    procedure SetCaptionOffsetY(const Value: Integer); // New setter for Y Offset
    procedure SetDisabledFontColor(const Value: TColor); // New setter for Disabled Font Color
    procedure SetTransparentChildren(const Value: Boolean); // New setter for TransparentChildren

    // Internal methods
    procedure FontChanged(Sender: TObject);
    procedure UpdateRegion; // Method to update the window region

  protected
    // Method overrides
    procedure Paint; override;
    procedure Loaded; override; // For initial setup after properties are loaded
    // procedure WndProc(var Message: TMessage); override; // If specific message handling is needed
    procedure Resize; override;
    procedure CreateWnd; override; // Added for UpdateRegion call on handle creation

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // Publish properties that will be visually configurable
    property Color: TColor read FColor write SetColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 0;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctNone;
    property Caption: TCaption read FCaption write SetCaption;
    property CaptionAlignment: System.Classes.TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter;
    property CaptionVerticalAlignment: TCaptionVerticalAlignment read FCaptionVerticalAlignment write SetCaptionVerticalAlignment default cvaCenter;
    property CaptionWordWrap: Boolean read FCaptionWordWrap write SetCaptionWordWrap default False;
    property CaptionOffsetX: Integer read FCaptionOffsetX write SetCaptionOffsetX default 0;
    property CaptionOffsetY: Integer read FCaptionOffsetY write SetCaptionOffsetY default 0;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor default clGrayText;
    property Font: TFont read FFont write SetComponentFont;
    property TransparentChildren: Boolean read FTransparentChildren write SetTransparentChildren default False; // New property

    property DropShadowEnabled: Boolean read FDropShadowEnabled write SetDropShadowEnabled default False;
    property DropShadowColor: TColor read FDropShadowColor write SetDropShadowColor default clBlack;
    property DropShadowOffset: TPoint read FDropShadowOffset write SetDropShadowOffset;
    property DropShadowBlurRadius: Integer read FDropShadowBlurRadius write SetDropShadowBlurRadius default 3;
    property Opacity: Byte read FOpacity write SetOpacity default 255; // Added for opacity control

    // Standard published properties (many inherited from TCustomControl)
    property Align;
    property Anchors;
    property Constraints;
    property DockSite; // Important for a panel-like control
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    // Standard events
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    // Add more events as needed (e.g., OnCanResize, OnConstrainedResize)
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CPanel]);
end;

constructor TANDMR_CPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBtnFace;
  FBorderColor := clBlack;
  FBorderThickness := 1;
  FBorderStyle := psSolid;
  FCornerRadius := 0;
  FRoundCornerType := rctNone;
  FCaption := '';
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI'; // Added for consistency
  FFont.Size := 9;         // Added for consistency
  FFont.OnChange := FontChanged;
  FCaptionAlignment := taCenter; // Initialize caption alignment
  FCaptionVerticalAlignment := cvaCenter; // Initialize vertical caption alignment
  FCaptionWordWrap := False; // Initialize WordWrap
  FCaptionOffsetX := 0; // Initialize X Offset
  FCaptionOffsetY := 0; // Initialize Y Offset
  FDisabledFontColor := clGrayText; // Initialize Disabled Font Color
  FTransparentChildren := False; // Initialize TransparentChildren
  FWindowRegion := 0; // Initialize window region handle

  FDropShadowEnabled := False;
  FDropShadowColor := clBlack;
  FDropShadowOffset := Point(2, 2);
  FDropShadowBlurRadius := 3;
  // FOpacity will be set and applied below

  Width := 185;
  Height := 85;
  // Initialize ControlStyle without csOpaque initially, SetOpacity will handle it.
  // csClipChildren removed to avoid compilation error on older VCLs.
  ControlStyle := ControlStyle + [csAcceptsControls, csReplicatable, csDoubleClicks];
  DoubleBuffered := True;

  FOpacity := 255; // Default to fully opaque
  SetOpacity(FOpacity); // Apply initial opacity and set ControlStyle flags accordingly

  // UpdateRegion; // Removed: Called too early, before handle and parent are set.
end;

destructor TANDMR_CPanel.Destroy;
begin
  if FWindowRegion <> 0 then
  begin
    DeleteObject(FWindowRegion);
    FWindowRegion := 0;
  end;
  FFont.Free;
  inherited Destroy;
end;

procedure TANDMR_CPanel.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetBorderThickness(const Value: Integer);
var
  CorrectedValue: Integer;
begin
  CorrectedValue := Max(0, Value);
  if FBorderThickness <> CorrectedValue then
  begin
    FBorderThickness := CorrectedValue;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetCornerRadius(const Value: Integer);
var
  CorrectedValue: Integer;
begin
  CorrectedValue := Max(0, Value);
  if FCornerRadius <> CorrectedValue then
  begin
    FCornerRadius := CorrectedValue;
    UpdateRegion; // Update region when corner radius changes
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
    UpdateRegion; // Update region when corner type changes
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetComponentFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TANDMR_CPanel.SetDropShadowEnabled(const Value: Boolean);
begin
  if FDropShadowEnabled <> Value then
  begin
    FDropShadowEnabled := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetDropShadowColor(const Value: TColor);
begin
  if FDropShadowColor <> Value then
  begin
    FDropShadowColor := Value;
    if FDropShadowEnabled then // Only repaint if shadow is active
      Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetDropShadowOffset(const Value: TPoint);
begin
  if (FDropShadowOffset.X <> Value.X) or (FDropShadowOffset.Y <> Value.Y) then
  begin
    FDropShadowOffset := Value;
    if FDropShadowEnabled then
      Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetDropShadowBlurRadius(const Value: Integer);
var
  CorrectedValue: Integer;
begin
  CorrectedValue := Max(0, Value);
  if FDropShadowBlurRadius <> CorrectedValue then
  begin
    FDropShadowBlurRadius := CorrectedValue;
    if FDropShadowEnabled then // Only repaint if shadow is active
      Invalidate;
  end;
end;

procedure TANDMR_CPanel.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CPanel.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 255 then
    begin
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
      if Parent <> nil then Parent.Invalidate; // Parent needs to redraw area behind
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    end;
    UpdateRegion; // Update region as opacity can affect if it's needed (indirectly, via UpdateRegion's own logic)
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.Paint;
var
  LG: TGPGraphics;
  LClientRect: TRect;
  LPathRect: TGPRectF;
  LShadowRect: TGPRectF;
  LPanelPath: TGPGraphicsPath;
  LShadowPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LTextRect: TRect;
  LShadowBrush: TGPSolidBrush;
  LBackgroundColor, LBorderColor: ARGB;
  LShadowColorAlpha: ARGB;
  // DashStyle: TGPSDashStyle; // Removed intermediate variable
  LDrawTextFlags: Cardinal;
  LPathInset: Single; // For border adjustment
begin
  inherited Paint; // Call inherited paint first

  LClientRect := Self.ClientRect;
  if (LClientRect.Width <= 0) or (LClientRect.Height <= 0) then Exit;

  LG := TGPGraphics.Create(Self.Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // Prepare path rectangle, adjusted for border thickness
    LPathInset := FBorderThickness / 2.0;
    LPathRect.X := LClientRect.Left + LPathInset;
    LPathRect.Y := LClientRect.Top + LPathInset;
    LPathRect.Width := LClientRect.Width - (2 * LPathInset); // or LClientRect.Width - FBorderThickness
    LPathRect.Height := LClientRect.Height - (2 * LPathInset); // or LClientRect.Height - FBorderThickness

    if LPathRect.Width <= 0 then LPathRect.Width := 1; // Ensure positive width
    if LPathRect.Height <= 0 then LPathRect.Height := 1; // Ensure positive height


    // --- 1. Draw Drop Shadow ---
    if FDropShadowEnabled and (FDropShadowBlurRadius > 0) then // Also check blur radius
    begin
      LShadowRect := LPathRect;
      // LShadowRect.Offset(FDropShadowOffset.X, FDropShadowOffset.Y); // Replaced with manual adjustment
      LShadowRect.X := LShadowRect.X + FDropShadowOffset.X;
      LShadowRect.Y := LShadowRect.Y + FDropShadowOffset.Y;

      LShadowPath := TGPGraphicsPath.Create; // Initialize the path object
      CreateGPRoundedPath(LShadowPath, LShadowRect, Single(FCornerRadius), FRoundCornerType); // Call as a procedure
      // if LShadowPath <> nil then // Assuming the procedure populates it, or raises an error. Path object itself existing is the check.
      try
        // For a simple shadow, use a semi-transparent color.
        // A more advanced blur would involve multiple layers or a bitmap effect.
        LShadowColorAlpha := ColorToARGB(FDropShadowColor, Max(0, Min(255, FOpacity div 3))); // Example: 1/3 of main opacity for shadow
        LShadowBrush := TGPSolidBrush.Create(LShadowColorAlpha);
        try
          LG.FillPath(LShadowBrush, LShadowPath);
        finally
          LShadowBrush.Free;
        end;
      finally
        LShadowPath.Free;
      end;
    end;

    // --- 2. Draw Panel Body and Border ---
    LPanelPath := TGPGraphicsPath.Create; // Initialize the path object
    CreateGPRoundedPath(LPanelPath, LPathRect, Single(FCornerRadius), FRoundCornerType); // Call as a procedure
    // if LPanelPath = nil then Exit; // Path object itself existing is the check. If it's empty, drawing ops might be no-ops or error.
    try
      // Fill the background
      if (FColor <> clNone) and (FOpacity > 0) then
      begin
        LBackgroundColor := ColorToARGB(FColor, FOpacity);
        LBrush := TGPSolidBrush.Create(LBackgroundColor);
        try
          LG.FillPath(LBrush, LPanelPath);
        finally
          LBrush.Free;
        end;
      end;

      // Draw the border
      if (FBorderThickness > 0) and (FBorderStyle <> psClear) and (FBorderColor <> clNone) and (FOpacity > 0) then
      begin
        LBorderColor := ColorToARGB(FBorderColor, FOpacity);
        LPen := TGPPen.Create(LBorderColor, FBorderThickness);
        try
          case FBorderStyle of
            psSolid: LPen.SetDashStyle(DashStyleSolid);
            psDash: LPen.SetDashStyle(DashStyleDash);
            psDot: LPen.SetDashStyle(DashStyleDot);
            psDashDot: LPen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: LPen.SetDashStyle(DashStyleDashDotDot);
            // psClear is handled by the condition "FBorderStyle <> psClear" before this block
          else
            LPen.SetDashStyle(DashStyleSolid);
          end;
          LG.DrawPath(LPen, LPanelPath);
        finally
          LPen.Free;
        end;
      end;
    finally
      LPanelPath.Free;
    end;

    // --- 3. Draw Caption ---
    if (FCaption <> '') and (FFont <> nil) and (FOpacity > 0) then
    begin
      Self.Canvas.Font.Assign(FFont);
      Self.Canvas.Brush.Style := bsClear; // Transparent background for text

      // Adjust text rect for padding, border. For simplicity, using ClientRect for now.
      // More precise calculation would involve FBorderThickness and potentially FCornerRadius
      // to avoid drawing text over rounded corners or too close to the border.
      LTextRect := Self.ClientRect;
      if FBorderThickness > 0 then // Basic padding from border
          InflateRect(LTextRect, -FBorderThickness -2, -FBorderThickness -2)
      else
          InflateRect(LTextRect, -2, -2);

      // Apply custom offsets
      OffsetRect(LTextRect, FCaptionOffsetX, FCaptionOffsetY);

      // Set text color with opacity. Note: VCL DrawText doesn't directly support alpha.
      // For true alpha-blended text, one would need to use GDI+ text rendering.
      // Here, we set the font color, which VCL will use.
      if Self.Enabled then
      begin
        Self.Canvas.Font.Color := FFont.Color; // Use the regular font color from FFont
      end
      else
      begin
        // If FDisabledFontColor is clNone, it implies we should use a system default for disabled text.
        // clGrayText is a common representation of this.
        // If FDisabledFontColor has a specific color (even if it's clGrayText set by user), use that.
        if FDisabledFontColor = clNone then
            Self.Canvas.Font.Color := clGrayText
        else
            Self.Canvas.Font.Color := FDisabledFontColor; // Use the user-defined or default FDisabledFontColor
      end;

      LDrawTextFlags := DT_NOPREFIX; // Base flags, DT_SINGLELINE or DT_WORDBREAK will be added later

      // Horizontal Alignment
      case FCaptionAlignment of
        taLeftJustify: LDrawTextFlags := LDrawTextFlags or DT_LEFT;
        taRightJustify: LDrawTextFlags := LDrawTextFlags or DT_RIGHT;
        taCenter: LDrawTextFlags := LDrawTextFlags or DT_CENTER;
      end;

      // Vertical Alignment
      case FCaptionVerticalAlignment of
        cvaTop: LDrawTextFlags := LDrawTextFlags or DT_TOP;
        cvaCenter: LDrawTextFlags := LDrawTextFlags or DT_VCENTER;
        cvaBottom: LDrawTextFlags := LDrawTextFlags or DT_BOTTOM;
      end;

      // WordWrap and SingleLine/Ellipsis
      if FCaptionWordWrap then
      begin
        LDrawTextFlags := LDrawTextFlags or DT_WORDBREAK;
        // Consider DT_EDITCONTROL for better multiline layout if needed, but DT_WORDBREAK is primary.
      end
      else
      begin
        LDrawTextFlags := LDrawTextFlags or DT_SINGLELINE;
        LDrawTextFlags := LDrawTextFlags or DT_END_ELLIPSIS; // Add ellipsis for single line if text overflows
      end;

      if Length(Trim(FCaption)) > 0 then // Check if caption is not just whitespace
         DrawText(Self.Canvas.Handle, PChar(FCaption), Length(FCaption), LTextRect, LDrawTextFlags);
    end;

  finally
    LG.Free;
  end;
end;

procedure TANDMR_CPanel.Loaded;
begin
  inherited Loaded;
  // Re-apply opacity to ensure ControlStyle is correct after DFM loading
  // This ensures that csOpaque/csParentBackground are set correctly based on initial FOpacity
  if FOpacity < 255 then
  begin
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
  end
  else
  begin
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  end;
  // No need to call SetOpacity(FOpacity) directly as that would be a redundant check.
  // We've already loaded FOpacity and now we're just ensuring ControlStyle matches.
  UpdateRegion; // Ensure region is set after DFM properties are loaded
  Invalidate; // Ensure a repaint after loading and ControlStyle adjustment
end;

procedure TANDMR_CPanel.SetCaptionAlignment(const Value: System.Classes.TAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
    Invalidate; // Caption alignment change requires repaint
  end;
end;

procedure TANDMR_CPanel.SetCaptionVerticalAlignment(const Value: TCaptionVerticalAlignment);
begin
  if FCaptionVerticalAlignment <> Value then
  begin
    FCaptionVerticalAlignment := Value;
    Invalidate; // Caption alignment change requires repaint
  end;
end;

procedure TANDMR_CPanel.SetCaptionWordWrap(const Value: Boolean);
begin
  if FCaptionWordWrap <> Value then
  begin
    FCaptionWordWrap := Value;
    Invalidate; // Caption wrapping change requires repaint
  end;
end;

procedure TANDMR_CPanel.SetCaptionOffsetX(const Value: Integer);
begin
  if FCaptionOffsetX <> Value then
  begin
    FCaptionOffsetX := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetCaptionOffsetY(const Value: Integer);
begin
  if FCaptionOffsetY <> Value then
  begin
    FCaptionOffsetY := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetDisabledFontColor(const Value: TColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    if not Enabled then // Only repaint if currently disabled
      Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetTransparentChildren(const Value: Boolean);
begin
  if FTransparentChildren <> Value then
  begin
    FTransparentChildren := Value;
    // This property is declarative in its basic form.
    // Its visual effect relies on Opacity < 255 (which sets csParentBackground).
    // If Opacity is already < 255, csParentBackground is active, making the panel's
    // own background transparent and thus child controls appear over the parent's background.
    // If Opacity is 255, this flag alone won't make children transparent over controls *behind* the panel.
    // A more advanced implementation might iterate child controls or use other techniques.
    Invalidate; // Repaint to reflect any conceptual change or if visual cues were added.
  end;
end;

procedure TANDMR_CPanel.Resize;
begin
  inherited Resize;
  UpdateRegion;
end;

procedure TANDMR_CPanel.CreateWnd;
begin
  inherited CreateWnd;
  UpdateRegion; // Ensure region is set when window handle is created
end;

procedure TANDMR_CPanel.UpdateRegion;
var
  LPath: TGPGraphicsPath;
  LGDIRgn: TGPRegion;
  LG: TGPGraphics;
  LClientRect: TRect;
  LPathRect: TGPRectF;
  NewRegion: HRGN; // Store the newly created region
  OldRegion: HRGN; // Store the current FWindowRegion before changing it
begin
  if not HandleAllocated then // Safeguard: Exit if handle is not allocated
    Exit;

  OldRegion := FWindowRegion; // Keep track of the current region
  NewRegion := 0;             // Initialize new region to null

  LClientRect := Self.ClientRect;

  // Only create a custom region if corners are rounded and dimensions are valid
  if (FCornerRadius > 0) and (LClientRect.Width > 0) and (LClientRect.Height > 0) then
  begin
    LPath := TGPGraphicsPath.Create;
    try
      LPathRect.X := 0; // Region coordinates are relative to the window's client area
      LPathRect.Y := 0;
      LPathRect.Width := LClientRect.Width;
      LPathRect.Height := LClientRect.Height;

      // Ensure width and height are not negative (already checked for LClientRect but good practice for LPathRect if modified)
      if LPathRect.Width < 0 then LPathRect.Width := 0;
      if LPathRect.Height < 0 then LPathRect.Height := 0;

      if (LPathRect.Width > 0) and (LPathRect.Height > 0) then // Double check after potential adjustments
      begin
        ANDMR_ComponentUtils.CreateGPRoundedPath(LPath, LPathRect, Single(FCornerRadius), FRoundCornerType);

        // Create a GDI+ region from this path
        // A temporary TGPGraphics object is needed for TGPRegion.GetHRGN
        LG := TGPGraphics.Create(Self.Handle);
        try
          LGDIRgn := TGPRegion.Create(LPath);
          try
            NewRegion := LGDIRgn.GetHRGN(LG); // This is the new region handle
          finally
            LGDIRgn.Free;
          end;
        finally
          LG.Free;
        end;
      end; // else NewRegion remains 0
    finally
      LPath.Free;
    end;
  end; // Else, NewRegion remains 0, effectively clearing or not setting a custom region.

  // Apply the new region (or clear if NewRegion is 0 and OldRegion was not 0)
  // Only call SetWindowRgn if the new region is different from the old one,
  // or if we are explicitly trying to clear a region when one was previously set.
  if (HandleAllocated) and ((NewRegion <> OldRegion) or (NewRegion = 0 And OldRegion <> 0 )) then
  begin
    SetWindowRgn(Self.Handle, NewRegion, True); // True to force repaint
    FWindowRegion := NewRegion; // Update the stored region handle

    // If there was an old region, and it's different from the new one, delete the old one.
    // The system takes ownership of NewRegion if NewRegion is not 0 and SetWindowRgn is successful.
    if (OldRegion <> 0) and (OldRegion <> NewRegion) then
    begin
      DeleteObject(OldRegion);
    end;
  end
  else if NewRegion <> 0 And NewRegion <> OldRegion then
  begin
    // If SetWindowRgn was not called (e.g. Handle not allocated yet) but we created a new region
    // that's different from OldRegion, we need to delete this new region as it won't be owned by the system.
    // And also delete OldRegion if it exists.
    DeleteObject(NewRegion);
    if OldRegion <> 0 then DeleteObject(OldRegion);
    FWindowRegion := 0; // No region is currently set
  end
  else if NewRegion = 0 And OldRegion <> 0 And not HandleAllocated then
  begin
    // If we intended to clear the region (NewRegion is 0) but couldn't call SetWindowRgn,
    // we still need to delete the OldRegion.
     DeleteObject(OldRegion);
     FWindowRegion := 0;
  end;
  // If NewRegion is 0 and OldRegion is 0, nothing to do.
  // If NewRegion is same as OldRegion (and not 0), nothing to do regarding SetWindowRgn or deletion.

end;

end.
