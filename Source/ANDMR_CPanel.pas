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

    // Internal methods
    procedure FontChanged(Sender: TObject);

  protected
    // Method overrides
    procedure Paint; override;
    procedure Loaded; override; // For initial setup after properties are loaded
    // procedure WndProc(var Message: TMessage); override; // If specific message handling is needed

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
    property CaptionAlignment: System.Classes.TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter; // Added
    property Font: TFont read FFont write SetComponentFont; // Use custom setter for Font

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
end;

destructor TANDMR_CPanel.Destroy;
begin
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
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
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
      //if Parent <> nil then Parent.Invalidate; // Parent needs to redraw area behind - this can cause flickering or issues if not handled carefully by parent
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    end;
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
      LTextRect := LClientRect;
      // InflateRect(LTextRect, -FBorderThickness - 2, -FBorderThickness - 2); // Simple padding

      // Set text color with opacity. Note: VCL DrawText doesn't directly support alpha.
      // For true alpha-blended text, one would need to use GDI+ text rendering.
      // Here, we set the font color, which VCL will use.
      Self.Canvas.Font.Color := FFont.Color; // Opacity is handled by FOpacity on the whole control for now

      LDrawTextFlags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX; // Basic flags

      case FCaptionAlignment of
        taLeftJustify: LDrawTextFlags := LDrawTextFlags or DT_LEFT;
        taRightJustify: LDrawTextFlags := LDrawTextFlags or DT_RIGHT;
        taCenter: LDrawTextFlags := LDrawTextFlags or DT_CENTER;
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

end.
