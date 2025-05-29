unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX; // Added ActiveX for IStream

type
  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);

  TImageMarginsControl = record
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
  end;

  TRoundCornerType = (
    rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight,
    rctTop, rctBottom, rctLeft, rctRight,
    rctTopLeftBottomRight, rctTopRightBottomLeft
  );

  TANDMR_CEdit = class(TCustomControl)
  private
    FText: string;
    FMaxLength: Integer;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    // Placeholders for future styling properties from CButton
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType; // Requires TRoundCornerType definition
    FActiveColor, FInactiveColor: TColor;
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;
    procedure SetBorderStyle(const Value: TPenStyle);

    FImage: TPicture;
    FImageVisible: Boolean;
    FImagePosition: TImagePositionSide;
    FImageAlignment: TImageAlignmentVertical;
    FImageMargins: TImageMarginsControl;
    // Setter method declarations
    procedure SetImage(const Value: TPicture);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetImagePosition(const Value: TImagePositionSide);
    procedure SetImageAlignment(const Value: TImageAlignmentVertical);
    procedure SetImageMargins(const Value: TImageMarginsControl);
    procedure ImageChanged(Sender: TObject); // OnChange handler for FImage

    FCaretVisible: Boolean;
    FCaretPosition: Integer; // Character index after which caret is shown
    FCaretTimer: TTimer;
    procedure CaretTimerTick(Sender: TObject);

    // Event fields
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;

    procedure SetText(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetActiveColor(const Value: TColor);
    procedure SetInactiveColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Integer);

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
    procedure Paint; override;
    // procedure Enter; override; // Removed
    // procedure Exit; override; // Removed
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Text: string read FText write SetText;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;

    // Placeholder styling properties (setters to be implemented later)
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clHighlight; // Color when focused
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clBtnFace; // Background color
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;

    property Image: TPicture read FImage write SetImage;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default True;
    property ImagePosition: TImagePositionSide read FImagePosition write SetImagePosition default ipsLeft;
    property ImageAlignment: TImageAlignmentVertical read FImageAlignment write SetImageAlignment default iavCenter;
    property ImageMargins: TImageMarginsControl read FImageMargins write SetImageMargins;

    // Standard inherited properties that are relevant
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font; // Will use inherited Font property
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;

    // Standard event handlers to publish
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CEdit]);
end;

// --- Helper Functions (ColorToARGB, CreateGPRoundedPath) ---
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then
  begin
    Result := (Alpha shl 24); // Transparent black or just alpha
    Exit;
  end;
  ColorRef := ColorToRGB(AColor); // Vcl.Graphics
  Result := (Alpha shl 24) or
            ((ColorRef and $000000FF) shl 16) or // B
            (ColorRef and $0000FF00) or          // G
            ((ColorRef and $00FF0000) shr 16);   // R
end;

procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.5; // Ensure this is Single if ARadiusValue is Single
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
begin
  APath.Reset;

  if (ARect.Width <= 0) or (ARect.Height <= 0) then
  begin
    Exit;
  end;

  LRadius := ARadiusValue;
  // Ensure LRadius calculations use floating point numbers if ARect dimensions are floats
  LRadius := Min(LRadius, Min(ARect.Width / 2.0, ARect.Height / 2.0));
  LRadius := Max(0.0, LRadius); // Max with 0.0 for float comparison

  LDiameter := LRadius * 2.0;

  if (AType = rctNone) or (LRadius < MIN_RADIUS_FOR_PATH) or (LDiameter <= 0) then
  begin
    APath.AddRectangle(ARect); // TGPGraphicsPath.AddRectangle takes TGPRectF
    Exit;
  end;

  RoundTL := AType in [rctAll, rctTopLeft, rctTop, rctLeft, rctTopLeftBottomRight];
  RoundTR := AType in [rctAll, rctTopRight, rctTop, rctRight, rctTopRightBottomLeft];
  RoundBL := AType in [rctAll, rctBottomLeft, rctBottom, rctLeft, rctTopRightBottomLeft];
  RoundBR := AType in [rctAll, rctBottomRight, rctBottom, rctRight, rctTopLeftBottomRight];

  APath.StartFigure;

  // Top-Left corner
  if RoundTL then
    APath.AddArc(ARect.X, ARect.Y, LDiameter, LDiameter, 180, 90)
  else // Start with a line segment to the top-left corner
    APath.AddLine(ARect.X, ARect.Y, ARect.X, ARect.Y);


  // Top edge
  APath.AddLine(ARect.X + IfThen(RoundTL, LRadius, 0.0), ARect.Y,
                ARect.X + ARect.Width - IfThen(RoundTR, LRadius, 0.0), ARect.Y);

  // Top-Right corner
  if RoundTR then
    APath.AddArc(ARect.X + ARect.Width - LDiameter, ARect.Y, LDiameter, LDiameter, 270, 90)
  else
    APath.AddLine(ARect.X + ARect.Width, ARect.Y, ARect.X + ARect.Width, ARect.Y);

  // Right edge
  APath.AddLine(ARect.X + ARect.Width, ARect.Y + IfThen(RoundTR, LRadius, 0.0),
                ARect.X + ARect.Width, ARect.Y + ARect.Height - IfThen(RoundBR, LRadius, 0.0));

  // Bottom-Right corner
  if RoundBR then
    APath.AddArc(ARect.X + ARect.Width - LDiameter, ARect.Y + ARect.Height - LDiameter, LDiameter, LDiameter, 0, 90)
  else
    APath.AddLine(ARect.X + ARect.Width, ARect.Y + ARect.Height, ARect.X + ARect.Width, ARect.Y + ARect.Height);

  // Bottom edge
  APath.AddLine(ARect.X + ARect.Width - IfThen(RoundBR, LRadius, 0.0), ARect.Y + ARect.Height,
                ARect.X + IfThen(RoundBL, LRadius, 0.0), ARect.Y + ARect.Height);

  // Bottom-Left corner
  if RoundBL then
    APath.AddArc(ARect.X, ARect.Y + ARect.Height - LDiameter, LDiameter, LDiameter, 90, 90)
  else
    APath.AddLine(ARect.X, ARect.Y + ARect.Height, ARect.X, ARect.Y + ARect.Height);

  APath.CloseFigure; // This connects the last point to the first (Left edge implicitly handled)
end;
// --- End Helper Functions ---

procedure TANDMR_CEdit.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value); // TPicture.Assign handles nil and actual assignment
  // OnChange event for TPicture is handled in constructor/destructor or when FImage is created
  Invalidate;
end;

procedure TANDMR_CEdit.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Invalidate; // Layout and repaint needed
  end;
end;

procedure TANDMR_CEdit.SetImagePosition(const Value: TImagePositionSide);
begin
  if FImagePosition <> Value then
  begin
    FImagePosition := Value;
    Invalidate; // Layout and repaint needed
  end;
end;

procedure TANDMR_CEdit.SetImageAlignment(const Value: TImageAlignmentVertical);
begin
  if FImageAlignment <> Value then
  begin
    FImageAlignment := Value;
    Invalidate; // Repaint needed
  end;
end;

procedure TANDMR_CEdit.SetImageMargins(const Value: TImageMarginsControl);
begin
  // Record comparison: check individual fields if direct comparison isn't ideal
  if (FImageMargins.Left <> Value.Left) or (FImageMargins.Top <> Value.Top) or
     (FImageMargins.Right <> Value.Right) or (FImageMargins.Bottom <> Value.Bottom) then
  begin
    FImageMargins := Value;
    Invalidate; // Layout and repaint needed
  end;
end;

procedure TANDMR_CEdit.ImageChanged(Sender: TObject);
begin
  Invalidate; // Repaint if the image content changes
end;

constructor TANDMR_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Initialization
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable]; // Corrected ControlStyle
  DoubleBuffered := True;
  Width := 150; // Default width
  Height := 25; // Default height
  TabStop := True;
  FText := '';
  FMaxLength := 0;
  FPasswordChar := #0;
  FReadOnly := False;
  FCornerRadius := 8;
  FRoundCornerType := rctAll;
  FActiveColor := clHighlight;
  FInactiveColor := clBtnFace;
  FBorderColor := clBlack;
  FBorderThickness := 1;
  FBorderStyle := psSolid;
  // Initialize Font (using inherited Font property)
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  Font.Color := clWindowText;

  FImage := TPicture.Create;
  FImage.OnChange := Self.ImageChanged; // Assign the OnChange handler

  // Initialize image properties
  FImageVisible := True; // Matches property default
  FImagePosition := ipsLeft; // Matches property default
  FImageAlignment := iavCenter; // Matches property default
  FImageMargins.Left := 2;
  FImageMargins.Top := 2;
  FImageMargins.Right := 2;
  FImageMargins.Bottom := 2;

  FCaretVisible := False;
  FCaretPosition := 0;
  FCaretTimer := TTimer.Create(Self); // Self is the owner
  FCaretTimer.Interval := GetCaretBlinkTime;
  FCaretTimer.OnTimer := CaretTimerTick;
  FCaretTimer.Enabled := False;
end;

destructor TANDMR_CEdit.Destroy;
begin
  FCaretTimer.Free; // Free the timer

  if Assigned(FImage) then // Good practice to check if assigned
  begin
    FImage.OnChange := nil; // Clear the event handler
    FImage.Free;
  end;

  inherited Destroy;
end;

procedure TANDMR_CEdit.SetText(const Value: string);
var
  OldText: string;
begin
  OldText := FText; // Store old text for OnChange comparison
  if FText <> Value then
  begin
    FText := Value;
    FCaretPosition := Length(FText); // Move caret to the end of the new text

    FCaretVisible := True; // Ensure caret is visible
    if Focused then // Reset blink if focused
    begin
      FCaretTimer.Enabled := False;
      FCaretTimer.Enabled := True;
    end;

    Invalidate;
    if Assigned(FOnChange) and (OldText <> FText) then // Check if text actually changed
    begin
      FOnChange(Self);
    end;
  end;
end;

procedure TANDMR_CEdit.SetMaxLength(const Value: Integer);
var
  OldText: string;
  TextChanged: Boolean;
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Max(0, Value); // Ensure MaxLength is not negative
    TextChanged := False;
    OldText := FText;

    if (FMaxLength > 0) and (Length(FText) > FMaxLength) then
    begin
      FText := Copy(FText, 1, FMaxLength);
      if FCaretPosition > Length(FText) then
        FCaretPosition := Length(FText);
      TextChanged := True;
    end;

    if TextChanged then
    begin
      FCaretVisible := True; // Ensure caret is visible after potential text change
      if Focused then // Reset blink if focused
      begin
        FCaretTimer.Enabled := False;
        FCaretTimer.Enabled := True;
      end;

      if Assigned(FOnChange) then // No need to check OldText <> FText, TextChanged covers it
      begin
        FOnChange(Self);
      end;
      Invalidate;
    end
    else
    begin
      // Even if text didn't change, MaxLength value changed, which might be important.
      // No direct visual change now, but could be in the future.
      // Invalidate; // Optional: if MaxLength had a visual representation
    end;
  end;
end;

procedure TANDMR_CEdit.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    Invalidate; // Text display will change
  end;
end;

procedure TANDMR_CEdit.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    // Keyboard input handling (KeyPress, KeyDown) already checks FReadOnly.
    // No direct visual change, but affects interaction.
    // If ReadOnly is true, caret might be styled differently (e.g. non-blinking or block),
    // but current implementation just prevents input.
    Invalidate; // In case future styling depends on ReadOnly
  end;
end;

procedure TANDMR_CEdit.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Invalidate; // Redraw control
  end;
end;

procedure TANDMR_CEdit.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
    Invalidate; // Redraw control
  end;
end;

procedure TANDMR_CEdit.SetActiveColor(const Value: TColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    Invalidate; // Redraw control if active
  end;
end;

procedure TANDMR_CEdit.SetInactiveColor(const Value: TColor);
begin
  if FInactiveColor <> Value then
  begin
    FInactiveColor := Value;
    Invalidate; // Redraw control if inactive
  end;
end;

procedure TANDMR_CEdit.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate; // Redraw control
  end;
end;

procedure TANDMR_CEdit.SetBorderThickness(const Value: Integer);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Value;
    Invalidate; // Redraw control
  end;
end;

procedure TANDMR_CEdit.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.Paint;
var
  LG: TGPGraphics;
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  PathRectF: TGPRectF;
  LRadiusValue: Single;
  LBorderColorToUse: TColor;
  LBackgroundColorToUse: TColor;
  DrawRect: TRect; // For text
  TextToDisplay: string;
  TextFlags: Cardinal;
  PaddingX: Integer; // This might be replaced or repurposed

  ImageRect: TRect;
  TextRect: TRect;
  ActualImageWidth, ActualImageHeight: Integer;
  AvailableContentHeight: Integer;
begin
  // inherited Paint; // Call if needed, but usually not for full custom paint

  // --- Calculate ImageRect and TextRect ---
  TextRect := ClientRect; // Start with the whole client area
  ImageRect := Rect(0,0,0,0); // Default empty image rect
  ActualImageWidth := 0;
  ActualImageHeight := 0;

  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin
    ActualImageWidth := FImage.Graphic.Width;
    ActualImageHeight := FImage.Graphic.Height;

    if FImagePosition = ipsLeft then
    begin
      ImageRect.Left := ClientRect.Left + FBorderThickness + FImageMargins.Left;
      ImageRect.Right := ImageRect.Left + ActualImageWidth;
      TextRect.Left := ImageRect.Right + FImageMargins.Right;
      TextRect.Right := ClientRect.Right - FBorderThickness;
    end
    else // ipsRight
    begin
      ImageRect.Right := ClientRect.Right - FBorderThickness - FImageMargins.Right;
      ImageRect.Left := ImageRect.Right - ActualImageWidth;
      TextRect.Right := ImageRect.Left - FImageMargins.Left;
      TextRect.Left := ClientRect.Left + FBorderThickness;
    end;

    AvailableContentHeight := ClientRect.Height - FBorderThickness * 2 - FImageMargins.Top - FImageMargins.Bottom;

    case FImageAlignment of
      iavTop:
        ImageRect.Top := ClientRect.Top + FBorderThickness + FImageMargins.Top;
      iavCenter:
        ImageRect.Top := ClientRect.Top + FBorderThickness + FImageMargins.Top +
                         (AvailableContentHeight - ActualImageHeight) div 2;
      iavBottom:
        ImageRect.Top := ClientRect.Bottom - FBorderThickness - FImageMargins.Bottom - ActualImageHeight;
    end;
    ImageRect.Bottom := ImageRect.Top + ActualImageHeight;

    TextRect.Top := ClientRect.Top + FBorderThickness;
    TextRect.Bottom := ClientRect.Bottom - FBorderThickness;

    if TextRect.Right < TextRect.Left then TextRect.Right := TextRect.Left;
    if TextRect.Bottom < TextRect.Top then TextRect.Bottom := TextRect.Top;
    if ImageRect.Right < ImageRect.Left then ImageRect.Left := ImageRect.Right;
    if ImageRect.Bottom < ImageRect.Top then ImageRect.Top := ImageRect.Bottom;
  end
  else // No image visible or assigned
  begin
    TextRect.Left := ClientRect.Left + FBorderThickness;
    TextRect.Top := ClientRect.Top + FBorderThickness;
    TextRect.Right := ClientRect.Right - FBorderThickness;
    TextRect.Bottom := ClientRect.Bottom - FBorderThickness;
  end;
  // --- End Calculate ImageRect and TextRect ---

  Canvas.Lock; // Lock canvas for VCL drawing (text, caret)
  try
    LBackgroundColorToUse := FInactiveColor;
    if csDesigning in ComponentState then
      LBackgroundColorToUse := clWhite;

    if Focused then
      LBorderColorToUse := FActiveColor
    else
      LBorderColorToUse := FBorderColor;

    // Initialize GDI+
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      // Correctly initialize PathRectF using record field assignment
      if FBorderThickness > 0 then
      begin
        PathRectF.X := FBorderThickness / 2.0;
        PathRectF.Y := FBorderThickness / 2.0;
        PathRectF.Width := Self.Width - FBorderThickness;
        PathRectF.Height := Self.Height - FBorderThickness;
      end
      else
      begin
        PathRectF.X := 0.0;
        PathRectF.Y := 0.0;
        PathRectF.Width := Self.Width;
        PathRectF.Height := Self.Height;
      end;

      PathRectF.Width := Max(0.0, PathRectF.Width); // Ensure non-negative float
      PathRectF.Height := Max(0.0, PathRectF.Height); // Ensure non-negative float

      LPath := TGPGraphicsPath.Create;
      try
        // LRadiusValue calculation should use PathRectF.Width / Height
        LRadiusValue := Min(FCornerRadius, Min(PathRectF.Width / 2.0, PathRectF.Height / 2.0));
        LRadiusValue := Max(0.0, LRadiusValue);

        // Call CreateGPRoundedPath with the correctly typed TGPRectF
        CreateGPRoundedPath(LPath, PathRectF, LRadiusValue, FRoundCornerType);

        if LPath.GetPointCount > 0 then
        begin
          // Fill Background
          if LBackgroundColorToUse <> clNone then
          begin
            LBrush := TGPSolidBrush.Create(ColorToARGB(LBackgroundColorToUse));
            try
              LG.FillPath(LBrush, LPath);
            finally
              LBrush.Free;
            end;
          end;

          // Draw Border
          if (FBorderThickness > 0) and (LBorderColorToUse <> clNone) then
          begin
            LPen := TGPPen.Create(ColorToARGB(LBorderColorToUse), FBorderThickness);
            try
              case FBorderStyle of
                psSolid: LPen.SetDashStyle(DashStyleSolid);
                psDash: LPen.SetDashStyle(DashStyleDash);
                psDot: LPen.SetDashStyle(DashStyleDot);
                psDashDot: LPen.SetDashStyle(DashStyleDashDot);
                psDashDotDot: LPen.SetDashStyle(DashStyleDashDotDot);
                psClear: LPen.SetDashStyle(DashStyleSolid); // Effectively no border if color is clNone
              else LPen.SetDashStyle(DashStyleSolid);
              end;
              if FBorderStyle <> psClear then // Only draw if not clear
                LG.DrawPath(LPen, LPath);
            finally
              LPen.Free;
            end;
          end;
        end;
      finally
        LPath.Free;
      end;
    finally
      LG.Free;
    end;

    // --- Draw Image ---
    if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
    begin
      // Ensure ImageRect dimensions are positive before attempting to draw
      if (ImageRect.Width > 0) and (ImageRect.Height > 0) then
      begin
        if FImage.Graphic is TPNGImage then
        begin
          var PngImage: TPNGImage;
          var PngStream: TMemoryStream;
          var GpSourceBitmap: TGPBitmap;
          var Adapter: IStream;
          // Removed redundant inner begin/end block
          PngImage := FImage.Graphic as TPNGImage;
          PngStream := TMemoryStream.Create;
          try
            PngImage.SaveToStream(PngStream);
            PngStream.Position := 0;
            Adapter := TStreamAdapter.Create(PngStream, soReference);
            GpSourceBitmap := TGPBitmap.Create(Adapter);
            try
              if (ImageRect.Width <> GpSourceBitmap.GetWidth()) or (ImageRect.Height <> GpSourceBitmap.GetHeight()) then
                 LG.SetInterpolationMode(InterpolationModeHighQualityBicubic)
              else
                 LG.SetInterpolationMode(InterpolationModeDefault);

              LG.DrawImage(GpSourceBitmap, ImageRect.Left, ImageRect.Top, ImageRect.Width, ImageRect.Height);
            finally
              GpSourceBitmap.Free;
            end;
          finally
            PngStream.Free;
          end;
        end // This end now correctly closes the "if FImage.Graphic is TPNGImage then" block
        else // For non-PNG images (TBitmap, TJPEGImage, etc.)
        begin
          // Use VCL Canvas.StretchDraw for other types.
          // This will be drawn on top of the GDI+ background.
          Canvas.StretchDraw(ImageRect, FImage.Graphic);
        end;
      end;
    end;

    // --- VCL Canvas drawing for Text and Caret ---
    // PaddingX variable is no longer used for this calculation directly.
    // TextRect is the available area for text, including its own padding.

    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear; // Text background is transparent

    var
      PaddedTextRect: TRect; // New variable for clarity
      InternalTextPaddingX: Integer;
      InternalTextPaddingY: Integer;

    InternalTextPaddingX := 4; // Example horizontal padding
    InternalTextPaddingY := 2; // Example vertical padding

    PaddedTextRect := TextRect; // Start with the calculated TextRect
    // Apply internal padding *within* TextRect
    PaddedTextRect.Left := TextRect.Left + InternalTextPaddingX;
    PaddedTextRect.Top := TextRect.Top + InternalTextPaddingY;
    PaddedTextRect.Right := TextRect.Right - InternalTextPaddingX;
    PaddedTextRect.Bottom := TextRect.Bottom - InternalTextPaddingY;

    // Ensure padded rect is not inverted
    if PaddedTextRect.Right < PaddedTextRect.Left then PaddedTextRect.Right := PaddedTextRect.Left;
    if PaddedTextRect.Bottom < PaddedTextRect.Top then PaddedTextRect.Bottom := PaddedTextRect.Top;

    if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then // Don't mask at design-time for usability
    begin
      TextToDisplay := StringOfChar(FPasswordChar, Length(FText));
    end
    else
    begin
      TextToDisplay := FText;
    end;

    // DrawRect is no longer taken from ClientRect here. It's PaddedTextRect.
    // The InflateRect(-2,-2) logic is now replaced by explicit PaddedTextRect calculation.

    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if Length(TextToDisplay) > 0 then
    begin
      // Use PaddedTextRect for DrawText
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), PaddedTextRect, TextFlags);
    end;

    // Draw Caret
    if Focused and FCaretVisible then
    begin
      var CaretXBase: Integer;
      var CaretTop, CaretHeight: Integer;
      var TextBeforeCaretVisible: string;
      var CaretXOffset: Integer; // Declared here as it's specific to caret logic

      if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
        TextBeforeCaretVisible := StringOfChar(FPasswordChar, FCaretPosition)
      else
        TextBeforeCaretVisible := Copy(FText, 1, FCaretPosition);

      CaretXBase := PaddedTextRect.Left; // Base X for caret is the start of padded text area

      CaretHeight := Canvas.TextHeight('Tg'); // Or use Font.Height
      // Center caret vertically within PaddedTextRect
      CaretTop := PaddedTextRect.Top + (PaddedTextRect.Height - CaretHeight) div 2;
      
      CaretXOffset := Canvas.TextWidth(TextBeforeCaretVisible);

      Canvas.Pen.Color := Font.Color;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(CaretXBase + CaretXOffset, CaretTop);
      Canvas.LineTo(CaretXBase + CaretXOffset, CaretTop + CaretHeight);
    end;

  finally
    Canvas.Unlock;
  end;
end;

procedure TANDMR_CEdit.CaretTimerTick(Sender: TObject);
begin
  if Focused then // Only blink if focused
  begin
    FCaretVisible := not FCaretVisible;
    Invalidate; // Repaint to show/hide caret
  end
  else
  begin
    FCaretVisible := False; // Ensure caret is hidden if not focused
    FCaretTimer.Enabled := False;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.CMEnter(var Message: TCMEnter);
begin
  inherited; // Call inherited handler for CM_ENTER
  FCaretVisible := True;
  FCaretTimer.Enabled := True;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  Invalidate;
end;

procedure TANDMR_CEdit.CMExit(var Message: TCMExit);
begin
  inherited; // Call inherited handler for CM_EXIT
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  if Assigned(FOnExit) then
    FOnExit(Self);
  Invalidate;
end;

procedure TANDMR_CEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldText: string;
  Changed: Boolean;
begin
  inherited KeyDown(Key, Shift);
  Changed := False;
  OldText := FText;

  if FReadOnly and not (Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_TAB, VK_RETURN]) then // Allow navigation in ReadOnly
  begin
    if not ( (Key = Ord('C')) and (ssCtrl in Shift) ) then // Allow Ctrl+C
    begin
        Key := 0;
        Exit;
    end;
  end;

  case Key of
    VK_BACK:
      begin
        if FReadOnly then Exit;
        if FCaretPosition > 0 then
        begin
          FText := Copy(FText, 1, FCaretPosition - 1) + Copy(FText, FCaretPosition + 1, MaxInt);
          Dec(FCaretPosition);
          Changed := True;
        end;
      end;
    VK_DELETE:
      begin
        if FReadOnly then Exit;
        if FCaretPosition < Length(FText) then
        begin
          FText := Copy(FText, 1, FCaretPosition) + Copy(FText, FCaretPosition + 2, MaxInt);
          Changed := True;
        end;
      end;
    VK_HOME:
      begin
        FCaretPosition := 0;
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_END:
      begin
        FCaretPosition := Length(FText);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_LEFT:
      begin
        if FCaretPosition > 0 then Dec(FCaretPosition);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_RIGHT:
      begin
        if FCaretPosition < Length(FText) then Inc(FCaretPosition);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
  else
    Exit; // Not a key we handle here, let default processing occur or KeyPress handle it.
  end;

  // If key was handled (i.e., one of the cases above)
  Key := 0; // Mark as handled

  if Changed then
  begin
    FCaretVisible := True; // Make sure caret is visible after action
    if Focused then
    begin
      FCaretTimer.Enabled := False; // Reset blink
      FCaretTimer.Enabled := True;
    end;

    if Assigned(FOnChange) and (OldText <> FText) then // Only trigger if text actually changed
    begin
      FOnChange(Self);
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.KeyPress(var Key: Char);
var
  OldText: string;
begin
  inherited KeyPress(Key);

  if Key = #8 then // Backspace is handled in KeyDown
  begin
    Key := #0; // Prevent system beep for backspace if not handled by KeyDown somehow
    Exit;
  end;

  if FReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  // Handle printable characters (and not control chars like Enter, Tab if KeyDown doesn't catch them first)
  if (Key >= ' ') then // Includes space and all printable chars
  begin
    OldText := FText;
    if (FMaxLength > 0) and (Length(FText) >= FMaxLength) then
    begin
      Key := #0;
      Exit; // MaxLength reached
    end;

    // Insert character at caret position
    if FCaretPosition > Length(FText) then FCaretPosition := Length(FText); // Ensure valid position
    FText := Copy(FText, 1, FCaretPosition) + Key + Copy(FText, FCaretPosition + 1, MaxInt);
    Inc(FCaretPosition);

    FCaretVisible := True; // Make sure caret is visible after typing
    if Focused then
    begin
      FCaretTimer.Enabled := False; // Reset blink
      FCaretTimer.Enabled := True;
    end;

    if Assigned(FOnChange) and (OldText <> FText) then
    begin
      FOnChange(Self);
    end;
    Invalidate;
  end;
  Key := #0; // Mark as handled
end;

procedure TANDMR_CEdit.Click;
begin
  inherited Click;
  // Most logic moved to MouseDown for immediate caret positioning and focus.
end;

procedure TANDMR_CEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  ClickX_RelativeToPaddedText: Integer; // Renamed for clarity
  CurrentWidth: Integer;
  CharWidth: Integer;
  TextToMeasure: string;
  LocalTextRect: TRect;
  PaddedLocalTextRect: TRect;
  ActualImageW: Integer;
  InternalTextPaddingX_Mouse: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    if CanFocus then
    begin
      if not Focused then SetFocus
      else
      begin
        FCaretVisible := True;
        FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
      end;
    end
    else
      Exit;

    // --- Simplified Recalculation of TextRect's X-bounds and Padded X-bounds for Click ---
    InternalTextPaddingX_Mouse := 4; // Must match Paint's InternalTextPaddingX

    LocalTextRect.Left := ClientRect.Left + FBorderThickness;
    LocalTextRect.Right := ClientRect.Right - FBorderThickness;
    // Vertical bounds (Top, Bottom) of LocalTextRect are not strictly needed for this X-based caret positioning,
    // but a full PtInRect check would use them.

    if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
    begin
      ActualImageW := FImage.Graphic.Width;
      if FImagePosition = ipsLeft then
      begin
        LocalTextRect.Left := ClientRect.Left + FBorderThickness + FImageMargins.Left + ActualImageW + FImageMargins.Right;
      end
      else // ipsRight
      begin
        // If image is on the right, TextRect.Right is affected.
        // TextRect.Left remains ClientRect.Left + FBorderThickness.
        LocalTextRect.Right := ClientRect.Right - FBorderThickness - FImageMargins.Right - ActualImageW - FImageMargins.Left;
      end;
    end;

    PaddedLocalTextRect.Left := LocalTextRect.Left + InternalTextPaddingX_Mouse;
    PaddedLocalTextRect.Right := LocalTextRect.Right - InternalTextPaddingX_Mouse;
    // For Y check, PaddedLocalTextRect.Top/Bottom would be:
    // PaddedLocalTextRect.Top := ClientRect.Top + FBorderThickness + InternalTextPaddingY_Mouse; (InternalTextPaddingY_Mouse = 2)
    // PaddedLocalTextRect.Bottom := ClientRect.Bottom - FBorderThickness - InternalTextPaddingY_Mouse;

    // --- End Simplified Recalculation ---

    if X < PaddedLocalTextRect.Left then
    begin
      FCaretPosition := 0;
    end
    else if X >= PaddedLocalTextRect.Right then // Use >= for right boundary
    begin
      FCaretPosition := Length(FText);
    end
    else
    begin
      ClickX_RelativeToPaddedText := X - PaddedLocalTextRect.Left;

      if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
        TextToMeasure := StringOfChar(FPasswordChar, Length(FText))
      else
        TextToMeasure := FText;

      Canvas.Font.Assign(Self.Font);
      CurrentWidth := 0;
      FCaretPosition := 0; // Default to beginning if no characters or calculation fails

      for I := 1 to Length(TextToMeasure) do
      begin
        CharWidth := Canvas.TextWidth(TextToMeasure[I]);
        if ClickX_RelativeToPaddedText < (CurrentWidth + CharWidth div 2) then
        begin
          FCaretPosition := I - 1;
          Break;
        end;
        CurrentWidth := CurrentWidth + CharWidth;
        FCaretPosition := I;
      end;

      // If click is past the center of the last character, ensure caret is at the very end
      if ClickX_RelativeToPaddedText >= CurrentWidth then
          FCaretPosition := Length(TextToMeasure);
    end;
    
    Invalidate;
  end;
end;

end.
