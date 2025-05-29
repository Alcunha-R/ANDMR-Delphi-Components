unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX; // Added ActiveX for IStream

type
  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);

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

    FImagePlacement: TImagePlacement;
    FImageDrawMode: TImageDrawMode;

    FSeparatorVisible: Boolean;
    FSeparatorColor: TColor;
    FSeparatorThickness: Integer;
    FSeparatorPadding: Integer; // Space on each side of the separator line

    // Setter method declarations for new fields
    procedure SetImagePlacement(const Value: TImagePlacement);
    procedure SetImageDrawMode(const Value: TImageDrawMode);
    procedure SetSeparatorVisible(const Value: Boolean);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSeparatorThickness(const Value: Integer);
    procedure SetSeparatorPadding(const Value: Integer);

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
    procedure CalculateLayout(out outImgRect, out outTxtRect, out outSepRect: TRect); virtual;
    procedure Paint; override;
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
    property ImagePlacement: TImagePlacement read FImagePlacement write SetImagePlacement default iplInsideBounds;
    property ImageDrawMode: TImageDrawMode read FImageDrawMode write SetImageDrawMode default idmProportional;

    property SeparatorVisible: Boolean read FSeparatorVisible write SetSeparatorVisible default False;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clGrayText;
    property SeparatorThickness: Integer read FSeparatorThickness write SetSeparatorThickness default 1;
    property SeparatorPadding: Integer read FSeparatorPadding write SetSeparatorPadding default 2;

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

procedure TANDMR_CEdit.CalculateLayout(out outImgRect, out outTxtRect, out outSepRect: TRect);
var
  WorkArea: TRect; // Area inside component borders
  ImgW, ImgH, SepW: Integer;
begin
  WorkArea := Self.ClientRect;
  InflateRect(WorkArea, -FBorderThickness, -FBorderThickness); // Available content area

  // Initialize output rects
  outImgRect := Rect(0,0,0,0);
  outSepRect := Rect(0,0,0,0);
  outTxtRect := WorkArea; // Text initially gets the whole content area

  // Get raw image dimensions
  ImgW := 0; ImgH := 0;
  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin
    ImgW := FImage.Graphic.Width;
    ImgH := FImage.Graphic.Height;
  end;

  // Get raw separator width
  SepW := 0;
  if FSeparatorVisible and (FSeparatorThickness > 0) then
  begin
    SepW := FSeparatorThickness;
  end;

  // --- Horizontal Layout ---
  if FImageVisible and (ImgW > 0) then
  begin
    if FImagePosition = ipsLeft then
    begin
      outImgRect.Left := WorkArea.Left + FImageMargins.Left;
      outImgRect.Right := outImgRect.Left + ImgW;
      outTxtRect.Left := outImgRect.Right + FImageMargins.Right;

      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Left := outTxtRect.Left + FSeparatorPadding;
        outSepRect.Right := outSepRect.Left + SepW;
        outTxtRect.Left := outSepRect.Right + FSeparatorPadding;
      end;
    end
    else // ipsRight (Image on Right)
    begin
      outImgRect.Right := WorkArea.Right - FImageMargins.Right;
      outImgRect.Left := outImgRect.Right - ImgW;
      outTxtRect.Right := outImgRect.Left - FImageMargins.Left;

      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Right := outTxtRect.Right - FSeparatorPadding;
        outSepRect.Left := outSepRect.Right - SepW;
        outTxtRect.Right := outSepRect.Left - FSeparatorPadding;
      end;
    end;
  end
  else if FSeparatorVisible and (SepW > 0) then // No image, but Separator visible
  begin
    // Example: Separator on left if no image (can be configured later)
    outSepRect.Left := WorkArea.Left + FSeparatorPadding;
    outSepRect.Right := outSepRect.Left + SepW;
    outTxtRect.Left := outSepRect.Right + FSeparatorPadding;
  end;

  // --- Vertical Layout ---
  if FImageVisible and (ImgW > 0) then
  begin
    var AvailHForImgLayout: Integer;
    AvailHForImgLayout := WorkArea.Height - FImageMargins.Top - FImageMargins.Bottom;
    AvailHForImgLayout := Max(0, AvailHForImgLayout);

    case FImageAlignment of
      iavTop:    outImgRect.Top := WorkArea.Top + FImageMargins.Top;
      iavCenter: outImgRect.Top := WorkArea.Top + FImageMargins.Top + (AvailHForImgLayout - ImgH) div 2;
      iavBottom: outImgRect.Top := WorkArea.Bottom - FImageMargins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;

    if outImgRect.Top < WorkArea.Top + FImageMargins.Top then
       outImgRect.Top := WorkArea.Top + FImageMargins.Top;
    if outImgRect.Bottom > WorkArea.Bottom - FImageMargins.Bottom then
       outImgRect.Bottom := WorkArea.Bottom - FImageMargins.Bottom;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  end;

  outTxtRect.Top := WorkArea.Top;
  outTxtRect.Bottom := WorkArea.Bottom;
  if FSeparatorVisible and (SepW > 0) then
  begin
    outSepRect.Top := WorkArea.Top;
    outSepRect.Bottom := WorkArea.Bottom;
  end;

  // Final safety checks
  if outTxtRect.Left < WorkArea.Left then outTxtRect.Left := WorkArea.Left;
  if outTxtRect.Right > WorkArea.Right then outTxtRect.Right := WorkArea.Right;
  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;

  if outImgRect.Left < WorkArea.Left then outImgRect.Left := WorkArea.Left;
  if outImgRect.Right > WorkArea.Right then outImgRect.Right := WorkArea.Right;
  if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
  
  if outSepRect.Left < WorkArea.Left then outSepRect.Left := WorkArea.Left;
  if outSepRect.Right > WorkArea.Right then outSepRect.Right := WorkArea.Right;
  if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left;

  if outTxtRect.Top < WorkArea.Top then outTxtRect.Top := WorkArea.Top;
  if outTxtRect.Bottom > WorkArea.Bottom then outTxtRect.Bottom := WorkArea.Bottom;
  if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;

  if outImgRect.Top < WorkArea.Top then outImgRect.Top := WorkArea.Top;
  if outImgRect.Bottom > WorkArea.Bottom then outImgRect.Bottom := WorkArea.Bottom;
  if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;

  if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top;
  if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
  if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
end;

procedure TANDMR_CEdit.SetImagePlacement(const Value: TImagePlacement);
begin
  if FImagePlacement <> Value then
  begin
    FImagePlacement := Value;
    Invalidate; // Major layout change
  end;
end;

procedure TANDMR_CEdit.SetImageDrawMode(const Value: TImageDrawMode);
begin
  if FImageDrawMode <> Value then
  begin
    FImageDrawMode := Value;
    Invalidate; // Image drawing will change
  end;
end;

procedure TANDMR_CEdit.SetSeparatorVisible(const Value: Boolean);
begin
  if FSeparatorVisible <> Value then
  begin
    FSeparatorVisible := Value;
    Invalidate; // Layout and repaint needed
  end;
end;

procedure TANDMR_CEdit.SetSeparatorColor(const Value: TColor);
begin
  if FSeparatorColor <> Value then
  begin
    FSeparatorColor := Value;
    if FSeparatorVisible then Invalidate; // Repaint if separator is visible
  end;
end;

procedure TANDMR_CEdit.SetSeparatorThickness(const Value: Integer);
var
  ValidThickness: Integer;
begin
  ValidThickness := Max(0, Value); // Ensure non-negative
  if FSeparatorThickness <> ValidThickness then
  begin
    FSeparatorThickness := ValidThickness;
    if FSeparatorVisible then Invalidate; // Layout and repaint if separator is visible
  end;
end;

procedure TANDMR_CEdit.SetSeparatorPadding(const Value: Integer);
var
  ValidPadding: Integer;
begin
  ValidPadding := Max(0, Value); // Ensure non-negative
  if FSeparatorPadding <> ValidPadding then
  begin
    FSeparatorPadding := ValidPadding;
    if FSeparatorVisible then Invalidate; // Layout and repaint if separator is visible
  end;
end;

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

  // Initialize new advanced image and separator fields
  FImagePlacement := iplInsideBounds;     // Matches property default
  FImageDrawMode := idmProportional;    // Matches property default

  FSeparatorVisible := False;           // Matches property default
  FSeparatorColor := clGrayText;        // Matches property default
  FSeparatorThickness := 1;             // Matches property default
  FSeparatorPadding := 2;               // Matches property default

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
  // DrawRect: TRect; // Replaced by PaddedTextRect for text drawing
  TextToDisplay: string;
  TextFlags: Cardinal;
  // PaddingX: Integer; // No longer used directly here

  outImgRect, outTxtRect, outSepRect: TRect; // Output from CalculateLayout
  // ActualImageWidth, ActualImageHeight: Integer; // Calculated within CalculateLayout if needed, or use FImage.Graphic
  // AvailableContentHeight: Integer; // Calculated within CalculateLayout

  // Variables for image drawing modes
  DrawImageRect: TRect;
  GraphicW, GraphicH: Integer;
  rRatio, rRectRatio: Double;

begin
  // Call CalculateLayout to get the rects
  CalculateLayout(outImgRect, outTxtRect, outSepRect);

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
      // LG.Free; // Moved to the outer try-finally block that wraps all GDI+ operations
    // end; // Original end of GDI+ background/border try-finally block

    // --- Draw Image (GDI+ part for PNGs, must be within LG scope) ---
    if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
    begin
      if (outImgRect.Width > 0) and (outImgRect.Height > 0) then
      begin
        GraphicW := FImage.Graphic.Width;
        GraphicH := FImage.Graphic.Height;

        if (GraphicW > 0) and (GraphicH > 0) then
        begin
          case FImageDrawMode of
            idmStretch: DrawImageRect := outImgRect;
            idmProportional:
              begin
                rRatio := GraphicW / GraphicH;
                if outImgRect.Height = 0 then rRectRatio := MaxDouble
                else rRectRatio := outImgRect.Width / outImgRect.Height;
                if rRectRatio > rRatio then
                begin
                  DrawImageRect.Height := outImgRect.Height;
                  DrawImageRect.Width := Round(outImgRect.Height * rRatio);
                end
                else
                begin
                  DrawImageRect.Width := outImgRect.Width;
                  if rRatio = 0 then DrawImageRect.Height := 0
                  else DrawImageRect.Height := Round(outImgRect.Width / rRatio);
                end;
                DrawImageRect.Left := outImgRect.Left + (outImgRect.Width - DrawImageRect.Width) div 2;
                DrawImageRect.Top := outImgRect.Top + (outImgRect.Height - DrawImageRect.Height) div 2;
                DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
                DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
              end;
            idmNormal:
              begin
                DrawImageRect.Width := GraphicW;
                DrawImageRect.Height := GraphicH;
                DrawImageRect.Left := outImgRect.Left + (outImgRect.Width - GraphicW) div 2;
                DrawImageRect.Top := outImgRect.Top + (outImgRect.Height - GraphicH) div 2;
                DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
                DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
              end;
          else DrawImageRect := outImgRect; // Default to stretch
          end;

          if (DrawImageRect.Width > 0) and (DrawImageRect.Height > 0) then
          begin
            if FImage.Graphic is TPNGImage then // PNG drawing uses LG
            begin
              var PngImage: TPNGImage;
              var PngStream: TMemoryStream;
              var GpSourceBitmap: TGPBitmap;
              var Adapter: IStream;
              PngImage := FImage.Graphic as TPNGImage;
              PngStream := TMemoryStream.Create;
              try
                PngImage.SaveToStream(PngStream);
                PngStream.Position := 0;
                Adapter := TStreamAdapter.Create(PngStream, soReference);
                GpSourceBitmap := TGPBitmap.Create(Adapter);
                try
                  if (DrawImageRect.Width <> GpSourceBitmap.GetWidth()) or (DrawImageRect.Height <> GpSourceBitmap.GetHeight()) then
                     LG.SetInterpolationMode(InterpolationModeHighQualityBicubic)
                  else
                     LG.SetInterpolationMode(InterpolationModeDefault);
                  LG.DrawImage(GpSourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height);
                finally
                  GpSourceBitmap.Free;
                end;
              finally
                PngStream.Free;
              end;
            end;
            // Non-PNG image drawing will be handled after LG is freed, using VCL Canvas
          end;
        end;
      end;
    end;

  finally // This finally is for the LG instance
    if Assigned(LG) then LG.Free;
  end; // End of GDI+ operations scope (LG.Free)

  // --- VCL Canvas Drawing Section (Non-PNG Image, Separator, Text, Caret) ---

  // Draw Non-PNG Image (if applicable and not drawn by GDI+)
  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin
    if not (FImage.Graphic is TPNGImage) then // Only if not already drawn as PNG
    begin
      if (outImgRect.Width > 0) and (outImgRect.Height > 0) then // Re-check outImgRect for context
      begin
        GraphicW := FImage.Graphic.Width; // Recalculate GraphicW/H if needed, or use from above if safe
        GraphicH := FImage.Graphic.Height;
        if (GraphicW > 0) and (GraphicH > 0) then
        begin
          // Recalculate DrawImageRect for non-PNG, similar to above
          // This re-calculation is necessary as DrawImageRect might not be in scope here
          // or to ensure clarity. For simplicity, we repeat the case logic.
          var VCLDrawImageRect: TRect; // Use a new variable for VCL drawing rect
          case FImageDrawMode of
            idmStretch: VCLDrawImageRect := outImgRect;
            idmProportional:
              begin
                rRatio := GraphicW / GraphicH;
                if outImgRect.Height = 0 then rRectRatio := MaxDouble
                else rRectRatio := outImgRect.Width / outImgRect.Height;
                if rRectRatio > rRatio then
                begin
                  VCLDrawImageRect.Height := outImgRect.Height;
                  VCLDrawImageRect.Width := Round(outImgRect.Height * rRatio);
                end
                else
                begin
                  VCLDrawImageRect.Width := outImgRect.Width;
                  if rRatio = 0 then VCLDrawImageRect.Height := 0
                  else VCLDrawImageRect.Height := Round(outImgRect.Width / rRatio);
                end;
                VCLDrawImageRect.Left := outImgRect.Left + (outImgRect.Width - VCLDrawImageRect.Width) div 2;
                VCLDrawImageRect.Top := outImgRect.Top + (outImgRect.Height - VCLDrawImageRect.Height) div 2;
                VCLDrawImageRect.Right := VCLDrawImageRect.Left + VCLDrawImageRect.Width;
                VCLDrawImageRect.Bottom := VCLDrawImageRect.Top + VCLDrawImageRect.Height;
              end;
            idmNormal:
              begin
                VCLDrawImageRect.Width := GraphicW;
                VCLDrawImageRect.Height := GraphicH;
                VCLDrawImageRect.Left := outImgRect.Left + (outImgRect.Width - GraphicW) div 2;
                VCLDrawImageRect.Top := outImgRect.Top + (outImgRect.Height - GraphicH) div 2;
                VCLDrawImageRect.Right := VCLDrawImageRect.Left + VCLDrawImageRect.Width;
                VCLDrawImageRect.Bottom := VCLDrawImageRect.Top + VCLDrawImageRect.Height;
              end;
          else VCLDrawImageRect := outImgRect; // Default
          end;

          if (VCLDrawImageRect.Width > 0) and (VCLDrawImageRect.Height > 0) then
          begin
            if FImageDrawMode = idmNormal then
               Canvas.Draw(VCLDrawImageRect.Left, VCLDrawImageRect.Top, FImage.Graphic)
            else // idmStretch, idmProportional
               Canvas.StretchDraw(VCLDrawImageRect, FImage.Graphic);
          end;
        end;
      end;
    end;
  end;

  // --- Draw Separator ---
    if FSeparatorVisible and (FSeparatorThickness > 0) and (outSepRect.Width > 0) and (outSepRect.Height > 0) then
    begin
      // The separator line is drawn in the middle of outSepRect horizontally
      var LineX: Integer;
      LineX := outSepRect.Left + outSepRect.Width div 2;

      Canvas.Pen.Color := FSeparatorColor;
      Canvas.Pen.Width := FSeparatorThickness;
      Canvas.Pen.Style := psSolid; // Or make this a property later FSeparatorStyle

      Canvas.MoveTo(LineX, outSepRect.Top);
      Canvas.LineTo(LineX, outSepRect.Bottom);
    end;

    // --- VCL Canvas drawing for Text and Caret ---
    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;

    var
      PaddedTextRect: TRect;
      InternalTextPaddingX: Integer;
      InternalTextPaddingY: Integer;

    InternalTextPaddingX := 4;
    InternalTextPaddingY := 2;

    PaddedTextRect := outTxtRect; // Use outTxtRect from CalculateLayout
    PaddedTextRect.Left := outTxtRect.Left + InternalTextPaddingX;
    PaddedTextRect.Top := outTxtRect.Top + InternalTextPaddingY;
    PaddedTextRect.Right := outTxtRect.Right - InternalTextPaddingX;
    PaddedTextRect.Bottom := outTxtRect.Bottom - InternalTextPaddingY;

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
  ClickX_RelativeToPaddedText: Integer;
  CurrentWidth: Integer;
  CharWidth: Integer;
  TextToMeasure: string;
  LayoutImgRect, LayoutTxtRect, LayoutSepRect: TRect; // To store results from CalculateLayout
  PaddedTextClickArea: TRect; // The actual clickable area for text
  InternalTextPaddingX_Mouse, InternalTextPaddingY_Mouse: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  Self.CalculateLayout(LayoutImgRect, LayoutTxtRect, LayoutSepRect); // Get current layout

  // Define internal padding (must match Paint's PaddedTextRect calculation)
  InternalTextPaddingX_Mouse := 4; // Matches Paint's InternalTextPaddingX
  InternalTextPaddingY_Mouse := 2; // Matches Paint's InternalTextPaddingY

  // Calculate the actual clickable area for text based on LayoutTxtRect and padding
  PaddedTextClickArea := LayoutTxtRect;
  PaddedTextClickArea.Left := LayoutTxtRect.Left + InternalTextPaddingX_Mouse;
  PaddedTextClickArea.Top := LayoutTxtRect.Top + InternalTextPaddingY_Mouse;
  PaddedTextClickArea.Right := LayoutTxtRect.Right - InternalTextPaddingX_Mouse;
  PaddedTextClickArea.Bottom := LayoutTxtRect.Bottom - InternalTextPaddingY_Mouse;

  // Ensure padded rect is not inverted
  if PaddedTextClickArea.Right < PaddedTextClickArea.Left then PaddedTextClickArea.Right := PaddedTextClickArea.Left;
  if PaddedTextClickArea.Bottom < PaddedTextClickArea.Top then PaddedTextClickArea.Bottom := PaddedTextClickArea.Top;

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

    // --- Caret positioning logic using PaddedTextClickArea ---
    // Check if click is within the vertical bounds of the PaddedTextClickArea first
    if not PtInRect(PaddedTextClickArea, Point(X,Y)) then
    begin
      // Click is outside the padded text area vertically.
      // Decide behavior: either do nothing, or set caret to beginning/end based on X.
      // For simplicity, if Y is outside, but X is near, one might still want to set caret.
      // Let's refine: if Y is outside, set to 0 if X is left, Length(FText) if X is right of overall TextRect.
      if X < PaddedTextClickArea.Left then FCaretPosition := 0
      else if X >= PaddedTextClickArea.Right then FCaretPosition := Length(FText)
      else begin // Click X is within horizontal bounds, but Y is outside. Set to nearest end.
        if Y < PaddedTextClickArea.Top then FCaretPosition := 0 // Or based on X proximity to start/end
        else FCaretPosition := Length(FText); // Y is below
      end;
    end
    else if X < PaddedTextClickArea.Left then // X is to the left, Y is inside
    begin
        FCaretPosition := 0;
    end
    else if X >= PaddedTextClickArea.Right then // X is to the right (or on edge), Y is inside
    begin
        FCaretPosition := Length(FText);
    end
    else // Click is within the horizontal and vertical text area
    begin
        ClickX_RelativeToPaddedText := X - PaddedTextClickArea.Left;

        if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
          TextToMeasure := StringOfChar(FPasswordChar, Length(FText))
        else
          TextToMeasure := FText;

        Canvas.Font.Assign(Self.Font);
        CurrentWidth := 0;
        FCaretPosition := 0;

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

        if ClickX_RelativeToPaddedText >= CurrentWidth then
            FCaretPosition := Length(TextToMeasure);
    end;
    
    Invalidate;
  end;
end;

end.
