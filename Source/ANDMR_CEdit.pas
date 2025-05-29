unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math; // Added GDI+ and Math

type
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
  PathRectF: TGPRectF; // Renamed from LRectF for clarity
  LRadiusValue: Single;
  LBorderColorToUse: TColor;
  LBackgroundColorToUse: TColor;
  DrawRect: TRect; // For text
  TextToDisplay: string;
  TextFlags: Cardinal;
  PaddingX: Integer;
begin
  // inherited Paint; // Call if needed, but usually not for full custom paint

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

      // Correctly initialize PathRectF as TGPRectF
      if FBorderThickness > 0 then
        PathRectF := TGPRectF.Create(FBorderThickness / 2.0, FBorderThickness / 2.0,
                                   Self.Width - FBorderThickness, Self.Height - FBorderThickness)
      else
        PathRectF := TGPRectF.Create(0.0, 0.0, Self.Width, Self.Height);

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

    // --- VCL Canvas drawing for Text and Caret ---
    PaddingX := FBorderThickness + 4; // Increased fixed padding

    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear; // Text background is transparent

    if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then // Don't mask at design-time for usability
    begin
      TextToDisplay := StringOfChar(FPasswordChar, Length(FText));
    end
    else
    begin
      TextToDisplay := FText;
    end;

    DrawRect := ClientRect;
    // Adjust DrawRect by padding, considering border thickness
    InflateRect(DrawRect, -PaddingX, -(FBorderThickness + 2));


    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if Length(TextToDisplay) > 0 then
    begin
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), DrawRect, TextFlags);
    end;

    // Draw Caret
    if Focused and FCaretVisible then
    begin
      var CaretXBase: Integer;
      var CaretTop, CaretHeight: Integer;
      var TempTextRect: TRect;
      var TextBeforeCaretVisible: string; // This is the string used for measuring caret position
      var CaretXOffset: Integer;

      if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
      begin
        TextBeforeCaretVisible := StringOfChar(FPasswordChar, FCaretPosition);
      end
      else
      begin
        TextBeforeCaretVisible := Copy(FText, 1, FCaretPosition);
      end;

      CaretXBase := DrawRect.Left;
      TempTextRect := DrawRect;

      CaretHeight := Canvas.TextHeight('Tg');
      CaretTop := TempTextRect.Top + (TempTextRect.Height - CaretHeight) div 2;
      
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

procedure TANDMR_CEdit.Enter;
begin
  inherited Enter;
  FCaretVisible := True;
  FCaretTimer.Enabled := True;
  // FCaretPosition := Length(FText); // Optionally set caret to end, or preserve. Current SetText sets it.
  if Assigned(FOnEnter) then FOnEnter(Self);
  Invalidate;
end;

procedure TANDMR_CEdit.Exit;
begin
  inherited Exit;
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  if Assigned(FOnExit) then FOnExit(Self);
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
  ClickX: Integer;
  CurrentWidth: Integer;
  CharWidth: Integer;
  TextToMeasure: string;
  // TextRect: TRect; // Not directly needed as Paint's DrawRect calculation is specific
  PaddingXValue: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    if CanFocus then // Check if control can receive focus
    begin
      if not Focused then
        SetFocus // This will call Enter, which handles caret timer etc.
      else
      begin // Already focused, ensure caret is responsive
        FCaretVisible := True;
        FCaretTimer.Enabled := False;
        FCaretTimer.Enabled := True;
      end;
    end
    else // Cannot focus
    begin
      Exit; // Don't process caret positioning if cannot focus
    end;

    // Determine text used for measurement (respecting PasswordChar)
    if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
      TextToMeasure := StringOfChar(FPasswordChar, Length(FText))
    else
      TextToMeasure := FText;

    // Calculate the effective text drawing area's left boundary (PaddingX)
    // This must match the padding used in the Paint method for text rendering.
    PaddingXValue := FBorderThickness + 4; // As used in Paint's InflateRect for DrawRect.Left

    // Adjust click X coordinate relative to the text area
    ClickX := X - PaddingXValue;

    CurrentWidth := 0;
    FCaretPosition := 0; // Default to beginning

    // Iterate through text to find character at click position
    Canvas.Font.Assign(Self.Font); // Ensure canvas font is set for measurement

    for I := 1 to Length(TextToMeasure) do
    begin
      CharWidth := Canvas.TextWidth(TextToMeasure[I]);
      if ClickX < (CurrentWidth + CharWidth div 2) then // Click is closer to left of current char
      begin
        FCaretPosition := I - 1;
        Break;
      end;
      CurrentWidth := CurrentWidth + CharWidth;
      FCaretPosition := I; // If loop finishes, caret is at the end
    end;
    
    // If ClickX is beyond the full text width (or text is empty and click is in padded area), ensure caret is at the very end
    // Or if click was before the first character's midpoint (e.g. ClickX < 0 after adjustment)
    if ClickX > CurrentWidth then
        FCaretPosition := Length(TextToMeasure)
    else if ClickX < 0 then // Click was in left padding before any text
        FCaretPosition := 0;


    Invalidate; // Repaint to show new caret position
  end;
end;

end.
