unit ANDMR_CScrollBar;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Winapi.Windows,
  Vcl.ExtCtrls,
  Winapi.Messages,
  System.UITypes,
  System.Types, // For TPointF, TRectF if used directly
  ANDMR_ComponentUtils, // Assumed to contain TBorderSettings, THoverSettings, helper functions
  Winapi.GDIPOBJ, // Contains TGPRectF, TGPPointF, TGPColor
  Winapi.GDIPAPI,
  System.Math; // Added for Max function, though SysUtils also has it. Good practice.

type
  TANDMR_ScrollBarOrientation = (soHorizontal, soVertical);
  TANDMR_CScrollBarStyle = (sbsSolid, sbsGradient, sbsModern, sbsFlatStyle, sbsWindows10, sbsMacOS);

  TANDMR_CScrollBar = class(TCustomControl)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FOrientation: TANDMR_ScrollBarOrientation;
    FStyle: TANDMR_CScrollBarStyle;
    FThumbSettings: TBorderSettings;
    FTrackSettings: TBorderSettings;
    FHoverSettings: THoverSettings;
    FOnChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FThumbRect: TRect; // Calculated thumb rectangle
    FIsThumbHovered: Boolean;
    FIsTrackHovered: Boolean;
    FIsDraggingThumb: Boolean;
    FDragOffset: Integer; // Offset of mouse click from thumb's top/left

    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetOrientation(Value: TANDMR_ScrollBarOrientation);
    procedure SetStyle(Value: TANDMR_CScrollBarStyle);
    procedure SetThumbSettings(Value: TBorderSettings);
    procedure SetTrackSettings(Value: TBorderSettings);
    procedure SetHoverSettings(Value: THoverSettings);
    procedure SettingsChanged(Sender: TObject);
    procedure UpdateThumbRect;

  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Resize; override; // Added to ensure thumb rect is updated on resize

    procedure DoChange; virtual;
    procedure DoScroll; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParams(APosition, AMin, AMax: Integer);
    function IsHorizontal: Boolean;
    function IsVertical: Boolean;
    function GetThumbTrackArea: TRect;

  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font; // Though not directly used for drawing text, standard to publish
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True; // Explicitly True for focus
    property Visible;

    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 10;
    property Orientation: TANDMR_ScrollBarOrientation read FOrientation write SetOrientation default soVertical;
    property Style: TANDMR_CScrollBarStyle read FStyle write SetStyle default sbsSolid;
    property ThumbSettings: TBorderSettings read FThumbSettings write SetThumbSettings;
    property TrackSettings: TBorderSettings read FTrackSettings write SetTrackSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter; // Already handled by CM_MOUSEENTER, but standard to publish
    property OnMouseLeave; // Already handled by CM_MOUSELEAVE, but standard to publish
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CScrollBar]);
end;

{ TANDMR_CScrollBar }

constructor TANDMR_CScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks, csReplicatable, csNeedsBorderPaint]; // csNeedsBorderPaint for themes if needed
  Width := 17; // Default width for vertical scrollbar
  Height := 100; // Default height for vertical scrollbar
  TabStop := True; // Ensure TabStop is true by default for focus

  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FSmallChange := 1;
  FLargeChange := 10;
  FOrientation := soVertical;
  FStyle := sbsSolid; // Initial style, will be properly set by SetStyle

  FThumbSettings := TBorderSettings.Create;
  FThumbSettings.OnChange := SettingsChanged;
  FTrackSettings := TBorderSettings.Create;
  FTrackSettings.OnChange := SettingsChanged;
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := SettingsChanged;
  FHoverSettings.OnAnimationProgress := SettingsChanged;

  FIsThumbHovered := False;
  FIsTrackHovered := False;
  FIsDraggingThumb := False;
  FDragOffset := 0;

  // Initialize with a default style. This will call UpdateThumbRect and Invalidate.
  SetStyle(sbsSolid);
end;

destructor TANDMR_CScrollBar.Destroy;
begin
  FThumbSettings.Free;
  FTrackSettings.Free;
  FHoverSettings.Free;
  inherited Destroy;
end;

procedure TANDMR_CScrollBar.Paint;

  // Helper function to convert VCL TColor to GDI+ TGPColor
  function VclToGPColor(AColor: TColor): TGPColor;
  var
    LColorRGB: Longint; // For ColorToRGB result
    Alpha, R, G, B: Byte;
  begin
    // Check if the TColor value has the high byte set, indicating it might be an ARGB value
    if (AColor and TColor($FF000000)) <> 0 then
    begin
      // Assuming TColor is storing ARGB as $AARRGGBB
      Alpha := Byte((AColor shr 24) and $FF);
      R := Byte((AColor shr 16) and $FF);
      G := Byte((AColor shr 8) and $FF);
      B := Byte(AColor and $FF);
      Result := (Alpha shl 24) or (R shl 16) or (G shl 8) or B;
    end
    else
    begin
      // Standard VCL color (RGB or system color index), assume opaque (Alpha = 255)
      LColorRGB := ColorToRGB(AColor); // Converts to COLORREF ($00BBGGRR format)
      Alpha := 255;
      R := GetRValue(LColorRGB); // Extracts Red from COLORREF
      G := GetGValue(LColorRGB); // Extracts Green from COLORREF
      B := GetBValue(LColorRGB); // Extracts Blue from COLORREF
      Result := (Alpha shl 24) or (R shl 16) or (G shl 8) or B;
    end;
  end;

var
  LGraphics: TGPGraphics;
  LClientRect: TRect;
  LTrackRect: TRect;
  LThumbRectCalculated: TRect;
  LTrackBrush, LThumbBrush: TGPBrush;
  LTrackPen, LThumbPen: TGPPen;
  LPath: TGPGraphicsPath;
  LStatus: TStatus;
  LTrackColor, LThumbColor, LTrackBorderColor, LThumbBorderColor: TColor;
  LBlendColor: TColor;
  LFocusRect: TRect;
  GradStartPoint, GradEndPoint: TGPPointF;
  CurrentThumbRadius: Integer;
  CalculatedPillRadius: Integer; // Added for clarity
  TempTrackGPRect, TempThumbGPRect: TGPRectF; // Variables for TGPRectF instances
begin
  inherited Paint; // Clears the canvas if csOpaque is set.

  LGraphics := TGPGraphics.Create(Canvas.Handle);
  try
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    LClientRect := GetClientRect;
    LTrackRect := GetThumbTrackArea; // This is ClientRect for now

    // --- Resolve Base Colors ---
    LTrackColor := FTrackSettings.BackgroundColor;
    LTrackBorderColor := FTrackSettings.Color;
    LThumbColor := FThumbSettings.BackgroundColor;
    LThumbBorderColor := FThumbSettings.Color;

    // --- Adjust colors for Disabled state ---
    if not Enabled then
    begin
      LTrackColor := BlendColors(LTrackColor, clGray, 0.65); // Assuming BlendColors is in utils
      LTrackBorderColor := BlendColors(LTrackBorderColor, clGray, 0.45);
      LThumbColor := BlendColors(LThumbColor, clGray, 0.65);
      LThumbBorderColor := BlendColors(LThumbBorderColor, clGray, 0.45);
    end;

    // --- 1. Draw Track ---
    if FTrackSettings.Visible and (LTrackRect.Width > 0) and (LTrackRect.Height > 0) then
    begin
      LPath := TGPGraphicsPath.Create;
      try
        // Initialize TGPRectF by assigning fields directly
        TempTrackGPRect.X := LTrackRect.Left;
        TempTrackGPRect.Y := LTrackRect.Top;
        TempTrackGPRect.Width := LTrackRect.Width;
        TempTrackGPRect.Height := LTrackRect.Height;

        // Assuming CreateGPRoundedPath is in utils
        CreateGPRoundedPath(LPath, TempTrackGPRect, FTrackSettings.CornerRadius, FTrackSettings.RoundCornerType);

        if (FTrackSettings.Thickness > 0) and (LTrackBorderColor <> clNone) then
        begin
          LTrackPen := TGPPen.Create(VclToGPColor(LTrackBorderColor), FTrackSettings.Thickness);
          try
            LGraphics.DrawPath(LTrackPen, LPath);
          finally
            LTrackPen.Free;
          end;
        end;
      finally
        LPath.Free;
      end;
    end;

    // --- 2. Calculate and Draw Thumb ---
    LThumbRectCalculated := FThumbRect; // Use the member field that's kept updated

    // Apply hover and drag effects to Thumb colors if enabled
    if Enabled then
    begin
      if FIsDraggingThumb then
      begin
//        if FHoverSettings.InteractionBackgroundColor <> clNone then
//          LThumbColor := BlendColors(LThumbColor, FHoverSettings.InteractionBackgroundColor, FHoverSettings.InteractionOpacity / 255.0);
//        if FHoverSettings.InteractionBorderColor <> clNone then
//          LThumbBorderColor := FHoverSettings.InteractionBorderColor;
      end
      else if FIsThumbHovered and FHoverSettings.Enabled then
      begin
        if FHoverSettings.HoverEffect = heFade then // Assuming heFade is an enum in utils
          LThumbColor := BlendColors(LThumbColor, FHoverSettings.BackgroundColor, FHoverSettings.CurrentAnimationValue / 255.0)
        else // heNone or other types, use fixed opacity
//          LThumbColor := BlendColors(LThumbColor, FHoverSettings.BackgroundColor, FHoverSettings.Opacity / 255.0);

        if FHoverSettings.BorderColor <> clNone then
          LThumbBorderColor := FHoverSettings.BorderColor;
      end;
    end;

    if FThumbSettings.Visible and (LThumbRectCalculated.Width > 0) and (LThumbRectCalculated.Height > 0) then
    begin
      LPath := TGPGraphicsPath.Create;
      try
        CurrentThumbRadius := FThumbSettings.CornerRadius;
        case FStyle of
          sbsModern, sbsWindows10, sbsMacOS:
            begin
              if IsHorizontal then
                CalculatedPillRadius := LThumbRectCalculated.Height div 2 - 1
              else
                CalculatedPillRadius := LThumbRectCalculated.Width div 2 - 1;
              // Corrected: Replace Max with if statement
              if CalculatedPillRadius > CurrentThumbRadius then
                CurrentThumbRadius := CalculatedPillRadius;
            end;
          sbsFlatStyle:
            CurrentThumbRadius := 0;
        end;
        // Corrected: Replace Max with if statement
        if CurrentThumbRadius < 0 then
           CurrentThumbRadius := 0; // Ensures CurrentThumbRadius is not negative

        // Initialize TGPRectF by assigning fields directly
        TempThumbGPRect.X := LThumbRectCalculated.Left;
        TempThumbGPRect.Y := LThumbRectCalculated.Top;
        TempThumbGPRect.Width := LThumbRectCalculated.Width;
        TempThumbGPRect.Height := LThumbRectCalculated.Height;

        CreateGPRoundedPath(LPath, TempThumbGPRect, CurrentThumbRadius, FThumbSettings.RoundCornerType);

        if LThumbColor <> clNone then
        begin
//          if (FStyle = sbsGradient) and FThumbSettings.GradientEnabled and (FThumbSettings.GradientStartColor <> clNone) and (FThumbSettings.GradientEndColor <> clNone) and Enabled then
//          begin
//            if FThumbSettings.GradientType = gtLinearHorizontal then
//            begin
//              // Initialize TGPPointF by assigning fields
//              GradStartPoint.X := LThumbRectCalculated.Left;
//              GradStartPoint.Y := LThumbRectCalculated.Top;
//              GradEndPoint.X := LThumbRectCalculated.Right;
//              GradEndPoint.Y := LThumbRectCalculated.Top;
//            end
//            else // gtLinearVertical or default
//            begin
//              // Initialize TGPPointF by assigning fields
//              GradStartPoint.X := LThumbRectCalculated.Left;
//              GradStartPoint.Y := LThumbRectCalculated.Top;
//              GradEndPoint.X := LThumbRectCalculated.Left;
//              GradEndPoint.Y := LThumbRectCalculated.Bottom;
//            end;
//            LThumbBrush := TGPLinearGradientBrush.Create(GradStartPoint, GradEndPoint,
//                                                       VclToGPColor(FThumbSettings.GradientStartColor), VclToGPColor(FThumbSettings.GradientEndColor));
//          end
//          else
//          begin
//            LThumbBrush := TGPSolidBrush.Create(VclToGPColor(LThumbColor));
//          end;
//          try
//            LGraphics.FillPath(LThumbBrush, LPath);
//          finally
//            LThumbBrush.Free;
//          end;
        end;

        if (FThumbSettings.Thickness > 0) and (LThumbBorderColor <> clNone) then
        begin
          LThumbPen := TGPPen.Create(VclToGPColor(LThumbBorderColor), FThumbSettings.Thickness);
          try
            LGraphics.DrawPath(LThumbPen, LPath);
          finally
            LThumbPen.Free;
          end;
        end;
      finally
        LPath.Free;
      end;
    end;

    // --- 3. Draw Focus Rectangle ---
    if Focused and TabStop then // ShowFocus is a TCustomControl property
    begin
      LFocusRect := LClientRect;
      InflateRect(LFocusRect, -1, -1); // Small inset for focus rect
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := clWindowText; // Standard focus color
      Canvas.Pen.Style := psDot;
      Canvas.Rectangle(LFocusRect); // Using VCL Canvas for standard focus rect
    end;

  finally
    LGraphics.Free;
  end;
end;

procedure TANDMR_CScrollBar.Resize;
begin
  inherited Resize;
  UpdateThumbRect; // Thumb position and size might change relative to new control dimensions
  Invalidate;
end;

procedure TANDMR_CScrollBar.SetParams(APosition, AMin, AMax: Integer);
var
  OldPos, OldMin, OldMax: Integer;
begin
  OldPos := FPosition;
  OldMin := FMin;
  OldMax := FMax;

  if AMin > AMax then
    AMin := AMax;
  if APosition < AMin then
    APosition := AMin;
  if APosition > AMax then
    APosition := AMax;

  if (FPosition <> APosition) or (FMin <> AMin) or (FMax <> AMax) then
  begin
    FPosition := APosition;
    FMin := AMin;
    FMax := AMax;
    UpdateThumbRect;
    Invalidate;
    if (OldPos <> FPosition) or (OldMin <> FMin) or (OldMax <> FMax) then // Check if any param truly changed
      DoChange;
    // DoScroll is typically for user interaction changing position, SetParams is programmatic.
    // If DoScroll is needed here, it implies a scroll event even on programmatic full setup.
    // For now, only DoChange to signify parameters have changed.
  end;
end;

procedure TANDMR_CScrollBar.SetMin(Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMin > FMax then // Ensure Min is not greater than Max
      FMax := FMin;
    if FPosition < FMin then
      FPosition := FMin; // Adjust position if it's now out of bounds

    UpdateThumbRect;
    Invalidate;
    DoChange;
  end;
end;

procedure TANDMR_CScrollBar.SetMax(Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMax < FMin then // Ensure Max is not less than Min
      FMin := FMax;
    if FPosition > FMax then
      FPosition := FMax; // Adjust position if it's now out of bounds

    UpdateThumbRect;
    Invalidate;
    DoChange;
  end;
end;

procedure TANDMR_CScrollBar.SetPosition(Value: Integer);
var
  OldPosition: Integer;
  ClampedValue: Integer;
begin
  OldPosition := FPosition;

  ClampedValue := Value;
  if ClampedValue < FMin then
    ClampedValue := FMin;
  if ClampedValue > FMax then
    ClampedValue := FMax;

  if FPosition <> ClampedValue then
  begin
    FPosition := ClampedValue;
    UpdateThumbRect; // Update thumb rect based on new position
    Invalidate;
    DoScroll; // Fire OnScroll as position is changing due to user or programmatic action
    DoChange; // Fire OnChange as position has changed
  end
  else if Value <> OldPosition then // If input 'Value' was different but clamped to same FPosition
  begin
    // This case means 'Value' was outside Min/Max, got clamped, but FPosition didn't change from its previous value.
    // Still, we might want to trigger scroll if the intent was to move to an edge.
    // However, standard behavior is often to only fire Scroll/Change if FPosition *actually* changes.
    // The current logic (if FPosition <> ClampedValue) is generally correct.
    // If an explicit scroll to edge (even if already there) needs to fire event, logic might need adjustment.
  end;
end;

procedure TANDMR_CScrollBar.SetSmallChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FSmallChange <> Value then
  begin
    FSmallChange := Value;
    // Invalidate; // Not strictly necessary as it doesn't change visual appearance directly
  end;
end;

procedure TANDMR_CScrollBar.SetLargeChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FLargeChange <> Value then
  begin
    FLargeChange := Value;
    UpdateThumbRect; // Thumb size depends on LargeChange
    Invalidate;
  end;
end;

procedure TANDMR_CScrollBar.SetOrientation(Value: TANDMR_ScrollBarOrientation);
var
  OldWidth, OldHeight: Integer;
begin
  if FOrientation <> Value then
  begin
    OldWidth := Width;
    OldHeight := Height;

    FOrientation := Value;

    // Attempt to intelligently swap width and height
    if IsHorizontal then
    begin
      if (Height = OldWidth) and (Width = OldHeight) then Exit; // Already swapped by user?
      if (OldHeight > OldWidth) or (Width = OldWidth) then // Common case: was vertical, or dimensions not yet swapped
      begin
        Width := OldHeight;
        Height := OldWidth;
      end;
      if Height = 0 then Height := 17; // Ensure minimum height
      if Width = 0 then Width := 100; // Ensure minimum width
    end
    else // IsVertical
    begin
      if (Width = OldHeight) and (Height = OldWidth) then Exit; // Already swapped by user?
      if (OldWidth > OldHeight) or (Height = OldHeight) then // Common case: was horizontal, or dimensions not yet swapped
      begin
        Width := OldHeight;
        Height := OldWidth;
      end;
      if Width = 0 then Width := 17; // Ensure minimum width
      if Height = 0 then Height := 100; // Ensure minimum height
    end;

    UpdateThumbRect;
    Invalidate;
    DoChange; // Orientation change is a significant change
  end;
end;

procedure TANDMR_CScrollBar.SetStyle(Value: TANDMR_CScrollBarStyle);
var
 LBlendColor: TColor; // Moved declaration here as it's used in sbsGradient
begin
  if FStyle = Value then Exit;

  try
    FStyle := Value;

    // Apply default settings for the selected style
    // Users can override these by modifying ThumbSettings, TrackSettings, HoverSettings properties afterwards
    case Value of
      sbsSolid:
      begin
//        FTrackSettings.GradientEnabled := False;
        FTrackSettings.BackgroundColor := clWhite;
        FTrackSettings.Color := clGray;
        FTrackSettings.Thickness := 1;
        FTrackSettings.CornerRadius := 0;

//        FThumbSettings.GradientEnabled := False;
        FThumbSettings.BackgroundColor := clScrollBar;
        FThumbSettings.Color := clBlack;
        FThumbSettings.Thickness := 1;
        FThumbSettings.CornerRadius := 0;

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade; // Example
        FHoverSettings.BackgroundColor := LighterColor(FThumbSettings.BackgroundColor, 20);
        FHoverSettings.BorderColor := FThumbSettings.Color;
//        FHoverSettings.InteractionBackgroundColor := DarkerColor(FThumbSettings.BackgroundColor, 10);
//        FHoverSettings.InteractionBorderColor := clBlack;
      end;

      sbsModern:
      begin
//        FTrackSettings.GradientEnabled := False;
        FTrackSettings.BackgroundColor := TColor($00F5F5F5);
        FTrackSettings.Color := TColor($00E0E0E0);
        FTrackSettings.Thickness := 1;
        FTrackSettings.CornerRadius := 3;

//        FThumbSettings.GradientEnabled := False;
        FThumbSettings.BackgroundColor := TColor($00C0C0C0);
        FThumbSettings.Color := TColor($00A0A0A0);
        FThumbSettings.Thickness := 1;
        FThumbSettings.CornerRadius := 6; // Will be further adjusted in Paint for pill shape

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade;
        FHoverSettings.BackgroundColor := LighterColor(FThumbSettings.BackgroundColor, 15);
        FHoverSettings.BorderColor := FThumbSettings.Color;
//        FHoverSettings.InteractionBackgroundColor := DarkerColor(FThumbSettings.BackgroundColor, 10);
//        FHoverSettings.InteractionBorderColor := TColor($00808080);
      end;

      sbsGradient:
      begin
//        FTrackSettings.GradientEnabled := True;
//        FTrackSettings.GradientStartColor := clWhite;
//        FTrackSettings.GradientEndColor := clBtnFace;
//        FTrackSettings.GradientType := gtLinearVertical;
        FTrackSettings.Color := clGray;
        FTrackSettings.Thickness := 1;
        FTrackSettings.CornerRadius := 0;

//        FThumbSettings.GradientEnabled := True;
//        FThumbSettings.GradientStartColor := LighterColor(clScrollBar, 10);
//        FThumbSettings.GradientEndColor := DarkerColor(clScrollBar, 10);
//        FThumbSettings.GradientType := gtLinearVertical;
        FThumbSettings.Color := clBlack;
        FThumbSettings.Thickness := 1;
        FThumbSettings.CornerRadius := 2;

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade;
//        LBlendColor := BlendColors(FThumbSettings.GradientStartColor, FThumbSettings.GradientEndColor, 0.5);
        FHoverSettings.BackgroundColor := LighterColor(LBlendColor, 20);
        FHoverSettings.BorderColor := clBlack;
//        FHoverSettings.InteractionBackgroundColor := DarkerColor(LBlendColor, 10);
//        FHoverSettings.InteractionBorderColor := clBlack;
      end;

      sbsFlatStyle:
      begin
//        FTrackSettings.GradientEnabled := False;
        FTrackSettings.BackgroundColor := clBtnFace;
        FTrackSettings.Color := clBtnFace; // No border or same as bg
        FTrackSettings.Thickness := 0;
        FTrackSettings.CornerRadius := 0;

//        FThumbSettings.GradientEnabled := False;
        FThumbSettings.BackgroundColor := clScrollBar;
        FThumbSettings.Color := clScrollBar; // No border or same as bg
        FThumbSettings.Thickness := 0;
        FThumbSettings.CornerRadius := 0; // Will be made sharp in Paint

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade;
        FHoverSettings.BackgroundColor := LighterColor(FThumbSettings.BackgroundColor, 25);
        FHoverSettings.BorderColor := FThumbSettings.BackgroundColor;
//        FHoverSettings.InteractionBackgroundColor := DarkerColor(FThumbSettings.BackgroundColor, 10);
//        FHoverSettings.InteractionBorderColor := FThumbSettings.BackgroundColor;
      end;

      sbsWindows10:
      begin
//        FTrackSettings.GradientEnabled := False;
        FTrackSettings.BackgroundColor := TColor($00F0F0F0);
        FTrackSettings.Color := TColor($00E0E0E0);
        FTrackSettings.Thickness := 1;
        FTrackSettings.CornerRadius := 0;

//        FThumbSettings.GradientEnabled := False;
        FThumbSettings.BackgroundColor := TColor($00AEAEAE);
        FThumbSettings.Color := TColor($008E8E8E);
        FThumbSettings.Thickness := 1;
        FThumbSettings.CornerRadius := 0; // Will be made pill-like in Paint

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade;
        FHoverSettings.BackgroundColor := TColor($00DADADA);
        FHoverSettings.BorderColor := TColor($008E8E8E);
//        FHoverSettings.InteractionBackgroundColor := TColor($009E9E9E);
//        FHoverSettings.InteractionBorderColor := TColor($00707070);
      end;

      sbsMacOS:
      begin
//        FTrackSettings.GradientEnabled := False;
        FTrackSettings.BackgroundColor := TColor($00E0E0E0); // Subtle track
        FTrackSettings.Color := FTrackSettings.BackgroundColor;
        FTrackSettings.Thickness := 0;
        FTrackSettings.CornerRadius := 7; // Rounded track ends

//        FThumbSettings.GradientEnabled := False;
        FThumbSettings.BackgroundColor := TColor($B0888888); // Semi-transparent dark gray
        FThumbSettings.Color := FThumbSettings.BackgroundColor;
        FThumbSettings.Thickness := 0;
        FThumbSettings.CornerRadius := 100; // Will be capped to pill shape in Paint

        FHoverSettings.Enabled := True;
        FHoverSettings.HoverEffect := heFade;
        FHoverSettings.BackgroundColor := TColor($D0989898); // Lighter/more opaque on hover
        FHoverSettings.BorderColor := FHoverSettings.BackgroundColor;
//        FHoverSettings.InteractionBackgroundColor := TColor($E0A0A0A0);
//        FHoverSettings.InteractionBorderColor := FHoverSettings.BackgroundColor;
      end;
    end;

    SettingsChanged(Self); // This calls UpdateThumbRect and Invalidate
  finally
    // LApplyingStyle := False;
  end;
end;

procedure TANDMR_CScrollBar.SetThumbSettings(Value: TBorderSettings);
begin
  FThumbSettings.Assign(Value);
  // OnChange event is hooked up, SettingsChanged will be called by FThumbSettings.
end;

procedure TANDMR_CScrollBar.SetTrackSettings(Value: TBorderSettings);
begin
  FTrackSettings.Assign(Value);
  // OnChange event is hooked up, SettingsChanged will be called by FTrackSettings.
end;

procedure TANDMR_CScrollBar.SetHoverSettings(Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  // OnChange event is hooked up, SettingsChanged will be called by FHoverSettings.
end;

procedure TANDMR_CScrollBar.SettingsChanged(Sender: TObject);
begin
  UpdateThumbRect;
  Invalidate;
end;

procedure TANDMR_CScrollBar.UpdateThumbRect;
var
  TrackLength, ThumbLengthPixels, ThumbPosPixels: Integer;
  Range: Integer;
  TrackArea: TRect;
  Denominator: Integer; // Corrected Delphi syntax
begin
  TrackArea := GetThumbTrackArea;
  Range := FMax - FMin;

  if (Range < 0) or (FLargeChange <= 0) or (TrackArea.IsEmpty) then
  begin
    // Invalid state or no room to draw, make thumb empty
    if IsHorizontal then
      FThumbRect := Rect(TrackArea.Left, TrackArea.Top, TrackArea.Left, TrackArea.Bottom)
    else
      FThumbRect := Rect(TrackArea.Left, TrackArea.Top, TrackArea.Right, TrackArea.Top);
    Exit;
  end;

  Denominator := Range + FLargeChange;
  if Denominator <= 0 then // If range is 0 and largechange is 0, or negative range
     Denominator := 1; // Avoid division by zero, implies thumb fills track or is minimal

  if IsHorizontal then
  begin
    TrackLength := TrackArea.Width;
    if Range = 0 then // If Min = Max, thumb should fill the track
      ThumbLengthPixels := TrackLength
    else
      ThumbLengthPixels := System.Math.Max(Round(TrackLength * FLargeChange / Denominator), 8); // Min thumb size 8px // Used System.Math.Max

    ThumbLengthPixels := System.Math.Min(ThumbLengthPixels, TrackLength); // Cannot be larger than track // Used System.Math.Min

    if Range = 0 then
      ThumbPosPixels := TrackArea.Left
    else
      ThumbPosPixels := TrackArea.Left + Round((TrackLength - ThumbLengthPixels) * (FPosition - FMin) / Range);

    // Ensure thumb stays within track bounds, especially with floating point inaccuracies
    ThumbPosPixels := System.Math.Max(TrackArea.Left, System.Math.Min(ThumbPosPixels, TrackArea.Right - ThumbLengthPixels)); // Used System.Math.Max/Min

    FThumbRect := Rect(ThumbPosPixels, TrackArea.Top, ThumbPosPixels + ThumbLengthPixels, TrackArea.Bottom);
  end
  else // Vertical
  begin
    TrackLength := TrackArea.Height;
    if Range = 0 then
      ThumbLengthPixels := TrackLength
    else
      ThumbLengthPixels := System.Math.Max(Round(TrackLength * FLargeChange / Denominator), 8); // Used System.Math.Max

    ThumbLengthPixels := System.Math.Min(ThumbLengthPixels, TrackLength); // Used System.Math.Min

    if Range = 0 then
      ThumbPosPixels := TrackArea.Top
    else
      ThumbPosPixels := TrackArea.Top + Round((TrackLength - ThumbLengthPixels) * (FPosition - FMin) / Range);

    ThumbPosPixels := System.Math.Max(TrackArea.Top, System.Math.Min(ThumbPosPixels, TrackArea.Bottom - ThumbLengthPixels)); // Used System.Math.Max/Min

    FThumbRect := Rect(TrackArea.Left, ThumbPosPixels, TrackArea.Right, ThumbPosPixels + ThumbLengthPixels);
  end;
end;

procedure TANDMR_CScrollBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewPos: Integer;
  Handled: Boolean;
begin
  inherited KeyDown(Key, Shift);
  NewPos := FPosition;
  Handled := True; // Assume handled unless specified otherwise

  case Key of
    VK_LEFT:
      if IsHorizontal then NewPos := FPosition - FSmallChange else Handled := False;
    VK_RIGHT:
      if IsHorizontal then NewPos := FPosition + FSmallChange else Handled := False;
    VK_UP:
      if IsVertical then NewPos := FPosition - FSmallChange else Handled := False;
    VK_DOWN:
      if IsVertical then NewPos := FPosition + FSmallChange else Handled := False;
    VK_PRIOR: // PageUp
      NewPos := FPosition - FLargeChange;
    VK_NEXT: // PageDown
      NewPos := FPosition + FLargeChange;
    VK_HOME:
      NewPos := FMin;
    VK_END:
      NewPos := FMax;
  else
    Handled := False; // Not a key we handle
  end;

  if Handled then
  begin
    if NewPos <> FPosition then
      Position := NewPos; // Setter handles clamping, Invalidate, DoScroll, DoChange
    Key := 0; // We've handled the key
  end;
end;

procedure TANDMR_CScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TrackRect: TRect;
  MousePt: TPoint;
  NewPos: Integer;
  Range: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not Enabled then Exit;
  if Button = mbLeft then
  begin
    SetFocus; // Set focus to the scrollbar on click
    MousePt := Point(X, Y);
    if PtInRect(FThumbRect, MousePt) then
    begin
      FIsDraggingThumb := True;
      if IsHorizontal then
        FDragOffset := X - FThumbRect.Left
      else
        FDragOffset := Y - FThumbRect.Top;
      SetCapture(Handle);
      Invalidate; // Update visual state (e.g., pressed thumb)
    end
    else // Click was on the track, not the thumb
    begin
      TrackRect := GetThumbTrackArea;
      if PtInRect(TrackRect, MousePt) then // Ensure click is within the track area
      begin
        Range := FMax - FMin;
        if Range <= 0 then Exit; // No range to scroll

        if IsHorizontal then
        begin
          if X < FThumbRect.Left then
            NewPos := FPosition - FLargeChange
          else if X > FThumbRect.Right then
            NewPos := FPosition + FLargeChange
          else
            Exit; // Clicked between thumb ends but not on thumb (unlikely if thumb is continuous)
        end
        else // Vertical
        begin
          if Y < FThumbRect.Top then
            NewPos := FPosition - FLargeChange
          else if Y > FThumbRect.Bottom then
            NewPos := FPosition + FLargeChange
          else
            Exit;
        end;
        Position := NewPos; // This will call DoScroll and DoChange
      end;
    end;
  end;
end;

procedure TANDMR_CScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CurrentThumbHovered: Boolean;
  TrackMousePos, NewPosValue, TrackLengthPixels, ThumbLengthPxls, RangeScrollable: Integer;
  MousePt: TPoint;
  TrackArea: TRect;
begin
  inherited MouseMove(Shift, X, Y);

  if not Enabled then Exit;

  MousePt := Point(X,Y);
  TrackArea := GetThumbTrackArea;

  CurrentThumbHovered := PtInRect(FThumbRect, MousePt);
  if CurrentThumbHovered <> FIsThumbHovered then
  begin
    FIsThumbHovered := CurrentThumbHovered;
    if FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heFade) then
      FHoverSettings.StartAnimation(FIsThumbHovered or FIsTrackHovered);
    Invalidate;
  end;

  // General track hover state (independent of thumb hover)
  // This is more reliably handled by CM_MOUSEENTER/LEAVE for the control itself
  // FIsTrackHovered is set by CM_MOUSEENTER/LEAVE.

  if FIsDraggingThumb then
  begin
    RangeScrollable := FMax - FMin;
    if RangeScrollable <= 0 then Exit;

    if IsHorizontal then
    begin
      TrackLengthPixels := TrackArea.Width;
      ThumbLengthPxls := FThumbRect.Width;
      TrackMousePos := X - TrackArea.Left - FDragOffset;
    end
    else
    begin
      TrackLengthPixels := TrackArea.Height;
      ThumbLengthPxls := FThumbRect.Height;
      TrackMousePos := Y - TrackArea.Top - FDragOffset;
    end;

    if TrackLengthPixels - ThumbLengthPxls <= 0 then Exit; // Thumb fills track or invalid

    // Clamp TrackMousePos to the valid range of thumb's top/left positions
    TrackMousePos := System.Math.Max(0, System.Math.Min(TrackMousePos, TrackLengthPixels - ThumbLengthPxls)); // Used System.Math.Max/Min

    NewPosValue := FMin + Round(TrackMousePos * RangeScrollable / (TrackLengthPixels - ThumbLengthPxls));

    Position := NewPosValue;
  end;
end;

procedure TANDMR_CScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not Enabled then Exit;

  if Button = mbLeft then
  begin
    if FIsDraggingThumb then
    begin
      FIsDraggingThumb := False;
      ReleaseCapture;
      Invalidate;
      // Check if mouse is still over thumb to maintain hover state
      if PtInRect(FThumbRect, Point(X,Y)) then
      begin
        if not FIsThumbHovered then
        begin
           FIsThumbHovered := True;
           // Optionally restart hover animation if it was interrupted by drag state
           if FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heFade) then
             FHoverSettings.StartAnimation(True);
        end;
      end
      else
      begin
         if FIsThumbHovered then
         begin
            FIsThumbHovered := False;
            if FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heFade) then
              FHoverSettings.StartAnimation(FIsTrackHovered); // Animate based on track hover if still over track
         end;
      end;
    end;
  end;
end;

procedure TANDMR_CScrollBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateThumbRect; // Thumb appearance might change
  Invalidate;
end;

procedure TANDMR_CScrollBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not Enabled then Exit;
  FIsTrackHovered := True;
  if FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heFade) then
    FHoverSettings.StartAnimation(True); // Start animation based on track hover
  Invalidate;
end;

procedure TANDMR_CScrollBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  // FIsTrackHovered should remain true if mouse is still within client rect,
  // but CM_MOUSELEAVE means it left the control boundaries.
  FIsTrackHovered := False;
  FIsThumbHovered := False; // Also reset thumb hover
  if FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heFade) then
    FHoverSettings.StartAnimation(False); // Stop animation
  Invalidate;
end;

procedure TANDMR_CScrollBar.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TANDMR_CScrollBar.DoScroll;
begin
  // UpdateThumbRect is now called by SetPosition before Invalidate and DoScroll.
  // This ensures FThumbRect is correct before any potential repaint triggered by OnScroll.
  // Invalidate is also called by SetPosition.
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

function TANDMR_CScrollBar.IsHorizontal: Boolean;
begin
  Result := FOrientation = soHorizontal;
end;

function TANDMR_CScrollBar.IsVertical: Boolean;
begin
  Result := FOrientation = soVertical;
end;

function TANDMR_CScrollBar.GetThumbTrackArea: TRect;
begin
  // For now, the entire client area is the track.
  // If arrows or margins are added, this function needs to be adjusted.
  Result := ClientRect;
end;

end.
