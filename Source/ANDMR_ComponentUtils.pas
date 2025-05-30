unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage; // Ensure GDIP units are here

type
  // Delphi type definitions (TRoundCornerType, etc. are already here)

  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);

  TRoundCornerType = (
    rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight,
    rctTop, rctBottom, rctLeft, rctRight,
    rctTopLeftBottomRight, rctTopRightBottomLeft
  );

  TInputType = (itNormal, itLettersOnly, itNumbersOnly, itNoSpecialChars, itAlphaNumericOnly);
  TTextCase = (tcNormal, tcUppercase, tcLowercase);
  TCaptionPosition = (cpAbove, cpBelow, cpLeft, cpRight);

  TPredefinedMaskType = (
    pmtNone,
    pmtCustom,
    pmtCPF,
    pmtCNPJ,
    pmtCEP,
    pmtPhoneBR,
    pmtDateDMY
  );

  TImageMarginsControl = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write SetLeft default 2;
    property Top: Integer read FTop write SetTop default 2;
    property Right: Integer read FRight write SetRight default 2;
    property Bottom: Integer read FBottom write SetBottom default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCaptionSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FText: string;
    FPosition: TCaptionPosition;
    FAlignment: TAlignment; // Vcl.Themes.TAlignment
    FFont: TFont;
    FColor: TColor;
    FOffset: Integer;
    FWordWrap: Boolean;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl;
    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetPosition(const Value: TCaptionPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Text: string read FText write SetText;
    property Position: TCaptionPosition read FPosition write SetPosition default cpAbove;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText;
    property Offset: Integer read FOffset write SetOffset default 2;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THoverSettings = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FFontColor: TColor;
    FCaptionFontColor: TColor;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetCaptionFontColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clInfoBk;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clHighlight;
    property FontColor: TColor read FFontColor write SetFontColor default clInfoText;
    property CaptionFontColor: TColor read FCaptionFontColor write SetCaptionFontColor default clInfoText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write SetLeft default 4;
    property Top: Integer read FTop write SetTop default 2;
    property Right: Integer read FRight write SetRight default 4;
    property Bottom: Integer read FBottom write SetBottom default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

// Helper function declarations
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);

implementation

uses
  System.Math,     // For Min, Max
  Winapi.ActiveX;  // For IStream, TStreamAdapter

{ TImageMarginsControl }
constructor TImageMarginsControl.Create;
begin
  inherited Create;
  FLeft := 2; FTop := 2; FRight := 2; FBottom := 2;
end;

procedure TImageMarginsControl.Assign(Source: TPersistent);
begin
  if Source is TImageMarginsControl then
  begin
    Self.FLeft := TImageMarginsControl(Source).FLeft;
    Self.FTop := TImageMarginsControl(Source).FTop;
    Self.FRight := TImageMarginsControl(Source).FRight;
    Self.FBottom := TImageMarginsControl(Source).FBottom;
    Changed; // Ensure OnChange is triggered if assigned
  end else inherited Assign(Source);
end;

procedure TImageMarginsControl.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TImageMarginsControl.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TImageMarginsControl.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TImageMarginsControl.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TImageMarginsControl.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;

{ TCaptionSettings }
constructor TCaptionSettings.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwner;
  FVisible := True;
  FText := '';
  FPosition := cpAbove;
  FAlignment := taLeftJustify;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;

  // --- Modification Start ---
  // Always use a hardcoded default font to avoid the E2362 error.
  // The attempt to inherit font via FOwnerControl.Font has proven problematic.
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  // Optionally, set other FFont properties like FFont.Color if a specific default is needed.
  // FFont.Color := clWindowText; // This is already the default for TFont.
  // --- Modification End ---

  FColor := clWindowText; // This is TCaptionSettings.FColor, the text color of the caption
  FOffset := 2;
  FWordWrap := False;
end;

destructor TCaptionSettings.Destroy;
begin
  FFont.OnChange := nil;
  FFont.Free;
  inherited Destroy;
end;

procedure TCaptionSettings.Assign(Source: TPersistent);
begin
  if Source is TCaptionSettings then
  begin
    FVisible := TCaptionSettings(Source).FVisible;
    FText := TCaptionSettings(Source).FText;
    FPosition := TCaptionSettings(Source).FPosition;
    FAlignment := TCaptionSettings(Source).FAlignment;
    FFont.Assign(TCaptionSettings(Source).FFont);
    FColor := TCaptionSettings(Source).FColor;
    FOffset := TCaptionSettings(Source).FOffset;
    FWordWrap := TCaptionSettings(Source).FWordWrap;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TCaptionSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCaptionSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCaptionSettings.SetAlignment(const Value: TAlignment); begin if FAlignment <> Value then begin FAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TCaptionSettings.SetFont(const Value: TFont); begin FFont.Assign(Value); Changed; end;
procedure TCaptionSettings.SetOffset(const Value: Integer); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end;
procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition); begin if FPosition <> Value then begin FPosition := Value; Changed; end; end;
procedure TCaptionSettings.SetText(const Value: string); begin if FText <> Value then begin FText := Value; Changed; end; end;
procedure TCaptionSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TCaptionSettings.SetWordWrap(const Value: Boolean); begin if FWordWrap <> Value then begin FWordWrap := Value; Changed; end; end;

{ THoverSettings }
constructor THoverSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FBackgroundColor := clInfoBk;
  FBorderColor := clHighlight;
  FFontColor := clInfoText;
  FCaptionFontColor := clInfoText;
end;

procedure THoverSettings.Assign(Source: TPersistent);
begin
  if Source is THoverSettings then
  begin
    FEnabled := THoverSettings(Source).FEnabled;
    FBackgroundColor := THoverSettings(Source).FBackgroundColor;
    FBorderColor := THoverSettings(Source).FBorderColor;
    FFontColor := THoverSettings(Source).FFontColor;
    FCaptionFontColor := THoverSettings(Source).FCaptionFontColor;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure THoverSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THoverSettings.SetBackgroundColor(const Value: TColor); begin if FBackgroundColor <> Value then begin FBackgroundColor := Value; Changed; end; end;
procedure THoverSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure THoverSettings.SetCaptionFontColor(const Value: TColor); begin if FCaptionFontColor <> Value then begin FCaptionFontColor := Value; Changed; end; end;
procedure THoverSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; end; end;
procedure THoverSettings.SetFontColor(const Value: TColor); begin if FFontColor <> Value then begin FFontColor := Value; Changed; end; end;

{ TTextMargins }
constructor TTextMargins.Create;
begin
  inherited Create;
  FLeft := 4; FTop := 2; FRight := 4; FBottom := 2;
end;

procedure TTextMargins.Assign(Source: TPersistent);
begin
  if Source is TTextMargins then
  begin
    Self.FLeft := TTextMargins(Source).FLeft;
    Self.FTop := TTextMargins(Source).FTop;
    Self.FRight := TTextMargins(Source).FRight;
    Self.FBottom := TTextMargins(Source).FBottom;
    Changed;
  end else inherited Assign(Source);
end;

procedure TTextMargins.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTextMargins.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TTextMargins.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TTextMargins.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TTextMargins.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;

function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
  EffectiveAlpha: Byte;
begin
  EffectiveAlpha := Alpha;
  if AColor = clNone then
  begin
    Result := (UInt32(EffectiveAlpha) shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor); // BGR format from VCL
  Result := (UInt32(EffectiveAlpha) shl 24) or
            ((ColorRef and $000000FF) shl 16) or
            (ColorRef and $0000FF00) or
            ((ColorRef and $00FF0000) shr 16);
end;

procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.1;
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
  Rect: TGPRectF;
begin
  APath.Reset;
  Rect := ARect;

  if Rect.Width < 0 then Rect.Width := 0;
  if Rect.Height < 0 then Rect.Height := 0;

  if (Rect.Width = 0) or (Rect.Height = 0) then
  begin
    if (Rect.Width > 0) and (Rect.Height > 0) then
       APath.AddRectangle(Rect);
    Exit;
  end;

  LRadius := ARadiusValue;
  LRadius := Min(LRadius, Rect.Width / 2.0);
  LRadius := Min(LRadius, Rect.Height / 2.0);
  LRadius := Max(0.0, LRadius);

  LDiameter := LRadius * 2.0;

  if (AType = rctNone) or (LRadius < MIN_RADIUS_FOR_PATH) or (LDiameter <= 0) or (Rect.Width < LDiameter) or (Rect.Height < LDiameter) then
  begin
    APath.AddRectangle(Rect);
    Exit;
  end;

  RoundTL := AType in [rctAll, rctTopLeft, rctTop, rctLeft, rctTopLeftBottomRight];
  RoundTR := AType in [rctAll, rctTopRight, rctTop, rctRight, rctTopRightBottomLeft];
  RoundBL := AType in [rctAll, rctBottomLeft, rctBottom, rctLeft, rctTopRightBottomLeft];
  RoundBR := AType in [rctAll, rctBottomRight, rctBottom, rctRight, rctTopLeftBottomRight];

  APath.StartFigure;

  if RoundTL then
    APath.AddArc(Rect.X, Rect.Y, LDiameter, LDiameter, 180, 90)
  else
    APath.AddLine(Rect.X, Rect.Y, Rect.X, Rect.Y);

  APath.AddLine(Rect.X + LRadius, Rect.Y, Rect.X + Rect.Width - LRadius, Rect.Y);

  if RoundTR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y, LDiameter, LDiameter, 270, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y);

  APath.AddLine(Rect.X + Rect.Width, Rect.Y + LRadius, Rect.X + Rect.Width, Rect.Y + Rect.Height - LRadius);

  if RoundBR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 0, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height, Rect.X + Rect.Width, Rect.Y + Rect.Height);

  APath.AddLine(Rect.X + Rect.Width - LRadius, Rect.Y + Rect.Height, Rect.X + LRadius, Rect.Y + Rect.Height);

  if RoundBL then
    APath.AddArc(Rect.X, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 90, 90)
  else
    APath.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y + Rect.Height);

  APath.CloseFigure;
end;

procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
var
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LRectF: TGPRectF;
  LRadiusValue: Single;
  LBorderThicknessValue: Single; // Use a Single for GDI+ operations
begin
  if (AGraphics = nil) or (ADrawArea.Width <= 0) or (ADrawArea.Height <= 0) then // Basic check
    Exit;

  LBorderThicknessValue := ABorderThickness; // Use parameter

  // Define the rectangle for path creation.
  // If there's a border, the path should trace the centerline of the border stroke.
  // This means the rectangle for the path is inset by half the border thickness.
  if LBorderThicknessValue > 0 then
  begin
    LRectF.X      := ADrawArea.Left + LBorderThicknessValue / 2.0;
    LRectF.Y      := ADrawArea.Top + LBorderThicknessValue / 2.0;
    LRectF.Width  := ADrawArea.Width - LBorderThicknessValue;
    LRectF.Height := ADrawArea.Height - LBorderThicknessValue;
  end
  else // No border, path is the ADrawArea itself
  begin
    LRectF.X      := ADrawArea.Left;
    LRectF.Y      := ADrawArea.Top;
    LRectF.Width  := ADrawArea.Width;
    LRectF.Height := ADrawArea.Height;
  end;

  // Ensure dimensions are not negative for path creation.
  LRectF.Width  := Max(0.0, LRectF.Width);
  LRectF.Height := Max(0.0, LRectF.Height);

  if (LRectF.Width = 0) or (LRectF.Height = 0) then // Cannot draw if path rect is empty
    Exit;

  // Calculate effective radius for rounding, cannot exceed half of the smallest dimension of LRectF.
  LRadiusValue := ACornerRadius; // Use parameter
  LRadiusValue := Min(LRadiusValue, LRectF.Width / 2.0);
  LRadiusValue := Min(LRadiusValue, LRectF.Height / 2.0);
  LRadiusValue := Max(0.0, LRadiusValue); // Ensure non-negative radius

  LPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LPath, LRectF, LRadiusValue, ARoundCornerType); // Use parameter

    if LPath.GetPointCount > 0 then // Check if path creation was successful
    begin
      // Fill Background
      if ABackgroundColor <> clNone then
      begin
        // Use AOpacity for the alpha component of the background color
        LBrush := TGPSolidBrush.Create(ColorToARGB(ABackgroundColor, AOpacity)); // Use parameter
        try
          AGraphics.FillPath(LBrush, LPath);
        finally
          LBrush.Free;
        end;
      end;

      // Draw Border
      if (LBorderThicknessValue > 0) and (ABorderColor <> clNone) and (ABorderStyle <> psClear) then // Use parameters
      begin
        // Use AOpacity for the alpha component of the border color
        LPen := TGPPen.Create(ColorToARGB(ABorderColor, AOpacity), LBorderThicknessValue); // Use parameters
        try
          case ABorderStyle of // Use parameter
            psSolid:      LPen.SetDashStyle(DashStyleSolid);
            psDash:       LPen.SetDashStyle(DashStyleDash);
            psDot:        LPen.SetDashStyle(DashStyleDot);
            psDashDot:    LPen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: LPen.SetDashStyle(DashStyleDashDotDot);
            else          LPen.SetDashStyle(DashStyleSolid);
          end;
          AGraphics.DrawPath(LPen, LPath);
        finally
          LPen.Free;
        end;
      end;
    end;
  finally
    LPath.Free;
  end;
end;

procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
var
  DrawImageRect: TRect;
  GraphicW, GraphicH: Integer;
  rRatio, rRectRatio: Double;
  tempCalculatedW, tempCalculatedH: Double; // For checking pre-rounding values
  PngStream: TMemoryStream;
  GpSourceBitmap: TGPBitmap;
  Adapter: IStream;
  E: Exception; // For the exception handler
begin
  if (AGraphics = nil) or (APNG = nil) then Exit;
  if (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then Exit;

  GraphicW := APNG.Width;
  GraphicH := APNG.Height;

  if (GraphicW <= 0) or (GraphicH <= 0) then
  begin
    if ADrawMode = idmNormal then
    begin
      // For idmNormal, proceed with current GraphicW/H.
    end
    else
    begin
      Exit;
    end;
  end;

  case ADrawMode of
    idmStretch:
      DrawImageRect := ADestRect;
    idmProportional:
      begin
        rRatio := GraphicW / GraphicH;

        if ADestRect.Height > 0 then
          rRectRatio := ADestRect.Width / ADestRect.Height
        else
          rRectRatio := MaxDouble;

        if rRectRatio > rRatio then
        begin
          DrawImageRect.Height := ADestRect.Height;
          tempCalculatedW := ADestRect.Height * rRatio;
          DrawImageRect.Width := Round(tempCalculatedW);
          if (DrawImageRect.Width = 0) and (tempCalculatedW > 0) and (ADestRect.Width > 0) then
            DrawImageRect.Width := 1;
        end
        else
        begin
          DrawImageRect.Width := ADestRect.Width;
          if rRatio > 0 then
          begin
            tempCalculatedH := ADestRect.Width / rRatio;
            DrawImageRect.Height := Round(tempCalculatedH);
            if (DrawImageRect.Height = 0) and (tempCalculatedH > 0) and (ADestRect.Height > 0) then
              DrawImageRect.Height := 1;
          end
          else
            DrawImageRect.Height := 0;
        end;

        DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - DrawImageRect.Width) div 2;
        DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - DrawImageRect.Height) div 2;
        DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
        DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
      end;
    idmNormal:
      begin
        DrawImageRect.Width := GraphicW;
        DrawImageRect.Height := GraphicH;
        DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - GraphicW) div 2;
        DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - GraphicH) div 2;
        DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
        DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
      end;
  else
    DrawImageRect := ADestRect;
  end;

  if ((DrawImageRect.Right <= DrawImageRect.Left) or (DrawImageRect.Bottom <= DrawImageRect.Top) or (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0)) then
    Exit;

  PngStream := TMemoryStream.Create;
  try // Outer try-finally for PngStream
    APNG.SaveToStream(PngStream);
    PngStream.Position := 0;
    Adapter := TStreamAdapter.Create(PngStream, soReference);
    GpSourceBitmap := TGPBitmap.Create(Adapter); // This can raise an exception
    try // Inner try-finally for GpSourceBitmap
      if (GpSourceBitmap = nil) or (GpSourceBitmap.GetLastStatus <> Ok) then
      begin
        // If GpSourceBitmap creation failed, exit without trying to use it
        // GpSourceBitmap.Free will be called in the finally block
        Exit;
      end;

      if (DrawImageRect.Width <> GpSourceBitmap.GetWidth()) or (DrawImageRect.Height <> GpSourceBitmap.GetHeight()) then
        AGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic)
      else
        AGraphics.SetInterpolationMode(InterpolationModeDefault);

      AGraphics.DrawImage(GpSourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height);
    finally
      GpSourceBitmap.Free; // Free GpSourceBitmap in all cases (success or exception during DrawImage)
    end;
  except // Exception handler for the outer try (e.g., issues with PngStream, Adapter, GpSourceBitmap.Create)
    on E: Exception do
    begin
      // Optional: Log or handle exception E
      // For example: System.Diagnostics.OutputDebugString(PChar('Error creating/processing PNG: ' + E.Message));
    end;
  end; // End of outer try-except
    PngStream.Free; // Free PngStream in all cases
end; // End of procedure

procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
var
  DrawImageRect: TRect;
  GraphicW, GraphicH: Integer;
  rRatio, rRectRatio: Double;
  tempCalculatedW, tempCalculatedH: Double; // For checking pre-rounding values
  E: Exception;
begin
  if (ACanvas = nil) or (AGraphic = nil) or (AGraphic.Empty) then Exit;
  if (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then Exit;

  GraphicW := AGraphic.Width;
  GraphicH := AGraphic.Height;

  if (GraphicW <= 0) or (GraphicH <= 0) then
  begin
    if ADrawMode = idmNormal then
    begin
      // For idmNormal, proceed.
    end
    else // For idmProportional or idmStretch, a 0-area source is invalid
    begin
      Exit;
    end;
  end;

  case ADrawMode of
    idmStretch:
      DrawImageRect := ADestRect;
    idmProportional:
      begin
        // This case assumes GraphicW > 0 and GraphicH > 0 due to the check above for idmProportional

        rRatio := GraphicW / GraphicH;

        if ADestRect.Height > 0 then
          rRectRatio := ADestRect.Width / ADestRect.Height
        else
          rRectRatio := MaxDouble;

        if rRectRatio > rRatio then // Fit to height
        begin
          DrawImageRect.Height := ADestRect.Height;
          tempCalculatedW := ADestRect.Height * rRatio;
          DrawImageRect.Width := Round(tempCalculatedW);
          if (DrawImageRect.Width = 0) and (tempCalculatedW > 0) and (ADestRect.Width > 0) then
            DrawImageRect.Width := 1;
        end
        else // Fit to width
        begin
          DrawImageRect.Width := ADestRect.Width;
          if rRatio > 0 then
          begin
            tempCalculatedH := ADestRect.Width / rRatio;
            DrawImageRect.Height := Round(tempCalculatedH);
            if (DrawImageRect.Height = 0) and (tempCalculatedH > 0) and (ADestRect.Height > 0) then
              DrawImageRect.Height := 1;
          end
          else
            DrawImageRect.Height := 0;
        end;

        DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - DrawImageRect.Width) div 2;
        DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - DrawImageRect.Height) div 2;
        DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
        DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
      end;
    idmNormal:
      begin
        DrawImageRect.Width := GraphicW;
        DrawImageRect.Height := GraphicH;
        DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - GraphicW) div 2;
        DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - GraphicH) div 2;
        DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width;
        DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height;
      end;
  else
    DrawImageRect := ADestRect; // Should not happen
  end;

  if ((DrawImageRect.Right <= DrawImageRect.Left) or (DrawImageRect.Bottom <= DrawImageRect.Top) or (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0)) then
    Exit;

  try
    if ADrawMode = idmNormal then
      ACanvas.Draw(DrawImageRect.Left, DrawImageRect.Top, AGraphic)
    else
      ACanvas.StretchDraw(DrawImageRect, AGraphic);
  except
    on E: Exception do
    begin
      // Optional: Log or handle
    end;
  end;
end;

procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
var LineX: Integer;
begin
  if (ACanvas = nil) or (AThickness <= 0) or (ASepRect.Width <= 0) or (ASepRect.Height <= 0) then Exit;
  LineX := ASepRect.Left + ASepRect.Width div 2; ACanvas.Pen.Color := AColor; ACanvas.Pen.Width := AThickness; ACanvas.Pen.Style := psSolid; ACanvas.MoveTo(LineX, ASepRect.Top); ACanvas.LineTo(LineX, ASepRect.Bottom);
end;

end.
