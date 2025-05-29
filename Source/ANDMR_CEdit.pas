unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);

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
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FActiveColor, FInactiveColor: TColor;
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;
    FImage: TPicture;
    FImageVisible: Boolean;
    FImagePosition: TImagePositionSide;
    FImageAlignment: TImageAlignmentVertical;
    FImageMargins: TImageMarginsControl;
    FImagePlacement: TImagePlacement;
    FImageDrawMode: TImageDrawMode;
    FSeparatorVisible: Boolean;
    FSeparatorColor: TColor;
    FSeparatorThickness: Integer;
    FSeparatorPadding: Integer;
    FSeparatorHeightMode: TSeparatorHeightMode;
    FSeparatorCustomHeight: Integer;
    FCaretVisible: Boolean;
    FCaretPosition: Integer;
    FCaretTimer: TTimer;

    // New private fields
    FFocusBorderColor: TColor;
    FFocusBorderColorVisible: Boolean;
    FFocusBackgroundColor: TColor;
    FFocusBackgroundColorVisible: Boolean;
    FFocusUnderlineColor: TColor;
    FFocusUnderlineVisible: Boolean;
    FFocusUnderlineThickness: Integer;
    FFocusUnderlineStyle: TPenStyle;
    FOpacity: Byte;
    FCurrentCursor: TCursor;

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
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetImage(const Value: TPicture);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetImagePosition(const Value: TImagePositionSide);
    procedure SetImageAlignment(const Value: TImageAlignmentVertical);
    procedure SetImageMargins(const Value: TImageMarginsControl);
    procedure ImageChanged(Sender: TObject);
    procedure ImageMarginsChanged(Sender: TObject);
    procedure SetImagePlacement(const Value: TImagePlacement);
    procedure SetImageDrawMode(const Value: TImageDrawMode);
    procedure SetSeparatorVisible(const Value: Boolean);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSeparatorThickness(const Value: Integer);
    procedure SetSeparatorPadding(const Value: Integer);
    procedure SetSeparatorHeightMode(const Value: TSeparatorHeightMode);
    procedure SetSeparatorCustomHeight(const Value: Integer);
    procedure CaretTimerTick(Sender: TObject);

    // New Set methods
    procedure SetFocusBorderColor(const Value: TColor);
    procedure SetFocusBorderColorVisible(const Value: Boolean);
    procedure SetFocusBackgroundColor(const Value: TColor);
    procedure SetFocusBackgroundColorVisible(const Value: Boolean);
    procedure SetFocusUnderlineColor(const Value: TColor);
    procedure SetFocusUnderlineVisible(const Value: Boolean);
    procedure SetFocusUnderlineThickness(const Value: Integer);
    procedure SetFocusUnderlineStyle(const Value: TPenStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetCurrentCursor(const Value: TCursor);

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
    procedure CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); virtual; // CORRECTED
    procedure DrawEditBox(const ADrawArea: TRect; AGraphics: TGPGraphics; ABackgroundColor: TColor; ABorderColor: TColor);
    procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
    procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
    procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
    procedure Paint; override; // Single Paint declaration
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
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clHighlight;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clBtnFace;
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
    property SeparatorHeightMode: TSeparatorHeightMode read FSeparatorHeightMode write SetSeparatorHeightMode default shmFull;
    property SeparatorCustomHeight: Integer read FSeparatorCustomHeight write SetSeparatorCustomHeight default 0; // Ensured semicolon

    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

    // New published properties
    property FocusBorderColor: TColor read FFocusBorderColor write SetFocusBorderColor;
    property FocusBorderColorVisible: Boolean read FFocusBorderColorVisible write SetFocusBorderColorVisible;
    property FocusBackgroundColor: TColor read FFocusBackgroundColor write SetFocusBackgroundColor;
    property FocusBackgroundColorVisible: Boolean read FFocusBackgroundColorVisible write SetFocusBackgroundColorVisible;
    property FocusUnderlineColor: TColor read FFocusUnderlineColor write SetFocusUnderlineColor;
    property FocusUnderlineVisible: Boolean read FFocusUnderlineVisible write SetFocusUnderlineVisible;
    property FocusUnderlineThickness: Integer read FFocusUnderlineThickness write SetFocusUnderlineThickness;
    property FocusUnderlineStyle: TPenStyle read FFocusUnderlineStyle write SetFocusUnderlineStyle;
    property Opacity: Byte read FOpacity write SetOpacity;
    property CurrentCursor: TCursor read FCurrentCursor write SetCurrentCursor;
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
  EffectiveAlpha: Byte;
begin
  // This function is general. The caller (e.g., Paint method using FOpacity)
  // is responsible for passing the desired alpha.
  EffectiveAlpha := Alpha;

  if AColor = clNone then
  begin
    // For clNone, only alpha matters, RGB is zero.
    Result := (UInt32(EffectiveAlpha) shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor); // BGR format from VCL
  Result := (UInt32(EffectiveAlpha) shl 24) or // Alpha
            ((ColorRef and $000000FF) shl 16) or // Blue component to ARGB Red position
            (ColorRef and $0000FF00) or          // Green component to ARGB Green position
            ((ColorRef and $00FF0000) shr 16);   // Red component to ARGB Blue position
end;

procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.1; // Adjusted for GDI+ which can handle smaller radii
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
  Rect: TGPRectF; // Use a local copy
begin
  APath.Reset;
  Rect := ARect; // Work with a copy

  // Ensure width and height are non-negative, GDI+ can be sensitive
  if Rect.Width < 0 then Rect.Width := 0;
  if Rect.Height < 0 then Rect.Height := 0;

  // If either dimension is zero, cannot form a meaningful path with arcs
  if (Rect.Width = 0) or (Rect.Height = 0) then
  begin
    // Add a simple rectangle if both >0 (though this case implies one is 0), else exit.
    // This prevents AddRectangle with zero width/height which might be problematic.
    if (Rect.Width > 0) and (Rect.Height > 0) then // Should not happen if one is already 0
       APath.AddRectangle(Rect);
    Exit;
  end;

  LRadius := ARadiusValue;
  // Radius cannot be more than half the width or height of the *current* Rect
  LRadius := Min(LRadius, Rect.Width / 2.0);
  LRadius := Min(LRadius, Rect.Height / 2.0);
  LRadius := Max(0.0, LRadius); // Ensure radius is not negative

  LDiameter := LRadius * 2.0;

  // If no rounding is needed or possible (e.g., radius too small, or rect too small for diameter)
  // Also check if Rect.Width or Rect.Height is less than LDiameter, which would make arcs impossible.
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

  // Top-Left corner
  if RoundTL then
    APath.AddArc(Rect.X, Rect.Y, LDiameter, LDiameter, 180, 90)
  else
    APath.AddLine(Rect.X, Rect.Y, Rect.X, Rect.Y); // Start point for line

  // Top edge
  APath.AddLine(Rect.X + LRadius, Rect.Y, Rect.X + Rect.Width - LRadius, Rect.Y);

  // Top-Right corner
  if RoundTR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y, LDiameter, LDiameter, 270, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y);

  // Right edge
  APath.AddLine(Rect.X + Rect.Width, Rect.Y + LRadius, Rect.X + Rect.Width, Rect.Y + Rect.Height - LRadius);

  // Bottom-Right corner
  if RoundBR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 0, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height, Rect.X + Rect.Width, Rect.Y + Rect.Height);

  // Bottom edge
  APath.AddLine(Rect.X + Rect.Width - LRadius, Rect.Y + Rect.Height, Rect.X + LRadius, Rect.Y + Rect.Height);

  // Bottom-Left corner
  if RoundBL then
    APath.AddArc(Rect.X, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 90, 90)
  else
    APath.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y + Rect.Height);

  APath.CloseFigure;
end;

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

{ TANDMR_CEdit }
procedure TANDMR_CEdit.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); // Corrected signature
var
  WorkArea: TRect; ImgW, ImgH, SepW: Integer;
  CurrentX: Integer; 
  CurrentX_End: Integer;
begin
  WorkArea := Self.ClientRect; InflateRect(WorkArea, -FBorderThickness, -FBorderThickness); // Available content area
  outImgRect := Rect(0,0,0,0); outSepRect := Rect(0,0,0,0); outTxtRect := WorkArea;
  ImgW := 0; ImgH := 0;
  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin ImgW := FImage.Graphic.Width; ImgH := FImage.Graphic.Height; end;
  SepW := 0;
  if FSeparatorVisible and (FSeparatorThickness > 0) then SepW := FSeparatorThickness;

  // --- Horizontal Layout (Refined Logic) ---
  if FImageVisible and (ImgW > 0) then
  begin
    if FImagePosition = ipsLeft then
    begin
      outImgRect.Left := WorkArea.Left + FImageMargins.Left;
      outImgRect.Right := outImgRect.Left + ImgW;
      CurrentX := outImgRect.Right + FImageMargins.Right;
      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Left := CurrentX + FSeparatorPadding;
        outSepRect.Right := outSepRect.Left + SepW;
        CurrentX := outSepRect.Right + FSeparatorPadding;
      end;
      outTxtRect.Left := CurrentX; 
    end
    else // ipsRight
    begin
      outImgRect.Right := WorkArea.Right - FImageMargins.Right;
      outImgRect.Left := outImgRect.Right - ImgW;
      CurrentX_End := outImgRect.Left - FImageMargins.Left;
      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Right := CurrentX_End - FSeparatorPadding;
        outSepRect.Left := outSepRect.Right - SepW;
        CurrentX_End := outSepRect.Left - FSeparatorPadding;
      end;
      outTxtRect.Right := CurrentX_End; 
    end;
  end
  else if FSeparatorVisible and (SepW > 0) then 
  begin
    outSepRect.Left := WorkArea.Left + FSeparatorPadding;
    outSepRect.Right := outSepRect.Left + SepW;
    outTxtRect.Left := outSepRect.Right + FSeparatorPadding;
  end;
  // --- End of Refined Horizontal Layout ---

  // --- Vertical Layout ---
  if FImageVisible and (ImgW > 0) then
  begin
    var AvailHForImgLayout: Integer; AvailHForImgLayout := WorkArea.Height - FImageMargins.Top - FImageMargins.Bottom; AvailHForImgLayout := Max(0, AvailHForImgLayout);
    case FImageAlignment of
      iavTop:    outImgRect.Top := WorkArea.Top + FImageMargins.Top;
      iavCenter: outImgRect.Top := WorkArea.Top + FImageMargins.Top + (AvailHForImgLayout - ImgH) div 2;
      iavBottom: outImgRect.Top := WorkArea.Bottom - FImageMargins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;
    if outImgRect.Top < WorkArea.Top + FImageMargins.Top then outImgRect.Top := WorkArea.Top + FImageMargins.Top;
    if outImgRect.Bottom > WorkArea.Bottom - FImageMargins.Bottom then outImgRect.Bottom := WorkArea.Bottom - FImageMargins.Bottom;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  end;
  outTxtRect.Top := WorkArea.Top; outTxtRect.Bottom := WorkArea.Bottom;
  if FSeparatorVisible and (SepW > 0) then
  begin
    var SepH: Integer; var RefTop, RefHeight: Integer; outSepRect.Top := WorkArea.Top; SepH := WorkArea.Height;
    case FSeparatorHeightMode of
      shmFull: begin end;
      shmAsText: begin RefTop := outTxtRect.Top; RefHeight := outTxtRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmAsImage: if FImageVisible and (ImgW > 0) and (outImgRect.Height > 0) then begin RefTop := outImgRect.Top; RefHeight := outImgRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmCustom: begin if FSeparatorCustomHeight > 0 then SepH := FSeparatorCustomHeight else SepH := WorkArea.Height; outSepRect.Top := WorkArea.Top + (WorkArea.Height - SepH) div 2; end;
    end;
    outSepRect.Bottom := outSepRect.Top + SepH;
    if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top;
    if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
    if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
  end;
  // --- End of Vertical Layout ---

  // Final safety checks
  if outTxtRect.Left < WorkArea.Left then outTxtRect.Left := WorkArea.Left; if outTxtRect.Right > WorkArea.Right then outTxtRect.Right := WorkArea.Right; if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;
  if outImgRect.Left < WorkArea.Left then outImgRect.Left := WorkArea.Left; if outImgRect.Right > WorkArea.Right then outImgRect.Right := WorkArea.Right; if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
  if outSepRect.Left < WorkArea.Left then outSepRect.Left := WorkArea.Left; if outSepRect.Right > WorkArea.Right then outSepRect.Right := WorkArea.Right; if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left;
  if outTxtRect.Top < WorkArea.Top then outTxtRect.Top := WorkArea.Top; if outTxtRect.Bottom > WorkArea.Bottom then outTxtRect.Bottom := WorkArea.Bottom; if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
  if outImgRect.Top < WorkArea.Top then outImgRect.Top := WorkArea.Top; if outImgRect.Bottom > WorkArea.Bottom then outImgRect.Bottom := WorkArea.Bottom; if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top; if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom; if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
end;

procedure TANDMR_CEdit.SetImagePlacement(const Value: TImagePlacement); begin if FImagePlacement <> Value then begin FImagePlacement := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageDrawMode(const Value: TImageDrawMode); begin if FImageDrawMode <> Value then begin FImageDrawMode := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorVisible(const Value: Boolean); begin if FSeparatorVisible <> Value then begin FSeparatorVisible := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorColor(const Value: TColor); begin if FSeparatorColor <> Value then begin FSeparatorColor := Value; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorThickness(const Value: Integer); var ValidThickness: Integer; begin ValidThickness := Max(0, Value); if FSeparatorThickness <> ValidThickness then begin FSeparatorThickness := ValidThickness; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorPadding(const Value: Integer); var ValidPadding: Integer; begin ValidPadding := Max(0, Value); if FSeparatorPadding <> ValidPadding then begin FSeparatorPadding := ValidPadding; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorHeightMode(const Value: TSeparatorHeightMode); begin if FSeparatorHeightMode <> Value then begin FSeparatorHeightMode := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorCustomHeight(const Value: Integer); var ValidHeight: Integer; begin ValidHeight := Max(0, Value); if FSeparatorCustomHeight <> ValidHeight then begin FSeparatorCustomHeight := ValidHeight; if FSeparatorHeightMode = shmCustom then Invalidate; end; end;

procedure TANDMR_CEdit.SetImage(const Value: TPicture); begin FImage.Assign(Value); Invalidate; end;
procedure TANDMR_CEdit.SetImageVisible(const Value: Boolean); begin if FImageVisible <> Value then begin FImageVisible := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImagePosition(const Value: TImagePositionSide); begin if FImagePosition <> Value then begin FImagePosition := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageAlignment(const Value: TImageAlignmentVertical); begin if FImageAlignment <> Value then begin FImageAlignment := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageMargins(const Value: TImageMarginsControl); begin FImageMargins.Assign(Value); Invalidate; end;
procedure TANDMR_CEdit.ImageChanged(Sender: TObject); begin Invalidate; end;
procedure TANDMR_CEdit.ImageMarginsChanged(Sender: TObject); begin Invalidate; end;

procedure TANDMR_CEdit.DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
var DrawImageRect: TRect; GraphicW, GraphicH: Integer; rRatio, rRectRatio: Double; PngStream: TMemoryStream; GpSourceBitmap: TGPBitmap; Adapter: IStream;
begin
  if (AGraphics = nil) or (APNG = nil) or (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then Exit;
  GraphicW := APNG.Width; GraphicH := APNG.Height; if (GraphicW <= 0) or (GraphicH <= 0) then Exit;
  case ADrawMode of
    idmStretch: DrawImageRect := ADestRect;
    idmProportional: begin rRatio := GraphicW / GraphicH; if ADestRect.Height = 0 then rRectRatio := MaxDouble else rRectRatio := ADestRect.Width / ADestRect.Height; if rRectRatio > rRatio then begin DrawImageRect.Height := ADestRect.Height; DrawImageRect.Width := Round(ADestRect.Height * rRatio); end else begin DrawImageRect.Width := ADestRect.Width; if rRatio = 0 then DrawImageRect.Height := 0 else DrawImageRect.Height := Round(ADestRect.Width / rRatio); end; DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - DrawImageRect.Width) div 2; DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - DrawImageRect.Height) div 2; DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width; DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height; end;
    idmNormal: begin DrawImageRect.Width := GraphicW; DrawImageRect.Height := GraphicH; DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - GraphicW) div 2; DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - GraphicH) div 2; DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width; DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height; end;
  else DrawImageRect := ADestRect; end;
  if (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0) then Exit;
  PngStream := TMemoryStream.Create; try APNG.SaveToStream(PngStream); PngStream.Position := 0; Adapter := TStreamAdapter.Create(PngStream, soReference); GpSourceBitmap := TGPBitmap.Create(Adapter); try if (DrawImageRect.Width <> GpSourceBitmap.GetWidth()) or (DrawImageRect.Height <> GpSourceBitmap.GetHeight()) then AGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic) else AGraphics.SetInterpolationMode(InterpolationModeDefault); AGraphics.DrawImage(GpSourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height); finally GpSourceBitmap.Free; end; finally PngStream.Free; end;
end;

procedure TANDMR_CEdit.DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
var DrawImageRect: TRect; GraphicW, GraphicH: Integer; rRatio, rRectRatio: Double;
begin
  if (ACanvas = nil) or (AGraphic = nil) or (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then Exit;
  GraphicW := AGraphic.Width; GraphicH := AGraphic.Height; if (GraphicW <= 0) or (GraphicH <= 0) then Exit;
  case ADrawMode of
    idmStretch: DrawImageRect := ADestRect;
    idmProportional: begin rRatio := GraphicW / GraphicH; if ADestRect.Height = 0 then rRectRatio := MaxDouble else rRectRatio := ADestRect.Width / ADestRect.Height; if rRectRatio > rRatio then begin DrawImageRect.Height := ADestRect.Height; DrawImageRect.Width := Round(ADestRect.Height * rRatio); end else begin DrawImageRect.Width := ADestRect.Width; if rRatio = 0 then DrawImageRect.Height := 0 else DrawImageRect.Height := Round(ADestRect.Width / rRatio); end; DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - DrawImageRect.Width) div 2; DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - DrawImageRect.Height) div 2; DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width; DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height; end;
    idmNormal: begin DrawImageRect.Width := GraphicW; DrawImageRect.Height := GraphicH; DrawImageRect.Left := ADestRect.Left + (ADestRect.Width - GraphicW) div 2; DrawImageRect.Top := ADestRect.Top + (ADestRect.Height - GraphicH) div 2; DrawImageRect.Right := DrawImageRect.Left + DrawImageRect.Width; DrawImageRect.Bottom := DrawImageRect.Top + DrawImageRect.Height; end;
  else DrawImageRect := ADestRect; end;
  if (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0) then Exit;
  if ADrawMode = idmNormal then ACanvas.Draw(DrawImageRect.Left, DrawImageRect.Top, AGraphic)
  else ACanvas.StretchDraw(DrawImageRect, AGraphic);
end;

procedure TANDMR_CEdit.DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
var LineX: Integer;
begin
  if (ACanvas = nil) or (AThickness <= 0) or (ASepRect.Width <= 0) or (ASepRect.Height <= 0) then Exit;
  LineX := ASepRect.Left + ASepRect.Width div 2; ACanvas.Pen.Color := AColor; ACanvas.Pen.Width := AThickness; ACanvas.Pen.Style := psSolid; ACanvas.MoveTo(LineX, ASepRect.Top); ACanvas.LineTo(LineX, ASepRect.Bottom);
end;

procedure TANDMR_CEdit.DrawEditBox(const ADrawArea: TRect; AGraphics: TGPGraphics; ABackgroundColor: TColor; ABorderColor: TColor);
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

  LBorderThicknessValue := Self.FBorderThickness; // Use the actual field value for consistency

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
  LRadiusValue := Self.FCornerRadius; // Use Self consistently
  LRadiusValue := Min(LRadiusValue, LRectF.Width / 2.0);
  LRadiusValue := Min(LRadiusValue, LRectF.Height / 2.0);
  LRadiusValue := Max(0.0, LRadiusValue); // Ensure non-negative radius

  LPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LPath, LRectF, LRadiusValue, Self.FRoundCornerType); // Use Self

    if LPath.GetPointCount > 0 then // Check if path creation was successful
    begin
      // Fill Background
      if ABackgroundColor <> clNone then
      begin
        // Use Self.FOpacity for the alpha component of the background color
        LBrush := TGPSolidBrush.Create(ColorToARGB(ABackgroundColor, Self.FOpacity));
        try
          AGraphics.FillPath(LBrush, LPath);
        finally
          LBrush.Free;
        end;
      end;

      // Draw Border
      if (LBorderThicknessValue > 0) and (ABorderColor <> clNone) and (Self.FBorderStyle <> psClear) then // Use Self
      begin
        // Use Self.FOpacity for the alpha component of the border color
        LPen := TGPPen.Create(ColorToARGB(ABorderColor, Self.FOpacity), LBorderThicknessValue);
        try
          case Self.FBorderStyle of // Use Self
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

constructor TANDMR_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  DoubleBuffered := True; Width := 150; Height := 25; TabStop := True; FText := ''; FMaxLength := 0; FPasswordChar := #0; FReadOnly := False;
  FCornerRadius := 8; FRoundCornerType := rctAll; FActiveColor := clHighlight; FInactiveColor := clBtnFace; FBorderColor := clBlack; FBorderThickness := 1; FBorderStyle := psSolid;
  Font.Name := 'Segoe UI'; Font.Size := 9; Font.Color := clWindowText;
  FImage := TPicture.Create; FImage.OnChange := Self.ImageChanged;
  FImageMargins := TImageMarginsControl.Create; FImageMargins.OnChange := Self.ImageMarginsChanged;
  FImageVisible := True; FImagePosition := ipsLeft; FImageAlignment := iavCenter;
  FImagePlacement := iplInsideBounds; FImageDrawMode := idmProportional;
  FSeparatorVisible := False; FSeparatorColor := clGrayText; FSeparatorThickness := 1; FSeparatorPadding := 2; FSeparatorHeightMode := shmFull; FSeparatorCustomHeight := 0;
  FCaretVisible := False; FCaretPosition := 0; FCaretTimer := TTimer.Create(Self); FCaretTimer.Interval := GetCaretBlinkTime; FCaretTimer.OnTimer := CaretTimerTick; FCaretTimer.Enabled := False;

  // Initialize new properties
  FFocusBorderColorVisible := False;
  FFocusBorderColor := clBlack;
  FFocusBackgroundColorVisible := False;
  FFocusBackgroundColor := clWindow;
  FFocusUnderlineVisible := False;
  FFocusUnderlineColor := clBlack;
  FFocusUnderlineThickness := 1;
  FFocusUnderlineStyle := psSolid;
  FOpacity := 255;
  FCurrentCursor := crIBeam;
  Self.Cursor := FCurrentCursor; // Set initial cursor
end;

destructor TANDMR_CEdit.Destroy;
begin
  FCaretTimer.Free; if Assigned(FImage) then begin FImage.OnChange := nil; FImage.Free; end; FImageMargins.Free;
  inherited Destroy;
end;

procedure TANDMR_CEdit.SetText(const Value: string);
var OldText: string;
begin OldText := FText; if FText <> Value then begin FText := Value; FCaretPosition := Length(FText); FCaretVisible := True; if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end; Invalidate; if Assigned(FOnChange) and (OldText <> FText) then FOnChange(Self); end;
end;

procedure TANDMR_CEdit.SetMaxLength(const Value: Integer);
var OldText: string; TextChanged: Boolean;
begin if FMaxLength <> Value then begin FMaxLength := Max(0, Value); TextChanged := False; OldText := FText; if (FMaxLength > 0) and (Length(FText) > FMaxLength) then begin FText := Copy(FText, 1, FMaxLength); if FCaretPosition > Length(FText) then FCaretPosition := Length(FText); TextChanged := True; end; if TextChanged then begin FCaretVisible := True; if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end; if Assigned(FOnChange) then FOnChange(Self); Invalidate; end; end;
end;

procedure TANDMR_CEdit.SetPasswordChar(const Value: Char); begin if FPasswordChar <> Value then begin FPasswordChar := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetReadOnly(const Value: Boolean); begin if FReadOnly <> Value then begin FReadOnly := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetCornerRadius(const Value: Integer); begin if FCornerRadius <> Value then begin FCornerRadius := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetRoundCornerType(const Value: TRoundCornerType); begin if FRoundCornerType <> Value then begin FRoundCornerType := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetActiveColor(const Value: TColor); begin if FActiveColor <> Value then begin FActiveColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetInactiveColor(const Value: TColor); begin if FInactiveColor <> Value then begin FInactiveColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderThickness(const Value: Integer); begin if FBorderThickness <> Value then begin FBorderThickness := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderStyle(const Value: TPenStyle); begin if FBorderStyle <> Value then begin FBorderStyle := Value; Invalidate; end; end;

// Implementation of new Set methods
procedure TANDMR_CEdit.SetFocusBorderColor(const Value: TColor);
begin
  if FFocusBorderColor <> Value then
  begin
    FFocusBorderColor := Value;
    if FFocusBorderColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBorderColorVisible(const Value: Boolean);
begin
  if FFocusBorderColorVisible <> Value then
  begin
    FFocusBorderColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBackgroundColor(const Value: TColor);
begin
  if FFocusBackgroundColor <> Value then
  begin
    FFocusBackgroundColor := Value;
    if FFocusBackgroundColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBackgroundColorVisible(const Value: Boolean);
begin
  if FFocusBackgroundColorVisible <> Value then
  begin
    FFocusBackgroundColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineColor(const Value: TColor);
begin
  if FFocusUnderlineColor <> Value then
  begin
    FFocusUnderlineColor := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineVisible(const Value: Boolean);
begin
  if FFocusUnderlineVisible <> Value then
  begin
    FFocusUnderlineVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineThickness(const Value: Integer);
var ValidThickness: Integer;
begin
  ValidThickness := Max(0, Value); // Ensure non-negative
  if FFocusUnderlineThickness <> ValidThickness then
  begin
    FFocusUnderlineThickness := ValidThickness;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineStyle(const Value: TPenStyle);
begin
  if FFocusUnderlineStyle <> Value then
  begin
    FFocusUnderlineStyle := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    // Handle csOpaque style based on opacity
    if FOpacity < 255 then
    begin
      ControlStyle := ControlStyle - [csOpaque];
      if Parent <> nil then Parent.Invalidate; // Invalidate parent to redraw background
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque];
    end;
    Invalidate; // Invalidate self to redraw with new opacity
  end;
end;

procedure TANDMR_CEdit.SetCurrentCursor(const Value: TCursor);
begin
  if FCurrentCursor <> Value then
  begin
    FCurrentCursor := Value;
    Self.Cursor := FCurrentCursor; // Update the actual cursor
  end;
end;

procedure TANDMR_CEdit.Paint;
var
  LG: TGPGraphics;
  TextToDisplay: string;
  TextFlags: Cardinal;
  imgR, txtR, sepR: TRect; 
  EditBoxBGColor, EditBoxBorderColor, OverallBGColor: TColor;
  RectToDrawEditBoxIn: TRect;
  PaddedTextDrawArea: TRect;
  InternalTextPaddingX, InternalTextPaddingY: Integer;
begin
  CalculateLayout(imgR, txtR, sepR);

  InternalTextPaddingX := 4;
  InternalTextPaddingY := 2;

  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try // GDI+ operations block
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      // Determine colors
      if csDesigning in ComponentState then
      begin
        EditBoxBGColor := clWhite;
        OverallBGColor := clBtnFace; 
      end
      else
      begin
        // Apply FocusBackgroundColor if visible and focused
        if Self.Focused and FFocusBackgroundColorVisible then
          EditBoxBGColor := FFocusBackgroundColor
        else
          EditBoxBGColor := clWindow; // Default background color
        OverallBGColor := Self.Color;
      end;

      // Apply FocusBorderColor if visible and focused
      if Self.Focused and FFocusBorderColorVisible then
        EditBoxBorderColor := FFocusBorderColor
      else if Self.Focused then // Default focus border color if new one not visible
        EditBoxBorderColor := FActiveColor
      else
        EditBoxBorderColor := FBorderColor; // Default non-focus border color

      if FImagePlacement = iplInsideBounds then
      begin
        RectToDrawEditBoxIn := ClientRect;
        var CompBGColorWhenInside: TColor;
        if csDesigning in ComponentState then CompBGColorWhenInside := clWhite
        else if Self.Focused and FFocusBackgroundColorVisible then CompBGColorWhenInside := FFocusBackgroundColor
        else if Self.Focused then CompBGColorWhenInside := clWindow // Default focus if FFocusBackgroundColorVisible is false
        else CompBGColorWhenInside := FInactiveColor;

        DrawEditBox(RectToDrawEditBoxIn, LG, CompBGColorWhenInside, EditBoxBorderColor);

        if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty and (FImage.Graphic is TPNGImage) then
          Self.DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode);
      end
      else // iplOutsideBounds
      begin
        if csDesigning in ComponentState then OverallBGColor := clBtnFace 
        else OverallBGColor := Self.Color; 
        
        // Apply FOpacity to the overall background clear
        LG.Clear(ColorToARGB(OverallBGColor, Self.FOpacity)); 

        if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty and (FImage.Graphic is TPNGImage) then
          Self.DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode);

        RectToDrawEditBoxIn := txtR;
        // Determine background for edit box when image is outside
        var EditBoxActualBGColor: TColor;
        if csDesigning in ComponentState then EditBoxActualBGColor := clWhite
        else if Self.Focused and FFocusBackgroundColorVisible then EditBoxActualBGColor := FFocusBackgroundColor
        else if Self.Focused then EditBoxActualBGColor := clWindow // default focused if FFocusBackgroundColorVisible is false
        else EditBoxActualBGColor := FInactiveColor; // default non-focused

        DrawEditBox(RectToDrawEditBoxIn, LG, EditBoxActualBGColor, EditBoxBorderColor);
      end;

      // Draw Focus Underline if visible and focused
      if Self.Focused and FFocusUnderlineVisible and (FFocusUnderlineThickness > 0) then
      begin
        var UnderlineY: Integer;
        var UnderlinePen: TGPPen;
        // Corrected Y position for underline, considering border thickness
        if FBorderThickness > 0 then
          UnderlineY := RectToDrawEditBoxIn.Bottom - FBorderThickness - (FFocusUnderlineThickness div 2)
        else
          UnderlineY := RectToDrawEditBoxIn.Bottom - (FFocusUnderlineThickness div 2);
        
        // Ensure underline is drawn just above the bottom edge or border
        UnderlineY := Min(UnderlineY, RectToDrawEditBoxIn.Bottom - FFocusUnderlineThickness);


        UnderlinePen := TGPPen.Create(ColorToARGB(FFocusUnderlineColor, Self.FOpacity), FFocusUnderlineThickness); // Apply Opacity to underline too
        try
          case FFocusUnderlineStyle of
            psSolid: UnderlinePen.SetDashStyle(DashStyleSolid);
            psDash: UnderlinePen.SetDashStyle(DashStyleDash);
            psDot: UnderlinePen.SetDashStyle(DashStyleDot);
            psDashDot: UnderlinePen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: UnderlinePen.SetDashStyle(DashStyleDashDotDot);
            else UnderlinePen.SetDashStyle(DashStyleSolid);
          end;
          // Draw underline across the width of RectToDrawEditBoxIn, respecting its horizontal bounds and border
          LG.DrawLine(UnderlinePen, RectToDrawEditBoxIn.Left + FBorderThickness, UnderlineY, RectToDrawEditBoxIn.Right - FBorderThickness, UnderlineY);
        finally
          UnderlinePen.Free;
        end;
      end;

    finally
      LG.Free;
    end; // End of GDI+ operations

    if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty and not (FImage.Graphic is TPNGImage) then
      Self.DrawNonPNGImageWithCanvas(Canvas, FImage.Graphic, imgR, FImageDrawMode);

    if FSeparatorVisible and (FSeparatorThickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
      Self.DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorColor, FSeparatorThickness);
    
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
      TextToDisplay := FText;

    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;
    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if Length(TextToDisplay) > 0 and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), PaddedTextDrawArea, TextFlags);

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
      Canvas.Pen.Color := Self.Font.Color;
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
  Self.Cursor := FCurrentCursor; // Ensure correct cursor on enter
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  Invalidate; // This will trigger a repaint, which will use focus properties
end;

procedure TANDMR_CEdit.CMExit(var Message: TCMExit);
begin
  inherited; // Call inherited handler for CM_EXIT
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Self.Cursor := crDefault; // Revert cursor on exit
  if Assigned(FOnExit) then
    FOnExit(Self);
  Invalidate; // This will trigger a repaint
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
[end of Source/ANDMR_CEdit.pas]

[end of Source/ANDMR_CEdit.pas]

[end of Source/ANDMR_CEdit.pas]

[end of Source/ANDMR_CEdit.pas]
