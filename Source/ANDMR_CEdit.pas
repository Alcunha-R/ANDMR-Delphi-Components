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

  TInputType = (itNormal, itLettersOnly, itNumbersOnly, itNoSpecialChars, itAlphaNumericOnly);

  TTextCase = (tcNormal, tcUppercase, tcLowercase);

  TCaptionPosition = (cpAbove, cpBelow, cpLeft, cpRight);

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
    FOwnerControl: TWinControl; // To access parent font if needed
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
    constructor Create(AOwner: TWinControl); // Changed AOwnerControl to AOwner
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Text: string read FText write SetText;
    property Position: TCaptionPosition read FPosition write SetPosition default cpAbove;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText; // Changed default
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
    FInputType: TInputType;
    FTextCase: TTextCase;
    FInputMask: string;
    FMaskedText: string;
    FRawText: string;
    FCaptionSettings: TCaptionSettings;
    FCaptionRect: TRect;
    FHoverSettings: THoverSettings; // New Field
    FHovered: Boolean;             // New Field

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
    procedure SetInputType(const Value: TInputType);
    procedure SetTextCase(const Value: TTextCase);
    procedure SetInputMask(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure SetHoverSettings(const Value: THoverSettings); // New Setter
    procedure HoverSettingsChanged(Sender: TObject);        // New Method

    procedure CMMouseEnter(var Message: TCMMouseEnter); message CM_MOUSEENTER; // Added message map
    procedure CMMouseLeave(var Message: TCMMouseLeave); message CM_MOUSELEAVE; // Added message map
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
    property InputType: TInputType read FInputType write SetInputType default itNormal;
    property TextCase: TTextCase read FTextCase write SetTextCase default tcNormal;
    property InputMask: string read FInputMask write SetInputMask;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings; // New Property
  end;

procedure Register;

implementation

uses System.Character; // Added for ToUpper/ToLower

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CEdit]);
end;

// --- Helper Functions (ColorToARGB, CreateGPRoundedPath) ---

{ TCaptionSettings }

constructor TCaptionSettings.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwner; // Store owner
  FVisible := True;
  FText := '';
  FPosition := cpAbove;
  FAlignment := taLeftJustify;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  if Assigned(FOwnerControl) then // Initialize with owner's font if available
    FFont.Assign(FOwnerControl.Font)
  else // Otherwise, set a common default
  begin
    FFont.Name := 'Segoe UI';
    FFont.Size := 9;
  end;
  FColor := clWindowText;
  FOffset := 2;
  FWordWrap := False;
end;

destructor TCaptionSettings.Destroy;
begin
  FFont.OnChange := nil; // Important before freeing
  FFont.Free;
  inherited Destroy;
end;

procedure TCaptionSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCaptionSettings.Assign(Source: TPersistent);
begin
  if Source is TCaptionSettings then
  begin
    FVisible := TCaptionSettings(Source).FVisible;
    FText := TCaptionSettings(Source).FText;
    FPosition := TCaptionSettings(Source).FPosition;
    FAlignment := TCaptionSettings(Source).FAlignment;
    FFont.Assign(TCaptionSettings(Source).FFont); // Font assignment
    FColor := TCaptionSettings(Source).FColor;
    FOffset := TCaptionSettings(Source).FOffset;
    FWordWrap := TCaptionSettings(Source).FWordWrap;
    Changed; // Notify after assigning all
  end
  else
    inherited Assign(Source);
end;

procedure TCaptionSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCaptionSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value); // Font.Assign will trigger its own OnChange if different
  // No direct Changed call here as Font.OnChange handles it via FontChanged -> Changed
end;

procedure TCaptionSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetOffset(const Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

{ THoverSettings }

constructor THoverSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FBackgroundColor := clInfoBk; // Example default, can be clNone or other
  FBorderColor := clHighlight; // Example default
  FFontColor := clInfoText;     // Example default
  FCaptionFontColor := clInfoText; // Example default
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

procedure THoverSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure THoverSettings.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure THoverSettings.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure THoverSettings.SetFontColor(const Value: TColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed;
  end;
end;

procedure THoverSettings.SetCaptionFontColor(const Value: TColor);
begin
  if FCaptionFontColor <> Value then
  begin
    FCaptionFontColor := Value;
    Changed;
  end;
end;

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
procedure TANDMR_CEdit.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
var
  WorkArea: TRect; // This will become the area for the edit control itself
  ImgW, ImgH, SepW: Integer;
  CurrentX, CurrentX_End: Integer;
  FullClientRect: TRect;
  CaptionHeight, CaptionWidth: Integer;
  TempCanvas: TCanvas; // For text size calculation
begin
  FullClientRect := Self.ClientRect;
  FCaptionRect := Rect(0,0,0,0); // Reset caption rect

  if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
  begin
    TempCanvas := TCanvas.Create;
    try
      TempCanvas.Font.Assign(FCaptionSettings.Font);
      // Calculate Caption Size (simplified, assuming single line for width if not wordwrap for height)
      // For WordWrap, TextRect with DT_CALCRECT is better but more complex here.
      // This is a basic estimation.
      CaptionHeight := TempCanvas.TextHeight(FCaptionSettings.Text);
      CaptionWidth := TempCanvas.TextWidth(FCaptionSettings.Text);
      if FCaptionSettings.WordWrap and (FCaptionSettings.Position in [cpAbove, cpBelow]) then
      begin
         var TempRect := Rect(0,0, FullClientRect.Width, 30000); // Assume width of control, large height
         DrawText(TempCanvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRect, DT_CALCRECT or DT_WORDBREAK);
         CaptionHeight := TempRect.Bottom - TempRect.Top;
         CaptionWidth := FullClientRect.Width; // Takes full width when above/below and wordwrap
      end else if FCaptionSettings.WordWrap and (FCaptionSettings.Position in [cpLeft, cpRight]) then
      begin
         var TempRect := Rect(0,0, CaptionWidth, FullClientRect.Height); // Use calculated width, full height for wrap
         DrawText(TempCanvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRect, DT_CALCRECT or DT_WORDBREAK);
         CaptionWidth := TempRect.Right - TempRect.Left; // May change if text is very short
         CaptionHeight := FullClientRect.Height;
      end;


    finally
      TempCanvas.Free;
    end;

    WorkArea := FullClientRect; // Start with full area, then adjust for caption

    case FCaptionSettings.Position of
      cpAbove:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Right, FullClientRect.Top + CaptionHeight);
          WorkArea.Top := FCaptionRect.Bottom + FCaptionSettings.Offset;
        end;
      cpBelow:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Bottom - CaptionHeight, FullClientRect.Right, FullClientRect.Bottom);
          WorkArea.Bottom := FCaptionRect.Top - FCaptionSettings.Offset;
        end;
      cpLeft:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Left + CaptionWidth, FullClientRect.Bottom);
          WorkArea.Left := FCaptionRect.Right + FCaptionSettings.Offset;
        end;
      cpRight:
        begin
          FCaptionRect := Rect(FullClientRect.Right - CaptionWidth, FullClientRect.Top, FullClientRect.Right, FullClientRect.Bottom);
          WorkArea.Right := FCaptionRect.Left - FCaptionSettings.Offset;
        end;
    end;
    // Ensure WorkArea (for edit control) is not inverted
    if WorkArea.Bottom < WorkArea.Top then WorkArea.Bottom := WorkArea.Top;
    if WorkArea.Right < WorkArea.Left then WorkArea.Right := WorkArea.Left;
  end
  else // No caption visible or text is empty
  begin
    WorkArea := FullClientRect; // Edit control uses the full client area
  end;

  // Existing layout logic now operates on 'WorkArea' instead of 'Self.ClientRect' directly
  InflateRect(WorkArea, -FBorderThickness, -FBorderThickness); // Available content area for edit itself
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
  FInputType := itNormal; // Initialize InputType
  FTextCase := tcNormal; // Initialize TextCase
  FInputMask := '';
  FMaskedText := '';
  FRawText := '';
  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FCaptionRect := Rect(0,0,0,0);
  FHoverSettings := THoverSettings.Create; // Create HoverSettings
  FHoverSettings.OnChange := HoverSettingsChanged; // Assign OnChange handler
  FHovered := False; // Initialize FHovered
end;

destructor TANDMR_CEdit.Destroy;
begin
  FHoverSettings.Free; // Free HoverSettings
  FCaptionSettings.Free;
  FCaretTimer.Free; if Assigned(FImage) then begin FImage.OnChange := nil; FImage.Free; end; FImageMargins.Free;
  inherited Destroy;
end;

procedure TANDMR_CEdit.SetText(const Value: string); // Value is considered RAW input
var
  OldFDisplayText: string; // FText will now represent FMaskedText for display
  ProcessedRawText: string; // Raw text after TextCase transformation
  NewMaskedText: string;
  NewUnmaskedText: string; // Corrected FRawText after applying mask
  RawIndex: Integer;
  MaskIndex: Integer;
  MaskChar: Char;
  IsLiteral: Boolean;
  CharToTest: Char;
  CharAllowed: Boolean;
begin
  OldFDisplayText := FText; // FText currently stores the masked text for display

  // 1. Apply TextCase to the incoming raw Value
  ProcessedRawText := Value;
  case FTextCase of
    tcUppercase: ProcessedRawText := System.SysUtils.UpperCase(ProcessedRawText);
    tcLowercase: ProcessedRawText := System.SysUtils.LowerCase(ProcessedRawText);
  end;

  // 2. Rebuild FRawText and FMaskedText based on ProcessedRawText and FInputMask
  NewUnmaskedText := '';
  NewMaskedText := '';

  if FInputMask <> '' then
  begin
    RawIndex := 1;
    for MaskIndex := 1 to Length(FInputMask) do
    begin
      MaskChar := FInputMask[MaskIndex];
      // Define literals as characters not in the set of placeholders
      IsLiteral := not (MaskChar IN ['9', 'L', 'A', '#']); // Assuming '#' is a generic placeholder for now

      if IsLiteral then
      begin
        NewMaskedText := NewMaskedText + MaskChar;
      end
      else // It's a placeholder
      begin
        if RawIndex <= Length(ProcessedRawText) then
        begin
          CharToTest := ProcessedRawText[RawIndex];
          CharAllowed := False;
          case MaskChar of
            '9': CharAllowed := CharToTest IN ['0'..'9'];
            'L': CharAllowed := System.Character.IsLetter(CharToTest);
            'A': CharAllowed := System.Character.IsLetterOrDigit(CharToTest);
            '#': CharAllowed := True; // Example: '#' accepts any char from raw text
          end;

          if CharAllowed then
          begin
            NewMaskedText := NewMaskedText + CharToTest;
            NewUnmaskedText := NewUnmaskedText + CharToTest; // Build the true FRawText
            Inc(RawIndex);
          end
          else // Char from ProcessedRawText doesn't fit mask placeholder
          begin
            NewMaskedText := NewMaskedText + '_'; // Placeholder for invalid/missing char
            // Do not increment RawIndex, as this raw char was not consumed.
            // Or, option: skip this raw char and try next? For now, assume strict adherence.
          end;
        end
        else // No more raw text to fill this placeholder
        begin
          NewMaskedText := NewMaskedText + '_'; // Placeholder for empty part of mask
        end;
      end;
    end;
    FRawText := NewUnmaskedText;
    FText := NewMaskedText; // FText (display text) is the newly built FMaskedText
    FMaskedText := NewMaskedText; // Keep FMaskedText field in sync
  end
  else // No input mask
  begin
    FRawText := ProcessedRawText; // Raw text is just the transformed input value
    FText := FRawText;          // Display text is also this raw text
    FMaskedText := FRawText;    // FMaskedText is same as FRawText when no mask
  end;

  // 3. Update caret and visual state
  //FCaretPosition := Length(FText); // Simplistic caret: end of text. TODO: Smarter caret.
  // For SetText, placing caret at end is a common behavior.
  // If trying to preserve caret: more complex, map old FRawText pos to new FRawText pos, then to FMaskedText pos.
  // For now, set to end of the new FText.
  if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);


  FCaretVisible := True;
  if Focused then
  begin
    FCaretTimer.Enabled := False;
    FCaretTimer.Enabled := True;
  end;

  if OldFDisplayText <> FText then
  begin
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
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

procedure TANDMR_CEdit.SetInputType(const Value: TInputType);
begin
  if FInputType <> Value then
  begin
    FInputType := Value;
    // Future: May need to validate/clear existing text if it doesn't match the new type.
    // For now, just set and let new input be filtered.
    // Invalidate might be useful if visual cues for input type are added later
    // or if immediate validation of existing text is performed.
    // Invalidate; 
  end;
end;

procedure TANDMR_CEdit.SetTextCase(const Value: TTextCase);
var
  OldText: string;
  TransformedText: string;
begin
  if FTextCase <> Value then
  begin
    OldText := FText; // Store original FText before any transformation by this call
    FTextCase := Value; // Assign the new case type

    TransformedText := FText; // Start with current FText for transformation
    case FTextCase of
      tcUppercase: TransformedText := System.SysUtils.UpperCase(FText);
      tcLowercase: TransformedText := System.SysUtils.LowerCase(FText);
      // tcNormal: No transformation needed on existing FText based on this new setting
    end;

    if FText <> TransformedText then // If the text actually changed after applying the new case
    begin
      FText := TransformedText; // Assign the transformed text
      FCaretPosition := Length(FText); // Reset caret to end
      if Assigned(FOnChange) then
         FOnChange(Self); // Text content changed
      Invalidate; // Repaint needed
    end
    else if OldText = TransformedText and Value <> tcNormal then
    begin
      // This case handles if FText was already, e.g., "TEXT" and tcUppercase is set.
      // FText doesn't change, OldText is same as TransformedText.
      // However, the *property* TextCase has changed, which might be relevant for designers
      // or other logic. A simple Invalidate might be too much if no visual change.
      // For now, if text didn't change, only Invalidate if a specific case is enforced
      // and a repaint might be desired by some logic (though not strictly necessary if text is same)
      // No OnChange here as FText content hasn't changed.
      Invalidate; // Or remove if no visual change is expected when text is already correct case.
    end;
  end;
end;

procedure TANDMR_CEdit.SetInputMask(const Value: string);
begin
  if FInputMask <> Value then
  begin
    FInputMask := Value;
    // When mask changes, clear existing text (FRawText, FMaskedText, and FText which is display).
    // SetText('') will handle resetting FRawText and FMaskedText and then FText.
    SetText(''); // This effectively clears FRawText and FMaskedText (via current SetText logic path)
                 // and then updates FText to the empty masked representation.
    FCaretPosition := 0; // Reset caret
    Invalidate;
    // A more advanced implementation might try to reformat FRawText based on the new mask.
  end;
end;

procedure TANDMR_CEdit.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  CaptionSettingsChanged(Self); // Trigger update
end;

procedure TANDMR_CEdit.CaptionSettingsChanged(Sender: TObject);
begin
  // This method is called when a property within FCaptionSettings changes.
  // Need to recalculate layout and repaint.
  // A more specific CalculateLayoutAndInvalidate could be:
  // Self.CalculateLayout(FImgRect, FTxtRect, FSepRect); // Assuming CalculateLayout updates internal fields or similar
  Invalidate; // For now, simple Invalidate. Layout recalculation will happen in Paint or a dedicated method.
  // TODO: Ensure layout is recalculated before painting if caption affects it.
  // For now, Paint will call CalculateLayout which should handle the new FCaptionRect
end;

procedure TANDMR_CEdit.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  HoverSettingsChanged(Self); // Trigger update, typically Invalidate
end;

procedure TANDMR_CEdit.HoverSettingsChanged(Sender: TObject);
begin
  if FHovered or (not FHoverSettings.Enabled) then // Repaint if currently hovered or if hover effect is disabled (to revert)
    Invalidate;
end;

procedure TANDMR_CEdit.CMMouseEnter(var Message: TCMMouseEnter);
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
    if FHoverSettings.Enabled then
      Invalidate;
  end;
end;

procedure TANDMR_CEdit.CMMouseLeave(var Message: TCMMouseLeave);
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
    if FHoverSettings.Enabled then
      Invalidate;
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
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      // --- Determine current state colors ---
      var CurrentEditBGColor, CurrentEditBorderColor, CurrentEditTextColor, CurrentCaptionTextColor: TColor;

      // 1. Start with base colors
      CurrentEditBGColor := FInactiveColor; // Or clWindow as base for edit area
      if FImagePlacement = iplInsideBounds then // If image is inside, the "edit box" is the whole thing
          CurrentEditBGColor := FInactiveColor // This might be Self.Color if FInactiveColor is a specific state
      else // Image outside, edit box has its own background
          CurrentEditBGColor := clWindow; // A common default for the text entry part

      CurrentEditBorderColor := FBorderColor;
      CurrentEditTextColor := Self.Font.Color; // From component's Font property
      CurrentCaptionTextColor := FCaptionSettings.Color;
      if CurrentCaptionTextColor = clDefault then CurrentCaptionTextColor := Self.Font.Color;


      // 2. Apply Hover Settings if enabled and hovered
      if FHovered and FHoverSettings.Enabled then
      begin
        if FHoverSettings.BackgroundColor <> clNone then CurrentEditBGColor := FHoverSettings.BackgroundColor;
        if FHoverSettings.BorderColor <> clNone then CurrentEditBorderColor := FHoverSettings.BorderColor;
        if FHoverSettings.FontColor <> clNone then CurrentEditTextColor := FHoverSettings.FontColor;
        if FHoverSettings.CaptionFontColor <> clNone then CurrentCaptionTextColor := FHoverSettings.CaptionFontColor;
      end;

      // 3. Apply Focus Settings (potentially override Hover, or combine)
      if Self.Focused then
      begin
        if FFocusBorderColorVisible and (FFocusBorderColor <> clNone) then
          CurrentEditBorderColor := FFocusBorderColor
        else if not (FHovered and FHoverSettings.Enabled and (FHoverSettings.BorderColor <> clNone)) then // if not hover border
          CurrentEditBorderColor := FActiveColor; // Default focus border

        if FFocusBackgroundColorVisible and (FFocusBackgroundColor <> clNone) then
          CurrentEditBGColor := FFocusBackgroundColor
        else if not (FHovered and FHoverSettings.Enabled and (FHoverSettings.BackgroundColor <> clNone)) then // if not hover bg
        begin
            if FImagePlacement = iplInsideBounds then CurrentEditBGColor := clWindow // Or a specific focus background
            else CurrentEditBGColor := clWindow; // For the text entry part when image is outside
        end;
        // EditTextColor and CaptionTextColor could also have focused states if desired
      end;

      // --- Painting ---
      // 1. Paint component's main background (covers entire ClientRect)
      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color; // Base color of the control
        Canvas.FillRect(Self.ClientRect);
      end; // If FOpacity < 255, parent's background shows through.

      // RectToDrawEditBoxIn: This is the area for the actual edit box part,
      // EXCLUDING the caption if image is iplOutsideBounds.
      // If image is iplInsideBounds, this is effectively the whole component area (minus caption).
      // This needs to be carefully determined based on CalculateLayout's outputs (txtR mainly for edit box part)
      // For iplInsideBounds, RectToDrawEditBoxIn might be ClientRect adjusted for caption.
      // For iplOutsideBounds, RectToDrawEditBoxIn is txtR (the edit field part).

      var EditBoxAreaToDrawIn: TRect; // The rectangle for DrawEditBox call
      var BackgroundForEditBoxItself: TColor; // The BG color passed to DrawEditBox

      if FImagePlacement = iplInsideBounds then
      begin
        // Edit box is the entire area adjusted for caption
        EditBoxAreaToDrawIn := FullClientRect; // Start with the full client rect
        if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
        begin // Adjust EditBoxAreaToDrawIn for caption
            case FCaptionSettings.Position of
              cpAbove: EditBoxAreaToDrawIn.Top := FCaptionRect.Bottom + FCaptionSettings.Offset;
              cpBelow: EditBoxAreaToDrawIn.Bottom := FCaptionRect.Top - FCaptionSettings.Offset;
              cpLeft:  EditBoxAreaToDrawIn.Left := FCaptionRect.Right + FCaptionSettings.Offset;
              cpRight: EditBoxAreaToDrawIn.Right := FCaptionRect.Left - FCaptionSettings.Offset;
            end;
        end;
        BackgroundForEditBoxItself := CurrentEditBGColor; // This BG is for the entire component area under edit controls
        DrawEditBox(EditBoxAreaToDrawIn, LG, BackgroundForEditBoxItself, CurrentEditBorderColor);
        if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty and (FImage.Graphic is TPNGImage) then
          Self.DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode);
      end
      else // iplOutsideBounds: Image is outside, text rect (txtR) is the edit box
      begin
         // OverallBGColor (for LG.Clear) should be Self.Color if painting the whole component background,
         // or a specific color for the area *around* the edit box if that's different.
         // The LG.Clear call was here: LG.Clear(ColorToARGB(OverallBGColor, Self.FOpacity));
         // This needs to be handled by the initial fill if FOpacity = 255.
         // If FOpacity < 255, the parent draws.
         // So, no specific LG.Clear here if initial fill is done.

        if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty and (FImage.Graphic is TPNGImage) then
          Self.DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode);

        EditBoxAreaToDrawIn := txtR; // txtR is calculated by CalculateLayout to be the edit field
        BackgroundForEditBoxItself := CurrentEditBGColor; // BG for the text entry field itself
        DrawEditBox(EditBoxAreaToDrawIn, LG, BackgroundForEditBoxItself, CurrentEditBorderColor);
      end;

      // Draw Focus Underline (uses CurrentEditBorderColor or a specific FFocusUnderlineColor)
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
    Canvas.Font.Color := CurrentEditTextColor; // Apply determined font color
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

    // --- Draw Caption ---
    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionRect.Width > 0) and (FCaptionRect.Height > 0) then
    begin
      Canvas.Font.Assign(FCaptionSettings.Font);
      if FCaptionSettings.Color = clDefault then
        Canvas.Font.Color := Self.Font.Color // Or a specific default caption text color
      else
        Canvas.Font.Color := FCaptionSettings.Color;
      Canvas.Brush.Style := bsClear; // Transparent background for text

      var CaptionDrawFlags: Cardinal;
      CaptionDrawFlags := DT_NOPREFIX; // Basic flag

      if FCaptionSettings.WordWrap then
        CaptionDrawFlags := CaptionDrawFlags or DT_WORDBREAK;

      // Horizontal Alignment for DrawText
      // Note: DT_CENTER, DT_RIGHT for DrawText apply to single line or within each line of word-wrap.
      // For true block alignment with TextRect, TAlignment is used directly.
      case FCaptionSettings.Alignment of
        taLeftJustify: CaptionDrawFlags := CaptionDrawFlags or DT_LEFT;
        taCenter: CaptionDrawFlags := CaptionDrawFlags or DT_CENTER;
        taRightJustify: CaptionDrawFlags := CaptionDrawFlags or DT_RIGHT;
      end;
      
      // Vertical Alignment consideration for cpLeft/cpRight (simplified for DrawText)
      // DT_VCENTER might be useful if drawing single line in a taller rect.
      // For multi-line word-wrapped text, vertical centering is more complex with DrawText.
      // TextRect handles this better.
      if FCaptionSettings.Position in [cpLeft, cpRight] then
         CaptionDrawFlags := CaptionDrawFlags or DT_VCENTER or DT_SINGLELINE; // If not word wrapping
      else if not FCaptionSettings.WordWrap then // For cpAbove, cpBelow if not word wrapping
         CaptionDrawFlags := CaptionDrawFlags or DT_VCENTER; // Center vertically if single line

      // Create a mutable copy of FCaptionRect for DrawText if needed
      var TempCaptionDrawRect := FCaptionRect;
      Canvas.Font.Assign(FCaptionSettings.Font); // Set font for caption
      Canvas.Font.Color := CurrentCaptionTextColor; // Apply determined caption font color
      DrawText(Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempCaptionDrawRect, CaptionDrawFlags);
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
var
  TempText: string;
  OriginalFText: string;
begin
  OriginalFText := FText; // Store FText before CMExit transformations

  // Apply final transformation before exiting focus
  if FTextCase <> tcNormal then // Only transform if a specific case is set
  begin
    TempText := FText;
    case FTextCase of
      tcUppercase: TempText := System.SysUtils.UpperCase(FText);
      tcLowercase: TempText := System.SysUtils.LowerCase(FText);
    end;
    if FText <> TempText then
    begin
       FText := TempText; // Update FText directly
       // FCaretPosition might need adjustment, e.g. to Length(FText) or try to preserve
       FCaretPosition := Length(FText);
       if Assigned(FOnChange) and (OriginalFText <> FText) then // Fire OnChange if text actually changed
         FOnChange(Self);
       // No need to call Invalidate here if the inherited CMExit or subsequent Invalidate will handle it
       // However, if FText changed, an Invalidate is good.
    end;
  end;

  inherited; // Call inherited handler for CM_EXIT (which might Invalidate)
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Self.Cursor := crDefault; // Revert cursor on exit

  if Assigned(FOnExit) then
    FOnExit(Self);

  // Invalidate if text changed or if default CMExit doesn't always cover it
  if OriginalFText <> FText then
    Invalidate
  else
    Invalidate; // Standard CMExit practice to repaint for focus change
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
        if FInputMask <> '' then
        begin
          if Length(FRawText) > 0 then
          begin
            FRawText := Copy(FRawText, 1, Length(FRawText) - 1);
            // Call SetText with the new FRawText to rebuild FMaskedText (which becomes FText)
            // and handle caret, invalidation, and OnChange.
            // Store current FText (display) to check if it changes, to prevent unnecessary OnChange/Invalidate.
            var OldDisplayText: string := FText;
            SetText(FRawText); // This will update FText (display), FMaskedText, and FRawText internally.
                               // It also handles TextCase.
            
            // Attempt to position caret intelligently after backspace
            // A simple approach: position before the last character of the new FRawText within FMaskedText
            var NewCaretPosInMask: Integer := 0;
            var TempRawLen: Integer := 0;
            var MaskIdx: Integer;
            for MaskIdx := 1 to Length(FMaskedText) do
            begin
              if not (FInputMask[MaskIdx] IN ['9','L','A','#']) then // It's a literal
              begin
                Inc(NewCaretPosInMask);
              end
              else // It's a placeholder
              begin
                if TempRawLen < Length(FRawText) then
                begin
                  Inc(TempRawLen);
                  Inc(NewCaretPosInMask);
                end
                else if TempRawLen = Length(FRawText) then // This is the first placeholder after the new raw text
                begin
                   Inc(NewCaretPosInMask); // Caret should be at this placeholder
                   Break;
                end
                else // Should not happen if FMaskedText is correctly built
                begin
                   Inc(NewCaretPosInMask);
                   Break;
                end;
              end;
               if MaskIdx = Length(FMaskedText) then NewCaretPosInMask := Length(FMaskedText); // If loop finishes, caret at end
            end;
            FCaretPosition := NewCaretPosInMask;
            if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);


            if OldDisplayText <> FText then Changed := True else Changed := False; // Set Changed based on actual display text change
          end;
          Key := 0; // Mark key as handled
          Exit;     // Exit because we've handled it here
        end
        else // No input mask, standard backspace logic
        begin
          if FCaretPosition > 0 then
          begin
            FText := Copy(FText, 1, FCaretPosition - 1) + Copy(FText, FCaretPosition + 1, MaxInt);
            Dec(FCaretPosition);
            Changed := True;
          end;
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
  AllowChar: Boolean; // Added for filtering
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

  // New InputType filtering logic
  // This filter applies primarily to printable characters.
  // Control characters (like Tab, Enter, Esc, etc.) are generally expected to either
  // be handled in KeyDown or allowed through KeyPress if they are not printable.
  if (FInputType <> itNormal) and (Key >= ' ') then // Filter only printable chars
  begin
    AllowChar := True; // Assume allowed, then restrict
    case FInputType of
      itLettersOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z'];
      itNumbersOnly: AllowChar := Key IN ['0'..'9'];
      itAlphaNumericOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9'];
      itNoSpecialChars: // Example: Alphanumeric + Space. Customize as needed.
        AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9', ' '];
      // itNormal is handled by falling through, all chars allowed by default.
    end;
    if not AllowChar then
    begin
      Key := #0; // Disallow character by setting it to null char
      // Do not Exit here, let Key := #0 be handled by the end of the procedure
    end;
  end;

  // Existing logic for handling printable characters (MaxLength, inserting char, etc.)
  // If Key was set to #0 by the filter, it will not satisfy (Key >= ' ')
  if (Key >= ' ') then // Process if Key is still a printable char (i.e., not #0 from filter)
  begin
    // Apply TextCase transformation to the incoming character
    case FTextCase of
      tcUppercase: Key := System.Character.ToUpper(Key);
      tcLowercase: Key := System.Character.ToLower(Key);
    end;

    // If InputMask is active, handle through mask logic
    if FInputMask <> '' then
    begin
      var MaskPlaceholdersCount: Integer := 0;
      var i: Integer;
      for i := 1 to Length(FInputMask) do
        if FInputMask[i] IN ['9','L','A','#'] then Inc(MaskPlaceholdersCount);

      if Length(FRawText) >= MaskPlaceholdersCount then
      begin
         Key := #0; // Mask is full, cannot add more characters to FRawText
         Exit;
      end;

      // Tentatively add the new character to FRawText and rebuild FMaskedText
      var TempRawText: string := FRawText + Key;
      var BuildRawText: string := '';
      var BuildMaskedText: string := '';
      var RawIdx: Integer := 1;
      var MaskIdx: Integer;
      var MaskDefChar: Char;
      var IsLit: Boolean;
      var CharToIns: Char;
      var CharOK: Boolean;

      for MaskIdx := 1 to Length(FInputMask) do
      begin
        MaskDefChar := FInputMask[MaskIdx];
        IsLit := not (MaskDefChar IN ['9', 'L', 'A', '#']);
        if IsLit then
        begin
          BuildMaskedText := BuildMaskedText + MaskDefChar;
        end
        else // Placeholder
        begin
          if RawIdx <= Length(TempRawText) then
          begin
            CharToIns := TempRawText[RawIdx];
            CharOK := False;
            case MaskDefChar of
              '9': CharOK := CharToIns IN ['0'..'9'];
              'L': CharOK := System.Character.IsLetter(CharToIns);
              'A': CharOK := System.Character.IsLetterOrDigit(CharToIns);
              '#': CharOK := True;
            end;

            if CharOK then
            begin
              BuildMaskedText := BuildMaskedText + CharToIns;
              BuildRawText := BuildRawText + CharToIns;
              Inc(RawIdx);
            end
            else // Invalid char for this specific mask placeholder
            begin
              Key := #0; // Signal that the key was invalid for the mask
              Exit;    // Stop processing this key press
            end;
          end
          else // Still placeholders in mask, but no more raw text (including the new Key)
          begin
            // This part might be reached if TempRawText was shorter than expected,
            // which shouldn't happen if CharOK logic is correct.
            // Or, it means the mask expects more chars than TempRawText provides.
            // Add placeholder to BuildMaskedText.
            BuildMaskedText := BuildMaskedText + '_';
          end;
        end;
      end;

      // If Key is still valid (#0 would mean it was rejected by mask validation)
      if Key <> #0 then
      begin
        OldText := FText; // FText holds the old FMaskedText
        FRawText := BuildRawText;
        FText := BuildMaskedText; // Update FText to new FMaskedText
        FMaskedText := BuildMaskedText; // Sync FMaskedText field
        FCaretPosition := Length(FText); // TODO: Smarter caret after insert
        
        FCaretVisible := True;
        if Focused then
        begin
          FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
        end;
        if OldText <> FText then
        begin
          Invalidate;
          if Assigned(FOnChange) then FOnChange(Self);
        end;
      end;
      Key := #0; // Mark Key as handled by mask logic
      Exit; // Exit KeyPress as mask logic has processed it
    end
    else // No InputMask, proceed with normal text insertion
    begin
      OldText := FText;
      if (FMaxLength > 0) and (Length(FText) >= FMaxLength) then
      begin
        Exit; // MaxLength reached
      end;

      if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);
      FText := Copy(FText, 1, FCaretPosition) + Key + Copy(FText, FCaretPosition + 1, MaxInt);
      Inc(FCaretPosition);

      FCaretVisible := True;
      if Focused then
      begin
        FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
      end;
      if OldText <> FText then
      begin
        Invalidate;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end;
  end;
  Key := #0; // Mark key as handled if it wasn't a printable char or was handled above.
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
