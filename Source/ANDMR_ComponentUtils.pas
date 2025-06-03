unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.UITypes, Vcl.ExtCtrls, System.Types; // Ensure GDIP units are here, Added Vcl.ExtCtrls for TTimer, Added System.Types for TPoint

type
  THoverEffect = (heNone, heFade, heScale);

  TCaptionVerticalAlignment = (cvaTop, cvaCenter, cvaBottom);

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

  TANDMR_Margins = class(TPersistent)
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
    FWordWrap: Boolean;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl;
    FVerticalAlignment: TCaptionVerticalAlignment;
    FOffset: TPoint; // Changed from FOffsetX, FOffsetY
    FDisabledColor: TColor;

    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetPosition(const Value: TCaptionPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetVerticalAlignment(const Value: TCaptionVerticalAlignment);
    procedure SetOffset(const Value: TPoint); // Changed from SetOffsetX, SetOffsetY
    procedure SetDisabledColor(const Value: TColor);
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
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify; // Horizontal Alignment
    property VerticalAlignment: TCaptionVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default cvaCenter;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGrayText;
    property Offset: TPoint read FOffset write SetOffset; // Changed from OffsetX, OffsetY. Default for TPoint is (0,0)
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

    FHoverEffect: THoverEffect;
    FAnimationTimerInterval: Integer;
    FAnimationStep: Integer;
    FAnimationTimer: TTimer;
    FCurrentAnimationValue: Integer;
    FAnimationDirection: Integer;
    FOwnerControl: TWinControl;
    FOnAnimationProgress: TNotifyEvent;

    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetCaptionFontColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetHoverEffect(const Value: THoverEffect);
    procedure SetAnimationTimerInterval(const Value: Integer);
    procedure SetAnimationStep(const Value: Integer);
    procedure DoAnimate(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwnerControl: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure StartAnimation(IsHovering: Boolean);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clSkyBlue;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clHighlight;
    property FontColor: TColor read FFontColor write SetFontColor default clBlack;
    property CaptionFontColor: TColor read FCaptionFontColor write SetCaptionFontColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property HoverEffect: THoverEffect read FHoverEffect write SetHoverEffect default heFade;
    property AnimationTimerInterval: Integer read FAnimationTimerInterval write SetAnimationTimerInterval default 15;
    property AnimationStep: Integer read FAnimationStep write SetAnimationStep default 20;
    property CurrentAnimationValue: Integer read FCurrentAnimationValue;
    property OnAnimationProgress: TNotifyEvent read FOnAnimationProgress write FOnAnimationProgress;
  end;

  TImageSettings = class(TPersistent)
  private
    FPicture: TPicture;
    FVisible: Boolean;
    FDrawMode: TImageDrawMode;
    FMargins: TANDMR_Margins;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl;
    FPosition: TImagePositionSide;
    FAlignmentVertical: TImageAlignmentVertical;
    FPlacement: TImagePlacement;

    procedure SetPicture(const Value: TPicture);
    procedure SetVisible(const Value: Boolean);
    procedure SetDrawMode(const Value: TImageDrawMode);
    procedure SetMargins(const Value: TANDMR_Margins);
    procedure SetPosition(const Value: TImagePositionSide);
    procedure SetAlignmentVertical(const Value: TImageAlignmentVertical);
    procedure SetPlacement(const Value: TImagePlacement);
    procedure InternalPictureChanged(Sender: TObject);
    procedure InternalMarginsChanged(Sender: TObject);
  protected
    procedure DoChange; virtual;
  public
    constructor Create(AOwnerControl: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Visible: Boolean read FVisible write SetVisible default True;
    property DrawMode: TImageDrawMode read FDrawMode write SetDrawMode default idmProportional;
    property Margins: TANDMR_Margins read FMargins write SetMargins;
    property Position: TImagePositionSide read FPosition write SetPosition default ipsLeft;
    property AlignmentVertical: TImageAlignmentVertical read FAlignmentVertical write SetAlignmentVertical default iavCenter;
    property Placement: TImagePlacement read FPlacement write SetPlacement default iplInsideBounds;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBorderSettings = class(TPersistent)
  private
    FColor: TColor;
    FThickness: Integer;
    FStyle: TPenStyle;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FBackgroundColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetThickness(const Value: Integer);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetBackgroundColor(const Value: TColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Thickness: Integer read FThickness write SetThickness default 1;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 0;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctNone;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFocusSettings = class(TPersistent)
  private
    FBorderColor: TColor;
    FBorderColorVisible: Boolean;
    FBackgroundColor: TColor;
    FBackgroundColorVisible: Boolean;
    FUnderlineColor: TColor;
    FUnderlineVisible: Boolean;
    FUnderlineThickness: Integer;
    FUnderlineStyle: TPenStyle;
    FOnChange: TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorVisible(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundColorVisible(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TColor);
    procedure SetUnderlineVisible(const Value: Boolean);
    procedure SetUnderlineThickness(const Value: Integer);
    procedure SetUnderlineStyle(const Value: TPenStyle);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderColorVisible: Boolean read FBorderColorVisible write SetBorderColorVisible default False;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BackgroundColorVisible: Boolean read FBackgroundColorVisible write SetBackgroundColorVisible default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBlack;
    property UnderlineVisible: Boolean read FUnderlineVisible write SetUnderlineVisible default False;
    property UnderlineThickness: Integer read FUnderlineThickness write SetUnderlineThickness default 1;
    property UnderlineStyle: TPenStyle read FUnderlineStyle write SetUnderlineStyle default psSolid;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSeparatorSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FColor: TColor;
    FThickness: Integer;
    FPadding: Integer;
    FHeightMode: TSeparatorHeightMode;
    FCustomHeight: Integer;
    FOnChange: TNotifyEvent;
    procedure SetVisible(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetThickness(const Value: Integer);
    procedure SetPadding(const Value: Integer);
    procedure SetHeightMode(const Value: TSeparatorHeightMode);
    procedure SetCustomHeight(const Value: Integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Color: TColor read FColor write SetColor default clGray;
    property Thickness: Integer read FThickness write SetThickness default 1;
    property Padding: Integer read FPadding write SetPadding default 2;
    property HeightMode: TSeparatorHeightMode read FHeightMode write SetHeightMode default shmFull;
    property CustomHeight: Integer read FCustomHeight write SetCustomHeight default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDropShadowSettings = class(TPersistent)
  private
    FEnabled: Boolean;
    FColor: TColor;
    FOffset: TPoint;
    FBlurRadius: Integer;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: TPoint);
    procedure SetBlurRadius(const Value: Integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Color: TColor read FColor write SetColor default clBlack;
    property Offset: TPoint read FOffset write SetOffset; // Default for TPoint is (0,0)
    property BlurRadius: Integer read FBlurRadius write SetBlurRadius default 3;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

// Helper function declarations
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);

function DarkerColor(Color: TColor; Percent: Byte = 30): TColor;
function LighterColor(Color: TColor; Percent: Byte = 30): TColor;
function BlendColors(Color1, Color2: TColor; Factor: Single): TColor;

procedure DrawComponentCaption(
  ACanvas: TCanvas;
  const ARect: TRect;
  const ACaption: string;
  AFont: TFont;
  AFontColor: TColor; // Effective font color (already considering hover, disabled etc.)
  AAlignmentHorizontal: TAlignment;
  AAlignmentVertical: TCaptionVerticalAlignment;
  AWordWrap: Boolean;
  AOpacity: Byte // For future GDI+ text rendering, currently affects AFontColor if blended
);

function ResolveStateColor(
  AIsEnabled: Boolean;
  AIsHovering: Boolean;
  AIsFocused: Boolean;
  ABaseColor: TColor;
  AHoverColor: TColor;     // Specific color for hover state
  AFocusColor: TColor;     // Specific color for focus state
  ADisabledColor: TColor; // Specific color for disabled state
  AAllowHoverEffect: Boolean = True;
  AAllowFocusEffect: Boolean = True;
  AHoverEffectOverridesFocus: Boolean = False; // New: If true, hover can override focus if both active
  AFallbackToTransparent: Boolean = False // New: If true and no specific state color, return clNone
): TColor;

implementation

uses
  System.Math,       // For Min, Max
  Winapi.ActiveX;    // For IStream, TStreamAdapter

{ TBorderSettings }

constructor TBorderSettings.Create;
begin
  inherited Create;
  FColor := clBlack;
  FThickness := 1;
  FStyle := psSolid;
  FCornerRadius := 0;
  FRoundCornerType := rctNone;
  FBackgroundColor := clNone;
end;

procedure TBorderSettings.Assign(Source: TPersistent);
begin
  if Source is TBorderSettings then
  begin
    FColor := TBorderSettings(Source).FColor;
    FThickness := TBorderSettings(Source).FThickness;
    FStyle := TBorderSettings(Source).FStyle;
    FCornerRadius := TBorderSettings(Source).FCornerRadius;
    FRoundCornerType := TBorderSettings(Source).FRoundCornerType;
    FBackgroundColor := TBorderSettings(Source).FBackgroundColor;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TBorderSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBorderSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TBorderSettings.SetThickness(const Value: Integer);
begin
  if FThickness <> Value then
  begin
    FThickness := Value;
    Changed;
  end;
end;

procedure TBorderSettings.SetStyle(const Value: TPenStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TBorderSettings.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Changed;
  end;
end;

procedure TBorderSettings.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
    Changed;
  end;
end;

procedure TBorderSettings.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

{ TFocusSettings }

constructor TFocusSettings.Create;
begin
  inherited Create;
  FBorderColor := clBlack;
  FBorderColorVisible := False;
  FBackgroundColor := clNone;
  FBackgroundColorVisible := False;
  FUnderlineColor := clBlack;
  FUnderlineVisible := False;
  FUnderlineThickness := 1;
  FUnderlineStyle := psSolid;
end;

procedure TFocusSettings.Assign(Source: TPersistent);
begin
  if Source is TFocusSettings then
  begin
    FBorderColor := TFocusSettings(Source).FBorderColor;
    FBorderColorVisible := TFocusSettings(Source).FBorderColorVisible;
    FBackgroundColor := TFocusSettings(Source).FBackgroundColor;
    FBackgroundColorVisible := TFocusSettings(Source).FBackgroundColorVisible;
    FUnderlineColor := TFocusSettings(Source).FUnderlineColor;
    FUnderlineVisible := TFocusSettings(Source).FUnderlineVisible;
    FUnderlineThickness := TFocusSettings(Source).FUnderlineThickness;
    FUnderlineStyle := TFocusSettings(Source).FUnderlineStyle;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TFocusSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFocusSettings.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetBorderColorVisible(const Value: Boolean);
begin
  if FBorderColorVisible <> Value then
  begin
    FBorderColorVisible := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetBackgroundColorVisible(const Value: Boolean);
begin
  if FBackgroundColorVisible <> Value then
  begin
    FBackgroundColorVisible := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetUnderlineColor(const Value: TColor);
begin
  if FUnderlineColor <> Value then
  begin
    FUnderlineColor := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetUnderlineVisible(const Value: Boolean);
begin
  if FUnderlineVisible <> Value then
  begin
    FUnderlineVisible := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetUnderlineThickness(const Value: Integer);
begin
  if FUnderlineThickness <> Value then
  begin
    FUnderlineThickness := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetUnderlineStyle(const Value: TPenStyle);
begin
  if FUnderlineStyle <> Value then
  begin
    FUnderlineStyle := Value;
    Changed;
  end;
end;

{ TSeparatorSettings }

constructor TSeparatorSettings.Create;
begin
  inherited Create;
  FVisible := False;
  FColor := clGray;
  FThickness := 1;
  FPadding := 2;
  FHeightMode := shmFull;
  FCustomHeight := 0;
end;

procedure TSeparatorSettings.Assign(Source: TPersistent);
begin
  if Source is TSeparatorSettings then
  begin
    FVisible := TSeparatorSettings(Source).FVisible;
    FColor := TSeparatorSettings(Source).FColor;
    FThickness := TSeparatorSettings(Source).FThickness;
    FPadding := TSeparatorSettings(Source).FPadding;
    FHeightMode := TSeparatorSettings(Source).FHeightMode;
    FCustomHeight := TSeparatorSettings(Source).FCustomHeight;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TSeparatorSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSeparatorSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TSeparatorSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSeparatorSettings.SetThickness(const Value: Integer);
begin
  if FThickness <> Value then
  begin
    FThickness := Value;
    Changed;
  end;
end;

procedure TSeparatorSettings.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    Changed;
  end;
end;

procedure TSeparatorSettings.SetHeightMode(const Value: TSeparatorHeightMode);
begin
  if FHeightMode <> Value then
  begin
    FHeightMode := Value;
    Changed;
  end;
end;

procedure TSeparatorSettings.SetCustomHeight(const Value: Integer);
begin
  if FCustomHeight <> Value then
  begin
    FCustomHeight := Value;
    Changed;
  end;
end;

{ TDropShadowSettings }

constructor TDropShadowSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FColor := clBlack;
  FOffset.X := 0;
  FOffset.Y := 0;
  FBlurRadius := 3;
end;

procedure TDropShadowSettings.Assign(Source: TPersistent);
begin
  if Source is TDropShadowSettings then
  begin
    FEnabled := TDropShadowSettings(Source).FEnabled;
    FColor := TDropShadowSettings(Source).FColor;
    FOffset := TDropShadowSettings(Source).FOffset;
    FBlurRadius := TDropShadowSettings(Source).FBlurRadius;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TDropShadowSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDropShadowSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TDropShadowSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TDropShadowSettings.SetOffset(const Value: TPoint);
begin
  if (FOffset.X <> Value.X) or (FOffset.Y <> Value.Y) then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TDropShadowSettings.SetBlurRadius(const Value: Integer);
begin
  if FBlurRadius <> Value then
  begin
    FBlurRadius := Value;
    Changed;
  end;
end;

{ TANDMR_Margins }
constructor TANDMR_Margins.Create;
begin
  inherited Create;
  FLeft := 2;
  FTop := 2;
  FRight := 2;
  FBottom := 2;
end;

procedure TANDMR_Margins.Assign(Source: TPersistent);
begin
  if Source is TANDMR_Margins then
  begin
    FLeft := TANDMR_Margins(Source).FLeft;
    FTop := TANDMR_Margins(Source).FTop;
    FRight := TANDMR_Margins(Source).FRight;
    FBottom := TANDMR_Margins(Source).FBottom;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TANDMR_Margins.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TANDMR_Margins.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TANDMR_Margins.SetTop(const Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TANDMR_Margins.SetRight(const Value: Integer);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TANDMR_Margins.SetBottom(const Value: Integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

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
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FColor := clWindowText;
  FWordWrap := False;
  FVerticalAlignment := cvaCenter;
  FOffset.X := 0; // Default for TPoint
  FOffset.Y := 0; // Default for TPoint
  FDisabledColor := clGrayText;
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
    FWordWrap := TCaptionSettings(Source).FWordWrap;
    FVerticalAlignment := TCaptionSettings(Source).FVerticalAlignment;
    FOffset := TCaptionSettings(Source).FOffset; // Assign TPoint
    FDisabledColor := TCaptionSettings(Source).FDisabledColor;
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
procedure TCaptionSettings.SetFont(const Value: TFont); begin FFont.Assign(Value); Changed; end; // FontChanged will call Changed
procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition); begin if FPosition <> Value then begin FPosition := Value; Changed; end; end;
procedure TCaptionSettings.SetText(const Value: string); begin if FText <> Value then begin FText := Value; Changed; end; end;
procedure TCaptionSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TCaptionSettings.SetWordWrap(const Value: Boolean); begin if FWordWrap <> Value then begin FWordWrap := Value; Changed; end; end;

procedure TCaptionSettings.SetVerticalAlignment(const Value: TCaptionVerticalAlignment);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetOffset(const Value: TPoint); // Changed
begin
  if (FOffset.X <> Value.X) or (FOffset.Y <> Value.Y) then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

{ THoverSettings }
constructor THoverSettings.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
  FEnabled := True;
  FBackgroundColor := clSkyBlue;
  FBorderColor := clHighlight;
  FFontColor := clBlack;
  FCaptionFontColor := clBlack;
  FHoverEffect := heFade;
  FAnimationTimerInterval := 15;
  FAnimationStep := 20;
  FCurrentAnimationValue := 0;
  FAnimationDirection := 0;
  FAnimationTimer := TTimer.Create(nil);
  FAnimationTimer.Interval := FAnimationTimerInterval;
  FAnimationTimer.OnTimer := DoAnimate;
  FAnimationTimer.Enabled := False;
end;

destructor THoverSettings.Destroy;
begin
  FAnimationTimer.Free;
  inherited Destroy;
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
    FHoverEffect := THoverSettings(Source).FHoverEffect;
    AnimationTimerInterval := THoverSettings(Source).AnimationTimerInterval; // Use property to update timer
    FAnimationStep := THoverSettings(Source).FAnimationStep;
    FCurrentAnimationValue := THoverSettings(Source).FCurrentAnimationValue;
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
procedure THoverSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; if not FEnabled then FCurrentAnimationValue := 0; end; end;
procedure THoverSettings.SetFontColor(const Value: TColor); begin if FFontColor <> Value then begin FFontColor := Value; Changed; end; end;

procedure THoverSettings.SetHoverEffect(const Value: THoverEffect);
begin
  if FHoverEffect <> Value then
  begin
    FHoverEffect := Value;
    FCurrentAnimationValue := 0;
    FAnimationTimer.Enabled := False;
    Changed;
  end;
end;

procedure THoverSettings.SetAnimationTimerInterval(const Value: Integer);
begin
  if FAnimationTimerInterval <> Value then
  begin
    FAnimationTimerInterval := Max(1, Value);
    FAnimationTimer.Interval := FAnimationTimerInterval;
    Changed;
  end;
end;

procedure THoverSettings.SetAnimationStep(const Value: Integer);
begin
  if FAnimationStep <> Value then
  begin
    FAnimationStep := Max(1, Value);
    Changed;
  end;
end;

procedure THoverSettings.DoAnimate(Sender: TObject);
var
  TargetValue: Integer;
begin
  if FAnimationDirection = 1 then
    TargetValue := 255
  else if FAnimationDirection = -1 then
    TargetValue := 0
  else
  begin
    FAnimationTimer.Enabled := False;
    Exit;
  end;

  if FAnimationDirection = 1 then
  begin
    Inc(FCurrentAnimationValue, FAnimationStep);
    if FCurrentAnimationValue >= TargetValue then
    begin
      FCurrentAnimationValue := TargetValue;
      FAnimationTimer.Enabled := False;
      FAnimationDirection := 0;
    end;
  end
  else if FAnimationDirection = -1 then
  begin
    Dec(FCurrentAnimationValue, FAnimationStep);
    if FCurrentAnimationValue <= TargetValue then
    begin
      FCurrentAnimationValue := TargetValue;
      FAnimationTimer.Enabled := False;
      FAnimationDirection := 0;
    end;
  end;

  if Assigned(FOnAnimationProgress) then
    FOnAnimationProgress(Self);

  if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
    FOwnerControl.Invalidate;
end;

procedure THoverSettings.StartAnimation(IsHovering: Boolean);
begin
  if (FHoverEffect = heNone) or not Self.Enabled then
  begin
    if IsHovering and Self.Enabled then
      FCurrentAnimationValue := 255
    else
      FCurrentAnimationValue := 0;

    if Assigned(FOnAnimationProgress) then
      FOnAnimationProgress(Self);

    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
      FOwnerControl.Invalidate;

    FAnimationTimer.Enabled := False;
    FAnimationDirection := 0;
    Exit;
  end;

  FAnimationTimer.Interval := FAnimationTimerInterval;

  if IsHovering then
  begin
    FAnimationDirection := 1;
    if FCurrentAnimationValue < 255 then
      FAnimationTimer.Enabled := True;
  end
  else
  begin
    FAnimationDirection := -1;
    if FCurrentAnimationValue > 0 then
      FAnimationTimer.Enabled := True;
  end;
end;

{ TImageSettings }

constructor TImageSettings.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
  FPicture := TPicture.Create;
  FPicture.OnChange := InternalPictureChanged;
  FMargins := TANDMR_Margins.Create;
  FMargins.OnChange := InternalMarginsChanged;
  FVisible := True;
  FDrawMode := idmProportional;
  FPosition := ipsLeft;
  FAlignmentVertical := iavCenter;
  FPlacement := iplInsideBounds;
end;

destructor TImageSettings.Destroy;
begin
  if Assigned(FPicture) then
  begin
    FPicture.OnChange := nil;
    FPicture.Free;
  end;
  if Assigned(FMargins) then
  begin
    FMargins.OnChange := nil;
    FMargins.Free;
  end;
  inherited Destroy;
end;

procedure TImageSettings.Assign(Source: TPersistent);
begin
  if Source is TImageSettings then
  begin
    Visible := TImageSettings(Source).Visible;
    DrawMode := TImageSettings(Source).DrawMode;
    Picture.Assign(TImageSettings(Source).Picture);
    Margins.Assign(TImageSettings(Source).Margins);
    FPosition := TImageSettings(Source).FPosition;
    FAlignmentVertical := TImageSettings(Source).FAlignmentVertical;
    FPlacement := TImageSettings(Source).FPlacement;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TImageSettings.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TImageSettings.InternalMarginsChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TImageSettings.InternalPictureChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TImageSettings.SetDrawMode(const Value: TImageDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetMargins(const Value: TANDMR_Margins);
begin
  FMargins.Assign(Value);
end;

procedure TImageSettings.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TImageSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetPosition(const Value: TImagePositionSide);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetAlignmentVertical(const Value: TImageAlignmentVertical);
begin
  if FAlignmentVertical <> Value then
  begin
    FAlignmentVertical := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetPlacement(const Value: TImagePlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    DoChange;
  end;
end;

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
            ((ColorRef and $000000FF) shl 16) or // Blue to Red
            (ColorRef and $0000FF00) or          // Green to Green
            ((ColorRef and $00FF0000) shr 16);   // Red to Blue
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
    // Only add rectangle if it has positive dimensions, otherwise, it's an empty path.
    // A zero-width or zero-height rectangle is valid for AddRectangle but might not be intended for drawing.
    // However, for path creation, it's okay to add it if dimensions are non-negative.
    // The check (Rect.Width > 0) and (Rect.Height > 0) was a bit too restrictive.
    // APath.AddRectangle(Rect) will handle zero width/height correctly (adds nothing or a line).
    // For consistency, let's ensure it's only added if it can form an area.
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
  else // Point for non-rounded corner
    APath.AddLine(Rect.X, Rect.Y, Rect.X, Rect.Y); // Effectively starts at top-left

  APath.AddLine(Rect.X + LRadius, Rect.Y, Rect.X + Rect.Width - LRadius, Rect.Y); // Top edge

  if RoundTR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y, LDiameter, LDiameter, 270, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y); // Top-right point

  APath.AddLine(Rect.X + Rect.Width, Rect.Y + LRadius, Rect.X + Rect.Width, Rect.Y + Rect.Height - LRadius); // Right edge

  if RoundBR then
    APath.AddArc(Rect.X + Rect.Width - LDiameter, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 0, 90)
  else
    APath.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height, Rect.X + Rect.Width, Rect.Y + Rect.Height); // Bottom-right point

  APath.AddLine(Rect.X + Rect.Width - LRadius, Rect.Y + Rect.Height, Rect.X + LRadius, Rect.Y + Rect.Height); // Bottom edge

  if RoundBL then
    APath.AddArc(Rect.X, Rect.Y + Rect.Height - LDiameter, LDiameter, LDiameter, 90, 90)
  else
    APath.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y + Rect.Height); // Bottom-left point

  APath.CloseFigure; // Connects to start (Rect.X, Rect.Y + LRadius)
end;

procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
var
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LRectF: TGPRectF;
  LRadiusValue: Single;
  LBorderThicknessValue: Single;
begin
  if (AGraphics = nil) or (ADrawArea.Width <= 0) or (ADrawArea.Height <= 0) then
    Exit;

  LBorderThicknessValue := ABorderThickness;

  if LBorderThicknessValue > 0 then
  begin
    LRectF.X      := ADrawArea.Left + LBorderThicknessValue / 2.0;
    LRectF.Y      := ADrawArea.Top + LBorderThicknessValue / 2.0;
    LRectF.Width  := ADrawArea.Width - LBorderThicknessValue;
    LRectF.Height := ADrawArea.Height - LBorderThicknessValue;
  end
  else
  begin
    LRectF.X      := ADrawArea.Left;
    LRectF.Y      := ADrawArea.Top;
    LRectF.Width  := ADrawArea.Width;
    LRectF.Height := ADrawArea.Height;
  end;

  LRectF.Width  := Max(0.0, LRectF.Width);
  LRectF.Height := Max(0.0, LRectF.Height);

  if (LRectF.Width = 0) or (LRectF.Height = 0) then
    Exit;

  LRadiusValue := ACornerRadius;
  LRadiusValue := Min(LRadiusValue, LRectF.Width / 2.0);
  LRadiusValue := Min(LRadiusValue, LRectF.Height / 2.0);
  LRadiusValue := Max(0.0, LRadiusValue);

  LPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LPath, LRectF, LRadiusValue, ARoundCornerType);

    if LPath.GetPointCount > 0 then
    begin
      if ABackgroundColor <> clNone then
      begin
        LBrush := TGPSolidBrush.Create(ColorToARGB(ABackgroundColor, AOpacity));
        try
          AGraphics.FillPath(LBrush, LPath);
        finally
          LBrush.Free;
        end;
      end;

      if (LBorderThicknessValue > 0) and (ABorderColor <> clNone) and (ABorderStyle <> psClear) then
      begin
        LPen := TGPPen.Create(ColorToARGB(ABorderColor, AOpacity), LBorderThicknessValue);
        try
          case ABorderStyle of
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
  tempCalculatedW, tempCalculatedH: Double;
  PngStream: TMemoryStream;
  GpSourceBitmap: TGPBitmap;
  Adapter: IStream;
  E: Exception;
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
  try
    APNG.SaveToStream(PngStream);
    PngStream.Position := 0;
    Adapter := TStreamAdapter.Create(PngStream, soReference);
    GpSourceBitmap := TGPBitmap.Create(Adapter);
    try
      if (GpSourceBitmap = nil) or (GpSourceBitmap.GetLastStatus <> Ok) then
      begin
        Exit;
      end;

      if (DrawImageRect.Width <> GpSourceBitmap.GetWidth()) or (DrawImageRect.Height <> GpSourceBitmap.GetHeight()) then
        AGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic)
      else
        AGraphics.SetInterpolationMode(InterpolationModeDefault);

      AGraphics.DrawImage(GpSourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height);
    finally
      GpSourceBitmap.Free;
    end;
  except
    on E: Exception do
    begin
      // Optional: Log or handle exception E
    end;
  end;
    PngStream.Free;
end;

procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
var
  DrawImageRect: TRect;
  GraphicW, GraphicH: Integer;
  rRatio, rRectRatio: Double;
  tempCalculatedW, tempCalculatedH: Double;
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
    DrawImageRect := ADestRect;
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

function DarkerColor(Color: TColor; Percent: Byte = 30): TColor;
var
  R, G, B: Byte;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := Max(0, Round(R * (100 - Percent) / 100));
  G := Max(0, Round(G * (100 - Percent) / 100));
  B := Max(0, Round(B * (100 - Percent) / 100));
  Result := RGB(R, G, B);
end;

function LighterColor(Color: TColor; Percent: Byte = 30): TColor;
var
  R, G, B: Byte;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := Min(255, Round(R + (255 - R) * Percent / 100));
  G := Min(255, Round(G + (255 - G) * Percent / 100));
  B := Min(255, Round(B + (255 - B) * Percent / 100));
  Result := RGB(R, G, B);
end;

function BlendColors(Color1, Color2: TColor; Factor: Single): TColor;
var
  R1, G1, B1, R2, G2, B2, R, G, B: Byte;
  IsTransparent1, IsTransparent2: Boolean;
begin
  if Factor <= 0.0 then Exit(Color1);
  if Factor >= 1.0 then Exit(Color2);

  IsTransparent1 := (Color1 = clNone) or (TAlphaColorRec(Color1).Alpha = 0);
  IsTransparent2 := (Color2 = clNone) or (TAlphaColorRec(Color2).Alpha = 0);

  if IsTransparent1 and IsTransparent2 then Exit(clNone);
  if IsTransparent1 then Exit(Color2);
  if IsTransparent2 then Exit(Color1);

  Color1 := ColorToRGB(Color1);
  Color2 := ColorToRGB(Color2);

  R1 := GetRValue(Color1); G1 := GetGValue(Color1); B1 := GetBValue(Color1);
  R2 := GetRValue(Color2); G2 := GetGValue(Color2); B2 := GetBValue(Color2);

  R := Round(R1 + (R2 - R1) * Factor);
  G := Round(G1 + (G2 - G1) * Factor);
  B := Round(B1 + (B2 - B1) * Factor);
  Result := RGB(R, G, B);
end;

procedure DrawComponentCaption(
  ACanvas: TCanvas;
  const ARect: TRect;
  const ACaption: string;
  AFont: TFont;
  AFontColor: TColor;
  AAlignmentHorizontal: TAlignment;
  AAlignmentVertical: TCaptionVerticalAlignment;
  AWordWrap: Boolean;
  AOpacity: Byte
);
var
  DrawTextFlags: Cardinal;
  TempRect: TRect;
begin
  if (ACaption = '') or (ARect.Width <= 0) or (ARect.Height <= 0) then
    Exit;

  ACanvas.Font.Assign(AFont);
  ACanvas.Font.Color := AFontColor;
  ACanvas.Brush.Style := bsClear;

  DrawTextFlags := DT_NOPREFIX;

  if AWordWrap then
    DrawTextFlags := DrawTextFlags or DT_WORDBREAK
  else
    DrawTextFlags := DrawTextFlags or DT_SINGLELINE or DT_END_ELLIPSIS;

  case AAlignmentHorizontal of
    taLeftJustify: DrawTextFlags := DrawTextFlags or DT_LEFT;
    taRightJustify: DrawTextFlags := DrawTextFlags or DT_RIGHT;
    taCenter: DrawTextFlags := DrawTextFlags or DT_CENTER;
  end;

  case AAlignmentVertical of
    cvaTop: DrawTextFlags := DrawTextFlags or DT_TOP;
    cvaCenter: DrawTextFlags := DrawTextFlags or DT_VCENTER;
    cvaBottom: DrawTextFlags := DrawTextFlags or DT_BOTTOM;
  end;

  if (DrawTextFlags and DT_WORDBREAK = 0) and (DrawTextFlags and DT_VCENTER = DT_VCENTER) then
      DrawTextFlags := DrawTextFlags or DT_SINGLELINE;

  TempRect := ARect;
  DrawText(ACanvas.Handle, PChar(ACaption), Length(ACaption), TempRect, DrawTextFlags);
end;

function ResolveStateColor(
  AIsEnabled: Boolean;
  AIsHovering: Boolean;
  AIsFocused: Boolean;
  ABaseColor: TColor;
  AHoverColor: TColor;
  AFocusColor: TColor;
  ADisabledColor: TColor;
  AAllowHoverEffect: Boolean = True;
  AAllowFocusEffect: Boolean = True;
  AHoverEffectOverridesFocus: Boolean = False;
  AFallbackToTransparent: Boolean = False
): TColor;
var
  LResultColor: TColor;
begin
  LResultColor := ABaseColor;

  if not AIsEnabled then
  begin
    if ADisabledColor <> clNone then Result := ADisabledColor
    else Result := ABaseColor;
    Exit;
  end;

  if AIsFocused and AAllowFocusEffect and (AFocusColor <> clNone) then
  begin
    LResultColor := AFocusColor;
  end;

  if AIsHovering and AAllowHoverEffect and (AHoverColor <> clNone) then
  begin
    if AHoverEffectOverridesFocus or not (AIsFocused and AAllowFocusEffect and (AFocusColor <> clNone)) then
    begin
      LResultColor := AHoverColor;
    end;
  end;

  if (LResultColor = ABaseColor) and AFallbackToTransparent and (ABaseColor = clNone) then
  begin
    Result := clNone;
  end
  else if LResultColor <> clNone then
  begin
    Result := LResultColor;
  end
  else if AFallbackToTransparent then
  begin
    Result := clNone;
  end
  else
  begin
    Result := ABaseColor;
  end;
end;

end.
