unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.UITypes, Vcl.ExtCtrls, System.Types, System.Variants; // Ensure GDIP units are here, Added Vcl.ExtCtrls for TTimer, Added System.Types for TPoint

type
  THoverEffect = (heNone, heFade, heScale);

  TCaptionVerticalAlignment = (cvaTop, cvaCenter, cvaBottom);

  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);

  TGradientType = (gtLinearVertical, gtLinearHorizontal);

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

  TCEditStatus = (
    cepsNormal,
    cepsError,
    cepsWarning,
    cepsSuccess
  );

  TTagType = (ttDefault, ttString, ttExtended, ttObject);

  TANDMR_Tag = class(TPersistent)
  private
    FValue: Variant;
    FType: TTagType;
    FOnChange: TNotifyEvent;
    procedure SetValue(const AValue: Variant);
    procedure SetType(const AValue: TTagType);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Variant read FValue write SetValue;
    property TagType: TTagType read FType write SetType default ttDefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TANDMR_TagString = class(TANDMR_Tag)
  private
    procedure SetStringValue(const AValue: string);
    function GetStringValue: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AsString: string read GetStringValue write SetStringValue;
  end;

  TANDMR_TagExtended = class(TANDMR_Tag)
  private
    FItems: TStringList;
    function GetItems: TStringList;
    procedure SetItems(const AValue: TStringList);
  protected
    procedure ItemsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Items: TStringList read GetItems write SetItems;
  end;

  TANDMR_TagObject = class(TANDMR_Tag)
  private
    FObjectValue: TObject; // Keep a direct reference for type safety
    procedure SetObjectValue(const AValue: TObject);
    function GetObjectValue: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // Renamed AsObject to ObjectValue to match Get/Set naming conventions often seen,
    // and to avoid conflict if a property named 'Object' was ever introduced at TObject level (unlikely but for consistency).
    // User can revert to AsObject if preferred.
    property ObjectValue: TObject read GetObjectValue write SetObjectValue;
  end;

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
    FOwnerControl: TWinControl; // Keep this if the caption settings need to interact with the control directly (e.g., invalidate)
    FVerticalAlignment: TCaptionVerticalAlignment;
    FOffset: TPoint;
    FDisabledColor: TColor;

    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetPosition(const Value: TCaptionPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetVerticalAlignment(const Value: TCaptionVerticalAlignment);
    procedure SetOffset(const Value: TPoint);
    procedure SetDisabledColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TWinControl); // Consider removing AOwner if not strictly needed for caption's own logic
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
    property Offset: TPoint read FOffset write SetOffset; // Default for TPoint is (0,0)
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
    FCurrentAnimationValue: Integer; // Represents the current state of animation (e.g., alpha level)
    FAnimationDirection: Integer;  // 1 for fade-in/scale-up, -1 for fade-out/scale-down, 0 for idle
    FOwnerControl: TWinControl;    // The control that owns these settings, used for invalidation.
    FOnAnimationProgress: TNotifyEvent; // Event fired on each animation step

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
    // FCurrentAnimationValue is a runtime state, typically not published or controlled externally in this manner.
    // It's read-only for inspection if needed.
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
    FOwnerControl: TWinControl; // Keep if needed for direct interactions like invalidation
    FPosition: TImagePositionSide;
    FAlignmentVertical: TImageAlignmentVertical;
    FPlacement: TImagePlacement;
    FTargetWidth: Integer;      // Added
    FTargetHeight: Integer;     // Added

    procedure SetPicture(const Value: TPicture);
    procedure SetVisible(const Value: Boolean);
    procedure SetDrawMode(const Value: TImageDrawMode);
    procedure SetMargins(const Value: TANDMR_Margins);
    procedure SetPosition(const Value: TImagePositionSide);
    procedure SetAlignmentVertical(const Value: TImageAlignmentVertical);
    procedure SetPlacement(const Value: TImagePlacement);
    procedure SetTargetWidth(const Value: Integer);    // Added
    procedure SetTargetHeight(const Value: Integer);   // Added
    procedure InternalPictureChanged(Sender: TObject);
    procedure InternalMarginsChanged(Sender: TObject);
  protected
    procedure DoChange; virtual; // Renamed from Changed to avoid conflict if a component also has a Changed method.
  public
    constructor Create(AOwnerControl: TWinControl); // Consider removing AOwnerControl if not strictly needed.
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
    property TargetWidth: Integer read FTargetWidth write SetTargetWidth default 0; // Added
    property TargetHeight: Integer read FTargetHeight write SetTargetHeight default 0; // Added
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBorderSettings = class(TPersistent)
  // ... (no changes to declaration, assuming it's fine)
  private
    FColor: TColor;
    FThickness: Integer;
    FStyle: TPenStyle;
    FColor: TColor;
    FThickness: Integer;
    FStyle: TPenStyle;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FBackgroundColor: TColor;
    FVisible: Boolean; // Added
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetThickness(const Value: Integer);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean); // Added
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True; // Added
    property Color: TColor read FColor write SetColor default clBlack;
    property Thickness: Integer read FThickness write SetThickness default 1;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 0;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctNone;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFocusSettings = class(TPersistent)
  // ... (no changes to declaration, assuming it's fine)
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
  // ... (no changes to declaration, assuming it's fine)
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
  // ... (no changes to declaration, assuming it's fine)
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

  TGradientSettings = class(TPersistent)
  private
    FEnabled: Boolean;
    FGradientType: TGradientType;
    FStartColor: TColor;
    FEndColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetGradientType(const Value: TGradientType);
    procedure SetStartColor(const Value: TColor);
    procedure SetEndColor(const Value: TColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property GradientType: TGradientType read FGradientType write SetGradientType default gtLinearVertical;
    property StartColor: TColor read FStartColor write SetStartColor default clNone;
    property EndColor: TColor read FEndColor write SetEndColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TProgressSettings = class(TPersistent)
  private
    FShowProgress: Boolean;
    FProgressColor: TColor;
    FHideCaptionWhileProcessing: Boolean;
    FAnimationTimerInterval: Integer;
    FOwnerControl: TWinControl; // Used to invalidate the control when a setting changes
    FOnChange: TNotifyEvent;

    procedure SetShowProgress(const Value: Boolean);
    procedure SetProgressColor(const Value: TColor);
    procedure SetHideCaptionWhileProcessing(const Value: Boolean);
    procedure SetAnimationTimerInterval(const Value: Integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwnerControl: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShowProgress: Boolean read FShowProgress write SetShowProgress default True;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clGray;
    property HideCaptionWhileProcessing: Boolean read FHideCaptionWhileProcessing write SetHideCaptionWhileProcessing default True;
    property AnimationTimerInterval: Integer read FAnimationTimerInterval write SetAnimationTimerInterval default 100;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

// Helper function declarations
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);

function DarkerColor(AColor: TColor; APercent: Byte = 30): TColor; // Renamed parameters for clarity
function LighterColor(AColor: TColor; APercent: Byte = 30): TColor; // Renamed parameters for clarity
function BlendColors(AColor1, AColor2: TColor; AFactor: Single): TColor; // Renamed parameters for clarity

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
  AHoverColor: TColor;      // Specific color for hover state
  AFocusColor: TColor;      // Specific color for focus state
  ADisabledColor: TColor;   // Specific color for disabled state
  AAllowHoverEffect: Boolean = True;
  AAllowFocusEffect: Boolean = True;
  AHoverEffectOverridesFocus: Boolean = False;
  AFallbackToTransparent: Boolean = False
): TColor;

implementation

uses
  System.Math,        // For Min, Max, Max
  Winapi.ActiveX;     // For IStream, TStreamAdapter // Make sure this is needed if only TStreamAdapter is used directly.

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
  FVisible := True; // Added initialization
end;

procedure TBorderSettings.Assign(Source: TPersistent);
var
  LSource: TBorderSettings;
begin
  if Source is TBorderSettings then
  begin
    LSource := TBorderSettings(Source);
    FColor := LSource.FColor;
    FThickness := LSource.FThickness;
    FStyle := LSource.FStyle;
    FCornerRadius := LSource.FCornerRadius;
    FRoundCornerType := LSource.FRoundCornerType;
    FBackgroundColor := LSource.FBackgroundColor;
    FVisible := LSource.FVisible; // Added assignment
    Changed; // Call changed once after all assignments specific to this class
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
    FCornerRadius := Max(0, Value); // Ensure CornerRadius is not negative
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

procedure TBorderSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TFocusSettings - similar Assign pattern can be applied }
// ... (Constructors, Setters as before) ...
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
var
  LSource: TFocusSettings;
begin
  if Source is TFocusSettings then
  begin
    LSource := TFocusSettings(Source);
    FBorderColor := LSource.FBorderColor;
    FBorderColorVisible := LSource.FBorderColorVisible;
    FBackgroundColor := LSource.FBackgroundColor;
    FBackgroundColorVisible := LSource.FBackgroundColorVisible;
    FUnderlineColor := LSource.FUnderlineColor;
    FUnderlineVisible := LSource.FUnderlineVisible;
    FUnderlineThickness := LSource.FUnderlineThickness;
    FUnderlineStyle := LSource.FUnderlineStyle;
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
// Setters for TFocusSettings remain the same
procedure TFocusSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure TFocusSettings.SetBorderColorVisible(const Value: Boolean); begin if FBorderColorVisible <> Value then begin FBorderColorVisible := Value; Changed; end; end;
procedure TFocusSettings.SetBackgroundColor(const Value: TColor); begin if FBackgroundColor <> Value then begin FBackgroundColor := Value; Changed; end; end;
procedure TFocusSettings.SetBackgroundColorVisible(const Value: Boolean); begin if FBackgroundColorVisible <> Value then begin FBackgroundColorVisible := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineColor(const Value: TColor); begin if FUnderlineColor <> Value then begin FUnderlineColor := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineVisible(const Value: Boolean); begin if FUnderlineVisible <> Value then begin FUnderlineVisible := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineThickness(const Value: Integer); begin if FUnderlineThickness <> Max(0, Value) then begin FUnderlineThickness := Max(0, Value); Changed; end; end; // Ensure non-negative
procedure TFocusSettings.SetUnderlineStyle(const Value: TPenStyle); begin if FUnderlineStyle <> Value then begin FUnderlineStyle := Value; Changed; end; end;


{ TSeparatorSettings - similar Assign pattern can be applied }
// ... (Constructors, Setters as before) ...
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
var
  LSource: TSeparatorSettings;
begin
  if Source is TSeparatorSettings then
  begin
    LSource := TSeparatorSettings(Source);
    FVisible := LSource.FVisible;
    FColor := LSource.FColor;
    FThickness := LSource.FThickness;
    FPadding := LSource.FPadding;
    FHeightMode := LSource.FHeightMode;
    FCustomHeight := LSource.FCustomHeight;
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
// Setters for TSeparatorSettings remain the same
procedure TSeparatorSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TSeparatorSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TSeparatorSettings.SetThickness(const Value: Integer); begin if FThickness <> Max(0, Value) then begin FThickness := Max(0, Value); Changed; end; end; // Ensure non-negative
procedure TSeparatorSettings.SetPadding(const Value: Integer); begin if FPadding <> Value then begin FPadding := Value; Changed; end; end; // Padding can be negative? Usually non-negative. Max(0,Value) if so.
procedure TSeparatorSettings.SetHeightMode(const Value: TSeparatorHeightMode); begin if FHeightMode <> Value then begin FHeightMode := Value; Changed; end; end;
procedure TSeparatorSettings.SetCustomHeight(const Value: Integer); begin if FCustomHeight <> Max(0,Value) then begin FCustomHeight := Max(0,Value); Changed; end; end; // Ensure non-negative


{ TDropShadowSettings - similar Assign pattern can be applied }
// ... (Constructors, Setters as before) ...
constructor TDropShadowSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FColor := clBlack;
  FOffset.Create(0,0); // More explicit TPoint initialization, though X,Y:=0 is fine.
  FBlurRadius := 3;
end;

procedure TDropShadowSettings.Assign(Source: TPersistent);
var
  LSource: TDropShadowSettings;
begin
  if Source is TDropShadowSettings then
  begin
    LSource := TDropShadowSettings(Source);
    FEnabled := LSource.FEnabled;
    FColor := LSource.FColor;
    FOffset := LSource.FOffset;
    FBlurRadius := LSource.FBlurRadius;
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
// Setters for TDropShadowSettings remain the same
procedure TDropShadowSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; end; end;
procedure TDropShadowSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TDropShadowSettings.SetOffset(const Value: TPoint); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end; // TPoint comparison is fine
procedure TDropShadowSettings.SetBlurRadius(const Value: Integer); begin if FBlurRadius <> Max(0, Value) then begin FBlurRadius := Max(0, Value); Changed; end; end; // Blur radius non-negative

{ TGradientSettings }

constructor TGradientSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FGradientType := gtLinearVertical;
  FStartColor := clNone;
  FEndColor := clNone;
end;

procedure TGradientSettings.Assign(Source: TPersistent);
var
  LSource: TGradientSettings;
begin
  if Source is TGradientSettings then
  begin
    LSource := TGradientSettings(Source);
    FEnabled := LSource.FEnabled;
    FGradientType := LSource.FGradientType;
    FStartColor := LSource.FStartColor;
    FEndColor := LSource.FEndColor;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TGradientSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGradientSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TGradientSettings.SetGradientType(const Value: TGradientType);
begin
  if FGradientType <> Value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TGradientSettings.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TGradientSettings.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

{ TProgressSettings }

constructor TProgressSettings.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl; // Store the owner control
  FShowProgress := True;
  FProgressColor := clGray;
  FHideCaptionWhileProcessing := True;
  FAnimationTimerInterval := 100;
end;

destructor TProgressSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TProgressSettings.Assign(Source: TPersistent);
var
  LSource: TProgressSettings;
begin
  if Source is TProgressSettings then
  begin
    LSource := TProgressSettings(Source);
    SetShowProgress(LSource.ShowProgress);
    SetProgressColor(LSource.ProgressColor);
    SetHideCaptionWhileProcessing(LSource.HideCaptionWhileProcessing);
    SetAnimationTimerInterval(LSource.AnimationTimerInterval);
  end
  else
    inherited Assign(Source);
end;

procedure TProgressSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOwnerControl) and (FOwnerControl.HandleAllocated) and (csDesigning in FOwnerControl.ComponentState) then
    FOwnerControl.Invalidate
  else if Assigned(FOwnerControl) and (FOwnerControl.HandleAllocated) and not (csDesigning in FOwnerControl.ComponentState) then
     FOwnerControl.Repaint;
end;

procedure TProgressSettings.SetShowProgress(const Value: Boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    Changed;
  end;
end;

procedure TProgressSettings.SetProgressColor(const Value: TColor);
begin
  if FProgressColor <> Value then
  begin
    FProgressColor := Value;
    Changed;
  end;
end;

procedure TProgressSettings.SetHideCaptionWhileProcessing(const Value: Boolean);
begin
  if FHideCaptionWhileProcessing <> Value then
  begin
    FHideCaptionWhileProcessing := Value;
    Changed;
  end;
end;

procedure TProgressSettings.SetAnimationTimerInterval(const Value: Integer);
begin
  if FAnimationTimerInterval <> Max(10, Value) then
  begin
    FAnimationTimerInterval := Max(10, Value);
    Changed;
  end;
end;

{ TANDMR_Tag }

constructor TANDMR_Tag.Create;
begin
  inherited Create;
  FType := ttDefault;
  FValue := Null; // Explicitly set to Null for clarity, though Variant initializes to Unassigned/Null
end;

procedure TANDMR_Tag.Assign(Source: TPersistent);
var
  LSource: TANDMR_Tag;
begin
  if Source is TANDMR_Tag then
  begin
    LSource := TANDMR_Tag(Source);
    // Use setters to ensure logic (like Changed) is encapsulated
    SetType(LSource.TagType); // Accessing property which uses FType
    SetValue(LSource.Value);  // Accessing property which uses FValue
    // Original code:
    // FValue := TANDMR_Tag(Source).FValue;
    // FType := TANDMR_Tag(Source).FType;
    // Changed; // This single Changed call was fine. Using setters achieves similar.
  end
  else
    inherited Assign(Source);
end;

procedure TANDMR_Tag.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TANDMR_Tag.SetValue(const AValue: Variant);
begin
  if FValue <> AValue then // Variant comparison can be tricky but generally works for simple types
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TANDMR_Tag.SetType(const AValue: TTagType);
begin
  if FType <> AValue then
  begin
    FType := AValue;
    // Optionally, clear FValue if type changes significantly, e.g., from ttObject to ttString
    // FValue := Null; // Or handle conversion if possible/desired
    Changed;
  end;
end;

{ TANDMR_TagString }

constructor TANDMR_TagString.Create;
begin
  inherited Create;
  FType := ttString;
  FValue := ''; // Initialize FValue to an empty string variant
end;

procedure TANDMR_TagString.Assign(Source: TPersistent);
begin
  inherited Assign(Source); // Calls TANDMR_Tag.Assign which handles FValue, FType and calls Changed.
  if Source is TANDMR_TagString then
  begin
    // No fields specific to TANDMR_TagString other than FValue/FType (handled by parent).
    // The original redundant "Changed;" call is removed.
  end
  else if Source is TANDMR_Tag then // Assigning from a generic TANDMR_Tag
  begin
    // Ensure our type is correctly ttString after assignment if that's a strict requirement.
    // The parent Assign would have copied source's FType.
    // If TANDMR_Tag(Source).FType was not ttString, FType would now reflect that.
    // If this class *must* always be ttString:
    if TagType <> ttString then // Access TagType property to use its setter
       SetType(ttString); // This will call Changed if FType actually changes.
    // FValue is already assigned by inherited call. GetStringValue handles conversion.
  end;
end;

function TANDMR_TagString.GetStringValue: string;
begin
  if VarIsNull(FValue) or VarIsEmpty(FValue) then // VarIsEmpty also checks for Null
    Result := ''
  else
    Result := VarToStr(FValue);
end;

procedure TANDMR_TagString.SetStringValue(const AValue: string);
begin
  // SetValue will compare and call Changed if needed.
  SetValue(AValue); // Let parent's SetValue handle the variant assignment and Changed notification.
  // Ensure FType is correct, in case it was different.
  if TagType <> ttString then // Access TagType property to use its setter
    SetType(ttString);
end;

{ TANDMR_TagExtended }

constructor TANDMR_TagExtended.Create;
begin
  inherited Create;
  FType := ttExtended;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChanged;
end;

destructor TANDMR_TagExtended.Destroy;
begin
  if Assigned(FItems) then // Good practice to check before accessing
  begin
    FItems.OnChange := nil;
    FItems.Free;
    FItems := nil; // Good practice
  end;
  inherited Destroy;
end;

procedure TANDMR_TagExtended.Assign(Source: TPersistent);
begin
  inherited Assign(Source); // Handles FValue, FType and calls Changed.
  if Source is TANDMR_TagExtended then
  begin
    // SetItems will assign FItems and call Changed.
    // This means OnChange might be triggered twice if parent properties also changed.
    // This is often acceptable.
    SetItems(TANDMR_TagExtended(Source).Items);
  end
  else if Source is TANDMR_Tag then
  begin
     if TagType <> ttExtended then
       SetType(ttExtended);
     // If FValue from TANDMR_Tag could represent items (e.g. delimited string),
     // logic could be added here to parse it into FItems.
     // Otherwise, FItems remains as is (likely empty if newly created).
  end;
end;

function TANDMR_TagExtended.GetItems: TStringList;
begin
  Result := FItems;
end;

procedure TANDMR_TagExtended.SetItems(const AValue: TStringList);
begin
  if AValue <> FItems then // Check if it's a different TStringList instance
  begin
    FItems.Assign(AValue); // Assigns content. TStringList.Assign does not trigger its OnChange.
    // Explicitly call Changed because the content of FItems (our primary value) has changed.
    Changed;
  end;
end;

procedure TANDMR_TagExtended.ItemsChanged(Sender: TObject);
begin
  // This is called when FItems content changes through its own methods (Add, Delete etc.)
  Changed; // Propagate change notification
end;

{ TANDMR_TagObject }

constructor TANDMR_TagObject.Create;
begin
  inherited Create;
  FType := ttObject;
  FObjectValue := nil;
  // FValue (Variant) from parent remains Null/Unassigned.
  // Consider if FValue should store the pointer: Value := NativeInt(nil);
end;

destructor TANDMR_TagObject.Destroy;
begin
  // This class does NOT own the FObjectValue.
  // User is responsible for managing the lifetime of the assigned object.
  FObjectValue := nil;
  inherited Destroy;
end;

function TANDMR_TagObject.GetObjectValue: TObject;
begin
  Result := FObjectValue;
end;

procedure TANDMR_TagObject.Assign(Source: TPersistent);
var
  LSourceTagObject: TANDMR_TagObject;
  LSourceTag: TANDMR_Tag;
  OldObjectValue: TObject;
begin
  inherited Assign(Source); // Handles FValue, FType from parent, and calls Changed.

  if Source is TANDMR_TagObject then
  begin
    LSourceTagObject := TANDMR_TagObject(Source);
    SetObjectValue(LSourceTagObject.ObjectValue); // Use setter to handle Changed if value differs
  end
  else if Source is TANDMR_Tag then // Source is a base TANDMR_Tag, but not TANDMR_TagObject
  begin
    LSourceTag := TANDMR_Tag(Source);
    OldObjectValue := FObjectValue;

    // WARNING: The following logic for converting Variant to TObject is potentially unsafe.
    // Standard Variants (varObject) are for OLE Automation IDispatch objects, not generic TObjects.
    // Casting IUnknown to TObject is also highly risky as an IUnknown is not necessarily a TObject.
    // A safer convention would be to store TObject pointers as NativeInt in the Variant FValue
    // if FType is ttObject.
    // Example of safer approach (if FValue stored NativeInt(obj)):
    // if (LSourceTag.TagType = ttObject) and VarIsNumeric(LSourceTag.Value) then
    //   try
    //     SetObjectValue(TObject(NativeInt(LSourceTag.Value)));
    //   except
    //     SetObjectValue(nil);
    //   end
    // else
    //   SetObjectValue(nil);

    // Current risky logic from user prompt:
    if (LSourceTag.Value <> Null) then
    begin
      if VarType(LSourceTag.Value) = varObject then
      begin
        // This assumes LSourceTag.Value contains an IDispatch that is also the TObject instance. Highly specific.
        FObjectValue := System.TVarData(LSourceTag.Value).VDispatch; // Very low-level and risky.
      end
      else if VarType(LSourceTag.Value) = varUnknown then // Check for IInterface
      begin
        try
          // This cast is unsafe. An IUnknown is not guaranteed to be a TObject.
          FObjectValue := IUnknown(LSourceTag.Value) as TObject;
        except
          FObjectValue := nil; // Or handle error
        end;
      end
      else
        FObjectValue := nil; // Cannot determine object from this variant type
    end
    else // LSourceTag.Value is Null
      FObjectValue := nil;

    if OldObjectValue <> FObjectValue then // If FObjectValue was actually changed by the above logic
      Changed; // Then notify this specific change.

    // Ensure FType is ttObject
    if TagType <> ttObject then
      SetType(ttObject);
  end;
end;

procedure TANDMR_TagObject.SetObjectValue(const AValue: TObject);
begin
  if FObjectValue <> AValue then
  begin
    FObjectValue := AValue;
    // Optionally, synchronize FValue in the parent TANDMR_Tag:
    // if AValue <> nil then
    //   SetValue(NativeInt(AValue)) // Store as NativeInt, requires FValue to handle this type.
    // else
    //   SetValue(Null);
    Changed;
  end;
  // Ensure FType is correct
  if TagType <> ttObject then
    SetType(ttObject);
end;

{ TANDMR_Margins - Assign pattern updated }
constructor TANDMR_Margins.Create;
begin
  inherited Create;
  FLeft := 2;
  FTop := 2;
  FRight := 2;
  FBottom := 2;
end;

procedure TANDMR_Margins.Assign(Source: TPersistent);
var
  LSource: TANDMR_Margins;
begin
  if Source is TANDMR_Margins then
  begin
    LSource := TANDMR_Margins(Source);
    FLeft := LSource.FLeft;
    FTop := LSource.FTop;
    FRight := LSource.FRight;
    FBottom := LSource.FBottom;
    Changed;
  end
  else
    inherited Assign(Source);
end;
// ... (Changed and Setters as before)
procedure TANDMR_Margins.Changed; begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure TANDMR_Margins.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TANDMR_Margins.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TANDMR_Margins.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TANDMR_Margins.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;


{ TCaptionSettings - Assign pattern updated }
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
  FFont.Name := 'Segoe UI'; // Consider using DefaultFont property of screen or application
  FFont.Size := 9;
  FColor := clWindowText;
  FWordWrap := False;
  FVerticalAlignment := cvaCenter;
  FOffset.Create(0,0); // Explicit TPoint initialization
  FDisabledColor := clGrayText;
end;

destructor TCaptionSettings.Destroy;
begin
  if Assigned(FFont) then
  begin
    FFont.OnChange := nil;
    FFont.Free;
    FFont := nil;
  end;
  inherited Destroy;
end;

procedure TCaptionSettings.Assign(Source: TPersistent);
var
  LSource: TCaptionSettings;
begin
  if Source is TCaptionSettings then
  begin
    LSource := TCaptionSettings(Source);
    // FOwnerControl is not typically assigned.
    SetVisible(LSource.Visible);
    SetText(LSource.Text);
    SetPosition(LSource.Position);
    SetAlignment(LSource.Alignment);
    SetFont(LSource.Font); // This assigns and calls FontChanged -> Changed
    SetColor(LSource.Color);
    SetWordWrap(LSource.WordWrap);
    SetVerticalAlignment(LSource.VerticalAlignment);
    SetOffset(LSource.Offset);
    SetDisabledColor(LSource.DisabledColor);
    // Individual setters call Changed. If Font.Assign is the only one,
    // one Changed might be missed if only other props changed.
    // However, TFont.Assign calls FontChanged which calls Self.Changed.
    // So multiple Changed calls might occur. A single Changed call at the end
    // after direct field assignments (FVisible := LSource.FVisible; etc.) would avoid this.
    // Sticking to user's setter pattern for now.
  end
  else
    inherited Assign(Source);
end;
// ... (Changed, FontChanged, and Setters as before)
procedure TCaptionSettings.Changed; begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure TCaptionSettings.FontChanged(Sender: TObject); begin Changed; end;
procedure TCaptionSettings.SetAlignment(const Value: TAlignment); begin if FAlignment <> Value then begin FAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TCaptionSettings.SetFont(const Value: TFont); begin FFont.Assign(Value); end;
procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition); begin if FPosition <> Value then begin FPosition := Value; Changed; end; end;
procedure TCaptionSettings.SetText(const Value: string); begin if FText <> Value then begin FText := Value; Changed; end; end;
procedure TCaptionSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TCaptionSettings.SetWordWrap(const Value: Boolean); begin if FWordWrap <> Value then begin FWordWrap := Value; Changed; end; end;
procedure TCaptionSettings.SetVerticalAlignment(const Value: TCaptionVerticalAlignment); begin if FVerticalAlignment <> Value then begin FVerticalAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetOffset(const Value: TPoint); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end;
procedure TCaptionSettings.SetDisabledColor(const Value: TColor); begin if FDisabledColor <> Value then begin FDisabledColor := Value; Changed; end; end;


{ THoverSettings - Assign pattern updated }
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
  FCurrentAnimationValue := 0; // Initial animation state
  FAnimationDirection := 0;   // Initial animation state
  FAnimationTimer := TTimer.Create(nil); // Owner should ideally be FOwnerControl if it's a component, or nil if FOwnerControl can be nil/non-component
  FAnimationTimer.Interval := FAnimationTimerInterval;
  FAnimationTimer.OnTimer := DoAnimate;
  FAnimationTimer.Enabled := False;
end;

destructor THoverSettings.Destroy;
begin
  FAnimationTimer.Free; // TTimer.Free handles Enabled := False and unhooking if owned.
  FAnimationTimer := nil;
  inherited Destroy;
end;

procedure THoverSettings.Assign(Source: TPersistent);
var
  LSource: THoverSettings;
begin
  if Source is THoverSettings then
  begin
    LSource := THoverSettings(Source);
    // FOwnerControl is not assigned.
    SetEnabled(LSource.Enabled);
    SetBackgroundColor(LSource.BackgroundColor);
    SetBorderColor(LSource.BorderColor);
    SetFontColor(LSource.FontColor);
    SetCaptionFontColor(LSource.CaptionFontColor);
    SetHoverEffect(LSource.HoverEffect);
    SetAnimationTimerInterval(LSource.AnimationTimerInterval); // Setter updates timer
    SetAnimationStep(LSource.AnimationStep);

    // FCurrentAnimationValue and FAnimationDirection are runtime states,
    // typically not copied during Assign. They should reset or be determined
    // by the new state (e.g. if Enabled is false, animation stops).
    // The current setters (e.g. SetEnabled, SetHoverEffect) already reset animation state.
    // So, not assigning FCurrentAnimationValue explicitly here is correct.
    // Original code copied FCurrentAnimationValue, which is generally not advised for runtime state.
    // Changed; // All setters call Changed, so this might be redundant or cause multiple calls.
    // If a single notification is preferred: assign to fields directly, then call Changed once.
  end
  else
    inherited Assign(Source);
end;
// ... (Changed and Setters as before, ensure setters handle animation state resets correctly)
procedure THoverSettings.Changed; begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure THoverSettings.SetBackgroundColor(const Value: TColor); begin if FBackgroundColor <> Value then begin FBackgroundColor := Value; Changed; end; end;
procedure THoverSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure THoverSettings.SetCaptionFontColor(const Value: TColor); begin if FCaptionFontColor <> Value then begin FCaptionFontColor := Value; Changed; end; end;
procedure THoverSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then // If disabled, stop and reset animation
    begin
      FAnimationTimer.Enabled := False;
      FCurrentAnimationValue := 0;
      FAnimationDirection := 0;
      // Potentially trigger OnAnimationProgress or Invalidate owner if visual state changes immediately
      if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
      if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
    end;
    Changed;
  end;
end;
procedure THoverSettings.SetFontColor(const Value: TColor); begin if FFontColor <> Value then begin FFontColor := Value; Changed; end; end;
procedure THoverSettings.SetHoverEffect(const Value: THoverEffect);
begin
  if FHoverEffect <> Value then
  begin
    FHoverEffect := Value;
    FAnimationTimer.Enabled := False; // Stop current animation
    FCurrentAnimationValue := 0;    // Reset animation state
    FAnimationDirection := 0;
    // Potentially trigger OnAnimationProgress or Invalidate owner
    if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
    Changed;
  end;
end;
procedure THoverSettings.SetAnimationTimerInterval(const Value: Integer);
begin
  if FAnimationTimerInterval <> Value then
  begin
    FAnimationTimerInterval := Max(1, Value); // Interval must be > 0
    FAnimationTimer.Interval := FAnimationTimerInterval;
    Changed;
  end;
end;
procedure THoverSettings.SetAnimationStep(const Value: Integer);
begin
  if FAnimationStep <> Value then
  begin
    FAnimationStep := Max(1, Value); // Step should be positive
    Changed;
  end;
end;
// ... (DoAnimate, StartAnimation as before)
procedure THoverSettings.DoAnimate(Sender: TObject);
var
  TargetValue: Integer;
  OldAnimationValue: Integer;
begin
  OldAnimationValue := FCurrentAnimationValue;

  if FAnimationDirection = 1 then // Fading In / Scaling Up
    TargetValue := 255
  else if FAnimationDirection = -1 then // Fading Out / Scaling Down
    TargetValue := 0
  else // No direction, animation should not be running
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
      FAnimationDirection := 0; // Animation complete
    end;
  end
  else // FAnimationDirection = -1
  begin
    Dec(FCurrentAnimationValue, FAnimationStep);
    if FCurrentAnimationValue <= TargetValue then
    begin
      FCurrentAnimationValue := TargetValue;
      FAnimationTimer.Enabled := False;
      FAnimationDirection := 0; // Animation complete
    end;
  end;

  if FCurrentAnimationValue <> OldAnimationValue then // Only notify/invalidate if value actually changed
  begin
    if Assigned(FOnAnimationProgress) then
      FOnAnimationProgress(Self);

    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
      FOwnerControl.Invalidate;
  end
  else if not FAnimationTimer.Enabled then // Value didn't change but timer stopped (reached target exactly)
  begin
     // Ensure final state is communicated if it landed on target without overshooting
     if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
     if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
  end;
end;

procedure THoverSettings.StartAnimation(IsHovering: Boolean);
begin
  if not Self.Enabled then // If hover effects are globally disabled
  begin
    // Ensure animation is stopped and value reflects non-hovered state
    if (FCurrentAnimationValue <> 0) or FAnimationTimer.Enabled then
    begin
        FAnimationTimer.Enabled := False;
        FCurrentAnimationValue := 0;
        FAnimationDirection := 0;
        if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
        if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
    end;
    Exit;
  end;

  if FHoverEffect = heNone then
  begin
    // Effect is none, but we might need to set a final state (fully on or off)
    var TargetValue: Integer;
    if IsHovering then TargetValue := 255 else TargetValue := 0;
    if (FCurrentAnimationValue <> TargetValue) or FAnimationTimer.Enabled then
    begin
        FAnimationTimer.Enabled := False;
        FCurrentAnimationValue := TargetValue;
        FAnimationDirection := 0;
        if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
        if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
    end;
    Exit;
  end;

  // Setup for active animation (heFade, heScale)
  FAnimationTimer.Interval := FAnimationTimerInterval;

  if IsHovering then
  begin
    if FCurrentAnimationValue < 255 then // Only start if not already fully hovered
    begin
      FAnimationDirection := 1; // Animate In
      FAnimationTimer.Enabled := True;
    end
    else if FAnimationDirection <> 0 then // If it was animating out, but now hovering again
    begin
        FAnimationDirection := 1;
        FAnimationTimer.Enabled := True;
    end
  end
  else // Not hovering
  begin
    if FCurrentAnimationValue > 0 then // Only start if not already fully non-hovered
    begin
      FAnimationDirection := -1; // Animate Out
      FAnimationTimer.Enabled := True;
    end
     else if FAnimationDirection <> 0 then // If it was animating in, but now not hovering
    begin
        FAnimationDirection := -1;
        FAnimationTimer.Enabled := True;
    end
  end;
end;


{ TImageSettings - Assign using setters }
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
  FTargetWidth := 0;  // Added initialization
  FTargetHeight := 0; // Added initialization
end;

destructor TImageSettings.Destroy;
begin
  if Assigned(FPicture) then
  begin
    FPicture.OnChange := nil;
    FPicture.Free;
    FPicture := nil;
  end;
  if Assigned(FMargins) then
  begin
    FMargins.OnChange := nil;
    FMargins.Free;
    FMargins := nil;
  end;
  inherited Destroy;
end;

procedure TImageSettings.Assign(Source: TPersistent);
var
  LSource: TImageSettings;
begin
  if Source is TImageSettings then
  begin
    LSource := TImageSettings(Source);
    // FOwnerControl not assigned
    SetVisible(LSource.Visible);
    SetDrawMode(LSource.DrawMode);
    SetPicture(LSource.Picture); // Uses TPicture.Assign, which handles its own change notification
    SetMargins(LSource.Margins); // Uses TANDMR_Margins.Assign
    SetPosition(LSource.Position);
    SetAlignmentVertical(LSource.AlignmentVertical);
    SetPlacement(LSource.Placement);
    SetTargetWidth(LSource.TargetWidth);     // Added assignment
    SetTargetHeight(LSource.TargetHeight);   // Added assignment
    // DoChange is called by setters.
  end
  else
    inherited Assign(Source);
end;

procedure TImageSettings.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;
// ... (Internal*Changed and Setters as before)
procedure TImageSettings.InternalMarginsChanged(Sender: TObject); begin DoChange; end;
procedure TImageSettings.InternalPictureChanged(Sender: TObject); begin DoChange; end;
procedure TImageSettings.SetDrawMode(const Value: TImageDrawMode); begin if FDrawMode <> Value then begin FDrawMode := Value; DoChange; end; end;
procedure TImageSettings.SetMargins(const Value: TANDMR_Margins); begin FMargins.Assign(Value); end;
procedure TImageSettings.SetPicture(const Value: TPicture); begin FPicture.Assign(Value); end;
procedure TImageSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; DoChange; end; end;
procedure TImageSettings.SetPosition(const Value: TImagePositionSide); begin if FPosition <> Value then begin FPosition := Value; DoChange; end; end;
procedure TImageSettings.SetAlignmentVertical(const Value: TImageAlignmentVertical); begin if FAlignmentVertical <> Value then begin FAlignmentVertical := Value; DoChange; end; end;
procedure TImageSettings.SetPlacement(const Value: TImagePlacement); begin if FPlacement <> Value then begin FPlacement := Value; DoChange; end; end;

procedure TImageSettings.SetTargetWidth(const Value: Integer);
begin
  if FTargetWidth <> Value then
  begin
    FTargetWidth := Max(0, Value); // Ensure non-negative
    DoChange;
  end;
end;

procedure TImageSettings.SetTargetHeight(const Value: Integer);
begin
  if FTargetHeight <> Value then
  begin
    FTargetHeight := Max(0, Value); // Ensure non-negative
    DoChange;
  end;
end;

{ ColorToARGB }
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then // Or use IsTransparentColor for modern Delphi if TColor can have alpha
  begin
    Result := (UInt32(Alpha) shl 24); // Transparent black with specified alpha
    Exit;
  end;
  ColorRef := ColorToRGB(AColor); // Converts TColor to BGR format (strips any TColor alpha)
  Result := (UInt32(Alpha) shl 24) or        // Alpha
            ((ColorRef and $000000FF) shl 16) or // Blue to ARGB Red
            (ColorRef and $0000FF00) or          // Green to ARGB Green
            ((ColorRef and $00FF0000) shr 16);   // Red to ARGB Blue
end;

{ CreateGPRoundedPath - seems largely okay, minor adjustment for clarity }
// ... (as before, logic is complex but seems standard for rounded rect paths) ...
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.1; // Minimum radius to consider applying arcs
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
  Rect: TGPRectF; // Use a local copy to modify if necessary
begin
  if not Assigned(APath) then Exit; // Guard clause
  APath.Reset;
  Rect := ARect; // Work with a copy

  // Ensure non-negative dimensions
  if Rect.Width < 0 then Rect.Width := 0;
  if Rect.Height < 0 then Rect.Height := 0;

  // If the rectangle has no area, add a simple rectangle (which might be empty or a line)
  // or simply exit if an empty path is preferred for zero-area.
  // Current logic adds a rectangle if Width and Height > 0.
  if (Rect.Width <= 0) or (Rect.Height <= 0) then
  begin
    // APath.AddRectangle(Rect); // Optionally add if behavior for line/point is desired
    Exit; // Or exit, resulting in an empty path
  end;

  LRadius := ARadiusValue;
  // Ensure radius is not more than half the smallest dimension
  LRadius := Min(LRadius, Rect.Width / 2.0);
  LRadius := Min(LRadius, Rect.Height / 2.0);
  LRadius := Max(0.0, LRadius); // Ensure radius is non-negative

  LDiameter := LRadius * 2.0;

  // If no rounding is requested, radius is too small, or rect is too small for the diameter
  if (AType = rctNone) or (LRadius < MIN_RADIUS_FOR_PATH) or (LDiameter <= 0) or
     (Rect.Width < LDiameter) or (Rect.Height < LDiameter) then
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
    APath.AddLine(Rect.X, Rect.Y, Rect.X, Rect.Y); // Degenerate line to set start point

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

  APath.CloseFigure; // Closes path by connecting to the start of the first segment (end of TL arc or TL point)
end;

{ DrawEditBox - seems okay }
// ... (as before)
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

{ DrawPNGImageWithGDI - Corrected stream handling }
procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
var
  DrawImageRect: TRect;
  GraphicW, GraphicH: Integer;
  rRatio, rRectRatio: Double;
  tempCalculatedW, tempCalculatedH: Double;
  PngStream: TMemoryStream;
  Adapter: IStream; // Interface, will be reference counted
  GpSourceBitmap: TGPBitmap;
  // E: Exception; // Removed, using structured exception handling
begin
  if (AGraphics = nil) or (APNG = nil) or APNG.Empty then Exit; // Added APNG.Empty check
  if (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then Exit;

  GraphicW := APNG.Width;
  GraphicH := APNG.Height;

  if (GraphicW <= 0) or (GraphicH <= 0) then
  begin
    // For idmNormal, an empty graphic might still be "drawn" (as nothing) at a point.
    // For other modes, it's usually an error or no-op.
    // Current logic exits if not idmNormal. This seems fine.
    if ADrawMode <> idmNormal then Exit;
    // If idmNormal, allow to proceed, DrawImageRect will be 0x0 or based on GraphicW/H.
  end;

  // ... (Calculation of DrawImageRect as before, ensure it's robust for GraphicW/H = 0 in idmNormal)
case ADrawMode of
  idmStretch:
    DrawImageRect := ADestRect;
  idmProportional:
    begin
      if (GraphicH = 0) or (GraphicW = 0) then // Avoid division by zero if image is empty
      begin
          DrawImageRect := System.Types.Rect(ADestRect.Left, ADestRect.Top, ADestRect.Left, ADestRect.Top); // Empty rect
          // Or center a 0x0 rect:
          // DrawImageRect.Width := 0; DrawImageRect.Height := 0;
      end else
      begin
        rRatio := GraphicW / GraphicH;
        if ADestRect.Height > 0 then
          rRectRatio := ADestRect.Width / ADestRect.Height
        else
          rRectRatio := MaxDouble; // Effectively fit to width if DestRect height is 0

        if rRectRatio > rRatio then // Fit to height
        begin
          DrawImageRect.Height := ADestRect.Height;
          tempCalculatedW := ADestRect.Height * rRatio;
          DrawImageRect.Width := Round(tempCalculatedW);
          if (DrawImageRect.Width = 0) and (tempCalculatedW > 0) and (ADestRect.Width > 0) then
            DrawImageRect.Width := 1; // Ensure minimum 1px if calculated positive
        end
        else // Fit to width (or if DestRect height is 0)
        begin
          DrawImageRect.Width := ADestRect.Width;
          if rRatio > 0 then // Avoid division by zero if rRatio is 0 (e.g. GraphicW = 0, GraphicH > 0)
          begin
            tempCalculatedH := ADestRect.Width / rRatio;
            DrawImageRect.Height := Round(tempCalculatedH);
            if (DrawImageRect.Height = 0) and (tempCalculatedH > 0) and (ADestRect.Height > 0) then
              DrawImageRect.Height := 1; // Ensure minimum 1px
          end
          else // rRatio is 0 or invalid (e.g. GraphicW=0)
            DrawImageRect.Height := 0;
        end;
      end;
      // Common centering logic for proportional and normal (if GraphicW/H valid)
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
else // Should not happen if enum is complete
  DrawImageRect := ADestRect;
end;


  if (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0) then
    Exit; // Nothing to draw if calculated rect is empty or invalid

  PngStream := TMemoryStream.Create;
  try
    APNG.SaveToStream(PngStream);
    PngStream.Position := 0;
    // TStreamAdapter (if creating an instance) should be managed if not assigned to an interface.
    // Assigning to IStream handles its lifetime via reference counting.
    Adapter := TStreamAdapter.Create(PngStream, soReference); // soReference: Adapter does not own PngStream.
                                                              // PngStream must be freed manually.
    GpSourceBitmap := TGPBitmap.Create(Adapter); // GDI+ bitmap from stream
    try
      if (GpSourceBitmap = nil) or (GpSourceBitmap.GetLastStatus <> Ok) then
      begin
        // Optional: Log GpSourceBitmap.GetLastStatus error
        Exit;
      end;

      // Set interpolation mode for scaling
      if (DrawImageRect.Width <> GpSourceBitmap.GetWidth()) or (DrawImageRect.Height <> GpSourceBitmap.GetHeight()) then
        AGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic)
      else
        AGraphics.SetInterpolationMode(InterpolationModeDefault); // Or InterpolationModeNearestNeighbor for non-scaled

      AGraphics.DrawImage(GpSourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height);
    finally
      GpSourceBitmap.Free; // GpSourceBitmap must be freed.
      // Adapter (IStream) will be released automatically by ARC when it goes out of scope.
    end;
  except
    on E: Exception do
    begin
      // Optional: Log or handle exception E.
      // Example: Log.Error('Error in DrawPNGImageWithGDI: ' + E.Message);
    end;
  end; // Outer try..except
  // PngStream MUST be freed regardless of exceptions during GDI+ operations.

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

{ DrawSeparatorWithCanvas - reformatted }
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
var
  LineX: Integer;
  OldPenColor: TColor;
  OldPenWidth: Integer;
  OldPenStyle: TPenStyle;
begin
  if (ACanvas = nil) or (AThickness <= 0) or (ASepRect.Width <= 0) or (ASepRect.Height <= 0) then
    Exit;

  OldPenColor := ACanvas.Pen.Color;
  OldPenWidth := ACanvas.Pen.Width;
  OldPenStyle := ACanvas.Pen.Style;
  try
    LineX := ASepRect.Left + (ASepRect.Width div 2); // Center of the separator area
    ACanvas.Pen.Color := AColor;
    ACanvas.Pen.Width := AThickness;
    ACanvas.Pen.Style := psSolid; // Assuming separator is always solid

    ACanvas.MoveTo(LineX, ASepRect.Top);
    ACanvas.LineTo(LineX, ASepRect.Bottom);
  finally
    ACanvas.Pen.Color := OldPenColor;
    ACanvas.Pen.Width := OldPenWidth;
    ACanvas.Pen.Style := OldPenStyle;
  end;
end;

{ DarkerColor, LighterColor, BlendColors - renamed parameters for clarity, logic seems okay }
function DarkerColor(AColor: TColor; APercent: Byte = 30): TColor;
var
  R, G, B: Byte;
  Factor: Double;
begin
  if AColor = clNone then Exit(clNone);
  AColor := ColorToRGB(AColor); // Ensure it's an RGB value
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);

  Factor := Max(0, Min(100, APercent)) / 100.0; // Ensure Percent is between 0 and 100

  R := Round(R * (1.0 - Factor));
  G := Round(G * (1.0 - Factor));
  B := Round(B * (1.0 - Factor));
  Result := RGB(R, G, B);
end;

function LighterColor(AColor: TColor; APercent: Byte = 30): TColor;
var
  R, G, B: Byte;
  Factor: Double;
begin
  if AColor = clNone then Exit(clNone);
  AColor := ColorToRGB(AColor);
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);

  Factor := Max(0, Min(100, APercent)) / 100.0;

  R := Round(R + (255 - R) * Factor);
  G := Round(G + (255 - G) * Factor);
  B := Round(B + (255 - B) * Factor);
  Result := RGB(R, G, B);
end;

function BlendColors(AColor1, AColor2: TColor; AFactor: Single): TColor;
var
  R1, G1, B1, R2, G2, B2, R, G, B: Byte;
  IsTransparent1, IsTransparent2: Boolean;
  EffectiveFactor: Single;
begin
  EffectiveFactor := Max(0.0, Min(1.0, AFactor)); // Clamp factor to [0, 1]

  if EffectiveFactor <= 0.0 then Exit(AColor1);
  if EffectiveFactor >= 1.0 then Exit(AColor2);

  // Consider clNone as fully transparent.
  // For TColors with alpha (Delphi XE+), this check might need to be more sophisticated
  // if alpha blending is desired instead of just treating clNone as a special case.
  IsTransparent1 := (AColor1 = clNone);
  IsTransparent2 := (AColor2 = clNone);

  if IsTransparent1 and IsTransparent2 then Exit(clNone);
  // If one color is transparent, blending typically means taking the other color,
  // potentially with its alpha adjusted by the factor.
  // Current logic returns the non-transparent color directly if the other is clNone.
  // This might be the desired behavior (e.g. factor is how much of Color2 to show over Color1).
  if IsTransparent1 then Exit(AColor2); // Or a more complex alpha blend if Color2 has alpha
  if IsTransparent2 then Exit(AColor1); // Or a more complex alpha blend if Color1 has alpha

  // Both colors are non-clNone, proceed with RGB blending
  AColor1 := ColorToRGB(AColor1); // Strip any potential alpha from TColor, work with pure RGB
  AColor2 := ColorToRGB(AColor2);

  R1 := GetRValue(AColor1); G1 := GetGValue(AColor1); B1 := GetBValue(AColor1);
  R2 := GetRValue(AColor2); G2 := GetGValue(AColor2); B2 := GetBValue(AColor2);

  R := Round(R1 + (R2 - R1) * EffectiveFactor);
  G := Round(G1 + (G2 - G1) * EffectiveFactor);
  B := Round(B1 + (B2 - B1) * EffectiveFactor);
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
