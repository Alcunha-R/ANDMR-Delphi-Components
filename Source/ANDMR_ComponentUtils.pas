unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.UITypes, Vcl.ExtCtrls, System.Types, System.Variants;

// TYPE DECLARATIONS
type
  THoverEffect = (heNone, heFade, heScale);
  TCaptionVerticalAlignment = (cvaTop, cvaCenter, cvaBottom);
  TImagePosition = (ipLeft, ipRight, ipTop, ipBottom, ipFill, ipCenter);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);
  TGradientType = (gtLinearVertical, gtLinearHorizontal, gtRadial, gtDiagonalDown, gtDiagonalUp, gtCenterBurst);
  TRoundCornerType = (rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight, rctTop, rctBottom, rctLeft, rctRight, rctTopLeftBottomRight, rctTopRightBottomLeft);
  TInputType = (itNormal, itLettersOnly, itNumbersOnly, itNoSpecialChars, itAlphaNumericOnly);
  TTextCase = (tcNormal, tcUppercase, tcLowercase);
  TCaptionPosition = (cpAbove, cpBelow, cpLeft, cpRight);
  TPredefinedMaskType = (pmtNone, pmtCustom, pmtCPF, pmtCNPJ, pmtCEP, pmtPhoneBR, pmtDateDMY);
  TCEditStatus = (cepsNormal, cepsError, cepsWarning, cepsSuccess);
  TProgressAnimationStyle = (pasRotatingSemiCircle, pasFullCircularSpinner, pasHorizontalBar, pasBouncingDots);
  TImageHorizontalAlignment = (ihaLeft, ihaCenter, ihaRight);
  TImageVerticalAlignment = (ivaTop, ivaCenter, ivaBottom);

  TClickSettings = class(TPersistent)
  private
    FEnabled: Boolean;
    FColor: TColor;
    FBorderColor: TColor;
    FFontColor: TColor;
    FDuration: Integer;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetDuration(const Value: Integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Color: TColor read FColor write SetColor default clNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property FontColor: TColor read FFontColor write SetFontColor default clNone;
    property Duration: Integer read FDuration write SetDuration default 100;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TANDMR_MultiTag = class(TPersistent)
  private
    FTag: NativeInt;
    FString: string;
    FExtended: TStringList;
    FObject: TObject;
    FOnChange: TNotifyEvent;
    procedure SetTag(const Value: NativeInt);
    procedure SetString(const Value: string);
    procedure SetExtended(const Value: TStringList);
    procedure SetObject(const Value: TObject);
    procedure ExtendedChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AsTag: NativeInt read FTag write SetTag default 0;
    property AsString: string read FString write SetString;
    property AsExtended: TStringList read FExtended write SetExtended;
    property AsObject: TObject read FObject write SetObject;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    FAlignment: TAlignment;
    FFont: TFont;
    FColor: TColor;
    FWordWrap: Boolean;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl;
    FVerticalAlignment: TCaptionVerticalAlignment;
    FOffset: TPoint;
    FDisabledColor: TColor;
    FMargins: TANDMR_Margins;
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
    procedure SetMargins(const Value: TANDMR_Margins);
  protected
    procedure Changed; virtual;
    procedure InternalMarginsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Text: string read FText write SetText;
    property Position: TCaptionPosition read FPosition write SetPosition default cpAbove;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property VerticalAlignment: TCaptionVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default cvaCenter;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGrayText;
    property Offset: TPoint read FOffset write SetOffset;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Margins: TANDMR_Margins read FMargins write SetMargins;
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
    FPosition: TImagePosition;
    FPlacement: TImagePlacement;
    FTargetWidth: Integer;
    FTargetHeight: Integer;
    FAutoSize: Boolean;
    FHorizontalAlign: TImageHorizontalAlignment;
    FVerticalAlign: TImageVerticalAlignment;
    procedure SetPicture(const Value: TPicture);
    procedure SetVisible(const Value: Boolean);
    procedure SetDrawMode(const Value: TImageDrawMode);
    procedure SetMargins(const Value: TANDMR_Margins);
    procedure SetPosition(const Value: TImagePosition);
    procedure SetPlacement(const Value: TImagePlacement);
    procedure SetTargetWidth(const Value: Integer);
    procedure SetTargetHeight(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetHorizontalAlign(const Value: TImageHorizontalAlignment);
    procedure SetVerticalAlign(const Value: TImageVerticalAlignment);
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
    property Position: TImagePosition read FPosition write SetPosition default ipLeft;
    property Placement: TImagePlacement read FPlacement write SetPlacement default iplInsideBounds;
    property TargetWidth: Integer read FTargetWidth write SetTargetWidth default 0;
    property TargetHeight: Integer read FTargetHeight write SetTargetHeight default 0;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HorizontalAlign: TImageHorizontalAlignment read FHorizontalAlign write SetHorizontalAlign default ihaCenter;
    property VerticalAlign: TImageVerticalAlignment read FVerticalAlign write SetVerticalAlign default ivaCenter;
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
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetThickness(const Value: Integer);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
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
    property Offset: TPoint read FOffset write SetOffset;
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
    FAnimationProgressStep: Integer;
    FOwnerControl: TWinControl;
    FOnChange: TNotifyEvent;
    FAnimationStyle: TProgressAnimationStyle;
    FProgressText: string;
    FShowProgressText: Boolean;
    procedure SetShowProgress(const Value: Boolean);
    procedure SetProgressColor(const Value: TColor);
    procedure SetHideCaptionWhileProcessing(const Value: Boolean);
    procedure SetAnimationTimerInterval(const Value: Integer);
    procedure SetAnimationProgressStep(const Value: Integer);
    procedure SetAnimationStyle(const Value: TProgressAnimationStyle);
    procedure SetProgressText(const Value: string);
    procedure SetShowProgressText(const Value: Boolean);
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
    property AnimationTimerInterval: Integer read FAnimationTimerInterval write SetAnimationTimerInterval default 40;
    property AnimationProgressStep: Integer read FAnimationProgressStep write SetAnimationProgressStep default 5;
    property AnimationStyle: TProgressAnimationStyle read FAnimationStyle write SetAnimationStyle default pasRotatingSemiCircle;
    property ProgressText: string read FProgressText write SetProgressText;
    property ShowProgressText: Boolean read FShowProgressText write SetShowProgressText default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

// HELPER FUNCTIONS
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
function DarkerColor(AColor: TColor; APercent: Byte = 30): TColor;
function LighterColor(AColor: TColor; APercent: Byte = 30): TColor;
function BlendColors(AColor1, AColor2: TColor; AFactor: Single): TColor;
procedure DrawComponentCaption(ACanvas: TCanvas; const ARect: TRect; const ACaption: string; AFont: TFont; AFontColor: TColor; AAlignmentHorizontal: TAlignment; AAlignmentVertical: TCaptionVerticalAlignment; AWordWrap: Boolean; AOpacity: Byte);
function ResolveStateColor(AIsEnabled: Boolean; AIsHovering: Boolean; AIsFocused: Boolean; ABaseColor: TColor; AHoverColor: TColor; AFocusColor: TColor; ADisabledColor: TColor; AAllowHoverEffect: Boolean = True; AAllowFocusEffect: Boolean = True; AHoverEffectOverridesFocus: Boolean = False; AFallbackToTransparent: Boolean = False): TColor;
function CalculateProportionalRect(const DestRect: TRect; ImgWidth, ImgHeight: Integer): TRect;

procedure DrawPNGImageWithGDI(AGraphics: TGPGraphics; APNG: TPNGImage; ADestRect: TRect; ADrawMode: TImageDrawMode);
procedure DrawNonPNGImageWithCanvas(ACanvas: TCanvas; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);

const
  ColorMatrixIdentity: TColorMatrix =
   ((1.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0));

implementation

uses
  System.Math,
  Winapi.ActiveX;

{ TClickSettings }

constructor TClickSettings.Create;
begin
  inherited Create;
  FEnabled := True;
  FColor := clNone;
  FBorderColor := clNone;
  FFontColor := clNone;
  FDuration := 100;
end;

procedure TClickSettings.Assign(Source: TPersistent);
var
  LSource: TClickSettings;
begin
  if Source is TClickSettings then
  begin
    LSource := TClickSettings(Source);
    Self.FEnabled := LSource.FEnabled;
    Self.FColor := LSource.FColor;
    Self.FBorderColor := LSource.FBorderColor;
    Self.FFontColor := LSource.FFontColor;
    Self.FDuration := LSource.FDuration;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TClickSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TClickSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; end; end;
procedure TClickSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TClickSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure TClickSettings.SetFontColor(const Value: TColor); begin if FFontColor <> Value then begin FFontColor := Value; Changed; end; end;
procedure TClickSettings.SetDuration(const Value: Integer); begin if FDuration <> Max(0, Value) then begin FDuration := Max(0, Value); Changed; end; end;

{ TBorderSettings }

constructor TBorderSettings.Create;
begin
  inherited Create;
  FVisible := True;
  FColor := clDkGray;
  FThickness := 1;
  FStyle := psSolid;
  FCornerRadius := 4;
  FRoundCornerType := rctAll;
  FBackgroundColor := clNone;
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
    FVisible := LSource.FVisible;
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
    FCornerRadius := Max(0, Value);
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

{ TFocusSettings }
constructor TFocusSettings.Create;
begin
  inherited Create;
  FBorderColor := clHighlight;
  FBorderColorVisible := False;
  FBackgroundColor := clNone;
  FBackgroundColorVisible := False;
  FUnderlineColor := clHighlight;
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

procedure TFocusSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure TFocusSettings.SetBorderColorVisible(const Value: Boolean); begin if FBorderColorVisible <> Value then begin FBorderColorVisible := Value; Changed; end; end;
procedure TFocusSettings.SetBackgroundColor(const Value: TColor); begin if FBackgroundColor <> Value then begin FBackgroundColor := Value; Changed; end; end;
procedure TFocusSettings.SetBackgroundColorVisible(const Value: Boolean); begin if FBackgroundColorVisible <> Value then begin FBackgroundColorVisible := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineColor(const Value: TColor); begin if FUnderlineColor <> Value then begin FUnderlineColor := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineVisible(const Value: Boolean); begin if FUnderlineVisible <> Value then begin FUnderlineVisible := Value; Changed; end; end;
procedure TFocusSettings.SetUnderlineThickness(const Value: Integer); begin if FUnderlineThickness <> Max(0, Value) then begin FUnderlineThickness := Max(0, Value); Changed; end; end;
procedure TFocusSettings.SetUnderlineStyle(const Value: TPenStyle); begin if FUnderlineStyle <> Value then begin FUnderlineStyle := Value; Changed; end; end;

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

procedure TSeparatorSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TSeparatorSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TSeparatorSettings.SetThickness(const Value: Integer); begin if FThickness <> Max(0, Value) then begin FThickness := Max(0, Value); Changed; end; end;
procedure TSeparatorSettings.SetPadding(const Value: Integer); begin if FPadding <> Value then begin FPadding := Value; Changed; end; end;
procedure TSeparatorSettings.SetHeightMode(const Value: TSeparatorHeightMode); begin if FHeightMode <> Value then begin FHeightMode := Value; Changed; end; end;
procedure TSeparatorSettings.SetCustomHeight(const Value: Integer); begin if FCustomHeight <> Max(0,Value) then begin FCustomHeight := Max(0,Value); Changed; end; end;

{ TDropShadowSettings }
constructor TDropShadowSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FColor := clGray;
  FOffset := System.Types.Point(2, 2);
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

procedure TDropShadowSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; end; end;
procedure TDropShadowSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TDropShadowSettings.SetOffset(const Value: TPoint); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end;
procedure TDropShadowSettings.SetBlurRadius(const Value: Integer); begin if FBlurRadius <> Max(0, Value) then begin FBlurRadius := Max(0, Value); Changed; end; end;

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
  FOwnerControl := AOwnerControl;
  FShowProgress := True;
  FProgressColor := clGray;
  FHideCaptionWhileProcessing := True;
  FAnimationTimerInterval := 40;
  FAnimationProgressStep := 5;
  FAnimationStyle := pasRotatingSemiCircle;
  FProgressText := '';
  FShowProgressText := False;
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
    SetAnimationProgressStep(LSource.AnimationProgressStep);
    SetAnimationStyle(LSource.AnimationStyle);
    SetProgressText(LSource.ProgressText);
    SetShowProgressText(LSource.ShowProgressText);
  end
  else
    inherited Assign(Source);
end;

procedure TProgressSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOwnerControl) and (FOwnerControl.HandleAllocated) then
  begin
    if (csDesigning in FOwnerControl.ComponentState) then
      FOwnerControl.Invalidate
    else
      FOwnerControl.Repaint;
  end;
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

procedure TProgressSettings.SetAnimationProgressStep(const Value: Integer);
begin
  if FAnimationProgressStep <> Max(1, Value) then
  begin
    FAnimationProgressStep := Max(1, Value);
    Changed;
  end;
end;

procedure TProgressSettings.SetAnimationStyle(const Value: TProgressAnimationStyle);
begin
  if FAnimationStyle <> Value then
  begin
    FAnimationStyle := Value;
    Changed;
  end;
end;

procedure TProgressSettings.SetProgressText(const Value: string);
begin
  if FProgressText <> Value then
  begin
    FProgressText := Value;
    Changed;
  end;
end;

procedure TProgressSettings.SetShowProgressText(const Value: Boolean);
begin
  if FShowProgressText <> Value then
  begin
    FShowProgressText := Value;
    Changed;
  end;
end;

{ TANDMR_MultiTag }

constructor TANDMR_MultiTag.Create;
begin
  inherited Create;
  FTag := 0;
  FString := '';
  FObject := nil;
  FExtended := TStringList.Create;
  FExtended.OnChange := ExtendedChanged;
end;

destructor TANDMR_MultiTag.Destroy;
begin
  FExtended.Free;
  inherited Destroy;
end;

procedure TANDMR_MultiTag.Assign(Source: TPersistent);
var
  LSource: TANDMR_MultiTag;
begin
  if Source is TANDMR_MultiTag then
  begin
    LSource := TANDMR_MultiTag(Source);
    Self.FTag := LSource.FTag;
    Self.FString := LSource.FString;
    Self.FObject := LSource.FObject;
    Self.FExtended.Assign(LSource.FExtended);
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TANDMR_MultiTag.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TANDMR_MultiTag.ExtendedChanged(Sender: TObject);
begin
  Changed;
end;

procedure TANDMR_MultiTag.SetTag(const Value: NativeInt);
begin
  if FTag <> Value then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TANDMR_MultiTag.SetString(const Value: string);
begin
  if FString <> Value then
  begin
    FString := Value;
    Changed;
  end;
end;

procedure TANDMR_MultiTag.SetExtended(const Value: TStringList);
begin
  FExtended.Assign(Value);
end;

procedure TANDMR_MultiTag.SetObject(const Value: TObject);
begin
  if FObject <> Value then
  begin
    FObject := Value;
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

procedure TANDMR_Margins.Changed; begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure TANDMR_Margins.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TANDMR_Margins.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TANDMR_Margins.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TANDMR_Margins.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;

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
  FOffset := System.Types.Point(0, 0);
  FDisabledColor := clGrayText;
  FMargins := TANDMR_Margins.Create;
  FMargins.OnChange := InternalMarginsChanged;
end;

destructor TCaptionSettings.Destroy;
begin
  if Assigned(FFont) then
  begin
    FFont.OnChange := nil;
    FFont.Free;
    FFont := nil;
  end;
  if Assigned(FMargins) then
  begin
    FMargins.OnChange := nil;
    FMargins.Free;
    FMargins := nil;
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
    SetVisible(LSource.Visible);
    SetText(LSource.Text);
    SetPosition(LSource.Position);
    SetAlignment(LSource.Alignment);
    SetFont(LSource.Font);
    SetColor(LSource.Color);
    SetWordWrap(LSource.WordWrap);
    SetVerticalAlignment(LSource.VerticalAlignment);
    SetOffset(LSource.Offset);
    SetDisabledColor(LSource.DisabledColor);
    SetMargins(LSource.Margins);
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

procedure TCaptionSettings.InternalMarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCaptionSettings.SetAlignment(const Value: TAlignment); begin if FAlignment <> Value then begin FAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TCaptionSettings.SetFont(const Value: TFont); begin FFont.Assign(Value); end;
procedure TCaptionSettings.SetMargins(const Value: TANDMR_Margins); begin FMargins.Assign(Value); end;
procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition); begin if FPosition <> Value then begin FPosition := Value; Changed; end; end;
procedure TCaptionSettings.SetText(const Value: string); begin if FText <> Value then begin FText := Value; Changed; end; end;
procedure TCaptionSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TCaptionSettings.SetWordWrap(const Value: Boolean); begin if FWordWrap <> Value then begin FWordWrap := Value; Changed; end; end;
procedure TCaptionSettings.SetVerticalAlignment(const Value: TCaptionVerticalAlignment); begin if FVerticalAlignment <> Value then begin FVerticalAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetOffset(const Value: TPoint); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end;
procedure TCaptionSettings.SetDisabledColor(const Value: TColor); begin if FDisabledColor <> Value then begin FDisabledColor := Value; Changed; end; end;

{ THoverSettings }
constructor THoverSettings.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
  FEnabled := True;
  FBackgroundColor := TColor($00E6E6E6);
  FBorderColor := clGrayText;
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
    SetEnabled(LSource.Enabled);
    SetBackgroundColor(LSource.BackgroundColor);
    SetBorderColor(LSource.BorderColor);
    SetFontColor(LSource.FontColor);
    SetCaptionFontColor(LSource.CaptionFontColor);
    SetHoverEffect(LSource.HoverEffect);
    SetAnimationTimerInterval(LSource.AnimationTimerInterval);
    SetAnimationStep(LSource.AnimationStep);
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

procedure THoverSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then
    begin
      FAnimationTimer.Enabled := False;
      FCurrentAnimationValue := 0;
      FAnimationDirection := 0;
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
    FAnimationTimer.Enabled := False;
    FCurrentAnimationValue := 0;
    FAnimationDirection := 0;
    if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
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
  OldAnimationValue: Integer;
begin
  OldAnimationValue := FCurrentAnimationValue;

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
  else
  begin
    Dec(FCurrentAnimationValue, FAnimationStep);
    if FCurrentAnimationValue <= TargetValue then
    begin
      FCurrentAnimationValue := TargetValue;
      FAnimationTimer.Enabled := False;
      FAnimationDirection := 0;
    end;
  end;

  if FCurrentAnimationValue <> OldAnimationValue then
  begin
    if Assigned(FOnAnimationProgress) then
      FOnAnimationProgress(Self);
    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
      FOwnerControl.Invalidate;
  end
  else if not FAnimationTimer.Enabled then
  begin
     if Assigned(FOnAnimationProgress) then FOnAnimationProgress(Self);
     if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then FOwnerControl.Invalidate;
  end;
end;

procedure THoverSettings.StartAnimation(IsHovering: Boolean);
begin
  if not Self.Enabled then
  begin
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

  FAnimationTimer.Interval := FAnimationTimerInterval;

  if IsHovering then
  begin
    if FCurrentAnimationValue < 255 then
    begin
      FAnimationDirection := 1;
      FAnimationTimer.Enabled := True;
    end
    else if FAnimationDirection <> 0 then
    begin
        FAnimationDirection := 1;
        FAnimationTimer.Enabled := True;
    end
  end
  else
  begin
    if FCurrentAnimationValue > 0 then
    begin
      FAnimationDirection := -1;
      FAnimationTimer.Enabled := True;
    end
     else if FAnimationDirection <> 0 then
    begin
        FAnimationDirection := -1;
        FAnimationTimer.Enabled := True;
    end
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
  FPosition := ipLeft;
  FPlacement := iplInsideBounds;
  FTargetWidth := 0;
  FTargetHeight := 0;
  FAutoSize := True;
  FHorizontalAlign := ihaCenter;
  FVerticalAlign := ivaCenter;
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
    SetVisible(LSource.Visible);
    SetDrawMode(LSource.DrawMode);
    SetPicture(LSource.Picture);
    SetMargins(LSource.Margins);
    SetPosition(LSource.Position);
    SetPlacement(LSource.Placement);
    SetTargetWidth(LSource.TargetWidth);
    SetTargetHeight(LSource.TargetHeight);
    SetAutoSize(LSource.AutoSize);
    SetHorizontalAlign(LSource.HorizontalAlign);
    SetVerticalAlign(LSource.VerticalAlign);
  end
  else
    inherited Assign(Source);
end;

procedure TImageSettings.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TImageSettings.InternalMarginsChanged(Sender: TObject); begin DoChange; end;
procedure TImageSettings.InternalPictureChanged(Sender: TObject); begin DoChange; end;
procedure TImageSettings.SetDrawMode(const Value: TImageDrawMode); begin if FDrawMode <> Value then begin FDrawMode := Value; DoChange; end; end;
procedure TImageSettings.SetMargins(const Value: TANDMR_Margins); begin FMargins.Assign(Value); end;
procedure TImageSettings.SetPicture(const Value: TPicture); begin FPicture.Assign(Value); end;
procedure TImageSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; DoChange; end; end;
procedure TImageSettings.SetPlacement(const Value: TImagePlacement); begin if FPlacement <> Value then begin FPlacement := Value; DoChange; end; end;

procedure TImageSettings.SetPosition(const Value: TImagePosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetTargetWidth(const Value: Integer);
begin
  if FTargetWidth <> Value then
  begin
    FTargetWidth := Max(0, Value);
    DoChange;
  end;
end;

procedure TImageSettings.SetTargetHeight(const Value: Integer);
begin
  if FTargetHeight <> Value then
  begin
    FTargetHeight := Max(0, Value);
    DoChange;
  end;
end;

procedure TImageSettings.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetHorizontalAlign(const Value: TImageHorizontalAlignment);
begin
  if FHorizontalAlign <> Value then
  begin
    FHorizontalAlign := Value;
    DoChange;
  end;
end;

procedure TImageSettings.SetVerticalAlign(const Value: TImageVerticalAlignment);
begin
  if FVerticalAlign <> Value then
  begin
    FVerticalAlign := Value;
    DoChange;
  end;
end;

{ ColorToARGB }
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then
  begin
    Result := (UInt32(Alpha) shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor);
  Result := (UInt32(Alpha) shl 24) or
            ((ColorRef and $000000FF) shl 16) or
            (ColorRef and $0000FF00) or
            ((ColorRef and $00FF0000) shr 16);
end;

{ CreateGPRoundedPath }
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.1;
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
  Rect: TGPRectF;
begin
  if not Assigned(APath) then Exit;
  APath.Reset;
  Rect := ARect;

  if Rect.Width < 0 then Rect.Width := 0;
  if Rect.Height < 0 then Rect.Height := 0;

  if (Rect.Width <= 0) or (Rect.Height <= 0) then
  begin
    Exit;
  end;

  LRadius := ARadiusValue;
  LRadius := Min(LRadius, Rect.Width / 2.0);
  LRadius := Min(LRadius, Rect.Height / 2.0);
  LRadius := Max(0.0, LRadius);

  LDiameter := LRadius * 2.0;

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

{ DrawEditBox }
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

{ DrawSeparatorWithCanvas }
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
    LineX := ASepRect.Left + (ASepRect.Width div 2);
    ACanvas.Pen.Color := AColor;
    ACanvas.Pen.Width := AThickness;
    ACanvas.Pen.Style := psSolid;

    ACanvas.MoveTo(LineX, ASepRect.Top);
    ACanvas.LineTo(LineX, ASepRect.Bottom);
  finally
    ACanvas.Pen.Color := OldPenColor;
    ACanvas.Pen.Width := OldPenWidth;
    ACanvas.Pen.Style := OldPenStyle;
  end;
end;

{ DarkerColor, LighterColor, BlendColors }
function DarkerColor(AColor: TColor; APercent: Byte = 30): TColor;
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
  EffectiveFactor := Max(0.0, Min(1.0, AFactor));

  if EffectiveFactor <= 0.0 then Exit(AColor1);
  if EffectiveFactor >= 1.0 then Exit(AColor2);

  IsTransparent1 := (AColor1 = clNone);
  IsTransparent2 := (AColor2 = clNone);

  if IsTransparent1 and IsTransparent2 then Exit(clNone);
  if IsTransparent1 then Exit(AColor2);
  if IsTransparent2 then Exit(AColor1);

  AColor1 := ColorToRGB(AColor1);
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

function CalculateProportionalRect(const DestRect: TRect; ImgWidth, ImgHeight: Integer): TRect;
var
  Ratio, RectRatio: Double;
begin
  if (ImgWidth <= 0) or (ImgHeight <= 0) or (DestRect.Width <= 0) or (DestRect.Height <= 0) then
  begin
    Result := System.Types.Rect(DestRect.Left, DestRect.Top, DestRect.Left, DestRect.Top);
    Exit;
  end;

  Ratio := ImgWidth / ImgHeight;
  RectRatio := DestRect.Width / DestRect.Height;

  if RectRatio > Ratio then
  begin
    Result.Height := DestRect.Height;
    Result.Width := Round(DestRect.Height * Ratio);
  end
  else
  begin
    Result.Width := DestRect.Width;
    Result.Height := Round(DestRect.Width / Ratio);
  end;

  Result.Left := DestRect.Left + (DestRect.Width - Result.Width) div 2;
  Result.Top := DestRect.Top + (DestRect.Height - Result.Height) div 2;
  Result.Right := Result.Left + Result.Width;
  Result.Bottom := Result.Top + Result.Height;
end;

end.
