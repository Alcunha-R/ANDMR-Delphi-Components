unit ANDMR_ComponentUtils;

{
  *****************************************************************************
  * *
  * Unit de Utilitários para Componentes Visuais ANDMR                *
  * *
  * Esta unidade fornece um conjunto de classes, tipos e rotinas auxiliares  *
  * para facilitar a criação de componentes VCL customizados com aparência   *
  * moderna. Inclui suporte a GDI+ para renderização avançada (cantos       *
  * arredondados, transparência, anti-aliasing), gerenciamento de estados    *
  * visuais (hover, focus, disabled), animações e configurações detalhadas   *
  * de aparência como bordas, sombras, gradientes e imagens.                 *
  * *
  *****************************************************************************
}

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, System.Variants,
  Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Winapi.Windows, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL;

type
  // Efeitos de Animação
  THoverEffect = (heNone, heFade, heScale);

  // Alinhamentos
  TCaptionVerticalAlignment = (cvaTop, cvaCenter, cvaBottom);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageHorizontalAlignment = (ihaLeft, ihaCenter, ihaRight);
  TImageVerticalAlignment = (ivaTop, ivaCenter, ivaBottom);
  TImagePosition = (ipLeft, ipRight, ipAbove, ipBelow, ipBehind);

  // Modos de Desenho e Aparência
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TImageStretchMode = (ismProportional, ismFlat); // Mantido para componentes que expõem uma propriedade simplificada
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);
  TGradientType = (gtLinearVertical, gtLinearHorizontal, gtRadial, gtDiagonalDown, gtDiagonalUp, gtCenterBurst);
  TRoundCornerType = (
    rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight,
    rctTop, rctBottom, rctLeft, rctRight,
    rctTopLeftBottomRight, rctTopRightBottomLeft
  );
  TProgressAnimationStyle = (pasRotatingSemiCircle, pasFullCircularSpinner, pasHorizontalBar, pasBouncingDots);

  // Tipos de Input e Texto
  TInputType = (itNormal, itLettersOnly, itNumbersOnly, itNoSpecialChars, itAlphaNumericOnly);
  TTextCase = (tcNormal, tcUppercase, tcLowercase);
  TCaptionPosition = (cpAbove, cpBelow, cpLeft, cpRight);
  TPredefinedMaskType = (pmtNone, pmtCustom, pmtCPF, pmtCNPJ, pmtCEP, pmtPhoneBR, pmtDateDMY);

  // Estados
  TCEditStatus = (cepsNormal, cepsError, cepsWarning, cepsSuccess);

  // Declaração antecipada para uso em TCaptionSettings
  TANDMR_Margins = class;

  { TClickSettings }
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

  { TANDMR_MultiTag }
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

  { TANDMR_Margins }
  TANDMR_Margins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
  protected
    procedure Changed; virtual;
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

  { TCaptionSettings }
  TCaptionSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FText: string;
    FPosition: TCaptionPosition;
    FAlignment: TAlignment;
    FVerticalAlignment: TCaptionVerticalAlignment;
    FFont: TFont;
    FColor: TColor;
    FDisabledColor: TColor;
    FWordWrap: Boolean;
    FOffset: TPoint;
    FMargins: TANDMR_Margins;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl;
    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetPosition(const Value: TCaptionPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetVerticalAlignment(const Value: TCaptionVerticalAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure SetMargins(const Value: TANDMR_Margins);
    procedure FontChanged(Sender: TObject);
    procedure InternalMarginsChanged(Sender: TObject);
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
    property VerticalAlignment: TCaptionVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default cvaCenter;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGrayText;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Offset: TPoint read FOffset write SetOffset;
    property Margins: TANDMR_Margins read FMargins write SetMargins;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { THoverSettings }
  THoverSettings = class(TPersistent)
  private
    FEnabled: Boolean;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FFontColor: TColor;
    FCaptionFontColor: TColor;
    FHoverEffect: THoverEffect;
    FAnimationTimerInterval: Integer;
    FAnimationStep: Integer;
    FCurrentAnimationValue: Integer;
    FAnimationDirection: Integer;
    FAnimationTimer: TTimer;
    FOwnerControl: TWinControl;
    FOnChange: TNotifyEvent;
    FOnAnimationProgress: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetCaptionFontColor(const Value: TColor);
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
    property HoverEffect: THoverEffect read FHoverEffect write SetHoverEffect default heFade;
    property AnimationTimerInterval: Integer read FAnimationTimerInterval write SetAnimationTimerInterval default 15;
    property AnimationStep: Integer read FAnimationStep write SetAnimationStep default 20;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAnimationProgress: TNotifyEvent read FOnAnimationProgress write FOnAnimationProgress;
    property CurrentAnimationValue: Integer read FCurrentAnimationValue;
  end;

  { TImageSettings }
  TImageSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FPicture: TPicture;
    FDrawMode: TImageDrawMode;
    FMargins: TANDMR_Margins;
    FPosition: TImagePosition;
    FPlacement: TImagePlacement;
    FHorizontalAlign: TImageHorizontalAlignment;
    FVerticalAlign: TImageVerticalAlignment;
    FAutoSize: Boolean;
    FTargetWidth: Integer;
    FTargetHeight: Integer;
    FOwnerControl: TWinControl;
    FOnChange: TNotifyEvent;
    procedure SetVisible(const Value: Boolean);
    procedure SetPicture(const Value: TPicture);
    procedure SetDrawMode(const Value: TImageDrawMode);
    procedure SetMargins(const Value: TANDMR_Margins);
    procedure SetPosition(const Value: TImagePosition);
    procedure SetPlacement(const Value: TImagePlacement);
    procedure SetHorizontalAlign(const Value: TImageHorizontalAlignment);
    procedure SetVerticalAlign(const Value: TImageVerticalAlignment);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetTargetWidth(const Value: Integer);
    procedure SetTargetHeight(const Value: Integer);
    procedure PictureChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwnerControl: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Picture: TPicture read FPicture write SetPicture;
    property DrawMode: TImageDrawMode read FDrawMode write SetDrawMode default idmProportional;
    property Margins: TANDMR_Margins read FMargins write SetMargins;
    property Position: TImagePosition read FPosition write SetPosition default ipLeft;
    property Placement: TImagePlacement read FPlacement write SetPlacement default iplInsideBounds;
    property HorizontalAlign: TImageHorizontalAlignment read FHorizontalAlign write SetHorizontalAlign default ihaCenter;
    property VerticalAlign: TImageVerticalAlignment read FVerticalAlign write SetVerticalAlign default ivaCenter;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property TargetWidth: Integer read FTargetWidth write SetTargetWidth default 0;
    property TargetHeight: Integer read FTargetHeight write SetTargetHeight default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TBorderSettings }
  TBorderSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FColor: TColor;
    FThickness: Integer;
    FStyle: TPenStyle;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FBackgroundColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetVisible(const Value: Boolean);
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
    property Visible: Boolean read FVisible write SetVisible default True;
    property Color: TColor read FColor write SetColor default clBlack;
    property Thickness: Integer read FThickness write SetThickness default 1;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 0;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctNone;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TFocusSettings }
  TFocusSettings = class(TPersistent)
  private
    FBorderColorVisible: Boolean;
    FBorderColor: TColor;
    FBackgroundColorVisible: Boolean;
    FBackgroundColor: TColor;
    FUnderlineVisible: Boolean;
    FUnderlineColor: TColor;
    FUnderlineThickness: Integer;
    FUnderlineStyle: TPenStyle;
    FOnChange: TNotifyEvent;
    procedure SetBorderColorVisible(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBackgroundColorVisible(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetUnderlineVisible(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TColor);
    procedure SetUnderlineThickness(const Value: Integer);
    procedure SetUnderlineStyle(const Value: TPenStyle);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColorVisible: Boolean read FBorderColorVisible write SetBorderColorVisible default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BackgroundColorVisible: Boolean read FBackgroundColorVisible write SetBackgroundColorVisible default False;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property UnderlineVisible: Boolean read FUnderlineVisible write SetUnderlineVisible default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBlack;
    property UnderlineThickness: Integer read FUnderlineThickness write SetUnderlineThickness default 1;
    property UnderlineStyle: TPenStyle read FUnderlineStyle write SetUnderlineStyle default psSolid;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSeparatorSettings }
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

  { TDropShadowSettings }
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

  { TGradientSettings }
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

  { TProgressSettings }
  TProgressSettings = class(TPersistent)
  private
    FShowProgress: Boolean;
    FProgressColor: TColor;
    FHideCaptionWhileProcessing: Boolean;
    FAnimationTimerInterval: Integer;
    FAnimationProgressStep: Integer;
    FAnimationStyle: TProgressAnimationStyle;
    FProgressText: string;
    FShowProgressText: Boolean;
    FOwnerControl: TWinControl;
    FOnChange: TNotifyEvent;
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

// --- Funções Utilitárias ---
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
procedure DrawEditBox(AGraphics: TGPGraphics; const ADrawArea: TRect; ABackgroundColor: TColor; ABorderColor: TColor; ABorderThickness: Integer; ABorderStyle: TPenStyle; ACornerRadius: Integer; ARoundCornerType: TRoundCornerType; AOpacity: Byte);
procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
procedure DrawComponentCaption(ACanvas: TCanvas; const ARect: TRect; const ACaption: string; AFont: TFont; AFontColor: TColor; AAlignmentHorizontal: TAlignment; AAlignmentVertical: TCaptionVerticalAlignment; AWordWrap: Boolean; AOpacity: Byte);

// REFATORADO: Função unificada para desenhar qualquer TGraphic usando GDI+
procedure DrawGraphicWithGDI(AGraphics: TGPGraphics; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);

function DarkerColor(AColor: TColor; APercent: Byte = 30): TColor;
function LighterColor(AColor: TColor; APercent: Byte = 30): TColor;
function BlendColors(AColor1, AColor2: TColor; AFactor: Single): TColor;
function CalculateProportionalRect(const DestRect: TRect; ImgWidth, ImgHeight: Integer): TRect;
function ResolveStateColor(AIsEnabled: Boolean; AIsHovering: Boolean; AIsFocused: Boolean; ABaseColor, AHoverColor, AFocusColor, ADisabledColor: TColor; AAllowHoverEffect: Boolean = True; AAllowFocusEffect: Boolean = True; AHoverEffectOverridesFocus: Boolean = False; AFallbackToTransparent: Boolean = False): TColor;


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

procedure TClickSettings.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TClickSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TClickSettings.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TClickSettings.SetFontColor(const Value: TColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed;
  end;
end;

procedure TClickSettings.SetDuration(const Value: Integer);
begin
  if FDuration <> Max(0, Value) then
  begin
    FDuration := Max(0, Value);
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
  FObject := nil; // Apenas remove a referência, não libera o objeto
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
  FVerticalAlignment := cvaCenter;
  FColor := clWindowText;
  FDisabledColor := clGrayText;
  FWordWrap := False;
  FOffset := System.Types.Point(0, 0);

  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;

  FMargins := TANDMR_Margins.Create;
  FMargins.OnChange := InternalMarginsChanged;
end;

destructor TCaptionSettings.Destroy;
begin
  FFont.Free;
  FMargins.Free;
  inherited Destroy;
end;

procedure TCaptionSettings.Assign(Source: TPersistent);
var
  LSource: TCaptionSettings;
begin
  if Source is TCaptionSettings then
  begin
    LSource := TCaptionSettings(Source);
    Self.Visible := LSource.Visible;
    Self.Text := LSource.Text;
    Self.Position := LSource.Position;
    Self.Alignment := LSource.Alignment;
    Self.VerticalAlignment := LSource.VerticalAlignment;
    Self.Font.Assign(LSource.Font);
    Self.Color := LSource.Color;
    Self.DisabledColor := LSource.DisabledColor;
    Self.WordWrap := LSource.WordWrap;
    Self.Offset := LSource.Offset;
    Self.Margins.Assign(LSource.Margins);
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

procedure TCaptionSettings.InternalMarginsChanged(Sender: TObject);
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

procedure TCaptionSettings.SetVerticalAlignment(const Value: TCaptionVerticalAlignment);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCaptionSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
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

procedure TCaptionSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetOffset(const Value: TPoint);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TCaptionSettings.SetMargins(const Value: TANDMR_Margins);
begin
  FMargins.Assign(Value);
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
var
  LSource: THoverSettings;
begin
  if Source is THoverSettings then
  begin
    LSource := THoverSettings(Source);
    Self.Enabled := LSource.Enabled;
    Self.BackgroundColor := LSource.BackgroundColor;
    Self.BorderColor := LSource.BorderColor;
    Self.FontColor := LSource.FontColor;
    Self.CaptionFontColor := LSource.CaptionFontColor;
    Self.HoverEffect := LSource.HoverEffect;
    Self.AnimationTimerInterval := LSource.AnimationTimerInterval;
    Self.AnimationStep := LSource.AnimationStep;
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
    FCurrentAnimationValue := Min(TargetValue, FCurrentAnimationValue + FAnimationStep)
  else
    FCurrentAnimationValue := Max(TargetValue, FCurrentAnimationValue - FAnimationStep);

  if FCurrentAnimationValue = TargetValue then
  begin
    FAnimationTimer.Enabled := False;
    FAnimationDirection := 0;
  end;

  if (FCurrentAnimationValue <> OldAnimationValue) or (not FAnimationTimer.Enabled) then
  begin
    if Assigned(FOnAnimationProgress) then
      FOnAnimationProgress(Self);
    if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
      FOwnerControl.Invalidate;
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
    var TargetValue: Integer := IfThen(IsHovering, 255, 0);
    if FCurrentAnimationValue <> TargetValue then
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
    end;
  end
  else
  begin
    if FCurrentAnimationValue > 0 then
    begin
      FAnimationDirection := -1;
      FAnimationTimer.Enabled := True;
    end;
  end;
end;

{ TImageSettings }

constructor TImageSettings.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
  FVisible := True;
  FDrawMode := idmProportional;
  FPosition := ipLeft;
  FPlacement := iplInsideBounds;
  FAutoSize := True;
  FTargetWidth := 0;
  FTargetHeight := 0;
  FHorizontalAlign := ihaCenter;
  FVerticalAlign := ivaCenter;

  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FMargins := TANDMR_Margins.Create;
  FMargins.OnChange := MarginsChanged;
end;

destructor TImageSettings.Destroy;
begin
  FPicture.Free;
  FMargins.Free;
  inherited Destroy;
end;

procedure TImageSettings.Assign(Source: TPersistent);
var
  LSource: TImageSettings;
begin
  if Source is TImageSettings then
  begin
    LSource := TImageSettings(Source);
    FVisible := LSource.FVisible;
    FDrawMode := LSource.FDrawMode;
    FPicture.Assign(LSource.Picture);
    FMargins.Assign(LSource.Margins);
    FPosition := LSource.FPosition;
    FPlacement := LSource.FPlacement;
    FAutoSize := LSource.FAutoSize;
    FTargetWidth := LSource.FTargetWidth;
    FTargetHeight := LSource.FTargetHeight;
    FHorizontalAlign := LSource.FHorizontalAlign;
    FVerticalAlign := LSource.FVerticalAlign;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TImageSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOwnerControl) and (FOwnerControl.HandleAllocated) and not (csDestroying in FOwnerControl.ComponentState) then
    FOwnerControl.Invalidate;
end;

procedure TImageSettings.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageSettings.MarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TImageSettings.SetDrawMode(const Value: TImageDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetMargins(const Value: TANDMR_Margins);
begin
  FMargins.Assign(Value);
end;

procedure TImageSettings.SetPosition(const Value: TImagePosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetPlacement(const Value: TImagePlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetHorizontalAlign(const Value: TImageHorizontalAlignment);
begin
  if FHorizontalAlign <> Value then
  begin
    FHorizontalAlign := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetVerticalAlign(const Value: TImageVerticalAlignment);
begin
  if FVerticalAlign <> Value then
  begin
    FVerticalAlign := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TImageSettings.SetTargetWidth(const Value: Integer);
begin
  if FTargetWidth <> Max(0, Value) then
  begin
    FTargetWidth := Max(0, Value);
    Changed;
  end;
end;

procedure TImageSettings.SetTargetHeight(const Value: Integer);
begin
  if FTargetHeight <> Max(0, Value) then
  begin
    FTargetHeight := Max(0, Value);
    Changed;
  end;
end;

{ TBorderSettings }

constructor TBorderSettings.Create;
begin
  inherited Create;
  FVisible := True;
  FColor := clBlack;
  FThickness := 1;
  FStyle := psSolid;
  FCornerRadius := 0;
  FRoundCornerType := rctNone;
  FBackgroundColor := clNone;
end;

procedure TBorderSettings.Assign(Source: TPersistent);
var
  LSource: TBorderSettings;
begin
  if Source is TBorderSettings then
  begin
    LSource := TBorderSettings(Source);
    FVisible := LSource.FVisible;
    FColor := LSource.FColor;
    FThickness := LSource.FThickness;
    FStyle := LSource.FStyle;
    FCornerRadius := LSource.FCornerRadius;
    FRoundCornerType := LSource.FRoundCornerType;
    FBackgroundColor := LSource.FBackgroundColor;
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

procedure TBorderSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
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
  if FCornerRadius <> Max(0, Value) then
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

{ TFocusSettings }

constructor TFocusSettings.Create;
begin
  inherited Create;
  FBorderColorVisible := False;
  FBorderColor := clBlack;
  FBackgroundColorVisible := False;
  FBackgroundColor := clNone;
  FUnderlineVisible := False;
  FUnderlineColor := clBlack;
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
    FBorderColorVisible := LSource.FBorderColorVisible;
    FBorderColor := LSource.FBorderColor;
    FBackgroundColorVisible := LSource.FBackgroundColorVisible;
    FBackgroundColor := LSource.FBackgroundColor;
    FUnderlineVisible := LSource.FUnderlineVisible;
    FUnderlineColor := LSource.FUnderlineColor;
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

procedure TFocusSettings.SetBorderColorVisible(const Value: Boolean);
begin
  if FBorderColorVisible <> Value then
  begin
    FBorderColorVisible := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
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

procedure TFocusSettings.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
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

procedure TFocusSettings.SetUnderlineColor(const Value: TColor);
begin
  if FUnderlineColor <> Value then
  begin
    FUnderlineColor := Value;
    Changed;
  end;
end;

procedure TFocusSettings.SetUnderlineThickness(const Value: Integer);
begin
  if FUnderlineThickness <> Max(0, Value) then
  begin
    FUnderlineThickness := Max(0, Value);
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
  if FThickness <> Max(0, Value) then
  begin
    FThickness := Max(0, Value);
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
  if FCustomHeight <> Max(0, Value) then
  begin
    FCustomHeight := Max(0, Value);
    Changed;
  end;
end;

{ TDropShadowSettings }

constructor TDropShadowSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FColor := clBlack;
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
  if FOffset <> Value then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TDropShadowSettings.SetBlurRadius(const Value: Integer);
begin
  if FBlurRadius <> Max(0, Value) then
  begin
    FBlurRadius := Max(0, Value);
    Changed;
  end;
end;

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
    Self.ShowProgress := LSource.ShowProgress;
    Self.ProgressColor := LSource.ProgressColor;
    Self.HideCaptionWhileProcessing := LSource.HideCaptionWhileProcessing;
    Self.AnimationTimerInterval := LSource.AnimationTimerInterval;
    Self.AnimationProgressStep := LSource.AnimationProgressStep;
    Self.AnimationStyle := LSource.AnimationStyle;
    Self.ProgressText := LSource.ProgressText;
    Self.ShowProgressText := LSource.ShowProgressText;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TProgressSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOwnerControl) and FOwnerControl.HandleAllocated then
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

{ Funções Utilitárias Globais }

function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then
  begin
    Result := (UInt32(0) shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor);
  Result := (UInt32(Alpha) shl 24) or
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
  if not Assigned(APath) then Exit;
  APath.Reset;
  Rect := ARect;

  if Rect.Width < 0 then Rect.Width := 0;
  if Rect.Height < 0 then Rect.Height := 0;

  if (Rect.Width <= 0) or (Rect.Height <= 0) then
    Exit;

  LRadius := Min(ARadiusValue, Min(Rect.Width / 2.0, Rect.Height / 2.0));
  LRadius := Max(0.0, LRadius);

  if (AType = rctNone) or (LRadius < MIN_RADIUS_FOR_PATH) then
  begin
    APath.AddRectangle(Rect);
    Exit;
  end;

  LDiameter := LRadius * 2.0;

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
  LBorderThicknessValue: Single;
begin
  if (AGraphics = nil) or (ADrawArea.Width <= 0) or (ADrawArea.Height <= 0) then
    Exit;

  LBorderThicknessValue := ABorderThickness;
  LRectF.X      := ADrawArea.Left;
  LRectF.Y      := ADrawArea.Top;
  LRectF.Width  := ADrawArea.Width;
  LRectF.Height := ADrawArea.Height;

  if LBorderThicknessValue > 0 then
  begin
    LRectF.X      := LRectF.X + LBorderThicknessValue / 2.0;
    LRectF.Y      := LRectF.Y + LBorderThicknessValue / 2.0;
    LRectF.Width  := LRectF.Width - LBorderThicknessValue;
    LRectF.Height := LRectF.Height - LBorderThicknessValue;
  end;

  if (LRectF.Width <= 0) or (LRectF.Height <= 0) then
    Exit;

  LRadiusValue := ACornerRadius;

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
            psDash:     LPen.SetDashStyle(DashStyleDash);
            psDot:      LPen.SetDashStyle(DashStyleDot);
            psDashDot:  LPen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: LPen.SetDashStyle(DashStyleDashDotDot);
            else        LPen.SetDashStyle(DashStyleSolid);
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

// *** FUNÇÃO UNIFICADA, CORRIGIDA E OTIMIZADA ***
// Substitui as antigas DrawGraphicWithGDI, DrawPNGImageWithGDI e DrawNonPNGImageWithCanvas
procedure DrawGraphicWithGDI(AGraphics: TGPGraphics; AGraphic: TGraphic; ADestRect: TRect; ADrawMode: TImageDrawMode);
var
  DrawImageRect: TRect;
  SourceBitmap: TGPBitmap;
  PngStream: TMemoryStream;
  Adapter: IStream;
  TempBitmap: Vcl.Graphics.TBitmap;
begin
  if (AGraphics = nil) or (AGraphic = nil) or AGraphic.Empty or (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then
    Exit;

  // 1. Calcula o retângulo de destino
  case ADrawMode of
    idmStretch: DrawImageRect := ADestRect;
    idmProportional: DrawImageRect := CalculateProportionalRect(ADestRect, AGraphic.Width, AGraphic.Height);
    idmNormal:
    begin
      DrawImageRect.Width  := AGraphic.Width;
      DrawImageRect.Height := AGraphic.Height;
      DrawImageRect.Left   := ADestRect.Left + (ADestRect.Width - AGraphic.Width) div 2;
      DrawImageRect.Top    := ADestRect.Top + (ADestRect.Height - AGraphic.Height) div 2;
    end;
  else
    DrawImageRect := ADestRect;
  end;

  if (DrawImageRect.Width <= 0) or (DrawImageRect.Height <= 0) then
    Exit;

  SourceBitmap := nil;
  try
    // 2. Converte o TGraphic para um TGPBitmap
    // Usando 'is' para checagem de tipo, que é mais seguro
    if AGraphic is TPNGImage then
    begin
      PngStream := TMemoryStream.Create;
      try
        // *** CORREÇÃO APLICADA AQUI: Cast na instância AGraphic ***
        TPNGImage(AGraphic).SaveToStream(PngStream);
        PngStream.Position := 0;
        Adapter := TStreamAdapter.Create(PngStream, soReference);
        SourceBitmap := TGPBitmap.Create(Adapter);
      finally
        PngStream.Free;
      end;
    end
    else if AGraphic is TBitmap then
    begin
      SourceBitmap := TGPBitmap.Create(TBitmap(AGraphic).Handle, TBitmap(AGraphic).Palette);
    end
    else // Para outros formatos (JPEG, GIF), converte para um TBitmap primeiro
    begin
      TempBitmap := Vcl.Graphics.TBitmap.Create;
      try
        TempBitmap.Assign(AGraphic);
        SourceBitmap := TGPBitmap.Create(TempBitmap.Handle, TempBitmap.Palette);
      finally
        TempBitmap.Free;
      end;
    end;

    // 3. Desenha o TGPBitmap na tela
    if (SourceBitmap <> nil) and (SourceBitmap.GetLastStatus = Ok) then
    begin
      AGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);
      AGraphics.DrawImage(SourceBitmap, DrawImageRect.Left, DrawImageRect.Top, DrawImageRect.Width, DrawImageRect.Height);
    end;
  finally
    if SourceBitmap <> nil then
      SourceBitmap.Free;
  end;
end;

procedure DrawSeparatorWithCanvas(ACanvas: TCanvas; ASepRect: TRect; AColor: TColor; AThickness: Integer);
var
  OldPen: record Color: TColor; Width: Integer; Style: TPenStyle; end;
  LineX: Integer;
begin
  if (ACanvas = nil) or (AThickness <= 0) or (ASepRect.Width <= 0) or (ASepRect.Height <= 0) then
    Exit;

  OldPen.Color := ACanvas.Pen.Color;
  OldPen.Width := ACanvas.Pen.Width;
  OldPen.Style := ACanvas.Pen.Style;
  try
    ACanvas.Pen.Color := AColor;
    ACanvas.Pen.Width := AThickness;
    ACanvas.Pen.Style := psSolid;

    LineX := ASepRect.Left + (ASepRect.Width div 2);
    ACanvas.MoveTo(LineX, ASepRect.Top);
    ACanvas.LineTo(LineX, ASepRect.Bottom);
  finally
    ACanvas.Pen.Color := OldPen.Color;
    ACanvas.Pen.Width := OldPen.Width;
    ACanvas.Pen.Style := OldPen.Style;
  end;
end;

procedure DrawComponentCaption(ACanvas: TCanvas; const ARect: TRect; const ACaption: string; AFont: TFont; AFontColor: TColor; AAlignmentHorizontal: TAlignment; AAlignmentVertical: TCaptionVerticalAlignment; AWordWrap: Boolean; AOpacity: Byte);
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

  if (DrawTextFlags and DT_WORDBREAK = 0) then
    DrawTextFlags := DrawTextFlags or DT_SINGLELINE;

  TempRect := ARect;
  DrawText(ACanvas.Handle, PChar(ACaption), Length(ACaption), TempRect, DrawTextFlags);
end;

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
begin
  AFactor := Max(0.0, Min(1.0, AFactor));
  if AFactor = 0.0 then Exit(AColor1);
  if AFactor = 1.0 then Exit(AColor2);

  if (AColor1 = clNone) and (AColor2 = clNone) then Exit(clNone);
  if AColor1 = clNone then Exit(AColor2);
  if AColor2 = clNone then Exit(AColor1);

  AColor1 := ColorToRGB(AColor1);
  AColor2 := ColorToRGB(AColor2);
  R1 := GetRValue(AColor1); G1 := GetGValue(AColor1); B1 := GetBValue(AColor1);
  R2 := GetRValue(AColor2); G2 := GetGValue(AColor2); B2 := GetBValue(AColor2);
  R := Round(R1 + (R2 - R1) * AFactor);
  G := Round(G1 + (G2 - G1) * AFactor);
  B := Round(B1 + (B2 - B1) * AFactor);
  Result := RGB(R, G, B);
end;

function CalculateProportionalRect(const DestRect: TRect; ImgWidth, ImgHeight: Integer): TRect;
var
  Ratio, RectRatio: Double;
begin
  if (ImgWidth <= 0) or (ImgHeight <= 0) or (DestRect.Width <= 0) or (DestRect.Height <= 0) then
  begin
    Result := System.Types.Rect(0, 0, 0, 0);
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

function ResolveStateColor(AIsEnabled: Boolean; AIsHovering: Boolean; AIsFocused: Boolean; ABaseColor, AHoverColor, AFocusColor, ADisabledColor: TColor; AAllowHoverEffect: Boolean; AAllowFocusEffect: Boolean; AHoverEffectOverridesFocus: Boolean; AFallbackToTransparent: Boolean): TColor;
begin
  // 1. Desabilitado (maior prioridade)
  if not AIsEnabled then
  begin
    Result := IfThen(ADisabledColor <> clNone, ADisabledColor, ABaseColor);
    Exit;
  end;

  // 2. Determina cores de Foco e Hover ativas
  var IsFocusActive := AIsFocused and AAllowFocusEffect and (AFocusColor <> clNone);
  var IsHoverActive := AIsHovering and AAllowHoverEffect and (AHoverColor <> clNone);

  // 3. Aplica cor com base na prioridade
  if IsHoverActive and (AHoverEffectOverridesFocus or not IsFocusActive) then
  begin
    Result := AHoverColor;
  end
  else if IsFocusActive then
  begin
    Result := AFocusColor;
  end
  else
  begin
    Result := ABaseColor;
  end;

  // 4. Fallback para transparente se a cor resultante for clNone
  if (Result = clNone) and not AFallbackToTransparent then
  begin
    Result := ABaseColor;
  end;
end;

// REMOVIDO: DrawPNGImageWithGDI foi incorporada em DrawGraphicWithGDI.
// REMOVIDO: DrawNonPNGImageWithCanvas foi incorporada em DrawGraphicWithGDI.

end.
