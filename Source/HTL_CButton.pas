unit HTL_CButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.GraphUtil, System.UITypes, HTL_ComponentUtils, // Alterado de ANDMR_ComponentUtils
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  Winapi.ActiveX;

type
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);
  TPresetType = (
    cptNone,
    cptAccept,
    cptDecline,
    cptSave,
    cptEdit,
    cptDelete,
    cptNext,
    cptPrevious,
    cptInfo,
    cptWarning,
    cptHelp
  );

  THTL_CButton = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings;
    FCaptionSettings: TCaptionSettings;
    FImageSettings: TImageSettings;
    FGradientSettings: TGradientSettings;
    FClickSettings: TClickSettings;
    FHoverSettings: THoverSettings;
    FProgressSettings: TProgressSettings;
    FSeparatorSettings: TSeparatorSettings;
    FTags: THTL_MultiTag; // Adicionado para o novo sistema de Tags

    FDisabledCursor: TCursor;
    FTransparent: Boolean;

    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectActive: Boolean;

    FOnClick: TNotifyEvent;
    FStyle: TButtonStyle;
    FPresetType: TPresetType;

    FProcessing: Boolean;
    FProgressTimer: TTimer;
    FProgressStep: Integer;
    FOriginalCaption: string;
    FOriginalEnabledState: Boolean;

    procedure BreakPresetLink;
    procedure SetProgressSettings(const Value: TProgressSettings);
    procedure SetImageSettings(const Value: TImageSettings);
    function GetImage: TPicture;
    procedure SetImage(const Value: TPicture);
    procedure ProgressTimerHandler(Sender: TObject);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);

    // --- M�todos de Configura��es ---
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetClickSettings(const Value: TClickSettings);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetPresetType(const Value: TPresetType);
    procedure SetTags(const Value: THTL_MultiTag); // Adicionado para o novo sistema de Tags

    // --- Handlers de Eventos Internos ---
    procedure HoverSettingsChanged(Sender: TObject);
    procedure BorderSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;

    // --- Getters & Setters de Propriedades Padr�o ---
    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetDisabledCursor(const Value: TCursor);
    procedure SetTransparent(const Value: Boolean);
    function IsEnabledStored: Boolean;
    function GetTag: NativeInt; // Adicionado para o novo sistema de Tags
    procedure SetTag(const Value: NativeInt); // Adicionado para o novo sistema de Tags

    // --- Message Handlers ---
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartProcessing;
    procedure StopProcessing;

    property Image: TPicture read GetImage write SetImage;
  published
    property Align;
    property Enabled read GetEnabled write SetEnabled stored IsEnabledStored;
    property Caption: string read GetCaption write SetCaption;

    // --- Propriedades de Configura��o ---
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property ClickSettings: TClickSettings read FClickSettings write SetClickSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;
    property Tags: THTL_MultiTag read FTags write SetTags; // Adicionado para o novo sistema de Tags

    // --- Propriedades de Estilo e Comportamento ---
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property DisabledCursor: TCursor read FDisabledCursor write SetDisabledCursor default crNo;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;

    // --- Eventos ---
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    // --- Propriedades Padr�o ---
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Tag: NativeInt read GetTag write SetTag default 0; // Propriedade Tag agora usa o novo sistema
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CButton]);
end;

{ THTL_CButton }

constructor THTL_CButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  Width := 100;
  Height := 45;
  TabStop := True;
  FTransparent := False;

  // --- Inicializa��o do novo sistema de Tags ---
  FTags := THTL_MultiTag.Create;
  FTags.OnChange := SettingsChanged;
  // ---------------------------------------------

  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;

  FDisabledCursor := crNo;
  Cursor := crHandPoint;

  DoubleBuffered := True;

  FClickEffectProgress := 0;
  FClickEffectActive := False;
  FClickEffectTimer := TTimer.Create(Self);
  FClickEffectTimer.Enabled := False;
  FClickEffectTimer.OnTimer := ClickEffectTimerHandler;

  FClickSettings := TClickSettings.Create;
  FClickSettings.OnChange := SettingsChanged;
  FClickSettings.Duration := 200;
  UpdateClickEffectTimerInterval;

  FStyle := bsSolid;
  FPresetType := cptNone;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := BorderSettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.BackgroundColor := clBtnFace;
  FBorderSettings.Color := clSilver;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := Self.Name;
  FCaptionSettings.Font.Style := [fsBold];
  FCaptionSettings.Font.Color := clWindowText;
  FCaptionSettings.Font.OnChange := FontChanged;
  FCaptionSettings.Alignment := taCenter;

  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := SettingsChanged;

  FProgressSettings := TProgressSettings.Create(Self);
  FProgressSettings.OnChange := SettingsChanged;

  FProcessing := False;
  FProgressTimer := TTimer.Create(Self);
  FProgressTimer.Enabled := False;
  FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
  FProgressTimer.OnTimer := ProgressTimerHandler;
  FOriginalEnabledState := True;

  FSeparatorSettings := TSeparatorSettings.Create;
  FSeparatorSettings.OnChange := SettingsChanged;
end;

destructor THTL_CButton.Destroy;
begin
  // --- Libera��o do novo sistema de Tags ---
  if Assigned(FTags) then
  begin
    FTags.OnChange := nil;
    FTags.Free;
    FTags := nil;
  end;
  // ---------------------------------------

  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;

  FHoverSettings.OnChange := nil;
  FHoverSettings.Free;

  FCaptionSettings.OnChange := nil;
  FCaptionSettings.Free;

  FImageSettings.OnChange := nil;
  FImageSettings.Free;

  FClickSettings.OnChange := nil;
  FClickSettings.Free;

  FClickEffectTimer.Free;

  if Assigned(FProgressSettings) then
  begin
    FProgressSettings.OnChange := nil;
    FProgressSettings.Free;
    FProgressSettings := nil;
  end;
  FProgressTimer.Free;

  FGradientSettings.OnChange := nil;
  FGradientSettings.Free;

  if Assigned(FSeparatorSettings) then
  begin
    FSeparatorSettings.OnChange := nil;
    FSeparatorSettings.Free;
    FSeparatorSettings := nil;
  end;

  inherited;
end;

// --- Implementa��o dos m�todos para o novo sistema de Tags ---
procedure THTL_CButton.SetTags(const Value: THTL_MultiTag);
begin
  FTags.Assign(Value);
end;

function THTL_CButton.GetTag: NativeInt;
begin
  Result := FTags.AsTag;
end;

procedure THTL_CButton.SetTag(const Value: NativeInt);
begin
  FTags.AsTag := Value;
end;
// -----------------------------------------------------------

procedure THTL_CButton.SetSeparatorSettings(const Value: TSeparatorSettings);
begin
  FSeparatorSettings.Assign(Value);
end;

procedure THTL_CButton.BreakPresetLink;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  if FPresetType <> cptNone then
    FPresetType := cptNone;
end;

procedure THTL_CButton.StartProcessing;
begin
  if FProgressSettings.ShowProgress and not FProcessing then
  begin
    FProcessing := True;
    FOriginalCaption := Self.Caption;
    FOriginalEnabledState := Self.Enabled;

    if FProgressSettings.HideCaptionWhileProcessing then
      Self.Caption := '';

    if Self.Enabled then
      Self.Enabled := False;

    FProgressStep := 0;
    FProgressTimer.Enabled := True;
    Repaint;
  end;
end;

procedure THTL_CButton.StopProcessing;
begin
  if FProcessing then
  begin
    FProcessing := False;
    FProgressTimer.Enabled := False;

    Self.Caption := FOriginalCaption;

    if Self.Enabled <> FOriginalEnabledState then
      Self.Enabled := FOriginalEnabledState;

    Repaint;
  end;
end;

procedure THTL_CButton.SetProgressSettings(const Value: TProgressSettings);
begin
  FProgressSettings.Assign(Value);
end;

procedure THTL_CButton.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
end;

function THTL_CButton.GetImage: TPicture;
begin
  Result := FImageSettings.Picture;
end;

procedure THTL_CButton.SetImage(const Value: TPicture);
begin
  FImageSettings.Picture.Assign(Value);
end;

procedure THTL_CButton.ProgressTimerHandler(Sender: TObject);
begin
  if FProcessing then
  begin
    Inc(FProgressStep);
    Repaint;
  end
  else
  begin
    FProgressTimer.Enabled := False;
  end;
end;

procedure THTL_CButton.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  Repaint;
end;

procedure THTL_CButton.HoverSettingsChanged(Sender: TObject);
begin
  BreakPresetLink;
  Repaint;
end;

procedure THTL_CButton.BorderSettingsChanged(Sender: TObject);
begin
  BreakPresetLink;
  Invalidate;
end;

procedure THTL_CButton.SettingsChanged(Sender: TObject);
begin
  BreakPresetLink;
  Repaint;
  Invalidate;
end;

procedure THTL_CButton.Loaded;
begin
  inherited Loaded;
  if FGradientSettings.StartColor = clNone then
    FGradientSettings.StartColor := FBorderSettings.BackgroundColor;
  if FGradientSettings.EndColor = clNone then
    FGradientSettings.EndColor := DarkerColor(FBorderSettings.BackgroundColor, 30);
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  Repaint;
end;

procedure THTL_CButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (csDesigning in ComponentState) and (AParent <> nil) then
  begin
    if (FCaptionSettings.Text = Name) and (FPresetType = cptNone) then
      FCaptionSettings.Text := '';
  end;
end;

function THTL_CButton.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure THTL_CButton.SetAlign(const Value: TAlign);
begin
  inherited Align := Value;
end;

procedure THTL_CButton.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

procedure THTL_CButton.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure THTL_CButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    StartClickEffect;
  end;
end;

procedure THTL_CButton.StartClickEffect;
begin
  if not Enabled or not FClickSettings.Enabled or (FClickSettings.Duration <= 0) then Exit;

  FClickEffectActive := True;
  FClickEffectProgress := 255;
  UpdateClickEffectTimerInterval;
  FClickEffectTimer.Enabled := True;
  Repaint;
end;

procedure THTL_CButton.UpdateClickEffectTimerInterval;
const
  MIN_INTERVAL = 10;
  FADE_STEP_VALUE = 20;
var
  NumTicks: Single;
  NewInterval: Integer;
begin
  if FClickSettings.Duration <= 0 then
  begin
    FClickEffectTimer.Interval := MIN_INTERVAL;
    Exit;
  end;

  NumTicks := 255 / FADE_STEP_VALUE;
  if NumTicks <= 0 then NumTicks := 1;

  NewInterval := Round(FClickSettings.Duration / NumTicks);
  FClickEffectTimer.Interval := Max(MIN_INTERVAL, NewInterval);
end;

function THTL_CButton.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

procedure THTL_CButton.SetEnabled(Value: Boolean);
begin
  if inherited Enabled <> Value then
  begin
    inherited Enabled := Value;
    if not Value then
    begin
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
      if FHoverSettings.Enabled then
         FHoverSettings.StartAnimation(False);
    end;
    Cursor := IfThen(Value, crHandPoint, FDisabledCursor);
    Repaint;
  end;
end;

function THTL_CButton.IsEnabledStored: Boolean;
begin
  Result := not inherited Enabled;
end;

function THTL_CButton.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure THTL_CButton.SetCaption(const Value: string);
begin
  if FCaptionSettings.Text <> Value then
  begin
    BreakPresetLink;
    FCaptionSettings.Text := Value;
  end;
end;

procedure THTL_CButton.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
end;

procedure THTL_CButton.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
end;

procedure THTL_CButton.SetClickSettings(const Value: TClickSettings);
begin
  FClickSettings.Assign(Value);
end;

procedure THTL_CButton.SetGradientSettings(const Value: TGradientSettings);
begin
  FGradientSettings.Assign(Value);
end;

procedure THTL_CButton.SetPresetType(const Value: TPresetType);
var
  PresetCaption: string;
  BaseColor: TColor;
  NewTitleColor: TColor;
begin
  if FPresetType <> Value then
  begin
    FPresetType := Value;
    PresetCaption := '';
    NewTitleColor := clWhite;

    case FPresetType of
      cptNone: begin Exit; end;
      cptAccept:   begin BaseColor := TColor($0050AF4C); PresetCaption := 'Confirmar'; NewTitleColor := clWhite; end;
      cptDecline:  begin BaseColor := TColor($00757575); PresetCaption := 'Cancelar';  NewTitleColor := clWhite; end;
      cptSave:     begin BaseColor := TColor($00F39621); PresetCaption := 'Salvar';    NewTitleColor := clWhite; end;
      cptEdit:     begin BaseColor := TColor($000098FF); PresetCaption := 'Editar';    NewTitleColor := clBlack; end;
      cptDelete:   begin BaseColor := TColor($003643F4); PresetCaption := 'Excluir';   NewTitleColor := clWhite; end;
      cptNext:     begin BaseColor := TColor($00F4A903); PresetCaption := 'Avan�ar';   NewTitleColor := clWhite; end;
      cptPrevious: begin BaseColor := TColor($009E9E9E); PresetCaption := 'Voltar';    NewTitleColor := clBlack; end;
      cptInfo:     begin BaseColor := TColor($00F7C34F); PresetCaption := 'Informa��o';NewTitleColor := clBlack; end;
      cptWarning:  begin BaseColor := TColor($003BEBFF); PresetCaption := 'Aviso';     NewTitleColor := clBlack; end;
      cptHelp:     begin BaseColor := TColor($008B7D60); PresetCaption := 'Ajuda';     NewTitleColor := clWhite; end;
    else
      BaseColor := FBorderSettings.BackgroundColor;
    end;

    FBorderSettings.BackgroundColor := BaseColor;
    FBorderSettings.Color := DarkerColor(BaseColor, 30);
    FHoverSettings.BackgroundColor := LighterColor(BaseColor, 25);
    FHoverSettings.BorderColor := BaseColor;
    FClickSettings.Color := DarkerColor(BaseColor, 25);
    FClickSettings.BorderColor := DarkerColor(BaseColor, 30);
    FCaptionSettings.Font.Color := NewTitleColor;

    if (GetRValue(FHoverSettings.BackgroundColor) * 0.299 + GetGValue(FHoverSettings.BackgroundColor) * 0.587 + GetBValue(FHoverSettings.BackgroundColor) * 0.114) > 186 then
      FHoverSettings.FontColor := clBlack
    else
      FHoverSettings.FontColor := clWhite;

    if (GetRValue(FClickSettings.Color) * 0.299 + GetGValue(FClickSettings.Color) * 0.587 + GetBValue(FClickSettings.Color) * 0.114) > 186 then
      FClickSettings.FontColor := clBlack
    else
      FClickSettings.FontColor := clWhite;

    if (Trim(Self.CaptionSettings.Text) = '') or (Self.CaptionSettings.Text <> PresetCaption) then
    begin
      Self.CaptionSettings.Text := PresetCaption;
    end
    else if FPresetType <> cptNone then
    begin
      Repaint;
    end;
  end;
end;

procedure THTL_CButton.FontChanged(Sender: TObject);
begin
  BreakPresetLink;
  Repaint;
end;

procedure THTL_CButton.SetDisabledCursor(const Value: TCursor);
begin
  if FDisabledCursor <> Value then
  begin
    FDisabledCursor := Value;
    if not Enabled then
      Cursor := FDisabledCursor;
  end;
end;

procedure THTL_CButton.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
    else
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    Repaint;
  end;
end;

procedure THTL_CButton.ClickEffectTimerHandler(Sender: TObject);
const
  FADE_STEP_VALUE = 20;
begin
  if FClickEffectActive then
  begin
    if FClickEffectProgress > 0 then
    begin
      Dec(FClickEffectProgress, FADE_STEP_VALUE);
      FClickEffectProgress := Max(0, FClickEffectProgress);
    end;

    if FClickEffectProgress <= 0 then
    begin
      FClickEffectProgress := 0;
      FClickEffectActive := False;
      FClickEffectTimer.Enabled := False;
    end;
    Repaint;
  end
  else
  begin
    FClickEffectProgress := 0;
    FClickEffectTimer.Enabled := False;
    Repaint;
  end;
end;

procedure THTL_CButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Cursor := IfThen(Enabled, crHandPoint, FDisabledCursor);
  if not Enabled then
  begin
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
      if FHoverSettings.Enabled then
         FHoverSettings.StartAnimation(False);
  end;
  Repaint;
end;

procedure THTL_CButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and FHoverSettings.Enabled then
  begin
    FHoverSettings.StartAnimation(True);
  end;
end;

procedure THTL_CButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and FHoverSettings.Enabled then
  begin
    FHoverSettings.StartAnimation(False);
  end
  else if FHoverSettings.CurrentAnimationValue > 0 then
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure THTL_CButton.Paint;
var
  LPathRect, LShadowPathDrawRect: TGPRectF;
  LRadiusValue, LPathInset: Single;
  LActualFillColor, LActualBorderColor, LInitialFillColor, LInitialBorderColor, LBaseStyleColor: TColor;
  LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalClickBorderColor: TColor;
  LG: TGPGraphics;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LTextArea: TRect;
  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY, AvailableWidth, AvailableHeight: Integer;
  LScaleFactor: Single;
  LCurrentTitleFont: TFont;
  LDrawFill, LDrawBorder, LCurrentGradientEnabled: Boolean;
  LActualBorderThickness: Integer;
  LHoverProgress: Single;
  LClickProgress: Single;
  LShadowAlphaToUse: Byte;
  LShadowOffsetXToUse, LShadowOffsetYToUse : Single;
  LPathWidth, LPathHeight: Single;
  LPresetDefaultCaption: string;
  LFinalCaptionToDraw: string;
  ButtonRectEffectiveF: TGPRectF;
  LAnimationStyle: TProgressAnimationStyle;
  LProgressText: string;
  LShowProgressText: Boolean;
  DotYOffset: array[0..2] of Integer;
  LImagePlacementRect, LTextPlacementRect: TRect;
  ButtonDrawingRect: TRect;
  ImageDrawingArea: TRect;
  ImageFinalDestRect: TRect;
const
  SHADOW_ALPHA = 50;
  SHADOW_OFFSET_X_CONST = 1;
  SHADOW_OFFSET_Y_CONST = 2;
  GRADIENT_DARK_FACTOR = 25;
  SHADOW_ALPHA_HOVER = 80;
  SHADOW_OFFSET_X_HOVER_FACTOR = 1.5;
  SHADOW_OFFSET_Y_HOVER_FACTOR = 1.5;

begin
  inherited Paint;

  LPresetDefaultCaption := '';
  if FPresetType <> cptNone then
  begin
    case FPresetType of
      cptAccept: LPresetDefaultCaption := 'Confirmar';
      cptDecline: LPresetDefaultCaption := 'Cancelar';
      cptSave: LPresetDefaultCaption := 'Salvar';
      cptEdit: LPresetDefaultCaption := 'Editar';
      cptDelete: LPresetDefaultCaption := 'Excluir';
      cptNext: LPresetDefaultCaption := 'Avan�ar';
      cptPrevious: LPresetDefaultCaption := 'Voltar';
      cptInfo: LPresetDefaultCaption := 'Informa��o';
      cptWarning: LPresetDefaultCaption := 'Aviso';
      cptHelp: LPresetDefaultCaption := 'Ajuda';
    end;
  end;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LAnimationStyle := FProgressSettings.AnimationStyle;
    LProgressText := FProgressSettings.ProgressText;
    LShowProgressText := FProgressSettings.ShowProgressText;

    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    LHoverProgress := 0;
    if Enabled and FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and (FHoverSettings.HoverEffect <> heNone) and not FProcessing then
      LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then
      LClickProgress := FClickEffectProgress / 255.0;

    LInitialFillColor := ResolveStateColor(Enabled, False, False, FBorderSettings.BackgroundColor, clNone, clNone, BlendColors(FBorderSettings.BackgroundColor, clGray, 0.65), False, False);
    LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderSettings.Color, clNone, clNone, BlendColors(FBorderSettings.Color, clGray, 0.7), False, False);
    LActualBorderThickness := FBorderSettings.Thickness;

    if FHoverSettings.BackgroundColor <> clNone then
      LFinalHoverColor := FHoverSettings.BackgroundColor
    else
      LFinalHoverColor := LighterColor(LInitialFillColor, 15);

    if FHoverSettings.BorderColor <> clNone then
      LFinalHoverBorderColor := FHoverSettings.BorderColor
    else
      LFinalHoverBorderColor := LInitialBorderColor;

    LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LInitialFillColor, 15), FClickSettings.Color);
    LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LInitialBorderColor, 15), FClickSettings.BorderColor);

    LActualFillColor := LInitialFillColor;
    LActualBorderColor := LInitialBorderColor;

    if (LHoverProgress > 0) and Enabled and FHoverSettings.Enabled then
    begin
        LActualFillColor := BlendColors(LInitialFillColor, LFinalHoverColor, LHoverProgress);
        LActualBorderColor := BlendColors(LInitialBorderColor, LFinalHoverBorderColor, LHoverProgress);
    end;

    if (LClickProgress > 0) and Enabled and FClickSettings.Enabled and (FClickSettings.Duration > 0) then
    begin
      LActualFillColor := BlendColors(LActualFillColor, LFinalClickColor, LClickProgress);
      LActualBorderColor := BlendColors(LActualBorderColor, LFinalClickBorderColor, LClickProgress);
    end;

    LCurrentGradientEnabled := FGradientSettings.Enabled;
    LDrawFill := True;
    LDrawBorder := LActualBorderThickness > 0;

    case FStyle of
      bsSolid: LCurrentGradientEnabled := False;
      bsFaded:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.8);
        LActualFillColor := LBaseStyleColor;
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 10), 0.7);
        LCurrentGradientEnabled := False;
        LActualBorderThickness := 0;
        LDrawBorder := False;
      end;
      bsBordered:
      begin
        LDrawFill := False;
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(FHoverSettings.BackgroundColor=clNone, LInitialFillColor, FHoverSettings.BackgroundColor), 70);
      end;
      bsLight:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
        LActualFillColor := LBaseStyleColor;
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LDrawBorder := LActualBorderThickness > 0;
      end;
      bsFlat:
      begin
        LCurrentGradientEnabled := False;
        LActualBorderThickness := 0;
        LDrawBorder := False;
        LFinalHoverBorderColor := LInitialFillColor;
      end;
      bsGhost:
      begin
        LDrawFill := False;
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LActualBorderColor := LInitialFillColor;
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(FHoverSettings.BackgroundColor=clNone, LInitialFillColor, FHoverSettings.BackgroundColor), 100);
        LFinalHoverBorderColor := LInitialFillColor;
      end;
      bsShadow:
      begin
        if FTransparent then
        begin
          LDrawFill := False;
          LCurrentGradientEnabled := False;
        end;
      end;
      bsGradient:
      begin
        LCurrentGradientEnabled := True;
        LDrawFill := True;
        LDrawBorder := LActualBorderThickness > 0;
      end;
      bsDark:
      begin
        LBaseStyleColor := DarkerColor(FBorderSettings.BackgroundColor, 60);
        if (GetRValue(LBaseStyleColor) < 30) and (GetGValue(LBaseStyleColor) < 30) and (GetBValue(LBaseStyleColor) < 30) then
            LBaseStyleColor := TColor($FF383838);
        LActualFillColor := LBaseStyleColor;
        LActualBorderColor := LighterColor(LBaseStyleColor, 20);
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        LDrawBorder := True;
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        if FHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FHoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LBaseStyleColor, 15);
        if FHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := LighterColor(LActualBorderColor, 15);
        LFinalClickColor := IfThen(FClickSettings.Color = clNone, LighterColor(LBaseStyleColor, 10), FClickSettings.Color);
        LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, LighterColor(LActualBorderColor, 10), FClickSettings.BorderColor);
      end;
      bsMaterial:
      begin
        LActualFillColor := FBorderSettings.BackgroundColor;
        LActualBorderColor := clNone;
        LActualBorderThickness := 0;
        LDrawBorder := False;
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        if FHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FHoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 10);
        LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LActualFillColor, 10), FClickSettings.Color);
      end;
      bsModern:
      begin
        LActualFillColor := FBorderSettings.BackgroundColor;
        LActualBorderColor := DarkerColor(LActualFillColor, 15);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        if FHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FHoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 8);
        if FHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := LActualFillColor;
        LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LActualFillColor, 8), FClickSettings.Color);
        LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LActualFillColor, 20), FClickSettings.BorderColor);
      end;
      bsWindows:
      begin
        LActualFillColor := TColor($FFEFEFEF);
        LActualBorderColor := TColor($FFDCDCDC);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        if FHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FHoverSettings.BackgroundColor
        else
          LFinalHoverColor := TColor($FFF5F5F5);
        if FHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := FBorderSettings.BackgroundColor;
        LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LActualFillColor, 10), FClickSettings.Color);
        LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LActualBorderColor, 10), FClickSettings.BorderColor);
      end;
      bsMacOS:
      begin
        LActualFillColor := TColor($FFF2F2F7);
        LActualBorderColor := TColor($FFD1D1D6);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        if FHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FHoverSettings.BackgroundColor
        else
          LFinalHoverColor := DarkerColor(LActualFillColor, 5);
        if FHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := DarkerColor(LActualBorderColor, 5);
        LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LActualFillColor, 12), FClickSettings.Color);
        LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LActualBorderColor, 12), FClickSettings.BorderColor);
      end;
    end;

    if FTransparent then
    begin
      LDrawFill := False;
      LCurrentGradientEnabled := False;
    end;
    LDrawBorder := LActualBorderThickness > 0;

    ButtonDrawingRect := ClientRect;
    ImageDrawingArea := System.Types.Rect(0,0,0,0);

    // --- Altera��o 1: C�lculo do espa�o do separador para iplOutsideBounds ---
    if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and (FImageSettings.Placement = iplOutsideBounds) then
    begin
      var ImageSlotW, ImageSlotH: Integer;
      var SeparatorSpace: Integer;

      if not FImageSettings.AutoSize and (FImageSettings.TargetWidth > 0) then
        ImageSlotW := FImageSettings.TargetWidth
      else
        ImageSlotW := FImageSettings.Picture.Width;

      if not FImageSettings.AutoSize and (FImageSettings.TargetHeight > 0) then
        ImageSlotH := FImageSettings.TargetHeight
      else
        ImageSlotH := FImageSettings.Picture.Height;

      ImageSlotW := ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
      ImageSlotH := ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;

      // Calcula o espa�o total para o separador (Padding + Linha + Padding)
      SeparatorSpace := 0;
      if FSeparatorSettings.Visible then
        SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness;

      case FImageSettings.ImagePosition of
        ipLeft:
        begin
          ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Top, ClientRect.Left + ImageSlotW, ClientRect.Bottom);
          ButtonDrawingRect.Left := ImageDrawingArea.Right + SeparatorSpace;
        end;
        ipRight:
        begin
          ImageDrawingArea := System.Types.Rect(ClientRect.Right - ImageSlotW, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
          ButtonDrawingRect.Right := ImageDrawingArea.Left - SeparatorSpace;
        end;
        ipTop:
        begin
          ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + ImageSlotH);
          ButtonDrawingRect.Top := ImageDrawingArea.Bottom + SeparatorSpace;
        end;
        ipBottom:
        begin
          ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Bottom - ImageSlotH, ClientRect.Right, ClientRect.Bottom);
          ButtonDrawingRect.Bottom := ImageDrawingArea.Top - SeparatorSpace;
        end;
      end;
      if ButtonDrawingRect.Right < ButtonDrawingRect.Left then ButtonDrawingRect.Right := ButtonDrawingRect.Left;
      if ButtonDrawingRect.Bottom < ButtonDrawingRect.Top then ButtonDrawingRect.Bottom := ButtonDrawingRect.Top;
    end;

    ButtonRectEffectiveF.Y := ButtonDrawingRect.Top;
    ButtonRectEffectiveF.X := ButtonDrawingRect.Left;
    ButtonRectEffectiveF.Width := ButtonDrawingRect.Width;
    ButtonRectEffectiveF.Height := ButtonDrawingRect.Height;

    if ((FStyle = bsShadow) or (FStyle = bsMaterial)) and (not FTransparent) then
    begin
      if FStyle = bsShadow then
      begin
        LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST;
        LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST;
        LShadowAlphaToUse := SHADOW_ALPHA;
        if (LHoverProgress > 0) and Enabled and FHoverSettings.Enabled then
        begin
          LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST + ((SHADOW_OFFSET_X_CONST * SHADOW_OFFSET_X_HOVER_FACTOR) - SHADOW_OFFSET_X_CONST) * LHoverProgress;
          LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST + ((SHADOW_OFFSET_Y_CONST * SHADOW_OFFSET_Y_HOVER_FACTOR) - SHADOW_OFFSET_Y_CONST) * LHoverProgress;
          LShadowAlphaToUse := Round(SHADOW_ALPHA + (SHADOW_ALPHA_HOVER - SHADOW_ALPHA) * LHoverProgress);
        end;
      end else
      begin
        LShadowOffsetXToUse := 1;
        LShadowOffsetYToUse := 2;
        LShadowAlphaToUse := 60;
        const  MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR = 1.8;
        const  MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR = 1.8;
        const  MATERIAL_SHADOW_ALPHA_HOVER = 90;
        if (LHoverProgress > 0) and Enabled and FHoverSettings.Enabled then
        begin
          var TempShadowOffsetXConst_Material: Single; var TempShadowOffsetYConst_Material: Single;
          TempShadowOffsetXConst_Material := 1; TempShadowOffsetYConst_Material := 2;
          LShadowOffsetXToUse := TempShadowOffsetXConst_Material + ((TempShadowOffsetXConst_Material * MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR) - TempShadowOffsetXConst_Material) * LHoverProgress;
          LShadowOffsetYToUse := TempShadowOffsetYConst_Material + ((TempShadowOffsetYConst_Material * MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR) - TempShadowOffsetYConst_Material) * LHoverProgress;
          LShadowAlphaToUse := Round(LShadowAlphaToUse + (MATERIAL_SHADOW_ALPHA_HOVER - LShadowAlphaToUse) * LHoverProgress);
        end;
      end;

      var TempX, TempY, TempW, TempH: Single;
      TempX := IfThen(LShadowOffsetXToUse < 0, Abs(LShadowOffsetXToUse), 0.0) + ButtonRectEffectiveF.X;
      TempY := IfThen(LShadowOffsetYToUse < 0, Abs(LShadowOffsetYToUse), 0.0) + ButtonRectEffectiveF.Y;
      TempW := ButtonRectEffectiveF.Width - Abs(LShadowOffsetXToUse);
      TempH := ButtonRectEffectiveF.Height - Abs(LShadowOffsetYToUse);

      ButtonRectEffectiveF := MakeRect(TempX, TempY, Max(0, TempW), Max(0, TempH));

      if LActualBorderThickness > 0 then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
      LPathWidth := ButtonRectEffectiveF.Width - 2 * LPathInset;
      LPathHeight := ButtonRectEffectiveF.Height - 2 * LPathInset;
      LPathWidth := Max(0, LPathWidth);
      LPathHeight := Max(0, LPathHeight);

      LShadowPathDrawRect := MakeRect(ButtonRectEffectiveF.X + LPathInset + LShadowOffsetXToUse,
                                      ButtonRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse,
                                      LPathWidth, LPathHeight);
      LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0);
      LRadiusValue := Max(0, LRadiusValue);

      LGPPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, LRadiusValue, FBorderSettings.RoundCornerType);
        if LGPPath.GetPointCount > 0 then
        begin
          LGPBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, LShadowAlphaToUse));
          try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end;
        end;
      finally LGPPath.Free; end;
    end
    else if LDrawBorder and (LActualBorderThickness > 0) then
    begin
      if ButtonRectEffectiveF.Width > 0 then ButtonRectEffectiveF.Width  := Max(0, ButtonRectEffectiveF.Width - 0.5);
      if ButtonRectEffectiveF.Height > 0 then ButtonRectEffectiveF.Height := Max(0, ButtonRectEffectiveF.Height - 0.5);
    end;

    if LDrawBorder and (LActualBorderThickness > 0) then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
    LPathRect := MakeRect(ButtonRectEffectiveF.X + LPathInset, ButtonRectEffectiveF.Y + LPathInset, ButtonRectEffectiveF.Width - 2 * LPathInset, ButtonRectEffectiveF.Height - 2 * LPathInset);
    LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LPathRect.Width, LPathRect.Height) / 2.0);
    LRadiusValue := Max(0, LRadiusValue);

    LGPPath := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(LGPPath, LPathRect, LRadiusValue, FBorderSettings.RoundCornerType);
      if LGPPath.GetPointCount > 0 then
      begin
        if LDrawFill and not FTransparent then
        begin
          if LCurrentGradientEnabled and (FGradientSettings.StartColor <> clNone) and (FGradientSettings.EndColor <> clNone) then
          begin
            var  gradientRect : TGPRectF; var  pathGradBrush: TGPPathGradientBrush; var  surroundColor: TGPColor; var  surroundCount: Integer;
            LGPPath.GetBounds(gradientRect, nil, nil);
            case FGradientSettings.GradientType of
              gtLinearHorizontal: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeHorizontal);
              gtDiagonalDown: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeForwardDiagonal);
              gtDiagonalUp: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeBackwardDiagonal);
              gtRadial, gtCenterBurst:
              begin
                pathGradBrush := TGPPathGradientBrush.Create(LGPPath);
                surroundColor := ColorToARGB(FGradientSettings.EndColor, 255);
                pathGradBrush.SetCenterColor(ColorToARGB(FGradientSettings.StartColor, 255));
                surroundCount := 1;
                pathGradBrush.SetSurroundColors(@surroundColor, surroundCount);
                if FGradientSettings.GradientType = gtCenterBurst then pathGradBrush.SetFocusScales(0.0, 0.0);
                LGPBrush := pathGradBrush;
              end;
            else LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeVertical);
            end;
          end else begin LGPBrush := TGPSolidBrush.Create(ColorToARGB(LActualFillColor, 255)); end;
          try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end;
        end;
        if LDrawBorder and (FBorderSettings.Style <> psClear) then
        begin
          LGPPen := TGPPen.Create(ColorToARGB(LActualBorderColor, 255), LActualBorderThickness);
          try
            case FBorderSettings.Style of
              psDash: LGPPen.SetDashStyle(DashStyleDash); psDot: LGPPen.SetDashStyle(DashStyleDot);
              psDashDot: LGPPen.SetDashStyle(DashStyleDashDot); psDashDotDot: LGPPen.SetDashStyle(DashStyleDashDotDot);
              else LGPPen.SetDashStyle(DashStyleSolid);
            end;
            LG.DrawPath(LGPPen, LGPPath);
          finally LGPPen.Free; end;
        end;
      end;
    finally LGPPath.Free; end;

    LTextPlacementRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y), Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width), Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));
    if LDrawBorder and (LActualBorderThickness > 0) then
      InflateRect(LTextPlacementRect, -Round(LActualBorderThickness), -Round(LActualBorderThickness));

    if FProcessing and FProgressSettings.ShowProgress then
    begin
        // *** IN�CIO DA CORRE��O DO PROGRESSO ***
        var LProgressRect: TRect;
        var LArcThickness: Integer;
        var LStartAngle, LSweepAngle: Single;
        var LProgressBarPen: TGPPen;
        var LProgressPath_Progress: TGPGraphicsPath;
        var ArcRectF: TGPRectF;
        var OriginalProgressRect: TRect;

        LProgressRect := ButtonDrawingRect; // <-- USA A �REA DE DESENHO DO BOT�O
        if FBorderSettings.Thickness > 0 then
          InflateRect(LProgressRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

        OriginalProgressRect := LProgressRect;
        // *** FIM DA CORRE��O DO PROGRESSO ***

        if LShowProgressText and (LProgressText <> '') then
        begin
          if LProgressRect.Width > 100 then
          begin
            OriginalProgressRect.Right := LProgressRect.Left + Round(LProgressRect.Width * 0.4);
            LProgressRect := OriginalProgressRect;
          end
          else
          begin
             InflateRect(LProgressRect, -Round(LProgressRect.Width * 0.1), -Round(LProgressRect.Height * 0.1));
          end;
        end;

        if LAnimationStyle in [pasRotatingSemiCircle, pasFullCircularSpinner, pasBouncingDots] then
        begin
          if LProgressRect.Width > LProgressRect.Height then
          begin
              LProgressRect.Left := LProgressRect.Left + (LProgressRect.Width - LProgressRect.Height) div 2;
              LProgressRect.Width := LProgressRect.Height;
          end
          else
          begin
              LProgressRect.Top := LProgressRect.Top + (LProgressRect.Height - LProgressRect.Width) div 2;
              LProgressRect.Height := LProgressRect.Width;
          end;
          InflateRect(LProgressRect, -Max(2, Round(Min(LProgressRect.Width, LProgressRect.Height) * 0.1)), -Max(2, Round(Min(LProgressRect.Width, LProgressRect.Height) * 0.1)));
        end;

        case LAnimationStyle of
          pasRotatingSemiCircle:
          begin
            if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
            begin
              LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
              LStartAngle := (FProgressStep * 10) mod 360;
              LSweepAngle := 270;
              LProgressPath_Progress := TGPGraphicsPath.Create;
              try
                ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
                if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
                begin
                  LProgressPath_Progress.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                  LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                  LProgressBarPen.SetStartCap(LineCapRound);
                  LProgressBarPen.SetEndCap(LineCapRound);
                  try
                    LG.DrawPath(LProgressBarPen, LProgressPath_Progress);
                  finally
                    LProgressBarPen.Free;
                  end;
                end;
              finally
                LProgressPath_Progress.Free;
              end;
            end;
          end;
          pasFullCircularSpinner:
          begin
            if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
            begin
              LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
              LStartAngle := (FProgressStep * 12) mod 360;
              LSweepAngle := 90;
              LProgressPath_Progress := TGPGraphicsPath.Create;
              try
                ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
                if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
                begin
                  LProgressPath_Progress.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                  LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                  LProgressBarPen.SetStartCap(LineCapRound);
                  LProgressBarPen.SetEndCap(LineCapRound);
                  try
                    LG.DrawPath(LProgressBarPen, LProgressPath_Progress);
                  finally
                    LProgressBarPen.Free;
                  end;
                end;
              finally
                LProgressPath_Progress.Free;
              end;
            end;
          end;
          pasHorizontalBar:
          begin
            var BarRect: TRect;
            var InnerBarWidth, InnerBarX: Integer;
            BarRect := OriginalProgressRect;
            if LShowProgressText and (LProgressText <> '') and (OriginalProgressRect.Width > 100) then
            begin
               BarRect := LProgressRect;
            end;

            // Ajuste para posicionar a barra verticalmente e definir sua espessura
            InflateRect(BarRect, 0, -BarRect.Height div 3); // Mant�m para centralizar verticalmente
            if BarRect.Height < 4 then
              BarRect.Height := Max(2, Min(LProgressRect.Height, 4));

            if BarRect.Width > 10 then
            begin
              // Desenha o fundo da �rea de progresso (opcional, mas mant�m a apar�ncia)
              LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 100));
              try
                LG.FillRectangle(LGPBrush, BarRect.Left, BarRect.Top, BarRect.Width, BarRect.Height);
              finally
                LGPBrush.Free;
              end;

              InnerBarWidth := BarRect.Width div 3;
              // <-- IN�CIO DA ALTERA��O: L�gica da anima��o
              if (BarRect.Width + InnerBarWidth) > 0 then
                  InnerBarX := (FProgressStep * 5) mod (BarRect.Width + InnerBarWidth)
              else
                  InnerBarX := 0;

              LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
              try
                // Desenha o indicador de progresso come�ando de fora do bot�o
                LG.FillRectangle(LGPBrush, BarRect.Left + InnerBarX - InnerBarWidth, BarRect.Top, InnerBarWidth, BarRect.Height);
              finally
                LGPBrush.Free;
              end;
              // <-- FIM DA ALTERA��O
            end;
          end;
          pasBouncingDots:
          begin
            const DotCount = 3;
            var DotSize, DotSpacing, TotalDotWidth, StartX, BaseY: Integer;
            var i: Integer;

            if (LProgressRect.Width > 0) and (LProgressRect.Height > 0) then
            begin
              DotSize := Max(4, Min(LProgressRect.Width div Max(1, (DotCount * 2)), LProgressRect.Height div 2));
              DotSpacing := DotSize div 2;
              TotalDotWidth := (DotCount * DotSize) + ((DotCount - 1) * DotSpacing);

              if TotalDotWidth > LProgressRect.Width then
              begin
                  DotSize := Max(2, LProgressRect.Width div (DotCount * 2));
                  DotSpacing := DotSize div 3;
                  TotalDotWidth := (DotCount * DotSize) + ((DotCount - 1) * DotSpacing);
              end;

              StartX := LProgressRect.Left + (LProgressRect.Width - TotalDotWidth) div 2;
              BaseY := LProgressRect.Top + (LProgressRect.Height - DotSize) div 2;

              for i := 0 to DotCount - 1 do
              begin
                DotYOffset[i] := Round( (DotSize / 2) * Sin( (FProgressStep * 0.2 + i * (PI/DotCount))) );
              end;

              LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
              try
                for i := 0 to DotCount - 1 do
                begin
                  LG.FillEllipse(LGPBrush, StartX + i * (DotSize + DotSpacing),
                                           BaseY + DotYOffset[i],
                                           DotSize, DotSize);
                end;
              finally
                LGPBrush.Free;
              end;
            end;
          end;
        end;

        if LShowProgressText and (LProgressText <> '') then
        begin
          var TextRect: TRect;
          var ProgressCaptionFont: TFont;
          var AnimationAreaRightBound: Integer;

          if (OriginalProgressRect.Width > 100) and (LAnimationStyle <> pasHorizontalBar) then
          begin
              AnimationAreaRightBound := LProgressRect.Left + LProgressRect.Width;
              TextRect.Left := AnimationAreaRightBound + Self.FCaptionSettings.Margins.Left;
              TextRect.Top  := OriginalProgressRect.Top;
              TextRect.Right := OriginalProgressRect.Left + OriginalProgressRect.Width - Self.FCaptionSettings.Margins.Right;
              TextRect.Bottom := OriginalProgressRect.Bottom;
          end
          else if (LAnimationStyle = pasHorizontalBar) and (OriginalProgressRect.Width > 100) then
          begin
              TextRect.Left := OriginalProgressRect.Left + Self.FCaptionSettings.Margins.Left;
              TextRect.Top := LProgressRect.Bottom + Self.FCaptionSettings.Margins.Top;
              TextRect.Right := OriginalProgressRect.Right - Self.FCaptionSettings.Margins.Right;
              TextRect.Bottom := OriginalProgressRect.Bottom;
          end
          else
          begin
              TextRect.Left := ClientRect.Left + Self.FCaptionSettings.Margins.Left;
              TextRect.Top := LProgressRect.Bottom + Self.FCaptionSettings.Margins.Top;
              TextRect.Right := ClientRect.Right - Self.FCaptionSettings.Margins.Right;
              TextRect.Bottom := ClientRect.Bottom - Self.FCaptionSettings.Margins.Bottom;
          end;

          if (TextRect.Width > 0) and (TextRect.Height > 0) then
          begin
            ProgressCaptionFont := TFont.Create;
            try
              ProgressCaptionFont.Assign(Self.FCaptionSettings.Font);
              DrawComponentCaption(Self.Canvas, TextRect, LProgressText, ProgressCaptionFont, ProgressCaptionFont.Color, taCenter, cvaCenter, False, 255);
            finally
              ProgressCaptionFont.Free;
            end;
          end;
        end;
    end
    else
    begin
      // --- L�GICA DE LAYOUT CORRIGIDA ---
      var imageSlotRect, textSlotRect: TRect;
      var imageAndMarginsW, imageAndMarginsH: Integer;

      // Inicializa os ret�ngulos de slot com a �rea de conte�do dispon�vel
      if FImageSettings.Placement = iplInsideBounds then
      begin
        textSlotRect := LTextPlacementRect;
        imageSlotRect := LTextPlacementRect;
      end
      else
      begin
        textSlotRect := LTextPlacementRect;
        imageSlotRect := ImageDrawingArea;
      end;

      LImgW := 0; LImgH := 0; LDrawW := 0; LDrawH := 0;

      // Calcula o tamanho que a imagem ocupar� (LDrawW, LDrawH)
      if (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and FImageSettings.Visible then
      begin
          AvailableWidth  := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
          AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);
          LImgW := FImageSettings.Picture.Width;
          LImgH := FImageSettings.Picture.Height;

          if FImageSettings.AutoSize then
          begin
             case FImageSettings.DrawMode of
              idmStretch: begin LDrawW := AvailableWidth; LDrawH := AvailableHeight; end;
              idmProportional:
              begin
                if (LImgW > 0) and (LImgH > 0) and (AvailableWidth > 0) and (AvailableHeight > 0) then
                begin
                  var imgAspectRatio := LImgW / LImgH; var availAspectRatio := AvailableWidth / AvailableHeight;
                  if availAspectRatio > imgAspectRatio then begin LDrawH := AvailableHeight; LDrawW := Round(LDrawH * imgAspectRatio); end
                  else begin LDrawW := AvailableWidth; LDrawH := Round(LDrawW / imgAspectRatio); end;
                end else begin LDrawW := 0; LDrawH := 0; end;
              end;
              idmNormal: begin LDrawW := LImgW; LDrawH := LImgH; end;
              else LDrawW := LImgW; LDrawH := LImgH;
            end;
          end
          else
          begin
             var targetW, targetH: Integer; targetW := FImageSettings.TargetWidth; targetH := FImageSettings.TargetHeight;
             case FImageSettings.DrawMode of
              idmStretch: begin LDrawW := targetW; LDrawH := targetH; end;
              idmProportional:
              begin
                if (LImgW > 0) and (LImgH > 0) and (targetW > 0) and (targetH > 0) then
                begin
                  var imgAspectRatio := LImgW / LImgH; var targetAspectRatio := targetW / targetH;
                  if targetAspectRatio > imgAspectRatio then begin LDrawH := targetH; LDrawW := Round(LDrawH * imgAspectRatio); end
                  else begin LDrawW := targetW; LDrawH := Round(LDrawW / imgAspectRatio); end;
                end else begin LDrawW := 0; LDrawH := 0; end;
              end;
              idmNormal: begin LDrawW := LImgW; LDrawH := LImgH; end;
              else begin LDrawW := LImgW; LDrawH := LImgH; end;
            end;
          end;
          LDrawW := Max(0, LDrawW); LDrawH := Max(0, LDrawH);
          if LDrawW > AvailableWidth then LDrawW := AvailableWidth;
          if LDrawH > AvailableHeight then LDrawH := AvailableHeight;
      end;

      // --- Altera��o 2: C�lculo do espa�o do separador para iplInsideBounds ---
      // Particiona o espa�o entre o slot da imagem e o slot do texto
      if (LDrawW > 0) and (FImageSettings.Placement = iplInsideBounds) then
      begin
          imageAndMarginsW := LDrawW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
          imageAndMarginsH := LDrawH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;

          // Calcula o espa�o total para o separador (Padding + Linha + Padding)
          var SeparatorSpace: Integer := 0;
          if FSeparatorSettings.Visible then
            SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness;

          case FImageSettings.ImagePosition of
            ipLeft:
            begin
              imageSlotRect.Width := imageAndMarginsW;
              textSlotRect.Left := imageSlotRect.Right + SeparatorSpace;
            end;
            ipRight:
            begin
              imageSlotRect.Left := imageSlotRect.Right - imageAndMarginsW;
              textSlotRect.Right := imageSlotRect.Left - SeparatorSpace;
            end;
            ipTop:
            begin
              imageSlotRect.Height := imageAndMarginsH;
              textSlotRect.Top := imageSlotRect.Bottom + SeparatorSpace;
            end;
            ipBottom:
            begin
              imageSlotRect.Top := imageSlotRect.Bottom - imageAndMarginsH;
              textSlotRect.Bottom := imageSlotRect.Top - SeparatorSpace;
            end;
            ipCenter, ipFill, ipBehind: ; // Nestes casos, os slots se sobrep�em
          end;
      end;

      // --- Desenho do Separador (l�gica unificada) ---
      if FSeparatorSettings.Visible and FImageSettings.Visible and
         (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and
         (FImageSettings.Placement in [iplInsideBounds, iplOutsideBounds]) and
         (FImageSettings.ImagePosition in [ipLeft, ipRight, ipTop, ipBottom]) then
      begin
        var ASepRect: TRect;
        var isVertical: Boolean;

        isVertical := FImageSettings.ImagePosition in [ipLeft, ipRight];

        // Usa 'imageDrawingArea' para iplOutsideBounds e 'imageSlotRect' para iplInsideBounds
        var refRect: TRect;
        if FImageSettings.Placement = iplOutsideBounds then
          refRect := ImageDrawingArea
        else
          refRect := imageSlotRect;

        // Calcula o ret�ngulo base para o separador, posicionado a `Padding` do elemento de refer�ncia
        case FImageSettings.ImagePosition of
          ipLeft:
            ASepRect := System.Types.Rect(refRect.Right + FSeparatorSettings.Padding, refRect.Top,
                                           refRect.Right + FSeparatorSettings.Padding + FSeparatorSettings.Thickness, refRect.Bottom);
          ipRight:
            ASepRect := System.Types.Rect(refRect.Left - FSeparatorSettings.Padding - FSeparatorSettings.Thickness, refRect.Top,
                                           refRect.Left - FSeparatorSettings.Padding, refRect.Bottom);
          ipTop:
            ASepRect := System.Types.Rect(refRect.Left, refRect.Bottom + FSeparatorSettings.Padding,
                                           refRect.Right, refRect.Bottom + FSeparatorSettings.Padding + FSeparatorSettings.Thickness);
          ipBottom:
             ASepRect := System.Types.Rect(refRect.Left, refRect.Top - FSeparatorSettings.Padding - FSeparatorSettings.Thickness,
                                            refRect.Right, refRect.Top - FSeparatorSettings.Padding);
        end;

        // Ajusta altura/largura baseado no HeightMode
        case FSeparatorSettings.HeightMode of
           shmFull: { O ret�ngulo j� est� com a dimens�o total };
           shmCustom:
             if isVertical then // Ajusta altura
             begin
               var currentHeight := ASepRect.Height;
               var customH := Min(currentHeight, FSeparatorSettings.CustomHeight);
               ASepRect.Top := ASepRect.Top + (currentHeight - customH) div 2;
               ASepRect.Bottom := ASepRect.Top + customH;
             end
             else // Ajusta largura
             begin
               var currentWidth := ASepRect.Width;
               var customW := Min(currentWidth, FSeparatorSettings.CustomHeight);
               ASepRect.Left := ASepRect.Left + (currentWidth - customW) div 2;
               ASepRect.Right := ASepRect.Left + customW;
             end;
        end;

        // Desenha o separador usando um preenchimento simples
        if (ASepRect.Width > 0) and (ASepRect.Height > 0) then
        begin
          Self.Canvas.Brush.Color := FSeparatorSettings.Color;
          Self.Canvas.Brush.Style := TBrushStyle.bsSolid;
          Self.Canvas.FillRect(ASepRect);
        end;
      end;

      // Desenha a imagem dentro do seu slot
      if (LDrawW > 0) then
      begin
          // Calcula a posi��o final da imagem (LImgX, LImgY) DENTRO DO SEU SLOT
          AvailableWidth  := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
          AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);

          case FImageSettings.HorizontalAlign of
            ihaLeft:   LImgX := imageSlotRect.Left + FImageSettings.Margins.Left;
            ihaCenter: LImgX := imageSlotRect.Left + FImageSettings.Margins.Left + (AvailableWidth - LDrawW) div 2;
            ihaRight:  LImgX := imageSlotRect.Right - FImageSettings.Margins.Right - LDrawW;
          else LImgX := imageSlotRect.Left + FImageSettings.Margins.Left;
          end;

          case FImageSettings.VerticalAlign of
            ivaTop:    LImgY := imageSlotRect.Top + FImageSettings.Margins.Top;
            ivaCenter: LImgY := imageSlotRect.Top + FImageSettings.Margins.Top + (AvailableHeight - LDrawH) div 2;
            ivaBottom: LImgY := imageSlotRect.Bottom - FImageSettings.Margins.Bottom - LDrawH;
          else LImgY := imageSlotRect.Top + FImageSettings.Margins.Top;
          end;

          ImageFinalDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

          // Efeito de escala no hover
          if Enabled and FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heScale) and (LHoverProgress > 0) then
          begin
            LScaleFactor := 1 + (LHoverProgress * (1.05 - 1));
            var ScaledW := Round(LDrawW * LScaleFactor);
            var ScaledH := Round(LDrawH * LScaleFactor);
            ImageFinalDestRect.Left := LImgX + (LDrawW - ScaledW) div 2;
            ImageFinalDestRect.Top := LImgY + (LDrawH - ScaledH) div 2;
            ImageFinalDestRect.Right := ImageFinalDestRect.Left + ScaledW;
            ImageFinalDestRect.Bottom := ImageFinalDestRect.Top + ScaledH;
          end;

          // Desenha a imagem
          if (ImageFinalDestRect.Right > ImageFinalDestRect.Left) and (ImageFinalDestRect.Bottom > ImageFinalDestRect.Top) then
          begin
            if FImageSettings.Picture.Graphic is TPNGImage then
              DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, ImageFinalDestRect, idmStretch)
            else if FImageSettings.Picture.Graphic <> nil then
              DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, ImageFinalDestRect, idmStretch);
          end;
      end;

      // Desenha o texto dentro do seu slot
      LTextArea := textSlotRect;
      if Trim(Self.CaptionSettings.Text) <> '' then LFinalCaptionToDraw := Self.CaptionSettings.Text
      else if Trim(LPresetDefaultCaption) <> '' then LFinalCaptionToDraw := LPresetDefaultCaption
      else LFinalCaptionToDraw := '';

      if Trim(LFinalCaptionToDraw) <> '' then
      begin
        LCurrentTitleFont := TFont.Create;
        try
          LCurrentTitleFont.Assign(FCaptionSettings.Font);
          if Enabled then
          begin
            if FHoverSettings.Enabled and (LHoverProgress > 0) and not FProcessing then
            begin
              if FHoverSettings.FontColor <> clNone then LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, FHoverSettings.FontColor, LHoverProgress)
              else if FHoverSettings.HoverEffect = heFade then LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, LighterColor(LActualFillColor, 80), LHoverProgress * 0.5);
              if FHoverSettings.HoverEffect = heScale then LCurrentTitleFont.Size := Round(FCaptionSettings.Font.Size * (1 + LHoverProgress * (1.05 - 1)));
            end;
            if FClickEffectActive and (LClickProgress > 0) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then
            begin
              if FClickSettings.FontColor <> clNone then
                LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickSettings.FontColor, LClickProgress);
            end;
          end
          else if not Enabled then begin LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, clGray, 0.6); end;

          // Aplica as margens do texto dentro do seu slot
          LTextArea.Left   := LTextArea.Left   + FCaptionSettings.Margins.Left;
          LTextArea.Top    := LTextArea.Top    + FCaptionSettings.Margins.Top;
          LTextArea.Right  := LTextArea.Right  - FCaptionSettings.Margins.Right;
          LTextArea.Bottom := LTextArea.Bottom - FCaptionSettings.Margins.Bottom;

          if (LTextArea.Width > 0) and (LTextArea.Height > 0) then
            DrawComponentCaption( Self.Canvas, LTextArea, LFinalCaptionToDraw, LCurrentTitleFont, LCurrentTitleFont.Color, FCaptionSettings.Alignment, cvaCenter, False, 255 );
        finally
          LCurrentTitleFont.Free;
        end;
      end;
    end;
  finally
    LG.Free;
  end;
  if Focused and TabStop and Enabled then
    DrawFocusRect(Canvas.Handle, ClientRect);
end;

procedure THTL_CButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and (Key in [VK_RETURN, VK_SPACE]) then
  begin
    StartClickEffect;
    Click;
    Key := 0;
  end;
end;

end.

