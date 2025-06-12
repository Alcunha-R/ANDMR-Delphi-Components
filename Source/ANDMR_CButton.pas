unit ANDMR_CButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.StdCtrls, System.Types, System.Math,
  Vcl.Imaging.pngimage, System.UITypes, ANDMR_ComponentUtils,
  Winapi.GDIPOBJ, Winapi.GDIPAPI;

type
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);
  TPresetType = (cptNone, cptAccept, cptDecline, cptSave, cptEdit, cptDelete, cptNext, cptPrevious, cptInfo, cptWarning, cptHelp);

  TANDMR_CButton = class(TCustomControl)
  private
    // State Fields
    FIsHovering: Boolean;
    FIsPressed: Boolean;
    FProcessing: Boolean;
    FOriginalCaption: string;
    FOriginalEnabledState: Boolean;
    FProgressStep: Integer;

    // Timers
    FClickEffectTimer: TTimer;
    FProgressTimer: TTimer;

    // Component Settings (Published)
    FTags: TANDMR_MultiTag;
    FBorderSettings: TBorderSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FClickSettings: TClickSettings;
    FGradientSettings: TGradientSettings;
    FProgressSettings: TProgressSettings;
    FStyle: TButtonStyle;
    FPresetType: TPresetType;
    FTransparent: Boolean;
    FDisabledCursor: TCursor;

    // Event Handlers for Settings
    procedure SettingsChanged(Sender: TObject);

    // Setters
    procedure SetAlign(const Value: TAlign);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
    procedure SetCaption(const Value: string);
    function GetCaption: string;
    procedure SetTags(const Value: TANDMR_MultiTag);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetClickSettings(const Value: TClickSettings);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetProgressSettings(const Value: TProgressSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetPresetType(const Value: TPresetType);
    procedure SetTransparent(const Value: Boolean);
    procedure SetDisabledCursor(const Value: TCursor);

    // Message and Event Handlers
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;

    // Timers Handlers
    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure ProgressTimerHandler(Sender: TObject);

  protected
    // Refactored Paint Methods
    procedure Paint; override;
    procedure PaintBackground(AGraphics: TGPGraphics; const ADrawRect: TRect);
    procedure PaintContent(AGraphics: TGPGraphics; const ADrawRect: TRect);
    procedure PaintProcessingIndicator(AGraphics: TGPGraphics; const ADrawRect: TRect);

    // Other protected methods
    procedure Loaded; override;
    procedure ApplyPreset(APreset: TPresetType);
    procedure StartClickEffect;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartProcessing;
    procedure StopProcessing;

  published
    // Main Properties
    property Align;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Caption: string read GetCaption write SetCaption;
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;

    // Settings Objects
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property ClickSettings: TClickSettings read FClickSettings write SetClickSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;
    property Tags: TANDMR_MultiTag read FTags write SetTags;

    // Appearance & Behavior
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property DisabledCursor: TCursor read FDisabledCursor write SetDisabledCursor default crNo;

    // Standard Events and Properties
    property OnClick;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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

uses Winapi.GDIPUTIL;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButton]);
end;

{ TANDMR_CButton }

constructor TANDMR_CButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csParentBackground, csSetCaption, csAcceptsControls, csNoStdEvents];
  Width := 120;
  Height := 40;
  TabStop := True;
  DoubleBuffered := True;
  Cursor := crHandPoint;
  FDisabledCursor := crNo;
  FTransparent := False;
  FStyle := bsSolid;

  // Create Settings Objects
  FTags := TANDMR_MultiTag.Create;
  FTags.OnChange := SettingsChanged;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.BackgroundColor := clTeal;
  FBorderSettings.Color := clBlack;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := Self.Name;
  FCaptionSettings.Font.Name := 'Segoe UI';
  FCaptionSettings.Font.Size := 9;
  FCaptionSettings.Font.Style := [fsBold];
  FCaptionSettings.Font.Color := clWhite;
  FCaptionSettings.Alignment := taCenter;
  FCaptionSettings.VerticalAlignment := cvaCenter;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := SettingsChanged;
  FHoverSettings.OnAnimationProgress := SettingsChanged;

  FClickSettings := TClickSettings.Create;
  FClickSettings.OnChange := SettingsChanged;

  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;

  FProgressSettings := TProgressSettings.Create(Self);
  FProgressSettings.OnChange := SettingsChanged;

  // Create Timers
  FClickEffectTimer := TTimer.Create(Self);
  FClickEffectTimer.Enabled := False;
  FClickEffectTimer.OnTimer := ClickEffectTimerHandler;

  FProgressTimer := TTimer.Create(Self);
  FProgressTimer.Enabled := False;
  FProgressTimer.OnTimer := ProgressTimerHandler;
end;

destructor TANDMR_CButton.Destroy;
begin
  FTags.Free;
  FBorderSettings.Free;
  FCaptionSettings.Free;
  FHoverSettings.Free;
  FClickSettings.Free;
  FGradientSettings.Free;
  FProgressSettings.Free;
  inherited;
end;

procedure TANDMR_CButton.Loaded;
begin
  inherited Loaded;
  ApplyPreset(FPresetType);
  SetTransparent(FTransparent);
end;

procedure TANDMR_CButton.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CButton.SetPresetType(const Value: TPresetType);
begin
  if FPresetType <> Value then
  begin
    FPresetType := Value;
    ApplyPreset(Value);
    Invalidate;
  end;
end;

procedure TANDMR_CButton.ApplyPreset(APreset: TPresetType);
var
  BaseColor, TitleColor: TColor;
begin
  if csLoading in ComponentState then Exit;
  if APreset = cptNone then Exit;

  var PresetCaption: string := '';
  case APreset of
    cptAccept:   begin BaseColor := TColor($0050AF4C); TitleColor := clWhite; PresetCaption := 'Confirmar'; end;
    cptDecline:  begin BaseColor := TColor($00757575); TitleColor := clWhite; PresetCaption := 'Cancelar';  end;
    cptSave:     begin BaseColor := TColor($00F39621); TitleColor := clWhite; PresetCaption := 'Salvar';    end;
    cptEdit:     begin BaseColor := TColor($000098FF); TitleColor := clBlack; PresetCaption := 'Editar';    end;
    cptDelete:   begin BaseColor := TColor($003643F4); TitleColor := clWhite; PresetCaption := 'Excluir';   end;
    cptNext:     begin BaseColor := TColor($00F4A903); TitleColor := clWhite; PresetCaption := 'Avançar';   end;
    cptPrevious: begin BaseColor := TColor($009E9E9E); TitleColor := clBlack; PresetCaption := 'Voltar';    end;
    cptInfo:     begin BaseColor := TColor($00F7C34F); TitleColor := clBlack; PresetCaption := 'Informação';end;
    cptWarning:  begin BaseColor := TColor($003BEBFF); TitleColor := clBlack; PresetCaption := 'Aviso';     end;
    cptHelp:     begin BaseColor := TColor($008B7D60); TitleColor := clWhite; PresetCaption := 'Ajuda';     end;
  else
    Exit;
  end;

  if (Caption = '') or (Caption = Self.Name) then Caption := PresetCaption;

  FBorderSettings.BackgroundColor := BaseColor;
  FBorderSettings.Color := DarkerColor(BaseColor, 30);
  FCaptionSettings.Font.Color := TitleColor;
  FHoverSettings.BackgroundColor := LighterColor(BaseColor, 20);
  FHoverSettings.BorderColor := BaseColor;
  FClickSettings.Color := DarkerColor(BaseColor, 15);
  FClickSettings.BorderColor := DarkerColor(BaseColor, 30);
end;

procedure TANDMR_CButton.SetAlign(const Value: TAlign);
begin
  if Align <> Value then inherited Align := Value;
end;

function TANDMR_CButton.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TANDMR_CButton.SetEnabled(const Value: Boolean);
begin
  if inherited Enabled <> Value then
  begin
    inherited Enabled := Value;
  end;
end;

function TANDMR_CButton.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure TANDMR_CButton.SetCaption(const Value: string);
begin
  FCaptionSettings.Text := Value;
end;

procedure TANDMR_CButton.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButton.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButton.SetDisabledCursor(const Value: TCursor);
begin
  if FDisabledCursor <> Value then
  begin
    FDisabledCursor := Value;
    if not Enabled then Cursor := FDisabledCursor;
  end;
end;

procedure TANDMR_CButton.SetTags(const Value: TANDMR_MultiTag); begin FTags.Assign(Value); end;
procedure TANDMR_CButton.SetBorderSettings(const Value: TBorderSettings); begin FBorderSettings.Assign(Value); end;
procedure TANDMR_CButton.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); end;
procedure TANDMR_CButton.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); end;
procedure TANDMR_CButton.SetClickSettings(const Value: TClickSettings); begin FClickSettings.Assign(Value); end;
procedure TANDMR_CButton.SetGradientSettings(const Value: TGradientSettings); begin FGradientSettings.Assign(Value); end;
procedure TANDMR_CButton.SetProgressSettings(const Value: TProgressSettings); begin FProgressSettings.Assign(Value); end;

procedure TANDMR_CButton.Click;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TANDMR_CButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and (Key in [VK_RETURN, VK_SPACE]) then
  begin
    FIsPressed := True;
    StartClickEffect;
    Click;
    Key := 0;
  end;
end;

procedure TANDMR_CButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    FIsPressed := True;
    StartClickEffect;
  end;
end;

procedure TANDMR_CButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsPressed := False;
  inherited;
end;

procedure TANDMR_CButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FIsHovering := True;
  if Enabled and FHoverSettings.Enabled then
    FHoverSettings.StartAnimation(True);
end;

procedure TANDMR_CButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FIsHovering := False;
  FIsPressed := False;
  if FHoverSettings.Enabled then
    FHoverSettings.StartAnimation(False);
end;

procedure TANDMR_CButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Cursor := IfThen(Enabled, crHandPoint, FDisabledCursor);
  if not Enabled then
  begin
    FIsHovering := False;
    FIsPressed := False;
    FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

procedure TANDMR_CButton.StartProcessing;
begin
  if FProgressSettings.ShowProgress and not FProcessing then
  begin
    FProcessing := True;
    FOriginalCaption := Self.Caption;
    FOriginalEnabledState := Self.Enabled;
    Self.Enabled := False;
    FProgressStep := 0;
    FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
    FProgressTimer.Enabled := True;
    Invalidate;
  end;
end;

procedure TANDMR_CButton.StopProcessing;
begin
  if FProcessing then
  begin
    FProcessing := False;
    FProgressTimer.Enabled := False;
    Self.Caption := FOriginalCaption;
    Self.Enabled := FOriginalEnabledState;
    Invalidate;
  end;
end;

procedure TANDMR_CButton.ProgressTimerHandler(Sender: TObject);
begin
  if FProcessing then
  begin
    Inc(FProgressStep);
    Invalidate;
  end
  else
    FProgressTimer.Enabled := False;
end;

procedure TANDMR_CButton.StartClickEffect;
begin
  if not Enabled or not FClickSettings.Enabled or (FClickSettings.Duration <= 0) then Exit;
  FClickEffectTimer.Tag := GetTickCount;
  FClickEffectTimer.Interval := 15;
  FClickEffectTimer.Enabled := True;
  Invalidate;
end;

procedure TANDMR_CButton.ClickEffectTimerHandler(Sender: TObject);
var
  ElapsedTime: Cardinal;
begin
  ElapsedTime := GetTickCount - FClickEffectTimer.Tag;
  if ElapsedTime >= FClickSettings.Duration then
  begin
    FClickEffectTimer.Enabled := False;
    FIsPressed := False;
  end;
  Invalidate;
end;

procedure TANDMR_CButton.Paint;
var
  LG: TGPGraphics;
  DrawRect: TRect;
begin
  DrawRect := ClientRect;
  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    PaintBackground(LG, DrawRect);

    if FProcessing and FProgressSettings.ShowProgress then
      PaintProcessingIndicator(LG, DrawRect)
    else
      PaintContent(LG, DrawRect);
  finally
    LG.Free;
  end;

  if Focused and TabStop and Enabled then
    DrawFocusRect(Canvas.Handle, DrawRect);
end;

procedure TANDMR_CButton.PaintBackground(AGraphics: TGPGraphics; const ADrawRect: TRect);
var
  LFillColor, LBorderColor, TempColor, ResolvedStartColor, ResolvedEndColor: TColor;
  LBorderThickness, SurroundColorCount: Integer;
  LRadius: Single;
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LPathRect: TGPRectF;
  LGradientEnabled: Boolean;
  ClickProgress: Single;
  PathBrush: TGPPathGradientBrush;
  CenterColor: TGPColor;
  SurroundColors: array[0..0] of TGPColor;
  LinMode: LinearGradientMode;
const
  GRADIENT_DARK_FACTOR = 25;
begin
  LFillColor := FBorderSettings.BackgroundColor;
  LBorderColor := FBorderSettings.Color;
  LBorderThickness := FBorderSettings.Thickness;
  LGradientEnabled := FGradientSettings.Enabled;

  case FStyle of
    bsSolid:    LGradientEnabled := False;
    bsFaded:    begin LFillColor := BlendColors(LFillColor, clWhite, 0.8); LBorderThickness := 0; LGradientEnabled := False; end;
    bsBordered: begin LFillColor := clNone; LBorderThickness := Max(1, LBorderThickness); LGradientEnabled := False; end;
    bsLight:    begin LFillColor := BlendColors(LFillColor, clWhite, 0.6); LBorderThickness := Max(1, LBorderThickness); LGradientEnabled := False; end;
    bsFlat:     begin LBorderThickness := 0; LGradientEnabled := False; end;
    bsGhost:    begin LFillColor := clNone; LBorderColor := LFillColor; LBorderThickness := Max(1, LBorderThickness); LGradientEnabled := False; end;
    bsDark:     begin LFillColor := DarkerColor(LFillColor, 60); LBorderColor := LighterColor(LFillColor, 20); LGradientEnabled := False; end;
    bsWindows:  begin LFillColor := TColor($FFEFEFEF); LBorderColor := TColor($FFDCDCDC); LBorderThickness := 1; LGradientEnabled := False; end;
    bsMacOS:    begin LFillColor := TColor($FFF2F2F7); LBorderColor := TColor($FFD1D1D6); LBorderThickness := 1; LGradientEnabled := False; end;
  end;

  if FTransparent then LFillColor := clNone;

  if not Enabled then
  begin
    LFillColor := BlendColors(LFillColor, clGray, 0.65);
    LBorderColor := BlendColors(LBorderColor, clGray, 0.70);
  end
  else
  begin
    TempColor := IfThen(FHoverSettings.BackgroundColor <> clNone, FHoverSettings.BackgroundColor, LighterColor(FBorderSettings.BackgroundColor, 15));
    LFillColor := BlendColors(LFillColor, TempColor, FHoverSettings.CurrentAnimationValue / 255.0);
    TempColor := IfThen(FHoverSettings.BorderColor <> clNone, FHoverSettings.BorderColor, FBorderSettings.BackgroundColor);
    LBorderColor := BlendColors(LBorderColor, TempColor, FHoverSettings.CurrentAnimationValue / 255.0);
    if FIsPressed and FClickEffectTimer.Enabled then
    begin
      ClickProgress := Min(1.0, (GetTickCount - FClickEffectTimer.Tag) / FClickSettings.Duration);
      TempColor := IfThen(FClickSettings.Color <> clNone, FClickSettings.Color, DarkerColor(FBorderSettings.BackgroundColor, 15));
      LFillColor := BlendColors(LFillColor, TempColor, ClickProgress);
      TempColor := IfThen(FClickSettings.BorderColor <> clNone, FClickSettings.BorderColor, DarkerColor(FBorderSettings.Color, 15));
      LBorderColor := BlendColors(LBorderColor, TempColor, ClickProgress);
    end;
  end;

  LPathRect.X := ADrawRect.Left; LPathRect.Y := ADrawRect.Top;
  LPathRect.Width := ADrawRect.Width; LPathRect.Height := ADrawRect.Height;
  if LBorderThickness > 0 then
  begin
    LPathRect.X := LPathRect.X + LBorderThickness / 2;
    LPathRect.Y := LPathRect.Y + LBorderThickness / 2;
    LPathRect.Width := LPathRect.Width - LBorderThickness;
    LPathRect.Height := LPathRect.Height - LBorderThickness;
  end;
  LPathRect.Width := Max(0, LPathRect.Width);
  LPathRect.Height := Max(0, LPathRect.Height);
  LRadius := Min(FBorderSettings.CornerRadius, Min(LPathRect.Width, LPathRect.Height) / 2.0);

  LPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LPath, LPathRect, LRadius, FBorderSettings.RoundCornerType);
    if (LFillColor <> clNone) and (LPath.GetPointCount > 0) then
    begin
      LBrush := nil;
      try
        if LGradientEnabled and FGradientSettings.Enabled then
        begin
          ResolvedStartColor := IfThen(FGradientSettings.StartColor = clNone, LFillColor, FGradientSettings.StartColor);
          ResolvedEndColor := IfThen(FGradientSettings.EndColor = clNone, DarkerColor(ResolvedStartColor, GRADIENT_DARK_FACTOR), FGradientSettings.EndColor);
          case FGradientSettings.GradientType of
            gtLinearVertical, gtLinearHorizontal, gtDiagonalDown, gtDiagonalUp:
            begin
              case FGradientSettings.GradientType of
                gtLinearVertical:   LinMode := LinearGradientModeVertical;
                gtLinearHorizontal: LinMode := LinearGradientModeHorizontal;
                gtDiagonalDown:     LinMode := LinearGradientModeForwardDiagonal;
                gtDiagonalUp:       LinMode := LinearGradientModeBackwardDiagonal;
              end;
              LBrush := TGPLinearGradientBrush.Create(LPathRect, ColorToARGB(ResolvedStartColor), ColorToARGB(ResolvedEndColor), LinMode);
            end;
            gtRadial, gtCenterBurst:
            begin
              PathBrush := TGPPathGradientBrush.Create(LPath);
              CenterColor := ColorToARGB(ResolvedStartColor);
              SurroundColors[0] := ColorToARGB(ResolvedEndColor);
              SurroundColorCount := 1;
              PathBrush.SetSurroundColors(@SurroundColors[0], SurroundColorCount);
              PathBrush.SetCenterPoint(MakePoint(LPathRect.X + LPathRect.Width / 2, LPathRect.Y + LPathRect.Height / 2));
              LBrush := PathBrush;
            end;
          end;
        end;
        if LBrush = nil then
          LBrush := TGPSolidBrush.Create(ColorToARGB(LFillColor));
        AGraphics.FillPath(LBrush, LPath);
      finally
        LBrush.Free;
      end;
    end;
    if (LBorderThickness > 0) and (LBorderColor <> clNone) and (FBorderSettings.Style <> psClear) then
    begin
      LPen := TGPPen.Create(ColorToARGB(LBorderColor), LBorderThickness);
      try
        AGraphics.DrawPath(LPen, LPath);
      finally
        LPen.Free;
      end;
    end;
  finally
    LPath.Free;
  end;
end;

//******************************************************************************
//** INÍCIO DA CORREÇÃO: Procedimento PaintContent revisado
//******************************************************************************
procedure TANDMR_CButton.PaintContent(AGraphics: TGPGraphics; const ADrawRect: TRect);
var
  LContentRect, LImageRect, LTextRect: TRect;
  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY: Integer;
  AvailableWidth, AvailableHeight: Integer;
  LCurrentTitleFont: TFont;
  imgRatio, availRatio: Double;
begin
  LContentRect := ADrawRect;
  InflateRect(LContentRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  // Inicializa retângulos e variáveis
  LImgW := 0; LImgH := 0;
  LDrawW := 0; LDrawH := 0;
  LImgX := 0; LImgY := 0;
  LImageRect := Rect(0, 0, 0, 0);

  LTextRect := Rect(LContentRect.Left + FCaptionSettings.Margins.Left,
                    LContentRect.Top + FCaptionSettings.Margins.Top,
                    LContentRect.Right - FCaptionSettings.Margins.Right,
                    LContentRect.Bottom - FCaptionSettings.Margins.Bottom);

  // Desenha o texto usando GDI+
  if (FCaptionSettings.Text <> '') and (LTextRect.Width > 0) and (LTextRect.Height > 0) then
  begin
    LCurrentTitleFont := TFont.Create;
    try
      LCurrentTitleFont.Assign(FCaptionSettings.Font);
      var LFontColor := FCaptionSettings.Font.Color;
      if not Enabled then LFontColor := clGrayText;

      var LFinalHoverFontColor := IfThen(FHoverSettings.FontColor <> clNone, FHoverSettings.FontColor, LFontColor);
      LFontColor := ANDMR_ComponentUtils.BlendColors(LFontColor, LFinalHoverFontColor, FHoverSettings.CurrentAnimationValue / 255.0);

      if FIsPressed and FClickEffectTimer.Enabled then
      begin
        var ClickProgress := Min(1.0, (GetTickCount - FClickEffectTimer.Tag) / FClickSettings.Duration);
        var LFinalClickFontColor := IfThen(FClickSettings.FontColor <> clNone, FClickSettings.FontColor, ANDMR_ComponentUtils.DarkerColor(LFontColor, 15));
        LFontColor := ANDMR_ComponentUtils.BlendColors(LFontColor, LFinalClickFontColor, ClickProgress);
      end;

      ANDMR_ComponentUtils.DrawComponentCaption(Self.Canvas, LTextRect, FCaptionSettings.Text, LCurrentTitleFont, LFontColor, FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, 255);
    finally
      LCurrentTitleFont.Free;
    end;
  end;
end;
//******************************************************************************
//** FIM DA CORREÇÃO
//******************************************************************************

procedure TANDMR_CButton.PaintProcessingIndicator(AGraphics: TGPGraphics; const ADrawRect: TRect);
var
  OriginalProgressRect, AnimationRect, TextRect, BarRect, DrawRect: TRect;
  ArcRectF: TGPRectF;
  LProgressPath: TGPGraphicsPath;
  LProgressBarPen: TGPPen;
  LGPBrush: TGPBrush;
  LStartAngle, LSweepAngle: Single;
  LArcThickness, BarWidth, BarX, DotSize, DotSpacing, TotalDotWidth, StartX, BaseY, i, VerticalPadding: Integer;
  DotYOffset: array[0..2] of Single;
  ProgressCaptionFont: TFont;
begin
  // --- 1. CALCULAR ÁREA DE CONTEÚDO BASE ---
  OriginalProgressRect := ADrawRect;
  if FBorderSettings.Thickness > 0 then
    InflateRect(OriginalProgressRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  VerticalPadding := Round(OriginalProgressRect.Height * 0.15);
  InflateRect(OriginalProgressRect, 0, -VerticalPadding);


  // --- 2. LÓGICA DE LAYOUT DINÂMICO ---
  // Por padrão, a animação ocupa todo o espaço.
  AnimationRect := OriginalProgressRect;
  TextRect := Rect(0,0,0,0); // Área de texto inicia vazia.

  if FProgressSettings.ShowProgressText and (FProgressSettings.ProgressText <> '') then
  begin
    // --- Layout COM Texto ---
    if FProgressSettings.AnimationStyle in [pasRotatingSemiCircle, pasFullCircularSpinner, pasBouncingDots] then
    begin
      // LAYOUT HORIZONTAL: Animação à esquerda, texto à direita.
      var AnimWidth := OriginalProgressRect.Height;
      // Verifica se há espaço para a animação quadrada, margens e um texto mínimo.
      if OriginalProgressRect.Width > (AnimWidth + FCaptionSettings.Margins.Left + 30) then
      begin
        AnimationRect.Width := AnimWidth;
        TextRect.Left := AnimationRect.Right + FCaptionSettings.Margins.Left;
        TextRect.Top := OriginalProgressRect.Top;
        TextRect.Right := OriginalProgressRect.Right - FCaptionSettings.Margins.Right;
        TextRect.Bottom := OriginalProgressRect.Bottom;
      end;
      // Se não houver espaço, o layout padrão (animação em tela cheia) é mantido.
    end
    else // pasHorizontalBar
    begin
      // **NOVO LAYOUT VERTICAL:** Texto em cima, barra de animação embaixo.
      TextRect := OriginalProgressRect;
      TextRect.Height := Round(OriginalProgressRect.Height * 0.5); // Texto ocupa 50% da altura

      AnimationRect := OriginalProgressRect;
      AnimationRect.Top := TextRect.Bottom; // Animação começa abaixo do texto
    end;
  end;

  // --- 3. DESENHAR ANIMAÇÃO DENTRO DA 'AnimationRect' ---
  case FProgressSettings.AnimationStyle of
    pasRotatingSemiCircle, pasFullCircularSpinner, pasBouncingDots:
    begin
      DrawRect := AnimationRect;
      if DrawRect.Width > DrawRect.Height then
      begin
        DrawRect.Left := DrawRect.Left + (DrawRect.Width - DrawRect.Height) div 2;
        DrawRect.Width := DrawRect.Height;
      end else
      begin
        DrawRect.Top := DrawRect.Top + (DrawRect.Height - DrawRect.Width) div 2;
        DrawRect.Height := DrawRect.Width;
      end;
      InflateRect(DrawRect, -2, -2);

      if (DrawRect.Width > 4) then
      begin
        if FProgressSettings.AnimationStyle = pasBouncingDots then
        begin
          DotSize := Max(4, DrawRect.Width div 5);
          DotSpacing := DotSize div 2;
          TotalDotWidth := (3 * DotSize) + (2 * DotSpacing);
          StartX := DrawRect.Left + (DrawRect.Width - TotalDotWidth) div 2;
          BaseY := DrawRect.Top + (DrawRect.Height - DotSize) div 2;
          LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
          try
            for i := 0 to 2 do
            begin
              DotYOffset[i] := (DotSize / 2.5) * Sin(FProgressStep * 0.2 + i * (PI/3));
              AGraphics.FillEllipse(LGPBrush, StartX + i * (DotSize + DotSpacing), BaseY + DotYOffset[i], DotSize, DotSize);
            end;
          finally
            LGPBrush.Free;
          end;
        end
        else
        begin
            LArcThickness := Max(2, DrawRect.Width div 8);
            ArcRectF.X := DrawRect.Left; ArcRectF.Y := DrawRect.Top;
            ArcRectF.Width := DrawRect.Width; ArcRectF.Height := DrawRect.Height;
            ArcRectF.X := ArcRectF.X + (LArcThickness / 2); ArcRectF.Y := ArcRectF.Y + (LArcThickness / 2);
            ArcRectF.Width := ArcRectF.Width - LArcThickness; ArcRectF.Height := ArcRectF.Height - LArcThickness;
            if (ArcRectF.Width > 0) then
            begin
              LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
              try
                LProgressBarPen.SetStartCap(LineCapRound); LProgressBarPen.SetEndCap(LineCapRound);
                if FProgressSettings.AnimationStyle = pasRotatingSemiCircle then
                begin
                  LStartAngle := (FProgressStep * 10) mod 360;
                  AGraphics.DrawArc(LProgressBarPen, ArcRectF, LStartAngle, 270);
                end
                else
                begin
                   LProgressPath := TGPGraphicsPath.Create;
                   try
                     LStartAngle := (FProgressStep * 12) mod 360;
                     LSweepAngle := 90 + Sin(FProgressStep * 0.1) * 45;
                     LProgressPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                     AGraphics.DrawPath(LProgressBarPen, LProgressPath);
                   finally
                     LProgressPath.Free;
                   end;
                end;
              finally
                LProgressBarPen.Free;
              end;
            end;
        end;
      end;
    end;
    pasHorizontalBar:
    begin
      BarRect := AnimationRect;
      if BarRect.Width > 10 then
      begin
        var BackgroundBarRect := BarRect;
        InflateRect(BackgroundBarRect, 0, -Round(BackgroundBarRect.Height * 0.45));
        if BackgroundBarRect.Height < 3 then BackgroundBarRect.Height := 2;

        LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 40));
        try
          AGraphics.FillRectangle(LGPBrush, BackgroundBarRect.Left, BackgroundBarRect.Top, BackgroundBarRect.Width, BackgroundBarRect.Height);
        finally
          LGPBrush.Free;
        end;

        BarWidth := BarRect.Width div 3;
        BarX := (FProgressStep * FProgressSettings.AnimationProgressStep) mod (BarRect.Width + BarWidth) - BarWidth;
        LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
        try
          AGraphics.FillRectangle(LGPBrush, BarRect.Left + BarX, BackgroundBarRect.Top, BarWidth, BackgroundBarRect.Height);
        finally
          LGPBrush.Free;
        end;
      end;
    end;
  end;

  // --- 4. DESENHAR O TEXTO DENTRO DA 'TextRect' ---
  if FProgressSettings.ShowProgressText and (TextRect.Width > 0) and (TextRect.Height > 0) then
  begin
    ProgressCaptionFont := TFont.Create;
    try
      ProgressCaptionFont.Assign(Self.CaptionSettings.Font);
      if not Enabled then ProgressCaptionFont.Color := clGrayText;
      DrawComponentCaption(Self.Canvas, TextRect, FProgressSettings.ProgressText, ProgressCaptionFont, ProgressCaptionFont.Color,
                           taCenter, cvaCenter, False, 255);
    finally
      ProgressCaptionFont.Free;
    end;
  end;
end;

end.
