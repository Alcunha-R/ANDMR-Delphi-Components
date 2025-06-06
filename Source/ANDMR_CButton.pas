unit ANDMR_CButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.GraphUtil, System.UITypes, ANDMR_ComponentUtils,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  Winapi.ActiveX;

type
  TImagePosition = (ipLeft, ipRight, ipAbove, ipBelow, ipBehind);
  TImageStretchMode = (ismProportional, ismFlat); // Retained for compatibility if TImageSettings uses it
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

  TANDMR_CButton = class(TCustomControl)
  private
    FImagePosition: TImagePosition;
    FTag: Integer;
    FInternalTagString: TANDMR_TagString;
    FInternalTagExtended: TANDMR_TagExtended;
    FInternalTagObject: TANDMR_TagObject;
    FDisabledCursor: TCursor;
    FTransparent: Boolean;

    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectDuration: Integer;
    FClickEffectActive: Boolean;

    FOnClick: TNotifyEvent;
    FStyle: TButtonStyle;
    FClickColor: TColor;
    FClickBorderColor: TColor;
    FClickTitleColor: TColor;

    FPresetType: TPresetType;
    FHoverSettings: THoverSettings;

    FProcessing: Boolean;
    FProgressTimer: TTimer;
    FProgressStep: Integer;
    FProgressSettings: TProgressSettings;
    FOriginalCaption: string;
    FOriginalEnabledState: Boolean;

    procedure SetProgressSettings(const Value: TProgressSettings);
    procedure ProgressTimerHandler(Sender: TObject);
    procedure SetHoverSettings(const Value: THoverSettings); // Retained as property setter for HoverSettings
    procedure HoverSettingsChanged(Sender: TObject);
    procedure BorderSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);

    procedure SetStyle(const Value: TButtonStyle);
    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);

    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);

    function GetCaption: string;
    procedure SetCaption(const Value: string);

    procedure SetImagePosition(const Value: TImagePosition);
    procedure SetTag(const Value: Integer);
    function GetTagString: string;
    procedure SetTagString(const Value: string);
    function GetTagExtended: Extended;
    procedure SetTagExtended(const Value: Extended); // Corrected from GetTagExtended to SetTagExtended
    function GetTagObject: TObject;
    procedure SetTagObject(const Value: TObject);
    procedure SetDisabledCursor(const Value: TCursor);
    procedure SetClickColor(const Value: TColor);
    procedure SetClickBorderColor(const Value: TColor);
    procedure SetClickTitleColor(const Value: TColor);
    procedure SetClickEffectDuration(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function IsEnabledStored: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetPresetType(const Value: TPresetType);

  protected
    FBorderSettings: TBorderSettings;
    FCaptionSettings: TCaptionSettings;
    FImageSettings: TImageSettings;
    FGradientSettings: TGradientSettings;
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
  published
    property BorderSettings: TBorderSettings read FBorderSettings write FBorderSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write FCaptionSettings;
    property ImageSettings: TImageSettings read FImageSettings write FImageSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write FGradientSettings;

    property Align;
    property Enabled read GetEnabled write SetEnabled stored IsEnabledStored;
    property Caption: string read GetCaption write SetCaption;

    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;

    property ClickColor: TColor read FClickColor write SetClickColor default clNone;
    property ClickBorderColor: TColor read FClickBorderColor write SetClickBorderColor default clNone;
    property ClickTitleColor: TColor read FClickTitleColor write SetClickTitleColor default clNone; // Retained

    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property ClickEffectDuration: Integer read FClickEffectDuration write SetClickEffectDuration default 200;

    property DisabledCursor: TCursor read FDisabledCursor write SetDisabledCursor default crNo;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TagString: string read GetTagString write SetTagString;
    property TagExtended: Extended read GetTagExtended write SetTagExtended;
    property TagObject: TObject read GetTagObject write SetTagObject;

    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;

    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font; // Standard VCL Font property, keep
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

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButton]);
end;

{ TANDMR_CButton }

constructor TANDMR_CButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  Width := 100;
  Height := 45;
  TabStop := True;
  FTransparent := False;
  FClickTitleColor := clNone;

  FImagePosition := ipLeft;

  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;

  FDisabledCursor := crNo;
  Cursor := crHandPoint;

  DoubleBuffered := True;

  FClickEffectDuration := 200;
  FClickEffectProgress := 0;
  FClickEffectActive := False;
  FClickEffectTimer := TTimer.Create(Self);
  FClickEffectTimer.Enabled := False;
  FClickEffectTimer.OnTimer := ClickEffectTimerHandler;
  UpdateClickEffectTimerInterval;

  FClickColor := clNone;
  FClickBorderColor := clNone;

  FStyle := bsSolid;
  FPresetType := cptNone;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := BorderSettingsChanged;
  FBorderSettings.CornerRadius := 12;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.BackgroundColor := clTeal;
  FBorderSettings.Color := clBlack;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := Self.Name;
  var TempTitleFont: TFont;
  TempTitleFont := TFont.Create;
  try
    TempTitleFont.Name := 'Segoe UI';
    TempTitleFont.Size := 9;
    TempTitleFont.Style := [fsBold];
    TempTitleFont.Color := clWindowText;
    FCaptionSettings.Font.Assign(TempTitleFont);
  finally
    TempTitleFont.Free;
  end;
  // FCaptionSettings.Font.OnChange := FontChanged; // Removed TANDMR_CButton.FontChanged
  FCaptionSettings.Alignment := taCenter;

  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := SettingsChanged;
  FImageSettings.Picture.Create;
  FImageSettings.Margins.Create;
  FImageSettings.DrawMode := idmProportional;

  FInternalTagString := TANDMR_TagString.Create;
  FInternalTagExtended := TANDMR_TagExtended.Create;
  FInternalTagObject := TANDMR_TagObject.Create;

  FProgressSettings := TProgressSettings.Create(Self);
  FProgressSettings.OnChange := SettingsChanged;

  FProcessing := False;
  FProgressTimer := TTimer.Create(Self);
  FProgressTimer.Enabled := False;
  FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
  FProgressTimer.OnTimer := ProgressTimerHandler;
  FOriginalEnabledState := True;
end;

destructor TANDMR_CButton.Destroy;
begin
  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;

  FHoverSettings.OnChange := nil;
  FHoverSettings.Free;

  FCaptionSettings.OnChange := nil;
  FCaptionSettings.Free;

  FImageSettings.OnChange := nil;
  FImageSettings.Free;

  FClickEffectTimer.Free;

  if Assigned(FProgressSettings) then
  begin
    FProgressSettings.OnChange := nil;
    FProgressSettings.Free;
    FProgressSettings := nil;
  end;
  FProgressTimer.Free;

  FInternalTagString.Free;
  FInternalTagExtended.Free;
  FInternalTagObject.Free;

  FGradientSettings.OnChange := nil;
  FGradientSettings.Free;

  inherited;
end;

procedure TANDMR_CButton.StartProcessing;
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

procedure TANDMR_CButton.StopProcessing;
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

procedure TANDMR_CButton.SetProgressSettings(const Value: TProgressSettings);
begin
  FProgressSettings.Assign(Value);
end;

procedure TANDMR_CButton.ProgressTimerHandler(Sender: TObject);
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

procedure TANDMR_CButton.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  Repaint;
end;

procedure TANDMR_CButton.HoverSettingsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CButton.BorderSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CButton.SettingsChanged(Sender: TObject);
begin
  Repaint;
  Invalidate;
end;

procedure TANDMR_CButton.Loaded;
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

  if (csDesigning in ComponentState) and (Parent <> nil) then
  begin
    if (Self.Caption = Name) and (FPresetType = cptNone) then
      Self.Caption := '';
  end;
  Repaint;
end;

procedure TANDMR_CButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (csDesigning in ComponentState) and (AParent <> nil) then
  begin
    if (FCaptionSettings.Text = Name) and (FPresetType = cptNone) then
      FCaptionSettings.Text := '';
  end;
end;

function TANDMR_CButton.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TANDMR_CButton.SetAlign(const Value: TAlign);
begin
  inherited Align := Value;
end;

procedure TANDMR_CButton.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TANDMR_CButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    StartClickEffect;
  end;
end;

procedure TANDMR_CButton.StartClickEffect;
begin
  if not Enabled or (FClickEffectDuration <= 0) then Exit;

  FClickEffectActive := True;
  FClickEffectProgress := 255;
  UpdateClickEffectTimerInterval;
  FClickEffectTimer.Enabled := True;
  Repaint;
end;

procedure TANDMR_CButton.UpdateClickEffectTimerInterval;
const
  MIN_INTERVAL = 10;
  FADE_STEP_VALUE = 20;
var
  NumTicks: Single;
  NewInterval: Integer;
begin
  if FClickEffectDuration <= 0 then
  begin
    FClickEffectTimer.Interval := MIN_INTERVAL;
    Exit;
  end;

  NumTicks := 255 / FADE_STEP_VALUE;
  if NumTicks <= 0 then NumTicks := 1;

  NewInterval := Round(FClickEffectDuration / NumTicks);
  FClickEffectTimer.Interval := Max(MIN_INTERVAL, NewInterval);
end;

function TANDMR_CButton.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TANDMR_CButton.SetEnabled(Value: Boolean);
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

function TANDMR_CButton.IsEnabledStored: Boolean;
begin
  Result := not inherited Enabled;
end;

function TANDMR_CButton.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure TANDMR_CButton.SetCaption(const Value: string);
begin
  if FCaptionSettings.Text <> Value then
  begin
    FCaptionSettings.Text := Value;
  end;
end;

procedure TANDMR_CButton.SetPresetType(const Value: TPresetType);
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
      cptNone: begin BaseColor := BorderSettings.BackgroundColor; Exit; end;
      cptAccept:   begin BaseColor := TColor($0050AF4C); PresetCaption := 'Confirmar'; NewTitleColor := clWhite; end;
      cptDecline:  begin BaseColor := TColor($00757575); PresetCaption := 'Cancelar';  NewTitleColor := clWhite; end;
      cptSave:     begin BaseColor := TColor($00F39621); PresetCaption := 'Salvar';    NewTitleColor := clWhite; end;
      cptEdit:     begin BaseColor := TColor($000098FF); PresetCaption := 'Editar';    NewTitleColor := clBlack; end;
      cptDelete:   begin BaseColor := TColor($003643F4); PresetCaption := 'Excluir';   NewTitleColor := clWhite; end;
      cptNext:     begin BaseColor := TColor($00F4A903); PresetCaption := 'Avançar';   NewTitleColor := clWhite; end;
      cptPrevious: begin BaseColor := TColor($009E9E9E); PresetCaption := 'Voltar';    NewTitleColor := clBlack; end;
      cptInfo:     begin BaseColor := TColor($00F7C34F); PresetCaption := 'Informação';NewTitleColor := clBlack; end;
      cptWarning:  begin BaseColor := TColor($003BEBFF); PresetCaption := 'Aviso';     NewTitleColor := clBlack; end;
      cptHelp:     begin BaseColor := TColor($008B7D60); PresetCaption := 'Ajuda';     NewTitleColor := clWhite; end;
    else
      BaseColor := BorderSettings.BackgroundColor;
    end;

    BorderSettings.BackgroundColor := BaseColor;
    BorderSettings.Color := DarkerColor(BaseColor, 30);
    HoverSettings.BackgroundColor := LighterColor(BaseColor, 25);
    HoverSettings.BorderColor := BaseColor;
    Self.ClickColor := DarkerColor(BaseColor, 25); // ClickColor is direct property
    Self.ClickBorderColor := DarkerColor(BaseColor, 30); // ClickBorderColor is direct property
    FCaptionSettings.Font.Color := NewTitleColor;

    if (GetRValue(HoverSettings.BackgroundColor) * 0.299 + GetGValue(HoverSettings.BackgroundColor) * 0.587 + GetBValue(HoverSettings.BackgroundColor) * 0.114) > 186 then
      FHoverSettings.FontColor := clBlack
    else
      FHoverSettings.FontColor := clWhite;

    if (GetRValue(Self.ClickColor) * 0.299 + GetGValue(Self.ClickColor) * 0.587 + GetBValue(Self.ClickColor) * 0.114) > 186 then
      Self.ClickTitleColor := clBlack
    else
      Self.ClickTitleColor := clWhite;

    if (Trim(FCaptionSettings.Text) = '') or (FCaptionSettings.Text <> PresetCaption) then
    begin
      FCaptionSettings.Text := PresetCaption;
    end
    else if FPresetType <> cptNone then
    begin
      Repaint;
    end;
  end;
end;

procedure TANDMR_CButton.SetImagePosition(const Value: TImagePosition);
begin
  if FImagePosition <> Value then
  begin
    FImagePosition := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetTag(const Value: Integer); begin FTag := Value; end;

function TANDMR_CButton.GetTagString: string;
begin
  Result := FInternalTagString.AsString;
end;

procedure TANDMR_CButton.SetTagString(const Value: string);
begin
  FInternalTagString.AsString := Value;
end;

function TANDMR_CButton.GetTagExtended: Extended;
begin
  if FInternalTagExtended.Items.Count > 0 then
  begin
    try
      Result := StrToFloatDef(FInternalTagExtended.Items[0], 0.0);
    except
      Result := 0.0;
    end;
  end
  else
    Result := 0.0;
end;

procedure TANDMR_CButton.SetTagExtended(const Value: Extended);
begin
  FInternalTagExtended.Items.Clear;
  FInternalTagExtended.Items.Add(FloatToStr(Value));
end;

function TANDMR_CButton.GetTagObject: TObject;
begin
  Result := FInternalTagObject.ObjectValue;
end;

procedure TANDMR_CButton.SetTagObject(const Value: TObject);
begin
  FInternalTagObject.ObjectValue := Value;
end;

procedure TANDMR_CButton.SetDisabledCursor(const Value: TCursor);
begin
  if FDisabledCursor <> Value then
  begin
    FDisabledCursor := Value;
    if not Enabled then
      Cursor := FDisabledCursor;
  end;
end;

procedure TANDMR_CButton.SetClickColor(const Value: TColor);
begin
  if FClickColor <> Value then
  begin
    FClickColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetClickBorderColor(const Value: TColor);
begin
  if FClickBorderColor <> Value then
  begin
    FClickBorderColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetClickTitleColor(const Value: TColor);
begin
  if FClickTitleColor <> Value then
  begin
    FClickTitleColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetClickEffectDuration(const Value: Integer);
begin
  if FClickEffectDuration <> Value then
  begin
    FClickEffectDuration := Max(0, Value);
    UpdateClickEffectTimerInterval;
  end;
end;

procedure TANDMR_CButton.SetTransparent(const Value: Boolean);
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

procedure TANDMR_CButton.ClickEffectTimerHandler(Sender: TObject);
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

procedure TANDMR_CButton.CMEnabledChanged(var Message: TMessage);
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

procedure TANDMR_CButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and HoverSettings.Enabled then // Use HoverSettings property
  begin
    FHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_CButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and HoverSettings.Enabled then // Use HoverSettings property
  begin
    FHoverSettings.StartAnimation(False);
  end
  else if FHoverSettings.CurrentAnimationValue > 0 then
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CButton.Paint;
var
  LPathRect, LShadowPathDrawRect: TGPRectF;
  LRadiusValue, LPathInset: Single;
  LActualFillColor, LActualBorderColor, LInitialFillColor, LInitialBorderColor, LBaseStyleColor: TColor;
  LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalClickBorderColor: TColor;
  LG: TGPGraphics;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LTextArea, LDestRect, LImageClipRect: TRect;
  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY, AvailableHeight, AvailableWidth: Integer;
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
      cptNext: LPresetDefaultCaption := 'Avançar';
      cptPrevious: LPresetDefaultCaption := 'Voltar';
      cptInfo: LPresetDefaultCaption := 'Informação';
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
    if Enabled and HoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and (HoverSettings.HoverEffect <> heNone) and not FProcessing then
      LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and (FClickEffectDuration > 0) and not FProcessing then
      LClickProgress := (255 - FClickEffectProgress) / 255.0;

  LInitialFillColor := ResolveStateColor(Enabled, False, False, BorderSettings.BackgroundColor, clNone, clNone, BlendColors(BorderSettings.BackgroundColor, clGray, 0.65), False, False);
  LInitialBorderColor := ResolveStateColor(Enabled, False, False, BorderSettings.Color, clNone, clNone, BlendColors(BorderSettings.Color, clGray, 0.7), False, False);
  LActualBorderThickness := BorderSettings.Thickness;

    if HoverSettings.BackgroundColor <> clNone then
      LFinalHoverColor := HoverSettings.BackgroundColor
    else
      LFinalHoverColor := LighterColor(LInitialFillColor, 15);

    if HoverSettings.BorderColor <> clNone then
      LFinalHoverBorderColor := HoverSettings.BorderColor
    else
      LFinalHoverBorderColor := LInitialBorderColor;

    LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LInitialFillColor, 15), FClickColor);
    LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LInitialBorderColor, 15), FClickBorderColor);

    LActualFillColor := LInitialFillColor;
    LActualBorderColor := LInitialBorderColor;
    LCurrentGradientEnabled := GradientSettings.Enabled;
    LDrawFill := True;
    LDrawBorder := LActualBorderThickness > 0;

    case FStyle of
      bsSolid:
      begin
        LCurrentGradientEnabled := False;
      end;
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
        LActualBorderThickness := Max(1, BorderSettings.Thickness);
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(HoverSettings.BackgroundColor=clNone, LInitialFillColor, HoverSettings.BackgroundColor), 70);
      end;
      bsLight:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
        LActualFillColor := LBaseStyleColor;
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, BorderSettings.Thickness);
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
        LActualBorderThickness := Max(1, BorderSettings.Thickness);
        LActualBorderColor := LInitialFillColor;
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(HoverSettings.BackgroundColor=clNone, LInitialFillColor, HoverSettings.BackgroundColor), 100);
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
        LBaseStyleColor := DarkerColor(BorderSettings.BackgroundColor, 60);
        if (GetRValue(LBaseStyleColor) < 30) and (GetGValue(LBaseStyleColor) < 30) and (GetBValue(LBaseStyleColor) < 30) then
            LBaseStyleColor := TColor($FF383838);

        LActualFillColor := LBaseStyleColor;
        LActualBorderColor := LighterColor(LBaseStyleColor, 20);
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        LDrawBorder := True;
        LActualBorderThickness := Max(1, BorderSettings.Thickness);

        if HoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := HoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LBaseStyleColor, 15);

        if HoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := HoverSettings.BorderColor
        else
          LFinalHoverBorderColor := LighterColor(LActualBorderColor, 15);

        LFinalClickColor := IfThen(FClickColor = clNone, LighterColor(LBaseStyleColor, 10), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, LighterColor(LActualBorderColor, 10), FClickBorderColor);
      end;
      bsMaterial:
      begin
        LActualFillColor := BorderSettings.BackgroundColor;
        LActualBorderColor := clNone;
        LActualBorderThickness := 0;
        LDrawBorder := False;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        if HoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := HoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 10);

        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 10), FClickColor);
      end;
      bsModern:
      begin
        LActualFillColor := BorderSettings.BackgroundColor;
        LActualBorderColor := DarkerColor(LActualFillColor, 15);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        if HoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := HoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 8);

        if HoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := HoverSettings.BorderColor
        else
          LFinalHoverBorderColor := LActualFillColor;

        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 8), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualFillColor, 20), FClickBorderColor);
      end;
      bsWindows:
      begin
        LActualFillColor := TColor($FFEFEFEF);
        LActualBorderColor := TColor($FFDCDCDC);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        if HoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := HoverSettings.BackgroundColor
        else
          LFinalHoverColor := TColor($FFF5F5F5);

        if HoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := HoverSettings.BorderColor
        else
          LFinalHoverBorderColor := BorderSettings.BackgroundColor;

        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 10), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualBorderColor, 10), FClickBorderColor);
      end;
      bsMacOS:
      begin
        LActualFillColor := TColor($FFF2F2F7);
        LActualBorderColor := TColor($FFD1D1D6);
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        if HoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := HoverSettings.BackgroundColor
        else
          LFinalHoverColor := DarkerColor(LActualFillColor, 5);

        if HoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := HoverSettings.BorderColor
        else
          LFinalHoverBorderColor := DarkerColor(LActualBorderColor, 5);

        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 12), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualBorderColor, 12), FClickBorderColor);
      end;
    end;

    if (LHoverProgress > 0) and Enabled and HoverSettings.Enabled then // Use HoverSettings property
    begin
      if LDrawFill or (FStyle = bsBordered) or (FStyle = bsGhost) then
        LActualFillColor := BlendColors(LActualFillColor, LFinalHoverColor, LHoverProgress);

      if LDrawBorder then
        LActualBorderColor := BlendColors(LActualBorderColor, LFinalHoverBorderColor, LHoverProgress)
      else if FStyle = bsFlat then
      begin
        LActualBorderColor := BlendColors(clNone, LFinalHoverBorderColor, LHoverProgress);
        LActualBorderThickness := Max(1, BorderSettings.Thickness);
        LDrawBorder := True;
      end;
    end;

    if (LClickProgress > 0) and Enabled and (FClickEffectDuration > 0) then
    begin
      LActualFillColor := BlendColors(LActualFillColor, LFinalClickColor, LClickProgress * 0.7 + 0.1);
      if LDrawBorder then
        LActualBorderColor := BlendColors(LActualBorderColor, LFinalClickBorderColor, LClickProgress * 0.7 + 0.1);
    end;

    if FTransparent then
    begin
      LDrawFill := False;
      LCurrentGradientEnabled := False;
    end;

    LDrawBorder := LActualBorderThickness > 0;

    ButtonRectEffectiveF := MakeRect(0.0, 0.0, Self.Width, Self.Height);

    if (FStyle = bsMaterial) and (not FTransparent) then
    begin
      LShadowOffsetXToUse := 1;
      LShadowOffsetYToUse := 2;
      LShadowAlphaToUse := 60;

      const  MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR = 1.8;
      const  MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR = 1.8;
      const  MATERIAL_SHADOW_ALPHA_HOVER = 90;

      if (LHoverProgress > 0) and Enabled and HoverSettings.Enabled then // Use HoverSettings property
      begin
        var TempShadowOffsetXConst_Material: Single;
        var TempShadowOffsetYConst_Material: Single;
        TempShadowOffsetXConst_Material := 1;
        TempShadowOffsetYConst_Material := 2;

        LShadowOffsetXToUse := TempShadowOffsetXConst_Material + ((TempShadowOffsetXConst_Material * MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR) - TempShadowOffsetXConst_Material) * LHoverProgress;
        LShadowOffsetYToUse := TempShadowOffsetYConst_Material + ((TempShadowOffsetYConst_Material * MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR) - TempShadowOffsetYConst_Material) * LHoverProgress;
        LShadowAlphaToUse := Round(LShadowAlphaToUse + (MATERIAL_SHADOW_ALPHA_HOVER - LShadowAlphaToUse) * LHoverProgress);
      end;
    end;

    if ((FStyle = bsShadow) or (FStyle = bsMaterial)) and (not FTransparent) then
    begin
      if FStyle = bsShadow then
      begin
        LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST;
        LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST;
        LShadowAlphaToUse := SHADOW_ALPHA;

        if (LHoverProgress > 0) and Enabled and HoverSettings.Enabled then // Use HoverSettings property
        begin
          LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST + ((SHADOW_OFFSET_X_CONST * SHADOW_OFFSET_X_HOVER_FACTOR) - SHADOW_OFFSET_X_CONST) * LHoverProgress;
          LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST + ((SHADOW_OFFSET_Y_CONST * SHADOW_OFFSET_Y_HOVER_FACTOR) - SHADOW_OFFSET_Y_CONST) * LHoverProgress;
          LShadowAlphaToUse := Round(SHADOW_ALPHA + (SHADOW_ALPHA_HOVER - SHADOW_ALPHA) * LHoverProgress);
        end;
      end;

      ButtonRectEffectiveF.X := IfThen(LShadowOffsetXToUse < 0, Abs(LShadowOffsetXToUse), 0.0);
      ButtonRectEffectiveF.Y := IfThen(LShadowOffsetYToUse < 0, Abs(LShadowOffsetYToUse), 0.0);
      ButtonRectEffectiveF.Width := Self.Width - Abs(LShadowOffsetXToUse);
      ButtonRectEffectiveF.Height := Self.Height - Abs(LShadowOffsetYToUse);
      ButtonRectEffectiveF.Width := Max(0, ButtonRectEffectiveF.Width);
      ButtonRectEffectiveF.Height := Max(0, ButtonRectEffectiveF.Height);

      if LActualBorderThickness > 0 then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
      LPathWidth := ButtonRectEffectiveF.Width - 2 * LPathInset;
      LPathHeight := ButtonRectEffectiveF.Height - 2 * LPathInset;
      LPathWidth := Max(0, LPathWidth);
      LPathHeight := Max(0, LPathHeight);

      LShadowPathDrawRect := MakeRect(ButtonRectEffectiveF.X + LPathInset + LShadowOffsetXToUse,
                                      ButtonRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse,
                                      LPathWidth, LPathHeight);
    LRadiusValue := Min(BorderSettings.CornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0);
      LRadiusValue := Max(0, LRadiusValue);

      LGPPath := TGPGraphicsPath.Create;
      try
      CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, LRadiusValue, BorderSettings.RoundCornerType);
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
    LRadiusValue := Min(BorderSettings.CornerRadius, Min(ButtonRectEffectiveF.Width, ButtonRectEffectiveF.Height) / 2.0);
    LRadiusValue := Max(0, LRadiusValue);

    var DrawAreaRect: TRect;
    DrawAreaRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                         Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width), Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));

    var BgColorToUse: TColor;
    if LDrawFill and not FTransparent then
    begin
      if LCurrentGradientEnabled then
        BgColorToUse := IfThen(GradientSettings.StartColor = clNone, LActualFillColor, GradientSettings.StartColor)
      else
        BgColorToUse := LActualFillColor;
    end
    else
      BgColorToUse := clNone;

    var BorderColorToUse: TColor;
    if LDrawBorder then
      BorderColorToUse := LActualBorderColor
    else
      BorderColorToUse := clNone;

    // ===== NEW GRADIENT/SOLID FILL AND BORDER LOGIC START =====

    var  ButtonBodyPath: TGPGraphicsPath;
    var  PathRectF: TGPRectF; // This will be the rect for CreateGPRoundedPath
      // LRadiusValue is already declared and calculated before the original DrawEditBox
      // LPathInset is already declared

    // Define PathRectF based on ButtonRectEffectiveF, adjusted for border thickness
    if LActualBorderThickness > 0 then
    begin
      PathRectF.X      := ButtonRectEffectiveF.X + LActualBorderThickness / 2.0;
      PathRectF.Y      := ButtonRectEffectiveF.Y + LActualBorderThickness / 2.0;
      PathRectF.Width  := ButtonRectEffectiveF.Width - LActualBorderThickness;
      PathRectF.Height := ButtonRectEffectiveF.Height - LActualBorderThickness;
    end
    else
    begin
      PathRectF.X      := ButtonRectEffectiveF.X;
      PathRectF.Y      := ButtonRectEffectiveF.Y;
      PathRectF.Width  := ButtonRectEffectiveF.Width;
      PathRectF.Height := ButtonRectEffectiveF.Height;
    end;
    PathRectF.Width  := Max(0.0, PathRectF.Width);
    PathRectF.Height := Max(0.0, PathRectF.Height);

    // LRadiusValue should be confirmed to be in scope and correctly calculated
    // based on the final ButtonRectEffectiveF before this block.
    // Assuming LRadiusValue is already correctly calculated (as it was for the original DrawEditBox call).

    if (PathRectF.Width > 0) and (PathRectF.Height > 0) then
    begin
      ButtonBodyPath := TGPGraphicsPath.Create;
      try
        // Note: LRadiusValue is used here, ensure it's the correct one for the current PathRectF context
        CreateGPRoundedPath(ButtonBodyPath, PathRectF, LRadiusValue, BorderSettings.RoundCornerType);

        if ButtonBodyPath.GetPointCount > 0 then
        begin
          // Fill Logic
          if LDrawFill and not FTransparent then
          begin
            if LCurrentGradientEnabled and FGradientSettings.Enabled then
            begin
              var GradientBrush: TGPLinearGradientBrush;
              var ResolvedStartColor, ResolvedEndColor: TColor;
              var GradRect: TGPRectF;
              var LinGradMode: Integer;

              ResolvedStartColor := FGradientSettings.StartColor;
              if ResolvedStartColor = clNone then
                ResolvedStartColor := LActualFillColor;

              ResolvedEndColor := FGradientSettings.EndColor;
              if ResolvedEndColor = clNone then
                ResolvedEndColor := DarkerColor(ResolvedStartColor, GRADIENT_DARK_FACTOR);

              GradRect := PathRectF;

              case FGradientSettings.GradientType of
                gtLinearVertical: LinGradMode := 1;
                gtLinearHorizontal: LinGradMode := 0;
              else
                LinGradMode := 1;
              end;

              GradientBrush := TGPLinearGradientBrush.Create(GradRect, ColorToARGB(ResolvedStartColor), ColorToARGB(ResolvedEndColor), LinGradMode);
              try
                LG.FillPath(GradientBrush, ButtonBodyPath);
              finally
                GradientBrush.Free;
              end;
            end
            else // Solid Fill
            begin
              var FillBrush: TGPSolidBrush;
              FillBrush := TGPSolidBrush.Create(ColorToARGB(LActualFillColor));
              try
                LG.FillPath(FillBrush, ButtonBodyPath);
              finally
                FillBrush.Free;
              end;
            end;
          end;

          // Border Drawing Logic
          if LDrawBorder and (LActualBorderThickness > 0) and (LActualBorderColor <> clNone) and (BorderSettings.Style <> psClear) then
          begin
            var BorderPen: TGPPen;
            BorderPen := TGPPen.Create(ColorToARGB(LActualBorderColor), LActualBorderThickness);
            try
              case BorderSettings.Style of
                psSolid:      BorderPen.SetDashStyle(DashStyleSolid);
                psDash:       BorderPen.SetDashStyle(DashStyleDash);
                psDot:        BorderPen.SetDashStyle(DashStyleDot);
                psDashDot:    BorderPen.SetDashStyle(DashStyleDashDot);
                psDashDotDot: BorderPen.SetDashStyle(DashStyleDashDotDot);
              else
                BorderPen.SetDashStyle(DashStyleSolid);
              end;
              LG.DrawPath(BorderPen, ButtonBodyPath);
            finally
              BorderPen.Free;
            end;
          end;
        end;
      finally
        ButtonBodyPath.Free;
      end;
    end;
    // ===== NEW GRADIENT/SOLID FILL AND BORDER LOGIC END =====

    LImageClipRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                              Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width),
                              Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));

    if ImageSettings.Placement = iplInsideBounds then
    begin
      if LDrawBorder and (LActualBorderThickness > 0) then
          InflateRect(LImageClipRect, -Round(LActualBorderThickness), -Round(LActualBorderThickness));
    end;

    if FProcessing and FProgressSettings.ShowProgress then
    begin
      var LProgressRect: TRect;
      var LArcThickness: Integer;
      var LStartAngle, LSweepAngle: Single;
      var LProgressBarPen: TGPPen;
      var LProgressPath: TGPGraphicsPath;
      var ArcRectF: TGPRectF;
      var OriginalProgressRect: TRect;

      LProgressRect := ClientRect;
      if BorderSettings.Thickness > 0 then
        InflateRect(LProgressRect, -BorderSettings.Thickness, -BorderSettings.Thickness);

      OriginalProgressRect := LProgressRect;

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
            LProgressPath := TGPGraphicsPath.Create;
            try
              ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
              if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
              begin
                LProgressPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                LProgressBarPen.SetStartCap(LineCapRound);
                LProgressBarPen.SetEndCap(LineCapRound);
                try
                  LG.DrawPath(LProgressBarPen, LProgressPath);
                finally
                  LProgressBarPen.Free;
                end;
              end;
            finally
              LProgressPath.Free;
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
            LProgressPath := TGPGraphicsPath.Create;
            try
              ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
              if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
              begin
                LProgressPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                LProgressBarPen.SetStartCap(LineCapRound);
                LProgressBarPen.SetEndCap(LineCapRound);
                try
                  LG.DrawPath(LProgressBarPen, LProgressPath);
                finally
                  LProgressBarPen.Free;
                end;
              end;
            finally
              LProgressPath.Free;
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

          InflateRect(BarRect, 0, -BarRect.Height div 3);
          if BarRect.Height < 4 then BarRect.Height := Max(2, Min(LProgressRect.Height, 4));
          if BarRect.Width > 10 then
          begin
            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 100));
            try
              LG.FillRectangle(LGPBrush, BarRect.Left, BarRect.Top, BarRect.Width, BarRect.Height);
            finally
              LGPBrush.Free;
            end;
            InnerBarWidth := BarRect.Width div 3;
            if BarRect.Width - InnerBarWidth > 0 then
                InnerBarX := (FProgressStep * 5) mod (BarRect.Width - InnerBarWidth)
            else
                InnerBarX := 0;

            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
            try
              LG.FillRectangle(LGPBrush, BarRect.Left + InnerBarX, BarRect.Top, InnerBarWidth, BarRect.Height);
            finally
              LGPBrush.Free;
            end;
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
            TextRect.Left := AnimationAreaRightBound + CaptionSettings.Margins.Left;
            TextRect.Top  := OriginalProgressRect.Top;
            TextRect.Right := OriginalProgressRect.Left + OriginalProgressRect.Width - CaptionSettings.Margins.Right;
            TextRect.Bottom := OriginalProgressRect.Bottom;
        end
        else if (LAnimationStyle = pasHorizontalBar) and (OriginalProgressRect.Width > 100) then
        begin
            TextRect.Left := OriginalProgressRect.Left + CaptionSettings.Margins.Left;
            TextRect.Top := LProgressRect.Bottom + CaptionSettings.Margins.Top;
            TextRect.Right := OriginalProgressRect.Right - CaptionSettings.Margins.Right;
            TextRect.Bottom := OriginalProgressRect.Bottom;
        end
        else
        begin
            TextRect.Left := ClientRect.Left + CaptionSettings.Margins.Left;
            TextRect.Top := LProgressRect.Bottom + CaptionSettings.Margins.Top;
            TextRect.Right := ClientRect.Right - CaptionSettings.Margins.Right;
            TextRect.Bottom := ClientRect.Bottom - CaptionSettings.Margins.Bottom;
        end;

        if (TextRect.Width > 0) and (TextRect.Height > 0) then
        begin
          ProgressCaptionFont := TFont.Create;
          try
            ProgressCaptionFont.Assign(Self.CaptionSettings.Font);
            DrawComponentCaption(Self.Canvas, TextRect, LProgressText, ProgressCaptionFont, ProgressCaptionFont.Color, taCenter, cvaCenter, False, 255);
          finally
            ProgressCaptionFont.Free;
          end;
        end;
      end;
    end;

    if not (FProcessing and FProgressSettings.ShowProgress and FProgressSettings.HideCaptionWhileProcessing) then
    begin
      LImgW := 0; LImgH := 0; LDrawW := 0; LDrawH := 0; LImgX := 0; LImgY := 0;
      AvailableWidth := 0; AvailableHeight := 0;
      var imageCanvasX, imageCanvasY: Integer;
      var imgAspectRatio, availAspectRatio, targetAspectRatio: Single;

      if (ImageSettings.Picture.Graphic <> nil) and not ImageSettings.Picture.Graphic.Empty and ImageSettings.Visible then
      begin
        LImgW := ImageSettings.Picture.Width;
        LImgH := ImageSettings.Picture.Height;

        AvailableWidth  := Max(0, LImageClipRect.Width - ImageSettings.Margins.Left - ImageSettings.Margins.Right);
        AvailableHeight := Max(0, LImageClipRect.Height - ImageSettings.Margins.Top - ImageSettings.Margins.Bottom);

        if ImageSettings.AutoSize then
        begin
          case ImageSettings.DrawMode of
            idmStretch:
            begin
              LDrawW := AvailableWidth;
              LDrawH := AvailableHeight;
            end;
            idmProportional:
            begin
              if (LImgW = 0) or (LImgH = 0) or (AvailableWidth <= 0) or (AvailableHeight <= 0) then
              begin LDrawW := 0; LDrawH := 0; end
              else
              begin
                imgAspectRatio := LImgW / LImgH;
                availAspectRatio := AvailableWidth / AvailableHeight;
                if availAspectRatio > imgAspectRatio then
                begin
                  LDrawH := AvailableHeight;
                  LDrawW := Round(LDrawH * imgAspectRatio);
                end
                else
                begin
                  LDrawW := AvailableWidth;
                  LDrawH := Round(LDrawW / imgAspectRatio);
                end;
              end;
            end;
            idmNormal:
            begin
              LDrawW := LImgW;
              LDrawH := LImgH;
            end;
          else
            LDrawW := LImgW; LDrawH := LImgH;
          end;
        end
        else
        begin
          var targetW, targetH: Integer;
          targetW := ImageSettings.TargetWidth;
          targetH := ImageSettings.TargetHeight;

          case ImageSettings.DrawMode of
            idmStretch:
            begin
              LDrawW := targetW;
              LDrawH := targetH;
            end;
            idmProportional:
            begin
              if (LImgW = 0) or (LImgH = 0) or (targetW <= 0) or (targetH <= 0) then
              begin LDrawW := 0; LDrawH := 0; end
              else
              begin
                imgAspectRatio := LImgW / LImgH;
                targetAspectRatio := targetW / targetH;
                if targetAspectRatio > imgAspectRatio then
                begin
                  LDrawH := targetH;
                  LDrawW := Round(LDrawH * imgAspectRatio);
                end
                else
                begin
                  LDrawW := targetW;
                  LDrawH := Round(LDrawW / imgAspectRatio);
                end;
              end;
            end;
            idmNormal:
            begin
              LDrawW := LImgW;
              LDrawH := LImgH;
            end;
          else
            LDrawW := LImgW; LDrawH := LImgH;
          end;
        end;

        LDrawW := Max(0, LDrawW);
        LDrawH := Max(0, LDrawH);

        if LDrawW > AvailableWidth then LDrawW := AvailableWidth;
        if LDrawH > AvailableHeight then LDrawH := AvailableHeight;

        imageCanvasX := LImageClipRect.Left + ImageSettings.Margins.Left;
        imageCanvasY := LImageClipRect.Top + ImageSettings.Margins.Top;

        case ImageSettings.HorizontalAlign of
          ihaLeft:   LImgX := imageCanvasX;
          ihaCenter: LImgX := imageCanvasX + (AvailableWidth - LDrawW) div 2;
          ihaRight:  LImgX := imageCanvasX + AvailableWidth - LDrawW;
        else LImgX := imageCanvasX;
        end;

        case ImageSettings.VerticalAlign of
          ivaTop:    LImgY := imageCanvasY;
          ivaCenter: LImgY := imageCanvasY + (AvailableHeight - LDrawH) div 2;
          ivaBottom: LImgY := imageCanvasY + AvailableHeight - LDrawH;
        else LImgY := imageCanvasY;
        end;

        case FImagePosition of
          ipLeft:
            LTextArea := Rect(LImgX + LDrawW + ImageSettings.Margins.Right + CaptionSettings.Margins.Left,
                              LImageClipRect.Top + CaptionSettings.Margins.Top,
                              LImageClipRect.Right - CaptionSettings.Margins.Right,
                              LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
          ipRight:
            LTextArea := Rect(LImageClipRect.Left + CaptionSettings.Margins.Left,
                              LImageClipRect.Top + CaptionSettings.Margins.Top,
                              LImgX - ImageSettings.Margins.Left - CaptionSettings.Margins.Right,
                              LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
          ipAbove:
            LTextArea := Rect(LImageClipRect.Left + CaptionSettings.Margins.Left,
                              LImgY + LDrawH + ImageSettings.Margins.Bottom + CaptionSettings.Margins.Top,
                              LImageClipRect.Right - CaptionSettings.Margins.Right,
                              LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
          ipBelow:
            LTextArea := Rect(LImageClipRect.Left + CaptionSettings.Margins.Left,
                              LImageClipRect.Top + CaptionSettings.Margins.Top,
                              LImageClipRect.Right - CaptionSettings.Margins.Right,
                              LImgY - ImageSettings.Margins.Top - CaptionSettings.Margins.Bottom);
          ipBehind:
            LTextArea := Rect(LImageClipRect.Left + CaptionSettings.Margins.Left, LImageClipRect.Top + CaptionSettings.Margins.Top,
                              LImageClipRect.Right - CaptionSettings.Margins.Right, LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
        else
           LTextArea := Rect(LImgX + LDrawW + ImageSettings.Margins.Right + CaptionSettings.Margins.Left,
                              LImageClipRect.Top + CaptionSettings.Margins.Top,
                              LImageClipRect.Right - CaptionSettings.Margins.Right,
                              LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
        end;
        LDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

        if Enabled and HoverSettings.Enabled and (HoverSettings.HoverEffect = heScale) and (LHoverProgress > 0) then
        begin
          LScaleFactor := 1 + (LHoverProgress * (1.05 - 1));
          var ScaledW, ScaledH: Integer;
          ScaledW := Round(LDrawW * LScaleFactor);
          ScaledH := Round(LDrawH * LScaleFactor);
          LDestRect.Left := LImgX + (LDrawW - ScaledW) div 2;
          LDestRect.Top := LImgY + (LDrawH - ScaledH) div 2;
          LDestRect.Right := LDestRect.Left + ScaledW;
          LDestRect.Bottom := LDestRect.Top + ScaledH;
        end;

        if (LDestRect.Right > LDestRect.Left) and (LDestRect.Bottom > LDestRect.Top) then
        begin
          if ImageSettings.Picture.Graphic is TPNGImage then
            DrawPNGImageWithGDI(LG, ImageSettings.Picture.Graphic as TPNGImage, LDestRect, idmStretch)
          else if ImageSettings.Picture.Graphic <> nil then
            DrawNonPNGImageWithCanvas(Self.Canvas, ImageSettings.Picture.Graphic, LDestRect, idmStretch);
        end;
      end
      else
      begin
        LTextArea := Rect(LImageClipRect.Left + CaptionSettings.Margins.Left, LImageClipRect.Top + CaptionSettings.Margins.Top,
                          LImageClipRect.Right - CaptionSettings.Margins.Right, LImageClipRect.Bottom - CaptionSettings.Margins.Bottom);
      end;

      if Trim(CaptionSettings.Text) <> '' then
        LFinalCaptionToDraw := CaptionSettings.Text
      else if Trim(LPresetDefaultCaption) <> '' then
        LFinalCaptionToDraw := LPresetDefaultCaption
      else
        LFinalCaptionToDraw := '';


      if Trim(LFinalCaptionToDraw) <> '' then
      begin
        LCurrentTitleFont := TFont.Create;
        try
          LCurrentTitleFont.Assign(CaptionSettings.Font);

          if Enabled then
          begin
            if HoverSettings.Enabled and (LHoverProgress > 0) and not FProcessing then
            begin
              if HoverSettings.FontColor <> clNone then
                LCurrentTitleFont.Color := BlendColors(CaptionSettings.Font.Color, HoverSettings.FontColor, LHoverProgress)
              else if HoverSettings.HoverEffect = heFade then
                LCurrentTitleFont.Color := BlendColors(CaptionSettings.Font.Color, LighterColor(LActualFillColor, 80), LHoverProgress * 0.5);

              if HoverSettings.HoverEffect = heScale then
                LCurrentTitleFont.Size := Round(CaptionSettings.Font.Size * (1 + LHoverProgress * (1.05 - 1)));
            end;

            if FClickEffectActive and (LClickProgress > 0) and (FClickEffectDuration > 0) and not FProcessing then
            begin
              if FClickTitleColor <> clNone then
                LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickTitleColor, LClickProgress);
            end;
          end
          else if not Enabled then
          begin
            LCurrentTitleFont.Color := BlendColors(CaptionSettings.Font.Color, clGray, 0.6);
          end;

          if LTextArea.Right < LTextArea.Left then LTextArea.Right := LTextArea.Left;
          if LTextArea.Bottom < LTextArea.Top then LTextArea.Bottom := LTextArea.Top;
          LTextArea.Left   := Max(LImageClipRect.Left, LTextArea.Left);
          LTextArea.Top    := Max(LImageClipRect.Top, LTextArea.Top);
          LTextArea.Right  := Min(LImageClipRect.Right, LTextArea.Right);
          LTextArea.Bottom := Min(LImageClipRect.Bottom, LTextArea.Bottom);

          if (LTextArea.Width > 0) and (LTextArea.Height > 0) then
          begin
            DrawComponentCaption( Self.Canvas, LTextArea, LFinalCaptionToDraw, LCurrentTitleFont, LCurrentTitleFont.Color, CaptionSettings.Alignment, cvaCenter, False, 255 );
          end;
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

procedure TANDMR_CButton.KeyDown(var Key: Word; Shift: TShiftState);
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
