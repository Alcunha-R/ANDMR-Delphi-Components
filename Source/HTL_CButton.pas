unit HTL_CButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.GraphUtil, System.UITypes, HTL_ComponentUtils,
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

  TDrawingState = record
    ActualFillColor: TColor;
    ActualBorderColor: TColor;
    ActualBorderThickness: Integer;
    CurrentGradientEnabled: Boolean;
    DrawFill: Boolean;
    DrawBorder: Boolean;
    HoverProgress: Single;
    ClickProgress: Single;
    FinalCaptionToDraw: string;
  end;

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
    FTags: THTL_MultiTag;

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

    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetClickSettings(const Value: TClickSettings);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetPresetType(const Value: TPresetType);
    procedure SetTags(const Value: THTL_MultiTag);

    procedure HoverSettingsChanged(Sender: TObject);
    procedure BorderSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;

    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetDisabledCursor(const Value: TCursor);
    procedure SetTransparent(const Value: Boolean);
    function IsEnabledStored: Boolean;
    function GetTag: NativeInt;
    procedure SetTag(const Value: NativeInt);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // Métodos de desenho refatorados
    procedure CalculateDrawingState(out AState: TDrawingState);
    procedure DrawButtonBackground(AGraphics: TGPGraphics; const AState: TDrawingState; const APathRect: TGPRectF);
    procedure DrawButtonBorder(AGraphics: TGPGraphics; const AState: TDrawingState; const APathRect: TGPRectF);
    procedure DrawButtonShadow(AGraphics: TGPGraphics; var AButtonRect: TGPRectF; const AState: TDrawingState);
    procedure DrawButtonContent(AGraphics: TGPGraphics; const AState: TDrawingState; const AContentRect: TRect);
    procedure DrawProgressIndicator(AGraphics: TGPGraphics; const AContentRect: TRect);
    procedure DrawSeparator(const AImageRect, ATextRect: TRect);
    procedure DrawThemedFocusRect;

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

    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property ClickSettings: TClickSettings read FClickSettings write SetClickSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;
    property Tags: THTL_MultiTag read FTags write SetTags;

    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property DisabledCursor: TCursor read FDisabledCursor write SetDisabledCursor default crNo;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Tag: NativeInt read GetTag write SetTag default 0;
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
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  Width := 100;
  Height := 45;
  TabStop := True;
  FTransparent := False;

  FTags := THTL_MultiTag.Create;
  FTags.OnChange := SettingsChanged;

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
  if Assigned(FTags) then
  begin
    FTags.OnChange := nil;
    FTags.Free;
    FTags := nil;
  end;

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
      cptNext:     begin BaseColor := TColor($00F4A903); PresetCaption := 'Avançar';   NewTitleColor := clWhite; end;
      cptPrevious: begin BaseColor := TColor($009E9E9E); PresetCaption := 'Voltar';    NewTitleColor := clBlack; end;
      cptInfo:     begin BaseColor := TColor($00F7C34F); PresetCaption := 'Informação';NewTitleColor := clBlack; end;
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

procedure THTL_CButton.CalculateDrawingState(out AState: TDrawingState);
var
  LInitialFillColor, LInitialBorderColor, LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalClickBorderColor, LBaseStyleColor: TColor;
  LPresetDefaultCaption: string;
begin
  // --- 1. Calculate Progress States ---
  AState.HoverProgress := 0;
  if Enabled and FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and (FHoverSettings.HoverEffect <> heNone) and not FProcessing then
    AState.HoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

  AState.ClickProgress := 0;
  if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then
    AState.ClickProgress := FClickEffectProgress / 255.0;

  // --- 2. Determine Base Colors ---
  LInitialFillColor := ResolveStateColor(Enabled, False, False, FBorderSettings.BackgroundColor, clNone, clNone, BlendColors(FBorderSettings.BackgroundColor, clGray, 0.65), False, False);
  LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderSettings.Color, clNone, clNone, BlendColors(FBorderSettings.Color, clGray, 0.7), False, False);

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

  // --- 3. Blend Colors based on State ---
  AState.ActualFillColor := LInitialFillColor;
  AState.ActualBorderColor := LInitialBorderColor;

  if (AState.HoverProgress > 0) and Enabled and FHoverSettings.Enabled then
  begin
    AState.ActualFillColor := BlendColors(LInitialFillColor, LFinalHoverColor, AState.HoverProgress);
    AState.ActualBorderColor := BlendColors(LInitialBorderColor, LFinalHoverBorderColor, AState.HoverProgress);
  end;

  if (AState.ClickProgress > 0) and Enabled and FClickSettings.Enabled and (FClickSettings.Duration > 0) then
  begin
    AState.ActualFillColor := BlendColors(AState.ActualFillColor, LFinalClickColor, AState.ClickProgress);
    AState.ActualBorderColor := BlendColors(AState.ActualBorderColor, LFinalClickBorderColor, AState.ClickProgress);
  end;

  // --- 4. Apply Style-Specific Overrides ---
  AState.CurrentGradientEnabled := FGradientSettings.Enabled;
  AState.DrawFill := True;
  AState.ActualBorderThickness := FBorderSettings.Thickness;
  AState.DrawBorder := AState.ActualBorderThickness > 0;

  case FStyle of
    bsSolid: AState.CurrentGradientEnabled := False;
    bsFaded:
    begin
      LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.8);
      AState.ActualFillColor := LBaseStyleColor;
      LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 10), 0.7);
      AState.CurrentGradientEnabled := False;
      AState.ActualBorderThickness := 0;
      AState.DrawBorder := False;
    end;
    bsBordered:
    begin
      AState.DrawFill := False;
      AState.CurrentGradientEnabled := False;
      AState.ActualBorderThickness := Max(1, FBorderSettings.Thickness);
      AState.DrawBorder := AState.ActualBorderThickness > 0;
      LFinalHoverColor := ColorToARGB(IfThen(FHoverSettings.BackgroundColor=clNone, LInitialFillColor, FHoverSettings.BackgroundColor), 70);
    end;
    bsLight:
    begin
      LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
      AState.ActualFillColor := LBaseStyleColor;
      LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
      AState.CurrentGradientEnabled := False;
      AState.ActualBorderThickness := Max(1, FBorderSettings.Thickness);
      AState.DrawBorder := AState.ActualBorderThickness > 0;
    end;
    bsFlat:
    begin
      AState.CurrentGradientEnabled := False;
      AState.ActualBorderThickness := 0;
      AState.DrawBorder := False;
      LFinalHoverBorderColor := LInitialFillColor;
    end;
    bsGhost:
    begin
      AState.DrawFill := False;
      AState.CurrentGradientEnabled := False;
      AState.ActualBorderThickness := Max(1, FBorderSettings.Thickness);
      AState.ActualBorderColor := LInitialFillColor;
      AState.DrawBorder := AState.ActualBorderThickness > 0;
      LFinalHoverColor := ColorToARGB(IfThen(FHoverSettings.BackgroundColor=clNone, LInitialFillColor, FHoverSettings.BackgroundColor), 100);
      LFinalHoverBorderColor := LInitialFillColor;
    end;
    bsShadow:
    begin
      if FTransparent then
      begin
        AState.DrawFill := False;
        AState.CurrentGradientEnabled := False;
      end;
    end;
    bsGradient:
    begin
      AState.CurrentGradientEnabled := True;
      AState.DrawFill := True;
      AState.DrawBorder := AState.ActualBorderThickness > 0;
    end;
    bsDark:
    begin
      LBaseStyleColor := DarkerColor(FBorderSettings.BackgroundColor, 60);
      if (GetRValue(LBaseStyleColor) < 30) and (GetGValue(LBaseStyleColor) < 30) and (GetBValue(LBaseStyleColor) < 30) then
          LBaseStyleColor := TColor($FF383838);
      AState.ActualFillColor := LBaseStyleColor;
      AState.ActualBorderColor := LighterColor(LBaseStyleColor, 20);
      AState.CurrentGradientEnabled := False;
      AState.DrawFill := True;
      AState.DrawBorder := True;
      AState.ActualBorderThickness := Max(1, FBorderSettings.Thickness);
      if FHoverSettings.BackgroundColor <> clNone then
        LFinalHoverColor := FHoverSettings.BackgroundColor
      else
        LFinalHoverColor := LighterColor(LBaseStyleColor, 15);
      if FHoverSettings.BorderColor <> clNone then
        LFinalHoverBorderColor := FHoverSettings.BorderColor
      else
        LFinalHoverBorderColor := LighterColor(AState.ActualBorderColor, 15);
      LFinalClickColor := IfThen(FClickSettings.Color = clNone, LighterColor(LBaseStyleColor, 10), FClickSettings.Color);
      LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, LighterColor(AState.ActualBorderColor, 10), FClickSettings.BorderColor);
    end;
    bsMaterial:
    begin
      AState.ActualFillColor := FBorderSettings.BackgroundColor;
      AState.ActualBorderColor := clNone;
      AState.ActualBorderThickness := 0;
      AState.DrawBorder := False;
      AState.CurrentGradientEnabled := False;
      AState.DrawFill := True;
      if FHoverSettings.BackgroundColor <> clNone then
        LFinalHoverColor := FHoverSettings.BackgroundColor
      else
        LFinalHoverColor := LighterColor(AState.ActualFillColor, 10);
      LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(AState.ActualFillColor, 10), FClickSettings.Color);
    end;
    bsModern:
    begin
      AState.ActualFillColor := FBorderSettings.BackgroundColor;
      AState.ActualBorderColor := DarkerColor(AState.ActualFillColor, 15);
      AState.ActualBorderThickness := 1;
      AState.DrawBorder := True;
      AState.CurrentGradientEnabled := False;
      AState.DrawFill := True;
      if FHoverSettings.BackgroundColor <> clNone then
        LFinalHoverColor := FHoverSettings.BackgroundColor
      else
        LFinalHoverColor := LighterColor(AState.ActualFillColor, 8);
      if FHoverSettings.BorderColor <> clNone then
        LFinalHoverBorderColor := FHoverSettings.BorderColor
      else
        LFinalHoverBorderColor := AState.ActualFillColor;
      LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(AState.ActualFillColor, 8), FClickSettings.Color);
      LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(AState.ActualFillColor, 20), FClickSettings.BorderColor);
    end;
    bsWindows:
    begin
      AState.ActualFillColor := TColor($FFEFEFEF);
      AState.ActualBorderColor := TColor($FFDCDCDC);
      AState.ActualBorderThickness := 1;
      AState.DrawBorder := True;
      AState.CurrentGradientEnabled := False;
      AState.DrawFill := True;
      if FHoverSettings.BackgroundColor <> clNone then
        LFinalHoverColor := FHoverSettings.BackgroundColor
      else
        LFinalHoverColor := TColor($FFF5F5F5);
      if FHoverSettings.BorderColor <> clNone then
        LFinalHoverBorderColor := FHoverSettings.BorderColor
      else
        LFinalHoverBorderColor := FBorderSettings.BackgroundColor;
      LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(AState.ActualFillColor, 10), FClickSettings.Color);
      LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(AState.ActualBorderColor, 10), FClickSettings.BorderColor);
    end;
    bsMacOS:
    begin
      AState.ActualFillColor := TColor($FFF2F2F7);
      AState.ActualBorderColor := TColor($FFD1D1D6);
      AState.ActualBorderThickness := 1;
      AState.DrawBorder := True;
      AState.CurrentGradientEnabled := False;
      AState.DrawFill := True;
      if FHoverSettings.BackgroundColor <> clNone then
        LFinalHoverColor := FHoverSettings.BackgroundColor
      else
        LFinalHoverColor := DarkerColor(AState.ActualFillColor, 5);
      if FHoverSettings.BorderColor <> clNone then
        LFinalHoverBorderColor := FHoverSettings.BorderColor
      else
        LFinalHoverBorderColor := DarkerColor(AState.ActualBorderColor, 5);
      LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(AState.ActualFillColor, 12), FClickSettings.Color);
      LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(AState.ActualBorderColor, 12), FClickSettings.BorderColor);
    end;
  end;

  // --- 5. Final Overrides ---
  if FTransparent then
  begin
    AState.DrawFill := False;
    AState.CurrentGradientEnabled := False;
  end;
  AState.DrawBorder := AState.ActualBorderThickness > 0;

  // --- 6. Calculate Final Caption ---
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

  if Trim(Self.CaptionSettings.Text) <> '' then
    AState.FinalCaptionToDraw := Self.CaptionSettings.Text
  else if Trim(LPresetDefaultCaption) <> '' then
    AState.FinalCaptionToDraw := LPresetDefaultCaption
  else
    AState.FinalCaptionToDraw := '';
end;

procedure THTL_CButton.DrawButtonShadow(AGraphics: TGPGraphics; var AButtonRect: TGPRectF; const AState: TDrawingState);
const
  SHADOW_ALPHA = 50;
  SHADOW_OFFSET_X_CONST = 1;
  SHADOW_OFFSET_Y_CONST = 2;
  SHADOW_ALPHA_HOVER = 80;
  SHADOW_OFFSET_X_HOVER_FACTOR = 1.5;
  SHADOW_OFFSET_Y_HOVER_FACTOR = 1.5;
  MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR = 1.8;
  MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR = 1.8;
  MATERIAL_SHADOW_ALPHA_HOVER = 90;
var
  LShadowOffsetX, LShadowOffsetY: Single;
  LShadowAlpha: Byte;
  LShadowPathRect, LPathRect: TGPRectF;
  LPathInset, LRadiusValue: Single;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPBrush;
begin
  if FTransparent or not (FStyle in [bsShadow, bsMaterial]) then
    Exit;

  // --- Calculate Shadow Properties ---
  if FStyle = bsShadow then
  begin
    LShadowOffsetX := SHADOW_OFFSET_X_CONST;
    LShadowOffsetY := SHADOW_OFFSET_Y_CONST;
    LShadowAlpha := SHADOW_ALPHA;
    if (AState.HoverProgress > 0) and Enabled and FHoverSettings.Enabled then
    begin
      LShadowOffsetX := SHADOW_OFFSET_X_CONST + ((SHADOW_OFFSET_X_CONST * SHADOW_OFFSET_X_HOVER_FACTOR) - SHADOW_OFFSET_X_CONST) * AState.HoverProgress;
      LShadowOffsetY := SHADOW_OFFSET_Y_CONST + ((SHADOW_OFFSET_Y_CONST * SHADOW_OFFSET_Y_HOVER_FACTOR) - SHADOW_OFFSET_Y_CONST) * AState.HoverProgress;
      LShadowAlpha := Round(SHADOW_ALPHA + (SHADOW_ALPHA_HOVER - SHADOW_ALPHA) * AState.HoverProgress);
    end;
  end else // bsMaterial
  begin
    LShadowOffsetX := 1;
    LShadowOffsetY := 2;
    LShadowAlpha := 60;
    if (AState.HoverProgress > 0) and Enabled and FHoverSettings.Enabled then
    begin
      LShadowOffsetX := 1 + ((1 * MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR) - 1) * AState.HoverProgress;
      LShadowOffsetY := 2 + ((2 * MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR) - 2) * AState.HoverProgress;
      LShadowAlpha := Round(60 + (MATERIAL_SHADOW_ALPHA_HOVER - 60) * AState.HoverProgress);
    end;
  end;

  // --- Adjust Button Rect for Shadow ---
  var TempX, TempY, TempW, TempH: Single;
  TempX := IfThen(LShadowOffsetX < 0, Abs(LShadowOffsetX), 0.0) + AButtonRect.X;
  TempY := IfThen(LShadowOffsetY < 0, Abs(LShadowOffsetY), 0.0) + AButtonRect.Y;
  TempW := AButtonRect.Width - Abs(LShadowOffsetX);
  TempH := AButtonRect.Height - Abs(LShadowOffsetY);
  AButtonRect := MakeRect(TempX, TempY, Max(0, TempW), Max(0, TempH));

  // --- Prepare Path for Shadow ---
  if AState.ActualBorderThickness > 0 then LPathInset := AState.ActualBorderThickness / 2.0 else LPathInset := 0.0;
  LPathRect.X := AButtonRect.X + LPathInset;
  LPathRect.Y := AButtonRect.Y + LPathInset;
  LPathRect.Width := Max(0, AButtonRect.Width - 2 * LPathInset);
  LPathRect.Height := Max(0, AButtonRect.Height - 2 * LPathInset);

  LShadowPathRect := MakeRect(LPathRect.X + LShadowOffsetX, LPathRect.Y + LShadowOffsetY, LPathRect.Width, LPathRect.Height);
  LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LShadowPathRect.Width, LShadowPathRect.Height) / 2.0);
  LRadiusValue := Max(0, LRadiusValue);

  // --- Draw Shadow ---
  LGPPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LGPPath, LShadowPathRect, LRadiusValue, FBorderSettings.RoundCornerType);
    if LGPPath.GetPointCount > 0 then
    begin
      LGPBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, LShadowAlpha));
      try
        AGraphics.FillPath(LGPBrush, LGPPath);
      finally
        LGPBrush.Free;
      end;
    end;
  finally
    LGPPath.Free;
  end;
end;

procedure THTL_CButton.DrawButtonBackground(AGraphics: TGPGraphics; const AState: TDrawingState; const APathRect: TGPRectF);
var
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPBrush;
  LRadiusValue: Single;
begin
  if not AState.DrawFill or FTransparent then
    Exit;

  LRadiusValue := Min(FBorderSettings.CornerRadius, Min(APathRect.Width, APathRect.Height) / 2.0);
  LRadiusValue := Max(0, LRadiusValue);

  LGPPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LGPPath, APathRect, LRadiusValue, FBorderSettings.RoundCornerType);
    if LGPPath.GetPointCount > 0 then
    begin
      if AState.CurrentGradientEnabled and (FGradientSettings.StartColor <> clNone) and (FGradientSettings.EndColor <> clNone) then
      begin
        var gradientRect: TGPRectF;
        LGPPath.GetBounds(gradientRect, nil, nil);
        case FGradientSettings.GradientType of
          gtLinearHorizontal: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeHorizontal);
          gtDiagonalDown: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeForwardDiagonal);
          gtDiagonalUp: LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeBackwardDiagonal);
          gtRadial, gtCenterBurst:
          begin
            var pathGradBrush := TGPPathGradientBrush.Create(LGPPath);
            var surroundColor := ColorToARGB(FGradientSettings.EndColor, 255);
            var surroundCount := 1;
            pathGradBrush.SetCenterColor(ColorToARGB(FGradientSettings.StartColor, 255));
            pathGradBrush.SetSurroundColors(@surroundColor, surroundCount);
            if FGradientSettings.GradientType = gtCenterBurst then pathGradBrush.SetFocusScales(0.0, 0.0);
            LGPBrush := pathGradBrush;
          end;
        else
          LGPBrush := TGPLinearGradientBrush.Create(gradientRect, ColorToARGB(FGradientSettings.StartColor, 255), ColorToARGB(FGradientSettings.EndColor, 255), LinearGradientModeVertical);
        end;
      end else
      begin
        LGPBrush := TGPSolidBrush.Create(ColorToARGB(AState.ActualFillColor, 255));
      end;

      try
        AGraphics.FillPath(LGPBrush, LGPPath);
      finally
        LGPBrush.Free;
      end;
    end;
  finally
    LGPPath.Free;
  end;
end;

procedure THTL_CButton.DrawButtonBorder(AGraphics: TGPGraphics; const AState: TDrawingState; const APathRect: TGPRectF);
var
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LRadiusValue: Single;
begin
  if not AState.DrawBorder or (FBorderSettings.Style = psClear) then
    Exit;

  LRadiusValue := Min(FBorderSettings.CornerRadius, Min(APathRect.Width, APathRect.Height) / 2.0);
  LRadiusValue := Max(0, LRadiusValue);

  LGPPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LGPPath, APathRect, LRadiusValue, FBorderSettings.RoundCornerType);
    if LGPPath.GetPointCount > 0 then
    begin
      LGPPen := TGPPen.Create(ColorToARGB(AState.ActualBorderColor, 255), AState.ActualBorderThickness);
      try
        case FBorderSettings.Style of
          psDash: LGPPen.SetDashStyle(DashStyleDash);
          psDot: LGPPen.SetDashStyle(DashStyleDot);
          psDashDot: LGPPen.SetDashStyle(DashStyleDashDot);
          psDashDotDot: LGPPen.SetDashStyle(DashStyleDashDotDot);
        else
          LGPPen.SetDashStyle(DashStyleSolid);
        end;
        AGraphics.DrawPath(LGPPen, LGPPath);
      finally
        LGPPen.Free;
      end;
    end;
  finally
    LGPPath.Free;
  end;
end;

procedure THTL_CButton.DrawButtonContent(AGraphics: TGPGraphics; const AState: TDrawingState; const AContentRect: TRect);
var
  imageSlotRect, textSlotRect, imageFinalDestRect: TRect;
  imageAndMarginsW, imageAndMarginsH: Integer;
  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY, AvailableWidth, AvailableHeight: Integer;
  LCurrentTitleFont: TFont;
  LScaleFactor: Single;
begin
  // --- 1. Calculate Layout: Image and Text Slots ---
  if FImageSettings.Placement = iplInsideBounds then
  begin
    textSlotRect := AContentRect;
    imageSlotRect := AContentRect;
  end
  else
  begin
    textSlotRect := AContentRect;
    // For iplOutsideBounds, the ImageDrawingArea is calculated in Paint method
    // and this procedure is called with the remaining ButtonDrawingRect.
    // We assume AContentRect is the text area.
    imageSlotRect := System.Types.Rect(0,0,0,0); // Placeholder, drawing is handled outside
  end;

  LImgW := 0; LImgH := 0; LDrawW := 0; LDrawH := 0;

  if (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and FImageSettings.Visible then
  begin
    AvailableWidth := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
    AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);
    LImgW := FImageSettings.Picture.Width;
    LImgH := FImageSettings.Picture.Height;

    // Calculate image draw size (LDrawW, LDrawH)
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

  // Partition space between image and text for iplInsideBounds
  if (LDrawW > 0) and (FImageSettings.Placement = iplInsideBounds) then
  begin
    imageAndMarginsW := LDrawW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
    imageAndMarginsH := LDrawH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;

    var SeparatorSpace: Integer := 0;
    if FSeparatorSettings.Visible then
      SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness;

    case FImageSettings.ImagePosition of
      ipLeft: begin imageSlotRect.Width := imageAndMarginsW; textSlotRect.Left := imageSlotRect.Right + SeparatorSpace; end;
      ipRight: begin imageSlotRect.Left := imageSlotRect.Right - imageAndMarginsW; textSlotRect.Right := imageSlotRect.Left - SeparatorSpace; end;
      ipTop: begin imageSlotRect.Height := imageAndMarginsH; textSlotRect.Top := imageSlotRect.Bottom + SeparatorSpace; end;
      ipBottom: begin imageSlotRect.Top := imageSlotRect.Bottom - imageAndMarginsH; textSlotRect.Bottom := imageSlotRect.Top - SeparatorSpace; end;
    end;
  end;

  // --- 2. Draw Image ---
  if (LDrawW > 0) then
  begin
    AvailableWidth := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
    AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);

    case FImageSettings.HorizontalAlign of
      ihaLeft: LImgX := imageSlotRect.Left + FImageSettings.Margins.Left;
      ihaCenter: LImgX := imageSlotRect.Left + FImageSettings.Margins.Left + (AvailableWidth - LDrawW) div 2;
      ihaRight: LImgX := imageSlotRect.Right - FImageSettings.Margins.Right - LDrawW;
    else LImgX := imageSlotRect.Left + FImageSettings.Margins.Left;
    end;

    case FImageSettings.VerticalAlign of
      ivaTop: LImgY := imageSlotRect.Top + FImageSettings.Margins.Top;
      ivaCenter: LImgY := imageSlotRect.Top + FImageSettings.Margins.Top + (AvailableHeight - LDrawH) div 2;
      ivaBottom: LImgY := imageSlotRect.Bottom - FImageSettings.Margins.Bottom - LDrawH;
    else LImgY := imageSlotRect.Top + FImageSettings.Margins.Top;
    end;

    imageFinalDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

    if Enabled and FHoverSettings.Enabled and (FHoverSettings.HoverEffect = heScale) and (AState.HoverProgress > 0) then
    begin
      LScaleFactor := 1 + (AState.HoverProgress * (1.05 - 1));
      var ScaledW := Round(LDrawW * LScaleFactor);
      var ScaledH := Round(LDrawH * LScaleFactor);
      imageFinalDestRect.Left := LImgX + (LDrawW - ScaledW) div 2;
      imageFinalDestRect.Top := LImgY + (LDrawH - ScaledH) div 2;
      imageFinalDestRect.Right := imageFinalDestRect.Left + ScaledW;
      imageFinalDestRect.Bottom := imageFinalDestRect.Top + ScaledH;
    end;

    if (imageFinalDestRect.Right > imageFinalDestRect.Left) and (imageFinalDestRect.Bottom > imageFinalDestRect.Top) then
    begin
      if FImageSettings.Picture.Graphic is TPNGImage then
        DrawPNGImageWithGDI(AGraphics, FImageSettings.Picture.Graphic as TPNGImage, imageFinalDestRect, idmStretch)
      else if FImageSettings.Picture.Graphic <> nil then
        DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, imageFinalDestRect, idmStretch);
    end;
  end;

  // --- 3. Draw Caption ---
  if Trim(AState.FinalCaptionToDraw) <> '' then
  begin
    LCurrentTitleFont := TFont.Create;
    try
      LCurrentTitleFont.Assign(FCaptionSettings.Font);
      if Enabled then
      begin
        if FHoverSettings.Enabled and (AState.HoverProgress > 0) and not FProcessing then
        begin
          if FHoverSettings.FontColor <> clNone then LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, FHoverSettings.FontColor, AState.HoverProgress)
          else if FHoverSettings.HoverEffect = heFade then LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, LighterColor(AState.ActualFillColor, 80), AState.HoverProgress * 0.5);
          if FHoverSettings.HoverEffect = heScale then LCurrentTitleFont.Size := Round(FCaptionSettings.Font.Size * (1 + AState.HoverProgress * (1.05 - 1)));
        end;
        if FClickEffectActive and (AState.ClickProgress > 0) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then
        begin
          if FClickSettings.FontColor <> clNone then
            LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickSettings.FontColor, AState.ClickProgress);
        end;
      end
      else if not Enabled then
      begin
        LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, clGray, 0.6);
      end;

      textSlotRect.Left := textSlotRect.Left + FCaptionSettings.Margins.Left;
      textSlotRect.Top := textSlotRect.Top + FCaptionSettings.Margins.Top;
      textSlotRect.Right := textSlotRect.Right - FCaptionSettings.Margins.Right;
      textSlotRect.Bottom := textSlotRect.Bottom - FCaptionSettings.Margins.Bottom;

      if (textSlotRect.Width > 0) and (textSlotRect.Height > 0) then
        DrawComponentCaption(Self.Canvas, textSlotRect, AState.FinalCaptionToDraw, LCurrentTitleFont, LCurrentTitleFont.Color, FCaptionSettings.Alignment, cvaCenter, False, 255);
    finally
      LCurrentTitleFont.Free;
    end;
  end;
end;

procedure THTL_CButton.DrawProgressIndicator(AGraphics: TGPGraphics; const AContentRect: TRect);
var
  LProgressRect, OriginalProgressRect, TextRect: TRect;
  LArcThickness, InnerBarWidth, InnerBarX: Integer;
  LStartAngle, LSweepAngle: Single;
  LProgressBarPen: TGPPen;
  LProgressPath: TGPGraphicsPath;
  ArcRectF: TGPRectF;
  LGPBrush: TGPBrush;
  DotYOffset: array[0..2] of Integer;
  ProgressCaptionFont: TFont;
begin
  if not (FProcessing and FProgressSettings.ShowProgress) then
    Exit;

  LProgressRect := AContentRect;
  if FBorderSettings.Thickness > 0 then
    InflateRect(LProgressRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);
  OriginalProgressRect := LProgressRect;

  // Adjust rects if showing progress text
  if FProgressSettings.ShowProgressText and (FProgressSettings.ProgressText <> '') then
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

  // Center animation rect for circular styles
  if FProgressSettings.AnimationStyle in [pasRotatingSemiCircle, pasFullCircularSpinner, pasBouncingDots] then
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

  // Draw animation
  case FProgressSettings.AnimationStyle of
    pasRotatingSemiCircle, pasFullCircularSpinner:
    begin
      if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
      begin
        LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
        if FProgressSettings.AnimationStyle = pasRotatingSemiCircle then
        begin
          LStartAngle := (FProgressStep * 10) mod 360;
          LSweepAngle := 270;
        end else
        begin
          LStartAngle := (FProgressStep * 12) mod 360;
          LSweepAngle := 90;
        end;

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
              AGraphics.DrawPath(LProgressBarPen, LProgressPath);
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
      var BarRect := OriginalProgressRect;
      if FProgressSettings.ShowProgressText and (FProgressSettings.ProgressText <> '') and (OriginalProgressRect.Width > 100) then
        BarRect := LProgressRect;

      InflateRect(BarRect, 0, -BarRect.Height div 3);
      if BarRect.Height < 4 then BarRect.Height := Max(2, Min(LProgressRect.Height, 4));

      if BarRect.Width > 10 then
      begin
        LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 100));
        try AGraphics.FillRectangle(LGPBrush, BarRect.Left, BarRect.Top, BarRect.Width, BarRect.Height); finally LGPBrush.Free; end;

        InnerBarWidth := BarRect.Width div 3;
        if (BarRect.Width + InnerBarWidth) > 0 then InnerBarX := (FProgressStep * 5) mod (BarRect.Width + InnerBarWidth) else InnerBarX := 0;

        LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
        try AGraphics.FillRectangle(LGPBrush, BarRect.Left + InnerBarX - InnerBarWidth, BarRect.Top, InnerBarWidth, BarRect.Height); finally LGPBrush.Free; end;
      end;
    end;
    pasBouncingDots:
    begin
      const DotCount = 3;
      var DotSize, DotSpacing, TotalDotWidth, StartX, BaseY, i: Integer;
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
          DotYOffset[i] := Round((DotSize / 2) * Sin((FProgressStep * 0.2 + i * (PI / DotCount))));
        LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
        try
          for i := 0 to DotCount - 1 do
            AGraphics.FillEllipse(LGPBrush, StartX + i * (DotSize + DotSpacing), BaseY + DotYOffset[i], DotSize, DotSize);
        finally
          LGPBrush.Free;
        end;
      end;
    end;
  end;

  // Draw progress text
  if FProgressSettings.ShowProgressText and (FProgressSettings.ProgressText <> '') then
  begin
    if (OriginalProgressRect.Width > 100) and (FProgressSettings.AnimationStyle <> pasHorizontalBar) then
    begin
      TextRect.Left := (LProgressRect.Left + LProgressRect.Width) + Self.FCaptionSettings.Margins.Left;
      TextRect.Top := OriginalProgressRect.Top;
      TextRect.Right := OriginalProgressRect.Left + OriginalProgressRect.Width - Self.FCaptionSettings.Margins.Right;
      TextRect.Bottom := OriginalProgressRect.Bottom;
    end
    else if (FProgressSettings.AnimationStyle = pasHorizontalBar) and (OriginalProgressRect.Width > 100) then
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
        DrawComponentCaption(Self.Canvas, TextRect, FProgressSettings.ProgressText, ProgressCaptionFont, ProgressCaptionFont.Color, taCenter, cvaCenter, False, 255);
      finally
        ProgressCaptionFont.Free;
      end;
    end;
  end;
end;

procedure THTL_CButton.DrawSeparator(const AImageRect, ATextRect: TRect);
var
  SepRect: TRect;
  isVertical: Boolean;
begin
  if not (FSeparatorSettings.Visible and FImageSettings.Visible and
         (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and
         (FImageSettings.Placement in [iplInsideBounds, iplOutsideBounds]) and
         (FImageSettings.ImagePosition in [ipLeft, ipRight, ipTop, ipBottom])) then
    Exit;

  isVertical := FImageSettings.ImagePosition in [ipLeft, ipRight];
  var refRect := AImageRect;

  case FImageSettings.ImagePosition of
    ipLeft: SepRect := System.Types.Rect(refRect.Right + FSeparatorSettings.Padding, refRect.Top, refRect.Right + FSeparatorSettings.Padding + FSeparatorSettings.Thickness, refRect.Bottom);
    ipRight: SepRect := System.Types.Rect(refRect.Left - FSeparatorSettings.Padding - FSeparatorSettings.Thickness, refRect.Top, refRect.Left - FSeparatorSettings.Padding, refRect.Bottom);
    ipTop: SepRect := System.Types.Rect(refRect.Left, refRect.Bottom + FSeparatorSettings.Padding, refRect.Right, refRect.Bottom + FSeparatorSettings.Padding + FSeparatorSettings.Thickness);
    ipBottom: SepRect := System.Types.Rect(refRect.Left, refRect.Top - FSeparatorSettings.Padding - FSeparatorSettings.Thickness, refRect.Right, refRect.Top - FSeparatorSettings.Padding);
  end;

  case FSeparatorSettings.HeightMode of
    shmCustom:
      if isVertical then
      begin
        var currentHeight := SepRect.Height;
        var customH := Min(currentHeight, FSeparatorSettings.CustomHeight);
        SepRect.Top := SepRect.Top + (currentHeight - customH) div 2;
        SepRect.Bottom := SepRect.Top + customH;
      end
      else
      begin
        var currentWidth := SepRect.Width;
        var customW := Min(currentWidth, FSeparatorSettings.CustomHeight);
        SepRect.Left := SepRect.Left + (currentWidth - customW) div 2;
        SepRect.Right := SepRect.Left + customW;
      end;
  end;

  if (SepRect.Width > 0) and (SepRect.Height > 0) then
  begin
    Self.Canvas.Brush.Color := FSeparatorSettings.Color;
    Self.Canvas.Brush.Style := TBrushStyle.bsSolid;
    Self.Canvas.FillRect(SepRect);
  end;
end;

procedure THTL_CButton.DrawThemedFocusRect;
begin
  if Focused and TabStop and Enabled then
    DrawFocusRect(Canvas.Handle, ClientRect);
end;

procedure THTL_CButton.Paint;
var
  LG: TGPGraphics;
  DrawingState: TDrawingState;
  ButtonRect, ContentRect, ImageDrawingArea: TRect;
  ButtonRectF, PathRectF: TGPRectF;
  PathInset: Single;
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // 1. Calculate the current state (colors, progress, etc.)
    CalculateDrawingState(DrawingState);

    // 2. Define drawing areas
    ButtonRect := ClientRect;
    ImageDrawingArea := System.Types.Rect(0,0,0,0);

    if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and (FImageSettings.Placement = iplOutsideBounds) then
    begin
      var ImageSlotW, ImageSlotH, SeparatorSpace: Integer;
      if not FImageSettings.AutoSize and (FImageSettings.TargetWidth > 0) then ImageSlotW := FImageSettings.TargetWidth else ImageSlotW := FImageSettings.Picture.Width;
      if not FImageSettings.AutoSize and (FImageSettings.TargetHeight > 0) then ImageSlotH := FImageSettings.TargetHeight else ImageSlotH := FImageSettings.Picture.Height;
      ImageSlotW := ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
      ImageSlotH := ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;
      SeparatorSpace := IfThen(FSeparatorSettings.Visible, (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness, 0);

      case FImageSettings.ImagePosition of
        ipLeft: begin ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Top, ClientRect.Left + ImageSlotW, ClientRect.Bottom); ButtonRect.Left := ImageDrawingArea.Right + SeparatorSpace; end;
        ipRight: begin ImageDrawingArea := System.Types.Rect(ClientRect.Right - ImageSlotW, ClientRect.Top, ClientRect.Right, ClientRect.Bottom); ButtonRect.Right := ImageDrawingArea.Left - SeparatorSpace; end;
        ipTop: begin ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + ImageSlotH); ButtonRect.Top := ImageDrawingArea.Bottom + SeparatorSpace; end;
        ipBottom: begin ImageDrawingArea := System.Types.Rect(ClientRect.Left, ClientRect.Bottom - ImageSlotH, ClientRect.Right, ClientRect.Bottom); ButtonRect.Bottom := ImageDrawingArea.Top - SeparatorSpace; end;
      end;
      if ButtonRect.Right < ButtonRect.Left then ButtonRect.Right := ButtonRect.Left;
      if ButtonRect.Bottom < ButtonRect.Top then ButtonRect.Bottom := ButtonRect.Top;
    end;

    ButtonRectF.X :=ButtonRect.Left;
    ButtonRectF.Y := ButtonRect.Top;
    ButtonRectF.Width := ButtonRect.Width;
    ButtonRectF.Height := Height;

    // 3. Draw Shadow (and adjust ButtonRectF accordingly)
    DrawButtonShadow(LG, ButtonRectF, DrawingState);

    // 4. Prepare main path
    if DrawingState.DrawBorder and (DrawingState.ActualBorderThickness > 0) then
    begin
      if ButtonRectF.Width > 0 then ButtonRectF.Width := Max(0, ButtonRectF.Width - 0.5);
      if ButtonRectF.Height > 0 then ButtonRectF.Height := Max(0, ButtonRectF.Height - 0.5);
      PathInset := DrawingState.ActualBorderThickness / 2.0;
    end else
      PathInset := 0.0;

    PathRectF := MakeRect(ButtonRectF.X + PathInset, ButtonRectF.Y + PathInset, ButtonRectF.Width - 2 * PathInset, ButtonRectF.Height - 2 * PathInset);

    // 5. Draw Background and Border
    DrawButtonBackground(LG, DrawingState, PathRectF);
    DrawButtonBorder(LG, DrawingState, PathRectF);

    // 6. Define content area
    ContentRect := Rect(Round(ButtonRectF.X), Round(ButtonRectF.Y), Round(ButtonRectF.X + ButtonRectF.Width), Round(ButtonRectF.Y + ButtonRectF.Height));
    if DrawingState.DrawBorder and (DrawingState.ActualBorderThickness > 0) then
      InflateRect(ContentRect, -Round(DrawingState.ActualBorderThickness), -Round(DrawingState.ActualBorderThickness));

    // 7. Draw Content (Progress or Normal)
    if FProcessing and FProgressSettings.ShowProgress then
    begin
      DrawProgressIndicator(LG, ContentRect);
    end
    else
    begin
      var textRect := ContentRect;
      var imageRect := ImageDrawingArea;
      if FImageSettings.Placement = iplInsideBounds then
        imageRect := ContentRect;

      DrawButtonContent(LG, DrawingState, textRect);
      DrawSeparator(imageRect, textRect);
    end;

  finally
    LG.Free;
  end;

  // 8. Draw Focus Rect
  DrawThemedFocusRect;
end;

end.