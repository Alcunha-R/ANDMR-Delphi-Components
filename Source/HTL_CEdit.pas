unit HTL_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.Types,
  System.Character,
  HTL_ComponentUtils;

type
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);

  THTL_CEdit = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings;
    FFocusSettings: TFocusSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FTextMargins: THTL_Margins;
    FGradientSettings: TGradientSettings;
    FClickSettings: TClickSettings;
    FProgressSettings: TProgressSettings;
    FImageSettings: TImageSettings;
    FSeparatorSettings: TSeparatorSettings;

    FText: string;
    FPlaceholder: string;
    FPlaceholderFont: TFont;
    FMaxLength: Integer;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FCaretVisible: Boolean;
    FCaretPosition: Integer;
    FCaretTimer: TTimer;
    FOpacity: Byte;
    FCurrentCursor: TCursor;
    FInputType: TInputType;
    FTextCase: TTextCase;
    FInputMask: string;
    FPredefinedMask: TPredefinedMaskType;
    FHovered: Boolean;
    FStatus: TCEditStatus;
    FStyle: TButtonStyle;
    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectActive: Boolean;
    FProcessing: Boolean;
    FProgressTimer: TTimer;
    FProgressStep: Integer;
    FOriginalText: string;
    FOriginalEnabledState: Boolean;

    FOnExit: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;

    procedure ApplyFormatting;

    procedure SetText(const Value: string);
    procedure SetPlaceholder(const Value: string);
    procedure SetPlaceholderFont(const Value: TFont);
    procedure SetMaxLength(const Value: Integer);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetFocusSettings(const Value: TFocusSettings);
    procedure SetOpacity(const Value: Byte);
    procedure SetCurrentCursor(const Value: TCursor);
    procedure SetInputType(const Value: TInputType);
    procedure SetTextCase(const Value: TTextCase);
    procedure SetInputMask(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetTextMargins(const Value: THTL_Margins);
    procedure SetPredefinedMask(const Value: TPredefinedMaskType);
    procedure SetStatus(const Value: TCEditStatus);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetClickSettings(const Value: TClickSettings);
    procedure SetProgressSettings(const Value: TProgressSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetImageSettings(const Value: TImageSettings);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);

    procedure CaretTimerTick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure TextMarginsChanged(Sender: TObject);
    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;
    procedure ProgressTimerHandler(Sender: TObject);
    procedure ImageSettingsChanged(Sender: TObject);
    procedure PlaceholderFontChanged(Sender: TObject);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartProcessing;
    procedure StopProcessing;

  published
    property Text: string read FText write SetText;
    property Placeholder: string read FPlaceholder write SetPlaceholder;
    property PlaceholderFont: TFont read FPlaceholderFont write SetPlaceholderFont;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;

    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property FocusSettings: TFocusSettings read FFocusSettings write SetFocusSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: THTL_Margins read FTextMargins write SetTextMargins;
    property Status: TCEditStatus read FStatus write SetStatus default cepsNormal;
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property ClickSettings: TClickSettings read FClickSettings write SetClickSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;

    property InputType: TInputType read FInputType write SetInputType default itNormal;
    property TextCase: TTextCase read FTextCase write SetTextCase default tcNormal;
    property InputMask: string read FInputMask write SetInputMask;
    property PredefinedMask: TPredefinedMaskType read FPredefinedMask write SetPredefinedMask default pmtNone;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property CurrentCursor: TCursor read FCurrentCursor write SetCurrentCursor default crIBeam;

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
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CEdit]);
end;

{ THTL_CEdit }

constructor THTL_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  DoubleBuffered := True;
  Width := 180; Height := 40; TabStop := True;
  FText := ''; FMaxLength := 0; FPasswordChar := #0; FReadOnly := False;

  FPlaceholder := '';
  FPlaceholderFont := TFont.Create;
  FPlaceholderFont.Assign(Self.Font);
  FPlaceholderFont.Color := clGray;
  FPlaceholderFont.Style := [fsItalic];
  FPlaceholderFont.OnChange := PlaceholderFontChanged;

  FBorderSettings := TBorderSettings.Create; FBorderSettings.OnChange := SettingsChanged; FBorderSettings.CornerRadius := 4; FBorderSettings.RoundCornerType := rctAll; FBorderSettings.Color := clSilver; FBorderSettings.Thickness := 1;
  FFocusSettings := TFocusSettings.Create; FFocusSettings.OnChange := SettingsChanged;
  FCaptionSettings := TCaptionSettings.Create(Self); FCaptionSettings.OnChange := CaptionSettingsChanged; FCaptionSettings.Font.Style := [fsBold]; FCaptionSettings.Font.Color := clGrayText;
  FHoverSettings := THoverSettings.Create(Self); FHoverSettings.OnChange := HoverSettingsChanged;
  FTextMargins := THTL_Margins.Create; FTextMargins.OnChange := TextMarginsChanged; FTextMargins.Left := 8; FTextMargins.Right := 8;
  FGradientSettings := TGradientSettings.Create; FGradientSettings.OnChange := SettingsChanged;
  FClickEffectProgress := 0; FClickEffectActive := False; FClickEffectTimer := TTimer.Create(Self); FClickEffectTimer.Enabled := False; FClickEffectTimer.OnTimer := ClickEffectTimerHandler;
  FClickSettings := TClickSettings.Create; FClickSettings.OnChange := SettingsChanged; FClickSettings.Duration := 200; UpdateClickEffectTimerInterval;
  FStyle := bsSolid;
  FProgressSettings := TProgressSettings.Create(Self); FProgressSettings.OnChange := SettingsChanged;
  FProcessing := False; FProgressTimer := TTimer.Create(Self); FProgressTimer.Enabled := False; FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval; FProgressTimer.OnTimer := ProgressTimerHandler; FOriginalEnabledState := True;
  FImageSettings := TImageSettings.Create(Self); FImageSettings.OnChange := ImageSettingsChanged;
  FSeparatorSettings := TSeparatorSettings.Create; FSeparatorSettings.OnChange := SettingsChanged; FSeparatorSettings.Visible := False;
  FCaretVisible := False; FCaretPosition := 0; FCaretTimer := TTimer.Create(Self); FCaretTimer.Interval := GetCaretBlinkTime; FCaretTimer.OnTimer := CaretTimerTick; FCaretTimer.Enabled := False;
  FOpacity := 255; FCurrentCursor := crIBeam; Self.Cursor := FCurrentCursor; FInputType := itNormal; FTextCase := tcNormal;
  FInputMask := ''; FPredefinedMask := pmtNone; FHovered := False; FStatus := cepsNormal;
end;

destructor THTL_CEdit.Destroy;
begin
  FBorderSettings.Free; FFocusSettings.Free; FCaptionSettings.Free; FHoverSettings.Free;
  FTextMargins.Free; FCaretTimer.Free; FGradientSettings.Free; FClickSettings.Free;
  FClickEffectTimer.Free; FProgressSettings.Free; FProgressTimer.Free;
  FImageSettings.Free; FSeparatorSettings.Free;
  FPlaceholderFont.Free;
  inherited Destroy;
end;

procedure THTL_CEdit.StartProcessing;
begin
  if FProgressSettings.ShowProgress and not FProcessing then
  begin
    FProcessing := True;
    FOriginalText := Self.Text;
    FOriginalEnabledState := Self.Enabled;
    if FProgressSettings.HideCaptionWhileProcessing then
      Self.Text := '';
    if Self.Enabled then
      Self.Enabled := False;
    FProgressStep := 0;
    FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
    FProgressTimer.Enabled := True;
    Repaint;
  end;
end;

procedure THTL_CEdit.StopProcessing;
begin
  if FProcessing then
  begin
    FProcessing := False;
    FProgressTimer.Enabled := False;
    Self.Text := FOriginalText;
    if Self.Enabled <> FOriginalEnabledState then
      Self.Enabled := FOriginalEnabledState;
    Repaint;
  end;
end;

procedure THTL_CEdit.ProgressTimerHandler(Sender: TObject);
begin
  if FProcessing then
  begin
    Inc(FProgressStep); // Incremento simples para animação mais fina
    Repaint;            // Usa Repaint para atualização imediata e mais fluida
  end
  else
    FProgressTimer.Enabled := False;
end;

procedure THTL_CEdit.StartClickEffect;
begin
  if not Enabled or not FClickSettings.Enabled or (FClickSettings.Duration <= 0) then Exit;
  FClickEffectActive := True; FClickEffectProgress := 255; UpdateClickEffectTimerInterval; FClickEffectTimer.Enabled := True; Invalidate;
end;

procedure THTL_CEdit.UpdateClickEffectTimerInterval;
const MIN_INTERVAL = 10; FADE_STEP_VALUE = 20;
var NumTicks: Single; NewInterval: Integer;
begin
  if FClickSettings.Duration <= 0 then begin FClickEffectTimer.Interval := MIN_INTERVAL; Exit; end;
  NumTicks := 255 / FADE_STEP_VALUE; if NumTicks <= 0 then NumTicks := 1;
  NewInterval := Round(FClickSettings.Duration / NumTicks); FClickEffectTimer.Interval := Max(MIN_INTERVAL, NewInterval);
end;

procedure THTL_CEdit.ClickEffectTimerHandler(Sender: TObject);
const FADE_STEP_VALUE = 20;
begin
  if FClickEffectActive then
  begin
    if FClickEffectProgress > 0 then Dec(FClickEffectProgress, FADE_STEP_VALUE);
    FClickEffectProgress := Max(0, FClickEffectProgress);
    if FClickEffectProgress <= 0 then begin FClickEffectProgress := 0; FClickEffectActive := False; FClickEffectTimer.Enabled := False; end;
    Invalidate;
  end else begin FClickEffectProgress := 0; FClickEffectTimer.Enabled := False; Invalidate; end;
end;

procedure THTL_CEdit.SettingsChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CEdit.ImageSettingsChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CEdit.CaptionSettingsChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CEdit.HoverSettingsChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CEdit.TextMarginsChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CEdit.PlaceholderFontChanged(Sender: TObject); begin Invalidate; end;

procedure THTL_CEdit.Paint;
var
  LG: TGPGraphics;
  FullClientRect, CaptionRect, WorkAreaRect, EditBoxDrawingRect, ImageDrawingArea: TRect;
  LPathRect, LShadowPathDrawRect: TGPRectF;
  LRadiusValue, LPathInset: Single;
  ActualBGColor, ActualBorderColor, ActualTextColor, ActualCaptionColor: TColor;
  LInitialFillColor, LInitialBorderColor, LBaseStyleColor: TColor;
  LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalClickBorderColor: TColor;
  LHoverProgress, LClickProgress, LScaleFactor: Single;
  IsFocused: Boolean;
  TextToDisplay: string;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LDrawFill, LDrawBorder, LCurrentGradientEnabled: Boolean;
  LActualBorderThickness: Integer;
  LShadowAlphaToUse: Byte;
  LShadowOffsetXToUse, LShadowOffsetYToUse : Single;
  EditBoxRectEffectiveF: TGPRectF;
  ContentRect: TRect;
  imageSlotRect, textSlotRect, ImageFinalDestRect, InsideCaptionRect: TRect;
  PaddedCaptionRect: TRect;
  LImgW, LImgH, LDrawW, LDrawH, AvailableWidth, AvailableHeight: Integer;
const
  SHADOW_ALPHA = 50; SHADOW_OFFSET_X_CONST = 1; SHADOW_OFFSET_Y_CONST = 2;
  SHADOW_ALPHA_HOVER = 80; SHADOW_OFFSET_X_HOVER_FACTOR = 1.5; SHADOW_OFFSET_Y_HOVER_FACTOR = 1.5;
begin
  inherited Paint;
  FullClientRect := Self.ClientRect;
  if (FullClientRect.Width <= 0) or (FullClientRect.Height <= 0) then Exit;

  IsFocused := Self.Focused;
  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    WorkAreaRect := FullClientRect;

    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionSettings.Placement = cplOutside) then
    begin
      Canvas.Font.Assign(FCaptionSettings.Font);
      var CapHeight := Canvas.TextHeight('Wg') + FCaptionSettings.Margins.Top + FCaptionSettings.Margins.Bottom;
      var CapWidth := Canvas.TextWidth(FCaptionSettings.Text) + FCaptionSettings.Margins.Left + FCaptionSettings.Margins.Right;

      case FCaptionSettings.Position of
        cpAbove: begin
          CaptionRect := Rect(WorkAreaRect.Left, WorkAreaRect.Top, WorkAreaRect.Right, WorkAreaRect.Top + CapHeight);
          WorkAreaRect.Top := CaptionRect.Bottom + FCaptionSettings.Offset.Y;
        end;
        cpBelow: begin
          CaptionRect := Rect(WorkAreaRect.Left, WorkAreaRect.Bottom - CapHeight, WorkAreaRect.Right, WorkAreaRect.Bottom);
          WorkAreaRect.Bottom := CaptionRect.Top - FCaptionSettings.Offset.Y;
        end;
        cpLeft: begin
          CaptionRect := Rect(WorkAreaRect.Left, WorkAreaRect.Top, WorkAreaRect.Left + CapWidth, WorkAreaRect.Bottom);
          WorkAreaRect.Left := CaptionRect.Right + FCaptionSettings.Offset.X;
        end;
        cpRight: begin
          CaptionRect := Rect(WorkAreaRect.Right - CapWidth, WorkAreaRect.Top, WorkAreaRect.Right, WorkAreaRect.Bottom);
          WorkAreaRect.Right := CaptionRect.Left - FCaptionSettings.Offset.X;
        end;
      end;

      if Self.Enabled then
        ActualCaptionColor := FCaptionSettings.Color
      else
        ActualCaptionColor := FCaptionSettings.DisabledColor;

      PaddedCaptionRect := CaptionRect;
      PaddedCaptionRect.Left   := PaddedCaptionRect.Left + FCaptionSettings.Margins.Left;
      PaddedCaptionRect.Top    := PaddedCaptionRect.Top + FCaptionSettings.Margins.Top;
      PaddedCaptionRect.Right  := PaddedCaptionRect.Right - FCaptionSettings.Margins.Right;
      PaddedCaptionRect.Bottom := PaddedCaptionRect.Bottom - FCaptionSettings.Margins.Bottom;

      DrawComponentCaption(
        Self.Canvas,
        PaddedCaptionRect,
        FCaptionSettings.Text,
        FCaptionSettings.Font,
        ActualCaptionColor,
        FCaptionSettings.Alignment,
        FCaptionSettings.VerticalAlignment,
        FCaptionSettings.WordWrap,
        FOpacity
      );
    end;

    EditBoxDrawingRect := WorkAreaRect;
    ImageDrawingArea := System.Types.Rect(0,0,0,0);
    if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and (FImageSettings.Placement = iplOutsideBounds) then
    begin
      var ImageSlotW, ImageSlotH, SeparatorSpace: Integer;
      if not FImageSettings.AutoSize and (FImageSettings.TargetWidth > 0) then ImageSlotW := FImageSettings.TargetWidth else ImageSlotW := FImageSettings.Picture.Width;
      if not FImageSettings.AutoSize and (FImageSettings.TargetHeight > 0) then ImageSlotH := FImageSettings.TargetHeight else ImageSlotH := FImageSettings.Picture.Height;
      ImageSlotW := ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
      ImageSlotH := ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;
      if FSeparatorSettings.Visible then SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness else SeparatorSpace := 0;

      case FImageSettings.ImagePosition of
        ipLeft: begin ImageDrawingArea := System.Types.Rect(WorkAreaRect.Left, WorkAreaRect.Top, WorkAreaRect.Left + ImageSlotW, WorkAreaRect.Bottom); EditBoxDrawingRect.Left := ImageDrawingArea.Right + SeparatorSpace; end;
        ipRight: begin ImageDrawingArea := System.Types.Rect(WorkAreaRect.Right - ImageSlotW, WorkAreaRect.Top, WorkAreaRect.Right, WorkAreaRect.Bottom); EditBoxDrawingRect.Right := ImageDrawingArea.Left - SeparatorSpace; end;
        ipTop: begin ImageDrawingArea := System.Types.Rect(WorkAreaRect.Left, WorkAreaRect.Top, WorkAreaRect.Right, WorkAreaRect.Top + ImageSlotH); EditBoxDrawingRect.Top := ImageDrawingArea.Bottom + SeparatorSpace; end;
        ipBottom: begin ImageDrawingArea := System.Types.Rect(WorkAreaRect.Left, WorkAreaRect.Bottom - ImageSlotH, WorkAreaRect.Right, WorkAreaRect.Bottom); EditBoxDrawingRect.Bottom := ImageDrawingArea.Top - SeparatorSpace; end;
      end;
      if EditBoxDrawingRect.Right < EditBoxDrawingRect.Left then EditBoxDrawingRect.Right := EditBoxDrawingRect.Left;
      if EditBoxDrawingRect.Bottom < EditBoxDrawingRect.Top then EditBoxDrawingRect.Bottom := EditBoxDrawingRect.Top;
    end;

    LHoverProgress := 0;
    if Enabled and FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and (FHoverSettings.HoverEffect <> heNone) and not FProcessing then
      LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then
      LClickProgress := FClickEffectProgress / 255.0;

    LInitialFillColor := ResolveStateColor(Enabled, False, False, FBorderSettings.BackgroundColor, clNone, clNone, clBtnFace, False, False, False, True);
    LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderSettings.Color, clNone, clNone, clGray, False, False);
    LActualBorderThickness := FBorderSettings.Thickness;

    if IsFocused and Enabled then
    begin
      case FFocusSettings.Style of
        fsBorder:
          LInitialBorderColor := FFocusSettings.BorderColor;
        fsBackgroundColor:
          if FFocusSettings.BackgroundColor <> clNone then
             LInitialFillColor := FFocusSettings.BackgroundColor;
      end;
    end;

    if Enabled and not FProcessing and (FStatus <> cepsNormal) then
    begin
      var StatusColor: TColor;
      case FStatus of
        cepsError:   StatusColor := TColor($005C5CFF);
        cepsWarning: StatusColor := TColor($002986E3);
        cepsSuccess: StatusColor := TColor($0064B434);
      else
        StatusColor := LInitialBorderColor;
      end;

      LInitialBorderColor := StatusColor;
      LActualBorderThickness := Max(LActualBorderThickness, 2);
      if LInitialFillColor <> clNone then
        LInitialFillColor := BlendColors(LInitialFillColor, StatusColor, 0.10);
    end;

    if FHoverSettings.BackgroundColor <> clNone then LFinalHoverColor := FHoverSettings.BackgroundColor else LFinalHoverColor := LighterColor(LInitialFillColor, 15);
    if FHoverSettings.BorderColor <> clNone then LFinalHoverBorderColor := FHoverSettings.BorderColor else LFinalHoverBorderColor := LInitialBorderColor;
    LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LInitialFillColor, 15), FClickSettings.Color);
    LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LInitialBorderColor, 15), FClickSettings.BorderColor);
    ActualBGColor := LInitialFillColor; ActualBorderColor := LInitialBorderColor;

    if (LHoverProgress > 0) and Enabled and FHoverSettings.Enabled then
    begin
      ActualBGColor := BlendColors(LInitialFillColor, LFinalHoverColor, LHoverProgress);
      ActualBorderColor := BlendColors(LInitialBorderColor, LFinalHoverBorderColor, LHoverProgress);
    end;
    if (LClickProgress > 0) and Enabled and FClickSettings.Enabled and (FClickSettings.Duration > 0) then
    begin
      ActualBGColor := BlendColors(ActualBGColor, LFinalClickColor, LClickProgress);
      ActualBorderColor := BlendColors(ActualBorderColor, LFinalClickBorderColor, LClickProgress);
    end;

    LCurrentGradientEnabled := FGradientSettings.Enabled; LDrawFill := True; LDrawBorder := LActualBorderThickness > 0;
    case FStyle of
      bsSolid: LCurrentGradientEnabled := False;
      bsBordered: begin LDrawFill := False; LCurrentGradientEnabled := False; LActualBorderThickness := Max(1, LActualBorderThickness); LDrawBorder := True; end;
      bsFlat: begin LCurrentGradientEnabled := False; LActualBorderThickness := 0; LDrawBorder := False; end;
      bsGhost: begin LDrawFill := False; LCurrentGradientEnabled := False; LActualBorderThickness := Max(1, LActualBorderThickness); ActualBorderColor := LInitialFillColor; LDrawBorder := True; end;
      bsGradient: begin LCurrentGradientEnabled := True; LDrawFill := True; LDrawBorder := LActualBorderThickness > 0; end;
    end;

    EditBoxRectEffectiveF.X := EditBoxDrawingRect.Left; EditBoxRectEffectiveF.Y := EditBoxDrawingRect.Top; EditBoxRectEffectiveF.Width := EditBoxDrawingRect.Width; EditBoxRectEffectiveF.Height := EditBoxDrawingRect.Height;

    if (FStyle in [bsShadow, bsMaterial]) then
    begin
      LShadowOffsetXToUse := 1; LShadowOffsetYToUse := 2; LShadowAlphaToUse := 60;
      var TempW := EditBoxRectEffectiveF.Width - Abs(LShadowOffsetXToUse); var TempH := EditBoxRectEffectiveF.Height - Abs(LShadowOffsetYToUse);
      EditBoxRectEffectiveF := MakeRect(EditBoxRectEffectiveF.X, EditBoxRectEffectiveF.Y, Max(0, TempW), Max(0, TempH));
      LPathInset := IfThen(LActualBorderThickness > 0, LActualBorderThickness / 2.0, 0.0);
      LShadowPathDrawRect := MakeRect(EditBoxRectEffectiveF.X + LPathInset + LShadowOffsetXToUse, EditBoxRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse, Max(0, EditBoxRectEffectiveF.Width - 2 * LPathInset), Max(0, EditBoxRectEffectiveF.Height - 2 * LPathInset));
      LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0);
      LGPPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, Max(0, LRadiusValue), FBorderSettings.RoundCornerType);
        if LGPPath.GetPointCount > 0 then begin LGPBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, LShadowAlphaToUse)); try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end; end;
      finally LGPPath.Free; end;
    end;

    if IsFocused and Enabled and (FFocusSettings.Style = fsGlow) and (FFocusSettings.GlowSize > 0) then
    begin
      var LGlowPath: TGPGraphicsPath;
      var LGlowBrush: TGPBrush;
      var LGlowRect: TGPRectF;
      var LGlowRadius: Single;
      var LGlowInset: Single := IfThen(LDrawBorder, LActualBorderThickness / 2.0, 0) - FFocusSettings.GlowSize;

      LGlowRect := MakeRect(EditBoxRectEffectiveF.X + LGlowInset, EditBoxRectEffectiveF.Y + LGlowInset, EditBoxRectEffectiveF.Width - 2 * LGlowInset, EditBoxRectEffectiveF.Height - 2 * LGlowInset);
      LGlowRadius := Min(FBorderSettings.CornerRadius + FFocusSettings.GlowSize, Min(LGlowRect.Width, LGlowRect.Height) / 2.0);

      LGlowPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGlowPath, LGlowRect, Max(0, LGlowRadius), FBorderSettings.RoundCornerType);
        if LGlowPath.GetPointCount > 0 then
        begin
          LGlowBrush := TGPSolidBrush.Create(ColorToARGB(FFocusSettings.GlowColor, FFocusSettings.GlowIntensity));
          try
            LG.FillPath(LGlowBrush, LGlowPath);
          finally
            LGlowBrush.Free;
          end;
        end;
      finally
        LGlowPath.Free;
      end;
    end;

    LPathInset := IfThen(LDrawBorder and (LActualBorderThickness > 0), LActualBorderThickness / 2.0, 0.0);
    LPathRect := MakeRect(EditBoxRectEffectiveF.X + LPathInset, EditBoxRectEffectiveF.Y + LPathInset, EditBoxRectEffectiveF.Width - 2 * LPathInset, EditBoxRectEffectiveF.Height - 2 * LPathInset);
    LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LPathRect.Width, LPathRect.Height) / 2.0);
    LGPPath := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(LGPPath, LPathRect, Max(0, LRadiusValue), FBorderSettings.RoundCornerType);
      if LGPPath.GetPointCount > 0 then
      begin
        if LDrawFill then
        begin
          if LCurrentGradientEnabled and (FGradientSettings.StartColor <> clNone) and (FGradientSettings.EndColor <> clNone) then
          begin LGPBrush := TGPLinearGradientBrush.Create(LPathRect, ColorToARGB(FGradientSettings.StartColor, FOpacity), ColorToARGB(FGradientSettings.EndColor, FOpacity), LinearGradientModeVertical); end
          else LGPBrush := TGPSolidBrush.Create(ColorToARGB(ActualBGColor, FOpacity));
          try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end;
        end;
        if LDrawBorder and (FBorderSettings.Style <> psClear) then
        begin LGPPen := TGPPen.Create(ColorToARGB(ActualBorderColor, FOpacity), LActualBorderThickness); try case FBorderSettings.Style of psDash: LGPPen.SetDashStyle(DashStyleDash); else LGPPen.SetDashStyle(DashStyleSolid); end; LG.DrawPath(LGPPen, LGPPath); finally LGPPen.Free; end; end;
      end;
    finally LGPPath.Free; end;

    ContentRect := Rect(Round(EditBoxRectEffectiveF.X), Round(EditBoxRectEffectiveF.Y), Round(EditBoxRectEffectiveF.X + EditBoxRectEffectiveF.Width), Round(EditBoxRectEffectiveF.Y + EditBoxRectEffectiveF.Height));
    if LDrawBorder and (LActualBorderThickness > 0) then InflateRect(ContentRect, -LActualBorderThickness, -LActualBorderThickness);

    if FProcessing and FProgressSettings.ShowProgress then
    begin
      var LProgressRect := ContentRect;
      case FProgressSettings.AnimationStyle of
        pasRotatingSemiCircle, pasFullCircularSpinner:
        begin
          var Side := Min(LProgressRect.Width, LProgressRect.Height);
          var CenteredSquareRect: TRect;
          CenteredSquareRect.Left   := LProgressRect.Left + (LProgressRect.Width - Side) div 2;
          CenteredSquareRect.Top    := LProgressRect.Top + (LProgressRect.Height - Side) div 2;
          CenteredSquareRect.Width  := Side;
          CenteredSquareRect.Height := Side;
          LProgressRect := CenteredSquareRect;

          InflateRect(LProgressRect, -4, -4);
          if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
          begin
            var LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
            var ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
            var LStartAngle, LSweepAngle: Single;
            var LProgressBarPen: TGPPen;

            if FProgressSettings.AnimationStyle = pasRotatingSemiCircle then
            begin
              LStartAngle := (FProgressStep * 10) mod 360;
              LSweepAngle := 270;
            end
            else // pasFullCircularSpinner
            begin
               LStartAngle := (FProgressStep * 12) mod 360;
               LSweepAngle := 90 + (Sin(FProgressStep / 5.0) * 80);
            end;

            LGPPath := TGPGraphicsPath.Create;
            try
              if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
              begin
                LGPPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                try
                  LProgressBarPen.SetStartCap(LineCapRound);
                  LProgressBarPen.SetEndCap(LineCapRound);
                  LG.DrawPath(LProgressBarPen, LGPPath);
                finally
                  LProgressBarPen.Free;
                end;
              end;
            finally
              LGPPath.Free;
            end;
          end;
        end;
        pasHorizontalBar:
        begin
          InflateRect(LProgressRect, -4, - Round(LProgressRect.Height / 3));
          var BarWidth := LProgressRect.Width div 3;
          if BarWidth > 0 then
          begin
            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 100));
            try // Desenha a trilha de fundo
              LG.FillRectangle(LGPBrush, LProgressRect.Left, LProgressRect.Top, LProgressRect.Width, LProgressRect.Height);
            finally
              LGPBrush.Free;
            end;

            var InnerBarX: Integer;
            if (LProgressRect.Width + BarWidth) > 0 then
                InnerBarX := (FProgressStep * 5) mod (LProgressRect.Width + BarWidth)
            else
                InnerBarX := 0;

            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
            try // Desenha a barra móvel
              LG.FillRectangle(LGPBrush, LProgressRect.Left + InnerBarX - BarWidth, LProgressRect.Top, BarWidth, LProgressRect.Height);
            finally
              LGPBrush.Free;
            end;
          end;
        end;
        pasBouncingDots:
        begin
          const NUM_DOTS = 3;
          InflateRect(LProgressRect, -4, -4);
          if (LProgressRect.Width > 0) and (LProgressRect.Height > 0) then
          begin
            var DotSize := Min(LProgressRect.Width / (NUM_DOTS * 2.0), LProgressRect.Height / 2.0);
            if DotSize > 1 then
            begin
              var DotSpacing := DotSize / 2.0;
              var TotalWidth := (NUM_DOTS * DotSize) + ((NUM_DOTS - 1) * DotSpacing);
              var StartX := LProgressRect.Left + (LProgressRect.Width - TotalWidth) / 2.0;
              var BaseY := LProgressRect.Top + (LProgressRect.Height - DotSize) / 2.0;

              LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
              try
                for var I := 0 to NUM_DOTS - 1 do
                begin
                  var yOffset := Round((DotSize / 2) * Sin((FProgressStep * 0.2) + (I * (PI / NUM_DOTS))));
                  LG.FillEllipse(LGPBrush, StartX + I * (DotSize + DotSpacing), BaseY + yOffset, DotSize, DotSize);
                end;
              finally
                LGPBrush.Free;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      if FImageSettings.Placement = iplInsideBounds then
      begin textSlotRect := ContentRect; imageSlotRect := ContentRect; end
      else begin textSlotRect := ContentRect; imageSlotRect := ImageDrawingArea; end;

      LDrawW := 0; LDrawH := 0;
      if (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and FImageSettings.Visible then
      begin
          AvailableWidth  := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right); AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);
          LImgW := FImageSettings.Picture.Width; LImgH := FImageSettings.Picture.Height;
          if FImageSettings.AutoSize then
          begin if (LImgW > 0) and (LImgH > 0) and (AvailableWidth > 0) and (AvailableHeight > 0) then begin var r := LImgW / LImgH; var rr := AvailableWidth / AvailableHeight; if rr > r then begin LDrawH := AvailableHeight; LDrawW := Round(LDrawH * r); end else begin LDrawW := AvailableWidth; LDrawH := Round(LDrawW / r); end; end else begin LDrawW := 0; LDrawH := 0; end; end
          else begin LDrawW := FImageSettings.TargetWidth; LDrawH := FImageSettings.TargetHeight; end;
          LDrawW := Max(0, Min(LDrawW, AvailableWidth)); LDrawH := Max(0, Min(LDrawH, AvailableHeight));
      end;
      if (LDrawW > 0) and (FImageSettings.Placement = iplInsideBounds) then
      begin
          var imageAndMarginsW := LDrawW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
          var SeparatorSpace: Integer;
          if FSeparatorSettings.Visible then SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness else SeparatorSpace := 0;
          case FImageSettings.ImagePosition of
            ipLeft: begin imageSlotRect.Width := imageAndMarginsW; textSlotRect.Left := imageSlotRect.Right + SeparatorSpace; end;
            ipRight: begin imageSlotRect.Left := imageSlotRect.Right - imageAndMarginsW; textSlotRect.Right := imageSlotRect.Left - SeparatorSpace; end;
          end;
      end;

      InsideCaptionRect := Rect(0,0,0,0);
      if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionSettings.Placement = cplInside) then
      begin
        Canvas.Font.Assign(FCaptionSettings.Font);
        var CapHeight := Canvas.TextHeight('Wg') + FCaptionSettings.Margins.Top + FCaptionSettings.Margins.Bottom;
        var CapWidth := Canvas.TextWidth(FCaptionSettings.Text) + FCaptionSettings.Margins.Left + FCaptionSettings.Margins.Right;

        case FCaptionSettings.Position of
          cpAbove: begin
            InsideCaptionRect := Rect(textSlotRect.Left, textSlotRect.Top, textSlotRect.Right, textSlotRect.Top + CapHeight);
            textSlotRect.Top := InsideCaptionRect.Bottom + FCaptionSettings.Offset.Y;
          end;
          cpBelow: begin
            InsideCaptionRect := Rect(textSlotRect.Left, textSlotRect.Bottom - CapHeight, textSlotRect.Right, textSlotRect.Bottom);
            textSlotRect.Bottom := InsideCaptionRect.Top - FCaptionSettings.Offset.Y;
          end;
          cpLeft: begin
            InsideCaptionRect := Rect(textSlotRect.Left, textSlotRect.Top, textSlotRect.Left + CapWidth, textSlotRect.Bottom);
            textSlotRect.Left := InsideCaptionRect.Right + FCaptionSettings.Offset.X;
          end;
          cpRight: begin
            InsideCaptionRect := Rect(textSlotRect.Right - CapWidth, textSlotRect.Top, textSlotRect.Right, textSlotRect.Bottom);
            textSlotRect.Right := InsideCaptionRect.Left - FCaptionSettings.Offset.X;
          end;
        end;
      end;

      if FSeparatorSettings.Visible and FImageSettings.Visible and (LDrawW > 0) and (FImageSettings.ImagePosition in [ipLeft, ipRight, ipTop, ipBottom]) then
      begin
        var ASepRect: TRect; var refRect: TRect; var isVertical: Boolean; isVertical := FImageSettings.ImagePosition in [ipLeft, ipRight];
        if FImageSettings.Placement = iplOutsideBounds then refRect := ImageDrawingArea else refRect := imageSlotRect;
        case FImageSettings.ImagePosition of
          ipLeft: ASepRect := System.Types.Rect(refRect.Right + FSeparatorSettings.Padding, refRect.Top, refRect.Right + FSeparatorSettings.Padding + FSeparatorSettings.Thickness, refRect.Bottom);
          ipRight: ASepRect := System.Types.Rect(refRect.Left - FSeparatorSettings.Padding - FSeparatorSettings.Thickness, refRect.Top, refRect.Left - FSeparatorSettings.Padding, refRect.Bottom);
          ipTop: ASepRect := System.Types.Rect(refRect.Left, refRect.Bottom + FSeparatorSettings.Padding, refRect.Right, refRect.Bottom + FSeparatorSettings.Padding + FSeparatorSettings.Thickness);
          ipBottom: ASepRect := System.Types.Rect(refRect.Left, refRect.Top - FSeparatorSettings.Padding - FSeparatorSettings.Thickness, refRect.Right, refRect.Top - FSeparatorSettings.Padding);
        end;
        case FSeparatorSettings.HeightMode of
          shmFull: { Altura total já calculada };
          shmCustom:
            if isVertical then begin if FSeparatorSettings.CustomHeight > 0 then begin var cH := ASepRect.Height; var cH2 := Min(cH, FSeparatorSettings.CustomHeight); ASepRect.Top := ASepRect.Top + (cH - cH2) div 2; ASepRect.Bottom := ASepRect.Top + cH2; end; end
            else begin if FSeparatorSettings.CustomHeight > 0 then begin var cW := ASepRect.Width; var cW2 := Min(cW, FSeparatorSettings.CustomHeight); ASepRect.Left := ASepRect.Left + (cW - cW2) div 2; ASepRect.Right := ASepRect.Left + cW2; end; end;
        end;
        if (ASepRect.Width > 0) and (ASepRect.Height > 0) then begin Self.Canvas.Brush.Color := FSeparatorSettings.Color; Self.Canvas.Brush.Style := TBrushStyle.bsSolid; Self.Canvas.FillRect(ASepRect); end;
      end;

      if (LDrawW > 0) then
      begin
          AvailableWidth := Max(0, imageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right); AvailableHeight := Max(0, imageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);
          var LImgX := imageSlotRect.Left + FImageSettings.Margins.Left + (AvailableWidth - LDrawW) div 2; var LImgY := imageSlotRect.Top + FImageSettings.Margins.Top + (AvailableHeight - LDrawH) div 2;
          ImageFinalDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);
          if (ImageFinalDestRect.Right > ImageFinalDestRect.Left) and (ImageFinalDestRect.Bottom > ImageFinalDestRect.Top) then begin if FImageSettings.Picture.Graphic is TPNGImage then DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, ImageFinalDestRect, idmStretch) else if FImageSettings.Picture.Graphic <> nil then DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, ImageFinalDestRect, idmStretch); end;
      end;

      var PaddedTextDrawArea := textSlotRect;
      InflateRect(PaddedTextDrawArea, -FTextMargins.Left, -FTextMargins.Top);
      if (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
      begin
        if (Length(FText) = 0) and not IsFocused and (FPlaceholder <> '') then
        begin
          Canvas.Font.Assign(FPlaceholderFont);
          Canvas.Brush.Style := bsClear;
          SetBkMode(Canvas.Handle, TRANSPARENT);
          DrawText(Canvas.Handle, PChar(FPlaceholder), -1, PaddedTextDrawArea, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL);
        end
        else
        begin
          if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then TextToDisplay := StringOfChar(FPasswordChar, Length(FText)) else TextToDisplay := FText;
          ActualTextColor := ResolveStateColor(Enabled, FHovered, IsFocused, Self.Font.Color, FHoverSettings.FontColor, Self.Font.Color, clGrayText, FHoverSettings.Enabled, False);
          Canvas.Font.Assign(Self.Font);
          Canvas.Font.Color := ActualTextColor;
          Canvas.Brush.Style := bsClear;
          SetBkMode(Canvas.Handle, TRANSPARENT);
          DrawText(Canvas.Handle, PChar(TextToDisplay), -1, PaddedTextDrawArea, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL);
          if IsFocused and FCaretVisible and not ReadOnly then
          begin
            var CaretXBase := PaddedTextDrawArea.Left;
            var CaretHeight := Canvas.TextHeight('Wg');
            var CaretTop := PaddedTextDrawArea.Top + (PaddedTextDrawArea.Height - CaretHeight) div 2;
            var CaretXOffset := Canvas.TextWidth(Copy(TextToDisplay, 1, FCaretPosition));
            Canvas.Pen.Color := ActualTextColor;
            Canvas.Pen.Width := 1;
            Canvas.MoveTo(CaretXBase + CaretXOffset, CaretTop);
            Canvas.LineTo(CaretXBase + CaretXOffset, CaretTop + CaretHeight);
          end;
        end;
      end;

      if (InsideCaptionRect.Width > 0) or (InsideCaptionRect.Height > 0) then
      begin
        if Self.Enabled then
          ActualCaptionColor := FCaptionSettings.Color
        else
          ActualCaptionColor := FCaptionSettings.DisabledColor;

        PaddedCaptionRect := InsideCaptionRect;
        PaddedCaptionRect.Left   := PaddedCaptionRect.Left + FCaptionSettings.Margins.Left;
        PaddedCaptionRect.Top    := PaddedCaptionRect.Top + FCaptionSettings.Margins.Top;
        PaddedCaptionRect.Right  := PaddedCaptionRect.Right - FCaptionSettings.Margins.Right;
        PaddedCaptionRect.Bottom := PaddedCaptionRect.Bottom - FCaptionSettings.Margins.Bottom;

        DrawComponentCaption(
          Self.Canvas,
          PaddedCaptionRect,
          FCaptionSettings.Text,
          FCaptionSettings.Font,
          ActualCaptionColor,
          FCaptionSettings.Alignment,
          FCaptionSettings.VerticalAlignment,
          FCaptionSettings.WordWrap,
          FOpacity
        );
      end;

      if IsFocused and Enabled and (FFocusSettings.Style = fsUnderline) and (FFocusSettings.UnderlineThickness > 0) then
      begin
          var LUnderlinePen: TGPPen;
          var yPos := EditBoxRectEffectiveF.Y + EditBoxRectEffectiveF.Height - (FFocusSettings.UnderlineThickness / 2);
          var x1 := EditBoxRectEffectiveF.X;
          var x2 := EditBoxRectEffectiveF.X + EditBoxRectEffectiveF.Width;

          LUnderlinePen := TGPPen.Create(ColorToARGB(FFocusSettings.UnderlineColor, 255), FFocusSettings.UnderlineThickness);
          try
            LG.DrawLine(LUnderlinePen, x1, yPos, x2, yPos);
          finally
            LUnderlinePen.Free;
          end;
      end;
    end;
  finally
    LG.Free;
  end;
end;

procedure THTL_CEdit.CaretTimerTick(Sender: TObject);
begin
  if Focused and not FProcessing then begin FCaretVisible := not FCaretVisible; Invalidate; end
  else begin FCaretVisible := False; FCaretTimer.Enabled := False; if Focused then Invalidate; end;
end;

procedure THTL_CEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  // Apenas pare as animações se o controle for desabilitado externamente,
  // e não como parte do estado de 'Processing'.
  if not Enabled and not FProcessing then
  begin
    FClickEffectActive := False;
    FClickEffectProgress := 0;
    FClickEffectTimer.Enabled := False;
    if FHoverSettings.Enabled then
      FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

procedure THTL_CEdit.CMEnter(var Message: TCMEnter);
begin
  inherited; FCaretVisible := True; FCaretTimer.Enabled := True; Self.Cursor := FCurrentCursor; if Assigned(FOnEnter) then FOnEnter(Self); Invalidate;
end;

procedure THTL_CEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Self.Cursor := crDefault;
  if Assigned(FOnExit) then FOnExit(Self);
  Invalidate;
end;

procedure THTL_CEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited; if not FHovered then begin FHovered := True; if FHoverSettings.Enabled then FHoverSettings.StartAnimation(True) else Invalidate; end;
end;

procedure THTL_CEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited; if FHovered then begin FHovered := False; if FHoverSettings.Enabled then FHoverSettings.StartAnimation(False) else Invalidate; end;
end;

procedure THTL_CEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldText: string;
  Changed: Boolean;
  RawText: string;
begin
  inherited KeyDown(Key, Shift);
  if FProcessing or FReadOnly then
  begin
    Key := 0;
    Exit;
  end;

  if (FInputMask <> '') and (Key = VK_BACK) then
  begin
    Key := 0;
    OldText := FText;
    RawText := UnmaskText(FText);
    if Length(RawText) > 0 then
    begin
      Delete(RawText, Length(RawText), 1);
      FText := FormatText(RawText, FInputMask);
      FCaretPosition := Length(FText) + 1;
      if OldText <> FText then
        if Assigned(FOnChange) then FOnChange(Self);
      Invalidate;
    end;
    Exit;
  end;

  if FInputMask = '' then
  begin
    Changed := False;
    OldText := FText;
    case Key of
      VK_BACK: if (FCaretPosition > 0) then begin FText := Copy(FText, 1, FCaretPosition - 1) + Copy(FText, FCaretPosition + 1, MaxInt); Dec(FCaretPosition); Changed := True; end;
      VK_DELETE: if (FCaretPosition < Length(FText)) then begin FText := Copy(FText, 1, FCaretPosition) + Copy(FText, FCaretPosition + 2, MaxInt); Changed := True; end;
      VK_HOME: begin FCaretPosition := 0; Changed := True; end;
      VK_END: begin FCaretPosition := Length(FText); Changed := True; end;
      VK_LEFT: begin if FCaretPosition > 0 then Dec(FCaretPosition); Changed := True; end;
      VK_RIGHT: begin if FCaretPosition < Length(FText) then Inc(FCaretPosition); Changed := True; end;
      else exit;
    end;
    Key := 0;
    if Changed then
    begin
      FCaretVisible := True;
      if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end;
      if Assigned(FOnChange) and (OldText <> FText) then FOnChange(Self);
      Invalidate;
    end;
  end;
end;

procedure THTL_CEdit.KeyPress(var Key: Char);
var
  OldText: string;
  RawText: string;
begin
  inherited KeyPress(Key);

  if (FInputMask = '') then
  begin
    if (Key = #8) or FProcessing or FReadOnly then begin Key := #0; Exit; end;
    if (FInputType <> itNormal) and (Key >= ' ') then
    begin
      var AllowChar := False;
      case FInputType of
        itLettersOnly: AllowChar := TCharacter.IsLetter(Key);
        itNumbersOnly: AllowChar := TCharacter.IsNumber(Key);
        itAlphaNumericOnly: AllowChar := TCharacter.IsLetterOrDigit(Key);
        else AllowChar := True;
      end;
      if not AllowChar then Key := #0;
    end;
    if (Key >= ' ') then
    begin
      case FTextCase of tcUppercase: Key := TCharacter.ToUpper(Key); tcLowercase: Key := TCharacter.ToLower(Key); end;
      OldText := FText; if (FMaxLength > 0) and (Length(FText) >= FMaxLength) then begin Key := #0; Exit; end;
      if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);
      FText := Copy(FText, 1, FCaretPosition) + Key + Copy(FText, FCaretPosition + 1, MaxInt); Inc(FCaretPosition);
      FCaretVisible := True; if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end;
      if OldText <> FText then begin Invalidate; if Assigned(FOnChange) then FOnChange(Self); end;
    end;
    if Key <> #0 then Key := #0;
    Exit;
  end;

  if (Key < ' ') or FProcessing or FReadOnly then
  begin
    Key := #0; Exit;
  end;

  case FInputType of
    itNumbersOnly: if not TCharacter.IsNumber(Key) then begin Key := #0; Exit; end;
    itLettersOnly: if not TCharacter.IsLetter(Key) then begin Key := #0; Exit; end;
    itAlphaNumericOnly: if not TCharacter.IsLetterOrDigit(Key) then begin Key := #0; Exit; end;
  end;

  if Length(FText) >= Length(FInputMask) then
  begin
    Key := #0;
    Exit;
  end;

  RawText := UnmaskText(FText) + Key;
  OldText := FText;
  FText := FormatText(RawText, FInputMask);

  FCaretPosition := Length(FText) + 1;
  FCaretVisible := True;
  if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end;
  if OldText <> FText then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
  end;

  Key := #0;
end;

procedure THTL_CEdit.Click;
begin
  inherited Click; if CanFocus and not Focused then SetFocus;
end;

procedure THTL_CEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, ClickX_RelativeToPaddedText, CurrentWidth, CharWidth: Integer; TextToMeasure: string; PaddedTextDrawArea: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FProcessing then Exit;
  if Button = mbLeft then
  begin
    StartClickEffect; if CanFocus and not Focused then SetFocus;
    FCaretVisible := True; FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
    PaddedTextDrawArea := ClientRect;
    InflateRect(PaddedTextDrawArea, -(FBorderSettings.Thickness + FImageSettings.Margins.Left + FTextMargins.Left), -(FBorderSettings.Thickness + FTextMargins.Top));
    if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and (FImageSettings.ImagePosition = ipLeft) then
      PaddedTextDrawArea.Left := PaddedTextDrawArea.Left + FImageSettings.Picture.Width + FImageSettings.Margins.Right + FSeparatorSettings.Thickness + FSeparatorSettings.Padding;
    ClickX_RelativeToPaddedText := X - PaddedTextDrawArea.Left;
    TextToMeasure := FText; if (FPasswordChar <> #0) then TextToMeasure := StringOfChar(FPasswordChar, Length(FText));
    Canvas.Font.Assign(Self.Font); CurrentWidth := 0; FCaretPosition := 0;
    for I := 1 to Length(TextToMeasure) do
    begin
      CharWidth := Canvas.TextWidth(TextToMeasure[I]); if ClickX_RelativeToPaddedText < (CurrentWidth + CharWidth div 2) then begin FCaretPosition := I - 1; Break; end;
      CurrentWidth := CurrentWidth + CharWidth; FCaretPosition := I;
    end;
    Invalidate;
  end;
end;

procedure THTL_CEdit.ApplyFormatting;
var
  LFormattedText: string;
begin
  if FInputMask <> '' then
  begin
    LFormattedText := FormatText(UnmaskText(FText), FInputMask);
    if FText <> LFormattedText then
    begin
      FText := LFormattedText;
      FCaretPosition := Length(FText);
      if Assigned(FOnChange) then
        FOnChange(Self);
      Invalidate;
    end;
  end;
end;

procedure THTL_CEdit.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
  end;
end;

procedure THTL_CEdit.SetPlaceholder(const Value: string);
begin
  if FPlaceholder <> Value then
  begin
    FPlaceholder := Value;
    Invalidate;
  end;
end;

procedure THTL_CEdit.SetPlaceholderFont(const Value: TFont);
begin
  FPlaceholderFont.Assign(Value);
end;

procedure THTL_CEdit.SetInputMask(const Value: string);
begin
  if FInputMask <> Value then
  begin
    FInputMask := Value;
    if FInputMask <> '' then
    begin
      FPredefinedMask := pmtCustom;
      FMaxLength := Length(FInputMask);
    end
    else
    begin
      FPredefinedMask := pmtNone;
      FMaxLength := 0;
    end;
    ApplyFormatting;
    Invalidate;
  end;
end;

procedure THTL_CEdit.SetPredefinedMask(const Value: TPredefinedMaskType);
var
  NewMask: string;
begin
  if FPredefinedMask <> Value then
  begin
    FPredefinedMask := Value;

    case Value of
      pmtCPF, pmtCNPJ, pmtRG, pmtCEP, pmtPhoneBRLandline, pmtPhoneBRMobile,
      pmtDateDMY, pmtDateYMD, pmtTimeHHMM, pmtTimeHHMMSS:
        Self.InputType := itNumbersOnly;
      pmtLicensePlateBR, pmtLicensePlateMercosul:
        Self.InputType := itAlphaNumericOnly;
    else
      Self.InputType := itNormal;
    end;

    case Value of
      pmtCPF: NewMask := '000.000.000-00';
      pmtCNPJ: NewMask := '00.000.000/0000-00';
      pmtRG: NewMask := '00.000.000-0';
      pmtCEP: NewMask := '00000-000';
      pmtPhoneBRLandline: NewMask := '(00) 0000-0000';
      pmtPhoneBRMobile: NewMask := '(00) 0 0000-0000';
      pmtDateDMY: NewMask := '00/00/0000';
      pmtDateYMD: NewMask := '0000/00/00';
      pmtTimeHHMM: NewMask := '00:00';
      pmtTimeHHMMSS: NewMask := '00:00:00';
      pmtLicensePlateBR: NewMask := '000-0000';
      pmtLicensePlateMercosul: NewMask := '000-0A00';
    else
      NewMask := '';
    end;
    SetInputMask(NewMask);
  end;
end;

procedure THTL_CEdit.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then begin FMaxLength := Max(0, Value); if (FMaxLength > 0) and (Length(FText) > FMaxLength) then begin FText := Copy(FText, 1, FMaxLength); if FCaretPosition > Length(FText) then FCaretPosition := Length(FText); if Assigned(FOnChange) then FOnChange(Self); Invalidate; end; end;
end;

procedure THTL_CEdit.SetPasswordChar(const Value: Char); begin if FPasswordChar <> Value then begin FPasswordChar := Value; Invalidate; end; end;
procedure THTL_CEdit.SetReadOnly(const Value: Boolean); begin if FReadOnly <> Value then begin FReadOnly := Value; Invalidate; end; end;
procedure THTL_CEdit.SetBorderSettings(const Value: TBorderSettings); begin FBorderSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); end;
procedure THTL_CEdit.SetTextMargins(const Value: THTL_Margins); begin FTextMargins.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetFocusSettings(const Value: TFocusSettings); begin FFocusSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetOpacity(const Value: Byte); begin if FOpacity <> Value then begin FOpacity := Value; Invalidate; end; end;
procedure THTL_CEdit.SetCurrentCursor(const Value: TCursor); begin if FCurrentCursor <> Value then begin FCurrentCursor := Value; Cursor := FCurrentCursor; end; end;
procedure THTL_CEdit.SetInputType(const Value: TInputType); begin if FInputType <> Value then begin FInputType := Value; end; end;
procedure THTL_CEdit.SetTextCase(const Value: TTextCase); begin if FTextCase <> Value then begin FTextCase := Value; SetText(FText); end; end;
procedure THTL_CEdit.SetStatus(const Value: TCEditStatus); begin if FStatus <> Value then begin FStatus := Value; Invalidate; end; end;
procedure THTL_CEdit.SetGradientSettings(const Value: TGradientSettings); begin FGradientSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetClickSettings(const Value: TClickSettings); begin FClickSettings.Assign(Value); UpdateClickEffectTimerInterval; SettingsChanged(Self); end;
procedure THTL_CEdit.SetProgressSettings(const Value: TProgressSettings); begin FProgressSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CEdit.SetStyle(const Value: TButtonStyle); begin if FStyle <> Value then begin FStyle := Value; Invalidate; end; end;
procedure THTL_CEdit.SetImageSettings(const Value: TImageSettings); begin FImageSettings.Assign(Value); end;
procedure THTL_CEdit.SetSeparatorSettings(const Value: TSeparatorSettings); begin FSeparatorSettings.Assign(Value); end;

end.

