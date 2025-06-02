unit ANDMR_TCheckBox;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, // For crHandPoint, Messages, VK_SPACE
  ANDMR_ComponentUtils;

type
  TANDMR_TCheckBox = class(TCustomControl)
  private
    FChecked: Boolean;
    FCaption: string;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FBoxColorUnchecked: TColor;
    FBoxColorChecked: TColor;
    FCheckMarkColor: TColor;
    FTitleFont: TFont;
    FTransparent: Boolean;
    FOnClick: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent;
    FInternalHoverSettings: THoverSettings;

    procedure SetChecked(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetBoxColorUnchecked(const Value: TColor);
    procedure SetBoxColorChecked(const Value: TColor);
    procedure SetCheckMarkColor(const Value: TColor);
    procedure SetTitleFont(const Value: TFont);
    procedure SetTransparent(const Value: Boolean);
    procedure SetInternalHoverSettings(const Value: THoverSettings);
    procedure SetEnabled(Value: Boolean);

    procedure FontChanged(Sender: TObject);
    procedure InternalHoverSettingsChanged(Sender: TObject);

  protected
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Checked: Boolean read FChecked write SetChecked;
    property Caption: string read FCaption write SetCaption;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType;
    property BoxColorUnchecked: TColor read FBoxColorUnchecked write SetBoxColorUnchecked;
    property BoxColorChecked: TColor read FBoxColorChecked write SetBoxColorChecked;
    property CheckMarkColor: TColor read FCheckMarkColor write SetCheckMarkColor;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property InternalHoverSettings: THoverSettings read FInternalHoverSettings write SetInternalHoverSettings;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;
  published
    // Properties will be moved here later
  end;

procedure Register; // Declaration for Register procedure

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes,
  ANDMR_ColorUtils;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_TCheckBox]);
end;

{ TANDMR_TCheckBox }

constructor TANDMR_TCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csReplicatable, csCaptureMouse, csNeedsBorderPaint, csAcceptsControls]; // csAcceptsControls for focus
  FTransparent := False;
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

  Width := 120;
  Height := 24;

  FChecked := False;
  FCaption := Name;
  FCornerRadius := 3;
  FRoundCornerType := rctAll;
  FBoxColorUnchecked := clWindow;
  FBoxColorChecked := clHighlight;
  FCheckMarkColor := clWindowText;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Segoe UI';
  FTitleFont.Size := 9;
  FTitleFont.Color := clWindowText;
  FTitleFont.OnChange := FontChanged;

  FInternalHoverSettings := THoverSettings.Create(Self);
  FInternalHoverSettings.OwnerControl := Self;
  FInternalHoverSettings.OnChange := InternalHoverSettingsChanged;
  FInternalHoverSettings.BackgroundColor := clNone;
  FInternalHoverSettings.BorderColor := clNone;
  FInternalHoverSettings.FontColor := clNone;
  FInternalHoverSettings.Enabled := True;

  TabStop := True; // Important for keyboard interaction
  Cursor := crHandPoint;
  DoubleBuffered := True;
end;

destructor TANDMR_TCheckBox.Destroy;
begin
  if Assigned(FInternalHoverSettings) then
  begin
    FInternalHoverSettings.OnChange := nil;
    FInternalHoverSettings.Free;
    FInternalHoverSettings := nil;
  end;

  if Assigned(FTitleFont) then
  begin
    FTitleFont.OnChange := nil;
    FTitleFont.Free;
    FTitleFont := nil;
  end;

  inherited Destroy;
end;

procedure TANDMR_TCheckBox.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_TCheckBox.InternalHoverSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_TCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TANDMR_TCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Self.Enabled and Assigned(FInternalHoverSettings) and FInternalHoverSettings.Enabled then
  begin
    FInternalHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_TCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FInternalHoverSettings) then
  begin
    FInternalHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_TCheckBox.Click;
begin
  if not Enabled then Exit;

  SetChecked(not Checked);

  if Assigned(FOnClick) then
    FOnClick(Self);
  // No inherited Click; as TCustomControl.Click is empty and TControl.Click would just call OnClick again.
end;

procedure TANDMR_TCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = 0 then // Key already handled by inherited call or a child control
    Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    // Perform the same action as a click
    SetChecked(not Checked); // Toggles state, repaints, and fires OnCheckChanged

    // Trigger the standard OnClick event
    if Assigned(FOnClick) then
      FOnClick(Self);

    Key := 0; // Mark key as handled
  end;
end;

procedure TANDMR_TCheckBox.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPSolidBrush;
  LGPPen: TGPPen;
  LPoints: array of TGPPointF;
  BoxRect, InnerBoxRect: TGPRectF;
  CaptionRect: TRect;
  CheckBoxSquareSize: Integer;
  LCurrentBoxColor, LCurrentCheckMarkColor, LCurrentCaptionColor, LBoxBorderColor, LBoxFillColor: TColor;
  LIsHovering: Boolean;
  LHoverProgress: Single;
  LCaptionFont: TFont;
  Padding: Integer;
  CheckmarkThickness: Single;
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

    Padding := 2;
    CheckBoxSquareSize := Min(Self.Height - (Padding * 2), 18);
    if CheckBoxSquareSize < 10 then CheckBoxSquareSize := 10;

    BoxRect.InitializeBase(Padding, (Self.Height - CheckBoxSquareSize) / 2, CheckBoxSquareSize, CheckBoxSquareSize);
    CaptionRect := Rect(Round(BoxRect.X + BoxRect.Width + Padding), 0, Self.Width - Padding, Self.Height);

    LIsHovering := FInternalHoverSettings.Enabled and (FInternalHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FInternalHoverSettings.CurrentAnimationValue / 255.0;

    LCurrentBoxColor := IfThen(FChecked, FBoxColorChecked, FBoxColorUnchecked);
    LCurrentCheckMarkColor := FCheckMarkColor;
    LCurrentCaptionColor := FTitleFont.Color;

    if LIsHovering then
    begin
      if FInternalHoverSettings.BackgroundColor <> clNone then
        LCurrentBoxColor := BlendColors(LCurrentBoxColor, FInternalHoverSettings.BackgroundColor, LHoverProgress);
      if FInternalHoverSettings.FontColor <> clNone then
        LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, FInternalHoverSettings.FontColor, LHoverProgress);
    end;

    if not Self.Enabled then
    begin
      LCurrentBoxColor := BlendColors(LCurrentBoxColor, clGray, 0.60);
      LCurrentCheckMarkColor := BlendColors(LCurrentCheckMarkColor, clGray, 0.60);
      LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, clGray, 0.50);
    end;

    LBoxBorderColor := DarkerColor(LCurrentBoxColor, IfThen(LIsHovering and (FInternalHoverSettings.BorderColor = clNone), 15, 5));
    if LIsHovering and (FInternalHoverSettings.BorderColor <> clNone) then
        LBoxBorderColor := BlendColors(LBoxBorderColor, FInternalHoverSettings.BorderColor, LHoverProgress);

    LBoxFillColor := LCurrentBoxColor;

    ANDMR_ComponentUtils.DrawEditBox(LG, Canvas, BoxRect, LBoxFillColor, LBoxBorderColor, 1.5, psSolid, FCornerRadius, FRoundCornerType, 255, FTransparent, Parent.Color);

    if FChecked then
    begin
      CheckmarkThickness := Max(1.5, CheckBoxSquareSize / 8);
      InnerBoxRect := BoxRect;
      InnerBoxRect.Inflate(-CheckBoxSquareSize * 0.25, -CheckBoxSquareSize * 0.25);

      LGPPen := TGPPen.Create(ColorToARGB(LCurrentCheckMarkColor, 255), CheckmarkThickness);
      LGPPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);

      SetLength(LPoints, 3);
      LPoints[0] := MakePoint(InnerBoxRect.X + InnerBoxRect.Width * 0.15, InnerBoxRect.Y + InnerBoxRect.Height * 0.45);
      LPoints[1] := MakePoint(InnerBoxRect.X + InnerBoxRect.Width * 0.40, InnerBoxRect.Y + InnerBoxRect.Height * 0.75);
      LPoints[2] := MakePoint(InnerBoxRect.X + InnerBoxRect.Width * 0.85, InnerBoxRect.Y + InnerBoxRect.Height * 0.25);
      LG.DrawLines(LGPPen, LPoints);

      LGPPen.Free;
    end;

    if (FCaption <> '') and (CaptionRect.Right > CaptionRect.Left) then
    begin
      LCaptionFont := TFont.Create;
      try
        LCaptionFont.Assign(FTitleFont);
        LCaptionFont.Color := LCurrentCaptionColor;
        CaptionRect.Left := CaptionRect.Left + Padding;
        ANDMR_ComponentUtils.DrawComponentCaption(Self.Canvas, CaptionRect, FCaption, LCaptionFont, LCurrentCaptionColor, taLeftJustify, cvaCenter, False, 255);
      finally
        LCaptionFont.Free;
      end;
    end;

    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      LGPPath := TGPGraphicsPath.Create;
      try
        Dim CombinedRect: TGPRectF;
        CombinedRect.InitializeBase(BoxRect.X-1, BoxRect.Y-1, (CaptionRect.Right - BoxRect.X)+2, BoxRect.Height+2);
        if FCaption = '' then
           CombinedRect.InitializeBase(BoxRect.X-1, BoxRect.Y-1, BoxRect.Width+2, BoxRect.Height+2);

        LGPPath.AddRectangle(CombinedRect);
        LGPPen := TGPPen.Create(ColorToARGB(LCurrentCaptionColor, 180));
        LGPPen.SetDashStyle(DashStyleDash);
        LG.DrawPath(LGPPen, LGPPath);
      finally
        LGPPath.Free;
        LGPPen.Free;
      end;
    end;

  finally
    LG.Free;
  end;
end;

procedure TANDMR_TCheckBox.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

procedure TANDMR_TCheckBox.SetCheckMarkColor(const Value: TColor);
begin
  if FCheckMarkColor <> Value then
  begin
    FCheckMarkColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
end;

procedure TANDMR_TCheckBox.SetInternalHoverSettings(const Value: THoverSettings);
begin
  FInternalHoverSettings.Assign(Value);
end;

procedure TANDMR_TCheckBox.SetRoundCornerType(
  const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TANDMR_TCheckBox.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
    else
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetBoxColorChecked(const Value: TColor);
begin
  if FBoxColorChecked <> Value then
  begin
    FBoxColorChecked := Value;
    Invalidate;
  end;
end;

procedure TANDMR_TCheckBox.SetBoxColorUnchecked(const Value: TColor);
begin
  if FBoxColorUnchecked <> Value then
  begin
    FBoxColorUnchecked := Value;
    Invalidate;
  end;
end;

end.
