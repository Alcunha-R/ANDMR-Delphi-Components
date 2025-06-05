unit ANDMR_CRadioBox;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, System.Types, ANDMR_ComponentUtils;

type
  TANDMR_CRadioBoxStyle = (crsCustom, crsDefault, crsGlowingMark); // Added crsGlowingMark

  TRadioUserPropertyOverrides = record
    Transparent_IsSet: Boolean;
    RadioColorUnchecked_IsSet: Boolean;
    RadioColorChecked_IsSet: Boolean;
    MarkColor_IsSet: Boolean;
    BorderSettings_IsCustomized: Boolean;
    CaptionSettings_IsCustomized: Boolean;
    HoverSettings_IsCustomized: Boolean;
    OverallComponentBorder_IsCustomized: Boolean;
  end;

  TANDMR_CRadioBox = class(TCustomControl)
  private
    FChecked: Boolean;
    FCaptionSettings: TCaptionSettings;
    FRadioIndicatorBorderSettings: TBorderSettings;
    FOverallComponentBorder: TBorderSettings;
    FHoverSettings: THoverSettings;
    FRadioColorUnchecked: TColor;
    FRadioColorChecked: TColor;
    FMarkColor: TColor;
    FTransparent: Boolean;
    FCurrentStyle: TANDMR_CRadioBoxStyle;
    FApplyingStyle: Boolean;
    FUserOverrides: TRadioUserPropertyOverrides;
    FIsGroupHovered: Boolean;
    FGroupHoverCaptionBackgroundColor: TColor;
    FOnClick: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent;

    procedure SetChecked(const Value: Boolean);
    procedure SetIsGroupHovered(const Value: Boolean);
    procedure SetGroupHoverCaptionBackgroundColor(const Value: TColor);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetRadioIndicatorBorder(const Value: TBorderSettings);
    procedure SetOverallComponentBorder(const Value: TBorderSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetRadioColorUnchecked(const Value: TColor);
    procedure SetRadioColorChecked(const Value: TColor);
    procedure SetMarkColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetCurrentStyle(const Value: TANDMR_CRadioBoxStyle);
    procedure SetEnabled(Value: Boolean);

    procedure InitializeUserOverrides;
    procedure ApplyStyle(AStyle: TANDMR_CRadioBoxStyle);
    procedure SettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);

  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearUserOverrides;

  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property Caption: string read GetCaption write SetCaption;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property RadioIndicatorBorder: TBorderSettings read FRadioIndicatorBorderSettings write SetRadioIndicatorBorder;
    property OverallComponentBorder: TBorderSettings read FOverallComponentBorder write SetOverallComponentBorder;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

    property RadioColorUnchecked: TColor read FRadioColorUnchecked write SetRadioColorUnchecked default clWindow;
    property RadioColorChecked: TColor read FRadioColorChecked write SetRadioColorChecked default clHighlight;
    property MarkColor: TColor read FMarkColor write SetMarkColor default clWindowText;

    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property CurrentStyle: TANDMR_CRadioBoxStyle read FCurrentStyle write SetCurrentStyle default crsCustom;

    property Enabled;
    property TabStop default True;
    property TabOrder;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property ParentFont;
    property ParentShowHint;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;

    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;

    property IsGroupHovered: Boolean read FIsGroupHovered write SetIsGroupHovered;
    property GroupHoverCaptionBackgroundColor: TColor read FGroupHoverCaptionBackgroundColor write SetGroupHoverCaptionBackgroundColor;
  end;

procedure Register;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CRadioBox]);
end;

{ TANDMR_CRadioBox }

constructor TANDMR_CRadioBox.Create(AOwner: TComponent);
var
  TempCaptionFont: TFont;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csReplicatable, csCaptureMouse, csOpaque, csNeedsBorderPaint];
  Width := 120;
  Height := 24;
  TabStop := True;

  FChecked := False;
  FTransparent := False;
  FApplyingStyle := False;
  InitializeUserOverrides;
  FIsGroupHovered := False;
  FGroupHoverCaptionBackgroundColor := clNone;

  FRadioColorUnchecked := clWindow;
  FRadioColorChecked := clActiveCaption;
  FMarkColor := clWindowText;

  FRadioIndicatorBorderSettings := TBorderSettings.Create;
  FRadioIndicatorBorderSettings.OnChange := SettingsChanged;
  FRadioIndicatorBorderSettings.CornerRadius := Height div 2;
  FRadioIndicatorBorderSettings.RoundCornerType := rctAll;
  FRadioIndicatorBorderSettings.BackgroundColor := clNone;
  FRadioIndicatorBorderSettings.Color := clBlack;
  FRadioIndicatorBorderSettings.Thickness := 1;
  FRadioIndicatorBorderSettings.Style := psSolid;

  FOverallComponentBorder := TBorderSettings.Create;
  FOverallComponentBorder.OnChange := SettingsChanged;
  FOverallComponentBorder.CornerRadius := 3;
  FOverallComponentBorder.RoundCornerType := rctAll;
  FOverallComponentBorder.BackgroundColor := clNone;
  FOverallComponentBorder.Color := clGray;
  FOverallComponentBorder.Thickness := 1;
  FOverallComponentBorder.Visible := False;
  FOverallComponentBorder.Style := psSolid;

  TempCaptionFont := TFont.Create;
  try
    TempCaptionFont.Name := 'Segoe UI';
    TempCaptionFont.Size := 9;
    TempCaptionFont.Color := clWindowText;

    FCaptionSettings := TCaptionSettings.Create(Self);
    FCaptionSettings.OnChange := SettingsChanged;
    FCaptionSettings.Text := Name;
    FCaptionSettings.Font.Assign(TempCaptionFont);
  finally
    TempCaptionFont.Free;
  end;
  FCaptionSettings.Alignment := taLeftJustify;
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.DisabledColor := clGrayText;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHoverSettings.Enabled := True;
  FHoverSettings.BackgroundColor := clNone;
  FHoverSettings.BorderColor := clHighlight;

  FCurrentStyle := crsCustom;
  Cursor := crHandPoint;
  DoubleBuffered := True;
end;

destructor TANDMR_CRadioBox.Destroy;
begin
  if Assigned(FRadioIndicatorBorderSettings) then
  begin
    FRadioIndicatorBorderSettings.OnChange := nil;
    FreeAndNil(FRadioIndicatorBorderSettings);
  end;
  if Assigned(FOverallComponentBorder) then
  begin
    FOverallComponentBorder.OnChange := nil;
    FreeAndNil(FOverallComponentBorder);
  end;
  if Assigned(FCaptionSettings) then
  begin
    FCaptionSettings.OnChange := nil;
    FreeAndNil(FCaptionSettings);
  end;
  if Assigned(FHoverSettings) then
  begin
    FHoverSettings.OnChange := nil;
    FreeAndNil(FHoverSettings);
  end;
  inherited Destroy;
end;

procedure TANDMR_CRadioBox.InitializeUserOverrides;
begin
  FillChar(FUserOverrides, SizeOf(FUserOverrides), 0);
end;

procedure TANDMR_CRadioBox.ClearUserOverrides;
begin
  InitializeUserOverrides;
  if FCurrentStyle <> crsCustom then
    ApplyStyle(FCurrentStyle)
  else
    Invalidate;
end;

procedure TANDMR_CRadioBox.ApplyStyle(AStyle: TANDMR_CRadioBoxStyle);
var
  TempCaptionFont: TFont; // Added to define font properties for styles
begin
  if FApplyingStyle then Exit;
  if AStyle = crsCustom then
  begin
    Invalidate;
    Exit;
  end;

  FApplyingStyle := True;
  TempCaptionFont := TFont.Create; // Create instance
  try
    case AStyle of
      crsDefault:
      begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.RadioColorUnchecked_IsSet then Self.RadioColorUnchecked := clWindow;
        if not FUserOverrides.RadioColorChecked_IsSet then Self.RadioColorChecked := clActiveCaption; // Default VCL checked color
        if not FUserOverrides.MarkColor_IsSet then Self.MarkColor := clWindowText;

        if not FUserOverrides.BorderSettings_IsCustomized then
        begin
          Self.RadioIndicatorBorder.Color := clBlack;
          Self.RadioIndicatorBorder.Thickness := 1;
          Self.RadioIndicatorBorder.CornerRadius := 8;
          Self.RadioIndicatorBorder.RoundCornerType := rctAll;
          Self.RadioIndicatorBorder.Style := psSolid;
          Self.RadioIndicatorBorder.BackgroundColor := clNone;
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False;
          Self.OverallComponentBorder.Color := clGray;
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clWindowText; TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9; // Assign to TempFont
          Self.CaptionSettings.Font.Assign(TempCaptionFont); // Assign TempFont to Actual Font
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := BlendColors(Self.RadioColorUnchecked, clHighlight, 0.1);
          Self.HoverSettings.BorderColor := clHighlight;
          Self.HoverSettings.FontColor := clWindowText;
        end;
      end;
      crsGlowingMark: // New Style
      begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.RadioColorUnchecked_IsSet then Self.RadioColorUnchecked := clWindow;
        if not FUserOverrides.RadioColorChecked_IsSet then Self.RadioColorChecked := TColor($FF007AFF); // Vibrant blue for checked radio
        if not FUserOverrides.MarkColor_IsSet then Self.MarkColor := clWhite; // White mark for contrast

        if not FUserOverrides.BorderSettings_IsCustomized then
        begin
          Self.RadioIndicatorBorder.Color := TColor($00777777); // Neutral border
          Self.RadioIndicatorBorder.Thickness := 1;
          Self.RadioIndicatorBorder.CornerRadius := 8;
          Self.RadioIndicatorBorder.RoundCornerType := rctAll;
          Self.RadioIndicatorBorder.Style := psSolid;
          Self.RadioIndicatorBorder.BackgroundColor := clNone;
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False;
          Self.OverallComponentBorder.Color := clGray;
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clWindowText; TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := BlendColors(Self.RadioColorUnchecked, TColor($FF007AFF), 0.15);
          Self.HoverSettings.BorderColor := TColor($FF007AFF); // Border matches checked color on hover
          Self.HoverSettings.FontColor := clWindowText;
        end;
      end;
    end;
  finally
    TempCaptionFont.Free; // Free TempFont
    FApplyingStyle := False;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SettingsChanged(Sender: TObject);
begin
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    if Sender = FRadioIndicatorBorderSettings then
      FUserOverrides.BorderSettings_IsCustomized := True
    else if Sender = FOverallComponentBorder then
      FUserOverrides.OverallComponentBorder_IsCustomized := True
    else if Sender = FCaptionSettings then
      FUserOverrides.CaptionSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.HoverSettingsChanged(Sender: TObject);
begin
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    FUserOverrides.HoverSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetChecked(const Value: Boolean);
var
  I: Integer;
  Sibling: TControl;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;

    if FChecked then
    begin
      if Parent <> nil then
      begin
        for I := 0 to Parent.ControlCount - 1 do
        begin
          Sibling := Parent.Controls[I];
          if (Sibling is TANDMR_CRadioBox) and (Sibling <> Self) then
          begin
            TANDMR_CRadioBox(Sibling).SetChecked(False);
          end;
        end;
      end;
    end;

    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

function TANDMR_CRadioBox.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure TANDMR_CRadioBox.SetCaption(const Value: string);
begin
  if FCaptionSettings.Text <> Value then
  begin
    FCaptionSettings.Text := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    FUserOverrides.CaptionSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetRadioIndicatorBorder(const Value: TBorderSettings);
begin
  FRadioIndicatorBorderSettings.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    FUserOverrides.BorderSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetOverallComponentBorder(const Value: TBorderSettings);
begin
  FOverallComponentBorder.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    FUserOverrides.OverallComponentBorder_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := crsCustom;
    FUserOverrides.HoverSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetRadioColorUnchecked(const Value: TColor);
begin
  if FRadioColorUnchecked <> Value then
  begin
    FRadioColorUnchecked := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := crsCustom;
      FUserOverrides.RadioColorUnchecked_IsSet := True;
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetRadioColorChecked(const Value: TColor);
begin
  if FRadioColorChecked <> Value then
  begin
    FRadioColorChecked := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := crsCustom;
      FUserOverrides.RadioColorChecked_IsSet := True;
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetMarkColor(const Value: TColor);
begin
  if FMarkColor <> Value then
  begin
    FMarkColor := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := crsCustom;
      FUserOverrides.MarkColor_IsSet := True;
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
    else
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

    if not FApplyingStyle then
    begin
      FCurrentStyle := crsCustom;
      FUserOverrides.Transparent_IsSet := True;
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetCurrentStyle(const Value: TANDMR_CRadioBoxStyle);
begin
  if Value = crsCustom then
  begin
    if FCurrentStyle <> crsCustom then
    begin
      FCurrentStyle := crsCustom;
      Invalidate;
    end;
    Exit;
  end;

  if (FCurrentStyle <> Value) or (FCurrentStyle = crsCustom) then
  begin
    InitializeUserOverrides;
    FCurrentStyle := Value;
    ApplyStyle(Value);
  end
  else
  begin
     InitializeUserOverrides;
     ApplyStyle(Value);
  end;
end;

procedure TANDMR_CRadioBox.SetEnabled(Value: Boolean);
begin
  if inherited Enabled <> Value then
  begin
    inherited SetEnabled(Value);
  end;
end;

procedure TANDMR_CRadioBox.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPBrush;
  LGPPen: TGPPen;
  RadioOuterRect: TGPRectF;
  RadioInnerRect: TGPRectF;
  RadioDrawTRect: TRect;
  CaptionPaintRect: TRect;
  RadioDiameter, ActualRadioDiameter: Integer;
  Padding: Integer;
  MarkDiameter: Integer;
  LCurrentRadioColorUnchecked, LCurrentRadioColorChecked, LCurrentMarkColor, LCurrentCaptionColor, LRadioFillColorToUse: TColor;
  LCurrentRadioBorderColorToUse: TColor;
  LIsHovering: Boolean;
  LHoverProgress: Single;
  ActualCaptionFont: TFont;
  LClientRectF: TGPRectF;
  TextMargin: Integer;
  EffectiveBorderThickness: Integer;

  OverallBorderPaintRect: TRect;
  ContentPaintRect: TRect;
  LOverallBorderColor: TColor;
  GlowPath: TGPGraphicsPath; // For glowing mark
  GlowBrush: TGPSolidBrush;  // For glowing mark
  GlowRect: TGPRectF;        // For glowing mark
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

    Padding := 2;
    TextMargin := 4;

    // --- 1. Component Background ---
    if not FTransparent then
    begin
      var BackgroundBrush: TGPSolidBrush;
      var ComponentBGColor: TColor;
      if FRadioIndicatorBorderSettings.BackgroundColor <> clNone then
        ComponentBGColor := FRadioIndicatorBorderSettings.BackgroundColor
      else
        ComponentBGColor := Self.Color;

      LClientRectF.X := ClientRect.Left;
      LClientRectF.Y := ClientRect.Top;
      LClientRectF.Width := ClientRect.Width;
      LClientRectF.Height := ClientRect.Height;
      BackgroundBrush := TGPSolidBrush.Create(ColorToARGB(ComponentBGColor, 255));
      try
        LG.FillRectangle(BackgroundBrush, LClientRectF);
      finally
        BackgroundBrush.Free;
      end;
    end;

    // --- 1.5 Overall Component Border ---
    OverallBorderPaintRect := ClientRect;
    ContentPaintRect := ClientRect;

    LIsHovering := FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    if FOverallComponentBorder.Visible and (FOverallComponentBorder.Thickness > 0) then
    begin
      LOverallBorderColor := FOverallComponentBorder.Color;
      if not Self.Enabled then
      begin
        LOverallBorderColor := BlendColors(LOverallBorderColor, clGray, 0.60);
      end;

      ANDMR_ComponentUtils.DrawEditBox(
        LG, OverallBorderPaintRect, clNone,
        LOverallBorderColor, FOverallComponentBorder.Thickness,
        FOverallComponentBorder.Style, FOverallComponentBorder.CornerRadius,
        FOverallComponentBorder.RoundCornerType, 255
      );
      InflateRect(ContentPaintRect, -FOverallComponentBorder.Thickness, -FOverallComponentBorder.Thickness);
    end;

    // --- 2. Determine Radio Element Size & Position ---
    EffectiveBorderThickness := FRadioIndicatorBorderSettings.Thickness;
    RadioDiameter := Max(8, ContentPaintRect.Height - (Padding * 2));
    ActualRadioDiameter := RadioDiameter - EffectiveBorderThickness;
    if Odd(ActualRadioDiameter) then Dec(ActualRadioDiameter);

    RadioOuterRect.Width  := ActualRadioDiameter;
    RadioOuterRect.Height := ActualRadioDiameter;
    RadioOuterRect.X      := ContentPaintRect.Left + Padding + (EffectiveBorderThickness div 2);
    RadioOuterRect.Y      := ContentPaintRect.Top + (ContentPaintRect.Height - ActualRadioDiameter) / 2;

    // --- 3. Calculate Caption Paint Rect ---
    CaptionPaintRect := ContentPaintRect;
    CaptionPaintRect.Left := Round(RadioOuterRect.X - ContentPaintRect.Left + ActualRadioDiameter + (EffectiveBorderThickness div 2) + TextMargin) + ContentPaintRect.Left;
    CaptionPaintRect.Top    := ContentPaintRect.Top + Padding;
    CaptionPaintRect.Bottom := ContentPaintRect.Top + ContentPaintRect.Height - Padding;
    CaptionPaintRect.Right  := ContentPaintRect.Left + ContentPaintRect.Width - Padding;
    if CaptionPaintRect.Right < CaptionPaintRect.Left then CaptionPaintRect.Right := CaptionPaintRect.Left;
    if CaptionPaintRect.Bottom < CaptionPaintRect.Top then CaptionPaintRect.Bottom := CaptionPaintRect.Top;

    // --- 4. Determine Colors and Font ---
    LCurrentRadioColorUnchecked := Self.FRadioColorUnchecked;
    LCurrentRadioColorChecked   := Self.FRadioColorChecked;
    LCurrentMarkColor           := Self.FMarkColor;
    LCurrentRadioBorderColorToUse := Self.FRadioIndicatorBorderSettings.Color;

    ActualCaptionFont := TFont.Create;
    try
      ActualCaptionFont.Assign(FCaptionSettings.Font);
      LCurrentCaptionColor := ActualCaptionFont.Color;

      if LIsHovering then
      begin
        if FHoverSettings.BackgroundColor <> clNone then
        begin
          LCurrentRadioColorUnchecked := BlendColors(LCurrentRadioColorUnchecked, FHoverSettings.BackgroundColor, LHoverProgress);
          LCurrentRadioColorChecked   := BlendColors(LCurrentRadioColorChecked,   FHoverSettings.BackgroundColor, LHoverProgress);
        end;
        if FHoverSettings.FontColor <> clNone then
          LCurrentCaptionColor := BlendColors(ActualCaptionFont.Color, FHoverSettings.FontColor, LHoverProgress);
        if FHoverSettings.BorderColor <> clNone then
           LCurrentRadioBorderColorToUse := BlendColors(LCurrentRadioBorderColorToUse, FHoverSettings.BorderColor, LHoverProgress);
      end;

      if not Self.Enabled then
      begin
        LCurrentRadioColorUnchecked := BlendColors(LCurrentRadioColorUnchecked, clGray, 0.65);
        LCurrentRadioColorChecked   := BlendColors(LCurrentRadioColorChecked,   clGray, 0.65);
        LCurrentMarkColor           := BlendColors(LCurrentMarkColor,           clGray, 0.65);
        LCurrentRadioBorderColorToUse := BlendColors(LCurrentRadioBorderColorToUse, clGray, 0.60);

        if FCaptionSettings.DisabledColor <> clNone then
          LCurrentCaptionColor := FCaptionSettings.DisabledColor
        else
          LCurrentCaptionColor := BlendColors(ActualCaptionFont.Color, clGray, 0.50);
      end;
    finally
      // ActualCaptionFont is freed later
    end;
    ActualCaptionFont.Color := LCurrentCaptionColor;

    // --- 5. Draw Radio Element (Circle and Mark) ---
    if FChecked then
      LRadioFillColorToUse := LCurrentRadioColorChecked
    else
      LRadioFillColorToUse := LCurrentRadioColorUnchecked;

    RadioDrawTRect := System.Types.Rect(
      Round(RadioOuterRect.X), Round(RadioOuterRect.Y),
      Round(RadioOuterRect.X + RadioOuterRect.Width), Round(RadioOuterRect.Y + RadioOuterRect.Height)
    );

    ANDMR_ComponentUtils.DrawEditBox(
      LG, RadioDrawTRect, LRadioFillColorToUse, LCurrentRadioBorderColorToUse,
      EffectiveBorderThickness,
      FRadioIndicatorBorderSettings.Style,
      ActualRadioDiameter div 2,
      FRadioIndicatorBorderSettings.RoundCornerType,
      255
    );

    if FChecked then
    begin
      MarkDiameter := Max(2, ActualRadioDiameter div 2);
      if Odd(MarkDiameter) and (ActualRadioDiameter > 4) then Dec(MarkDiameter);

      RadioInnerRect.Width  := MarkDiameter;
      RadioInnerRect.Height := MarkDiameter;
      RadioInnerRect.X      := RadioOuterRect.X + (ActualRadioDiameter - MarkDiameter) / 2;
      RadioInnerRect.Y      := RadioOuterRect.Y + (ActualRadioDiameter - MarkDiameter) / 2;

      // Glowing Mark Effect for crsGlowingMark style
      if (FCurrentStyle = crsGlowingMark) and Self.Enabled then
      begin
        GlowRect := RadioInnerRect;
//        GlowRect.Inflate(MarkDiameter * 0.5, MarkDiameter * 0.5); // Glow size relative to mark size

        GlowPath := TGPGraphicsPath.Create;
        try
          GlowPath.AddEllipse(GlowRect);
          GlowBrush := TGPSolidBrush.Create(ColorToARGB(BlendColors(LCurrentMarkColor, clWhite, 0.6), 120)); // Semi-transparent glow
          try
            LG.FillPath(GlowBrush, GlowPath);
          finally
            GlowBrush.Free;
          end;
        finally
          GlowPath.Free;
        end;
      end;

      LGPBrush := TGPSolidBrush.Create(ColorToARGB(LCurrentMarkColor, 255));
      try
         LG.FillEllipse(LGPBrush, RadioInnerRect.X, RadioInnerRect.Y, RadioInnerRect.Width, RadioInnerRect.Height);
      finally
        LGPBrush.Free;
      end;
    end;

    // --- 6. Draw Caption ---
    if FIsGroupHovered and (FGroupHoverCaptionBackgroundColor <> clNone) then
    begin
      var TempBrush: TGPSolidBrush;
      TempBrush := TGPSolidBrush.Create(ColorToARGB(FGroupHoverCaptionBackgroundColor, 255));
      try
        var FillRectF: TGPRectF;
        FillRectF.X := CaptionPaintRect.Left; FillRectF.Y := CaptionPaintRect.Top;
        FillRectF.Width := CaptionPaintRect.Width; FillRectF.Height := CaptionPaintRect.Height;
        LG.FillRectangle(TempBrush, FillRectF);
      finally
        TempBrush.Free;
      end;
    end;

    if (FCaptionSettings.Text <> '') and (CaptionPaintRect.Right > CaptionPaintRect.Left) and (CaptionPaintRect.Bottom > CaptionPaintRect.Top) then
    begin
      ANDMR_ComponentUtils.DrawComponentCaption(
        Self.Canvas,
        CaptionPaintRect,
        FCaptionSettings.Text,
        ActualCaptionFont,
        ActualCaptionFont.Color,
        FCaptionSettings.Alignment,
        FCaptionSettings.VerticalAlignment,
        FCaptionSettings.WordWrap,
        255
      );
    end;
    ActualCaptionFont.Free;

    // --- 7. Draw Focus Rectangle ---
    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      var FocusBoundsTRect: TRect;
      FocusBoundsTRect.Left   := ContentPaintRect.Left + Padding div 2;
      FocusBoundsTRect.Top    := ContentPaintRect.Top + Padding div 2;
      FocusBoundsTRect.Right  := ContentPaintRect.Left + ContentPaintRect.Width - Padding div 2;
      FocusBoundsTRect.Bottom := ContentPaintRect.Top + ContentPaintRect.Height - Padding div 2;

      if FocusBoundsTRect.Right < FocusBoundsTRect.Left then FocusBoundsTRect.Right := FocusBoundsTRect.Left;
      if FocusBoundsTRect.Bottom < FocusBoundsTRect.Top then FocusBoundsTRect.Bottom := FocusBoundsTRect.Top;

      if (FocusBoundsTRect.Right > FocusBoundsTRect.Left) and (FocusBoundsTRect.Bottom > FocusBoundsTRect.Top) then
         Self.Canvas.DrawFocusRect(FocusBoundsTRect);
      end;

  finally
    LG.Free;
  end;
end;

procedure TANDMR_CRadioBox.Click;
begin
  if not Enabled then Exit;
  if not FChecked then
  begin
    SetChecked(True);
  end;
  inherited Click;
end;

procedure TANDMR_CRadioBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = 0 then Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    if not FChecked then
    begin
      Click;
    end;
    Key := 0;
  end;
end;

procedure TANDMR_CRadioBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Self.Enabled and Assigned(FHoverSettings) and FHoverSettings.Enabled then
  begin
    FHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_CRadioBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FHoverSettings) then
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CRadioBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    if Assigned(FHoverSettings) then FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

procedure TANDMR_CRadioBox.SetIsGroupHovered(const Value: Boolean);
begin
  if FIsGroupHovered <> Value then
  begin
    FIsGroupHovered := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CRadioBox.SetGroupHoverCaptionBackgroundColor(const Value: TColor);
begin
  if FGroupHoverCaptionBackgroundColor <> Value then
  begin
    FGroupHoverCaptionBackgroundColor := Value;
    Invalidate;
  end;
end;

end.
