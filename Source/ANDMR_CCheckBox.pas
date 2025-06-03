unit ANDMR_CCheckBox;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, // For crHandPoint, Messages, VK_SPACE
  System.Types, // For TRect, TPoint, etc.
  ANDMR_ComponentUtils;

type
  TCheckBoxState = (csUnchecked, csChecked, csIndeterminate);

  TANDMR_CCheckBoxStyle = (cbsCustom, cbsLight, cbsDark, cbsMaterial, cbsFlat, cbsModern,
                           cbsGhost, cbsFaded, cbsBordered, cbsIOS, cbsWin11); // Added new styles

  TANDMR_CCheckBox = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings;
    FCaptionSettings: TCaptionSettings;
    FState: TCheckBoxState;
    FBoxColorUnchecked: TColor;
    FBoxColorChecked: TColor;
    FCheckMarkColor: TColor;
    FTransparent: Boolean;
    FOnClick: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent;
    FHoverSettings: THoverSettings;
    FCurrentStyle: TANDMR_CCheckBoxStyle;
    FApplyingStyle: Boolean; // Flag to prevent recursion when applying styles

    procedure SetBorderSettings(const Value: TBorderSettings);
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure SetState(const Value: TCheckBoxState);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetBoxColorUnchecked(const Value: TColor);
    procedure SetBoxColorChecked(const Value: TColor);
    procedure SetCheckMarkColor(const Value: TColor);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetTransparent(const Value: Boolean);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetEnabled(Value: Boolean);
    procedure SetCurrentStyle(const Value: TANDMR_CCheckBoxStyle);
    procedure ApplyStyle(AStyle: TANDMR_CCheckBoxStyle);

    procedure HoverSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); // Handles changes from FBorderSettings, FCaptionSettings

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

    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

  published
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write SetState;
    property Caption: string read GetCaption write SetCaption;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property BoxColorUnchecked: TColor read FBoxColorUnchecked write SetBoxColorUnchecked default clWindow;
    property BoxColorChecked: TColor read FBoxColorChecked write SetBoxColorChecked default clHighlight;
    property CheckMarkColor: TColor read FCheckMarkColor write SetCheckMarkColor default clWindowText;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property CurrentStyle: TANDMR_CCheckBoxStyle read FCurrentStyle write SetCurrentStyle default cbsCustom;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;

    // Standard properties
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font; // Keep standard Font property for VCL designer, even if CaptionSettings.Font is primary
    property ParentFont;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes, Vcl.Consts; // Added Vcl.Consts for color names if needed

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CCheckBox]);
end;

{ TANDMR_CCheckBox }

constructor TANDMR_CCheckBox.Create(AOwner: TComponent);
var
  TempTitleFont: TFont;
begin
  inherited Create(AOwner);

  FApplyingStyle := False; // Initialize flag

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csReplicatable, csCaptureMouse, csNeedsBorderPaint, csAcceptsControls];
  Width := 120;
  Height := 24;

  // Initialize properties that will be managed by styles or direct setting
  // These are the 'cbsCustom' defaults if no style is applied initially or if CurrentStyle is cbsCustom
  FState := csUnchecked;
  FBoxColorUnchecked := clWindow;
  FBoxColorChecked := clHighlight;
  FCheckMarkColor := clWindowText;
  FTransparent := False;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 3;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.BackgroundColor := clNone;
  FBorderSettings.Color := clBlack;
  FBorderSettings.Thickness := 1;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHoverSettings.BackgroundColor := clNone;
  FHoverSettings.BorderColor := clNone;
  FHoverSettings.FontColor := clNone;
  FHoverSettings.Enabled := True;

  TempTitleFont := TFont.Create;
  try
    TempTitleFont.Name := 'Segoe UI';
    TempTitleFont.Size := 9;
    TempTitleFont.Color := clWindowText;

    FCaptionSettings := TCaptionSettings.Create(Self);
    FCaptionSettings.OnChange := SettingsChanged;
    FCaptionSettings.Text := Name;
    FCaptionSettings.Font.Assign(TempTitleFont);
  finally
    TempTitleFont.Free;
  end;
  FCaptionSettings.Alignment := taLeftJustify;
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.DisabledColor := clGrayText; // Default disabled color

  // Set initial style - cbsCustom means the above values are used.
  // If you wanted a different default style, you would set FCurrentStyle and then call ApplyStyle.
  FCurrentStyle := cbsCustom; // Default style
  // Example: To make cbsLight the default style on creation:
  // FCurrentStyle := cbsLight;
  // ApplyStyle(FCurrentStyle); // Call this *after* all sub-objects (FBorderSettings etc.) are created.

  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

  TabStop := True;
  Cursor := crHandPoint;
  DoubleBuffered := True;
  Enabled := True; // Default to enabled
end;

destructor TANDMR_CCheckBox.Destroy;
begin
  if Assigned(FBorderSettings) then
  begin
    FBorderSettings.OnChange := nil;
    FBorderSettings.Free;
    FBorderSettings := nil;
  end;

  if Assigned(FHoverSettings) then
  begin
    FHoverSettings.OnChange := nil;
    FHoverSettings.Free;
    FHoverSettings := nil;
  end;

  if Assigned(FCaptionSettings) then
  begin
    FCaptionSettings.OnChange := nil;
    FCaptionSettings.Free;
    FCaptionSettings := nil;
  end;

  inherited Destroy;
end;

procedure TANDMR_CCheckBox.ApplyStyle(AStyle: TANDMR_CCheckBoxStyle);
begin
  if FApplyingStyle then Exit; // Prevent re-entrancy

  FApplyingStyle := True;
  try
    // Note: Setters for individual properties (like SetBoxColorChecked)
    // will check FApplyingStyle. If True, they won't change FCurrentStyle to cbsCustom.

    case AStyle of
      cbsCustom: begin
        // When style is set to Custom, we don't change existing properties.
        // The component reflects the manually set values.
      end;
      cbsLight: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := clWindow;
        Self.BoxColorChecked := clHighlight;
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := clBlack;
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 3;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clWindow;

        Self.CaptionSettings.Font.Color := clWindowText;
        Self.CaptionSettings.Font.Name := 'Segoe UI';
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := clGrayText;

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := BlendColors(clWindow, clHighlight, 0.1);
        Self.HoverSettings.BorderColor := clHighlight;
        Self.HoverSettings.FontColor := clWindowText;
      end;
      cbsDark: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := TColor($00555555);
        Self.BoxColorChecked := TColor($000099FF);
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($00777777);
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 3;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := TColor($00333333);

        Self.CaptionSettings.Font.Color := clWhite;
        Self.CaptionSettings.Font.Name := 'Segoe UI';
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := TColor($00888888);

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($00666666);
        Self.HoverSettings.BorderColor := TColor($0033CCFF);
        Self.HoverSettings.FontColor := clWhite;
      end;
      cbsMaterial: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := clWhite;
        Self.BoxColorChecked := TColor($FF2196F3); // Material Blue 500
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($FFBDBDBD); // Grey 400
        Self.BorderSettings.Thickness := 2;
        Self.BorderSettings.CornerRadius := 2;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clNone;

        Self.CaptionSettings.Font.Color := TColor($DE000000); // Black 87%
        Self.CaptionSettings.Font.Name := 'Roboto';
        Self.CaptionSettings.Font.Size := 10;
        Self.CaptionSettings.DisabledColor := TColor($61000000); // Black 38%

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($1F000000); // Black 12% overlay
        Self.HoverSettings.BorderColor := TColor($FF2196F3);
        Self.HoverSettings.FontColor := clNone;
      end;
      cbsFlat: begin
        Self.Transparent := True;
        Self.BoxColorUnchecked := TColor($00E0E0E0);
        Self.BoxColorChecked := TColor($00757575);
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($00AAAAAA);
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 0;
        Self.BorderSettings.RoundCornerType := rctNone;
        Self.BorderSettings.BackgroundColor := clNone;

        Self.CaptionSettings.Font.Color := clWindowText;
        Self.CaptionSettings.Font.Name := 'Segoe UI';
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := clGrayText;

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($00D0D0D0);
        Self.HoverSettings.BorderColor := TColor($00757575);
        Self.HoverSettings.FontColor := clNone;
      end;
      cbsModern: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := TColor($00F0F2F5);
        Self.BoxColorChecked := TColor($FF007AFF); // System Blue
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($00D1D1D6);
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 5;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clNone;

        Self.CaptionSettings.Font.Color := TColor($FF1D1D1F);
        Self.CaptionSettings.Font.Name := 'Segoe UI';
        Self.CaptionSettings.Font.Size := 10;
        Self.CaptionSettings.DisabledColor := TColor($FFBCBCBF);

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := BlendColors(FBoxColorUnchecked, FBoxColorChecked, 0.1);
        Self.HoverSettings.BorderColor := TColor($FF007AFF);
        Self.HoverSettings.FontColor := clNone;
      end;
      cbsGhost: begin
        Self.Transparent := True; // Key feature of ghost style
        Self.BoxColorUnchecked := clNone; // No fill for the box initially
        Self.BoxColorChecked := clNone;   // No fill when checked either, relies on checkmark
        Self.CheckMarkColor := clGrayText; // Or a theme accent color

        Self.BorderSettings.Color := clSilver; // Light border for the box
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 3;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clNone; // Control background is transparent

        Self.CaptionSettings.Font.Color := clGrayText; // Muted caption
        Self.CaptionSettings.Font.Name := 'Segoe UI';
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := BlendColors(clSilver, clGray, 0.5);

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($1A808080); // Very subtle gray fill on hover (10% opacity gray)
        Self.HoverSettings.BorderColor := clGray; // Border becomes more prominent
        Self.HoverSettings.FontColor := clWindowText; // Caption becomes more prominent
      end;
      cbsFaded: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := TColor($00F5F5F5); // Very light gray
        Self.BoxColorChecked := TColor($00E0E0E0);   // Slightly darker light gray
        Self.CheckMarkColor := TColor($00A0A0A0);   // Medium gray checkmark

        Self.BorderSettings.Color := TColor($00DCDCDC); // Light gray border
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 3;
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := TColor($00FAFAFA); // Almost white background

        Self.CaptionSettings.Font.Color := TColor($00B0B0B0); // Light gray caption
        Self.CaptionSettings.Font.Name := 'Segoe UI Light'; // Lighter font variant
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := TColor($00E0E0E0);

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($00E8E8E8); // Slightly darker on hover
        Self.HoverSettings.BorderColor := TColor($00C8C8C8);
        Self.HoverSettings.FontColor := TColor($00909090);
      end;
      cbsBordered: begin
        Self.Transparent := True; // Often used with prominent borders
        Self.BoxColorUnchecked := clNone; // No box fill
        Self.BoxColorChecked := clNone;   // Checkmark provides visual cue
        Self.CheckMarkColor := clWindowText;

        Self.BorderSettings.Color := clBlack; // Strong border color
        Self.BorderSettings.Thickness := 2;   // Thicker border
        Self.BorderSettings.CornerRadius := 1; // Sharper corners
        Self.BorderSettings.RoundCornerType := rctAll; // Or rctNone for very sharp
        Self.BorderSettings.BackgroundColor := clNone; // Control background transparent

        Self.CaptionSettings.Font.Color := clWindowText;
        Self.CaptionSettings.Font.Name := 'Segoe UI Semibold';
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := clGrayText;

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($0D000000); // Slight dark fill (5% black)
        Self.HoverSettings.BorderColor := clHighlight; // Border changes to accent on hover
        Self.HoverSettings.FontColor := clNone;
      end;
      cbsIOS: begin
        Self.Transparent := False; // iOS controls typically have a solid feel
        Self.BoxColorUnchecked := clWhite;
        Self.BoxColorChecked := TColor($FF34C759); // iOS Green
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($FFC7C7CC); // iOS Separator Gray
        Self.BorderSettings.Thickness := 1; // Or 1.5 for retina-like sharpness if using GDI+ carefully
        Self.BorderSettings.CornerRadius := 10; // Highly rounded, almost circular for small boxes
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clNone; // Let control background be default or parent

        Self.CaptionSettings.Font.Color := clBlack;
        Self.CaptionSettings.Font.Name := 'San Francisco'; // Ideal, fallback to Segoe UI
        Self.CaptionSettings.Font.Size := 10; // iOS uses slightly larger base sizes
        Self.CaptionSettings.DisabledColor := TColor($FFBCBCBF); // iOS Disabled Text Gray

        Self.HoverSettings.Enabled := True;
        // iOS doesn't have strong hover states like desktop, more about touch feedback (not directly applicable here)
        // Subtle brightness change for desktop mimicry
        Self.HoverSettings.BackgroundColor := BlendColors(FBoxColorUnchecked, clGray, 0.05);
        Self.HoverSettings.BorderColor := TColor($FFAEAEB2); // Slightly darker gray
        Self.HoverSettings.FontColor := clNone;
      end;
      cbsWin11: begin
        Self.Transparent := False;
        Self.BoxColorUnchecked := TColor($FFF9F9F9); // Off-white, very light gray
        Self.BoxColorChecked := TColor($FF0078D4);   // Windows Accent Blue
        Self.CheckMarkColor := clWhite;

        Self.BorderSettings.Color := TColor($FFD6D6D6); // Subtle border
        Self.BorderSettings.Thickness := 1;
        Self.BorderSettings.CornerRadius := 4; // Win11 uses soft rounded corners
        Self.BorderSettings.RoundCornerType := rctAll;
        Self.BorderSettings.BackgroundColor := clNone; // Usually transparent background for the control itself

        Self.CaptionSettings.Font.Color := TColor($E4000000); // Near black (89% opacity)
        Self.CaptionSettings.Font.Name := 'Segoe UI Variable Text'; // Ideal, fallback to Segoe UI
        Self.CaptionSettings.Font.Size := 9;
        Self.CaptionSettings.DisabledColor := TColor($60000000); // Disabled (38% opacity)

        Self.HoverSettings.Enabled := True;
        Self.HoverSettings.BackgroundColor := TColor($FFF0F0F0); // Slightly darker on hover for unchecked
        Self.HoverSettings.BorderColor := TColor($FFB0B0B0); // Border darkens slightly
        // For checked state hover, the fill might darken slightly, or an outer glow (harder without more effects)
        // Here, we'll just ensure the border hover is consistent.
        Self.HoverSettings.FontColor := clNone;
      end;
    end;
  finally
    FApplyingStyle := False;
  end;
  Invalidate; // Repaint after applying any style (except custom if it means no change)
end;

procedure TANDMR_CCheckBox.SetCurrentStyle(const Value: TANDMR_CCheckBoxStyle);
begin
  if FCurrentStyle <> Value then
  begin
    FCurrentStyle := Value;
    ApplyStyle(Value); // This will also Invalidate
    // If Value is cbsCustom, ApplyStyle does nothing, but FCurrentStyle is now cbsCustom.
    // This is correct if user explicitly selects "Custom" from a list.
  end;
end;

procedure TANDMR_CCheckBox.SettingsChanged(Sender: TObject); // For FBorderSettings, FCaptionSettings
begin
  if not FApplyingStyle and (FCurrentStyle <> cbsCustom) then
  begin
    FCurrentStyle := cbsCustom; // Mark as custom if a sub-setting is changed manually
                                // No need to call ApplyStyle(cbsCustom) as it's a no-op
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.HoverSettingsChanged(Sender: TObject);
begin
  if not FApplyingStyle and (FCurrentStyle <> cbsCustom) then
  begin
    // If hover settings are considered part of the main style, then changing them makes it custom.
    FCurrentStyle := cbsCustom;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  // SettingsChanged will be called by FBorderSettings.OnChange if assigned properly
  // If not, then manual call to SettingsChanged or direct logic here:
  if not FApplyingStyle then FCurrentStyle := cbsCustom;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  // SettingsChanged will be called by FCaptionSettings.OnChange
  if not FApplyingStyle then FCurrentStyle := cbsCustom;
  Invalidate;
end;

procedure TANDMR_CCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TANDMR_CCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Self.Enabled and Assigned(FHoverSettings) and FHoverSettings.Enabled then
  begin
    FHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_CCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FHoverSettings) then
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CCheckBox.Click;
begin
  if not Enabled then Exit;

  case FState of
    csUnchecked: SetState(csChecked);
    csChecked: SetState(csUnchecked);
    csIndeterminate: SetState(csChecked);
  end;

  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TANDMR_CCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = 0 then Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    Click; // Simulate a click which handles state change and OnClick event
    Key := 0;
  end;
end;

procedure TANDMR_CCheckBox.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPSolidBrush;
  LGPPen: TGPPen;
  LPoints: array of TGPPointF;
  BoxRect: TGPRectF;
  BoxDrawRect: TRect;
  CaptionRect: TRect;
  CheckBoxSquareSize: Integer;
  LCurrentBoxColorUnchecked, LCurrentBoxColorChecked, LCurrentCheckMarkColor, LCurrentCaptionColor, LBoxFillColor: TColor; // LBoxBorderColor removed, using ActualBorderColor
  LIsHovering: Boolean;
  LHoverProgress: Single;
  LCaptionFont: TFont; // Used for assigning from FCaptionSettings.Font
  Padding: Integer;
  CheckmarkThickness: Single;
  CombinedRect: TGPRectF;
  ActualBorderColor: TColor; // ActualBorderColor for the box border
  // ActualBackgroundColor, ActualCaptionFontColor removed as LCurrent... serve their purpose
  ActualBoxBorderThickness: Integer;
  ActualCornerRadius: Integer;
  ActualRoundCornerType: TRoundCornerType;
  ActualCaptionFont: TFont; // Font object used for drawing caption
  LClientRectF: TGPRectF; // For GDI+ FillRectangle
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

    Padding := 2;
    CheckBoxSquareSize := Min(Self.Height - (Padding * 2), 18);
    if CheckBoxSquareSize < 10 then CheckBoxSquareSize := 10;

    BoxRect.X := Padding;
    BoxRect.Y := (Self.Height - CheckBoxSquareSize) / 2;
    BoxRect.Width := CheckBoxSquareSize;
    BoxRect.Height := CheckBoxSquareSize;

    CaptionRect := Rect(Round(BoxRect.X + BoxRect.Width + Padding), 0, Self.Width - Padding, Self.Height);

    LIsHovering := FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    // Start with base colors from properties
    LCurrentBoxColorUnchecked := FBoxColorUnchecked;
    LCurrentBoxColorChecked := FBoxColorChecked;
    LCurrentCheckMarkColor := FCheckMarkColor;

    ActualCaptionFont := TFont.Create; // Font for drawing
    LCaptionFont := TFont.Create; // Temporary font for initial settings
    try
      LCaptionFont.Assign(FCaptionSettings.Font); // Base font settings from component property
      ActualCaptionFont.Assign(LCaptionFont);    // Initialize ActualCaptionFont with base settings
      LCurrentCaptionColor := LCaptionFont.Color; // Base caption color from font settings

      // Determine actual colors and settings based on hover and enabled state
      if LIsHovering then
      begin
        // Hover background color for the checkbox square itself
        if FHoverSettings.BackgroundColor <> clNone then
        begin
          LCurrentBoxColorUnchecked := BlendColors(LCurrentBoxColorUnchecked, FHoverSettings.BackgroundColor, LHoverProgress);
          LCurrentBoxColorChecked := BlendColors(LCurrentBoxColorChecked, FHoverSettings.BackgroundColor, LHoverProgress);
        end;
        // Hover font color for the caption
        if FHoverSettings.FontColor <> clNone then
          LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, FHoverSettings.FontColor, LHoverProgress);

        // Determine the box border color for hover state
        if FHoverSettings.BorderColor <> clNone then
            ActualBorderColor := BlendColors(FBorderSettings.Color, FHoverSettings.BorderColor, LHoverProgress)
        else // No specific hover border color, use default border color (might be slightly modified if needed)
            ActualBorderColor := FBorderSettings.Color;
      end
      else // Not hovering
      begin
        ActualBorderColor := FBorderSettings.Color; // Default box border color
      end;

      if not Self.Enabled then
      begin
        LCurrentBoxColorUnchecked := BlendColors(LCurrentBoxColorUnchecked, clGray, 0.60);
        LCurrentBoxColorChecked := BlendColors(LCurrentBoxColorChecked, clGray, 0.60);
        LCurrentCheckMarkColor := BlendColors(LCurrentCheckMarkColor, clGray, 0.60);

        if FCaptionSettings.DisabledColor <> clNone then
          LCurrentCaptionColor := FCaptionSettings.DisabledColor
        else
          LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, clGray, 0.50); // Default graying for caption
        ActualBorderColor := BlendColors(ActualBorderColor, clGray, 0.60); // Gray out border as well
      end;
    finally
      LCaptionFont.Free; // Free the temporary font
    end;

    ActualCaptionFont.Color := LCurrentCaptionColor; // Set the final calculated color to the drawing font


    // Determine box fill color based on state
    case FState of
      csChecked: LBoxFillColor := LCurrentBoxColorChecked;
      csUnchecked: LBoxFillColor := LCurrentBoxColorUnchecked;
      csIndeterminate: LBoxFillColor := LCurrentBoxColorUnchecked; // Using Unchecked background for Indeterminate
    else
      LBoxFillColor := LCurrentBoxColorUnchecked;
    end;

    // Draw overall control background if not transparent and a background color is set
    if not FTransparent and (FBorderSettings.BackgroundColor <> clNone) then
    begin
       var BackgroundBrush: TGPSolidBrush;
       // Convert ClientRect (TRect) to TGPRectF for GDI+
       LClientRectF.X := ClientRect.Left;
       LClientRectF.Y := ClientRect.Top;
       LClientRectF.Width := ClientRect.Width;
       LClientRectF.Height := ClientRect.Height;
       BackgroundBrush := TGPSolidBrush.Create(ColorToARGB(FBorderSettings.BackgroundColor, 255)); // Alpha 255 for opaque
       try
         LG.FillRectangle(BackgroundBrush, LClientRectF); // Fill entire control background
       finally
         BackgroundBrush.Free;
       end;
    end;


    BoxDrawRect := Rect(Round(BoxRect.X), Round(BoxRect.Y), Round(BoxRect.X + BoxRect.Width), Round(BoxRect.Y + BoxRect.Height));

    // Use FBorderSettings for the checkbox square's border and corners
    ActualBoxBorderThickness := FBorderSettings.Thickness;
    ActualCornerRadius := FBorderSettings.CornerRadius;
    ActualRoundCornerType := FBorderSettings.RoundCornerType;

    ANDMR_ComponentUtils.DrawEditBox(
      LG,
      BoxDrawRect,           // TRect is fine if DrawEditBox expects it
      LBoxFillColor,         // Background of the checkbox square itself
      ActualBorderColor,     // Border of the checkbox square
      ActualBoxBorderThickness,
      psSolid,
      ActualCornerRadius,
      ActualRoundCornerType,
      255 // Opacity for the box drawing
    );

    // Draw Checkmark or Indeterminate symbol
    if FState = csChecked then
    begin
      CheckmarkThickness := Max(1.5, CheckBoxSquareSize / 8);
      LGPPen := TGPPen.Create(ColorToARGB(LCurrentCheckMarkColor, 255), CheckmarkThickness);
      LGPPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
      try
        SetLength(LPoints, 3);
        LPoints[0].X := BoxRect.X + BoxRect.Width * 0.20;
        LPoints[0].Y := BoxRect.Y + BoxRect.Height * 0.50;
        LPoints[1].X := BoxRect.X + BoxRect.Width * 0.45;
        LPoints[1].Y := BoxRect.Y + BoxRect.Height * 0.75;
        LPoints[2].X := BoxRect.X + BoxRect.Width * 0.80;
        LPoints[2].Y := BoxRect.Y + BoxRect.Height * 0.25;
        if Length(LPoints) > 1 then
          LG.DrawLines(LGPPen, PGPPointF(LPoints), Length(LPoints));
      finally
        LGPPen.Free;
      end;
    end
    else if FState = csIndeterminate then
    begin
      LGPBrush := TGPSolidBrush.Create(ColorToARGB(LCurrentCheckMarkColor, 255));
      try
        var IndeterminateSymbolRect: TGPRectF;
        IndeterminateSymbolRect.Width := BoxRect.Width * 0.6; // Make it a bit larger
        IndeterminateSymbolRect.Height := Max(2, CheckBoxSquareSize / 7); // Make it a bar
        IndeterminateSymbolRect.X := BoxRect.X + (BoxRect.Width - IndeterminateSymbolRect.Width) / 2;
        IndeterminateSymbolRect.Y := BoxRect.Y + (BoxRect.Height - IndeterminateSymbolRect.Height) / 2;
        LG.FillRectangle(LGPBrush, IndeterminateSymbolRect);
      finally
        LGPBrush.Free;
      end;
    end;

    // Draw Caption
    if (FCaptionSettings.Text <> '') and (CaptionRect.Right > CaptionRect.Left) then
    begin
      CaptionRect.Left := CaptionRect.Left + Padding; // Add padding between box and text
      ANDMR_ComponentUtils.DrawComponentCaption(
        Self.Canvas, // Use VCL Canvas for text for better theme integration if needed
        CaptionRect,
        FCaptionSettings.Text,
        ActualCaptionFont, // Use the font with the final color
        ActualCaptionFont.Color, // Pass the final color explicitly
        FCaptionSettings.Alignment,
        FCaptionSettings.VerticalAlignment,
        FCaptionSettings.WordWrap,
        255
      );
    end;
    ActualCaptionFont.Free; // Free the font used for drawing

    // Draw Focus Rectangle
    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      LGPPath := TGPGraphicsPath.Create;
      LGPPen := nil; // Initialize to nil
      try
        // Focus around the box and caption area
        CombinedRect.X := BoxRect.X - 2; // Slightly outside
        CombinedRect.Y := Min(BoxRect.Y, Single(CaptionRect.Top)) - 2;
        var textWidth: Integer;
        if FCaptionSettings.Text <> '' then
           textWidth := Canvas.TextWidth(FCaptionSettings.Text) + Padding // Approx text width
        else
           textWidth := 0;

        CombinedRect.Width := (BoxRect.Width + Padding + textWidth) + 4;
        CombinedRect.Height := Max(BoxRect.Height, Single(Canvas.TextHeight(FCaptionSettings.Text))) + 4;


        if FCaptionSettings.Text = '' then // Only around the box if no caption
        begin
           CombinedRect.X := BoxRect.X - 2;
           CombinedRect.Y := BoxRect.Y - 2;
           CombinedRect.Width := BoxRect.Width + 4;
           CombinedRect.Height := BoxRect.Height + 4;
        end;

        LGPPath.AddRectangle(CombinedRect);
        LGPPen := TGPPen.Create(ColorToARGB(LCurrentCaptionColor, 180)); // Use caption color for focus
        LGPPen.SetDashStyle(DashStyleDash);
        LG.DrawPath(LGPPen, LGPPath);
      finally
        LGPPath.Free;
        if Assigned(LGPPen) then LGPPen.Free;
      end;
    end;

  finally
    LG.Free;
  end;
end;


function TANDMR_CCheckBox.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure TANDMR_CCheckBox.SetCaption(const Value: string);
begin
  if FCaptionSettings.Text <> Value then
  begin
    FCaptionSettings.Text := Value; // This should trigger FCaptionSettings.OnChange -> SettingsChanged
    if not FApplyingStyle then FCurrentStyle := cbsCustom;
    Invalidate;
  end;
end;

function TANDMR_CCheckBox.GetChecked: Boolean;
begin
  Result := (FState = csChecked);
end;

procedure TANDMR_CCheckBox.SetChecked(const Value: Boolean);
var
  NewState: TCheckBoxState;
begin
  if Value then
    NewState := csChecked
  else
    NewState := csUnchecked;

  if FState <> NewState then // This check is important
  begin
    SetState(NewState); // SetState handles Invalidate and OnCheckChanged
  end;
end;

procedure TANDMR_CCheckBox.SetState(const Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    // No style change here, state is independent of visual style definition
    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

procedure TANDMR_CCheckBox.SetBoxColorUnchecked(const Value: TColor);
begin
  if FBoxColorUnchecked <> Value then
  begin
    FBoxColorUnchecked := Value;
    if not FApplyingStyle then FCurrentStyle := cbsCustom;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetBoxColorChecked(const Value: TColor);
begin
  if FBoxColorChecked <> Value then
  begin
    FBoxColorChecked := Value;
    if not FApplyingStyle then FCurrentStyle := cbsCustom;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetCheckMarkColor(const Value: TColor);
begin
  if FCheckMarkColor <> Value then
  begin
    FCheckMarkColor := Value;
    if not FApplyingStyle then FCurrentStyle := cbsCustom;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
    else
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

    if not FApplyingStyle then FCurrentStyle := cbsCustom;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  // HoverSettingsChanged will be called by FHoverSettings.OnChange
  if not FApplyingStyle then FCurrentStyle := cbsCustom;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    inherited SetEnabled(Value); // inherited handles CM_ENABLEDCHANGED which calls Invalidate
    // No style change for enabled typically
  end;
end;

end.
