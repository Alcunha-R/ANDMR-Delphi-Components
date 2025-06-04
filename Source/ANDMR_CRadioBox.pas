unit ANDMR_CRadioBox;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, System.Types, ANDMR_ComponentUtils;

type
  // Forward declaration if TANDMR_CRadioBoxStyle is complex, or define directly
  TANDMR_CRadioBoxStyle = (crsCustom, crsDefault); // Placeholder for styles

  // Structure to keep track of user-set properties (Similar to CCheckBox)
  TRadioUserPropertyOverrides = record
    Transparent_IsSet: Boolean;
    RadioColorUnchecked_IsSet: Boolean;
    RadioColorChecked_IsSet: Boolean;
    MarkColor_IsSet: Boolean;
    BorderSettings_IsCustomized: Boolean; // For the radio circle's border
    CaptionSettings_IsCustomized: Boolean;
    HoverSettings_IsCustomized: Boolean;
    OverallComponentBorder_IsCustomized: Boolean;
    // Add more as needed
  end;

  TANDMR_CRadioBox = class(TCustomControl)
  private
    FChecked: Boolean;
    FCaptionSettings: TCaptionSettings;
    FRadioIndicatorBorderSettings: TBorderSettings; // For the radio circle's border and component background
    FOverallComponentBorder: TBorderSettings;
    FHoverSettings: THoverSettings;
    FRadioColorUnchecked: TColor; // Color of the radio circle when unchecked
    FRadioColorChecked: TColor;   // Color of the radio circle background when checked (outer part)
    FMarkColor: TColor;           // Color of the inner mark when checked
    FTransparent: Boolean;
    FCurrentStyle: TANDMR_CRadioBoxStyle;
    FApplyingStyle: Boolean;      // To prevent re-entrant calls during style application
    FUserOverrides: TRadioUserPropertyOverrides;
    FIsGroupHovered: Boolean;                  // New field
    FGroupHoverCaptionBackgroundColor: TColor; // New field
    FOnClick: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent; // Event triggered when FChecked changes

    // Property Setters & Getters
    procedure SetChecked(const Value: Boolean);
    procedure SetIsGroupHovered(const Value: Boolean);                // New setter
    procedure SetGroupHoverCaptionBackgroundColor(const Value: TColor); // New setter
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetRadioIndicatorBorder(const Value: TBorderSettings);
    procedure SetOverallComponentBorder(const Value: TBorderSettings); // New Setter
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetRadioColorUnchecked(const Value: TColor);
    procedure SetRadioColorChecked(const Value: TColor);
    procedure SetMarkColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetCurrentStyle(const Value: TANDMR_CRadioBoxStyle);
    procedure SetEnabled(Value: Boolean); // To override and call Invalidate

    procedure InitializeUserOverrides;
    procedure ApplyStyle(AStyle: TANDMR_CRadioBoxStyle);
    procedure SettingsChanged(Sender: TObject); // Handles FBorderSettings and FCaptionSettings changes
    procedure HoverSettingsChanged(Sender: TObject);


  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    // procedure Loaded; override; // If needed for specific initializations after properties are set

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearUserOverrides;

  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property Caption: string read GetCaption write SetCaption;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property RadioIndicatorBorder: TBorderSettings read FRadioIndicatorBorderSettings write SetRadioIndicatorBorder; // For radio circle border & component BG
    property OverallComponentBorder: TBorderSettings read FOverallComponentBorder write SetOverallComponentBorder; // New Property
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

    property RadioColorUnchecked: TColor read FRadioColorUnchecked write SetRadioColorUnchecked default clWindow;
    property RadioColorChecked: TColor read FRadioColorChecked write SetRadioColorChecked default clHighlight; // Or a more typical radio button checked color
    property MarkColor: TColor read FMarkColor write SetMarkColor default clWindowText; // Inner dot color

    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property CurrentStyle: TANDMR_CRadioBoxStyle read FCurrentStyle write SetCurrentStyle default crsCustom;

    property Enabled; // Inherited, but we have a setter
    property TabStop default True;
    property TabOrder;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property ParentFont;
    property ParentShowHint;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;

    // Standard event properties
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    // Add other relevant inherited properties

    // Group Hover Properties
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

  // Default colors (can be refined by styles)
  FRadioColorUnchecked := clWindow;
  FRadioColorChecked := clActiveCaption; // A common system color for checked elements
  FMarkColor := clWindowText; // Color of the dot inside

  // BorderSettings for the radio circle itself
  FRadioIndicatorBorderSettings := TBorderSettings.Create;
  FRadioIndicatorBorderSettings.OnChange := SettingsChanged;
  FRadioIndicatorBorderSettings.CornerRadius := Height div 2; // For a circle, radius is half the component height initially
  FRadioIndicatorBorderSettings.RoundCornerType := rctAll;    // All corners rounded to make a circle
  FRadioIndicatorBorderSettings.BackgroundColor := clNone;    // Radio indicator's own background area, not component's.
  FRadioIndicatorBorderSettings.Color := clBlack;             // Radio circle border color
  FRadioIndicatorBorderSettings.Thickness := 1;               // Radio circle border thickness
  FRadioIndicatorBorderSettings.Style := psSolid;             // Default style for indicator border

  // Overall Component Border
  FOverallComponentBorder := TBorderSettings.Create;
  FOverallComponentBorder.OnChange := SettingsChanged; // Or a new OverallComponentBorderChanged if distinct logic is needed
  FOverallComponentBorder.CornerRadius := 3;
  FOverallComponentBorder.RoundCornerType := rctAll;
  FOverallComponentBorder.BackgroundColor := clNone; // Explicitly not used for overall border's fill
  FOverallComponentBorder.Color := clGray;
  FOverallComponentBorder.Thickness := 1;
  FOverallComponentBorder.Visible := False; // Default to not visible
  FOverallComponentBorder.Style := psSolid;

  // CaptionSettings
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

  // HoverSettings
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHoverSettings.Enabled := True;
  FHoverSettings.BackgroundColor := clNone; // Or a subtle hover color
  FHoverSettings.BorderColor := clHighlight; // Hover border for the radio circle

  FCurrentStyle := crsCustom; // Default to custom, no specific style applied initially
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
begin
  if FApplyingStyle then Exit;
  if AStyle = crsCustom then
  begin
    Invalidate;
    Exit;
  end;

  FApplyingStyle := True;
  try
    // Example for a 'Default' style - expand with more styles later
    case AStyle of
      crsDefault:
      begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.RadioColorUnchecked_IsSet then Self.RadioColorUnchecked := clWindow;
        if not FUserOverrides.RadioColorChecked_IsSet then Self.RadioColorChecked := clActiveCaption;
        if not FUserOverrides.MarkColor_IsSet then Self.MarkColor := clWindowText;

        if not FUserOverrides.BorderSettings_IsCustomized then // For RadioIndicatorBorder
        begin
          Self.RadioIndicatorBorder.Color := clBlack; // Radio circle border
          Self.RadioIndicatorBorder.Thickness := 1;
          // Assuming a typical indicator diameter of 16px for styling purposes
          Self.RadioIndicatorBorder.CornerRadius := 8; // Half of 16px
          Self.RadioIndicatorBorder.RoundCornerType := rctAll;
          Self.RadioIndicatorBorder.Style := psSolid;
          Self.RadioIndicatorBorder.BackgroundColor := clNone; // Important for radio indicator
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Default: no overall border for radio
          Self.OverallComponentBorder.Color := clGray;
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3; // General small radius
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          Self.CaptionSettings.Font.Color := clWindowText;
          Self.CaptionSettings.Font.Name := 'Segoe UI';
          Self.CaptionSettings.Font.Size := 9;
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := BlendColors(Self.RadioColorUnchecked, clHighlight, 0.1); // Subtle fill change
          Self.HoverSettings.BorderColor := clHighlight; // Border changes color
          Self.HoverSettings.FontColor := clWindowText; // Assuming caption might change color
        end;
      end;
      // Add more styles here...
    end;
  finally
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
      FUserOverrides.BorderSettings_IsCustomized := True // This flags indicator border customization
    else if Sender = FOverallComponentBorder then
      FUserOverrides.OverallComponentBorder_IsCustomized := True // This flags overall component border customization
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

// --- Property Setters and Getters ---
procedure TANDMR_CRadioBox.SetChecked(const Value: Boolean);
var
  I: Integer;
  Sibling: TControl;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;

    if FChecked then // If this radio button is being checked
    begin
      // Uncheck other radio buttons in the same group (parent)
      if Parent <> nil then
      begin
        for I := 0 to Parent.ControlCount - 1 do
        begin
          Sibling := Parent.Controls[I];
          if (Sibling is TANDMR_CRadioBox) and (Sibling <> Self) then
          begin
            // GroupIndex property could be used here if implemented,
            // for now, all TANDMR_CRadioBox under the same parent are considered a group.
            TANDMR_CRadioBox(Sibling).SetChecked(False); // Call SetChecked, not FChecked directly
          end;
        end;
      end;
    end;

    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self); // Trigger event
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
    // FCaptionSettings.OnChange (SettingsChanged) will handle FUserOverrides.CaptionSettings_IsCustomized
    // and set FCurrentStyle := crsCustom.
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
    if FCurrentStyle <> crsCustom then // Only invalidate if changing from a non-custom style
    begin
      FCurrentStyle := crsCustom;
      Invalidate; // To reflect that properties are now "custom"
    end;
    Exit; // Do not apply custom style, just set the flag and exit
  end;

  // If changing to a specific style (or re-applying the same style to reset overrides)
  // Or if current style was custom and now a specific style is being applied
  if (FCurrentStyle <> Value) or (FCurrentStyle = crsCustom) then
  begin
    InitializeUserOverrides; // Clear user overrides before applying a new style
    FCurrentStyle := Value;  // Set the new style
    ApplyStyle(Value);       // Apply the style (this will Invalidate)
  end
  else // This case implies FCurrentStyle = Value and Value <> crsCustom.
       // This means user is trying to re-apply the same style, possibly to reset overrides.
  begin
     InitializeUserOverrides; // Clear user overrides
     ApplyStyle(Value);       // Re-apply the style to reset its properties to defaults
  end;
end;

procedure TANDMR_CRadioBox.SetEnabled(Value: Boolean);
begin
  if inherited Enabled <> Value then // Access inherited property getter
  begin
    inherited SetEnabled(Value);
    // CMEnabledChanged message will be triggered, which calls Invalidate.
    // No direct Invalidate call needed here.
  end;
end;

// --- Event Handlers and Overridden Methods ---
procedure TANDMR_CRadioBox.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath; // For focus rect if needed
  LGPBrush: TGPBrush;
  LGPPen: TGPPen;           // For focus rect if needed
  RadioOuterRect: TGPRectF; // Outer bounding box for the radio circle
  RadioInnerRect: TGPRectF; // Bounding box for the inner mark
  RadioDrawTRect: TRect;    // TRect version for DrawEditBox
  CaptionPaintRect: TRect;
  RadioDiameter, ActualRadioDiameter: Integer; // ActualRadioDiameter considers border
  Padding: Integer;
  MarkDiameter: Integer;
  LCurrentRadioColorUnchecked, LCurrentRadioColorChecked, LCurrentMarkColor, LCurrentCaptionColor, LRadioFillColorToUse: TColor;
  LCurrentRadioBorderColorToUse: TColor;
  LIsHovering: Boolean;
  LHoverProgress: Single;
  ActualCaptionFont: TFont;
  LClientRectF: TGPRectF;
  TextMargin: Integer;       // Space between radio circle and text
  EffectiveBorderThickness: Integer;

  OverallBorderPaintRect: TRect; // Rect for drawing the overall component border
  ContentPaintRect: TRect;       // Rect inside the overall component border
  LOverallBorderColor: TColor;
begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit); // Or TextRenderingHintAntiAlias for smoother text

    Padding := 2; // General internal padding
    TextMargin := 4; // Space between radio circle and caption text

    // --- 1. Component Background ---
    if not FTransparent then
    begin
      var BackgroundBrush: TGPSolidBrush;
      var ComponentBGColor: TColor;

      // Component background logic: uses FRadioIndicatorBorderSettings.BackgroundColor if set, otherwise Self.Color.
      // Given FRadioIndicatorBorderSettings.BackgroundColor is clNone by default for radio indicator,
      // Self.Color will typically define the component background unless FTransparent is True.
      if FRadioIndicatorBorderSettings.BackgroundColor <> clNone then
        ComponentBGColor := FRadioIndicatorBorderSettings.BackgroundColor
      else
        ComponentBGColor := Self.Color; // Fallback to control's own Color property

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
    end
    else
    begin
      // VCL handles parent background if csParentBackground is set.
    end;

    // --- 1.5 Overall Component Border ---
    OverallBorderPaintRect := ClientRect;
    ContentPaintRect := ClientRect; // Will be deflated if border is visible

    LIsHovering := FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    if FOverallComponentBorder.Visible and (FOverallComponentBorder.Thickness > 0) then
    begin
      LOverallBorderColor := FOverallComponentBorder.Color;
      // Note: Hover effect for overall border is not explicitly defined in FHoverSettings yet.
      // For now, it only changes with Enabled state. Styles can override color directly.
      if not Self.Enabled then
      begin
        LOverallBorderColor := BlendColors(LOverallBorderColor, clGray, 0.60);
      end;

      ANDMR_ComponentUtils.DrawEditBox(
        LG, OverallBorderPaintRect, clNone, // Fill is by component background already drawn
        LOverallBorderColor, FOverallComponentBorder.Thickness,
        FOverallComponentBorder.Style, FOverallComponentBorder.CornerRadius,
        FOverallComponentBorder.RoundCornerType, 255 // Opacity
      );
      // Deflate ContentPaintRect to be inside the overall border
      InflateRect(ContentPaintRect, -FOverallComponentBorder.Thickness, -FOverallComponentBorder.Thickness);
    end;

    // --- 2. Determine Radio Element Size & Position (within ContentPaintRect) ---
    EffectiveBorderThickness := FRadioIndicatorBorderSettings.Thickness;
    // RadioDiameter based on ContentPaintRect's height
    RadioDiameter := Max(8, ContentPaintRect.Height - (Padding * 2));

    ActualRadioDiameter := RadioDiameter - EffectiveBorderThickness;
    if Odd(ActualRadioDiameter) then Dec(ActualRadioDiameter);

    RadioOuterRect.Width  := ActualRadioDiameter;
    RadioOuterRect.Height := ActualRadioDiameter;
    // Positioned relative to ContentPaintRect
    RadioOuterRect.X      := ContentPaintRect.Left + Padding + (EffectiveBorderThickness div 2);
    RadioOuterRect.Y      := ContentPaintRect.Top + (ContentPaintRect.Height - ActualRadioDiameter) / 2;


    // --- 3. Calculate Caption Paint Rect (within ContentPaintRect) ---
    CaptionPaintRect := ContentPaintRect; // Start with content rect
    CaptionPaintRect.Left := Round(RadioOuterRect.X - ContentPaintRect.Left + ActualRadioDiameter + (EffectiveBorderThickness div 2) + TextMargin) + ContentPaintRect.Left;
    CaptionPaintRect.Top    := ContentPaintRect.Top + Padding;
    CaptionPaintRect.Bottom := ContentPaintRect.Top + ContentPaintRect.Height - Padding;
    CaptionPaintRect.Right  := ContentPaintRect.Left + ContentPaintRect.Width - Padding;
    // Ensure valid rect
    if CaptionPaintRect.Right < CaptionPaintRect.Left then CaptionPaintRect.Right := CaptionPaintRect.Left;
    if CaptionPaintRect.Bottom < CaptionPaintRect.Top then CaptionPaintRect.Bottom := CaptionPaintRect.Top;


    // --- 4. Determine Colors and Font based on State (Hover, Disabled) ---
    LCurrentRadioColorUnchecked := Self.FRadioColorUnchecked;
    LCurrentRadioColorChecked   := Self.FRadioColorChecked;
    LCurrentMarkColor           := Self.FMarkColor;
    LCurrentRadioBorderColorToUse := Self.FRadioIndicatorBorderSettings.Color;

    ActualCaptionFont := TFont.Create;
    try
      ActualCaptionFont.Assign(FCaptionSettings.Font);
      LCurrentCaptionColor := ActualCaptionFont.Color; // Start with base caption color

      ActualCaptionFont.Assign(FCaptionSettings.Font);
      LCurrentCaptionColor := ActualCaptionFont.Color; // Start with base caption color

      // LIsHovering and LHoverProgress are already calculated before overall border drawing

      if LIsHovering then
      begin
        // Hover effect on radio circle's fill color
        if FHoverSettings.BackgroundColor <> clNone then
        begin
          LCurrentRadioColorUnchecked := BlendColors(LCurrentRadioColorUnchecked, FHoverSettings.BackgroundColor, LHoverProgress);
          LCurrentRadioColorChecked   := BlendColors(LCurrentRadioColorChecked,   FHoverSettings.BackgroundColor, LHoverProgress);
        end;
        // Hover effect on caption font color
        if FHoverSettings.FontColor <> clNone then
          LCurrentCaptionColor := BlendColors(ActualCaptionFont.Color, FHoverSettings.FontColor, LHoverProgress);
        // Hover effect on radio circle's border color
        if FHoverSettings.BorderColor <> clNone then // This applies to indicator border
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
      // ActualCaptionFont is used and then freed
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
      EffectiveBorderThickness, // From FRadioIndicatorBorderSettings.Thickness
      FRadioIndicatorBorderSettings.Style, // Use style from FRadioIndicatorBorderSettings
      ActualRadioDiameter div 2,        // Radius for circle
      FRadioIndicatorBorderSettings.RoundCornerType, // Use RoundCornerType
      255
    );

    // Draw the inner mark if checked
    if FChecked then
    begin
      // MarkDiameter is typically a fraction of the radio button's diameter
      MarkDiameter := Max(2, ActualRadioDiameter div 2);
      // Ensure mark diameter is even for better centering, if radio is not tiny
      if Odd(MarkDiameter) and (ActualRadioDiameter > 4) then Dec(MarkDiameter);


      RadioInnerRect.Width  := MarkDiameter;
      RadioInnerRect.Height := MarkDiameter;
      // Center the mark inside the RadioOuterRect
      RadioInnerRect.X      := RadioOuterRect.X + (ActualRadioDiameter - MarkDiameter) / 2;
      RadioInnerRect.Y      := RadioOuterRect.Y + (ActualRadioDiameter - MarkDiameter) / 2;

      LGPBrush := TGPSolidBrush.Create(ColorToARGB(LCurrentMarkColor, 255));
      try
         LG.FillEllipse(LGPBrush, RadioInnerRect.X, RadioInnerRect.Y, RadioInnerRect.Width, RadioInnerRect.Height);
      finally
        LGPBrush.Free;
      end;
    end;

    // --- 6. Draw Caption ---
    // Background for caption if group is hovered
    if FIsGroupHovered and (FGroupHoverCaptionBackgroundColor <> clNone) then
    begin
      var TempBrush: TGPSolidBrush;
      TempBrush := TGPSolidBrush.Create(ColorToARGB(FGroupHoverCaptionBackgroundColor, 255));
      try
        var FillRectF: TGPRectF;
        FillRectF.X := CaptionPaintRect.Left;
        FillRectF.Y := CaptionPaintRect.Top;
        FillRectF.Width := CaptionPaintRect.Width;
        FillRectF.Height := CaptionPaintRect.Height;
        LG.FillRectangle(TempBrush, FillRectF);
      finally
        TempBrush.Free;
      end;
    end;

    if (FCaptionSettings.Text <> '') and (CaptionPaintRect.Right > CaptionPaintRect.Left) and (CaptionPaintRect.Bottom > CaptionPaintRect.Top) then
    begin
      ANDMR_ComponentUtils.DrawComponentCaption(
        Self.Canvas,         // Pass VCL Canvas
        CaptionPaintRect,
        FCaptionSettings.Text,
        ActualCaptionFont,   // Font object with color already set
        ActualCaptionFont.Color, // Pass the final color (though DrawComponentCaption might also use font's color)
        FCaptionSettings.Alignment,
        FCaptionSettings.VerticalAlignment,
        FCaptionSettings.WordWrap,
        255 // Opacity for caption (currently DrawComponentCaption uses Canvas.Font.Color)
      );
    end;
    ActualCaptionFont.Free; // Free the temporary font object

    // --- 7. Draw Focus Rectangle (within ContentPaintRect) ---
    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      var FocusBoundsTRect: TRect;
      // Focus rectangle should be within ContentPaintRect
      FocusBoundsTRect.Left   := ContentPaintRect.Left + Padding div 2;
      FocusBoundsTRect.Top    := ContentPaintRect.Top + Padding div 2;
      FocusBoundsTRect.Right  := ContentPaintRect.Left + ContentPaintRect.Width - Padding div 2;
      FocusBoundsTRect.Bottom := ContentPaintRect.Top + ContentPaintRect.Height - Padding div 2;

      // Ensure focus rect is valid
      if FocusBoundsTRect.Right < FocusBoundsTRect.Left then FocusBoundsTRect.Right := FocusBoundsTRect.Left;
      if FocusBoundsTRect.Bottom < FocusBoundsTRect.Top then FocusBoundsTRect.Bottom := FocusBoundsTRect.Top;

      // Use VCL's DrawFocusRect for standard look and feel, applied to the content area
      if (FocusBoundsTRect.Right > FocusBoundsTRect.Left) and (FocusBoundsTRect.Bottom > FocusBoundsTRect.Top) then
         Self.Canvas.DrawFocusRect(FocusBoundsTRect);
      // GDI+ alternative can also be used here, making sure it draws within ContentPaintRect
      end;

  finally
    LG.Free;
  end;
end;

procedure TANDMR_CRadioBox.Click;
begin
  if not Enabled then Exit;

  // A radio button once checked, cannot be unchecked by clicking it again.
  // It's unchecked when another in its group is checked.
  if not FChecked then
  begin
    SetChecked(True); // This will handle unchecking others and calling OnClick
  end;
  // We call OnClick regardless of state change because that's standard VCL behavior for many controls.
  // However, for a radio button, OnClick might be more appropriate only when it becomes checked.
  // Let's call it always for now to be like TCheckBox, can be refined.
  inherited Click; // This calls the FOnClick event
  // if Assigned(FOnClick) then FOnClick(Self); // Redundant if inherited Click does it.
end;

procedure TANDMR_CRadioBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = 0 then Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    if not FChecked then // Only "click" if it's not already checked
    begin
      Click; // This will call SetChecked(True)
    end;
    Key := 0; // Mark key as handled
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
  if Assigned(FHoverSettings) then // Always try to stop, even if disabled, to reset state
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CRadioBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then // If disabled, ensure hover animation is stopped/reset
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
      // GDI+ alternative can also be used here, making sure it draws within ContentPaintRect
      end;

  finally
    LG.Free;
  end;
end;

procedure TANDMR_CRadioBox.Click;
begin
  if not Enabled then Exit;

  // A radio button once checked, cannot be unchecked by clicking it again.
  // It's unchecked when another in its group is checked.
  if not FChecked then
  begin
    SetChecked(True); // This will handle unchecking others and calling OnClick
  end;
  // We call OnClick regardless of state change because that's standard VCL behavior for many controls.
  // However, for a radio button, OnClick might be more appropriate only when it becomes checked.
  // Let's call it always for now to be like TCheckBox, can be refined.
  inherited Click; // This calls the FOnClick event
  // if Assigned(FOnClick) then FOnClick(Self); // Redundant if inherited Click does it.
end;

procedure TANDMR_CRadioBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = 0 then Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    if not FChecked then // Only "click" if it's not already checked
    begin
      Click; // This will call SetChecked(True)
    end;
    Key := 0; // Mark key as handled
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
  if Assigned(FHoverSettings) then // Always try to stop, even if disabled, to reset state
  begin
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CRadioBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then // If disabled, ensure hover animation is stopped/reset
  begin
    if Assigned(FHoverSettings) then FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

end.
