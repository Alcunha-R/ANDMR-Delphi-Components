unit ANDMR_CCheckBox;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, // For crHandPoint, Messages, VK_SPACE
  System.Types, // For TRect, TPoint, etc.
  ANDMR_ComponentUtils;

type
  TCheckBoxState = (csUnchecked, csChecked, csIndeterminate);

  TANDMR_CCheckBox = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings; // Added
    FCaptionSettings: TCaptionSettings; // Added
    FState: TCheckBoxState;
    // FCaption: string; // Removed
    // FCornerRadius: Integer; // Moved to FBorderSettings
    // FRoundCornerType: TRoundCornerType; // Moved to FBorderSettings
    FBoxColorUnchecked: TColor;
    FBoxColorChecked: TColor;
    FCheckMarkColor: TColor;
    // FTitleFont: TFont; // Removed
    FTransparent: Boolean;
    FOnClick: TNotifyEvent;
    FOnCheckChanged: TNotifyEvent;
    FInternalHoverSettings: THoverSettings;

    function GetChecked: Boolean; // Added
    procedure SetChecked(const Value: Boolean);
    procedure SetState(const Value: TCheckBoxState); // Added
    function GetCaption: string; // Added
    procedure SetCaption(const Value: string);
    function GetCornerRadius: Integer; // Getter
    procedure SetCornerRadius(const Value: Integer); // Setter
    function GetRoundCornerType: TRoundCornerType; // Getter
    procedure SetRoundCornerType(const Value: TRoundCornerType); // Setter
    procedure SetBoxColorUnchecked(const Value: TColor);
    procedure SetBoxColorChecked(const Value: TColor);
    procedure SetCheckMarkColor(const Value: TColor);
    function GetTitleFont: TFont; // Added
    procedure SetTitleFont(const Value: TFont);
    procedure SetTransparent(const Value: Boolean);
    procedure SetInternalHoverSettings(const Value: THoverSettings);
    procedure SetEnabled(Value: Boolean);

    // procedure FontChanged(Sender: TObject); // Removed
    procedure InternalHoverSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); // Added

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

    property InternalHoverSettings: THoverSettings read FInternalHoverSettings write SetInternalHoverSettings;

  published
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write SetState; // Added
    property Caption: string read GetCaption write SetCaption; // Changed
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius;
    property RoundCornerType: TRoundCornerType read GetRoundCornerType write SetRoundCornerType;
    property BoxColorUnchecked: TColor read FBoxColorUnchecked write SetBoxColorUnchecked;
    property BoxColorChecked: TColor read FBoxColorChecked write SetBoxColorChecked;
    property CheckMarkColor: TColor read FCheckMarkColor write SetCheckMarkColor;
    property TitleFont: TFont read GetTitleFont write SetTitleFont; // Changed
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;
  end;

procedure Register; // Declaration for Register procedure

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CCheckBox]);
end;

{ TANDMR_CCheckBox }

constructor TANDMR_CCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csReplicatable, csCaptureMouse, csNeedsBorderPaint, csAcceptsControls]; // csAcceptsControls for focus
  FTransparent := False;
  // Transparent style setting moved after FBorderSettings initialization if it affects background

  Width := 120;
  Height := 24;

  FState := csUnchecked;
  // FCaption := Name; // This line is effectively replaced by FCaptionSettings.Text := Name below
  // FCornerRadius := 3; // Moved to FBorderSettings
  // FRoundCornerType := rctAll; // Moved to FBorderSettings
  FBoxColorUnchecked := clWindow;
  FBoxColorChecked := clHighlight;
  FCheckMarkColor := clWindowText;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 3;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.BackgroundColor := clNone; // Checkbox itself doesn't have a fill behind the box by default
  FBorderSettings.Color := clBlack; // Default border color for the box
  FBorderSettings.Thickness := 1;   // Default border thickness

  if FTransparent then // Apply transparency after FBorderSettings might be involved
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

  // FTitleFont := TFont.Create; // Removed
  // FTitleFont.Name := 'Segoe UI'; // Removed
  // FTitleFont.Size := 9; // Removed
  // FTitleFont.Color := clWindowText; // Removed
  // FTitleFont.OnChange := FontChanged; // Removed

  FInternalHoverSettings := THoverSettings.Create(Self);
  FInternalHoverSettings.OnChange := InternalHoverSettingsChanged;
  FInternalHoverSettings.BackgroundColor := clNone;
  FInternalHoverSettings.BorderColor := clNone;
  FInternalHoverSettings.FontColor := clNone;
  FInternalHoverSettings.Enabled := True;

  // Temp FTitleFont to assign to FCaptionSettings, as original FTitleFont is removed
  var TempTitleFont: TFont;
  TempTitleFont := TFont.Create;
  try
    TempTitleFont.Name := 'Segoe UI';
    TempTitleFont.Size := 9;
    TempTitleFont.Color := clWindowText;
    // TempTitleFont.OnChange is not needed here as FCaptionSettings.Font.OnChange will be set

    FCaptionSettings := TCaptionSettings.Create(Self);
    FCaptionSettings.OnChange := SettingsChanged; // Use existing SettingsChanged
    FCaptionSettings.Text := Name; // Set default caption to component Name
    FCaptionSettings.Font.Assign(TempTitleFont); // Transfer initial font settings
    // FCaptionSettings.Font.OnChange := FontChanged; // Removed assignment
  finally
    TempTitleFont.Free;
  end;
  FCaptionSettings.Alignment := taLeftJustify; // Default for checkbox caption
  FCaptionSettings.VerticalAlignment := cvaCenter;

  TabStop := True; // Important for keyboard interaction
  Cursor := crHandPoint;
  DoubleBuffered := True;
end;

destructor TANDMR_CCheckBox.Destroy;
begin
  if Assigned(FBorderSettings) then
  begin
    FBorderSettings.OnChange := nil;
    FBorderSettings.Free;
    FBorderSettings := nil;
  end;

  if Assigned(FInternalHoverSettings) then
  begin
    FInternalHoverSettings.OnChange := nil;
    FInternalHoverSettings.Free;
    FInternalHoverSettings := nil;
  end;

  if Assigned(FCaptionSettings) then
  begin
    FCaptionSettings.OnChange := nil;
    FCaptionSettings.Free;
    FCaptionSettings := nil;
  end;

  // if Assigned(FTitleFont) then // Removed
  // begin // Removed
  //   FTitleFont.OnChange := nil; // Removed
  //   FTitleFont.Free; // Removed
  //   FTitleFont := nil; // Removed
  // end; // Removed

  inherited Destroy;
end;

procedure TANDMR_CCheckBox.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

// procedure TANDMR_CCheckBox.FontChanged(Sender: TObject); // Removed
// begin // Removed
// Invalidate; // Removed
// end; // Removed

procedure TANDMR_CCheckBox.InternalHoverSettingsChanged(Sender: TObject);
begin
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
  if Self.Enabled and Assigned(FInternalHoverSettings) and FInternalHoverSettings.Enabled then
  begin
    FInternalHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_CCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FInternalHoverSettings) then
  begin
    FInternalHoverSettings.StartAnimation(False);
  end;
end;

procedure TANDMR_CCheckBox.Click;
begin
  if not Enabled then Exit;

  case FState of
    csUnchecked: SetState(csChecked);
    csChecked: SetState(csUnchecked);
    csIndeterminate: SetState(csChecked); // Or csUnchecked, depending on desired UX. Let's go with csChecked.
  end;

  if Assigned(FOnClick) then
    FOnClick(Self);
  // No inherited Click; as TCustomControl.Click is empty and TControl.Click would just call OnClick again.
end;

procedure TANDMR_CCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = 0 then // Key already handled by inherited call or a child control
    Exit;

  if Self.Enabled and (Key = VK_SPACE) then
  begin
    case FState of
      csUnchecked: SetState(csChecked);
      csChecked: SetState(csUnchecked);
      csIndeterminate: SetState(csChecked); // Consistent with Click
    end;

    // Trigger the standard OnClick event
    if Assigned(FOnClick) then
      FOnClick(Self);

    Key := 0; // Mark key as handled
  end;
end;

procedure TANDMR_CCheckBox.Paint;
var
  LG: TGPGraphics;
  LGPPath: TGPGraphicsPath;
  LGPBrush: TGPSolidBrush;
  LGPPen: TGPPen;
  LPoints: array of TGPPointF;
  BoxRect: TGPRectF; // Keep as TGPRectF for GDI+ math
  BoxDrawRect: TRect; // For DrawEditBox
  InnerBoxRect: TGPRectF;
  CaptionRect: TRect;
  CheckBoxSquareSize: Integer;
  LCurrentBoxColor, LCurrentCheckMarkColor, LCurrentCaptionColor, LBoxBorderColor, LBoxFillColor: TColor;
  LIsHovering: Boolean;
  LHoverProgress: Single;
  LCaptionFont: TFont;
  Padding: Integer;
  CheckmarkThickness: Single;
  CombinedRect: TGPRectF; // Moved declaration here
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

    LIsHovering := FInternalHoverSettings.Enabled and (FInternalHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FInternalHoverSettings.CurrentAnimationValue / 255.0;

    // LCurrentBoxColor := IfThen(FChecked, FBoxColorChecked, FBoxColorUnchecked);
    case FState of
      csChecked: LCurrentBoxColor := FBoxColorChecked;
      csUnchecked: LCurrentBoxColor := FBoxColorUnchecked;
      csIndeterminate: LCurrentBoxColor := FBoxColorUnchecked; // Using Unchecked background for Indeterminate
    else
      LCurrentBoxColor := FBoxColorUnchecked;
    end;
    LCurrentCheckMarkColor := FCheckMarkColor;
    // LCurrentCaptionColor := FTitleFont.Color; // Base color from FCaptionSettings.Color
    LCurrentCaptionColor := FCaptionSettings.Color;


    if LIsHovering then
    begin
      if FInternalHoverSettings.BackgroundColor <> clNone then
        LCurrentBoxColor := BlendColors(LCurrentBoxColor, FInternalHoverSettings.BackgroundColor, LHoverProgress);
      // Hover caption color from FInternalHoverSettings.CaptionFontColor
      if FInternalHoverSettings.CaptionFontColor <> clNone then
        LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, FInternalHoverSettings.CaptionFontColor, LHoverProgress);
    end;

    if not Self.Enabled then
    begin
      LCurrentBoxColor := BlendColors(LCurrentBoxColor, clGray, 0.60);
      LCurrentCheckMarkColor := BlendColors(LCurrentCheckMarkColor, clGray, 0.60);
      // Disabled caption color from FCaptionSettings.DisabledColor or default graying
      if FCaptionSettings.DisabledColor <> clNone then
        LCurrentCaptionColor := FCaptionSettings.DisabledColor
      else
        LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, clGray, 0.50);
    end;

    LBoxBorderColor := DarkerColor(LCurrentBoxColor, IfThen(LIsHovering and (FInternalHoverSettings.BorderColor = clNone), 15, 5));
    if LIsHovering and (FInternalHoverSettings.BorderColor <> clNone) then
        LBoxBorderColor := BlendColors(LBoxBorderColor, FInternalHoverSettings.BorderColor, LHoverProgress);

    if FTransparent then
      LBoxFillColor := clNone
    else
      LBoxFillColor := LCurrentBoxColor;

    BoxDrawRect := Rect(Round(BoxRect.X), Round(BoxRect.Y), Round(BoxRect.X + BoxRect.Width), Round(BoxRect.Y + BoxRect.Height));

    ANDMR_ComponentUtils.DrawEditBox(
      LG,                 // AGraphics
      BoxDrawRect,        // ADrawArea
      LBoxFillColor,      // ABackgroundColor
      LBoxBorderColor,    // ABorderColor
      FBorderSettings.Thickness, // ABorderThickness (Integer) from FBorderSettings
      psSolid,            // ABorderStyle (remains psSolid for checkbox)
      FBorderSettings.CornerRadius,      // ACornerRadius from FBorderSettings
      FBorderSettings.RoundCornerType,   // ARoundCornerType from FBorderSettings
      255                 // AOpacity
    );

    if FState = csChecked then
    begin
      CheckmarkThickness := Max(1.5, CheckBoxSquareSize / 8);
      // InnerBoxRect := BoxRect; // Not strictly needed here if points are relative to BoxRect
      // System.Types.InflateRect(InnerBoxRect, -CheckBoxSquareSize * 0.20, -CheckBoxSquareSize * 0.20);

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
        if Length(LPoints) > 1 then // Ensure there are at least two points to draw a line
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
        IndeterminateSymbolRect.Width := BoxRect.Width * 0.5;
        IndeterminateSymbolRect.Height := BoxRect.Height * 0.5;
        IndeterminateSymbolRect.X := BoxRect.X + (BoxRect.Width - IndeterminateSymbolRect.Width) / 2;
        IndeterminateSymbolRect.Y := BoxRect.Y + (BoxRect.Height - IndeterminateSymbolRect.Height) / 2;

        if IndeterminateSymbolRect.Width < 4 then IndeterminateSymbolRect.Width := 4;
        if IndeterminateSymbolRect.Height < 4 then IndeterminateSymbolRect.Height := 4;
        IndeterminateSymbolRect.X := BoxRect.X + (BoxRect.Width - IndeterminateSymbolRect.Width) / 2;
        IndeterminateSymbolRect.Y := BoxRect.Y + (BoxRect.Height - IndeterminateSymbolRect.Height) / 2;

        LG.FillRectangle(LGPBrush, IndeterminateSymbolRect);
      finally
        LGPBrush.Free;
      end;
    end;

    if (FCaptionSettings.Text <> '') and (CaptionRect.Right > CaptionRect.Left) then // Use FCaptionSettings.Text
    begin
      LCaptionFont := TFont.Create;
      try
        LCaptionFont.Assign(FCaptionSettings.Font); // Use FCaptionSettings.Font
        LCaptionFont.Color := LCurrentCaptionColor; // Already determined
        CaptionRect.Left := CaptionRect.Left + Padding;
        ANDMR_ComponentUtils.DrawComponentCaption(
          Self.Canvas,
          CaptionRect,
          FCaptionSettings.Text, // Use FCaptionSettings.Text
          LCaptionFont, // Assigned from FCaptionSettings.Font
          LCurrentCaptionColor,
          FCaptionSettings.Alignment, // Use FCaptionSettings.Alignment
          FCaptionSettings.VerticalAlignment, // Use FCaptionSettings.VerticalAlignment
          FCaptionSettings.WordWrap, // Use FCaptionSettings.WordWrap
          255
        );
      finally
        LCaptionFont.Free;
      end;
    end;

    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      LGPPath := TGPGraphicsPath.Create;
      try
        CombinedRect.X := BoxRect.X - 1;
        CombinedRect.Y := BoxRect.Y - 1;
        CombinedRect.Width := (CaptionRect.Right - BoxRect.X) + 2;
        CombinedRect.Height := BoxRect.Height + 2;

        if FCaptionSettings.Text = '' then // Use FCaptionSettings.Text
        begin
           CombinedRect.X := BoxRect.X - 1;
           CombinedRect.Y := BoxRect.Y - 1;
           CombinedRect.Width := BoxRect.Width + 2;
           CombinedRect.Height := BoxRect.Height + 2;
        end;

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

function TANDMR_CCheckBox.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure TANDMR_CCheckBox.SetCaption(const Value: string);
begin
  FCaptionSettings.Text := Value;
  // Invalidation is handled by FCaptionSettings.OnChange via SettingsChanged
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

  if FState <> NewState then
  begin
    FState := NewState;
    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

procedure TANDMR_CCheckBox.SetState(const Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Invalidate;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

procedure TANDMR_CCheckBox.SetCheckMarkColor(const Value: TColor);
begin
  if FCheckMarkColor <> Value then
  begin
    FCheckMarkColor := Value;
    Invalidate;
  end;
end;

function TANDMR_CCheckBox.GetCornerRadius: Integer;
begin
  Result := FBorderSettings.CornerRadius;
end;

procedure TANDMR_CCheckBox.SetCornerRadius(const Value: Integer);
begin
  FBorderSettings.CornerRadius := Value;
  // FBorderSettings.OnChange will trigger Invalidate via SettingsChanged
end;

procedure TANDMR_CCheckBox.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
end;

procedure TANDMR_CCheckBox.SetInternalHoverSettings(const Value: THoverSettings);
begin
  FInternalHoverSettings.Assign(Value);
end;

procedure TANDMR_CCheckBox.SetRoundCornerType(const Value: TRoundCornerType);
begin
  FBorderSettings.RoundCornerType := Value;
  // FBorderSettings.OnChange will trigger Invalidate via SettingsChanged
end;

function TANDMR_CCheckBox.GetRoundCornerType: TRoundCornerType;
begin
  Result := FBorderSettings.RoundCornerType;
end;

function TANDMR_CCheckBox.GetTitleFont: TFont;
begin
  Result := FCaptionSettings.Font;
end;

procedure TANDMR_CCheckBox.SetTitleFont(const Value: TFont);
begin
  FCaptionSettings.Font.Assign(Value);
  // Invalidation is handled by FCaptionSettings.OnChange or its Font.OnChange via SettingsChanged
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
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetBoxColorChecked(const Value: TColor);
begin
  if FBoxColorChecked <> Value then
  begin
    FBoxColorChecked := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetBoxColorUnchecked(const Value: TColor);
begin
  if FBoxColorUnchecked <> Value then
  begin
    FBoxColorUnchecked := Value;
    Invalidate;
  end;
end;

end.
