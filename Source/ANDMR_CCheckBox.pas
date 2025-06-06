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
                          cbsGhost, cbsFaded, cbsBordered, cbsIOS, cbsWin11);

  // Enumeração para a posição do indicador de "marcado" (o elemento de check)
  TCheckBoxIndicatorPosition = (
    cipLeftTop, cipLeftCenter, cipLeftBottom,
    cipTopLeft, cipTopCenter, cipTopRight,
    cipRightTop, cipRightCenter, cipRightBottom,
    cipBottomLeft, cipBottomCenter, cipBottomRight,
    cipCenter // Indicador centralizado
  );

  // Structure to keep track of user-set properties
  TUserPropertyOverrides = record
    Transparent_IsSet: Boolean;
    BoxColorUnchecked_IsSet: Boolean;
    BoxColorChecked_IsSet: Boolean;
    CheckMarkColor_IsSet: Boolean;
    BorderSettings_IsCustomized: Boolean; // For the check element's border
    CaptionSettings_IsCustomized: Boolean;
    HoverSettings_IsCustomized: Boolean;
    OverallComponentBorder_IsCustomized: Boolean; // New
    CheckedIndicatorPosition_IsSet: Boolean;    // New
    CheckedIndicatorSize_IsSet: Boolean;        // New
  end;

  TANDMR_CCheckBox = class(TCustomControl)
  private
    FIndicatorBorderSettings: TBorderSettings; // Existing: for the check element's border and component background
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
    FApplyingStyle: Boolean;
    FUserOverrides: TUserPropertyOverrides;

    // New fields for overall component border and check element positioning
    FOverallComponentBorder: TBorderSettings;
    FCheckedIndicatorPosition: TCheckBoxIndicatorPosition;
    FCheckedIndicatorSize: Integer; // Size of the check element (box)

    procedure SetIndicatorBorder(const Value: TBorderSettings);
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

    // Setters for new properties
    procedure SetOverallComponentBorder(const Value: TBorderSettings);
    procedure SetCheckedIndicatorPosition(const Value: TCheckBoxIndicatorPosition);
    procedure SetCheckedIndicatorSize(const Value: Integer);

    procedure ApplyStyle(AStyle: TANDMR_CCheckBoxStyle);

    procedure HoverSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); // Handles FBorderSettings and FCaptionSettings
    procedure OverallComponentBorderChanged(Sender: TObject); // New handler

    procedure InitializeUserOverrides;

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
    procedure ClearUserOverrides; // Public method to clear all overrides

  published
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write SetState;
    property Caption: string read GetCaption write SetCaption;
    property IndicatorBorder: TBorderSettings read FIndicatorBorderSettings write SetIndicatorBorder; // Existing: for check element border & component background
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property BoxColorUnchecked: TColor read FBoxColorUnchecked write SetBoxColorUnchecked default clWindow;
    property BoxColorChecked: TColor read FBoxColorChecked write SetBoxColorChecked default clHighlight;
    property CheckMarkColor: TColor read FCheckMarkColor write SetCheckMarkColor default clWindowText;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property CurrentStyle: TANDMR_CCheckBoxStyle read FCurrentStyle write SetCurrentStyle default cbsCustom;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

    // New published properties
    property OverallComponentBorder: TBorderSettings read FOverallComponentBorder write SetOverallComponentBorder;
    property CheckedIndicatorPosition: TCheckBoxIndicatorPosition read FCheckedIndicatorPosition write SetCheckedIndicatorPosition default cipLeftCenter;
    property CheckedIndicatorSize: Integer read FCheckedIndicatorSize write SetCheckedIndicatorSize default 0; // 0 means auto-calculate

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;

    // Standard properties
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font; // Note: FCaptionSettings.Font is the primary font for text
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
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math, Vcl.Themes, Vcl.Consts;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CCheckBox]);
end;

{ TANDMR_CCheckBox }

procedure TANDMR_CCheckBox.InitializeUserOverrides;
begin
  FillChar(FUserOverrides, SizeOf(FUserOverrides), 0); // Sets all Boolean flags to False
end;

constructor TANDMR_CCheckBox.Create(AOwner: TComponent);
var
  TempTitleFont: TFont;
begin
  inherited Create(AOwner);

  FApplyingStyle := False;
  InitializeUserOverrides;

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csReplicatable, csCaptureMouse, csNeedsBorderPaint, csAcceptsControls];
  Width := 120;
  Height := 24; // Default height, might need adjustment based on indicator size and overall border

  FState := csUnchecked;
  FBoxColorUnchecked := clWindow;
  FBoxColorChecked := clHighlight;
  FCheckMarkColor := clWindowText;
  FTransparent := False;

  // Existing BorderSettings for the check element and component background
  FIndicatorBorderSettings := TBorderSettings.Create;
  FIndicatorBorderSettings.OnChange := SettingsChanged;
  FIndicatorBorderSettings.CornerRadius := 3;
  FIndicatorBorderSettings.RoundCornerType := rctAll;
  FIndicatorBorderSettings.BackgroundColor := clNone; // This is for the component's background
  FIndicatorBorderSettings.Color := clBlack;         // This is for the check element's border
  FIndicatorBorderSettings.Thickness := 1;           // For the check element's border

  // New OverallComponentBorder
  FOverallComponentBorder := TBorderSettings.Create;
  FOverallComponentBorder.OnChange := OverallComponentBorderChanged;
  FOverallComponentBorder.CornerRadius := 3; // Default, can be styled
  FOverallComponentBorder.RoundCornerType := rctAll;
  FOverallComponentBorder.BackgroundColor := clNone; // Not used for overall border, background is from FBorderSettings.BackgroundColor
  FOverallComponentBorder.Color := clGray;    // Default overall border color
  FOverallComponentBorder.Thickness := 1;     // Default overall border thickness
//  FOverallComponentBorder.Visible := False;   // Default to not visible to maintain original appearance

  // New Check Element Positioning and Size
  FCheckedIndicatorPosition := cipLeftCenter; // Default position
  FCheckedIndicatorSize := 0;                 // Default size (auto-calculated based on height)

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
  FCaptionSettings.DisabledColor := clGrayText;

  FCurrentStyle := cbsCustom;

  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

  TabStop := True;
  Cursor := crHandPoint;
  DoubleBuffered := True;
  Enabled := True;
end;

destructor TANDMR_CCheckBox.Destroy;
begin
  if Assigned(FIndicatorBorderSettings) then
  begin
    FIndicatorBorderSettings.OnChange := nil;
    FIndicatorBorderSettings.Free;
    FIndicatorBorderSettings := nil;
  end;

  if Assigned(FOverallComponentBorder) then // Free new border settings
  begin
    FOverallComponentBorder.OnChange := nil;
    FOverallComponentBorder.Free;
    FOverallComponentBorder := nil;
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

procedure TANDMR_CCheckBox.ClearUserOverrides;
begin
  InitializeUserOverrides;
  // After clearing overrides, re-apply the current style to reset to its defaults
  // or let the user set properties manually if FCurrentStyle is cbsCustom.
  if FCurrentStyle <> cbsCustom then
    ApplyStyle(FCurrentStyle) // Re-apply to get style defaults
  else
    Invalidate; // If custom, just repaint with current (potentially mixed) values
end;

procedure TANDMR_CCheckBox.ApplyStyle(AStyle: TANDMR_CCheckBoxStyle);
var
  TempCaptionFont: TFont;
begin
  if FApplyingStyle then Exit;
  if AStyle = cbsCustom then
  begin
    Invalidate;
    Exit;
  end;

  FApplyingStyle := True;
  TempCaptionFont := TFont.Create;
  try
    // Apply properties only if not overridden by the user.
    case AStyle of
      cbsLight: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := clWindow;
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := clHighlight;
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := clBlack;       // Check element's border
          Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 3;
          Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := clWindow; // Component background
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clWindowText; TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True; Self.HoverSettings.BackgroundColor := BlendColors(clWindow, clHighlight, 0.1);
          Self.HoverSettings.BorderColor := clHighlight; Self.HoverSettings.FontColor := clWindowText;
        end;

        // New properties for Light style
        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Typically no overall border for light
          Self.OverallComponentBorder.Color := clGray;
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      cbsDark: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := TColor($00555555);
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($000099FF);
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($00777777); Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 3; Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := TColor($00333333); // Component background
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clWhite; TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := TColor($00888888);
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True; Self.HoverSettings.BackgroundColor := TColor($00666666);
          Self.HoverSettings.BorderColor := TColor($0033CCFF); Self.HoverSettings.FontColor := clWhite;
        end;

        // New properties for Dark style
        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Typically no overall border for dark
          Self.OverallComponentBorder.Color := TColor($00AAAAAA);
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      cbsMaterial: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := clWhite;
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($FF2196F3);
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($FFBDBDBD); Self.IndicatorBorder.Thickness := 2;
          Self.IndicatorBorder.CornerRadius := 2; Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := clNone; // Component background (Material often transparent bg on cards)
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := TColor($DE000000); TempCaptionFont.Name := 'Roboto'; TempCaptionFont.Size := 10;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := TColor($61000000);
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True; Self.HoverSettings.BackgroundColor := TColor($1F000000); // Ripple effect like
          Self.HoverSettings.BorderColor := TColor($FF2196F3); Self.HoverSettings.FontColor := clNone;
        end;

        // New properties for Material style
        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Material checkbox usually no overall border
          Self.OverallComponentBorder.Color := TColor($FFBDBDBD);
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 2;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 20; // Material checkboxes are a bit larger
      end;
      // ... Implement similar blocks for ALL other styles (cbsFlat, cbsModern, etc.)
      // adding default values for OverallComponentBorder, CheckedIndicatorPosition, CheckedIndicatorSize.
      // For brevity, I'll skip the full list here, but you should complete it. Example for cbsFlat:
      cbsFlat: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := True;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := TColor($00E0E0E0);
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($00757575);
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($00AAAAAA); Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 0; Self.IndicatorBorder.RoundCornerType := rctNone;
          Self.IndicatorBorder.BackgroundColor := clNone; // Component background
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clWindowText; TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True; Self.HoverSettings.BackgroundColor := TColor($00D0D0D0);
          Self.HoverSettings.BorderColor := TColor($00757575); Self.HoverSettings.FontColor := clNone;
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Flat usually no overall border
          Self.OverallComponentBorder.Color := TColor($00AAAAAA);
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 0;
          Self.OverallComponentBorder.RoundCornerType := rctNone;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      // Add other styles here...
      cbsModern: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := TColor($00F0F2F5);
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($FF007AFF);
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;
        if not FUserOverrides.BorderSettings_IsCustomized then
        begin
          Self.IndicatorBorder.Color := TColor($00D1D1D6); Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 5; Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := clNone;
          Self.IndicatorBorder.Style := psSolid;
        end;
        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := TColor($FF1D1D1F); TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 10;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := TColor($FFBCBCBF);
        end;
        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True; Self.HoverSettings.BackgroundColor := BlendColors(FBoxColorUnchecked, FBoxColorChecked, 0.1);
          Self.HoverSettings.BorderColor := TColor($FF007AFF); Self.HoverSettings.FontColor := clNone;
        end;
        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := True; // Modern might have a subtle overall border
          Self.OverallComponentBorder.Color := TColor($00E5E5EA);
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 6;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0; // Approx 18-20px
      end;
      cbsGhost: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := True;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := clNone; // Fully transparent box
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := clNone;   // Fully transparent box for checked (mark provides visual)
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := TColor($FF333333); // Dark checkmark

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($FF999999); // Lighter border for unchecked state
          Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 4;
          Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := clNone; // Component background is transparent
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := TColor($FF333333); TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := TColor($1FCCCCCC); // Very subtle hover fill
          Self.HoverSettings.BorderColor := TColor($FF333333); // Border darkens on hover
          Self.HoverSettings.FontColor := TColor($FF333333);
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // No overall border for ghost style
          Self.OverallComponentBorder.Color := clNone;
          Self.OverallComponentBorder.Thickness := 0;
          Self.OverallComponentBorder.CornerRadius := 0;
          Self.OverallComponentBorder.RoundCornerType := rctNone;
          Self.OverallComponentBorder.Style := psClear;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      cbsFaded: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := TColor($00F0F0F0);
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($00D5E5F5); // Light blue checked
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := TColor($00336699); // Darker blue checkmark

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($00CCCCCC); // Light gray border
          Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 3;
          Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := TColor($00FAFAFA); // Light component BG
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := TColor($00777777); TempCaptionFont.Name := 'Segoe UI'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := TColor($00BBBBBB);
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := TColor($00E8F0F8);
          Self.HoverSettings.BorderColor := TColor($00AECAF0);
          Self.HoverSettings.FontColor := TColor($005588AA);
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False;
          Self.OverallComponentBorder.Color := clNone;
          Self.OverallComponentBorder.Thickness := 0;
          Self.OverallComponentBorder.CornerRadius := 0;
          Self.OverallComponentBorder.RoundCornerType := rctNone;
          Self.OverallComponentBorder.Style := psClear;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      cbsBordered: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := clWindow;
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := clWindow; // Box remains window color, checkmark appears
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clHighlight; // Standard highlight for checkmark

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := clGray;       // Check element's border - Modernized
          Self.IndicatorBorder.Thickness := 2;
          Self.IndicatorBorder.CornerRadius := 1;
          Self.IndicatorBorder.RoundCornerType := rctNone; // Square corners for indicator
          Self.IndicatorBorder.BackgroundColor := clWindow; // Standard component BG
          Self.IndicatorBorder.Style := psSolid;
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
          Self.HoverSettings.BackgroundColor := BlendColors(clWindow, clHighlight, 0.05);
          Self.HoverSettings.BorderColor := clHighlight; // Indicator border changes on hover
          Self.HoverSettings.FontColor := clWindowText;
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := True; // This style HAS an overall border
          Self.OverallComponentBorder.Color := clSilver;
          Self.OverallComponentBorder.Thickness := 1;
          Self.OverallComponentBorder.CornerRadius := 3;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psSolid;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 0;
      end;
      cbsIOS: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False; // iOS usually non-transparent background for controls
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := clWindow;
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($FF34C759); // iOS Green
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder (the round check area)
        begin
          Self.IndicatorBorder.Color := TColor($FFDCDCDC); // Light gray border for unchecked
          Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 10; // iOS uses very rounded indicators (adjust based on CheckedIndicatorSize)
          Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := clWindow; // Component BG
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := clBlack; TempCaptionFont.Name := 'San Francisco'; TempCaptionFont.Size := 10; // iOS like font
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := clGrayText;
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := BlendColors(clWindow, clGray, 0.1);
          Self.HoverSettings.BorderColor := TColor($FF34C759); // Border becomes green on hover/focus
          Self.HoverSettings.FontColor := clBlack;
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // iOS controls typically no outer border, rely on spacing
          Self.OverallComponentBorder.Color := clNone;
          Self.OverallComponentBorder.Thickness := 0;
          Self.OverallComponentBorder.CornerRadius := 0;
          Self.OverallComponentBorder.RoundCornerType := rctNone;
          Self.OverallComponentBorder.Style := psClear;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 20; // iOS indicators are often around 20-22px
      end;
      cbsWin11: begin
        if not FUserOverrides.Transparent_IsSet then Self.Transparent := False;
        if not FUserOverrides.BoxColorUnchecked_IsSet then Self.BoxColorUnchecked := TColor($FFF9F9F9); // Win11 light theme box
        if not FUserOverrides.BoxColorChecked_IsSet then Self.BoxColorChecked := TColor($FF0078D4);   // Win11 accent blue
        if not FUserOverrides.CheckMarkColor_IsSet then Self.CheckMarkColor := clWhite;

        if not FUserOverrides.BorderSettings_IsCustomized then // For IndicatorBorder
        begin
          Self.IndicatorBorder.Color := TColor($FFACACAC); // Border for unchecked
          Self.IndicatorBorder.Thickness := 1;
          Self.IndicatorBorder.CornerRadius := 4; // Win11 uses slightly rounded corners
          Self.IndicatorBorder.RoundCornerType := rctAll;
          Self.IndicatorBorder.BackgroundColor := TColor($FFF3F3F3); // Win11 control background
          Self.IndicatorBorder.Style := psSolid;
        end;

        if not FUserOverrides.CaptionSettings_IsCustomized then
        begin
          TempCaptionFont.Color := TColor($FF000000); TempCaptionFont.Name := 'Segoe UI Variable Text'; TempCaptionFont.Size := 9;
          Self.CaptionSettings.Font.Assign(TempCaptionFont);
          Self.CaptionSettings.DisabledColor := TColor($FF6A6A6A);
        end;

        if not FUserOverrides.HoverSettings_IsCustomized then
        begin
          Self.HoverSettings.Enabled := True;
          Self.HoverSettings.BackgroundColor := TColor($FFEDEDED); // Hover fill for unchecked
          Self.HoverSettings.BorderColor := TColor($FF005A9E);   // Darker blue for border on hover
          Self.HoverSettings.FontColor := TColor($FF000000);
        end;

        if not FUserOverrides.OverallComponentBorder_IsCustomized then
        begin
          Self.OverallComponentBorder.Visible := False; // Win11 CheckBox usually no distinct overall border, but part of a list item or standalone
          Self.OverallComponentBorder.Color := clNone;
          Self.OverallComponentBorder.Thickness := 0;
          Self.OverallComponentBorder.CornerRadius := 4;
          Self.OverallComponentBorder.RoundCornerType := rctAll;
          Self.OverallComponentBorder.Style := psClear;
        end;
        if not FUserOverrides.CheckedIndicatorPosition_IsSet then Self.CheckedIndicatorPosition := cipLeftCenter;
        if not FUserOverrides.CheckedIndicatorSize_IsSet then Self.CheckedIndicatorSize := 20; // Win11 check indicators are around 20px
      end;
    end;
  finally
    TempCaptionFont.Free;
    FApplyingStyle := False;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetCurrentStyle(const Value: TANDMR_CCheckBoxStyle);
begin
  // Logic for SetCurrentStyle remains largely the same:
  // it clears FUserOverrides and calls ApplyStyle.
  if Value = cbsCustom then
  begin
    if FCurrentStyle <> cbsCustom then
    begin
      FCurrentStyle := cbsCustom;
      Invalidate;
    end;
    Exit;
  end;

  if FCurrentStyle = Value then
  begin
    InitializeUserOverrides;
    ApplyStyle(Value);
    Exit;
  end;

  if (FCurrentStyle <> Value) or (FCurrentStyle = cbsCustom) then
  begin
    InitializeUserOverrides;
    FCurrentStyle := Value;
    ApplyStyle(Value);
  end;
end;

procedure TANDMR_CCheckBox.SettingsChanged(Sender: TObject);
begin
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    if Sender = FIndicatorBorderSettings then // This is for the check element's border
      FUserOverrides.BorderSettings_IsCustomized := True
    else if Sender = FCaptionSettings then
      FUserOverrides.CaptionSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.OverallComponentBorderChanged(Sender: TObject);
begin
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.OverallComponentBorder_IsCustomized := True;
  end;
  Invalidate;
end;


procedure TANDMR_CCheckBox.HoverSettingsChanged(Sender: TObject);
begin
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.HoverSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetIndicatorBorder(const Value: TBorderSettings);
begin
  FIndicatorBorderSettings.Assign(Value); // For check element
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.BorderSettings_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.CaptionSettings_IsCustomized := True;
  end;
  Invalidate;
end;

// Setters for new properties
procedure TANDMR_CCheckBox.SetOverallComponentBorder(const Value: TBorderSettings);
begin
  FOverallComponentBorder.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.OverallComponentBorder_IsCustomized := True;
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetCheckedIndicatorPosition(const Value: TCheckBoxIndicatorPosition);
begin
  if FCheckedIndicatorPosition <> Value then
  begin
    FCheckedIndicatorPosition := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.CheckedIndicatorPosition_IsSet := True;
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetCheckedIndicatorSize(const Value: Integer);
begin
  if FCheckedIndicatorSize <> Value then
  begin
    FCheckedIndicatorSize := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.CheckedIndicatorSize_IsSet := True;
    end;
    Invalidate;
  end;
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
    csIndeterminate: SetState(csChecked); // Or csUnchecked, depending on desired behavior
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
    Click;
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
  BoxRect: TGPRectF;          // For the check element
  BoxDrawRect: TRect;         // TRect version of BoxRect
  CaptionPaintRect: TRect;    // Where caption is actually painted
  EffectiveCheckBoxSquareSize: Integer;
  LCurrentBoxColorUnchecked, LCurrentBoxColorChecked, LCurrentCheckMarkColor, LCurrentCaptionColor, LBoxFillColor: TColor;
  LIsHovering: Boolean;
  LHoverProgress: Single;
  LCaptionFont: TFont;        // Temporary font for modifications
  ActualCaptionFont: TFont;   // Font used for drawing
  Padding: Integer;
  CheckmarkThickness: Single;
  CombinedRectForFocus: TGPRectF;
  ActualCheckElementBorderColor: TColor; // Border color for the check element
  // ActualBoxBorderThickness: Integer; // Use FBorderSettings.Thickness directly
  // ActualCornerRadius: Integer;     // Use FBorderSettings.CornerRadius directly
  // ActualRoundCornerType: TRoundCornerType; // Use FBorderSettings.RoundCornerType directly
  LClientRectF: TGPRectF;
  NewTextMarginMultiplier: Integer;

  OverallBorderPaintRect: TRect; // Rect for drawing the overall component border
  ContentPaintRect: TRect;       // Rect inside the overall component border
  OverallBorderColor: TColor;
  OverallBorderThickness: Integer;
  OverallCornerRadius: Integer;
  OverallRoundCornerType: TRoundCornerType;

begin
  inherited Paint;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

    Padding := 2; // General padding
    NewTextMarginMultiplier := 3; // For caption next to check element

    // --- 1. Component Background ---
    if not FTransparent then
    begin
      var BackgroundBrush: TGPSolidBrush;
      var ComponentBGColor: TColor;

      if FIndicatorBorderSettings.BackgroundColor <> clNone then // Existing FBorderSettings.BackgroundColor is for component BG
        ComponentBGColor := FIndicatorBorderSettings.BackgroundColor
      else
        ComponentBGColor := Self.Color; // Fallback to control's Color if no specific BG is set

      LClientRectF.X := ClientRect.Left; LClientRectF.Y := ClientRect.Top;
      LClientRectF.Width := ClientRect.Width; LClientRectF.Height := ClientRect.Height;
      BackgroundBrush := TGPSolidBrush.Create(ColorToARGB(ComponentBGColor, 255));
      try
        LG.FillRectangle(BackgroundBrush, LClientRectF);
      finally
        BackgroundBrush.Free;
      end;
    end
    else
    begin
      // If transparent, VCL should handle drawing parent background if csParentBackground is set.
      // Or, if more explicit control is needed: StyleServices.DrawParentBackground(Self, Canvas.Handle, nil, True);
    end;

    // --- 2. Overall Component Border ---
    OverallBorderPaintRect := ClientRect;
    ContentPaintRect := ClientRect; // Start with full client rect, then deflate

    OverallBorderColor := FOverallComponentBorder.Color;
    OverallBorderThickness := FOverallComponentBorder.Thickness;
    OverallCornerRadius := FOverallComponentBorder.CornerRadius;
    OverallRoundCornerType := FOverallComponentBorder.RoundCornerType;

    // Adjust overall border color if hovering and enabled
    LIsHovering := FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and Self.Enabled;
    LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

    if LIsHovering and (FHoverSettings.BorderColor <> clNone) then // Check if overall border is visible
    begin
        // Assuming FHoverSettings.BorderColor is meant for the check element,
        // or we need a new FHoverSettings.OverallBorderColor.
        // For now, let's assume hover border color applies to check element.
        // If you want overall border to change on hover, add a specific property to THoverSettings.
    end;
    if not Self.Enabled then
    begin
        OverallBorderColor := BlendColors(OverallBorderColor, clGray, 0.60);
    end;


    if (OverallBorderThickness > 0) then
    begin
      // Use DrawEditBox to draw the overall border.
      // Pass clNone for fill color if DrawEditBox supports transparent fill,
      // otherwise, the background is already drawn.
      // The important part is that DrawEditBox draws the border lines.
      ANDMR_ComponentUtils.DrawEditBox(
        LG, OverallBorderPaintRect, clNone, // Fill color (clNone if only border is needed and BG is drawn)
        OverallBorderColor, OverallBorderThickness,
        psSolid, OverallCornerRadius, OverallRoundCornerType, 255
      );
      // Deflate ContentPaintRect to be inside the overall border
      InflateRect(ContentPaintRect, -OverallBorderThickness, -OverallBorderThickness);
    end;

    // --- 3. Determine Check Element Size ---
    if FCheckedIndicatorSize > 0 then
      EffectiveCheckBoxSquareSize := FCheckedIndicatorSize
    else // Auto-calculate based on ContentPaintRect height
      EffectiveCheckBoxSquareSize := Min(ContentPaintRect.Height - (Padding * 2), 18);

    if EffectiveCheckBoxSquareSize < 10 then EffectiveCheckBoxSquareSize := 10; // Minimum size

    // --- 4. Calculate Check Element Position (BoxRect) within ContentPaintRect ---
    // Initialize BoxRect (TGPRectF)
    BoxRect.Width := EffectiveCheckBoxSquareSize;
    BoxRect.Height := EffectiveCheckBoxSquareSize;

    case FCheckedIndicatorPosition of
      cipLeftTop:     begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Top + Padding; end;
      cipLeftCenter:  begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Top + (ContentPaintRect.Height - EffectiveCheckBoxSquareSize) / 2; end;
      cipLeftBottom:  begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Bottom - EffectiveCheckBoxSquareSize - Padding; end;

      cipTopLeft:     begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Top + Padding; end;
      cipTopCenter:   begin BoxRect.X := ContentPaintRect.Left + (ContentPaintRect.Width - EffectiveCheckBoxSquareSize) / 2; BoxRect.Y := ContentPaintRect.Top + Padding; end;
      cipTopRight:    begin BoxRect.X := ContentPaintRect.Right - EffectiveCheckBoxSquareSize - Padding; BoxRect.Y := ContentPaintRect.Top + Padding; end;

      cipRightTop:    begin BoxRect.X := ContentPaintRect.Right - EffectiveCheckBoxSquareSize - Padding; BoxRect.Y := ContentPaintRect.Top + Padding; end;
      cipRightCenter: begin BoxRect.X := ContentPaintRect.Right - EffectiveCheckBoxSquareSize - Padding; BoxRect.Y := ContentPaintRect.Top + (ContentPaintRect.Height - EffectiveCheckBoxSquareSize) / 2; end;
      cipRightBottom: begin BoxRect.X := ContentPaintRect.Right - EffectiveCheckBoxSquareSize - Padding; BoxRect.Y := ContentPaintRect.Bottom - EffectiveCheckBoxSquareSize - Padding; end;

      cipBottomLeft:  begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Bottom - EffectiveCheckBoxSquareSize - Padding; end;
      cipBottomCenter:begin BoxRect.X := ContentPaintRect.Left + (ContentPaintRect.Width - EffectiveCheckBoxSquareSize) / 2; BoxRect.Y := ContentPaintRect.Bottom - EffectiveCheckBoxSquareSize - Padding; end;
      cipBottomRight: begin BoxRect.X := ContentPaintRect.Right - EffectiveCheckBoxSquareSize - Padding; BoxRect.Y := ContentPaintRect.Bottom - EffectiveCheckBoxSquareSize - Padding; end;

      cipCenter:      begin BoxRect.X := ContentPaintRect.Left + (ContentPaintRect.Width - EffectiveCheckBoxSquareSize) / 2; BoxRect.Y := ContentPaintRect.Top + (ContentPaintRect.Height - EffectiveCheckBoxSquareSize) / 2; end;
    else // Default to cipLeftCenter
      begin BoxRect.X := ContentPaintRect.Left + Padding; BoxRect.Y := ContentPaintRect.Top + (ContentPaintRect.Height - EffectiveCheckBoxSquareSize) / 2; end;
    end;

    // Ensure BoxRect is within ContentPaintRect (crude clipping, might need refinement)
    if BoxRect.X < ContentPaintRect.Left then BoxRect.X := ContentPaintRect.Left;
    if BoxRect.Y < ContentPaintRect.Top then BoxRect.Y := ContentPaintRect.Top;
    if BoxRect.X + BoxRect.Width > ContentPaintRect.Right then BoxRect.Width := ContentPaintRect.Right - BoxRect.X;
    if BoxRect.Y + BoxRect.Height > ContentPaintRect.Bottom then BoxRect.Height := ContentPaintRect.Bottom - BoxRect.Y;


    // --- 5. Calculate Caption Paint Rect ---
    // Initialize with the full ContentPaintRect, then adjust based on indicator position
    CaptionPaintRect := ContentPaintRect;

    case FCheckedIndicatorPosition of
      cipLeftTop, cipLeftCenter, cipLeftBottom:
        CaptionPaintRect.Left := Round(BoxRect.X + BoxRect.Width + (Padding * Max(1, NewTextMarginMultiplier -1) )); // Space for check element + margin
      cipRightTop, cipRightCenter, cipRightBottom:
        CaptionPaintRect.Right := Round(BoxRect.X - (Padding * Max(1, NewTextMarginMultiplier -1) ));
      cipTopLeft, cipTopCenter, cipTopRight:
        CaptionPaintRect.Top := Round(BoxRect.Y + BoxRect.Height + Padding);
      cipBottomLeft, cipBottomCenter, cipBottomRight:
        CaptionPaintRect.Bottom := Round(BoxRect.Y - Padding);
      cipCenter: // If indicator is in center, caption might be tricky.
                 // For now, let's assume caption is not drawn or drawn around it.
                 // This example will effectively hide caption if indicator is centered and large.
        begin
          // Decide how to handle caption when indicator is centered.
          // Option 1: Don't draw caption.
          CaptionPaintRect := System.Types.Rect(0,0,0,0); // Effectively hide
          // Option 2: Try to draw it to one side (e.g., to the right if space)
          // if (ContentPaintRect.Right - (BoxRect.X + BoxRect.Width)) > (Canvas.TextWidth(FCaptionSettings.Text) + Padding) then
          // CaptionPaintRect.Left := Round(BoxRect.X + BoxRect.Width + Padding)
          // else CaptionPaintRect := System.Types.Rect(0,0,0,0);
        end;
    end;
    // Ensure caption rect is valid
    if CaptionPaintRect.Right < CaptionPaintRect.Left then CaptionPaintRect.Right := CaptionPaintRect.Left;
    if CaptionPaintRect.Bottom < CaptionPaintRect.Top then CaptionPaintRect.Bottom := CaptionPaintRect.Top;


    // --- 6. Determine Colors and Font based on State (Hover, Disabled) ---
    LCurrentBoxColorUnchecked := FBoxColorUnchecked;
    LCurrentBoxColorChecked := FBoxColorChecked;
    LCurrentCheckMarkColor := FCheckMarkColor;
    ActualCheckElementBorderColor := FIndicatorBorderSettings.Color; // From existing BorderSettings for check element

    ActualCaptionFont := TFont.Create;
    LCaptionFont := TFont.Create;
    try
      LCaptionFont.Assign(FCaptionSettings.Font);
      ActualCaptionFont.Assign(LCaptionFont); // Start with base caption font
      LCurrentCaptionColor := ActualCaptionFont.Color;

      if LIsHovering then
      begin
        if FHoverSettings.BackgroundColor <> clNone then // This hover BG applies to check element fill
        begin
          LCurrentBoxColorUnchecked := BlendColors(LCurrentBoxColorUnchecked, FHoverSettings.BackgroundColor, LHoverProgress);
          LCurrentBoxColorChecked := BlendColors(LCurrentBoxColorChecked, FHoverSettings.BackgroundColor, LHoverProgress);
        end;
        if FHoverSettings.FontColor <> clNone then
          LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, FHoverSettings.FontColor, LHoverProgress);

        if FHoverSettings.BorderColor <> clNone then // This hover border applies to check element
           ActualCheckElementBorderColor := BlendColors(ActualCheckElementBorderColor, FHoverSettings.BorderColor, LHoverProgress);
      end;

      if not Self.Enabled then
      begin
        LCurrentBoxColorUnchecked := BlendColors(LCurrentBoxColorUnchecked, clGray, 0.60);
        LCurrentBoxColorChecked := BlendColors(LCurrentBoxColorChecked, clGray, 0.60);
        LCurrentCheckMarkColor := BlendColors(LCurrentCheckMarkColor, clGray, 0.60);
        ActualCheckElementBorderColor := BlendColors(ActualCheckElementBorderColor, clGray, 0.60);

        if FCaptionSettings.DisabledColor <> clNone then
          LCurrentCaptionColor := FCaptionSettings.DisabledColor
        else
          LCurrentCaptionColor := BlendColors(LCurrentCaptionColor, clGray, 0.50);
      end;
    finally
      LCaptionFont.Free;
    end;
    ActualCaptionFont.Color := LCurrentCaptionColor;


    // --- 7. Draw Check Element (Box and Mark/Indeterminate) ---
    case FState of
      csChecked: LBoxFillColor := LCurrentBoxColorChecked;
      csUnchecked: LBoxFillColor := LCurrentBoxColorUnchecked;
      csIndeterminate: LBoxFillColor := LCurrentBoxColorUnchecked; // Or specific color for indeterminate fill
    else
      LBoxFillColor := LCurrentBoxColorUnchecked;
    end;

    BoxDrawRect := System.Types.Rect(Round(BoxRect.X), Round(BoxRect.Y), Round(BoxRect.X + BoxRect.Width), Round(BoxRect.Y + BoxRect.Height));

    // Draw the check element using existing FBorderSettings for its border style
    ANDMR_ComponentUtils.DrawEditBox(
      LG, BoxDrawRect, LBoxFillColor, ActualCheckElementBorderColor,
      FIndicatorBorderSettings.Thickness, psSolid, FIndicatorBorderSettings.CornerRadius, FIndicatorBorderSettings.RoundCornerType, 255
    );

    // Draw CheckMark or Indeterminate Mark
    if (BoxRect.Width > 0) And (BoxRect.Height > 0) then // Only draw if size is valid
    begin
        if FState = csChecked then
        begin
          CheckmarkThickness := Max(1.5, EffectiveCheckBoxSquareSize / 8);
          LGPPen := TGPPen.Create(ColorToARGB(LCurrentCheckMarkColor, 255), CheckmarkThickness);
          LGPPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
          try
            SetLength(LPoints, 3);
            LPoints[0].X := BoxRect.X + BoxRect.Width * 0.20; LPoints[0].Y := BoxRect.Y + BoxRect.Height * 0.50;
            LPoints[1].X := BoxRect.X + BoxRect.Width * 0.45; LPoints[1].Y := BoxRect.Y + BoxRect.Height * 0.75;
            LPoints[2].X := BoxRect.X + BoxRect.Width * 0.80; LPoints[2].Y := BoxRect.Y + BoxRect.Height * 0.25;
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
            IndeterminateSymbolRect.Width := BoxRect.Width * 0.6;
            IndeterminateSymbolRect.Height := Max(2, EffectiveCheckBoxSquareSize / 7);
            IndeterminateSymbolRect.X := BoxRect.X + (BoxRect.Width - IndeterminateSymbolRect.Width) / 2;
            IndeterminateSymbolRect.Y := BoxRect.Y + (BoxRect.Height - IndeterminateSymbolRect.Height) / 2;
            LG.FillRectangle(LGPBrush, IndeterminateSymbolRect);
          finally
            LGPBrush.Free;
          end;
        end;
    end;

    // --- 8. Draw Caption ---
    if (FCaptionSettings.Text <> '') and (CaptionPaintRect.Right > CaptionPaintRect.Left) and (CaptionPaintRect.Bottom > CaptionPaintRect.Top) then
    begin
      // Add a small internal padding for the caption text within its calculated paint rect
      var FinalCaptionDrawRect := CaptionPaintRect;
      InflateRect(FinalCaptionDrawRect, -Padding, -Padding div 2); // Small horizontal and vertical padding

      if (FinalCaptionDrawRect.Right > FinalCaptionDrawRect.Left) And (FinalCaptionDrawRect.Bottom > FinalCaptionDrawRect.Top) then
      begin
          ANDMR_ComponentUtils.DrawComponentCaption(
            Self.Canvas, FinalCaptionDrawRect, FCaptionSettings.Text, ActualCaptionFont,
            ActualCaptionFont.Color, FCaptionSettings.Alignment,
            FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, 255
          );
      end;
    end;
    ActualCaptionFont.Free;


    // --- 9. Draw Focus Rectangle ---
    if Self.Focused and Self.TabStop and Self.Enabled then
    begin
      LGPPath := TGPGraphicsPath.Create;
      LGPPen := nil;
      try
        // Focus rectangle should encompass the visible elements within ContentPaintRect
        // This needs to be calculated based on the actual drawn positions of BoxRect and CaptionPaintRect
        var FocusContentLeft, FocusContentTop, FocusContentRight, FocusContentBottom: Single;

        FocusContentLeft := BoxRect.X;
        FocusContentTop := BoxRect.Y;
        FocusContentRight := BoxRect.X + BoxRect.Width;
        FocusContentBottom := BoxRect.Y + BoxRect.Height;

        if (FCaptionSettings.Text <> '') and (CaptionPaintRect.Right > CaptionPaintRect.Left) then
        begin
            FocusContentLeft   := Min(FocusContentLeft, CaptionPaintRect.Left);
            FocusContentTop    := Min(FocusContentTop, CaptionPaintRect.Top);
            FocusContentRight  := Max(FocusContentRight, CaptionPaintRect.Right);
            FocusContentBottom := Max(FocusContentBottom, CaptionPaintRect.Bottom);
        end;

        // Ensure focus rect is within ContentPaintRect bounds as a fallback
        FocusContentLeft   := Max(FocusContentLeft, ContentPaintRect.Left);
        FocusContentTop    := Max(FocusContentTop, ContentPaintRect.Top);
        FocusContentRight  := Min(FocusContentRight, ContentPaintRect.Right);
        FocusContentBottom := Min(FocusContentBottom, ContentPaintRect.Bottom);

        if (FocusContentRight > FocusContentLeft) and (FocusContentBottom > FocusContentTop) then
        begin
            CombinedRectForFocus.X := FocusContentLeft - Padding;
            CombinedRectForFocus.Y := FocusContentTop - Padding;
            CombinedRectForFocus.Width := (FocusContentRight - FocusContentLeft) + (Padding * 2);
            CombinedRectForFocus.Height := (FocusContentBottom - FocusContentTop) + (Padding * 2);

            // Clip focus rect to ContentPaintRect to avoid drawing outside overall border
            if CombinedRectForFocus.X < ContentPaintRect.Left then CombinedRectForFocus.X := ContentPaintRect.Left;
            if CombinedRectForFocus.Y < ContentPaintRect.Top then CombinedRectForFocus.Y := ContentPaintRect.Top;
            if CombinedRectForFocus.X + CombinedRectForFocus.Width > ContentPaintRect.Right then
               CombinedRectForFocus.Width := ContentPaintRect.Right - CombinedRectForFocus.X;
            if CombinedRectForFocus.Y + CombinedRectForFocus.Height > ContentPaintRect.Bottom then
               CombinedRectForFocus.Height := ContentPaintRect.Bottom - CombinedRectForFocus.Y;

            if (CombinedRectForFocus.Width > 0) And (CombinedRectForFocus.Height > 0) then
            begin
                LGPPath.AddRectangle(CombinedRectForFocus);
                LGPPen := TGPPen.Create(ColorToARGB(LCurrentCaptionColor, 180)); // Use caption color for focus
                LGPPen.SetDashStyle(DashStyleDash);
                LG.DrawPath(LGPPen, LGPPath);
            end;
        end;
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
    FCaptionSettings.Text := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.CaptionSettings_IsCustomized := True; // Already exists
    end;
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
  if Value then NewState := csChecked else NewState := csUnchecked;
  if FState <> NewState then
  begin
    SetState(NewState);
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

procedure TANDMR_CCheckBox.SetBoxColorUnchecked(const Value: TColor);
begin
  if FBoxColorUnchecked <> Value then
  begin
    FBoxColorUnchecked := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.BoxColorUnchecked_IsSet := True; // Already exists
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetBoxColorChecked(const Value: TColor);
begin
  if FBoxColorChecked <> Value then
  begin
    FBoxColorChecked := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.BoxColorChecked_IsSet := True; // Already exists
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetCheckMarkColor(const Value: TColor);
begin
  if FCheckMarkColor <> Value then
  begin
    FCheckMarkColor := Value;
    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.CheckMarkColor_IsSet := True; // Already exists
    end;
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

    if not FApplyingStyle then
    begin
      FCurrentStyle := cbsCustom;
      FUserOverrides.Transparent_IsSet := True; // Already exists
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CCheckBox.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  if not FApplyingStyle then
  begin
    FCurrentStyle := cbsCustom;
    FUserOverrides.HoverSettings_IsCustomized := True; // Already exists
  end;
  Invalidate;
end;

procedure TANDMR_CCheckBox.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then // Use inherited GetEnabled
  begin
    inherited SetEnabled(Value);
    // CMEnabledChanged will be called, which calls Invalidate.
  end;
end;

end.
