unit ANDMR_CCheckBox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.StdCtrls, System.Types,
  System.UITypes, ANDMR_ComponentUtils, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL; // Added GDI+ units

type
  TCCheckBoxStyle = (cbsLight, cbsDark, cbsMaterial, cbsFlat, cbsModern, cbsIOS, cbsWin11);
  TCCheckBoxElementStyle = (cbeseChecked, cbeseBordered, cbeseSolid);

  TANDMR_CCheckBox = class(TCustomControl)
  private
    FChecked: Boolean;
    FStyle: TCCheckBoxStyle;
    FElementStyle: TCCheckBoxElementStyle;
    FCaption: string;
    FColor: TColor;
    FOverallBorderColor: TColor; // For checkbox element border primarily
    FCheckMarkColor: TColor;
    FElementBoxColor: TColor; // Fill color for the checkbox element
    FTransparent: Boolean;
    FOnClick: TNotifyEvent;
    FOnChange: TNotifyEvent;

    FImageSettings: TImageSettings;
    FCaptionSettings: TCaptionSettings;
    FBorderSettings: TBorderSettings; // For the main component border
    FHoverSettings: THoverSettings;

    FIsHovering: Boolean;
    FElementCornerRadius: Integer; // New: Radius for the checkbox element corners

    procedure SetChecked(const Value: Boolean);
    function GetChecked: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetStyle(const Value: TCCheckBoxStyle);
    procedure SetElementStyle(const Value: TCCheckBoxElementStyle);
    procedure SetColor(const Value: TColor);
    procedure SetOverallBorderColor(const Value: TColor);
    procedure SetCheckMarkColor(const Value: TColor);
    procedure SetElementBoxColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetElementCornerRadius(const Value: Integer);

    function GetFont: TFont;
    procedure SetFont(const Value: TFont);

    procedure SetImageSettings(const Value: TImageSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetHoverSettings(const Value: THoverSettings);

  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;


    procedure Loaded; override;

    procedure InternalCaptionSettingsChanged(Sender: TObject);
    procedure InternalImageSettingsChanged(Sender: TObject);
    procedure InternalBorderSettingsChanged(Sender: TObject);
    procedure InternalHoverSettingsChanged(Sender: TObject);
    procedure InternalFontChanged(Sender: TObject);


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Checked: Boolean read GetChecked write SetChecked default False;
    property Caption: string read FCaption write SetCaption;
    property Style: TCCheckBoxStyle read FStyle write SetStyle default cbsLight;
    property ElementStyle: TCCheckBoxElementStyle read FElementStyle write SetElementStyle default cbeseChecked;
    property ElementCornerRadius: Integer read FElementCornerRadius write SetElementCornerRadius default 2; // New published property

    property Color: TColor read FColor write SetColor default clWindow;
    property OverallBorderColor: TColor read FOverallBorderColor write SetOverallBorderColor default clGray;
    property CheckMarkColor: TColor read FCheckMarkColor write SetCheckMarkColor default clWindowText;
    property ElementBoxColor: TColor read FElementBoxColor write SetElementBoxColor default clBtnFace;

    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property Image: TImageSettings read FImageSettings write SetImageSettings;
    property CaptionProperties: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property Border: TBorderSettings read FBorderSettings write SetBorderSettings; // Main component border
    property Hover: THoverSettings read FHoverSettings write SetHoverSettings;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font: TFont read GetFont write SetFont;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CCheckBox]);
end;

{ TANDMR_CCheckBox }

constructor TANDMR_CCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csAcceptsControls, csSetCaption];
  Width := 120;
  Height := 25;
  TabStop := True;

  FChecked := False;
  FStyle := cbsLight;
  FElementStyle := cbeseChecked;
  FElementCornerRadius := 2; // Default radius for checkbox element
  FColor := clWindow;
  FOverallBorderColor := clGray; // Default border for element
  FCheckMarkColor := clWindowText;
  FElementBoxColor := clBtnFace;
  FTransparent := False;
  FIsHovering := False;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := InternalCaptionSettingsChanged;
  FCaptionSettings.Font.OnChange := InternalFontChanged;
  FCaption := Name;
  FCaptionSettings.Text := FCaption;
  FCaptionSettings.Position := cpRight;
  FCaptionSettings.Alignment := taLeftJustify;
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.Offset := Point(5, 0);
  FCaptionSettings.Color := Self.Font.Color;

  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := InternalImageSettingsChanged;
  FImageSettings.Visible := False;

  FBorderSettings := TBorderSettings.Create; // For main component border
  FBorderSettings.OnChange := InternalBorderSettingsChanged;
  FBorderSettings.Thickness := 0; // Default to no main component border for a cleaner look
  FBorderSettings.Color := clBlack;
  FBorderSettings.BackgroundColor := FColor;
  FBorderSettings.Visible := False; // Main border not visible by default

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := InternalHoverSettingsChanged;
  FHoverSettings.Enabled := True;
  FHoverSettings.BackgroundColor := clNone; // No background color change on hover by default
  FHoverSettings.BorderColor := clNone;     // No border color change on hover by default
  FHoverSettings.CaptionFontColor := FCaptionSettings.Color;
  FHoverSettings.FontColor := FCaptionSettings.Color;
end;

destructor TANDMR_CCheckBox.Destroy;
begin
  FCaptionSettings.OnChange := nil;
  if Assigned(FCaptionSettings.Font) then FCaptionSettings.Font.OnChange := nil;
  FCaptionSettings.Free;
  FImageSettings.OnChange := nil;
  FImageSettings.Free;
  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;
  FHoverSettings.OnChange := nil;
  FHoverSettings.Free;
  inherited Destroy;
end;

procedure TANDMR_CCheckBox.Loaded;
begin
  inherited Loaded;
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  InternalCaptionSettingsChanged(Self);
  InternalBorderSettingsChanged(Self);
  InternalImageSettingsChanged(Self);
  InternalHoverSettingsChanged(Self);
  Repaint;
end;

procedure TANDMR_CCheckBox.Paint;
var
  GP: TGPGraphics;
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  CheckBoxRectF: TGPRectF;
  CheckElementSize: Integer;
  CaptionRect: TRect;
  EffectiveClientRect: TRect;
  LCurrentColor, LCurrentOverallBorderColor, LCurrentElementBoxColor, LCurrentCheckMarkColor, LCurrentCaptionColor: TColor;
  LCurrentElementBorderColor: TColor; // Specific for the checkbox element's border
  LHoverProgress: Single;
  LCheckElementRadius: Single;
  LDrawElementBorder: Boolean;
  LFillElementBox: Boolean;
begin
  inherited Paint;
  EffectiveClientRect := GetClientRect;
  LHoverProgress := 0;

  if FIsHovering and FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and not (csDesigning in ComponentState) then
    LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;

  // Determine effective colors based on state (normal, hover, disabled)
  if not Enabled then
  begin
    LCurrentColor := BlendColors(FColor, clGray, 0.65);
    LCurrentOverallBorderColor := BlendColors(FBorderSettings.Color, clGray, 0.70); // Main component border
    LCurrentElementBorderColor := BlendColors(FOverallBorderColor, clGray, 0.60); // Checkbox element border
    LCurrentElementBoxColor := BlendColors(FElementBoxColor, clGray, 0.55);
    LCurrentCheckMarkColor := BlendColors(FCheckMarkColor, clGray, 0.50);
    LCurrentCaptionColor := FCaptionSettings.DisabledColor;
  end
  else
  begin
    LCurrentColor := FColor;
    LCurrentOverallBorderColor := FBorderSettings.Color; // Main component border
    LCurrentElementBorderColor := FOverallBorderColor;   // Checkbox element border
    LCurrentElementBoxColor := FElementBoxColor;
    LCurrentCheckMarkColor := FCheckMarkColor;
    LCurrentCaptionColor := FCaptionSettings.Color;

    if LHoverProgress > 0 then
    begin
      if FHoverSettings.BackgroundColor <> clNone then // Hover for main component background
        LCurrentColor := BlendColors(FColor, FHoverSettings.BackgroundColor, LHoverProgress);
      if FHoverSettings.BorderColor <> clNone then // Hover for main component border
        LCurrentOverallBorderColor := BlendColors(FBorderSettings.Color, FHoverSettings.BorderColor, LHoverProgress);

      // Hover for ElementBoxColor and its Border
      LCurrentElementBoxColor := BlendColors(FElementBoxColor, LighterColor(FElementBoxColor, 20), LHoverProgress);
      LCurrentElementBorderColor := BlendColors(FOverallBorderColor, LighterColor(FOverallBorderColor, 30), LHoverProgress);


      if FHoverSettings.CaptionFontColor <> clNone then
        LCurrentCaptionColor := BlendColors(FCaptionSettings.Color, FHoverSettings.CaptionFontColor, LHoverProgress)
      else if FHoverSettings.FontColor <> clNone then
        LCurrentCaptionColor := BlendColors(FCaptionSettings.Color, FHoverSettings.FontColor, LHoverProgress);
    end;
  end;

  GP := TGPGraphics.Create(Canvas.Handle);
  try
    GP.SetSmoothingMode(SmoothingModeAntiAlias);
    GP.SetPixelOffsetMode(PixelOffsetModeHalf);

    // Background
    if not FTransparent then
    begin
      LBrush := TGPSolidBrush.Create(ColorToARGB(LCurrentColor));
      try GP.FillRectangle(LBrush, EffectiveClientRect.Left, EffectiveClientRect.Top, EffectiveClientRect.Width, EffectiveClientRect.Height);
      finally LBrush.Free; end;
    end;

    // Main Component Border (using FBorderSettings)
    if FBorderSettings.Visible and (FBorderSettings.Thickness > 0) and not FTransparent then
    begin
      LPath := TGPGraphicsPath.Create;
      LPen := TGPPen.Create(ColorToARGB(LCurrentOverallBorderColor), FBorderSettings.Thickness);
      try
        var ComponentRectF: TGPRectF;
        ComponentRectF.X := EffectiveClientRect.Left + FBorderSettings.Thickness / 2;
        ComponentRectF.Y := EffectiveClientRect.Top + FBorderSettings.Thickness / 2;
        ComponentRectF.Width := EffectiveClientRect.Width - FBorderSettings.Thickness;
        ComponentRectF.Height := EffectiveClientRect.Height - FBorderSettings.Thickness;
        ComponentRectF.Width  := Max(0.0, ComponentRectF.Width);
        ComponentRectF.Height := Max(0.0, ComponentRectF.Height);

        CreateGPRoundedPath(LPath, ComponentRectF, FBorderSettings.CornerRadius, FBorderSettings.RoundCornerType);
        GP.DrawPath(LPen, LPath);
      finally
        LPath.Free;
        LPen.Free;
      end;
    end;

  // --- Layout Metrics ---
  var DrawableRect: TRect; // Inner rect after accounting for main component border
  var EstimatedCaptionSize: TSize = TSize.Create(0,0);
  var EstimatedImageSize: TSize = TSize.Create(0,0);
  var CheckBoxOffsetX, CheckBoxOffsetY: Integer;

  DrawableRect := EffectiveClientRect;
  if FBorderSettings.Visible and (FBorderSettings.Thickness > 0) then
    InflateRect(DrawableRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  CheckBoxOffsetX := DrawableRect.Left + 2; // Default start X for CheckBox
  CheckBoxOffsetY := DrawableRect.Top + (DrawableRect.Height - CheckElementSize) div 2; // Default start Y (centered)

  // Estimate caption space
    if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') then
  begin
    Canvas.Font.Assign(FCaptionSettings.Font);
    EstimatedCaptionSize.cx := Canvas.TextWidth(FCaptionSettings.Text) + FCaptionSettings.Offset.X;
    EstimatedCaptionSize.cy := Canvas.TextHeight(FCaptionSettings.Text) + FCaptionSettings.Offset.Y;
  end;

  // Estimate image space
  var ActualImageWidth, ActualImageHeight: Integer;
  ActualImageWidth := 0; ActualImageHeight := 0;

  if FImageSettings.Visible and Assigned(FImageSettings.Picture) and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
  begin
    ActualImageWidth := IfThen(FImageSettings.TargetWidth > 0, FImageSettings.TargetWidth, FImageSettings.Picture.Graphic.Width);
    ActualImageHeight := IfThen(FImageSettings.TargetHeight > 0, FImageSettings.TargetHeight, FImageSettings.Picture.Graphic.Height);

    if ActualImageWidth <= 0 then ActualImageWidth := 20; // Default fallback if graphic is empty and no target
    if ActualImageHeight <= 0 then ActualImageHeight := 20; // Default fallback

    EstimatedImageSize.cx := ActualImageWidth + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
    EstimatedImageSize.cy := ActualImageHeight + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;
  end
  else
  begin
    EstimatedImageSize.cx := 0;
    EstimatedImageSize.cy := 0;
  end;

  // --- Checkbox Element Size & Position ---
  // Initial CheckElementSize based on available height
  CheckElementSize := Max(10, Min(DrawableRect.Height - 4, 20));

  // Adjust CheckElementSize and CheckBoxOffsetX based on caption/image to the sides
  var HorizontalSpacingNeeded: Integer = 0;
  if FCaptionSettings.Visible and (FCaptionSettings.Position in [cpLeft, cpRight]) then
    HorizontalSpacingNeeded := HorizontalSpacingNeeded + EstimatedCaptionSize.cx;
  if FImageSettings.Visible and (FImageSettings.Position in [ipsLeft, ipsRight, ipsLeftOfCaption, ipsRightOfCaption]) then // Simplified check
    HorizontalSpacingNeeded := HorizontalSpacingNeeded + EstimatedImageSize.cx;

  CheckElementSize := Min(CheckElementSize, DrawableRect.Width - 4 - HorizontalSpacingNeeded);
  CheckElementSize := Max(10, CheckElementSize); // Ensure minimum size

  // Final CheckBoxRectF (assuming BiDiMode = bdLeftToRight for now)
  // If caption is to the left, offset checkbox.
  if FCaptionSettings.Visible and (FCaptionSettings.Position = cpLeft) then
     CheckBoxOffsetX := CheckBoxOffsetX + EstimatedCaptionSize.cx;
  // If image is to the left of checkbox (and caption is not cpLeft), offset checkbox.
  if FImageSettings.Visible and (FImageSettings.Position = ipsLeft) and not (FCaptionSettings.Visible and FCaptionSettings.Position = cpLeft) then
     CheckBoxOffsetX := CheckBoxOffsetX + EstimatedImageSize.cx;


  CheckBoxRectF.X := CheckBoxOffsetX;
  CheckBoxRectF.Y := DrawableRect.Top + (DrawableRect.Height - CheckElementSize) / 2; // Recenter with final CheckElementSize
    CheckBoxRectF.Width := CheckElementSize;
    CheckBoxRectF.Height := CheckElementSize;


    LPath := TGPGraphicsPath.Create;
    LCheckElementRadius := FElementCornerRadius;
    LDrawElementBorder := True;
    LFillElementBox := True;

    // Style-specific color adjustments and properties
    case FStyle of
      cbsLight:
        begin
          LCheckElementRadius := FElementCornerRadius;
          // Default colors are fine for cbsLight
        end;
      cbsDark:
        begin
          LCheckElementRadius := FElementCornerRadius;
          // Base colors are already set for LCurrent variables from general logic
          // No specific overrides for cbsLight, it uses the general logic + FElementStyle
        end;
      cbsDark:
        begin
          LCheckElementRadius := FElementCornerRadius;
          var BaseDarkBg: TColor = StringToColor('#333333');
          var BaseDarkElementBg: TColor = StringToColor('#404040');
          var BaseDarkElementBorder: TColor = StringToColor('#555555');
          var BaseDarkCheckMark: TColor = clSilver;
          var BaseDarkCaption: TColor = clSilver;

          if not Enabled then
          begin
            LCurrentColor := BlendColors(ColorOrDefault(FColor, BaseDarkBg), clGray, 0.65);
            LCurrentOverallBorderColor := BlendColors(ColorOrDefault(FBorderSettings.Color, BaseDarkElementBorder), clGray, 0.70);
            LCurrentElementBorderColor := BlendColors(ColorOrDefault(FOverallBorderColor, BaseDarkElementBorder), clGray, 0.60);
            LCurrentElementBoxColor := BlendColors(ColorOrDefault(FElementBoxColor, BaseDarkElementBg), clGray, 0.55);
            LCurrentCheckMarkColor := BlendColors(ColorOrDefault(FCheckMarkColor, BaseDarkCheckMark), clGray, 0.50);
            LCurrentCaptionColor := BlendColors(ColorOrDefault(FCaptionSettings.Color, BaseDarkCaption), FCaptionSettings.DisabledColor, 0.5);
          end
          else
          begin
            LCurrentColor := ColorOrDefault(FColor, BaseDarkBg);
            LCurrentOverallBorderColor := ColorOrDefault(FBorderSettings.Color, BaseDarkElementBorder);
            LCurrentElementBorderColor := ColorOrDefault(FOverallBorderColor, BaseDarkElementBorder);
            LCurrentElementBoxColor := ColorOrDefault(FElementBoxColor, BaseDarkElementBg);
            LCurrentCheckMarkColor := ColorOrDefault(FCheckMarkColor, BaseDarkCheckMark);
            LCurrentCaptionColor := ColorOrDefault(FCaptionSettings.Color, BaseDarkCaption);

            if LHoverProgress > 0 then
            begin
              LCurrentElementBoxColor := BlendColors(LCurrentElementBoxColor, LighterColor(LCurrentElementBoxColor, 30), LHoverProgress);
              LCurrentElementBorderColor := BlendColors(LCurrentElementBorderColor, LighterColor(LCurrentElementBorderColor, 40), LHoverProgress);
              if FHoverSettings.BackgroundColor <> clNone then
                 LCurrentColor := BlendColors(LCurrentColor, FHoverSettings.BackgroundColor, LHoverProgress);
            end;
          end;
        end;
      cbsMaterial:
        begin
          LCheckElementRadius := Max(FElementCornerRadius, 2);
          var BaseMaterialAccent: TColor = FElementBoxColor; // User defined accent
          var BaseMaterialCheckmark: TColor = ColorOrDefault(FCheckMarkColor, clWhite);
          var BaseMaterialUncheckedBorder: TColor = ColorOrDefault(FOverallBorderColor, clGray);

          if not Enabled then
          begin
            LCurrentElementBorderColor := BlendColors(BaseMaterialUncheckedBorder, clSilver, 0.8);
            LCurrentElementBoxColor := BlendColors(BaseMaterialAccent, clSilver, 0.9);
            LCurrentCheckMarkColor := clGray;
            LCurrentCaptionColor := FCaptionSettings.DisabledColor;
          end
          else
          begin
            LCurrentCaptionColor := FCaptionSettings.Color; // Standard caption color
            if FChecked then
            begin
              LCurrentElementBoxColor := BaseMaterialAccent;
              LCurrentElementBorderColor := BaseMaterialAccent;
              LCurrentCheckMarkColor := BaseMaterialCheckmark;
              LDrawElementBorder := True; LFillElementBox := True;
            end
            else // Unchecked
            begin
              LCurrentElementBoxColor := clNone;
              LCurrentElementBorderColor := BaseMaterialUncheckedBorder;
              LDrawElementBorder := True; LFillElementBox := False;
            end;

            if LHoverProgress > 0 then
            begin
              if FChecked then
                LCurrentElementBoxColor := BlendColors(BaseMaterialAccent, ColorToARGB(clBlack, 40), LHoverProgress)
              else
                LCurrentElementBorderColor := BlendColors(BaseMaterialUncheckedBorder, BaseMaterialAccent, LHoverProgress);
            end;
          end;
        end;
      cbsFlat:
        begin
          LCheckElementRadius := 0;
          LFillElementBox := FChecked Or (FIsHovering and Enabled);
          LDrawElementBorder := FChecked Or (FIsHovering and Enabled); // Border only if checked or hovered
          if not LFillElementBox then LCurrentElementBoxColor := clNone; // No fill if not checked/hovered
          if not LDrawElementBorder then LCurrentElementBorderColor := clNone; // No border if not checked/hovered
        end;
      cbsModern:
        begin
          LCheckElementRadius := Max(FElementCornerRadius, 3); // Slightly more rounded default
          // Uses base colors (FColor, FElementBoxColor etc.) via LCurrent... vars already set
          // Hover: Subtle highlight (already handled by general hover logic on LCurrent... vars)
          // Disabled: Already handled by general disabled logic
        end;
      cbsIOS:
        begin
          LCheckElementRadius := Max(FElementCornerRadius, CheckElementSize / 2.8); // Very rounded
          var IOSGreen: TColor = StringToColor('#34C759');
          var IOSUncheckedBorder: TColor = ColorOrDefault(FOverallBorderColor, StringToColor('#AEAEB2'));
          var IOSCheckMark: TColor = ColorOrDefault(FCheckMarkColor, clWhite);

          if not Enabled then
          begin
            LCurrentElementBoxColor := BlendColors(IOSGreen, clSilver, 0.85);
            LCurrentElementBorderColor := BlendColors(IOSUncheckedBorder, clSilver, 0.7);
            LCurrentCheckMarkColor := clGray;
            LCurrentCaptionColor := FCaptionSettings.DisabledColor;
          end
          else
          begin
            LCurrentCaptionColor := FCaptionSettings.Color;
            if FChecked then
            begin
              LCurrentElementBoxColor := ColorOrDefault(FElementBoxColor, IOSGreen);
              LCurrentElementBorderColor := LCurrentElementBoxColor; // No distinct border when checked
              LCurrentCheckMarkColor := IOSCheckMark;
              LFillElementBox := True; LDrawElementBorder := True; // Or False for border if truly borderless
            end
            else // Unchecked
            begin
              LCurrentElementBoxColor := clNone; // Or a very light gray fill: StringToColor('#EFEFF4');
              LCurrentElementBorderColor := IOSUncheckedBorder;
              LFillElementBox := False; LDrawElementBorder := True;
            end;
            if LHoverProgress > 0 then
            begin
              if FChecked then
                LCurrentElementBoxColor := BlendColors(ColorOrDefault(FElementBoxColor, IOSGreen), ColorToARGB(clBlack, 30), LHoverProgress)
              else
                LCurrentElementBorderColor := BlendColors(IOSUncheckedBorder, ColorOrDefault(FElementBoxColor, IOSGreen), LHoverProgress); // Border animates to accent
            end;
          end;
        end;
      cbsWin11:
        begin
          LCheckElementRadius := Max(FElementCornerRadius, 4); // Typical Win11 rounding
          var Win11Accent: TColor = FElementBoxColor; // User defined accent
          var Win11CheckMark: TColor = ColorOrDefault(FCheckMarkColor, clWhite);
          var Win11UncheckedBorder: TColor = ColorOrDefault(FOverallBorderColor, StringToColor('#ACACAC'));
          var Win11UncheckedFill: TColor = StringToColor('#F9F9F9'); // Very light, almost white

          if not Enabled then
          begin
            LCurrentElementBoxColor := BlendColors(Win11Accent, clSilver, 0.85);
            LCurrentElementBorderColor := BlendColors(Win11UncheckedBorder, clSilver, 0.7);
            LCurrentCheckMarkColor := clGray;
            LCurrentCaptionColor := FCaptionSettings.DisabledColor;
          end
          else
          begin
            LCurrentCaptionColor := FCaptionSettings.Color;
            if FChecked then
            begin
              LCurrentElementBoxColor := Win11Accent;
              LCurrentElementBorderColor := Win11Accent; // No distinct border
              LCurrentCheckMarkColor := Win11CheckMark;
              LFillElementBox := True; LDrawElementBorder := True;
            end
            else // Unchecked
            begin
              LCurrentElementBoxColor := IfThen(FIsHovering, BlendColors(Win11UncheckedFill, ColorToARGB(clBlack,15), LHoverProgress), Win11UncheckedFill);
              LCurrentElementBorderColor := IfThen(FIsHovering, BlendColors(Win11UncheckedBorder, ColorToARGB(clBlack,90), LHoverProgress), Win11UncheckedBorder);
              LFillElementBox := True; LDrawElementBorder := True;
            end;

            if LHoverProgress > 0 and FChecked then // Hover on checked
            begin
               LCurrentElementBoxColor := BlendColors(Win11Accent, ColorToARGB(clBlack, 30), LHoverProgress);
            end;
          end;
        end;

    else // Default to cbsLight behavior if style not handled above
        LCheckElementRadius := FElementCornerRadius;
    end;

    // ElementStyle can further modify appearance after Style defaults are set.
    // This is more relevant for styles like cbsLight, cbsDark, cbsModern.
    // Styles like Material, IOS, Win11 often have strong opinions on fill/border.
    if not (FStyle in [cbsMaterial, cbsIOS, cbsWin11, cbsFlat]) then
    begin
      case FElementStyle of
        cbeseChecked: // Default behavior, rely on FStyle's settings for checked/unchecked
          begin
            // Defaults from FStyle are usually fine.
            // For cbsFlat, LFillElementBox is already set if checked or hovered.
          end;
        cbeseBordered:
          begin
            LDrawElementBorder := True;
            LFillElementBox := FChecked;
            if not FChecked then
            begin
              if FStyle = cbsDark then
                LCurrentElementBoxColor := ColorToARGB(LCurrentColor, 50) // Darker, more transparent fill
              else
                LCurrentElementBoxColor := ColorToARGB(LCurrentColor, 30); // Lighter, more transparent fill
            end;
          end;
        cbeseSolid:
          begin
            LDrawElementBorder := True;
            LFillElementBox := True;
            if not FChecked then // Unchecked but solid style
            begin
               if FStyle = cbsDark then
                 LCurrentElementBoxColor := BlendColors(LCurrentElementBoxColor, LCurrentColor, 0.5)
               else
                 LCurrentElementBoxColor := BlendColors(LCurrentElementBoxColor, LCurrentColor, 0.7);
            end;
          end;
      end;
    end;

    CreateGPRoundedPath(LPath, CheckBoxRectF, LCheckElementRadius, rctAll);

    if LFillElementBox and (ColorToRGB(LCurrentElementBoxColor) <> clNone) and (Alpha(LCurrentElementBoxColor) > 0) then
    begin
      LBrush := TGPSolidBrush.Create(ColorToARGB(LCurrentElementBoxColor));
      try GP.FillPath(LBrush, LPath);
      finally LBrush.Free; end;
    end;

    if LDrawElementBorder then
    begin
      LPen := TGPPen.Create(ColorToARGB(LCurrentElementBorderColor), 1);
      try GP.DrawPath(LPen, LPath);
      finally LPen.Free; end;
    end;
    LPath.Free;

    if FChecked then
    begin
      LPen := TGPPen.Create(ColorToARGB(LCurrentCheckMarkColor), Max(1, CheckElementSize div 7));
      LPen.SetStartCap(LineCapRound); LPen.SetEndCap(LineCapRound); LPen.SetLineJoin(LineJoinRound);
      try
        GP.DrawLine(LPen, CheckBoxRectF.X + CheckElementSize * 0.25, CheckBoxRectF.Y + CheckElementSize * 0.5,
                          CheckBoxRectF.X + CheckElementSize * 0.45, CheckBoxRectF.Y + CheckElementSize * 0.75);
        GP.DrawLine(LPen, CheckBoxRectF.X + CheckElementSize * 0.45, CheckBoxRectF.Y + CheckElementSize * 0.75,
                          CheckBoxRectF.X + CheckElementSize * 0.75, CheckBoxRectF.Y + CheckElementSize * 0.30);
      finally LPen.Free; end;
    end;

  finally
    GP.Free;
  end;


  // --- Caption Drawing ---
  var ActualCaptionRect: TRect; // The final rect used for drawing caption
  if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') then
  begin
    CaptionRect := DrawableRect; // Start with full drawable area, then constrain

    case FCaptionSettings.Position of
      cpLeft:
        begin
          CaptionRect.Right := Round(CheckBoxRectF.X - FCaptionSettings.Offset.X);
          CaptionRect.Left := CaptionRect.Left + FCaptionSettings.Offset.X; // Add left offset from component edge
        end;
      cpRight:
        begin
          CaptionRect.Left := Round(CheckBoxRectF.X + CheckBoxRectF.Width + FCaptionSettings.Offset.X);
          // CaptionRect.Right is initially DrawableRect.Right, potentially reduced by image
        end;
      cpAbove:
        begin
          CaptionRect.Bottom := Round(CheckBoxRectF.Y - FCaptionSettings.Offset.Y);
          CaptionRect.Top := CaptionRect.Top + FCaptionSettings.Offset.Y;
          // Horizontal alignment with checkbox or component width
          CaptionRect.Left := Round(CheckBoxRectF.X);
          CaptionRect.Right := Round(CheckBoxRectF.X + CheckBoxRectF.Width);
          if FCaptionSettings.Alignment = taCenter then AlignRectToCenter(CaptionRect, DrawableRect)
          else if FCaptionSettings.Alignment = taRightJustify then CaptionRect.Right := DrawableRect.Right - FCaptionSettings.Offset.X
          else CaptionRect.Left := DrawableRect.Left + FCaptionSettings.Offset.X;


        end;
      cpBelow:
        begin
          CaptionRect.Top := Round(CheckBoxRectF.Y + CheckBoxRectF.Height + FCaptionSettings.Offset.Y);
          // Vertical alignment with checkbox or component width
          CaptionRect.Left := Round(CheckBoxRectF.X);
          CaptionRect.Right := Round(CheckBoxRectF.X + CheckBoxRectF.Width);
          if FCaptionSettings.Alignment = taCenter then AlignRectToCenter(CaptionRect, DrawableRect)
          else if FCaptionSettings.Alignment = taRightJustify then CaptionRect.Right := DrawableRect.Right - FCaptionSettings.Offset.X
          else CaptionRect.Left := DrawableRect.Left + FCaptionSettings.Offset.X;
        end;
    end;

    // For cpLeft and cpRight, adjust vertical position based on VerticalAlignment
    if FCaptionSettings.Position in [cpLeft, cpRight] then
    begin
        Canvas.Font.Assign(FCaptionSettings.Font); // Ensure font is set for TextHeight
        var textHeight := Canvas.TextHeight(FCaptionSettings.Text);
        case FCaptionSettings.VerticalAlignment of
          cvaTop: CaptionRect.Top := Round(CheckBoxRectF.Y);
          cvaCenter: CaptionRect.Top := Round(CheckBoxRectF.Y + (CheckBoxRectF.Height - textHeight) / 2);
          cvaBottom: CaptionRect.Top := Round(CheckBoxRectF.Y + CheckBoxRectF.Height - textHeight);
        end;
        CaptionRect.Bottom := CaptionRect.Top + textHeight;
    end;
    ActualCaptionRect := CaptionRect; // Store this before image might modify it
  end;


  // --- Image Drawing ---
  var ImageDrawRect: TRect;
  if FImageSettings.Visible and Assigned(FImageSettings.Picture) and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
  begin
    var LImgW, LImgH, DrawWidth, DrawHeight: Integer;
    var AvailableImageWidth, AvailableImageHeight: Integer;
    var LScaleFactor: Single;

    LImgW := FImageSettings.Picture.Graphic.Width;
    LImgH := FImageSettings.Picture.Graphic.Height;

    var ImgAnchorX: Integer;
    // Determine initial anchor point for image (typically right of checkbox or caption)
    if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') and (FCaptionSettings.Position = cpRight) then
      ImgAnchorX := ActualCaptionRect.Right + FImageSettings.Margins.Left
    else
      ImgAnchorX := Round(CheckBoxRectF.X + CheckBoxRectF.Width + FImageSettings.Margins.Left);

    AvailableImageWidth := Max(0, DrawableRect.Right - ImgAnchorX - FImageSettings.Margins.Right);
    AvailableImageHeight := Max(0, DrawableRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);

    if FImageSettings.AutoSize then
    begin
      case FImageSettings.DrawMode of
        idmStretch:
        begin
          DrawWidth := AvailableImageWidth;
          DrawHeight := AvailableImageHeight;
        end;
        idmProportional:
        begin
          if (LImgW = 0) or (LImgH = 0) then
          begin DrawWidth := 0; DrawHeight := 0; end
          else
          begin
            LScaleFactor := Min(AvailableImageWidth / LImgW, AvailableImageHeight / LImgH);
            DrawWidth := Round(LImgW * LScaleFactor);
            DrawHeight := Round(LImgH * LScaleFactor);
          end;
        end;
        idmNormal:
        begin
          DrawWidth := LImgW;
          DrawHeight := LImgH;
          // Clamp to available space if normal size exceeds it
          if DrawWidth > AvailableImageWidth then DrawWidth := AvailableImageWidth;
          if DrawHeight > AvailableImageHeight then DrawHeight := AvailableImageHeight;
        end;
      else // Default to proportional
        if (LImgW = 0) or (LImgH = 0) then
        begin DrawWidth := 0; DrawHeight := 0; end
        else
        begin
          LScaleFactor := Min(AvailableImageWidth / LImgW, AvailableImageHeight / LImgH);
          DrawWidth := Round(LImgW * LScaleFactor);
          DrawHeight := Round(LImgH * LScaleFactor);
        end;
      end;
    end
    else // AutoSize = False
    begin
      DrawWidth := FImageSettings.TargetWidth;
      DrawHeight := FImageSettings.TargetHeight;

      if (DrawWidth = 0) and (DrawHeight = 0) then
      begin
        if FImageSettings.DrawMode = idmStretch then
        begin
          DrawWidth := AvailableImageWidth;
          DrawHeight := AvailableImageHeight;
        end
        else // idmProportional or idmNormal
        begin
          DrawWidth := LImgW;
          DrawHeight := LImgH;
        end;
      end
      else if (DrawWidth = 0) and (DrawHeight > 0) then // Width is 0, Height is specified
      begin
        if LImgH > 0 then DrawWidth := Round(LImgW / LImgH * DrawHeight)
        else DrawWidth := LImgW; // Fallback
      end
      else if (DrawWidth > 0) and (DrawHeight = 0) then // Height is 0, Width is specified
      begin
        if LImgW > 0 then DrawHeight := Round(LImgH / LImgW * DrawWidth)
        else DrawHeight := LImgH; // Fallback
      end;

      // For AutoSize = False, DrawMode idmProportional means fitting within TargetWidth/Height
      if FImageSettings.DrawMode = idmProportional then
      begin
        if (DrawWidth > 0) and (DrawHeight > 0) and (LImgW > 0) and (LImgH > 0) then
        begin
            LScaleFactor := Min(DrawWidth / LImgW, DrawHeight / LImgH);
            DrawWidth := Round(LImgW * LScaleFactor);
            DrawHeight := Round(LImgH * LScaleFactor);
        end;
      end;
      // idmNormal with TargetSize means image is drawn at TargetSize (or original if TargetSize is 0,0)
      // idmStretch with TargetSize means image is stretched to TargetSize

      // Clamping to available space for AutoSize = False
      if (DrawWidth > AvailableImageWidth) or (DrawHeight > AvailableImageHeight) then
      begin
        if (DrawWidth > 0) and (DrawHeight > 0) then
        begin
          LScaleFactor := Min(AvailableImageWidth / DrawWidth, AvailableImageHeight / DrawHeight);
          DrawWidth := Round(DrawWidth * LScaleFactor);
          DrawHeight := Round(DrawHeight * LScaleFactor);
        end
        else // If one dimension is zero or negative, just hard clamp
        begin
           DrawWidth := Min(DrawWidth, AvailableImageWidth);
           DrawHeight := Min(DrawHeight, AvailableImageHeight);
        end;
      end;
    end;

    DrawWidth := Max(0, DrawWidth);
    DrawHeight := Max(0, DrawHeight);

    ImageDrawRect.Left := ImgAnchorX;
    case FImageSettings.AlignmentVertical of
      iavTop: ImageDrawRect.Top := DrawableRect.Top + FImageSettings.Margins.Top;
      iavCenter: ImageDrawRect.Top := DrawableRect.Top + FImageSettings.Margins.Top + (AvailableImageHeight - DrawHeight) div 2; // Use AvailableImageHeight for centering
      iavBottom: ImageDrawRect.Top := DrawableRect.Bottom - FImageSettings.Margins.Bottom - DrawHeight;
    else // Default to Center
      ImageDrawRect.Top := DrawableRect.Top + FImageSettings.Margins.Top + (AvailableImageHeight - DrawHeight) div 2; // Use AvailableImageHeight
    end;
    ImageDrawRect.Right := ImageDrawRect.Left + DrawWidth;
    ImageDrawRect.Bottom := ImageDrawRect.Top + DrawHeight;

    // If image is now to the right, it might reduce space for a cpRight caption
    if FCaptionSettings.Visible and (FCaptionSettings.Position = cpRight) and (Trim(FCaptionSettings.Text) <> '') and (ImageDrawRect.Left < ActualCaptionRect.Right) then
    begin
       ActualCaptionRect.Right := Max(ActualCaptionRect.Left, ImageDrawRect.Left - FCaptionSettings.Offset.X);
    end;

    // Clip image rect to drawable area
    ImageDrawRect := IntersectRect(ImageDrawRect, DrawableRect);

    if (ImageDrawRect.Right > ImageDrawRect.Left) and (ImageDrawRect.Bottom > ImageDrawRect.Top) then
    begin
      if IsPNG(FImageSettings.Picture.Graphic) then
        DrawPNGImageWithGDI(GP, FImageSettings.Picture.Graphic as TPNGObject, ImageDrawRect, FImageSettings.DrawMode, FImageSettings.Opacity)
      else
        DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, ImageDrawRect, FImageSettings.DrawMode);
    end;
  end;

  // Draw caption text last, after its rect might have been adjusted by image.
  if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') then
  begin
    if (ActualCaptionRect.Right > ActualCaptionRect.Left) and (ActualCaptionRect.Bottom > ActualCaptionRect.Top) then
    begin
      DrawComponentCaption(
        Self.Canvas, ActualCaptionRect, FCaptionSettings.Text, FCaptionSettings.Font, LCurrentCaptionColor,
        FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, 255 // Opacity handled by LCurrentCaptionColor if needed
      );
    end;
  end;

  if Focused and TabStop and Enabled then
    DrawFocusRect(Self.Canvas.Handle, EffectiveClientRect);
end;

procedure TANDMR_CCheckBox.SetElementCornerRadius(const Value: Integer);
begin
  if FElementCornerRadius <> Value then
  begin
    FElementCornerRadius := Max(0, Value);
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Repaint;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TANDMR_CCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TANDMR_CCheckBox.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Assigned(FCaptionSettings) then FCaptionSettings.Text := Value; else Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetStyle(const Value: TCCheckBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetElementStyle(const Value: TCCheckBoxElementStyle);
begin
  if FElementStyle <> Value then
  begin
    FElementStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FBorderSettings) then FBorderSettings.BackgroundColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetOverallBorderColor(const Value: TColor);
begin
  if FOverallBorderColor <> Value then
  begin
    FOverallBorderColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetCheckMarkColor(const Value: TColor);
begin
  if FCheckMarkColor <> Value then
  begin
    FCheckMarkColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetElementBoxColor(const Value: TColor);
begin
  if FElementBoxColor <> Value then
  begin
    FElementBoxColor := Value;
    Repaint;
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
    Invalidate;
  end;
end;

function TANDMR_CCheckBox.GetFont: TFont;
begin
  if Assigned(FCaptionSettings) and Assigned(FCaptionSettings.Font) then Result := FCaptionSettings.Font
  else Result := inherited Font;
end;

procedure TANDMR_CCheckBox.SetFont(const Value: TFont);
var PrevOnChange: TNotifyEvent;
begin
  if Assigned(FCaptionSettings) and Assigned(FCaptionSettings.Font) then
  begin
    PrevOnChange := FCaptionSettings.Font.OnChange;
    FCaptionSettings.Font.OnChange := nil;
    try FCaptionSettings.Font.Assign(Value);
    finally FCaptionSettings.Font.OnChange := PrevOnChange; end;
    if FCaptionSettings.Font.OnChange = nil then InternalFontChanged(FCaptionSettings.Font);
  end
  else
  begin
    inherited Font.Assign(Value); Repaint;
  end;
end;

procedure TANDMR_CCheckBox.SetImageSettings(const Value: TImageSettings); begin FImageSettings.Assign(Value); end;
procedure TANDMR_CCheckBox.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); end;
procedure TANDMR_CCheckBox.SetBorderSettings(const Value: TBorderSettings); begin FBorderSettings.Assign(Value); end;
procedure TANDMR_CCheckBox.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); end;

procedure TANDMR_CCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    if not Focused then SetFocus; Click;
  end;
end;

procedure TANDMR_CCheckBox.Click;
begin
  if Enabled then
  begin
    SetChecked(not FChecked);
    if Assigned(FOnClick) then FOnClick(Self);
  end;
end;

procedure TANDMR_CCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Enabled and (Key = VK_SPACE) and (Shift = []) then
  begin
    Click;
    Key := 0;
  end;
end;

procedure TANDMR_CCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FIsHovering := True;
    if Enabled and FHoverSettings.Enabled then FHoverSettings.StartAnimation(True)
    else Repaint;
  end;
end;

procedure TANDMR_CCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
   if not (csDesigning in ComponentState) then
   begin
    FIsHovering := False;
    if Enabled and FHoverSettings.Enabled then FHoverSettings.StartAnimation(False)
    else Repaint;
   end;
end;

procedure TANDMR_CCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FIsHovering := False;
    if Assigned(FHoverSettings) and FHoverSettings.Enabled then FHoverSettings.StartAnimation(False);
  end;
  Repaint;
end;

procedure TANDMR_CCheckBox.InternalCaptionSettingsChanged(Sender: TObject);
begin
  if Assigned(FCaptionSettings) and (FCaption <> FCaptionSettings.Text) then FCaption := FCaptionSettings.Text;
  Repaint;
end;

procedure TANDMR_CCheckBox.InternalImageSettingsChanged(Sender: TObject); Repaint; end;

procedure TANDMR_CCheckBox.InternalBorderSettingsChanged(Sender: TObject);
begin
  if Assigned(FBorderSettings) then
  begin
    if FColor <> FBorderSettings.BackgroundColor then FColor := FBorderSettings.BackgroundColor;
    // FOverallBorderColor is now primarily for the checkbox element, not synced from FBorderSettings.Color
  end;
  Repaint;
end;

procedure TANDMR_CCheckBox.InternalHoverSettingsChanged(Sender: TObject); Repaint; end;
procedure TANDMR_CCheckBox.InternalFontChanged(Sender: TObject); Repaint; end;

end.
