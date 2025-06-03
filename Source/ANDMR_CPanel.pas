unit ANDMR_CPanel;

interface

uses
  Vcl.Controls, // Moved to top
  // Core System units
  System.SysUtils, System.Classes, System.Types, System.Math,
  // Windows API
  Winapi.Windows, Winapi.Messages,
  // GDI+
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  // VCL core (remaining)
  Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls,
  // Custom utilities
  ANDMR_ComponentUtils;

type
  TANDMR_CPanel = class(TCustomControl)
  private
    // New Settings Objects
    FBorderSettings: TBorderSettings;
    FDropShadowSettings: TDropShadowSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;

    // Private fields for properties
    FOpacity: Byte;
    FTransparentChildren: Boolean;
    FWindowRegion: HRGN;
    FIsHovering: Boolean;

    // Property Getters/Setters
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetBorderColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    function GetBorderThickness: Integer;
    procedure SetBorderThickness(const Value: Integer);
    function GetBorderStyle: TPenStyle;
    procedure SetBorderStyle(const Value: TPenStyle);
    function GetCornerRadius: Integer;
    procedure SetCornerRadius(const Value: Integer);
    function GetRoundCornerType: TRoundCornerType;
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    // GetFont and SetFont are now in protected
    function GetDropShadowEnabled: Boolean;
    procedure SetDropShadowEnabled(const Value: Boolean);
    function GetDropShadowColor: TColor;
    procedure SetDropShadowColor(const Value: TColor);
    function GetDropShadowOffset: TPoint;
    procedure SetDropShadowOffset(const Value: TPoint);
    function GetDropShadowBlurRadius: Integer;
    procedure SetDropShadowBlurRadius(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetCaptionAlignment: System.Classes.TAlignment;
    procedure SetCaptionAlignment(const Value: System.Classes.TAlignment);
    function GetCaptionVerticalAlignment: TCaptionVerticalAlignment;
    procedure SetCaptionVerticalAlignment(const Value: TCaptionVerticalAlignment);
    function GetCaptionWordWrap: Boolean;
    procedure SetCaptionWordWrap(const Value: Boolean);
    function GetCaptionOffsetX: Integer;
    procedure SetCaptionOffsetX(const Value: Integer);
    function GetCaptionOffsetY: Integer;
    procedure SetCaptionOffsetY(const Value: Integer);
    function GetDisabledFontColor: TColor;
    procedure SetDisabledFontColor(const Value: TColor);
    procedure SetTransparentChildren(const Value: Boolean);
    procedure SetHoverSettings(const Value: THoverSettings);

    // Internal methods
    procedure HoverSettingsChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure UpdateRegion;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  protected
    // Method overrides
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    function GetFont: TFont; // Getter for published Font property
    procedure SetFont(Value: TFont);   // Removed 'override'

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Color: TColor read GetColor write SetColor default clBtnFace;
    property BorderColor: TColor read GetBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read GetBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read GetBorderStyle write SetBorderStyle default psSolid;
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius default 0;
    property RoundCornerType: TRoundCornerType read GetRoundCornerType write SetRoundCornerType default rctNone;
    property Caption: string read GetCaption write SetCaption;
    property CaptionAlignment: System.Classes.TAlignment read GetCaptionAlignment write SetCaptionAlignment default taCenter;
    property CaptionVerticalAlignment: TCaptionVerticalAlignment read GetCaptionVerticalAlignment write SetCaptionVerticalAlignment default cvaCenter;
    property CaptionWordWrap: Boolean read GetCaptionWordWrap write SetCaptionWordWrap default False;
    property CaptionOffsetX: Integer read GetCaptionOffsetX write SetCaptionOffsetX default 0;
    property CaptionOffsetY: Integer read GetCaptionOffsetY write SetCaptionOffsetY default 0;
    property DisabledFontColor: TColor read GetDisabledFontColor write SetDisabledFontColor default clGrayText;
    property Font: TFont read GetFont write SetFont;
    property TransparentChildren: Boolean read FTransparentChildren write SetTransparentChildren default False;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;

    property DropShadowEnabled: Boolean read GetDropShadowEnabled write SetDropShadowEnabled default False;
    property DropShadowColor: TColor read GetDropShadowColor write SetDropShadowColor default clBlack;
    property DropShadowOffset: TPoint read GetDropShadowOffset write SetDropShadowOffset;
    property DropShadowBlurRadius: Integer read GetDropShadowBlurRadius write SetDropShadowBlurRadius default 3;
    property Opacity: Byte read FOpacity write SetOpacity default 255;

    property Align;
    property Anchors;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CPanel]);
end;

constructor TANDMR_CPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWindowRegion := 0;
  FTransparentChildren := False;
  FIsHovering := False;

  // Instantiate new settings objects and set their defaults
  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.BackgroundColor := clBtnFace;
  FBorderSettings.Color := clBlack;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;
  FBorderSettings.CornerRadius := 0;
  FBorderSettings.RoundCornerType := rctNone;

  FDropShadowSettings := TDropShadowSettings.Create;
  FDropShadowSettings.OnChange := SettingsChanged;
  FDropShadowSettings.Enabled := False;
  FDropShadowSettings.Color := clBlack;
  FDropShadowSettings.Offset := Point(2, 2);
  FDropShadowSettings.BlurRadius := 3;

  FCaptionSettings := TCaptionSettings.Create(Self); // Owner is Self
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := '';
  FCaptionSettings.Font.Name := 'Segoe UI';
  FCaptionSettings.Font.Size := 9;
  FCaptionSettings.Font.Color := clWindowText;
  FCaptionSettings.Alignment := taCenter;
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.WordWrap := False;
  FCaptionSettings.Offset := Point(0,0); // Initialize TPoint offset
  FCaptionSettings.DisabledColor := clGrayText;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  // Defaults for FHoverSettings are set within THoverSettings.Create

  Width := 185;
  Height := 85;
  ControlStyle := ControlStyle + [csAcceptsControls, csReplicatable, csDoubleClicks];
  DoubleBuffered := True;

  FOpacity := 255;
  SetOpacity(FOpacity); // Apply initial opacity and set ControlStyle flags
end;

destructor TANDMR_CPanel.Destroy;
begin
  if FWindowRegion <> 0 then
  begin
    DeleteObject(FWindowRegion);
    FWindowRegion := 0;
  end;

  FHoverSettings.OnChange := nil;
  FHoverSettings.Free;

  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;
  FDropShadowSettings.OnChange := nil;
  FDropShadowSettings.Free;
  FCaptionSettings.OnChange := nil;
  FCaptionSettings.Free;

  inherited Destroy;
end;

procedure TANDMR_CPanel.SettingsChanged(Sender: TObject);
begin
  UpdateRegion;
  Invalidate;
end;

function TANDMR_CPanel.GetColor: TColor; begin Result := FBorderSettings.BackgroundColor; end;
procedure TANDMR_CPanel.SetColor(const Value: TColor); begin FBorderSettings.BackgroundColor := Value; end;

function TANDMR_CPanel.GetBorderColor: TColor; begin Result := FBorderSettings.Color; end;
procedure TANDMR_CPanel.SetBorderColor(const Value: TColor); begin FBorderSettings.Color := Value; end;

function TANDMR_CPanel.GetBorderThickness: Integer; begin Result := FBorderSettings.Thickness; end;
procedure TANDMR_CPanel.SetBorderThickness(const Value: Integer); begin FBorderSettings.Thickness := Max(0,Value); end;

function TANDMR_CPanel.GetBorderStyle: TPenStyle; begin Result := FBorderSettings.Style; end;
procedure TANDMR_CPanel.SetBorderStyle(const Value: TPenStyle); begin FBorderSettings.Style := Value; end;

function TANDMR_CPanel.GetCornerRadius: Integer; begin Result := FBorderSettings.CornerRadius; end;
procedure TANDMR_CPanel.SetCornerRadius(const Value: Integer);
begin
  if FBorderSettings.CornerRadius <> Max(0,Value) then
  begin
    FBorderSettings.CornerRadius := Max(0,Value);
  end;
end;

function TANDMR_CPanel.GetRoundCornerType: TRoundCornerType; begin Result := FBorderSettings.RoundCornerType; end;
procedure TANDMR_CPanel.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FBorderSettings.RoundCornerType <> Value then
  begin
    FBorderSettings.RoundCornerType := Value;
  end;
end;

function TANDMR_CPanel.GetCaption: string; begin Result := FCaptionSettings.Text; end;
procedure TANDMR_CPanel.SetCaption(const Value: string); begin FCaptionSettings.Text := Value; end;

function TANDMR_CPanel.GetFont: TFont;
begin
  Result := FCaptionSettings.Font;
end;

procedure TANDMR_CPanel.SetFont(Value: TFont);
begin
  // The published Font property should directly control the caption's font.
  // FCaptionSettings.Font.Assign will trigger FCaptionSettings.Font.OnChange,
  // which in turn calls FCaptionSettings.Changed, which calls Self.SettingsChanged.
  FCaptionSettings.Font.Assign(Value);
  // The Invalidate call here is therefore redundant if FCaptionSettings.OnChange works.
  // However, keeping it for safety or if direct sub-property changes of Font
  // (e.g., Font.Name := 'Arial') are made elsewhere and don't trigger through Assign.
  // For now, let's assume FCaptionSettings.OnChange is sufficient.
  // If Self.Invalidate is still needed, it can be added back, but
  // TCaptionSettings.FontChanged calls Changed, which calls FOnChange, which is Self.SettingsChanged.
  // Self.SettingsChanged calls Invalidate. So it should be fine.
end;

function TANDMR_CPanel.GetDropShadowEnabled: Boolean; begin Result := FDropShadowSettings.Enabled; end;
procedure TANDMR_CPanel.SetDropShadowEnabled(const Value: Boolean); begin FDropShadowSettings.Enabled := Value; end;

function TANDMR_CPanel.GetDropShadowColor: TColor; begin Result := FDropShadowSettings.Color; end;
procedure TANDMR_CPanel.SetDropShadowColor(const Value: TColor); begin FDropShadowSettings.Color := Value; end;

function TANDMR_CPanel.GetDropShadowOffset: TPoint; begin Result := FDropShadowSettings.Offset; end;
procedure TANDMR_CPanel.SetDropShadowOffset(const Value: TPoint); begin FDropShadowSettings.Offset := Value; end;

function TANDMR_CPanel.GetDropShadowBlurRadius: Integer; begin Result := FDropShadowSettings.BlurRadius; end;
procedure TANDMR_CPanel.SetDropShadowBlurRadius(const Value: Integer); begin FDropShadowSettings.BlurRadius := Max(0,Value); end;

procedure TANDMR_CPanel.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 255 then
    begin
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
      if Parent <> nil then Parent.Invalidate;
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    end;
    UpdateRegion;
    Invalidate;
  end;
end;

function TANDMR_CPanel.GetCaptionAlignment: System.Classes.TAlignment; begin Result := FCaptionSettings.Alignment; end;
procedure TANDMR_CPanel.SetCaptionAlignment(const Value: System.Classes.TAlignment); begin FCaptionSettings.Alignment := Value; end;

function TANDMR_CPanel.GetCaptionVerticalAlignment: TCaptionVerticalAlignment; begin Result := FCaptionSettings.VerticalAlignment; end;
procedure TANDMR_CPanel.SetCaptionVerticalAlignment(const Value: TCaptionVerticalAlignment); begin FCaptionSettings.VerticalAlignment := Value; end;

function TANDMR_CPanel.GetCaptionWordWrap: Boolean; begin Result := FCaptionSettings.WordWrap; end;
procedure TANDMR_CPanel.SetCaptionWordWrap(const Value: Boolean); begin FCaptionSettings.WordWrap := Value; end;

function TANDMR_CPanel.GetCaptionOffsetX: Integer; begin Result := FCaptionSettings.Offset.X; end;
procedure TANDMR_CPanel.SetCaptionOffsetX(const Value: Integer); begin FCaptionSettings.Offset := Point(Value, FCaptionSettings.Offset.Y); end;

function TANDMR_CPanel.GetCaptionOffsetY: Integer; begin Result := FCaptionSettings.Offset.Y; end;
procedure TANDMR_CPanel.SetCaptionOffsetY(const Value: Integer); begin FCaptionSettings.Offset := Point(FCaptionSettings.Offset.X, Value); end;

function TANDMR_CPanel.GetDisabledFontColor: TColor; begin Result := FCaptionSettings.DisabledColor; end;
procedure TANDMR_CPanel.SetDisabledFontColor(const Value: TColor); begin FCaptionSettings.DisabledColor := Value; end;

procedure TANDMR_CPanel.SetTransparentChildren(const Value: Boolean);
begin
  if FTransparentChildren <> Value then
  begin
    FTransparentChildren := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CPanel.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
end;

procedure TANDMR_CPanel.HoverSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CPanel.Paint;
var
  LG: TGPGraphics;
  LClientRect: TRect;
  LShadowRect: TGPRectF;
  LShadowPath: TGPGraphicsPath;
  LTextRect: TRect;
  LShadowBrush: TGPSolidBrush;
  LShadowColorAlpha: ARGB;
  CurrentColor, CurrentBorderColor, CurrentCaptionColor: TColor;
begin
  inherited Paint;

  LClientRect := Self.ClientRect;
  if (LClientRect.Width <= 0) or (LClientRect.Height <= 0) then Exit;

  LG := TGPGraphics.Create(Self.Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    var LHoverProgress: Single := FHoverSettings.CurrentAnimationValue / 255.0;

    var TrueBasePanelBG, TargetHoverPanelBG, DisabledPanelBG: TColor;
    var NonHoveredPanelBG, TargetStatePanelBG: TColor;

    TrueBasePanelBG := FBorderSettings.BackgroundColor;
    TargetHoverPanelBG := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
    DisabledPanelBG := BlendColors(FBorderSettings.BackgroundColor, clGray, 0.65);

    NonHoveredPanelBG := ResolveStateColor(Self.Enabled, False, False, TrueBasePanelBG, clNone, clNone, DisabledPanelBG, False, False);
    TargetStatePanelBG := ResolveStateColor(Self.Enabled, FIsHovering, False, TrueBasePanelBG, TargetHoverPanelBG, clNone, DisabledPanelBG, FHoverSettings.Enabled, False);

    if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FIsHovering then
      CurrentColor := BlendColors(NonHoveredPanelBG, TargetStatePanelBG, LHoverProgress)
    else
      CurrentColor := TargetStatePanelBG;

    var TrueBasePanelBorder, TargetHoverPanelBorder, DisabledPanelBorder: TColor;
    var NonHoveredPanelBorder, TargetStatePanelBorder: TColor;

    TrueBasePanelBorder := FBorderSettings.Color;
    TargetHoverPanelBorder := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
    DisabledPanelBorder := BlendColors(FBorderSettings.Color, clGray, 0.7);

    NonHoveredPanelBorder := ResolveStateColor(Self.Enabled, False, False, TrueBasePanelBorder, clNone, clNone, DisabledPanelBorder, False, False);
    TargetStatePanelBorder := ResolveStateColor(Self.Enabled, FIsHovering, False, TrueBasePanelBorder, TargetHoverPanelBorder, clNone, DisabledPanelBorder, FHoverSettings.Enabled, False);

    if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FIsHovering then
      CurrentBorderColor := BlendColors(NonHoveredPanelBorder, TargetStatePanelBorder, LHoverProgress)
    else
      CurrentBorderColor := TargetStatePanelBorder;

    if FDropShadowSettings.Enabled and (FDropShadowSettings.BlurRadius > 0) then
    begin
      var ShadowPathInset: Single := FBorderSettings.Thickness / 2.0;
      LShadowRect.X := LClientRect.Left + ShadowPathInset + FDropShadowSettings.Offset.X;
      LShadowRect.Y := LClientRect.Top + ShadowPathInset + FDropShadowSettings.Offset.Y;
      LShadowRect.Width := LClientRect.Width - (2 * ShadowPathInset);
      LShadowRect.Height := LClientRect.Height - (2 * ShadowPathInset);
      if LShadowRect.Width <=0 then LShadowRect.Width := 1;
      if LShadowRect.Height <=0 then LShadowRect.Height := 1;

      LShadowPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LShadowPath, LShadowRect, Single(FBorderSettings.CornerRadius), FBorderSettings.RoundCornerType);
        LShadowColorAlpha := ColorToARGB(FDropShadowSettings.Color, Max(0, Min(255, FOpacity div 3)));
        LShadowBrush := TGPSolidBrush.Create(LShadowColorAlpha);
        try
          LG.FillPath(LShadowBrush, LShadowPath);
        finally
          LShadowBrush.Free;
        end;
      finally
        LShadowPath.Free;
      end;
    end;

    DrawEditBox(LG, LClientRect, CurrentColor, CurrentBorderColor, FBorderSettings.Thickness, FBorderSettings.Style, FBorderSettings.CornerRadius, FBorderSettings.RoundCornerType, FOpacity);

    if (FCaptionSettings.Text <> '') and (FCaptionSettings.Font <> nil) then
    begin
      LTextRect := Self.ClientRect;
      if FBorderSettings.Thickness > 0 then
          InflateRect(LTextRect, -FBorderSettings.Thickness -2, -FBorderSettings.Thickness -2)
      else
          InflateRect(LTextRect, -2, -2);
      OffsetRect(LTextRect, FCaptionSettings.Offset.X, FCaptionSettings.Offset.Y);

      var TrueBasePanelCaptionColor, TargetHoverPanelCaptionColor, DisabledPanelCaptionColor: TColor;
      var NonHoveredPanelCaptionColor, TargetStatePanelCaptionColor: TColor;

      TrueBasePanelCaptionColor := FCaptionSettings.Color;
      TargetHoverPanelCaptionColor := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);
      DisabledPanelCaptionColor := FCaptionSettings.DisabledColor;

      NonHoveredPanelCaptionColor := ResolveStateColor(Self.Enabled, False, False, TrueBasePanelCaptionColor, clNone, clNone, DisabledPanelCaptionColor, False, False);
      TargetStatePanelCaptionColor := ResolveStateColor(Self.Enabled, FIsHovering, False, TrueBasePanelCaptionColor, TargetHoverPanelCaptionColor, clNone, DisabledPanelCaptionColor, FHoverSettings.Enabled, False);

      if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FIsHovering then
        CurrentCaptionColor := BlendColors(NonHoveredPanelCaptionColor, TargetStatePanelCaptionColor, LHoverProgress)
      else
        CurrentCaptionColor := TargetStatePanelCaptionColor;

      if (LTextRect.Width > 0) and (LTextRect.Height > 0) and (Length(Trim(FCaptionSettings.Text)) > 0) then
      begin
        DrawComponentCaption(
          Self.Canvas,
          LTextRect,
          FCaptionSettings.Text,
          FCaptionSettings.Font,
          CurrentCaptionColor,
          FCaptionSettings.Alignment,
          FCaptionSettings.VerticalAlignment,
          FCaptionSettings.WordWrap,
          FOpacity
        );
      end;
    end;
  finally
    LG.Free;
  end;
end;

procedure TANDMR_CPanel.Loaded;
begin
  inherited Loaded;
  if FOpacity < 255 then
  begin
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground];
  end
  else
  begin
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  end;
  UpdateRegion;
  Invalidate;
end;

procedure TANDMR_CPanel.Resize;
begin
  inherited Resize;
  UpdateRegion;
end;

procedure TANDMR_CPanel.CreateWnd;
begin
  inherited CreateWnd;
  UpdateRegion;
end;

procedure TANDMR_CPanel.UpdateRegion;
var
  LPath: TGPGraphicsPath;
  LGDIRgn: TGPRegion;
  LG: TGPGraphics;
  LClientRect: TRect;
  LPathRect: TGPRectF;
  NewRegion: HRGN;
  OldRegion: HRGN;
begin
  if not HandleAllocated then
    Exit;

  OldRegion := FWindowRegion;
  NewRegion := 0;

  LClientRect := Self.ClientRect;

  if (FBorderSettings.CornerRadius > 0) and (LClientRect.Width > 0) and (LClientRect.Height > 0) then
  begin
    LPath := TGPGraphicsPath.Create;
    try
      LPathRect.X := 0;
      LPathRect.Y := 0;
      LPathRect.Width := LClientRect.Width;
      LPathRect.Height := LClientRect.Height;

      if LPathRect.Width < 0 then LPathRect.Width := 0;
      if LPathRect.Height < 0 then LPathRect.Height := 0;

      if (LPathRect.Width > 0) and (LPathRect.Height > 0) then
      begin
        ANDMR_ComponentUtils.CreateGPRoundedPath(LPath, LPathRect, Single(FBorderSettings.CornerRadius), FBorderSettings.RoundCornerType);
        LG := TGPGraphics.Create(Self.Handle);
        try
          LGDIRgn := TGPRegion.Create(LPath);
          try
            NewRegion := LGDIRgn.GetHRGN(LG);
          finally
            LGDIRgn.Free;
          end;
        finally
          LG.Free;
        end;
      end;
    finally
      LPath.Free;
    end;
  end;

  if (HandleAllocated) and ((NewRegion <> OldRegion) or ((NewRegion = 0) And (OldRegion <> 0) )) then
  begin
    SetWindowRgn(Self.Handle, NewRegion, True);
    FWindowRegion := NewRegion;
    if (OldRegion <> 0) and (OldRegion <> NewRegion) then
    begin
      DeleteObject(OldRegion);
    end;
  end
  else if (NewRegion <> 0) And (NewRegion <> OldRegion) then
  begin
    DeleteObject(NewRegion);
    if OldRegion <> 0 then DeleteObject(OldRegion);
    FWindowRegion := 0;
  end
  else if (NewRegion = 0) And (OldRegion <> 0) And not HandleAllocated then
  begin
    DeleteObject(OldRegion);
    FWindowRegion := 0;
  end;
end;

procedure TANDMR_CPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FIsHovering then
  begin
    FIsHovering := True;
  end;
  FHoverSettings.StartAnimation(True);
end;

procedure TANDMR_CPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FIsHovering then
  begin
    FIsHovering := False;
  end;
  FHoverSettings.StartAnimation(False);
end;

end.
