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
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetDropShadowSettings(const Value: TDropShadowSettings);
    procedure SetOpacity(const Value: Byte);
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property TransparentChildren: Boolean read FTransparentChildren write SetTransparentChildren default False;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property DropShadowSettings: TDropShadowSettings read FDropShadowSettings write SetDropShadowSettings;
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
  FBorderSettings.BackgroundColor := clWhite; // Modernized
  FBorderSettings.Color := clSilver; // Modernized
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;
  FBorderSettings.CornerRadius := 6; // Modernized
  FBorderSettings.RoundCornerType := rctAll; // Modernized

  FDropShadowSettings := TDropShadowSettings.Create;
  FDropShadowSettings.OnChange := SettingsChanged;
  FDropShadowSettings.Enabled := False;
  FDropShadowSettings.Color := clGray; // Modernized
  FDropShadowSettings.Offset := Point(3, 3); // Modernized
  FDropShadowSettings.BlurRadius := 8; // Modernized

  FCaptionSettings := TCaptionSettings.Create(Self); // Owner is Self
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := '';
  FCaptionSettings.Font.Name := 'Segoe UI';
  FCaptionSettings.Font.Size := 9;
  FCaptionSettings.Font.Color := clGrayText; // Modernized
  FCaptionSettings.Font.Style := [fsBold]; // Modernized
  FCaptionSettings.Alignment := taLeftJustify; // Modernized
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.Offset := Point(4,0); // Modernized for taLeftJustify
  FCaptionSettings.DisabledColor := clGrayText;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  // Defaults for FHoverSettings are set within THoverSettings.Create (already modernized)

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

procedure TANDMR_CPanel.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure TANDMR_CPanel.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure TANDMR_CPanel.SetDropShadowSettings(const Value: TDropShadowSettings);
begin
  FDropShadowSettings.Assign(Value);
  SettingsChanged(Self);
end;

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
