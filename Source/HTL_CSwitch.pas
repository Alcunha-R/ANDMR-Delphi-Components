unit HTL_CSwitch;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Winapi.Messages, Vcl.Forms, Vcl.StdCtrls, System.UITypes, System.Types,
  System.Math, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  HTL_ComponentUtils, Vcl.ExtCtrls;

type
  TSwitchStyle = (
    ssDefault,
    ssFlat,
    ssIOS,
    ssMaterial,
    ssSquareThumb,
    ssLineSlider,
    ssIOSWithIcon,
    ssSquareWithIcon,
    ssSlidingIcon,
    ssCustom
  );

  TSwitchArchetype = (
    saPill,
    saSquare,
    saLine,
    saSlidingIcon
  );

  TSwitchIconSet = (isNone, isPower, isCheckX);

  TSwitchSettings = class(TPersistent)
  private
    FTrackColorUnchecked: TColor;
    FTrackColorChecked: TColor;
    FTrackBorderColor: TColor;
    FThumbColorUnchecked: TColor;
    FThumbColorChecked: TColor;
    FThumbSizeRatio: Single;
    FTextChecked: string;
    FTextUnchecked: string;
    FFontChecked: TFont;
    FFontUnchecked: TFont;
    FArchetype: TSwitchArchetype;
    FOnChange: TNotifyEvent;
    FDrawThumbShadow: Boolean;
    FDrawThumbInnerBorder: Boolean;
    FDrawTextLabel: Boolean;
    FIconSet: TSwitchIconSet;
    procedure SetTrackColorUnchecked(const Value: TColor);
    procedure SetTrackColorChecked(const Value: TColor);
    procedure SetTrackBorderColor(const Value: TColor);
    procedure SetThumbColorUnchecked(const Value: TColor);
    procedure SetThumbColorChecked(const Value: TColor);
    procedure SetThumbSizeRatio(const Value: Single);
    procedure SetTextChecked(const Value: string);
    procedure SetTextUnchecked(const Value: string);
    procedure SetFontChecked(const Value: TFont);
    procedure SetFontUnchecked(const Value: TFont);
    procedure SetArchetype(const Value: TSwitchArchetype);
    procedure SetDrawThumbShadow(const Value: Boolean);
    procedure SetDrawThumbInnerBorder(const Value: Boolean);
    procedure SetDrawTextLabel(const Value: Boolean);
    procedure SetIconSet(const Value: TSwitchIconSet);
  protected
    procedure Changed; virtual;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Archetype: TSwitchArchetype read FArchetype write SetArchetype default saPill;
    property TrackColorUnchecked: TColor read FTrackColorUnchecked write SetTrackColorUnchecked default $00BBBBBB;
    property TrackColorChecked: TColor read FTrackColorChecked write SetTrackColorChecked default $0050AF4C;
    property TrackBorderColor: TColor read FTrackBorderColor write SetTrackBorderColor default clGray;
    property ThumbColorUnchecked: TColor read FThumbColorUnchecked write SetThumbColorUnchecked default clWhite;
    property ThumbColorChecked: TColor read FThumbColorChecked write SetThumbColorChecked default clWhite;
    property ThumbSizeRatio: Single read FThumbSizeRatio write SetThumbSizeRatio;
    property TextChecked: string read FTextChecked write SetTextChecked;
    property TextUnchecked: string read FTextUnchecked write SetTextUnchecked;
    property FontChecked: TFont read FFontChecked write SetFontChecked;
    property FontUnchecked: TFont read FFontUnchecked write SetFontUnchecked;
    property DrawThumbShadow: Boolean read FDrawThumbShadow write SetDrawThumbShadow default False;
    property DrawThumbInnerBorder: Boolean read FDrawThumbInnerBorder write SetDrawThumbInnerBorder default False;
    property DrawTextLabel: Boolean read FDrawTextLabel write SetDrawTextLabel default False;
    property IconSet: TSwitchIconSet read FIconSet write SetIconSet default isNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THTL_CSwitch = class(TCustomControl)
  private
    FChecked: Boolean;
    FStyle: TSwitchStyle;
    FSwitchSettings: TSwitchSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FAnimationTimer: TTimer;
    FAnimationProgress: Single;
    FAnimateToChecked: Boolean;
    FOnCheckChanged: TNotifyEvent;
    procedure ApplyStyleSettings(AStyle: TSwitchStyle);
    procedure SetChecked(const Value: Boolean);
    procedure SetStyle(const Value: TSwitchStyle);
    procedure SetSwitchSettings(const Value: TSwitchSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure AnimationTimerHandler(Sender: TObject);
    procedure StartToggleAnimation(AChecked: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure BreakStyleLink;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Style: TSwitchStyle read FStyle write SetStyle default ssDefault;
    property Checked: Boolean read FChecked write SetChecked default False;
    property SwitchSettings: TSwitchSettings read FSwitchSettings write SetSwitchSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;
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
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

procedure Register;

implementation

procedure DrawStringWithGDI(AGraphics: TGPGraphics; const AText: string; AFont: TFont; const ARect: TGPRectF; AStringFormat: TGPStringFormat; AColor: TColor; AOpacity: Byte = 255);
var LStyle: Integer; StatusFont: TGPFont; FontBrush: TGPSolidBrush;
begin
  if not Assigned(AGraphics) or (AText = '') or (AOpacity = 0) then Exit;
  LStyle := FontStyleRegular; if fsBold in AFont.Style then LStyle := LStyle or FontStyleBold; if fsItalic in AFont.Style then LStyle := LStyle or FontStyleItalic; if Vcl.Graphics.fsUnderline in AFont.Style then LStyle := LStyle or FontStyleUnderline; if fsStrikeOut in AFont.Style then LStyle := LStyle or FontStyleStrikeout;
  StatusFont := TGPFont.Create(AFont.Name, Single(AFont.Size), LStyle, UnitPixel);
  try FontBrush := TGPSolidBrush.Create(ColorToARGB(AColor, AOpacity)); try AGraphics.DrawString(AText, -1, StatusFont, ARect, AStringFormat, FontBrush); finally FontBrush.Free; end; finally StatusFont.Free; end;
end;
procedure Register; begin RegisterComponents('HOTLINE', [THTL_CSwitch]); end;

{ TSwitchSettings }
constructor TSwitchSettings.Create;
begin
  inherited;
  FArchetype := saPill;
  FDrawThumbShadow := False;
  FDrawThumbInnerBorder := False;
  FDrawTextLabel := False;
  FIconSet := isNone;
  FTrackColorUnchecked := TColor($00BBBBBB);
  FTrackColorChecked   := TColor($0050AF4C);
  FTrackBorderColor    := clGray;
  FThumbColorUnchecked := clWhite;
  FThumbColorChecked   := clWhite;
  FThumbSizeRatio      := 0.85;
  FTextChecked := '';
  FTextUnchecked := '';
  FFontChecked := TFont.Create;
  FFontChecked.OnChange := FontChanged;
  FFontChecked.Name := 'Segoe UI';
  FFontChecked.Size := 7;
  FFontChecked.Color := clWhite;
  FFontChecked.Style := [fsBold];
  FFontUnchecked := TFont.Create;
  FFontUnchecked.OnChange := FontChanged;
  FFontUnchecked.Assign(FFontChecked);
end;

destructor TSwitchSettings.Destroy; begin FFontChecked.Free; FFontUnchecked.Free; inherited; end;

procedure TSwitchSettings.Assign(Source: TPersistent);
var LSource: TSwitchSettings;
begin
  if Source is TSwitchSettings then
  begin
    LSource := TSwitchSettings(Source);
    FArchetype            := LSource.FArchetype;
    FDrawThumbShadow      := LSource.FDrawThumbShadow;
    FDrawThumbInnerBorder := LSource.FDrawThumbInnerBorder;
    FDrawTextLabel        := LSource.FDrawTextLabel;
    FIconSet              := LSource.FIconSet;
    FTrackColorUnchecked  := LSource.FTrackColorUnchecked;
    FTrackColorChecked    := LSource.FTrackColorChecked;
    FTrackBorderColor     := LSource.FTrackBorderColor;
    FThumbColorUnchecked  := LSource.FThumbColorUnchecked;
    FThumbColorChecked    := LSource.FThumbColorChecked;
    FThumbSizeRatio       := LSource.FThumbSizeRatio;
    FTextChecked          := LSource.FTextChecked;
    FTextUnchecked        := LSource.FTextUnchecked;
    FFontChecked.Assign(LSource.FFontChecked);
    FFontUnchecked.Assign(LSource.FFontUnchecked);
    Changed;
  end else inherited Assign(Source);
end;
procedure TSwitchSettings.Changed; begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure TSwitchSettings.FontChanged(Sender: TObject); begin Changed; end;
procedure TSwitchSettings.SetArchetype(const Value: TSwitchArchetype); begin if FArchetype <> Value then begin FArchetype := Value; Changed; end; end;
procedure TSwitchSettings.SetDrawThumbShadow(const Value: Boolean); begin if FDrawThumbShadow <> Value then begin FDrawThumbShadow := Value; Changed; end; end;
procedure TSwitchSettings.SetDrawThumbInnerBorder(const Value: Boolean); begin if FDrawThumbInnerBorder <> Value then begin FDrawThumbInnerBorder := Value; Changed; end; end;
procedure TSwitchSettings.SetDrawTextLabel(const Value: Boolean); begin if FDrawTextLabel <> Value then begin FDrawTextLabel := Value; Changed; end; end;
procedure TSwitchSettings.SetIconSet(const Value: TSwitchIconSet); begin if FIconSet <> Value then begin FIconSet := Value; Changed; end; end;
procedure TSwitchSettings.SetFontChecked(const Value: TFont); begin FFontChecked.Assign(Value); end;
procedure TSwitchSettings.SetFontUnchecked(const Value: TFont); begin FFontUnchecked.Assign(Value); end;
procedure TSwitchSettings.SetTrackColorUnchecked(const Value: TColor); begin if FTrackColorUnchecked <> Value then begin FTrackColorUnchecked := Value; Changed; end; end;
procedure TSwitchSettings.SetTrackColorChecked(const Value: TColor); begin if FTrackColorChecked <> Value then begin FTrackColorChecked := Value; Changed; end; end;
procedure TSwitchSettings.SetTrackBorderColor(const Value: TColor); begin if FTrackBorderColor <> Value then begin FTrackBorderColor := Value; Changed; end; end;
procedure TSwitchSettings.SetThumbColorUnchecked(const Value: TColor); begin if FThumbColorUnchecked <> Value then begin FThumbColorUnchecked := Value; Changed; end; end;
procedure TSwitchSettings.SetThumbColorChecked(const Value: TColor); begin if FThumbColorChecked <> Value then begin FThumbColorChecked := Value; Changed; end; end;
procedure TSwitchSettings.SetThumbSizeRatio(const Value: Single); var LNewValue: Single; begin LNewValue := Max(0.1, Min(1.0, Value)); if FThumbSizeRatio <> LNewValue then begin FThumbSizeRatio := LNewValue; Changed; end; end;
procedure TSwitchSettings.SetTextChecked(const Value: string); begin if FTextChecked <> Value then begin FTextChecked := Value; Changed; end; end;
procedure TSwitchSettings.SetTextUnchecked(const Value: string); begin if FTextUnchecked <> Value then begin FTextUnchecked := Value; Changed; end; end;

{ THTL_CSwitch }
constructor THTL_CSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csReplicatable, csDoubleClicks];
  Width := 150; Height := 35; TabStop := True; Cursor := crHandPoint; DoubleBuffered := True;
  FChecked := False;
  FSwitchSettings := TSwitchSettings.Create;
  FSwitchSettings.OnChange := SettingsChanged;

  // *** CORREÇÃO: Aplica o estilo padrão APENAS em tempo de design ***
  if (csDesigning in ComponentState) then
  begin
    ApplyStyleSettings(ssDefault);
    FStyle := ssDefault;
  end;

  FCaptionSettings := TCaptionSettings.Create(Self); FCaptionSettings.OnChange := CaptionSettingsChanged; FCaptionSettings.Text := 'THTL_CSwitch'; FCaptionSettings.Alignment := taLeftJustify; FCaptionSettings.Position := cpRight; FCaptionSettings.Margins.Left := 8;
  FHoverSettings := THoverSettings.Create(Self); FHoverSettings.OnChange := HoverSettingsChanged; FHoverSettings.BackgroundColor := clNone; FHoverSettings.BorderColor := clNone; FHoverSettings.HoverEffect := heFade;
  FAnimationTimer := TTimer.Create(Self); FAnimationTimer.Enabled := False; FAnimationTimer.Interval := 15; FAnimationTimer.OnTimer := AnimationTimerHandler; FAnimationProgress := 0.0; FAnimateToChecked := False;
end;

destructor THTL_CSwitch.Destroy;
begin FSwitchSettings.Free; FCaptionSettings.Free; FHoverSettings.Free; FAnimationTimer.Free; inherited; end;

procedure THTL_CSwitch.Loaded;
begin inherited; if FChecked then FAnimationProgress := 1.0 else FAnimationProgress := 0.0; end;

procedure THTL_CSwitch.SetChecked(const Value: Boolean);
begin if FChecked <> Value then begin FChecked := Value; StartToggleAnimation(Value); if Assigned(FOnCheckChanged) then FOnCheckChanged(Self); end; end;

procedure THTL_CSwitch.StartToggleAnimation(AChecked: Boolean);
begin
  if not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  begin FAnimateToChecked := AChecked; FAnimationTimer.Enabled := True; end
  else begin if AChecked then FAnimationProgress := 1.0 else FAnimationProgress := 0.0; Repaint; end;
end;

procedure THTL_CSwitch.AnimationTimerHandler(Sender: TObject);
const ANIMATION_STEP = 0.1; var TargetProgress: Single; NeedsRepaint: Boolean;
begin
  NeedsRepaint := False; if FAnimateToChecked then TargetProgress := 1.0 else TargetProgress := 0.0;
  if abs(FAnimationProgress - TargetProgress) < ANIMATION_STEP then
  begin FAnimationProgress := TargetProgress; FAnimationTimer.Enabled := False; NeedsRepaint := True; end
  else begin if FAnimationProgress < TargetProgress then FAnimationProgress := FAnimationProgress + ANIMATION_STEP else FAnimationProgress := FAnimationProgress - ANIMATION_STEP; NeedsRepaint := True; end;
  if NeedsRepaint then Repaint;
end;

procedure THTL_CSwitch.SetSwitchSettings(const Value: TSwitchSettings); begin FSwitchSettings.Assign(Value); end;
procedure THTL_CSwitch.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); end;
procedure THTL_CSwitch.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); end;

procedure THTL_CSwitch.BreakStyleLink;
begin if (csLoading in ComponentState) or (csDestroying in ComponentState) then Exit; if FStyle <> ssCustom then FStyle := ssCustom; end;

procedure THTL_CSwitch.ApplyStyleSettings(AStyle: TSwitchStyle);
begin
  if AStyle = ssCustom then Exit;

  FSwitchSettings.OnChange := nil;
  try
    FSwitchSettings.DrawTextLabel := False;
    FSwitchSettings.DrawThumbShadow := False;
    FSwitchSettings.DrawThumbInnerBorder := False;
    FSwitchSettings.IconSet := isNone;
    FSwitchSettings.TextChecked := '';
    FSwitchSettings.TextUnchecked := '';

    case AStyle of
      ssDefault:
        begin
          FSwitchSettings.Archetype           := saPill;
          FSwitchSettings.DrawTextLabel       := True;
          FSwitchSettings.TrackColorUnchecked := TColor($00BBBBBB);
          FSwitchSettings.TrackColorChecked   := TColor($0050AF4C);
          FSwitchSettings.TrackBorderColor    := clGray;
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.85;
          FSwitchSettings.TextChecked         := 'ON';
          FSwitchSettings.TextUnchecked       := 'OFF';
        end;
      ssFlat:
        begin
          FSwitchSettings.Archetype           := saPill;
          FSwitchSettings.TrackColorUnchecked := TColor($00E0E0E0);
          FSwitchSettings.TrackColorChecked   := TColor($0084C781);
          FSwitchSettings.TrackBorderColor    := clNone;
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.8;
        end;
      ssIOS:
        begin
          FSwitchSettings.Archetype           := saPill;
          FSwitchSettings.DrawThumbShadow     := True;
          FSwitchSettings.DrawThumbInnerBorder:= True;
          FSwitchSettings.TrackColorUnchecked := TColor($00EBE9E9);
          FSwitchSettings.TrackColorChecked   := TColor($0059C734);
          FSwitchSettings.TrackBorderColor    := TColor($00DADADA);
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.93;
        end;
      ssMaterial:
        begin
          FSwitchSettings.Archetype           := saPill;
          FSwitchSettings.DrawThumbShadow     := True;
          FSwitchSettings.TrackColorUnchecked := TColor($00BDBDBD);
          FSwitchSettings.TrackColorChecked   := TColor($0084C781);
          FSwitchSettings.TrackBorderColor    := clNone;
          FSwitchSettings.ThumbColorUnchecked := TColor($00F5F5F5);
          FSwitchSettings.ThumbColorChecked   := TColor($0050AF4C);
          FSwitchSettings.ThumbSizeRatio      := 0.8;
        end;
      ssSquareThumb:
        begin
          FSwitchSettings.Archetype           := saSquare;
          FSwitchSettings.TrackColorUnchecked := TColor($00E6DCD8);
          FSwitchSettings.TrackColorChecked   := TColor($0086B852);
          FSwitchSettings.TrackBorderColor    := clNone;
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.8;
        end;
      ssLineSlider:
        begin
          FSwitchSettings.Archetype           := saLine;
          FSwitchSettings.DrawThumbShadow     := True;
          FSwitchSettings.DrawThumbInnerBorder:= True;
          FSwitchSettings.TrackColorUnchecked := TColor($00DAD4CE);
          FSwitchSettings.TrackColorChecked   := TColor($0086B852);
          FSwitchSettings.TrackBorderColor    := clNone;
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.85;
        end;
      ssIOSWithIcon:
        begin
          FSwitchSettings.Archetype           := saPill;
          FSwitchSettings.DrawThumbShadow     := True;
          FSwitchSettings.DrawThumbInnerBorder:= True;
          FSwitchSettings.IconSet             := isPower;
          FSwitchSettings.TrackColorUnchecked := TColor($00E6DCD8);
          FSwitchSettings.TrackColorChecked   := TColor($0086B852);
          FSwitchSettings.TrackBorderColor    := TColor($00D3CEC8);
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := clWhite;
          FSwitchSettings.ThumbSizeRatio      := 0.85;
        end;
      ssSquareWithIcon:
        begin
          FSwitchSettings.Archetype           := saSquare;
          FSwitchSettings.IconSet             := isCheckX;
          FSwitchSettings.TrackColorUnchecked := TColor($00E6DCD8);
          FSwitchSettings.TrackColorChecked   := TColor($0086B852);
          FSwitchSettings.TrackBorderColor    := clNone;
          FSwitchSettings.ThumbColorUnchecked := clWhite;
          FSwitchSettings.ThumbColorChecked   := TColor($0086B852);
          FSwitchSettings.ThumbSizeRatio      := 0.85;
        end;
      ssSlidingIcon:
        begin
          FSwitchSettings.Archetype           := saSlidingIcon;
          FSwitchSettings.IconSet             := isCheckX;
          FSwitchSettings.TrackColorUnchecked := TColor($00F0F0F0);
          FSwitchSettings.TrackColorChecked   := TColor($0086B852);
          FSwitchSettings.TrackBorderColor    := TColor($00D0D0D0);
          FSwitchSettings.ThumbColorUnchecked := clNone;
          FSwitchSettings.ThumbColorChecked   := clNone;
          FSwitchSettings.ThumbSizeRatio      := 0.75;
        end;
    end;
  finally
    FSwitchSettings.OnChange := SettingsChanged;
  end;
end;

procedure THTL_CSwitch.SetStyle(const Value: TSwitchStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  if Value <> ssCustom then
  begin
    ApplyStyleSettings(Value);
  end;
  Repaint;
end;

procedure THTL_CSwitch.SettingsChanged(Sender: TObject);
begin
  BreakStyleLink;
  Repaint;
end;

procedure THTL_CSwitch.HoverSettingsChanged(Sender: TObject); begin Repaint; end;
procedure THTL_CSwitch.CaptionSettingsChanged(Sender: TObject); begin Repaint; end;
procedure THTL_CSwitch.Click; begin inherited; if Enabled then Self.Checked := not Self.Checked; end;
procedure THTL_CSwitch.KeyDown(var Key: Word; Shift: TShiftState); begin inherited; if Enabled and (Key in [VK_RETURN, VK_SPACE]) then begin Click; Key := 0; end; end;
procedure THTL_CSwitch.CMMouseEnter(var Message: TMessage); begin inherited; if Enabled and FHoverSettings.Enabled then FHoverSettings.StartAnimation(True); end;
procedure THTL_CSwitch.CMMouseLeave(var Message: TMessage); begin inherited; if Enabled and FHoverSettings.Enabled then FHoverSettings.StartAnimation(False); end;
procedure THTL_CSwitch.CMEnabledChanged(var Message: TMessage); begin inherited; Repaint; end;

procedure THTL_CSwitch.Paint;
var
  LG: TGPGraphics;
  SwitchPartRect, CaptionPartRect: TRect;
  TrackRect, ThumbRect, IconRect: TGPRectF;
  Path, IconPath: TGPGraphicsPath;
  Brush: TGPBrush;
  Pen, IconPen: TGPPen;
  Radius, ThumbSize, ThumbPadding, ThumbX, ThumbY: Single;
  ActualTrackColor, ActualThumbColor, ActualBorderColor: TColor;
  HoverFactor: Single;
  DisabledColorFactor: Single;
  StringFormat: TGPStringFormat;
  TrackCornerRadius, ThumbCornerRadius: Single;
  IconColor: TColor;
  IconOpacity: Byte;
  InflateMargin: Single;
begin
  inherited Paint;
  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);
    LG.SetTextRenderingHint(TextRenderingHintAntiAlias);

    // 1. LÓGICA DE LAYOUT RESPONSIVO (Inalterada)
    var  TotalRect: TRect;
    var  SwitchMinWidth, SwitchNaturalWidth, FinalSwitchWidth: Integer;
    var  CaptionReqWidth, CaptionReqHeight: Integer;
    var  StatusTextCheckedSize, StatusTextUncheckedSize: TSize;
    TotalRect := ClientRect;
    SwitchPartRect := TotalRect;
    CaptionPartRect := System.Types.Rect(0,0,0,0);
    Canvas.Font.Assign(FSwitchSettings.FontChecked);
    StatusTextCheckedSize := Canvas.TextExtent(FSwitchSettings.TextChecked);
    Canvas.Font.Assign(FSwitchSettings.FontUnchecked);
    StatusTextUncheckedSize := Canvas.TextExtent(FSwitchSettings.TextUnchecked);
    var TextPadding := Round(TotalRect.Height * 0.5);
    SwitchMinWidth := StatusTextCheckedSize.cx + StatusTextUncheckedSize.cx + TextPadding;
    SwitchNaturalWidth := Round(TotalRect.Height * 1.8);
    FinalSwitchWidth := Max(SwitchMinWidth, SwitchNaturalWidth);
    if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') then
    begin
       Canvas.Font.Assign(FCaptionSettings.Font);
       var CaptionSize := Canvas.TextExtent(FCaptionSettings.Text);
       CaptionReqWidth := CaptionSize.cx + FCaptionSettings.Margins.Left + FCaptionSettings.Margins.Right;
       CaptionReqHeight := CaptionSize.cy + FCaptionSettings.Margins.Top + FCaptionSettings.Margins.Bottom;
       case FCaptionSettings.Position of
        cpLeft:
        begin
          CaptionPartRect := System.Types.Rect(TotalRect.Left, TotalRect.Top, TotalRect.Left + CaptionReqWidth, TotalRect.Bottom);
          SwitchPartRect := System.Types.Rect(CaptionPartRect.Right, TotalRect.Top, TotalRect.Right, TotalRect.Bottom);
        end;
        cpRight:
        begin
          var ActualSwitchWidth := Min(FinalSwitchWidth, TotalRect.Width - CaptionReqWidth);
          SwitchPartRect := System.Types.Rect(TotalRect.Left, TotalRect.Top, TotalRect.Left + ActualSwitchWidth, TotalRect.Bottom);
          CaptionPartRect := System.Types.Rect(SwitchPartRect.Right, TotalRect.Top, TotalRect.Right, TotalRect.Bottom);
        end;
        cpAbove:
        begin
           CaptionPartRect := System.Types.Rect(TotalRect.Left, TotalRect.Top, TotalRect.Right, TotalRect.Top + CaptionReqHeight);
           SwitchPartRect := System.Types.Rect(TotalRect.Left, CaptionPartRect.Bottom, TotalRect.Right, TotalRect.Bottom);
        end;
        cpBelow:
        begin
           SwitchPartRect.Height := SwitchPartRect.Height - CaptionReqHeight;
           CaptionPartRect := System.Types.Rect(TotalRect.Left, SwitchPartRect.Bottom, TotalRect.Right, TotalRect.Bottom);
        end;
      end;
    end;

    // 2. Calcular cores e fatores de animação
    HoverFactor := FHoverSettings.CurrentAnimationValue / 255.0;
    ActualTrackColor := BlendColors(FSwitchSettings.TrackColorUnchecked, FSwitchSettings.TrackColorChecked, FAnimationProgress);
    if FSwitchSettings.Archetype <> saSlidingIcon then
      ActualThumbColor := BlendColors(FSwitchSettings.ThumbColorUnchecked, FSwitchSettings.ThumbColorChecked, FAnimationProgress)
    else
      ActualThumbColor := clNone;
    ActualBorderColor := FSwitchSettings.TrackBorderColor;
    if HoverFactor > 0 then
    begin
        ActualTrackColor := BlendColors(ActualTrackColor, LighterColor(ActualTrackColor, 15), HoverFactor);
        if ActualThumbColor <> clNone then
          ActualThumbColor := BlendColors(ActualThumbColor, LighterColor(ActualThumbColor, 10), HoverFactor);
        ActualBorderColor := BlendColors(ActualBorderColor, DarkerColor(ActualBorderColor, 15), HoverFactor);
    end;
    if not Enabled then
    begin
      DisabledColorFactor := 0.6;
      ActualTrackColor := BlendColors(ActualTrackColor, clGray, DisabledColorFactor);
       if ActualThumbColor <> clNone then
        ActualThumbColor := BlendColors(ActualThumbColor, clGray, DisabledColorFactor);
      ActualBorderColor := BlendColors(ActualBorderColor, clGray, DisabledColorFactor);
    end;

    // 3. Desenhar o Fundo (Track)
    TrackRect.X      := SwitchPartRect.Left;
    TrackRect.Y      := SwitchPartRect.Top;
    TrackRect.Width  := SwitchPartRect.Width;
    TrackRect.Height := SwitchPartRect.Height;
    TrackRect.X      := TrackRect.X + 1;
    TrackRect.Y      := TrackRect.Y + 1;
    TrackRect.Width  := TrackRect.Width - 2;
    TrackRect.Height := TrackRect.Height - 2;
    Path := TGPGraphicsPath.Create;
    try
      if FSwitchSettings.Archetype = saLine then
      begin
        Radius := TrackRect.Height / 2.0;
        Pen := TGPPen.Create(ColorToARGB(ActualTrackColor), 3.0);
        try
          Pen.SetStartCap(LineCapRound);
          Pen.SetEndCap(LineCapRound);
          var LineY := TrackRect.Y + TrackRect.Height / 2;
          LG.DrawLine(Pen, TrackRect.X + Radius, LineY, TrackRect.X + TrackRect.Width - Radius, LineY);
        finally
          Pen.Free;
        end;
      end
      else
      begin
        var BorderToDraw: TColor;
        if (FSwitchSettings.Archetype = saSlidingIcon) and (FAnimationProgress < 0.5) then
           BorderToDraw := FSwitchSettings.TrackBorderColor
        else if (FSwitchSettings.IconSet = isPower) and (FAnimationProgress < 0.5) then
           BorderToDraw := FSwitchSettings.TrackBorderColor
        else
           BorderToDraw := clNone;

        if FSwitchSettings.Archetype in [saSquare, saSlidingIcon] then
           TrackCornerRadius := 6
        else
           TrackCornerRadius := TrackRect.Height / 2.0;

        CreateGPRoundedPath(Path, TrackRect, TrackCornerRadius, rctAll);
        if Path.GetPointCount > 0 then
        begin
          Brush := TGPSolidBrush.Create(ColorToARGB(ActualTrackColor));
          try LG.FillPath(Brush, Path); finally Brush.Free; end;
          if BorderToDraw <> clNone then
          begin
            Pen := TGPPen.Create(ColorToARGB(BorderToDraw), 1.0);
            try LG.DrawPath(Pen, Path); finally Pen.Free; end;
          end;
        end;
      end;
    finally
      Path.Free;
    end;

    // 4. Desenhar Texto de Status (ON/OFF)
    if FSwitchSettings.DrawTextLabel then
    begin
      StringFormat := TGPStringFormat.Create;
      try
        StringFormat.SetLineAlignment(StringAlignmentCenter);
        var LblPadding := Abs(TrackRect.Height * 0.2);
        IconOpacity := Round(255 * FAnimationProgress);
        if (Trim(FSwitchSettings.TextChecked) <> '') and (IconOpacity > 0) then
        begin
          StringFormat.SetAlignment(StringAlignmentNear);
          var TextRectChecked: TGPRectF := TrackRect;
          TextRectChecked.X := TextRectChecked.X + LblPadding; TextRectChecked.Width := TextRectChecked.Width - (2 * LblPadding);
          var FinalFontColor := IfThen(Enabled, FSwitchSettings.FontChecked.Color, BlendColors(FSwitchSettings.FontChecked.Color, clGray, DisabledColorFactor));
          DrawStringWithGDI(LG, FSwitchSettings.TextChecked, FSwitchSettings.FontChecked, TextRectChecked, StringFormat, FinalFontColor, IconOpacity);
        end;
        IconOpacity := Round(255 * (1 - FAnimationProgress));
        if (Trim(FSwitchSettings.TextUnchecked) <> '') and (IconOpacity > 0) then
        begin
          StringFormat.SetAlignment(StringAlignmentFar);
          var TextRectUnchecked: TGPRectF := TrackRect;
          TextRectUnchecked.X := TextRectUnchecked.X + LblPadding; TextRectUnchecked.Width := TextRectUnchecked.Width - (2 * LblPadding);
          var FinalFontColor := IfThen(Enabled, FSwitchSettings.FontUnchecked.Color, BlendColors(FSwitchSettings.FontUnchecked.Color, clGray, DisabledColorFactor));
          DrawStringWithGDI(LG, FSwitchSettings.TextUnchecked, FSwitchSettings.FontUnchecked, TextRectUnchecked, StringFormat, FinalFontColor, IconOpacity);
        end;
      finally
        StringFormat.Free;
      end;
    end;

    // 5. Desenhar o Botão (Thumb) ou o Ícone Deslizante
    if (TrackRect.Height > 0) and (TrackRect.Width > TrackRect.Height) then
    begin
      if FSwitchSettings.Archetype = saSlidingIcon then
      begin
        IconPath := TGPGraphicsPath.Create;
        IconPen := TGPPen.Create(clWhite, 2.0);
        try
            IconPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
            var iconSize   := TrackRect.Height * 0.55;
            var iconMargin := (TrackRect.Height - iconSize) / 2;
            var startIconX := TrackRect.X + iconMargin;
            var endIconX   := TrackRect.X + TrackRect.Width - iconSize - iconMargin;
            var iconX      := startIconX + (endIconX - startIconX) * FAnimationProgress;
            IconRect.X := iconX; IconRect.Y := TrackRect.Y + iconMargin; IconRect.Width := iconSize; IconRect.Height := iconSize;
            IconOpacity := Round(255 * FAnimationProgress);
            if IconOpacity > 0 then
            begin
              IconPen.SetColor(ColorToARGB(clWhite, IconOpacity));
              IconPath.Reset;
              IconPath.AddLine(IconRect.X, IconRect.Y + IconRect.Height * 0.5, IconRect.X + IconRect.Width * 0.4, IconRect.Y + IconRect.Height);
              IconPath.AddLine(IconRect.X + IconRect.Width * 0.4, IconRect.Y + IconRect.Height, IconRect.X + IconRect.Width, IconRect.Y);
              LG.DrawPath(IconPen, IconPath);
            end;
            IconOpacity := Round(255 * (1 - FAnimationProgress));
            if IconOpacity > 0 then
            begin
              IconPen.SetColor(ColorToARGB(TColor($009A9A9A), IconOpacity));
              IconPath.Reset;
              IconPath.AddLine(IconRect.X, IconRect.Y, IconRect.X + IconRect.Width, IconRect.Y + IconRect.Height);
              LG.DrawPath(IconPen, IconPath);
              IconPath.Reset;
              IconPath.AddLine(IconRect.X, IconRect.Y + IconRect.Height, IconRect.X + IconRect.Width, IconRect.Y);
              LG.DrawPath(IconPen, IconPath);
            end;
        finally
            IconPath.Free;
            IconPen.Free;
        end;
      end
      else
      begin
        ThumbSize := TrackRect.Height * FSwitchSettings.ThumbSizeRatio;
        ThumbPadding := (TrackRect.Height - ThumbSize) / 2.0;
        var StartX := TrackRect.X + ThumbPadding;
        var EndX := TrackRect.X + TrackRect.Width - ThumbSize - ThumbPadding;
        ThumbX := StartX + (EndX - StartX) * FAnimationProgress;
        ThumbY := TrackRect.Y + ThumbPadding;
        ThumbRect.X := ThumbX; ThumbRect.Y := ThumbY; ThumbRect.Width := ThumbSize; ThumbRect.Height := ThumbSize;
        Brush := TGPSolidBrush.Create(ColorToARGB(ActualThumbColor));
        try
          if FSwitchSettings.Archetype = saSquare then
          begin
            ThumbCornerRadius := 4.0;
            Path := TGPGraphicsPath.Create;
            try CreateGPRoundedPath(Path, ThumbRect, ThumbCornerRadius, rctAll); LG.FillPath(Brush, Path); finally Path.Free; end;
          end else begin
            if FSwitchSettings.DrawThumbShadow then
            begin
              var ShadowBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, 40));
              try LG.FillEllipse(ShadowBrush, ThumbX, ThumbY + 1, ThumbSize, ThumbSize); finally ShadowBrush.Free; end;
            end;
            LG.FillEllipse(Brush, ThumbX, ThumbY, ThumbSize, ThumbSize);
            if FSwitchSettings.DrawThumbInnerBorder then
            begin
              var InnerShadowPen := TGPPen.Create(ColorToARGB(clBlack, 35), 1.5);
              try LG.DrawEllipse(InnerShadowPen, ThumbX, ThumbY, ThumbSize, ThumbSize); finally InnerShadowPen.Free; end;
            end;
          end;

          if FSwitchSettings.IconSet <> isNone then
          begin
            IconPath := TGPGraphicsPath.Create;
            IconPen := TGPPen.Create(clWhite, 2.0);
            try
              IconPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
              IconRect := ThumbRect;
              InflateMargin := ThumbSize * 0.25;
              IconRect.X := IconRect.X + InflateMargin; IconRect.Y := IconRect.Y + InflateMargin; IconRect.Width := IconRect.Width - (2 * InflateMargin); IconRect.Height := IconRect.Height - (2 * InflateMargin);
              if FSwitchSettings.IconSet = isPower then
              begin
                IconColor := ActualTrackColor;
                IconOpacity := Round(255 * FAnimationProgress);
                if IconOpacity > 0 then
                begin
                  IconPen.SetColor(ColorToARGB(IconColor, IconOpacity));
                  LG.DrawLine(IconPen, IconRect.X + IconRect.Width/2, IconRect.Y, IconRect.X + IconRect.Width/2, IconRect.Y + IconRect.Height);
                end;
                IconOpacity := Round(255 * (1 - FAnimationProgress));
                if IconOpacity > 0 then
                begin
                  IconPen.SetColor(ColorToARGB(IconColor, IconOpacity));
                  LG.DrawEllipse(IconPen, IconRect);
                end;
              end
              else if FSwitchSettings.IconSet = isCheckX then
              begin
                IconOpacity := Round(255 * FAnimationProgress);
                if IconOpacity > 0 then
                begin
                  IconPen.SetColor(ColorToARGB(clWhite, IconOpacity));
                  IconPath.Reset;
                  IconPath.AddLine(IconRect.X, IconRect.Y + IconRect.Height * 0.5, IconRect.X + IconRect.Width * 0.4, IconRect.Y + IconRect.Height);
                  IconPath.AddLine(IconRect.X + IconRect.Width * 0.4, IconRect.Y + IconRect.Height, IconRect.X + IconRect.Width, IconRect.Y);
                  LG.DrawPath(IconPen, IconPath);
                end;
                IconOpacity := Round(255 * (1 - FAnimationProgress));
                if IconOpacity > 0 then
                begin
                  IconColor := TColor($00808080);
                  IconPen.SetColor(ColorToARGB(IconColor, IconOpacity));
                  IconPath.Reset;
                  IconPath.AddLine(IconRect.X, IconRect.Y, IconRect.X + IconRect.Width, IconRect.Y + IconRect.Height);
                  LG.DrawPath(IconPen, IconPath);
                  IconPath.Reset;
                  IconPath.AddLine(IconRect.X, IconRect.Y + IconRect.Height, IconRect.X + IconRect.Width, IconRect.Y);
                  LG.DrawPath(IconPen, IconPath);
                end;
              end;
            finally
              IconPath.Free;
              IconPen.Free;
            end;
          end;
        finally
          Brush.Free;
        end;
      end;
    end;

    // 6. Desenhar Legenda Externa (Inalterado)
    if FCaptionSettings.Visible and (Trim(FCaptionSettings.Text) <> '') then
    begin
      var FontColor := IfThen(Enabled, FCaptionSettings.Font.Color, FCaptionSettings.DisabledColor);
      var LblRect := CaptionPartRect;
      LblRect.Left := LblRect.Left + FCaptionSettings.Margins.Left; LblRect.Top := LblRect.Top + FCaptionSettings.Margins.Top; LblRect.Right := LblRect.Right - FCaptionSettings.Margins.Right; LblRect.Bottom := LblRect.Bottom - FCaptionSettings.Margins.Bottom;
      if (LblRect.Width > 0) and (LblRect.Height > 0) then DrawComponentCaption(Self.Canvas, LblRect, FCaptionSettings.Text, FCaptionSettings.Font, FontColor, FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, 255);
    end;
  finally
    LG.Free;
  end;
  if Focused and TabStop and Enabled then DrawFocusRect(Canvas.Handle, ClientRect);
end;

end.
