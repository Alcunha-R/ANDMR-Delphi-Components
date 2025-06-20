unit HTL_CToggleSwitch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  HTL_ComponentUtils;

type
  THTL_ToggleStyle = (
    tsStyle1, // iOS-like, Grey/Purple
    tsStyle2, // Pill with text, Dark
    tsStyle3, // Pill with icon, Red/Green
    tsStyle4, // Pill with text, Red/Green
    tsStyle5, // iOS-like, Grey/Blue
    tsStyle6, // Pill with icon and text, Red/Green
    tsStyle7, // Minimalist, Border and Icon only
    tsCustom  // User-defined
  );

  THTL_CToggleSwitch = class(TCustomControl)
  private
    FChecked: Boolean;
    FOnColor: TColor;
    FOffColor: TColor;
    FThumbColor: TColor;
    FBorderColor: TColor;
    FOnChange: TNotifyEvent;
    FAnimationTimer: TTimer;
    FAnimationPosition: Single;
    FTargetPosition: Single;

    FStyle: THTL_ToggleStyle;
    FOnText: string;
    FOffText: string;
    FTextFont: TFont;
    FOnIcon: TPicture;
    FOffIcon: TPicture;
    FShowTextInTrack: Boolean;
    FShowIconInThumb: Boolean;

    procedure SetChecked(const Value: Boolean);
    procedure SetOnColor(const Value: TColor);
    procedure SetOffColor(const Value: TColor);
    procedure SetThumbColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetStyle(const Value: THTL_ToggleStyle);
    procedure SetOnText(const Value: string);
    procedure SetOffText(const Value: string);
    procedure SetTextFont(const Value: TFont);
    procedure SetOnIcon(const Value: TPicture);
    procedure SetOffIcon(const Value: TPicture);
    procedure SetShowTextInTrack(const Value: Boolean);
    procedure SetShowIconInThumb(const Value: Boolean);
    procedure IconChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);

    procedure Animate(Sender: TObject);
    procedure StartAnimation;
    procedure ApplyStyleSettings;

  protected
    procedure Paint; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    property Align;
    property Anchors;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Style: THTL_ToggleStyle read FStyle write SetStyle default tsStyle1;

    property OnColor: TColor read FOnColor write SetOnColor;
    property OffColor: TColor read FOffColor write SetOffColor;
    property ThumbColor: TColor read FThumbColor write SetThumbColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;

    property ShowTextInTrack: Boolean read FShowTextInTrack write SetShowTextInTrack default False;
    property OnText: string read FOnText write SetOnText;
    property OffText: string read FOffText write SetOffText;
    property TextFont: TFont read FTextFont write SetTextFont;

    property ShowIconInThumb: Boolean read FShowIconInThumb write SetShowIconInThumb default False;
    property OnIcon: TPicture read FOnIcon write SetOnIcon;
    property OffIcon: TPicture read FOffIcon write SetOffIcon;

    property Enabled;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

uses
  System.Math,
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  Winapi.GDIPUTIL;

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CToggleSwitch]);
end;

{ THTL_CToggleSwitch }

constructor THTL_CToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csClickEvents];
  Width := 80;
  Height := 34;
  DoubleBuffered := True;

  FOnText := 'ON';
  FOffText := 'OFF';
  FTextFont := TFont.Create;
  FTextFont.OnChange := FontChanged;
  FOnIcon := TPicture.Create;
  FOnIcon.OnChange := IconChanged;
  FOffIcon := TPicture.Create;
  FOffIcon.OnChange := IconChanged;

  FChecked := False;
  FStyle := tsCustom; // Inicia como custom para o setter de estilo funcionar
  SetStyle(tsStyle1); // Aplica o estilo padrão tsStyle1 com suas cores

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 10;
  FAnimationTimer.OnTimer := Animate;

  FAnimationPosition := 0.0;
  FTargetPosition := 0.0;
end;

destructor THTL_CToggleSwitch.Destroy;
begin
  FAnimationTimer.Free;
  FTextFont.Free;
  FOnIcon.Free;
  FOffIcon.Free;
  inherited;
end;

procedure THTL_CToggleSwitch.Assign(Source: TPersistent);
begin
  if Source is THTL_CToggleSwitch then
  begin
    inherited Assign(Source);
    with THTL_CToggleSwitch(Source) do
    begin
      Self.FOnColor := FOnColor;
      Self.FOffColor := FOffColor;
      Self.FThumbColor := FThumbColor;
      Self.FBorderColor := FBorderColor;
      Self.FOnText := FOnText;
      Self.FOffText := FOffText;
      Self.FTextFont.Assign(FTextFont);
      Self.FOnIcon.Assign(FOnIcon);
      Self.FOffIcon.Assign(FOffIcon);
      Self.FShowTextInTrack := FShowTextInTrack;
      Self.FShowIconInThumb := FShowIconInThumb;
      Self.SetStyle(FStyle);
      Self.SetChecked(FChecked);
    end;
  end else
    inherited Assign(Source);
end;

procedure THTL_CToggleSwitch.Loaded;
begin
  inherited;
  if FChecked then
  begin
    FAnimationPosition := 1.0;
    FTargetPosition := 1.0;
  end
  else
  begin
    FAnimationPosition := 0.0;
    FTargetPosition := 0.0;
  end;
  Invalidate;
end;

procedure THTL_CToggleSwitch.Resize;
begin
  inherited;
  Invalidate;
end;

procedure THTL_CToggleSwitch.Click;
begin
  inherited;
  if Enabled then
  begin
    SetChecked(not FChecked);
  end;
end;

procedure THTL_CToggleSwitch.IconChanged(Sender: TObject);
begin Invalidate; end;

procedure THTL_CToggleSwitch.FontChanged(Sender: TObject);
begin Invalidate; end;

procedure THTL_CToggleSwitch.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    StartAnimation;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THTL_CToggleSwitch.SetOnColor(const Value: TColor);
begin if FOnColor <> Value then begin FOnColor := Value; Invalidate; FStyle := tsCustom; end; end;
procedure THTL_CToggleSwitch.SetOffColor(const Value: TColor);
begin if FOffColor <> Value then begin FOffColor := Value; Invalidate; FStyle := tsCustom; end; end;
procedure THTL_CToggleSwitch.SetThumbColor(const Value: TColor);
begin if FThumbColor <> Value then begin FThumbColor := Value; Invalidate; FStyle := tsCustom; end; end;
procedure THTL_CToggleSwitch.SetBorderColor(const Value: TColor);
begin if FBorderColor <> Value then begin FBorderColor := Value; Invalidate; FStyle := tsCustom; end; end;
procedure THTL_CToggleSwitch.SetOnText(const Value: string);
begin if FOnText <> Value then begin FOnText := Value; Invalidate; end; end;
procedure THTL_CToggleSwitch.SetOffText(const Value: string);
begin if FOffText <> Value then begin FOffText := Value; Invalidate; end; end;
procedure THTL_CToggleSwitch.SetTextFont(const Value: TFont);
begin FTextFont.Assign(Value); Invalidate; end;
procedure THTL_CToggleSwitch.SetOnIcon(const Value: TPicture);
begin FOnIcon.Assign(Value); Invalidate; end;
procedure THTL_CToggleSwitch.SetOffIcon(const Value: TPicture);
begin FOffIcon.Assign(Value); Invalidate; end;

procedure THTL_CToggleSwitch.SetShowTextInTrack(const Value: Boolean);
begin if FShowTextInTrack <> Value then begin FShowTextInTrack := Value; Invalidate; FStyle := tsCustom; end; end;
procedure THTL_CToggleSwitch.SetShowIconInThumb(const Value: Boolean);
begin if FShowIconInThumb <> Value then begin FShowIconInThumb := Value; Invalidate; FStyle := tsCustom; end; end;

procedure THTL_CToggleSwitch.SetStyle(const Value: THTL_ToggleStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if FStyle <> tsCustom then
    begin
      ApplyStyleSettings;
    end;
    Invalidate;
  end;
end;

procedure THTL_CToggleSwitch.ApplyStyleSettings;
begin
  case FStyle of
    tsStyle1:
    begin
      FOnColor := TColor($FFAA96FD);
      FOffColor := TColor($FFE5E5EA);
      FThumbColor := clWhite;
      FBorderColor := TColor($FFE5E5EA);
      FShowTextInTrack := False;
      FShowIconInThumb := False;
    end;
    tsStyle2:
    begin
      FOnColor := TColor($FF323232);
      FOffColor := TColor($FF323232);
      FThumbColor := clWhite;
      FBorderColor := clBlack;
      FTextFont.Color := clWhite;
      FShowTextInTrack := True;
      FShowIconInThumb := False;
    end;
    tsStyle3:
    begin
      FOnColor := TColor($FF2ECC71);
      FOffColor := TColor($FFE74C3C);
      FThumbColor := clWhite;
      FBorderColor := TColor($00FFFFFF); // Borda transparente
      FShowTextInTrack := False;
      FShowIconInThumb := True;
    end;
    tsStyle4:
    begin
      FOnColor := TColor($FF2ECC71);
      FOffColor := TColor($FFE74C3C);
      FThumbColor := TColor($FFECF0F1); // Thumb cinza claro
      FBorderColor := TColor($00FFFFFF);
      FTextFont.Color := clWhite;
      FShowTextInTrack := True;
      FShowIconInThumb := False;
    end;
    tsStyle5:
    begin
      FOnColor := TColor($FF3498DB);
      FOffColor := TColor($FFBDC3C7);
      FThumbColor := clWhite;
      FBorderColor := TColor($00FFFFFF);
      FShowTextInTrack := False;
      FShowIconInThumb := False;
    end;
    tsStyle6:
    begin
      FOnColor := TColor($FF2ECC71);
      FOffColor := TColor($FFE74C3C);
      FThumbColor := clWhite;
      FBorderColor := TColor($00FFFFFF);
      FTextFont.Color := clWhite;
      FShowTextInTrack := True;
      FShowIconInThumb := True;
    end;
    tsStyle7:
    begin
      FOnColor := TColor($FF2ECC71);
      FOffColor := TColor($FFE74C3C);
      FThumbColor := clWhite;
      FBorderColor := TColor($FFBDC3C7);
      FShowTextInTrack := False;
      FShowIconInThumb := True;
    end;
  end;
end;

procedure THTL_CToggleSwitch.StartAnimation;
begin
  FTargetPosition := IfThen(FChecked, 1.0, 0.0);
  FAnimationTimer.Enabled := True;
end;

procedure THTL_CToggleSwitch.Animate(Sender: TObject);
const
  AnimationStep = 0.1;
begin
  if Abs(FAnimationPosition - FTargetPosition) < AnimationStep then
  begin
    FAnimationPosition := FTargetPosition;
    FAnimationTimer.Enabled := False;
  end
  else if FAnimationPosition < FTargetPosition then
  begin
    FAnimationPosition := FAnimationPosition + AnimationStep;
  end
  else
  begin
    FAnimationPosition := FAnimationPosition - AnimationStep;
  end;
  Invalidate;
end;

procedure THTL_CToggleSwitch.Paint;
const
  BorderWidth = 1.0;
var
  LG: TGPGraphics;
  LBackgroundColor, CurrentBorderColor: TColor;
  LTrackRectF, LThumbRectF: TGPRectF;
  LRadius, LThumbX: Single;
  LPadding, LThumbSize: Integer;
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  IconRect: TRect;
  IconToShow: TPicture;
begin
  inherited;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    LPadding := 2;
    LThumbSize := Height - (LPadding * 2);

    LTrackRectF.X := BorderWidth / 2.0;
    LTrackRectF.Y := BorderWidth / 2.0;
    LTrackRectF.Width := Max(0, Self.Width - BorderWidth);
    LTrackRectF.Height := Max(0, Self.Height - BorderWidth);
    LRadius := LTrackRectF.Height / 2.0;

    LBackgroundColor := BlendColors(FOffColor, FOnColor, FAnimationPosition);
    CurrentBorderColor := FBorderColor;

    if not Enabled then
    begin
      LBackgroundColor := clBtnFace;
      CurrentBorderColor := clGray;
    end;

    // --- Desenha a trilha (track) ---
    LPath := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(LPath, LTrackRectF, LRadius, rctAll);
      if FStyle = tsStyle7 then
        LBrush := TGPSolidBrush.Create(ColorToARGB(Self.Color)) // Fundo transparente
      else
        LBrush := TGPSolidBrush.Create(ColorToARGB(LBackgroundColor));
      try
        LG.FillPath(LBrush, LPath);
      finally
        LBrush.Free;
      end;

      LPen := TGPPen.Create(ColorToARGB(CurrentBorderColor), BorderWidth);
      try
        LG.DrawPath(LPen, LPath);
      finally
        LPen.Free;
      end;
    finally
      LPath.Free;
    end;

    // --- Desenha Texto na Trilha ---
    if FShowTextInTrack then
    begin
        var sf: TGPStringFormat := TGPStringFormat.Create;
        var font: TGPFont := TGPFont.Create(Self.Canvas.Handle, FTextFont.Handle);
        var textBrush: TGPSolidBrush;
        var textLayoutRect: TGPRectF;
        try
          sf.SetAlignment(StringAlignmentCenter);
          sf.SetLineAlignment(StringAlignmentCenter);
          textBrush := TGPSolidBrush.Create(ColorToARGB(FTextFont.Color));
          try
            // Desenha "OFF"
            textLayoutRect := LTrackRectF;
            textLayoutRect.Width := textLayoutRect.Width - LRadius - LPadding;
            textLayoutRect.X := LTrackRectF.X + LRadius;
            if FAnimationPosition < 0.8 then // Só desenha se houver espaço
              LG.DrawString(FOffText, Length(FOffText), font, textLayoutRect, sf, textBrush);

            // Desenha "ON"
            textLayoutRect := LTrackRectF;
            textLayoutRect.Width := textLayoutRect.Width - LRadius - LPadding;
            if FAnimationPosition > 0.2 then // Só desenha se houver espaço
              LG.DrawString(FOnText, Length(FOnText), font, textLayoutRect, sf, textBrush);
          finally
            textBrush.Free;
          end;
        finally
          sf.Free;
          font.Free;
        end;
    end;

    // --- Calcula e desenha o botão (thumb) ---
    LThumbX := LPadding + ((Width - (LPadding * 2) - LThumbSize) * FAnimationPosition);
    LThumbRectF.X := LThumbX;
    LThumbRectF.Y := LPadding;
    LThumbRectF.Width := LThumbSize;
    LThumbRectF.Height := LThumbSize;

    var ThumbBrushColor: TColor := FThumbColor;
    if not Enabled then ThumbBrushColor := LighterColor(clBtnFace, 20);

    LBrush := TGPSolidBrush.Create(ColorToARGB(ThumbBrushColor));
    try LG.FillEllipse(LBrush, LThumbRectF); finally LBrush.Free; end;

    // --- Desenha Ícone no Thumb ---
    if FShowIconInThumb then
    begin
        // **** LINHA CORRIGIDA ABAIXO ****
        if FChecked then
          IconToShow := FOnIcon
        else
          IconToShow := FOffIcon;

        if Assigned(IconToShow) and (IconToShow.Graphic <> nil) and not IconToShow.Graphic.Empty then
        begin
          var IconSize := Round(LThumbSize * 0.7);
          IconRect := Rect(
              Round(LThumbRectF.X + (LThumbSize - IconSize) / 2),
              Round(LThumbRectF.Y + (LThumbSize - IconSize) / 2),
              0,0
          );
          IconRect.Right := IconRect.Left + IconSize;
          IconRect.Bottom := IconRect.Top + IconSize;
          DrawGraphicWithGDI(LG, IconToShow.Graphic, IconRect, idmProportional);
        end;
    end;

  finally
    LG.Free;
  end;
end;

end.
