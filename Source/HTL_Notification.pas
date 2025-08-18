unit HTL_Notification;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, System.Math,
  Winapi.Windows, Winapi.Messages,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  HTL_ComponentUtils, HTL_CButton;

type
  TNotificationType = (ntInfo, ntSuccess, ntWarning, ntError, ntCustom);
  TNotificationPosition = (npTopRight, npBottomRight, npTopLeft, npBottomLeft, npCenterScreen, npCenterParent);
  TNotificationStyle = (nsToast, nsModal);
  TNotificationVisualStyle = (nvsColored, nvsLight);

  THTL_NotificationForm = class(TForm)
  private
    FNotificationType: TNotificationType;
    FNotificationStyle: TNotificationStyle;
    FVisualStyle: TNotificationVisualStyle;
    FPosition: TNotificationPosition;
    FDuration: Integer;
    FTitle: string;
    FMessage: string;
    FParentForm: TForm;

    FBorderSettings: TBorderSettings;
    FTitleSettings: TCaptionSettings;
    FMessageSettings: TCaptionSettings;
    FImageSettings: TImageSettings;
    FBtnClose: THTL_CButton;
    FCloseTimer: TTimer;

    procedure DrawDefaultIcon(AGraphics: TGPGraphics; const ADestRect: TRect; AColor: TColor; ABackgroundColor: TColor);
    procedure ApplyPreset;
    procedure SetupAppearance;
    procedure PositionForm;
    procedure FadeOutAndClose;
    procedure CloseTimerTick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowNotification(const ATitle, AMessage: string;
      AType: TNotificationType = ntInfo;
      AVisualStyle: TNotificationVisualStyle = nvsColored;
      AStyle: TNotificationStyle = nsToast;
      APosition: TNotificationPosition = npTopRight;
      ADuration: Integer = 4000;
      AParent: TForm = nil;
      ACustomIcon: TPicture = nil);
  end;

implementation

{ THTL_NotificationForm }

constructor THTL_NotificationForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  DoubleBuffered := True;
  OnClose := FormClose;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.CornerRadius := 35; // Raio maior para o formato "pill"
  FBorderSettings.Thickness := 1;
  FBorderSettings.RoundCornerType := rctAll;

  FTitleSettings := TCaptionSettings.Create(Self);
  FTitleSettings.Font.Name := 'Segoe UI';
  FTitleSettings.Font.Size := 10;
  FTitleSettings.Font.Style := [fsBold];
  FTitleSettings.Alignment := taLeftJustify;
  FTitleSettings.VerticalAlignment := cvaTop;

  FMessageSettings := TCaptionSettings.Create(Self);
  FMessageSettings.Font.Name := 'Segoe UI';
  FMessageSettings.Font.Size := 9;
  FMessageSettings.Alignment := taLeftJustify;
  FMessageSettings.VerticalAlignment := cvaTop;
  FMessageSettings.WordWrap := True;

  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.Visible := True;
  FImageSettings.AutoSize := False;
  FImageSettings.TargetWidth := 24; // Ícone menor
  FImageSettings.TargetHeight := 24;
  FImageSettings.ImagePosition := ipLeft;
  FImageSettings.Placement := iplInsideBounds;
  FImageSettings.Margins.Left := 15;
  FImageSettings.Margins.Right := 12;

  FBtnClose := THTL_CButton.Create(Self);
  FBtnClose.Parent := Self;
  FBtnClose.SetBounds(0, 0, 24, 24);
  FBtnClose.Caption := '✕'; // Caractere 'X' mais elegante
  FBtnClose.Style := bsFlat;
  FBtnClose.BorderSettings.BackgroundColor := clNone;
  FBtnClose.BorderSettings.Thickness := 0;
  FBtnClose.CaptionSettings.Font.Color := clGray;
  FBtnClose.CaptionSettings.Font.Size := 10;
  FBtnClose.HoverSettings.BackgroundColor := TColor($1A000000); // Fundo preto com baixa opacidade
  FBtnClose.HoverSettings.FontColor := clBlack;
  FBtnClose.Transparent := True;
  FBtnClose.OnClick := BtnCloseClick;

  FCloseTimer := TTimer.Create(Self);
  FCloseTimer.Enabled := False;
  FCloseTimer.OnTimer := CloseTimerTick;
end;

destructor THTL_NotificationForm.Destroy;
begin
  FBorderSettings.Free;
  FTitleSettings.Free;
  FMessageSettings.Free;
  FImageSettings.Free;
  inherited Destroy;
end;

procedure THTL_NotificationForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // Informamos ao Windows que lidamos com a limpeza do fundo.
  // Isso impede que a VCL pinte a propriedade sólida Color do formulário
  // antes que nosso método Paint personalizado seja chamado. Isso é crucial para
  // obter um formulário com áreas transparentes (como cantos arredondados).
  Message.Result := 1;
end;

procedure THTL_NotificationForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_LAYERED;
  Params.WndParent := GetDesktopWindow;
  if (FParentForm <> nil) and (FNotificationStyle = nsModal) then
    Params.WndParent := FParentForm.Handle;
end;

procedure THTL_NotificationForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure THTL_NotificationForm.BtnCloseClick(Sender: TObject);
begin
  FadeOutAndClose;
end;

procedure THTL_NotificationForm.CloseTimerTick(Sender: TObject);
begin
  FCloseTimer.Enabled := False;
  FadeOutAndClose;
end;

procedure THTL_NotificationForm.FadeOutAndClose;
var
  i: Integer;
begin
  FCloseTimer.Enabled := False;
  if Self.AlphaBlendValue = 0 then Exit;

  i := 255;
  while i >= 0 do
  begin
    Self.AlphaBlendValue := i;
    Application.ProcessMessages;
    Sleep(5);
    Dec(i, 15);
  end;
  Close;
end;

procedure THTL_NotificationForm.DrawDefaultIcon(AGraphics: TGPGraphics; const ADestRect: TRect; AColor: TColor; ABackgroundColor: TColor);
var
  Pen: TGPPen;
  Path, IconShapePath: TGPGraphicsPath;
  IconRect: TGPRectF;
  CenterX, CenterY, Radius: Single;
  Brush, DotBrush: TGPSolidBrush;
  IsLightStyle: Boolean;
  DotColor: TGPColor;
  CheckP1, CheckP2, CheckP3: TGPPointF;
  TrianglePoints: array[0..2] of TGPPointF;
  HexPoints: array[0..5] of TGPPointF;
  Angle: Single;
  i: Integer;
begin
  if (AGraphics = nil) or (ADestRect.Width <= 0) then Exit;

  IsLightStyle := FVisualStyle = nvsLight;

  Pen := TGPPen.Create(ColorToARGB(AColor, Self.AlphaBlendValue), 2);
  Pen.SetLineJoin(LineJoinRound);
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapRound);

  Path := TGPGraphicsPath.Create;
  IconShapePath := TGPGraphicsPath.Create;
  Brush := TGPSolidBrush.Create(ColorToARGB(AColor, Self.AlphaBlendValue));
  DotBrush := nil;
  try
    IconRect := RectToGPRectF(ADestRect);
    CenterX := IconRect.X + IconRect.Width / 2;
    CenterY := IconRect.Y + IconRect.Height / 2;
    Radius := (Min(IconRect.Width, IconRect.Height) / 2) - 1;

    // Desenha o fundo do ícone para o estilo 'Light'
    if IsLightStyle then
    begin
      AGraphics.FillEllipse(Brush, IconRect.X, IconRect.Y, IconRect.Width-1, IconRect.Height-1);
      Pen.SetColor(ColorToARGB(ABackgroundColor, Self.AlphaBlendValue)); // Cor do 'check' é a cor de fundo do form
    end;

    case FNotificationType of
      ntSuccess:
      begin
        if not IsLightStyle then
          AGraphics.DrawEllipse(Pen, IconRect.X, IconRect.Y, IconRect.Width-1, IconRect.Height-1);
        CheckP1 := MakePoint(CenterX - Radius * 0.45, CenterY);
        CheckP2 := MakePoint(CenterX - Radius * 0.1, CenterY + Radius * 0.4);
        CheckP3 := MakePoint(CenterX + Radius * 0.5, CenterY - Radius * 0.3);
        Path.AddLine(CheckP1, CheckP2);
        Path.AddLine(CheckP2, CheckP3);
        AGraphics.DrawPath(Pen, Path);
      end;
      ntWarning:
      begin
        TrianglePoints[0] := MakePoint(CenterX, IconRect.Y + 1);
        TrianglePoints[1] := MakePoint(IconRect.X + IconRect.Width - 1, IconRect.Y + IconRect.Height - 1);
        TrianglePoints[2] := MakePoint(IconRect.X + 1, IconRect.Y + IconRect.Height - 1);
        IconShapePath.AddPolygon(PGPPointF(@TrianglePoints[0]), 3);

        if IsLightStyle then AGraphics.FillPath(Brush, IconShapePath) else AGraphics.DrawPath(Pen, IconShapePath);

        Path.AddLine(CenterX, CenterY - Radius * 0.3, CenterX, CenterY + Radius * 0.2);
        AGraphics.DrawPath(Pen, Path);

        Pen.GetColor(DotColor);
        DotBrush := TGPSolidBrush.Create(DotColor);
        AGraphics.FillEllipse(DotBrush, CenterX - 1, CenterY + Radius * 0.5, 2, 2);
      end;
      ntInfo:
      begin
        if IsLightStyle then AGraphics.FillEllipse(Brush, IconRect.X, IconRect.Y, IconRect.Width-1, IconRect.Height-1)
        else AGraphics.DrawEllipse(Pen, IconRect.X, IconRect.Y, IconRect.Width-1, IconRect.Height-1);

        Path.AddLine(CenterX, CenterY + Radius * 0.1, CenterX, CenterY + Radius * 0.5);
        AGraphics.DrawPath(Pen, Path);

        Pen.GetColor(DotColor);
        DotBrush := TGPSolidBrush.Create(DotColor);
        AGraphics.FillEllipse(DotBrush, CenterX - 1, CenterY - Radius * 0.5, 2, 2);
      end;
      ntError:
      begin
        for i := 0 to 5 do
        begin
          Angle := (i * 60 - 90) * PI / 180;
          HexPoints[i] := MakePoint(CenterX + Radius * Cos(Angle), CenterY + Radius * Sin(Angle));
        end;
        IconShapePath.AddPolygon(PGPPointF(@HexPoints[0]), 6);
        if IsLightStyle then AGraphics.FillPath(Brush, IconShapePath) else AGraphics.DrawPath(Pen, IconShapePath);

        Path.AddLine(CenterX, CenterY - Radius * 0.4, CenterX, CenterY + Radius * 0.1);
        AGraphics.DrawPath(Pen, Path);

        Pen.GetColor(DotColor);
        DotBrush := TGPSolidBrush.Create(DotColor);
        AGraphics.FillEllipse(DotBrush, CenterX - 1, CenterY + Radius * 0.4, 2, 2);
      end;
    end;
  finally
    Path.Free;
    IconShapePath.Free;
    Pen.Free;
    Brush.Free;
    if Assigned(DotBrush) then DotBrush.Free;
  end;
end;

procedure THTL_NotificationForm.Paint;
var
  LG: TGPGraphics;
  LPath, LShadowPath: TGPGraphicsPath;
  LBrush, LShadowBrush: TGPBrush;
  LPen: TGPPen;
  LRectF, LShadowRectF: TGPRectF;
  LRect: TRect;
  LRadius: Single;
  IconRect, TitleRect, MessageRect, TextContentRect, ContentRect: TRect;
  ShadowColor: TColor;
  ShadowOpacity: Integer;
  HasCustomIcon, HasDefaultIcon: Boolean;
  TotalTextHeight, ContentTop: Integer;
begin
  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.Clear(ColorToARGB(clNone, 0));
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    LRect := ClientRect;
    InflateRect(LRect, -5, -5); // Mais espaço para a sombra suave
    LRectF := RectToGPRectF(LRect);
    LRadius := Min(FBorderSettings.CornerRadius, Min(LRectF.Width, LRectF.Height) / 2);
    if FBorderSettings.RoundCornerType = rctNone then LRadius := 0;

    // --- 1. Desenha a Sombra (mais suave) ---
    LShadowRectF := LRectF;
    LShadowRectF.X := LShadowRectF.X + 1;
    LShadowRectF.Y := LShadowRectF.Y + 3;
    ShadowColor := clBlack;
    ShadowOpacity := 15;

    LShadowPath := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(LShadowPath, LShadowRectF, LRadius, FBorderSettings.RoundCornerType);
      LShadowBrush := TGPSolidBrush.Create(ColorToARGB(ShadowColor, Round(ShadowOpacity * (Self.AlphaBlendValue / 255.0))));
      try
        LG.FillPath(LShadowBrush, LShadowPath);
      finally
        LShadowBrush.Free;
      end;
    finally
      LShadowPath.Free;
    end;

    // --- 2. Desenha o Corpo Principal ---
    LPath := TGPGraphicsPath.Create;
    try
      CreateGPRoundedPath(LPath, LRectF, LRadius, FBorderSettings.RoundCornerType);
      LBrush := TGPSolidBrush.Create(ColorToARGB(FBorderSettings.BackgroundColor, Self.AlphaBlendValue));
      try
        LG.FillPath(LBrush, LPath);
      finally
        LBrush.Free;
      end;
      if FBorderSettings.Thickness > 0 then
      begin
        LPen := TGPPen.Create(ColorToARGB(FBorderSettings.Color, Self.AlphaBlendValue), FBorderSettings.Thickness);
        try
          LG.DrawPath(LPen, LPath);
        finally
          LPen.Free;
        end;
      end;
    finally
      LPath.Free;
    end;

    // --- 3. Lógica de Layout e Desenho do Conteúdo ---
    HasCustomIcon := FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty;
    HasDefaultIcon := FImageSettings.Visible and not HasCustomIcon and (FNotificationType <> ntCustom);

    ContentRect := LRect;
    InflateRect(ContentRect, -FImageSettings.Margins.Left, 0);
    ContentRect.Right := ContentRect.Right - (FBtnClose.Width + 5);

    if HasCustomIcon or HasDefaultIcon then
    begin
      IconRect.Left := ContentRect.Left;
      IconRect.Width := FImageSettings.TargetWidth;
      TextContentRect.Left := IconRect.Right + FImageSettings.Margins.Right;
    end
    else
    begin
      IconRect := System.Types.Rect(0,0,0,0);
      TextContentRect.Left := ContentRect.Left;
    end;

    TextContentRect.Right := ContentRect.Right;
    Canvas.Font.Assign(FTitleSettings.Font);
    TotalTextHeight := Canvas.TextHeight(FTitle);
    Canvas.Font.Assign(FMessageSettings.Font);
    TotalTextHeight := TotalTextHeight + Canvas.TextHeight(FMessage) + 4; // 4 é o espaçamento

    ContentTop := LRect.Top + (LRect.Height - TotalTextHeight) div 2;
    TitleRect := System.Types.Rect(TextContentRect.Left, ContentTop, TextContentRect.Right, ContentTop + Canvas.TextHeight(FTitle));
    MessageRect := System.Types.Rect(TextContentRect.Left, TitleRect.Bottom + 4, TextContentRect.Right, TitleRect.Bottom + 4 + Canvas.TextHeight(FMessage));

    if HasCustomIcon or HasDefaultIcon then
    begin
      IconRect.Top := LRect.Top + (LRect.Height - FImageSettings.TargetHeight) div 2;
      IconRect.Height := FImageSettings.TargetHeight;
      if HasCustomIcon then
        DrawGraphicWithGDI(LG, FImageSettings.Picture.Graphic, IconRect, idmProportional)
      else
        DrawDefaultIcon(LG, IconRect, FTitleSettings.Font.Color, FBorderSettings.BackgroundColor);
    end;

    DrawComponentCaption(Self.Canvas, TitleRect, FTitle, FTitleSettings.Font, FTitleSettings.Font.Color, FTitleSettings.Alignment, cvaCenter, False, Self.AlphaBlendValue);
    DrawComponentCaption(Self.Canvas, MessageRect, FMessage, FMessageSettings.Font, FMessageSettings.Font.Color, FMessageSettings.Alignment, cvaCenter, True, Self.AlphaBlendValue);

  finally
    LG.Free;
  end;
end;

procedure THTL_NotificationForm.ApplyPreset;
var
  TitleFontColor, MessageFontColor,
  BorderColor, BackgroundColor, IconColor: TColor;
begin
  // Definindo paletas de cores para ambos os estilos
  case FVisualStyle of
    nvsColored:
      case FNotificationType of
        ntSuccess: begin BackgroundColor := TColor($00EAF4E6); IconColor := TColor($0053A834); MessageFontColor := TColor($0068635F); BorderColor := TColor($00D8EADF); end;
        ntWarning: begin BackgroundColor := TColor($00FEF7E8); IconColor := TColor($00F2994A); MessageFontColor := TColor($0068635F); BorderColor := TColor($00FCE9CC); end;
        ntInfo:    begin BackgroundColor := TColor($00EBF5FF); IconColor := TColor($002F80ED); MessageFontColor := TColor($0068635F); BorderColor := TColor($00D5E9FA); end;
        ntError:   begin BackgroundColor := TColor($00FDEEEE); IconColor := TColor($00EB5757); MessageFontColor := TColor($0068635F); BorderColor := TColor($00FAD8D8); end;
      else
        BackgroundColor := clWhite; IconColor := clBlack; MessageFontColor := clGray; BorderColor := clSilver;
      end;
    nvsLight:
      case FNotificationType of
        ntSuccess: begin BackgroundColor := clWhite; IconColor := TColor($0027AE60); MessageFontColor := TColor($004F4F4F); BorderColor := TColor($00E0E0E0); end;
        ntWarning: begin BackgroundColor := clWhite; IconColor := TColor($00F2994A); MessageFontColor := TColor($004F4F4F); BorderColor := TColor($00E0E0E0); end;
        ntInfo:    begin BackgroundColor := clWhite; IconColor := TColor($002F80ED); MessageFontColor := TColor($004F4F4F); BorderColor := TColor($00E0E0E0); end;
        ntError:   begin BackgroundColor := clWhite; IconColor := TColor($00EB5757); MessageFontColor := TColor($004F4F4F); BorderColor := TColor($00E0E0E0); end;
      else
        BackgroundColor := clWhite; IconColor := clBlack; MessageFontColor := clGray; BorderColor := clSilver;
      end;
  end;

  TitleFontColor := IconColor; // Título tem a mesma cor do ícone

  Self.Color := BackgroundColor;
  FBorderSettings.BackgroundColor := BackgroundColor;
  FBorderSettings.Color := BorderColor;
  FBorderSettings.Thickness := 1;
  FTitleSettings.Font.Color := TitleFontColor;
  FMessageSettings.Font.Color := MessageFontColor;
  FBtnClose.CaptionSettings.Font.Color := MessageFontColor; // Cor do 'X' mais sutil
  FBtnClose.HoverSettings.FontColor := TitleFontColor;
end;

procedure THTL_NotificationForm.SetupAppearance;
begin
  Self.Width := 400;
  Self.Height := 70; // Altura reduzida

  ApplyPreset;

  FTitleSettings.Text := FTitle;
  FMessageSettings.Text := FMessage;

  FBtnClose.Left := Self.Width - FBtnClose.Width - 12;
  FBtnClose.Top := (Self.Height - FBtnClose.Height) div 2;

  if FNotificationStyle = nsModal then
  begin
    if FParentForm <> nil then FParentForm.Enabled := False;
    FBtnClose.Visible := True;
    FCloseTimer.Enabled := False;
  end
  else
  begin
    FBtnClose.Visible := True;
    FCloseTimer.Interval := FDuration;
    FCloseTimer.Enabled := True;
  end;

  Self.AlphaBlend := True;
  Self.AlphaBlendValue := 0;
end;

procedure THTL_NotificationForm.PositionForm;
var
  WorkArea: TRect;
const
  MARGIN = 20;
begin
  if (FPosition = npCenterParent) and (FParentForm <> nil) then
    WorkArea := FParentForm.BoundsRect
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);

  case FPosition of
    npTopRight:      begin Left := WorkArea.Right - Width - MARGIN; Top := WorkArea.Top + MARGIN; end;
    npBottomRight:   begin Left := WorkArea.Right - Width - MARGIN; Top := WorkArea.Bottom - Height - MARGIN; end;
    npTopLeft:       begin Left := WorkArea.Left + MARGIN; Top := WorkArea.Top + MARGIN; end;
    npBottomLeft:    begin Left := WorkArea.Left + MARGIN; Top := WorkArea.Bottom - Height - MARGIN; end;
    npCenterScreen:  begin Left := (WorkArea.Left + WorkArea.Right - Width) div 2; Top := (WorkArea.Top + WorkArea.Bottom - Height) div 2; end;
    npCenterParent:  begin Left := WorkArea.Left + (WorkArea.Width - Width) div 2; Top := WorkArea.Top + (WorkArea.Height - Height) div 2; end;
  end;
end;

class procedure THTL_NotificationForm.ShowNotification(const ATitle, AMessage: string;
  AType: TNotificationType; AVisualStyle: TNotificationVisualStyle; AStyle: TNotificationStyle;
  APosition: TNotificationPosition; ADuration: Integer; AParent: TForm;
  ACustomIcon: TPicture);
var
  LForm: THTL_NotificationForm;
  i: Integer;
begin
  LForm := THTL_NotificationForm.Create(Application);
  try
    LForm.FTitle := ATitle;
    LForm.FMessage := AMessage;
    LForm.FNotificationType := AType;
    LForm.FVisualStyle := AVisualStyle;
    LForm.FNotificationStyle := AStyle;
    LForm.FPosition := APosition;
    LForm.FDuration := ADuration;
    LForm.FParentForm := AParent;

    if Assigned(ACustomIcon) then
      LForm.FImageSettings.Picture.Assign(ACustomIcon);

    LForm.SetupAppearance;
    LForm.PositionForm;

    LForm.Show;

    i := 0;
    while i <= 255 do
    begin
      LForm.AlphaBlendValue := i;
      Application.ProcessMessages;
      Sleep(5);
      Inc(i, 15);
    end;
    LForm.AlphaBlendValue := 255;

    if LForm.FNotificationStyle = nsModal then
    begin
      LForm.ShowModal;
      if (LForm.FParentForm <> nil) and Assigned(LForm.FParentForm) then
        LForm.FParentForm.Enabled := True;
    end;

  except
    on E: Exception do
    begin
      if (AParent <> nil) and Assigned(AParent) and (not AParent.Enabled) then
        AParent.Enabled := True;
      if Assigned(LForm) then
        LForm.Free;
      Raise;
    end;
  end;
end;

end.
