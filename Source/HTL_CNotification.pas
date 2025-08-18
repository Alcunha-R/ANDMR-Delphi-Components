unit HTL_CNotification;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.pngimage, Vcl.GraphUtil,
  System.UITypes,
  // --- Dependências Essenciais ---
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Winapi.ActiveX,
  HTL_ComponentUtils;

type
  // Define o propósito/tema da notificação (cor, ícone padrão, etc.)
  TNotificationType = (ntSuccess, ntWarning, ntInfo, ntError, ntCustom);

  // Define o estilo visual principal, baseado na imagem de exemplo
  TNotificationStyle = (nsFaded, nsSolidWithShadow);

  THTL_CNotification = class(TCustomControl)
  private
    // --- Campos de Propriedades ---
    FTitle: string;
    FMessage: string;
    FTitleFont: TFont;
    FMessageFont: TFont;
    FNotificationType: TNotificationType;
    FNotificationStyle: TNotificationStyle;
    FShowCloseButton: Boolean;
    FIcon: TImageSettings;
    FBorderSettings: TBorderSettings;
    FGradientSettings: TGradientSettings;
    FShadowSettings: TDropShadowSettings;

    // --- Campos de Eventos ---
    FOnCloseClick: TNotifyEvent;

    // --- Campos de Estado Interno ---
    FCloseButtonRect: TRect;
    FIsCloseButtonHot: Boolean;
    FIsHovering: Boolean;

    // --- Setters ---
    procedure SetTitle(const Value: string);
    procedure SetMessage(const Value: string);
    procedure SetTitleFont(const Value: TFont);
    procedure SetMessageFont(const Value: TFont);
    procedure SetNotificationType(const Value: TNotificationType);
    procedure SetNotificationStyle(const Value: TNotificationStyle);
    procedure SetShowCloseButton(const Value: Boolean);
    procedure SetIcon(const Value: TImageSettings);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetShadowSettings(const Value: TDropShadowSettings);
    procedure FontChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);

    // --- Handlers de Mensagens e Eventos ---
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  protected
    // --- Métodos Protegidos e Overrides ---
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;

    // --- Métodos de Desenho ---
    procedure CalculateLayout(out AIconRect, ATitleRect, AMessageRect, ACloseButtonRect, ATextContainerRect: TRect);
    procedure DrawShadow(AGraphics: TGPGraphics; var AClientRectF: TGPRectF);
    procedure DrawBackgroundAndBorder(AGraphics: TGPGraphics; const APathRect: TGPRectF);
    procedure DrawContent(AGraphics: TGPGraphics; const AIconRect, ATitleRect, AMessageRect, ACloseButtonRect: TRect);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // --- Propriedades Publicadas (visíveis no Object Inspector) ---
    property Title: string read FTitle write SetTitle;
    property Message: string read FMessage write SetMessage;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property MessageFont: TFont read FMessageFont write SetMessageFont;

    property NotificationType: TNotificationType read FNotificationType write SetNotificationType default ntCustom;
    property NotificationStyle: TNotificationStyle read FNotificationStyle write SetNotificationStyle default nsSolidWithShadow;

    property ShowCloseButton: Boolean read FShowCloseButton write SetShowCloseButton default True;

    // --- Propriedades de Configuração (reutilizadas de HTL_ComponentUtils) ---
    property Icon: TImageSettings read FIcon write SetIcon;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property ShadowSettings: TDropShadowSettings read FShadowSettings write SetShadowSettings;

    // --- Eventos ---
    property OnClick;
    property OnCloseClick: TNotifyEvent read FOnCloseClick write FOnCloseClick;

    // --- Propriedades Padrão ---
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property TabOrder;
    property TabStop;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CNotification]);
end;

{ THTL_CNotification }

constructor THTL_CNotification.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // --- Configuração Inicial do Controle ---
  ControlStyle := [csReplicatable, csClickEvents, csCaptureMouse, csDoubleClicks, csParentBackground];
  Width := 300;
  Height := 70;
  DoubleBuffered := True;
  FIsCloseButtonHot := False;
  FIsHovering := False;

  // --- Instanciação das Propriedades de Configuração ---
  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 12;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;

  FShadowSettings := TDropShadowSettings.Create;
  FShadowSettings.OnChange := SettingsChanged;
  FShadowSettings.Enabled := True;
  FShadowSettings.Color := TColor($99000000); // Preto com Alpha
  FShadowSettings.BlurRadius := 8;
  FShadowSettings.Offset := Point(0, 4);

  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;

  FIcon := TImageSettings.Create(Self);
  FIcon.OnChange := SettingsChanged;
  FIcon.ImagePosition := ipLeft;
  FIcon.Placement := iplInsideBounds;
  FIcon.AutoSize := True;
  FIcon.DrawMode := idmProportional;
  FIcon.TargetWidth := 24;
  FIcon.TargetHeight := 24;
  FIcon.Margins.Left := 15;
  FIcon.Margins.Right := 10;
  FIcon.Margins.Top := 10;
  FIcon.Margins.Bottom := 10;
  FIcon.VerticalAlign := ivaCenter;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Segoe UI';
  FTitleFont.Size := 10;
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := FontChanged;

  FMessageFont := TFont.Create;
  FMessageFont.Name := 'Segoe UI';
  FMessageFont.Size := 9;
  FMessageFont.OnChange := FontChanged;

  FShowCloseButton := True;

  // --- Configuração Padrão ---
  SetNotificationType(ntSuccess); // Inicia com um tipo para visualização
  SetNotificationStyle(nsSolidWithShadow);
end;

destructor THTL_CNotification.Destroy;
begin
  FBorderSettings.Free;
  FShadowSettings.Free;
  FGradientSettings.Free;
  FIcon.Free;
  FTitleFont.Free;
  FMessageFont.Free;
  inherited;
end;

procedure THTL_CNotification.Loaded;
begin
  inherited Loaded;
  SetNotificationType(FNotificationType); // Reaplica o tema quando o formulário carrega
  Repaint;
end;

procedure THTL_CNotification.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure THTL_CNotification.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

// --- SETTERS ---

procedure THTL_CNotification.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
end;

procedure THTL_CNotification.SetGradientSettings(const Value: TGradientSettings);
begin
  FGradientSettings.Assign(Value);
end;

procedure THTL_CNotification.SetIcon(const Value: TImageSettings);
begin
  FIcon.Assign(Value);
end;

procedure THTL_CNotification.SetMessage(const Value: string);
begin
  if FMessage <> Value then
  begin
    FMessage := Value;
    Invalidate;
  end;
end;

procedure THTL_CNotification.SetMessageFont(const Value: TFont);
begin
  FMessageFont.Assign(Value);
end;

procedure THTL_CNotification.SetNotificationStyle(const Value: TNotificationStyle);
begin
  if FNotificationStyle <> Value then
  begin
    FNotificationStyle := Value;
    FShadowSettings.Enabled := (Value = nsSolidWithShadow);
    FGradientSettings.Enabled := (Value = nsFaded);
    Invalidate;
  end;
end;

procedure THTL_CNotification.SetNotificationType(const Value: TNotificationType);
var
  BaseColor: TColor;
begin
  if (FNotificationType <> Value) or not (csLoading in ComponentState) then
  begin
    FNotificationType := Value;

    case FNotificationType of
      ntSuccess: begin
        BaseColor := TColor($FFDCF2DA); // Fundo verde claro
        FBorderSettings.Color := TColor($FFB7E1B1);
        FTitleFont.Color := TColor($FF285B22);
        FMessageFont.Color := TColor($FF437C3D);
        if Title = '' then Title := 'Payment processed';
      end;
      ntWarning: begin
        BaseColor := TColor($FFFFF4D4); // Fundo amarelo claro
        FBorderSettings.Color := TColor($FFFFE6A6);
        FTitleFont.Color := TColor($FF8C6400);
        FMessageFont.Color := TColor($FF997424);
        if Title = '' then Title := 'Connection failed';
      end;
      ntInfo: begin
        BaseColor := TColor($FFDCEBFF); // Fundo azul claro
        FBorderSettings.Color := TColor($FFBCD6FF);
        FTitleFont.Color := TColor($FF0043A2);
        FMessageFont.Color := TColor($FF265DAA);
        if Title = '' then Title := 'Update available';
      end;
      ntError: begin
        BaseColor := TColor($FFFFE2E2); // Fundo vermelho claro
        FBorderSettings.Color := TColor($FFFFC7C7);
        FTitleFont.Color := TColor($FF931F1F);
        FMessageFont.Color := TColor($FFB03737);
        if Title = '' then Title := 'Something went wrong';
      end;
      ntCustom: begin
        // Não faz nada, permite configuração manual completa
        Invalidate;
        Exit;
      end;
    end;

    FBorderSettings.BackgroundColor := BaseColor;
    FGradientSettings.StartColor := LighterColor(BaseColor, 10);
    FGradientSettings.EndColor := DarkerColor(BaseColor, 5);

    Invalidate;
  end;
end;

procedure THTL_CNotification.SetShadowSettings(const Value: TDropShadowSettings);
begin
  FShadowSettings.Assign(Value);
end;

procedure THTL_CNotification.SetShowCloseButton(const Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Invalidate;
  end;
end;

procedure THTL_CNotification.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Invalidate;
  end;
end;

procedure THTL_CNotification.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;


// --- EVENTOS DE MOUSE ---

procedure THTL_CNotification.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FIsHovering := True;
end;

procedure THTL_CNotification.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FIsHovering := False;
  if FIsCloseButtonHot then
  begin
    FIsCloseButtonHot := False;
    Invalidate;
    Cursor := crDefault;
  end;
end;

procedure THTL_CNotification.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and FShowCloseButton and PtInRect(FCloseButtonRect, Point(X, Y)) then
  begin
    if Assigned(FOnCloseClick) then
      FOnCloseClick(Self)
    else
      Self.Visible := False; // Comportamento padrão: esconder
  end;
end;

procedure THTL_CNotification.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  IsHot: Boolean;
begin
  inherited;
  if FShowCloseButton then
  begin
    IsHot := PtInRect(FCloseButtonRect, Point(X, Y));
    if IsHot <> FIsCloseButtonHot then
    begin
      FIsCloseButtonHot := IsHot;
      if FIsCloseButtonHot then Cursor := crHandPoint else Cursor := crDefault;
      Invalidate;
    end;
  end;
end;


// --- MÉTODOS DE DESENHO ---

procedure THTL_CNotification.CalculateLayout(out AIconRect, ATitleRect,
  AMessageRect, ACloseButtonRect, ATextContainerRect: TRect);
var
  ContentArea: TRect;
  IconSpace: Integer;
  CloseButtonSpace: Integer;
  TitleHeight, MessageHeight: Integer;
begin
  ContentArea := Self.ClientRect;
  InflateRect(ContentArea, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  // 1. Calcula espaço do Ícone
  AIconRect := Rect(0,0,0,0);
  IconSpace := 0;
  if FIcon.Visible and (FIcon.Picture.Graphic <> nil) and not FIcon.Picture.Graphic.Empty then
  begin
     IconSpace := FIcon.Margins.Left + FIcon.TargetWidth + FIcon.Margins.Right;
     AIconRect := Rect(
       ContentArea.Left + FIcon.Margins.Left,
       ContentArea.Top + (ContentArea.Height - FIcon.TargetHeight) div 2,
       ContentArea.Left + FIcon.Margins.Left + FIcon.TargetWidth,
       ContentArea.Top + (ContentArea.Height - FIcon.TargetHeight) div 2 + FIcon.TargetHeight
     );
  end;

  // 2. Calcula espaço do Botão Fechar
  ACloseButtonRect := Rect(0,0,0,0);
  CloseButtonSpace := 0;
  if FShowCloseButton then
  begin
    CloseButtonSpace := 15 + 15; // 15px de largura para o 'x' + 15px de margem direita
    ACloseButtonRect := Rect(
      ContentArea.Right - CloseButtonSpace,
      ContentArea.Top,
      ContentArea.Right - 15,
      ContentArea.Bottom
    );
  end;

  // 3. Calcula área do container de texto
  ATextContainerRect := Rect(
    ContentArea.Left + IconSpace,
    ContentArea.Top + 10,
    ContentArea.Right - CloseButtonSpace,
    ContentArea.Bottom - 10
  );

  // 4. Calcula área do Título e da Mensagem
  Canvas.Font.Assign(FTitleFont);
  TitleHeight := Canvas.TextHeight('Wg');
  Canvas.Font.Assign(FMessageFont);
  MessageHeight := Canvas.TextHeight('Wg');

  ATitleRect := Rect(
    ATextContainerRect.Left,
    ATextContainerRect.Top + (ATextContainerRect.Height - TitleHeight - MessageHeight - 2) div 2,
    ATextContainerRect.Right,
    0
  );
  ATitleRect.Bottom := ATitleRect.Top + TitleHeight;

  AMessageRect := Rect(
    ATextContainerRect.Left,
    ATitleRect.Bottom + 2,
    ATextContainerRect.Right,
    0
  );
  AMessageRect.Bottom := AMessageRect.Top + MessageHeight;

  // Global para uso em eventos de mouse
  FCloseButtonRect := ACloseButtonRect;
end;


// Substitua o procedimento inteiro em HTL_CNotification.pas

procedure THTL_CNotification.DrawShadow(AGraphics: TGPGraphics; var AClientRectF: TGPRectF);
var
  LShadowPath: TGPGraphicsPath;
  LShadowBrush: TGPBrush;
  LShadowRect: TGPRectF;
  LRadius: Single;
begin
  if not FShadowSettings.Enabled then Exit;

  // Ajusta o retângulo do cliente para dar espaço à sombra
  var OffsetX := Abs(FShadowSettings.Offset.X);
  var OffsetY := Abs(FShadowSettings.Offset.Y);
  var Blur := FShadowSettings.BlurRadius;
  AClientRectF.X := AClientRectF.X + Blur;
  AClientRectF.Y := AClientRectF.Y + Blur;
  AClientRectF.Width := Max(0, AClientRectF.Width - (Blur * 2));
  AClientRectF.Height := Max(0, AClientRectF.Height - (Blur * 2));

  // Retângulo da sombra
  LShadowRect := MakeRect(
    AClientRectF.X + FShadowSettings.Offset.X,
    AClientRectF.Y + FShadowSettings.Offset.Y,
    AClientRectF.Width,
    AClientRectF.Height
  );

  LRadius := Min(FBorderSettings.CornerRadius, Min(LShadowRect.Width, LShadowRect.Height) / 2.0);
  LShadowPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LShadowPath, LShadowRect, LRadius, FBorderSettings.RoundCornerType);
    if LShadowPath.GetPointCount > 0 then
    begin
      // --- LINHA CORRIGIDA ---
      // A chamada agora é mais simples e usa a nova função 'ColorToARGB' aprimorada.
      LShadowBrush := TGPSolidBrush.Create(ColorToARGB(FShadowSettings.Color));
      try
        // Simulação de "Blur" preenchendo o caminho.
        // GDI+ não tem um blur nativo simples. Esta é uma aproximação.
        AGraphics.FillPath(LShadowBrush, LShadowPath);
      finally
        LShadowBrush.Free;
      end;
    end;
  finally
    LShadowPath.Free;
  end;
end;

procedure THTL_CNotification.DrawBackgroundAndBorder(AGraphics: TGPGraphics; const APathRect: TGPRectF);
var
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LRadius: Single;
begin
  LRadius := Min(FBorderSettings.CornerRadius, Min(APathRect.Width, APathRect.Height) / 2.0);
  LPath := TGPGraphicsPath.Create;
  try
    CreateGPRoundedPath(LPath, APathRect, LRadius, FBorderSettings.RoundCornerType);

    if LPath.GetPointCount > 0 then
    begin
      // --- Fundo ---
      if FGradientSettings.Enabled and (FNotificationStyle = nsFaded) then
      begin
        LBrush := TGPLinearGradientBrush.Create(APathRect, ColorToARGB(FGradientSettings.StartColor), ColorToARGB(FGradientSettings.EndColor), LinearGradientModeVertical);
      end else
      begin
        LBrush := TGPSolidBrush.Create(ColorToARGB(FBorderSettings.BackgroundColor));
      end;

      try
        AGraphics.FillPath(LBrush, LPath);
      finally
        LBrush.Free;
      end;

      // --- Borda ---
      if (FBorderSettings.Thickness > 0) and (FBorderSettings.Style <> psClear) then
      begin
        LPen := TGPPen.Create(ColorToARGB(FBorderSettings.Color), FBorderSettings.Thickness);
        try
          AGraphics.DrawPath(LPen, LPath);
        finally
          LPen.Free;
        end;
      end;
    end;
  finally
    LPath.Free;
  end;
end;

procedure THTL_CNotification.DrawContent(AGraphics: TGPGraphics;
  const AIconRect, ATitleRect, AMessageRect, ACloseButtonRect: TRect);
var
  LGPen: TGPPen;
  HotCloseColor: TColor;
begin
  // 1. Desenha o Ícone
  if FIcon.Visible and (FIcon.Picture.Graphic <> nil) and not FIcon.Picture.Graphic.Empty then
  begin
    if FIcon.Picture.Graphic is TPNGImage then
      DrawPNGImageWithGDI(AGraphics, FIcon.Picture.Graphic as TPNGImage, AIconRect, FIcon.DrawMode)
    else
      DrawNonPNGImageWithCanvas(Self.Canvas, FIcon.Picture.Graphic, AIconRect, FIcon.DrawMode);
  end;

  // 2. Desenha os Textos
  if (Title <> '') then
    DrawComponentCaption(Self.Canvas, ATitleRect, Title, FTitleFont, FTitleFont.Color, taLeftJustify, cvaCenter, False, 255);

  if (Message <> '') then
    DrawComponentCaption(Self.Canvas, AMessageRect, Message, FMessageFont, FMessageFont.Color, taLeftJustify, cvaCenter, False, 255);

  // 3. Desenha o Botão Fechar
  if FShowCloseButton then
  begin
    if FIsCloseButtonHot then
      HotCloseColor := DarkerColor(FMessageFont.Color, 40)
    else
      HotCloseColor := FMessageFont.Color;

    LGPen := TGPPen.Create(ColorToARGB(HotCloseColor), 1.5);
    try
      LGPen.SetStartCap(LineCapRound);
      LGPen.SetEndCap(LineCapRound);
      var x1 := ACloseButtonRect.Left;
      var y1 := ACloseButtonRect.Top + (ACloseButtonRect.Height - 10) div 2;
      var x2 := x1 + 10;
      var y2 := y1 + 10;
      AGraphics.DrawLine(LGPen, x1, y1, x2, y2);
      AGraphics.DrawLine(LGPen, x1, y2, x2, y1);
    finally
      LGPen.Free;
    end;
  end;
end;


procedure THTL_CNotification.Paint;
var
  Graphics: TGPGraphics;
  ClientRectF: TGPRectF;
  PathRectF: TGPRectF;
  PathInset: Single;
  IconRect, TitleRect, MessageRect, CloseButtonRect, TextContainerRect: TRect;
begin
  inherited Paint;
  Graphics := TGPGraphics.Create(Canvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);

    ClientRectF := RectToGPRectF(Self.ClientRect);

    // 1. Desenha a Sombra (e ajusta ClientRectF se necessário)
    if FNotificationStyle = nsSolidWithShadow then
    begin
      DrawShadow(Graphics, ClientRectF);
    end;

    // 2. Prepara o caminho principal
    PathInset := FBorderSettings.Thickness / 2.0;
    PathRectF := MakeRect(
      ClientRectF.X + PathInset,
      ClientRectF.Y + PathInset,
      Max(0, ClientRectF.Width - FBorderSettings.Thickness),
      Max(0, ClientRectF.Height - FBorderSettings.Thickness)
    );

    // 3. Desenha Fundo e Borda
    DrawBackgroundAndBorder(Graphics, PathRectF);

    // 4. Calcula o Layout do Conteúdo
    CalculateLayout(IconRect, TitleRect, MessageRect, CloseButtonRect, TextContainerRect);

    // 5. Desenha o Conteúdo
    DrawContent(Graphics, IconRect, TitleRect, MessageRect, CloseButtonRect);

  finally
    Graphics.Free;
  end;
end;

end.
