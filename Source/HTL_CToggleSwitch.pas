unit HTL_CToggleSwitch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Winapi.Windows, // Para GetRValue, etc.
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls;

type
  THTL_CToggleSwitch = class(TCustomControl)
  private
    // --- Campos Internos ---
    FChecked: Boolean;
    FOnColor: TColor;
    FOffColor: TColor;
    FThumbColor: TColor;
    FBorderColor: TColor;
    FOnChange: TNotifyEvent;

    // Campos para animação
    FAnimationTimer: TTimer;
    FAnimationPosition: Single; // 0.0 (desligado) para 1.0 (ligado)
    FTargetPosition: Single;

    // --- Métodos Setters ---
    procedure SetChecked(const Value: Boolean);
    procedure SetOnColor(const Value: TColor);
    procedure SetOffColor(const Value: TColor);
    procedure SetThumbColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);

    // --- Métodos Internos ---
    procedure Animate(Sender: TObject);
    procedure StartAnimation;

  protected
    // --- Métodos Protegidos (Overrides) ---
    procedure Paint; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Resize; override;

  public
    // --- Construtor ---
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // --- Propriedades Publicadas (visíveis no Object Inspector) ---
    property Align;
    property Anchors;
    property Checked: Boolean read FChecked write SetChecked default False;
    property OnColor: TColor read FOnColor write SetOnColor default clGreen;
    property OffColor: TColor read FOffColor write SetOffColor default clBtnFace;
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clWhite;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
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
  Winapi.GDIPUTIL,
  HTL_ComponentUtils;

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CToggleSwitch]);
end;

{ THTL_CToggleSwitch }

constructor THTL_CToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // --- Inicialização das propriedades padrão ---
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csPannable, csClickEvents];
  Width := 80;
  Height := 34;
  DoubleBuffered := True; // Essencial para animações sem piscar

  FChecked := False;
  FOnColor := TColor($FF4CD964); // Verde iOS
  FOffColor := TColor($FFE5E5EA); // Cinza claro iOS
  FThumbColor := clWhite;
  FBorderColor := TColor($FFD1D1D6);

  // --- Configuração do Timer de Animação ---
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 10; // Intervalo curto para animação fluida
  FAnimationTimer.OnTimer := Animate;

  // --- Posição inicial da animação ---
  FAnimationPosition := 0.0;
  FTargetPosition := 0.0;
end;

destructor THTL_CToggleSwitch.Destroy;
begin
  FAnimationTimer.Free;
  inherited;
end;

procedure THTL_CToggleSwitch.Loaded;
begin
  inherited;
  // Garante que a posição inicial esteja correta ao carregar o form
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
  // Garante que o componente seja redesenhado se o tamanho mudar
  Invalidate;
end;

procedure THTL_CToggleSwitch.Click;
begin
  inherited;
  if Enabled then
  begin
    SetChecked(not FChecked); // Inverte o estado
  end;
end;

procedure THTL_CToggleSwitch.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    StartAnimation; // Inicia a animação

    // Dispara o evento OnChange, se atribuído
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
  end;
end;

procedure THTL_CToggleSwitch.SetOnColor(const Value: TColor);
begin
  if FOnColor <> Value then
  begin
    FOnColor := Value;
    Invalidate;
  end;
end;

procedure THTL_CToggleSwitch.SetOffColor(const Value: TColor);
begin
  if FOffColor <> Value then
  begin
    FOffColor := Value;
    Invalidate;
  end;
end;

procedure THTL_CToggleSwitch.SetThumbColor(const Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Invalidate;
  end;
end;

procedure THTL_CToggleSwitch.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure THTL_CToggleSwitch.StartAnimation;
begin
  if FChecked then
    FTargetPosition := 1.0 // Alvo é a posição "ligado"
  else
    FTargetPosition := 0.0; // Alvo é a posição "desligado"

  FAnimationTimer.Enabled := True; // Ativa o timer para começar a animação
end;

procedure THTL_CToggleSwitch.Animate(Sender: TObject);
const
  AnimationStep = 0.1; // Velocidade da animação (maior = mais rápido)
begin
  // Move a posição atual em direção à posição alvo
  if FAnimationPosition < FTargetPosition then
  begin
    FAnimationPosition := FAnimationPosition + AnimationStep;
    if FAnimationPosition >= FTargetPosition then
    begin
      FAnimationPosition := FTargetPosition;
      FAnimationTimer.Enabled := False; // Para o timer
    end;
  end
  else if FAnimationPosition > FTargetPosition then
  begin
    FAnimationPosition := FAnimationPosition - AnimationStep;
    if FAnimationPosition <= FTargetPosition then
    begin
      FAnimationPosition := FTargetPosition;
      FAnimationTimer.Enabled := False; // Para o timer
    end;
  end
  else
  begin
    FAnimationTimer.Enabled := False; // Já está no alvo, para o timer
  end;

  Invalidate; // Redesenha o controle a cada passo da animação
end;

procedure THTL_CToggleSwitch.Paint;
const
  BorderWidth = 1.0;
var
  LG: TGPGraphics;
  LBackgroundColor, CurrentBorderColor, CurrentThumbColor, CurrentThumbBorderColor: TColor;
  LTrackRectF, LThumbRectF: TGPRectF;
  LRadius, LThumbX: Single;
  LPadding, LThumbSize: Integer;
  LPath: TGPGraphicsPath;
  LBrush: TGPBrush;
  LPen: TGPPen;
begin
  inherited;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    // Ativa o Anti-aliasing para um desenho suave
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    // CORREÇÃO: Ajusta o alinhamento de pixel para evitar cortes
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // --- Calcula dimensões ---
    LPadding := 2;
    LThumbSize := Height - (LPadding * 2);

    // Inseta o retângulo de desenho pela metade da espessura da borda
    LTrackRectF.X := BorderWidth / 2.0;
    LTrackRectF.Y := BorderWidth / 2.0;
    LTrackRectF.Width := Max(0, Self.Width - BorderWidth);
    LTrackRectF.Height := Max(0, Self.Height - BorderWidth);

    // CORREÇÃO: O raio deve ser baseado na altura do retângulo de desenho
    LRadius := LTrackRectF.Height / 2.0;


    // --- Define cores com base no estado e animação ---
    LBackgroundColor := BlendColors(FOffColor, FOnColor, FAnimationPosition);
    CurrentBorderColor := FBorderColor;
    CurrentThumbColor := FThumbColor;
    CurrentThumbBorderColor := DarkerColor(FBorderColor, 20);

    if not Enabled then
    begin
      LBackgroundColor := clBtnFace;
      CurrentBorderColor := clGray;
      CurrentThumbColor := LighterColor(clBtnFace, 20);
      CurrentThumbBorderColor := DarkerColor(clBtnFace, 20);
    end;

    // --- Desenha a trilha (track) com GDI+ ---
    LPath := TGPGraphicsPath.Create;
    try
      // Cria um caminho de retângulo arredondado para a trilha
      CreateGPRoundedPath(LPath, LTrackRectF, LRadius, rctAll);

      // Preenche a trilha
      LBrush := TGPSolidBrush.Create(ColorToARGB(LBackgroundColor));
      try
        LG.FillPath(LBrush, LPath);
      finally
        LBrush.Free;
      end;

      // Desenha a borda da trilha
      LPen := TGPPen.Create(ColorToARGB(CurrentBorderColor), BorderWidth);
      try
        LG.DrawPath(LPen, LPath);
      finally
        LPen.Free;
      end;
    finally
      LPath.Free;
    end;

    // --- Calcula a posição do botão (thumb) ---
    LThumbX := LPadding + ((Width - (LPadding * 2) - LThumbSize) * FAnimationPosition);

    LThumbRectF.X := LThumbX;
    LThumbRectF.Y := LPadding;
    LThumbRectF.Width := LThumbSize;
    LThumbRectF.Height := LThumbSize;

    // --- Desenha o botão (thumb) com GDI+ ---
    // Preenchimento do botão
    LBrush := TGPSolidBrush.Create(ColorToARGB(CurrentThumbColor));
    try
      LG.FillEllipse(LBrush, LThumbRectF);
    finally
      LBrush.Free;
    end;

    // Borda do botão
    LPen := TGPPen.Create(ColorToARGB(CurrentThumbBorderColor), BorderWidth);
    try
      LG.DrawEllipse(LPen, LThumbRectF);
    finally
      LPen.Free;
    end;

  finally
    LG.Free;
  end;
end;

end.
