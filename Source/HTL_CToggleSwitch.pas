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

// --- Funções Auxiliares de Cor ---

function InterpolateColor(ColorA, ColorB: TColor; Factor: Single): TColor;
var
  R1, G1, B1, R2, G2, B2, R, G, B: Byte;
begin
  R1 := GetRValue(ColorToRGB(ColorA));
  G1 := GetGValue(ColorToRGB(ColorA));
  B1 := GetBValue(ColorToRGB(ColorA));
  R2 := GetRValue(ColorToRGB(ColorB));
  G2 := GetGValue(ColorToRGB(ColorB));
  B2 := GetBValue(ColorToRGB(ColorB));
  R := Round(R1 + (R2 - R1) * Factor);
  G := Round(G1 + (G2 - G1) * Factor);
  B := Round(B1 + (B2 - B1) * Factor);
  Result := RGB(R, G, B);
end;

function DarkenColor(AColor: TColor; AAmount: Integer): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(ColorToRGB(AColor));
  G := GetGValue(ColorToRGB(AColor));
  B := GetBValue(ColorToRGB(AColor));
  if R > AAmount then R := R - AAmount else R := 0;
  if G > AAmount then G := G - AAmount else G := 0;
  if B > AAmount then B := B - AAmount else B := 0;
  Result := RGB(R, G, B);
end;

function LightenColor(AColor: TColor; AAmount: Integer): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(ColorToRGB(AColor));
  G := GetGValue(ColorToRGB(AColor));
  B := GetBValue(ColorToRGB(AColor));
  if R < (255 - AAmount) then R := R + AAmount else R := 255;
  if G < (255 - AAmount) then G := G + AAmount else G := 255;
  if B < (255 - AAmount) then B := B + AAmount else B := 255;
  Result := RGB(R, G, B);
end;


procedure Register;
begin
  RegisterComponents('HTL', [THTL_CToggleSwitch]);
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
var
  LBackgroundColor: TColor;
  LTrackRect: TRect;
  LThumbRect: TRect;
  LRadius: Integer;
  LPadding: Integer;
  LThumbSize: Integer;
  LThumbX: Integer;
begin
  inherited;

  // --- Calcula dimensões ---
  LRadius := Height div 2;
  LPadding := 2;
  LThumbSize := Height - (LPadding * 2);
  LTrackRect := Rect(0, 0, Width, Height);

  // --- Define cores com base no estado e animação ---
  LBackgroundColor := InterpolateColor(FOffColor, FOnColor, FAnimationPosition);

  // Cor quando desabilitado
  if not Enabled then
  begin
    LBackgroundColor := clBtnFace;
  end;

  // --- Desenha a trilha (track) ---
  Canvas.Pen.Color := FBorderColor;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Color := LBackgroundColor;

  if not Enabled then
     Canvas.Pen.Color := clGray;

  Canvas.RoundRect(LTrackRect.Left, LTrackRect.Top, LTrackRect.Right, LTrackRect.Bottom, LRadius, LRadius);

  // --- Calcula a posição do botão (thumb) ---
  LThumbX := LPadding + Round((Width - (LPadding * 2) - LThumbSize) * FAnimationPosition);
  LThumbRect := Rect(LThumbX, LPadding, LThumbX + LThumbSize, LPadding + LThumbSize);

  // --- Desenha o botão (thumb) ---
  Canvas.Pen.Color := DarkenColor(FBorderColor, 20); // Borda sutil para o botão
  Canvas.Brush.Color := FThumbColor;

  if not Enabled then
  begin
    Canvas.Pen.Color := DarkenColor(clBtnFace, 20);
    Canvas.Brush.Color := LightenColor(clBtnFace, 20);
  end;

  Canvas.Ellipse(LThumbRect);
end;

end.
