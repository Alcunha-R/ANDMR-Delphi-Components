unit HTL_CMemo;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.Types, System.Character,
  HTL_ComponentUtils;

type
  // Este tipo foi copiado do CEdit. O ideal seria movê-lo para a unit HTL_ComponentUtils
  // para ser compartilhado entre os componentes.
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);

  // --- Estrutura para centralizar o cálculo do layout ---
  TMemoLayout = record
    FullControlRect: TRect;     // O ClientRect completo
    OutsideCaptionRect: TRect;  // Retângulo para a legenda (se externa)
    OutsideImageRect: TRect;    // Retângulo para a imagem (se externa)
    EditBoxRect: TRect;         // A caixa principal (borda, fundo)
    ContentRect: TRect;         // Área dentro da borda
    InsideImageRect: TRect;     // Retângulo para a imagem (se interna)
    InsideCaptionRect: TRect;   // Retângulo para a legenda (se interna)
    SeparatorRect: TRect;       // Retângulo para o separador
    FinalImageDestRect: TRect;  // Retângulo final de desenho da imagem (com alinhamento)
    MemoRect: TRect;            // O retângulo final para o TMemo
  end;

type
  THTL_CMemo = class(TCustomControl)
  private
    // --- Fields from CEdit ---
    FBorderSettings: TBorderSettings;
    FFocusSettings: TFocusSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FTextMargins: THTL_Margins;
    FGradientSettings: TGradientSettings;
    FClickSettings: TClickSettings;
    FProgressSettings: TProgressSettings;
    FImageSettings: TImageSettings;
    FSeparatorSettings: TSeparatorSettings;
    FStyle: TButtonStyle;
    FStatus: TCEditStatus;

    // --- Fields for effects from CEdit ---
    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectActive: Boolean;
    FProcessing: Boolean;
    FProgressTimer: TTimer;
    FProgressStep: Integer;
    FOriginalEnabledState: Boolean;

    // --- Original CMemo fields ---
    FHovered: Boolean;
    FOpacity: Byte;
    FInternalMemo: TMemo;

    // --- Events ---
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FTabStop: Boolean;

    // --- Accessors for Memo properties ---
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const Value: Boolean);
    function GetScrollBars: TScrollStyle;
    procedure SetScrollBars(const Value: TScrollStyle);
    function GetMaxLength: Integer;
    procedure SetMaxLength(const Value: Integer);
    procedure SetTabStop(Value: Boolean);

    // --- Setters for new/updated properties ---
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetFocusSettings(const Value: TFocusSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetTextMargins(const Value: THTL_Margins);
    procedure SetGradientSettings(const Value: TGradientSettings);
    procedure SetClickSettings(const Value: TClickSettings);
    procedure SetProgressSettings(const Value: TProgressSettings);
    procedure SetImageSettings(const Value: TImageSettings);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetStatus(const Value: TCEditStatus);
    procedure SetOpacity(const Value: Byte);

    // --- Effect handlers from CEdit ---
    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;
    procedure ProgressTimerHandler(Sender: TObject);

    // --- Change handlers ---
    procedure SettingsChanged(Sender: TObject);

    // --- Internal Memo event handlers ---
    procedure InternalMemoChange(Sender: TObject);
    procedure InternalMemoEnter(Sender: TObject);
    procedure InternalMemoExit(Sender: TObject);
    procedure InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure InternalMemoKeyPress(Sender: TObject; var Key: Char);
    procedure InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    // --- Message handlers from CEdit ---
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

  protected
    function CalculateLayout: TMemoLayout;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure UpdateInternalMemoBounds;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartProcessing;
    procedure StopProcessing;
    procedure SetFocus; override;

  published
    // --- Standard Memo properties ---
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default True;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssVertical;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;

    // --- Settings properties from CEdit ---
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property FocusSettings: TFocusSettings read FFocusSettings write SetFocusSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: THTL_Margins read FTextMargins write SetTextMargins;
    property Status: TCEditStatus read FStatus write SetStatus default cepsNormal;
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property ClickSettings: TClickSettings read FClickSettings write SetClickSettings;
    property GradientSettings: TGradientSettings read FGradientSettings write SetGradientSettings;
    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;
    property Opacity: Byte read FOpacity write SetOpacity default 255;

    // --- Standard control properties ---
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop: Boolean read FTabStop write SetTabStop default True;
    property Visible;

    // --- Events ---
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CMemo]);
end;

{ THTL_CMemo }

constructor THTL_CMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // --- Estilo e tamanho padrão ---
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csAcceptsControls];
  DoubleBuffered := True;
  Width := 185;
  Height := 80;
  TabStop := True;
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  Font.Color := clWindowText;

  // --- Inicialização das classes de settings (padrão CEdit) ---
  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 4;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.Color := clSilver;
  FBorderSettings.Thickness := 1;
  FBorderSettings.BackgroundColor := clWindow;

  FFocusSettings := TFocusSettings.Create;
  FFocusSettings.OnChange := SettingsChanged;
  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Font.Style := [fsBold];
  FCaptionSettings.Font.Color := clGrayText;
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := SettingsChanged;
  FTextMargins := THTL_Margins.Create;
  FTextMargins.OnChange := SettingsChanged;
  FTextMargins.Left := 8;
  FTextMargins.Right := 8;
  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;
  FClickSettings := TClickSettings.Create;
  FClickSettings.OnChange := SettingsChanged;
  FClickSettings.Duration := 200;
  FProgressSettings := TProgressSettings.Create(Self);
  FProgressSettings.OnChange := SettingsChanged;
  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := SettingsChanged;
  FSeparatorSettings := TSeparatorSettings.Create;
  FSeparatorSettings.OnChange := SettingsChanged;
  FSeparatorSettings.Visible := False;

  // --- Inicialização dos Timers e Efeitos (padrão CEdit) ---
  FClickEffectTimer := TTimer.Create(Self);
  FClickEffectTimer.Enabled := False;
  FClickEffectTimer.OnTimer := ClickEffectTimerHandler;
  UpdateClickEffectTimerInterval;
  FClickEffectProgress := 0;
  FClickEffectActive := False;
  FProgressTimer := TTimer.Create(Self);
  FProgressTimer.Enabled := False;
  FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
  FProgressTimer.OnTimer := ProgressTimerHandler;
  FProcessing := False;
  FOriginalEnabledState := True;

  // --- Inicialização de variáveis de estado ---
  FStyle := bsSolid;
  FStatus := cepsNormal;
  FOpacity := 255;
  FHovered := False;

  // --- Criação do TMemo interno ---
  FInternalMemo := TMemo.Create(Self);
  FInternalMemo.Parent := Self;
  FInternalMemo.Align := alNone;
  FInternalMemo.BorderStyle := bsNone;
  FInternalMemo.Font.Assign(Self.Font);
  FInternalMemo.Color := FBorderSettings.BackgroundColor;
  FInternalMemo.WordWrap := True;
  FInternalMemo.ScrollBars := ssVertical;
  FInternalMemo.TabStop := Self.TabStop;
  FInternalMemo.OnChange := InternalMemoChange;
  FInternalMemo.OnEnter := InternalMemoEnter;
  FInternalMemo.OnExit := InternalMemoExit;
  FInternalMemo.OnKeyDown := InternalMemoKeyDown;
  FInternalMemo.OnKeyPress := InternalMemoKeyPress;
  FInternalMemo.OnKeyUp := InternalMemoKeyUp;
end;

destructor THTL_CMemo.Destroy;
begin
  // --- Liberar todos os objetos ---
  FreeAndNil(FBorderSettings);
  FreeAndNil(FFocusSettings);
  FreeAndNil(FCaptionSettings);
  FreeAndNil(FHoverSettings);
  FreeAndNil(FTextMargins);
  FreeAndNil(FGradientSettings);
  FreeAndNil(FClickSettings);
  FreeAndNil(FClickEffectTimer);
  FreeAndNil(FProgressSettings);
  FreeAndNil(FProgressTimer);
  FreeAndNil(FImageSettings);
  FreeAndNil(FSeparatorSettings);
  FreeAndNil(FInternalMemo);
  inherited Destroy;
end;

// --- Métodos de Processamento (Loading) ---

procedure THTL_CMemo.StartProcessing;
begin
  if FProgressSettings.ShowProgress and not FProcessing then
  begin
    FProcessing := True;
    FOriginalEnabledState := Self.Enabled;
    if Self.Enabled then
      Self.Enabled := False;
    FInternalMemo.Visible := False; // Esconde o memo
    FProgressStep := 0;
    FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
    FProgressTimer.Enabled := True;
    Invalidate;
  end;
end;

procedure THTL_CMemo.StopProcessing;
begin
  if FProcessing then
  begin
    FProcessing := False;
    FProgressTimer.Enabled := False;
    if Self.Enabled <> FOriginalEnabledState then
      Self.Enabled := FOriginalEnabledState;
    FInternalMemo.Visible := True; // Mostra o memo novamente
    Invalidate;
  end;
end;

procedure THTL_CMemo.ProgressTimerHandler(Sender: TObject);
begin
  if FProcessing then
  begin
    Inc(FProgressStep, FProgressSettings.AnimationProgressStep);
    Invalidate;
  end
  else
    FProgressTimer.Enabled := False;
end;

// --- Métodos de Efeito de Clique ---

procedure THTL_CMemo.StartClickEffect;
begin
  if not Enabled or not FClickSettings.Enabled or (FClickSettings.Duration <= 0) then Exit;
  FClickEffectActive := True;
  FClickEffectProgress := 255;
  UpdateClickEffectTimerInterval;
  FClickEffectTimer.Enabled := True;
  Invalidate;
end;

procedure THTL_CMemo.UpdateClickEffectTimerInterval;
const
  MIN_INTERVAL = 10;
  FADE_STEP_VALUE = 20;
var
  NumTicks: Single;
  NewInterval: Integer;
begin
  if FClickSettings.Duration <= 0 then
  begin
    FClickEffectTimer.Interval := MIN_INTERVAL;
    Exit;
  end;
  NumTicks := 255 / FADE_STEP_VALUE;
  if NumTicks <= 0 then NumTicks := 1;
  NewInterval := Round(FClickSettings.Duration / NumTicks);
  FClickEffectTimer.Interval := Max(MIN_INTERVAL, NewInterval);
end;

procedure THTL_CMemo.ClickEffectTimerHandler(Sender: TObject);
const
  FADE_STEP_VALUE = 20;
begin
  if FClickEffectActive then
  begin
    if FClickEffectProgress > 0 then
      Dec(FClickEffectProgress, FADE_STEP_VALUE);

    FClickEffectProgress := Max(0, FClickEffectProgress);

    if FClickEffectProgress <= 0 then
    begin
      FClickEffectProgress := 0;
      FClickEffectActive := False;
      FClickEffectTimer.Enabled := False;
    end;
    Invalidate;
  end else
  begin
    FClickEffectProgress := 0;
    FClickEffectTimer.Enabled := False;
    Invalidate;
  end;
end;

// --- Handlers de Eventos e Mensagens ---

procedure THTL_CMemo.SettingsChanged(Sender: TObject);
begin
  UpdateInternalMemoBounds;
  Invalidate;
end;

procedure THTL_CMemo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled and not FProcessing then
  begin
    FClickEffectActive := False;
    FClickEffectProgress := 0;
    FClickEffectTimer.Enabled := False;
    if FHoverSettings.Enabled then
      FHoverSettings.StartAnimation(False);
  end;
  Invalidate;
end;

procedure THTL_CMemo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if Assigned(FOnEnter) then FOnEnter(Self);
  Invalidate;
end;

procedure THTL_CMemo.CMExit(var Message: TCMExit);
begin
  inherited;
  if Assigned(FOnExit) then FOnExit(Self);
  Invalidate;
end;

procedure THTL_CMemo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
    if FHoverSettings.Enabled then
      FHoverSettings.StartAnimation(True)
    else
      Invalidate;
  end;
end;

procedure THTL_CMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
    if FHoverSettings.Enabled then
      FHoverSettings.StartAnimation(False)
    else
      Invalidate;
  end;
end;

procedure THTL_CMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Font.Assign(Self.Font);
  end;
  UpdateInternalMemoBounds;
  Invalidate;
end;

// --- Métodos de Controle ---
procedure THTL_CMemo.Loaded;
begin
  inherited;
  UpdateInternalMemoBounds;
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Visible := Self.Visible;
    FInternalMemo.TabStop := Self.TabStop;
  end;
end;

procedure THTL_CMemo.Resize;
begin
  inherited Resize;
  UpdateInternalMemoBounds;
  Invalidate;
end;

procedure THTL_CMemo.SetFocus;
begin
  if TabStop and CanFocus and Assigned(FInternalMemo) then
    FInternalMemo.SetFocus;
end;

procedure THTL_CMemo.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    UpdateControlState;
    if Assigned(FInternalMemo) then
      FInternalMemo.TabStop := Value;
  end;
end;

procedure THTL_CMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FProcessing then Exit;

  if Button = mbLeft then
  begin
    StartClickEffect;
    if CanFocus and not Focused then
       Self.SetFocus;
  end;
end;

// --- Lógica de Posicionamento e Desenho ---

function THTL_CMemo.CalculateLayout: TMemoLayout;
var
  TempCanvas: TCanvas;
  CapHeight, CapWidth: Integer;
  ImageSlotW, ImageSlotH, SeparatorSpace, CornerPadding: Integer;
  RemainingContent: TRect;
begin
  // --- 1. Inicialização ---
  FillChar(Result, SizeOf(TMemoLayout), 0);
  Result.FullControlRect := Self.ClientRect;
  Result.EditBoxRect := Result.FullControlRect;

  // --- 2. Calcular espaço para Legenda Externa (Outside) ---
  if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionSettings.Placement = cplOutside) then
  begin
    TempCanvas := TCanvas.Create;
    try
      TempCanvas.Handle := GetDC(0);
      TempCanvas.Font.Assign(FCaptionSettings.Font);
      CapHeight := TempCanvas.TextHeight('Wg') + FCaptionSettings.Margins.Top + FCaptionSettings.Margins.Bottom;
      CapWidth := TempCanvas.TextWidth(FCaptionSettings.Text) + FCaptionSettings.Margins.Left + FCaptionSettings.Margins.Right;
    finally
      ReleaseDC(0, TempCanvas.Handle);
      TempCanvas.Free;
    end;

    case FCaptionSettings.Position of
      cpAbove:
      begin
        Result.OutsideCaptionRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Top, Result.EditBoxRect.Right, Result.EditBoxRect.Top + CapHeight);
        Result.EditBoxRect.Top := Result.OutsideCaptionRect.Bottom + FCaptionSettings.Offset.Y;
      end;
      cpBelow:
      begin
        Result.OutsideCaptionRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Bottom - CapHeight, Result.EditBoxRect.Right, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Bottom := Result.OutsideCaptionRect.Top - FCaptionSettings.Offset.Y;
      end;
      cpLeft:
      begin
        Result.OutsideCaptionRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Top, Result.EditBoxRect.Left + CapWidth, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Left := Result.OutsideCaptionRect.Right + FCaptionSettings.Offset.X;
      end;
      cpRight:
      begin
        Result.OutsideCaptionRect := Rect(Result.EditBoxRect.Right - CapWidth, Result.EditBoxRect.Top, Result.EditBoxRect.Right, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Right := Result.OutsideCaptionRect.Left - FCaptionSettings.Offset.X;
      end;
    end;
  end;

  // --- 3. Calcular espaço para Imagem Externa (Outside) ---
  if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and (FImageSettings.Placement = iplOutsideBounds) then
  begin
    if not FImageSettings.AutoSize and (FImageSettings.TargetWidth > 0) then ImageSlotW := FImageSettings.TargetWidth else ImageSlotW := FImageSettings.Picture.Width;
    if not FImageSettings.AutoSize and (FImageSettings.TargetHeight > 0) then ImageSlotH := FImageSettings.TargetHeight else ImageSlotH := FImageSettings.Picture.Height;
    ImageSlotW := ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right;
    ImageSlotH := ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom;
    if FSeparatorSettings.Visible then SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness else SeparatorSpace := 0;

    case FImageSettings.ImagePosition of
      ipLeft:
      begin
        Result.OutsideImageRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Top, Result.EditBoxRect.Left + ImageSlotW, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Left := Result.OutsideImageRect.Right + SeparatorSpace;
      end;
      ipRight:
      begin
        Result.OutsideImageRect := Rect(Result.EditBoxRect.Right - ImageSlotW, Result.EditBoxRect.Top, Result.EditBoxRect.Right, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Right := Result.OutsideImageRect.Left - SeparatorSpace;
      end;
      ipTop:
      begin
        Result.OutsideImageRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Top, Result.EditBoxRect.Right, Result.EditBoxRect.Top + ImageSlotH);
        Result.EditBoxRect.Top := Result.OutsideImageRect.Bottom + SeparatorSpace;
      end;
      ipBottom:
      begin
        Result.OutsideImageRect := Rect(Result.EditBoxRect.Left, Result.EditBoxRect.Bottom - ImageSlotH, Result.EditBoxRect.Right, Result.EditBoxRect.Bottom);
        Result.EditBoxRect.Bottom := Result.OutsideImageRect.Top - SeparatorSpace;
      end;
    end;
  end;

  // --- 4. Ajustar para Sombras ---
  if (FStyle in [bsShadow, bsMaterial]) then
     InflateRect(Result.EditBoxRect, -1, -2);

  // --- 5. Calcular ContentRect (área dentro da borda) ---
  Result.ContentRect := Result.EditBoxRect;
  if FBorderSettings.Thickness > 0 then
    InflateRect(Result.ContentRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);
  RemainingContent := Result.ContentRect;

  // --- 6. Particionar ContentRect para elementos Internos ---
  SeparatorSpace := 0;
  if FSeparatorSettings.Visible then
    SeparatorSpace := (FSeparatorSettings.Padding * 2) + FSeparatorSettings.Thickness;

  // 6.1 Imagem Interna
  if FImageSettings.Visible and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and (FImageSettings.Placement = iplInsideBounds) then
  begin
    var AvailableW := Max(0, RemainingContent.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
    var AvailableH := Max(0, RemainingContent.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);
    if not FImageSettings.AutoSize and (FImageSettings.TargetWidth > 0) then ImageSlotW := FImageSettings.TargetWidth else ImageSlotW := Min(FImageSettings.Picture.Width, AvailableW);
    if not FImageSettings.AutoSize and (FImageSettings.TargetHeight > 0) then ImageSlotH := FImageSettings.TargetHeight else ImageSlotH := Min(FImageSettings.Picture.Height, AvailableH);

    case FImageSettings.ImagePosition of
      ipLeft:
      begin
        Result.InsideImageRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Left + ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right, RemainingContent.Bottom);
        RemainingContent.Left := Result.InsideImageRect.Right;
        Result.SeparatorRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Left + SeparatorSpace, RemainingContent.Bottom);
        RemainingContent.Left := Result.SeparatorRect.Right;
      end;
      ipRight:
      begin
        Result.InsideImageRect := Rect(RemainingContent.Right - (ImageSlotW + FImageSettings.Margins.Left + FImageSettings.Margins.Right), RemainingContent.Top, RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Right := Result.InsideImageRect.Left;
        Result.SeparatorRect := Rect(RemainingContent.Right - SeparatorSpace, RemainingContent.Top, RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Right := Result.SeparatorRect.Left;
      end;
      ipTop:
      begin
        Result.InsideImageRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Right, RemainingContent.Top + ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom);
        RemainingContent.Top := Result.InsideImageRect.Bottom;
        Result.SeparatorRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Right, RemainingContent.Top + SeparatorSpace);
        RemainingContent.Top := Result.SeparatorRect.Bottom;
      end;
      ipBottom:
      begin
        Result.InsideImageRect := Rect(RemainingContent.Left, RemainingContent.Bottom - (ImageSlotH + FImageSettings.Margins.Top + FImageSettings.Margins.Bottom), RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Bottom := Result.InsideImageRect.Top;
        Result.SeparatorRect := Rect(RemainingContent.Left, RemainingContent.Bottom - SeparatorSpace, RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Bottom := Result.SeparatorRect.Top;
      end;
    end;
  end;

  // 6.2 Legenda Interna
  if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionSettings.Placement = cplInside) then
  begin
    TempCanvas := TCanvas.Create;
    try
      TempCanvas.Handle := GetDC(0);
      TempCanvas.Font.Assign(FCaptionSettings.Font);
      CapHeight := TempCanvas.TextHeight('Wg') + FCaptionSettings.Margins.Top + FCaptionSettings.Margins.Bottom;
      CapWidth := TempCanvas.TextWidth(FCaptionSettings.Text) + FCaptionSettings.Margins.Left + FCaptionSettings.Margins.Right;
    finally
      ReleaseDC(0, TempCanvas.Handle);
      TempCanvas.Free;
    end;

    case FCaptionSettings.Position of
      cpAbove:
      begin
        Result.InsideCaptionRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Right, RemainingContent.Top + CapHeight + FCaptionSettings.Offset.Y);
        RemainingContent.Top := Result.InsideCaptionRect.Bottom;
      end;
      cpBelow:
      begin
        Result.InsideCaptionRect := Rect(RemainingContent.Left, RemainingContent.Bottom - CapHeight - FCaptionSettings.Offset.Y, RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Bottom := Result.InsideCaptionRect.Top;
      end;
      cpLeft:
      begin
        Result.InsideCaptionRect := Rect(RemainingContent.Left, RemainingContent.Top, RemainingContent.Left + CapWidth + FCaptionSettings.Offset.X, RemainingContent.Bottom);
        RemainingContent.Left := Result.InsideCaptionRect.Right;
      end;
      cpRight:
      begin
        Result.InsideCaptionRect := Rect(RemainingContent.Right - CapWidth - FCaptionSettings.Offset.X, RemainingContent.Top, RemainingContent.Right, RemainingContent.Bottom);
        RemainingContent.Right := Result.InsideCaptionRect.Left;
      end;
    end;
  end;

  // --- 7. Calcular retângulo final do TMemo ---
  Result.MemoRect := RemainingContent;
  InflateRect(Result.MemoRect, -FTextMargins.Left, -FTextMargins.Top);
  Result.MemoRect.Right := Result.MemoRect.Right - FTextMargins.Right;
  Result.MemoRect.Bottom := Result.MemoRect.Bottom - FTextMargins.Bottom;

  if (Self.FBorderSettings.CornerRadius > 0) and (Self.FBorderSettings.RoundCornerType <> rctNone) then
  begin
    CornerPadding := Round(Self.FBorderSettings.CornerRadius * 0.4);
    InflateRect(Result.MemoRect, -CornerPadding, -CornerPadding);
  end;

  // Garantir que o retângulo é válido
  if Result.MemoRect.Right < Result.MemoRect.Left then Result.MemoRect.Right := Result.MemoRect.Left;
  if Result.MemoRect.Bottom < Result.MemoRect.Top then Result.MemoRect.Bottom := Result.MemoRect.Top;

  // --- 8. Calcular o retângulo de destino final da Imagem (com alinhamento) ---
  var ImageSlotRect: TRect;
  if FImageSettings.Placement = iplInsideBounds then
    ImageSlotRect := Result.InsideImageRect
  else
    ImageSlotRect := Result.OutsideImageRect;

  if not IsRectEmpty(ImageSlotRect) and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty then
  begin
      var LDrawW, LDrawH, AvailableWidth, AvailableHeight, LImgX, LImgY: Integer;
      AvailableWidth := Max(0, ImageSlotRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
      AvailableHeight := Max(0, ImageSlotRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);

      if FImageSettings.AutoSize then
      begin
         var TempRect := CalculateProportionalRect(Rect(0,0,AvailableWidth, AvailableHeight), FImageSettings.Picture.Width, FImageSettings.Picture.Height);
         LDrawW := TempRect.Width;
         LDrawH := TempRect.Height;
      end
      else
      begin
          LDrawW := FImageSettings.TargetWidth;
          LDrawH := FImageSettings.TargetHeight;
      end;

      LImgX := ImageSlotRect.Left + FImageSettings.Margins.Left;
      case FImageSettings.HorizontalAlign of
          ihaCenter: LImgX := LImgX + (AvailableWidth - LDrawW) div 2;
          ihaRight:  LImgX := LImgX + AvailableWidth - LDrawW;
      end;

      LImgY := ImageSlotRect.Top + FImageSettings.Margins.Top;
      case FImageSettings.VerticalAlign of
          ivaCenter: LImgY := LImgY + (AvailableHeight - LDrawH) div 2;
          ivaBottom: LImgY := LImgY + AvailableHeight - LDrawH;
      end;
      Result.FinalImageDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);
  end;
end;


procedure THTL_CMemo.UpdateInternalMemoBounds;
var
  Layout: TMemoLayout;
begin
  if not HandleAllocated then Exit;

  Layout := CalculateLayout;

  if Assigned(FInternalMemo) and (FInternalMemo.BoundsRect <> Layout.MemoRect) then
  begin
    FInternalMemo.BoundsRect := Layout.MemoRect;
  end;
end;

procedure THTL_CMemo.Paint;
var
  LG: TGPGraphics;
  Layout: TMemoLayout;
  LPathRect, LShadowPathDrawRect: TGPRectF;
  LRadiusValue, LPathInset: Single;
  ActualBGColor, ActualBorderColor, ActualCaptionColor: TColor;
  LHoverProgress, LClickProgress: Single;
  IsFocusedOnCtrl: Boolean;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LDrawFill, LDrawBorder, LCurrentGradientEnabled: Boolean;
  LActualBorderThickness: Integer;
begin
  inherited Paint;

  if (ClientRect.Width <= 0) or (ClientRect.Height <= 0) then Exit;

  // --- 1. Obter o layout calculado ---
  Layout := CalculateLayout;
  IsFocusedOnCtrl := Self.Focused or (Assigned(FInternalMemo) and FInternalMemo.Focused);

  // --- 2. Atualizar o Memo Interno (necessário antes de desenhar a borda) ---
  if Assigned(FInternalMemo) and (FInternalMemo.BoundsRect <> Layout.MemoRect) then
    FInternalMemo.BoundsRect := Layout.MemoRect;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // --- 3. Desenhar Legenda Externa (se houver) ---
    if not IsRectEmpty(Layout.OutsideCaptionRect) then
    begin
      ActualCaptionColor := ResolveStateColor(Enabled, FHovered, IsFocusedOnCtrl, FCaptionSettings.Color, FHoverSettings.CaptionFontColor, FCaptionSettings.Color, FCaptionSettings.DisabledColor, FHoverSettings.Enabled);
      DrawComponentCaption(Self.Canvas, Layout.OutsideCaptionRect, FCaptionSettings.Text, FCaptionSettings.Font, ActualCaptionColor, FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, FOpacity);
    end;

    // --- 4. Calcular cores e efeitos ---
    LHoverProgress := 0; if Enabled and FHoverSettings.Enabled and (FHoverSettings.CurrentAnimationValue > 0) and (FHoverSettings.HoverEffect <> heNone) and not FProcessing then LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
    LClickProgress := 0; if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and FClickSettings.Enabled and (FClickSettings.Duration > 0) and not FProcessing then LClickProgress := FClickEffectProgress / 255.0;
    var LInitialFillColor := ResolveStateColor(Enabled, False, False, FBorderSettings.BackgroundColor, clNone, clNone, clBtnFace, False, False, False, True);
    var LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderSettings.Color, clNone, clNone, clGray, False, False);
    LActualBorderThickness := FBorderSettings.Thickness;
    if IsFocusedOnCtrl and Enabled then begin case FFocusSettings.Style of fsBorder: LInitialBorderColor := FFocusSettings.BorderColor; fsBackgroundColor: if FFocusSettings.BackgroundColor <> clNone then LInitialFillColor := FFocusSettings.BackgroundColor; end; end;
    if Enabled and not FProcessing and (FStatus <> cepsNormal) then begin var StatusColor: TColor; case FStatus of cepsError: StatusColor := TColor($005C5CFF); cepsWarning: StatusColor := TColor($002986E3); cepsSuccess: StatusColor := TColor($0064B434); else StatusColor := LInitialBorderColor; end; LInitialBorderColor := StatusColor; LActualBorderThickness := Max(LActualBorderThickness, 2); if LInitialFillColor <> clNone then LInitialFillColor := BlendColors(LInitialFillColor, StatusColor, 0.10); end;
    var LFinalHoverColor := IfThen(FHoverSettings.BackgroundColor <> clNone, FHoverSettings.BackgroundColor, LighterColor(LInitialFillColor, 15));
    var LFinalHoverBorderColor := IfThen(FHoverSettings.BorderColor <> clNone, FHoverSettings.BorderColor, LInitialBorderColor);
    var LFinalClickColor := IfThen(FClickSettings.Color = clNone, DarkerColor(LInitialFillColor, 15), FClickSettings.Color);
    var LFinalClickBorderColor := IfThen(FClickSettings.BorderColor = clNone, DarkerColor(LInitialBorderColor, 15), FClickSettings.BorderColor);
    ActualBGColor := LInitialFillColor; ActualBorderColor := LInitialBorderColor;
    if (LHoverProgress > 0) and Enabled and FHoverSettings.Enabled then begin ActualBGColor := BlendColors(LInitialFillColor, LFinalHoverColor, LHoverProgress); ActualBorderColor := BlendColors(LInitialBorderColor, LFinalHoverBorderColor, LHoverProgress); end;
    if (LClickProgress > 0) and Enabled and FClickSettings.Enabled and (FClickSettings.Duration > 0) then begin ActualBGColor := BlendColors(ActualBGColor, LFinalClickColor, LClickProgress); ActualBorderColor := BlendColors(ActualBorderColor, LFinalClickBorderColor, LClickProgress); end;
    LCurrentGradientEnabled := FGradientSettings.Enabled; LDrawFill := True; LDrawBorder := LActualBorderThickness > 0;
    case FStyle of bsSolid: LCurrentGradientEnabled := False; bsBordered: begin LDrawFill := False; LCurrentGradientEnabled := False; LActualBorderThickness := Max(1, LActualBorderThickness); LDrawBorder := True; end; bsFlat: begin LCurrentGradientEnabled := False; LActualBorderThickness := 0; LDrawBorder := False; end; bsGhost: begin LDrawFill := False; LCurrentGradientEnabled := False; LActualBorderThickness := Max(1, LActualBorderThickness); ActualBorderColor := LInitialFillColor; LDrawBorder := True; end; bsGradient: begin LCurrentGradientEnabled := True; LDrawFill := True; LDrawBorder := LActualBorderThickness > 0; end; end;

    // --- Sincroniza a cor de fundo do TMemo interno ---
    if Assigned(FInternalMemo) and (FInternalMemo.Color <> ActualBGColor) and not (csDesigning in ComponentState) then
       FInternalMemo.Color := ActualBGColor;

    // --- 5. Desenho da Moldura (EditBox) ---
    var EditBoxRectEffectiveF: TGPRectF; EditBoxRectEffectiveF.X := Layout.EditBoxRect.Left; EditBoxRectEffectiveF.Y := Layout.EditBoxRect.Top; EditBoxRectEffectiveF.Width := Layout.EditBoxRect.Width; EditBoxRectEffectiveF.Height := Layout.EditBoxRect.Height;
    if (FStyle in [bsShadow, bsMaterial]) then begin var LShadowOffsetXToUse := 1; var LShadowOffsetYToUse := 2; var LShadowAlphaToUse := 60; var TempW := EditBoxRectEffectiveF.Width - Abs(LShadowOffsetXToUse); var TempH := EditBoxRectEffectiveF.Height - Abs(LShadowOffsetYToUse); EditBoxRectEffectiveF := MakeRect(EditBoxRectEffectiveF.X, EditBoxRectEffectiveF.Y, Max(0, TempW), Max(0, TempH)); LPathInset := IfThen(LActualBorderThickness > 0, LActualBorderThickness / 2.0, 0.0); LShadowPathDrawRect := MakeRect(EditBoxRectEffectiveF.X + LPathInset + LShadowOffsetXToUse, EditBoxRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse, Max(0, EditBoxRectEffectiveF.Width - 2 * LPathInset), Max(0, EditBoxRectEffectiveF.Height - 2 * LPathInset)); LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0); LGPPath := TGPGraphicsPath.Create;
    try CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, Max(0, LRadiusValue), FBorderSettings.RoundCornerType); if LGPPath.GetPointCount > 0 then begin LGPBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, LShadowAlphaToUse)); try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end; end; finally LGPPath.Free; end; end;
    if IsFocusedOnCtrl and Enabled and (FFocusSettings.Style = fsGlow) and (FFocusSettings.GlowSize > 0) then begin var LGlowPath: TGPGraphicsPath; var LGlowBrush: TGPBrush; var LGlowRect: TGPRectF; var LGlowRadius: Single; var LGlowInset: Single := IfThen(LDrawBorder, LActualBorderThickness / 2.0, 0) - FFocusSettings.GlowSize; LGlowRect := MakeRect(EditBoxRectEffectiveF.X + LGlowInset, EditBoxRectEffectiveF.Y + LGlowInset, EditBoxRectEffectiveF.Width - 2 * LGlowInset, EditBoxRectEffectiveF.Height - 2 * LGlowInset); LGlowRadius := Min(FBorderSettings.CornerRadius + FFocusSettings.GlowSize, Min(LGlowRect.Width, LGlowRect.Height) / 2.0); LGlowPath := TGPGraphicsPath.Create; try CreateGPRoundedPath(LGlowPath, LGlowRect, Max(0, LGlowRadius), FBorderSettings.RoundCornerType); if LGlowPath.GetPointCount > 0 then begin LGlowBrush := TGPSolidBrush.Create(ColorToARGB(FFocusSettings.GlowColor, FFocusSettings.GlowIntensity)); try LG.FillPath(LGlowBrush, LGlowPath); finally LGlowBrush.Free; end; end; finally LGlowPath.Free; end; end;
    LPathInset := IfThen(LDrawBorder and (LActualBorderThickness > 0), LActualBorderThickness / 2.0, 0.0); LPathRect := MakeRect(EditBoxRectEffectiveF.X + LPathInset, EditBoxRectEffectiveF.Y + LPathInset, EditBoxRectEffectiveF.Width - 2 * LPathInset, EditBoxRectEffectiveF.Height - 2 * LPathInset); LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LPathRect.Width, LPathRect.Height) / 2.0); LGPPath := TGPGraphicsPath.Create; try CreateGPRoundedPath(LGPPath, LPathRect, Max(0, LRadiusValue), FBorderSettings.RoundCornerType); if LGPPath.GetPointCount > 0 then begin if LDrawFill then begin if LCurrentGradientEnabled and (FGradientSettings.StartColor <> clNone) and (FGradientSettings.EndColor <> clNone) then LGPBrush := TGPLinearGradientBrush.Create(LPathRect, ColorToARGB(FGradientSettings.StartColor, FOpacity), ColorToARGB(FGradientSettings.EndColor, FOpacity), LinearGradientModeVertical) else LGPBrush := TGPSolidBrush.Create(ColorToARGB(ActualBGColor, FOpacity)); try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end; end; if LDrawBorder and (FBorderSettings.Style <> psClear) then begin LGPPen := TGPPen.Create(ColorToARGB(ActualBorderColor, FOpacity), LActualBorderThickness); try case FBorderSettings.Style of psDash: LGPPen.SetDashStyle(DashStyleDash); else LGPPen.SetDashStyle(DashStyleSolid); end; LG.DrawPath(LGPPen, LGPPath); finally LGPPen.Free; end; end; end; finally LGPPath.Free; end;

    // --- 6. Desenhar estado de Processamento (se ativo) OU o conteúdo normal ---
    if FProcessing and FProgressSettings.ShowProgress then
    begin
      var LProgressRect := Layout.ContentRect;
      case FProgressSettings.AnimationStyle of
         pasRotatingSemiCircle: begin InflateRect(LProgressRect, -4, -4); if (LProgressRect.Width > 0) and (LProgressRect.Height > 0) then begin var LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8); var LStartAngle := (FProgressStep * 10) mod 360; var ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness); LGPPath := TGPGraphicsPath.Create; try LGPPath.AddArc(ArcRectF, LStartAngle, 270); LGPPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness); try LG.DrawPath(LGPPen, LGPPath); finally LGPPen.Free; end; finally LGPPath.Free; end; end; end;
         pasBouncingDots: begin const NUM_DOTS = 3; InflateRect(LProgressRect, -4, -4); var DotSize := Min(LProgressRect.Width / (NUM_DOTS * 2), LProgressRect.Height / 2); if DotSize > 1 then begin var TotalWidth := NUM_DOTS * DotSize + (NUM_DOTS - 1) * (DotSize/2); var StartX := LProgressRect.Left + (LProgressRect.Width - TotalWidth) / 2; var YPosCenter := LProgressRect.Top + LProgressRect.Height / 2; var MaxOffset := (LProgressRect.Height / 2) - (DotSize / 2); LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255)); try for var I := 0 to NUM_DOTS - 1 do begin var phase := (FProgressStep * 5) + (I * 45); var yOffset := -Abs(Sin(phase * PI / 180) * MaxOffset); var DotX := StartX + I * (DotSize * 1.5); var DotY := YPosCenter + yOffset; LG.FillEllipse(LGPBrush, DotX, DotY, DotSize, DotSize); end; finally LGPBrush.Free; end; end; end;
      end;
    end
    else
    begin
        // --- 7. Desenhar conteúdo normal (Imagem, Separador, Legenda Interna) ---

        // Desenhar Separador
        if FSeparatorSettings.Visible and not IsRectEmpty(Layout.SeparatorRect) then
        begin
          Self.Canvas.Brush.Color := FSeparatorSettings.Color;
          Self.Canvas.Brush.Style := TBrushStyle.bsSolid;
          Self.Canvas.FillRect(Layout.SeparatorRect);
        end;

        // --- CORREÇÃO: Usar DrawGraphicWithGDI para todos os tipos de imagem ---
        if FImageSettings.Visible and not IsRectEmpty(Layout.FinalImageDestRect) and (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty then
        begin
            // A função DrawGraphicWithGDI converte qualquer TGraphic para um TGPBitmap e desenha com GDI+.
            // O modo de desenho idmStretch é usado porque FinalImageDestRect já foi calculado com o tamanho correto.
            DrawGraphicWithGDI(LG, FImageSettings.Picture.Graphic, Layout.FinalImageDestRect, idmStretch);
        end;

        // Desenhar Legenda Interna
        if not IsRectEmpty(Layout.InsideCaptionRect) then
        begin
          ActualCaptionColor := ResolveStateColor(Enabled, FHovered, IsFocusedOnCtrl, FCaptionSettings.Color, FHoverSettings.CaptionFontColor, FCaptionSettings.Color, FCaptionSettings.DisabledColor, FHoverSettings.Enabled);
          DrawComponentCaption(Self.Canvas, Layout.InsideCaptionRect, FCaptionSettings.Text, FCaptionSettings.Font, ActualCaptionColor, FCaptionSettings.Alignment, FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, FOpacity);
        end;
    end;

    // --- 8. Desenhar Efeito de Foco (Underline) ---
    if IsFocusedOnCtrl and Enabled and (FFocusSettings.Style = fsUnderline) and (FFocusSettings.UnderlineThickness > 0) then
    begin
        var LUnderlinePen: TGPPen;
        var yPos := EditBoxRectEffectiveF.Y + EditBoxRectEffectiveF.Height - (FFocusSettings.UnderlineThickness / 2);
        var x1 := EditBoxRectEffectiveF.X;
        var x2 := EditBoxRectEffectiveF.X + EditBoxRectEffectiveF.Width;
        LUnderlinePen := TGPPen.Create(ColorToARGB(FFocusSettings.UnderlineColor, 255), FFocusSettings.UnderlineThickness);
        try
          LG.DrawLine(LUnderlinePen, x1, yPos, x2, yPos);
        finally
          LUnderlinePen.Free;
        end;
    end;
  finally
    LG.Free;
  end;
end;


// --- Setters e Getters ---

procedure THTL_CMemo.SetBorderSettings(const Value: TBorderSettings); begin FBorderSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetFocusSettings(const Value: TFocusSettings); begin FFocusSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetTextMargins(const Value: THTL_Margins); begin FTextMargins.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetGradientSettings(const Value: TGradientSettings); begin FGradientSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetClickSettings(const Value: TClickSettings); begin FClickSettings.Assign(Value); UpdateClickEffectTimerInterval; SettingsChanged(Self); end;
procedure THTL_CMemo.SetProgressSettings(const Value: TProgressSettings); begin FProgressSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetImageSettings(const Value: TImageSettings); begin FImageSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetSeparatorSettings(const Value: TSeparatorSettings); begin FSeparatorSettings.Assign(Value); SettingsChanged(Self); end;
procedure THTL_CMemo.SetStyle(const Value: TButtonStyle); begin if FStyle <> Value then begin FStyle := Value; Invalidate; end; end;
procedure THTL_CMemo.SetStatus(const Value: TCEditStatus); begin if FStatus <> Value then begin FStatus := Value; Invalidate; end; end;
procedure THTL_CMemo.SetOpacity(const Value: Byte); begin if FOpacity <> Value then begin FOpacity := Value; Invalidate; end; end;

function THTL_CMemo.GetLines: TStrings; begin Result := FInternalMemo.Lines; end;
procedure THTL_CMemo.SetLines(const Value: TStrings); begin FInternalMemo.Lines.Assign(Value); end;
function THTL_CMemo.GetReadOnly: Boolean; begin Result := FInternalMemo.ReadOnly; end;
procedure THTL_CMemo.SetReadOnly(const Value: Boolean); begin FInternalMemo.ReadOnly := Value; end;
function THTL_CMemo.GetWordWrap: Boolean; begin Result := FInternalMemo.WordWrap; end;
procedure THTL_CMemo.SetWordWrap(const Value: Boolean); begin FInternalMemo.WordWrap := Value; end;
function THTL_CMemo.GetScrollBars: TScrollStyle; begin Result := FInternalMemo.ScrollBars; end;
procedure THTL_CMemo.SetScrollBars(const Value: TScrollStyle); begin FInternalMemo.ScrollBars := Value; end;
function THTL_CMemo.GetMaxLength: Integer; begin Result := FInternalMemo.MaxLength; end;
procedure THTL_CMemo.SetMaxLength(const Value: Integer); begin FInternalMemo.MaxLength := Value; end;

// --- Eventos do Memo Interno ---
procedure THTL_CMemo.InternalMemoChange(Sender: TObject); begin if Assigned(FOnChange) then FOnChange(Self); end;
procedure THTL_CMemo.InternalMemoEnter(Sender: TObject); begin if not Self.Focused then Invalidate; if Assigned(FOnEnter) then FOnEnter(Self); end;
procedure THTL_CMemo.InternalMemoExit(Sender: TObject); begin Invalidate; if Assigned(FOnExit) then FOnExit(Self); end;
procedure THTL_CMemo.InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); begin if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift); end;
procedure THTL_CMemo.InternalMemoKeyPress(Sender: TObject; var Key: Char); begin if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key); end;
procedure THTL_CMemo.InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); begin if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift); end;

end.

