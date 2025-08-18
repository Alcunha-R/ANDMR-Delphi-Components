unit HTL_CCheckBox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Winapi.Messages, Vcl.Forms, Vcl.StdCtrls, System.UITypes, System.Types,
  System.Math, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  HTL_ComponentUtils;

type
  // *** CORREÇÃO: Renomeado 'cbsCircle' para 'cbsCircular' para evitar conflito ***
  TCheckBoxStyle = (cbsDefault, cbsFlat, cbsFilled, cbsCircular, cbsCustom);

  TCheckBoxShape = (cbsSquare, cbsCircle);
  TCheckMarkStyle = (cmsCheck, cmsCross, cmsSolid);

  THTL_CCheckBox = class(TCustomControl)
  private
    FStyle: TCheckBoxStyle;
    FChecked: Boolean;
    FCaptionSettings: TCaptionSettings;
    FBorderSettings: TBorderSettings;
    FHoverSettings: THoverSettings;
    FFocusSettings: TFocusSettings;
    FCheckColor: TColor;
    FCheckedBackgroundColor: TColor;
    FCheckedBorderColor: TColor;
    FBoxShape: TCheckBoxShape;
    FCheckMarkStyle: TCheckMarkStyle;
    FOnCheckChanged: TNotifyEvent;
    // Mapeamento para as propriedades Caption e Font
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    // Procedimentos internos
    procedure SetStyle(const Value: TCheckBoxStyle);
    procedure SetChecked(const Value: Boolean);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetFocusSettings(const Value: TFocusSettings);
    procedure SetCheckColor(const Value: TColor);
    procedure SetCheckedBackgroundColor(const Value: TColor);
    procedure SetCheckedBorderColor(const Value: TColor);
    procedure SetBoxShape(const Value: TCheckBoxShape);
    procedure SetCheckMarkStyle(const Value: TCheckMarkStyle);
    procedure SettingsChanged(Sender: TObject);
    procedure BreakStyleLink;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Style: TCheckBoxStyle read FStyle write SetStyle default cbsDefault;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property FocusSettings: TFocusSettings read FFocusSettings write SetFocusSettings;

    property BoxShape: TCheckBoxShape read FBoxShape write SetBoxShape default cbsSquare;
    property CheckMarkStyle: TCheckMarkStyle read FCheckMarkStyle write SetCheckMarkStyle default cmsCheck;
    property CheckColor: TColor read FCheckColor write SetCheckColor default clWindowText;
    property CheckedBackgroundColor: TColor read FCheckedBackgroundColor write SetCheckedBackgroundColor default clNone;
    property CheckedBorderColor: TColor read FCheckedBorderColor write SetCheckedBorderColor default clNone;

    property OnCheckChanged: TNotifyEvent read FOnCheckChanged write FOnCheckChanged;
    property Align;
    property Anchors;
    property Caption: string read GetCaption write SetCaption;
    property Constraints;
    property Enabled;
    property Font: TFont read GetFont write SetFont;
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

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CCheckBox]);
end;

{ THTL_CCheckBox }

constructor THTL_CCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse, csReplicatable, csDoubleClicks];
  Width := 150;
  Height := 25;
  TabStop := True;
  Cursor := crHandPoint;
  DoubleBuffered := True;

  FChecked := False;
  FCheckColor := clWindowText;
  FCheckedBackgroundColor := clNone;
  FCheckedBorderColor := clNone;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  FCaptionSettings.Text := Name;
  FCaptionSettings.Position := cpRight;
  FCaptionSettings.Alignment := taLeftJustify;
  FCaptionSettings.VerticalAlignment := cvaCenter;
  FCaptionSettings.Margins.Left := 6;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.BackgroundColor := clNone;

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := SettingsChanged;
  FHoverSettings.BackgroundColor := TColor($00F0F0F0);
  FHoverSettings.BorderColor := clHighlight;
  FHoverSettings.AnimationStep := 255;

  FFocusSettings := TFocusSettings.Create;
  FFocusSettings.OnChange := SettingsChanged;
  FFocusSettings.Style := fsBorder;

  if (csDesigning in ComponentState) then
    SetStyle(cbsDefault);
end;

destructor THTL_CCheckBox.Destroy;
begin
  FCaptionSettings.Free;
  FBorderSettings.Free;
  FHoverSettings.Free;
  FFocusSettings.Free;
  inherited;
end;

procedure THTL_CCheckBox.Loaded;
begin
  inherited;
  if FCaptionSettings.Text = '' then
    FCaptionSettings.Text := Name;
end;

procedure THTL_CCheckBox.BreakStyleLink;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then Exit;
  if FStyle <> cbsCustom then
    FStyle := cbsCustom;
end;

procedure THTL_CCheckBox.SettingsChanged(Sender: TObject);
begin
  BreakStyleLink;
  Repaint;
end;

procedure THTL_CCheckBox.Click;
begin
  if Enabled then
  begin
    Self.Checked := not Self.Checked;
    inherited Click;
  end;
end;

procedure THTL_CCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and (Key = VK_SPACE) then
  begin
    Click;
    Key := 0;
  end;
end;

procedure THTL_CCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and FHoverSettings.Enabled then
    FHoverSettings.StartAnimation(True);
end;

procedure THTL_CCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and FHoverSettings.Enabled then
    FHoverSettings.StartAnimation(False);
end;

procedure THTL_CCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure THTL_CCheckBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  Repaint;
end;

procedure THTL_CCheckBox.SetStyle(const Value: TCheckBoxStyle);
begin
  if FStyle = Value then Exit;

  FBorderSettings.OnChange := nil;
  FCaptionSettings.OnChange := nil;
  try
    case Value of
      cbsDefault:
        begin
          Self.BoxShape := cbsSquare;
          Self.CheckMarkStyle := cmsCheck;
          Self.CheckColor := clWindowText;
          Self.CheckedBackgroundColor := clNone;
          Self.CheckedBorderColor := clNone;
          FBorderSettings.BackgroundColor := clNone; // Em vez de clWindow
          FBorderSettings.Color := clGray;
          FBorderSettings.CornerRadius := 3;
          FBorderSettings.Thickness := 1;
        end;
      cbsFlat:
        begin
          Self.BoxShape := cbsSquare;
          Self.CheckMarkStyle := cmsCheck;
          Self.CheckColor := TColor($0050AF4C);
          Self.CheckedBackgroundColor := clNone;
          Self.CheckedBorderColor := TColor($0050AF4C);
          FBorderSettings.BackgroundColor := TColor($00FAFAFA);
          FBorderSettings.Color := TColor($00BDBDBD);
          FBorderSettings.CornerRadius := 3;
          FBorderSettings.Thickness := 2;
        end;
      cbsFilled:
        begin
          Self.BoxShape := cbsSquare;
          Self.CheckMarkStyle := cmsCheck;
          Self.CheckColor := clWhite;
          Self.CheckedBackgroundColor := TColor($0050AF4C);
          Self.CheckedBorderColor := TColor($0050AF4C);
          FBorderSettings.BackgroundColor := clNone;
          FBorderSettings.Color := TColor($00BDBDBD);
          FBorderSettings.CornerRadius := 4;
          FBorderSettings.Thickness := 1;
        end;
      // *** CORREÇÃO: Usando o novo nome do estilo 'cbsCircular' ***
      cbsCircular:
        begin
          Self.BoxShape := cbsCircle;
          Self.CheckMarkStyle := cmsSolid;
          Self.CheckColor := clHighlight;
          Self.CheckedBackgroundColor := clNone;
          Self.CheckedBorderColor := clHighlight;
          FBorderSettings.BackgroundColor := clNone;
          FBorderSettings.Color := clGray;
          FBorderSettings.CornerRadius := 0;
          FBorderSettings.Thickness := 1;
        end;
    end;
  finally
    FBorderSettings.OnChange := SettingsChanged;
    FCaptionSettings.OnChange := SettingsChanged;
  end;
  FStyle := Value;
  Repaint;
end;

procedure THTL_CCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Repaint;
    if Assigned(FOnCheckChanged) then
      FOnCheckChanged(Self);
  end;
end;

procedure THTL_CCheckBox.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  BreakStyleLink;
end;

procedure THTL_CCheckBox.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  BreakStyleLink;
end;

procedure THTL_CCheckBox.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  BreakStyleLink;
end;

procedure THTL_CCheckBox.SetFocusSettings(const Value: TFocusSettings);
begin
  FFocusSettings.Assign(Value);
  BreakStyleLink;
end;

procedure THTL_CCheckBox.SetCheckColor(const Value: TColor);
begin
  if FCheckColor <> Value then
  begin
    FCheckColor := Value;
    BreakStyleLink;
    Repaint;
  end;
end;

procedure THTL_CCheckBox.SetCheckedBackgroundColor(const Value: TColor);
begin
  if FCheckedBackgroundColor <> Value then
  begin
    FCheckedBackgroundColor := Value;
    BreakStyleLink;
    Repaint;
  end;
end;

procedure THTL_CCheckBox.SetCheckedBorderColor(const Value: TColor);
begin
  if FCheckedBorderColor <> Value then
  begin
    FCheckedBorderColor := Value;
    BreakStyleLink;
    Repaint;
  end;
end;

procedure THTL_CCheckBox.SetBoxShape(const Value: TCheckBoxShape);
begin
  if FBoxShape <> Value then
  begin
    FBoxShape := Value;
    BreakStyleLink;
    Repaint;
  end;
end;

procedure THTL_CCheckBox.SetCheckMarkStyle(const Value: TCheckMarkStyle);
begin
  if FCheckMarkStyle <> Value then
  begin
    FCheckMarkStyle := Value;
    BreakStyleLink;
    Repaint;
  end;
end;

function THTL_CCheckBox.GetCaption: string;
begin
  Result := FCaptionSettings.Text;
end;

procedure THTL_CCheckBox.SetCaption(const Value: string);
begin
  if FCaptionSettings.Text <> Value then
  begin
    FCaptionSettings.Text := Value;
    BreakStyleLink;
  end;
end;

function THTL_CCheckBox.GetFont: TFont;
begin
  Result := FCaptionSettings.Font;
end;

procedure THTL_CCheckBox.SetFont(const Value: TFont);
begin
  FCaptionSettings.Font.Assign(Value);
  BreakStyleLink;
end;

procedure THTL_CCheckBox.Paint;
var
  LG: TGPGraphics;
  BoxRect, CaptionRect: TRect;
  BoxSize: Integer;
  HoverFactor, CheckFactor: Single;
  ActualBorderColor, ActualBackgroundColor, ActualCheckColor: TColor;
  Path, IconPath: TGPGraphicsPath;
  Pen: TGPPen;
  Brush: TGPBrush;
begin
  inherited;
  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // 1. Calcular Layout
    BoxSize := Self.Height - 4;
    BoxRect := System.Types.Rect(2, 2, 2 + BoxSize, 2 + BoxSize);
    CaptionRect := ClientRect;
    CaptionRect.Left := BoxRect.Right + FCaptionSettings.Margins.Left;

    // 2. Calcular Cores
    HoverFactor := FHoverSettings.CurrentAnimationValue / 255.0;
    if FChecked then CheckFactor := 1.0 else CheckFactor := 0.0;

    if Enabled then
    begin
      var TgtBgColor   := IfThen(FChecked and (FCheckedBackgroundColor <> clNone), FCheckedBackgroundColor, FBorderSettings.BackgroundColor);
      var TgtBorderColor := IfThen(FChecked and (FCheckedBorderColor <> clNone), FCheckedBorderColor, FBorderSettings.Color);

      ActualBackgroundColor := BlendColors(FBorderSettings.BackgroundColor, TgtBgColor, CheckFactor);
      ActualBorderColor     := BlendColors(FBorderSettings.Color, TgtBorderColor, CheckFactor);
      ActualCheckColor      := FCheckColor;

      ActualBackgroundColor := BlendColors(ActualBackgroundColor, FHoverSettings.BackgroundColor, HoverFactor);
      ActualBorderColor     := BlendColors(ActualBorderColor, FHoverSettings.BorderColor, HoverFactor);

      if Focused and (FFocusSettings.Style in [fsBorder, fsGlow]) then
        ActualBorderColor := FFocusSettings.BorderColor;
    end
    else
    begin
      ActualBackgroundColor := clBtnFace;
      ActualBorderColor := clGrayText;
      ActualCheckColor := clGrayText;
    end;

    // 3. Desenhar a Caixa
    Path := TGPGraphicsPath.Create;
    try
      if FBoxShape = cbsSquare then
        CreateGPRoundedPath(Path, RectToGPRectF(BoxRect), FBorderSettings.CornerRadius, rctAll)
      else
        Path.AddEllipse(RectToGPRectF(BoxRect));

      if Path.GetPointCount > 0 then
      begin
        if ActualBackgroundColor <> clNone then
        begin
          Brush := TGPSolidBrush.Create(ColorToARGB(ActualBackgroundColor));
          try
            LG.FillPath(Brush, Path);
          finally
            Brush.Free;
          end;
        end;

        Pen := TGPPen.Create(ColorToARGB(ActualBorderColor), FBorderSettings.Thickness);
        try LG.DrawPath(Pen, Path); finally Pen.Free; end;
      end;
    finally
      Path.Free;
    end;

    // 4. Desenhar o Marcador
    if FChecked then
    begin
      IconPath := TGPGraphicsPath.Create;
      Pen := TGPPen.Create(ColorToARGB(ActualCheckColor), 2.0);
      try
        Pen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
        var IconRect := BoxRect;
        IconRect.Inflate(-Round(BoxSize * 0.25), -Round(BoxSize * 0.25));

        case FCheckMarkStyle of
          cmsCheck:
            begin
              IconPath.AddLine(IconRect.Left, IconRect.Top + IconRect.Height * 0.5, IconRect.Left + IconRect.Width * 0.4, IconRect.Bottom);
              IconPath.AddLine(IconRect.Left + IconRect.Width * 0.4, IconRect.Bottom, IconRect.Right, IconRect.Top);
              LG.DrawPath(Pen, IconPath);
            end;
          cmsCross:
            begin
              IconPath.AddLine(IconRect.Left, IconRect.Top, IconRect.Right, IconRect.Bottom);
              IconPath.AddLine(IconRect.Left, IconRect.Bottom, IconRect.Right, IconRect.Top);
              LG.DrawPath(Pen, IconPath);
            end;
          cmsSolid:
            begin
              Brush := TGPSolidBrush.Create(ColorToARGB(ActualCheckColor));
              try
                if FBoxShape = cbsSquare then
                  LG.FillRectangle(Brush, RectToGPRectF(IconRect))
                else
                  LG.FillEllipse(Brush, RectToGPRectF(IconRect));
              finally
                Brush.Free;
              end;
            end;
        end;
      finally
        IconPath.Free;
        Pen.Free;
      end;
    end;

    // 5. Desenhar o Texto
    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
    begin
       var FontColor := IfThen(Enabled, FCaptionSettings.Color, clGrayText);
       DrawComponentCaption(Self.Canvas, CaptionRect, FCaptionSettings.Text,
         FCaptionSettings.Font, FontColor, FCaptionSettings.Alignment,
         FCaptionSettings.VerticalAlignment, FCaptionSettings.WordWrap, 255);
    end;

    // 6. Desenhar Foco
    if Focused and TabStop and Enabled and (FFocusSettings.Style = fsBorder) then
    begin
      // A VCL pode desenhar um retângulo de foco padrão se desejado
      // DrawFocusRect(Canvas.Handle, ClientRect);
    end;

  finally
    LG.Free;
  end;
end;

end.
