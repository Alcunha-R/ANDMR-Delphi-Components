unit ANDMR_CButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.GraphUtil, System.UITypes, ANDMR_ComponentUtils, // Added
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  Winapi.ActiveX; // Adicionado para TStreamAdapter

type
  TImagePosition = (ipLeft, ipRight, ipAbove, ipBelow, ipBehind);
  TGradientType = (gtLinearVertical, gtLinearHorizontal);
  TImageStretchMode = (ismProportional, ismFlat);
  // THoverEffect = (heNone, heFade, heScale); // Moved to ANDMR_ComponentUtils.pas
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient);
  TPresetType = (
    cptNone,       // Sem predefinicao, usa as cores do componente
    cptAccept,     // Aceitar, Confirmar (Verde)
    cptDecline,    // Recusar, Cancelar (Vermelho/Cinza)
    cptSave,       // Salvar (Azul)
    cptEdit,       // Editar (Laranja/Amarelo)
    cptDelete,     // Excluir (Vermelho)
    cptNext,       // Proximo, Continuar (Azul/Verde)
    cptPrevious,   // Anterior, Voltar (Cinza/Azul)
    cptInfo,       // Informacao (Azul claro)
    cptWarning,    // Aviso (Amarelo/Laranja)
    cptHelp        // Ajuda (Azul claro)
  );

  TANDMR_CButton = class(TCustomControl)
  private
    FCaption: string;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FActiveColor, FInactiveColor, FHoverColor: TColor;
    FTitleFont: TFont;
    FIsHovering: Boolean;
    FImage: TPicture;
    FTextAlign: TAlignment;
    // FHoverAnimationValue: Integer; // Removed, managed by THoverSettings
    // FHoverAnimationTimer: TTimer; // Removed, managed by THoverSettings
    FGradientEnabled: Boolean;
    FGradientType: TGradientType;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FImagePosition: TImagePosition;
    FImageMargins, FTextMargins: TANDMR_Margins;
    FImageStretchMode: TImageStretchMode;
    // FHoverAnimationStep: Integer; // Removed, managed by THoverSettings
    // FHoverAnimationDirection: Integer; // Removed, managed by THoverSettings
    FTag: Integer;
    FTagString: string;
    FTagExtended: Extended;
    FTagObject: TObject;
    // FHoverEffect: THoverEffect; // Removed, managed by FInternalHoverSettings
    FDisabledCursor: TCursor;
    FTransparent: Boolean;

    FInternalHoverSettings: THoverSettings; // Added for new HoverSettings
    procedure SetInternalHoverSettings(const Value: THoverSettings); // Added for new HoverSettings
    procedure InternalHoverSettingsChanged(Sender: TObject); // Added for new HoverSettings

    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectDuration: Integer;
    FClickEffectActive: Boolean;

    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;

    FOnClick: TNotifyEvent;
    FStyle: TButtonStyle;
    FClickColor: TColor;
    FHoverBorderColor: TColor;
    FEnableHoverEffect: Boolean;
    FClickBorderColor: TColor;
    FHoverTitleColor: TColor;
    FClickTitleColor: TColor;

    FPresetType: TPresetType;

    procedure SetStyle(const Value: TButtonStyle);
    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);
    procedure ResizeMe;

    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetActiveColor(const Value: TColor);
    procedure SetInactiveColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetTitleFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetImage(const Value: TPicture);
    procedure SetTextAlign(const Value: TAlignment);
    procedure SetGradientEnabled(const Value: Boolean);
    procedure SetGradientType(const Value: TGradientType);
    procedure SetGradientStartColor(const Value: TColor);
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetImagePosition(const Value: TImagePosition);
    procedure SetImageStretchMode(const Value: TImageStretchMode);
    procedure SetTag(const Value: Integer);
    procedure SetTagString(const Value: string);
    procedure SetTagObject(const Value: TObject);
    procedure SetHoverEffect(const Value: THoverEffect);
    procedure SetDisabledCursor(const Value: TCursor);
    procedure SetImageMargins(const Value: TANDMR_Margins);
    procedure SetTextMargins(const Value: TANDMR_Margins);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Integer);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetClickColor(const Value: TColor);
    procedure SetHoverBorderColor(const Value: TColor);
    procedure SetClickBorderColor(const Value: TColor);
    procedure SetEnableHoverEffect(const Value: Boolean);
    procedure SetHoverTitleColor(const Value: TColor);
    procedure SetClickTitleColor(const Value: TColor);
    procedure SetClickEffectDuration(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);

    // procedure DoHoverAnimation(Sender: TObject); // Removed, logic now in THoverSettings

    function GetEnableHoverEffect: Boolean; // Added for property redirection
    function GetHoverColor: TColor; // Added for property redirection
    function GetHoverBorderColor: TColor; // Added for property redirection
    function GetHoverTitleColor: TColor; // Restoring this declaration
    function GetHoverEffect: THoverEffect; // This was correctly added before

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function IsEnabledStored: Boolean;
    procedure MarginsChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetTagExtended(const Value: Extended);
    procedure SetPresetType(const Value: TPresetType); // MODIFIED: Color values inside will change

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Enabled read GetEnabled write SetEnabled stored IsEnabledStored;
    property Caption: string read FCaption write SetCaption;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 12;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clTeal;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clGray;
    property HoverColor: TColor read GetHoverColor write SetHoverColor; // default clSkyBlue; // Default managed by THoverSettings
    property HoverTitleColor: TColor read GetHoverTitleColor write SetHoverTitleColor; // default clNone; // Default managed by THoverSettings
    property ClickTitleColor: TColor read FClickTitleColor write SetClickTitleColor default clNone;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property Image: TPicture read FImage write SetImage;
    property TextAlign: TAlignment read FTextAlign write SetTextAlign default taCenter;

    property GradientEnabled: Boolean read FGradientEnabled write SetGradientEnabled default False;
    property GradientType: TGradientType read FGradientType write SetGradientType default gtLinearVertical;
    property GradientStartColor: TColor read FGradientStartColor write SetGradientStartColor;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor;

    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property ImageMargins: TANDMR_Margins read FImageMargins write SetImageMargins;
    property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;
    property ImageStretchMode: TImageStretchMode read FImageStretchMode write SetImageStretchMode default ismProportional;

    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property HoverBorderColor: TColor read GetHoverBorderColor write SetHoverBorderColor; // default clNone; // Default managed by THoverSettings
    property ClickColor: TColor read FClickColor write SetClickColor default clNone;
    property ClickBorderColor: TColor read FClickBorderColor write SetClickBorderColor default clNone;

    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property EnableHoverEffect: Boolean read GetEnableHoverEffect write SetEnableHoverEffect default True;
    property HoverEffect: THoverEffect read GetHoverEffect write SetHoverEffect default heFade; // Changed to use getter
    property ClickEffectDuration: Integer read FClickEffectDuration write SetClickEffectDuration default 200;

    property DisabledCursor: TCursor read FDisabledCursor write SetDisabledCursor default crNo;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TagString: string read FTagString write SetTagString;
    property TagExtended: Extended read FTagExtended write SetTagExtended;
    property TagObject: TObject read FTagObject write SetTagObject;

    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;

    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButton]);
end;

{ TANDMR_CButton }

constructor TANDMR_CButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  Width := 100;
  Height := 45;
  TabStop := True;
  FTransparent := False;

  FCornerRadius := 12;
  FRoundCornerType := rctAll;
  FActiveColor := clTeal;
  FInactiveColor := clGray;
  FHoverColor := clSkyBlue; // This is read via GetHoverColor from THoverSettings now
  FHoverTitleColor := clNone; // This is read via GetHoverTitleColor from THoverSettings now
  FClickTitleColor := clNone;

  // FHoverAnimationTimer := TTimer.Create(Self); // Removed
  // FHoverAnimationTimer.Interval := 15; // Removed
  // FHoverAnimationTimer.Enabled := False; // Removed
  // FHoverAnimationTimer.OnTimer := DoHoverAnimation; // Removed
  // FHoverAnimationStep := 20; // Removed
  // FHoverAnimationValue := 0; // Removed
  // FHoverAnimationDirection := 1; // Removed
  // FHoverEffect := heFade; // Field removed, property uses FInternalHoverSettings, default set in THoverSettings
  FEnableHoverEffect := True; // This property will now use THoverSettings

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Segoe UI';
  FTitleFont.Size := 9;
  FTitleFont.Style := [fsBold];
  FTitleFont.Color := clWindowText;
  FTitleFont.OnChange := FontChanged;

  FTextMargins := TANDMR_Margins.Create;
  FTextMargins.OnChange := MarginsChanged;
  FTextAlign := taCenter;

  FImage := TPicture.Create;
  FImagePosition := ipLeft;
  FImageMargins := TANDMR_Margins.Create;
  FImageMargins.OnChange := MarginsChanged;
  FImageStretchMode := ismProportional;

  FGradientEnabled := False;
  FGradientType := gtLinearVertical;
  FGradientStartColor := clNone;
  FGradientEndColor := clNone;

  FDisabledCursor := crNo;
  Cursor := crHandPoint;

  DoubleBuffered := True;

  FClickEffectDuration := 200;
  FClickEffectProgress := 0;
  FClickEffectActive := False;
  FClickEffectTimer := TTimer.Create(Self);
  FClickEffectTimer.Enabled := False;
  FClickEffectTimer.OnTimer := ClickEffectTimerHandler;
  UpdateClickEffectTimerInterval;

  FBorderColor := clBlack;
  FBorderThickness := 1;
  FBorderStyle := psSolid;
  FHoverBorderColor := clNone;
  FClickColor := clNone;
  FClickBorderColor := clNone;

  FStyle := bsSolid;
  FPresetType := cptNone;
  FCaption := Self.Name;

  // Initialize InternalHoverSettings
  FInternalHoverSettings := THoverSettings.Create(Self); // Pass Self as OwnerControl
  FInternalHoverSettings.OnChange := InternalHoverSettingsChanged;
end;

destructor TANDMR_CButton.Destroy;
begin
  FInternalHoverSettings.OnChange := nil; // Good practice before freeing
  FInternalHoverSettings.Free;            // Free the new HoverSettings

  FTitleFont.OnChange := nil;
  FTitleFont.Free;
  FImage.Free;
  FImageMargins.Free;
  FTextMargins.Free;
  // FHoverAnimationTimer.Free; // Removed
  FClickEffectTimer.Free;
  inherited;
end;

procedure TANDMR_CButton.SetInternalHoverSettings(const Value: THoverSettings);
begin
  FInternalHoverSettings.Assign(Value);
  // The OnChange is already set in the constructor,
  // and TPersistent.Assign does not copy event handlers.
  // If FInternalHoverSettings was freed and recreated, OnChange would need to be reassigned.
  // However, standard property assignment shouldn't free/recreate.
  // Call a method that implies change, like Repaint or Invalidate
  Repaint;
end;

procedure TANDMR_CButton.InternalHoverSettingsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CButton.Loaded;
begin
  inherited Loaded;
  if FGradientStartColor = clNone then
    FGradientStartColor := FActiveColor;
  if FGradientEndColor = clNone then
    FGradientEndColor := DarkerColor(FActiveColor, 30);
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  Repaint;
end;

procedure TANDMR_CButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (csDesigning in ComponentState) and (AParent <> nil) then
  begin
    if (FCaption = Name) and (FPresetType = cptNone) then
      FCaption := '';
  end;
end;

function TANDMR_CButton.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TANDMR_CButton.SetAlign(const Value: TAlign);
begin
  inherited Align := Value;
end;

procedure TANDMR_CButton.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.ResizeMe;
begin
  SetCornerRadius(FCornerRadius);
  Repaint;
end;

procedure TANDMR_CButton.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TANDMR_CButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    StartClickEffect;
  end;
end;

procedure TANDMR_CButton.StartClickEffect;
begin
  if not Enabled or (FClickEffectDuration <= 0) then Exit;

  FClickEffectActive := True;
  FClickEffectProgress := 255;
  UpdateClickEffectTimerInterval;
  FClickEffectTimer.Enabled := True;
  Repaint;
end;

procedure TANDMR_CButton.UpdateClickEffectTimerInterval;
const
  MIN_INTERVAL = 10;
  FADE_STEP_VALUE = 20;
var
  NumTicks: Single;
  NewInterval: Integer;
begin
  if FClickEffectDuration <= 0 then
  begin
    FClickEffectTimer.Interval := MIN_INTERVAL;
    Exit;
  end;

  NumTicks := 255 / FADE_STEP_VALUE;
  if NumTicks <= 0 then NumTicks := 1;

  NewInterval := Round(FClickEffectDuration / NumTicks);
  FClickEffectTimer.Interval := Max(MIN_INTERVAL, NewInterval);
end;

function TANDMR_CButton.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TANDMR_CButton.SetEnabled(Value: Boolean);
begin
  if inherited Enabled <> Value then
  begin
    inherited Enabled := Value;
    if not Value then
    begin
      FIsHovering := False;
      FHoverAnimationValue := 0;
      FHoverAnimationTimer.Enabled := False;
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
    end;
    Cursor := IfThen(Value, crHandPoint, FDisabledCursor);
    Repaint;
  end;
end;

function TANDMR_CButton.IsEnabledStored: Boolean;
begin
  Result := not inherited Enabled;
end;

procedure TANDMR_CButton.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetCornerRadius(const Value: Integer);
var
  MaxRadius: Integer;
begin
  if FCornerRadius <> Value then
  begin
    if (Width > 0) and (Height > 0) then
      MaxRadius := Min(Width, Height) div 2
    else
      MaxRadius := Value;

    FCornerRadius := EnsureRange(Value, 0, MaxRadius);
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetRoundCornerType(const Value: TRoundCornerType);
begin
  if FRoundCornerType <> Value then
  begin
    FRoundCornerType := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetActiveColor(const Value: TColor);
var
  OldActiveColor: TColor;
  OldDerivedEndColor: TColor;
begin
  if FActiveColor <> Value then
  begin
    OldActiveColor := FActiveColor;
    FActiveColor := Value;

    if (FGradientStartColor = OldActiveColor) or (FGradientStartColor = clNone) then
      SetGradientStartColor(FActiveColor);

    OldDerivedEndColor := DarkerColor(OldActiveColor, 30);
    if (FGradientEndColor = OldDerivedEndColor) or (FGradientEndColor = clNone) then
      SetGradientEndColor(DarkerColor(FActiveColor, 30));

    Repaint;
  end;
end;

procedure TANDMR_CButton.SetInactiveColor(const Value: TColor);
begin
  if FInactiveColor <> Value then
  begin
    FInactiveColor := Value;
    Repaint;
  end;
end;

// MODIFIED: Revised preset colors
procedure TANDMR_CButton.SetPresetType(const Value: TPresetType);
var
  PresetCaption: string;
  BaseColor: TColor;
  NewTitleColor: TColor;
begin
  if FPresetType <> Value then
  begin
    FPresetType := Value;
    PresetCaption := '';
    NewTitleColor := clWhite; // Default assumption, will be overridden

    case FPresetType of
      cptNone: begin BaseColor := FActiveColor; Exit; end; // Keep current colors if None
      // Green for accept/confirm (Material Design Green 500: #4CAF50 -> BGR: $0050AF4C)
      cptAccept:   begin BaseColor := TColor($0050AF4C); PresetCaption := 'Confirmar'; NewTitleColor := clWhite; end;
      // Neutral Grey for decline/cancel (Material Design Grey 600: #757575 -> BGR: $00757575)
      cptDecline:  begin BaseColor := TColor($00757575); PresetCaption := 'Cancelar';  NewTitleColor := clWhite; end;
      // Blue for save (Material Design Blue 500: #2196F3 -> BGR: $00F39621)
      cptSave:     begin BaseColor := TColor($00F39621); PresetCaption := 'Salvar';    NewTitleColor := clWhite; end;
      // Orange for edit (Material Design Orange 500: #FF9800 -> BGR: $000098FF)
      cptEdit:     begin BaseColor := TColor($000098FF); PresetCaption := 'Editar';    NewTitleColor := clBlack; end;
      // Red for delete (Material Design Red 500: #F44336 -> BGR: $003643F4)
      cptDelete:   begin BaseColor := TColor($003643F4); PresetCaption := 'Excluir';   NewTitleColor := clWhite; end;
      // Light Blue for next/continue (Material Design Light Blue 500: #03A9F4 -> BGR: $00F4A903)
      cptNext:     begin BaseColor := TColor($00F4A903); PresetCaption := 'Avançar';   NewTitleColor := clWhite; end;
      // Lighter Grey for previous/back (Material Design Grey 500: #9E9E9E -> BGR: $009E9E9E)
      cptPrevious: begin BaseColor := TColor($009E9E9E); PresetCaption := 'Voltar';    NewTitleColor := clBlack; end;
      // Calm Light Blue for info (Material Design Light Blue 400: #29B6F6 -> BGR: $00F6B629, using 400 for differentiation: #4FC3F7 -> BGR: $00F7C34F)
      cptInfo:     begin BaseColor := TColor($00F7C34F); PresetCaption := 'Informação';NewTitleColor := clBlack; end;
      // Yellow for warning (Material Design Yellow 500: #FFEB3B -> BGR: $003BEBFF)
      cptWarning:  begin BaseColor := TColor($003BEBFF); PresetCaption := 'Aviso';     NewTitleColor := clBlack; end;
      // Distinct Blue Grey for help (Material Design Blue Grey 500: #607D8B -> BGR: $008B7D60)
      cptHelp:     begin BaseColor := TColor($008B7D60); PresetCaption := 'Ajuda';     NewTitleColor := clWhite; end;
    else
      BaseColor := FActiveColor; // Fallback, though cptNone should handle it.
    end;

    Self.ActiveColor := BaseColor;
    Self.BorderColor := DarkerColor(BaseColor, 30);
    Self.HoverColor := LighterColor(BaseColor, 25);
    Self.HoverBorderColor := BaseColor;
    Self.ClickColor := DarkerColor(BaseColor, 25);
    Self.ClickBorderColor := DarkerColor(BaseColor, 30);
    Self.TitleFont.Color := NewTitleColor;

    // Auto-adjust hover/click title color based on background luminance
    // This logic is kept from your original code and is generally good.
    if (GetRValue(Self.HoverColor) * 0.299 + GetGValue(Self.HoverColor) * 0.587 + GetBValue(Self.HoverColor) * 0.114) > 186 then
      Self.HoverTitleColor := clBlack
    else
      Self.HoverTitleColor := clWhite;

    if (GetRValue(Self.ClickColor) * 0.299 + GetGValue(Self.ClickColor) * 0.587 + GetBValue(Self.ClickColor) * 0.114) > 186 then
      Self.ClickTitleColor := clBlack
    else
      Self.ClickTitleColor := clWhite;

    if (Trim(Self.FCaption) = '') or (Self.FCaption <> PresetCaption) then
    begin
      Self.FCaption := PresetCaption; // Calls SetCaption, which repaints
      Repaint;
    end
    else
    begin
      Repaint; // Colors changed, repaint even if caption wasn't set by preset
    end;
  end;
end;

procedure TANDMR_CButton.SetHoverColor(const Value: TColor);
begin
  if FInternalHoverSettings.BackgroundColor <> Value then
  begin
    FInternalHoverSettings.BackgroundColor := Value;
    // FHoverColor := Value; // Remove direct field assignment
    // Repaint; // Handled by FInternalHoverSettings.OnChange
  end;
end;

function TANDMR_CButton.GetHoverColor: TColor;
begin
  Result := FInternalHoverSettings.BackgroundColor;
end;

procedure TANDMR_CButton.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TANDMR_CButton.FontChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CButton.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  Repaint;
end;

procedure TANDMR_CButton.SetTextAlign(const Value: TAlignment);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetGradientEnabled(const Value: Boolean);
begin
  if FGradientEnabled <> Value then
  begin
    FGradientEnabled := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetGradientType(const Value: TGradientType);
begin
  if FGradientType <> Value then
  begin
    FGradientType := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetGradientStartColor(const Value: TColor);
begin
  if FGradientStartColor <> Value then
  begin
    FGradientStartColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetGradientEndColor(const Value: TColor);
begin
  if FGradientEndColor <> Value then
  begin
    FGradientEndColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetImagePosition(const Value: TImagePosition);
begin
  if FImagePosition <> Value then
  begin
    FImagePosition := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetImageStretchMode(const Value: TImageStretchMode);
begin
  if FImageStretchMode <> Value then
  begin
    FImageStretchMode := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetImageMargins(const Value: TANDMR_Margins);
begin
  FImageMargins.Assign(Value);
end;

procedure TANDMR_CButton.SetTextMargins(const Value: TANDMR_Margins);
begin
  FTextMargins.Assign(Value);
end;

procedure TANDMR_CButton.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

procedure TANDMR_CButton.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

procedure TANDMR_CButton.SetTagExtended(const Value: Extended);
begin
  FTagExtended := Value;
end;

procedure TANDMR_CButton.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TANDMR_CButton.SetHoverEffect(const Value: THoverEffect);
begin
  // Delegate to FInternalHoverSettings. THoverSettings.SetHoverEffect handles
  // value checking, resetting animation, and calling Changed (which triggers repaint).
  FInternalHoverSettings.HoverEffect := Value;
end;

function TANDMR_CButton.GetHoverEffect: THoverEffect;
begin
  Result := FInternalHoverSettings.HoverEffect;
end;

procedure TANDMR_CButton.SetDisabledCursor(const Value: TCursor);
begin
  if FDisabledCursor <> Value then
  begin
    FDisabledCursor := Value;
    if not Enabled then
      Cursor := FDisabledCursor;
  end;
end;

procedure TANDMR_CButton.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetBorderThickness(const Value: Integer);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Max(0, Value);
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetClickColor(const Value: TColor);
begin
  if FClickColor <> Value then
  begin
    FClickColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetHoverBorderColor(const Value: TColor);
begin
  if FInternalHoverSettings.BorderColor <> Value then
  begin
    FInternalHoverSettings.BorderColor := Value;
    // FHoverBorderColor := Value; // Remove direct field assignment
    // Repaint; // Handled by FInternalHoverSettings.OnChange
  end;
end;

function TANDMR_CButton.GetHoverBorderColor: TColor;
begin
  Result := FInternalHoverSettings.BorderColor;
end;

procedure TANDMR_CButton.SetClickBorderColor(const Value: TColor);
begin
  if FClickBorderColor <> Value then
  begin
    FClickBorderColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetEnableHoverEffect(const Value: Boolean);
begin
  if FInternalHoverSettings.Enabled <> Value then
  begin
    FInternalHoverSettings.Enabled := Value; // Redirect to new settings object
    if not Value then
    begin
      FIsHovering := False;
      // FHoverAnimationValue := 0; // Removed, THoverSettings.SetEnabled handles its internal animation value
      // FHoverAnimationTimer.Enabled := False; // Removed, THoverSettings manages its timer
    end;
    // Repaint is called by THoverSettings.SetEnabled via OnChange, or explicitly if needed.
    // Explicit Repaint here ensures CButton updates if FIsHovering state change has visual implications
    // not covered by THoverSettings.CurrentAnimationValue being 0.
    Repaint;
  end;
end;

function TANDMR_CButton.GetEnableHoverEffect: Boolean;
begin
  Result := FInternalHoverSettings.Enabled;
end;

procedure TANDMR_CButton.SetHoverTitleColor(const Value: TColor);
begin
  if FInternalHoverSettings.FontColor <> Value then
  begin
    FInternalHoverSettings.FontColor := Value;
    // FHoverTitleColor := Value; // Remove direct field assignment
    // Repaint; // Handled by FInternalHoverSettings.OnChange
  end;
end;

function TANDMR_CButton.GetHoverTitleColor: TColor;
begin
  Result := FInternalHoverSettings.FontColor;
end;

procedure TANDMR_CButton.SetClickTitleColor(const Value: TColor);
begin
  if FClickTitleColor <> Value then
  begin
    FClickTitleColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CButton.SetClickEffectDuration(const Value: Integer);
begin
  if FClickEffectDuration <> Value then
  begin
    FClickEffectDuration := Max(0, Value);
    UpdateClickEffectTimerInterval;
  end;
end;

procedure TANDMR_CButton.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
    else
      ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
    Repaint;
  end;
end;

procedure TANDMR_CButton.MarginsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CButton.ClickEffectTimerHandler(Sender: TObject);
const
  FADE_STEP_VALUE = 20;
begin
  if FClickEffectActive then
  begin
    if FClickEffectProgress > 0 then
    begin
      Dec(FClickEffectProgress, FADE_STEP_VALUE);
      FClickEffectProgress := Max(0, FClickEffectProgress);
    end;

    if FClickEffectProgress <= 0 then
    begin
      FClickEffectProgress := 0;
      FClickEffectActive := False;
      FClickEffectTimer.Enabled := False;
    end;
    Repaint;
  end
  else
  begin
    FClickEffectProgress := 0;
    FClickEffectTimer.Enabled := False;
    Repaint;
  end;
end;

procedure TANDMR_CButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Cursor := IfThen(Enabled, crHandPoint, FDisabledCursor);
  if not Enabled then
  begin
      FIsHovering := False;
      FHoverAnimationValue := 0;
      FHoverAnimationTimer.Enabled := False;
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
  end;
  Repaint;
end;

procedure TANDMR_CButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and GetEnableHoverEffect then // Use getter for FInternalHoverSettings.Enabled
  begin
    FIsHovering := True;
    // FInternalHoverSettings.HoverEffect is read by its StartAnimation method.
    // THoverSettings will also handle invalidation/repainting.
    FInternalHoverSettings.StartAnimation(True);
    // The direct Invalidate/Repaint for heScale is removed as THoverSettings handles it.
  end;
end;

procedure TANDMR_CButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and GetEnableHoverEffect then // Use getter
  begin
    FIsHovering := False;
    FInternalHoverSettings.StartAnimation(False);
    // The direct Invalidate/Repaint for heScale is removed as THoverSettings handles it.
  end
  // If hover is disabled while the mouse is outside but animation was ongoing
  else if not GetEnableHoverEffect and (FInternalHoverSettings.CurrentAnimationValue > 0) then
  begin
    FIsHovering := False; // Ensure this is set
    FInternalHoverSettings.StartAnimation(False); // This will force animation to 0 and repaint
  end;
end;

// procedure TANDMR_CButton.DoHoverAnimation(Sender: TObject); // Removed
// var
//   TargetValue: Integer;
//   ValueChanged: Boolean;
// begin
//   ValueChanged := False;
//   if FIsHovering then
//     TargetValue := 255
//   else
//     TargetValue := 0;
// 
//   if FHoverAnimationValue <> TargetValue then
//   begin
//     if FHoverAnimationValue < TargetValue then
//     begin
//       Inc(FHoverAnimationValue, FHoverAnimationStep);
//       if FHoverAnimationValue > TargetValue then FHoverAnimationValue := TargetValue;
//     end
//     else
//     begin
//       Dec(FHoverAnimationValue, FHoverAnimationStep);
//       if FHoverAnimationValue < TargetValue then FHoverAnimationValue := TargetValue;
//     end;
//     ValueChanged := True;
//   end;
// 
//   if ValueChanged then
//   begin
//     if FHoverEffect = heScale then Invalidate else Repaint;
//   end
//   else
//   begin
//     FHoverAnimationTimer.Enabled := False;
//     if FHoverEffect = heScale then Invalidate else Repaint;
//   end;
// end;

procedure TANDMR_CButton.Paint;
var
  LPathRect, LShadowPathDrawRect: TGPRectF;
  LRadiusValue, LPathInset: Single;
  LActualFillColor, LActualBorderColor, LInitialFillColor, LInitialBorderColor, LBaseStyleColor: TColor;
  LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalClickBorderColor: TColor;
  LG: TGPGraphics;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LTextArea, LDestRect, LImageClipRect: TRect;
  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY, AvailableHeight, AvailableWidth: Integer;
  LScaleFactor: Single;
  LCurrentTitleFont: TFont;
  LDrawFill, LDrawBorder, LCurrentGradientEnabled: Boolean;
  LActualBorderThickness: Integer;
  LHoverProgress: Single;
  LClickProgress: Single;
  LShadowAlphaToUse: Byte;
  LShadowOffsetXToUse, LShadowOffsetYToUse : Single;
  LPathWidth, LPathHeight: Single; // Added back for shadow calculation
  LPresetDefaultCaption: string;
  LFinalCaptionToDraw: string;
  ButtonRectEffectiveF: TGPRectF;
  // DrawFormatFlags: Cardinal; // Removed, handled by DrawComponentCaption
  // StartColor_Fill: TColor; // Removed (gradient part)
  // EndColor_Fill: TColor; // Removed (gradient part)

const
  SHADOW_ALPHA = 50;
  SHADOW_OFFSET_X_CONST = 1;
  SHADOW_OFFSET_Y_CONST = 2;
  GRADIENT_DARK_FACTOR = 25;
  SHADOW_ALPHA_HOVER = 80;
  SHADOW_OFFSET_X_HOVER_FACTOR = 1.5;
  SHADOW_OFFSET_Y_HOVER_FACTOR = 1.5;

begin
  inherited Paint;

  LPresetDefaultCaption := '';
  if FPresetType <> cptNone then
  begin
    case FPresetType of
      cptAccept: LPresetDefaultCaption := 'Confirmar';
      cptDecline: LPresetDefaultCaption := 'Cancelar';
      cptSave: LPresetDefaultCaption := 'Salvar';
      cptEdit: LPresetDefaultCaption := 'Editar';
      cptDelete: LPresetDefaultCaption := 'Excluir';
      cptNext: LPresetDefaultCaption := 'Avançar';
      cptPrevious: LPresetDefaultCaption := 'Voltar';
      cptInfo: LPresetDefaultCaption := 'Informação';
      cptWarning: LPresetDefaultCaption := 'Aviso';
      cptHelp: LPresetDefaultCaption := 'Ajuda';
    end;
  end;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    LHoverProgress := 0;
    // Use FInternalHoverSettings.CurrentAnimationValue and FInternalHoverSettings.HoverEffect
    if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.CurrentAnimationValue > 0) and (FInternalHoverSettings.HoverEffect <> heNone) then
      LHoverProgress := FInternalHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and (FClickEffectDuration > 0) then
      LClickProgress := (255 - FClickEffectProgress) / 255.0;

    LInitialFillColor := ResolveStateColor(Enabled, False, False, FActiveColor, clNone, clNone, FInactiveColor, False, False);
    LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderColor, clNone, clNone, BlendColors(FBorderColor, clGray, 0.7), False, False);
    LActualBorderThickness := FBorderThickness; // This remains as is, not dependent on ResolveStateColor for this part

    // Updated Hover Color Logic using FInternalHoverSettings
    if FInternalHoverSettings.BackgroundColor <> clNone then
      LFinalHoverColor := FInternalHoverSettings.BackgroundColor
    else
      LFinalHoverColor := LighterColor(LInitialFillColor, 15); // Fallback

    if FInternalHoverSettings.BorderColor <> clNone then
      LFinalHoverBorderColor := FInternalHoverSettings.BorderColor
    else
      LFinalHoverBorderColor := LInitialBorderColor; // Fallback

    LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LInitialFillColor, 15), FClickColor);
    LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LInitialBorderColor, 15), FClickBorderColor);

    LActualFillColor := LInitialFillColor;
    LActualBorderColor := LInitialBorderColor;
    LCurrentGradientEnabled := FGradientEnabled;
    LDrawFill := True;
    LDrawBorder := LActualBorderThickness > 0;

    case FStyle of
      bsSolid: begin end;
      bsFaded:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.8);
        LActualFillColor := LBaseStyleColor;
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 10), 0.7);
        LCurrentGradientEnabled := False;
        LActualBorderThickness := 0;
        LDrawBorder := False;
      end;
      bsBordered:
      begin
        LDrawFill := False;
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderThickness);
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(FHoverColor=clNone, LInitialFillColor, FHoverColor), 70);
      end;
      bsLight:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
        LActualFillColor := LBaseStyleColor;
        LActualBorderColor := LInitialBorderColor;
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderThickness);
        LDrawBorder := LActualBorderThickness > 0;
      end;
      bsFlat:
      begin
        LCurrentGradientEnabled := False;
        LActualBorderThickness := 0;
        LDrawBorder := False;
        LFinalHoverBorderColor := LInitialFillColor;
      end;
      bsGhost:
      begin
        LDrawFill := False;
        LCurrentGradientEnabled := False;
        LActualBorderThickness := Max(1, FBorderThickness);
        LActualBorderColor := LInitialFillColor;
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(FHoverColor=clNone, LInitialFillColor, FHoverColor), 100);
        LFinalHoverBorderColor := LInitialFillColor;
      end;
      bsShadow:
      begin
        if FTransparent then
        begin
          LDrawFill := False;
          LCurrentGradientEnabled := False;
        end;
      end;
      bsGradient:
      begin
        LCurrentGradientEnabled := True;
        LDrawFill := True;
        LDrawBorder := LActualBorderThickness > 0;
      end;
    end;

    if (LHoverProgress > 0) and Enabled and GetEnableHoverEffect then
    begin
      if LDrawFill or (FStyle = bsBordered) or (FStyle = bsGhost) then
        LActualFillColor := BlendColors(LActualFillColor, LFinalHoverColor, LHoverProgress);

      if LDrawBorder then
        LActualBorderColor := BlendColors(LActualBorderColor, LFinalHoverBorderColor, LHoverProgress)
      else if FStyle = bsFlat then
      begin
        LActualBorderColor := BlendColors(clNone, LFinalHoverBorderColor, LHoverProgress);
        LActualBorderThickness := Max(1, FBorderThickness);
        LDrawBorder := True;
      end;
    end;

    if (LClickProgress > 0) and Enabled and (FClickEffectDuration > 0) then
    begin
      LActualFillColor := BlendColors(LActualFillColor, LFinalClickColor, LClickProgress * 0.7 + 0.1);
      if LDrawBorder then
        LActualBorderColor := BlendColors(LActualBorderColor, LFinalClickBorderColor, LClickProgress * 0.7 + 0.1);
    end;

    if FTransparent then
    begin
      LDrawFill := False;
      LCurrentGradientEnabled := False;
    end;

    LDrawBorder := LActualBorderThickness > 0;

    ButtonRectEffectiveF := MakeRect(0.0, 0.0, Self.Width, Self.Height);

    if (FStyle = bsShadow) and (not FTransparent) then
    begin
      LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST;
      LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST;
      LShadowAlphaToUse := SHADOW_ALPHA;

      if (LHoverProgress > 0) and Enabled and GetEnableHoverEffect then
      begin
        LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST + ((SHADOW_OFFSET_X_CONST * SHADOW_OFFSET_X_HOVER_FACTOR) - SHADOW_OFFSET_X_CONST) * LHoverProgress;
        LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST + ((SHADOW_OFFSET_Y_CONST * SHADOW_OFFSET_Y_HOVER_FACTOR) - SHADOW_OFFSET_Y_CONST) * LHoverProgress;
        LShadowAlphaToUse := Round(SHADOW_ALPHA + (SHADOW_ALPHA_HOVER - SHADOW_ALPHA) * LHoverProgress);
      end;

      ButtonRectEffectiveF.X := IfThen(LShadowOffsetXToUse < 0, Abs(LShadowOffsetXToUse), 0.0);
      ButtonRectEffectiveF.Y := IfThen(LShadowOffsetYToUse < 0, Abs(LShadowOffsetYToUse), 0.0);
      ButtonRectEffectiveF.Width := Self.Width - Abs(LShadowOffsetXToUse);
      ButtonRectEffectiveF.Height := Self.Height - Abs(LShadowOffsetYToUse);
      ButtonRectEffectiveF.Width := Max(0, ButtonRectEffectiveF.Width);
      ButtonRectEffectiveF.Height := Max(0, ButtonRectEffectiveF.Height);

      // Reinstated calculations for LPathInset, LPathWidth, LPathHeight for shadow
      if LActualBorderThickness > 0 then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
      LPathWidth := ButtonRectEffectiveF.Width - 2 * LPathInset;
      LPathHeight := ButtonRectEffectiveF.Height - 2 * LPathInset;
      LPathWidth := Max(0, LPathWidth);
      LPathHeight := Max(0, LPathHeight);

      LShadowPathDrawRect := MakeRect(ButtonRectEffectiveF.X + LPathInset + LShadowOffsetXToUse,
                                      ButtonRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse,
                                      LPathWidth, LPathHeight);
      LRadiusValue := Min(FCornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0);
      LRadiusValue := Max(0, LRadiusValue);

      LGPPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, LRadiusValue, FRoundCornerType);
        if LGPPath.GetPointCount > 0 then
        begin
          LGPBrush := TGPSolidBrush.Create(ColorToARGB(clBlack, LShadowAlphaToUse));
          try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end;
        end;
      finally LGPPath.Free; end;
    end
    else if LDrawBorder and (LActualBorderThickness > 0) then
    begin
      if ButtonRectEffectiveF.Width > 0 then ButtonRectEffectiveF.Width  := Max(0, ButtonRectEffectiveF.Width - 0.5);
      if ButtonRectEffectiveF.Height > 0 then ButtonRectEffectiveF.Height := Max(0, ButtonRectEffectiveF.Height - 0.5);
    end;

    if LDrawBorder and (LActualBorderThickness > 0) then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
    // LPathWidth := ButtonRectEffectiveF.Width - 2 * LPathInset; // Not needed, DrawEditBox handles internal path
    // LPathHeight := ButtonRectEffectiveF.Height - 2 * LPathInset; // Not needed
    // LPathWidth := Max(0, LPathWidth); LPathHeight := Max(0, LPathHeight); // Not needed

    // LPathRect := MakeRect(ButtonRectEffectiveF.X + LPathInset, // Not needed
    // ButtonRectEffectiveF is already the outer rect for DrawEditBox
    // LRadiusValue is still calculated based on FCornerRadius and the dimensions of the drawing area.
    // The drawing area for radius calculation should be ButtonRectEffectiveF.
    LRadiusValue := Min(FCornerRadius, Min(ButtonRectEffectiveF.Width, ButtonRectEffectiveF.Height) / 2.0);
    LRadiusValue := Max(0, LRadiusValue);

    var DrawAreaRect: TRect;
    DrawAreaRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                         Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width), Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));

    var BgColorToUse: TColor;
    if LDrawFill and not FTransparent then
    begin
      if LCurrentGradientEnabled then // DrawEditBox uses solid fill, choose one color for gradient
        BgColorToUse := IfThen(FGradientStartColor = clNone, LActualFillColor, FGradientStartColor)
      else
        BgColorToUse := LActualFillColor;
    end
    else
      BgColorToUse := clNone;

    var BorderColorToUse: TColor;
    if LDrawBorder then
      BorderColorToUse := LActualBorderColor
    else
      BorderColorToUse := clNone;

    DrawEditBox(LG,
                DrawAreaRect,
                BgColorToUse,
                BorderColorToUse,
                LActualBorderThickness,
                FBorderStyle,
                Round(LRadiusValue),
                FRoundCornerType,
                255); // Opacity for CButton is handled by FTransparent/clNone

    LImageClipRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                           Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width),
                           Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));
    if LDrawBorder and (LActualBorderThickness > 0) then
        InflateRect(LImageClipRect, -Round(LActualBorderThickness), -Round(LActualBorderThickness));


    if (FImage.Graphic <> nil) and not FImage.Graphic.Empty then
    begin
      LImgW := FImage.Width; LImgH := FImage.Height;
      case FImageStretchMode of
        ismProportional:
        begin
          if (LImgW = 0) or (LImgH = 0) then begin LDrawW := 0; LDrawH := 0; end
          else
          begin
            AvailableWidth  := Max(0, LImageClipRect.Width - FImageMargins.Left - FImageMargins.Right);
            AvailableHeight := Max(0, LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom);
            if FImagePosition in [ipLeft, ipRight] then
            begin
              LDrawH := AvailableHeight;
              if LImgH <> 0 then LDrawW := Round(LImgW / LImgH * LDrawH) else LDrawW := 0;
              if LDrawW > AvailableWidth then
              begin
                LDrawW := AvailableWidth;
                if LImgW <> 0 then LDrawH := Round(LImgH / LImgW * LDrawW) else LDrawH := 0;
              end;
            end
            else
            begin
              LDrawW := AvailableWidth;
              if LImgW <> 0 then LDrawH := Round(LImgH / LImgW * LDrawW) else LDrawH := 0;
              if LDrawH > AvailableHeight then
              begin
                LDrawH := AvailableHeight;
                if LImgH <> 0 then LDrawW := Round(LImgW / LImgH * LDrawH) else LDrawW := 0;
              end;
            end;
          end;
        end;
        ismFlat:
        begin
          LDrawW := Max(0, LImageClipRect.Width - FImageMargins.Left - FImageMargins.Right);
          LDrawH := Max(0, LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom);
        end;
      else LDrawW := LImgW; LDrawH := LImgH;
      end;

      if (LImgW > 0) and (LDrawW <=0) then LDrawW := Min(LImgW, Max(0, LImageClipRect.Width div 3));
      if (LImgH > 0) and (LDrawH <=0) then LDrawH := Min(LImgH, Max(0, LImageClipRect.Height div 3));

      case FImagePosition of
        ipLeft:
        begin
          LImgX := LImageClipRect.Left + FImageMargins.Left;
          LImgY := LImageClipRect.Top + FImageMargins.Top + (Max(0,LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom - LDrawH)) div 2;
          LTextArea := Rect(LImgX + LDrawW + FImageMargins.Right + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        ipRight:
        begin
          LImgX := LImageClipRect.Right - LDrawW - FImageMargins.Right;
          LImgY := LImageClipRect.Top + FImageMargins.Top + (Max(0,LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom - LDrawH)) div 2;
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImgX - FImageMargins.Left - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        ipAbove:
        begin
          LImgX := LImageClipRect.Left + FImageMargins.Left + (Max(0,LImageClipRect.Width - FImageMargins.Left - FImageMargins.Right - LDrawW)) div 2;
          LImgY := LImageClipRect.Top + FImageMargins.Top;
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImgY + LDrawH + FImageMargins.Bottom + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        ipBelow:
        begin
          LImgX := LImageClipRect.Left + FImageMargins.Left + (Max(0,LImageClipRect.Width - FImageMargins.Left - FImageMargins.Right - LDrawW)) div 2;
          LImgY := LImageClipRect.Bottom - LDrawH - FImageMargins.Bottom;
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImgY - FImageMargins.Top - FTextMargins.Bottom);
        end;
        ipBehind:
        begin
          LImgX := LImageClipRect.Left + FImageMargins.Left + (Max(0,LImageClipRect.Width - FImageMargins.Left - FImageMargins.Right - LDrawW)) div 2;
          LImgY := LImageClipRect.Top + FImageMargins.Top + (Max(0,LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom - LDrawH)) div 2;
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
      else
        begin
          LImgX := LImageClipRect.Left + FImageMargins.Left;
          LImgY := LImageClipRect.Top + FImageMargins.Top + (Max(0,LImageClipRect.Height - FImageMargins.Top - FImageMargins.Bottom - LDrawH)) div 2;
          LTextArea := Rect(LImgX + LDrawW + FImageMargins.Right + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
      end;
      LDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

      // Use FInternalHoverSettings.HoverEffect for image scaling check
      if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.HoverEffect = heScale) and (LHoverProgress > 0) then
      begin
        LScaleFactor := 1 + (LHoverProgress * (1.05 - 1));
        InflateRect(LDestRect, Round(LDrawW * (LScaleFactor - 1) / 2), Round(LDrawH * (LScaleFactor - 1) / 2));
      end;

      if (LDestRect.Right > LDestRect.Left) and (LDestRect.Bottom > LDestRect.Top) then
      begin
        var CurrentDrawMode: TImageDrawMode;
        case FImageStretchMode of
          ismProportional: CurrentDrawMode := idmProportional;
          ismFlat: CurrentDrawMode := idmStretch; // Mapped ismFlat to idmStretch
        else
          CurrentDrawMode := idmProportional; // Default case
        end;

        if FImage.Graphic is TPNGImage then
        begin
          DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, LDestRect, CurrentDrawMode);
        end
        else if FImage.Graphic <> nil then
        begin
          DrawNonPNGImageWithCanvas(Self.Canvas, FImage.Graphic, LDestRect, CurrentDrawMode);
        end;
      end;
    end
    else
      LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                        LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);

    if Trim(Self.FCaption) <> '' then
      LFinalCaptionToDraw := Self.FCaption
    else if Trim(LPresetDefaultCaption) <> '' then
      LFinalCaptionToDraw := LPresetDefaultCaption
    else
      LFinalCaptionToDraw := '';


    if Trim(LFinalCaptionToDraw) <> '' then
    begin
      LCurrentTitleFont := TFont.Create; // Still need this for hover/click effects on font
      try
        LCurrentTitleFont.Assign(FTitleFont);

        if Enabled then
        begin
          if GetEnableHoverEffect and (LHoverProgress > 0) then
          begin
            // Use FInternalHoverSettings.FontColor for hover title color
            if FInternalHoverSettings.FontColor <> clNone then
              LCurrentTitleFont.Color := BlendColors(FTitleFont.Color, FInternalHoverSettings.FontColor, LHoverProgress)
            // Use FInternalHoverSettings.HoverEffect for font effects
            else if FInternalHoverSettings.HoverEffect = heFade then // Apply fade effect to font color if no specific hover title color (fallback)
              LCurrentTitleFont.Color := BlendColors(FTitleFont.Color, LighterColor(LActualFillColor, 80), LHoverProgress * 0.5);

            if FInternalHoverSettings.HoverEffect = heScale then // Scale font size if scale effect is active
              LCurrentTitleFont.Size := Round(FTitleFont.Size * (1 + LHoverProgress * (1.05 - 1)));
          end;

          if FClickEffectActive and (LClickProgress > 0) and (FClickEffectDuration > 0) then
          begin
            if FClickTitleColor <> clNone then // Apply click effect to font color
              LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickTitleColor, LClickProgress);
          end;
        end
        else // Disabled state
        begin
          LCurrentTitleFont.Color := BlendColors(FTitleFont.Color, clGray, 0.6);
        end;

        // Ensure LTextArea is valid
        if LTextArea.Right < LTextArea.Left then LTextArea.Right := LTextArea.Left;
        if LTextArea.Bottom < LTextArea.Top then LTextArea.Bottom := LTextArea.Top;
        LTextArea.Left   := Max(LImageClipRect.Left, LTextArea.Left);
        LTextArea.Top    := Max(LImageClipRect.Top, LTextArea.Top);
        LTextArea.Right  := Min(LImageClipRect.Right, LTextArea.Right);
        LTextArea.Bottom := Min(LImageClipRect.Bottom, LTextArea.Bottom);

        if (LTextArea.Width > 0) and (LTextArea.Height > 0) then
        begin
          DrawComponentCaption(
            Self.Canvas,
            LTextArea,
            LFinalCaptionToDraw,
            LCurrentTitleFont,
            LCurrentTitleFont.Color, // Pass the calculated color
            FTextAlign,
            cvaCenter, // TANDMR_CButton typically centers text vertically
            False,     // TANDMR_CButton caption is single line
            255        // Opacity handled by LCurrentTitleFont.Color or component transparency
          );
        end;
      finally
        LCurrentTitleFont.Free;
      end;
    end;
  finally
    LG.Free;
  end;

  if Focused and TabStop and Enabled then
    DrawFocusRect(Canvas.Handle, ClientRect);

end;

procedure TANDMR_CButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and (Key in [VK_RETURN, VK_SPACE]) then
  begin
    StartClickEffect;
    Click;
    Key := 0;
  end;
end;

end.
