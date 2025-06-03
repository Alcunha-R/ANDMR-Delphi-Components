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
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient);
  TPresetType = (
    cptNone,      // Sem predefinicao, usa as cores do componente
    cptAccept,    // Aceitar, Confirmar (Verde)
    cptDecline,   // Recusar, Cancelar (Vermelho/Cinza)
    cptSave,      // Salvar (Azul)
    cptEdit,      // Editar (Laranja/Amarelo)
    cptDelete,    // Excluir (Vermelho)
    cptNext,      // Proximo, Continuar (Azul/Verde)
    cptPrevious,  // Anterior, Voltar (Cinza/Azul)
    cptInfo,      // Informacao (Azul claro)
    cptWarning,   // Aviso (Amarelo/Laranja)
    cptHelp       // Ajuda (Azul claro)
  );

  TANDMR_CButton = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings;
    FCaption: string;
    FCaptionSettings: TCaptionSettings; // Added
    FImageSettings: TImageSettings;   // Added
    FGradientEnabled: Boolean;
    FGradientType: TGradientType;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FImagePosition: TImagePosition;
    FTextMargins: TANDMR_Margins; // FImageMargins removed
    FTag: Integer;
    FTagString: string;
    FTagExtended: Extended;
    FTagObject: TObject;
    FDisabledCursor: TCursor;
    FTransparent: Boolean;

    FClickEffectTimer: TTimer;
    FClickEffectProgress: Integer;
    FClickEffectDuration: Integer;
    FClickEffectActive: Boolean;

    FOnClick: TNotifyEvent;
    FStyle: TButtonStyle;
    FClickColor: TColor;
    FClickBorderColor: TColor;
    FClickTitleColor: TColor;

    FPresetType: TPresetType;

    FInternalHoverSettings: THoverSettings;

    procedure SetInternalHoverSettings(const Value: THoverSettings);
    procedure InternalHoverSettingsChanged(Sender: TObject);
    procedure BorderSettingsChanged(Sender: TObject); // New handler for FBorderSettings
    procedure SettingsChanged(Sender: TObject); // Added

    procedure SetStyle(const Value: TButtonStyle);
    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);

    procedure ClickEffectTimerHandler(Sender: TObject);
    procedure StartClickEffect;
    procedure UpdateClickEffectTimerInterval;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetCaption(const Value: string);
    function GetCornerRadius: Integer; // Getter
    procedure SetCornerRadius(const Value: Integer); // Setter
    function GetRoundCornerType: TRoundCornerType; // Getter
    procedure SetRoundCornerType(const Value: TRoundCornerType); // Setter
    function GetActiveColor: TColor; // Getter for FBorderSettings.BackgroundColor
    procedure SetActiveColor(const Value: TColor); // Setter for FBorderSettings.BackgroundColor
    procedure SetHoverColor(const Value: TColor);
    function GetTitleFont: TFont; // Changed
    procedure SetTitleFont(const Value: TFont);
    procedure FontChanged(Sender: TObject); // This will be FCaptionSettings.Font.OnChange
    function GetImage: TPicture; // Changed
    procedure SetImage(const Value: TPicture);
    function GetTextAlign: TAlignment; // Changed
    procedure SetTextAlign(const Value: TAlignment);
    procedure SetGradientEnabled(const Value: Boolean);
    procedure SetGradientType(const Value: TGradientType);
    procedure SetGradientStartColor(const Value: TColor);
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetImagePosition(const Value: TImagePosition);
    function GetImageStretchMode: TImageStretchMode; // Changed
    procedure SetImageStretchMode(const Value: TImageStretchMode);
    procedure SetTag(const Value: Integer);
    procedure SetTagString(const Value: string);
    procedure SetTagObject(const Value: TObject);
    procedure SetHoverEffect(const Value: THoverEffect);
    procedure SetDisabledCursor(const Value: TCursor);
    function GetImageMargins: TANDMR_Margins; // Changed
    procedure SetImageMargins(const Value: TANDMR_Margins);
    procedure SetTextMargins(const Value: TANDMR_Margins);
    function GetBorderColor: TColor; // Getter
    procedure SetBorderColor(const Value: TColor); // Setter
    function GetBorderThickness: Integer; // Getter
    procedure SetBorderThickness(const Value: Integer); // Setter
    function GetBorderStyle: TPenStyle; // Getter
    procedure SetBorderStyle(const Value: TPenStyle); // Setter
    procedure SetClickColor(const Value: TColor);
    procedure SetHoverBorderColor(const Value: TColor);
    procedure SetClickBorderColor(const Value: TColor);
    procedure SetEnableHoverEffect(const Value: Boolean);
    procedure SetHoverTitleColor(const Value: TColor);
    procedure SetClickTitleColor(const Value: TColor);
    procedure SetClickEffectDuration(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);

    function GetEnableHoverEffect: Boolean;
    function GetHoverColor: TColor;
    function GetHoverBorderColor: TColor;
    function GetHoverTitleColor: TColor;
    function GetHoverEffect: THoverEffect;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function IsEnabledStored: Boolean;
    procedure MarginsChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetTagExtended(const Value: Extended);
    procedure SetPresetType(const Value: TPresetType);

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
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius default 12;
    property RoundCornerType: TRoundCornerType read GetRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read GetActiveColor write SetActiveColor default clTeal;
    property HoverColor: TColor read GetHoverColor write SetHoverColor;
    property HoverTitleColor: TColor read GetHoverTitleColor write SetHoverTitleColor;
    property ClickTitleColor: TColor read FClickTitleColor write SetClickTitleColor default clNone;
    property TitleFont: TFont read GetTitleFont write SetTitleFont; // Changed
    property Image: TPicture read GetImage write SetImage; // Changed
    property TextAlign: TAlignment read GetTextAlign write SetTextAlign default taCenter; // Changed

    property GradientEnabled: Boolean read FGradientEnabled write SetGradientEnabled default False;
    property GradientType: TGradientType read FGradientType write SetGradientType default gtLinearVertical;
    property GradientStartColor: TColor read FGradientStartColor write SetGradientStartColor;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor;

    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property ImageMargins: TANDMR_Margins read GetImageMargins write SetImageMargins; // Changed
    property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;
    property ImageStretchMode: TImageStretchMode read GetImageStretchMode write SetImageStretchMode default ismProportional; // Changed

    property BorderColor: TColor read GetBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read GetBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read GetBorderStyle write SetBorderStyle default psSolid;
    property HoverBorderColor: TColor read GetHoverBorderColor write SetHoverBorderColor;
    property ClickColor: TColor read FClickColor write SetClickColor default clNone;
    property ClickBorderColor: TColor read FClickBorderColor write SetClickBorderColor default clNone;

    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property EnableHoverEffect: Boolean read GetEnableHoverEffect write SetEnableHoverEffect default True;
    property HoverEffect: THoverEffect read GetHoverEffect write SetHoverEffect default heFade;
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
  FClickTitleColor := clNone;

  // FTitleFont := TFont.Create; // Removed
  // FTitleFont.Name := 'Segoe UI'; // Removed
  // FTitleFont.Size := 9; // Removed
  // FTitleFont.Style := [fsBold]; // Removed
  // FTitleFont.Color := clWindowText; // Removed
  // FTitleFont.OnChange := FontChanged; // Will be handled by FCaptionSettings.Font.OnChange

  FTextMargins := TANDMR_Margins.Create;
  FTextMargins.OnChange := MarginsChanged;

  FImagePosition := ipLeft;

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

  FClickColor := clNone;
  FClickBorderColor := clNone;

  FStyle := bsSolid;
  FPresetType := cptNone;
  FCaption := Self.Name;

  FInternalHoverSettings := THoverSettings.Create(Self);
  FInternalHoverSettings.OnChange := InternalHoverSettingsChanged;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := BorderSettingsChanged;
  FBorderSettings.CornerRadius := 12; // Default FCornerRadius
  FBorderSettings.RoundCornerType := rctAll; // Default FRoundCornerType
  FBorderSettings.BackgroundColor := clTeal; // Default FActiveColor
  FBorderSettings.Color := clBlack; // Default FBorderColor
  FBorderSettings.Thickness := 1; // Default FBorderThickness
  FBorderSettings.Style := psSolid; // Default FBorderStyle

  // Create and initialize FCaptionSettings
  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := SettingsChanged;
  // Transfer initial values from old fields (if they existed before this point) to FCaptionSettings.Font
  // Assuming FTitleFont was created temporarily for this or accessed via a property if it still exists
  // For a clean refactor, FTitleFont fields would be set directly if no TFont object was created yet.
  // However, the original code creates FTitleFont, then transfers.
  // We need to ensure FCaptionSettings gets these initial values if FTitleFont is removed before this block.
  // Let's assume FTitleFont was created for setup and values are transferred:
  var TempTitleFont: TFont;
  TempTitleFont := TFont.Create;
  try
    TempTitleFont.Name := 'Segoe UI';
    TempTitleFont.Size := 9;
    TempTitleFont.Style := [fsBold];
    TempTitleFont.Color := clWindowText;
    FCaptionSettings.Font.Assign(TempTitleFont);
  finally
    TempTitleFont.Free;
  end;
  FCaptionSettings.Font.OnChange := FontChanged;
  FCaptionSettings.Alignment := taCenter; // Default FTextAlign

  // Create and initialize FImageSettings
  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := SettingsChanged;
  // Similarly, for FImage, FImageMargins, FImageStretchMode
  // FImageSettings.Picture.Assign(FImage); // FImage would be created temporarily or use defaults
  // FImageSettings.Margins.Assign(FImageMargins); // FImageMargins would be created temporarily or use defaults
  // Default assignment for picture and margins if not assigned from old fields
  FImageSettings.Picture.Create; // Ensure picture object exists
  FImageSettings.Margins.Create; // Ensure margins object exists

  // Initial FImageStretchMode was ismProportional
  FImageSettings.DrawMode := idmProportional;
end;

destructor TANDMR_CButton.Destroy;
begin
  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;

  FInternalHoverSettings.OnChange := nil;
  FInternalHoverSettings.Free;

  // Free FCaptionSettings
  FCaptionSettings.OnChange := nil;
  FCaptionSettings.Free;

  // Free FImageSettings
  FImageSettings.OnChange := nil;
  FImageSettings.Free;

  FTextMargins.Free;
  FClickEffectTimer.Free;
  inherited;
end;

procedure TANDMR_CButton.SetInternalHoverSettings(const Value: THoverSettings);
begin
  FInternalHoverSettings.Assign(Value);
  Repaint;
end;

procedure TANDMR_CButton.InternalHoverSettingsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CButton.BorderSettingsChanged(Sender: TObject);
begin
  Invalidate; // Or Repaint if more direct control is needed
end;

procedure TANDMR_CButton.SettingsChanged(Sender: TObject);
begin
  Repaint;
  Invalidate;
end;

procedure TANDMR_CButton.Loaded;
begin
  inherited Loaded;
  if FGradientStartColor = clNone then
    FGradientStartColor := FBorderSettings.BackgroundColor;
  if FGradientEndColor = clNone then
    FGradientEndColor := DarkerColor(FBorderSettings.BackgroundColor, 30);
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
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
      if FInternalHoverSettings.Enabled then
         FInternalHoverSettings.StartAnimation(False);
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

// Getters and Setters for Border Properties
function TANDMR_CButton.GetCornerRadius: Integer; begin Result := FBorderSettings.CornerRadius; end;
procedure TANDMR_CButton.SetCornerRadius(const Value: Integer);
var MaxRadius: Integer;
begin
  if (Width > 0) and (Height > 0) then MaxRadius := Min(Width, Height) div 2
  else MaxRadius := Value;
  FBorderSettings.CornerRadius := EnsureRange(Value, 0, MaxRadius);
  // FBorderSettings.OnChange will trigger repaint
end;

function TANDMR_CButton.GetRoundCornerType: TRoundCornerType; begin Result := FBorderSettings.RoundCornerType; end;
procedure TANDMR_CButton.SetRoundCornerType(const Value: TRoundCornerType); begin FBorderSettings.RoundCornerType := Value; end;

function TANDMR_CButton.GetActiveColor: TColor; begin Result := FBorderSettings.BackgroundColor; end;
procedure TANDMR_CButton.SetActiveColor(const Value: TColor);
var
  OldActiveColor: TColor;
  OldDerivedEndColor: TColor;
begin
  if FBorderSettings.BackgroundColor <> Value then
  begin
    OldActiveColor := FBorderSettings.BackgroundColor;
    FBorderSettings.BackgroundColor := Value;

    if (FGradientStartColor = OldActiveColor) or (FGradientStartColor = clNone) then
      SetGradientStartColor(FBorderSettings.BackgroundColor);

    OldDerivedEndColor := DarkerColor(OldActiveColor, 30);
    if (FGradientEndColor = OldDerivedEndColor) or (FGradientEndColor = clNone) then
      SetGradientEndColor(DarkerColor(FBorderSettings.BackgroundColor, 30));
    // FBorderSettings.OnChange will trigger repaint
  end;
end;

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
    NewTitleColor := clWhite;

    case FPresetType of
      cptNone: begin BaseColor := FBorderSettings.BackgroundColor; Exit; end; // Use FBorderSettings.BackgroundColor
      cptAccept:   begin BaseColor := TColor($0050AF4C); PresetCaption := 'Confirmar'; NewTitleColor := clWhite; end;
      cptDecline:  begin BaseColor := TColor($00757575); PresetCaption := 'Cancelar';  NewTitleColor := clWhite; end;
      cptSave:     begin BaseColor := TColor($00F39621); PresetCaption := 'Salvar';    NewTitleColor := clWhite; end;
      cptEdit:     begin BaseColor := TColor($000098FF); PresetCaption := 'Editar';    NewTitleColor := clBlack; end;
      cptDelete:   begin BaseColor := TColor($003643F4); PresetCaption := 'Excluir';   NewTitleColor := clWhite; end;
      cptNext:     begin BaseColor := TColor($00F4A903); PresetCaption := 'Avançar';   NewTitleColor := clWhite; end;
      cptPrevious: begin BaseColor := TColor($009E9E9E); PresetCaption := 'Voltar';    NewTitleColor := clBlack; end;
      cptInfo:     begin BaseColor := TColor($00F7C34F); PresetCaption := 'Informação';NewTitleColor := clBlack; end;
      cptWarning:  begin BaseColor := TColor($003BEBFF); PresetCaption := 'Aviso';     NewTitleColor := clBlack; end;
      cptHelp:     begin BaseColor := TColor($008B7D60); PresetCaption := 'Ajuda';     NewTitleColor := clWhite; end;
    else
      BaseColor := FBorderSettings.BackgroundColor; // Use FBorderSettings.BackgroundColor
    end;

    FBorderSettings.BackgroundColor := BaseColor; // Set ActiveColor via FBorderSettings
    FBorderSettings.Color := DarkerColor(BaseColor, 30); // Set BorderColor via FBorderSettings
    Self.HoverColor := LighterColor(BaseColor, 25);
    Self.HoverBorderColor := BaseColor;
    Self.ClickColor := DarkerColor(BaseColor, 25);
    Self.ClickBorderColor := DarkerColor(BaseColor, 30);
    FCaptionSettings.Font.Color := NewTitleColor; // Changed

    if (GetRValue(Self.HoverColor) * 0.299 + GetGValue(Self.HoverColor) * 0.587 + GetBValue(Self.HoverColor) * 0.114) > 186 then
      FInternalHoverSettings.FontColor := clBlack // Changed
    else
      FInternalHoverSettings.FontColor := clWhite; // Changed

    if (GetRValue(Self.ClickColor) * 0.299 + GetGValue(Self.ClickColor) * 0.587 + GetBValue(Self.ClickColor) * 0.114) > 186 then
      Self.ClickTitleColor := clBlack
    else
      Self.ClickTitleColor := clWhite;

    if (Trim(Self.FCaption) = '') or (Self.FCaption <> PresetCaption) then
    begin
      Self.FCaption := PresetCaption; // FCaption setter calls Repaint
    end
    else if FPresetType <> cptNone then // Repaint if a preset was applied, even if caption didn't change
    begin
      Repaint; // This repaint might be redundant if FCaptionSettings.OnChange handles it.
    end;
  end;
end;

function TANDMR_CButton.GetTitleFont: TFont;
begin
  Result := FCaptionSettings.Font;
end;

procedure TANDMR_CButton.SetHoverColor(const Value: TColor);
begin
  if FInternalHoverSettings.BackgroundColor <> Value then
  begin
    FInternalHoverSettings.BackgroundColor := Value;
  end;
end;

function TANDMR_CButton.GetHoverColor: TColor;
begin
  Result := FInternalHoverSettings.BackgroundColor;
end;

procedure TANDMR_CButton.SetTitleFont(const Value: TFont);
begin
  FCaptionSettings.Font.Assign(Value);
  // Repaint is handled by FCaptionSettings.OnChange via SettingsChanged or FontChanged directly
end;

procedure TANDMR_CButton.FontChanged(Sender: TObject);
// This method is now assigned to FCaptionSettings.Font.OnChange
// Or, if FCaptionSettings.OnChange = SettingsChanged is sufficient, this can be removed
// and SettingsChanged will handle the repaint. For now, let's keep it for directness.
begin
  Repaint;
end;

function TANDMR_CButton.GetImage: TPicture;
begin
  Result := FImageSettings.Picture;
end;

procedure TANDMR_CButton.SetImage(const Value: TPicture);
begin
  FImageSettings.Picture.Assign(Value);
  // Repaint is handled by FImageSettings.OnChange via SettingsChanged
  Repaint; // Keep repaint for safety, though SettingsChanged should cover it.
end;

function TANDMR_CButton.GetTextAlign: TAlignment;
begin
  Result := FCaptionSettings.Alignment;
end;

procedure TANDMR_CButton.SetTextAlign(const Value: TAlignment);
begin
  // FCaptionSettings.Alignment setter should handle repaint via OnChange
  FCaptionSettings.Alignment := Value;
  // If FCaptionSettings.OnChange doesn't trigger repaint for Alignment, add Repaint here.
  // Based on TCaptionSettings implementation, its OnChange should be called.
  // Adding Repaint just in case, but ideally SettingsChanged covers this.
  Repaint;
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

function TANDMR_CButton.GetImageStretchMode: TImageStretchMode;
begin
  if FImageSettings.DrawMode = idmProportional then
    Result := ismProportional
  else if FImageSettings.DrawMode = idmStretch then // Assuming idmStretch is the equivalent of ismFlat
    Result := ismFlat
  else
    Result := ismProportional; // Default or map other modes if necessary
end;

procedure TANDMR_CButton.SetImageStretchMode(const Value: TImageStretchMode);
begin
  if Value = ismProportional then
    FImageSettings.DrawMode := idmProportional
  else if Value = ismFlat then
    FImageSettings.DrawMode := idmStretch;
  // Repaint is handled by FImageSettings.OnChange via SettingsChanged
  Repaint; // Keep repaint for safety
end;

function TANDMR_CButton.GetImageMargins: TANDMR_Margins;
begin
  Result := FImageSettings.Margins;
end;

procedure TANDMR_CButton.SetImageMargins(const Value: TANDMR_Margins);
begin
  FImageSettings.Margins.Assign(Value);
  // Repaint is handled by FImageSettings.OnChange via SettingsChanged
  Repaint; // Keep repaint for safety
end;

procedure TANDMR_CButton.SetTextMargins(const Value: TANDMR_Margins);
begin
  FTextMargins.Assign(Value);
end;

procedure TANDMR_CButton.SetTag(const Value: Integer); begin FTag := Value; end;
procedure TANDMR_CButton.SetTagString(const Value: string); begin FTagString := Value; end;
procedure TANDMR_CButton.SetTagExtended(const Value: Extended); begin FTagExtended := Value; end;
procedure TANDMR_CButton.SetTagObject(const Value: TObject); begin FTagObject := Value; end;

procedure TANDMR_CButton.SetHoverEffect(const Value: THoverEffect);
begin
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

function TANDMR_CButton.GetBorderColor: TColor; begin Result := FBorderSettings.Color; end;
procedure TANDMR_CButton.SetBorderColor(const Value: TColor); begin FBorderSettings.Color := Value; end;

function TANDMR_CButton.GetBorderThickness: Integer; begin Result := FBorderSettings.Thickness; end;
procedure TANDMR_CButton.SetBorderThickness(const Value: Integer); begin FBorderSettings.Thickness := Max(0, Value); end;

function TANDMR_CButton.GetBorderStyle: TPenStyle; begin Result := FBorderSettings.Style; end;
procedure TANDMR_CButton.SetBorderStyle(const Value: TPenStyle); begin FBorderSettings.Style := Value; end;

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
    FInternalHoverSettings.Enabled := Value;
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
      FClickEffectActive := False;
      FClickEffectProgress := 0;
      FClickEffectTimer.Enabled := False;
      if FInternalHoverSettings.Enabled then
         FInternalHoverSettings.StartAnimation(False);
  end;
  Repaint;
end;

procedure TANDMR_CButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and GetEnableHoverEffect then
  begin
    FInternalHoverSettings.StartAnimation(True);
  end;
end;

procedure TANDMR_CButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and GetEnableHoverEffect then
  begin
    FInternalHoverSettings.StartAnimation(False);
  end
  else if FInternalHoverSettings.CurrentAnimationValue > 0 then
  begin
    FInternalHoverSettings.StartAnimation(False);
  end;
end;

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
  LPathWidth, LPathHeight: Single;
  LPresetDefaultCaption: string;
  LFinalCaptionToDraw: string;
  ButtonRectEffectiveF: TGPRectF;

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
    if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.CurrentAnimationValue > 0) and (FInternalHoverSettings.HoverEffect <> heNone) then
      LHoverProgress := FInternalHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and (FClickEffectDuration > 0) then
      LClickProgress := (255 - FClickEffectProgress) / 255.0;

  LInitialFillColor := ResolveStateColor(Enabled, False, False, FBorderSettings.BackgroundColor, clNone, clNone, BlendColors(FBorderSettings.BackgroundColor, clGray, 0.65), False, False); // Use FBorderSettings.BackgroundColor and blend for disabled
  LInitialBorderColor := ResolveStateColor(Enabled, False, False, FBorderSettings.Color, clNone, clNone, BlendColors(FBorderSettings.Color, clGray, 0.7), False, False); // Use FBorderSettings.Color
  LActualBorderThickness := FBorderSettings.Thickness; // Use FBorderSettings.Thickness

    if FInternalHoverSettings.BackgroundColor <> clNone then
      LFinalHoverColor := FInternalHoverSettings.BackgroundColor
    else
      LFinalHoverColor := LighterColor(LInitialFillColor, 15);

    if FInternalHoverSettings.BorderColor <> clNone then
      LFinalHoverBorderColor := FInternalHoverSettings.BorderColor
    else
      LFinalHoverBorderColor := LInitialBorderColor;

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
      LActualBorderThickness := Max(1, FBorderSettings.Thickness); // Use FBorderSettings.Thickness
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(GetHoverColor=clNone, LInitialFillColor, GetHoverColor), 70);
      end;
      bsLight:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
        LActualFillColor := LBaseStyleColor;
      LActualBorderColor := LInitialBorderColor; // This is FBorderSettings.Color
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
        LCurrentGradientEnabled := False;
      LActualBorderThickness := Max(1, FBorderSettings.Thickness); // Use FBorderSettings.Thickness
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
        LActualBorderThickness := Max(1, FBorderSettings.Thickness); // Corrected from FBorderThickness
        LActualBorderColor := LInitialFillColor;
        LDrawBorder := LActualBorderThickness > 0;
        LFinalHoverColor := ColorToARGB(IfThen(GetHoverColor=clNone, LInitialFillColor, GetHoverColor), 100);
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
        LActualBorderThickness := Max(1, FBorderSettings.Thickness); // Use FBorderSettings.Thickness
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

      if LActualBorderThickness > 0 then LPathInset := LActualBorderThickness / 2.0 else LPathInset := 0.0;
      LPathWidth := ButtonRectEffectiveF.Width - 2 * LPathInset;
      LPathHeight := ButtonRectEffectiveF.Height - 2 * LPathInset;
      LPathWidth := Max(0, LPathWidth);
      LPathHeight := Max(0, LPathHeight);

      LShadowPathDrawRect := MakeRect(ButtonRectEffectiveF.X + LPathInset + LShadowOffsetXToUse,
                                      ButtonRectEffectiveF.Y + LPathInset + LShadowOffsetYToUse,
                                      LPathWidth, LPathHeight);
    LRadiusValue := Min(FBorderSettings.CornerRadius, Min(LShadowPathDrawRect.Width, LShadowPathDrawRect.Height) / 2.0); // Use FBorderSettings.CornerRadius
      LRadiusValue := Max(0, LRadiusValue);

      LGPPath := TGPGraphicsPath.Create;
      try
      CreateGPRoundedPath(LGPPath, LShadowPathDrawRect, LRadiusValue, FBorderSettings.RoundCornerType); // Use FBorderSettings.RoundCornerType
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
    LRadiusValue := Min(FBorderSettings.CornerRadius, Min(ButtonRectEffectiveF.Width, ButtonRectEffectiveF.Height) / 2.0); // Use FBorderSettings.CornerRadius
    LRadiusValue := Max(0, LRadiusValue);

    var DrawAreaRect: TRect;
    DrawAreaRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                         Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width), Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));

    var BgColorToUse: TColor;
    if LDrawFill and not FTransparent then
    begin
      if LCurrentGradientEnabled then
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

    DrawEditBox(LG, DrawAreaRect, BgColorToUse, BorderColorToUse, LActualBorderThickness, FBorderSettings.Style, Round(LRadiusValue), FBorderSettings.RoundCornerType, 255); // Use FBorderSettings.Style and FBorderSettings.RoundCornerType

    LImageClipRect := Rect(Round(ButtonRectEffectiveF.X), Round(ButtonRectEffectiveF.Y),
                              Round(ButtonRectEffectiveF.X + ButtonRectEffectiveF.Width),
                              Round(ButtonRectEffectiveF.Y + ButtonRectEffectiveF.Height));
    if LDrawBorder and (LActualBorderThickness > 0) then
        InflateRect(LImageClipRect, -Round(LActualBorderThickness), -Round(LActualBorderThickness));


    if (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty then // Use FImageSettings.Picture
    begin
      LImgW := FImageSettings.Picture.Width; LImgH := FImageSettings.Picture.Height; // Use FImageSettings.Picture
      case FImageSettings.DrawMode of // Use FImageSettings.DrawMode
        idmProportional: // Changed from ismProportional
        begin
          if (LImgW = 0) or (LImgH = 0) then begin LDrawW := 0; LDrawH := 0; end
          else
          begin
            AvailableWidth  := Max(0, LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right); // Use FImageSettings.Margins
            AvailableHeight := Max(0, LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom); // Use FImageSettings.Margins
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
        idmStretch: // Changed from ismFlat
        begin
          LDrawW := Max(0, LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right); // Use FImageSettings.Margins
          LDrawH := Max(0, LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom); // Use FImageSettings.Margins
        end;
      else LDrawW := LImgW; LDrawH := LImgH; // Default case if FImageSettings.DrawMode is neither
      end;

      if (LImgW > 0) and (LDrawW <=0) then LDrawW := Min(LImgW, Max(0, LImageClipRect.Width div 3));
      if (LImgH > 0) and (LDrawH <=0) then LDrawH := Min(LImgH, Max(0, LImageClipRect.Height div 3));

      case FImagePosition of
        ipLeft:
        begin
          LImgX := LImageClipRect.Left + FImageSettings.Margins.Left; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Top + FImageSettings.Margins.Top + (Max(0,LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom - LDrawH)) div 2; // Use FImageSettings.Margins
          LTextArea := Rect(LImgX + LDrawW + FImageSettings.Margins.Right + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top, // Use FImageSettings.Margins
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        ipRight:
        begin
          LImgX := LImageClipRect.Right - LDrawW - FImageSettings.Margins.Right; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Top + FImageSettings.Margins.Top + (Max(0,LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom - LDrawH)) div 2; // Use FImageSettings.Margins
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImgX - FImageSettings.Margins.Left - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom); // Use FImageSettings.Margins
        end;
        ipAbove:
        begin
          LImgX := LImageClipRect.Left + FImageSettings.Margins.Left + (Max(0,LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right - LDrawW)) div 2; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Top + FImageSettings.Margins.Top; // Use FImageSettings.Margins
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImgY + LDrawH + FImageSettings.Margins.Bottom + FTextMargins.Top, // Use FImageSettings.Margins
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        ipBelow:
        begin
          LImgX := LImageClipRect.Left + FImageSettings.Margins.Left + (Max(0,LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right - LDrawW)) div 2; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Bottom - LDrawH - FImageSettings.Margins.Bottom; // Use FImageSettings.Margins
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImgY - FImageSettings.Margins.Top - FTextMargins.Bottom); // Use FImageSettings.Margins
        end;
        ipBehind:
        begin
          LImgX := LImageClipRect.Left + FImageSettings.Margins.Left + (Max(0,LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right - LDrawW)) div 2; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Top + FImageSettings.Margins.Top + (Max(0,LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom - LDrawH)) div 2; // Use FImageSettings.Margins
          LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
      else // Default to ipLeft
        begin
          LImgX := LImageClipRect.Left + FImageSettings.Margins.Left; // Use FImageSettings.Margins
          LImgY := LImageClipRect.Top + FImageSettings.Margins.Top + (Max(0,LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom - LDrawH)) div 2; // Use FImageSettings.Margins
          LTextArea := Rect(LImgX + LDrawW + FImageSettings.Margins.Right + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top, // Use FImageSettings.Margins
                            LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
      end;
      LDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

      if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.HoverEffect = heScale) and (LHoverProgress > 0) then
      begin
        LScaleFactor := 1 + (LHoverProgress * (1.05 - 1));
        InflateRect(LDestRect, Round(LDrawW * (LScaleFactor - 1) / 2), Round(LDrawH * (LScaleFactor - 1) / 2));
      end;

      if (LDestRect.Right > LDestRect.Left) and (LDestRect.Bottom > LDestRect.Top) then
      begin
        // LDestRect is ALREADY calculated based on FImageSettings.DrawMode.
        // The drawing helpers should stretch the source image into this pre-calculated LDestRect.
        if FImageSettings.Picture.Graphic is TPNGImage then // Use FImageSettings.Picture
        begin
          DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, LDestRect, idmStretch); // Always stretch into LDestRect // Use FImageSettings.Picture
        end
        else if FImageSettings.Picture.Graphic <> nil then // Use FImageSettings.Picture
        begin
          DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, LDestRect, idmStretch); // Always stretch into LDestRect // Use FImageSettings.Picture
        end;
      end;
    end
    else // No image
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
      LCurrentTitleFont := TFont.Create;
      try
        LCurrentTitleFont.Assign(FCaptionSettings.Font); // Use FCaptionSettings.Font

        if Enabled then
        begin
          if GetEnableHoverEffect and (LHoverProgress > 0) then
          begin
            if FInternalHoverSettings.FontColor <> clNone then
              LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, FInternalHoverSettings.FontColor, LHoverProgress) // Use FCaptionSettings.Font.Color
            else if FInternalHoverSettings.HoverEffect = heFade then
              LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, LighterColor(LActualFillColor, 80), LHoverProgress * 0.5); // Use FCaptionSettings.Font.Color

            if FInternalHoverSettings.HoverEffect = heScale then
              LCurrentTitleFont.Size := Round(FCaptionSettings.Font.Size * (1 + LHoverProgress * (1.05 - 1))); // Use FCaptionSettings.Font.Size
          end;

          if FClickEffectActive and (LClickProgress > 0) and (FClickEffectDuration > 0) then
          begin
            if FClickTitleColor <> clNone then
              LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickTitleColor, LClickProgress);
          end;
        end
        else
        begin
          LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, clGray, 0.6); // Use FCaptionSettings.Font.Color
        end;

        if LTextArea.Right < LTextArea.Left then LTextArea.Right := LTextArea.Left;
        if LTextArea.Bottom < LTextArea.Top then LTextArea.Bottom := LTextArea.Top;
        LTextArea.Left   := Max(LImageClipRect.Left, LTextArea.Left);
        LTextArea.Top    := Max(LImageClipRect.Top, LTextArea.Top);
        LTextArea.Right  := Min(LImageClipRect.Right, LTextArea.Right);
        LTextArea.Bottom := Min(LImageClipRect.Bottom, LTextArea.Bottom);

        if (LTextArea.Width > 0) and (LTextArea.Height > 0) then
        begin
          DrawComponentCaption( Self.Canvas, LTextArea, LFinalCaptionToDraw, LCurrentTitleFont, LCurrentTitleFont.Color, FCaptionSettings.Alignment, cvaCenter, False, 255 ); // Use FCaptionSettings.Alignment
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
