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
  TImageStretchMode = (ismProportional, ismFlat);
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsGradient, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);
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
    FGradientSettings: TGradientSettings;
    FImagePosition: TImagePosition;
    FTextMargins: TANDMR_Margins; // FImageMargins removed
    FTag: Integer;
    FInternalTagString: TANDMR_TagString;
    FInternalTagExtended: TANDMR_TagExtended;
    FInternalTagObject: TANDMR_TagObject;
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

    // New fields for progress animation
    FProcessing: Boolean;
    FProgressTimer: TTimer;
    FProgressStep: Integer;
    FProgressSettings: TProgressSettings; // Added
    FOriginalCaption: string;
    FOriginalEnabledState: Boolean; // New field
    // End of new fields

    procedure SetProgressSettings(const Value: TProgressSettings); // Added

    procedure ProgressTimerHandler(Sender: TObject); // Added
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
    function GetGradientEnabled: Boolean;
    procedure SetGradientEnabled(const Value: Boolean);
    function GetGradientType: TGradientType;
    procedure SetGradientType(const Value: TGradientType);
    function GetGradientStartColor: TColor;
    procedure SetGradientStartColor(const Value: TColor);
    function GetGradientEndColor: TColor;
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetImagePosition(const Value: TImagePosition);
    function GetImageStretchMode: TImageStretchMode; // Changed
    procedure SetImageStretchMode(const Value: TImageStretchMode);
    procedure SetTag(const Value: Integer);
    function GetTagString: string;
    procedure SetTagString(const Value: string);
    function GetTagExtended: Extended;
    function GetTagObject: TObject;
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
    procedure StartProcessing; // New public method
    procedure StopProcessing; // New public method
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

    property GradientEnabled: Boolean read GetGradientEnabled write SetGradientEnabled default False;
    property GradientType: TGradientType read GetGradientType write SetGradientType default gtLinearVertical;
    property GradientStartColor: TColor read GetGradientStartColor write SetGradientStartColor; // Default clNone is handled by TGradientSettings
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor; // Default clNone is handled by TGradientSettings

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
    property TagString: string read GetTagString write SetTagString;
    property TagExtended: Extended read GetTagExtended write SetTagExtended;
    property TagObject: TObject read GetTagObject write SetTagObject;

    property PresetType: TPresetType read FPresetType write SetPresetType default cptNone;

    property ProgressSettings: TProgressSettings read FProgressSettings write SetProgressSettings;

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

  FGradientSettings := TGradientSettings.Create;
  FGradientSettings.OnChange := SettingsChanged;

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

  FInternalTagString := TANDMR_TagString.Create;
  FInternalTagExtended := TANDMR_TagExtended.Create;
  FInternalTagObject := TANDMR_TagObject.Create;

  FProgressSettings := TProgressSettings.Create(Self);
  FProgressSettings.OnChange := SettingsChanged;

  // Initializations for progress animation
  FProcessing := False;
  FProgressTimer := TTimer.Create(Self);
  FProgressTimer.Enabled := False;
  FProgressTimer.Interval := FProgressSettings.AnimationTimerInterval;
  FProgressTimer.OnTimer := ProgressTimerHandler;
  FOriginalEnabledState := True;
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

  if Assigned(FProgressSettings) then
  begin
    FProgressSettings.OnChange := nil;
    FProgressSettings.Free;
    FProgressSettings := nil;
  end;
  FProgressTimer.Free; // Added

  FInternalTagString.Free;
  FInternalTagExtended.Free;
  FInternalTagObject.Free;

  FGradientSettings.OnChange := nil;
  FGradientSettings.Free;

  inherited;
end;

procedure TANDMR_CButton.StartProcessing;
begin
  if FProgressSettings.ShowProgress and not FProcessing then
  begin
    FProcessing := True;
    FOriginalCaption := Self.Caption;
    FOriginalEnabledState := Self.Enabled;

    if FProgressSettings.HideCaptionWhileProcessing then
      Self.Caption := '';

    if Self.Enabled then // Only change if it was enabled
      Self.Enabled := False;

    FProgressStep := 0;
    FProgressTimer.Enabled := True;
    Repaint;
  end;
end;

procedure TANDMR_CButton.StopProcessing;
begin
  if FProcessing then // Only act if processing was active
  begin
    FProcessing := False;
    FProgressTimer.Enabled := False;

    Self.Caption := FOriginalCaption; // Restore caption

    // Restore the button's original enabled state
    if Self.Enabled <> FOriginalEnabledState then
      Self.Enabled := FOriginalEnabledState;

    Repaint;
  end;
end;

procedure TANDMR_CButton.SetProgressSettings(const Value: TProgressSettings);
begin
  FProgressSettings.Assign(Value);
end;

procedure TANDMR_CButton.ProgressTimerHandler(Sender: TObject);
begin
  if FProcessing then // Only do this if we are in the processing state
  begin
    Inc(FProgressStep);
    // Optional: Cap or cycle FProgressStep if the animation has a fixed number of frames.
    // For example, if it's a 12-step spinner:
    // if FProgressStep >= 12 then FProgressStep := 0;
    Repaint;
  end
  else
  begin
    // If not processing, ensure the timer is stopped.
    // This is a safeguard, as StopProcessing should handle it.
    FProgressTimer.Enabled := False;
  end;
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
  if FGradientSettings.StartColor = clNone then
    FGradientSettings.StartColor := FBorderSettings.BackgroundColor;
  if FGradientSettings.EndColor = clNone then
    FGradientSettings.EndColor := DarkerColor(FBorderSettings.BackgroundColor, 30);
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
    FBorderSettings.BackgroundColor := Value; // This will trigger FBorderSettings.OnChange -> BorderSettingsChanged -> Invalidate

    // Use the property access to ensure OnChange is triggered for gradient settings
    if (FGradientSettings.StartColor = OldActiveColor) or (FGradientSettings.StartColor = clNone) then
      FGradientSettings.StartColor := FBorderSettings.BackgroundColor;

    OldDerivedEndColor := DarkerColor(OldActiveColor, 30);
    if (FGradientSettings.EndColor = OldDerivedEndColor) or (FGradientSettings.EndColor = clNone) then
      FGradientSettings.EndColor := DarkerColor(FBorderSettings.BackgroundColor, 30);
    // FGradientSettings.OnChange will trigger repaint if its properties were changed
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

function TANDMR_CButton.GetGradientEnabled: Boolean;
begin
  Result := FGradientSettings.Enabled;
end;

procedure TANDMR_CButton.SetGradientEnabled(const Value: Boolean);
begin
  FGradientSettings.Enabled := Value; // OnChange will trigger repaint
end;

function TANDMR_CButton.GetGradientType: TGradientType;
begin
  Result := FGradientSettings.GradientType;
end;

procedure TANDMR_CButton.SetGradientType(const Value: TGradientType);
begin
  FGradientSettings.GradientType := Value; // OnChange will trigger repaint
end;

function TANDMR_CButton.GetGradientStartColor: TColor;
begin
  Result := FGradientSettings.StartColor;
end;

procedure TANDMR_CButton.SetGradientStartColor(const Value: TColor);
begin
  FGradientSettings.StartColor := Value; // OnChange will trigger repaint
end;

function TANDMR_CButton.GetGradientEndColor: TColor;
begin
  Result := FGradientSettings.EndColor;
end;

procedure TANDMR_CButton.SetGradientEndColor(const Value: TColor);
begin
  FGradientSettings.EndColor := Value; // OnChange will trigger repaint
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

function TANDMR_CButton.GetTagString: string;
begin
  Result := FInternalTagString.AsString;
end;

procedure TANDMR_CButton.SetTagString(const Value: string);
begin
  FInternalTagString.AsString := Value;
  // Tags usually don't trigger repaint, but if an OnChange mechanism is needed later,
  // FInternalTagString.OnChange could be assigned here or in Create.
end;

function TANDMR_CButton.GetTagExtended: Extended;
begin
  // TANDMR_TagExtended stores a TStringList. The original FTagExtended was an Extended type.
  // This indicates a mismatch in the plan or understanding.
  // For now, let's assume TagExtended should map to the first item in TANDMR_TagExtended.Items if it's a valid float,
  // or handle it as string if that's more appropriate.
  // Given the original was 'Extended', we'll try to convert.
  if FInternalTagExtended.Items.Count > 0 then
  begin
    try
      Result := StrToFloatDef(FInternalTagExtended.Items[0], 0.0);
    except
      Result := 0.0; // Default on conversion error
    end;
  end
  else
    Result := 0.0;
end;

procedure TANDMR_CButton.SetTagExtended(const Value: Extended);
begin
  FInternalTagExtended.Items.Clear;
  FInternalTagExtended.Items.Add(FloatToStr(Value));
end;

function TANDMR_CButton.GetTagObject: TObject;
begin
  Result := FInternalTagObject.ObjectValue;
end;

procedure TANDMR_CButton.SetTagObject(const Value: TObject);
begin
  FInternalTagObject.ObjectValue := Value;
end;

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
  LAnimationStyle: TProgressAnimationStyle;
  LProgressText: string;
  LShowProgressText: Boolean;
  DotYOffset: array[0..2] of Integer; // For DotCount = 3

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
    LAnimationStyle := FProgressSettings.AnimationStyle;
    LProgressText := FProgressSettings.ProgressText;
    LShowProgressText := FProgressSettings.ShowProgressText;

    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    LHoverProgress := 0;
    if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.CurrentAnimationValue > 0) and (FInternalHoverSettings.HoverEffect <> heNone) and not FProcessing then // Added not FProcessing
      LHoverProgress := FInternalHoverSettings.CurrentAnimationValue / 255.0;

    LClickProgress := 0;
    if Enabled and FClickEffectActive and (FClickEffectProgress <= 255) and (FClickEffectDuration > 0) and not FProcessing then // Added not FProcessing
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
    LCurrentGradientEnabled := FGradientSettings.Enabled;
    LDrawFill := True;
    LDrawBorder := LActualBorderThickness > 0;

    case FStyle of
      bsSolid:
      begin
        // Ensure bsSolid is truly solid, ignoring FGradientEnabled.
        LCurrentGradientEnabled := False;
        // LActualFillColor is LInitialFillColor (from FBorderSettings.BackgroundColor)
        // LActualBorderColor is LInitialBorderColor (from FBorderSettings.Color)
        // LActualBorderThickness is FBorderSettings.Thickness (inherited)
        // LDrawFill is True, LDrawBorder is True (if thickness > 0) (inherited)
      end;
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
        // Ensure border thickness respects FBorderSettings.Thickness but is at least 1 for this style.
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LDrawBorder := LActualBorderThickness > 0; // Should be true if thickness > 0
        LFinalHoverColor := ColorToARGB(IfThen(GetHoverColor=clNone, LInitialFillColor, GetHoverColor), 70); // Fill on hover
        // LActualBorderColor remains LInitialBorderColor
      end;
      bsLight:
      begin
        LBaseStyleColor := BlendColors(LInitialFillColor, clWhite, 0.6);
        LActualFillColor := LBaseStyleColor;
        // LActualBorderColor remains LInitialBorderColor (FBorderSettings.Color)
        LFinalHoverColor := BlendColors(LBaseStyleColor, LighterColor(LInitialFillColor, 20), 0.7);
        LCurrentGradientEnabled := False;
        // Ensure border thickness respects FBorderSettings.Thickness but is at least 1 for this style.
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LDrawBorder := LActualBorderThickness > 0; // Should be true if thickness > 0
      end;
      bsFlat:
      begin
        LCurrentGradientEnabled := False;
        LActualBorderThickness := 0;
        LDrawBorder := False;
        // LActualFillColor is LInitialFillColor
        LFinalHoverBorderColor := LInitialFillColor; // On hover, a border appears using the fill color
        // Click colors will use standard logic based on LActualFillColor
      end;
      bsGhost:
      begin
        LDrawFill := False;
        LCurrentGradientEnabled := False;
        // Border should use FBorderSettings.Thickness but be at least 1. Its color is LInitialFillColor.
        LActualBorderThickness := Max(1, FBorderSettings.Thickness);
        LActualBorderColor := LInitialFillColor; // Border takes the base fill color
        LDrawBorder := LActualBorderThickness > 0; // Should be true if thickness > 0
        LFinalHoverColor := ColorToARGB(IfThen(GetHoverColor=clNone, LInitialFillColor, GetHoverColor), 100); // Fill on hover
        LFinalHoverBorderColor := LInitialFillColor; // Border color on hover remains the same
      end;
      bsShadow:
      begin
        // This style primarily enables shadow rendering via the common shadow logic.
        // If transparent, the fill is disabled to make it a pure shadow effect.
        if FTransparent then
        begin
          LDrawFill := False;
          LCurrentGradientEnabled := False; // No fill means no gradient on fill
        end;
        // LActualFillColor is LInitialFillColor (relevant if not transparent)
        // LActualBorderColor is LInitialBorderColor (relevant for path shape for shadow)
        // LActualBorderThickness is FBorderSettings.Thickness (relevant for path shape for shadow)
      end;
      bsGradient:
      begin
        LCurrentGradientEnabled := True; // Force gradient
        LDrawFill := True;
        // LActualFillColor is LInitialFillColor (used as gradient start if FGradientStartColor is clNone)
        // LActualBorderColor is LInitialBorderColor
        // LActualBorderThickness is FBorderSettings.Thickness
        LDrawBorder := LActualBorderThickness > 0; // Draw border if thickness > 0
      end;
      bsDark:
      begin
        // Base Colors for Dark Style
        LBaseStyleColor := DarkerColor(FBorderSettings.BackgroundColor, 60); // Significantly darken the base active color
        // For now, using a fixed dark color as a fallback if the base color isn't suitable, as per instructions.
        // A more sophisticated IsColorVeryDark check could be added later if needed.
        if (GetRValue(LBaseStyleColor) < 30) and (GetGValue(LBaseStyleColor) < 30) and (GetBValue(LBaseStyleColor) < 30) then
            LBaseStyleColor := TColor($FF383838); // A specific dark gray

        LActualFillColor := LBaseStyleColor;
        LActualBorderColor := LighterColor(LBaseStyleColor, 20); // Border slightly lighter than fill
        LCurrentGradientEnabled := False;
        LDrawFill := True;
        LDrawBorder := True; // Assuming a subtle border for dark style
        LActualBorderThickness := Max(1, FBorderSettings.Thickness); // Ensure border is at least 1px for bsDark

        // Hover Colors for Dark Style
        if FInternalHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FInternalHoverSettings.BackgroundColor // Allow user override
        else
          LFinalHoverColor := LighterColor(LBaseStyleColor, 15);

        if FInternalHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FInternalHoverSettings.BorderColor // Allow user override
        else
          LFinalHoverBorderColor := LighterColor(LActualBorderColor, 15);

        // Click Colors for Dark Style (subtly different from base dark)
        LFinalClickColor := IfThen(FClickColor = clNone, LighterColor(LBaseStyleColor, 10), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, LighterColor(LActualBorderColor, 10), FClickBorderColor);
      end;
      bsMaterial:
      begin
        // Material Design uses the primary color, often from presets.
        LActualFillColor := FBorderSettings.BackgroundColor; // Base color from ActiveColor
        LActualBorderColor := clNone; // Material buttons often have no border or a very subtle one
        LActualBorderThickness := 0; // No border or minimal
        LDrawBorder := False;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        // Hover: Slightly lighter version of the fill color or a standard overlay
        if FInternalHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FInternalHoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 10); // Subtle lighten

        // Click: Material has a ripple; for now, use color feedback.
        // A darker shade or a standard overlay.
        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 10), FClickColor);

        // Shadow properties for Material look (will be used by the common shadow drawing logic)
        // Note: LShadowOffsetXToUse, LShadowOffsetYToUse, LShadowAlphaToUse are declared outside this case block.
        // Their values will be set here and used by the shadow drawing logic later in the Paint method.
        // This section is for setting parameters; the actual shadow drawing condition modification is separate.
      end;
      bsModern:
      begin
        // Modern style: Clean, often flat with emphasis on typography and subtle interactions.
        LActualFillColor := FBorderSettings.BackgroundColor;
        // Border can be a slightly darker version of the fill or a neutral tone.
        LActualBorderColor := DarkerColor(LActualFillColor, 15);
        LActualBorderThickness := 1; // Clean 1px border
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        // Hover: Subtle changes. Fill might lighten, border might change color or intensity.
        if FInternalHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FInternalHoverSettings.BackgroundColor
        else
          LFinalHoverColor := LighterColor(LActualFillColor, 8); // Very subtle lighten

        if FInternalHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FInternalHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := LActualFillColor; // Border matches fill on hover for a softer look, or could be a highlight color

        // Click: Subtle darkening or clear feedback.
        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 8), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualFillColor, 20), FClickBorderColor);

        // No shadow for this style. The existing shadow logic is conditioned for bsShadow or bsMaterial.
      end;
      bsWindows:
      begin
        // Windows (Fluent-inspired): Light base, subtle border.
        LActualFillColor := TColor($FFEFEFEF); // Light gray
        LActualBorderColor := TColor($FFDCDCDC); // Slightly darker gray for border
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        // Hover: Lighten fill slightly, border might use ActiveColor or similar highlight
        if FInternalHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FInternalHoverSettings.BackgroundColor
        else
          LFinalHoverColor := TColor($FFF5F5F5); // Even lighter gray

        if FInternalHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FInternalHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := FBorderSettings.BackgroundColor; // Accent color for border on hover

        // Click: Darken fill
        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 10), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualBorderColor, 10), FClickBorderColor);
        // No shadow for this style in this iteration.
      end;
      bsMacOS:
      begin
        // macOS-inspired: Often light gray or themed, subtle changes on interaction.
        LActualFillColor := TColor($FFF2F2F7); // Common macOS light/medium gray
        LActualBorderColor := TColor($FFD1D1D6); // Subtle border, slightly darker than fill
        LActualBorderThickness := 1;
        LDrawBorder := True;
        LCurrentGradientEnabled := False;
        LDrawFill := True;

        // Hover: Fill slightly darkens
        if FInternalHoverSettings.BackgroundColor <> clNone then
          LFinalHoverColor := FInternalHoverSettings.BackgroundColor
        else
          LFinalHoverColor := DarkerColor(LActualFillColor, 5); // Subtle darken

        if FInternalHoverSettings.BorderColor <> clNone then
          LFinalHoverBorderColor := FInternalHoverSettings.BorderColor
        else
          LFinalHoverBorderColor := DarkerColor(LActualBorderColor, 5);

        // Click: Fill darkens more noticeably
        LFinalClickColor := IfThen(FClickColor = clNone, DarkerColor(LActualFillColor, 12), FClickColor);
        LFinalClickBorderColor := IfThen(FClickBorderColor = clNone, DarkerColor(LActualBorderColor, 12), FClickBorderColor);
        // No shadow for this style in this iteration.
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

    // Shadow configuration for bsMaterial - needs to be done before the main shadow block
    if (FStyle = bsMaterial) and (not FTransparent) then
    begin
      // Default shadow properties for Material style
      LShadowOffsetXToUse := 1;
      LShadowOffsetYToUse := 2;
      LShadowAlphaToUse := 60;

      const  MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR = 1.8;
      const  MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR = 1.8;
      const  MATERIAL_SHADOW_ALPHA_HOVER = 90;

      if (LHoverProgress > 0) and Enabled and GetEnableHoverEffect then
      begin
        // Calculate hover shadow properties for Material style
        var TempShadowOffsetXConst_Material: Single; // Renamed to avoid conflict
        var TempShadowOffsetYConst_Material: Single; // Renamed to avoid conflict
        TempShadowOffsetXConst_Material := 1; // Base X for Material
        TempShadowOffsetYConst_Material := 2; // Base Y for Material

        LShadowOffsetXToUse := TempShadowOffsetXConst_Material + ((TempShadowOffsetXConst_Material * MATERIAL_SHADOW_OFFSET_X_HOVER_FACTOR) - TempShadowOffsetXConst_Material) * LHoverProgress;
        LShadowOffsetYToUse := TempShadowOffsetYConst_Material + ((TempShadowOffsetYConst_Material * MATERIAL_SHADOW_OFFSET_Y_HOVER_FACTOR) - TempShadowOffsetYConst_Material) * LHoverProgress;
        LShadowAlphaToUse := Round(LShadowAlphaToUse + (MATERIAL_SHADOW_ALPHA_HOVER - LShadowAlphaToUse) * LHoverProgress);
      end;
    end;

    if ((FStyle = bsShadow) or (FStyle = bsMaterial)) and (not FTransparent) then
    begin
      // If bsShadow, set its specific shadow parameters
      if FStyle = bsShadow then
      begin
        LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST;
        LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST;
        LShadowAlphaToUse := SHADOW_ALPHA;

        // Hover adjustments specifically for bsShadow
        if (LHoverProgress > 0) and Enabled and GetEnableHoverEffect then
        begin
          LShadowOffsetXToUse := SHADOW_OFFSET_X_CONST + ((SHADOW_OFFSET_X_CONST * SHADOW_OFFSET_X_HOVER_FACTOR) - SHADOW_OFFSET_X_CONST) * LHoverProgress;
          LShadowOffsetYToUse := SHADOW_OFFSET_Y_CONST + ((SHADOW_OFFSET_Y_CONST * SHADOW_OFFSET_Y_HOVER_FACTOR) - SHADOW_OFFSET_Y_CONST) * LHoverProgress;
          LShadowAlphaToUse := Round(SHADOW_ALPHA + (SHADOW_ALPHA_HOVER - SHADOW_ALPHA) * LHoverProgress);
        end;
      end;
      // For bsMaterial, LShadowOffsetXToUse, LShadowOffsetYToUse, LShadowAlphaToUse are already set by the dedicated block above, including its hover effect.

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
        BgColorToUse := IfThen(FGradientSettings.StartColor = clNone, LActualFillColor, FGradientSettings.StartColor)
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

    if FImageSettings.Placement = iplInsideBounds then // iplInsideBounds is the default historical behavior
    begin
      if LDrawBorder and (LActualBorderThickness > 0) then
          InflateRect(LImageClipRect, -Round(LActualBorderThickness), -Round(LActualBorderThickness));
    end;
    // For iplOutsideBounds, LImageClipRect remains the full ButtonRectEffectiveF.
    // Margins will then position the image within this full rect.

    // >>> START NEW PROGRESS ANIMATION LOGIC <<<
    if FProcessing and FProgressSettings.ShowProgress then
    begin
      var LProgressRect: TRect;
      var LArcThickness: Integer;
      var LStartAngle, LSweepAngle: Single;
      var LProgressBarPen: TGPPen;
      var LProgressPath: TGPGraphicsPath;
      var ArcRectF: TGPRectF;
      var OriginalProgressRect: TRect; // To store the initial calculation

      LProgressRect := ClientRect; // Start with full client rect
      if FBorderSettings.Thickness > 0 then
        InflateRect(LProgressRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

      OriginalProgressRect := LProgressRect; // Save this before potential adjustments for text

      // If text is shown, adjust LProgressRect to make space for the animation.
      // This example assumes animation on the left, text on the right.
      // A more sophisticated layout might be needed for other text positions.
      if LShowProgressText and (LProgressText <> '') then
      begin
        // Attempt to reserve about 40% for animation, 60% for text, if wide enough
        if LProgressRect.Width > 100 then // Only if button is reasonably wide
        begin
          OriginalProgressRect.Right := LProgressRect.Left + Round(LProgressRect.Width * 0.4); // Animation takes left 40%
          LProgressRect := OriginalProgressRect; // Animation will be drawn in this potentially smaller rect
        end
        else // Button too narrow, text might go below or animation shrinks a lot
        begin
           // For narrow buttons, perhaps animation shrinks more, or text is prioritized differently
           InflateRect(LProgressRect, -Round(LProgressRect.Width * 0.1), -Round(LProgressRect.Height * 0.1)); // General shrink
        end;
      end;

      // Make the animation area square-ish for circular/dot animations
      if LAnimationStyle in [pasRotatingSemiCircle, pasFullCircularSpinner, pasBouncingDots] then
      begin
        if LProgressRect.Width > LProgressRect.Height then
        begin
            LProgressRect.Left := LProgressRect.Left + (LProgressRect.Width - LProgressRect.Height) div 2;
            LProgressRect.Width := LProgressRect.Height;
        end
        else
        begin
            LProgressRect.Top := LProgressRect.Top + (LProgressRect.Height - LProgressRect.Width) div 2;
            LProgressRect.Height := LProgressRect.Width;
        end;
        // Reduce size slightly to give some padding for circular/dot animations
        InflateRect(LProgressRect, -Max(2, Round(Min(LProgressRect.Width, LProgressRect.Height) * 0.1)), -Max(2, Round(Min(LProgressRect.Width, LProgressRect.Height) * 0.1)));
      end;


      case LAnimationStyle of
        pasRotatingSemiCircle:
        begin
          if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
          begin
            LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
            LStartAngle := (FProgressStep * 10) mod 360;
            LSweepAngle := 270;
            LProgressPath := TGPGraphicsPath.Create;
            try
              ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
              if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
              begin
                LProgressPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                LProgressBarPen.SetStartCap(LineCapRound);
                LProgressBarPen.SetEndCap(LineCapRound);
                try
                  LG.DrawPath(LProgressBarPen, LProgressPath);
                finally
                  LProgressBarPen.Free;
                end;
              end;
            finally
              LProgressPath.Free;
            end;
          end;
        end;
        pasFullCircularSpinner:
        begin
          if (LProgressRect.Width > 4) and (LProgressRect.Height > 4) then
          begin
            LArcThickness := Max(2, Min(LProgressRect.Width, LProgressRect.Height) div 8);
            LStartAngle := (FProgressStep * 12) mod 360;
            LSweepAngle := 90;
            LProgressPath := TGPGraphicsPath.Create;
            try
              ArcRectF := MakeRect(LProgressRect.Left + LArcThickness / 2, LProgressRect.Top + LArcThickness / 2, LProgressRect.Width - LArcThickness, LProgressRect.Height - LArcThickness);
              if (ArcRectF.Width > 0) and (ArcRectF.Height > 0) then
              begin
                LProgressPath.AddArc(ArcRectF, LStartAngle, LSweepAngle);
                LProgressBarPen := TGPPen.Create(ColorToARGB(FProgressSettings.ProgressColor, 255), LArcThickness);
                LProgressBarPen.SetStartCap(LineCapRound);
                LProgressBarPen.SetEndCap(LineCapRound);
                try
                  LG.DrawPath(LProgressBarPen, LProgressPath);
                finally
                  LProgressBarPen.Free;
                end;
              end;
            finally
              LProgressPath.Free;
            end;
          end;
        end;
        pasHorizontalBar:
        begin
          var BarRect: TRect;
          var InnerBarWidth, InnerBarX: Integer;
          // Use OriginalProgressRect for horizontal bar as it might span more width if text is also present
          BarRect := OriginalProgressRect;
          if LShowProgressText and (LProgressText <> '') and (OriginalProgressRect.Width > 100) then
          begin
            // If text is shown, the animation area (OriginalProgressRect) was already adjusted
            // The horizontal bar should fit within this adjusted OriginalProgressRect (which is LProgressRect here)
             BarRect := LProgressRect; // Use the already adjusted rect for animation
          end;

          InflateRect(BarRect, 0, -BarRect.Height div 3);
          if BarRect.Height < 4 then BarRect.Height := Max(2, Min(LProgressRect.Height, 4)); // Ensure min height but not exceeding LProgressRect
          if BarRect.Width > 10 then
          begin
            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 100));
            try
              LG.FillRectangle(LGPBrush, BarRect.Left, BarRect.Top, BarRect.Width, BarRect.Height);
            finally
              LGPBrush.Free;
            end;
            InnerBarWidth := BarRect.Width div 3;
            if BarRect.Width - InnerBarWidth > 0 then // Ensure InnerBarX calculation is valid
                InnerBarX := (FProgressStep * 5) mod (BarRect.Width - InnerBarWidth)
            else
                InnerBarX := 0;

            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
            try
              LG.FillRectangle(LGPBrush, BarRect.Left + InnerBarX, BarRect.Top, InnerBarWidth, BarRect.Height);
            finally
              LGPBrush.Free;
            end;
          end;
        end;
        pasBouncingDots:
        begin
          const DotCount = 3;
          var DotSize, DotSpacing, TotalDotWidth, StartX, BaseY: Integer;
          // var DotYOffset: array[0..DotCount-1] of Integer; // Moved to main var block
          var i: Integer;

          if (LProgressRect.Width > 0) and (LProgressRect.Height > 0) then // Ensure LProgressRect is valid
          begin
            DotSize := Max(4, Min(LProgressRect.Width div Max(1, (DotCount * 2)), LProgressRect.Height div 2)); // Adjusted DotSize
            DotSpacing := DotSize div 2;
            TotalDotWidth := (DotCount * DotSize) + ((DotCount - 1) * DotSpacing);

            if TotalDotWidth > LProgressRect.Width then // If dots are too wide, shrink them
            begin
                DotSize := Max(2, LProgressRect.Width div (DotCount * 2));
                DotSpacing := DotSize div 3;
                TotalDotWidth := (DotCount * DotSize) + ((DotCount - 1) * DotSpacing);
            end;

            StartX := LProgressRect.Left + (LProgressRect.Width - TotalDotWidth) div 2;
            BaseY := LProgressRect.Top + (LProgressRect.Height - DotSize) div 2;

            for i := 0 to DotCount - 1 do
            begin
              DotYOffset[i] := Round( (DotSize / 2) * Sin( (FProgressStep * 0.2 + i * (PI/DotCount))) );
            end;

            LGPBrush := TGPSolidBrush.Create(ColorToARGB(FProgressSettings.ProgressColor, 255));
            try
              for i := 0 to DotCount - 1 do
              begin
                LG.FillEllipse(LGPBrush, StartX + i * (DotSize + DotSpacing),
                                         BaseY + DotYOffset[i],
                                         DotSize, DotSize);
              end;
            finally
              LGPBrush.Free;
            end;
          end;
        end;
      end; // case LAnimationStyle

      // Draw Progress Text
      if LShowProgressText and (LProgressText <> '') then
      begin
        var TextRect: TRect;
        var ProgressCaptionFont: TFont;
        var AnimationAreaRightBound: Integer;

        // Calculate TextRect based on where the animation was drawn (LProgressRect)
        // and the original full progress area (OriginalProgressRect)
        if (OriginalProgressRect.Width > 100) and (LAnimationStyle <> pasHorizontalBar) then // If wide and not full-width bar
        begin
            AnimationAreaRightBound := LProgressRect.Left + LProgressRect.Width; // Right edge of (possibly shrunk) animation area
            TextRect.Left := AnimationAreaRightBound + Self.FTextMargins.Left;
            TextRect.Top  := OriginalProgressRect.Top; // Align with top of original progress area
            TextRect.Right := OriginalProgressRect.Left + OriginalProgressRect.Width - Self.FTextMargins.Right; // Use full original width for text
            TextRect.Bottom := OriginalProgressRect.Bottom; // Align with bottom of original progress area
        end
        else if (LAnimationStyle = pasHorizontalBar) and (OriginalProgressRect.Width > 100) then // Horizontal bar might be full width
        begin
            // For horizontal bar, text might go below, or overlay if very short. This example: below.
            TextRect.Left := OriginalProgressRect.Left + Self.FTextMargins.Left;
            TextRect.Top := LProgressRect.Bottom + Self.FTextMargins.Top; // Text below the bar
            TextRect.Right := OriginalProgressRect.Right - Self.FTextMargins.Right;
            TextRect.Bottom := OriginalProgressRect.Bottom; // Extend to bottom of original progress area
        end
        else // Narrow button or other cases: Text centered below animation area
        begin
            TextRect.Left := ClientRect.Left + Self.FTextMargins.Left;
            TextRect.Top := LProgressRect.Bottom + Self.FTextMargins.Top;
            TextRect.Right := ClientRect.Right - Self.FTextMargins.Right;
            TextRect.Bottom := ClientRect.Bottom - Self.FTextMargins.Bottom;
        end;

        if (TextRect.Width > 0) and (TextRect.Height > 0) then
        begin
          ProgressCaptionFont := TFont.Create;
          try
            ProgressCaptionFont.Assign(Self.FCaptionSettings.Font);
            // ProgressCaptionFont.Color := ...; // Optional: Different color for progress text
            DrawComponentCaption(Self.Canvas, TextRect, LProgressText, ProgressCaptionFont, ProgressCaptionFont.Color, taCenter, cvaCenter, False, 255);
          finally
            ProgressCaptionFont.Free;
          end;
        end;
      end;
    end;
    // >>> END NEW PROGRESS ANIMATION LOGIC <<<

    // Conditionally draw image and caption
    if not (FProcessing and FProgressSettings.ShowProgress and FProgressSettings.HideCaptionWhileProcessing) then
    begin
      LImgW := 0; LImgH := 0; LDrawW := 0; LDrawH := 0; LImgX := 0; LImgY := 0;
      AvailableWidth := 0; AvailableHeight := 0;
      var imageCanvasX, imageCanvasY: Integer;
      var imgAspectRatio, availAspectRatio, targetAspectRatio: Single; // Use Single for AspectRatio

      if (FImageSettings.Picture.Graphic <> nil) and not FImageSettings.Picture.Graphic.Empty and FImageSettings.Visible then
      begin
        LImgW := FImageSettings.Picture.Width;
        LImgH := FImageSettings.Picture.Height;

        AvailableWidth  := Max(0, LImageClipRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right);
        AvailableHeight := Max(0, LImageClipRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom);

        if FImageSettings.AutoSize then
        begin
          case FImageSettings.DrawMode of
            idmStretch:
            begin
              LDrawW := AvailableWidth;
              LDrawH := AvailableHeight;
            end;
            idmProportional:
            begin
              if (LImgW = 0) or (LImgH = 0) or (AvailableWidth <= 0) or (AvailableHeight <= 0) then
              begin LDrawW := 0; LDrawH := 0; end
              else
              begin
                imgAspectRatio := LImgW / LImgH;
                availAspectRatio := AvailableWidth / AvailableHeight;
                if availAspectRatio > imgAspectRatio then // Fit to height
                begin
                  LDrawH := AvailableHeight;
                  LDrawW := Round(LDrawH * imgAspectRatio);
                end
                else // Fit to width
                begin
                  LDrawW := AvailableWidth;
                  LDrawH := Round(LDrawW / imgAspectRatio);
                end;
              end;
            end;
            idmNormal:
            begin
              LDrawW := LImgW;
              LDrawH := LImgH;
            end;
          else // Should not happen, but default to normal
            LDrawW := LImgW; LDrawH := LImgH;
          end;
        end
        else // Not AutoSize (use TargetWidth, TargetHeight)
        begin
          var targetW, targetH: Integer;
          targetW := FImageSettings.TargetWidth;
          targetH := FImageSettings.TargetHeight;

          case FImageSettings.DrawMode of
            idmStretch:
            begin
              LDrawW := targetW;
              LDrawH := targetH;
            end;
            idmProportional:
            begin
              if (LImgW = 0) or (LImgH = 0) or (targetW <= 0) or (targetH <= 0) then
              begin LDrawW := 0; LDrawH := 0; end
              else
              begin
                imgAspectRatio := LImgW / LImgH;
                targetAspectRatio := targetW / targetH;
                if targetAspectRatio > imgAspectRatio then // Fit to target height
                begin
                  LDrawH := targetH;
                  LDrawW := Round(LDrawH * imgAspectRatio);
                end
                else // Fit to target width
                begin
                  LDrawW := targetW;
                  LDrawH := Round(LDrawW / imgAspectRatio);
                end;
              end;
            end;
            idmNormal:
            begin
              LDrawW := LImgW;
              LDrawH := LImgH;
              // Optionally clip to target dimensions if idmNormal should not exceed target
              // LDrawW := Min(LDrawW, targetW);
              // LDrawH := Min(LDrawH, targetH);
            end;
          else // Should not happen, but default to normal (respecting original image size)
            LDrawW := LImgW; LDrawH := LImgH;
          end;
        end;

        LDrawW := Max(0, LDrawW);
        LDrawH := Max(0, LDrawH);

        // Clip LDrawW and LDrawH to AvailableWidth and AvailableHeight if they exceed
        // This is important especially for idmNormal or when TargetWidth/Height are larger than available space
        if LDrawW > AvailableWidth then LDrawW := AvailableWidth;
        if LDrawH > AvailableHeight then LDrawH := AvailableHeight;

        imageCanvasX := LImageClipRect.Left + FImageSettings.Margins.Left;
        imageCanvasY := LImageClipRect.Top + FImageSettings.Margins.Top;

        case FImageSettings.HorizontalAlign of
          ihaLeft:   LImgX := imageCanvasX;
          ihaCenter: LImgX := imageCanvasX + (AvailableWidth - LDrawW) div 2;
          ihaRight:  LImgX := imageCanvasX + AvailableWidth - LDrawW;
        else LImgX := imageCanvasX; // Default to left
        end;

        case FImageSettings.VerticalAlign of
          ivaTop:    LImgY := imageCanvasY;
          ivaCenter: LImgY := imageCanvasY + (AvailableHeight - LDrawH) div 2;
          ivaBottom: LImgY := imageCanvasY + AvailableHeight - LDrawH;
        else LImgY := imageCanvasY; // Default to top
        end;

        // Adjust LTextArea based on image position and dimensions
        case FImagePosition of
          ipLeft:
            LTextArea := Rect(LImgX + LDrawW + FImageSettings.Margins.Right + FTextMargins.Left,
                              LImageClipRect.Top + FTextMargins.Top,
                              LImageClipRect.Right - FTextMargins.Right,
                              LImageClipRect.Bottom - FTextMargins.Bottom);
          ipRight:
            LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left,
                              LImageClipRect.Top + FTextMargins.Top,
                              LImgX - FImageSettings.Margins.Left - FTextMargins.Right,
                              LImageClipRect.Bottom - FTextMargins.Bottom);
          ipAbove:
            LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left,
                              LImgY + LDrawH + FImageSettings.Margins.Bottom + FTextMargins.Top,
                              LImageClipRect.Right - FTextMargins.Right,
                              LImageClipRect.Bottom - FTextMargins.Bottom);
          ipBelow:
            LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left,
                              LImageClipRect.Top + FTextMargins.Top,
                              LImageClipRect.Right - FTextMargins.Right,
                              LImgY - FImageSettings.Margins.Top - FTextMargins.Bottom);
          ipBehind: // Text area covers the whole space, image is behind
            LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                              LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
        else // Default to ipLeft behavior for LTextArea
           LTextArea := Rect(LImgX + LDrawW + FImageSettings.Margins.Right + FTextMargins.Left,
                              LImageClipRect.Top + FTextMargins.Top,
                              LImageClipRect.Right - FTextMargins.Right,
                              LImageClipRect.Bottom - FTextMargins.Bottom);
        end;
        LDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

        if Enabled and GetEnableHoverEffect and (FInternalHoverSettings.HoverEffect = heScale) and (LHoverProgress > 0) then
        begin
          LScaleFactor := 1 + (LHoverProgress * (1.05 - 1)); // Example scale factor
          var ScaledW, ScaledH: Integer;
          ScaledW := Round(LDrawW * LScaleFactor);
          ScaledH := Round(LDrawH * LScaleFactor);
          // Adjust LDestRect for scaling, centered on original LImgX, LImgY + LDrawW/2, LDrawH/2
          LDestRect.Left := LImgX + (LDrawW - ScaledW) div 2;
          LDestRect.Top := LImgY + (LDrawH - ScaledH) div 2;
          LDestRect.Right := LDestRect.Left + ScaledW;
          LDestRect.Bottom := LDestRect.Top + ScaledH;
        end;

        if (LDestRect.Right > LDestRect.Left) and (LDestRect.Bottom > LDestRect.Top) then
        begin
          if FImageSettings.Picture.Graphic is TPNGImage then
            DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, LDestRect, idmStretch) // Always stretch
          else if FImageSettings.Picture.Graphic <> nil then
            DrawNonPNGImageWithCanvas(Self.Canvas, FImageSettings.Picture.Graphic, LDestRect, idmStretch); // Always stretch
        end;
      end
      else // No image or image not visible
      begin
        LTextArea := Rect(LImageClipRect.Left + FTextMargins.Left, LImageClipRect.Top + FTextMargins.Top,
                          LImageClipRect.Right - FTextMargins.Right, LImageClipRect.Bottom - FTextMargins.Bottom);
      end;

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

          if Enabled then // Font color/size adjustments based on state (hover, click), only if not processing
          begin
            if GetEnableHoverEffect and (LHoverProgress > 0) and not FProcessing then // Added not FProcessing
            begin
              if FInternalHoverSettings.FontColor <> clNone then
                LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, FInternalHoverSettings.FontColor, LHoverProgress) // Use FCaptionSettings.Font.Color
              else if FInternalHoverSettings.HoverEffect = heFade then
                LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, LighterColor(LActualFillColor, 80), LHoverProgress * 0.5); // Use FCaptionSettings.Font.Color

              if FInternalHoverSettings.HoverEffect = heScale then
                LCurrentTitleFont.Size := Round(FCaptionSettings.Font.Size * (1 + LHoverProgress * (1.05 - 1))); // Use FCaptionSettings.Font.Size
            end;

            if FClickEffectActive and (LClickProgress > 0) and (FClickEffectDuration > 0) and not FProcessing then // Added not FProcessing
            begin
              if FClickTitleColor <> clNone then
                LCurrentTitleFont.Color := BlendColors(LCurrentTitleFont.Color, FClickTitleColor, LClickProgress);
            end;
          end
          else if not Enabled then // Disabled state font color
          begin
            LCurrentTitleFont.Color := BlendColors(FCaptionSettings.Font.Color, clGray, 0.6); // Use FCaptionSettings.Font.Color
          end;
          // If FProcessing, the original FCaptionSettings.Font.Color is used unless hidden

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
    end; // End of "if not (FProcessing and FShowProgress and FHideCaptionWhileProcessing) then"

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
