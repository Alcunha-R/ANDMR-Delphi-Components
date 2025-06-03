unit ANDMR_CMemo;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  ANDMR_ComponentUtils, // Already includes System.Types for TPoint
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.Math;

type
  TANDMR_CMemo = class(TCustomControl)
  private
    // New Settings Objects
    FBorderSettings: TBorderSettings;
    FFocusSettings: TFocusSettings;
    FSeparatorSettings: TSeparatorSettings;
    FImageSettings: TImageSettings; // Contains Picture, Margins, etc.
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FTextMargins: TANDMR_Margins;

    // Fields specific to TANDMR_CMemo or retained temporarily
    FCaptionRect: TRect;
    FHovered: Boolean;
    FActiveColor: TColor; // Retained for now (used for focus border if FFocusSettings.BorderColor is not set)

    FOpacity: Byte;
    FInternalMemo: TMemo;

    // Event Fields
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;

    // Getter/Setter Declarations for Memo Properties
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

    // Getters/Setters for Appearance Properties (delegating to settings objects)
    function GetCornerRadius: Integer;
    procedure SetCornerRadius(const Value: Integer);
    function GetRoundCornerType: TRoundCornerType;
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetActiveColor(const Value: TColor);
    function GetInactiveColor: TColor;
    procedure SetInactiveColor(const Value: TColor);
    function GetBorderColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    function GetBorderThickness: Integer;
    procedure SetBorderThickness(const Value: Integer);
    function GetBorderStyle: TPenStyle;
    procedure SetBorderStyle(const Value: TPenStyle);

    function GetImage: TPicture;
    procedure SetImage(const Value: TPicture);
    function GetImageVisible: Boolean;
    procedure SetImageVisible(const Value: Boolean);
    function GetImagePosition: TImagePositionSide;
    procedure SetImagePosition(const Value: TImagePositionSide);
    function GetImageAlignment: TImageAlignmentVertical;
    procedure SetImageAlignment(const Value: TImageAlignmentVertical);
    function GetImageMargins: TANDMR_Margins;
    procedure SetImageMargins(const Value: TANDMR_Margins);
    function GetImagePlacement: TImagePlacement;
    procedure SetImagePlacement(const Value: TImagePlacement);
    function GetImageDrawMode: TImageDrawMode;
    procedure SetImageDrawMode(const Value: TImageDrawMode);

    function GetSeparatorVisible: Boolean;
    procedure SetSeparatorVisible(const Value: Boolean);
    function GetSeparatorColor: TColor;
    procedure SetSeparatorColor(const Value: TColor);
    function GetSeparatorThickness: Integer;
    procedure SetSeparatorThickness(const Value: Integer);
    function GetSeparatorPadding: Integer;
    procedure SetSeparatorPadding(const Value: Integer);
    function GetSeparatorHeightMode: TSeparatorHeightMode;
    procedure SetSeparatorHeightMode(const Value: TSeparatorHeightMode);
    function GetSeparatorCustomHeight: Integer;
    procedure SetSeparatorCustomHeight(const Value: Integer);

    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetTextMargins(const Value: TANDMR_Margins);

    function GetFocusBorderColor: TColor;
    procedure SetFocusBorderColor(const Value: TColor);
    function GetFocusBorderColorVisible: Boolean;
    procedure SetFocusBorderColorVisible(const Value: Boolean);
    function GetFocusBackgroundColor: TColor;
    procedure SetFocusBackgroundColor(const Value: TColor);
    function GetFocusBackgroundColorVisible: Boolean;
    procedure SetFocusBackgroundColorVisible(const Value: Boolean);
    function GetFocusUnderlineColor: TColor;
    procedure SetFocusUnderlineColor(const Value: TColor);
    function GetFocusUnderlineVisible: Boolean;
    procedure SetFocusUnderlineVisible(const Value: Boolean);
    function GetFocusUnderlineThickness: Integer;
    procedure SetFocusUnderlineThickness(const Value: Integer);
    function GetFocusUnderlineStyle: TPenStyle;
    procedure SetFocusUnderlineStyle(const Value: TPenStyle);
    procedure SetOpacity(const Value: Byte);

    // Event Handlers for Settings Objects
    procedure SettingsChanged(Sender: TObject);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure TextMarginsChanged(Sender: TObject);

    // Internal Memo Event Handlers (Delegates)
    procedure InternalMemoChange(Sender: TObject);
    procedure InternalMemoEnter(Sender: TObject);
    procedure InternalMemoExit(Sender: TObject);
    procedure InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure InternalMemoKeyPress(Sender: TObject; var Key: Char);
    procedure InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  protected
    // Control Message Handlers
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetTabStop(Value: Boolean);

    procedure CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); virtual;
    procedure UpdateInternalMemoBounds; virtual;
    procedure Resize; override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    // Core Memo Properties
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default True;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssVertical;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;

    // Appearance Properties
    property CornerRadius: Integer read GetCornerRadius write SetCornerRadius default 8;
    property RoundCornerType: TRoundCornerType read GetRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clHighlight;
    property InactiveColor: TColor read GetInactiveColor write SetInactiveColor default clBtnFace;
    property BorderColor: TColor read GetBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read GetBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read GetBorderStyle write SetBorderStyle default psSolid;

    property Image: TPicture read GetImage write SetImage;
    property ImageVisible: Boolean read GetImageVisible write SetImageVisible default True;
    property ImagePosition: TImagePositionSide read GetImagePosition write SetImagePosition default ipsLeft;
    property ImageAlignment: TImageAlignmentVertical read GetImageAlignment write SetImageAlignment default iavCenter;
    property ImageMargins: TANDMR_Margins read GetImageMargins write SetImageMargins;
    property ImagePlacement: TImagePlacement read GetImagePlacement write SetImagePlacement default iplInsideBounds;
    property ImageDrawMode: TImageDrawMode read GetImageDrawMode write SetImageDrawMode default idmProportional;

    property SeparatorVisible: Boolean read GetSeparatorVisible write SetSeparatorVisible default False;
    property SeparatorColor: TColor read GetSeparatorColor write SetSeparatorColor default clGrayText;
    property SeparatorThickness: Integer read GetSeparatorThickness write SetSeparatorThickness default 1;
    property SeparatorPadding: Integer read GetSeparatorPadding write SetSeparatorPadding default 2;
    property SeparatorHeightMode: TSeparatorHeightMode read GetSeparatorHeightMode write SetSeparatorHeightMode default shmFull;
    property SeparatorCustomHeight: Integer read GetSeparatorCustomHeight write SetSeparatorCustomHeight default 0;

    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;

    property FocusBorderColor: TColor read GetFocusBorderColor write SetFocusBorderColor;
    property FocusBorderColorVisible: Boolean read GetFocusBorderColorVisible write SetFocusBorderColorVisible;
    property FocusBackgroundColor: TColor read GetFocusBackgroundColor write SetFocusBackgroundColor;
    property FocusBackgroundColorVisible: Boolean read GetFocusBackgroundColorVisible write SetFocusBackgroundColorVisible;
    property FocusUnderlineColor: TColor read GetFocusUnderlineColor write SetFocusUnderlineColor;
    property FocusUnderlineVisible: Boolean read GetFocusUnderlineVisible write SetFocusUnderlineVisible;
    property FocusUnderlineThickness: Integer read GetFocusUnderlineThickness write SetFocusUnderlineThickness;
    property FocusUnderlineStyle: TPenStyle read GetFocusUnderlineStyle write SetFocusUnderlineStyle;

    property Opacity: Byte read FOpacity write SetOpacity default 255;

    // Standard Properties
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    // Standard Event Properties
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CMemo]);
end;

{ TANDMR_CMemo }

constructor TANDMR_CMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csSetCaption];
  DoubleBuffered := True;
  Width := 185;
  Height := 80;
  TabStop := True;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.Color := clBlack;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;
  FBorderSettings.BackgroundColor := clBtnFace;

  FFocusSettings := TFocusSettings.Create;
  FFocusSettings.OnChange := SettingsChanged;
  FFocusSettings.BorderColorVisible := False;
  FFocusSettings.BorderColor := clBlack;
  FFocusSettings.BackgroundColorVisible := False;
  FFocusSettings.BackgroundColor := clWindow;
  FFocusSettings.UnderlineVisible := False;
  FFocusSettings.UnderlineColor := clBlack;
  FFocusSettings.UnderlineThickness := 1;
  FFocusSettings.UnderlineStyle := psSolid;

  FSeparatorSettings := TSeparatorSettings.Create;
  FSeparatorSettings.OnChange := SettingsChanged;
  FSeparatorSettings.Visible := False;
  FSeparatorSettings.Color := clGrayText;
  FSeparatorSettings.Thickness := 1;
  FSeparatorSettings.Padding := 2;
  FSeparatorSettings.HeightMode := shmFull;
  FSeparatorSettings.CustomHeight := 0;

  FImageSettings := TImageSettings.Create(Self);
  FImageSettings.OnChange := SettingsChanged;
  FImageSettings.Visible := True;
  FImageSettings.Position := ipsLeft;
  FImageSettings.AlignmentVertical := iavCenter;
  FImageSettings.Placement := iplInsideBounds;
  FImageSettings.DrawMode := idmProportional;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FTextMargins := TANDMR_Margins.Create;
  FTextMargins.OnChange := TextMarginsChanged;

  FActiveColor := clHighlight;
  FCaptionRect := Rect(0,0,0,0);
  FHovered := False;
  FOpacity := 255;
  Self.Font.Name := 'Segoe UI';
  Self.Font.Size := 9;
  Self.Font.Color := clWindowText;

  FInternalMemo := TMemo.Create(Self);
  FInternalMemo.Parent := Self;
  FInternalMemo.Align := alNone;
  FInternalMemo.BorderStyle := bsNone;
  FInternalMemo.TabStop := True;
  FInternalMemo.WordWrap := True;
  FInternalMemo.ScrollBars := ssVertical;
  FInternalMemo.Font.Assign(Self.Font);
  FInternalMemo.Color := clWindow;

  FInternalMemo.OnChange := InternalMemoChange;
  FInternalMemo.OnEnter := InternalMemoEnter;
  FInternalMemo.OnExit := InternalMemoExit;
  FInternalMemo.OnKeyDown := InternalMemoKeyDown;
  FInternalMemo.OnKeyPress := InternalMemoKeyPress;
  FInternalMemo.OnKeyUp := InternalMemoKeyUp;
end;

destructor TANDMR_CMemo.Destroy;
begin
  if Assigned(FBorderSettings) then FBorderSettings.OnChange := nil;
  FreeAndNil(FBorderSettings);
  if Assigned(FFocusSettings) then FFocusSettings.OnChange := nil;
  FreeAndNil(FFocusSettings);
  if Assigned(FSeparatorSettings) then FSeparatorSettings.OnChange := nil;
  FreeAndNil(FSeparatorSettings);
  if Assigned(FImageSettings) then FImageSettings.OnChange := nil;
  FreeAndNil(FImageSettings);
  if Assigned(FCaptionSettings) then FCaptionSettings.OnChange := nil;
  FreeAndNil(FCaptionSettings);
  if Assigned(FHoverSettings) then FHoverSettings.OnChange := nil;
  FreeAndNil(FHoverSettings);
  if Assigned(FTextMargins) then FTextMargins.OnChange := nil;
  FreeAndNil(FTextMargins);

  FreeAndNil(FInternalMemo);
  inherited Destroy;
end;

procedure TANDMR_CMemo.Loaded;
begin
  inherited Loaded;
  UpdateInternalMemoBounds;
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Visible := Self.Visible;
    FInternalMemo.TabStop := Self.TabStop;
  end;
end;

procedure TANDMR_CMemo.SettingsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then // Corrected set elements
  begin
    Invalidate;
    Exit;
  end;
  if HandleAllocated and not (csDestroying in ComponentState) then
  begin
    UpdateInternalMemoBounds;
  end;
  Invalidate;
end;

procedure TANDMR_CMemo.CaptionSettingsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then // Corrected set elements
  begin
    Invalidate;
    Exit;
  end;
  if HandleAllocated and not (csDestroying in ComponentState) then
  begin
    UpdateInternalMemoBounds;
  end;
  Invalidate;
end;

procedure TANDMR_CMemo.HoverSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CMemo.TextMarginsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then // Corrected set elements
  begin
    Invalidate;
    Exit;
  end;
  if HandleAllocated and not (csDestroying in ComponentState) then
  begin
    UpdateInternalMemoBounds;
  end;
  Invalidate;
end;

procedure TANDMR_CMemo.InternalMemoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TANDMR_CMemo.InternalMemoEnter(Sender: TObject);
begin
  if not Self.Focused then
  begin
     Invalidate;
  end;
end;

procedure TANDMR_CMemo.InternalMemoExit(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CMemo.InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

procedure TANDMR_CMemo.InternalMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

procedure TANDMR_CMemo.InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

function TANDMR_CMemo.GetCornerRadius: Integer; begin Result := FBorderSettings.CornerRadius; end;
procedure TANDMR_CMemo.SetCornerRadius(const Value: Integer); begin FBorderSettings.CornerRadius := Value; end;
function TANDMR_CMemo.GetRoundCornerType: TRoundCornerType; begin Result := FBorderSettings.RoundCornerType; end;
procedure TANDMR_CMemo.SetRoundCornerType(const Value: TRoundCornerType); begin FBorderSettings.RoundCornerType := Value; end;
procedure TANDMR_CMemo.SetActiveColor(const Value: TColor); begin if FActiveColor <> Value then begin FActiveColor := Value; Invalidate; end; end;
function TANDMR_CMemo.GetInactiveColor: TColor; begin Result := FBorderSettings.BackgroundColor; end;
procedure TANDMR_CMemo.SetInactiveColor(const Value: TColor); begin FBorderSettings.BackgroundColor := Value; end;
function TANDMR_CMemo.GetBorderColor: TColor; begin Result := FBorderSettings.Color; end;
procedure TANDMR_CMemo.SetBorderColor(const Value: TColor); begin FBorderSettings.Color := Value; end;
function TANDMR_CMemo.GetBorderThickness: Integer; begin Result := FBorderSettings.Thickness; end;
procedure TANDMR_CMemo.SetBorderThickness(const Value: Integer); begin FBorderSettings.Thickness := Value; end;
function TANDMR_CMemo.GetBorderStyle: TPenStyle; begin Result := FBorderSettings.Style; end;
procedure TANDMR_CMemo.SetBorderStyle(const Value: TPenStyle); begin FBorderSettings.Style := Value; end;

function TANDMR_CMemo.GetImage: TPicture; begin Result := FImageSettings.Picture; end;
procedure TANDMR_CMemo.SetImage(const Value: TPicture); begin FImageSettings.Picture.Assign(Value); end;
function TANDMR_CMemo.GetImageVisible: Boolean; begin Result := FImageSettings.Visible; end;
procedure TANDMR_CMemo.SetImageVisible(const Value: Boolean); begin FImageSettings.Visible := Value; end;
function TANDMR_CMemo.GetImagePosition: TImagePositionSide; begin Result := FImageSettings.Position; end;
procedure TANDMR_CMemo.SetImagePosition(const Value: TImagePositionSide); begin FImageSettings.Position := Value; end;
function TANDMR_CMemo.GetImageAlignment: TImageAlignmentVertical; begin Result := FImageSettings.AlignmentVertical; end;
procedure TANDMR_CMemo.SetImageAlignment(const Value: TImageAlignmentVertical); begin FImageSettings.AlignmentVertical := Value; end;
function TANDMR_CMemo.GetImageMargins: TANDMR_Margins; begin Result := FImageSettings.Margins; end;
procedure TANDMR_CMemo.SetImageMargins(const Value: TANDMR_Margins); begin FImageSettings.Margins.Assign(Value); end;
function TANDMR_CMemo.GetImagePlacement: TImagePlacement; begin Result := FImageSettings.Placement; end;
procedure TANDMR_CMemo.SetImagePlacement(const Value: TImagePlacement); begin FImageSettings.Placement := Value; end;
function TANDMR_CMemo.GetImageDrawMode: TImageDrawMode; begin Result := FImageSettings.DrawMode; end;
procedure TANDMR_CMemo.SetImageDrawMode(const Value: TImageDrawMode); begin FImageSettings.DrawMode := Value; end;

function TANDMR_CMemo.GetSeparatorVisible: Boolean; begin Result := FSeparatorSettings.Visible; end;
procedure TANDMR_CMemo.SetSeparatorVisible(const Value: Boolean); begin FSeparatorSettings.Visible := Value; end;
function TANDMR_CMemo.GetSeparatorColor: TColor; begin Result := FSeparatorSettings.Color; end;
procedure TANDMR_CMemo.SetSeparatorColor(const Value: TColor); begin FSeparatorSettings.Color := Value; end;
function TANDMR_CMemo.GetSeparatorThickness: Integer; begin Result := FSeparatorSettings.Thickness; end;
procedure TANDMR_CMemo.SetSeparatorThickness(const Value: Integer); begin FSeparatorSettings.Thickness := Value; end;
function TANDMR_CMemo.GetSeparatorPadding: Integer; begin Result := FSeparatorSettings.Padding; end;
procedure TANDMR_CMemo.SetSeparatorPadding(const Value: Integer); begin FSeparatorSettings.Padding := Value; end;
function TANDMR_CMemo.GetSeparatorHeightMode: TSeparatorHeightMode; begin Result := FSeparatorSettings.HeightMode; end;
procedure TANDMR_CMemo.SetSeparatorHeightMode(const Value: TSeparatorHeightMode); begin FSeparatorSettings.HeightMode := Value; end;
function TANDMR_CMemo.GetSeparatorCustomHeight: Integer; begin Result := FSeparatorSettings.CustomHeight; end;
procedure TANDMR_CMemo.SetSeparatorCustomHeight(const Value: Integer); begin FSeparatorSettings.CustomHeight := Value; end;

procedure TANDMR_CMemo.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); end;
procedure TANDMR_CMemo.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); end;
procedure TANDMR_CMemo.SetTextMargins(const Value: TANDMR_Margins); begin FTextMargins.Assign(Value); end;

function TANDMR_CMemo.GetFocusBorderColor: TColor; begin Result := FFocusSettings.BorderColor; end;
procedure TANDMR_CMemo.SetFocusBorderColor(const Value: TColor); begin FFocusSettings.BorderColor := Value; end;
function TANDMR_CMemo.GetFocusBorderColorVisible: Boolean; begin Result := FFocusSettings.BorderColorVisible; end;
procedure TANDMR_CMemo.SetFocusBorderColorVisible(const Value: Boolean); begin FFocusSettings.BorderColorVisible := Value; end;
function TANDMR_CMemo.GetFocusBackgroundColor: TColor; begin Result := FFocusSettings.BackgroundColor; end;
procedure TANDMR_CMemo.SetFocusBackgroundColor(const Value: TColor); begin FFocusSettings.BackgroundColor := Value; end;
function TANDMR_CMemo.GetFocusBackgroundColorVisible: Boolean; begin Result := FFocusSettings.BackgroundColorVisible; end;
procedure TANDMR_CMemo.SetFocusBackgroundColorVisible(const Value: Boolean); begin FFocusSettings.BackgroundColorVisible := Value; end;
function TANDMR_CMemo.GetFocusUnderlineColor: TColor; begin Result := FFocusSettings.UnderlineColor; end;
procedure TANDMR_CMemo.SetFocusUnderlineColor(const Value: TColor); begin FFocusSettings.UnderlineColor := Value; end;
function TANDMR_CMemo.GetFocusUnderlineVisible: Boolean; begin Result := FFocusSettings.UnderlineVisible; end;
procedure TANDMR_CMemo.SetFocusUnderlineVisible(const Value: Boolean); begin FFocusSettings.UnderlineVisible := Value; end;
function TANDMR_CMemo.GetFocusUnderlineThickness: Integer; begin Result := FFocusSettings.UnderlineThickness; end;
procedure TANDMR_CMemo.SetFocusUnderlineThickness(const Value: Integer); begin FFocusSettings.UnderlineThickness := Value; end;
function TANDMR_CMemo.GetFocusUnderlineStyle: TPenStyle; begin Result := FFocusSettings.UnderlineStyle; end;
procedure TANDMR_CMemo.SetFocusUnderlineStyle(const Value: TPenStyle); begin FFocusSettings.UnderlineStyle := Value; end;

procedure TANDMR_CMemo.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 255 then
    begin
      ControlStyle := ControlStyle - [csOpaque];
      if Parent <> nil then Parent.Invalidate;
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque];
    end;
    Invalidate;
  end;
end;

function TANDMR_CMemo.GetLines: TStrings;
begin
  Result := FInternalMemo.Lines;
end;

procedure TANDMR_CMemo.SetLines(const Value: TStrings);
begin
  FInternalMemo.Lines.Assign(Value);
end;

function TANDMR_CMemo.GetReadOnly: Boolean;
begin
  Result := FInternalMemo.ReadOnly;
end;

procedure TANDMR_CMemo.SetReadOnly(const Value: Boolean);
begin
  FInternalMemo.ReadOnly := Value;
end;

function TANDMR_CMemo.GetWordWrap: Boolean;
begin
  Result := FInternalMemo.WordWrap;
end;

procedure TANDMR_CMemo.SetWordWrap(const Value: Boolean);
begin
  FInternalMemo.WordWrap := Value;
end;

function TANDMR_CMemo.GetScrollBars: TScrollStyle;
begin
  Result := FInternalMemo.ScrollBars;
end;

procedure TANDMR_CMemo.SetScrollBars(const Value: TScrollStyle);
begin
  FInternalMemo.ScrollBars := Value;
end;

function TANDMR_CMemo.GetMaxLength: Integer;
begin
  Result := FInternalMemo.MaxLength;
end;

procedure TANDMR_CMemo.SetMaxLength(const Value: Integer);
begin
  FInternalMemo.MaxLength := Value;
end;

procedure TANDMR_CMemo.CMFontChanged(var Message: TMessage);
begin
  inherited; // Call the inherited CM_FONTCHANGED handler
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Font.Assign(Self.Font); // Propagate the change to the internal memo
  end;
  UpdateInternalMemoBounds; // Font changes can affect layout
  Invalidate; // Repaint the entire component
end;

procedure TANDMR_CMemo.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
const
  DefaultCaptionOffset = 2;
var
  WorkArea: TRect;
  ImgW, ImgH, SepW: Integer;
  FullClientRect: TRect;
  CaptionHeight, CaptionWidth: Integer;
  OriginalFont: TFont;
  OriginalImgW, OriginalImgH: Integer;
  availWForImg, availHForImg: Integer;
  rImageRatio, rAvailBoxRatio: Double;
  tempW, tempH: Double;
  EffectiveCaptionOffsetX, EffectiveCaptionOffsetY: Integer;
begin
  if not HandleAllocated then // Guard against operations requiring a handle
  begin
    outImgRect := Rect(0,0,0,0);
    outTxtRect := Rect(0, 0, Width, Height); // Use Width/Height as a fallback
    outSepRect := Rect(0,0,0,0);
    Exit;
  end;

  FullClientRect := Self.ClientRect;
  FCaptionRect := Rect(0,0,0,0);

  if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
  begin
    OriginalFont := TFont.Create;
    try
      OriginalFont.Assign(Self.Canvas.Font);
      Self.Canvas.Font.Assign(FCaptionSettings.Font);

      CaptionHeight := Self.Canvas.TextHeight(FCaptionSettings.Text);
      CaptionWidth := Self.Canvas.TextWidth(FCaptionSettings.Text);
      if FCaptionSettings.WordWrap and (FCaptionSettings.Position in [cpAbove, cpBelow]) then
      begin
          var TempRectCap := Rect(0,0, FullClientRect.Width, 30000);
          DrawText(Self.Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRectCap, DT_CALCRECT or DT_WORDBREAK);
          CaptionHeight := TempRectCap.Bottom - TempRectCap.Top;
          CaptionWidth := FullClientRect.Width;
      end else if FCaptionSettings.WordWrap and (FCaptionSettings.Position in [cpLeft, cpRight]) then
      begin
          var TempRectCap := Rect(0,0, CaptionWidth, FullClientRect.Height);
          DrawText(Self.Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRectCap, DT_CALCRECT or DT_WORDBREAK);
          CaptionWidth := TempRectCap.Right - TempRectCap.Left;
          CaptionHeight := FullClientRect.Height;
      end;
    finally
      Self.Canvas.Font.Assign(OriginalFont);
      OriginalFont.Free;
    end;

    WorkArea := FullClientRect;
    EffectiveCaptionOffsetX := FCaptionSettings.Offset.X;
    EffectiveCaptionOffsetY := FCaptionSettings.Offset.Y;
    if (EffectiveCaptionOffsetX = 0) and (EffectiveCaptionOffsetY = 0) then
    begin
        EffectiveCaptionOffsetX := DefaultCaptionOffset;
        EffectiveCaptionOffsetY := DefaultCaptionOffset;
    end;

    case FCaptionSettings.Position of
      cpAbove: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Right, FullClientRect.Top + CaptionHeight); WorkArea.Top := FCaptionRect.Bottom + EffectiveCaptionOffsetY; end;
      cpBelow: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Bottom - CaptionHeight, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Bottom := FCaptionRect.Top - EffectiveCaptionOffsetY; end;
      cpLeft:  begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Left + CaptionWidth, FullClientRect.Bottom); WorkArea.Left := FCaptionRect.Right + EffectiveCaptionOffsetX; end;
      cpRight: begin FCaptionRect := Rect(FullClientRect.Right - CaptionWidth, FullClientRect.Top, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Right := FCaptionRect.Left - EffectiveCaptionOffsetX; end;
    end;
    if WorkArea.Bottom < WorkArea.Top then WorkArea.Bottom := WorkArea.Top;
    if WorkArea.Right < WorkArea.Left then WorkArea.Right := WorkArea.Left;
  end
  else
    WorkArea := FullClientRect;

  if FBorderSettings.Thickness > 0 then
      InflateRect(WorkArea, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  outImgRect := Rect(0,0,0,0);
  outSepRect := Rect(0,0,0,0);
  outTxtRect := WorkArea;

  ImgW := 0; ImgH := 0;
  if FImageSettings.Visible and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
  begin
    OriginalImgW := FImageSettings.Picture.Graphic.Width;
    OriginalImgH := FImageSettings.Picture.Graphic.Height;

    if (OriginalImgW > 0) and (OriginalImgH > 0) then
    begin
      availWForImg := outTxtRect.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right;
      availHForImg := outTxtRect.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
      availWForImg := Max(0, availWForImg);
      availHForImg := Max(0, availHForImg);

      if (availWForImg > 0) and (availHForImg > 0) then
      begin
        case FImageSettings.DrawMode of
          idmProportional:
          begin
            rImageRatio := OriginalImgW / OriginalImgH;
            rAvailBoxRatio := availWForImg / availHForImg;
            if rAvailBoxRatio > rImageRatio then
            begin
              ImgH := availHForImg;
              tempW := availHForImg * rImageRatio;
              ImgW := Round(tempW);
              if (ImgW = 0) and (tempW > 0) then ImgW := 1;
            end
            else
            begin
              ImgW := availWForImg;
              if rImageRatio > 0 then
              begin
                tempH := availWForImg / rImageRatio;
                ImgH := Round(tempH);
                if (ImgH = 0) and (tempH > 0) then ImgH := 1;
              end else ImgH := 0;
            end;
          end;
          idmStretch:
          begin
            ImgW := availWForImg;
            ImgH := availHForImg;
          end;
          idmNormal:
          begin
            ImgW := OriginalImgW;
            ImgH := OriginalImgH;
          end;
        else
            rImageRatio := OriginalImgW / OriginalImgH;
            rAvailBoxRatio := availWForImg / availHForImg;
            if rAvailBoxRatio > rImageRatio then begin ImgH := availHForImg; tempW := availHForImg * rImageRatio; ImgW := Round(tempW); if (ImgW = 0) and (tempW > 0) then ImgW := 1; end
            else begin ImgW := availWForImg; if rImageRatio > 0 then begin tempH := availWForImg / rImageRatio; ImgH := Round(tempH); if (ImgH = 0) and (tempH > 0) then ImgH := 1; end else ImgH := 0; end;
        end;
      end;
    end;
  end;
  ImgW := Max(0, ImgW);
  ImgH := Max(0, ImgH);

  SepW := 0;
  if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) then
    SepW := FSeparatorSettings.Thickness;

  if FImageSettings.Visible and (ImgW > 0) then
  begin
    if FImageSettings.Position = ipsLeft then
    begin
      outImgRect.Left := outTxtRect.Left + FImageSettings.Margins.Left;
      outImgRect.Right := outImgRect.Left + ImgW;
      outTxtRect.Left := outImgRect.Right + FImageSettings.Margins.Right;
      if SepW > 0 then
      begin
        outSepRect.Left := outTxtRect.Left + FSeparatorSettings.Padding;
        outSepRect.Right := outSepRect.Left + SepW;
        outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding;
      end;
    end
    else
    begin
      outImgRect.Right := outTxtRect.Right - FImageSettings.Margins.Right;
      outImgRect.Left := outImgRect.Right - ImgW;
      outTxtRect.Right := outImgRect.Left - FImageSettings.Margins.Left;
      if SepW > 0 then
      begin
        outSepRect.Right := outTxtRect.Right - FSeparatorSettings.Padding;
        outSepRect.Left := outSepRect.Right - SepW;
        outTxtRect.Right := outSepRect.Left - FSeparatorSettings.Padding;
      end;
    end;

    var AvailHForImgLayoutAdjusted: Integer := WorkArea.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
    AvailHForImgLayoutAdjusted := Max(0, AvailHForImgLayoutAdjusted);
    case FImageSettings.AlignmentVertical of
      iavTop:    outImgRect.Top := WorkArea.Top + FImageSettings.Margins.Top;
      iavCenter: outImgRect.Top := WorkArea.Top + FImageSettings.Margins.Top + (AvailHForImgLayoutAdjusted - ImgH) div 2;
      iavBottom: outImgRect.Top := WorkArea.Bottom - FImageSettings.Margins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;
    if outImgRect.Top < WorkArea.Top then outImgRect.Top := WorkArea.Top;
    if outImgRect.Bottom > WorkArea.Bottom then outImgRect.Bottom := WorkArea.Bottom;
    if outImgRect.Left < WorkArea.Left then outImgRect.Left := WorkArea.Left;
    if outImgRect.Right > WorkArea.Right then outImgRect.Right := WorkArea.Right;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
    if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
  end
  else if SepW > 0 then
  begin
    outSepRect.Left := outTxtRect.Left + FSeparatorSettings.Padding;
    outSepRect.Right := outSepRect.Left + SepW;
    outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding;
  end;

  if SepW > 0 then
  begin
    var SepH: Integer;
    var RefTop, RefHeight: Integer;
    outSepRect.Top := WorkArea.Top;
    SepH := WorkArea.Height;

    case FSeparatorSettings.HeightMode of
      shmFull: ;
      shmAsText: begin
        RefTop := WorkArea.Top; RefHeight := WorkArea.Height;
        SepH := RefHeight; outSepRect.Top := RefTop;
      end;
      shmAsImage: if FImageSettings.Visible and (ImgW > 0) and (outImgRect.Height > 0) then
                  begin RefTop := outImgRect.Top; RefHeight := outImgRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmCustom: if FSeparatorSettings.CustomHeight > 0 then
                 begin SepH := FSeparatorSettings.CustomHeight; outSepRect.Top := WorkArea.Top + (WorkArea.Height - SepH) div 2; end
                 else SepH := WorkArea.Height;
    end;
    outSepRect.Bottom := outSepRect.Top + SepH;
    if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top;
    if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
    if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
  end;

  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;
  if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
end;

procedure TANDMR_CMemo.UpdateInternalMemoBounds;
var
  LImgRect, LTxtRect, LSepRect: TRect;
  MemoRect: TRect;
begin
  CalculateLayout(LImgRect, LTxtRect, LSepRect);

  MemoRect.Left   := LTxtRect.Left + FTextMargins.Left;
  MemoRect.Top    := LTxtRect.Top + FTextMargins.Top;
  MemoRect.Right  := LTxtRect.Right - FTextMargins.Right;
  MemoRect.Bottom := LTxtRect.Bottom - FTextMargins.Bottom;

  if MemoRect.Right < MemoRect.Left then MemoRect.Right := MemoRect.Left;
  if MemoRect.Bottom < MemoRect.Top then MemoRect.Bottom := MemoRect.Top;

  if Assigned(FInternalMemo) then
  begin
    if (FInternalMemo.BoundsRect.Left <> MemoRect.Left) or
       (FInternalMemo.BoundsRect.Top <> MemoRect.Top) or
       (FInternalMemo.BoundsRect.Right <> MemoRect.Right) or
       (FInternalMemo.BoundsRect.Bottom <> MemoRect.Bottom) then
    begin
      FInternalMemo.BoundsRect := MemoRect;
    end;
  end;
end;

procedure TANDMR_CMemo.Resize;
begin
  inherited Resize;
  UpdateInternalMemoBounds;
  Invalidate;
end;

procedure TANDMR_CMemo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if FFocusSettings.BorderColorVisible or FFocusSettings.BackgroundColorVisible or FFocusSettings.UnderlineVisible then
    Invalidate;
  if Assigned(FInternalMemo) and FInternalMemo.CanFocus then
    FInternalMemo.SetFocus;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TANDMR_CMemo.CMExit(var Message: TCMExit);
begin
  if FFocusSettings.BorderColorVisible or FFocusSettings.BackgroundColorVisible or FFocusSettings.UnderlineVisible then
    Invalidate;
  inherited;
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TANDMR_CMemo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
  end;
  FHoverSettings.StartAnimation(True);
end;

procedure TANDMR_CMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
  end;
  FHoverSettings.StartAnimation(False);
end;

procedure TANDMR_CMemo.SetTabStop(Value: Boolean);
begin
  inherited TabStop := Value;
  if Assigned(FInternalMemo) then
    FInternalMemo.TabStop := Self.TabStop;
end;

procedure TANDMR_CMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if CanFocus and (not Focused) and Assigned(FInternalMemo) then
    begin
       Self.SetFocus;
    end
    else if Focused and Assigned(FInternalMemo) and (FInternalMemo.Handle <> GetFocus) then
    begin
       FInternalMemo.SetFocus;
    end;
  end;
end;

procedure TANDMR_CMemo.Paint;
var
  LG: TGPGraphics;
  imgR, txtR, sepR: TRect;
  FullClientRect: TRect;
  ActualFrameBG, ActualBorderColor, ActualMemoBG, ActualMemoFontColor, ActualCaptionTextColor: TColor;
  EditBoxDrawingRect: TRect;
  IsComponentFocused: Boolean;
begin
  UpdateInternalMemoBounds;
  CalculateLayout(imgR, txtR, sepR);
  FullClientRect := Self.ClientRect;

  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      var LHoverProgress: Single := FHoverSettings.CurrentAnimationValue / 255.0;
      IsComponentFocused := Self.Focused or (Assigned(FInternalMemo) and FInternalMemo.Focused);

      var BaseFrameBG, BaseBorderColor, BaseMemoBG, BaseMemoFontColor, BaseCaptionColor: TColor;
      BaseFrameBG := FBorderSettings.BackgroundColor;
      BaseBorderColor := FBorderSettings.Color;
      BaseMemoBG := clWindow;
      BaseMemoFontColor := Self.Font.Color;
      BaseCaptionColor := IfThen(FCaptionSettings.Color = clDefault, Self.Font.Color, FCaptionSettings.Color);

      var HoverFrameBG, HoverBorderColor, HoverMemoBG, HoverMemoFontColor, HoverCaptionColor: TColor;
      HoverFrameBG := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
      HoverBorderColor := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
      HoverMemoBG := clNone;
      HoverMemoFontColor := IfThen(FHoverSettings.Enabled, FHoverSettings.FontColor, clNone);
      HoverCaptionColor := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);

      var FocusFrameBG, FocusBorderColor, FocusMemoBG, FocusMemoFontColor, FocusCaptionColor: TColor;
      FocusFrameBG := clNone;
      FocusBorderColor := IfThen(FFocusSettings.BorderColorVisible, FFocusSettings.BorderColor, FActiveColor);
      FocusMemoBG := IfThen(FFocusSettings.BackgroundColorVisible, FFocusSettings.BackgroundColor, clNone);
      FocusMemoFontColor := BaseMemoFontColor;
      FocusCaptionColor := BaseCaptionColor;

      var DisabledFrameBG, DisabledBorderColor, DisabledMemoBG, DisabledMemoFontColor, DisabledCaptionColor: TColor;
      DisabledFrameBG := BaseFrameBG;
      DisabledBorderColor := BaseBorderColor;
      DisabledMemoBG := BlendColors(BaseMemoBG, clGray, 0.1);
      DisabledMemoFontColor := clGrayText;
      DisabledCaptionColor := clGrayText;

      var NonHoveredFrameBG, NonHoveredBorderColor, NonHoveredMemoBG, NonHoveredMemoFontColor, NonHoveredCaptionColor: TColor;
      NonHoveredFrameBG     := ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseFrameBG,         clNone,            FocusFrameBG,      DisabledFrameBG,      FHoverSettings.Enabled, True, True);
      NonHoveredBorderColor := ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseBorderColor,     clNone,            FocusBorderColor,  DisabledBorderColor,  FHoverSettings.Enabled, FFocusSettings.BorderColorVisible);
      NonHoveredMemoBG      := ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseMemoBG,          clNone,            FocusMemoBG,       DisabledMemoBG,       False,                  FFocusSettings.BackgroundColorVisible);
      NonHoveredMemoFontColor:= ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseMemoFontColor,   clNone,            FocusMemoFontColor,DisabledMemoFontColor,False,                  False);
      NonHoveredCaptionColor:= ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseCaptionColor,    clNone,            FocusCaptionColor, DisabledCaptionColor, False,                  False);

      var TargetStateFrameBG, TargetStateBorderColor, TargetStateMemoBG, TargetStateMemoFontColor, TargetStateCaptionColor: TColor;
      TargetStateFrameBG      := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseFrameBG,         HoverFrameBG,      FocusFrameBG,      DisabledFrameBG,      FHoverSettings.Enabled, True, True);
      TargetStateBorderColor  := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseBorderColor,     HoverBorderColor,  FocusBorderColor,  DisabledBorderColor,  FHoverSettings.Enabled, FFocusSettings.BorderColorVisible);
      TargetStateMemoBG       := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseMemoBG,          HoverMemoBG,       FocusMemoBG,       DisabledMemoBG,       FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible);
      TargetStateMemoFontColor:= ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseMemoFontColor,   HoverMemoFontColor,FocusMemoFontColor,DisabledMemoFontColor,FHoverSettings.Enabled, False);
      TargetStateCaptionColor := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseCaptionColor,    HoverCaptionColor, FocusCaptionColor, DisabledCaptionColor, FHoverSettings.Enabled, False);

      if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FHovered then
      begin
        ActualFrameBG         := BlendColors(NonHoveredFrameBG,      TargetStateFrameBG,      LHoverProgress);
        ActualBorderColor     := BlendColors(NonHoveredBorderColor,  TargetStateBorderColor,  LHoverProgress);
        ActualMemoBG          := BlendColors(NonHoveredMemoBG,       TargetStateMemoBG,       LHoverProgress);
        ActualMemoFontColor   := BlendColors(NonHoveredMemoFontColor,TargetStateMemoFontColor,LHoverProgress);
        ActualCaptionTextColor:= BlendColors(NonHoveredCaptionColor, TargetStateCaptionColor, LHoverProgress);
      end
      else
      begin
        ActualFrameBG         := TargetStateFrameBG;
        ActualBorderColor     := TargetStateBorderColor;
        ActualMemoBG          := TargetStateMemoBG;
        ActualMemoFontColor   := TargetStateMemoFontColor;
        ActualCaptionTextColor:= TargetStateCaptionColor;
      end;

      if Assigned(FInternalMemo) then
      begin
        if FInternalMemo.Color <> ActualMemoBG then FInternalMemo.Color := ActualMemoBG;
        if FInternalMemo.Font.Color <> ActualMemoFontColor then FInternalMemo.Font.Color := ActualMemoFontColor;
      end;

      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(FullClientRect);
      end;

      EditBoxDrawingRect := txtR;

      var BackgroundForEditBoxFrame: TColor;
      if FImageSettings.Placement = iplInsideBounds then
          BackgroundForEditBoxFrame := ActualFrameBG
      else
          BackgroundForEditBoxFrame := ActualMemoBG;

      DrawEditBox(LG, EditBoxDrawingRect, BackgroundForEditBoxFrame, ActualBorderColor,
                  FBorderSettings.Thickness, FBorderSettings.Style, FBorderSettings.CornerRadius,
                  FBorderSettings.RoundCornerType, FOpacity);

      if FImageSettings.Visible and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
      begin
        if (FImageSettings.Picture.Graphic is TPNGImage) then
          DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, imgR, FImageSettings.DrawMode)
        else
          DrawNonPNGImageWithCanvas(Canvas, FImageSettings.Picture.Graphic, imgR, FImageSettings.DrawMode);
      end;

      if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
        DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorSettings.Color, FSeparatorSettings.Thickness);

      if IsComponentFocused and FFocusSettings.UnderlineVisible and (FFocusSettings.UnderlineThickness > 0) then
      begin
        var UnderlineY: Integer;
        var UnderlinePen: TGPPen;
        if FBorderSettings.Thickness > 0 then
          UnderlineY := EditBoxDrawingRect.Bottom - (FBorderSettings.Thickness div 2) - (FFocusSettings.UnderlineThickness div 2)
        else
          UnderlineY := EditBoxDrawingRect.Bottom - (FFocusSettings.UnderlineThickness div 2);
        UnderlineY := Min(UnderlineY, EditBoxDrawingRect.Bottom - FFocusSettings.UnderlineThickness);

        UnderlinePen := TGPPen.Create(ColorToARGB(FFocusSettings.UnderlineColor, Self.FOpacity), FFocusSettings.UnderlineThickness);
        try
          case FFocusSettings.UnderlineStyle of
            psSolid: UnderlinePen.SetDashStyle(DashStyleSolid);
            psDash: UnderlinePen.SetDashStyle(DashStyleDash);
            psDot: UnderlinePen.SetDashStyle(DashStyleDot);
            psDashDot: UnderlinePen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: UnderlinePen.SetDashStyle(DashStyleDashDotDot);
            else UnderlinePen.SetDashStyle(DashStyleSolid);
          end;
          var UnderlineStartX, UnderlineEndX : Integer;
          UnderlineStartX := EditBoxDrawingRect.Left + Max(1, FBorderSettings.Thickness div 2);
          UnderlineEndX   := EditBoxDrawingRect.Right - Max(1, FBorderSettings.Thickness div 2);
          if FBorderSettings.Thickness = 0 then
          begin
            UnderlineStartX := EditBoxDrawingRect.Left;
            UnderlineEndX   := EditBoxDrawingRect.Right;
          end;

          LG.DrawLine(UnderlinePen, UnderlineStartX, UnderlineY, UnderlineEndX, UnderlineY);
        finally
          UnderlinePen.Free;
        end;
      end;
    finally
      LG.Free;
    end;

    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionRect.Width > 0) and (FCaptionRect.Height > 0) then
    begin
      var VAlign: TCaptionVerticalAlignment;
      if FCaptionSettings.Position in [cpLeft, cpRight] then
        VAlign := cvaCenter
      else
        VAlign := cvaTop;

      DrawComponentCaption(
        Self.Canvas, FCaptionRect, FCaptionSettings.Text, FCaptionSettings.Font,
        ActualCaptionTextColor, FCaptionSettings.Alignment, VAlign,
        FCaptionSettings.WordWrap, FOpacity
      );
    end;
  finally
    Canvas.Unlock;
  end;
end;

end.
