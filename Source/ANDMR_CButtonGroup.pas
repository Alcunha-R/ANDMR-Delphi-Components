unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.GraphUtil, System.UITypes,
  ANDMR_ComponentUtils, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Winapi.ActiveX;

type
  TANDMR_CButtonGroup = class; // Forward declaration
  TButtonAppearance = class;
  TButtonStateColors = class;
  TGroupBorderSettings = class;
  TSelectionSettings = class;

  // Copied from TANDMR_CButton for compatibility
  TImagePosition = (ipLeft, ipRight, ipAbove, ipBelow, ipBehind);
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);

  // Class to hold state-specific colors
  TButtonStateColors = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FHoverBorderColor: TColor;
    FClickColor: TColor;
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);
    procedure SetHoverBorderColor(const Value: TColor);
    procedure SetClickColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property HoverColor: TColor read FHoverColor write SetHoverColor default clNone;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor default clNone;
    property HoverBorderColor: TColor read FHoverBorderColor write SetHoverBorderColor default clNone;
    property ClickColor: TColor read FClickColor write SetClickColor default clNone;
  end;

  // Class to hold general appearance properties
  TButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCornerRadius: Integer;
    FStateColors: TButtonStateColors;
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetStateColors(const Value: TButtonStateColors);
    procedure StateColorsChanged(Sender: TObject);

    // Proxy Getters/Setters for easier access to hover/click properties
    function GetClickColor: TColor;
    function GetHoverBorderColor: TColor;
    function GetHoverColor: TColor;
    function GetHoverFontColor: TColor;
    procedure SetClickColor(const Value: TColor);
    procedure SetHoverBorderColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);

  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;

    // Proxy properties for direct access
    property HoverColor: TColor read GetHoverColor write SetHoverColor;
    property HoverFontColor: TColor read GetHoverFontColor write SetHoverFontColor;
    property HoverBorderColor: TColor read GetHoverBorderColor write SetHoverBorderColor;
    property ClickColor: TColor read GetClickColor write SetClickColor;

    // The full state colors object is also available for structured access
    property StateColors: TButtonStateColors read FStateColors write SetStateColors;
  end;


  TANDMR_CGroupButtonItem = class(TCollectionItem)
  private
    FCaption: string;
    FFont: TFont;
    FOnClick: TNotifyEvent;
    FTag: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FWidth: Integer;
    FStyle: TButtonStyle;
    FImageSettings: TImageSettings;
    FImagePosition: TImagePosition;
    FAppearance: TButtonAppearance;

    procedure SetCaption(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTag(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure FontChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetImageSettings(const Value: TImageSettings);
    procedure SetImagePosition(const Value: TImagePosition);
    procedure SetAppearance(const Value: TButtonAppearance);

  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Click;
  published
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    property Appearance: TButtonAppearance read FAppearance write SetAppearance;
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Tag: Integer read FTag write SetTag default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Width: Integer read FWidth write SetWidth default 100;
  end;

  TANDMR_CGroupButtonItems = class(TCollection)
  private
    FOwner: TANDMR_CButtonGroup;
    function GetItem(Index: Integer): TANDMR_CGroupButtonItem;
    procedure SetItem(Index: Integer; const Value: TANDMR_CGroupButtonItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TANDMR_CButtonGroup);
    function Add: TANDMR_CGroupButtonItem;
    property Items[Index: Integer]: TANDMR_CGroupButtonItem read GetItem write SetItem; default;
  end;

  // Class to hold the group's border settings
  TGroupBorderSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FColor: TColor;
    FWidth: Integer;
    FCornerRadius: Integer;
    procedure SetVisible(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetWidth(const Value: Integer);
    procedure SetCornerRadius(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Color: TColor read FColor write SetColor default clSilver;
    property Width: Integer read FWidth write SetWidth default 1;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;
  end;

  // Class to hold selection-related properties
  TSelectionSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FSelectedColor: TColor;
    FSelectedFontColor: TColor;
    FAllowDeselection: Boolean;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFontColor(const Value: TColor);
    procedure SetAllowDeselection(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property SelectedFontColor: TColor read FSelectedFontColor write SetSelectedFontColor;
    property AllowDeselection: Boolean read FAllowDeselection write SetAllowDeselection default True;
  end;

  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FItems: TANDMR_CGroupButtonItems;
    FHoveredItem: TANDMR_CGroupButtonItem;
    FClickedItem: TANDMR_CGroupButtonItem;
    FSelectedItem: TANDMR_CGroupButtonItem;
    FSpacing: Integer;
    FAutoSize: Boolean;
    FGroupBorder: TGroupBorderSettings;
    FSelection: TSelectionSettings;

    // Global Properties
    FGlobalFont: TFont;
    FGlobalAppearance: TButtonAppearance;
    FGlobalStyle: TButtonStyle;

    procedure SetItems(const Value: TANDMR_CGroupButtonItems);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure UpdateWidth;
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetSelectedItemIndex: Integer;
    procedure SetGroupBorder(const Value: TGroupBorderSettings);
    procedure SetSelection(const Value: TSelectionSettings);

    // Global Property Setters
    procedure SetGlobalFont(const Value: TFont);
    procedure SetGlobalAppearance(const Value: TButtonAppearance);
    procedure SetGlobalStyle(const Value: TButtonStyle);
    procedure GlobalFontChanged(Sender: TObject);
    procedure GlobalsChanged(Sender: TObject);


    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
  protected
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedItem: TANDMR_CGroupButtonItem read FSelectedItem;
  published
    property Items: TANDMR_CGroupButtonItems read FItems write SetItems;
    property Spacing: Integer read FSpacing write SetSpacing default 8;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property GroupBorder: TGroupBorderSettings read FGroupBorder write SetGroupBorder;
    property Selection: TSelectionSettings read FSelection write SetSelection;
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex default -1;

    // Global properties to apply to all items
    property GlobalFont: TFont read FGlobalFont write SetGlobalFont;
    property GlobalAppearance: TButtonAppearance read FGlobalAppearance write SetGlobalAppearance;
    property GlobalStyle: TButtonStyle read FGlobalStyle write SetGlobalStyle default bsSolid;

    property Align;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
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
  RegisterComponents('ANDMR', [TANDMR_CButtonGroup]);
end;

{ TButtonStateColors }

constructor TButtonStateColors.Create;
begin
  inherited;
  FHoverColor := TColor($00EAEAEA);
  FHoverFontColor := clNone;
  FHoverBorderColor := clNone;
  FClickColor := TColor($00DDDDDD);
end;

procedure TButtonStateColors.Assign(Source: TPersistent);
begin
  if Source is TButtonStateColors then
  begin
    FClickColor := TButtonStateColors(Source).FClickColor;
    FHoverColor := TButtonStateColors(Source).FHoverColor;
    FHoverBorderColor := TButtonStateColors(Source).FHoverBorderColor;
    FHoverFontColor := TButtonStateColors(Source).FHoverFontColor;
    Changed;
  end
  else
    inherited;
end;

procedure TButtonStateColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TButtonStateColors.SetClickColor(const Value: TColor);
begin
  if FClickColor <> Value then
  begin
    FClickColor := Value;
    Changed;
  end;
end;

procedure TButtonStateColors.SetHoverBorderColor(const Value: TColor);
begin
  if FHoverBorderColor <> Value then
  begin
    FHoverBorderColor := Value;
    Changed;
  end;
end;

procedure TButtonStateColors.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then
  begin
    FHoverColor := Value;
    Changed;
  end;
end;

procedure TButtonStateColors.SetHoverFontColor(const Value: TColor);
begin
  if FHoverFontColor <> Value then
  begin
    FHoverFontColor := Value;
    Changed;
  end;
end;

{ TButtonAppearance }

constructor TButtonAppearance.Create;
begin
  inherited;
  FColor := TColor($00F5F5F5);
  FBorderColor := TColor($00E0E0E0);
  FBorderWidth := 0;
  FCornerRadius := 0;
  FStateColors := TButtonStateColors.Create;
  FStateColors.OnChange := StateColorsChanged;
end;

destructor TButtonAppearance.Destroy;
begin
  FStateColors.Free;
  inherited;
end;

procedure TButtonAppearance.Assign(Source: TPersistent);
begin
  if Source is TButtonAppearance then
  begin
    FColor := TButtonAppearance(Source).FColor;
    FBorderColor := TButtonAppearance(Source).FBorderColor;
    FBorderWidth := TButtonAppearance(Source).FBorderWidth;
    FCornerRadius := TButtonAppearance(Source).FCornerRadius;
    FStateColors.Assign(TButtonAppearance(Source).FStateColors);
    Changed;
  end
  else
    inherited;
end;

procedure TButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TButtonAppearance.StateColorsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TButtonAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TButtonAppearance.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TButtonAppearance.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TButtonAppearance.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Changed;
  end;
end;

procedure TButtonAppearance.SetStateColors(const Value: TButtonStateColors);
begin
  FStateColors.Assign(Value);
end;

function TButtonAppearance.GetClickColor: TColor;
begin
  Result := FStateColors.ClickColor;
end;

function TButtonAppearance.GetHoverBorderColor: TColor;
begin
  Result := FStateColors.HoverBorderColor;
end;

function TButtonAppearance.GetHoverColor: TColor;
begin
  Result := FStateColors.HoverColor;
end;

function TButtonAppearance.GetHoverFontColor: TColor;
begin
  Result := FStateColors.HoverFontColor;
end;

procedure TButtonAppearance.SetClickColor(const Value: TColor);
begin
  FStateColors.ClickColor := Value;
end;

procedure TButtonAppearance.SetHoverBorderColor(const Value: TColor);
begin
  FStateColors.HoverBorderColor := Value;
end;

procedure TButtonAppearance.SetHoverColor(const Value: TColor);
begin
  FStateColors.HoverColor := Value;
end;

procedure TButtonAppearance.SetHoverFontColor(const Value: TColor);
begin
  FStateColors.HoverFontColor := Value;
end;

{ TANDMR_CGroupButtonItem }

constructor TANDMR_CGroupButtonItem.Create(Collection: TCollection);
var
  LGroupOwner: TANDMR_CButtonGroup;
begin
  inherited;
  FVisible := True;
  FEnabled := True;
  FWidth := 100;
  FStyle := bsSolid;
  FImagePosition := ipLeft;

  FAppearance := TButtonAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;

  FFont := TFont.Create;
  FFont.Style := [fsBold];
  FFont.Color := clBlack;
  FFont.OnChange := FontChanged;

  if (Collection <> nil) and (Collection.Owner is TANDMR_CButtonGroup) then
  begin
    LGroupOwner := Collection.Owner as TANDMR_CButtonGroup;
    FImageSettings := TImageSettings.Create(LGroupOwner);
    FImageSettings.OnChange := SettingsChanged;
  end;
end;

destructor TANDMR_CGroupButtonItem.Destroy;
begin
  FAppearance.Free;
  FImageSettings.Free;
  FFont.Free;
  inherited;
end;

procedure TANDMR_CGroupButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TANDMR_CGroupButtonItem then
  begin
    with TANDMR_CGroupButtonItem(Dest) do
    begin
      Self.FCaption := FCaption;
      Self.FFont.Assign(FFont);
      Self.FOnClick := FOnClick;
      Self.FTag := FTag;
      Self.FVisible := FVisible;
      Self.FEnabled := FEnabled;
      Self.FWidth := FWidth;
      Self.FStyle := FStyle;
      Self.FImagePosition := FImagePosition;
      Self.FAppearance.Assign(FAppearance);
      Self.ImageSettings.Assign(FImageSettings);
    end;
  end
  else
    inherited;
end;

procedure TANDMR_CGroupButtonItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TANDMR_CGroupButtonItem.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TANDMR_CGroupButtonItem.SettingsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TANDMR_CGroupButtonItem.AppearanceChanged(Sender: TObject);
begin
  Changed(False);
end;

function TANDMR_CGroupButtonItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TANDMR_CGroupButtonItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetImagePosition(const Value: TImagePosition);
begin
  if FImagePosition <> Value then
  begin
    FImagePosition := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

procedure TANDMR_CGroupButtonItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetAppearance(const Value: TButtonAppearance);
begin
  FAppearance.Assign(Value);
end;


{ TANDMR_CGroupButtonItems }

function TANDMR_CGroupButtonItems.Add: TANDMR_CGroupButtonItem;
begin
  Result := TANDMR_CGroupButtonItem(inherited Add);
  // Apply global settings to the new item
  Result.Appearance.Assign(FOwner.GlobalAppearance);
  Result.Font.Assign(FOwner.GlobalFont);
  Result.Style := FOwner.GlobalStyle;
end;

constructor TANDMR_CGroupButtonItems.Create(AOwner: TANDMR_CButtonGroup);
begin
  inherited Create(TANDMR_CGroupButtonItem);
  FOwner := AOwner;
end;

function TANDMR_CGroupButtonItems.GetItem(Index: Integer): TANDMR_CGroupButtonItem;
begin
  Result := TANDMR_CGroupButtonItem(inherited GetItem(Index));
end;

function TANDMR_CGroupButtonItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TANDMR_CGroupButtonItems.SetItem(Index: Integer; const Value: TANDMR_CGroupButtonItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TANDMR_CGroupButtonItems.Update(Item: TCollectionItem);
begin
  inherited;
  if not (csLoading in FOwner.ComponentState) then
  begin
    FOwner.UpdateWidth;
    FOwner.Invalidate;
  end;
end;

{ TGroupBorderSettings }

constructor TGroupBorderSettings.Create;
begin
  inherited;
  FVisible := False;
  FColor := TColor($00E0E0E0);
  FWidth := 2;
  FCornerRadius := 15;
end;

procedure TGroupBorderSettings.Assign(Source: TPersistent);
begin
  if Source is TGroupBorderSettings then
  begin
    FVisible := TGroupBorderSettings(Source).FVisible;
    FColor := TGroupBorderSettings(Source).FColor;
    FWidth := TGroupBorderSettings(Source).FWidth;
    FCornerRadius := TGroupBorderSettings(Source).FCornerRadius;
    Changed;
  end
  else
    inherited;
end;

procedure TGroupBorderSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGroupBorderSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TGroupBorderSettings.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Changed;
  end;
end;

procedure TGroupBorderSettings.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TGroupBorderSettings.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TSelectionSettings }

constructor TSelectionSettings.Create;
begin
  inherited;
  FSelectedColor := TColor($002979FF);
  FSelectedFontColor := clWhite;
  FAllowDeselection := False;
end;

procedure TSelectionSettings.Assign(Source: TPersistent);
begin
  if Source is TSelectionSettings then
  begin
    FSelectedColor := TSelectionSettings(Source).FSelectedColor;
    FSelectedFontColor := TSelectionSettings(Source).FSelectedFontColor;
    FAllowDeselection := TSelectionSettings(Source).FAllowDeselection;
    Changed;
  end
  else
    inherited;
end;

procedure TSelectionSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSelectionSettings.SetAllowDeselection(const Value: Boolean);
begin
  if FAllowDeselection <> Value then
  begin
    FAllowDeselection := Value;
    Changed;
  end;
end;

procedure TSelectionSettings.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Changed;
  end;
end;

procedure TSelectionSettings.SetSelectedFontColor(const Value: TColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    Changed;
  end;
end;

{ TANDMR_CButtonGroup }

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csPannable];
  Width := 300;
  Height := 45;

  FItems := TANDMR_CGroupButtonItems.Create(Self);
  FSpacing := 0;
  FHoveredItem := nil;
  FClickedItem := nil;
  FSelectedItem := nil;
  FAutoSize := True;
  DoubleBuffered := True;

  FGroupBorder := TGroupBorderSettings.Create;
  FGroupBorder.OnChange := GlobalsChanged;

  FSelection := TSelectionSettings.Create;
  FSelection.OnChange := GlobalsChanged;

  FGlobalFont := TFont.Create;
  FGlobalFont.OnChange := GlobalFontChanged;
  FGlobalFont.Style := [fsBold];
  FGlobalFont.Color := clBlack;

  FGlobalAppearance := TButtonAppearance.Create;
  FGlobalAppearance.OnChange := GlobalsChanged;

  FGlobalStyle := bsSolid;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FGlobalAppearance.Free;
  FGlobalFont.Free;
  FSelection.Free;
  FGroupBorder.Free;
  FItems.Free;
  inherited;
end;

procedure TANDMR_CButtonGroup.Loaded;
begin
  inherited Loaded;
  UpdateWidth;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.Paint;
var
  i: Integer;
  LItem: TANDMR_CGroupButtonItem;
  LRect, LInnerRect, LTextArea, LDestRect: TRect;
  LCurrentX: Integer;
  LActualFillColor, LActualBorderColor, LActualFontColor, LInitialFillColor, LInitialBorderColor: TColor;
  LFinalHoverColor, LFinalHoverBorderColor, LFinalClickColor, LFinalHoverFontColor: TColor;
  LG: TGPGraphics;
  LGroupPath, LButtonPath, LClipPath: TGPGraphicsPath;
  LGroupPen: TGPPen;
  LGroupRectF, LButtonRectF, LClipRectF: TGPRectF;
  LHalfBorder: Single;
  LDrawFill, LDrawBorder: Boolean;
  LActualBorderThickness: Integer;

  LImgW, LImgH, LDrawW, LDrawH, LImgX, LImgY, AvailableWidth, AvailableHeight: Integer;
  isHovered, isClicked, isSelected, isDisabled: Boolean;
begin
  inherited;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ClientRect);

    LInnerRect := ClientRect;
    // Draw the main group border and set up the inner rect for content
    if FGroupBorder.Visible and (FGroupBorder.Width > 0) then
    begin
      LGroupRectF := MakeRect(Single(0), Single(0), Single(Width), Single(Height));
      LHalfBorder := FGroupBorder.Width / 2;
      LGroupRectF.X := LGroupRectF.X + LHalfBorder;
      LGroupRectF.Y := LGroupRectF.Y + LHalfBorder;
      LGroupRectF.Width := LGroupRectF.Width - FGroupBorder.Width;
      LGroupRectF.Height := LGroupRectF.Height - FGroupBorder.Width;

      LGroupPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGroupPath, LGroupRectF, FGroupBorder.CornerRadius, rctAll);
        LGroupPen := TGPPen.Create(ColorToARGB(FGroupBorder.Color), FGroupBorder.Width);
        try
          LG.DrawPath(LGroupPen, LGroupPath);
        finally
          LGroupPen.Free;
        end;
      finally
        LGroupPath.Free;
      end;
      // The content will be drawn inside the border
      InflateRect(LInnerRect, -FGroupBorder.Width, -FGroupBorder.Width);
    end;

    // Set a clipping region to ensure items are drawn within the rounded border
    if FGroupBorder.Visible and (FGroupBorder.CornerRadius > 0) then
    begin
      LClipPath := TGPGraphicsPath.Create;
      try
        LClipRectF := MakeRect(Single(LInnerRect.Left), Single(LInnerRect.Top), Single(LInnerRect.Width), Single(LInnerRect.Height));
        var LClipRadius := Max(0, FGroupBorder.CornerRadius - FGroupBorder.Width);
        CreateGPRoundedPath(LClipPath, LClipRectF, LClipRadius, rctAll);
        LG.SetClip(LClipPath, CombineModeReplace);
      finally
        LClipPath.Free;
      end;
    end;

    LCurrentX := LInnerRect.Left;
    for i := 0 to FItems.Count - 1 do
    begin
      LItem := FItems.Items[i];
      if not LItem.Visible then Continue;

      LRect := Rect(LCurrentX, LInnerRect.Top, LCurrentX + LItem.Width, LInnerRect.Bottom);

      // Determine state for the current item
      isDisabled := not (Self.Enabled and LItem.Enabled);
      isHovered  := (LItem = FHoveredItem) and not isDisabled;
      isClicked  := (LItem = FClickedItem) and not isDisabled;
      isSelected := (LItem = FSelectedItem) and not isDisabled;

      // --- Start of TANDMR_CButton Paint Logic Adaptation ---

      LInitialFillColor   := LItem.Appearance.Color;
      LInitialBorderColor := LItem.Appearance.BorderColor;
      LActualFontColor    := LItem.Font.Color;

      if isDisabled then
      begin
        LActualFillColor   := BlendColors(LInitialFillColor, clGray, 0.65);
        LActualBorderColor := BlendColors(LInitialBorderColor, clGray, 0.7);
        LActualFontColor   := clGrayText;
      end
      else
      begin
        LActualFillColor   := LInitialFillColor;
        LActualBorderColor := LInitialBorderColor;

        if isSelected then
        begin
          LActualFillColor := FSelection.SelectedColor;
          LActualFontColor := FSelection.SelectedFontColor;
        end
        else
        begin
          // Hover and Click states only apply if not selected
          LFinalHoverColor := IfThen(LItem.Appearance.HoverColor = clNone, LighterColor(LInitialFillColor, 15), LItem.Appearance.HoverColor);
          LFinalHoverBorderColor := LItem.Appearance.BorderColor;
          if LItem.Appearance.HoverBorderColor <> clNone then
            LFinalHoverBorderColor := LItem.Appearance.HoverBorderColor;

          LFinalClickColor := IfThen(LItem.Appearance.ClickColor = clNone, DarkerColor(LInitialFillColor, 15), LItem.Appearance.ClickColor);
          LFinalHoverFontColor := IfThen(LItem.Appearance.HoverFontColor = clNone, LActualFontColor, LItem.Appearance.HoverFontColor);


          if isHovered then
          begin
            LActualFillColor := LFinalHoverColor;
            LActualBorderColor := LFinalHoverBorderColor;
            LActualFontColor := LFinalHoverFontColor;
          end;

          if isClicked then
            LActualFillColor := LFinalClickColor;
        end;
      end;

      LDrawFill := True;
      LDrawBorder := True;
      LActualBorderThickness := LItem.Appearance.BorderWidth;

      // Apply style-specific rendering
      case LItem.Style of
        bsFaded:
        begin
            LActualFillColor := BlendColors(LInitialFillColor, clWhite, 0.8);
            if isHovered then LActualFillColor := BlendColors(LActualFillColor, LighterColor(LInitialFillColor, 10), 0.7);
            LDrawBorder := False;
        end;
        bsBordered:
        begin
            LDrawFill := isHovered or isSelected;
            if isHovered and not isSelected then LActualFillColor := ColorToARGB(IfThen(LItem.Appearance.HoverColor=clNone, LInitialFillColor, LItem.Appearance.HoverColor), 70);
            LDrawBorder := True;
            LActualBorderThickness := Max(1, LItem.Appearance.BorderWidth);
        end;
        bsGhost:
        begin
            LDrawFill := isHovered or isSelected;
            if isHovered and not isSelected then LActualFillColor := ColorToARGB(IfThen(LItem.Appearance.HoverColor=clNone, LInitialFillColor, LItem.Appearance.HoverColor), 100);
            LDrawBorder := True;
            LActualBorderColor := LInitialFillColor;
            LActualBorderThickness := Max(1, LItem.Appearance.BorderWidth);
        end;
        bsFlat:
        begin
          LDrawBorder := isHovered;
          if isHovered then LActualBorderColor := LInitialFillColor;
        end;
      end;

      // Now, draw the button body
      LButtonPath := TGPGraphicsPath.Create;
      try
        LButtonRectF := MakeRect(Single(LRect.Left), Single(LRect.Top), Single(LRect.Right - LRect.Left), Single(LRect.Bottom - LRect.Top));
        CreateGPRoundedPath(LButtonPath, LButtonRectF, LItem.Appearance.CornerRadius, rctAll);

        if LDrawFill then
        begin
          var FillBrush := TGPSolidBrush.Create(ColorToARGB(LActualFillColor));
          try LG.FillPath(FillBrush, LButtonPath); finally FillBrush.Free; end;
        end;

        if LDrawBorder and (LActualBorderThickness > 0) then
        begin
          var BorderPen := TGPPen.Create(ColorToARGB(LActualBorderColor), LActualBorderThickness);
          try LG.DrawPath(BorderPen, LButtonPath); finally BorderPen.Free; end;
        end;
      finally
        LButtonPath.Free;
      end;

      // --- Drawing Image and Caption ---
      LTextArea := LRect; // Start with the full item rect
      LImgW := 0; LImgH := 0; LDrawW := 0; LDrawH := 0;

      if (LItem.ImageSettings.Picture.Graphic <> nil) and not LItem.ImageSettings.Picture.Graphic.Empty and LItem.ImageSettings.Visible then
      begin
        LImgW := LItem.ImageSettings.Picture.Width;
        LImgH := LItem.ImageSettings.Picture.Height;

        AvailableWidth  := LRect.Width - LItem.ImageSettings.Margins.Left - LItem.ImageSettings.Margins.Right;
        AvailableHeight := LRect.Height - LItem.ImageSettings.Margins.Top - LItem.ImageSettings.Margins.Bottom;

        if (LImgW > 0) and (LImgH > 0) then
        begin
          var imgAspectRatio := LImgW / LImgH;
          if (AvailableWidth / AvailableHeight) > imgAspectRatio then
          begin
            LDrawH := AvailableHeight;
            LDrawW := Round(LDrawH * imgAspectRatio);
          end
          else
          begin
            LDrawW := AvailableWidth;
            LDrawH := Round(LDrawW / imgAspectRatio);
          end;
        end;

        // Position image
        LImgX := LRect.Left + LItem.ImageSettings.Margins.Left + (AvailableWidth - LDrawW) div 2;
        LImgY := LRect.Top + LItem.ImageSettings.Margins.Top + (AvailableHeight - LDrawH) div 2;
        LDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

        // Adjust text area based on image position
        case LItem.ImagePosition of
          ipLeft:   LTextArea.Left := LDestRect.Right + LItem.ImageSettings.Margins.Right;
          ipRight:  LTextArea.Right := LDestRect.Left - LItem.ImageSettings.Margins.Left;
          ipAbove:  LTextArea.Top := LDestRect.Bottom + LItem.ImageSettings.Margins.Bottom;
          ipBelow:  LTextArea.Bottom := LDestRect.Top - LItem.ImageSettings.Margins.Top;
        end;

        // Draw image
        if (LDestRect.Right > LDestRect.Left) and (LDestRect.Bottom > LDestRect.Top) then
        begin
          if LItem.ImageSettings.Picture.Graphic is TPNGImage then
            DrawPNGImageWithGDI(LG, LItem.ImageSettings.Picture.Graphic as TPNGImage, LDestRect, idmStretch)
          else
            DrawNonPNGImageWithCanvas(Self.Canvas, LItem.ImageSettings.Picture.Graphic, LDestRect, idmStretch);
        end;
      end;

      // Draw Caption
      if (Trim(LItem.Caption) <> '') and (LTextArea.Width > 0) and (LTextArea.Height > 0) then
      begin
        Self.Canvas.Font.Assign(LItem.Font);
        Self.Canvas.Font.Color := LActualFontColor;
        Self.Canvas.Brush.Style := bsClear;
        DrawText(Self.Canvas.Handle, PChar(LItem.Caption), -1, LTextArea, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
      end;

      // --- End of TANDMR_CButton Paint Logic Adaptation ---

      LCurrentX := LCurrentX + LItem.Width + FSpacing;
    end;
  finally
    LG.ResetClip; // Reset clip to not affect other controls
    LG.Free;
  end;
end;

procedure TANDMR_CButtonGroup.UpdateWidth;
var
  i: Integer;
  LTotalWidth: Integer;
  LVisibleItemCount: Integer;
  LItem: TANDMR_CGroupButtonItem;
begin
  if (csLoading in ComponentState) or not FAutoSize then
    Exit;

  LTotalWidth := 0;
  LVisibleItemCount := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    LItem := FItems.Items[i];
    if LItem.Visible then
    begin
      LTotalWidth := LTotalWidth + LItem.Width;
      Inc(LVisibleItemCount);
    end;
  end;

  if LVisibleItemCount > 1 then
    LTotalWidth := LTotalWidth + ((LVisibleItemCount - 1) * FSpacing);

  if FGroupBorder.Visible and (FGroupBorder.Width > 0) then
    LTotalWidth := LTotalWidth + (FGroupBorder.Width * 2);

  if Self.Width <> LTotalWidth then
    Self.Width := LTotalWidth;
end;

function TANDMR_CButtonGroup.GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
var
  i: Integer;
  LItem: TANDMR_CGroupButtonItem;
  LCurrentX, LBorderOffset: Integer;
begin
  Result := nil;

  LBorderOffset := 0;
  if FGroupBorder.Visible and (FGroupBorder.Width > 0) then
    LBorderOffset := FGroupBorder.Width;

  LCurrentX := LBorderOffset;
  for i := 0 to FItems.Count - 1 do
  begin
    LItem := FItems.Items[i];
    if not LItem.Visible then Continue;

    if PtInRect(Rect(LCurrentX, LBorderOffset, LCurrentX + LItem.Width, Height - LBorderOffset), Point(X, Y)) then
    begin
      Result := LItem;
      Exit;
    end;
    LCurrentX := LCurrentX + LItem.Width + FSpacing;
  end;
end;

procedure TANDMR_CButtonGroup.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TANDMR_CButtonGroup.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoveredItem <> nil then
  begin
    FHoveredItem := nil;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LItem: TANDMR_CGroupButtonItem;
begin
  inherited;
  if Button = mbLeft then
  begin
    LItem := GetItemAt(X, Y);
    if (LItem <> nil) and LItem.Enabled then
    begin
      FClickedItem := LItem;
      Invalidate;
    end;
  end;
end;

procedure TANDMR_CButtonGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LItem: TANDMR_CGroupButtonItem;
begin
  inherited;
  LItem := GetItemAt(X, Y);
  if LItem <> FHoveredItem then
  begin
    FHoveredItem := LItem;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LItem: TANDMR_CGroupButtonItem;
begin
  inherited;
  if Button = mbLeft then
  begin
    LItem := GetItemAt(X, Y);
    if (LItem <> nil) and (LItem = FClickedItem) and LItem.Enabled then
    begin
      if LItem = FSelectedItem then
      begin
        if FSelection.AllowDeselection then
          FSelectedItem := nil;
      end
      else
      begin
        FSelectedItem := LItem;
      end;

      LItem.Click;
    end;
    FClickedItem := nil;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      UpdateWidth;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetItems(const Value: TANDMR_CGroupButtonItems);
begin
  FItems.Assign(Value);
  UpdateWidth;
end;

procedure TANDMR_CButtonGroup.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    UpdateWidth;
    Invalidate;
  end;
end;

function TANDMR_CButtonGroup.GetSelectedItemIndex: Integer;
begin
  if Assigned(FSelectedItem) then
    Result := FSelectedItem.Index
  else
    Result := -1;
end;

procedure TANDMR_CButtonGroup.SetSelectedItemIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FItems.Count) then
  begin
    if FItems[Value] <> FSelectedItem then
    begin
      FSelectedItem := FItems[Value];
      Invalidate;
    end;
  end
  else
  begin
    if FSelectedItem <> nil then
    begin
      FSelectedItem := nil;
      Invalidate;
    end;
  end;
end;

procedure TANDMR_CButtonGroup.SetGroupBorder(const Value: TGroupBorderSettings);
begin
  FGroupBorder.Assign(Value);
end;

procedure TANDMR_CButtonGroup.SetSelection(const Value: TSelectionSettings);
begin
  FSelection.Assign(Value);
end;

// Global Property Setters
procedure TANDMR_CButtonGroup.GlobalFontChanged(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Font.Assign(FGlobalFont);
  Invalidate;
end;

procedure TANDMR_CButtonGroup.GlobalsChanged(Sender: TObject);
begin
  if Sender is TButtonAppearance then
  begin
     for var i := 0 to FItems.Count - 1 do
      FItems[i].Appearance.Assign(FGlobalAppearance);
  end;

  UpdateWidth;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetGlobalFont(const Value: TFont);
begin
  FGlobalFont.Assign(Value);
end;

procedure TANDMR_CButtonGroup.SetGlobalAppearance(const Value: TButtonAppearance);
begin
  FGlobalAppearance.Assign(Value);
end;

procedure TANDMR_CButtonGroup.SetGlobalStyle(const Value: TButtonStyle);
var
  i: Integer;
begin
  if FGlobalStyle <> Value then
  begin
    FGlobalStyle := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].Style := Value;
    Invalidate;
  end;
end;

end.
