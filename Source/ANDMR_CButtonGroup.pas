unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Math, System.UITypes,
  System.Win.ComObj, // Added for TStreamAdapter
  Winapi.Windows, Winapi.Messages, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Winapi.ActiveX,
  Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.GraphUtil,
  ANDMR_ComponentUtils, ANDMR_CButton;

type
  TANDMR_CButtonGroup = class; // Forward declaration
  TButtonAppearance = class;
  TButtonStateColors = class;
  TGroupBorderSettings = class;
  TSelectionSettings = class;
  TANDMR_CGroupButtonItem = class;

  { Forward declaration from ANDMR_ComponentUtils for clarity }
  TButtonStyle = (bsSolid, bsFaded, bsBordered, bsLight, bsFlat, bsGhost, bsShadow, bsDark, bsMaterial, bsModern, bsWindows, bsMacOS);

  // Class to hold state-specific colors for a button
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

  // Class to hold general appearance properties for a button
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
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 0;
    property StateColors: TButtonStateColors read FStateColors write SetStateColors;
  end;

  // Represents a single button item in the group
  TANDMR_CGroupButtonItem = class(TCollectionItem)
  private
    FCaption: TCaptionSettings;
    FAppearance: TButtonAppearance;
    FImage: TImageSettings;
    FStyle: TButtonStyle;
    FOnClick: TNotifyEvent;
    FTag: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FWidth: Integer;

    procedure SetCaption(const Value: TCaptionSettings);
    procedure SetAppearance(const Value: TButtonAppearance);
    procedure SetImage(const Value: TImageSettings);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetTag(const Value: Integer);
    procedure SettingsChanged(Sender: TObject);
    function GetCaptionText: string;
    procedure SetCaptionText(const Value: string);

  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Click;
  published
    property CaptionText: string read GetCaptionText write SetCaptionText;
    property Caption: TCaptionSettings read FCaption write SetCaption;
    property Appearance: TButtonAppearance read FAppearance write SetAppearance;
    property Style: TButtonStyle read FStyle write SetStyle default bsSolid;
    property Image: TImageSettings read FImage write SetImage;
    property Width: Integer read FWidth write SetWidth default 70;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Tag: Integer read FTag write SetTag default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  // Collection of button items
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

  // Settings for the outer border of the group
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
    property Visible: Boolean read FVisible write SetVisible default True;
    property Color: TColor read FColor write SetColor default clSilver;
    property Width: Integer read FWidth write SetWidth default 2;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 20;
  end;

  // Settings for button selection behavior
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
    property AllowDeselection: Boolean read FAllowDeselection write SetAllowDeselection default False;
  end;

  // Main Button Group Component
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
    FGlobalCaption: TCaptionSettings;
    FGlobalImage: TImageSettings;
    FGlobalAppearance: TButtonAppearance; // Acts as a template for new items
    FGlobalStyle: TButtonStyle; // Acts as a template for new items

    procedure SetItems(const Value: TANDMR_CGroupButtonItems);
    procedure SetSpacing(const Value: Integer);
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetSelectedItemIndex: Integer;
    procedure SetGroupBorder(const Value: TGroupBorderSettings);
    procedure SetSelection(const Value: TSelectionSettings);
    procedure SetGlobalCaption(const Value: TCaptionSettings);
    procedure SetGlobalImage(const Value: TImageSettings);
    procedure SetGlobalAppearance(const Value: TButtonAppearance);
    procedure SetGlobalStyle(const Value: TButtonStyle);
    procedure SettingsChanged(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetAutoSize(const Value: Boolean);
    procedure UpdateSize;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
    function GetButtonBlockRect: TRect;

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedItem: TANDMR_CGroupButtonItem read FSelectedItem;
  published
    // Component Properties
    property Align;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    // Group-specific Properties
    property GroupBorder: TGroupBorderSettings read FGroupBorder write SetGroupBorder;
    property Spacing: Integer read FSpacing write SetSpacing default 1;

    // Item Collection and Selection
    property Items: TANDMR_CGroupButtonItems read FItems write SetItems;
    property Selection: TSelectionSettings read FSelection write SetSelection;
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex default -1;

    // Global properties that apply to the component as a whole or act as templates
    property GlobalCaption: TCaptionSettings read FGlobalCaption write SetGlobalCaption;
    property GlobalAppearance: TButtonAppearance read FGlobalAppearance write SetGlobalAppearance; // Template for items
    property GlobalStyle: TButtonStyle read FGlobalStyle write SetGlobalStyle default bsSolid; // Template for items

    // Events
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

procedure DrawGraphicWithGDIPlus(AGraphics: TGPGraphics; AGraphic: TGraphic; const ADestRect: TRect);
var
  LTempBmp: TBitmap;
  LGPBmp: TGPBitmap;
begin
  // Sai se o gráfico for inválido, vazio ou a área de destino não tiver tamanho.
  if not Assigned(AGraphic) or AGraphic.Empty or (ADestRect.Width <= 0) or (ADestRect.Height <= 0) then
    Exit;

  // Abordagem universal e robusta para TODOS os tipos de TGraphic (BMP, JPG, PNG, ICO, etc.)
  LTempBmp := TBitmap.Create;
  try
    // Configura o bitmap intermediário para ter alta qualidade e suporte a transparência.
    // Isto é CRUCIAL para que a transparência do PNG seja preservada.
    LTempBmp.SetSize(AGraphic.Width, AGraphic.Height);
    LTempBmp.PixelFormat := pf32bit;
    LTempBmp.AlphaFormat := afPremultiplied; // Essencial para o canal alfa

    // Desenha o gráfico original (seja PNG ou outro) no nosso bitmap de 32-bit.
    // A VCL cuida da conversão e da transparência aqui.
    LTempBmp.Canvas.Draw(0, 0, AGraphic);

    // Cria o bitmap GDI+ a partir do HBITMAP do nosso bitmap VCL.
    // O '0' como segundo parâmetro indica que não há uma paleta de cores.
    LGPBmp := TGPBitmap.Create(LTempBmp.Handle, 0);
    try
      // Finalmente, desenha a imagem na tela usando GDI+.
      AGraphics.DrawImage(LGPBmp, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height);
    finally
      LGPBmp.Free;
    end;
  finally
    LTempBmp.Free;
  end;
end;

function CalcProportionalRect(const AContainer: TRect; ASourceWidth, ASourceHeight: Integer; AProportional: Boolean): TRect;
var
  LContainerRatio, LSourceRatio: Double;
  LScale: Double;
begin
  Result := AContainer;

  if (ASourceWidth = 0) or (ASourceHeight = 0) or not AProportional then
    Exit;

  LContainerRatio := Result.Width / Result.Height;
  LSourceRatio := ASourceWidth / ASourceHeight;

  if LSourceRatio > LContainerRatio then
    LScale := Result.Width / ASourceWidth
  else
    LScale := Result.Height / ASourceHeight;

  Result.Right := Result.Left + Round(ASourceWidth * LScale);
  Result.Bottom := Result.Top + Round(ASourceHeight * LScale);

  // Center the result rect inside the container
  Result.Offset((AContainer.Width - Result.Width) div 2, (AContainer.Height - Result.Height) div 2);
end;

//------------------------------------------------------------------------------
// TButtonStateColors
//------------------------------------------------------------------------------
constructor TButtonStateColors.Create;
begin
  inherited;
  FHoverColor := TColor($00EAEAEA);
  FHoverFontColor := clBlack;
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

//------------------------------------------------------------------------------
// TButtonAppearance
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// TANDMR_CGroupButtonItem
//------------------------------------------------------------------------------
constructor TANDMR_CGroupButtonItem.Create(Collection: TCollection);
var
  LGroupOwner: TANDMR_CButtonGroup;
begin
  inherited;
  FVisible := True;
  FEnabled := True;
  FWidth := 70;
  FStyle := bsSolid;
  FTag := 0;

  if (Collection <> nil) and (Collection.Owner is TANDMR_CButtonGroup) then
    LGroupOwner := Collection.Owner as TANDMR_CButtonGroup
  else
    LGroupOwner := nil;

  FAppearance := TButtonAppearance.Create;
  FAppearance.OnChange := SettingsChanged;

  FCaption := TCaptionSettings.Create(LGroupOwner);
  FCaption.OnChange := SettingsChanged;
  FCaption.Alignment := taCenter;
  FCaption.Font.Style := [fsBold];
  FCaption.Font.Color := clGray;
  FCaption.Text := 'Item ' + IntToStr(Index);

  FImage := TImageSettings.Create(LGroupOwner);
  FImage.OnChange := SettingsChanged;

  // Apply global templates from owner if available
  if Assigned(LGroupOwner) then
  begin
    Appearance.Assign(LGroupOwner.GlobalAppearance);
    Style := LGroupOwner.GlobalStyle;
  end;
end;

destructor TANDMR_CGroupButtonItem.Destroy;
begin
  FAppearance.Free;
  FImage.Free;
  FCaption.Free;
  inherited;
end;

procedure TANDMR_CGroupButtonItem.AssignTo(Dest: TPersistent);
var
  LDestItem: TANDMR_CGroupButtonItem;
begin
  if Dest is TANDMR_CGroupButtonItem then
  begin
    LDestItem := TANDMR_CGroupButtonItem(Dest);
    LDestItem.Caption.Assign(Self.FCaption);
    LDestItem.Appearance.Assign(Self.FAppearance);
    LDestItem.Image.Assign(Self.FImage);
    LDestItem.FStyle := Self.FStyle;
    LDestItem.FWidth := Self.FWidth;
    LDestItem.FVisible := Self.FVisible;
    LDestItem.FEnabled := Self.FEnabled;
    LDestItem.FTag := Self.FTag;
    LDestItem.FOnClick := Self.FOnClick;
  end
  else
    inherited;
end;

procedure TANDMR_CGroupButtonItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TANDMR_CGroupButtonItem.SettingsChanged(Sender: TObject);
begin
  Changed(False);
end;

function TANDMR_CGroupButtonItem.GetDisplayName: string;
begin
  if FCaption.Text <> '' then
    Result := FCaption.Text
  else
    Result := inherited GetDisplayName;
end;

function TANDMR_CGroupButtonItem.GetCaptionText: string;
begin
  Result := FCaption.Text;
end;

procedure TANDMR_CGroupButtonItem.SetCaptionText(const Value: string);
begin
  FCaption.Text := Value;
end;

procedure TANDMR_CGroupButtonItem.SetCaption(const Value: TCaptionSettings);
begin
  FCaption.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetAppearance(const Value: TButtonAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetImage(const Value: TImageSettings);
begin
  FImage.Assign(Value);
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


//------------------------------------------------------------------------------
// TANDMR_CGroupButtonItems
//------------------------------------------------------------------------------
constructor TANDMR_CGroupButtonItems.Create(AOwner: TANDMR_CButtonGroup);
begin
  inherited Create(TANDMR_CGroupButtonItem);
  FOwner := AOwner;
end;

function TANDMR_CGroupButtonItems.Add: TANDMR_CGroupButtonItem;
begin
  Result := TANDMR_CGroupButtonItem(inherited Add);
  // Note: Item constructor now applies global templates
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
    FOwner.UpdateSize;
    FOwner.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// TGroupBorderSettings
//------------------------------------------------------------------------------
constructor TGroupBorderSettings.Create;
begin
  inherited;
  FVisible := True;
  FColor := TColor($00E0E0E0);
  FWidth := 2;
  FCornerRadius := 20;
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

//------------------------------------------------------------------------------
// TSelectionSettings
//------------------------------------------------------------------------------
constructor TSelectionSettings.Create;
begin
  inherited;
  FSelectedColor := TColor($00E0E0E0);
  FSelectedFontColor := clBlack;
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

//------------------------------------------------------------------------------
// TANDMR_CButtonGroup
//------------------------------------------------------------------------------
constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csPannable, csDoubleClicks];
  Width := 300;
  Height := 40;
  Cursor := crHandPoint;
  DoubleBuffered := True;
  FAutoSize := True;

  FItems := TANDMR_CGroupButtonItems.Create(Self);
  FSpacing := 1;
  FHoveredItem := nil;
  FClickedItem := nil;
  FSelectedItem := nil;

  // Create property objects
  FGroupBorder := TGroupBorderSettings.Create;
  FSelection := TSelectionSettings.Create;
  FGlobalCaption := TCaptionSettings.Create(Self);
  FGlobalImage := TImageSettings.Create(Self);
  FGlobalAppearance := TButtonAppearance.Create;

  // Set initial default values BEFORE assigning OnChange handlers
  FGlobalStyle := bsSolid;
  FGlobalCaption.Font.Style := [fsBold];
  FGlobalCaption.Font.Color := clBlack;

  // NOW assign the OnChange handlers
  FGroupBorder.OnChange := SettingsChanged;
  FSelection.OnChange := SettingsChanged;
  FGlobalCaption.OnChange := SettingsChanged;
  FGlobalImage.OnChange := SettingsChanged;
  FGlobalAppearance.OnChange := SettingsChanged;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FGlobalAppearance.Free;
  FGlobalImage.Free;
  FGlobalCaption.Free;
  FSelection.Free;
  FGroupBorder.Free;
  FItems.Free;
  inherited;
end;

procedure TANDMR_CButtonGroup.Loaded;
begin
  inherited Loaded;
  UpdateSize;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; // Indicate that background is erased
end;

function TANDMR_CButtonGroup.GetButtonBlockRect: TRect;
var
  i: Integer;
  LCaptionW, LCapH, LImageW, LButtonsTotalWidth, LVisibleItemCount: Integer;
  LMainContentRect: TRect;
  LCurrentX: Integer;
  LSourceGraphic: TGraphic;
begin
  Result := System.Types.Rect(0,0,0,0);
  if not HandleAllocated then Exit;

  // Re-calculate the layout exactly as in the Paint method to find the button block's rect
  LMainContentRect := ClientRect;

  if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') and (FGlobalCaption.Position in [cpAbove, cpBelow]) then
  begin
    Canvas.Font.Assign(FGlobalCaption.Font);
    LCapH := Canvas.TextHeight(FGlobalCaption.Text) + FGlobalCaption.Margins.Top + FGlobalCaption.Margins.Bottom;
    if FGlobalCaption.Position = cpAbove then
      LMainContentRect.Top := LMainContentRect.Top + LCapH
    else
      LMainContentRect.Bottom := LMainContentRect.Bottom - LCapH;
  end;

  LCurrentX := LMainContentRect.Left;

  if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') and (FGlobalCaption.Position = cpLeft) then
  begin
    Canvas.Font.Assign(FGlobalCaption.Font);
    LCaptionW := Canvas.TextWidth(FGlobalCaption.Text) + FGlobalCaption.Margins.Left + FGlobalCaption.Margins.Right;
    LCurrentX := LCurrentX + LCaptionW;
  end;

  if FGlobalImage.Visible and Assigned(FGlobalImage.Picture.Graphic) and not FGlobalImage.Picture.Graphic.Empty then
  begin
    LSourceGraphic := FGlobalImage.Picture.Graphic;
    if FGlobalImage.AutoSize and (FGlobalImage.DrawMode = idmProportional) then
    begin
        LImageW := (LMainContentRect.Height - FGlobalImage.Margins.Top - FGlobalImage.Margins.Bottom);
    end
    else
    begin
        LImageW := IfThen(FGlobalImage.TargetWidth > 0, FGlobalImage.TargetWidth, FGlobalImage.Picture.Width);
    end;
    LCurrentX := LCurrentX + LImageW + FGlobalImage.Margins.Left + FGlobalImage.Margins.Right;
  end;

  LVisibleItemCount := 0;
  LButtonsTotalWidth := 0;
  for i := 0 to FItems.Count - 1 do
    if FItems[i].Visible then
    begin
      LButtonsTotalWidth := LButtonsTotalWidth + FItems[i].Width;
      Inc(LVisibleItemCount);
    end;
  if LVisibleItemCount > 1 then
    LButtonsTotalWidth := LButtonsTotalWidth + (FSpacing * (LVisibleItemCount - 1));

  Result := Rect(LCurrentX, LMainContentRect.Top, LCurrentX + LButtonsTotalWidth, LMainContentRect.Bottom);
end;

procedure TANDMR_CButtonGroup.Paint;
var
  i: Integer;
  LItem: TANDMR_CGroupButtonItem;
  LItemRect, LGlobalImageRect, LGlobalCaptionRect, LItemImageRect, LItemCaptionRect, LButtonBlockRect, LMainContentRect: TRect;
  LCurrentX: Integer;
  LActualFillColor, LActualBorderColor, LActualFontColor: TColor;
  LG: TGPGraphics;
  LPath: TGPGraphicsPath;
  LGroupPen: TGPPen;
  LGroupRectF: TGPRectF;
  LHalfBorder: Single;
  LActualBorderThickness: Integer;
  isHovered, isClicked, isSelected, isDisabled: Boolean;
  LClipRadius: Integer;
  LCaptionW, LCapH, LButtonsTotalWidth, LVisibleItemCount: Integer;
  LSourceGraphic: TGraphic;
  LImageContainerRect: TRect; // Retângulo para o container da imagem do item
begin
  inherited;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetInterpolationMode(InterpolationModeHighQualityBicubic);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ClientRect);

    LMainContentRect := ClientRect;
    LGlobalCaptionRect := System.Types.Rect(0,0,0,0);
    LImageContainerRect := System.Types.Rect(0,0,0,0);

    if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') and (FGlobalCaption.Position in [cpAbove, cpBelow]) then
    begin
      Canvas.Font.Assign(FGlobalCaption.Font);
      LCapH := Canvas.TextHeight(FGlobalCaption.Text) + FGlobalCaption.Margins.Top + FGlobalCaption.Margins.Bottom;
      if FGlobalCaption.Position = cpAbove then
      begin
        LGlobalCaptionRect := Rect(LMainContentRect.Left, LMainContentRect.Top, LMainContentRect.Right, LMainContentRect.Top + LCapH);
        LMainContentRect.Top := LGlobalCaptionRect.Bottom;
      end
      else
      begin
        LGlobalCaptionRect := Rect(LMainContentRect.Left, LMainContentRect.Bottom - LCapH, LMainContentRect.Right, LMainContentRect.Bottom);
        LMainContentRect.Bottom := LGlobalCaptionRect.Top;
      end;
    end;

    LCurrentX := LMainContentRect.Left;

    if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') and (FGlobalCaption.Position = cpLeft) then
    begin
        Canvas.Font.Assign(FGlobalCaption.Font);
        LCaptionW := Canvas.TextWidth(FGlobalCaption.Text) + FGlobalCaption.Margins.Left + FGlobalCaption.Margins.Right;
        LGlobalCaptionRect := Rect(LCurrentX, LMainContentRect.Top, LCurrentX + LCaptionW, LMainContentRect.Bottom);
        LCurrentX := LGlobalCaptionRect.Right;
    end;

    if FGlobalImage.Visible and Assigned(FGlobalImage.Picture.Graphic) and not FGlobalImage.Picture.Graphic.Empty then
    begin
        LSourceGraphic := FGlobalImage.Picture.Graphic;
        var LImageW := IfThen(FGlobalImage.TargetWidth > 0, FGlobalImage.TargetWidth, LSourceGraphic.Width);
        LImageContainerRect := Rect(LCurrentX + FGlobalImage.Margins.Left, LMainContentRect.Top + FGlobalImage.Margins.Top, LCurrentX + FGlobalImage.Margins.Left + LImageW, LMainContentRect.Bottom - FGlobalImage.Margins.Bottom);
        LGlobalImageRect := CalcProportionalRect(LImageContainerRect, LSourceGraphic.Width, LSourceGraphic.Height, FGlobalImage.DrawMode = idmProportional);
        LCurrentX := LImageContainerRect.Right + FGlobalImage.Margins.Right;
    end;

    LVisibleItemCount := 0;
    LButtonsTotalWidth := 0;
    for i := 0 to FItems.Count - 1 do
      if FItems[i].Visible then
      begin
        LButtonsTotalWidth := LButtonsTotalWidth + FItems[i].Width;
        Inc(LVisibleItemCount);
      end;
    if LVisibleItemCount > 1 then
      LButtonsTotalWidth := LButtonsTotalWidth + (FSpacing * (LVisibleItemCount - 1));
    LButtonBlockRect := Rect(LCurrentX, LMainContentRect.Top, LCurrentX + LButtonsTotalWidth, LMainContentRect.Bottom);
    LCurrentX := LButtonBlockRect.Right;

    if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') and (FGlobalCaption.Position = cpRight) then
    begin
      Canvas.Font.Assign(FGlobalCaption.Font);
      LCaptionW := Canvas.TextWidth(FGlobalCaption.Text) + FGlobalCaption.Margins.Left + FGlobalCaption.Margins.Right;
      LGlobalCaptionRect := Rect(LCurrentX, LMainContentRect.Top, LCurrentX + LCaptionW, LMainContentRect.Bottom);
    end;

    if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') then
      DrawComponentCaption(Self.Canvas, LGlobalCaptionRect, FGlobalCaption.Text, FGlobalCaption.Font, FGlobalCaption.Font.Color, FGlobalCaption.Alignment, FGlobalCaption.VerticalAlignment, FGlobalCaption.WordWrap, 255);

    if FGlobalImage.Visible and Assigned(FGlobalImage.Picture.Graphic) and not FGlobalImage.Picture.Graphic.Empty then
    begin
      DrawGraphicWithGDIPlus(LG, FGlobalImage.Picture.Graphic, LGlobalImageRect);
    end;

    if FItems.Count > 0 then
    begin
      if FGroupBorder.Visible and (FGroupBorder.Width > 0) then
      begin
        LGroupRectF := MakeRect(Single(LButtonBlockRect.Left), Single(LButtonBlockRect.Top), Single(LButtonBlockRect.Width), Single(LButtonBlockRect.Height));
        LHalfBorder := FGroupBorder.Width / 2;

        // ===== INÍCIO DA CORREÇÃO (Substituição do InflateRect) =====
        LGroupRectF.X := LGroupRectF.X + LHalfBorder;
        LGroupRectF.Y := LGroupRectF.Y + LHalfBorder;
        LGroupRectF.Width := LGroupRectF.Width - FGroupBorder.Width;
        LGroupRectF.Height := LGroupRectF.Height - FGroupBorder.Width;
        // ===== FIM DA CORREÇÃO =====

        LPath := TGPGraphicsPath.Create;
        try
          CreateGPRoundedPath(LPath, LGroupRectF, FGroupBorder.CornerRadius, rctAll);
          LGroupPen := TGPPen.Create(ColorToARGB(FGroupBorder.Color), FGroupBorder.Width);
          try LG.DrawPath(LGroupPen, LPath); finally LGroupPen.Free; end;
        finally LPath.Free; end;
      end;

      LPath := TGPGraphicsPath.Create;
      try
        LClipRadius := Max(0, FGroupBorder.CornerRadius - Ceil(FGroupBorder.Width / 2));
        var ClipRect := LButtonBlockRect; InflateRect(ClipRect, -FGroupBorder.Width, -FGroupBorder.Width);
        CreateGPRoundedPath(LPath, MakeRect(Single(ClipRect.Left), Single(ClipRect.Top), Single(ClipRect.Width), Single(ClipRect.Height)), LClipRadius, rctAll);
        LG.SetClip(LPath, CombineModeReplace);
      finally LPath.Free; end;

      LCurrentX := LButtonBlockRect.Left + FGroupBorder.Width;
      for i := 0 to FItems.Count - 1 do
      begin
        LItem := FItems.Items[i];
        if not (Assigned(LItem) and LItem.Visible) then Continue;

        LItemRect := Rect(LCurrentX, LButtonBlockRect.Top + FGroupBorder.Width, LCurrentX + LItem.Width, LButtonBlockRect.Bottom - FGroupBorder.Width);
        isDisabled := not (Self.Enabled and LItem.Enabled); isHovered := (LItem = FHoveredItem) and not isDisabled;
        isClicked := (LItem = FClickedItem) and not isDisabled; isSelected := (LItem = FSelectedItem) and not isDisabled;

        var InitialFillColor := LItem.Appearance.Color; var InitialBorderColor := LItem.Appearance.BorderColor;
        LActualFontColor := LItem.Caption.Font.Color;
        if isDisabled then
        begin
          LActualFillColor := BlendColors(InitialFillColor, clGray, 0.65); LActualBorderColor := BlendColors(InitialBorderColor, clGray, 0.7);
          LActualFontColor := clGrayText;
        end
        else
        begin
          LActualFillColor := InitialFillColor; LActualBorderColor := InitialBorderColor;
          if isSelected then
          begin
            LActualFillColor := FSelection.SelectedColor; LActualFontColor := FSelection.SelectedFontColor;
          end
          else
          begin
            if isHovered then
            begin
              if LItem.Appearance.StateColors.HoverColor <> clNone then LActualFillColor := LItem.Appearance.StateColors.HoverColor;
              if LItem.Appearance.StateColors.HoverBorderColor <> clNone then LActualBorderColor := LItem.Appearance.StateColors.HoverBorderColor;
              if LItem.Appearance.StateColors.HoverFontColor <> clNone then LActualFontColor := LItem.Appearance.StateColors.HoverFontColor;
            end;
            if isClicked and (LItem.Appearance.StateColors.ClickColor <> clNone) then
              LActualFillColor := LItem.Appearance.StateColors.ClickColor;
          end;
        end;
        LActualBorderThickness := LItem.Appearance.BorderWidth;
        LPath := TGPGraphicsPath.Create;
        try
          CreateGPRoundedPath(LPath, MakeRect(Single(LItemRect.Left), Single(LItemRect.Top), Single(LItemRect.Width), Single(LItemRect.Height)), LItem.Appearance.CornerRadius, rctAll);
          var FillBrush := TGPSolidBrush.Create(ColorToARGB(LActualFillColor));
          try LG.FillPath(FillBrush, LPath); finally FillBrush.Free; end;
          if LActualBorderThickness > 0 then
          begin
            var BorderPen := TGPPen.Create(ColorToARGB(LActualBorderColor), LActualBorderThickness);
            try LG.DrawPath(BorderPen, LPath); finally BorderPen.Free; end;
          end;
        finally LPath.Free; end;

        LItemCaptionRect := LItemRect;
        var LItemImageContainer: TRect;

        if LItem.Image.Visible and Assigned(LItem.Image.Picture.Graphic) and not LItem.Image.Picture.Graphic.Empty then
        begin
          var LImageW := IfThen(LItem.Image.TargetWidth > 0, LItem.Image.TargetWidth, LItem.Image.Picture.Width);
          var LImageH := IfThen(LItem.Image.TargetHeight > 0, LItem.Image.TargetHeight, LItem.Image.Picture.Height);
          var LImageTotalW := LImageW + LItem.Image.Margins.Left + LItem.Image.Margins.Right;
          var LImageTotalH := LImageH + LItem.Image.Margins.Top + LItem.Image.Margins.Bottom;

          LItemImageRect := Rect(
              LItemImageContainer.Left + LItem.Image.Margins.Left,
              LItemImageContainer.Top + LItem.Image.Margins.Top,
              LItemImageContainer.Right - LItem.Image.Margins.Right,
              LItemImageContainer.Bottom - LItem.Image.Margins.Bottom
          );

          if LItem.Image.DrawMode = idmProportional then
             LItemImageRect := CalcProportionalRect(LItemImageRect, LItem.Image.Picture.Graphic.Width, LItem.Image.Picture.Graphic.Height, True);

          DrawGraphicWithGDIPlus(LG, LItem.Image.Picture.Graphic, LItemImageRect);
        end;

        if LItem.Caption.Visible and (LItem.Caption.Text <> '') then
          DrawComponentCaption(Self.Canvas, LItemCaptionRect, LItem.Caption.Text, LItem.Caption.Font, LActualFontColor, LItem.Caption.Alignment, LItem.Caption.VerticalAlignment, LItem.Caption.WordWrap, 255);

        LCurrentX := LCurrentX + LItem.Width + FSpacing;
      end;
    end;
  finally
    LG.ResetClip;
    LG.Free;
  end;
end;

procedure TANDMR_CButtonGroup.UpdateSize;
var
  i: Integer;
  LRequiredWidth, LRequiredHeight: Integer;
  LButtonsWidth, LButtonsHeight: Integer;
  LImageWidth, LImageHeight: Integer;
  LCaptionWidth, LCaptionHeight: Integer;
  LVisibleItemCount: Integer;
  LMainContentWidth, LMainContentHeight: Integer;
  LSourceGraphic: TGraphic;
  LImgW, LImgH: Integer;
begin
  if (csLoading in ComponentState) or not FAutoSize or not HandleAllocated then
    Exit;

  // Buttons Block
  LButtonsWidth := 0;
  LVisibleItemCount := 0;
  for i := 0 to FItems.Count - 1 do
    if FItems[i].Visible then
    begin
      LButtonsWidth := LButtonsWidth + FItems[i].Width;
      Inc(LVisibleItemCount);
    end;
  if LVisibleItemCount > 1 then
    LButtonsWidth := LButtonsWidth + ((LVisibleItemCount - 1) * FSpacing);
  if FGroupBorder.Visible and (FGroupBorder.Width > 0) then
    LButtonsWidth := LButtonsWidth + (FGroupBorder.Width * 2);
  LButtonsHeight := 40; // Default height, can be improved later

  // Global Image
  LImageWidth := 0;
  LImageHeight := 0;
  if FGlobalImage.Visible and Assigned(FGlobalImage.Picture.Graphic) and not FGlobalImage.Picture.Graphic.Empty then
  begin
    LSourceGraphic := FGlobalImage.Picture.Graphic;
    if FGlobalImage.AutoSize and (FGlobalImage.DrawMode = idmProportional) then
    begin
        // For proportional autosize, reserve a square space based on button height
        LImgW := LButtonsHeight - FGlobalImage.Margins.Top - FGlobalImage.Margins.Bottom;
        LImgH := LImgW;
    end
    else
    begin
        LImgW := IfThen(FGlobalImage.TargetWidth > 0, FGlobalImage.TargetWidth, LSourceGraphic.Width);
        LImgH := IfThen(FGlobalImage.TargetHeight > 0, FGlobalImage.TargetHeight, LSourceGraphic.Height);
    end;
    LImageWidth := LImgW + FGlobalImage.Margins.Left + FGlobalImage.Margins.Right;
    LImageHeight := LImgH + FGlobalImage.Margins.Top + FGlobalImage.Margins.Bottom;
  end;

  // Global Caption
  LCaptionWidth := 0;
  LCaptionHeight := 0;
  if FGlobalCaption.Visible and (FGlobalCaption.Text <> '') then
  begin
    Canvas.Font.Assign(FGlobalCaption.Font);
    LCaptionWidth := Canvas.TextWidth(FGlobalCaption.Text) + FGlobalCaption.Margins.Left + FGlobalCaption.Margins.Right;
    LCaptionHeight := Canvas.TextHeight(FGlobalCaption.Text) + FGlobalCaption.Margins.Top + FGlobalCaption.Margins.Bottom;
  end;

  // Main Content Block (Image + Buttons)
  LMainContentWidth := LImageWidth + LButtonsWidth;
  LMainContentHeight := Max(LButtonsHeight, LImageHeight);

  // Determine final component size based on layout
  case FGlobalCaption.Position of
    cpLeft, cpRight:
    begin
      LRequiredWidth := LMainContentWidth + LCaptionWidth;
      LRequiredHeight := Max(LMainContentHeight, LCaptionHeight);
    end;
    cpAbove, cpBelow:
    begin
      LRequiredWidth := Max(LMainContentWidth, LCaptionWidth);
      LRequiredHeight := LMainContentHeight + LCaptionHeight;
    end;
  else
    // Default case (no caption)
    LRequiredWidth := LMainContentWidth;
    LRequiredHeight := LMainContentHeight;
  end;

  if Self.Width <> LRequiredWidth then
    Self.Width := LRequiredWidth;
  if Self.Height <> LRequiredHeight then
    Self.Height := LRequiredHeight;
end;

function TANDMR_CButtonGroup.GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
begin
  Result := nil;
  if not HandleAllocated then Exit;
  var LButtonBlockRect := GetButtonBlockRect;

  if not PtInRect(LButtonBlockRect, Point(X, Y)) then
    Exit;

  var LCurrentX := LButtonBlockRect.Left + FGroupBorder.Width;
  for var i := 0 to FItems.Count - 1 do
  begin
    var LItem := FItems.Items[i];
    if not (Assigned(LItem) and LItem.Visible) then Continue;

    var LItemRect := Rect(LCurrentX, LButtonBlockRect.Top + FGroupBorder.Width, LCurrentX + LItem.Width, LButtonBlockRect.Bottom - FGroupBorder.Width);
    if PtInRect(LItemRect, Point(X, Y)) then
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

    if FClickedItem <> nil then
    begin
      FClickedItem := nil;
      Invalidate;
    end;
  end;
end;

procedure TANDMR_CButtonGroup.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      UpdateSize;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetItems(const Value: TANDMR_CGroupButtonItems);
begin
  FItems.Assign(Value);
  UpdateSize;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    UpdateSize;
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

procedure TANDMR_CButtonGroup.SettingsChanged(Sender: TObject);
begin
  // If a global template changes, apply it to existing items
  if Sender = FGlobalAppearance then
  begin
    for var i := 0 to FItems.Count - 1 do
      FItems[i].Appearance.Assign(FGlobalAppearance);
  end;
  // For other settings, just repaint
  UpdateSize;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SetGlobalCaption(const Value: TCaptionSettings);
begin
  FGlobalCaption.Assign(Value);
end;

procedure TANDMR_CButtonGroup.SetGlobalImage(const Value: TImageSettings);
begin
  FGlobalImage.Assign(Value);
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
    // Apply new style to all items
    for i := 0 to FItems.Count - 1 do
      FItems[i].Style := Value;
    Invalidate;
  end;
end;

end.
