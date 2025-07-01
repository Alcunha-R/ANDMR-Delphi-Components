unit HTL_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Types,
  Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, System.UITypes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI,
  HTL_CButton, HTL_ComponentUtils;

type
  THTL_CButtonGroup = class; // Declaração antecipada
  TButtonGroupOrientation = (bgoHorizontal, bgoVertical);
  TCaptionPlacement = (plcOutside, plcInside);

  // NOVO: Classe para configurar a aparência do botão selecionado.
  THTL_CSelectionSettings = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBorder: TBorderSettings;
    FCaption: TCaptionSettings;
    FOnChange: TNotifyEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorder(const Value: TBorderSettings);
    procedure SetCaption(const Value: TCaptionSettings);
    procedure SettingsChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: THTL_CButtonGroup);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clHighlight;
    property Border: TBorderSettings read FBorder write SetBorder;
    property Caption: TCaptionSettings read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THTL_CGroupButtonGlobalSettings = class(TPersistent)
  private
    FBorder: TBorderSettings;
    FCaption: TCaptionSettings;
    FImage: TImageSettings;
    FHover: THoverSettings;
    FOnChange: TNotifyEvent;
    FOwner: THTL_CButtonGroup;
    procedure SetBorder(const Value: TBorderSettings);
    procedure SetCaption(const Value: TCaptionSettings);
    procedure SetImage(const Value: TImageSettings);
    procedure SetHover(const Value: THoverSettings);
    procedure SettingsChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: THTL_CButtonGroup);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Border: TBorderSettings read FBorder write SetBorder;
    property Caption: TCaptionSettings read FCaption write SetCaption;
    property Image: TImageSettings read FImage write SetImage;
    property Hover: THoverSettings read FHover write SetHover;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THTL_CGroupButtonItem = class(TCollectionItem)
  private
    FCaption: string;
    FImage: TPicture;
    FVisible: Boolean;
    FEnabled: Boolean;
    FTag: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FBorder: TBorderSettings;
    FHover: THoverSettings;
    FCaptionSettings: TCaptionSettings;
    FImageSettings: TImageSettings;
    procedure SetCaption(const Value: string);
    procedure SetImage(const Value: TPicture);
    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetBorder(const Value: TBorderSettings);
    procedure SetHover(const Value: THoverSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetImageSettings(const Value: TImageSettings);
    procedure SettingsChanged(Sender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: TPicture read FImage write SetImage;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Tag: Integer read FTag write FTag default 0;
    property Width: Integer read FWidth write SetWidth default 75;
    property Height: Integer read FHeight write SetHeight default 30;
    property Border: TBorderSettings read FBorder write SetBorder;
    property Hover: THoverSettings read FHover write SetHover;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
  end;

  THTL_CGroupButtonItems = class(TCollection)
  private
    FOwner: THTL_CButtonGroup;
    function GetItem(Index: Integer): THTL_CGroupButtonItem;
    procedure SetItem(Index: Integer; const Value: THTL_CGroupButtonItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: THTL_CButtonGroup);
    function Add: THTL_CGroupButtonItem;
    property Items[Index: Integer]: THTL_CGroupButtonItem read GetItem write SetItem; default;
    // NOVO: Adiciona a função IndexOf para facilitar a busca de itens.
    function IndexOf(Item: THTL_CGroupButtonItem): Integer;
  end;

  THTL_CButtonGroup = class(TCustomControl)
  private
    FItems: THTL_CGroupButtonItems;
    FBorder: TBorderSettings;
    FCaption: TCaptionSettings;
    FGlobalSettings: THTL_CGroupButtonGlobalSettings;
    FOrientation: TButtonGroupOrientation;
    FSpacing: Integer;
    FAutoSize: Boolean;
    FCaptionPlacement: TCaptionPlacement;
    FHoveredItem: THTL_CGroupButtonItem;
    FClickedItem: THTL_CGroupButtonItem;

    // NOVO: Campos para gerenciamento da seleção
    FSelectedItemIndex: Integer;
    FSelectionSettings: THTL_CSelectionSettings;
    FOnChange: TNotifyEvent;

    procedure SetItems(const Value: THTL_CGroupButtonItems);
    procedure SetBorder(const Value: TBorderSettings);
    procedure SetCaption(const Value: TCaptionSettings);
    procedure SetGlobalSettings(const Value: THTL_CGroupButtonGlobalSettings);
    procedure SetOrientation(const Value: TButtonGroupOrientation);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetCaptionPlacement(const Value: TCaptionPlacement);

    // NOVO: Setters e Getters para as novas propriedades de seleção
    procedure SetItemIndex(const Value: Integer);
    function GetSelectedItem: THTL_CGroupButtonItem;
    procedure SetSelectionSettings(const Value: THTL_CSelectionSettings);

    procedure SettingsChanged(Sender: TObject);
    procedure UpdateLayout;
    function GetItemAt(X, Y: Integer): THTL_CGroupButtonItem;
    function GetItemRect(Item: THTL_CGroupButtonItem): TRect;
    function GetContentRect: TRect;
    // NOVO: Método para disparar o evento OnChange
    procedure DoChange; virtual;

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // ALTERADO
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // NOVO: Propriedades de seleção publicadas
    property ItemIndex: Integer read FSelectedItemIndex write SetItemIndex default -1;
    property SelectedItem: THTL_CGroupButtonItem read GetSelectedItem;
    property SelectionSettings: THTL_CSelectionSettings read FSelectionSettings write SetSelectionSettings;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Items: THTL_CGroupButtonItems read FItems write SetItems;
    property Orientation: TButtonGroupOrientation read FOrientation write SetOrientation default bgoHorizontal;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Border: TBorderSettings read FBorder write SetBorder;
    property Caption: TCaptionSettings read FCaption write SetCaption;
    property CaptionPlacement: TCaptionPlacement read FCaptionPlacement write SetCaptionPlacement default plcOutside;
    property Global: THTL_CGroupButtonGlobalSettings read FGlobalSettings write SetGlobalSettings;

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

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

procedure Register;

implementation

uses
  System.Math;

const
  InternalPadding = 0;

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CButtonGroup]);
end;

//------------------------------------------------------------------------------
// NOVO: THTL_CSelectionSettings
//------------------------------------------------------------------------------
constructor THTL_CSelectionSettings.Create(AOwner: THTL_CButtonGroup);
begin
  inherited Create;
  FBackgroundColor := clHighlight;
  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;
  FBorder.Color := clBlue;
  FBorder.Thickness := 2;

  FCaption := TCaptionSettings.Create(AOwner);
  FCaption.OnChange := SettingsChanged;
  FCaption.Font.Color := clHighlightText;
  FCaption.Font.Style := [fsBold];
end;

destructor THTL_CSelectionSettings.Destroy;
begin
  FBorder.Free;
  FCaption.Free;
  inherited Destroy;
end;

procedure THTL_CSelectionSettings.Assign(Source: TPersistent);
begin
  if Source is THTL_CSelectionSettings then
  begin
    with THTL_CSelectionSettings(Source) do
    begin
      Self.FBackgroundColor := FBackgroundColor;
      Self.FBorder.Assign(FBorder);
      Self.FCaption.Assign(FCaption);
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure THTL_CSelectionSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THTL_CSelectionSettings.SettingsChanged(Sender: TObject);
begin
  Changed;
end;

procedure THTL_CSelectionSettings.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure THTL_CSelectionSettings.SetBorder(const Value: TBorderSettings);
begin
  FBorder.Assign(Value);
end;

procedure THTL_CSelectionSettings.SetCaption(const Value: TCaptionSettings);
begin
  FCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// THTL_CGroupButtonGlobalSettings
//------------------------------------------------------------------------------
constructor THTL_CGroupButtonGlobalSettings.Create(AOwner: THTL_CButtonGroup);
begin
  inherited Create;
  FOwner := AOwner;
  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;
  FBorder.BackgroundColor := clBtnFace;
  FBorder.Color := clDkGray;
  FBorder.CornerRadius := 4;

  FCaption := TCaptionSettings.Create(FOwner);
  FCaption.OnChange := SettingsChanged;

  FImage := TImageSettings.Create(FOwner);
  FImage.OnChange := SettingsChanged;

  FHover := THoverSettings.Create(FOwner);
  FHover.OnChange := SettingsChanged;
end;

destructor THTL_CGroupButtonGlobalSettings.Destroy;
begin
  FBorder.Free;
  FCaption.Free;
  FImage.Free;
  FHover.Free;
  inherited Destroy;
end;

procedure THTL_CGroupButtonGlobalSettings.Assign(Source: TPersistent);
begin
  if Source is THTL_CGroupButtonGlobalSettings then
  begin
    with THTL_CGroupButtonGlobalSettings(Source) do
    begin
      Self.FBorder.Assign(FBorder);
      Self.FCaption.Assign(FCaption);
      Self.FImage.Assign(FImage);
      Self.FHover.Assign(FHover);
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure THTL_CGroupButtonGlobalSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THTL_CGroupButtonGlobalSettings.SettingsChanged(Sender: TObject);
begin
  Changed;
end;

procedure THTL_CGroupButtonGlobalSettings.SetBorder(const Value: TBorderSettings);
begin
  FBorder.Assign(Value);
end;

procedure THTL_CGroupButtonGlobalSettings.SetCaption(const Value: TCaptionSettings);
begin
  FCaption.Assign(Value);
end;

procedure THTL_CGroupButtonGlobalSettings.SetImage(const Value: TImageSettings);
begin
  FImage.Assign(Value);
end;

procedure THTL_CGroupButtonGlobalSettings.SetHover(const Value: THoverSettings);
begin
  FHover.Assign(Value);
end;


//------------------------------------------------------------------------------
// THTL_CGroupButtonItem
//------------------------------------------------------------------------------

constructor THTL_CGroupButtonItem.Create(Collection: TCollection);
var
  LGroup: THTL_CButtonGroup;
begin
  inherited Create(Collection);
  FVisible := True;
  FEnabled := True;
  FWidth := 75;
  FHeight := 30;
  FImage := TPicture.Create;
  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;
  FHover := THoverSettings.Create(nil);
  FHover.OnChange := SettingsChanged;
  FCaptionSettings := TCaptionSettings.Create(nil);
  FCaptionSettings.OnChange := SettingsChanged;
  FImageSettings := TImageSettings.Create(nil);
  FImageSettings.OnChange := SettingsChanged;

  if (Collection <> nil) and (Collection.Owner is THTL_CButtonGroup) then
  begin
    LGroup := THTL_CButtonGroup(Collection.Owner);
    FCaption := 'Button' + IntToStr(Collection.Count);
    FBorder.Assign(LGroup.Global.Border);
    FHover.Assign(LGroup.Global.Hover);
    FCaptionSettings.Assign(LGroup.Global.Caption);
    FImageSettings.Assign(LGroup.Global.Image);
  end;
end;

destructor THTL_CGroupButtonItem.Destroy;
begin
  FImage.Free;
  FBorder.Free;
  FHover.Free;
  FCaptionSettings.Free;
  FImageSettings.Free;
  inherited Destroy;
end;

procedure THTL_CGroupButtonItem.Assign(Source: TPersistent);
begin
  if Source is THTL_CGroupButtonItem then
  begin
    inherited Assign(Source);
    with THTL_CGroupButtonItem(Source) do
    begin
      Self.FCaption := FCaption;
      Self.FImage.Assign(FImage);
      Self.FVisible := FVisible;
      Self.FEnabled := FEnabled;
      Self.FTag := FTag;
      Self.FWidth := FWidth;
      Self.FHeight := FHeight;
      Self.FBorder.Assign(FBorder);
      Self.FHover.Assign(FHover);
      Self.FCaptionSettings.Assign(FCaptionSettings);
      Self.FImageSettings.Assign(FImageSettings);
    end;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function THTL_CGroupButtonItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure THTL_CGroupButtonItem.SettingsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure THTL_CGroupButtonItem.SetCaption(const Value: string); begin if FCaption <> Value then begin FCaption := Value; Changed(False); end; end;
procedure THTL_CGroupButtonItem.SetImage(const Value: TPicture); begin FImage.Assign(Value); Changed(False); end;
procedure THTL_CGroupButtonItem.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed(False); end; end;
procedure THTL_CGroupButtonItem.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed(False); end; end;
procedure THTL_CGroupButtonItem.SetWidth(const Value: Integer); begin if FWidth <> Value then begin FWidth := Value; Changed(False); end; end;
procedure THTL_CGroupButtonItem.SetHeight(const Value: Integer); begin if FHeight <> Value then begin FHeight := Value; Changed(False); end; end;
procedure THTL_CGroupButtonItem.SetBorder(const Value: TBorderSettings); begin FBorder.Assign(Value); end;
procedure THTL_CGroupButtonItem.SetHover(const Value: THoverSettings); begin FHover.Assign(Value); end;
procedure THTL_CGroupButtonItem.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); end;
procedure THTL_CGroupButtonItem.SetImageSettings(const Value: TImageSettings); begin FImageSettings.Assign(Value); end;

//------------------------------------------------------------------------------
// THTL_CGroupButtonItems
//------------------------------------------------------------------------------

constructor THTL_CGroupButtonItems.Create(AOwner: THTL_CButtonGroup);
begin
  inherited Create(THTL_CGroupButtonItem);
  FOwner := AOwner;
end;

function THTL_CGroupButtonItems.Add: THTL_CGroupButtonItem; begin Result := THTL_CGroupButtonItem(inherited Add); end;
function THTL_CGroupButtonItems.GetItem(Index: Integer): THTL_CGroupButtonItem; begin Result := THTL_CGroupButtonItem(inherited GetItem(Index)); end;
function THTL_CGroupButtonItems.GetOwner: TPersistent; begin Result := FOwner; end;
procedure THTL_CGroupButtonItems.SetItem(Index: Integer; const Value: THTL_CGroupButtonItem); begin inherited SetItem(Index, Value); end;
procedure THTL_CGroupButtonItems.Update(Item: TCollectionItem); begin inherited; if Assigned(FOwner) and not (csLoading in FOwner.ComponentState) then FOwner.UpdateLayout; end;

// NOVO
function THTL_CGroupButtonItems.IndexOf(Item: THTL_CGroupButtonItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i] = Item then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
// THTL_CButtonGroup
//------------------------------------------------------------------------------

constructor THTL_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csReplicatable, csDoubleClicks, csCaptureMouse, csAcceptsControls, csSetCaption];
  DoubleBuffered := True;
  Width := 200;
  Height := 50;
  TabStop := True;

  FItems := THTL_CGroupButtonItems.Create(Self);
  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;
  FBorder.Visible := True;
  FBorder.CornerRadius := 6;
  FBorder.Color := clGray;

  FCaption := TCaptionSettings.Create(Self);
  FCaption.OnChange := SettingsChanged;
  FCaption.Text := 'Button Group';

  FGlobalSettings := THTL_CGroupButtonGlobalSettings.Create(Self);
  FGlobalSettings.OnChange := SettingsChanged;

  // NOVO: Inicializa campos e objetos de seleção
  FSelectedItemIndex := -1;
  FSelectionSettings := THTL_CSelectionSettings.Create(Self);
  FSelectionSettings.OnChange := SettingsChanged;

  FOrientation := bgoHorizontal;
  FSpacing := 4;
  FAutoSize := True;
  FCaptionPlacement := plcOutside;
  FHoveredItem := nil;
  FClickedItem := nil;
end;

destructor THTL_CButtonGroup.Destroy;
begin
  FItems.Free;
  FBorder.Free;
  FCaption.Free;
  FGlobalSettings.Free;
  FSelectionSettings.Free; // NOVO
  inherited Destroy;
end;

procedure THTL_CButtonGroup.Loaded; begin inherited; UpdateLayout; end;
procedure THTL_CButtonGroup.Resize; begin inherited; UpdateLayout; Invalidate; end;

procedure THTL_CButtonGroup.SettingsChanged(Sender: TObject);
var
  i: Integer;
begin
  if Sender = FGlobalSettings then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      FItems[i].Border.Assign(FGlobalSettings.Border);
      FItems[i].Hover.Assign(FGlobalSettings.Hover);
      FItems[i].CaptionSettings.Assign(FGlobalSettings.Caption);
      FItems[i].ImageSettings.Assign(FGlobalSettings.Image);
    end;
  end;
  UpdateLayout;
  Invalidate; // ALTERADO: Garante a repintura em qualquer mudança
end;

function THTL_CButtonGroup.GetContentRect: TRect;
var
  LCaptionHeight, LCaptionWidth: Integer;
begin
  Result := ClientRect;
  if (FCaptionPlacement = plcOutside) and FCaption.Visible and (FCaption.Text <> '') then
  begin
    Self.Canvas.Font.Assign(FCaption.Font);
    var TempRect := Rect(0, 0, 32767, 32767);
    DrawText(Self.Canvas.Handle, PChar(FCaption.Text), -1, TempRect, DT_CALCRECT or DT_SINGLELINE);
    LCaptionHeight := TempRect.Height + FCaption.Margins.Top + FCaption.Margins.Bottom + Abs(FCaption.Offset.Y);
    LCaptionWidth := TempRect.Width + FCaption.Margins.Left + FCaption.Margins.Right + Abs(FCaption.Offset.X);

    case FCaption.Position of
      cpAbove: Result.Top := Result.Top + LCaptionHeight;
      cpBelow: Result.Bottom := Result.Bottom - LCaptionHeight;
      cpLeft: Result.Left := Result.Left + LCaptionWidth;
      cpRight: Result.Right := Result.Right - LCaptionWidth;
    end;
  end;
  if FBorder.Visible then
  begin
    InflateRect(Result, -FBorder.Thickness, -FBorder.Thickness);
    InflateRect(Result, -InternalPadding, -InternalPadding);
  end;
  if (FCaptionPlacement = plcInside) and FCaption.Visible and (FCaption.Text <> '') then
  begin
    Self.Canvas.Font.Assign(FCaption.Font);
    var TempRect := Rect(0, 0, Result.Width, 32767);
    DrawText(Self.Canvas.Handle, PChar(FCaption.Text), -1, TempRect, DT_CALCRECT or IfThen(FCaption.WordWrap, DT_WORDBREAK, 0));
    LCaptionHeight := TempRect.Height + FCaption.Margins.Top + FCaption.Margins.Bottom;
    LCaptionWidth := TempRect.Width + FCaption.Margins.Left + FCaption.Margins.Right;

    case FCaption.Position of
      cpAbove: Result.Top := Result.Top + LCaptionHeight + FCaption.Offset.Y;
      cpBelow: Result.Bottom := Result.Bottom - LCaptionHeight - FCaption.Offset.Y;
      cpLeft: Result.Left := Result.Left + LCaptionWidth + FCaption.Offset.X;
      cpRight: Result.Right := Result.Right - LCaptionWidth - FCaption.Offset.X;
    end;
  end;
  if Result.Right < Result.Left then Result.Right := Result.Left;
  if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
end;


procedure THTL_CButtonGroup.UpdateLayout;
var
  i: Integer;
  BtnContentW, BtnContentH, MaxItemW, MaxItemH: Integer;
  CaptionW, CaptionH, FrameW, FrameH, TotalW, TotalH: Integer;
  ContentW, ContentH: Integer;
begin
  if (csLoading in ComponentState) or not FAutoSize then Exit;
  BtnContentW := 0; BtnContentH := 0; MaxItemW := 0; MaxItemH := 0;
  if FItems.Count > 0 then
  begin
    for i := 0 to FItems.Count - 1 do
      if FItems[i].Visible then
      begin
        if FOrientation = bgoHorizontal then
        begin
          if BtnContentW > 0 then BtnContentW := BtnContentW + FSpacing;
          BtnContentW := BtnContentW + FItems[i].Width;
          if FItems[i].Height > MaxItemH then MaxItemH := FItems[i].Height;
        end
        else
        begin
          if BtnContentH > 0 then BtnContentH := BtnContentH + FSpacing;
          BtnContentH := BtnContentH + FItems[i].Height;
          if FItems[i].Width > MaxItemW then MaxItemW := FItems[i].Width;
        end;
      end;
    if FOrientation = bgoHorizontal then BtnContentH := MaxItemH else BtnContentW := MaxItemW;
  end;
  CaptionW := 0; CaptionH := 0;
  if FCaption.Visible and (FCaption.Text <> '') then
  begin
    Self.Canvas.Font.Assign(FCaption.Font);
    var TempRect := Rect(0,0, 32767, 32767);
    DrawText(Self.Canvas.Handle, PChar(FCaption.Text), -1, TempRect, DT_CALCRECT or DT_SINGLELINE);
    CaptionH := TempRect.Height + FCaption.Margins.Top + FCaption.Margins.Bottom + Abs(FCaption.Offset.Y);
    CaptionW := TempRect.Width + FCaption.Margins.Left + FCaption.Margins.Right + Abs(FCaption.Offset.X);
  end;
  ContentW := BtnContentW;
  ContentH := BtnContentH;
  if (FCaptionPlacement = plcInside) and (CaptionW > 0) and (CaptionH > 0) then
  begin
      case FCaption.Position of
        cpAbove, cpBelow: ContentH := Max(ContentH, BtnContentH + CaptionH);
        cpLeft, cpRight:  ContentW := Max(ContentW, BtnContentW + CaptionW);
      end;
  end;
  FrameW := 0; FrameH := 0;
  if FBorder.Visible then
  begin
    FrameW := FBorder.Thickness * 2;
    FrameH := FBorder.Thickness * 2;
  end;
  FrameW := FrameW + (InternalPadding * 2);
  FrameH := FrameH + (InternalPadding * 2);
  TotalW := ContentW + FrameW;
  TotalH := ContentH + FrameH;
  if (FCaptionPlacement = plcOutside) and (CaptionW > 0) and (CaptionH > 0) then
  begin
    case FCaption.Position of
      cpAbove, cpBelow: TotalH := TotalH + CaptionH;
      cpLeft, cpRight:  TotalW := TotalW + CaptionW;
    end;
  end;
  if (Self.Width <> TotalW) or (Self.Height <> TotalH) then
    Self.SetBounds(Self.Left, Self.Top, TotalW, TotalH);
  Invalidate;
end;

procedure THTL_CButtonGroup.Paint;
var
  LG: TGPGraphics;
  i: Integer;
  Item: THTL_CGroupButtonItem;
  ItemRect: TRect;
  ItemBackgroundColor: TColor;
  ItemBorderSettings: TBorderSettings;    // Usado para as configurações de borda
  ItemCaptionSettings: TCaptionSettings;  // Usado para as configurações de texto
  LCaptionPaintRect, LBorderRect: TRect;
  MainPath: TGPGraphicsPath;
  MainClipRegion: TGPRegion;
  OriginalClip: TGPRegion;
  GPRect: TGPRectF;
  LIsSelected: Boolean; // NOVO
begin
  inherited;
  LG := TGPGraphics.Create(Self.Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeHighQuality);
    LG.SetInterpolationMode(InterpolationModeHighQualityBicubic);
    LG.SetPixelOffsetMode(PixelOffsetModeHighQuality);

    LBorderRect := ClientRect;
    if (FCaptionPlacement = plcOutside) and FCaption.Visible and (FCaption.Text <> '') then
    begin
        Self.Canvas.Font.Assign(FCaption.Font);
        var TempRect := Rect(0,0, 32767, 32767);
        DrawText(Self.Canvas.Handle, PChar(FCaption.Text), -1, TempRect, DT_CALCRECT or DT_SINGLELINE);
        var LCaptionHeight := TempRect.Height + FCaption.Margins.Top + FCaption.Margins.Bottom + Abs(FCaption.Offset.Y);
        var LCaptionWidth := TempRect.Width + FCaption.Margins.Left + FCaption.Margins.Right + Abs(FCaption.Offset.X);
        case FCaption.Position of
          cpAbove: LBorderRect.Top := LBorderRect.Top + LCaptionHeight;
          cpBelow: LBorderRect.Bottom := LBorderRect.Bottom - LCaptionHeight;
          cpLeft:  LBorderRect.Left := LBorderRect.Left + LCaptionWidth;
          cpRight: LBorderRect.Right := LBorderRect.Right - LCaptionWidth;
        end;
    end;

    MainPath := TGPGraphicsPath.Create;
    try
      var DrawRect := LBorderRect;
      if FBorder.Visible and (FBorder.Thickness > 0) then
      begin
        InflateRect(DrawRect, -1, -1);
      end;
      GPRect.X := DrawRect.Left; GPRect.Y := DrawRect.Top;
      GPRect.Width := DrawRect.Width; GPRect.Height := DrawRect.Height;
      CreateGPRoundedPath(MainPath, GPRect, FBorder.CornerRadius, FBorder.RoundCornerType);

      MainClipRegion := TGPRegion.Create(MainPath);
      OriginalClip := TGPRegion.Create;
      LG.GetClip(OriginalClip);
      LG.SetClip(MainClipRegion, CombineModeReplace);
      try
        // LÓGICA DE DESENHO ALTERADA
        for i := 0 to FItems.Count - 1 do
        begin
          Item := FItems[i];
          if not Item.Visible then Continue;

          ItemRect := GetItemRect(Item);
          if IsRectEmpty(ItemRect) then continue;

          LIsSelected := (i = FSelectedItemIndex);

          // Determina as configurações a serem usadas com base no estado (Selecionado, Hover, Normal)
          if LIsSelected and Self.Enabled then
          begin
            ItemBackgroundColor := FSelectionSettings.BackgroundColor;
            ItemBorderSettings := FSelectionSettings.Border;
            ItemCaptionSettings := FSelectionSettings.Caption;
          end
          else
          begin
            ItemBackgroundColor := Item.Border.BackgroundColor;
            ItemBorderSettings := Item.Border;
            ItemCaptionSettings := Item.CaptionSettings;
          end;

          // Aplica o efeito de Hover sobre a cor já definida (seja normal ou selecionada)
          if (Item = FHoveredItem) and Self.Enabled then
          begin
             if Item.Hover.BackgroundColor <> clNone then ItemBackgroundColor := Item.Hover.BackgroundColor;
             if Item.Hover.BorderColor <> clNone then ItemBorderSettings.Color := Item.Hover.BorderColor;
             if Item.Hover.FontColor <> clNone then ItemCaptionSettings.Font.Color := Item.Hover.FontColor;
          end;

          DrawEditBox(LG, ItemRect, ItemBackgroundColor, ItemBorderSettings.Color, ItemBorderSettings.Thickness,
            ItemBorderSettings.Style, 0, rctNone, 255);

          var ButtonContentRect := ItemRect;
          InflateRect(ButtonContentRect, -ItemBorderSettings.Thickness, -ItemBorderSettings.Thickness);

          if (Item.Caption <> '') and Assigned(ItemCaptionSettings) then
          begin
            DrawComponentCaption(Self.Canvas, ButtonContentRect, Item.Caption, ItemCaptionSettings.Font,
              ItemCaptionSettings.Font.Color, ItemCaptionSettings.Alignment, ItemCaptionSettings.VerticalAlignment, ItemCaptionSettings.WordWrap, 255);
          end;
        end;
      finally
        LG.SetClip(OriginalClip, CombineModeReplace);
        OriginalClip.Free;
        MainClipRegion.Free;
      end;
    finally
      MainPath.Free;
    end;

    if FBorder.Visible then
    begin
        DrawEditBox(LG, LBorderRect, clNone, FBorder.Color, FBorder.Thickness, FBorder.Style, FBorder.CornerRadius, FBorder.RoundCornerType, 255);
    end;
    if FCaption.Visible and (FCaption.Text <> '') then
    begin
      LCaptionPaintRect := ClientRect;
      if FCaptionPlacement = plcInside then
      begin
        if FBorder.Visible then InflateRect(LCaptionPaintRect, -FBorder.Thickness, -FBorder.Thickness);
      end
      else
      begin
        var TempBorderRect := ClientRect;
        Self.Canvas.Font.Assign(FCaption.Font);
        var TempTextRect := Rect(0,0, 32767, 32767);
        DrawText(Self.Canvas.Handle, PChar(FCaption.Text), -1, TempTextRect, DT_CALCRECT or DT_SINGLELINE);
        var LCapHeight := TempTextRect.Height + FCaption.Margins.Top + FCaption.Margins.Bottom + Abs(FCaption.Offset.Y);
        var LCapWidth  := TempTextRect.Width + FCaption.Margins.Left + FCaption.Margins.Right + Abs(FCaption.Offset.X);
        case FCaption.Position of
          cpAbove: LCaptionPaintRect := System.Types.Rect(TempBorderRect.Left, TempBorderRect.Top, TempBorderRect.Right, TempBorderRect.Top + LCapHeight);
          cpBelow: LCaptionPaintRect := System.Types.Rect(TempBorderRect.Left, TempBorderRect.Bottom - LCapHeight, TempBorderRect.Right, TempBorderRect.Bottom);
          cpLeft:  LCaptionPaintRect := System.Types.Rect(TempBorderRect.Left, TempBorderRect.Top, TempBorderRect.Left + LCapWidth, TempBorderRect.Bottom);
          cpRight: LCaptionPaintRect := System.Types.Rect(TempBorderRect.Right - LCapWidth, TempBorderRect.Top, TempBorderRect.Right, TempBorderRect.Bottom);
        end;
      end;
      DrawComponentCaption(Self.Canvas, LCaptionPaintRect, FCaption.Text,
        FCaption.Font, FCaption.Color, FCaption.Alignment, FCaption.VerticalAlignment,
        FCaption.WordWrap, 255);
    end;
  finally
    LG.Free;
  end;
end;

function THTL_CButtonGroup.GetItemRect(Item: THTL_CGroupButtonItem): TRect;
var
  i: Integer;
  CurrentX, CurrentY: Integer;
  LContentRect: TRect;
  ItemX, ItemY: Integer;
begin
  Result := Rect(0,0,0,0);
  LContentRect := GetContentRect;
  CurrentX := LContentRect.Left;
  CurrentY := LContentRect.Top;
  for i := 0 to FItems.Count - 1 do
  begin
    if not FItems[i].Visible then continue;
    if FItems[i] = Item then
    begin
      if FOrientation = bgoHorizontal then
      begin
        ItemY := LContentRect.Top + (LContentRect.Height - Item.Height) div 2;
        Result := Rect(CurrentX, ItemY, CurrentX + Item.Width, ItemY + Item.Height);
      end
      else
      begin
        ItemX := LContentRect.Left + (LContentRect.Width - Item.Width) div 2;
        Result := Rect(ItemX, CurrentY, ItemX + Item.Width, CurrentY + Item.Height);
      end;
      Exit;
    end;
    if FOrientation = bgoHorizontal then
      CurrentX := CurrentX + FItems[i].Width + FSpacing
    else
      CurrentY := CurrentY + FItems[i].Height + FSpacing;
  end;
end;

function THTL_CButtonGroup.GetItemAt(X, Y: Integer): THTL_CGroupButtonItem;
var
  i: Integer;
  ItemRect: TRect;
begin
  Result := nil;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems[i].Visible and FItems[i].Enabled then
    begin
      ItemRect := GetItemRect(FItems[i]);
      if PtInRect(ItemRect, Point(X, Y)) then
      begin
        Result := FItems[i];
        Exit;
      end;
    end;
  end;
end;

procedure THTL_CButtonGroup.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin inherited; if Button = mbLeft then begin FClickedItem := GetItemAt(X, Y); Invalidate; end; end;
procedure THTL_CButtonGroup.MouseMove(Shift: TShiftState; X, Y: Integer); var Item: THTL_CGroupButtonItem; begin inherited; Item := GetItemAt(X, Y); if Item <> FHoveredItem then begin FHoveredItem := Item; Invalidate; end; end;

// ALTERADO
procedure THTL_CButtonGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: THTL_CGroupButtonItem;
begin
  inherited;
  if Button = mbLeft then
  begin
    Item := GetItemAt(X, Y);
    // Verifica se o clique foi liberado sobre o mesmo botão que foi pressionado
    if (Item <> nil) and (Item = FClickedItem) then
    begin
      // Define o item clicado como o selecionado
      Self.ItemIndex := FItems.IndexOf(Item);
      // Dispara o evento OnClick do grupo, se houver
      Click;
    end;
    FClickedItem := nil;
    // A repintura já é tratada pelo setter de ItemIndex
  end;
end;

procedure THTL_CButtonGroup.CMMouseLeave(var Message: TMessage); begin inherited; if FHoveredItem <> nil then begin FHoveredItem := nil; Invalidate; end; end;

// --- Setters ---
procedure THTL_CButtonGroup.SetItems(const Value: THTL_CGroupButtonItems); begin FItems.Assign(Value); UpdateLayout; end;
procedure THTL_CButtonGroup.SetBorder(const Value: TBorderSettings); begin FBorder.Assign(Value); UpdateLayout; end;
procedure THTL_CButtonGroup.SetCaption(const Value: TCaptionSettings); begin FCaption.Assign(Value); UpdateLayout; end;
procedure THTL_CButtonGroup.SetGlobalSettings(const Value: THTL_CGroupButtonGlobalSettings); begin FGlobalSettings.Assign(Value); UpdateLayout; end;
procedure THTL_CButtonGroup.SetOrientation(const Value: TButtonGroupOrientation); begin if FOrientation <> Value then begin FOrientation := Value; UpdateLayout; end; end;
procedure THTL_CButtonGroup.SetSpacing(const Value: Integer); begin if FSpacing <> Value then begin FSpacing := Value; UpdateLayout; end; end;
procedure THTL_CButtonGroup.SetAutoSize(const Value: Boolean); begin if FAutoSize <> Value then begin FAutoSize := Value; UpdateLayout; end; end;
procedure THTL_CButtonGroup.SetCaptionPlacement(const Value: TCaptionPlacement); begin if FCaptionPlacement <> Value then begin FCaptionPlacement := Value; UpdateLayout; Invalidate; end; end;

// --- NOVO: Métodos de Seleção ---
procedure THTL_CButtonGroup.SetItemIndex(const Value: Integer);
begin
  // Permite valores de -1 (nenhum selecionado) até o último item.
  if (Value < -1) or (Value >= FItems.Count) then
    Exit; // Ou pode-se levantar uma exceção: raise EListError.Create('Índice fora dos limites');

  if FSelectedItemIndex <> Value then
  begin
    FSelectedItemIndex := Value;
    DoChange; // Dispara o evento OnChange
    Invalidate; // Força a repintura para mostrar o novo estado visual
  end;
end;

function THTL_CButtonGroup.GetSelectedItem: THTL_CGroupButtonItem;
begin
  if (FSelectedItemIndex >= 0) and (FSelectedItemIndex < FItems.Count) then
    Result := FItems[FSelectedItemIndex]
  else
    Result := nil;
end;

procedure THTL_CButtonGroup.SetSelectionSettings(const Value: THTL_CSelectionSettings);
begin
  FSelectionSettings.Assign(Value);
  Invalidate;
end;

procedure THTL_CButtonGroup.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
