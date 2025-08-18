unit HTL_CExpandingButton;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, Vcl.Graphics,
  Winapi.Windows, Winapi.Messages, Vcl.ExtCtrls, Vcl.StdCtrls,
  HTL_ComponentUtils, HTL_CButton, HTL_CButtonGroup, Winapi.GDIPOBJ, Winapi.GDIPAPI;

type
  { Declarações antecipadas }
  THTL_CExpandingButton = class;

  {
    THTL_CExpandingButtonItem: Define um único botão que aparece quando o componente se expande.
  }
  THTL_CExpandingButtonItem = class(TCollectionItem)
  private
    FCaption: string;
    FImage: TPicture;
    FEnabled: Boolean;
    FVisible: Boolean;
    FTag: NativeInt;
    FHint: string;
    FImageSettings: TImageSettings;
    procedure SetCaption(const Value: string);
    procedure SetImage(const Value: TPicture);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
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
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: NativeInt read FTag write FTag;
    property Hint: string read FHint write FHint;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
  end;

  {
    THTL_CExpandingButtonItems: Uma coleção para armazenar todos os THTL_CExpandingButtonItem.
  }
  THTL_CExpandingButtonItems = class(TCollection)
  private
    FOwner: THTL_CExpandingButton;
    function GetItem(Index: Integer): THTL_CExpandingButtonItem;
    procedure SetItem(Index: Integer; const Value: THTL_CExpandingButtonItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: THTL_CExpandingButton);
    function Add: THTL_CExpandingButtonItem;
    property Items[Index: Integer]: THTL_CExpandingButtonItem read GetItem write SetItem; default;
  end;

  { Evento disparado quando um dos botões expandidos é clicado. }
  TExpandingButtonItemClickEvent = procedure(Sender: TObject; Item: THTL_CExpandingButtonItem) of object;

  {
    THTL_CExpandingButton: O componente principal.
    Ele herda de THTL_CButton para reutilizar a aparência e o comportamento do botão principal.
  }
  THTL_CExpandingButton = class(THTL_CButton)
  private
    FItems: THTL_CExpandingButtonItems;
    FExpanded: Boolean;
    FCollapseTimer: TTimer;
    FCollapseDelay: Cardinal;
    FSpacing: Integer;
    FOrientation: TButtonGroupOrientation;
    FHoveredItem: THTL_CExpandingButtonItem;
    FClickedItem: THTL_CExpandingButtonItem;
    FOnItemClick: TExpandingButtonItemClickEvent;

    { Propriedades para definir a aparência dos itens expandidos }
    FItemWidth: Integer;
    FItemHeight: Integer;
    FItemSettings: THTL_CGroupButtonGlobalSettings;

    { Campos internos para gerenciar o tamanho }
    FOriginalWidth: Integer;
    FOriginalHeight: Integer;

    procedure SetItems(const Value: THTL_CExpandingButtonItems);
    procedure SetCollapseDelay(const Value: Cardinal);
    procedure SetSpacing(const Value: Integer);
    procedure SetOrientation(const Value: TButtonGroupOrientation);
    procedure SetItemSettings(const Value: THTL_CGroupButtonGlobalSettings);
    procedure SetItemWidth(const Value: Integer);
    procedure SetItemHeight(const Value: Integer);

    procedure CollapseTimerHandler(Sender: TObject);
    procedure Expand;
    procedure Collapse(AImmediate: Boolean = False);
    procedure UpdateBounds(AIsExpanded: Boolean);
    function GetItemAt(X, Y: Integer): THTL_CExpandingButtonItem;
    function GetItemRect(AIndex: Integer): TRect;
    procedure ItemsChanged(Sender: TObject);

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Propriedades principais do comportamento de expansão }
    property Items: THTL_CExpandingButtonItems read FItems write SetItems;
    property CollapseDelay: Cardinal read FCollapseDelay write SetCollapseDelay default 500;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Orientation: TButtonGroupOrientation read FOrientation write SetOrientation default bgoHorizontal;

    { Propriedades para customizar a aparência dos itens expandidos }
    property ItemWidth: Integer read FItemWidth write SetItemWidth default 40;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 40;
    property ItemSettings: THTL_CGroupButtonGlobalSettings read FItemSettings write SetItemSettings;

    { Evento de clique nos itens }
    property OnItemClick: TExpandingButtonItemClickEvent read FOnItemClick write FOnItemClick;

    { Propriedades republicadas de THTL_CButton para o botão principal }
    property Caption;
    property Image;
    property Style;
    property BorderSettings;
    property CaptionSettings;
    property HoverSettings;
    property ClickSettings;
    property GradientSettings;
    property Enabled;
    property OnClick;
    property Align;
    property Anchors;
    property Constraints;
    property TabStop;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CExpandingButton]);
end;

{ THTL_CExpandingButtonItem }

constructor THTL_CExpandingButtonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FEnabled := True;
  FImage := TPicture.Create;
  FImageSettings := TImageSettings.Create(nil);
  FImageSettings.OnChange := SettingsChanged;
  if (Collection <> nil) and (Collection.Owner is THTL_CExpandingButton) then
  begin
    FCaption := 'Item' + IntToStr(Collection.Count);
  end;
end;

destructor THTL_CExpandingButtonItem.Destroy;
begin
  FImage.Free;
  FImageSettings.Free;
  inherited Destroy;
end;

procedure THTL_CExpandingButtonItem.Assign(Source: TPersistent);
begin
  if Source is THTL_CExpandingButtonItem then
  begin
    with THTL_CExpandingButtonItem(Source) do
    begin
      Self.FCaption := FCaption;
      Self.FImage.Assign(FImage);
      Self.FEnabled := FEnabled;
      Self.FVisible := FVisible;
      Self.FTag := FTag;
      Self.FHint := FHint;
      Self.FImageSettings.Assign(FImageSettings);
    end;
  end else
    inherited Assign(Source);
end;

function THTL_CExpandingButtonItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure THTL_CExpandingButtonItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure THTL_CExpandingButtonItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  Changed(False);
end;

procedure THTL_CExpandingButtonItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure THTL_CExpandingButtonItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure THTL_CExpandingButtonItem.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
end;

procedure THTL_CExpandingButtonItem.SettingsChanged(Sender: TObject);
begin
  Changed(False);
end;


{ THTL_CExpandingButtonItems }

constructor THTL_CExpandingButtonItems.Create(AOwner: THTL_CExpandingButton);
begin
  inherited Create(THTL_CExpandingButtonItem);
  FOwner := AOwner;
end;

function THTL_CExpandingButtonItems.Add: THTL_CExpandingButtonItem;
begin
  Result := THTL_CExpandingButtonItem(inherited Add);
end;

function THTL_CExpandingButtonItems.GetItem(Index: Integer): THTL_CExpandingButtonItem;
begin
  Result := THTL_CExpandingButtonItem(inherited GetItem(Index));
end;

function THTL_CExpandingButtonItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure THTL_CExpandingButtonItems.SetItem(Index: Integer; const Value: THTL_CExpandingButtonItem);
begin
  inherited SetItem(Index, Value);
end;

procedure THTL_CExpandingButtonItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOwner) and not (csLoading in FOwner.ComponentState) then
    FOwner.ItemsChanged(Self);
end;

{ THTL_CExpandingButton }

constructor THTL_CExpandingButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Inicializa os novos objetos e propriedades }
  FItems := THTL_CExpandingButtonItems.Create(Self);
  FItemSettings := THTL_CGroupButtonGlobalSettings.Create(nil);
  FItemSettings.OnChange := ItemsChanged;

  FCollapseTimer := TTimer.Create(Self);
  FCollapseTimer.Enabled := False;
  FCollapseTimer.OnTimer := CollapseTimerHandler;

  FExpanded := False;
  FCollapseDelay := 500;
  FSpacing := 2;
  FOrientation := bgoHorizontal;
  FHoveredItem := nil;
  FClickedItem := nil;
  FItemWidth := 40;
  FItemHeight := 40;

  { Armazena o tamanho original }
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

destructor THTL_CExpandingButton.Destroy;
begin
  FItems.Free;
  FItemSettings.Free;
  FCollapseTimer.Free;
  inherited Destroy;
end;

procedure THTL_CExpandingButton.Loaded;
begin
  inherited;
  { Armazena o tamanho definido no design-time como o tamanho "colapsado" }
  FOriginalWidth := Self.Width;
  FOriginalHeight := Self.Height;
end;

procedure THTL_CExpandingButton.Paint;
var
  LG: TGPGraphics;
  i: Integer;
  Item: THTL_CExpandingButtonItem;
  ItemRect: TRect;
  ItemBackgroundColor, ItemBorderColor: TColor;
  ItemCaptionFont: TFont;
begin
  { Se não estiver expandido, usa a rotina de pintura da classe pai (THTL_CButton) }
  if not FExpanded or (Items.Count = 0) then
  begin
    inherited Paint;
    Exit;
  end;

  { Lógica de pintura para o estado expandido }
  inherited Paint; // Pinta o fundo primeiro

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    for i := 0 to FItems.Count - 1 do
    begin
      Item := FItems[i];
      if not Item.Visible then Continue;

      ItemRect := GetItemRect(i);
      if IsRectEmpty(ItemRect) then Continue;

      { Define as cores base }
      ItemBackgroundColor := FItemSettings.Border.BackgroundColor;
      ItemBorderColor := FItemSettings.Border.Color;
      ItemCaptionFont := TFont.Create;
      try
        ItemCaptionFont.Assign(FItemSettings.Caption.Font);

        { Aplica efeito de hover se o mouse estiver sobre o item }
        if (Item = FHoveredItem) and Item.Enabled then
        begin
          if FItemSettings.Hover.BackgroundColor <> clNone then
            ItemBackgroundColor := FItemSettings.Hover.BackgroundColor;
          if FItemSettings.Hover.BorderColor <> clNone then
            ItemBorderColor := FItemSettings.Hover.BorderColor;
          if FItemSettings.Hover.FontColor <> clNone then
            ItemCaptionFont.Color := FItemSettings.Hover.FontColor;
        end;

        { Desabilita o item visualmente }
        if not Item.Enabled then
        begin
            ItemBackgroundColor := BlendColors(ItemBackgroundColor, clGray, 0.65);
            ItemBorderColor := BlendColors(ItemBorderColor, clGray, 0.7);
            ItemCaptionFont.Color := clGrayText;
        end;

        { Desenha a caixa do item }
        DrawEditBox(LG, ItemRect, ItemBackgroundColor, ItemBorderColor,
          FItemSettings.Border.Thickness, FItemSettings.Border.Style,
          FItemSettings.Border.CornerRadius, FItemSettings.Border.RoundCornerType, 255);

        var ContentRect := ItemRect;
        InflateRect(ContentRect, -FItemSettings.Border.Thickness, -FItemSettings.Border.Thickness);

        { Desenha a imagem e o texto do item }
        var ImageRect := ContentRect;
        var TextRect := ContentRect;

        if (Item.Image.Graphic <> nil) and not Item.Image.Graphic.Empty then
        begin
            DrawGraphicWithGDI(LG, Item.Image.Graphic, ImageRect, Item.ImageSettings.DrawMode);
        end;

        if (Item.Caption <> '') then
        begin
          DrawComponentCaption(Self.Canvas, TextRect, Item.Caption, ItemCaptionFont,
            ItemCaptionFont.Color, FItemSettings.Caption.Alignment,
            FItemSettings.Caption.VerticalAlignment, FItemSettings.Caption.WordWrap, 255);
        end;

      finally
        ItemCaptionFont.Free;
      end;
    end;
  finally
    LG.Free;
  end;
end;

procedure THTL_CExpandingButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not Enabled then Exit;
  FCollapseTimer.Enabled := False;
  Expand;
end;

procedure THTL_CExpandingButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not Enabled then Exit;
  FCollapseTimer.Enabled := False; { Cancela qualquer timer anterior }
  FCollapseTimer.Interval := FCollapseDelay;
  FCollapseTimer.Enabled := True;
end;

procedure THTL_CExpandingButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FExpanded and (Button = mbLeft) then
  begin
    FClickedItem := GetItemAt(X, Y);
    Invalidate;
  end
  else
  begin
    { Se não estiver expandido, usa o comportamento do botão normal }
    inherited;
  end;
end;

procedure THTL_CExpandingButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: THTL_CExpandingButtonItem;
begin
  if FExpanded then
  begin
    Item := GetItemAt(X, Y);
    if Item <> FHoveredItem then
    begin
      FHoveredItem := Item;
      Invalidate;
      if FHoveredItem <> nil then
        Self.Hint := FHoveredItem.Hint
      else
        Self.Hint := '';
    end;
    { Se o mouse está sobre a área expandida, reinicia o timer de colapso }
    FCollapseTimer.Enabled := False;
    FCollapseTimer.Enabled := True;
  end
  else
  begin
    inherited;
  end;
end;

procedure THTL_CExpandingButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: THTL_CExpandingButtonItem;
begin
  if FExpanded and (Button = mbLeft) and Assigned(FClickedItem) then
  begin
    Item := GetItemAt(X, Y);
    if (Item = FClickedItem) and Item.Enabled then
    begin
      if Assigned(FOnItemClick) then
        FOnItemClick(Self, Item);
      Collapse(True); // Colapsa imediatamente após o clique
    end;
    FClickedItem := nil;
    Invalidate;
  end
  else
  begin
    inherited;
  end;
end;

procedure THTL_CExpandingButton.Click;
begin
  { O OnClick padrão só é disparado se o botão não estiver expandido. }
  if not FExpanded then
  begin
    inherited Click;
  end;
end;

procedure THTL_CExpandingButton.Expand;
begin
  if FExpanded or (Items.Count = 0) then Exit;

  FExpanded := True;
  UpdateBounds(True);
  Invalidate;
end;

procedure THTL_CExpandingButton.Collapse(AImmediate: Boolean = False);
var
  P: TPoint;
begin
  if not FExpanded then Exit;

  { **CORRIGIDO** Verifica se o mouse ainda está sobre o controle antes de colapsar }
  P := Self.ScreenToClient(Mouse.CursorPos);
  if not PtInRect(Self.ClientRect, P) or AImmediate then
  begin
      FExpanded := False;
      FHoveredItem := nil;
      FClickedItem := nil;
      FCollapseTimer.Enabled := False;
      UpdateBounds(False);
      Invalidate;
  end;
end;

procedure THTL_CExpandingButton.CollapseTimerHandler(Sender: TObject);
begin
  Collapse;
end;

procedure THTL_CExpandingButton.UpdateBounds(AIsExpanded: Boolean);
var
  VisibleItems: Integer;
  i: Integer;
  NewWidth, NewHeight: Integer;
begin
  if AIsExpanded then
  begin
    VisibleItems := 0;
    for i := 0 to Items.Count - 1 do
      if Items[i].Visible then
        Inc(VisibleItems);

    if VisibleItems = 0 then
    begin
      Collapse(True);
      Exit;
    end;

    if FOrientation = bgoHorizontal then
    begin
      NewWidth := (FItemWidth * VisibleItems) + (FSpacing * (VisibleItems - 1));
      NewHeight := FItemHeight;
    end
    else // bgoVertical
    begin
      NewWidth := FItemWidth;
      NewHeight := (FItemHeight * VisibleItems) + (FSpacing * (VisibleItems - 1));
    end;

    Self.SetBounds(Self.Left, Self.Top, NewWidth, NewHeight);
  end
  else
  begin
    { Retorna ao tamanho original }
    Self.SetBounds(Self.Left, Self.Top, FOriginalWidth, FOriginalHeight);
  end;
end;

function THTL_CExpandingButton.GetItemAt(X, Y: Integer): THTL_CExpandingButtonItem;
var
  i: Integer;
  ItemRect: TRect;
begin
  Result := nil;
  if not FExpanded then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    if FItems[i].Visible then
    begin
      ItemRect := GetItemRect(i);
      if PtInRect(ItemRect, Point(X, Y)) then
      begin
        Result := FItems[i];
        Exit;
      end;
    end;
  end;
end;

function THTL_CExpandingButton.GetItemRect(AIndex: Integer): TRect;
var
  CurrentX, CurrentY: Integer;
  VisibleIndex: Integer;
  i: Integer;
begin
  Result := Rect(0,0,0,0);
  if not FExpanded then Exit;

  CurrentX := 0;
  CurrentY := 0;
  VisibleIndex := -1;

  for i := 0 to AIndex do
  begin
      if Items[i].Visible then
        Inc(VisibleIndex);
  end;

  if VisibleIndex < 0 then Exit;

  if FOrientation = bgoHorizontal then
  begin
    CurrentX := (FItemWidth + FSpacing) * VisibleIndex;
    Result := Rect(CurrentX, CurrentY, CurrentX + FItemWidth, CurrentY + FItemHeight);
  end
  else // bgoVertical
  begin
    CurrentY := (FItemHeight + FSpacing) * VisibleIndex;
    Result := Rect(CurrentX, CurrentY, CurrentX + FItemWidth, CurrentY + FItemHeight);
  end;
end;

procedure THTL_CExpandingButton.ItemsChanged(Sender: TObject);
begin
  if FExpanded then
  begin
    UpdateBounds(True);
    Invalidate;
  end;
end;

{ Setters }
procedure THTL_CExpandingButton.SetItems(const Value: THTL_CExpandingButtonItems);
begin
  FItems.Assign(Value);
  ItemsChanged(Self);
end;

procedure THTL_CExpandingButton.SetCollapseDelay(const Value: Cardinal);
begin
  FCollapseDelay := Value;
  if FCollapseTimer.Enabled then
  begin
    FCollapseTimer.Interval := FCollapseDelay;
  end;
end;

procedure THTL_CExpandingButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    ItemsChanged(Self);
  end;
end;

procedure THTL_CExpandingButton.SetOrientation(const Value: TButtonGroupOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    ItemsChanged(Self);
  end;
end;

procedure THTL_CExpandingButton.SetItemSettings(const Value: THTL_CGroupButtonGlobalSettings);
begin
  FItemSettings.Assign(Value);
  Invalidate;
end;

procedure THTL_CExpandingButton.SetItemWidth(const Value: Integer);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    ItemsChanged(Self);
  end;
end;

procedure THTL_CExpandingButton.SetItemHeight(const Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    ItemsChanged(Self);
  end;
end;

end.

