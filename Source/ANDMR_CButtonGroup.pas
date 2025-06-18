unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Types,
  Vcl.Controls, Vcl.Graphics, ANDMR_CButton, ANDMR_ComponentUtils,
  Winapi.Windows, Winapi.Messages;

type
  TANDMR_CButtonGroup = class; // Forward declaration
  TButtonGroupOrientation = (bgoHorizontal, bgoVertical);

  { TANDMR_CGroupButtonItem }
  // Represents a single button item within the group. It's a collection item,
  // not a full TWinControl, making the group more lightweight.
  TANDMR_CGroupButtonItem = class(TCollectionItem)
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

    // Setters
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

  { TANDMR_CGroupButtonItems }
  // Collection to hold the button items.
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

  { TANDMR_CButtonGroup }
  // The main Button Group component.
  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FItems: TANDMR_CGroupButtonItems;
    FBorder: TBorderSettings;         // Global border for the entire group
    FCaption: TCaptionSettings;       // Global caption for the entire group
    FImage: TImageSettings;           // Global image settings (template)
    FHover: THoverSettings;           // Global hover settings (template)
    FGlobalButtonBorder: TBorderSettings; // Global border settings for individual buttons (template)

    FOrientation: TButtonGroupOrientation;
    FSpacing: Integer;
    FAutoSize: Boolean;

    FHoveredItem: TANDMR_CGroupButtonItem;
    FClickedItem: TANDMR_CGroupButtonItem;

    // Setters
    procedure SetItems(const Value: TANDMR_CGroupButtonItems);
    procedure SetBorder(const Value: TBorderSettings);
    procedure SetCaption(const Value: TCaptionSettings);
    procedure SetImage(const Value: TImageSettings);
    procedure SetHover(const Value: THoverSettings);
    procedure SetGlobalButtonBorder(const Value: TBorderSettings);
    procedure SetOrientation(const Value: TButtonGroupOrientation);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);

    procedure SettingsChanged(Sender: TObject);
    procedure UpdateLayout;
    function GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
    function GetItemRect(Item: TANDMR_CGroupButtonItem): TRect;

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // Core Properties
    property Items: TANDMR_CGroupButtonItems read FItems write SetItems;
    property Orientation: TButtonGroupOrientation read FOrientation write SetOrientation default bgoHorizontal;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;

    // Global Appearance Properties (act as templates for new items and for the group itself)
    property Border: TBorderSettings read FBorder write SetBorder;
    property Caption: TCaptionSettings read FCaption write SetCaption;
    property Image: TImageSettings read FImage write SetImage;
    property Hover: THoverSettings read FHover write SetHover;
    property GlobalButtonBorder: TBorderSettings read FGlobalButtonBorder write SetGlobalButtonBorder;


    // Standard VCL Properties
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

    // Events
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
  System.Math, Winapi.GDIPOBJ, Winapi.GDIPAPI;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CButtonGroup]);
end;

//------------------------------------------------------------------------------
// TANDMR_CGroupButtonItem
//------------------------------------------------------------------------------

constructor TANDMR_CGroupButtonItem.Create(Collection: TCollection);
var
  LGroup: TANDMR_CButtonGroup;
begin
  inherited Create(Collection);
  FVisible := True;
  FEnabled := True;
  FWidth := 75;
  FHeight := 30;

  FImage := TPicture.Create;

  // Create individual settings objects
  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;

  FHover := THoverSettings.Create(nil); // No owner control needed for animation timer
  FHover.OnChange := SettingsChanged;

  FCaptionSettings := TCaptionSettings.Create(nil);
  FCaptionSettings.OnChange := SettingsChanged;

  FImageSettings := TImageSettings.Create(nil);
  FImageSettings.OnChange := SettingsChanged;

  // Apply global templates from the owner group
  if (Collection <> nil) and (Collection.Owner is TANDMR_CButtonGroup) then
  begin
    LGroup := TANDMR_CButtonGroup(Collection.Owner);
    FCaption := 'Button' + IntToStr(Collection.Count);
    FBorder.Assign(LGroup.GlobalButtonBorder);
    FHover.Assign(LGroup.Hover);
    FCaptionSettings.Assign(LGroup.Caption);
    FImageSettings.Assign(LGroup.Image);
  end;
end;

destructor TANDMR_CGroupButtonItem.Destroy;
begin
  FImage.Free;
  FBorder.Free;
  FHover.Free;
  FCaptionSettings.Free;
  FImageSettings.Free;
  inherited Destroy;
end;

procedure TANDMR_CGroupButtonItem.Assign(Source: TPersistent);
begin
  if Source is TANDMR_CGroupButtonItem then
  begin
    inherited Assign(Source);
    with TANDMR_CGroupButtonItem(Source) do
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

function TANDMR_CGroupButtonItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TANDMR_CGroupButtonItem.SettingsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TANDMR_CGroupButtonItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  Changed(False);
end;

procedure TANDMR_CGroupButtonItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
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

procedure TANDMR_CGroupButtonItem.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetBorder(const Value: TBorderSettings);
begin
  FBorder.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetHover(const Value: THoverSettings);
begin
  FHover.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
end;

procedure TANDMR_CGroupButtonItem.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
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
  if Assigned(FOwner) and not (csLoading in FOwner.ComponentState) then
    FOwner.UpdateLayout;
end;

//------------------------------------------------------------------------------
// TANDMR_CButtonGroup
//------------------------------------------------------------------------------

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csReplicatable, csDoubleClicks, csCaptureMouse, csAcceptsControls, csSetCaption];
  DoubleBuffered := True;
  Width := 200;
  Height := 50;
  TabStop := True;

  FItems := TANDMR_CGroupButtonItems.Create(Self);

  FBorder := TBorderSettings.Create;
  FBorder.OnChange := SettingsChanged;
  FBorder.Visible := True;
  FBorder.CornerRadius := 6;
  FBorder.Color := clGray;

  FCaption := TCaptionSettings.Create(Self);
  FCaption.OnChange := SettingsChanged;
  FCaption.Text := 'Button Group';

  FImage := TImageSettings.Create(Self);
  FImage.OnChange := SettingsChanged;

  FHover := THoverSettings.Create(Self);
  FHover.OnChange := SettingsChanged;

  FGlobalButtonBorder := TBorderSettings.Create;
  FGlobalButtonBorder.OnChange := SettingsChanged;
  FGlobalButtonBorder.BackgroundColor := clBtnFace;
  FGlobalButtonBorder.Color := clDkGray;
  FGlobalButtonBorder.CornerRadius := 4;

  FOrientation := bgoHorizontal;
  FSpacing := 4;
  FAutoSize := True;
  FHoveredItem := nil;
  FClickedItem := nil;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FItems.Free;
  FBorder.Free;
  FCaption.Free;
  FImage.Free;
  FHover.Free;
  FGlobalButtonBorder.Free;
  inherited Destroy;
end;

procedure TANDMR_CButtonGroup.Loaded;
begin
  inherited;
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TANDMR_CButtonGroup.SettingsChanged(Sender: TObject);
begin
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.UpdateLayout;
var
  i: Integer;
  RequiredWidth, RequiredHeight, MaxItemWidth, MaxItemHeight: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if FAutoSize then
    begin
      RequiredWidth := 0;
      RequiredHeight := 0;
      MaxItemWidth := 0;
      MaxItemHeight := 0;

      for i := 0 to FItems.Count - 1 do
      begin
        if FItems[i].Visible then
        begin
          if FOrientation = bgoHorizontal then
          begin
            if RequiredWidth > 0 then
              RequiredWidth := RequiredWidth + FSpacing;
            RequiredWidth := RequiredWidth + FItems[i].Width;
            if FItems[i].Height > MaxItemHeight then
              MaxItemHeight := FItems[i].Height;
          end
          else // bgoVertical
          begin
            if RequiredHeight > 0 then
              RequiredHeight := RequiredHeight + FSpacing;
            RequiredHeight := RequiredHeight + FItems[i].Height;
            if FItems[i].Width > MaxItemWidth then
              MaxItemWidth := FItems[i].Width;
          end;
        end;
      end;

      if FOrientation = bgoHorizontal then
        RequiredHeight := MaxItemHeight
      else
        RequiredWidth := MaxItemWidth;

      // Add border and caption padding
      if FBorder.Visible then
      begin
        RequiredWidth := RequiredWidth + FBorder.Thickness * 2 + 4;
        RequiredHeight := RequiredHeight + FBorder.Thickness * 2 + 4;
      end;
      if FCaption.Visible and (FCaption.Text <> '') and (FCaption.Position in [cpAbove, cpBelow]) then
      begin
         // A simple estimate for caption height
         RequiredHeight := RequiredHeight + Self.Canvas.TextHeight(FCaption.Text) + 4;
      end;

      Self.SetBounds(Self.Left, Self.Top, RequiredWidth, RequiredHeight);
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.Paint;
var
  LG: TGPGraphics;
  i: Integer;
  Item: TANDMR_CGroupButtonItem;
  ItemRect: TRect;
  ItemColor, ItemBorderColor, ItemFontColor: TColor;
begin
  inherited;
  LG := TGPGraphics.Create(Self.Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);

    // 1. Draw Group Border and Background
    if FBorder.Visible then
    begin
      DrawEditBox(LG, ClientRect, FBorder.BackgroundColor, FBorder.Color, FBorder.Thickness,
        FBorder.Style, FBorder.CornerRadius, FBorder.RoundCornerType, 255);
    end;

    // 2. Draw Group Caption
    if FCaption.Visible and (FCaption.Text <> '') then
    begin
       // Basic caption drawing logic
       // A more sophisticated layout would be needed for perfect positioning
       var CaptionRect := ClientRect;
       DrawComponentCaption(Self.Canvas, CaptionRect, FCaption.Text, FCaption.Font, FCaption.Color, taCenter, cvaTop, False, 255);
    end;

    // 3. Draw each button item
    for i := 0 to FItems.Count - 1 do
    begin
      Item := FItems[i];
      if not Item.Visible then Continue;

      ItemRect := GetItemRect(Item);
      if IsRectEmpty(ItemRect) then continue;

      // Determine item colors based on state (normal, hover, clicked)
      ItemColor := Item.Border.BackgroundColor;
      ItemBorderColor := Item.Border.Color;
      ItemFontColor := Item.CaptionSettings.Font.Color;

      if Item = FHoveredItem then
      begin
         if Item.Hover.BackgroundColor <> clNone then
            ItemColor := Item.Hover.BackgroundColor;
         if Item.Hover.BorderColor <> clNone then
            ItemBorderColor := Item.Hover.BorderColor;
         if Item.Hover.FontColor <> clNone then
            ItemFontColor := Item.Hover.FontColor;
      end;
      // Note: A "clicked" state could also be added here

      // Draw the button's background and border
      DrawEditBox(LG, ItemRect, ItemColor, ItemBorderColor, Item.Border.Thickness,
        Item.Border.Style, Item.Border.CornerRadius, Item.Border.RoundCornerType, 255);

      // Draw the button's caption and image
      var ContentRect := ItemRect;
      InflateRect(ContentRect, -Item.Border.Thickness, -Item.Border.Thickness);

      if (Item.Caption <> '') and Assigned(Item.CaptionSettings) then
      begin
        DrawComponentCaption(Self.Canvas, ContentRect, Item.Caption, Item.CaptionSettings.Font,
          ItemFontColor, taCenter, cvaCenter, False, 255);
      end;
      // Image drawing logic would go here
    end;

  finally
    LG.Free;
  end;
end;

function TANDMR_CButtonGroup.GetItemRect(Item: TANDMR_CGroupButtonItem): TRect;
var
  i: Integer;
  CurrentX, CurrentY: Integer;
begin
  Result := Rect(0,0,0,0);
  CurrentX := FBorder.Thickness + 2; // Start inside the border
  CurrentY := FBorder.Thickness + 2; // Start inside the border

  if FCaption.Visible and (FCaption.Text <> '') and (FCaption.Position in [cpAbove, cpBelow]) then
      CurrentY := CurrentY + Self.Canvas.TextHeight(FCaption.Text) + 4;

  for i := 0 to FItems.Count - 1 do
  begin
    if not FItems[i].Visible then continue;

    if FItems[i] = Item then
    begin
      Result := Rect(CurrentX, CurrentY, CurrentX + Item.Width, CurrentY + Item.Height);
      Exit;
    end;

    if FOrientation = bgoHorizontal then
      CurrentX := CurrentX + FItems[i].Width + FSpacing
    else
      CurrentY := CurrentY + FItems[i].Height + FSpacing;
  end;
end;

function TANDMR_CButtonGroup.GetItemAt(X, Y: Integer): TANDMR_CGroupButtonItem;
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


procedure TANDMR_CButtonGroup.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FClickedItem := GetItemAt(X, Y);
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: TANDMR_CGroupButtonItem;
begin
  inherited;
  Item := GetItemAt(X, Y);
  if Item <> FHoveredItem then
  begin
    FHoveredItem := Item;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TANDMR_CGroupButtonItem;
begin
  inherited;
  if Button = mbLeft then
  begin
    Item := GetItemAt(X, Y);
    if (Item <> nil) and (Item = FClickedItem) then
    begin
      // This is where you would trigger an item's OnClick event if it had one.
      // For now, we just call the group's OnClick.
      Click;
    end;
    FClickedItem := nil;
    Invalidate;
  end;
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


// --- Setters ---

procedure TANDMR_CButtonGroup.SetItems(const Value: TANDMR_CGroupButtonItems);
begin
  FItems.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetBorder(const Value: TBorderSettings);
begin
  FBorder.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetCaption(const Value: TCaptionSettings);
begin
  FCaption.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetImage(const Value: TImageSettings);
begin
  FImage.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetHover(const Value: THoverSettings);
begin
  FHover.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetGlobalButtonBorder(const Value: TBorderSettings);
begin
  FGlobalButtonBorder.Assign(Value);
  UpdateLayout;
end;

procedure TANDMR_CButtonGroup.SetOrientation(const Value: TButtonGroupOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    UpdateLayout;
  end;
end;

procedure TANDMR_CButtonGroup.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    UpdateLayout;
  end;
end;

procedure TANDMR_CButtonGroup.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    UpdateLayout;
  end;
end;

end.
