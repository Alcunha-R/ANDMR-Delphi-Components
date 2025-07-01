unit HTL_CTabBar;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Types,
  Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  Winapi.Windows, Winapi.Messages, System.UITypes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI,
  HTL_ComponentUtils;

type
  TTabBarStyle = (
    tsContained, tsPill, tsUnderlined, tsUnderlinedInverted, tsGradientUnderlined,
    tsDottedIndicator, tsSegmented, tsBordered, tsDualLine, tsSideLines
  );

  TTabBarOrientation = (tboHorizontal, tboVertical);

  THTL_CTabBar = class;

  THTL_CTabItem = class(TCollectionItem)
  private
    FCaption: string;
    FVisible: Boolean;
    FEnabled: Boolean;
    FTag: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FImageSettings: TImageSettings; // Adicionado
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetImageSettings(const Value: TImageSettings); // Adicionado
    function GetImage: TPicture; // Adicionado
    procedure SetImage(const Value: TPicture); // Adicionado
    procedure ImageSettingsChanged(Sender: TObject); // Adicionado
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override; // Adicionado
    procedure Assign(Source: TPersistent); override; // Adicionado
  published
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write SetWidth default 80;
    property Height: Integer read FHeight write SetHeight default 30;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Tag: Integer read FTag write FTag default 0;
    property Image: TPicture read GetImage write SetImage; // Adicionado
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings; // Adicionado
  end;

  THTL_CTabItems = class(TCollection)
  private
    FOwner: THTL_CTabBar;
    function GetItem(Index: Integer): THTL_CTabItem;
    procedure SetItem(Index: Integer; const Value: THTL_CTabItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: THTL_CTabBar);
    function Add: THTL_CTabItem;
    property Items[Index: Integer]: THTL_CTabItem read GetItem write SetItem; default;
  end;

  THTL_CTabBar = class(TCustomControl)
  private
    FItems: THTL_CTabItems;
    FContainer: TBorderSettings;
    FStyle: TTabBarStyle;
    FSpacing: Integer;
    FAutoSizeTabs: Boolean;
    FActiveIndex: Integer;
    FActiveIndicator: TBorderSettings;
    FGradient: TGradientSettings;
    FActiveFont: TFont;            // ALTERADO: de FActiveFontColor para FActiveFont
    FInactiveFontColor: TColor;
    FHoverSettings: THoverSettings;
    FOnChange: TNotifyEvent;

    FAnimationTimer: TTimer;
    FActiveIndicatorAnimProgress: Single;
    FHoveredTabIndex: Integer;
    FLastHoveredTabIndex: Integer;
    FFromRect: TRect;
    FToRect: TRect;
    FOrientation: TTabBarOrientation;

    procedure SetItems(const Value: THTL_CTabItems);
    procedure SetContainer(const Value: TBorderSettings);
    procedure SetStyle(const Value: TTabBarStyle);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoSizeTabs(const Value: Boolean);
    procedure SetActiveIndex(const Value: Integer);
    procedure SetActiveIndicator(const Value: TBorderSettings);
    procedure SetGradient(const Value: TGradientSettings);
    procedure SetActiveFont(const Value: TFont); // ALTERADO: de SetActiveFontColor para SetActiveFont
    procedure SetInactiveFontColor(const Value: TColor);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetOrientation(const Value: TTabBarOrientation);
    procedure Animate(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); // ADICIONADO
    procedure UpdateLayout;
    procedure ApplyStyleDefaults;
    function GetItemAt(X, Y: Integer): Integer;
    function GetItemRect(Index: Integer): TRect;

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items: THTL_CTabItems read FItems write SetItems;
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex default -1;
    property Style: TTabBarStyle read FStyle write SetStyle default tsContained;
    property Orientation: TTabBarOrientation read FOrientation write SetOrientation default tboHorizontal;
    property Spacing: Integer read FSpacing write SetSpacing default 0;
    property AutoSizeTabs: Boolean read FAutoSizeTabs write SetAutoSizeTabs default True;
    property Container: TBorderSettings read FContainer write SetContainer;
    property ActiveIndicator: TBorderSettings read FActiveIndicator write SetActiveIndicator;
    property Gradient: TGradientSettings read FGradient write SetGradient;
    property ActiveFont: TFont read FActiveFont write SetActiveFont; // ALTERADO
    property InactiveFontColor: TColor read FInactiveFontColor write SetInactiveFontColor default TColor($00666666);
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

uses System.Math, Vcl.Imaging.pngimage;

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CTabBar]);
end;

{ THTL_CTabItem }
constructor THTL_CTabItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FEnabled := True;
  FWidth := 80;
  FCaption := 'Tab ' + IntToStr(Collection.Count);
  // Garante que o Owner do ImageSettings é o THTL_CTabBar, um TWinControl.
  FImageSettings := TImageSettings.Create(THTL_CTabItems(Collection).FOwner);
  FImageSettings.OnChange := ImageSettingsChanged;
end;

destructor THTL_CTabItem.Destroy;
begin
  FImageSettings.Free;
  inherited;
end;

procedure THTL_CTabItem.Assign(Source: TPersistent);
begin
  if Source is THTL_CTabItem then
  begin
    FCaption := THTL_CTabItem(Source).Caption;
    FWidth := THTL_CTabItem(Source).Width;
    FVisible := THTL_CTabItem(Source).Visible;
    FEnabled := THTL_CTabItem(Source).Enabled;
    FTag := THTL_CTabItem(Source).Tag;
    ImageSettings.Assign(THTL_CTabItem(Source).ImageSettings);
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function THTL_CTabItem.GetDisplayName: string; begin if FCaption <> '' then Result := FCaption else Result := inherited GetDisplayName; end;
procedure THTL_CTabItem.SetCaption(const Value: string); begin if FCaption <> Value then begin FCaption := Value; Changed(False); end; end;
procedure THTL_CTabItem.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed(False); end; end;
procedure THTL_CTabItem.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed(False); end; end;
procedure THTL_CTabItem.SetWidth(const Value: Integer); begin if FWidth <> Value then begin FWidth := Value; Changed(False); end; end;
procedure THTL_CTabItem.SetHeight(const Value: Integer); begin if FHeight <> Value then begin FHeight := Value; Changed(False); end; end;
procedure THTL_CTabItem.ImageSettingsChanged(Sender: TObject); begin Changed(False); end;
procedure THTL_CTabItem.SetImageSettings(const Value: TImageSettings); begin FImageSettings.Assign(Value); end;
function THTL_CTabItem.GetImage: TPicture; begin Result := FImageSettings.Picture; end;
procedure THTL_CTabItem.SetImage(const Value: TPicture); begin FImageSettings.Picture.Assign(Value); end;

{ THTL_CTabItems }
constructor THTL_CTabItems.Create(AOwner: THTL_CTabBar); begin inherited Create(THTL_CTabItem); FOwner := AOwner; end;
function THTL_CTabItems.Add: THTL_CTabItem; begin Result := THTL_CTabItem(inherited Add); end;
function THTL_CTabItems.GetItem(Index: Integer): THTL_CTabItem; begin Result := THTL_CTabItem(inherited GetItem(Index)); end;
function THTL_CTabItems.GetOwner: TPersistent; begin Result := FOwner; end;
procedure THTL_CTabItems.SetItem(Index: Integer; const Value: THTL_CTabItem); begin inherited SetItem(Index, Value); end;
procedure THTL_CTabItems.Update(Item: TCollectionItem); begin inherited; if Assigned(FOwner) and not (csLoading in FOwner.ComponentState) then FOwner.UpdateLayout; end;

{ THTL_CTabBar }
constructor THTL_CTabBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csReplicatable, csDoubleClicks, csCaptureMouse, csSetCaption, csAcceptsControls, csNeedsBorderPaint];
  DoubleBuffered := True;
  Width := 250;
  Height := 40;
  TabStop := True;
  Font.Name := 'Segoe UI';
  Font.Size := 10;
  Font.OnChange := FontChanged; // ADICIONADO

  // ADICIONADO: Configuração da nova fonte ativa
  FActiveFont := TFont.Create;
  FActiveFont.Assign(Self.Font); // Herda as propriedades da fonte principal
  FActiveFont.Style := [fsBold];   // Define uma sobreposição padrão (negrito)
  FActiveFont.Color := clBlack;    // Define uma cor padrão
  FActiveFont.OnChange := FontChanged;

  FItems := THTL_CTabItems.Create(Self);

  FContainer := TBorderSettings.Create;
  FContainer.OnChange := SettingsChanged;
  FContainer.BackgroundColor := TColor($00F2F2F2);
  FContainer.Color := TColor($00CCCCCC);
  FContainer.Thickness := 1;
  FContainer.CornerRadius := 8;

  FActiveIndicator := TBorderSettings.Create;
  FActiveIndicator.OnChange := SettingsChanged;
  FActiveIndicator.BackgroundColor := clWhite;
  FActiveIndicator.Color := clHighlight;
  FActiveIndicator.Thickness := 2;
  FActiveIndicator.CornerRadius := 6;

  FGradient := TGradientSettings.Create;
  FGradient.OnChange := SettingsChanged;
  FGradient.Enabled := False;
  FGradient.StartColor := TColor($00FF8C00);
  FGradient.EndColor := TColor($004169E1);

  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnAnimationProgress := SettingsChanged;
  FHoverSettings.BackgroundColor := clSkyBlue;
  FHoverSettings.BorderColor := clHighlight;
  FHoverSettings.FontColor := clHighlight;
  FHoverSettings.HoverEffect := heFade;

  FStyle := tsContained;
  FSpacing := 0;
  FAutoSizeTabs := True;
  FActiveIndex := -1;
  FOrientation := tboHorizontal;
  FInactiveFontColor := TColor($00666666);
  FHoveredTabIndex := -1;
  FLastHoveredTabIndex := -1;
  FActiveIndicatorAnimProgress := 1.0;

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 15;
  FAnimationTimer.OnTimer := Animate;

  ApplyStyleDefaults;
end;

destructor THTL_CTabBar.Destroy;
begin
  FItems.Free;
  FContainer.Free;
  FActiveIndicator.Free;
  FGradient.Free;
  FHoverSettings.Free;
  FAnimationTimer.Free;
  FActiveFont.Free; // ADICIONADO
  inherited Destroy;
end;

procedure THTL_CTabBar.Loaded;
begin
  inherited;
  if (FActiveIndex >= 0) and (FActiveIndex < FItems.Count) then
  begin
    FToRect := GetItemRect(FActiveIndex);
    FFromRect := FToRect;
    FActiveIndicatorAnimProgress := 1.0;
  end;
  UpdateLayout;
end;

procedure THTL_CTabBar.Resize; begin inherited; UpdateLayout; end;
procedure THTL_CTabBar.SettingsChanged(Sender: TObject); begin Invalidate; end;
// ADICIONADO: Novo handler para mudança de fonte
procedure THTL_CTabBar.FontChanged(Sender: TObject); begin Invalidate; end;
procedure THTL_CTabBar.UpdateLayout; begin Invalidate; Repaint; end;

procedure THTL_CTabBar.Animate(Sender: TObject);
begin
  if FActiveIndicatorAnimProgress < 1.0 then
  begin
    FActiveIndicatorAnimProgress := Min(1.0, FActiveIndicatorAnimProgress + 0.1);
    Invalidate;
  end
  else
  begin
    FAnimationTimer.Enabled := False;
  end;
end;

function THTL_CTabBar.GetItemAt(X, Y: Integer): Integer;
var i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    if FItems[i].Visible and PtInRect(GetItemRect(i), Point(X, Y)) then Exit(i);
  Result := -1;
end;

procedure THTL_CTabBar.CMMouseEnter(var Message: TMessage);
var
  TrackMouseEventStruct: TTrackMouseEvent;
begin
  inherited;
  TrackMouseEventStruct.cbSize := SizeOf(TTrackMouseEvent);
  TrackMouseEventStruct.dwFlags := TME_LEAVE;
  TrackMouseEventStruct.hwndTrack := Self.Handle;
  TrackMouseEventStruct.dwHoverTime := HOVER_DEFAULT;
  Winapi.Windows.TrackMouseEvent(TrackMouseEventStruct);
end;

procedure THTL_CTabBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Self.Cursor := crDefault;
  if FHoveredTabIndex <> -1 then
  begin
    FLastHoveredTabIndex := FHoveredTabIndex;
    FHoveredTabIndex := -1;
    FHoverSettings.StartAnimation(False);
  end;
end;

procedure THTL_CTabBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHoverIndex: Integer;
begin
  inherited;
  if not Enabled then
  begin
    Self.Cursor := crDefault;
    Exit;
  end;

  NewHoverIndex := GetItemAt(X, Y);

  if (NewHoverIndex <> -1) and FItems[NewHoverIndex].Enabled then
    Self.Cursor := crHandPoint
  else
    Self.Cursor := crDefault;

  if NewHoverIndex <> FHoveredTabIndex then
  begin
    if FHoveredTabIndex <> -1 then
    begin
      FLastHoveredTabIndex := FHoveredTabIndex;
    end;
    FHoveredTabIndex := NewHoverIndex;

    if (FHoveredTabIndex <> -1) and (FLastHoveredTabIndex <> -1) then
    begin
      FHoverSettings.ResetAnimation;
    end;

    if FHoveredTabIndex <> -1 then
    begin
      FHoverSettings.StartAnimation(True);
    end
    else
    begin
      FHoverSettings.StartAnimation(False);
    end;
  end;
end;

procedure THTL_CTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ItemIndex: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    ItemIndex := GetItemAt(X, Y);
    if (ItemIndex <> -1) and FItems[ItemIndex].Enabled then
      Self.ActiveIndex := ItemIndex;
  end;
end;

procedure THTL_CTabBar.SetActiveIndex(const Value: Integer);
var PrevIndex: Integer;
begin
  PrevIndex := FActiveIndex;
  if (Value >= -1) and (Value < FItems.Count) and (FActiveIndex <> Value) then
  begin
    FActiveIndex := Value;
    if (PrevIndex > -1) and (PrevIndex < FItems.Count) then
      FFromRect := GetItemRect(PrevIndex)
    else if FItems.Count > 0 then
      FFromRect := GetItemRect(IfThen(Value > -1, Value, 0));
    if (FActiveIndex > -1) then
      FToRect := GetItemRect(FActiveIndex)
    else
      FToRect := FFromRect;
    FActiveIndicatorAnimProgress := 0.0;
    FAnimationTimer.Enabled := True;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
  end;
end;

procedure THTL_CTabBar.SetItems(const Value: THTL_CTabItems); begin FItems.Assign(Value); UpdateLayout; Invalidate; end;
procedure THTL_CTabBar.SetContainer(const Value: TBorderSettings); begin FContainer.Assign(Value); Invalidate; end;
procedure THTL_CTabBar.SetSpacing(const Value: Integer); begin if FSpacing <> Value then begin FSpacing := Value; UpdateLayout; Invalidate; end; end;
procedure THTL_CTabBar.SetActiveIndicator(const Value: TBorderSettings); begin FActiveIndicator.Assign(Value); Invalidate; end;
procedure THTL_CTabBar.SetInactiveFontColor(const Value: TColor); begin if FInactiveFontColor <> Value then begin FInactiveFontColor := Value; Invalidate; end; end;
procedure THTL_CTabBar.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); Invalidate; end;
procedure THTL_CTabBar.SetGradient(const Value: TGradientSettings); begin FGradient.Assign(Value); Invalidate; end;
procedure THTL_CTabBar.SetAutoSizeTabs(const Value: Boolean); begin if FAutoSizeTabs <> Value then begin FAutoSizeTabs := Value; UpdateLayout; Invalidate; end; end;
// ALTERADO: de SetActiveFontColor para SetActiveFont
procedure THTL_CTabBar.SetActiveFont(const Value: TFont); begin FActiveFont.Assign(Value); Invalidate; end;

procedure THTL_CTabBar.SetOrientation(const Value: TTabBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    UpdateLayout;
  end;
end;

procedure THTL_CTabBar.ApplyStyleDefaults;
begin
  case FStyle of
    tsContained, tsPill, tsSegmented:
      FHoverSettings.Targets := [htBackground, htFont];
    tsUnderlined, tsUnderlinedInverted, tsGradientUnderlined, tsDottedIndicator,
    tsDualLine, tsSideLines, tsBordered:
      FHoverSettings.Targets := [htFont, htIndicator];
  else
    FHoverSettings.Targets := [htBackground, htFont, htBorder];
  end;
end;

procedure THTL_CTabBar.SetStyle(const Value: TTabBarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    ApplyStyleDefaults;
    FActiveIndicatorAnimProgress := 1.0;
    if (FActiveIndex >= 0) and (FActiveIndex < FItems.Count) then
    begin
      FToRect := GetItemRect(FActiveIndex);
      FFromRect := FToRect;
    end;
    if FStyle = tsGradientUnderlined then
      FGradient.Enabled := True
    else
      FGradient.Enabled := False;
    Invalidate;
  end;
end;

function THTL_CTabBar.GetItemRect(Index: Integer): TRect;
var
  i, VisibleItemIndex, VisibleCount: Integer;
  ContentWidth, ContentHeight, TotalSpacing, TotalManualSize: Integer;
  VisibleItemsSizes: TList<Integer>;
  CurrentPos: Integer;
begin
  Result := Rect(0,0,0,0);
  if (Index < 0) or (Index >= FItems.Count) then Exit;

  VisibleItemsSizes := TList<Integer>.Create;
  try
    VisibleCount := 0;
    for i := 0 to FItems.Count - 1 do
    begin
      if FItems[i].Visible then
      begin
        Inc(VisibleCount);
        if FOrientation = tboHorizontal then
          VisibleItemsSizes.Add(FItems[i].Width)
        else
          VisibleItemsSizes.Add(FItems[i].Height);
      end;
    end;

    if VisibleCount = 0 then Exit;

    ContentWidth := Self.ClientWidth - (FContainer.Thickness * 2);
    ContentHeight := Self.ClientHeight - (FContainer.Thickness * 2);
    TotalSpacing := Max(0, VisibleCount - 1) * FSpacing;
    CurrentPos := FContainer.Thickness;
    VisibleItemIndex := -1;
    for i := 0 to Index do
      if FItems[i].Visible then
        Inc(VisibleItemIndex);

    if FOrientation = tboHorizontal then
    begin
      if FAutoSizeTabs then
      begin
        var NetTabWidth := ContentWidth - TotalSpacing;
        Result.Left := FContainer.Thickness + Round(VisibleItemIndex * (NetTabWidth / VisibleCount) + (VisibleItemIndex * FSpacing));
        Result.Right := FContainer.Thickness + Round((VisibleItemIndex + 1) * (NetTabWidth / VisibleCount) + (VisibleItemIndex * FSpacing));
        if (VisibleItemIndex = VisibleCount - 1) then
          Result.Right := FContainer.Thickness + ContentWidth;
      end
      else
      begin
        TotalManualSize := 0;
        for i := 0 to VisibleItemsSizes.Count - 1 do
          TotalManualSize := TotalManualSize + VisibleItemsSizes[i];
        TotalManualSize := TotalManualSize + TotalSpacing;
        CurrentPos := FContainer.Thickness + Max(0, (ContentWidth - TotalManualSize) div 2);
        var TempVisibleIdx: Integer := -1;
        for i := 0 to FItems.Count - 1 do
        begin
          if FItems[i].Visible then
          begin
            Inc(TempVisibleIdx);
            if TempVisibleIdx = VisibleItemIndex then
            begin
              Result.Left := CurrentPos;
              Result.Right := CurrentPos + FItems[i].Width;
              Break;
            end;
            CurrentPos := CurrentPos + FItems[i].Width + FSpacing;
          end;
        end;
      end;
      Result.Top := FContainer.Thickness;
      Result.Bottom := FContainer.Thickness + ContentHeight;
    end
    else // tboVertical
    begin
      if FAutoSizeTabs then
      begin
        var NetTabHeight := ContentHeight - TotalSpacing;
        Result.Top := FContainer.Thickness + Round(VisibleItemIndex * (NetTabHeight / VisibleCount) + (VisibleItemIndex * FSpacing));
        Result.Bottom := FContainer.Thickness + Round((VisibleItemIndex + 1) * (NetTabHeight / VisibleCount) + (VisibleItemIndex * FSpacing));
        if (VisibleItemIndex = VisibleCount - 1) then
          Result.Bottom := FContainer.Thickness + ContentHeight;
      end
      else
      begin
        TotalManualSize := 0;
        for i := 0 to VisibleItemsSizes.Count - 1 do
          TotalManualSize := TotalManualSize + VisibleItemsSizes[i];
        TotalManualSize := TotalManualSize + TotalSpacing;
        CurrentPos := FContainer.Thickness + Max(0, (ContentHeight - TotalManualSize) div 2);
        var TempVisibleIdx: Integer := -1;
        for i := 0 to FItems.Count - 1 do
        begin
          if FItems[i].Visible then
          begin
            Inc(TempVisibleIdx);
            if TempVisibleIdx = VisibleItemIndex then
            begin
              Result.Top := CurrentPos;
              Result.Bottom := CurrentPos + FItems[i].Height;
              Break;
            end;
            CurrentPos := CurrentPos + FItems[i].Height + FSpacing;
          end;
        end;
      end;
      Result.Left := FContainer.Thickness;
      Result.Right := FContainer.Thickness + ContentWidth;
    end;
  finally
    VisibleItemsSizes.Free;
  end;
end;

procedure THTL_CTabBar.Paint;
var
  LG: TGPGraphics;
  i, LastVisibleIndex: Integer;
  Item: THTL_CTabItem;
  ItemRect, IndicatorRect: TRect;
  FinalIndicatorColor, BaseBgColor: TColor;
  IndicatorPen: TGPPen;
  IndicatorBrush: TGPBrush;
  SeparatorPen: TGPPen;
  IsItemActive, IsItemDisabled: Boolean;
  LHoverProgress: Single;
  IsCrossFade: Boolean;
begin
  inherited;
  LG := TGPGraphics.Create(Self.Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeHighQuality);
    LG.SetInterpolationMode(InterpolationModeHighQualityBicubic);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    if FContainer.Visible then
      DrawEditBox(LG, ClientRect, FContainer.BackgroundColor, FContainer.Color, FContainer.Thickness, FContainer.Style, FContainer.CornerRadius, FContainer.RoundCornerType, 255);

    BaseBgColor := FContainer.BackgroundColor;
    if not FContainer.Visible or (BaseBgColor = clNone) then
       BaseBgColor := Self.Color;

    IsCrossFade := (FLastHoveredTabIndex <> -1) and (FHoveredTabIndex <> -1);

    if IsCrossFade and (FHoverSettings.CurrentAnimationValue = 255) then
    begin
        FLastHoveredTabIndex := -1;
    end
    else if not IsCrossFade and (FLastHoveredTabIndex <> -1) and (FHoveredTabIndex = -1) and (FHoverSettings.CurrentAnimationValue = 0) then
    begin
       FLastHoveredTabIndex := -1;
    end;

    if (FActiveIndex >= 0) and (FActiveIndex < FItems.Count) and FItems[FActiveIndex].Visible then
    begin
      IndicatorRect.Left   := Round(FFromRect.Left + (FToRect.Left - FFromRect.Left) * FActiveIndicatorAnimProgress);
      IndicatorRect.Top    := Round(FFromRect.Top + (FToRect.Top - FFromRect.Top) * FActiveIndicatorAnimProgress);
      IndicatorRect.Right  := Round(FFromRect.Right + (FToRect.Right - FFromRect.Right) * FActiveIndicatorAnimProgress);
      IndicatorRect.Bottom := Round(FFromRect.Bottom + (FToRect.Bottom - FFromRect.Bottom) * FActiveIndicatorAnimProgress);
      InflateRect(IndicatorRect, -FSpacing, -FSpacing);

      FinalIndicatorColor := FActiveIndicator.Color;
      if (FActiveIndex = FHoveredTabIndex) and (htIndicator in FHoverSettings.Targets) then
         FinalIndicatorColor := BlendColors(FActiveIndicator.Color, FHoverSettings.BorderColor, FHoverSettings.CurrentAnimationValue / 255.0);

      case FStyle of
        tsContained: DrawEditBox(LG, IndicatorRect, FActiveIndicator.BackgroundColor, FinalIndicatorColor, FActiveIndicator.Thickness, FActiveIndicator.Style, FActiveIndicator.CornerRadius, FActiveIndicator.RoundCornerType, 255);
        tsPill: DrawEditBox(LG, IndicatorRect, FActiveIndicator.BackgroundColor, FinalIndicatorColor, FActiveIndicator.Thickness, FActiveIndicator.Style, Min(IndicatorRect.Width, IndicatorRect.Height) div 2, rctAll, 255);
        tsUnderlined, tsUnderlinedInverted:
        begin
          IndicatorPen := TGPPen.Create(ColorToARGB(FinalIndicatorColor), FActiveIndicator.Thickness);
          try
            IndicatorPen.SetStartCap(LineCapRound);
            IndicatorPen.SetEndCap(LineCapRound);
            if FOrientation = tboHorizontal then
              if FStyle = tsUnderlined then
                LG.DrawLine(IndicatorPen, IndicatorRect.Left + FActiveIndicator.CornerRadius, IndicatorRect.Bottom - (FActiveIndicator.Thickness/2), IndicatorRect.Right - FActiveIndicator.CornerRadius, IndicatorRect.Bottom - (FActiveIndicator.Thickness/2))
              else
                LG.DrawLine(IndicatorPen, IndicatorRect.Left + FActiveIndicator.CornerRadius, IndicatorRect.Top + (FActiveIndicator.Thickness/2), IndicatorRect.Right - FActiveIndicator.CornerRadius, IndicatorRect.Top + (FActiveIndicator.Thickness/2))
            else
              if FStyle = tsUnderlined then
                LG.DrawLine(IndicatorPen, IndicatorRect.Right - (FActiveIndicator.Thickness/2), IndicatorRect.Top + FActiveIndicator.CornerRadius, IndicatorRect.Right - (FActiveIndicator.Thickness/2), IndicatorRect.Bottom - FActiveIndicator.CornerRadius)
              else
                LG.DrawLine(IndicatorPen, IndicatorRect.Left + (FActiveIndicator.Thickness/2), IndicatorRect.Top + FActiveIndicator.CornerRadius, IndicatorRect.Left + (FActiveIndicator.Thickness/2), IndicatorRect.Bottom - FActiveIndicator.CornerRadius);
          finally
            IndicatorPen.Free;
          end;
        end;
        tsGradientUnderlined: begin var GradRect: TGPRectF; GradRect.X := IndicatorRect.Left; GradRect.Y := IndicatorRect.Bottom - FActiveIndicator.Thickness; GradRect.Width := IndicatorRect.Width; GradRect.Height := FActiveIndicator.Thickness; if (GradRect.Width > 0) and (GradRect.Height > 0) and FGradient.Enabled then begin IndicatorBrush := TGPLinearGradientBrush.Create(GradRect, ColorToARGB(FGradient.StartColor), ColorToARGB(FGradient.EndColor), LinearGradientModeHorizontal); try LG.FillRectangle(IndicatorBrush, GradRect); finally IndicatorBrush.Free; end; end; end;
        tsDottedIndicator: begin var DotSize := Min(6, IndicatorRect.Height div 4); if FItems.Count > FActiveIndex then begin var TextWidth := Self.Canvas.TextWidth(FItems[FActiveIndex].Caption); var DotX := IndicatorRect.Left + (IndicatorRect.Width - TextWidth) div 2 - DotSize - 4; var DotY := IndicatorRect.Top + (IndicatorRect.Height - DotSize) div 2; IndicatorBrush := TGPSolidBrush.Create(ColorToARGB(FActiveFont.Color)); try LG.FillEllipse(IndicatorBrush, DotX, DotY, DotSize, DotSize); finally IndicatorBrush.Free; end; end; end;
        tsBordered: DrawEditBox(LG, IndicatorRect, clNone, FinalIndicatorColor, FActiveIndicator.Thickness, psSolid, FActiveIndicator.CornerRadius, FActiveIndicator.RoundCornerType, 255);
        tsDualLine: begin IndicatorPen := TGPPen.Create(ColorToARGB(FinalIndicatorColor), FActiveIndicator.Thickness); try LG.DrawLine(IndicatorPen, IndicatorRect.Left, IndicatorRect.Top + 1, IndicatorRect.Right, IndicatorRect.Top + 1); LG.DrawLine(IndicatorPen, IndicatorRect.Left, IndicatorRect.Bottom - 1, IndicatorRect.Right, IndicatorRect.Bottom - 1); finally IndicatorPen.Free; end; end;
        tsSideLines: begin IndicatorPen := TGPPen.Create(ColorToARGB(FinalIndicatorColor), FActiveIndicator.Thickness); try LG.DrawLine(IndicatorPen, IndicatorRect.Left + 1, IndicatorRect.Top, IndicatorRect.Left + 1, IndicatorRect.Bottom); LG.DrawLine(IndicatorPen, IndicatorRect.Right - 1, IndicatorRect.Top, IndicatorRect.Right - 1, IndicatorRect.Bottom); finally IndicatorPen.Free; end; end;
      end;
    end;

    for i := 0 to FItems.Count - 1 do
    begin
      if not FItems[i].Visible then Continue;
      ItemRect := GetItemRect(i);
      if IsRectEmpty(ItemRect) then continue;

      LHoverProgress := 0.0;
      if IsCrossFade then
      begin
        if i = FLastHoveredTabIndex then LHoverProgress := 1.0 - (FHoverSettings.CurrentAnimationValue / 255.0);
        if i = FHoveredTabIndex then LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
      end
      else
      begin
        if (i = FHoveredTabIndex) or (i = FLastHoveredTabIndex) then
           LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
      end;

      if (LHoverProgress > 0.001) and not (i = FActiveIndex) and FItems[i].Enabled and (htBackground in FHoverSettings.Targets) then
      begin
        var FinalBackgroundColor := BlendColors(BaseBgColor, FHoverSettings.BackgroundColor, LHoverProgress);
        var HoverRadius := IfThen(FStyle = tsPill, ItemRect.Height div 2, FActiveIndicator.CornerRadius);
        DrawEditBox(LG, ItemRect, FinalBackgroundColor, clNone, 0, psClear, HoverRadius, rctAll, 255);
      end;
    end;

    for i := 0 to FItems.Count - 1 do
    begin
      Item := FItems[i];
      if not Item.Visible then Continue;
      ItemRect := GetItemRect(i);
      if IsRectEmpty(ItemRect) then continue;

      if (FStyle = tsSegmented) and (i = FActiveIndex) then
        DrawEditBox(LG, ItemRect, FActiveIndicator.BackgroundColor, FActiveIndicator.Color, FActiveIndicator.Thickness, FActiveIndicator.Style, FActiveIndicator.CornerRadius, rctAll, 255);

      // --- INÍCIO DA LÓGICA DE FONTE E COR (MODIFICADO) ---

      var  FontToUse: TFont;
      var  ColorToUse: TColor;

      IsItemActive := (i = FActiveIndex);
      IsItemDisabled := not Item.Enabled;

      if IsItemDisabled then
      begin
        FontToUse := Self.Font;
        ColorToUse := clGrayText;
      end
      else if IsItemActive then
      begin
        FontToUse := FActiveFont;
        ColorToUse := FActiveFont.Color; // A cor vem do objeto de fonte ativa
      end
      else // Inativo e Habilitado
      begin
        FontToUse := Self.Font;
        ColorToUse := FInactiveFontColor;

        LHoverProgress := 0.0;
        if IsCrossFade then
        begin
          if i = FLastHoveredTabIndex then LHoverProgress := 1.0 - (FHoverSettings.CurrentAnimationValue / 255.0);
          if i = FHoveredTabIndex then LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
        end
        else
        begin
          if (i = FHoveredTabIndex) or (i = FLastHoveredTabIndex) then
             LHoverProgress := FHoverSettings.CurrentAnimationValue / 255.0;
        end;

        if (LHoverProgress > 0.001) and (htFont in FHoverSettings.Targets) then
        begin
          ColorToUse := BlendColors(FInactiveFontColor, FHoverSettings.FontColor, LHoverProgress);
        end;
      end;
      // --- FIM DA LÓGICA DE FONTE E COR ---

      // --- INÍCIO DA LÓGICA DE LAYOUT DE IMAGEM E TEXTO ---
      var  imageSlotRect, textSlotRect: TRect;
      var  imageAndMarginsW, imageAndMarginsH: Integer;
      var  LDrawW, LDrawH: Integer;
      var  AvailableWidth, AvailableHeight: Integer;
      var  LImgW, LImgH: Integer;
      var  ImageFinalDestRect: TRect;
      var  LImgX, LImgY: Integer;

      textSlotRect := ItemRect;
      imageSlotRect := ItemRect;
      LDrawW := 0;
      LDrawH := 0;

      // Calcula o tamanho do desenho da imagem
      if (Item.ImageSettings.Picture.Graphic <> nil) and not Item.ImageSettings.Picture.Graphic.Empty and Item.ImageSettings.Visible then
      begin
        AvailableWidth := Max(0, imageSlotRect.Width - Item.ImageSettings.Margins.Left - Item.ImageSettings.Margins.Right);
        AvailableHeight := Max(0, imageSlotRect.Height - Item.ImageSettings.Margins.Top - Item.ImageSettings.Margins.Bottom);
        LImgW := Item.ImageSettings.Picture.Width;
        LImgH := Item.ImageSettings.Picture.Height;

        if Item.ImageSettings.AutoSize then
        begin
          case Item.ImageSettings.DrawMode of
            idmStretch:
              begin
                LDrawW := AvailableWidth;
                LDrawH := AvailableHeight;
              end;
            idmProportional:
              begin
                if (LImgW > 0) and (LImgH > 0) and (AvailableWidth > 0) and (AvailableHeight > 0) then
                begin
                  var imgAspectRatio := LImgW / LImgH;
                  var availAspectRatio := AvailableWidth / AvailableHeight;
                  if availAspectRatio > imgAspectRatio then
                  begin
                    LDrawH := AvailableHeight;
                    LDrawW := Round(LDrawH * imgAspectRatio);
                  end
                  else
                  begin
                    LDrawW := AvailableWidth;
                    LDrawH := Round(LDrawW / imgAspectRatio);
                  end;
                end
                else
                begin
                  LDrawW := 0;
                  LDrawH := 0;
                end;
              end;
            idmNormal:
              begin
                LDrawW := LImgW;
                LDrawH := LImgH;
              end;
          else
            LDrawW := LImgW;
            LDrawH := LImgH;
          end;
        end
        else
        begin
          var targetW, targetH: Integer;
          targetW := Item.ImageSettings.TargetWidth;
          targetH := Item.ImageSettings.TargetHeight;
          case Item.ImageSettings.DrawMode of
            idmStretch:
              begin
                LDrawW := targetW;
                LDrawH := targetH;
              end;
            idmProportional:
              begin
                if (LImgW > 0) and (LImgH > 0) and (targetW > 0) and (targetH > 0) then
                begin
                  var imgAspectRatio := LImgW / LImgH;
                  var targetAspectRatio := targetW / targetH;
                  if targetAspectRatio > imgAspectRatio then
                  begin
                    LDrawH := targetH;
                    LDrawW := Round(LDrawH * imgAspectRatio);
                  end
                  else
                  begin
                    LDrawW := targetW;
                    LDrawH := Round(LDrawW / imgAspectRatio);
                  end;
                end
                else
                begin
                  LDrawW := 0;
                  LDrawH := 0;
                end;
              end;
            idmNormal:
              begin
                LDrawW := LImgW;
                LDrawH := LImgH;
              end;
          else
            begin
              LDrawW := LImgW;
              LDrawH := LImgH;
            end;
          end;
        end;
        LDrawW := Max(0, LDrawW);
        LDrawH := Max(0, LDrawH);
        if LDrawW > AvailableWidth then LDrawW := AvailableWidth;
        if LDrawH > AvailableHeight then LDrawH := AvailableHeight;
      end;

      // Particiona o espaço entre a imagem e o texto
      if (LDrawW > 0) then
      begin
        imageAndMarginsW := LDrawW + Item.ImageSettings.Margins.Left + Item.ImageSettings.Margins.Right;
        imageAndMarginsH := LDrawH + Item.ImageSettings.Margins.Top + Item.ImageSettings.Margins.Bottom;

        case Item.ImageSettings.ImagePosition of
          ipLeft:   begin imageSlotRect.Width := imageAndMarginsW; textSlotRect.Left := imageSlotRect.Right; end;
          ipRight:  begin imageSlotRect.Left := imageSlotRect.Right - imageAndMarginsW; textSlotRect.Right := imageSlotRect.Left; end;
          ipTop:    begin imageSlotRect.Height := imageAndMarginsH; textSlotRect.Top := imageSlotRect.Bottom; end;
          ipBottom: begin imageSlotRect.Top := imageSlotRect.Bottom - imageAndMarginsH; textSlotRect.Bottom := imageSlotRect.Top; end;
          ipCenter, ipFill, ipBehind: ; // Nesses casos, os slots se sobrepõem
        end;
      end;

      // Desenha a imagem
      if (LDrawW > 0) then
      begin
        AvailableWidth := Max(0, imageSlotRect.Width - Item.ImageSettings.Margins.Left - Item.ImageSettings.Margins.Right);
        AvailableHeight := Max(0, imageSlotRect.Height - Item.ImageSettings.Margins.Top - Item.ImageSettings.Margins.Bottom);

        case Item.ImageSettings.HorizontalAlign of
          ihaLeft:   LImgX := imageSlotRect.Left + Item.ImageSettings.Margins.Left;
          ihaCenter: LImgX := imageSlotRect.Left + Item.ImageSettings.Margins.Left + (AvailableWidth - LDrawW) div 2;
          ihaRight:  LImgX := imageSlotRect.Right - Item.ImageSettings.Margins.Right - LDrawW;
        else LImgX := imageSlotRect.Left + Item.ImageSettings.Margins.Left;
        end;

        case Item.ImageSettings.VerticalAlign of
          ivaTop:    LImgY := imageSlotRect.Top + Item.ImageSettings.Margins.Top;
          ivaCenter: LImgY := imageSlotRect.Top + Item.ImageSettings.Margins.Top + (AvailableHeight - LDrawH) div 2;
          ivaBottom: LImgY := imageSlotRect.Bottom - Item.ImageSettings.Margins.Bottom - LDrawH;
        else LImgY := imageSlotRect.Top + Item.ImageSettings.Margins.Top;
        end;

        ImageFinalDestRect := Rect(LImgX, LImgY, LImgX + LDrawW, LImgY + LDrawH);

        if (ImageFinalDestRect.Right > ImageFinalDestRect.Left) and (ImageFinalDestRect.Bottom > ImageFinalDestRect.Top) then
        begin
          if Item.ImageSettings.Picture.Graphic is TPNGImage then
            DrawPNGImageWithGDI(LG, Item.ImageSettings.Picture.Graphic as TPNGImage, ImageFinalDestRect, idmStretch)
          else if Item.ImageSettings.Picture.Graphic <> nil then
            DrawNonPNGImageWithCanvas(Self.Canvas, Item.ImageSettings.Picture.Graphic, ImageFinalDestRect, idmStretch);
        end;
      end;

      // Desenha o texto (MODIFICADO para usar FontToUse e ColorToUse)
      if Trim(Item.Caption) <> '' then
      begin
        if (textSlotRect.Width > 0) and (textSlotRect.Height > 0) then
          DrawComponentCaption(Self.Canvas, textSlotRect, Item.Caption, FontToUse, ColorToUse, taCenter, cvaCenter, false, 255);
      end;
      // --- FIM DA LÓGICA DE LAYOUT ---
    end;

    if FStyle = tsSegmented then
    begin
      LastVisibleIndex := -1;
      for i := FItems.Count - 1 downto 0 do if FItems[i].Visible then begin LastVisibleIndex := i; break; end;
      if LastVisibleIndex > 0 then
      begin
        SeparatorPen := TGPPen.Create(ColorToARGB(FContainer.Color), FContainer.Thickness);
        try
          for i := 0 to LastVisibleIndex - 1 do
            if FItems[i].Visible and (i <> FActiveIndex) and ((i + 1) <> FActiveIndex) then
            begin
              ItemRect := GetItemRect(i);
              if FOrientation = tboHorizontal then
                LG.DrawLine(SeparatorPen, ItemRect.Right, ItemRect.Top + 4, ItemRect.Right, ItemRect.Bottom - 4)
              else
                LG.DrawLine(SeparatorPen, ItemRect.Left + 4, ItemRect.Bottom, ItemRect.Right - 4, ItemRect.Bottom);
            end;
        finally
          SeparatorPen.Free;
        end;
      end;
    end;
  finally
    LG.Free;
  end;
end;

end.

