unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.ExtCtrls, Winapi.Messages, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Types, System.Math, Vcl.Imaging.pngimage, Vcl.GraphUtil, System.UITypes,
  ANDMR_ComponentUtils, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Winapi.ActiveX;

type
  TANDMR_CButtonGroup = class; // Forward declaration

  TANDMR_CGroupButtonItem = class(TCollectionItem)
  private
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCornerRadius: Integer;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FClickColor: TColor;
    FOnClick: TNotifyEvent;
    FTag: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FWidth: Integer; // Specific width for this button item

    procedure SetCaption(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);
    procedure SetClickColor(const Value: TColor);
    procedure SetTag(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure FontChanged(Sender: TObject);
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
    property Color: TColor read FColor write SetColor default clTeal;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 12;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clNone;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor default clNone;
    property ClickColor: TColor read FClickColor write SetClickColor default clNone;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Tag: Integer read FTag write SetTag default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Width: Integer read FWidth write SetWidth default 100; // Default width
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

  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FItems: TANDMR_CGroupButtonItems;
    FHoveredItem: TANDMR_CGroupButtonItem;
    FClickedItem: TANDMR_CGroupButtonItem;
    FSelectedItem: TANDMR_CGroupButtonItem;
    FSpacing: Integer;
    FAutoSize: Boolean;
    FAllowDeselection: Boolean;

    // Group Border Properties
    FGroupBorderVisible: Boolean;
    FGroupBorderColor: TColor;
    FGroupBorderWidth: Integer;
    FGroupCornerRadius: Integer;

    // Selection Properties
    FSelectedColor: TColor;
    FSelectedFontColor: TColor;

    // Global Properties
    FGlobalFont: TFont;
    FGlobalColor: TColor;
    FGlobalBorderColor: TColor;
    FGlobalBorderWidth: Integer;
    FGlobalCornerRadius: Integer;
    FGlobalHoverColor: TColor;
    FGlobalHoverFontColor: TColor;
    FGlobalClickColor: TColor;

    procedure SetItems(const Value: TANDMR_CGroupButtonItems);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure UpdateWidth;
    procedure SetSelectedItemIndex(const Value: Integer);
    function GetSelectedItemIndex: Integer;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFontColor(const Value: TColor);

    // Group Border Setters
    procedure SetGroupBorderVisible(const Value: Boolean);
    procedure SetGroupBorderColor(const Value: TColor);
    procedure SetGroupBorderWidth(const Value: Integer);
    procedure SetGroupCornerRadius(const Value: Integer);

    // Global Property Setters
    procedure SetGlobalFont(const Value: TFont);
    procedure SetGlobalColor(const Value: TColor);
    procedure SetGlobalBorderColor(const Value: TColor);
    procedure SetGlobalBorderWidth(const Value: Integer);
    procedure SetGlobalCornerRadius(const Value: Integer);
    procedure SetGlobalHoverColor(const Value: TColor);
    procedure SetGlobalHoverFontColor(const Value: TColor);
    procedure SetGlobalClickColor(const Value: TColor);
    procedure GlobalFontChanged(Sender: TObject);

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
    // Properties for individual item management
    property Items: TANDMR_CGroupButtonItems read FItems write SetItems;
    property Spacing: Integer read FSpacing write SetSpacing default 5;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;

    // Group Border Properties
    property GroupBorderVisible: Boolean read FGroupBorderVisible write SetGroupBorderVisible default False;
    property GroupBorderColor: TColor read FGroupBorderColor write SetGroupBorderColor default clBlack;
    property GroupBorderWidth: Integer read FGroupBorderWidth write SetGroupBorderWidth default 1;
    property GroupCornerRadius: Integer read FGroupCornerRadius write SetGroupCornerRadius default 12;

    // Selection Properties
    property SelectedItemIndex: Integer read GetSelectedItemIndex write SetSelectedItemIndex default -1;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property SelectedFontColor: TColor read FSelectedFontColor write SetSelectedFontColor;
    property AllowDeselection: Boolean read FAllowDeselection write FAllowDeselection default True;

    // Global properties to apply to all items
    property GlobalFont: TFont read FGlobalFont write SetGlobalFont;
    property GlobalColor: TColor read FGlobalColor write SetGlobalColor;
    property GlobalBorderColor: TColor read FGlobalBorderColor write SetGlobalBorderColor;
    property GlobalBorderWidth: Integer read FGlobalBorderWidth write SetGlobalBorderWidth;
    property GlobalCornerRadius: Integer read FGlobalCornerRadius write SetGlobalCornerRadius;
    property GlobalHoverColor: TColor read FGlobalHoverColor write SetGlobalHoverColor;
    property GlobalHoverFontColor: TColor read FGlobalHoverFontColor write SetGlobalHoverFontColor;
    property GlobalClickColor: TColor read FGlobalClickColor write SetGlobalClickColor;


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

// Utility function to draw on the button
procedure DrawButton(Canvas: TCanvas; const ARect: TRect; const AText: string; AFont: TFont; AColor, ABorderColor, AFontColor: TColor; ABorderWidth, ACornerRadius: Integer; AGraphics: TGPGraphics);
var
  LPath: TGPGraphicsPath;
  LRectF: TGPRectF;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LTextRect: TRect; // Local copy for DrawText
begin
  // Create a TGPRectF from the TRect. Note that TRect has no Width/Height properties.
  // We use floating point values to call the correct MakeRect overload.
  LRectF := MakeRect(Single(ARect.Left), Single(ARect.Top), Single(ARect.Right - ARect.Left), Single(ARect.Bottom - ARect.Top));

  LPath := TGPGraphicsPath.Create;
  try
    // Use the custom utility to create a rounded rectangle path
    CreateGPRoundedPath(LPath, LRectF, ACornerRadius, rctAll);

    // Fill the button background
    LBrush := TGPSolidBrush.Create(ColorToARGB(AColor));
    try
      AGraphics.FillPath(LBrush, LPath);
    finally
      LBrush.Free;
    end;

    // Draw the button border if it has a width
    if ABorderWidth > 0 then
    begin
      LPen := TGPPen.Create(ColorToARGB(ABorderColor), ABorderWidth);
      try
        AGraphics.DrawPath(LPen, LPath);
      finally
        LPen.Free;
      end;
    end;
  finally
    LPath.Free;
  end;

  // Draw the button caption
  // Make a mutable copy of ARect because DrawText modifies its rect parameter
  LTextRect := ARect;
  Canvas.Font.Assign(AFont);
  Canvas.Font.Color := AFontColor;
  Canvas.Brush.Style := bsClear; // Make sure text background is transparent
  DrawText(Canvas.Handle, PChar(AText), Length(AText), LTextRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

{ TANDMR_CGroupButtonItem }

constructor TANDMR_CGroupButtonItem.Create(Collection: TCollection);
begin
  inherited;
  FVisible := True;
  FEnabled := True;
  FColor := clTeal;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FCornerRadius := 12;
  FHoverColor := clNone;
  FClickColor := clNone;
  FHoverFontColor := clNone;
  FWidth := 100;

  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FFont.Style := [fsBold];
  FFont.OnChange := FontChanged;
end;

destructor TANDMR_CGroupButtonItem.Destroy;
begin
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
      Self.FColor := FColor;
      Self.FBorderColor := FBorderColor;
      Self.FBorderWidth := FBorderWidth;
      Self.FCornerRadius := FCornerRadius;
      Self.FHoverColor := FHoverColor;
      Self.FHoverFontColor := FHoverFontColor;
      Self.FClickColor := FClickColor;
      Self.FOnClick := FOnClick;
      Self.FTag := FTag;
      Self.FVisible := FVisible;
      Self.FEnabled := FEnabled;
      Self.FWidth := FWidth;
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

function TANDMR_CGroupButtonItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TANDMR_CGroupButtonItem.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetClickColor(const Value: TColor);
begin
  if FClickColor <> Value then
  begin
    FClickColor := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
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

procedure TANDMR_CGroupButtonItem.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then
  begin
    FHoverColor := Value;
    Changed(False);
  end;
end;

procedure TANDMR_CGroupButtonItem.SetHoverFontColor(const Value: TColor);
begin
  if FHoverFontColor <> Value then
  begin
    FHoverFontColor := Value;
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

{ TANDMR_CGroupButtonItems }

function TANDMR_CGroupButtonItems.Add: TANDMR_CGroupButtonItem;
begin
  Result := TANDMR_CGroupButtonItem(inherited Add);
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

{ TANDMR_CButtonGroup }

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csPannable];
  Width := 300;
  Height := 45;

  FItems := TANDMR_CGroupButtonItems.Create(Self);
  FSpacing := 5;
  FHoveredItem := nil;
  FClickedItem := nil;
  FSelectedItem := nil;
  FAutoSize := True;
  FAllowDeselection := True;
  DoubleBuffered := True;

  // Initialize Group Border
  FGroupBorderVisible := False;
  FGroupBorderColor := clBlack;
  FGroupBorderWidth := 1;
  FGroupCornerRadius := 12;

  FSelectedColor := clHighlight;
  FSelectedFontColor := clHighlightText;

  FGlobalFont := TFont.Create;
  FGlobalFont.OnChange := GlobalFontChanged;
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FGlobalFont.Free;
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
  LRect, LInnerRect: TRect;
  LCurrentX: Integer;
  LColor, LFontColor, LBorderColor: TColor;
  LG: TGPGraphics;
  LGroupPath: TGPGraphicsPath;
  LGroupPen: TGPPen;
  LGroupRectF: TGPRectF;
begin
  inherited;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ClientRect);

    // Draw the main group border
    if FGroupBorderVisible and (FGroupBorderWidth > 0) then
    begin
      LGroupRectF := MakeRect(0, 0, Width, Height);
      InflateRectF(LGroupRectF, -FGroupBorderWidth / 2, -FGroupBorderWidth / 2);

      LGroupPath := TGPGraphicsPath.Create;
      try
        CreateGPRoundedPath(LGroupPath, LGroupRectF, FGroupCornerRadius, rctAll);
        LGroupPen := TGPPen.Create(ColorToARGB(FGroupBorderColor), FGroupBorderWidth);
        try
          LG.DrawPath(LGroupPen, LGroupPath);
        finally
          LGroupPen.Free;
        end;
      finally
        LGroupPath.Free;
      end;
    end;

    LInnerRect := ClientRect;
    if FGroupBorderVisible and (FGroupBorderWidth > 0) then
      InflateRect(LInnerRect, -FGroupBorderWidth, -FGroupBorderWidth);


    LCurrentX := LInnerRect.Left;
    for i := 0 to FItems.Count - 1 do
    begin
      LItem := FItems.Items[i];
      if not LItem.Visible then Continue;

      LRect := Rect(LCurrentX, LInnerRect.Top, LCurrentX + LItem.Width, LInnerRect.Bottom);
      LFontColor := LItem.Font.Color;
      LColor := LItem.Color;
      LBorderColor := LItem.BorderColor;

      if LItem.Enabled then
      begin
        if LItem = FSelectedItem then
        begin
          LColor := FSelectedColor;
          LFontColor := FSelectedFontColor;
        end
        else if LItem = FHoveredItem then
        begin
          if LItem.HoverColor <> clNone then
            LColor := LItem.HoverColor;
          if LItem.HoverFontColor <> clNone then
            LFontColor := LItem.HoverFontColor;
        end;

        if LItem = FClickedItem then
        begin
          if LItem.ClickColor <> clNone then
            LColor := LItem.ClickColor;
        end;
      end
      else
      begin
        LColor := clBtnFace;
        LFontColor := clGrayText;
        LBorderColor := clGray;
      end;

      DrawButton(Canvas, LRect, LItem.Caption, LItem.Font, LColor, LBorderColor, LFontColor, LItem.BorderWidth, LItem.CornerRadius, LG);
      LCurrentX := LCurrentX + LItem.Width + FSpacing;
    end;
  finally
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

  if FGroupBorderVisible and (FGroupBorderWidth > 0) then
    LTotalWidth := LTotalWidth + (FGroupBorderWidth * 2);

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
  if FGroupBorderVisible and (FGroupBorderWidth > 0) then
    LBorderOffset := FGroupBorderWidth;

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
        if FAllowDeselection then
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

procedure TANDMR_CButtonGroup.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetSelectedFontColor(const Value: TColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    Invalidate;
  end;
end;

// Group Border Setters
procedure TANDMR_CButtonGroup.SetGroupBorderVisible(const Value: Boolean);
begin
  if FGroupBorderVisible <> Value then
  begin
    FGroupBorderVisible := Value;
    UpdateWidth;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGroupBorderColor(const Value: TColor);
begin
  if FGroupBorderColor <> Value then
  begin
    FGroupBorderColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGroupBorderWidth(const Value: Integer);
begin
  if FGroupBorderWidth <> Value then
  begin
    FGroupBorderWidth := Value;
    UpdateWidth;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGroupCornerRadius(const Value: Integer);
begin
  if FGroupCornerRadius <> Value then
  begin
    FGroupCornerRadius := Value;
    Invalidate;
  end;
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

procedure TANDMR_CButtonGroup.SetGlobalFont(const Value: TFont);
begin
  FGlobalFont.Assign(Value);
  // The OnChange event (GlobalFontChanged) will handle the update
end;

procedure TANDMR_CButtonGroup.SetGlobalColor(const Value: TColor);
var
  i: Integer;
begin
  if FGlobalColor <> Value then
  begin
    FGlobalColor := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].Color := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalBorderColor(const Value: TColor);
var
  i: Integer;
begin
  if FGlobalBorderColor <> Value then
  begin
    FGlobalBorderColor := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].BorderColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalBorderWidth(const Value: Integer);
var
  i: Integer;
begin
  if FGlobalBorderWidth <> Value then
  begin
    FGlobalBorderWidth := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].BorderWidth := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalCornerRadius(const Value: Integer);
var
  i: Integer;
begin
  if FGlobalCornerRadius <> Value then
  begin
    FGlobalCornerRadius := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].CornerRadius := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalHoverColor(const Value: TColor);
var
  i: Integer;
begin
  if FGlobalHoverColor <> Value then
  begin
    FGlobalHoverColor := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].HoverColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalHoverFontColor(const Value: TColor);
var
  i: Integer;
begin
  if FGlobalHoverFontColor <> Value then
  begin
    FGlobalHoverFontColor := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].HoverFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetGlobalClickColor(const Value: TColor);
var
  i: Integer;
begin
  if FGlobalClickColor <> Value then
  begin
    FGlobalClickColor := Value;
    for i := 0 to FItems.Count - 1 do
      FItems[i].ClickColor := Value;
    Invalidate;
  end;
end;

end.
