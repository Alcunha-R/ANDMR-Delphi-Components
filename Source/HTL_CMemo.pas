unit HTL_CMemo;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  HTL_ComponentUtils,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.Math;

type
  THTL_CMemo = class(TCustomControl)
  private
    FBorderSettings: TBorderSettings;
    FFocusSettings: TFocusSettings;
    FSeparatorSettings: TSeparatorSettings;
    FImageSettings: TImageSettings;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FTextMargins: THTL_Margins;

    FCaptionRect: TRect;
    FHovered: Boolean;
    FOpacity: Byte;
    FInternalMemo: TMemo;

    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;

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

    procedure SetBorderSettings(const Value: TBorderSettings);
    procedure SetFocusSettings(const Value: TFocusSettings);
    procedure SetSeparatorSettings(const Value: TSeparatorSettings);
    procedure SetImageSettings(const Value: TImageSettings);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetTextMargins(const Value: THTL_Margins);
    procedure SetOpacity(const Value: Byte);

    procedure SettingsChanged(Sender: TObject);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure TextMarginsChanged(Sender: TObject);

    procedure InternalMemoChange(Sender: TObject);
    procedure InternalMemoEnter(Sender: TObject);
    procedure InternalMemoExit(Sender: TObject);
    procedure InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure InternalMemoKeyPress(Sender: TObject; var Key: Char);
    procedure InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  protected
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
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default True;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssVertical;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;

    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings;
    property FocusSettings: TFocusSettings read FFocusSettings write SetFocusSettings;
    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: THTL_Margins read FTextMargins write SetTextMargins;

    property Opacity: Byte read FOpacity write SetOpacity default 255;

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
  RegisterComponents('HOTLINE', [THTL_CMemo]);
end;

{ THTL_CMemo }

constructor THTL_CMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csSetCaption, csAcceptsControls];
  DoubleBuffered := True;
  Width := 185;
  Height := 80;
  TabStop := True;

  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 4; // Modernized
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.Color := clSilver; // Modernized
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;
  FBorderSettings.BackgroundColor := clWindow; // Modernized

  FFocusSettings := TFocusSettings.Create;
  FFocusSettings.OnChange := SettingsChanged;
  FFocusSettings.BorderColorVisible := True;
  FFocusSettings.BorderColor := clHighlight;
  FFocusSettings.BackgroundColorVisible := False;
  FFocusSettings.BackgroundColor := clWindow;
  FFocusSettings.UnderlineVisible := False;
  FFocusSettings.UnderlineColor := clHighlight; // Modernized
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
//  FImageSettings.Position := ipLeft;
  FImageSettings.Placement := iplInsideBounds;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FCaptionSettings.Font.Style := [fsBold]; // Modernized
  FCaptionSettings.Font.Color := clGrayText; // Modernized
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FTextMargins := THTL_Margins.Create;
  FTextMargins.OnChange := TextMarginsChanged;

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
  FInternalMemo.Color := FBorderSettings.BackgroundColor; // Will now correctly be clWindow

  FInternalMemo.OnChange := InternalMemoChange;
  FInternalMemo.OnEnter := InternalMemoEnter;
  FInternalMemo.OnExit := InternalMemoExit;
  FInternalMemo.OnKeyDown := InternalMemoKeyDown;
  FInternalMemo.OnKeyPress := InternalMemoKeyPress;
  FInternalMemo.OnKeyUp := InternalMemoKeyUp;
end;

destructor THTL_CMemo.Destroy;
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

procedure THTL_CMemo.Loaded;
begin
  inherited Loaded;
  UpdateInternalMemoBounds;
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Visible := Self.Visible;
    FInternalMemo.TabStop := Self.TabStop;
  end;
end;

procedure THTL_CMemo.SettingsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then
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

procedure THTL_CMemo.CaptionSettingsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then
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

procedure THTL_CMemo.HoverSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure THTL_CMemo.TextMarginsChanged(Sender: TObject);
begin
  if (ComponentState * [csLoading, csReading, csDesigning]) <> [] then
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

procedure THTL_CMemo.InternalMemoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure THTL_CMemo.InternalMemoEnter(Sender: TObject);
begin
  if not Self.Focused then
  begin
     Invalidate;
  end;
end;

procedure THTL_CMemo.InternalMemoExit(Sender: TObject);
begin
  Invalidate;
end;

procedure THTL_CMemo.InternalMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

procedure THTL_CMemo.InternalMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

procedure THTL_CMemo.InternalMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

procedure THTL_CMemo.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure THTL_CMemo.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure THTL_CMemo.SetSeparatorSettings(const Value: TSeparatorSettings);
begin
  FSeparatorSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure THTL_CMemo.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  CaptionSettingsChanged(Self);
end;

procedure THTL_CMemo.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  HoverSettingsChanged(Self); // Call HoverSettingsChanged
end;

procedure THTL_CMemo.SetTextMargins(const Value: THTL_Margins);
begin
  FTextMargins.Assign(Value);
  TextMarginsChanged(Self);
end;

procedure THTL_CMemo.SetFocusSettings(const Value: TFocusSettings);
begin
  FFocusSettings.Assign(Value);
  SettingsChanged(Self);
end;

procedure THTL_CMemo.SetOpacity(const Value: Byte);
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

function THTL_CMemo.GetLines: TStrings;
begin
  Result := FInternalMemo.Lines;
end;

procedure THTL_CMemo.SetLines(const Value: TStrings);
begin
  FInternalMemo.Lines.Assign(Value);
end;

function THTL_CMemo.GetReadOnly: Boolean;
begin
  Result := FInternalMemo.ReadOnly;
end;

procedure THTL_CMemo.SetReadOnly(const Value: Boolean);
begin
  FInternalMemo.ReadOnly := Value;
end;

function THTL_CMemo.GetWordWrap: Boolean;
begin
  Result := FInternalMemo.WordWrap;
end;

procedure THTL_CMemo.SetWordWrap(const Value: Boolean);
begin
  FInternalMemo.WordWrap := Value;
end;

function THTL_CMemo.GetScrollBars: TScrollStyle;
begin
  Result := FInternalMemo.ScrollBars;
end;

procedure THTL_CMemo.SetScrollBars(const Value: TScrollStyle);
begin
  FInternalMemo.ScrollBars := Value;
end;

function THTL_CMemo.GetMaxLength: Integer;
begin
  Result := FInternalMemo.MaxLength;
end;

procedure THTL_CMemo.SetMaxLength(const Value: Integer);
begin
  FInternalMemo.MaxLength := Value;
end;

procedure THTL_CMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Font.Assign(Self.Font);
  end;
  UpdateInternalMemoBounds;
  Invalidate;
end;

procedure THTL_CMemo.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
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
  rImageRatio, rAvailBoxRatio: Double; // Kept for AutoSize = True branch
  tempW, tempH: Double; // Kept for AutoSize = True branch
  EffectiveCaptionOffsetX, EffectiveCaptionOffsetY: Integer;
  // New variables for AutoSize = False branch
  targetW, targetH: Integer;
  imgAspectRatio, targetAspectRatio: Single; // Using Single for aspect ratios
  MemoContentArea: TRect; // For iplOutsideBounds
  availHForImgLayoutAdjusted: Integer; // For vertical alignment helper
begin
  if not HandleAllocated then
  begin
    outImgRect := Rect(0,0,0,0);
    outTxtRect := Rect(0, 0, Width, Height);
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
    WorkArea := FullClientRect; // This is now BaseWorkArea conceptually

  // Define BaseWorkArea, MemoDrawingArea, ImagePlacementArea
  var BaseWorkArea: TRect;
  var MemoDrawingArea: TRect; // Area for text and internal elements, border-deflated
  var ImagePlacementArea: TRect; // Area for image placement

  BaseWorkArea := WorkArea; // WorkArea from caption adjustments is our BaseWorkArea

  MemoDrawingArea := BaseWorkArea;
  if FBorderSettings.Thickness > 0 then
    InflateRect(MemoDrawingArea, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  ImagePlacementArea := BaseWorkArea; // Default for iplOutsideBounds
  if FImageSettings.Placement = iplInsideBounds then
    ImagePlacementArea := MemoDrawingArea; // For iplInsideBounds, image uses MemoDrawingArea

  // Initialize output rects
  outTxtRect := MemoDrawingArea; // Text rect starts as the full memo drawing area
  outImgRect := Rect(0,0,0,0);
  outSepRect := Rect(0,0,0,0);
  // MemoContentArea is effectively replaced by MemoDrawingArea or ImagePlacementArea depending on context

  ImgW := 0; ImgH := 0;
  if FImageSettings.Visible and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
  begin
    OriginalImgW := FImageSettings.Picture.Graphic.Width;
    OriginalImgH := FImageSettings.Picture.Graphic.Height;

    if (OriginalImgW > 0) and (OriginalImgH > 0) then
    begin
      // Calculate available space for image within ImagePlacementArea
      availWForImg := ImagePlacementArea.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right;
      availHForImg := ImagePlacementArea.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
      availWForImg := Max(0, availWForImg);
      availHForImg := Max(0, availHForImg);

      if (availWForImg > 0) and (availHForImg > 0) then
      begin
        if FImageSettings.AutoSize then
        begin
          // AutoSize = True: Existing logic using available space
          case FImageSettings.DrawMode of
            idmProportional:
            begin
              rImageRatio := OriginalImgW / OriginalImgH; // Using Double as per original var
              rAvailBoxRatio := availWForImg / availHForImg; // Using Double
              if rAvailBoxRatio > rImageRatio then // Fit to height
              begin
                ImgH := availHForImg;
                tempW := availHForImg * rImageRatio; // Using Double
                ImgW := Round(tempW);
                if (ImgW = 0) and (tempW > 0) then ImgW := 1;
              end
              else // Fit to width
              begin
                ImgW := availWForImg;
                if rImageRatio > 0 then
                begin
                  tempH := availWForImg / rImageRatio; // Using Double
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
          else // Default, similar to proportional or normal based on context
            ImgW := OriginalImgW; ImgH := OriginalImgH;
          end;
        end
        else // AutoSize = False: Use TargetWidth and TargetHeight
        begin
          targetW := FImageSettings.TargetWidth;
          targetH := FImageSettings.TargetHeight;

          case FImageSettings.DrawMode of
            idmProportional:
            begin
              if (OriginalImgW = 0) or (OriginalImgH = 0) or (targetW <= 0) or (targetH <= 0) then
              begin
                ImgW := 0; ImgH := 0;
              end
              else
              begin
                imgAspectRatio := OriginalImgW / OriginalImgH; // Using Single
                targetAspectRatio := targetW / targetH;     // Using Single
                if targetAspectRatio > imgAspectRatio then // Fit to target height
                begin
                  ImgH := targetH;
                  ImgW := Round(ImgH * imgAspectRatio);
                end
                else // Fit to target width
                begin
                  ImgW := targetW;
                  ImgH := Round(ImgW / imgAspectRatio);
                end;
              end;
            end;
            idmStretch:
            begin
              ImgW := targetW;
              ImgH := targetH;
            end;
          else // Default, similar to normal
            ImgW := OriginalImgW; ImgH := OriginalImgH;
          end;
        end;
      end
      else // Not (availWForImg > 0) and (availHForImg > 0)
      begin
        ImgW := 0; // No available space for image
        ImgH := 0;
      end;
    end
    else // Not (OriginalImgW > 0) and (OriginalImgH > 0)
    begin
      ImgW := 0; // Original image has no dimensions
      ImgH := 0;
    end;
  end
  else // Image not visible or no graphic
  begin
    ImgW := 0;
    ImgH := 0;
  end;

  ImgW := Max(0, ImgW);
  ImgH := Max(0, ImgH);

  SepW := 0;
  if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) then
    SepW := FSeparatorSettings.Thickness;

  // --- Unified Image and Text Layout Logic ---
  if FImageSettings.Visible and (ImgW > 0) then
  begin
    var slotStartX, slotAvailableWidth, slotEndX: Integer; // For Horizontal Alignment

    // Horizontal positioning of image within ImagePlacementArea
//    if FImageSettings.Position = ipLeft then
//    begin
//      slotStartX := ImagePlacementArea.Left + FImageSettings.Margins.Left;
//      slotAvailableWidth := availWForImg;
//      case FImageSettings.HorizontalAlign of
//        ihaLeft:   outImgRect.Left := slotStartX;
//        ihaCenter: outImgRect.Left := slotStartX + (slotAvailableWidth - ImgW) div 2;
//        ihaRight:  outImgRect.Left := slotStartX + slotAvailableWidth - ImgW;
//      else         outImgRect.Left := slotStartX + (slotAvailableWidth - ImgW) div 2; // Default center
//      end;
//      outImgRect.Right := outImgRect.Left + ImgW;
//    end
//    else // ipsRight
//    begin
//      slotEndX := ImagePlacementArea.Right - FImageSettings.Margins.Right;
//      slotAvailableWidth := availWForImg;
//      case FImageSettings.HorizontalAlign of
//        ihaLeft:   outImgRect.Left := slotEndX - slotAvailableWidth;
//        ihaCenter: outImgRect.Left := slotEndX - slotAvailableWidth + (slotAvailableWidth - ImgW) div 2;
//        ihaRight:  outImgRect.Left := slotEndX - ImgW;
//      else         outImgRect.Left := slotEndX - slotAvailableWidth + (slotAvailableWidth - ImgW) div 2; // Default center
//      end;
//      outImgRect.Right := outImgRect.Left + ImgW;
//    end;

    // Vertical positioning of image within ImagePlacementArea
    availHForImgLayoutAdjusted := ImagePlacementArea.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
    availHForImgLayoutAdjusted := Max(0, availHForImgLayoutAdjusted);
    case FImageSettings.VerticalAlign of
      ivaTop:    outImgRect.Top := ImagePlacementArea.Top + FImageSettings.Margins.Top;
      ivaCenter: outImgRect.Top := ImagePlacementArea.Top + FImageSettings.Margins.Top + (availHForImgLayoutAdjusted - ImgH) div 2;
      ivaBottom: outImgRect.Top := ImagePlacementArea.Bottom - FImageSettings.Margins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;

    // Clip image rect to ImagePlacementArea
    if outImgRect.Left < ImagePlacementArea.Left then outImgRect.Left := ImagePlacementArea.Left;
    if outImgRect.Right > ImagePlacementArea.Right then outImgRect.Right := ImagePlacementArea.Right;
    if outImgRect.Top < ImagePlacementArea.Top then outImgRect.Top := ImagePlacementArea.Top;
    if outImgRect.Bottom > ImagePlacementArea.Bottom then outImgRect.Bottom := ImagePlacementArea.Bottom;
    if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
    ImgW := outImgRect.Width; // Update ImgW/H after clipping
    ImgH := outImgRect.Height;


    // This block is removed as the logic is now integrated below.
  end
  else // No image visible
  begin
    outImgRect := Rect(0,0,0,0); // Ensure image rect is zeroed if not visible
    ImgW := 0; ImgH := 0;
    // outTxtRect remains MemoDrawingArea, potentially adjusted by separator if visible without image
  end;

  // --- Combined Image, Separator, and TextRect Horizontal Adjustment ---
  if FImageSettings.Visible and (ImgW > 0) then
  begin
    if FImageSettings.Placement = iplInsideBounds then
    begin // Image AND Separator (if visible) are INSIDE MemoDrawingArea, carving space from outTxtRect
//      if FImageSettings.Position = ipLeft then
//      begin
//        if FSeparatorSettings.Visible and (SepW > 0) then
//        begin
//          outSepRect.Left := outImgRect.Right + FImageSettings.Margins.Right + FSeparatorSettings.Padding;
//          outSepRect.Right := outSepRect.Left + SepW;
//          outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding;
//        end
//        else // No separator
//        begin
//          outTxtRect.Left := outImgRect.Right + FImageSettings.Margins.Right;
//          outSepRect := Rect(0,0,0,0);
//        end;
//      end
//      else // Image position ipsRight
//      begin
//        if FSeparatorSettings.Visible and (SepW > 0) then
//        begin
//          outSepRect.Right := outImgRect.Left - FImageSettings.Margins.Left - FSeparatorSettings.Padding;
//          outSepRect.Left := outSepRect.Right - SepW;
//          outTxtRect.Right := outSepRect.Left - FSeparatorSettings.Padding;
//        end
//        else // No separator
//        begin
//          outTxtRect.Right := outImgRect.Left - FImageSettings.Margins.Left;
//          outSepRect := Rect(0,0,0,0);
//        end;
//      end;
    end
    else // iplOutsideBounds: Image is OUTSIDE MemoDrawingArea. outTxtRect is not changed by image.
         // Separator is between image and MemoDrawingArea.
    begin
      if FSeparatorSettings.Visible and (SepW > 0) then
      begin
//        if FImageSettings.Position = ipLeft then
//        begin
//          outSepRect.Left := outImgRect.Right + FImageSettings.Margins.Right + FSeparatorSettings.Padding;
//          outSepRect.Right := outSepRect.Left + SepW;
//          // Note: outTxtRect.Left (MemoDrawingArea.Left) is not affected by image/sep outside
//        end
//        else // ipsRight
//        begin
//          outSepRect.Right := outImgRect.Left - FImageSettings.Margins.Left - FSeparatorSettings.Padding;
//          outSepRect.Left := outSepRect.Right - SepW;
//          // Note: outTxtRect.Right (MemoDrawingArea.Right) is not affected by image/sep outside
//        end;
      end
      else // No separator
      begin
        outSepRect := Rect(0,0,0,0);
      end;
      // outTxtRect remains MemoDrawingArea as image is outside.
    end;
  end
  else // No image is visible
  begin
    // Separator might still be visible, e.g., at an edge of MemoDrawingArea
    if FSeparatorSettings.Visible and (SepW > 0) then
    begin
      // Default: place separator on left of MemoDrawingArea if no image
      // This could be made configurable if needed (e.g. separator position without image)
      outSepRect.Left := MemoDrawingArea.Left + FSeparatorSettings.Padding;
      outSepRect.Right := outSepRect.Left + SepW;
      outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding;
    end
    else // No image and no separator
    begin
      outSepRect := Rect(0,0,0,0);
      // outTxtRect remains MemoDrawingArea
    end;
  end;

  // --- Common Vertical Separator positioning and final clipping ---
  if FSeparatorSettings.Visible and (SepW > 0) and not IsRectEmpty(outSepRect) then
  begin
    var SepH: Integer;
    var SepRefRect: TRect; // Reference for separator's vertical extent
    if FImageSettings.Placement = iplInsideBounds then
        SepRefRect := MemoDrawingArea // Separator is relative to the memo's drawing area
    else if FImageSettings.Visible and (ImgW > 0) then // Outside, and image exists
        SepRefRect := outImgRect // Align with the image vertically
    else
        SepRefRect := MemoDrawingArea; // Fallback if image outside but not visible

    case FSeparatorSettings.HeightMode of
      shmFull:    SepH := SepRefRect.Height;
      shmAsText:  SepH := outTxtRect.Height;
      shmAsImage: if FImageSettings.Visible and (ImgH > 0) then SepH := ImgH else SepH := SepRefRect.Height;
      shmCustom:  if FSeparatorSettings.CustomHeight > 0 then SepH := FSeparatorSettings.CustomHeight else SepH := SepRefRect.Height;
    else SepH := SepRefRect.Height;
    end;
    SepH := Max(0, SepH);

    outSepRect.Top := SepRefRect.Top + (SepRefRect.Height - SepH) div 2;
    outSepRect.Bottom := outSepRect.Top + SepH;

    // Clip separator to its relevant context (MemoDrawingArea if inside, or FullClientRect if related to outside image)
    var ClipRectForSep: TRect;
    if FImageSettings.Placement = iplInsideBounds then ClipRectForSep := MemoDrawingArea
    else ClipRectForSep := FullClientRect; // Allow separator to be outside MemoDrawingArea if image is

    if outSepRect.Left < ClipRectForSep.Left then outSepRect.Left := ClipRectForSep.Left;
    if outSepRect.Right > ClipRectForSep.Right then outSepRect.Right := ClipRectForSep.Right;
    if outSepRect.Top < ClipRectForSep.Top then outSepRect.Top := ClipRectForSep.Top;
    if outSepRect.Bottom > ClipRectForSep.Bottom then outSepRect.Bottom := ClipRectForSep.Bottom;
    if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left;
    if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
    SepW := outSepRect.Width;
  end else outSepRect := Rect(0,0,0,0);


  // Final clipping and validation of outTxtRect to MemoDrawingArea
  if outTxtRect.Left < MemoDrawingArea.Left then outTxtRect.Left := MemoDrawingArea.Left;
  if outTxtRect.Right > MemoDrawingArea.Right then outTxtRect.Right := MemoDrawingArea.Right;
  if outTxtRect.Top < MemoDrawingArea.Top then outTxtRect.Top := MemoDrawingArea.Top;
  if outTxtRect.Bottom > MemoDrawingArea.Bottom then outTxtRect.Bottom := MemoDrawingArea.Bottom;
  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;
  if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
end;

procedure THTL_CMemo.UpdateInternalMemoBounds;
var
  LImgRect, LTxtRect, LSepRect: TRect;
  MemoRect: TRect;
begin
  CalculateLayout(LImgRect, LTxtRect, LSepRect);

  MemoRect.Left   := LTxtRect.Left + FTextMargins.Left;
  MemoRect.Top    := LTxtRect.Top + FTextMargins.Top;
  MemoRect.Right  := LTxtRect.Right - FTextMargins.Right;
  MemoRect.Bottom := LTxtRect.Bottom - FTextMargins.Bottom;

  if (Self.FBorderSettings.CornerRadius > 0) and (Self.FBorderSettings.RoundCornerType <> TRoundCornerType.rctNone) then
  begin
    var CornerPadding: Integer;
    CornerPadding := Round(Self.FBorderSettings.CornerRadius * 0.5);

    var IsTopLeftRounded, IsTopRightRounded, IsBottomLeftRounded, IsBottomRightRounded: Boolean;

    IsTopLeftRounded := Self.FBorderSettings.RoundCornerType in
      [TRoundCornerType.rctAll, TRoundCornerType.rctTopLeft, TRoundCornerType.rctTop, TRoundCornerType.rctLeft, TRoundCornerType.rctTopLeftBottomRight];
    IsTopRightRounded := Self.FBorderSettings.RoundCornerType in
      [TRoundCornerType.rctAll, TRoundCornerType.rctTopRight, TRoundCornerType.rctTop, TRoundCornerType.rctRight, TRoundCornerType.rctTopRightBottomLeft];
    IsBottomLeftRounded := Self.FBorderSettings.RoundCornerType in
      [TRoundCornerType.rctAll, TRoundCornerType.rctBottomLeft, TRoundCornerType.rctBottom, TRoundCornerType.rctLeft, TRoundCornerType.rctTopRightBottomLeft];
    IsBottomRightRounded := Self.FBorderSettings.RoundCornerType in
      [TRoundCornerType.rctAll, TRoundCornerType.rctBottomRight, TRoundCornerType.rctBottom, TRoundCornerType.rctRight, TRoundCornerType.rctTopLeftBottomRight];

    if IsTopLeftRounded or IsBottomLeftRounded then
      MemoRect.Left := MemoRect.Left + CornerPadding;

    if IsTopRightRounded or IsBottomRightRounded then
      MemoRect.Right := MemoRect.Right - CornerPadding;

    if IsTopLeftRounded or IsTopRightRounded then
      MemoRect.Top := MemoRect.Top + CornerPadding;

    if IsBottomLeftRounded or IsBottomRightRounded then
      MemoRect.Bottom := MemoRect.Bottom - CornerPadding;
  end;

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

procedure THTL_CMemo.Resize;
begin
  inherited Resize;
  UpdateInternalMemoBounds;
  Invalidate;
end;

procedure THTL_CMemo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if FFocusSettings.BorderColorVisible or FFocusSettings.BackgroundColorVisible or FFocusSettings.UnderlineVisible then
    Invalidate;
  if Assigned(FInternalMemo) and FInternalMemo.CanFocus then
    FInternalMemo.SetFocus;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure THTL_CMemo.CMExit(var Message: TCMExit);
begin
  if FFocusSettings.BorderColorVisible or FFocusSettings.BackgroundColorVisible or FFocusSettings.UnderlineVisible then
    Invalidate;
  inherited;
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure THTL_CMemo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
  end;
  FHoverSettings.StartAnimation(True);
end;

procedure THTL_CMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
  end;
  FHoverSettings.StartAnimation(False);
end;

procedure THTL_CMemo.SetTabStop(Value: Boolean);
begin
  inherited TabStop := Value;
  if Assigned(FInternalMemo) then
    FInternalMemo.TabStop := Self.TabStop;
end;

procedure THTL_CMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure THTL_CMemo.Paint;
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
      BaseMemoBG := FBorderSettings.BackgroundColor;
      BaseMemoFontColor := Self.Font.Color;
      BaseCaptionColor := IfThen(FCaptionSettings.Color = clDefault, Self.Font.Color, FCaptionSettings.Color);

      var HoverFrameBG, HoverBorderColor, HoverMemoBG, HoverMemoFontColor, HoverCaptionColor: TColor;
      HoverFrameBG := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
      HoverBorderColor := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
      HoverMemoBG := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
      HoverMemoFontColor := IfThen(FHoverSettings.Enabled, FHoverSettings.FontColor, clNone);
      HoverCaptionColor := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);

      var FocusFrameBG, FocusBorderColor, FocusMemoBG, FocusMemoFontColor, FocusCaptionColor: TColor;
      FocusFrameBG := IfThen(FFocusSettings.BackgroundColorVisible, FFocusSettings.BackgroundColor, clNone);
      FocusBorderColor := FFocusSettings.BorderColor;
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
      NonHoveredFrameBG     := ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseFrameBG,         clNone,            FocusFrameBG,      DisabledFrameBG,      FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      NonHoveredBorderColor := ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseBorderColor,     clNone,            FocusBorderColor,  DisabledBorderColor,  FHoverSettings.Enabled, FFocusSettings.BorderColorVisible);
      NonHoveredMemoFontColor:= ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseMemoFontColor,   clNone,            FocusMemoFontColor,DisabledMemoFontColor,False,                  False);
      NonHoveredCaptionColor:= ResolveStateColor(Self.Enabled, False, IsComponentFocused, BaseCaptionColor,    clNone,            FocusCaptionColor, DisabledCaptionColor, False,                  False);

      var TargetStateFrameBG, TargetStateBorderColor, TargetStateMemoBG, TargetStateMemoFontColor, TargetStateCaptionColor: TColor;
      TargetStateFrameBG      := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseFrameBG,         HoverFrameBG,      FocusFrameBG,      DisabledFrameBG,      FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      TargetStateBorderColor  := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseBorderColor,     HoverBorderColor,  FocusBorderColor,  DisabledBorderColor,  FHoverSettings.Enabled, FFocusSettings.BorderColorVisible);
      TargetStateMemoFontColor:= ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseMemoFontColor,   HoverMemoFontColor,FocusMemoFontColor,DisabledMemoFontColor,FHoverSettings.Enabled, False);
      TargetStateCaptionColor := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, BaseCaptionColor,    HoverCaptionColor, FocusCaptionColor, DisabledCaptionColor, FHoverSettings.Enabled, False);

// New calculation for TargetStateMemoBG:
if Self.Enabled then
begin
  if FHovered and FHoverSettings.Enabled and (FHoverSettings.BackgroundColor <> clNone) then
  begin
    TargetStateMemoBG := FHoverSettings.BackgroundColor;
  end
  else if IsComponentFocused and FFocusSettings.BackgroundColorVisible and (FFocusSettings.BackgroundColor <> clNone) then
  begin
    TargetStateMemoBG := FFocusSettings.BackgroundColor;
  end
  else
  begin
    TargetStateMemoBG := BaseMemoBG;
  end;
end
else
begin
  TargetStateMemoBG := DisabledMemoBG;
end;

// New calculation for NonHoveredMemoBG:
if Self.Enabled then
begin
  if IsComponentFocused and FFocusSettings.BackgroundColorVisible and (FFocusSettings.BackgroundColor <> clNone) then
  begin
    NonHoveredMemoBG := FFocusSettings.BackgroundColor;
  end
  else
  begin
    NonHoveredMemoBG := BaseMemoBG;
  end;
end
else
begin
  NonHoveredMemoBG := DisabledMemoBG;
end;

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
//        if (FImageSettings.Picture.Graphic is TPNGImage) then
//          DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, imgR, idmStretch)
//        else
//          DrawNonPNGImageWithCanvas(Canvas, FImageSettings.Picture.Graphic, imgR, idmStretch);
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
