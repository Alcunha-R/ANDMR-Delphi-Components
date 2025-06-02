unit ANDMR_CMemo;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  ANDMR_ComponentUtils,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, Vcl.Imaging.pngimage,
  System.Math; // Added units

type
  TANDMR_CMemo = class(TCustomControl)
  private
    FCaptionRect: TRect;
    FHovered: Boolean;
    FCornerRadius: Integer;
    FRoundCornerType: TRoundCornerType;
    FActiveColor, FInactiveColor: TColor;
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;
    FImage: TPicture;
    FImageVisible: Boolean;
    FImagePosition: TImagePositionSide;
    FImageAlignment: TImageAlignmentVertical;
    FImageMargins: TANDMR_Margins;
    FImagePlacement: TImagePlacement;
    FImageDrawMode: TImageDrawMode;
    FSeparatorVisible: Boolean;
    FSeparatorColor: TColor;
    FSeparatorThickness: Integer;
    FSeparatorPadding: Integer;
    FSeparatorHeightMode: TSeparatorHeightMode;
    FSeparatorCustomHeight: Integer;
    FCaptionSettings: TCaptionSettings;
    FHoverSettings: THoverSettings;
    FTextMargins: TANDMR_Margins;
    FFocusBorderColor: TColor;
    FFocusBorderColorVisible: Boolean;
    FFocusBackgroundColor: TColor;
    FFocusBackgroundColorVisible: Boolean;
    FFocusUnderlineColor: TColor;
    FFocusUnderlineVisible: Boolean;
    FFocusUnderlineThickness: Integer;
    FFocusUnderlineStyle: TPenStyle;
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
    // procedure SetFont(const Value: TFont); // Declaration will be in public/protected

    // Setter Methods for Appearance
    procedure SetCornerRadius(const Value: Integer);
    procedure SetRoundCornerType(const Value: TRoundCornerType);
    procedure SetActiveColor(const Value: TColor);
    procedure SetInactiveColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Integer);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetImage(const Value: TPicture);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetImagePosition(const Value: TImagePositionSide);
    procedure SetImageAlignment(const Value: TImageAlignmentVertical);
    procedure SetImageMargins(const Value: TANDMR_Margins);
    procedure SetImagePlacement(const Value: TImagePlacement);
    procedure SetImageDrawMode(const Value: TImageDrawMode);
    procedure SetSeparatorVisible(const Value: Boolean);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSeparatorThickness(const Value: Integer);
    procedure SetSeparatorPadding(const Value: Integer);
    procedure SetSeparatorHeightMode(const Value: TSeparatorHeightMode);
    procedure SetSeparatorCustomHeight(const Value: Integer);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure SetTextMargins(const Value: TANDMR_Margins);
    procedure SetFocusBorderColor(const Value: TColor);
    procedure SetFocusBorderColorVisible(const Value: Boolean);
    procedure SetFocusBackgroundColor(const Value: TColor);
    procedure SetFocusBackgroundColorVisible(const Value: Boolean);
    procedure SetFocusUnderlineColor(const Value: TColor);
    procedure SetFocusUnderlineVisible(const Value: Boolean);
    procedure SetFocusUnderlineThickness(const Value: Integer);
    procedure SetFocusUnderlineStyle(const Value: TPenStyle);
    procedure SetOpacity(const Value: Byte);

    // Event Handlers for Settings Objects
    procedure ImageChanged(Sender: TObject);
    procedure ImageMarginsChanged(Sender: TObject);
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
    procedure SetTabStop(Value: Boolean);
    procedure SetFont(const Value: TFont);  // Moved to protected

    // Methods copied/adapted from TANDMR_CEdit
    procedure CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); virtual;
    // DrawEditBox, DrawPNGImageWithGDI, DrawNonPNGImageWithCanvas, DrawSeparatorWithCanvas are now in ANDMR_ComponentUtils
    procedure UpdateInternalMemoBounds; virtual; // New for TANDMR_CMemo
    procedure Resize; override; // New for TANDMR_CMemo
    procedure Loaded; override; // Added for CMemo fix
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    // procedure SetFont(const Value: TFont); override; // Declaration moved to protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    // Core Memo Properties
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default True;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssVertical;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;

    // Appearance Properties
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;
    property RoundCornerType: TRoundCornerType read FRoundCornerType write SetRoundCornerType default rctAll;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clHighlight;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderThickness: Integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;

    property Image: TPicture read FImage write SetImage;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default True;
    property ImagePosition: TImagePositionSide read FImagePosition write SetImagePosition default ipsLeft;
    property ImageAlignment: TImageAlignmentVertical read FImageAlignment write SetImageAlignment default iavCenter;
    property ImageMargins: TANDMR_Margins read FImageMargins write SetImageMargins;
    property ImagePlacement: TImagePlacement read FImagePlacement write SetImagePlacement default iplInsideBounds;
    property ImageDrawMode: TImageDrawMode read FImageDrawMode write SetImageDrawMode default idmProportional;

    property SeparatorVisible: Boolean read FSeparatorVisible write SetSeparatorVisible default False;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clGrayText;
    property SeparatorThickness: Integer read FSeparatorThickness write SetSeparatorThickness default 1;
    property SeparatorPadding: Integer read FSeparatorPadding write SetSeparatorPadding default 2;
    property SeparatorHeightMode: TSeparatorHeightMode read FSeparatorHeightMode write SetSeparatorHeightMode default shmFull;
    property SeparatorCustomHeight: Integer read FSeparatorCustomHeight write SetSeparatorCustomHeight default 0;

    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;

    property FocusBorderColor: TColor read FFocusBorderColor write SetFocusBorderColor;
    property FocusBorderColorVisible: Boolean read FFocusBorderColorVisible write SetFocusBorderColorVisible;
    property FocusBackgroundColor: TColor read FFocusBackgroundColor write SetFocusBackgroundColor;
    property FocusBackgroundColorVisible: Boolean read FFocusBackgroundColorVisible write SetFocusBackgroundColorVisible;
    property FocusUnderlineColor: TColor read FFocusUnderlineColor write SetFocusUnderlineColor;
    property FocusUnderlineVisible: Boolean read FFocusUnderlineVisible write SetFocusUnderlineVisible;
    property FocusUnderlineThickness: Integer read FFocusUnderlineThickness write SetFocusUnderlineThickness;
    property FocusUnderlineStyle: TPenStyle read FFocusUnderlineStyle write SetFocusUnderlineStyle;

    property Opacity: Byte read FOpacity write SetOpacity default 255;

    // Standard Properties
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font; // Will be used by the internal TMemo
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True; // Important for a Memo-like control
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

uses
  System.Types; // System.UITypes removed as it's in interface uses
  // ANDMR_ComponentUtils; // Removed as it's in the interface uses clause

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CMemo]);
end;

{ TANDMR_CMemo }

procedure TANDMR_CMemo.ImageChanged(Sender: TObject); begin UpdateInternalMemoBounds; Invalidate; end;
procedure TANDMR_CMemo.ImageMarginsChanged(Sender: TObject); begin UpdateInternalMemoBounds; Invalidate; end;
procedure TANDMR_CMemo.CaptionSettingsChanged(Sender: TObject); begin UpdateInternalMemoBounds; Invalidate; end;
procedure TANDMR_CMemo.HoverSettingsChanged(Sender: TObject); begin Invalidate; end; // Hover generally doesn't change bounds
procedure TANDMR_CMemo.TextMarginsChanged(Sender: TObject); begin UpdateInternalMemoBounds; Invalidate; end;

// --- Internal Memo Event Handler Implementations ---
procedure TANDMR_CMemo.InternalMemoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TANDMR_CMemo.InternalMemoEnter(Sender: TObject);
begin
  // This event signifies FInternalMemo itself received focus.
  // TANDMR_CMemo's own focus (frame, etc.) is handled by CMEnter.
  // No need to call FOnEnter(Self) here as CMEnter will do it when appropriate.
  // However, we might want to force a repaint of the parent if focus appearance changes.
  Invalidate; // Repaint TANDMR_CMemo to reflect focus on its internal part
end;

procedure TANDMR_CMemo.InternalMemoExit(Sender: TObject);
begin
  // This event signifies FInternalMemo itself lost focus.
  // TANDMR_CMemo's own focus loss is handled by CMExit.
  // No need to call FOnExit(Self) here.
  Invalidate; // Repaint TANDMR_CMemo to reflect focus loss on its internal part
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
// --- End of Internal Memo Event Handler Implementations ---

// --- Setter Implementations (Copied from TANDMR_CEdit) ---
procedure TANDMR_CMemo.SetCornerRadius(const Value: Integer); begin if FCornerRadius <> Value then begin FCornerRadius := Value; Invalidate; end; end;
procedure TANDMR_CMemo.SetRoundCornerType(const Value: TRoundCornerType); begin if FRoundCornerType <> Value then begin FRoundCornerType := Value; Invalidate; end; end;
procedure TANDMR_CMemo.SetActiveColor(const Value: TColor); begin if FActiveColor <> Value then begin FActiveColor := Value; Invalidate; end; end;
procedure TANDMR_CMemo.SetInactiveColor(const Value: TColor); begin if FInactiveColor <> Value then begin FInactiveColor := Value; Invalidate; end; end;
procedure TANDMR_CMemo.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Invalidate; end; end;
procedure TANDMR_CMemo.SetBorderThickness(const Value: Integer); begin if FBorderThickness <> Value then begin FBorderThickness := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetBorderStyle(const Value: TPenStyle); begin if FBorderStyle <> Value then begin FBorderStyle := Value; Invalidate; end; end; // Style itself doesn't change bounds

procedure TANDMR_CMemo.SetImage(const Value: TPicture); begin FImage.Assign(Value); ImageChanged(Self); end; // ImageChanged now calls UpdateInternalMemoBounds
procedure TANDMR_CMemo.SetImageVisible(const Value: Boolean); begin if FImageVisible <> Value then begin FImageVisible := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetImagePosition(const Value: TImagePositionSide); begin if FImagePosition <> Value then begin FImagePosition := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetImageAlignment(const Value: TImageAlignmentVertical); begin if FImageAlignment <> Value then begin FImageAlignment := Value; UpdateInternalMemoBounds; Invalidate; end; end; // Alignment might affect overall layout if image size is dynamic or clips
procedure TANDMR_CMemo.SetImageMargins(const Value: TANDMR_Margins); begin FImageMargins.Assign(Value); ImageMarginsChanged(Self); end; // ImageMarginsChanged now calls UpdateInternalMemoBounds
procedure TANDMR_CMemo.SetImagePlacement(const Value: TImagePlacement); begin if FImagePlacement <> Value then begin FImagePlacement := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetImageDrawMode(const Value: TImageDrawMode); begin if FImageDrawMode <> Value then begin FImageDrawMode := Value; Invalidate; end; end; // Draw mode unlikely to change bounds

procedure TANDMR_CMemo.SetSeparatorVisible(const Value: Boolean); begin if FSeparatorVisible <> Value then begin FSeparatorVisible := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetSeparatorColor(const Value: TColor); begin if FSeparatorColor <> Value then begin FSeparatorColor := Value; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CMemo.SetSeparatorThickness(const Value: Integer); var ValidThickness: Integer; begin ValidThickness := Max(0, Value); if FSeparatorThickness <> ValidThickness then begin FSeparatorThickness := ValidThickness; if FSeparatorVisible then UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetSeparatorPadding(const Value: Integer); var ValidPadding: Integer; begin ValidPadding := Max(0, Value); if FSeparatorPadding <> ValidPadding then begin FSeparatorPadding := ValidPadding; if FSeparatorVisible then UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetSeparatorHeightMode(const Value: TSeparatorHeightMode); begin if FSeparatorHeightMode <> Value then begin FSeparatorHeightMode := Value; UpdateInternalMemoBounds; Invalidate; end; end;
procedure TANDMR_CMemo.SetSeparatorCustomHeight(const Value: Integer); var ValidHeight: Integer; begin ValidHeight := Max(0, Value); if FSeparatorCustomHeight <> ValidHeight then begin FSeparatorCustomHeight := ValidHeight; if (FSeparatorVisible and (FSeparatorHeightMode = shmCustom)) then UpdateInternalMemoBounds; Invalidate; end; end;

procedure TANDMR_CMemo.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); CaptionSettingsChanged(Self); end;
procedure TANDMR_CMemo.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); HoverSettingsChanged(Self); end;
procedure TANDMR_CMemo.SetTextMargins(const Value: TANDMR_Margins); begin FTextMargins.Assign(Value); TextMarginsChanged(Self); end;

procedure TANDMR_CMemo.SetFocusBorderColor(const Value: TColor);
begin
  if FFocusBorderColor <> Value then
  begin
    FFocusBorderColor := Value;
    if FFocusBorderColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusBorderColorVisible(const Value: Boolean);
begin
  if FFocusBorderColorVisible <> Value then
  begin
    FFocusBorderColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusBackgroundColor(const Value: TColor);
begin
  if FFocusBackgroundColor <> Value then
  begin
    FFocusBackgroundColor := Value;
    if FFocusBackgroundColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusBackgroundColorVisible(const Value: Boolean);
begin
  if FFocusBackgroundColorVisible <> Value then
  begin
    FFocusBackgroundColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusUnderlineColor(const Value: TColor);
begin
  if FFocusUnderlineColor <> Value then
  begin
    FFocusUnderlineColor := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusUnderlineVisible(const Value: Boolean);
begin
  if FFocusUnderlineVisible <> Value then
  begin
    FFocusUnderlineVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusUnderlineThickness(const Value: Integer);
var ValidThickness: Integer;
begin
  ValidThickness := Max(0, Value);
  if FFocusUnderlineThickness <> ValidThickness then
  begin
    FFocusUnderlineThickness := ValidThickness;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CMemo.SetFocusUnderlineStyle(const Value: TPenStyle);
begin
  if FFocusUnderlineStyle <> Value then
  begin
    FFocusUnderlineStyle := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

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
// --- End of Setter Implementations ---

// --- Getter/Setter Implementations for Memo Properties ---
function TANDMR_CMemo.GetLines: TStrings;
begin
  Result := FInternalMemo.Lines;
end;

procedure TANDMR_CMemo.SetLines(const Value: TStrings);
begin
  FInternalMemo.Lines.Assign(Value);
  Invalidate;
end;

function TANDMR_CMemo.GetReadOnly: Boolean;
begin
  Result := FInternalMemo.ReadOnly;
end;

procedure TANDMR_CMemo.SetReadOnly(const Value: Boolean);
begin
  if FInternalMemo.ReadOnly <> Value then
  begin
    FInternalMemo.ReadOnly := Value;
    // Invalidate might not be strictly necessary if ReadOnly doesn't change appearance directly
    // but good practice if any visual cue for ReadOnly state is ever added.
    // Invalidate;
  end;
end;

function TANDMR_CMemo.GetWordWrap: Boolean;
begin
  Result := FInternalMemo.WordWrap;
end;

procedure TANDMR_CMemo.SetWordWrap(const Value: Boolean);
begin
  if FInternalMemo.WordWrap <> Value then
  begin
    FInternalMemo.WordWrap := Value;
    Invalidate; // WordWrap change affects layout and requires repaint
  end;
end;

function TANDMR_CMemo.GetScrollBars: TScrollStyle;
begin
  Result := FInternalMemo.ScrollBars;
end;

procedure TANDMR_CMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if FInternalMemo.ScrollBars <> Value then
  begin
    FInternalMemo.ScrollBars := Value;
    Invalidate; // Scrollbar changes can affect layout/appearance
  end;
end;

function TANDMR_CMemo.GetMaxLength: Integer;
begin
  Result := FInternalMemo.MaxLength;
end;

procedure TANDMR_CMemo.SetMaxLength(const Value: Integer);
begin
  if FInternalMemo.MaxLength <> Value then
  begin
    FInternalMemo.MaxLength := Value;
    // No Invalidate needed as MaxLength is a behavior, not visual
  end;
end;

procedure TANDMR_CMemo.SetFont(const Value: TFont);
begin
  inherited ;
  if Assigned(FInternalMemo) and Assigned(Self.Font) then
  begin
    FInternalMemo.Font.Assign(Self.Font);
  end;
  UpdateInternalMemoBounds; // Font changes can affect layout
  Invalidate;
end;
// --- End of Getter/Setter Implementations for Memo Properties ---

// --- CalculateLayout (still local, might be specific) and other local methods ---
procedure TANDMR_CMemo.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
var
  WorkArea: TRect;
  ImgW, ImgH, SepW: Integer;
  CurrentX, CurrentX_End: Integer;
  FullClientRect: TRect;
  CaptionHeight, CaptionWidth: Integer;
  OriginalFont: TFont;
begin
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
         var TempRect := Rect(0,0, FullClientRect.Width, 30000);
         DrawText(Self.Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRect, DT_CALCRECT or DT_WORDBREAK);
         CaptionHeight := TempRect.Bottom - TempRect.Top;
         CaptionWidth := FullClientRect.Width;
      end else if FCaptionSettings.WordWrap and (FCaptionSettings.Position in [cpLeft, cpRight]) then
      begin
         var TempRect := Rect(0,0, CaptionWidth, FullClientRect.Height);
         DrawText(Self.Canvas.Handle, PChar(FCaptionSettings.Text), Length(FCaptionSettings.Text), TempRect, DT_CALCRECT or DT_WORDBREAK);
         CaptionWidth := TempRect.Right - TempRect.Left;
         CaptionHeight := FullClientRect.Height;
      end;
    finally
      Self.Canvas.Font.Assign(OriginalFont);
      OriginalFont.Free;
    end;

    WorkArea := FullClientRect;
    case FCaptionSettings.Position of
      cpAbove: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Right, FullClientRect.Top + CaptionHeight); WorkArea.Top := FCaptionRect.Bottom + FCaptionSettings.Offset; end;
      cpBelow: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Bottom - CaptionHeight, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Bottom := FCaptionRect.Top - FCaptionSettings.Offset; end;
      cpLeft:  begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Left + CaptionWidth, FullClientRect.Bottom); WorkArea.Left := FCaptionRect.Right + FCaptionSettings.Offset; end;
      cpRight: begin FCaptionRect := Rect(FullClientRect.Right - CaptionWidth, FullClientRect.Top, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Right := FCaptionRect.Left - FCaptionSettings.Offset; end;
    end;
    if WorkArea.Bottom < WorkArea.Top then WorkArea.Bottom := WorkArea.Top;
    if WorkArea.Right < WorkArea.Left then WorkArea.Right := WorkArea.Left;
  end
  else
    WorkArea := FullClientRect;

  InflateRect(WorkArea, -FBorderThickness, -FBorderThickness);
  outImgRect := Rect(0,0,0,0); outSepRect := Rect(0,0,0,0); outTxtRect := WorkArea;
  ImgW := 0; ImgH := 0;
  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin ImgW := FImage.Graphic.Width; ImgH := FImage.Graphic.Height; end;
  SepW := 0;
  if FSeparatorVisible and (FSeparatorThickness > 0) then SepW := FSeparatorThickness;

  if FImageVisible and (ImgW > 0) then
  begin
    if FImagePosition = ipsLeft then
    begin
      outImgRect.Left := WorkArea.Left + FImageMargins.Left;
      outImgRect.Right := outImgRect.Left + ImgW;
      CurrentX := outImgRect.Right + FImageMargins.Right;
      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Left := CurrentX + FSeparatorPadding;
        outSepRect.Right := outSepRect.Left + SepW;
        CurrentX := outSepRect.Right + FSeparatorPadding;
      end;
      outTxtRect.Left := CurrentX;
    end
    else // ipsRight
    begin
      outImgRect.Right := WorkArea.Right - FImageMargins.Right;
      outImgRect.Left := outImgRect.Right - ImgW;
      CurrentX_End := outImgRect.Left - FImageMargins.Left;
      if FSeparatorVisible and (SepW > 0) then
      begin
        outSepRect.Right := CurrentX_End - FSeparatorPadding;
        outSepRect.Left := outSepRect.Right - SepW;
        CurrentX_End := outSepRect.Left - FSeparatorPadding;
      end;
      outTxtRect.Right := CurrentX_End;
    end;
  end
  else if FSeparatorVisible and (SepW > 0) then
  begin
    outSepRect.Left := WorkArea.Left + FSeparatorPadding; // Should be WorkArea.Left
    outSepRect.Right := outSepRect.Left + SepW;
    outTxtRect.Left := outSepRect.Right + FSeparatorPadding;
  end;

  if FImageVisible and (ImgW > 0) then
  begin
    var AvailHForImgLayout: Integer := WorkArea.Height - FImageMargins.Top - FImageMargins.Bottom; AvailHForImgLayout := Max(0, AvailHForImgLayout);
    case FImageAlignment of
      iavTop:    outImgRect.Top := WorkArea.Top + FImageMargins.Top;
      iavCenter: outImgRect.Top := WorkArea.Top + FImageMargins.Top + (AvailHForImgLayout - ImgH) div 2;
      iavBottom: outImgRect.Top := WorkArea.Bottom - FImageMargins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;
    if outImgRect.Top < WorkArea.Top + FImageMargins.Top then outImgRect.Top := WorkArea.Top + FImageMargins.Top;
    if outImgRect.Bottom > WorkArea.Bottom - FImageMargins.Bottom then outImgRect.Bottom := WorkArea.Bottom - FImageMargins.Bottom;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  end;
  outTxtRect.Top := WorkArea.Top; outTxtRect.Bottom := WorkArea.Bottom;
  if FSeparatorVisible and (SepW > 0) then
  begin
    var SepH: Integer; var RefTop, RefHeight: Integer; outSepRect.Top := WorkArea.Top; SepH := WorkArea.Height;
    case FSeparatorHeightMode of
      shmFull: ;
      shmAsText: begin RefTop := outTxtRect.Top; RefHeight := outTxtRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmAsImage: if FImageVisible and (ImgW > 0) and (outImgRect.Height > 0) then begin RefTop := outImgRect.Top; RefHeight := outImgRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmCustom: begin if FSeparatorCustomHeight > 0 then SepH := FSeparatorCustomHeight else SepH := WorkArea.Height; outSepRect.Top := WorkArea.Top + (WorkArea.Height - SepH) div 2; end;
    end;
    outSepRect.Bottom := outSepRect.Top + SepH;
    if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top;
    if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
    if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
  end;

  // Final safety checks, ensure no rect has negative width/height
  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left; if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
  if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left; if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left; if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
end;

// Implementations of DrawEditBox, DrawPNGImageWithGDI, DrawNonPNGImageWithCanvas, DrawSeparatorWithCanvas were removed.
// They are now centralized in ANDMR_ComponentUtils.

procedure TANDMR_CMemo.UpdateInternalMemoBounds;
var
  LImgRect, LTxtRect, LSepRect: TRect;
  MemoRect: TRect;
begin
  // Calculate the basic layout of image, text area, separator, caption
  CalculateLayout(LImgRect, LTxtRect, LSepRect);

  // The text area for FInternalMemo is LTxtRect, adjusted by FTextMargins
  MemoRect.Left   := LTxtRect.Left + FTextMargins.Left;
  MemoRect.Top    := LTxtRect.Top + FTextMargins.Top;
  MemoRect.Right  := LTxtRect.Right - FTextMargins.Right;
  MemoRect.Bottom := LTxtRect.Bottom - FTextMargins.Bottom;

  // Ensure rect is not inverted
  if MemoRect.Right < MemoRect.Left then MemoRect.Right := MemoRect.Left;
  if MemoRect.Bottom < MemoRect.Top then MemoRect.Bottom := MemoRect.Top;

  if Assigned(FInternalMemo) then
  begin
    // Avoid recursive updates if BoundsRect itself triggers a resize/paint
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
// --- End of Copied Drawing Helper Implementations ---

// --- Control Message Handler Implementations ---
procedure TANDMR_CMemo.CMEnter(var Message: TCMEnter);
begin
  inherited; // Let TCustomControl handle its part (e.g., setting Self.Focused)
  // FHovered might need to be false if focus is gained without mouse hover - CMMouseLeave should handle FHovered.

  // Update visual appearance for focus on the TANDMR_CMemo frame
  if FFocusBorderColorVisible or FFocusBackgroundColorVisible or FFocusUnderlineVisible then
    Invalidate;

  // Transfer focus to the internal memo
  if Assigned(FInternalMemo) and FInternalMemo.CanFocus then
    FInternalMemo.SetFocus;

  // Fire the component's OnEnter event
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TANDMR_CMemo.CMExit(var Message: TCMExit);
begin
  // Update visual appearance for focus loss on the TANDMR_CMemo frame
  if FFocusBorderColorVisible or FFocusBackgroundColorVisible or FFocusUnderlineVisible then
    Invalidate;

  inherited; // Let TCustomControl handle its part (e.g., clearing Self.Focused)

  // Fire the component's OnExit event
  // Note: FInternalMemo.OnExit would have already fired if focus is moving from FInternalMemo out of the component.
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TANDMR_CMemo.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
    // if FHoverSettings.Enabled then Invalidate; // Old logic removed
  end;
  FHoverSettings.StartAnimation(True);
end;

procedure TANDMR_CMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
    // if FHoverSettings.Enabled then Invalidate; // Old logic removed
  end;
  FHoverSettings.StartAnimation(False);
end;

procedure TANDMR_CMemo.SetTabStop(Value: Boolean);
begin
  inherited ; // Call ancestor's virtual method
  if Assigned(FInternalMemo) then
    FInternalMemo.TabStop := Self.TabStop; // Synchronize with the property's actual new state
end;

procedure TANDMR_CMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if CanFocus and (not Focused) and Assigned(FInternalMemo) then // If TANDMR_CMemo itself can get focus
    begin
       // SetFocus will trigger CMEnter, which then sets focus to FInternalMemo
       Self.SetFocus;
    end
    else if Focused and Assigned(FInternalMemo) and (FInternalMemo.Handle <> GetFocus) then
    begin
       // If TANDMR_CMemo is focused, but FInternalMemo is not (e.g. clicked on frame), set focus to FInternalMemo
       FInternalMemo.SetFocus;
    end;
    // If click is within FInternalMemo's bounds, it will handle its own focus and caret.
    // No need to explicitly position caret here as TMemo does that.
  end;
end;
// --- End of Control Message Handler Implementations ---

constructor TANDMR_CMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable, csSetCaption];
  DoubleBuffered := True; // From CEdit
  Width := 185; // Default TMemo width (can be adjusted)
  Height := 80; // Default TMemo height (can be adjusted)

  // Initialize Appearance Fields (copied from TANDMR_CEdit.Create)
  FCornerRadius := 8;
  FRoundCornerType := rctAll;
  FActiveColor := clHighlight; // Or a color more suitable for Memo focus like clActiveCaption
  FInactiveColor := clBtnFace; // Or clWindow for a more traditional Memo look
  FBorderColor := clBlack;
  FBorderThickness := 1;
  FBorderStyle := psSolid;

  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageMargins := TANDMR_Margins.Create;
  FImageMargins.OnChange := ImageMarginsChanged;
  FImageVisible := True; // Default to true, can be changed by user
  FImagePosition := ipsLeft;
  FImageAlignment := iavCenter;
  FImagePlacement := iplInsideBounds; // Or iplOutsideBounds depending on desired default
  FImageDrawMode := idmProportional;

  FSeparatorVisible := False;
  FSeparatorColor := clGrayText;
  FSeparatorThickness := 1;
  FSeparatorPadding := 2;
  FSeparatorHeightMode := shmFull;
  FSeparatorCustomHeight := 0;

  FCaptionSettings := TCaptionSettings.Create(Self); // Pass Self (TWinControl) as owner
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FCaptionRect := Rect(0,0,0,0);

  FHoverSettings := THoverSettings.Create(Self); // Pass Self as OwnerControl
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHovered := False;

  FTextMargins := TANDMR_Margins.Create;
  FTextMargins.OnChange := TextMarginsChanged;

  // Initialize Focus Properties
  FFocusBorderColorVisible := False;
  FFocusBorderColor := clBlack;
  FFocusBackgroundColorVisible := False;
  FFocusBackgroundColor := clWindow; // Typical for text fields
  FFocusUnderlineVisible := False;
  FFocusUnderlineColor := clBlack;
  FFocusUnderlineThickness := 1;
  FFocusUnderlineStyle := psSolid;

  FOpacity := 255;

  // Font: TANDMR_CMemo will use its own Font property, which will be assigned to an internal TMemo.
  // For now, just ensure a default for the control itself if it were to draw text directly (though it won't primarily).
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  Font.Color := clWindowText;

  // Initialize Internal Memo
  FInternalMemo := TMemo.Create(Self);
  FInternalMemo.Parent := Self; // Important: Makes this control the VCL parent
  FInternalMemo.Align := alNone; // Will be controlled by custom layout logic
  FInternalMemo.BorderStyle := bsNone; // No border for the internal memo
  FInternalMemo.TabStop := True; // Allow tabbing to the internal memo
  FInternalMemo.WordWrap := True; // Default, can be overridden by property
  FInternalMemo.ScrollBars := ssVertical; // Default, can be overridden
  if Assigned(Self.Font) then // Apply the component's font to the internal memo
     FInternalMemo.Font.Assign(Self.Font);
  FInternalMemo.Color := clWindow; // Default background, will be updated by Paint logic
  // FInternalMemo.Visible := True; // Ensure it's visible; default is true

  // Assign Internal Memo Event Handlers
  FInternalMemo.OnChange := InternalMemoChange;
  FInternalMemo.OnEnter := InternalMemoEnter;
  FInternalMemo.OnExit := InternalMemoExit;
  FInternalMemo.OnKeyDown := InternalMemoKeyDown;
  FInternalMemo.OnKeyPress := InternalMemoKeyPress;
  FInternalMemo.OnKeyUp := InternalMemoKeyUp;

  // UpdateInternalMemoBounds; // Set initial bounds for FInternalMemo - Moved to Loaded
end;

destructor TANDMR_CMemo.Destroy;
begin
  if Assigned(FImage) then // Check before calling OnChange = nil
  begin
    FImage.OnChange := nil;
    FImage.Free;
  end;
  FImageMargins.Free;
  FCaptionSettings.Free;
  FHoverSettings.Free;
  FTextMargins.Free;
  FInternalMemo.Free; // Free the internal memo
  inherited Destroy;
end;

procedure TANDMR_CMemo.Paint;
var
  LG: TGPGraphics;
  // TextToDisplay: string; // Removed, FInternalMemo handles its own text
  // TextFlags: Cardinal; // Removed
  imgR, txtR, sepR: TRect;
  RectToDrawEditBoxIn: TRect;
  // PaddedTextDrawArea: TRect; // Removed, FInternalMemo handles its own padding via client rect
  FullClientRect: TRect;
  ActualEditBGColor, ActualEditBorderColor, ActualEditTextColor, ActualCaptionTextColor: TColor;
  EditBoxDrawingRect: TRect;
  BGForDrawEditBox: TColor;
begin
  UpdateInternalMemoBounds; // Ensure internal memo bounds are correct first

  FullClientRect := Self.ClientRect;
  // CalculateLayout is now called by UpdateInternalMemoBounds,
  // but we need its results (imgR, txtR, sepR) for drawing the frame.
  // FCaptionRect is also set within CalculateLayout.
  CalculateLayout(imgR, txtR, sepR);


  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      var LHoverProgress: Single := FHoverSettings.CurrentAnimationValue / 255.0;
      var IsComponentFocused: Boolean := Self.Focused or (Assigned(FInternalMemo) and FInternalMemo.Focused);

      // --- Define True Base Colors (Enabled, Not Focused, Not Hovered) ---
      var TrueBaseFrameBG, TrueBaseBorderCol, TrueBaseTextCol, TrueBaseCaptionCol: TColor;
      TrueBaseFrameBG := IfThen(FImagePlacement = iplInsideBounds, FInactiveColor, Self.Color);
      TrueBaseBorderCol := FBorderColor;
      TrueBaseTextCol := Self.Font.Color; // Font color for the internal memo
      TrueBaseCaptionCol := IfThen(FCaptionSettings.Color = clDefault, Self.Font.Color, FCaptionSettings.Color);

      // --- Define Target Hover Colors (from FHoverSettings) ---
      var TargetHoverFrameBG, TargetHoverBorderCol, TargetHoverTextCol, TargetHoverCaptionCol: TColor;
      TargetHoverFrameBG := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
      TargetHoverBorderCol := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
      TargetHoverTextCol := IfThen(FHoverSettings.Enabled, FHoverSettings.FontColor, clNone);
      TargetHoverCaptionCol := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);

      // --- Define Focus Colors ---
      var FocusFrameBGToUse, FocusBorderColToUse, FocusMemoBGToUse, FocusTextColToUse, FocusCaptionColToUse: TColor;
      FocusFrameBGToUse := TargetHoverFrameBG; // Frame BG often follows hover when focused, or could be specific
      FocusBorderColToUse := IfThen(FFocusBorderColorVisible, FFocusBorderColor, FActiveColor);
      FocusMemoBGToUse := IfThen(FFocusBackgroundColorVisible, FFocusBackgroundColor, clNone); // Specific for memo area
      FocusTextColToUse := TrueBaseTextCol; // No separate focus text color for memo
      FocusCaptionColToUse := TrueBaseCaptionCol; // No separate focus caption color

      // --- Define Disabled Colors ---
      var DisabledFrameBGForResolve, DisabledBorderColForResolve, DisabledMemoBGForResolve, DisabledTextColForResolve, DisabledCaptionColForResolve: TColor;
      DisabledFrameBGForResolve := TrueBaseFrameBG;
      DisabledBorderColForResolve := TrueBaseBorderCol;
      DisabledMemoBGForResolve := clWindow; // Or clBtnFace
      DisabledTextColForResolve := IfThen(TrueBaseTextCol = clWindowText, clGrayText, DarkerColor(TrueBaseTextCol, 50));
      DisabledCaptionColForResolve := IfThen(TrueBaseCaptionCol = clWindowText, clGrayText, DarkerColor(TrueBaseCaptionCol,50));

      // --- Calculate Non-Hovered State Colors (considering focus) ---
      var NonHoveredFrameBG, NonHoveredBorderCol, NonHoveredMemoBG, NonHoveredTextCol, NonHoveredCaptionCol: TColor;
      NonHoveredFrameBG := ResolveStateColor(Self.Enabled, FALSE, IsComponentFocused, TrueBaseFrameBG, clNone, FocusFrameBGToUse, DisabledFrameBGForResolve, FHoverSettings.Enabled, True, True);
      NonHoveredBorderCol := ResolveStateColor(Self.Enabled, FALSE, IsComponentFocused, TrueBaseBorderCol, clNone, FocusBorderColToUse, DisabledBorderColForResolve, FHoverSettings.Enabled, True, False);
      NonHoveredMemoBG := ResolveStateColor(Self.Enabled, FALSE, IsComponentFocused, clWindow, clNone, FocusMemoBGToUse, DisabledMemoBGForResolve, False, FFocusBackgroundColorVisible, False);
      NonHoveredTextCol := ResolveStateColor(Self.Enabled, FALSE, IsComponentFocused, TrueBaseTextCol, clNone, FocusTextColToUse, DisabledTextColForResolve, False, False, False);
      NonHoveredCaptionCol := ResolveStateColor(Self.Enabled, FALSE, IsComponentFocused, TrueBaseCaptionCol, clNone, FocusCaptionColToUse, DisabledCaptionColForResolve, False, False, False);

      // --- Calculate Target State Colors (considering current FHovered, focus) ---
      var TargetStateFrameBG, TargetStateBorderCol, TargetStateMemoBG, TargetStateTextCol, TargetStateCaptionCol: TColor;
      TargetStateFrameBG := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, TrueBaseFrameBG, TargetHoverFrameBG, FocusFrameBGToUse, DisabledFrameBGForResolve, FHoverSettings.Enabled, True, True);
      TargetStateBorderCol := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, TrueBaseBorderCol, TargetHoverBorderCol, FocusBorderColToUse, DisabledBorderColForResolve, FHoverSettings.Enabled, True, False);
      TargetStateMemoBG := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, clWindow, TargetHoverTextCol, FocusMemoBGToUse, DisabledMemoBGForResolve, FHoverSettings.Enabled, FFocusBackgroundColorVisible, False); // Note: TargetHoverTextCol used for MemoBG hover to see if it makes sense, or should be TargetHoverFrameBG
      TargetStateTextCol := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, TrueBaseTextCol, TargetHoverTextCol, FocusTextColToUse, DisabledTextColForResolve, FHoverSettings.Enabled, False, False);
      TargetStateCaptionCol := ResolveStateColor(Self.Enabled, FHovered, IsComponentFocused, TrueBaseCaptionCol, TargetHoverCaptionCol, FocusCaptionColToUse, DisabledCaptionColForResolve, FHoverSettings.Enabled, False, False);

      // --- Blend colors if animation is active towards a hovered state ---
      if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FHovered then
      begin
        BGForDrawEditBox      := BlendColors(NonHoveredFrameBG, TargetStateFrameBG, LHoverProgress);
        ActualEditBorderColor := BlendColors(NonHoveredBorderCol, TargetStateBorderColor, LHoverProgress);
        ActualEditBGColor     := BlendColors(NonHoveredMemoBG, TargetStateMemoBG, LHoverProgress); // For InternalMemo.Color
        ActualEditTextColor   := BlendColors(NonHoveredTextCol, TargetStateTextCol, LHoverProgress); // For InternalMemo.Font.Color
        ActualCaptionTextColor:= BlendColors(NonHoveredCaptionCol, TargetStateCaptionCol, LHoverProgress);
      end
      else // No animation towards hover (or reversing/settled)
      begin
        BGForDrawEditBox      := TargetStateFrameBG;
        ActualEditBorderColor := TargetStateBorderColor;
        ActualEditBGColor     := TargetStateMemoBG;    // For InternalMemo.Color
        ActualEditTextColor   := TargetStateTextCol;  // For InternalMemo.Font.Color
        ActualCaptionTextColor:= TargetStateCaptionCol;
      end;

      // --- Painting Frame and Background ---
      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color; // Main component background
        Canvas.FillRect(FullClientRect);
      end;

      if FImagePlacement = iplInsideBounds then
      begin
        EditBoxDrawingRect := FullClientRect;
        if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
        begin
            case FCaptionSettings.Position of
              cpAbove: EditBoxDrawingRect.Top := FCaptionRect.Bottom + FCaptionSettings.Offset;
              cpBelow: EditBoxDrawingRect.Bottom := FCaptionRect.Top - FCaptionSettings.Offset;
              cpLeft:  EditBoxDrawingRect.Left := FCaptionRect.Right + FCaptionSettings.Offset;
              cpRight: EditBoxDrawingRect.Right := FCaptionRect.Left - FCaptionSettings.Offset;
            end;
        end;
        // BGForDrawEditBox was already set based on FInactiveColor or Self.Color
      end
      else // iplOutsideBounds
      begin
        // The "edit box" visual that receives BGForDrawEditBox is effectively the txtR area.
        // The area around it is Self.Color.
        EditBoxDrawingRect := txtR; // txtR is from CalculateLayout
        // BGForDrawEditBox was already set to Self.Color for the frame,
        // but the internal memo will use ActualEditBGColor.
        // For clarity, DrawEditBox here will draw the border around txtR.
        // The background for the memo itself is handled by FInternalMemo.Color.
      end;
      RectToDrawEditBoxIn := EditBoxDrawingRect;


      // Update FInternalMemo's Color and Font Color
      if Assigned(FInternalMemo) then
      begin
        if FInternalMemo.Color <> ActualEditBGColor then
           FInternalMemo.Color := ActualEditBGColor;
        if FInternalMemo.Font.Color <> ActualEditTextColor then
           FInternalMemo.Font.Color := ActualEditTextColor;
      end;

      // Draw the main component frame/border (not the internal memo's border)
      // The background color passed here is for the area *behind* the memo if image is iplInsideBounds
      DrawEditBox(LG, EditBoxDrawingRect, BGForDrawEditBox, ActualEditBorderColor, FBorderThickness, FBorderStyle, FCornerRadius, FRoundCornerType, FOpacity);


      if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
      begin
        if (FImage.Graphic is TPNGImage) then
          DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode) // Call to global
        else
          DrawNonPNGImageWithCanvas(Canvas, FImage.Graphic, imgR, FImageDrawMode); // Call to global
      end;

      if FSeparatorVisible and (FSeparatorThickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
        DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorColor, FSeparatorThickness); // Call to global

      if Self.Focused or (Assigned(FInternalMemo) and FInternalMemo.Focused) then
      begin
        if FFocusUnderlineVisible and (FFocusUnderlineThickness > 0) then
        begin
          var UnderlineY: Integer;
          var UnderlinePen: TGPPen;
          if FBorderThickness > 0 then UnderlineY := EditBoxDrawingRect.Bottom - FBorderThickness - (FFocusUnderlineThickness div 2)
          else UnderlineY := EditBoxDrawingRect.Bottom - (FFocusUnderlineThickness div 2);
          UnderlineY := Min(UnderlineY, EditBoxDrawingRect.Bottom - FFocusUnderlineThickness);
          UnderlinePen := TGPPen.Create(ColorToARGB(FFocusUnderlineColor, Self.FOpacity), FFocusUnderlineThickness);
          try
            case FFocusUnderlineStyle of psSolid: UnderlinePen.SetDashStyle(DashStyleSolid); psDash: UnderlinePen.SetDashStyle(DashStyleDash); psDot: UnderlinePen.SetDashStyle(DashStyleDot); psDashDot: UnderlinePen.SetDashStyle(DashStyleDashDot); psDashDotDot: UnderlinePen.SetDashStyle(DashStyleDashDotDot); else UnderlinePen.SetDashStyle(DashStyleSolid); end;
            LG.DrawLine(UnderlinePen, EditBoxDrawingRect.Left + FBorderThickness, UnderlineY, EditBoxDrawingRect.Right - FBorderThickness, UnderlineY);
          finally UnderlinePen.Free; end;
        end;
      end;

    finally
      LG.Free;
    end; // End of GDI+ operations for frame

    // TEXT AND CARET ARE DRAWN BY FInternalMemo ITSELF.

    // --- Draw Caption --- (Drawn on top of everything else by this component)
    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionRect.Width > 0) and (FCaptionRect.Height > 0) then
    begin
      var VAlign: TCaptionVerticalAlignment;
      if FCaptionSettings.Position in [cpLeft, cpRight] then
        VAlign := cvaCenter
      else // cpAbove, cpBelow
        VAlign := cvaTop;

      DrawComponentCaption(
        Self.Canvas,
        FCaptionRect,
        FCaptionSettings.Text,
        FCaptionSettings.Font,
        ActualCaptionTextColor,
        FCaptionSettings.Alignment,
        VAlign,
        FCaptionSettings.WordWrap,
        FOpacity
      );
    end;

  finally
    Canvas.Unlock;
  end;
end;

procedure TANDMR_CMemo.Loaded;
begin
  inherited Loaded;
  UpdateInternalMemoBounds;
  // Ensure the internal memo's visibility is synchronized with the component's visibility
  // This is important because the internal memo might have been created but not shown
  // if the component was initially not visible.
  if Assigned(FInternalMemo) then
  begin
    FInternalMemo.Visible := Self.Visible;
  end;
end;

end.
