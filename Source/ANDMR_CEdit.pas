unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Math,
  Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Forms, Vcl.Themes, Vcl.Mask, // Added Vcl.Mask
  Winapi.Windows, Winapi.Messages,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Winapi.ActiveX, Vcl.ExtCtrls; // For TStreamAdapter if image features are kept

type
  // Re-used from TANDMR_CButton or similar
  TEdgeMargins = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FLeft, FTop, FRight, FBottom: Integer;
    procedure SetValue(Index: Integer; const Value: Integer);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer index 0 read FLeft write SetValue default 5;
    property Top: Integer index 1 read FTop write SetValue default 5;
    property Right: Integer index 2 read FRight write SetValue default 5;
    property Bottom: Integer index 3 read FBottom write SetValue default 5;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // --- New Enums for TANDMR_CEdit ---
  TCEditInputType = (
    itNormal,          // Any character
    itNumbersOnly,     // Only 0-9
    itInteger,         // Numbers, optional leading '-'
    itFloat,           // Numbers, optional leading '-', one decimal separator
    itLettersOnly,     // Only a-z, A-Z
    itAlphaNumeric,    // Letters and numbers
    itNoSpecialChars,  // Letters, numbers, space
    itEmailAddress,    // Basic email format validation
    itCustomRegex      // Validation via custom regex (advanced)
  );

  TCEditCharacterCasing = (
    ccNormal,
    ccUpperCase,
    ccLowerCase
  );

  TCEditFocusStyle = (
    fsBorder,          // Highlight the entire border
    fsBottomLine,      // Highlight only the bottom border or a line underneath
    fsBackgroundColor, // Change background color
    fsUnderlineText    // Underline the text area
  );

  // Basic mask types, can be expanded
  TCEditMaskType = (
    mtNone,
    mtPhoneBR,         // (XX)XXXXX-XXXX or (XX)XXXX-XXXX
    mtCPF,             // XXX.XXX.XXX-XX
    mtCNPJ,            // XX.XXX.XXX/XXXX-XX
    mtCEP,             // XXXXX-XXX
    mtDateDMY,         // DD/MM/YYYY
    mtTimeHHMM,        // HH:MM
    mtCustom           // User-defined mask in CustomMask property
  );

  TCEditIconPosition = (ipNone, ipLeft, ipRight); // For optional icon

  // --- TANDMR_CEdit Component ---
  TANDMR_CEdit = class(TCustomControl)
  private
    // --- Core Text Properties ---
    FText: string;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FMaxLength: Integer; // 0 for no limit
    FTextAlign: TAlignment; // Horizontal alignment of text
    FPlaceHolder: string;
    FPlaceHolderFontColor: TColor;
    FInputType: TCEditInputType;
    FCharacterCasing: TCEditCharacterCasing;
    FCustomValidationRegex: string; // For itCustomRegex

    // --- Masking ---
    FMaskType: TCEditMaskType;
    FCustomMask: string;      // Raw mask string, e.g., '!(99)9999-9999;1;_'
//    FEditMask: TEditMask;     // Internal VCL mask object for processing
    FMaskedText: string;      // Text with mask literals
    FUseMaskBehavior: Boolean; // If true, strictly adheres to mask during input

    // --- Visual Properties (adapted from Button) ---
    FCornerRadius: Integer;
    FBackgroundColor: TColor;       // Normal background
    FDisabledColor: TColor;         // Background when disabled
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FBorderStyle: TPenStyle;        // psSolid, psDash etc.
    FTextFont: TFont;
    FTransparent: Boolean;

    // --- Focus Properties ---
    FIsFocused: Boolean;
    FFocusColor: TColor;            // Color used for focus indication (border, line, bg)
    FFocusStyle: TCEditFocusStyle;
    FFocusBorderThickness: Integer; // Can be different from normal border

    // --- Icon Properties (Optional, adapted from Button's Image) ---
    FIcon: TPicture;
    FIconPosition: TCEditIconPosition;
    FIconMargins: TEdgeMargins;
    FIconWidth: Integer; // Desired width for the icon area
    FOnClickIcon: TNotifyEvent;

    // --- Internal State ---
    FCursorPos: Integer;          // Caret position in unmasked text
    FCursorVisible: Boolean;
    FCursorTimer: TTimer;
    FTextOffset: Integer;         // Horizontal scroll offset in pixels
    FSelectionStart: Integer;     // Start of selection in unmasked text
    FSelectionLength: Integer;    // Length of selection
    FSelecting: Boolean;          // True when mouse is dragging to select
    FMouseDownPos: TPoint;        // Position of mouse down for selection logic
    FInternalUpdate: Boolean;     // To prevent recursive updates

    // --- Property Setters/Getters ---
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);
    procedure SetTextAlign(const Value: TAlignment);
    procedure SetPlaceHolder(const Value: string);
    procedure SetPlaceHolderFontColor(const Value: TColor);
    procedure SetInputType(const Value: TCEditInputType);
    procedure SetCharacterCasing(const Value: TCEditCharacterCasing);
    procedure SetCustomValidationRegex(const Value: string);
    procedure SetMaskType(const Value: TCEditMaskType);
    procedure SetCustomMask(const Value: string);
    procedure SetUseMaskBehavior(const Value: Boolean);

    procedure SetCornerRadius(const Value: Integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Integer);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetTextFont(const Value: TFont);
    procedure TextFontChanged(Sender: TObject);
    procedure SetTransparent(const Value: Boolean);

    procedure SetFocusColor(const Value: TColor);
//    procedure SetFocusStyle(const Value: TCEditFocusStyle);
//    procedure SetFocusBorderThickness(const Value: Integer);

    procedure SetIcon(const Value: TPicture);
    procedure SetIconPosition(const Value: TCEditIconPosition);
    procedure SetIconMargins(const Value: TEdgeMargins);
    procedure SetIconWidth(const Value: Integer);

    // --- Internal Methods ---
    procedure UpdateControlStyle;
    procedure CursorTimerHandler(Sender: TObject);
    procedure ShowCaret;
    procedure HideCaret;
    procedure ResetCaretBlink;
    function GetDisplayRect: TRect; // Area for text drawing
    function GetTextRect: TRect;    // Area within display rect considering text margins
    function GetIconRect: TRect;    // Area for icon if present
    function CharPosToX(CharIndex: Integer): Integer;
    function XToCharPos(X: Integer): Integer;
    procedure AdjustTextOffset;
    procedure ApplyCharacterCasing(var Ch: Char); overload;
    function ApplyCharacterCasing(const S: string): string; overload;
    function IsValidChar(Ch: Char; CurrentFullText: string): Boolean;
    function IsValidText(const AText: string): Boolean;
    procedure InsertCharacter(Ch: Char);
    procedure DeleteSelection;
    procedure HandleBackspace;
    procedure HandleDelete;
    procedure SelectAll;
    procedure ClearSelection;
    procedure UpdateMaskedText;
    function GetUnmaskedText: string;
    procedure ApplyMaskToText(const InputText: string; UpdateCursor: Boolean = True);
    procedure InitializeEditMask;
    function GetEffectiveMaxLength: Integer;

    // --- Message Handlers & Overrides ---
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED; // VCL standard
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE; // Handle paste
    procedure WMChar(var Message: TWMChar); message WM_CHAR;   // For KeyPress like behavior

  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    // procedure KeyPress(var Key: Char); override; // WMChar is often preferred for full control
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

//    property EditMask: TEditMask read FEditMask; // Access to internal mask object
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    property Text: string read GetText write SetText;
  published
    // --- Core Text Properties ---
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0; // 0 = no limit
    property TextAlign: TAlignment read FTextAlign write SetTextAlign default taLeftJustify;
    property PlaceHolder: string read FPlaceHolder write SetPlaceHolder;
    property PlaceHolderFontColor: TColor read FPlaceHolderFontColor write SetPlaceHolderFontColor default clGrayText;
    property InputType: TCEditInputType read FInputType write SetInputType default itNormal;
    property CharacterCasing: TCEditCharacterCasing read FCharacterCasing write SetCharacterCasing default ccNormal;
    property CustomValidationRegex: string read FCustomValidationRegex write SetCustomValidationRegex;

    // --- Masking ---
    property MaskType: TCEditMaskType read FMaskType write SetMaskType default mtNone;
    property CustomMask: string read FCustomMask write SetCustomMask; // Example: '!(99)0000-0000;1;_'
    property UseMaskBehavior: Boolean read FUseMaskBehavior write SetUseMaskBehavior default True;


    // --- Visual Properties ---
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 8;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWindow;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderThickness: Integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property TextFont: TFont read FTextFont write SetTextFont;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    // --- Focus Properties ---
    property FocusColor: TColor read FFocusColor write SetFocusColor default clHighlight;
//    property FocusStyle: TCEditFocusStyle read FFocusStyle write SetFocusStyle default fsBorder;
//    property FocusBorderThickness: Integer read FFocusBorderThickness write SetFocusBorderThickness default 2;

    // --- Icon Properties ---
    property Icon: TPicture read FIcon write SetIcon;
    property IconPosition: TCEditIconPosition read FIconPosition write SetIconPosition default ipNone;
    property IconMargins: TEdgeMargins read FIconMargins write SetIconMargins;
    property IconWidth: Integer read FIconWidth write SetIconWidth default 24;
    property OnClickIcon: TNotifyEvent read FOnClickIcon write FOnClickIcon;

    // --- Standard VCL Properties ---
    property Align;
    property Anchors;
    property Constraints;
    property Cursor default crIBeam; // Default to IBeam for text input
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font; // Inherited, but TextFont is primary for displayed text
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    // --- Events ---
//    property OnChange; // Standard event when text changes
    property OnClick;
    property OnDblClick;
    property OnEnter;  // Standard focus enter
    property OnExit;   // Standard focus exit
    property OnKeyDown;
    property OnKeyPress; // Standard KeyPress
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Custom events can be added, e.g. OnValidate, OnMaskError
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CEdit]);
end;

// Helper function to darken a color (re-use or ensure it's available)
function DarkerColorCE(Color: TColor; Percent: Byte = 20): TColor;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  Result := RGB(
    Max(0, Round(GetRValue(Color) * (100 - Percent) / 100)),
    Max(0, Round(GetGValue(Color) * (100 - Percent) / 100)),
    Max(0, Round(GetBValue(Color) * (100 - Percent) / 100))
  );
end;

// Helper function to lighten a color (re-use or ensure it's available)
function LighterColorCE(Color: TColor; Percent: Byte = 20): TColor;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  Result := RGB(
    Min(255, Round(GetRValue(Color) + (255 - GetRValue(Color)) * Percent / 100)),
    Min(255, Round(GetGValue(Color) + (255 - GetGValue(Color)) * Percent / 100)),
    Min(255, Round(GetBValue(Color) + (255 - GetBValue(Color)) * Percent / 100))
  );
end;

// Helper function to convert TColor to ARGB Cardinal (re-use or ensure it's available)
function ColorToARGB_CE(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then
  begin
    Result := (Alpha shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor);
  Result := (Alpha shl 24) or
            ((ColorRef and $000000FF) shl 16) or
            (ColorRef and $0000FF00) or
            ((ColorRef and $00FF0000) shr 16);
end;

{ TEdgeMargins }

constructor TEdgeMargins.Create;
begin
  inherited Create;
  FLeft := 5; // Default margins for edit-like control
  FTop := 5;
  FRight := 5;
  FBottom := 5;
end;

procedure TEdgeMargins.Assign(Source: TPersistent);
begin
  if Source is TEdgeMargins then
  begin
    FLeft := TEdgeMargins(Source).FLeft;
    FTop := TEdgeMargins(Source).FTop;
    FRight := TEdgeMargins(Source).FRight;
    FBottom := TEdgeMargins(Source).FBottom;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TEdgeMargins.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEdgeMargins.SetValue(Index: Integer; const Value: Integer);
begin
  case Index of
    0: if FLeft <> Value then FLeft := Value;
    1: if FTop <> Value then FTop := Value;
    2: if FRight <> Value then FRight := Value;
    3: if FBottom <> Value then FBottom := Value;
  else
    Exit;
  end;
  DoChange;
end;

{ TANDMR_CEdit }

constructor TANDMR_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse,
                                  csDoubleClicks, csReplicatable, csSetCaption,
                                  csNeedsBorderPaint]; // csNeedsBorderPaint for custom border
  Width := 150;
  Height := 25; // Typical TEdit height
  TabStop := True;
  Cursor := crIBeam;

  FTransparent := False;
  FCornerRadius := 8;
  FBackgroundColor := clWindow;
  FDisabledColor := clBtnFace;
  FBorderColor := clGray;
  FBorderThickness := 1;
  FBorderStyle := psSolid;
  FFocusColor := clHighlight;
  FFocusStyle := fsBorder;
  FFocusBorderThickness := 2;

  FTextFont := TFont.Create;
  FTextFont.Name := 'Segoe UI';
  FTextFont.Size := 9;
  FTextFont.Color := clWindowText;
  FTextFont.OnChange := TextFontChanged;

  FText := '';
  FPasswordChar := #0;
  FReadOnly := False;
  FMaxLength := 0;
  FTextAlign := taLeftJustify;
  FPlaceHolder := '';
  FPlaceHolderFontColor := clGrayText;
  FInputType := itNormal;
  FCharacterCasing := ccNormal;
  FCustomValidationRegex := '';

  FMaskType := mtNone;
  FCustomMask := '';
  FUseMaskBehavior := True;
//  FEditMask := TEditMask.Create(''); // Create with empty mask initially

  FIcon := TPicture.Create;
  FIconPosition := ipNone;
  FIconMargins := TEdgeMargins.Create;
//  FIconMargins.OnChange := Repaint; // Assuming MarginsChanged calls Repaint
  FIconWidth := 24;

  FCursorPos := 0;
  FCursorVisible := False;
  FCursorTimer := TTimer.Create(Self);
  FCursorTimer.Interval := GetCaretBlinkTime; // System caret blink time
  FCursorTimer.OnTimer := CursorTimerHandler;
  FTextOffset := 0;
  FSelectionStart := 0;
  FSelectionLength := 0;
  FSelecting := False;
  FInternalUpdate := False;

  DoubleBuffered := True;
  UpdateControlStyle;
end;

destructor TANDMR_CEdit.Destroy;
begin
  FCursorTimer.Free;
  FTextFont.Free;
  FIcon.Free;
  FIconMargins.Free;
//  FEditMask.Free;
  inherited;
end;

procedure TANDMR_CEdit.Loaded;
begin
  inherited Loaded;
  UpdateControlStyle; // Ensure style is correct after loading
  InitializeEditMask; // Initialize based on loaded mask properties
  ApplyMaskToText(FText, False); // Apply initial mask to any loaded text
  Repaint;
end;

procedure TANDMR_CEdit.UpdateControlStyle;
begin
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  // csAcceptsControls might be needed if we embed other controls, not for now.
end;

procedure TANDMR_CEdit.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    UpdateControlStyle;
    Invalidate; // Use Invalidate for transparency changes
  end;
end;

procedure TANDMR_CEdit.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
  // TextFontChanged will call Repaint
end;

procedure TANDMR_CEdit.TextFontChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TANDMR_CEdit.SetText(const Value: string);
var
  NewText: string;
  OldCursorPos: Integer;
begin
  if FInternalUpdate then Exit;
  if FText = Value then Exit; // No change

  FInternalUpdate := True;
  try
    NewText := Value;
    OldCursorPos := FCursorPos;

    // Apply character casing first
    if FCharacterCasing <> ccNormal then
      NewText := ApplyCharacterCasing(NewText);

    // Apply MaxLength if not using mask (mask handles its own length)
    if (FMaskType = mtNone) and (FMaxLength > 0) and (Length(NewText) > FMaxLength) then
      NewText := Copy(NewText, 1, FMaxLength);

    // If using mask, validate and format
    if FMaskType <> mtNone then
    begin
      ApplyMaskToText(NewText); // This updates FText and FCursorPos
    end
    else
    begin
      // Validate non-masked text
      if IsValidText(NewText) then
      begin
        FText := NewText;
        // Adjust cursor position if text length changed
        if OldCursorPos > Length(FText) then
          FCursorPos := Length(FText)
        else
          FCursorPos := OldCursorPos; // Or try to maintain relative position
      end
      else
      begin
        // Optionally provide feedback for invalid input
        Exit; // Do not accept invalid text
      end;
    end;

    ClearSelection;
    AdjustTextOffset;
    ResetCaretBlink;
//    if Assigned(OnChange) then // VCL standard OnChange
//      OnChange(Self);
    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

function TANDMR_CEdit.GetText: string;
begin
//  if FMaskType <> mtNone and FUseMaskBehavior then
//    Result := GetUnmaskedText // Return unmasked text by default
//  else
//    Result := FText;
end;

// --- Placeholder for other Setters/Getters and Methods ---
// These will be implemented progressively.

procedure TANDMR_CEdit.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if FReadOnly and FIsFocused then // If becoming read-only while focused
    begin
       HideCaret; // Hide caret as it's not editable
    end
    else if not FReadOnly and FIsFocused then
    begin
       ShowCaret; // Show caret if becoming editable and focused
    end;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetMaxLength(const Value: Integer);
var
  EffectiveMaxLength: Integer;
begin
  EffectiveMaxLength := Max(0, Value); // Ensure non-negative
  if FMaxLength <> EffectiveMaxLength then
  begin
    FMaxLength := EffectiveMaxLength;
    // If current text exceeds new max length (and no mask), truncate it
    if (FMaskType = mtNone) and (FMaxLength > 0) and (Length(FText) > FMaxLength) then
    begin
      FText := Copy(FText, 1, FMaxLength);
      if FCursorPos > FMaxLength then FCursorPos := FMaxLength;
      ClearSelection;
      AdjustTextOffset;
      Repaint;
//      if Assigned(OnChange) then OnChange(Self);
    end;
  end;
end;

procedure TANDMR_CEdit.SetTextAlign(const Value: TAlignment);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetPlaceHolder(const Value: string);
begin
  if FPlaceHolder <> Value then
  begin
    FPlaceHolder := Value;
    if (FText = '') and not FIsFocused then Repaint;
  end;
end;

procedure TANDMR_CEdit.SetPlaceHolderFontColor(const Value: TColor);
begin
  if FPlaceHolderFontColor <> Value then
  begin
    FPlaceHolderFontColor := Value;
    if (FText = '') and not FIsFocused then Repaint;
  end;
end;

procedure TANDMR_CEdit.SetInputType(const Value: TCEditInputType);
begin
  if FInputType <> Value then
  begin
    FInputType := Value;
    // Optionally, re-validate current text and clear if invalid
    // For simplicity, new validation applies to future input
  end;
end;

procedure TANDMR_CEdit.SetCharacterCasing(const Value: TCEditCharacterCasing);
var
  OldText: string;
begin
  if FCharacterCasing <> Value then
  begin
    FCharacterCasing := Value;
    OldText := FText;
    // Apply new casing to existing text
    FText := ApplyCharacterCasing(FText);
    if OldText <> FText then
    begin
//      if Assigned(OnChange) then OnChange(Self);
      Repaint;
    end;
  end;
end;

procedure TANDMR_CEdit.SetCustomValidationRegex(const Value: string);
begin
  if FCustomValidationRegex <> Value then
  begin
    FCustomValidationRegex := Value;
    // Re-validate if needed
  end;
end;

procedure TANDMR_CEdit.InitializeEditMask;
var
  MaskStr: string;
begin
  MaskStr := '';
  case FMaskType of
    mtPhoneBR: MaskStr := '!(99)99999-9999;1;_'; // Allows 8 or 9 digits for mobile
    mtCPF:     MaskStr := '!999.999.999-99;1;_';
    mtCNPJ:    MaskStr := '!99.999.999/9999-99;1;_';
    mtCEP:     MaskStr := '!99999-999;1;_';
    mtDateDMY: MaskStr := '!99/99/9999;1;_';
    mtTimeHHMM:MaskStr := '!99:99;1;_';
    mtCustom:  MaskStr := FCustomMask;
  end;

//  if FEditMask.Mask <> MaskStr then
//  begin
//    FEditMask.Mask := MaskStr;
//    // After changing mask, re-apply to current text
//    ApplyMaskToText(GetUnmaskedText, True);
//  end;
end;

procedure TANDMR_CEdit.SetMaskType(const Value: TCEditMaskType);
begin
  if FMaskType <> Value then
  begin
    FMaskType := Value;
    InitializeEditMask; // This will re-apply mask and repaint
  end;
end;

procedure TANDMR_CEdit.SetCustomMask(const Value: string);
begin
  if FCustomMask <> Value then
  begin
    FCustomMask := Value;
    if FMaskType = mtCustom then
    begin
      InitializeEditMask; // This will re-apply mask and repaint
    end;
  end;
end;

procedure TANDMR_CEdit.SetUseMaskBehavior(const Value: Boolean);
begin
  if FUseMaskBehavior <> Value then
  begin
    FUseMaskBehavior := Value;
    // Re-apply text to mask or vice-versa if behavior changes
    ApplyMaskToText(GetUnmaskedText, True);
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Max(0, Value);
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then Repaint;
  end;
end;

procedure TANDMR_CEdit.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetBorderThickness(const Value: Integer);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Max(0, Value);
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    if FIsFocused then Repaint;
  end;
end;

//procedure TANDMR_CButton.SetFocusStyle(const Value: TCEditFocusStyle);
//begin
//  if FFocusStyle <> Value then
//  begin
//    FFocusStyle := Value;
//    if FIsFocused then Repaint;
//  end;
//end;
//
//procedure TANDMR_CButton.SetFocusBorderThickness(const Value: Integer);
//begin
//  if FFocusBorderThickness <> Max(0, Value) then
//  begin
//    FFocusBorderThickness := Max(0,Value);
//    if FIsFocused then Repaint;
//  end;
//end;

procedure TANDMR_CEdit.SetIcon(const Value: TPicture);
begin
  FIcon.Assign(Value);
  Repaint;
end;

procedure TANDMR_CEdit.SetIconPosition(const Value: TCEditIconPosition);
begin
  if FIconPosition <> Value then
  begin
    FIconPosition := Value;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.SetIconMargins(const Value: TEdgeMargins);
begin
  FIconMargins.Assign(Value);
  Repaint;
end;

procedure TANDMR_CEdit.SetIconWidth(const Value: Integer);
begin
  if FIconWidth <> Max(0, Value) then
  begin
    FIconWidth := Max(0, Value);
    Repaint;
  end;
end;

// --- Internal Helper Methods Implementation (Stubs) ---

procedure TANDMR_CEdit.CursorTimerHandler(Sender: TObject);
begin
  if FIsFocused and not FReadOnly then
  begin
    FCursorVisible := not FCursorVisible;
    // Invalidate only the caret region for efficiency
    // For now, full repaint for simplicity
    Repaint;
  end;
end;

procedure TANDMR_CEdit.ShowCaret;
begin
  if FIsFocused and not FReadOnly then
  begin
    FCursorVisible := True;
    FCursorTimer.Enabled := True;
    Repaint; // Or InvalidateCaretRect
  end;
end;

procedure TANDMR_CEdit.HideCaret;
begin
  FCursorVisible := False;
  FCursorTimer.Enabled := False;
  Repaint; // Or InvalidateCaretRect
end;

procedure TANDMR_CEdit.ResetCaretBlink;
begin
  if FIsFocused and not FReadOnly then
  begin
    HideCaret;
    ShowCaret;
  end;
end;

function TANDMR_CEdit.GetDisplayRect: TRect;
begin
  Result := ClientRect;
  // Potentially adjust for border thickness if border is drawn outside
end;

function TANDMR_CEdit.GetTextRect: TRect;
var
  IconSpaceLeft, IconSpaceRight: Integer;
begin
  Result := GetDisplayRect;

  IconSpaceLeft := 0;
  IconSpaceRight := 0;

  if (FIconPosition = ipLeft) and (FIcon <> nil) and not FIcon.Graphic.Empty then
    IconSpaceLeft := FIconWidth + FIconMargins.Left + FIconMargins.Right;
  if (FIconPosition = ipRight) and (FIcon <> nil) and not FIcon.Graphic.Empty then
    IconSpaceRight := FIconWidth + FIconMargins.Left + FIconMargins.Right;

//  Result.Left   := Result.Left + FBorderThickness + FTextMargins.Left + IconSpaceLeft;
//  Result.Top    := Result.Top + FBorderThickness + FTextMargins.Top;
//  Result.Right  := Result.Right - FBorderThickness - FTextMargins.Right - IconSpaceRight;
//  Result.Bottom := Result.Bottom - FBorderThickness - FTextMargins.Bottom;

  if Result.Right < Result.Left then Result.Right := Result.Left;
  if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
end;

function TANDMR_CEdit.GetIconRect: TRect;
var
  TextRectHeight: Integer;
begin
  Result := Rect(0,0,0,0);
  if (FIconPosition = ipNone) or (FIcon = nil) or FIcon.Graphic.Empty then Exit;

  TextRectHeight := GetTextRect.Height; // Use text rect height for vertical centering
  if TextRectHeight <=0 then TextRectHeight := ClientRect.Height - FBorderThickness * 2;


//  case FIconPosition of
//    ipLeft:
//      Result := System.Types.Rect(
//        FBorderThickness + FIconMargins.Left,
//        FBorderThickness + FIconMargins.Top + (TextRectHeight - Min(FIcon.Height, FIconWidth)) div 2,
//        FBorderThickness + FIconMargins.Left + FIconWidth,
//        FBorderThickness + FIconMargins.Top + (TextRectHeight - Min(FIcon.Height, FIconWidth)) div 2 + Min(FIcon.Height, FIconWidth)
//      );
//    ipRight:
//      Result := System.Types.Rect(
//        Width - FBorderThickness - FIconMargins.Right - FIconWidth,
//        FBorderThickness + FIconMargins.Top + (TextRectHeight - Min(FIcon.Height, FIconWidth)) div 2,
//        Width - FBorderThickness - FIconMargins.Right,
//        FBorderThickness + FIconMargins.Top + (TextRectHeight - Min(FIcon.Height, FIconWidth)) div 2 + Min(FIcon.Height, FIconWidth)
//      );
//  end;
  if Result.Right < Result.Left then Result.Right := Result.Left;
  if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
end;


function TANDMR_CEdit.CharPosToX(CharIndex: Integer): Integer;
var
  DrawTextStr: string;
  TextSize: TSize;
  EffectiveCharIndex: Integer;
begin
  EffectiveCharIndex := Min(CharIndex, Length(FMaskedText)); // Use FMaskedText for drawing position
  if FPasswordChar <> #0 then
    DrawTextStr := StringOfChar(FPasswordChar, EffectiveCharIndex)
  else
    DrawTextStr := Copy(FMaskedText, 1, EffectiveCharIndex);

  Canvas.Font.Assign(FTextFont);
  TextSize := Canvas.TextExtent(DrawTextStr);
  Result := TextSize.cx - FTextOffset;
end;

function TANDMR_CEdit.XToCharPos(X: Integer): Integer;
var
  I: Integer;
  CurrentX, PrevX: Integer;
  DrawTextStr: string;
  AdjustedX: Integer;
begin
  AdjustedX := X + FTextOffset;
  if AdjustedX <= 0 then Exit(0);

  Canvas.Font.Assign(FTextFont);
  PrevX := 0;
  for I := 0 to Length(FMaskedText) do // Use FMaskedText
  begin
    if FPasswordChar <> #0 then
      DrawTextStr := StringOfChar(FPasswordChar, I)
    else
      DrawTextStr := Copy(FMaskedText, 1, I);

    CurrentX := Canvas.TextWidth(DrawTextStr);
    if AdjustedX <= PrevX + (CurrentX - PrevX) div 2 then
    begin
      Result := I -1; // Position before this char
      if Result < 0 then Result := 0;
      Exit;
    end;
    if AdjustedX < CurrentX then
    begin
      Result := I;
      Exit;
    end;
    PrevX := CurrentX;
  end;
  Result := Length(FMaskedText); // If beyond text, position at end
end;

procedure TANDMR_CEdit.AdjustTextOffset;
var
  CaretX: Integer;
  TextRect: TRect;
  TextWidthPixels: Integer;
begin
  TextRect := GetTextRect;
  if TextRect.Right <= TextRect.Left then Exit;

  Canvas.Font.Assign(FTextFont);
  TextWidthPixels := Canvas.TextWidth(FMaskedText); // Use FMaskedText for width calculation

  CaretX := CharPosToX(FCursorPos) + FTextOffset; // Absolute X of caret start

  if TextWidthPixels <= TextRect.Width then // Text fits, no offset needed
  begin
    FTextOffset := 0;
  end
  else // Text wider than display area
  begin
    if CaretX < FTextOffset then // Caret moved left of visible area
      FTextOffset := CaretX
    else if CaretX > FTextOffset + TextRect.Width then // Caret moved right of visible area
      FTextOffset := CaretX - TextRect.Width + Canvas.TextWidth('W') // +W to show some context
    else
    begin
      // Caret is visible, but check if offset can be reduced
//      if FTextOffset > 0 and (TextWidthPixels - FTextOffset < TextRect.Width) then
//         FTextOffset := Max(0, TextWidthPixels - TextRect.Width);
    end;
    FTextOffset := EnsureRange(FTextOffset, 0, Max(0, TextWidthPixels - TextRect.Width));
  end;
  Repaint;
end;

procedure TANDMR_CEdit.ApplyCharacterCasing(var Ch: Char);
begin
//  case FCharacterCasing of
////    ccUpperCase: Ch := System.SysUtils.UpCase(Ch);
////    ccLowerCase: Ch := System.SysUtils.LowerCase(Ch);
//  end;
end;

function TANDMR_CEdit.ApplyCharacterCasing(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  if FCharacterCasing = ccNormal then Exit;
  for I := 1 to Length(Result) do
    ApplyCharacterCasing(PChar(Result)[I-1]); // Directly modify char in PChar
end;

function TANDMR_CEdit.IsValidChar(Ch: Char; CurrentFullText: string): Boolean;
var
  TempText: string;
begin
  Result := True; // Default to true
  case FInputType of
    itNumbersOnly: Result := CharInSet(Ch, ['0'..'9']);
    itInteger:
      begin
        if CharInSet(Ch, ['0'..'9']) then Exit(True);
        if (Ch = '-') and (Length(CurrentFullText) = 0) then Exit(True); // Allow '-' at start
        Result := False;
      end;
    itFloat:
      begin
        if CharInSet(Ch, ['0'..'9']) then Exit(True);
        if (Ch = '-') and (Length(CurrentFullText) = 0) then Exit(True);
        // Allow one decimal separator based on locale
        if (Ch = FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator, CurrentFullText) = 0) then
          Exit(True);
        Result := False;
      end;
    itLettersOnly: Result := CharInSet(Ch, ['a'..'z', 'A'..'Z']);
    itAlphaNumeric: Result := CharInSet(Ch, ['a'..'z', 'A'..'Z', '0'..'9']);
    itNoSpecialChars: Result := CharInSet(Ch, ['a'..'z', 'A'..'Z', '0'..'9', ' ']);
    itEmailAddress: // Very basic check, more robust would need regex
      Result := CharInSet(Ch, ['a'..'z', 'A'..'Z', '0'..'9', '@', '.', '_', '-']);
    itCustomRegex:
      begin
        // Requires a regex library. For now, treat as itNormal.
        // Example: uses RegularExpressions; TRegEx.IsMatch(Ch, FCustomValidationRegex);
        Result := True;
      end;
  end;
end;

function TANDMR_CEdit.IsValidText(const AText: string): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := True;
  if FInputType = itNormal then Exit; // No validation for normal

  for I := 1 to Length(AText) do
  begin
    Ch := AText[I];
    if not IsValidChar(Ch, Copy(AText, 1, I-1)) then // Pass text up to current char for context
    begin
      Result := False;
      Exit;
    end;
  end;

  // Post-loop validation for types like itFloat (e.g. ensure not just "-")
  if FInputType = itFloat then
  begin
    if AText = '-' then Exit(False);
    if AText = FormatSettings.DecimalSeparator then Exit(False);
    if (Length(AText) > 1) and (AText[1] = FormatSettings.DecimalSeparator) and (AText[2] = '-') then Exit(False); // e.g. ".-"
  end;
//  if FInputType = itInteger and (AText = '-') then Exit(False);

  // TODO: Add regex validation for itCustomRegex and more robust itEmailAddress
end;

procedure TANDMR_CEdit.InsertCharacter(Ch: Char);
var
  UnmaskedText, TempText: string;
  OldCursorPos: Integer;
  CanInsert: Boolean;
begin
  if FReadOnly then Exit;
  if FSelectionLength > 0 then DeleteSelection;

  ApplyCharacterCasing(Ch); // Apply casing to the char being inserted

  UnmaskedText := GetUnmaskedText;
  OldCursorPos := FCursorPos; // Cursor in unmasked text

  // Check MaxLength against unmasked text if no mask or mask behavior is off
  if (FMaskType = mtNone) or not FUseMaskBehavior then
  begin
    if (GetEffectiveMaxLength > 0) and (Length(UnmaskedText) >= GetEffectiveMaxLength) then
    begin
      // Max length reached
      Exit;
    end;
  end;

  CanInsert := True;
  if FMaskType = mtNone then // No mask, direct validation
  begin
    TempText := Copy(UnmaskedText, 1, OldCursorPos) + Ch + Copy(UnmaskedText, OldCursorPos + 1, MaxInt);
    CanInsert := IsValidText(TempText); // Validate the potential new text
  end
  else // With mask
  begin
    // Mask validation is more complex, TEditMask handles some of it.
    // We try to insert into the unmasked text and then re-apply the mask.
    // IsValidChar can be used for pre-filtering if not using strict mask behavior.
    if not FUseMaskBehavior and not IsValidChar(Ch, UnmaskedText) then
      CanInsert := False;
  end;

  if not CanInsert then
  begin
    // Optionally provide feedback for invalid char
    Exit;
  end;

  FInternalUpdate := True;
  try
//    if FMaskType <> mtNone and FUseMaskBehavior then
//    begin
//      // Let TEditMask handle character insertion. This is complex.
//      // A simplified approach: insert into unmasked and re-apply.
//      TempText := Copy(UnmaskedText, 1, OldCursorPos) + Ch + Copy(UnmaskedText, OldCursorPos + 1, MaxInt);
//      ApplyMaskToText(TempText); // This updates FText, FMaskedText, and potentially FCursorPos
//      // FCursorPos should ideally be set by ApplyMaskToText based on mask logic
//    end
//    else
//    begin
//      // Insert into FText directly (or unmasked part if mask is present but not strictly used)
//      FText := Copy(FText, 1, OldCursorPos) + Ch + Copy(FText, OldCursorPos + 1, MaxInt);
//      FCursorPos := OldCursorPos + 1;
//      if FMaskType <> mtNone then UpdateMaskedText; // Update visual mask if present
//    end;
//
//    if Assigned(OnChange) then OnChange(Self);
//    AdjustTextOffset;
//    ResetCaretBlink;
//    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TANDMR_CEdit.DeleteSelection;
var
  UnmaskedText: string;
begin
  if FSelectionLength = 0 then Exit;

  FInternalUpdate := True;
  try
    UnmaskedText := GetUnmaskedText;
    System.Delete(UnmaskedText, FSelectionStart + 1, FSelectionLength);
    FCursorPos := FSelectionStart;
    ClearSelection; // Resets FSelectionStart and FSelectionLength

//    if FMaskType <> mtNone and FUseMaskBehavior then
//    begin
//      ApplyMaskToText(UnmaskedText)
//    end
//    else
//    begin
//      FText := UnmaskedText; // Assuming FText was unmasked or mask is visual only
//      if FMaskType <> mtNone then UpdateMaskedText;
//    end;
//
//    if Assigned(OnChange) then OnChange(Self);
    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TANDMR_CEdit.HandleBackspace;
var
  UnmaskedText: string;
begin
  if FReadOnly then Exit;
  if FSelectionLength > 0 then
  begin
    DeleteSelection;
    Exit;
  end;

  if FCursorPos = 0 then Exit; // At the beginning

  FInternalUpdate := True;
  try
    UnmaskedText := GetUnmaskedText;
    System.Delete(UnmaskedText, FCursorPos, 1); // Delete char before cursor
    Dec(FCursorPos);

//    if FMaskType <> mtNone and FUseMaskBehavior then
//    begin
//      ApplyMaskToText(UnmaskedText);
//    end
//    else
//    begin
//      FText := UnmaskedText;
//      if FMaskType <> mtNone then UpdateMaskedText;
//    end;
//
//    if Assigned(OnChange) then OnChange(Self);
    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TANDMR_CEdit.HandleDelete;
var
  UnmaskedText: string;
begin
  if FReadOnly then Exit;
  if FSelectionLength > 0 then
  begin
    DeleteSelection;
    Exit;
  end;

  UnmaskedText := GetUnmaskedText;
  if FCursorPos >= Length(UnmaskedText) then Exit; // At the end

  FInternalUpdate := True;
  try
    System.Delete(UnmaskedText, FCursorPos + 1, 1); // Delete char at cursor

//    if FMaskType <> mtNone and FUseMaskBehavior then
//    begin
//      ApplyMaskToText(UnmaskedText); // Cursor pos should be maintained by ApplyMaskToText
//    end
//    else
//    begin
//      FText := UnmaskedText;
//      if FMaskType <> mtNone then UpdateMaskedText;
//      // FCursorPos remains the same
//    end;
//
//    if Assigned(OnChange) then OnChange(Self);
    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TANDMR_CEdit.SelectAll;
begin
  FSelectionStart := 0;
  FSelectionLength := Length(GetUnmaskedText); // Select based on unmasked text length
  FCursorPos := FSelectionLength; // Move caret to end of selection
  AdjustTextOffset;
  ResetCaretBlink;
  Repaint;
end;

procedure TANDMR_CEdit.ClearSelection;
begin
  FSelectionStart := 0;
  FSelectionLength := 0;
  Repaint;
end;

procedure TANDMR_CEdit.UpdateMaskedText;
begin
  if FMaskType = mtNone then
    FMaskedText := FText
  else
  begin
    // This is tricky. TEditMask.EditText is the display text.
    // We need to ensure FText (unmasked) is consistent.
    // For now, assume FMaskedText is primarily for display.
    // If FUseMaskBehavior is true, FText should be unmasked, FMaskedText is EditMask.EditText
//    if FUseMaskBehavior then
//      FMaskedText := FEditMask.EditText // Get text with literals from VCL's mask
//    else
//      FMaskedText := FText; // If not using strict mask, display raw text
  end;
end;

function TANDMR_CEdit.GetUnmaskedText: string;
begin
//  if FMaskType <> mtNone and Assigned(FEditMask) then
//  begin
//    // TEditMask doesn't directly expose unmasked text easily without literals.
//    // We need to strip literals based on the mask.
//    // This is a simplification; a robust solution is more involved.
//    var TempMask: TEditMask;
//    TempMask := TEditMask.Create(FEditMask.Mask);
//    try
//      TempMask.EditText := FMaskedText; // or FText if FMaskedText isn't primary store
//      Result := TempMask.Value; // Value property often gives unmasked text
//      if Result = FEditMask.Mask then Result := ''; // If only mask literals, return empty
//    finally
//      TempMask.Free;
//    end;
//    // Crude literal stripping if .Value doesn't work as expected for all masks
//    if Result = '' and FMaskedText <> '' then
//    begin
//        var I: Integer;
//        Result := '';
//        for I := 1 to Length(FMaskedText) do
//        begin
//            if not FEditMask.IsLiteral(I-1) then // IsLiteral is 0-based
//                Result := Result + FMaskedText[I];
//        end;
//    end;
//
//  end
//  else
//    Result := FText;
end;

// This procedure applies the current mask to InputText.
// It updates FText (unmasked), FMaskedText (display), and FCursorPos.
procedure TANDMR_CEdit.ApplyMaskToText(const InputText: string; UpdateCursor: Boolean = True);
var
  OldCursorInMasked: Integer;
  NewCursorInMasked: Integer;
  Succeeded: Boolean;
begin
  if FMaskType = mtNone then
  begin
    FText := InputText;
    FMaskedText := FText;
    if UpdateCursor then
      FCursorPos := EnsureRange(FCursorPos, 0, Length(FText));
    Exit;
  end;

//  if not Assigned(FEditMask) then InitializeEditMask;
//
//  // Store old cursor position relative to masked text to try and restore it
//  if UpdateCursor then
//    OldCursorInMasked := FEditMask.CursorPosToCharPos(FCursorPos) // This needs careful handling
//  else
//    OldCursorInMasked := -1;
//
//  // TEditMask.SetText tries to format the input according to the mask.
//  Succeeded := FEditMask.SetText(InputText);
//
//  FMaskedText := FEditMask.EditText; // This is the text with literals
//  FText := GetUnmaskedText;          // Update our internal unmasked text
//
//  if UpdateCursor then
//  begin
//    // Try to restore cursor position or set to a sensible place
//    // This is complex due to literals. TEditMask might handle this internally
//    // if we could directly manipulate its cursor.
//    // For now, a simple approach:
//    if OldCursorInMasked <> -1 then
//    begin
//        NewCursorInMasked := FEditMask.ValidateAndConvertPos(OldCursorInMasked, True);
//        FCursorPos := FEditMask.CharPosToCursorPos(NewCursorInMasked); // Convert back to unmasked index
//    end
//    else // If not updating cursor based on old pos, set to end of unmasked text
//    begin
//        FCursorPos := Length(FText);
//    end;
//    FCursorPos := EnsureRange(FCursorPos, 0, Length(FText));
//  end;
  Repaint;
end;

function TANDMR_CEdit.GetEffectiveMaxLength: Integer;
begin
//  if FMaskType <> mtNone and Assigned(FEditMask) and FUseMaskBehavior then
//    Result := FEditMask.MaxLen // Mask determines its own max length of unmasked chars
//  else
//    Result := FMaxLength;
end;

// --- Message Handlers & Overrides (Stubs / Basic Implementation) ---

procedure TANDMR_CEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  // Optional: Change cursor or visual feedback on mouse enter
end;

procedure TANDMR_CEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  // Optional: Revert cursor or visual feedback
end;

procedure TANDMR_CEdit.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
//  FIsFocused := (Message.FocusedControl = Self);
  if FIsFocused then
  begin
    ShowCaret;
    SelectAll; // Standard Windows behavior: select all text on focus via Tab
    if Assigned(OnEnter) then OnEnter(Self);
  end
  else
  begin
    HideCaret;
    ClearSelection;
    if Assigned(OnExit) then OnExit(Self);
  end;
  Repaint; // Redraw to show focus state (border, caret)
end;

procedure TANDMR_CEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    HideCaret;
    FIsFocused := False; // Cannot be focused if disabled
  end;
  Repaint;
end;

procedure TANDMR_CEdit.WMPaste(var Message: TWMPaste);
var
  ClipboardText: string;
  I: Integer;
begin
  if FReadOnly then Exit;

//  if Clipboard.HasFormat(CF_TEXT) then
//  begin
//    ClipboardText := Clipboard.AsText;
//    if FSelectionLength > 0 then DeleteSelection;
//
//    // Insert char by char to respect validation, casing, mask, maxlength
//    for I := 1 to Length(ClipboardText) do
//    begin
//      InsertCharacter(ClipboardText[I]);
//      if (GetEffectiveMaxLength > 0) and (Length(GetUnmaskedText) >= GetEffectiveMaxLength) then
//        Break; // Stop if max length reached
//    end;
//    Message.Result := 0; // Indicate paste was handled
//  end
//  else
//    inherited; // Default processing if not text
end;

procedure TANDMR_CEdit.WMChar(var Message: TWMChar);
begin
  if FReadOnly then
  begin
    Message.Result := 0; // Handled, do nothing
    Exit;
  end;

//  case Message.CharCode of
//    VK_BACK: HandleBackspace; // Backspace
//    VK_RETURN: // Enter key
//      begin
//        // For single line edit, typically might trigger default button or change focus
//        // For now, just consume it. Could add OnEnterKeyPress event.
//        SelectNext(Self, True, True); // Example: move to next control
//      end;
//    VK_ESCAPE: // Escape key
//      begin
//        // Optional: Clear text or revert to original
//        // Clear;
//      end;
//    0..31: // Control characters (except tab, enter, backspace handled above)
//      begin
//        // Check for Ctrl+A (Select All) - typically handled in KeyDown
//        if Message.CharCode = $01 then // Ctrl+A
//        begin
//           SelectAll;
//           Message.Result := 0;
//           Exit;
//        end;
//        // Other control chars, ignore for now
//      end
//  else // Printable character
//    InsertCharacter(Char(Message.CharCode));
//  end;
  Message.Result := 0; // Indicate character was handled
end;

procedure TANDMR_CEdit.Paint;
var
  LPathRect: TGPRectF;
  LRadius, LPathInset: Single;
  LCurrentBorderColor, LCurrentFillColor: TColor;
  LCurrentBorderThickness: Integer;
  LG: TGPGraphics;
  LGPBrush: TGPBrush;
  LGPPath: TGPGraphicsPath;
  LGPPen: TGPPen;
  LTextRect, LDisplayRect, LIconDrawRect: TRect;
  LDrawText: string;
  DrawFormatFlags: Cardinal;
  Diameter: Single;
  CaretX, CaretY, CaretHeight: Integer;
  SelStartX, SelEndX: Integer;
  SelRect: TRect;
  IconBmp: TGPBitmap;
  Adapter: IStream;
  PngStream: TMemoryStream;
const
  MIN_RADIUS_FOR_ARC = 0.5;
begin
  inherited Paint; // Important for parent background if transparent

  LDisplayRect := GetDisplayRect;
  LTextRect := GetTextRect;

  LG := TGPGraphics.Create(Canvas.Handle);
  try
    LG.SetSmoothingMode(SmoothingModeAntiAlias);
    LG.SetPixelOffsetMode(PixelOffsetModeHalf);

    // --- Determine Colors and Border ---
    if Enabled then
    begin
      LCurrentFillColor := FBackgroundColor;
      LCurrentBorderColor := FBorderColor;
      LCurrentBorderThickness := FBorderThickness;
      if FIsFocused then
      begin
        case FFocusStyle of
          fsBorder: LCurrentBorderColor := FFocusColor;
          fsBottomLine: LCurrentBorderColor := FBorderColor; // Base border, extra line later
          fsBackgroundColor: LCurrentFillColor := FFocusColor;
          fsUnderlineText: LCurrentBorderColor := FBorderColor; // Base border, underline later
        end;
        if (FFocusStyle = fsBorder) or (FFocusStyle = fsBottomLine) then // BottomLine might want thicker base too
            LCurrentBorderThickness := Max(FBorderThickness, FFocusBorderThickness);
      end;
    end
    else // Disabled
    begin
      LCurrentFillColor := FDisabledColor;
      LCurrentBorderColor := DarkerColorCE(FDisabledColor, 20);
      LCurrentBorderThickness := FBorderThickness;
    end;

    // --- Draw Background ---
    if not FTransparent then
    begin
      LGPPath := TGPGraphicsPath.Create;
      try
        // Path for fill (slightly inset from outer border if border is thick)
        LPathInset := LCurrentBorderThickness / 2.0;
        if LPathInset < 0 then LPathInset := 0;
        LPathRect := MakeRect(LDisplayRect.Left + LPathInset, LDisplayRect.Top + LPathInset,
                               LDisplayRect.Width - 2*LPathInset, LDisplayRect.Height - 2*LPathInset);
        LRadius := Min(FCornerRadius - LPathInset, Min(LPathRect.Width, LPathRect.Height) / 2.0);
        LRadius := Max(0, LRadius);

        if LRadius < MIN_RADIUS_FOR_ARC then
        begin if (LPathRect.Width > 0) and (LPathRect.Height > 0) then LGPPath.AddRectangle(LPathRect); end
        else
        begin
          Diameter := LRadius * 2;
          if Diameter > LPathRect.Width then Diameter := LPathRect.Width;
          if Diameter > LPathRect.Height then Diameter := LPathRect.Height;
          if Diameter > 0 then
          begin
            LGPPath.AddArc(LPathRect.X, LPathRect.Y, Diameter, Diameter, 180, 90);
            LGPPath.AddArc(LPathRect.X + LPathRect.Width - Diameter, LPathRect.Y, Diameter, Diameter, 270, 90);
            LGPPath.AddArc(LPathRect.X + LPathRect.Width - Diameter, LPathRect.Y + LPathRect.Height - Diameter, Diameter, Diameter, 0, 90);
            LGPPath.AddArc(LPathRect.X, LPathRect.Y + LPathRect.Height - Diameter, Diameter, Diameter, 90, 90);
            LGPPath.CloseFigure;
          end else if (LPathRect.Width > 0) and (LPathRect.Height > 0) then LGPPath.AddRectangle(LPathRect);
        end;

        if LGPPath.GetPointCount > 0 then
        begin
          LGPBrush := TGPSolidBrush.Create(ColorToARGB_CE(LCurrentFillColor));
          try LG.FillPath(LGPBrush, LGPPath); finally LGPBrush.Free; end;
        end;
      finally LGPPath.Free; end;
    end; // not FTransparent

    // --- Clip to Text Area for Text Drawing ---
    // Create a clipping region for text and placeholder
    // This ensures text doesn't draw over borders or icon areas
//    var ClipPath: TGPGraphicsPath;
//    ClipPath := TGPGraphicsPath.Create;
//    try
//        ClipPath.AddRectangle(MakeRectF(LTextRect.Left, LTextRect.Top, LTextRect.Width, LTextRect.Height));
//        LG.SetClip(ClipPath, CombineModeReplace);
//    finally
//        ClipPath.Free;
//    end;
//
//
//    // --- Draw Text or Placeholder ---
//    Canvas.Font.Assign(FTextFont);
    if (FText = '') and (FPlaceHolder <> '') and not FIsFocused then
    begin
      LDrawText := FPlaceHolder;
      Canvas.Font.Color := FPlaceHolderFontColor;
    end
    else
    begin
      if FPasswordChar <> #0 then
        LDrawText := StringOfChar(FPasswordChar, Length(FMaskedText))
      else
        LDrawText := FMaskedText; // Display masked text
      Canvas.Font.Color := FTextFont.Color;
      if not Enabled then Canvas.Font.Color := clGrayText;
    end;

    DrawFormatFlags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;
    case FTextAlign of
      taLeftJustify: DrawFormatFlags := DrawFormatFlags or DT_LEFT;
      taRightJustify: DrawFormatFlags := DrawFormatFlags or DT_RIGHT;
      taCenter: DrawFormatFlags := DrawFormatFlags or DT_CENTER;
    end;

    // Adjust LTextRect for drawing with FTextOffset
    var AdjustedTextRect: TRect;
    AdjustedTextRect := LTextRect;
    AdjustedTextRect.Left := AdjustedTextRect.Left - FTextOffset;
    // Right extent needs to be large enough if text is scrolled
    AdjustedTextRect.Right := AdjustedTextRect.Left + Max(LTextRect.Width, Canvas.TextWidth(LDrawText) + 10);


    if (AdjustedTextRect.Right > AdjustedTextRect.Left) and (AdjustedTextRect.Bottom > AdjustedTextRect.Top) then
       DrawText(Canvas.Handle, PChar(LDrawText), Length(LDrawText), AdjustedTextRect, DrawFormatFlags);

    LG.ResetClip(); // Reset clipping region

    // --- Draw Selection ---
//    if FIsFocused and (FSelectionLength > 0) and (FPasswordChar = #0) then
//    begin
//      SelStartX := LTextRect.Left + CharPosToX(FSelectionStart);
//      SelEndX   := LTextRect.Left + CharPosToX(FSelectionStart + FSelectionLength);
//      SelRect   := System.Types.Rect(Min(SelStartX, SelEndX), LTextRect.Top, Max(SelStartX, SelEndX), LTextRect.Bottom);
//
//      // Use GDI+ for selection rectangle to respect clipping and anti-aliasing
//      var SelBrush: TGPSolidBrush;
//      SelBrush := TGPSolidBrush.Create(ColorToARGB_CE(clHighlight, 128)); // Semi-transparent highlight
//      try
////        LG.FillRectangle(SelBrush, MakeRectF(SelRect.Left, SelRect.Top, SelRect.Width, SelRect.Height));
////      finally
////        SelBrush.Free;
////      end;
////      // Redraw selected text in highlight text color
////      Canvas.Font.Color := clHighlightText;
////      var SelectedDrawText: string;
////      SelectedDrawText := Copy(FMaskedText, FSelectionStart + 1, FSelectionLength);
////
////      // Clip again for selected text drawing
////      ClipPath := TGPGraphicsPath.Create;
//      try
//          ClipPath.AddRectangle(MakeRectF(SelRect.Left, SelRect.Top, SelRect.Width, SelRect.Height));
//          LG.SetClip(ClipPath, CombineModeReplace);
//          // Adjust rect for drawing selected part
//          AdjustedTextRect.Left := SelRect.Left - (CharPosToX(FSelectionStart) + FTextOffset); // Align start of selected text
//          DrawText(Canvas.Handle, PChar(SelectedDrawText), Length(SelectedDrawText), AdjustedTextRect, DrawFormatFlags);
//      finally
//          ClipPath.Free;
//      end;
//      LG.ResetClip();
//    end;


    // --- Draw Border ---
//    if FBorderThickness > 0 then
//    begin
//      LGPPath := TGPGraphicsPath.Create;
//      try
//        LPathInset := LCurrentBorderThickness / 2.0; // Border is drawn centered on the path
//        LPathRect := MakeRectF(LDisplayRect.Left + LPathInset, LDisplayRect.Top + LPathInset,
//                               LDisplayRect.Width - 2*LPathInset, LDisplayRect.Height - 2*LPathInset);
//        LRadius := Min(FCornerRadius - LPathInset, Min(LPathRect.Width, LPathRect.Height) / 2.0);
//        LRadius := Max(0, LRadius);
//
//        if LRadius < MIN_RADIUS_FOR_ARC then
//        begin if (LPathRect.Width > 0) and (LPathRect.Height > 0) then LGPPath.AddRectangle(LPathRect); end
//        else
//        begin
//          Diameter := LRadius * 2;
//          if Diameter > LPathRect.Width then Diameter := LPathRect.Width;
//          if Diameter > LPathRect.Height then Diameter := LPathRect.Height;
//           if Diameter > 0 then
//           begin
//            LGPPath.AddArc(LPathRect.X, LPathRect.Y, Diameter, Diameter, 180, 90);
//            LGPPath.AddArc(LPathRect.X + LPathRect.Width - Diameter, LPathRect.Y, Diameter, Diameter, 270, 90);
//            LGPPath.AddArc(LPathRect.X + LPathRect.Width - Diameter, LPathRect.Y + LPathRect.Height - Diameter, Diameter, Diameter, 0, 90);
//            LGPPath.AddArc(LPathRect.X, LPathRect.Y + LPathRect.Height - Diameter, Diameter, Diameter, 90, 90);
//            LGPPath.CloseFigure;
//           end else if (LPathRect.Width > 0) and (LPathRect.Height > 0) then LGPPath.AddRectangle(LPathRect);
//        end;
//
//        if LGPPath.GetPointCount > 0 then
//        begin
//          LGPPen := TGPPen.Create(ColorToARGB_CE(LCurrentBorderColor), LCurrentBorderThickness);
//          try
//            case FBorderStyle of
//              psDash: LGPPen.SetDashStyle(DashStyleDash);
//              psDot: LGPPen.SetDashStyle(DashStyleDot);
//              psDashDot: LGPPen.SetDashStyle(DashStyleDashDot);
//              psDashDotDot: LGPPen.SetDashStyle(DashStyleDashDotDot);
//            else LGPPen.SetDashStyle(DashStyleSolid);
//            end;
//            LG.DrawPath(LGPPen, LGPPath);
//          finally LGPPen.Free; end;
//        end;
//      finally LGPPath.Free; end;
//    end; // FBorderThickness > 0
//
//    // --- Draw Focus Style (BottomLine or UnderlineText) ---
//    if FIsFocused and Enabled then
//    begin
//      if FFocusStyle = fsBottomLine then
//      begin
//        LGPPen := TGPPen.Create(ColorToARGB_CE(FFocusColor), FFocusBorderThickness);
//        try
//          LG.DrawLine(LGPPen, LDisplayRect.Left + FFocusBorderThickness div 2, LDisplayRect.Bottom - FFocusBorderThickness div 2,
//                              LDisplayRect.Right - FFocusBorderThickness div 2, LDisplayRect.Bottom - FFocusBorderThickness div 2);
//        finally LGPPen.Free; end;
//      end
//      else if FFocusStyle = fsUnderlineText then
//      begin
//         // Underline the actual text area
//         LGPPen := TGPPen.Create(ColorToARGB_CE(FFocusColor), Max(1, FFocusBorderThickness div 2)); // Thinner line for underline
//         try
//            LG.DrawLine(LGPPen, LTextRect.Left - FTextOffset, LTextRect.Bottom + 1,
//                                LTextRect.Left - FTextOffset + Canvas.TextWidth(LDrawText), LTextRect.Bottom + 1);
//         finally LGPPen.Free; end;
//      end;
//    end;
//
//    // --- Draw Icon ---
//    LIconDrawRect := GetIconRect;
//    if (FIconPosition <> ipNone) and (FIcon <> nil) and not FIcon.Graphic.Empty and
//       (LIconDrawRect.Width > 0) and (LIconDrawRect.Height > 0) then
//    begin
//        if FIcon.Graphic is TPNGImage then
//        begin
//            PngStream := TMemoryStream.Create;
//            try
//                (FIcon.Graphic as TPNGImage).SaveToStream(PngStream);
//                PngStream.Position := 0;
//                Adapter := TStreamAdapter.Create(PngStream, soReference);
//                IconBmp := TGPBitmap.Create(Adapter);
//                try
//                    LG.SetInterpolationMode(InterpolationModeHighQualityBicubic);
//                    LG.DrawImage(IconBmp, LIconDrawRect.Left, LIconDrawRect.Top, LIconDrawRect.Width, LIconDrawRect.Height);
//                finally
//                    IconBmp.Free;
//                end;
//            finally
//                PngStream.Free;
//            end;
//        end
//        else // Other image types
//        begin
//            // Fallback to VCL Canvas draw if not PNG, GDI+ can also draw other formats from HBITMAP
//            var TempBmp: TBitmap;
//            TempBmp := TBitmap.Create;
//            try
//                TempBmp.Assign(FIcon.Graphic);
//                Canvas.StretchDraw(LIconDrawRect, TempBmp); // Using VCL Canvas here for simplicity
//            finally
//                TempBmp.Free;
//            end;
//        end;
//    end;


    // --- Draw Caret ---
//    if FIsFocused and FCursorVisible and not FReadOnly then
//    begin
//      CaretX := LTextRect.Left + CharPosToX(FCursorPos); // X position relative to LTextRect.Left
//      CaretY := LTextRect.Top;
//      CaretHeight := LTextRect.Height;
//      // Ensure caret is within the visible text area horizontally
//      if (CaretX >= LTextRect.Left) and (CaretX <= LTextRect.Right + 1) then // +1 to allow caret at end
//      begin
//        LGPPen := TGPPen.Create(ColorToARGB_CE(FTextFont.Color), 1); // Caret color same as text
//        try
//          LG.DrawLine(LGPPen, CaretX, CaretY, CaretX, CaretY + CaretHeight);
//        finally LGPPen.Free; end;
//      end;
//    end;
//
//  finally
//    LG.Free;
//  end;


  // Draw focus rectangle if standard VCL focus rect is desired (usually not for custom painted controls)
  // if Focused and TabStop and StyleServices.ShowFocusRects then
  // DrawFocusRect(Canvas.Handle, ClientRect);

  finally

  end;


end;


procedure TANDMR_CEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Ctrl, Alt, ShiftKey: Boolean; // Delphi XE+ has IsCtrlPressed etc.
begin
  inherited;
  if FReadOnly then Exit;

  Ctrl := Shift * [ssCtrl] <> [];
  Alt  := Shift * [ssAlt] <> [];
  ShiftKey := Shift * [ssShift] <> [];


  case Key of
    VK_LEFT:
      begin
        if Ctrl then // Word left
        begin
          // TODO: Implement word navigation
          if FCursorPos > 0 then Dec(FCursorPos);
        end
        else if ShiftKey then // Extend selection left
        begin
            if FSelectionLength = 0 then FSelectionStart := FCursorPos;
            if FCursorPos > 0 then
            begin
                Dec(FCursorPos);
                if FCursorPos < FSelectionStart then FSelectionLength := FSelectionStart - FCursorPos
                else FSelectionLength := FSelectionLength -1;
            end;
        end
        else // Move caret left
        begin
          ClearSelection;
          if FCursorPos > 0 then Dec(FCursorPos);
        end;
        Key := 0;
      end;
    VK_RIGHT:
      begin
        if Ctrl then // Word right
        begin
          // TODO: Implement word navigation
          if FCursorPos < Length(GetUnmaskedText) then Inc(FCursorPos);
        end
        else if ShiftKey then // Extend selection right
        begin
            if FSelectionLength = 0 then FSelectionStart := FCursorPos;
            if FCursorPos < Length(GetUnmaskedText) then
            begin
                Inc(FCursorPos);
                if FCursorPos > FSelectionStart + FSelectionLength then FSelectionLength := FCursorPos - FSelectionStart
                else FSelectionLength := FSelectionLength + 1; // This logic needs refinement for selection
            end;
        end
        else // Move caret right
        begin
          ClearSelection;
          if FCursorPos < Length(GetUnmaskedText) then Inc(FCursorPos);
        end;
        Key := 0;
      end;
    VK_HOME:
      begin
        if ShiftKey then
        begin
            if FSelectionLength = 0 then FSelectionStart := FCursorPos;
            FSelectionLength := FSelectionStart - 0; // From current start to beginning
            FSelectionStart := 0;
        end else ClearSelection;
        FCursorPos := 0;
        Key := 0;
      end;
    VK_END:
      begin
        if ShiftKey then
        begin
            if FSelectionLength = 0 then FSelectionStart := FCursorPos;
            FSelectionLength := Length(GetUnmaskedText) - FSelectionStart;
        end else ClearSelection;
        FCursorPos := Length(GetUnmaskedText);
        Key := 0;
      end;
    VK_DELETE:
      begin
        HandleDelete;
        Key := 0;
      end;
    VK_INSERT:
      begin
//        if ShiftKey then WMPaste(TWMPaste.Create); // Shift+Insert = Paste
        // else toggle Overwrite mode (not implemented)
        Key := 0;
      end;
    Ord('A'): if Ctrl then begin SelectAll; Key := 0; end;
    Ord('C'): if Ctrl then begin CopyToClipboard; Key := 0; end;
    Ord('V'): if Ctrl then begin PasteFromClipboard; Key := 0; end;
    Ord('X'): if Ctrl then begin CutToClipboard; Key := 0; end;
    Ord('Z'): if Ctrl then begin (* TODO: Undo *) Key := 0; end;
  end;

  if Key <> 0 then // If key was not handled above
  begin
    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  end;
end;

procedure TANDMR_CEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not FIsFocused then
  begin
    SetFocus; // VCL SetFocus will trigger CMFocusChanged
  end;

  if Button = mbLeft then
  begin
    var TextRect := GetTextRect;
    FCursorPos := XToCharPos(X - TextRect.Left); // X relative to text area start
    FCursorPos := EnsureRange(FCursorPos, 0, Length(GetUnmaskedText));

    if not (Shift * [ssShift] <> []) then // If Shift is not pressed, clear previous selection
    begin
        ClearSelection;
        FSelecting := True;
        FSelectionStart := FCursorPos;
        FMouseDownPos := Point(X,Y);
    end
    else // Shift is pressed, extend selection
    begin
        FSelecting := True; // Start or continue selection
        // FSelectionStart is kept from previous state or set if no active selection
        if FSelectionLength = 0 then FSelectionStart := FCursorPos; // Anchor point for shift-click

        if FCursorPos < FSelectionStart then
        begin
            FSelectionLength := FSelectionStart - FCursorPos;
            FSelectionStart := FCursorPos; // New start is the current cursor
        end
        else
        begin
            FSelectionLength := FCursorPos - FSelectionStart;
        end;
    end;

    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  end;
  // Handle icon click
  if (FIconPosition <> ipNone) and PtInRect(GetIconRect, Point(X,Y)) and Assigned(FOnClickIcon) then
  begin
      FOnClickIcon(Self);
  end;
end;

procedure TANDMR_CEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewCursorPos: Integer;
  TextRect: TRect;
begin
  inherited;
  if FSelecting and (Shift * [ssLeft] <> []) then // If left button is down
  begin
    TextRect := GetTextRect;
    NewCursorPos := XToCharPos(X - TextRect.Left);
    NewCursorPos := EnsureRange(NewCursorPos, 0, Length(GetUnmaskedText));

    if NewCursorPos <> FCursorPos then // Only update if cursor actually moved
    begin
        FCursorPos := NewCursorPos;
        if FCursorPos < FSelectionStart then
        begin
            FSelectionLength := FSelectionStart - FCursorPos;
            // FSelectionStart remains the anchor
        end
        else
        begin
            FSelectionLength := FCursorPos - FSelectionStart;
        end;
        AdjustTextOffset;
        ResetCaretBlink; // Keep caret visible during selection drag
        Repaint;
    end;
  end;
end;

procedure TANDMR_CEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FSelecting := False;
  end;
end;

procedure TANDMR_CEdit.DblClick;
begin
  inherited;
  SelectAll; // Standard double-click behavior
end;

procedure TANDMR_CEdit.Clear;
begin
  if FReadOnly then Exit;
  FInternalUpdate := True;
  try
    FText := '';
    FMaskedText := '';
    FCursorPos := 0;
    ClearSelection;
//    if Assigned(FEditMask) then FEditMask.Clear; // Clear internal VCL mask state
//    if Assigned(OnChange) then OnChange(Self);
    AdjustTextOffset;
    ResetCaretBlink;
    Repaint;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TANDMR_CEdit.CopyToClipboard;
var
  SelectedText: string;
begin
  if FSelectionLength > 0 then
  begin
    SelectedText := Copy(GetUnmaskedText, FSelectionStart + 1, FSelectionLength);
    if FPasswordChar <> #0 then // Don't copy password chars
        Exit;
//    Clipboard.AsText := SelectedText;
  end;
end;

procedure TANDMR_CEdit.CutToClipboard;
begin
  if FReadOnly then Exit;
  CopyToClipboard;
  DeleteSelection; // This will trigger OnChange and repaint
end;

procedure TANDMR_CEdit.PasteFromClipboard;
begin
  // WM_PASTE message handles this more robustly
  Perform(WM_PASTE, 0, 0);
end;

end.
