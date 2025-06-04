unit ANDMR_CEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Windows,
  Vcl.StdCtrls, System.UITypes, Winapi.Messages, Vcl.Forms, Vcl.Themes,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL, System.Math, Winapi.ActiveX,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  ANDMR_ComponentUtils;

type
  TANDMR_CEdit = class(TCustomControl)
  private
    // New Settings Objects
    FBorderSettings: TBorderSettings;
    FFocusSettings: TFocusSettings;
    FSeparatorSettings: TSeparatorSettings;
    FImageSettings: TImageSettings;

    FText: string;
    FMaxLength: Integer;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FCaretVisible: Boolean;
    FCaretPosition: Integer;
    FCaretTimer: TTimer;
    FOpacity: Byte;
    FCurrentCursor: TCursor;
    FInputType: TInputType;
    FTextCase: TTextCase;
    FInputMask: string;
    FMaskedText: string;
    FRawText: string;
    FCaptionSettings: TCaptionSettings;
    FCaptionRect: TRect;
    FHoverSettings: THoverSettings;
    FHovered: Boolean;
    FTextMargins: TANDMR_Margins;
    FPredefinedMask: TPredefinedMaskType;
    FOnExit: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FStatus: TCEditStatus; // New field

    procedure SetText(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetBorderSettings(const Value: TBorderSettings); // New
    procedure SetImageSettings(const Value: TImageSettings); // New

    procedure SetSeparatorSettings(const Value: TSeparatorSettings);

    procedure CaretTimerTick(Sender: TObject);
    procedure SetFocusSettings(const Value: TFocusSettings);

    procedure SetOpacity(const Value: Byte);
    procedure SetCurrentCursor(const Value: TCursor);
    procedure SetInputType(const Value: TInputType);
    procedure SetTextCase(const Value: TTextCase);
    procedure SetInputMask(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure TextMarginsChanged(Sender: TObject);
    procedure SetTextMargins(const Value: TANDMR_Margins);
    procedure SetPredefinedMask(const Value: TPredefinedMaskType);
    procedure SetStatus(const Value: TCEditStatus); // New setter declaration

    procedure SettingsChanged(Sender: TObject); // Generic handler for Border, Focus, Separator settings
    procedure ImageSettingsChanged(Sender: TObject);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
    procedure CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); virtual;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Text: string read FText write SetText;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property BorderSettings: TBorderSettings read FBorderSettings write SetBorderSettings; // New
    property ImageSettings: TImageSettings read FImageSettings write SetImageSettings; // New

    property SeparatorSettings: TSeparatorSettings read FSeparatorSettings write SetSeparatorSettings;

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
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

    property FocusSettings: TFocusSettings read FFocusSettings write SetFocusSettings; // New
    property Opacity: Byte read FOpacity write SetOpacity;
    property CurrentCursor: TCursor read FCurrentCursor write SetCurrentCursor;
    property InputType: TInputType read FInputType write SetInputType default itNormal;
    property TextCase: TTextCase read FTextCase write SetTextCase default tcNormal;
    property InputMask: string read FInputMask write SetInputMask;
    property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
    property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;
    property PredefinedMask: TPredefinedMaskType read FPredefinedMask write SetPredefinedMask default pmtNone;
    property Status: TCEditStatus read FStatus write SetStatus; // New published property
  end;

procedure Register;

implementation

uses
  System.Character;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CEdit]);
end;

{ TANDMR_CEdit }

constructor TANDMR_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  DoubleBuffered := True;
  Width := 150;
  Height := 25;
  TabStop := True;
  FText := '';
  FMaxLength := 0;
  FPasswordChar := #0;
  FReadOnly := False;
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  Font.Color := clWindowText;

  // Instantiate new settings objects
  FBorderSettings := TBorderSettings.Create;
  FBorderSettings.OnChange := SettingsChanged;
  FBorderSettings.CornerRadius := 8;
  FBorderSettings.RoundCornerType := rctAll;
  FBorderSettings.Color := clBlack;
  FBorderSettings.Thickness := 1;
  FBorderSettings.Style := psSolid;
  FBorderSettings.BackgroundColor := clBtnFace; // Default for InactiveColor (background of border area)

  FFocusSettings := TFocusSettings.Create;
  FFocusSettings.OnChange := SettingsChanged;
  FFocusSettings.BorderColorVisible := True; // Changed
  FFocusSettings.BorderColor := clHighlight; // Changed
  FFocusSettings.BackgroundColorVisible := False;
  FFocusSettings.BackgroundColor := clWindow;
  FFocusSettings.UnderlineVisible := False;
  FFocusSettings.UnderlineColor := clBlack;
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
  FImageSettings.OnChange := ImageSettingsChanged;
  // Image settings defaults are handled within TImageSettings constructor

  FCaretVisible := False;
  FCaretPosition := 0;
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Interval := GetCaretBlinkTime;
  FCaretTimer.OnTimer := CaretTimerTick;
  FCaretTimer.Enabled := False;
  FOpacity := 255;
  FCurrentCursor := crIBeam;
  Self.Cursor := FCurrentCursor;
  FInputType := itNormal;
  FTextCase := tcNormal;
  FInputMask := '';
  FMaskedText := '';
  FRawText := '';
  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FCaptionRect := Rect(0,0,0,0);
  FHoverSettings := THoverSettings.Create(Self);
  FHoverSettings.OnChange := HoverSettingsChanged;
  FHovered := False;
  FTextMargins := TANDMR_Margins.Create;
  FTextMargins.OnChange := TextMarginsChanged;
  FPredefinedMask := pmtNone;
  FStatus := cepsNormal; // Initialize new field
end;

destructor TANDMR_CEdit.Destroy;
begin
  FBorderSettings.OnChange := nil;
  FBorderSettings.Free;
  FFocusSettings.OnChange := nil;
  FFocusSettings.Free;
  FSeparatorSettings.OnChange := nil;
  FSeparatorSettings.Free;
  FImageSettings.OnChange := nil;
  FImageSettings.Free;

  FTextMargins.OnChange := nil;
  FTextMargins.Free;
  FHoverSettings.OnChange := nil;
  FHoverSettings.Free;
  FCaptionSettings.Free;
  FCaretTimer.Free;
  inherited Destroy;
end;

procedure TANDMR_CEdit.SetText(const Value: string);
var
  OldFDisplayText: string;
  ProcessedRawText: string;
  NewMaskedText: string;
  NewUnmaskedText: string;
  RawIndex: Integer;
  MaskIndex: Integer;
  MaskChar: Char;
  IsLiteral: Boolean;
  CharToTest: Char;
  CharAllowed: Boolean;
begin
  OldFDisplayText := FText;

  ProcessedRawText := Value;
  case FTextCase of
    tcUppercase: ProcessedRawText := System.SysUtils.UpperCase(ProcessedRawText);
    tcLowercase: ProcessedRawText := System.SysUtils.LowerCase(ProcessedRawText);
  end;

  NewUnmaskedText := '';
  NewMaskedText := '';

  if FInputMask <> '' then
  begin
    RawIndex := 1;
    for MaskIndex := 1 to Length(FInputMask) do
    begin
      MaskChar := FInputMask[MaskIndex];
      IsLiteral := not (MaskChar IN ['9', '0', 'L', 'A', '#']);

      if IsLiteral then
      begin
        NewMaskedText := NewMaskedText + MaskChar;
      end
      else
      begin
        if RawIndex <= Length(ProcessedRawText) then
        begin
          CharToTest := ProcessedRawText[RawIndex];
          CharAllowed := False;
          case MaskChar of
            '9', '0': CharAllowed := CharToTest IN ['0'..'9'];
            'L': CharAllowed := System.Character.IsLetter(CharToTest);
            'A': CharAllowed := System.Character.IsLetterOrDigit(CharToTest);
            '#': CharAllowed := True;
          end;

          if CharAllowed then
          begin
            NewMaskedText := NewMaskedText + CharToTest;
            NewUnmaskedText := NewUnmaskedText + CharToTest;
            Inc(RawIndex);
          end
          else
          begin
            NewMaskedText := NewMaskedText + '_'; // Placeholder for invalid char
          end;
        end
        else
        begin
          NewMaskedText := NewMaskedText + '_'; // Placeholder for empty part of mask
        end;
      end;
    end;
    FRawText := NewUnmaskedText;
    FText := NewMaskedText; // FText (display text) is the newly built FMaskedText
    FMaskedText := NewMaskedText; // Keep FMaskedText field in sync
  end
  else // No input mask
  begin
    FRawText := ProcessedRawText; // Raw text is just the transformed input value
    FText := FRawText;       // Display text is also this raw text
    FMaskedText := FRawText;     // FMaskedText is same as FRawText when no mask
  end;

  if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);

  FCaretVisible := True;
  if Focused then
  begin
    FCaretTimer.Enabled := False;
    FCaretTimer.Enabled := True;
  end;

  if OldFDisplayText <> FText then
  begin
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TANDMR_CEdit.SetMaxLength(const Value: Integer);
var OldText: string; TextChanged: Boolean;
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Max(0, Value);
    TextChanged := False;
    OldText := FText;
    if (FMaxLength > 0) and (Length(FText) > FMaxLength) then
    begin
      FText := Copy(FText, 1, FMaxLength);
      if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);
      TextChanged := True;
    end;
    if TextChanged then
    begin
      FCaretVisible := True;
      if Focused then
      begin
        FCaretTimer.Enabled := False;
        FCaretTimer.Enabled := True;
      end;
      if Assigned(FOnChange) then FOnChange(Self);
      Invalidate;
    end;
  end;
end;

procedure TANDMR_CEdit.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetReadOnly(const Value: Boolean); begin if FReadOnly <> Value then begin FReadOnly := Value; Invalidate; end; end;

procedure TANDMR_CEdit.SetBorderSettings(const Value: TBorderSettings);
begin
  FBorderSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CEdit.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CEdit.ImageSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TANDMR_CEdit.SetImageSettings(const Value: TImageSettings);
begin
  FImageSettings.Assign(Value);
  Invalidate; // Or call ImageSettingsChanged if more logic is needed
end;

procedure TANDMR_CEdit.SetSeparatorSettings(const Value: TSeparatorSettings);
begin
  FSeparatorSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CEdit.CaretTimerTick(Sender: TObject); begin if Focused then begin FCaretVisible := not FCaretVisible; Invalidate; end else begin FCaretVisible := False; FCaretTimer.Enabled := False; Invalidate; end; end;

procedure TANDMR_CEdit.SetFocusSettings(const Value: TFocusSettings);
begin
  FFocusSettings.Assign(Value);
  Invalidate;
end;

procedure TANDMR_CEdit.SetOpacity(const Value: Byte); begin if FOpacity <> Value then begin FOpacity := Value; if FOpacity < 255 then begin ControlStyle := ControlStyle - [csOpaque]; if Parent <> nil then Parent.Invalidate; end else begin ControlStyle := ControlStyle + [csOpaque]; end; Invalidate; end; end;
procedure TANDMR_CEdit.SetCurrentCursor(const Value: TCursor); begin if FCurrentCursor <> Value then begin FCurrentCursor := Value; Self.Cursor := FCurrentCursor; end; end;
procedure TANDMR_CEdit.SetInputType(const Value: TInputType); begin if FInputType <> Value then begin FInputType := Value; end; end;
procedure TANDMR_CEdit.SetTextCase(const Value: TTextCase); var OldText: string; TransformedText: string; begin if FTextCase <> Value then begin OldText := FText; FTextCase := Value; TransformedText := FText; case FTextCase of tcUppercase: TransformedText := System.SysUtils.UpperCase(FText); tcLowercase: TransformedText := System.SysUtils.LowerCase(FText); end; if FText <> TransformedText then begin FText := TransformedText; FCaretPosition := Length(FText); if Assigned(FOnChange) then FOnChange(Self); Invalidate; end else if (OldText = TransformedText) and (Value <> tcNormal) then begin Invalidate; end; end; end;
procedure TANDMR_CEdit.SetInputMask(const Value: string); var OldRawText: string; begin if FInputMask <> Value then begin OldRawText := FRawText; FInputMask := Value; if FInputMask = '' then FPredefinedMask := pmtNone else FPredefinedMask := pmtCustom; SetText(OldRawText); Invalidate; end; end;
procedure TANDMR_CEdit.SetCaptionSettings(const Value: TCaptionSettings); begin FCaptionSettings.Assign(Value); CaptionSettingsChanged(Self); end;
procedure TANDMR_CEdit.CaptionSettingsChanged(Sender: TObject); begin Invalidate; end;
procedure TANDMR_CEdit.SetHoverSettings(const Value: THoverSettings); begin FHoverSettings.Assign(Value); HoverSettingsChanged(Self); end;
procedure TANDMR_CEdit.HoverSettingsChanged(Sender: TObject); begin if FHovered or (not FHoverSettings.Enabled) then Invalidate; end;
procedure TANDMR_CEdit.TextMarginsChanged(Sender: TObject); begin Invalidate; end;
procedure TANDMR_CEdit.SetTextMargins(const Value: TANDMR_Margins); begin FTextMargins.Assign(Value); end;
procedure TANDMR_CEdit.SetPredefinedMask(const Value: TPredefinedMaskType); var NewMaskValue: string; OldRawText: string; begin if FPredefinedMask <> Value then begin OldRawText := FRawText; FPredefinedMask := Value; case FPredefinedMask of pmtCPF: NewMaskValue := '000.000.000-00'; pmtCNPJ: NewMaskValue := '00.000.000/0000-00'; pmtCEP: NewMaskValue := '00000-000'; pmtPhoneBR: NewMaskValue := '(00) 90000-0000'; pmtDateDMY: NewMaskValue := '00/00/0000'; pmtCustom: Exit; pmtNone: NewMaskValue := ''; else NewMaskValue := ''; end; if FInputMask <> NewMaskValue then begin FInputMask := NewMaskValue; SetText(OldRawText); end; Invalidate; end; end;

procedure TANDMR_CEdit.SetStatus(const Value: TCEditStatus);
begin
  if FStatus <> Value then
  begin
    FStatus := Value;

    case FStatus of
      cepsNormal:
        begin
          FBorderSettings.Color := clBlack;
          FFocusSettings.BorderColor := clHighlight;
          Self.Font.Color := clWindowText;
          // Assuming FBorderSettings.BackgroundColor and FFocusSettings.BackgroundColor
          // are not changed by other styles or should revert to their own defaults.
          // If they need explicit reset, it would be:
          // FBorderSettings.BackgroundColor := clBtnFace; // or its original default
          // FFocusSettings.BackgroundColor := clWindow; // or its original default
        end;
      cepsError:
        begin
          FBorderSettings.Color := clRed;
          FFocusSettings.BorderColor := DarkerColor(clRed, 30);
          Self.Font.Color := clRed;
        end;
      cepsWarning:
        begin
          FBorderSettings.Color := TColor($004C92FF); // Orange
          FFocusSettings.BorderColor := DarkerColor(TColor($004C92FF), 30);
          Self.Font.Color := TColor($004C92FF); // Orange
        end;
      cepsSuccess:
        begin
          FBorderSettings.Color := TColor($00993300); // Green
          FFocusSettings.BorderColor := DarkerColor(TColor($00993300), 30);
          Self.Font.Color := TColor($00993300); // Green
        end;
    end;

    // Notify settings objects if they have dependent logic on their OnChange,
    // though Invalidate() below will cause a repaint that uses these values.
    // If FBorderSettings.Changed or FFocusSettings.Changed do more than just Invalidate,
    // they might be needed. For now, Paint reads these directly.
    // FBorderSettings.Changed;
    // FFocusSettings.Changed;

    Invalidate;
  end;
end;

procedure TANDMR_CEdit.CMMouseEnter(var Message: TMessage); begin inherited; if not FHovered then begin FHovered := True; end; FHoverSettings.StartAnimation(True); end;
procedure TANDMR_CEdit.CMMouseLeave(var Message: TMessage); begin inherited; if FHovered then begin FHovered := False; end; FHoverSettings.StartAnimation(False); end;

procedure TANDMR_CEdit.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
const
  CaptionLayoutOffset = 2; // Default offset if FCaptionSettings.Offset is not available
var
  WorkArea: TRect;
  ImgW, ImgH, SepW: Integer;
  FullClientRect: TRect;
  CaptionHeight, CaptionWidth: Integer;
  OriginalFont: TFont;
  OriginalImgW, OriginalImgH: Integer;
  availWForImg, availHForImg: Integer;
  rImageRatio, rAvailBoxRatio: Double; // For AutoSize = True
  tempW, tempH: Double; // For AutoSize = True
  targetW, targetH: Integer; // For AutoSize = False
  imgAspectRatio, targetAspectRatio: Single; // For AutoSize = False & Proportional
  slotStartX, slotAvailableWidth, slotEndX: Integer; // For Horizontal Alignment
  AvailHForImgLayoutAdjusted: Integer; // For Vertical Alignment
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
    case FCaptionSettings.Position of
      cpAbove: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Right, FullClientRect.Top + CaptionHeight); WorkArea.Top := FCaptionRect.Bottom + CaptionLayoutOffset; end;
      cpBelow: begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Bottom - CaptionHeight, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Bottom := FCaptionRect.Top - CaptionLayoutOffset; end;
      cpLeft:  begin FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Left + CaptionWidth, FullClientRect.Bottom); WorkArea.Left := FCaptionRect.Right + CaptionLayoutOffset; end;
      cpRight: begin FCaptionRect := Rect(FullClientRect.Right - CaptionWidth, FullClientRect.Top, FullClientRect.Right, FullClientRect.Bottom); WorkArea.Right := FCaptionRect.Left - CaptionLayoutOffset; end;
    end;
    if WorkArea.Bottom < WorkArea.Top then WorkArea.Bottom := WorkArea.Top;
    if WorkArea.Right < WorkArea.Left then WorkArea.Right := WorkArea.Left;
  end
  else
    WorkArea := FullClientRect;

  // ImagePlacementArea holds the WorkArea *before* border deflation
  var ImagePlacementArea: TRect;
  ImagePlacementArea := WorkArea;

  // The main WorkArea for text and separator should always be inside borders
  InflateRect(WorkArea, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

  // Now, determine the specific area for image placement
  if FImageSettings.Placement = iplInsideBounds then
  begin
    // For iplInsideBounds, the image is constrained by the same area as text/separator.
    ImagePlacementArea := WorkArea; // Use the border-deflated WorkArea
  end;
  // Else, for iplOutsideBounds, ImagePlacementArea remains the WorkArea before border deflation.

  outImgRect := Rect(0,0,0,0);
  outSepRect := Rect(0,0,0,0);
  outTxtRect := WorkArea; // TextRect is based on the (potentially) border-deflated WorkArea

  ImgW := 0; ImgH := 0;
  if FImageSettings.Visible and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
  begin
    OriginalImgW := FImageSettings.Picture.Graphic.Width;
    OriginalImgH := FImageSettings.Picture.Graphic.Height;

    if (OriginalImgW > 0) and (OriginalImgH > 0) then
    begin
      // Use ImagePlacementArea for image dimensioning
      availWForImg := ImagePlacementArea.Width - FImageSettings.Margins.Left - FImageSettings.Margins.Right;
      availHForImg := ImagePlacementArea.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
      availWForImg := Max(0, availWForImg);
      availHForImg := Max(0, availHForImg);

      if (availWForImg > 0) and (availHForImg > 0) then
      begin
        if FImageSettings.AutoSize then
        begin
          // AutoSize = True: Use available space
          case FImageSettings.DrawMode of
            idmProportional:
            begin
              rImageRatio := OriginalImgW / OriginalImgH;
              rAvailBoxRatio := availWForImg / availHForImg;
              if rAvailBoxRatio > rImageRatio then // Fit to height
              begin
                ImgH := availHForImg;
                tempW := availHForImg * rImageRatio;
                ImgW := Round(tempW);
                if (ImgW = 0) and (tempW > 0) then ImgW := 1;
              end
              else // Fit to width
              begin
                ImgW := availWForImg;
                if rImageRatio > 0 then
                begin
                  tempH := availWForImg / rImageRatio;
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
            idmNormal:
            begin
              ImgW := OriginalImgW;
              ImgH := OriginalImgH;
              // Image will be clipped by outImgRect calculation if larger than ImagePlacementArea margins
            end;
          else // Default for AutoSize True if unknown draw mode
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
              begin ImgW := 0; ImgH := 0; end
              else
              begin
                imgAspectRatio := OriginalImgW / OriginalImgH;
                targetAspectRatio := targetW / targetH;
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
            idmNormal:
            begin
              ImgW := OriginalImgW;
              ImgH := OriginalImgH;
              // Optional: ImgW := Min(ImgW, targetW); ImgH := Min(ImgH, targetH);
            end;
          else // Default for AutoSize False if unknown draw mode
            ImgW := 0; ImgH := 0;
          end;
        end;
      end
      else // No available space for image if margins consume all space
      begin
        ImgW := 0; ImgH := 0;
      end;
    end
    else // Original image has no dimensions
    begin
      ImgW := 0; ImgH := 0;
    end;
  end
  else // Image not visible or no graphic
  begin
    ImgW := 0; ImgH := 0;
  end;

  ImgW := Max(0, ImgW);
  ImgH := Max(0, ImgH);

  if FImageSettings.Visible and (ImgW > 0) and (ImgH > 0) then
  begin
    if FImageSettings.Position = ipsLeft then
    begin
      slotStartX := ImagePlacementArea.Left + FImageSettings.Margins.Left;
      slotAvailableWidth := availWForImg; // Calculated based on ImagePlacementArea and ImageSettings.Margins
      case FImageSettings.HorizontalAlign of
        ihaLeft:   outImgRect.Left := slotStartX;
        ihaCenter: outImgRect.Left := slotStartX + (slotAvailableWidth - ImgW) div 2;
        ihaRight:  outImgRect.Left := slotStartX + slotAvailableWidth - ImgW;
      else         outImgRect.Left := slotStartX + (slotAvailableWidth - ImgW) div 2; // Default center
      end;
      outImgRect.Right := outImgRect.Left + ImgW;
    end
    else // ipsRight
    begin
      slotEndX := ImagePlacementArea.Right - FImageSettings.Margins.Right;
      slotAvailableWidth := availWForImg;
      case FImageSettings.HorizontalAlign of
        ihaLeft:   outImgRect.Left := slotEndX - slotAvailableWidth;
        ihaCenter: outImgRect.Left := slotEndX - slotAvailableWidth + (slotAvailableWidth - ImgW) div 2;
        ihaRight:  outImgRect.Left := slotEndX - ImgW;
      else         outImgRect.Left := slotEndX - slotAvailableWidth + (slotAvailableWidth - ImgW) div 2; // Default center
      end;
      outImgRect.Right := outImgRect.Left + ImgW;
    end;

    AvailHForImgLayoutAdjusted := ImagePlacementArea.Height - FImageSettings.Margins.Top - FImageSettings.Margins.Bottom;
    AvailHForImgLayoutAdjusted := Max(0, AvailHForImgLayoutAdjusted);

    case FImageSettings.AlignmentVertical of
      iavTop:    outImgRect.Top := ImagePlacementArea.Top + FImageSettings.Margins.Top;
      iavCenter: outImgRect.Top := ImagePlacementArea.Top + FImageSettings.Margins.Top + (AvailHForImgLayoutAdjusted - ImgH) div 2;
      iavBottom: outImgRect.Top := ImagePlacementArea.Bottom - FImageSettings.Margins.Bottom - ImgH;
    end;
    outImgRect.Bottom := outImgRect.Top + ImgH;

    // Clip outImgRect against ImagePlacementArea
    if outImgRect.Left < ImagePlacementArea.Left then outImgRect.Left := ImagePlacementArea.Left;
    if outImgRect.Right > ImagePlacementArea.Right then outImgRect.Right := ImagePlacementArea.Right;
    if outImgRect.Top < ImagePlacementArea.Top then outImgRect.Top := ImagePlacementArea.Top;
    if outImgRect.Bottom > ImagePlacementArea.Bottom then outImgRect.Bottom := ImagePlacementArea.Bottom;
    if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
    if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;

    ImgW := outImgRect.Width;
    ImgH := outImgRect.Height;
  end
  else
  begin
    ImgW := 0; ImgH := 0;
    outImgRect := Rect(0,0,0,0);
  end;

  SepW := 0;
  if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) then SepW := FSeparatorSettings.Thickness;

  if SepW > 0 then
  begin
    if FImageSettings.Visible and (ImgW > 0) then
    begin
      if FImageSettings.Position = ipsLeft then outSepRect.Left := outImgRect.Right + FImageSettings.Margins.Right + FSeparatorSettings.Padding
      else outSepRect.Left := outImgRect.Left - FImageSettings.Margins.Left - FSeparatorSettings.Padding - SepW;
    end
    else
    begin
      if FImageSettings.Position = ipsLeft then outSepRect.Left := WorkArea.Left + FSeparatorSettings.Padding
      else outSepRect.Left := WorkArea.Right - FSeparatorSettings.Padding - SepW;
    end;
    outSepRect.Right := outSepRect.Left + SepW;

    var SepH: Integer;
    SepH := 0;
    case FSeparatorSettings.HeightMode of
      shmFull: SepH := WorkArea.Height;
      shmAsText: SepH := outTxtRect.Height; // Note: outTxtRect height not yet defined here, might need adjustment or use WorkArea (border-deflated) height
      shmAsImage: if FImageSettings.Visible and (ImgH > 0) then SepH := ImgH else SepH := WorkArea.Height; // WorkArea (border-deflated)
      shmCustom: if FSeparatorSettings.CustomHeight > 0 then SepH := FSeparatorSettings.CustomHeight else SepH := WorkArea.Height; // WorkArea (border-deflated)
    end;
    SepH := Max(0, SepH);
    outSepRect.Top := WorkArea.Top + (WorkArea.Height - SepH) div 2; // Position within border-deflated WorkArea
    outSepRect.Bottom := outSepRect.Top + SepH;

    // Clip separator against border-deflated WorkArea
    if outSepRect.Left < WorkArea.Left then outSepRect.Left := WorkArea.Left; if outSepRect.Right > WorkArea.Right then outSepRect.Right := WorkArea.Right;
    if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top; if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
    if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left; if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
    SepW := outSepRect.Width;
  end
  else
  begin SepW := 0; outSepRect := Rect(0,0,0,0); end;

  if FImageSettings.Visible and (ImgW > 0) then
  begin
    if FImageSettings.Position = ipsLeft then
    begin
      outTxtRect.Left := outImgRect.Right + FImageSettings.Margins.Right;
      if SepW > 0 then outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding;
    end
    else
    begin
      outTxtRect.Right := outImgRect.Left - FImageSettings.Margins.Left;
      if SepW > 0 then outTxtRect.Right := outSepRect.Left - FSeparatorSettings.Padding;
    end;
  end
  else if SepW > 0 then
  begin
    if FImageSettings.Position = ipsLeft then outTxtRect.Left := outSepRect.Right + FSeparatorSettings.Padding
    else outTxtRect.Right := outSepRect.Left - FSeparatorSettings.Padding;
  end;

  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left; if outTxtRect.Left < WorkArea.Left then outTxtRect.Left := WorkArea.Left; if outTxtRect.Right > WorkArea.Right then outTxtRect.Right := WorkArea.Right; if outTxtRect.Top < WorkArea.Top then outTxtRect.Top := WorkArea.Top; if outTxtRect.Bottom > WorkArea.Bottom then outTxtRect.Bottom := WorkArea.Bottom; if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
end;

procedure TANDMR_CEdit.Paint;
var
  LG: TGPGraphics;
  TextToDisplay: string;
  TextFlags: Cardinal;
  imgR, txtR, sepR: TRect;
  PaddedTextDrawArea: TRect;
  FullClientRect: TRect;
  ActualEditBGColor, ActualEditBorderColor, ActualEditTextColor, ActualCaptionTextColor: TColor;
  EditBoxDrawingRect: TRect;
  BGForDrawEditBox: TColor;
begin
  FullClientRect := Self.ClientRect;
  // FCaptionRect is calculated by CalculateLayout and is available as a field
  CalculateLayout(imgR, txtR, sepR);

  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      var LHoverProgress: Single := FHoverSettings.CurrentAnimationValue / 255.0;
      var TrueBaseComponentBG, TrueBaseEditBG, HoverEditBGFromSettings, FocusEditBGFromSettings, DisabledEditBGForResolve: TColor;
      var TrueBaseBorderCol, HoverBorderColFromSettings, FocusBorderColFromSettings, DisabledBorderColForResolve: TColor;
      var TrueBaseTextCol, HoverTextColFromSettings, FocusTextColFromSettings, DisabledTextColForResolve: TColor;
      var TrueBaseCaptionCol, HoverCaptionColFromSettings, FocusCaptionColFromSettings, DisabledCaptionColForResolve: TColor;

      var NonHoveredComponentBGColor, TargetStateComponentBGColor, ResolvedComponentFrameBG: TColor;
      var NonHoveredTextAreaBGColor, TargetStateTextAreaBGColor, ActualTextAreaBGColor: TColor; // Renamed ActualEditBGColor
      var NonHoveredBorderColor, TargetStateBorderColor, ActualEditBorderColor: TColor;
      var NonHoveredTextColor, TargetStateTextColor, ActualEditTextColor: TColor;
      var NonHoveredCaptionColor, TargetStateCaptionColor, ActualCaptionTextColor: TColor;

      // Define base colors
      TrueBaseComponentBG := FBorderSettings.BackgroundColor; // Background for the entire component frame
      TrueBaseEditBG := IfThen(FImageSettings.Placement = iplInsideBounds, FBorderSettings.BackgroundColor, clWindow); // Background specific to text area
      TrueBaseBorderCol := FBorderSettings.Color;
      TrueBaseTextCol := Self.Font.Color;
      TrueBaseCaptionCol := IfThen(FCaptionSettings.Color = clDefault, Self.Font.Color, FCaptionSettings.Color);

      // Define hover state colors from settings
      HoverEditBGFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone); // This might apply to component or text area based on context
      HoverBorderColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
      HoverTextColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.FontColor, clNone);
      HoverCaptionColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);

      // Define focus state colors from settings
      FocusEditBGFromSettings := IfThen(FFocusSettings.BackgroundColorVisible, FFocusSettings.BackgroundColor, clNone); // This might apply to component or text area
      FocusBorderColFromSettings := FFocusSettings.BorderColor;
      FocusTextColFromSettings := TrueBaseTextCol; // Typically text color doesn't change on focus itself, but background/border do
      FocusCaptionColFromSettings := TrueBaseCaptionCol;

      // Define disabled state colors (these are usually fixed, not from settings directly)
      DisabledEditBGForResolve := TrueBaseEditBG; // For text area
      DisabledBorderColForResolve := TrueBaseBorderCol;
      DisabledTextColForResolve := IfThen(TrueBaseTextCol = clWindowText, clGrayText, DarkerColor(TrueBaseTextCol, 50));
      DisabledCaptionColForResolve := IfThen(TrueBaseCaptionCol = clWindowText, clGrayText, DarkerColor(TrueBaseCaptionCol,50));

      // Resolve Non-Hovered Colors
      NonHoveredComponentBGColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseComponentBG, clNone, IfThen(FFocusSettings.BackgroundColorVisible, FFocusSettings.BackgroundColor, clNone), TrueBaseComponentBG, FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      NonHoveredTextAreaBGColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseEditBG, clNone, FocusEditBGFromSettings, DisabledEditBGForResolve, FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      NonHoveredBorderColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseBorderCol, clNone, FocusBorderColFromSettings, DisabledBorderColForResolve, FHoverSettings.Enabled, FFocusSettings.BorderColorVisible, False);
      NonHoveredTextColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseTextCol, clNone, FocusTextColFromSettings, DisabledTextColForResolve, FHoverSettings.Enabled, False, False);
      NonHoveredCaptionColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseCaptionCol, clNone, FocusCaptionColFromSettings, DisabledCaptionColForResolve, FHoverSettings.Enabled, False, False);

      // Resolve Target State Colors (Hovered or Focused)
      TargetStateComponentBGColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseComponentBG, IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone), IfThen(FFocusSettings.BackgroundColorVisible, FFocusSettings.BackgroundColor, clNone), TrueBaseComponentBG, FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      TargetStateTextAreaBGColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseEditBG, HoverEditBGFromSettings, FocusEditBGFromSettings, DisabledEditBGForResolve, FHoverSettings.Enabled, FFocusSettings.BackgroundColorVisible, True);
      TargetStateBorderColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseBorderCol, HoverBorderColFromSettings, FocusBorderColFromSettings, DisabledBorderColForResolve, FHoverSettings.Enabled, FFocusSettings.BorderColorVisible, False);
      TargetStateTextColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseTextCol, HoverTextColFromSettings, FocusTextColFromSettings, DisabledTextColForResolve, FHoverSettings.Enabled, False, False);
      TargetStateCaptionColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseCaptionCol, HoverCaptionColFromSettings, FocusCaptionColFromSettings, DisabledCaptionColForResolve, FHoverSettings.Enabled, False, False);

      // Blend for Hover Animation
      if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FHovered then
      begin
        ResolvedComponentFrameBG := BlendColors(NonHoveredComponentBGColor, TargetStateComponentBGColor, LHoverProgress);
        ActualTextAreaBGColor := BlendColors(NonHoveredTextAreaBGColor, TargetStateTextAreaBGColor, LHoverProgress);
        ActualEditBorderColor := BlendColors(NonHoveredBorderColor, TargetStateBorderColor, LHoverProgress);
        ActualEditTextColor := BlendColors(NonHoveredTextColor, TargetStateTextColor, LHoverProgress);
        ActualCaptionTextColor := BlendColors(NonHoveredCaptionColor, TargetStateCaptionColor, LHoverProgress);
      end
      else
      begin
        ResolvedComponentFrameBG := TargetStateComponentBGColor;
        ActualTextAreaBGColor := TargetStateTextAreaBGColor;
        ActualEditBorderColor := TargetStateBorderColor;
        ActualEditTextColor := TargetStateTextColor;
        ActualCaptionTextColor := TargetStateCaptionColor;
      end;

      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color; // Parent background color
        Canvas.FillRect(FullClientRect);
      end;

      // Calculate OverallBorderedRect for drawing the main border
      var OverallBorderedRect: TRect;
      const CaptionLayoutOffsetConst = 2; // Match CalculateLayout
      OverallBorderedRect := FullClientRect;
      if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
      begin
        case FCaptionSettings.Position of
          cpAbove: OverallBorderedRect.Top := FCaptionRect.Bottom + CaptionLayoutOffsetConst;
          cpBelow: OverallBorderedRect.Bottom := FCaptionRect.Top - CaptionLayoutOffsetConst;
          cpLeft:  OverallBorderedRect.Left := FCaptionRect.Right + CaptionLayoutOffsetConst;
          cpRight: OverallBorderedRect.Right := FCaptionRect.Left - CaptionLayoutOffsetConst;
        end;
        if OverallBorderedRect.Bottom < OverallBorderedRect.Top then OverallBorderedRect.Bottom := OverallBorderedRect.Top;
        if OverallBorderedRect.Right < OverallBorderedRect.Left then OverallBorderedRect.Right := OverallBorderedRect.Left;
      end;
      if FBorderSettings.Thickness > 0 then
        InflateRect(OverallBorderedRect, -FBorderSettings.Thickness, -FBorderSettings.Thickness);

      // Draw the main component border and background
      DrawEditBox(LG, OverallBorderedRect, ResolvedComponentFrameBG, ActualEditBorderColor, FBorderSettings.Thickness, FBorderSettings.Style, FBorderSettings.CornerRadius, FBorderSettings.RoundCornerType, FOpacity);

      // Fill the text area (txtR) if its background is different from the component frame's background
      // This is especially true if iplOutsideBounds (clWindow based) or if themes cause divergence.
      if (ActualTextAreaBGColor <> ResolvedComponentFrameBG) or (FImageSettings.Placement = iplOutsideBounds) then
      begin
        if (txtR.Width > 0) and (txtR.Height > 0) then
        begin
          var BrushTxt: TGPBrush;
          BrushTxt := TGPSolidBrush.Create(ColorToARGB(ActualTextAreaBGColor, FOpacity));
          try
            LG.FillRectangle(BrushTxt, txtR.Left, txtR.Top, txtR.Width, txtR.Height);
          finally
            BrushTxt.Free;
          end;
        end;
      end;

      if FImageSettings.Visible and Assigned(FImageSettings.Picture.Graphic) and not FImageSettings.Picture.Graphic.Empty then
      begin
        if (FImageSettings.Picture.Graphic is TPNGImage) then
          DrawPNGImageWithGDI(LG, FImageSettings.Picture.Graphic as TPNGImage, imgR, idmStretch)
        else
          DrawNonPNGImageWithCanvas(Canvas, FImageSettings.Picture.Graphic, imgR, idmStretch);
      end;

      if FSeparatorSettings.Visible and (FSeparatorSettings.Thickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
        DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorSettings.Color, FSeparatorSettings.Thickness);

      if Self.Focused and FFocusSettings.UnderlineVisible and (FFocusSettings.UnderlineThickness > 0) then
      begin
        var UnderlineY: Integer;
        var UnderlinePen: TGPPen;
        if FBorderSettings.Thickness > 0 then
          UnderlineY := EditBoxDrawingRect.Bottom - FBorderSettings.Thickness - (FFocusSettings.UnderlineThickness div 2)
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
          LG.DrawLine(UnderlinePen, EditBoxDrawingRect.Left + FBorderSettings.Thickness, UnderlineY, EditBoxDrawingRect.Right - FBorderSettings.Thickness, UnderlineY);
        finally
          UnderlinePen.Free;
        end;
      end;
    finally
      LG.Free;
    end;

    PaddedTextDrawArea := txtR;
    PaddedTextDrawArea.Left := txtR.Left + FTextMargins.Left;
    PaddedTextDrawArea.Top := txtR.Top + FTextMargins.Top;
    PaddedTextDrawArea.Right := txtR.Right - FTextMargins.Right;
    PaddedTextDrawArea.Bottom := txtR.Bottom - FTextMargins.Bottom;

    if PaddedTextDrawArea.Right < PaddedTextDrawArea.Left then PaddedTextDrawArea.Right := PaddedTextDrawArea.Left;
    if PaddedTextDrawArea.Bottom < PaddedTextDrawArea.Top then PaddedTextDrawArea.Bottom := PaddedTextDrawArea.Top;

    if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
      TextToDisplay := StringOfChar(FPasswordChar, Length(FText))
    else
      TextToDisplay := FText;

    Canvas.Font.Assign(Self.Font);
    Canvas.Font.Color := ActualEditTextColor;
    Canvas.Brush.Style := bsClear;
    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if (Length(TextToDisplay) > 0) and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), PaddedTextDrawArea, TextFlags);

    if Self.Focused and FCaretVisible and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
    begin
      var CaretXBase, CaretTop, CaretHeight, CaretXOffset: Integer;
      var TextBeforeCaretVisible: string;
      if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
        TextBeforeCaretVisible := StringOfChar(FPasswordChar, FCaretPosition)
      else
        TextBeforeCaretVisible := Copy(FText, 1, FCaretPosition);

      CaretXBase := PaddedTextDrawArea.Left;
      CaretHeight := Canvas.TextHeight('Tg');
      CaretTop := PaddedTextDrawArea.Top + (PaddedTextDrawArea.Height - CaretHeight) div 2;
      CaretXOffset := Canvas.TextWidth(TextBeforeCaretVisible);
      Canvas.Pen.Color := ActualEditTextColor;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(CaretXBase + CaretXOffset, CaretTop);
      Canvas.LineTo(CaretXBase + CaretXOffset, CaretTop + CaretHeight);
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

procedure TANDMR_CEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FCaretVisible := True;
  FCaretTimer.Enabled := True;
  Self.Cursor := FCurrentCursor;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  Invalidate;
end;

procedure TANDMR_CEdit.CMExit(var Message: TCMExit);
var
  TempText: string;
  OriginalFText: string;
begin
  OriginalFText := FText;

  if FTextCase <> tcNormal then
  begin
    TempText := FText;
    case FTextCase of
      tcUppercase: TempText := System.SysUtils.UpperCase(FText);
      tcLowercase: TempText := System.SysUtils.LowerCase(FText);
    end;
    if FText <> TempText then
    begin
        FText := TempText;
        FCaretPosition := Length(FText);
        if Assigned(FOnChange) and (OriginalFText <> FText) then
          FOnChange(Self);
    end;
  end;

  inherited;
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Self.Cursor := crDefault;

  if Assigned(FOnExit) then
    FOnExit(Self);

  if OriginalFText <> FText then
    Invalidate
  else
    Invalidate; // Invalidate even if text didn't change, to remove focus visuals
end;

procedure TANDMR_CEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldText: string;
  Changed: Boolean;
begin
  inherited KeyDown(Key, Shift);
  Changed := False;
  OldText := FText;

  if FReadOnly and not (Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_TAB, VK_RETURN]) then
  begin
    if not ( (Key = Ord('C')) and (ssCtrl in Shift) ) then // Allow Ctrl+C for copy
    begin
        Key := 0;
        Exit;
    end;
  end;

  case Key of
    VK_BACK:
      begin
        if FReadOnly then Exit;
        if FInputMask <> '' then
        begin
          if Length(FRawText) > 0 then
          begin
            FRawText := Copy(FRawText, 1, Length(FRawText) - 1);
            // var OldDisplayText: string := FText; // Not needed as SetText handles comparison
            SetText(FRawText); // This updates FText, FMaskedText, FRawText, handles TextCase, OnChange, Invalidate

            var NewCaretPosInMask: Integer := 0;
            var TempRawLen: Integer := 0;
            var MaskIdx: Integer;
            for MaskIdx := 1 to Length(FMaskedText) do // FMaskedText is updated by SetText
            begin
              if not (FInputMask[MaskIdx] IN ['9','L','A','#','0']) then // Check against mask definition
              begin
                Inc(NewCaretPosInMask);
              end
              else // It's a placeholder in the mask
              begin
                if TempRawLen < Length(FRawText) then // If there's raw text for this placeholder
                begin
                  Inc(TempRawLen);
                  Inc(NewCaretPosInMask);
                end
                else if TempRawLen = Length(FRawText) then // This is the first placeholder after the new raw text ends
                begin
                    Inc(NewCaretPosInMask); // Caret should be at this empty placeholder
                    Break;
                end
                else // Should not happen if FMaskedText is correctly built
                begin
                    Inc(NewCaretPosInMask); // Fallback, move to this placeholder
                    Break;
                end;
              end;
              if MaskIdx = Length(FMaskedText) then NewCaretPosInMask := Length(FMaskedText); // If loop finishes, caret at end
            end;
            FCaretPosition := NewCaretPosInMask;
            if FCaretPosition > Length(FText) then FCaretPosition := Length(FText); // FText is display text
          end;
          Key := 0;
          Exit; // SetText has handled invalidation and OnChange if needed
        end
        else // No input mask
        begin
          if FCaretPosition > 0 then
          begin
            FText := Copy(FText, 1, FCaretPosition - 1) + Copy(FText, FCaretPosition + 1, MaxInt);
            Dec(FCaretPosition);
            Changed := True;
          end;
        end;
      end;
    VK_DELETE:
      begin
        if FReadOnly then Exit;
        if FCaretPosition < Length(FText) then
        begin
          // TODO: Add mask handling for VK_DELETE similar to VK_BACK if complex interaction is needed.
          // For now, simple delete on FText, then SetText will re-apply mask.
          var TempUnmasked: string;
          if FInputMask <> '' then
          begin
            // This is a simplified approach for delete with mask.
            // A more robust solution would identify the raw character at caret and remove it.
            // For now, we delete from FText and let SetText rebuild.
            TempUnmasked := FRawText; // Preserve current raw text
            FText := Copy(FText, 1, FCaretPosition) + Copy(FText, FCaretPosition + 2, MaxInt);
            // Attempt to rebuild raw text from the modified masked text. This is imperfect.
            // A better way is to adjust FRawText based on what mask placeholder was affected.
            // For now, we'll just re-set with the potentially modified raw text or let SetText(FText) try to parse.
            // This part needs careful review for complex masks.
            // Let's assume for now we operate on FRawText if possible.
            // This section for VK_DELETE with mask needs more robust logic.
            // As a placeholder for more complex logic:
            if Length(FRawText) > FCaretPosition then // Simplified: Try to remove char from raw text if caret is within its effective length
                 FRawText := Copy(FRawText, 1, FCaretPosition) + Copy(FRawText, FCaretPosition + 2, MaxInt);
            SetText(FRawText); // Re-apply mask and update
            // Caret position after SetText might need re-adjustment based on mask.
            // FCaretPosition might be implicitly set by SetText or needs explicit setting here.
          end
          else
          begin
            FText := Copy(FText, 1, FCaretPosition) + Copy(FText, FCaretPosition + 2, MaxInt);
          end;
          Changed := True;
        end;
      end;
    VK_HOME:  begin FCaretPosition := 0; Changed := True; end;
    VK_END:   begin FCaretPosition := Length(FText); Changed := True; end;
    VK_LEFT:  begin if FCaretPosition > 0 then Dec(FCaretPosition); Changed := True; end;
    VK_RIGHT: begin if FCaretPosition < Length(FText) then Inc(FCaretPosition); Changed := True; end;
  else
    Exit; // Not a key we handle in this specific block
  end;

  Key := 0; // Mark key as handled by this KeyDown logic

  if Changed then
  begin
    FCaretVisible := True;
    if Focused then
    begin
      FCaretTimer.Enabled := False;
      FCaretTimer.Enabled := True;
    end;

    if Assigned(FOnChange) and (OldText <> FText) then
    begin
      FOnChange(Self);
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.KeyPress(var Key: Char);
var
  OldText: string;
  AllowChar: Boolean;
begin
  inherited KeyPress(Key);

  if Key = #8 then // Backspace is handled in KeyDown
  begin
    Key := #0;
    Exit;
  end;

  if FReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  if (FInputType <> itNormal) and (Key >= ' ') then
  begin
    AllowChar := True;
    case FInputType of
      itLettersOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z'];
      itNumbersOnly: AllowChar := Key IN ['0'..'9'];
      itAlphaNumericOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9'];
      itNoSpecialChars: AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9', ' '];
    end;
    if not AllowChar then
    begin
      Key := #0;
    end;
  end;

  if (Key >= ' ') then // Process printable characters
  begin
    case FTextCase of
      tcUppercase: Key := System.Character.ToUpper(Key);
      tcLowercase: Key := System.Character.ToLower(Key);
    end;

    if FInputMask <> '' then
    begin
      var MaskPlaceholdersCount: Integer := 0;
      var i: Integer;
      for i := 1 to Length(FInputMask) do
        if FInputMask[i] IN ['9','0','L','A','#'] then Inc(MaskPlaceholdersCount);

      if Length(FRawText) >= MaskPlaceholdersCount then // Already full according to mask
      begin
          Key := #0;
          Exit;
      end;

      var TempRawTextForValidation: string := FRawText + Key;
      var BuildRawText: string := '';
      var BuildMaskedText: string := '';
      var RawIdx: Integer := 1;
      var MaskIdx: Integer;
      var MaskDefChar: Char;
      var IsLit: Boolean;
      var CharToIns: Char;
      var CharOK: Boolean;
      var NextCaretPosInMask: Integer := -1;

      for MaskIdx := 1 to Length(FInputMask) do
      begin
        MaskDefChar := FInputMask[MaskIdx];
        IsLit := not (MaskDefChar IN ['9', '0', 'L', 'A', '#']);
        if IsLit then
        begin
          BuildMaskedText := BuildMaskedText + MaskDefChar;
          // If caret was at the end of raw text, and we encounter a literal,
          // the caret should effectively jump after this literal if we are inserting.
          if (NextCaretPosInMask = -1) and (RawIdx > Length(BuildRawText)) then
             NextCaretPosInMask := Length(BuildMaskedText);

        end
        else // Placeholder
        begin
          if RawIdx <= Length(TempRawTextForValidation) then
          begin
            CharToIns := TempRawTextForValidation[RawIdx];
            CharOK := False;
            case MaskDefChar of
              '9', '0': CharOK := CharToIns IN ['0'..'9'];
              'L': CharOK := System.Character.IsLetter(CharToIns);
              'A': CharOK := System.Character.IsLetterOrDigit(CharToIns);
              '#': CharOK := True;
            end;

            if CharOK then
            begin
              BuildMaskedText := BuildMaskedText + CharToIns;
              BuildRawText := BuildRawText + CharToIns;
              if RawIdx = Length(TempRawTextForValidation) then // This is the char just inserted
                 NextCaretPosInMask := Length(BuildMaskedText); // Caret after this char
              Inc(RawIdx);
            end
            else // Char not allowed by mask definition
            begin
              Key := #0; // Reject key
              Exit;
            end;
          end
          else // Mask expects more, but no more raw text (including the new Key)
          begin
            BuildMaskedText := BuildMaskedText + '_'; // Placeholder for empty part
            if NextCaretPosInMask = -1 then // First available spot for caret
                NextCaretPosInMask := Length(BuildMaskedText);
          end;
        end;
      end;

      // If Key is still valid (not #0)
      if Key <> #0 then
      begin
        OldText := FText; // FText holds the old FMaskedText
        FRawText := BuildRawText;
        FText := BuildMaskedText;
        FMaskedText := BuildMaskedText;

        // Adjust caret to be after the inserted character, or at the next available placeholder
        var CurrentRawLenProcessed: Integer := 0;
        var CaretFound: Boolean := False;
        FCaretPosition := 0;
        for i := 1 to Length(FMaskedText) do
        begin
            FCaretPosition := i;
            if not (FInputMask[i] IN ['9','0','L','A','#']) then // Literal
            begin
                if CurrentRawLenProcessed = Length(FRawText) then // If all raw chars placed, and this is first literal after
                begin
                    CaretFound := True;
                    Break;
                end;
            end
            else // Placeholder
            begin
                Inc(CurrentRawLenProcessed);
                if CurrentRawLenProcessed = Length(FRawText) +1 then // This is the placeholder AFTER the last raw char
                begin
                   CaretFound := True;
                   Break;
                end
                else if CurrentRawLenProcessed > Length(FRawText) then // Should be the first empty placeholder
                begin
                    CaretFound := True;
                    Break;
                end;
            end;
        end;
        if not CaretFound then FCaretPosition := Length(FText); // Fallback to end


        if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);

        FCaretVisible := True;
        if Focused then
        begin
          FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
        end;
        if OldText <> FText then
        begin
          Invalidate;
          if Assigned(FOnChange) then FOnChange(Self);
        end;
      end;
      Key := #0; // Mark Key as handled by mask logic
      Exit;
    end
    else // No InputMask
    begin
      OldText := FText;
      if (FMaxLength > 0) and (Length(FText) >= FMaxLength) then
      begin
        Key := #0; // Prevent insertion if max length reached
        Exit;
      end;

      if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);
      FText := Copy(FText, 1, FCaretPosition) + Key + Copy(FText, FCaretPosition + 1, MaxInt);
      Inc(FCaretPosition);

      FCaretVisible := True;
      if Focused then
      begin
        FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
      end;
      if OldText <> FText then
      begin
        Invalidate;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    end;
  end;
  Key := #0; // Mark key as handled or rejected
end;

procedure TANDMR_CEdit.Click;
begin
  inherited Click;
  // Additional click logic if needed, though MouseDown usually handles focus/caret
end;

procedure TANDMR_CEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  ClickX_RelativeToPaddedText: Integer;
  CurrentWidth: Integer;
  CharWidth: Integer;
  TextToMeasure: string;
  LayoutImgRect, LayoutTxtRect, LayoutSepRect: TRect;
  PaddedTextClickArea: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  Self.CalculateLayout(LayoutImgRect, LayoutTxtRect, LayoutSepRect);

  PaddedTextClickArea := LayoutTxtRect;
  PaddedTextClickArea.Left := LayoutTxtRect.Left + FTextMargins.Left;
  PaddedTextClickArea.Top := LayoutTxtRect.Top + FTextMargins.Top;
  PaddedTextClickArea.Right := LayoutTxtRect.Right - FTextMargins.Right;
  PaddedTextClickArea.Bottom := LayoutTxtRect.Bottom - FTextMargins.Bottom;

  if PaddedTextClickArea.Right < PaddedTextClickArea.Left then PaddedTextClickArea.Right := PaddedTextClickArea.Left;
  if PaddedTextClickArea.Bottom < PaddedTextClickArea.Top then PaddedTextClickArea.Bottom := PaddedTextClickArea.Top;

  if Button = mbLeft then
  begin
    if CanFocus then
    begin
      if not Focused then SetFocus
      else // Already focused, ensure caret is visible and timer reset
      begin
        FCaretVisible := True;
        FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
      end;
    end
    else // Cannot focus (e.g., not enabled)
      Exit;

    // Determine caret position based on click coordinates
    if not PtInRect(PaddedTextClickArea, Point(X,Y)) then // Click outside padded text area
    begin
      // If click is to the left of text area, caret at beginning
      if X < PaddedTextClickArea.Left then FCaretPosition := 0
      // If click is to the right, caret at end
      else if X >= PaddedTextClickArea.Right then FCaretPosition := Length(FText)
      // If click is above or below, but horizontally within some range, decide (e.g. closest end)
      // This part can be refined, for now, if not left or right, it implies it's outside vertically
      // or in a non-text part of the component. Let's default to end or beginning based on X.
      else if X < (PaddedTextClickArea.Left + PaddedTextClickArea.Width div 2) then FCaretPosition := 0
      else FCaretPosition := Length(FText);
    end
    else // Click is within the padded text area
    begin
        ClickX_RelativeToPaddedText := X - PaddedTextClickArea.Left;

        if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
          TextToMeasure := StringOfChar(FPasswordChar, Length(FText))
        else
          TextToMeasure := FText;

        Canvas.Font.Assign(Self.Font);
        CurrentWidth := 0;
        FCaretPosition := 0; // Default to beginning

        for I := 1 to Length(TextToMeasure) do
        begin
          CharWidth := Canvas.TextWidth(TextToMeasure[I]);
          // If click is before the middle of the current character
          if ClickX_RelativeToPaddedText < (CurrentWidth + CharWidth div 2) then
          begin
            FCaretPosition := I - 1;
            Break;
          end;
          CurrentWidth := CurrentWidth + CharWidth;
          FCaretPosition := I; // If click is after this char's midpoint, caret goes after it
        end;
        // If loop completed and click was beyond all characters, FCaretPosition is already Length(TextToMeasure)
    end;

    Invalidate; // To redraw caret and focus visuals
  end;
end;

end.
