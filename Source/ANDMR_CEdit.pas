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
    FText: string;
    FMaxLength: Integer;
    FPasswordChar: Char;
    FReadOnly: Boolean;
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
    FCaretVisible: Boolean;
    FCaretPosition: Integer;
    FCaretTimer: TTimer;

    // New private fields
    FFocusBorderColor: TColor;
    FFocusBorderColorVisible: Boolean;
    FFocusBackgroundColor: TColor;
    FFocusBackgroundColorVisible: Boolean;
    FFocusUnderlineColor: TColor;
    FFocusUnderlineVisible: Boolean;
    FFocusUnderlineThickness: Integer;
    FFocusUnderlineStyle: TPenStyle;
    FOpacity: Byte;
    FCurrentCursor: TCursor;
    FInputType: TInputType;
    FTextCase: TTextCase;
    FInputMask: string;
    FMaskedText: string;
    FRawText: string;
    FCaptionSettings: TCaptionSettings;
    FCaptionRect: TRect;
    FHoverSettings: THoverSettings; // New Field
    FHovered: Boolean;
    FTextMargins: TANDMR_Margins; // New Field for Text Margins
    FPredefinedMask: TPredefinedMaskType; // New Field for Predefined Mask
    FOnExit: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;             // New Field

    procedure SetText(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetPasswordChar(const Value: Char);
    procedure SetReadOnly(const Value: Boolean);
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
    procedure ImageChanged(Sender: TObject);
    procedure ImageMarginsChanged(Sender: TObject);
    procedure SetImagePlacement(const Value: TImagePlacement);
    procedure SetImageDrawMode(const Value: TImageDrawMode);
    procedure SetSeparatorVisible(const Value: Boolean);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSeparatorThickness(const Value: Integer);
    procedure SetSeparatorPadding(const Value: Integer);
    procedure SetSeparatorHeightMode(const Value: TSeparatorHeightMode);
    procedure SetSeparatorCustomHeight(const Value: Integer);
    procedure CaretTimerTick(Sender: TObject);

    // New Set methods
    procedure SetFocusBorderColor(const Value: TColor);
    procedure SetFocusBorderColorVisible(const Value: Boolean);
    procedure SetFocusBackgroundColor(const Value: TColor);
    procedure SetFocusBackgroundColorVisible(const Value: Boolean);
    procedure SetFocusUnderlineColor(const Value: TColor);
    procedure SetFocusUnderlineVisible(const Value: Boolean);
    procedure SetFocusUnderlineThickness(const Value: Integer);
    procedure SetFocusUnderlineStyle(const Value: TPenStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetCurrentCursor(const Value: TCursor);
    procedure SetInputType(const Value: TInputType);
    procedure SetTextCase(const Value: TTextCase);
    procedure SetInputMask(const Value: string);
    procedure SetCaptionSettings(const Value: TCaptionSettings);
    procedure CaptionSettingsChanged(Sender: TObject);
    procedure SetHoverSettings(const Value: THoverSettings);
    procedure HoverSettingsChanged(Sender: TObject);
    procedure TextMarginsChanged(Sender: TObject); // New Method for Text Margins
    procedure SetTextMargins(const Value: TANDMR_Margins); // New Setter for Text Margins
    procedure SetPredefinedMask(const Value: TPredefinedMaskType); // New Setter for Predefined Mask

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER; // Changed to TMessage
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE; // Changed to TMessage
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
    procedure CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect); virtual; // CORRECTED
    // Removed Declarations: DrawEditBox, DrawPNGImageWithGDI, DrawNonPNGImageWithCanvas, DrawSeparatorWithCanvas
    procedure Paint; override; // Single Paint declaration
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

    // New published properties
  property FocusBorderColor: TColor read FFocusBorderColor write SetFocusBorderColor;
  property FocusBorderColorVisible: Boolean read FFocusBorderColorVisible write SetFocusBorderColorVisible;
  property FocusBackgroundColor: TColor read FFocusBackgroundColor write SetFocusBackgroundColor;
  property FocusBackgroundColorVisible: Boolean read FFocusBackgroundColorVisible write SetFocusBackgroundColorVisible;
  property FocusUnderlineColor: TColor read FFocusUnderlineColor write SetFocusUnderlineColor;
  property FocusUnderlineVisible: Boolean read FFocusUnderlineVisible write SetFocusUnderlineVisible;
  property FocusUnderlineThickness: Integer read FFocusUnderlineThickness write SetFocusUnderlineThickness;
  property FocusUnderlineStyle: TPenStyle read FFocusUnderlineStyle write SetFocusUnderlineStyle;
  property Opacity: Byte read FOpacity write SetOpacity;
  property CurrentCursor: TCursor read FCurrentCursor write SetCurrentCursor;
  property InputType: TInputType read FInputType write SetInputType default itNormal;
  property TextCase: TTextCase read FTextCase write SetTextCase default tcNormal;
  property InputMask: string read FInputMask write SetInputMask;
  property CaptionSettings: TCaptionSettings read FCaptionSettings write SetCaptionSettings;
  property HoverSettings: THoverSettings read FHoverSettings write SetHoverSettings;
  property TextMargins: TANDMR_Margins read FTextMargins write SetTextMargins;
  property PredefinedMask: TPredefinedMaskType read FPredefinedMask write SetPredefinedMask default pmtNone;
  end;

procedure Register;

implementation

uses
  System.Character; // Added for ToUpper/ToLower
  // ANDMR_ComponentUtils; // Removed as it's in the interface uses clause

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CEdit]);
end;

// Implementations for TCaptionSettings, THoverSettings are in ANDMR_ComponentUtils.pas
// TImageMarginsControl implementation is in ANDMR_ComponentUtils.pas
// TTextMargins implementation is in ANDMR_ComponentUtils.pas
// Helper functions ColorToARGB, CreateGPRoundedPath, DrawEditBox, DrawPNGImageWithGDI, DrawNonPNGImageWithCanvas, DrawSeparatorWithCanvas are in ANDMR_ComponentUtils.pas

{ TANDMR_CEdit }
procedure TANDMR_CEdit.CalculateLayout(out outImgRect: TRect; out outTxtRect: TRect; out outSepRect: TRect);
var
  WorkArea: TRect;
  ImgW, ImgH, SepW: Integer;
  // CurrentX, CurrentX_End: Integer; // We will manage positions more directly
  FullClientRect: TRect;
  CaptionHeight, CaptionWidth: Integer;
  OriginalFont: TFont;
  OriginalImgW, OriginalImgH: Integer;
  availWForImg, availHForImg: Integer;
  rImageRatio, rAvailBoxRatio: Double;
  tempW, tempH: Double;
begin
  FullClientRect := Self.ClientRect;
  FCaptionRect := Rect(0,0,0,0);

  // 1. Determine WorkArea (area for the edit control itself, after caption)
  if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') then
  begin
    // ... (caption calculation logic - this part is assumed to be okay) ...
    // [This section is the same as in the existing file, calculating FCaptionRect and WorkArea based on caption]
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
      cpAbove:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Right, FullClientRect.Top + CaptionHeight);
          WorkArea.Top := FCaptionRect.Bottom + FCaptionSettings.Offset;
        end;
      cpBelow:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Bottom - CaptionHeight, FullClientRect.Right, FullClientRect.Bottom);
          WorkArea.Bottom := FCaptionRect.Top - FCaptionSettings.Offset;
        end;
      cpLeft:
        begin
          FCaptionRect := Rect(FullClientRect.Left, FullClientRect.Top, FullClientRect.Left + CaptionWidth, FullClientRect.Bottom);
          WorkArea.Left := FCaptionRect.Right + FCaptionSettings.Offset;
        end;
      cpRight:
        begin
          FCaptionRect := Rect(FullClientRect.Right - CaptionWidth, FullClientRect.Top, FullClientRect.Right, FullClientRect.Bottom);
          WorkArea.Right := FCaptionRect.Left - FCaptionSettings.Offset;
        end;
    end;
    if WorkArea.Bottom < WorkArea.Top then WorkArea.Bottom := WorkArea.Top;
    if WorkArea.Right < WorkArea.Left then WorkArea.Right := WorkArea.Left;
  end
  else
    WorkArea := FullClientRect;

  InflateRect(WorkArea, -FBorderThickness, -FBorderThickness);

  // Initialize out params
  outImgRect := Rect(0,0,0,0);
  outSepRect := Rect(0,0,0,0);
  outTxtRect := WorkArea; // Start with text taking full work area, then shrink

  // 2. Calculate Image Dimensions (ImgW, ImgH) and outImgRect
  ImgW := 0; ImgH := 0;
  if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
  begin
    OriginalImgW := FImage.Graphic.Width;
    OriginalImgH := FImage.Graphic.Height;

    if (OriginalImgW > 0) and (OriginalImgH > 0) then
    begin
      if FImageDrawMode = idmProportional then
      begin
        availWForImg := WorkArea.Width - FImageMargins.Left - FImageMargins.Right;
        availHForImg := WorkArea.Height - FImageMargins.Top - FImageMargins.Bottom;
        availWForImg := Max(0, availWForImg);
        availHForImg := Max(0, availHForImg);

        if (availWForImg > 0) and (availHForImg > 0) then
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
            end
            else ImgH := 0;
          end;
        end
        else // No available space for proportional image
        begin ImgW := 0; ImgH := 0; end;
      end
      else // Not idmProportional (stretch, normal)
      begin
        ImgW := OriginalImgW; // Layout uses original dimensions
        ImgH := OriginalImgH;
      end;
    end; // else OriginalImgW/H <=0, so ImgW, ImgH remain 0

    ImgW := Max(0, ImgW);
    ImgH := Max(0, ImgH);

    if ImgW > 0 then // Only define outImgRect if image has width
    begin
      // Position outImgRect horizontally based on FImagePosition
      if FImagePosition = ipsLeft then
      begin
        outImgRect.Left := WorkArea.Left + FImageMargins.Left;
        outImgRect.Right := outImgRect.Left + ImgW;
      end
      else // ipsRight
      begin
        outImgRect.Right := WorkArea.Right - FImageMargins.Right;
        outImgRect.Left := outImgRect.Right - ImgW;
      end;

      // Position outImgRect vertically based on FImageAlignment
      // AvailHForImgLayout is the height of WorkArea minus vertical margins
      var AvailHForImgLayoutAdjusted: Integer := WorkArea.Height - FImageMargins.Top - FImageMargins.Bottom;
      AvailHForImgLayoutAdjusted := Max(0, AvailHForImgLayoutAdjusted);

      case FImageAlignment of
        iavTop:    outImgRect.Top := WorkArea.Top + FImageMargins.Top;
        iavCenter: outImgRect.Top := WorkArea.Top + FImageMargins.Top + (AvailHForImgLayoutAdjusted - ImgH) div 2;
        iavBottom: outImgRect.Top := WorkArea.Bottom - FImageMargins.Bottom - ImgH;
      end;
      outImgRect.Bottom := outImgRect.Top + ImgH;

      // Clip outImgRect to WorkArea (important after alignment)
      if outImgRect.Left < WorkArea.Left then outImgRect.Left := WorkArea.Left;
      if outImgRect.Right > WorkArea.Right then outImgRect.Right := WorkArea.Right;
      if outImgRect.Top < WorkArea.Top then outImgRect.Top := WorkArea.Top;
      if outImgRect.Bottom > WorkArea.Bottom then outImgRect.Bottom := WorkArea.Bottom;
      // Ensure non-negative width/height for outImgRect after clipping
      if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
      if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;

      // Update ImgW, ImgH from the final clipped outImgRect for subsequent layout calculations
      ImgW := outImgRect.Width;
      ImgH := outImgRect.Height;
    end;
  end; // End FImageVisible

  // 3. Calculate Separator Rectangle (outSepRect)
  SepW := 0;
  if FSeparatorVisible and (FSeparatorThickness > 0) then
    SepW := FSeparatorThickness;

  if SepW > 0 then
  begin
    if FImageVisible and (ImgW > 0) then // Separator is between image and text
    begin
      if FImagePosition = ipsLeft then
      begin
        outSepRect.Left := outImgRect.Right + FImageMargins.Right + FSeparatorPadding;
      end
      else // Image is on the right
      begin
        outSepRect.Left := outImgRect.Left - FImageMargins.Left - FSeparatorPadding - SepW;
      end;
    end
    else // Separator is at the start or end of WorkArea (no image)
    begin
      if FImagePosition = ipsLeft then // Assuming separator follows image position logic
        outSepRect.Left := WorkArea.Left + FSeparatorPadding
      else
        outSepRect.Left := WorkArea.Right - FSeparatorPadding - SepW;
    end;
    outSepRect.Right := outSepRect.Left + SepW;

    // Vertical position of separator (same as existing logic)
    var SepH: Integer; var RefTop, RefHeight: Integer; outSepRect.Top := WorkArea.Top; SepH := WorkArea.Height;
    case FSeparatorHeightMode of
      shmFull: ; // SepH is WorkArea.Height
      shmAsText: begin RefTop := outTxtRect.Top; RefHeight := outTxtRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end; // outTxtRect not final yet, potential issue here
      shmAsImage: if FImageVisible and (ImgW > 0) and (outImgRect.Height > 0) then begin RefTop := outImgRect.Top; RefHeight := outImgRect.Height; SepH := RefHeight; outSepRect.Top := RefTop; end;
      shmCustom: begin if FSeparatorCustomHeight > 0 then SepH := FSeparatorCustomHeight else SepH := WorkArea.Height; outSepRect.Top := WorkArea.Top + (WorkArea.Height - SepH) div 2; end;
    end;
    outSepRect.Bottom := outSepRect.Top + SepH;

    // Clip outSepRect (similar to outImgRect clipping)
    if outSepRect.Left < WorkArea.Left then outSepRect.Left := WorkArea.Left;
    if outSepRect.Right > WorkArea.Right then outSepRect.Right := WorkArea.Right;
    if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top;
    if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom;
    if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left;
    if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;
    SepW := outSepRect.Width; // Update SepW from final clipped separator rect
  end;


  // 4. Calculate Text Rectangle (outTxtRect)
  // outTxtRect was initialized to WorkArea. Now adjust its Left/Right based on image and separator.
  if FImageVisible and (ImgW > 0) then
  begin
    if FImagePosition = ipsLeft then
    begin
      outTxtRect.Left := outImgRect.Right + FImageMargins.Right;
      if SepW > 0 then // Separator is to the right of image
        outTxtRect.Left := outSepRect.Right + FSeparatorPadding;
    end
    else // Image is on the right
    begin
      outTxtRect.Right := outImgRect.Left - FImageMargins.Left;
      if SepW > 0 then // Separator is to the left of image
        outTxtRect.Right := outSepRect.Left - FSeparatorPadding;
    end;
  end
  else if SepW > 0 then // No image, but separator is visible
  begin
    if FImagePosition = ipsLeft then // Assuming separator follows image position logic
      outTxtRect.Left := outSepRect.Right + FSeparatorPadding
    else
      outTxtRect.Right := outSepRect.Left - FSeparatorPadding;
  end;
  // Ensure outTxtRect does not have negative width
  if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;


  // Re-evaluate shmAsText for separator now that outTxtRect is more final (horizontally)
  // This is a bit circular if separator height depends on text height and text position depends on separator.
  // For simplicity, the previous shmAsText calculation based on initial outTxtRect might be kept,
  // or this part needs more careful handling if text height changes significantly.
  // The current shmAsText logic for separator using a potentially not-yet-finalized outTxtRect is problematic.
  // A simpler approach for shmAsText for now: use WorkArea.Height for separator if text rect isn't stable.
  // Or, if text is always vertically centered in WorkArea, use WorkArea.Height.
  // For now, we'll leave the shmAsText logic as is and assume it uses the outTxtRect.Top/Height from WorkArea.
  // If shmAsText is critical, this part would need a more iterative or refined calculation.

  // Final safety clipping for all rects (already partially done for img and sep)
  // This is a general safeguard.
  if outTxtRect.Left < WorkArea.Left then outTxtRect.Left := WorkArea.Left; if outTxtRect.Right > WorkArea.Right then outTxtRect.Right := WorkArea.Right; if outTxtRect.Right < outTxtRect.Left then outTxtRect.Right := outTxtRect.Left;
  if outImgRect.Left < WorkArea.Left then outImgRect.Left := WorkArea.Left; if outImgRect.Right > WorkArea.Right then outImgRect.Right := WorkArea.Right; if outImgRect.Right < outImgRect.Left then outImgRect.Right := outImgRect.Left;
  if outSepRect.Left < WorkArea.Left then outSepRect.Left := WorkArea.Left; if outSepRect.Right > WorkArea.Right then outSepRect.Right := WorkArea.Right; if outSepRect.Right < outSepRect.Left then outSepRect.Right := outSepRect.Left;

  if outTxtRect.Top < WorkArea.Top then outTxtRect.Top := WorkArea.Top; if outTxtRect.Bottom > WorkArea.Bottom then outTxtRect.Bottom := WorkArea.Bottom; if outTxtRect.Bottom < outTxtRect.Top then outTxtRect.Bottom := outTxtRect.Top;
  if outImgRect.Top < WorkArea.Top then outImgRect.Top := WorkArea.Top; if outImgRect.Bottom > WorkArea.Bottom then outImgRect.Bottom := WorkArea.Bottom; if outImgRect.Bottom < outImgRect.Top then outImgRect.Bottom := outImgRect.Top;
  if outSepRect.Top < WorkArea.Top then outSepRect.Top := WorkArea.Top; if outSepRect.Bottom > WorkArea.Bottom then outSepRect.Bottom := WorkArea.Bottom; if outSepRect.Bottom < outSepRect.Top then outSepRect.Bottom := outSepRect.Top;

end;

procedure TANDMR_CEdit.SetImagePlacement(const Value: TImagePlacement); begin if FImagePlacement <> Value then begin FImagePlacement := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageDrawMode(const Value: TImageDrawMode); begin if FImageDrawMode <> Value then begin FImageDrawMode := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorVisible(const Value: Boolean); begin if FSeparatorVisible <> Value then begin FSeparatorVisible := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorColor(const Value: TColor); begin if FSeparatorColor <> Value then begin FSeparatorColor := Value; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorThickness(const Value: Integer); var ValidThickness: Integer; begin ValidThickness := Max(0, Value); if FSeparatorThickness <> ValidThickness then begin FSeparatorThickness := ValidThickness; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorPadding(const Value: Integer); var ValidPadding: Integer; begin ValidPadding := Max(0, Value); if FSeparatorPadding <> ValidPadding then begin FSeparatorPadding := ValidPadding; if FSeparatorVisible then Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorHeightMode(const Value: TSeparatorHeightMode); begin if FSeparatorHeightMode <> Value then begin FSeparatorHeightMode := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetSeparatorCustomHeight(const Value: Integer); var ValidHeight: Integer; begin ValidHeight := Max(0, Value); if FSeparatorCustomHeight <> ValidHeight then begin FSeparatorCustomHeight := ValidHeight; if FSeparatorHeightMode = shmCustom then Invalidate; end; end;

procedure TANDMR_CEdit.SetImage(const Value: TPicture); begin FImage.Assign(Value); Invalidate; end;
procedure TANDMR_CEdit.SetImageVisible(const Value: Boolean); begin if FImageVisible <> Value then begin FImageVisible := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImagePosition(const Value: TImagePositionSide); begin if FImagePosition <> Value then begin FImagePosition := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageAlignment(const Value: TImageAlignmentVertical); begin if FImageAlignment <> Value then begin FImageAlignment := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetImageMargins(const Value: TANDMR_Margins); begin FImageMargins.Assign(Value); Invalidate; end;
procedure TANDMR_CEdit.ImageChanged(Sender: TObject); begin Invalidate; end;
procedure TANDMR_CEdit.ImageMarginsChanged(Sender: TObject); begin Invalidate; end;

// Implementations of DrawPNGImageWithGDI, DrawNonPNGImageWithCanvas, DrawSeparatorWithCanvas, DrawEditBox were moved to ANDMR_ComponentUtils

constructor TANDMR_CEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csCaptureMouse, csDoubleClicks, csReplicatable];
  DoubleBuffered := True; Width := 150; Height := 25; TabStop := True; FText := ''; FMaxLength := 0; FPasswordChar := #0; FReadOnly := False;
  FCornerRadius := 8; FRoundCornerType := rctAll; FActiveColor := clHighlight; FInactiveColor := clBtnFace; FBorderColor := clBlack; FBorderThickness := 1; FBorderStyle := psSolid;
  Font.Name := 'Segoe UI'; Font.Size := 9; Font.Color := clWindowText;
  FImage := TPicture.Create; FImage.OnChange := Self.ImageChanged;
  FImageMargins := TANDMR_Margins.Create; FImageMargins.OnChange := Self.ImageMarginsChanged;
  FImageVisible := True; FImagePosition := ipsLeft; FImageAlignment := iavCenter;
  FImagePlacement := iplInsideBounds; FImageDrawMode := idmProportional;
  FSeparatorVisible := False; FSeparatorColor := clGrayText; FSeparatorThickness := 1; FSeparatorPadding := 2; FSeparatorHeightMode := shmFull; FSeparatorCustomHeight := 0;
  FCaretVisible := False; FCaretPosition := 0; FCaretTimer := TTimer.Create(Self); FCaretTimer.Interval := GetCaretBlinkTime; FCaretTimer.OnTimer := CaretTimerTick; FCaretTimer.Enabled := False;

  // Initialize new properties
  FFocusBorderColorVisible := False;
  FFocusBorderColor := clBlack;
  FFocusBackgroundColorVisible := False;
  FFocusBackgroundColor := clWindow;
  FFocusUnderlineVisible := False;
  FFocusUnderlineColor := clBlack;
  FFocusUnderlineThickness := 1;
  FFocusUnderlineStyle := psSolid;
  FOpacity := 255;
  FCurrentCursor := crIBeam;
  Self.Cursor := FCurrentCursor; // Set initial cursor
  FInputType := itNormal; // Initialize InputType
  FTextCase := tcNormal; // Initialize TextCase
  FInputMask := '';
  FMaskedText := '';
  FRawText := '';
  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := CaptionSettingsChanged;
  FCaptionRect := Rect(0,0,0,0);
  FHoverSettings := THoverSettings.Create(Self); // Create HoverSettings, pass Self as Owner
  FHoverSettings.OnChange := HoverSettingsChanged; // Assign OnChange handler
  FHovered := False; // Initialize FHovered
  FTextMargins := TANDMR_Margins.Create; // Create TextMargins
  FTextMargins.OnChange := TextMarginsChanged; // Assign OnChange handler
  FPredefinedMask := pmtNone; // Initialize PredefinedMask
end;

destructor TANDMR_CEdit.Destroy;
begin
  FTextMargins.Free; // Free TextMargins
  FHoverSettings.Free; // Free HoverSettings
  FCaptionSettings.Free;
  FCaretTimer.Free; if Assigned(FImage) then begin FImage.OnChange := nil; FImage.Free; end; FImageMargins.Free;
  inherited Destroy;
end;

procedure TANDMR_CEdit.SetText(const Value: string); // Value is considered RAW input
var
  OldFDisplayText: string; // FText will now represent FMaskedText for display
  ProcessedRawText: string; // Raw text after TextCase transformation
  NewMaskedText: string;
  NewUnmaskedText: string; // Corrected FRawText after applying mask
  RawIndex: Integer;
  MaskIndex: Integer;
  MaskChar: Char;
  IsLiteral: Boolean;
  CharToTest: Char;
  CharAllowed: Boolean;
begin
  OldFDisplayText := FText; // FText currently stores the masked text for display

  // 1. Apply TextCase to the incoming raw Value
  ProcessedRawText := Value;
  case FTextCase of
    tcUppercase: ProcessedRawText := System.SysUtils.UpperCase(ProcessedRawText);
    tcLowercase: ProcessedRawText := System.SysUtils.LowerCase(ProcessedRawText);
  end;

  // 2. Rebuild FRawText and FMaskedText based on ProcessedRawText and FInputMask
  NewUnmaskedText := '';
  NewMaskedText := '';

  if FInputMask <> '' then
  begin
    RawIndex := 1;
    for MaskIndex := 1 to Length(FInputMask) do
    begin
      MaskChar := FInputMask[MaskIndex];
      // Define literals as characters not in the set of placeholders
      IsLiteral := not (MaskChar IN ['9', '0', 'L', 'A', '#']); // Added '0' as a placeholder

      if IsLiteral then
      begin
        NewMaskedText := NewMaskedText + MaskChar;
      end
      else // It's a placeholder
      begin
        if RawIndex <= Length(ProcessedRawText) then
        begin
          CharToTest := ProcessedRawText[RawIndex];
          CharAllowed := False;
          case MaskChar of
            '9', '0': CharAllowed := CharToTest IN ['0'..'9']; // Added '0'
            'L': CharAllowed := System.Character.IsLetter(CharToTest);
            'A': CharAllowed := System.Character.IsLetterOrDigit(CharToTest);
            '#': CharAllowed := True; // Example: '#' accepts any char from raw text
          end;

          if CharAllowed then
          begin
            NewMaskedText := NewMaskedText + CharToTest;
            NewUnmaskedText := NewUnmaskedText + CharToTest; // Build the true FRawText
            Inc(RawIndex);
          end
          else // Char from ProcessedRawText doesn't fit mask placeholder
          begin
            NewMaskedText := NewMaskedText + '_'; // Placeholder for invalid/missing char
            // Do not increment RawIndex, as this raw char was not consumed.
            // Or, option: skip this raw char and try next? For now, assume strict adherence.
          end;
        end
        else // No more raw text to fill this placeholder
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
    FText := FRawText;          // Display text is also this raw text
    FMaskedText := FRawText;    // FMaskedText is same as FRawText when no mask
  end;

  // 3. Update caret and visual state
  //FCaretPosition := Length(FText); // Simplistic caret: end of text. TODO: Smarter caret.
  // For SetText, placing caret at end is a common behavior.
  // If trying to preserve caret: more complex, map old FRawText pos to new FRawText pos, then to FMaskedText pos.
  // For now, set to end of the new FText.
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
begin if FMaxLength <> Value then begin FMaxLength := Max(0, Value); TextChanged := False; OldText := FText; if (FMaxLength > 0) and (Length(FText) > FMaxLength) then begin FText := Copy(FText, 1, FMaxLength); if FCaretPosition > Length(FText) then FCaretPosition := Length(FText); TextChanged := True; end; if TextChanged then begin FCaretVisible := True; if Focused then begin FCaretTimer.Enabled := False; FCaretTimer.Enabled := True; end; if Assigned(FOnChange) then FOnChange(Self); Invalidate; end; end;
end;

procedure TANDMR_CEdit.SetPasswordChar(const Value: Char); begin if FPasswordChar <> Value then begin FPasswordChar := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetReadOnly(const Value: Boolean); begin if FReadOnly <> Value then begin FReadOnly := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetCornerRadius(const Value: Integer); begin if FCornerRadius <> Value then begin FCornerRadius := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetRoundCornerType(const Value: TRoundCornerType); begin if FRoundCornerType <> Value then begin FRoundCornerType := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetActiveColor(const Value: TColor); begin if FActiveColor <> Value then begin FActiveColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetInactiveColor(const Value: TColor); begin if FInactiveColor <> Value then begin FInactiveColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderThickness(const Value: Integer); begin if FBorderThickness <> Value then begin FBorderThickness := Value; Invalidate; end; end;
procedure TANDMR_CEdit.SetBorderStyle(const Value: TPenStyle); begin if FBorderStyle <> Value then begin FBorderStyle := Value; Invalidate; end; end;

// Implementation of new Set methods
procedure TANDMR_CEdit.SetFocusBorderColor(const Value: TColor);
begin
  if FFocusBorderColor <> Value then
  begin
    FFocusBorderColor := Value;
    if FFocusBorderColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBorderColorVisible(const Value: Boolean);
begin
  if FFocusBorderColorVisible <> Value then
  begin
    FFocusBorderColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBackgroundColor(const Value: TColor);
begin
  if FFocusBackgroundColor <> Value then
  begin
    FFocusBackgroundColor := Value;
    if FFocusBackgroundColorVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusBackgroundColorVisible(const Value: Boolean);
begin
  if FFocusBackgroundColorVisible <> Value then
  begin
    FFocusBackgroundColorVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineColor(const Value: TColor);
begin
  if FFocusUnderlineColor <> Value then
  begin
    FFocusUnderlineColor := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineVisible(const Value: Boolean);
begin
  if FFocusUnderlineVisible <> Value then
  begin
    FFocusUnderlineVisible := Value;
    if Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineThickness(const Value: Integer);
var ValidThickness: Integer;
begin
  ValidThickness := Max(0, Value); // Ensure non-negative
  if FFocusUnderlineThickness <> ValidThickness then
  begin
    FFocusUnderlineThickness := ValidThickness;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetFocusUnderlineStyle(const Value: TPenStyle);
begin
  if FFocusUnderlineStyle <> Value then
  begin
    FFocusUnderlineStyle := Value;
    if FFocusUnderlineVisible and Focused then Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    // Handle csOpaque style based on opacity
    if FOpacity < 255 then
    begin
      ControlStyle := ControlStyle - [csOpaque];
      if Parent <> nil then Parent.Invalidate; // Invalidate parent to redraw background
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque];
    end;
    Invalidate; // Invalidate self to redraw with new opacity
  end;
end;

procedure TANDMR_CEdit.SetCurrentCursor(const Value: TCursor);
begin
  if FCurrentCursor <> Value then
  begin
    FCurrentCursor := Value;
    Self.Cursor := FCurrentCursor; // Update the actual cursor
  end;
end;

procedure TANDMR_CEdit.SetInputType(const Value: TInputType);
begin
  if FInputType <> Value then
  begin
    FInputType := Value;
    // Future: May need to validate/clear existing text if it doesn't match the new type.
    // For now, just set and let new input be filtered.
    // Invalidate might be useful if visual cues for input type are added later
    // or if immediate validation of existing text is performed.
    // Invalidate;
  end;
end;

procedure TANDMR_CEdit.SetTextCase(const Value: TTextCase);
var
  OldText: string;
  TransformedText: string;
begin
  if FTextCase <> Value then
  begin
    OldText := FText; // Store original FText before any transformation by this call
    FTextCase := Value; // Assign the new case type

    TransformedText := FText; // Start with current FText for transformation
    case FTextCase of
      tcUppercase: TransformedText := System.SysUtils.UpperCase(FText);
      tcLowercase: TransformedText := System.SysUtils.LowerCase(FText);
      // tcNormal: No transformation needed on existing FText based on this new setting
    end;

    if FText <> TransformedText then // If the text actually changed after applying the new case
    begin
      FText := TransformedText; // Assign the transformed text
      FCaretPosition := Length(FText); // Reset caret to end
      if Assigned(FOnChange) then
         FOnChange(Self); // Text content changed
      Invalidate; // Repaint needed
    end
    else if (OldText = TransformedText) and (Value <> tcNormal) then
    begin
      // This case handles if FText was already, e.g., "TEXT" and tcUppercase is set.
      // FText doesn't change, OldText is same as TransformedText.
      // However, the *property* TextCase has changed, which might be relevant for designers
      // or other logic. A simple Invalidate might be too much if no visual change.
      // For now, if text didn't change, only Invalidate if a specific case is enforced
      // and a repaint might be desired by some logic (though not strictly necessary if text is same)
      // No OnChange here as FText content hasn't changed.
      Invalidate; // Or remove if no visual change is expected when text is already correct case.
    end;
  end;
end;

procedure TANDMR_CEdit.SetInputMask(const Value: string);
var
  OldRawText: string;
begin
  if FInputMask <> Value then
  begin
    OldRawText := FRawText; // Preserve raw text
    FInputMask := Value;

    if FInputMask = '' then
      FPredefinedMask := pmtNone // If mask string is cleared, no predefined type applies
    else
      FPredefinedMask := pmtCustom; // Any direct non-empty mask string implies custom type

    // Re-apply the old raw text to the new mask format
    SetText(OldRawText);
    Invalidate; // Ensure repaint for property change in designer
  end;
end;

procedure TANDMR_CEdit.SetCaptionSettings(const Value: TCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
  CaptionSettingsChanged(Self); // Trigger update
end;

procedure TANDMR_CEdit.CaptionSettingsChanged(Sender: TObject);
begin
  // This method is called when a property within FCaptionSettings changes.
  // Need to recalculate layout and repaint.
  // A more specific CalculateLayoutAndInvalidate could be:
  // Self.CalculateLayout(FImgRect, FTxtRect, FSepRect); // Assuming CalculateLayout updates internal fields or similar
  Invalidate; // For now, simple Invalidate. Layout recalculation will happen in Paint or a dedicated method.
  // TODO: Ensure layout is recalculated before painting if caption affects it.
  // For now, Paint will call CalculateLayout which should handle the new FCaptionRect
end;

procedure TANDMR_CEdit.SetHoverSettings(const Value: THoverSettings);
begin
  FHoverSettings.Assign(Value);
  HoverSettingsChanged(Self); // Trigger update, typically Invalidate
end;

procedure TANDMR_CEdit.HoverSettingsChanged(Sender: TObject);
begin
  if FHovered or (not FHoverSettings.Enabled) then // Repaint if currently hovered or if hover effect is disabled (to revert)
    Invalidate;
end;

procedure TANDMR_CEdit.TextMarginsChanged(Sender: TObject);
begin
  Invalidate; // Recalculate and repaint
end;

procedure TANDMR_CEdit.SetTextMargins(const Value: TANDMR_Margins);
begin
  FTextMargins.Assign(Value);
  // TextMarginsChanged will be called by FTextMargins.Assign if values actually change
  // and if FTextMargins.Assign calls Changed.
  // To be safe, we can call Invalidate here if direct assignment might not trigger OnChange.
  // However, the current TTextMargins.Assign does call Changed.
end;

procedure TANDMR_CEdit.SetPredefinedMask(const Value: TPredefinedMaskType);
var
  NewMaskValue: string;
  OldRawText: string;
begin
  if FPredefinedMask <> Value then
  begin
    OldRawText := FRawText; // Preserve raw text
    FPredefinedMask := Value;

    case FPredefinedMask of
      pmtCPF:     NewMaskValue := '000.000.000-00';
      pmtCNPJ:    NewMaskValue := '00.000.000/0000-00';
      pmtCEP:     NewMaskValue := '00000-000';
      pmtPhoneBR: NewMaskValue := '(00) 90000-0000';
      pmtDateDMY: NewMaskValue := '00/00/0000';
      pmtCustom:  Exit; // Do nothing, FInputMask is king
      pmtNone:    NewMaskValue := '';
    else
      NewMaskValue := ''; // Should not happen, but default to no mask
    end;

    // Directly change FInputMask to avoid feedback loop with SetInputMask property setter
    if FInputMask <> NewMaskValue then
    begin
      FInputMask := NewMaskValue;
      // Now re-apply the old raw text to the new mask format.
      // SetText handles FRawText, FMaskedText, FText, and Invalidation.
      SetText(OldRawText);
    end;
    Invalidate; // Ensure repaint for property change in designer
  end;
end;

procedure TANDMR_CEdit.CMMouseEnter(var Message: TMessage); // Changed to TMessage
begin
  inherited;
  if not FHovered then
  begin
    FHovered := True;
    // if FHoverSettings.Enabled then Invalidate; // Old logic
  end;
  FHoverSettings.StartAnimation(True); // New animation call
end;

procedure TANDMR_CEdit.CMMouseLeave(var Message: TMessage); // Changed to TMessage
begin
  inherited;
  if FHovered then
  begin
    FHovered := False;
    // if FHoverSettings.Enabled then Invalidate; // Old logic
  end;
  FHoverSettings.StartAnimation(False); // New animation call
end;

procedure TANDMR_CEdit.Paint;
var
  LG: TGPGraphics;
  TextToDisplay: string;
  TextFlags: Cardinal;
  imgR, txtR, sepR: TRect;
  //OverallBGColor: TColor; // Superseded by Actual* logic
  RectToDrawEditBoxIn: TRect; // Used for FocusUnderline, should be the EditBoxDrawingRect
  PaddedTextDrawArea: TRect;
  FullClientRect: TRect;

  // Variables for determined colors based on state
  ActualEditBGColor, ActualEditBorderColor, ActualEditTextColor, ActualCaptionTextColor: TColor;
  // Variables for the specific area to draw the edit box and its background
  EditBoxDrawingRect: TRect; // The rect for the DrawEditBox call
  BGForDrawEditBox: TColor;  // The background color for DrawEditBox
begin
  FullClientRect := Self.ClientRect; // Cache client rect
  CalculateLayout(imgR, txtR, sepR); // This sets FCaptionRect and out params for edit area

  Canvas.Lock;
  try
    LG := TGPGraphics.Create(Canvas.Handle);
    try
      LG.SetSmoothingMode(SmoothingModeAntiAlias);
      LG.SetPixelOffsetMode(PixelOffsetModeHalf);

      var LHoverProgress: Single := FHoverSettings.CurrentAnimationValue / 255.0;

      // --- Determine current state colors ---
      var TrueBaseEditBG, HoverEditBGFromSettings, FocusEditBGFromSettings, DisabledEditBGForResolve: TColor;
      var TrueBaseBorderCol, HoverBorderColFromSettings, FocusBorderColFromSettings, DisabledBorderColForResolve: TColor;
      var TrueBaseTextCol, HoverTextColFromSettings, FocusTextColFromSettings, DisabledTextColForResolve: TColor;
      var TrueBaseCaptionCol, HoverCaptionColFromSettings, FocusCaptionColFromSettings, DisabledCaptionColForResolve: TColor;

      var NonHoveredBGColor, TargetStateBGColor: TColor;
      var NonHoveredBorderColor, TargetStateBorderColor: TColor;
      var NonHoveredTextColor, TargetStateTextColor: TColor;
      var NonHoveredCaptionColor, TargetStateCaptionColor: TColor;

      // Define TRUE base colors (Enabled, not focused, not hovered)
      TrueBaseEditBG := IfThen(FImagePlacement = iplInsideBounds, FInactiveColor, clWindow);
      TrueBaseBorderCol := FBorderColor;
      TrueBaseTextCol := Self.Font.Color;
      TrueBaseCaptionCol := IfThen(FCaptionSettings.Color = clDefault, Self.Font.Color, FCaptionSettings.Color);

      // Define TARGET hover colors from FHoverSettings
      HoverEditBGFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.BackgroundColor, clNone);
      HoverBorderColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.BorderColor, clNone);
      HoverTextColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.FontColor, clNone);
      HoverCaptionColFromSettings := IfThen(FHoverSettings.Enabled, FHoverSettings.CaptionFontColor, clNone);

      // Define FOCUS colors
      FocusEditBGFromSettings := IfThen(FFocusBackgroundColorVisible, FFocusBackgroundColor, clNone);
      FocusBorderColFromSettings := IfThen(FFocusBorderColorVisible, FFocusBorderColor, FActiveColor);
      FocusTextColFromSettings := TrueBaseTextCol; // No separate focus text color property
      FocusCaptionColFromSettings := TrueBaseCaptionCol; // No separate focus caption color

      // Define DISABLED colors (these are final, no blending for disabled typically)
      DisabledEditBGForResolve := TrueBaseEditBG; // Or a specific dimmed color
      DisabledBorderColForResolve := TrueBaseBorderCol; // Or a specific dimmed color
      DisabledTextColForResolve := IfThen(TrueBaseTextCol = clWindowText, clGrayText, DarkerColor(TrueBaseTextCol, 50));
      DisabledCaptionColForResolve := IfThen(TrueBaseCaptionCol = clWindowText, clGrayText, DarkerColor(TrueBaseCaptionCol,50));

      // 1. Calculate NonHovered colors (Enabled, possibly Focused, NOT Hovered)
      NonHoveredBGColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseEditBG, clNone, FocusEditBGFromSettings, DisabledEditBGForResolve, FHoverSettings.Enabled, FFocusBackgroundColorVisible, True);
      NonHoveredBorderColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseBorderCol, clNone, FocusBorderColFromSettings, DisabledBorderColForResolve, FHoverSettings.Enabled, True, False);
      NonHoveredTextColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseTextCol, clNone, FocusTextColFromSettings, DisabledTextColForResolve, FHoverSettings.Enabled, False, False);
      NonHoveredCaptionColor := ResolveStateColor(Self.Enabled, FALSE, Self.Focused, TrueBaseCaptionCol, clNone, FocusCaptionColFromSettings, DisabledCaptionColForResolve, FHoverSettings.Enabled, False, False);

      // 2. Calculate TargetState colors (current full state including hover, focus)
      TargetStateBGColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseEditBG, HoverEditBGFromSettings, FocusEditBGFromSettings, DisabledEditBGForResolve, FHoverSettings.Enabled, FFocusBackgroundColorVisible, True);
      TargetStateBorderColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseBorderCol, HoverBorderColFromSettings, FocusBorderColFromSettings, DisabledBorderColForResolve, FHoverSettings.Enabled, True, False);
      TargetStateTextColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseTextCol, HoverTextColFromSettings, FocusTextColFromSettings, DisabledTextColForResolve, FHoverSettings.Enabled, False, False);
      TargetStateCaptionColor := ResolveStateColor(Self.Enabled, FHovered, Self.Focused, TrueBaseCaptionCol, HoverCaptionColFromSettings, FocusCaptionColFromSettings, DisabledCaptionColForResolve, FHoverSettings.Enabled, False, False);

      // 3. Blend if animation is active towards a hovered state
      if (LHoverProgress > 0) and FHoverSettings.Enabled and (FHoverSettings.HoverEffect <> heNone) and FHovered then
      begin
        ActualEditBGColor := BlendColors(NonHoveredBGColor, TargetStateBGColor, LHoverProgress);
        ActualEditBorderColor := BlendColors(NonHoveredBorderColor, TargetStateBorderColor, LHoverProgress);
        ActualEditTextColor := BlendColors(NonHoveredTextColor, TargetStateTextColor, LHoverProgress);
        ActualCaptionTextColor := BlendColors(NonHoveredCaptionColor, TargetStateCaptionColor, LHoverProgress);
      end
      else // No animation towards hover, or animation is reversing/finished, or hover disabled/none
      begin
        ActualEditBGColor := TargetStateBGColor;
        ActualEditBorderColor := TargetStateBorderColor;
        ActualEditTextColor := TargetStateTextColor;
        ActualCaptionTextColor := TargetStateCaptionColor;
      end;

      // --- Painting ---
      // 1. Paint component's main background (entire ClientRect)
      if FOpacity = 255 then
      begin
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(FullClientRect);
      end; // If FOpacity < 255, parent's background shows through due to csOpaque removal.

      // 2. Determine the rectangle for the main edit box drawing operations
      //    and the background color to pass to DrawEditBox.
      if FImagePlacement = iplInsideBounds then
      begin
        // The "edit box" visual is the entire component area, adjusted for caption.
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
        BGForDrawEditBox := ActualEditBGColor; // This is the background for the main component shape.
      end
      else // iplOutsideBounds
      begin
        // The "edit box" visual is only the text input area (txtR).
        // The area around it (part of FullClientRect) was filled by Self.Color or is transparent.
        EditBoxDrawingRect := txtR; // txtR is already adjusted for caption and image.
        BGForDrawEditBox := ActualEditBGColor; // This is the background specifically for the text field.
      end;
      RectToDrawEditBoxIn := EditBoxDrawingRect; // For focus underline, it should relate to what was just drawn by DrawEditBox.

      // 3. Draw the main edit box (background and border)
      DrawEditBox(LG, EditBoxDrawingRect, BGForDrawEditBox, ActualEditBorderColor, FBorderThickness, FBorderStyle, FCornerRadius, FRoundCornerType, FOpacity);

      // 4. Draw Image
      // For iplInsideBounds, image is drawn on top of BGForDrawEditBox.
      // For iplOutsideBounds, image is drawn on Self.Color (already painted or transparent)
      if FImageVisible and Assigned(FImage.Graphic) and not FImage.Graphic.Empty then
      begin
        if (FImage.Graphic is TPNGImage) then
          DrawPNGImageWithGDI(LG, FImage.Graphic as TPNGImage, imgR, FImageDrawMode)
        else
          DrawNonPNGImageWithCanvas(Canvas, FImage.Graphic, imgR, FImageDrawMode);
      end;

      // 5. Draw Separator (drawn on top of whatever is beneath it)
      if FSeparatorVisible and (FSeparatorThickness > 0) and (sepR.Width > 0) and (sepR.Height > 0) then
        DrawSeparatorWithCanvas(Canvas, sepR, FSeparatorColor, FSeparatorThickness);

      // 6. Draw Focus Underline
      if Self.Focused and FFocusUnderlineVisible and (FFocusUnderlineThickness > 0) then
      begin
        var UnderlineY: Integer;
        var UnderlinePen: TGPPen;

        // Underline should be relative to the main edit box area that was just drawn.
        if FBorderThickness > 0 then
          UnderlineY := EditBoxDrawingRect.Bottom - FBorderThickness - (FFocusUnderlineThickness div 2)
        else
          UnderlineY := EditBoxDrawingRect.Bottom - (FFocusUnderlineThickness div 2);
        UnderlineY := Min(UnderlineY, EditBoxDrawingRect.Bottom - FFocusUnderlineThickness);

        UnderlinePen := TGPPen.Create(ColorToARGB(FFocusUnderlineColor, Self.FOpacity), FFocusUnderlineThickness);
        try
          case FFocusUnderlineStyle of
            psSolid: UnderlinePen.SetDashStyle(DashStyleSolid);
            psDash: UnderlinePen.SetDashStyle(DashStyleDash);
            psDot: UnderlinePen.SetDashStyle(DashStyleDot);
            psDashDot: UnderlinePen.SetDashStyle(DashStyleDashDot);
            psDashDotDot: UnderlinePen.SetDashStyle(DashStyleDashDotDot);
            else UnderlinePen.SetDashStyle(DashStyleSolid);
          end;
          LG.DrawLine(UnderlinePen, EditBoxDrawingRect.Left + FBorderThickness, UnderlineY, EditBoxDrawingRect.Right - FBorderThickness, UnderlineY);
        finally
          UnderlinePen.Free;
        end;
      end;

    finally
      LG.Free;
    end; // End of GDI+ operations

    // --- Text Rendering (using Canvas, relative to txtR) ---
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
      TextToDisplay := FText; // FText is the masked text if mask is used

    Canvas.Font.Assign(Self.Font);
    Canvas.Font.Color := ActualEditTextColor; // Use the determined color
    Canvas.Brush.Style := bsClear;
    TextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_EDITCONTROL;

    if (Length(TextToDisplay) > 0) and (PaddedTextDrawArea.Width > 0) and (PaddedTextDrawArea.Height > 0) then
      DrawText(Canvas.Handle, PChar(TextToDisplay), Length(TextToDisplay), PaddedTextDrawArea, TextFlags);

    // --- Caret Drawing (relative to PaddedTextDrawArea) ---
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

    // --- Draw Caption ---
    if FCaptionSettings.Visible and (FCaptionSettings.Text <> '') and (FCaptionRect.Width > 0) and (FCaptionRect.Height > 0) then
    begin
      var VAlign: TCaptionVerticalAlignment;
      if FCaptionSettings.Position in [cpLeft, cpRight] then
        VAlign := cvaCenter
      else // cpAbove, cpBelow
        VAlign := cvaTop; // Typically text starts at the top of its calculated rect

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

procedure TANDMR_CEdit.CaretTimerTick(Sender: TObject);
begin
  if Focused then // Only blink if focused
  begin
    FCaretVisible := not FCaretVisible;
    Invalidate; // Repaint to show/hide caret
  end
  else
  begin
    FCaretVisible := False; // Ensure caret is hidden if not focused
    FCaretTimer.Enabled := False;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.CMEnter(var Message: TCMEnter);
begin
  inherited; // Call inherited handler for CM_ENTER
  FCaretVisible := True;
  FCaretTimer.Enabled := True;
  Self.Cursor := FCurrentCursor; // Ensure correct cursor on enter
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  Invalidate; // This will trigger a repaint, which will use focus properties
end;

procedure TANDMR_CEdit.CMExit(var Message: TCMExit);
var
  TempText: string;
  OriginalFText: string;
begin
  OriginalFText := FText; // Store FText before CMExit transformations

  // Apply final transformation before exiting focus
  if FTextCase <> tcNormal then // Only transform if a specific case is set
  begin
    TempText := FText;
    case FTextCase of
      tcUppercase: TempText := System.SysUtils.UpperCase(FText);
      tcLowercase: TempText := System.SysUtils.LowerCase(FText);
    end;
    if FText <> TempText then
    begin
       FText := TempText; // Update FText directly
       // FCaretPosition might need adjustment, e.g. to Length(FText) or try to preserve
       FCaretPosition := Length(FText);
       if Assigned(FOnChange) and (OriginalFText <> FText) then // Fire OnChange if text actually changed
         FOnChange(Self);
       // No need to call Invalidate here if the inherited CMExit or subsequent Invalidate will handle it
       // However, if FText changed, an Invalidate is good.
    end;
  end;

  inherited; // Call inherited handler for CM_EXIT (which might Invalidate)
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Self.Cursor := crDefault; // Revert cursor on exit

  if Assigned(FOnExit) then
    FOnExit(Self);

  // Invalidate if text changed or if default CMExit doesn't always cover it
  if OriginalFText <> FText then
    Invalidate
  else
    Invalidate; // Standard CMExit practice to repaint for focus change
end;

procedure TANDMR_CEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldText: string;
  Changed: Boolean;
begin
  inherited KeyDown(Key, Shift);
  Changed := False;
  OldText := FText;

  if FReadOnly and not (Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_TAB, VK_RETURN]) then // Allow navigation in ReadOnly
  begin
    if not ( (Key = Ord('C')) and (ssCtrl in Shift) ) then // Allow Ctrl+C
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
            // Call SetText with the new FRawText to rebuild FMaskedText (which becomes FText)
            // and handle caret, invalidation, and OnChange.
            // Store current FText (display) to check if it changes, to prevent unnecessary OnChange/Invalidate.
            var OldDisplayText: string := FText;
            SetText(FRawText); // This will update FText (display), FMaskedText, and FRawText internally.
                               // It also handles TextCase.

            // Attempt to position caret intelligently after backspace
            // A simple approach: position before the last character of the new FRawText within FMaskedText
            var NewCaretPosInMask: Integer := 0;
            var TempRawLen: Integer := 0;
            var MaskIdx: Integer;
            for MaskIdx := 1 to Length(FMaskedText) do
            begin
              if not (FInputMask[MaskIdx] IN ['9','L','A','#']) then // It's a literal
              begin
                Inc(NewCaretPosInMask);
              end
              else // It's a placeholder
              begin
                if TempRawLen < Length(FRawText) then
                begin
                  Inc(TempRawLen);
                  Inc(NewCaretPosInMask);
                end
                else if TempRawLen = Length(FRawText) then // This is the first placeholder after the new raw text
                begin
                   Inc(NewCaretPosInMask); // Caret should be at this placeholder
                   Break;
                end
                else // Should not happen if FMaskedText is correctly built
                begin
                   Inc(NewCaretPosInMask);
                   Break;
                end;
              end;
               if MaskIdx = Length(FMaskedText) then NewCaretPosInMask := Length(FMaskedText); // If loop finishes, caret at end
            end;
            FCaretPosition := NewCaretPosInMask;
            if FCaretPosition > Length(FText) then FCaretPosition := Length(FText);


            if OldDisplayText <> FText then Changed := True else Changed := False; // Set Changed based on actual display text change
          end;
          Key := 0; // Mark key as handled
          Exit;     // Exit because we've handled it here
        end
        else // No input mask, standard backspace logic
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
          FText := Copy(FText, 1, FCaretPosition) + Copy(FText, FCaretPosition + 2, MaxInt);
          Changed := True;
        end;
      end;
    VK_HOME:
      begin
        FCaretPosition := 0;
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_END:
      begin
        FCaretPosition := Length(FText);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_LEFT:
      begin
        if FCaretPosition > 0 then Dec(FCaretPosition);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
    VK_RIGHT:
      begin
        if FCaretPosition < Length(FText) then Inc(FCaretPosition);
        Changed := True; // Not text change, but caret pos change, repaint needed
      end;
  else
    Exit; // Not a key we handle here, let default processing occur or KeyPress handle it.
  end;

  // If key was handled (i.e., one of the cases above)
  Key := 0; // Mark as handled

  if Changed then
  begin
    FCaretVisible := True; // Make sure caret is visible after action
    if Focused then
    begin
      FCaretTimer.Enabled := False; // Reset blink
      FCaretTimer.Enabled := True;
    end;

    if Assigned(FOnChange) and (OldText <> FText) then // Only trigger if text actually changed
    begin
      FOnChange(Self);
    end;
    Invalidate;
  end;
end;

procedure TANDMR_CEdit.KeyPress(var Key: Char);
var
  OldText: string;
  AllowChar: Boolean; // Added for filtering
begin
  inherited KeyPress(Key);

  if Key = #8 then // Backspace is handled in KeyDown
  begin
    Key := #0; // Prevent system beep for backspace if not handled by KeyDown somehow
    Exit;
  end;

  if FReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  // New InputType filtering logic
  // This filter applies primarily to printable characters.
  // Control characters (like Tab, Enter, Esc, etc.) are generally expected to either
  // be handled in KeyDown or allowed through KeyPress if they are not printable.
  if (FInputType <> itNormal) and (Key >= ' ') then // Filter only printable chars
  begin
    AllowChar := True; // Assume allowed, then restrict
    case FInputType of
      itLettersOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z'];
      itNumbersOnly: AllowChar := Key IN ['0'..'9'];
      itAlphaNumericOnly: AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9'];
      itNoSpecialChars: // Example: Alphanumeric + Space. Customize as needed.
        AllowChar := Key IN ['a'..'z', 'A'..'Z', '0'..'9', ' '];
      // itNormal is handled by falling through, all chars allowed by default.
    end;
    if not AllowChar then
    begin
      Key := #0; // Disallow character by setting it to null char
      // Do not Exit here, let Key := #0 be handled by the end of the procedure
    end;
  end;

  // Existing logic for handling printable characters (MaxLength, inserting char, etc.)
  // If Key was set to #0 by the filter, it will not satisfy (Key >= ' ')
  if (Key >= ' ') then // Process if Key is still a printable char (i.e., not #0 from filter)
  begin
    // Apply TextCase transformation to the incoming character
    case FTextCase of
      tcUppercase: Key := System.Character.ToUpper(Key);
      tcLowercase: Key := System.Character.ToLower(Key);
    end;

    // If InputMask is active, handle through mask logic
    if FInputMask <> '' then
    begin
      var MaskPlaceholdersCount: Integer := 0;
      var i: Integer;
      for i := 1 to Length(FInputMask) do
        if FInputMask[i] IN ['9','0','L','A','#'] then Inc(MaskPlaceholdersCount); // Added '0'

      if Length(FRawText) >= MaskPlaceholdersCount then
      begin
         Key := #0; // Mask is full, cannot add more characters to FRawText
         Exit;
      end;

      // Tentatively add the new character to FRawText and rebuild FMaskedText
      var TempRawText: string := FRawText + Key;
      var BuildRawText: string := '';
      var BuildMaskedText: string := '';
      var RawIdx: Integer := 1;
      var MaskIdx: Integer;
      var MaskDefChar: Char;
      var IsLit: Boolean;
      var CharToIns: Char;
      var CharOK: Boolean;

      for MaskIdx := 1 to Length(FInputMask) do
      begin
        MaskDefChar := FInputMask[MaskIdx];
        IsLit := not (MaskDefChar IN ['9', '0', 'L', 'A', '#']); // Added '0'
        if IsLit then
        begin
          BuildMaskedText := BuildMaskedText + MaskDefChar;
        end
        else // Placeholder
        begin
          if RawIdx <= Length(TempRawText) then
          begin
            CharToIns := TempRawText[RawIdx];
            CharOK := False;
            case MaskDefChar of
              '9', '0': CharOK := CharToIns IN ['0'..'9']; // Added '0'
              'L': CharOK := System.Character.IsLetter(CharToIns);
              'A': CharOK := System.Character.IsLetterOrDigit(CharToIns);
              '#': CharOK := True;
            end;

            if CharOK then
            begin
              BuildMaskedText := BuildMaskedText + CharToIns;
              BuildRawText := BuildRawText + CharToIns;
              Inc(RawIdx);
            end
            else // Invalid char for this specific mask placeholder
            begin
              Key := #0; // Signal that the key was invalid for the mask
              Exit;    // Stop processing this key press
            end;
          end
          else // Still placeholders in mask, but no more raw text (including the new Key)
          begin
            // This part might be reached if TempRawText was shorter than expected,
            // which shouldn't happen if CharOK logic is correct.
            // Or, it means the mask expects more chars than TempRawText provides.
            // Add placeholder to BuildMaskedText.
            BuildMaskedText := BuildMaskedText + '_';
          end;
        end;
      end;

      // If Key is still valid (#0 would mean it was rejected by mask validation)
      if Key <> #0 then
      begin
        OldText := FText; // FText holds the old FMaskedText
        FRawText := BuildRawText;
        FText := BuildMaskedText; // Update FText to new FMaskedText
        FMaskedText := BuildMaskedText; // Sync FMaskedText field
        FCaretPosition := Length(FText); // TODO: Smarter caret after insert

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
      Exit; // Exit KeyPress as mask logic has processed it
    end
    else // No InputMask, proceed with normal text insertion
    begin
      OldText := FText;
      if (FMaxLength > 0) and (Length(FText) >= FMaxLength) then
      begin
        Exit; // MaxLength reached
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
  Key := #0; // Mark key as handled if it wasn't a printable char or was handled above.
end;

procedure TANDMR_CEdit.Click;
begin
  inherited Click;
  // Most logic moved to MouseDown for immediate caret positioning and focus.
end;

procedure TANDMR_CEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  ClickX_RelativeToPaddedText: Integer;
  CurrentWidth: Integer;
  CharWidth: Integer;
  TextToMeasure: string;
  LayoutImgRect, LayoutTxtRect, LayoutSepRect: TRect; // To store results from CalculateLayout
  PaddedTextClickArea: TRect; // The actual clickable area for text
begin
  inherited MouseDown(Button, Shift, X, Y);

  Self.CalculateLayout(LayoutImgRect, LayoutTxtRect, LayoutSepRect); // Get current layout

  // Calculate the actual clickable area for text based on LayoutTxtRect and FTextMargins
  PaddedTextClickArea := LayoutTxtRect;
  PaddedTextClickArea.Left := LayoutTxtRect.Left + FTextMargins.Left;
  PaddedTextClickArea.Top := LayoutTxtRect.Top + FTextMargins.Top;
  PaddedTextClickArea.Right := LayoutTxtRect.Right - FTextMargins.Right;
  PaddedTextClickArea.Bottom := LayoutTxtRect.Bottom - FTextMargins.Bottom;

  // Ensure padded rect is not inverted
  if PaddedTextClickArea.Right < PaddedTextClickArea.Left then PaddedTextClickArea.Right := PaddedTextClickArea.Left;
  if PaddedTextClickArea.Bottom < PaddedTextClickArea.Top then PaddedTextClickArea.Bottom := PaddedTextClickArea.Top;

  if Button = mbLeft then
  begin
    if CanFocus then
    begin
      if not Focused then SetFocus
      else
      begin
        FCaretVisible := True;
        FCaretTimer.Enabled := False; FCaretTimer.Enabled := True;
      end;
    end
    else
      Exit;

    // --- Caret positioning logic using PaddedTextClickArea ---
    // Check if click is within the vertical bounds of the PaddedTextClickArea first
    if not PtInRect(PaddedTextClickArea, Point(X,Y)) then
    begin
      // Click is outside the padded text area vertically.
      // Decide behavior: either do nothing, or set caret to beginning/end based on X.
      // For simplicity, if Y is outside, but X is near, one might still want to set caret.
      // Let's refine: if Y is outside, set to 0 if X is left, Length(FText) if X is right of overall TextRect.
      if X < PaddedTextClickArea.Left then FCaretPosition := 0
      else if X >= PaddedTextClickArea.Right then FCaretPosition := Length(FText)
      else begin // Click X is within horizontal bounds, but Y is outside. Set to nearest end.
        if Y < PaddedTextClickArea.Top then FCaretPosition := 0 // Or based on X proximity to start/end
        else FCaretPosition := Length(FText); // Y is below
      end;
    end
    else if X < PaddedTextClickArea.Left then // X is to the left, Y is inside
    begin
        FCaretPosition := 0;
    end
    else if X >= PaddedTextClickArea.Right then // X is to the right (or on edge), Y is inside
    begin
        FCaretPosition := Length(FText);
    end
    else // Click is within the horizontal and vertical text area
    begin
        ClickX_RelativeToPaddedText := X - PaddedTextClickArea.Left;

        if (FPasswordChar <> #0) and not (csDesigning in ComponentState) then
          TextToMeasure := StringOfChar(FPasswordChar, Length(FText))
        else
          TextToMeasure := FText;

        Canvas.Font.Assign(Self.Font);
        CurrentWidth := 0;
        FCaretPosition := 0;

        for I := 1 to Length(TextToMeasure) do
        begin
          CharWidth := Canvas.TextWidth(TextToMeasure[I]);
          if ClickX_RelativeToPaddedText < (CurrentWidth + CharWidth div 2) then
          begin
            FCaretPosition := I - 1;
            Break;
          end;
          CurrentWidth := CurrentWidth + CharWidth;
          FCaretPosition := I;
        end;

        if ClickX_RelativeToPaddedText >= CurrentWidth then
            FCaretPosition := Length(TextToMeasure);
    end;

    Invalidate;
  end;
end;

end.
