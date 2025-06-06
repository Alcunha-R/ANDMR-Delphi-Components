unit ANDMR_CDatePicker;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.DateUtils, System.UITypes,
  System.Math,
  Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls,
  Winapi.Windows, Winapi.Messages,
  ANDMR_ComponentUtils;

type
  { Forward declaration for keyboard focus tracking }
  TFocusedDate = record
  private
    FDate: TDate;
    FIsValid: Boolean;
  public
    procedure Invalidate;
    procedure SetDate(ADate: TDate);
    property Date: TDate read FDate;
    property IsValid: Boolean read FIsValid;
  end;

  { Main Date Picker Component }
  TANDMR_CDatePicker = class(TCustomControl)
  private
    //--- Core Date Properties
    FStartDate: TDate;
    FEndDate: TDate;
    FMinDate: TDate;
    FMaxDate: TDate;
    FCurrentMonth: Word;
    FCurrentYear: Word;

    //--- User Interaction State
    FFocusedDate: TFocusedDate; // For keyboard navigation
    FHotDay: Integer;      // Day number under the mouse cursor (hover effect)
    FIsMouseDown: Boolean;    // Track if a mouse button is pressed

    //--- Layout & Drawing Rects
    FPrevMonthRect: TRect;
    FNextMonthRect: TRect;
    FCellWidth: Integer;
    FCellHeight: Integer;
    FCalendarGridStartY: Integer;

    //--- Published Appearance Properties
    FRangeColor: TColor;
    FTodayBorderColor: TColor;
    FSelectionFontColor: TColor;
    FSelectionColor: TColor;
    FHotBackColor: TColor;
    FHotFontColor: TColor;
    FFocusedBorderColor: TColor;
    FShowTodayIndicator: Boolean;
    FShowRangeHighlight: Boolean;

    //--- Events
    FOnChange: TNotifyEvent;

    //--- Setters for Properties
    procedure SetStartDate(const Value: TDate);
    procedure SetEndDate(const Value: TDate);
    procedure SetMinDate(const Value: TDate);
    procedure SetMaxDate(const Value: TDate);
    procedure SetRangeColor(const Value: TColor);
    procedure SetTodayBorderColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetHotBackColor(const Value: TColor);
    procedure SetHotFontColor(const Value: TColor);
    procedure SetFocusedBorderColor(const Value: TColor);
    procedure SetShowTodayIndicator(const Value: Boolean);
    procedure SetShowRangeHighlight(const Value: Boolean);

    //--- Internal Logic & Drawing
    procedure DrawCalendar(ACanvas: TCanvas);
    procedure DrawDayCell(ACanvas: TCanvas; const ARect: TRect; ADay: Integer;
      AIsSelectedStart, AIsSelectedEnd, AIsInRange, AIsToday, AIsFocused, AIsHot, AIsEnabled: Boolean);
    procedure DrawButton(ACanvas: TCanvas; const ARect: TRect; const ACaption: string; AIsPressed, AEnabled: Boolean);
    procedure CalculateLayout;
    function GetDayFromPoint(const APoint: TPoint): Integer;
    function IsDateEnabled(const ADate: TDate): Boolean;
    procedure UpdateHotDay(NewHotDay: Integer);
    procedure UpdateFocusedDate(NewDate: TDate; AForceVisible: Boolean = True);
    procedure ChangeMonth(ADelta: Integer);
    procedure DoChange;

    //--- Event Handlers
    procedure DayCellClick(ADay: Integer);

  protected
    //--- Overridden Methods
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    //--- Message Handlers for specific Windows messages
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMGotFocus(var Message: TMessage); message CM_GOTFOCUS;
    procedure CMLostFocus(var Message: TMessage); message CM_LOSTFOCUS;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ClearSelection;
    procedure GoToToday;

  published
    //--- Core Properties
    property StartDate: TDate read FStartDate write SetStartDate;
    property EndDate: TDate read FEndDate write SetEndDate;
    property MinDate: TDate read FMinDate write SetMinDate;
    property MaxDate: TDate read FMaxDate write SetMaxDate;

    //--- Appearance Properties
    property RangeColor: TColor read FRangeColor write SetRangeColor;
    property TodayBorderColor: TColor read FTodayBorderColor write SetTodayBorderColor default clRed;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor default clHighlightText;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property HotBackColor: TColor read FHotBackColor write SetHotBackColor;
    property HotFontColor: TColor read FHotFontColor write SetHotFontColor;
    property FocusedBorderColor: TColor read FFocusedBorderColor write SetFocusedBorderColor;
    property ShowTodayIndicator: Boolean read FShowTodayIndicator write SetShowTodayIndicator default True;
    property ShowRangeHighlight: Boolean read FShowRangeHighlight write SetShowRangeHighlight default True;

    //--- Standard Control Properties
    property Align;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property Enabled default True;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    //--- Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

const
  //--- Layout constants for better maintainability
  HEADER_HEIGHT = 30;
  DAY_NAMES_HEIGHT = 20;
  NAV_BUTTON_WIDTH = 70;
  CELL_PADDING = 2;
  MIN_CELL_DIM = 20; // Minimum width and height for a day cell
  INVALID_DAY = 0;   // Represents an invalid or empty day cell
  DAYS_IN_WEEK = 7;
  MAX_WEEKS_IN_VIEW = 6;

  //--- Text Format Flags for DrawText API
  DT_CENTER_FLAGS = DT_CENTER or DT_VCENTER or DT_SINGLELINE;

procedure Register;
begin
  RegisterComponents('ANDMR', [TANDMR_CDatePicker]);
end;

{ TFocusedDate }

procedure TFocusedDate.Invalidate;
begin
  FIsValid := False;
  FDate := 0;
end;

procedure TFocusedDate.SetDate(ADate: TDate);
begin
  FDate := ADate;
  FIsValid := True;
end;

{ TANDMR_CDatePicker }

constructor TANDMR_CDatePicker.Create(AOwner: TComponent);
var
  LToday: TDate;
  LDay: Word;
begin
  inherited Create(AOwner);
  //--- Set initial control style and dimensions
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks, csCaptureMouse,
    csReplicatable, csNeedsBorderPaint, csAcceptsControls, csClickEvents, csSetCaption];
  Width := 250;
  Height := 280;
  TabStop := True;

  //--- Initialize date properties
  LToday := Date; // Use function call for clarity
  FStartDate := 0;
  FEndDate := 0;
  FMinDate := 0;
  FMaxDate := 0;
  DecodeDate(LToday, FCurrentYear, FCurrentMonth, LDay);
  FFocusedDate.SetDate(LToday);

  //--- Initialize state properties
  FHotDay := INVALID_DAY;
  FIsMouseDown := False;

  //--- Initialize appearance properties with default values
  // BlendColors is in ANDMR_ComponentUtils, assumed to be a helper function
  FRangeColor         := BlendColors(clWindow, clHighlight, 0.20);
  FTodayBorderColor   := clRed;
  FSelectionFontColor := clHighlightText;
  FSelectionColor     := clHighlight;
  FFocusedBorderColor := clGray;
  FHotBackColor       := BlendColors(Color, clGray, 0.15);
  FHotFontColor       := Font.Color;
  FShowTodayIndicator := True;
  FShowRangeHighlight := True;

  //--- Set default font and color
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  Color := clWindow;
  ParentColor := False;

  //--- Perform initial layout calculation
  CalculateLayout;
end;

//------------------------------------------------------------------------------
// Region: Main Drawing and Layout
//------------------------------------------------------------------------------
{$REGION 'Main Drawing and Layout'}
procedure TANDMR_CDatePicker.Paint;
begin
  inherited Paint;
  DrawCalendar(Canvas);

  //--- Draw focus rectangle around the entire control when it has focus
  if Focused and TabStop and Enabled then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clGrayText;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect); // Use ClientRect for better alignment
  end;
end;

procedure TANDMR_CDatePicker.CalculateLayout;
var
  LClientWidth: Integer;
begin
  // At design-time, when the component is first dropped, it has no handle.
  // In this case, ClientWidth would raise an exception.
  // We can safely use the Width property as an approximation because borders are not a factor here.
  if HandleAllocated then
    LClientWidth := Self.ClientWidth
  else
    LClientWidth := Self.Width;

  // This function calculates the sizes and positions of all calendar elements.
  // It's called on creation and resize, but NOT during Paint for efficiency.
  FCalendarGridStartY := HEADER_HEIGHT + DAY_NAMES_HEIGHT + CELL_PADDING;

  FPrevMonthRect := System.Types.Rect(CELL_PADDING, CELL_PADDING, CELL_PADDING + NAV_BUTTON_WIDTH, HEADER_HEIGHT - CELL_PADDING);
  FNextMonthRect := System.Types.Rect(Width - NAV_BUTTON_WIDTH - CELL_PADDING, CELL_PADDING, Width - CELL_PADDING, HEADER_HEIGHT - CELL_PADDING);

  //--- Calculate cell dimensions based on available space
  if LClientWidth > (2 * CELL_PADDING) then
    FCellWidth := Max(MIN_CELL_DIM, (LClientWidth - (2 * CELL_PADDING)) div DAYS_IN_WEEK)
  else
    FCellWidth := MIN_CELL_DIM;

  if Height > (FCalendarGridStartY + CELL_PADDING) then
    FCellHeight := Max(MIN_CELL_DIM, (Height - FCalendarGridStartY - CELL_PADDING) div MAX_WEEKS_IN_VIEW)
  else
    FCellHeight := MIN_CELL_DIM;
end;

procedure TANDMR_CDatePicker.DrawCalendar(ACanvas: TCanvas);
var
  i, LDaysInMonth, LCurrentDay, Row, Col: Integer;
  LDateToDraw, LFirstDayOfMonth: TDate;
  LIsSelectedStart, LIsSelectedEnd, LIsInRange, LIsToday, LIsFocused, LIsHot, LIsEnabled: Boolean;
  LMonthYearStr: string;
  LFirstDayOffset: Integer;
  LGridRect, LHeaderMonthRect, LDayNameRect: TRect;
  LMousePos: TPoint;
begin
  //--- Don't draw if component is too small to display anything meaningful
  if (FCellWidth <= 0) or (FCellHeight <= 0) then Exit;

  LFirstDayOfMonth := EncodeDate(FCurrentYear, FCurrentMonth, 1);
  LMousePos := ScreenToClient(Mouse.CursorPos);

  //--- 1. Draw Header (Month/Year and Nav Buttons)
  LMonthYearStr := FormatDateTime('mmmm yyyy', LFirstDayOfMonth);
  ACanvas.Font.Assign(Self.Font);
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Size := ACanvas.Font.Size + 1;
  ACanvas.Brush.Style := bsClear;
  LHeaderMonthRect := System.Types.Rect(FPrevMonthRect.Right, FPrevMonthRect.Top, FNextMonthRect.Left, FPrevMonthRect.Bottom);
  DrawText(ACanvas.Handle, PChar(LMonthYearStr), -1, LHeaderMonthRect, DT_CENTER_FLAGS);
  DrawButton(ACanvas, FPrevMonthRect, '<', FIsMouseDown and PtInRect(FPrevMonthRect, LMousePos), True);
  DrawButton(ACanvas, FNextMonthRect, '>', FIsMouseDown and PtInRect(FNextMonthRect, LMousePos), True);
  ACanvas.Brush.Style := bsSolid;

  //--- 2. Draw Day Names (Sun, Mon, Tue...)
  ACanvas.Font.Assign(Self.Font);
  ACanvas.Font.Style := [fsBold];
  LGridRect := System.Types.Rect(CELL_PADDING, HEADER_HEIGHT + CELL_PADDING, Width - CELL_PADDING, HEADER_HEIGHT + DAY_NAMES_HEIGHT);
  for i := 0 to High(FormatSettings.ShortDayNames) do
  begin
    LDayNameRect := System.Types.Rect(LGridRect.Left + i * FCellWidth, LGridRect.Top, LGridRect.Left + (i + 1) * FCellWidth, LGridRect.Bottom);
    // Adjust index for 1-based ShortDayNames array
    DrawText(ACanvas.Handle, PChar(FormatSettings.ShortDayNames[i + 1]), -1, LDayNameRect, DT_CENTER_FLAGS);
  end;

  //--- 3. Draw Day Cells
  LDaysInMonth := DaysInMonth(LFirstDayOfMonth);
  LFirstDayOffset := DayOfTheWeek(LFirstDayOfMonth) - 1; // 0=Sun, 1=Mon...
  LCurrentDay := 1;

  for Row := 0 to MAX_WEEKS_IN_VIEW - 1 do
  begin
    for Col := 0 to DAYS_IN_WEEK - 1 do
    begin
      LGridRect := System.Types.Rect(
        CELL_PADDING + Col * FCellWidth,
        FCalendarGridStartY + Row * FCellHeight,
        CELL_PADDING + (Col + 1) * FCellWidth,
        FCalendarGridStartY + (Row + 1) * FCellHeight
      );
      InflateRect(LGridRect, -CELL_PADDING, -CELL_PADDING);

      if (Row * DAYS_IN_WEEK + Col >= LFirstDayOffset) and (LCurrentDay <= LDaysInMonth) then
      begin
        // This is a valid day cell for the current month
        LDateToDraw := EncodeDate(FCurrentYear, FCurrentMonth, LCurrentDay);

        // Determine the state of the day
        LIsEnabled       := IsDateEnabled(LDateToDraw);
        LIsSelectedStart := (FStartDate <> 0) and SameDate(LDateToDraw, FStartDate);
        LIsSelectedEnd   := (FEndDate <> 0) and SameDate(LDateToDraw, FEndDate);
        LIsInRange       := FShowRangeHighlight and (FStartDate <> 0) and (FEndDate <> 0) and (FStartDate < FEndDate) and (LDateToDraw > FStartDate) and (LDateToDraw < FEndDate);
        LIsToday         := SameDate(LDateToDraw, Date);
        LIsFocused       := FFocusedDate.IsValid and SameDate(LDateToDraw, FFocusedDate.Date);
        LIsHot           := (LCurrentDay = FHotDay);

        DrawDayCell(ACanvas, LGridRect, LCurrentDay, LIsSelectedStart, LIsSelectedEnd, LIsInRange, LIsToday, LIsFocused, LIsHot, LIsEnabled);
        Inc(LCurrentDay);
      end
      else
      begin
        // This is an empty cell (before start of month or after end).
        ACanvas.Brush.Color := Self.Color;
        ACanvas.FillRect(LGridRect);
      end;
    end;
    if LCurrentDay > LDaysInMonth then Break; // Stop drawing rows if all days are drawn
  end;
end;

procedure TANDMR_CDatePicker.DrawDayCell(ACanvas: TCanvas; const ARect: TRect; ADay: Integer;
  AIsSelectedStart, AIsSelectedEnd, AIsInRange, AIsToday, AIsFocused, AIsHot, AIsEnabled: Boolean);
var
  LDayStr: string;
  LTextColor, LBackgroundColor: TColor;
  LFocusRect: TRect;
begin
  LDayStr := IntToStr(ADay);
  ACanvas.Font.Assign(Self.Font);

  //--- Determine background and text color based on the day's state hierarchy
  if not AIsEnabled then
  begin
    LBackgroundColor := BlendColors(Self.Color, clGray, 0.1);
    LTextColor := clGrayText;
  end
  else
  begin
    LBackgroundColor := Self.Color;
    LTextColor := Self.Font.Color;
    if AIsInRange then LBackgroundColor := FRangeColor;
    if AIsHot then
    begin
      LBackgroundColor := FHotBackColor;
      LTextColor := FHotFontColor;
    end;
    if AIsSelectedStart or AIsSelectedEnd then
    begin
      LBackgroundColor := FSelectionColor;
      LTextColor := FSelectionFontColor;
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
    end;
  end;

  //--- 1. Fill background
  ACanvas.Brush.Color := LBackgroundColor;
  ACanvas.Pen.Style := psClear;
  ACanvas.Rectangle(ARect);

  //--- 2. Draw Borders
  ACanvas.Brush.Style := bsClear;
  if FShowTodayIndicator and AIsToday then
  begin
    ACanvas.Pen.Color := FTodayBorderColor;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.Rectangle(ARect);
  end;
  if Focused and AIsFocused and AIsEnabled then
  begin
    LFocusRect := ARect;
    InflateRect(LFocusRect, -1, -1);
    ACanvas.Pen.Color := FFocusedBorderColor;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Width := 1;
    ACanvas.Rectangle(LFocusRect);
  end;

  //--- 3. Draw Day Number
  var TempRect := ARect; // DrawText requires a non-const TRect
  ACanvas.Font.Color := LTextColor;
  ACanvas.Brush.Style := bsClear;
  DrawText(ACanvas.Handle, PChar(LDayStr), -1, TempRect, DT_CENTER_FLAGS);
end;

procedure TANDMR_CDatePicker.DrawButton(ACanvas: TCanvas; const ARect: TRect; const ACaption: string; AIsPressed, AEnabled: Boolean);
var
  LTempRect: TRect;
  LBackgroundColor: TColor;
  LTextColor: TColor;
begin
  LTempRect := ARect; // Create a local, modifiable copy for DrawEdge

  // --- 1. Determine Colors based on State ---
  if not AEnabled then
  begin
    LBackgroundColor := clBtnFace;
    LTextColor := clGrayText;
  end
  else if AIsPressed then
  begin
    // Use slightly darker color for pressed state to give feedback
    LBackgroundColor := clBtnShadow;
    LTextColor := clBtnText;
  end
  else
  begin
    // Normal state
    LBackgroundColor := clBtnFace;
    LTextColor := clBtnText;
  end;

  // --- 2. Draw Button Background ---
  ACanvas.Brush.Color := LBackgroundColor;
  ACanvas.FillRect(ARect);

  // --- 3. Draw Button Border (for a 3D effect) ---
  // The DrawEdge WinAPI call provides a simple 3D border effect
  if AEnabled then
  begin
    if AIsPressed then
      // Sunken edge for pressed look
      DrawEdge(ACanvas.Handle, LTempRect, BDR_SUNKENINNER, BF_RECT)
    else
      // Raised edge for normal look
      DrawEdge(ACanvas.Handle, LTempRect, BDR_RAISEDINNER, BF_RECT);
  end
  else
  begin
    // A simple single border for disabled state
    ACanvas.Pen.Color := clGray;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(ARect);
  end;

  // --- 4. Draw Caption ---
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := LTextColor;
  // Use the same temporary rect for DrawText
  DrawText(ACanvas.Handle, PChar(ACaption), -1, LTempRect, DT_CENTER_FLAGS);
end;
{$ENDREGION}

//------------------------------------------------------------------------------
// Region: User Interaction (Mouse & Keyboard)
//------------------------------------------------------------------------------
{$REGION 'User Interaction (Mouse & Keyboard)'}
procedure TANDMR_CDatePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LPt: TPoint;
  LClickedDay: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  LPt := System.Types.Point(X, Y);

  if (Button = mbLeft) and Enabled then
  begin
    FIsMouseDown := True;
    if PtInRect(FPrevMonthRect, LPt) or PtInRect(FNextMonthRect, LPt) then
    begin
      Invalidate;
    end
    else
    begin
      LClickedDay := GetDayFromPoint(LPt);
      if LClickedDay <> INVALID_DAY then
        DayCellClick(LClickedDay);
    end;
  end;
end;

procedure TANDMR_CDatePicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LPt: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled and FIsMouseDown then
  begin
    FIsMouseDown := False;
    LPt := System.Types.Point(X, Y);
    if PtInRect(FPrevMonthRect, LPt) then
      ChangeMonth(-1)
    else if PtInRect(FNextMonthRect, LPt) then
      ChangeMonth(1)
    else
      Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LPt: TPoint;
  LDayUnderCursor: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not Enabled then Exit;

  LPt := System.Types.Point(X, Y);
  LDayUnderCursor := GetDayFromPoint(LPt);
  UpdateHotDay(LDayUnderCursor);
end;

procedure TANDMR_CDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  LNewFocusedDate: TDate;
begin
  inherited KeyDown(Key, Shift);
  if not (Enabled and TabStop) then Exit;

  if not FFocusedDate.IsValid then
  begin
    UpdateFocusedDate(Date);
    if not FFocusedDate.IsValid then Exit;
  end;

  LNewFocusedDate := FFocusedDate.Date;
  case Key of
    VK_LEFT:  LNewFocusedDate := IncDay(FFocusedDate.Date, -1);
    VK_RIGHT: LNewFocusedDate := IncDay(FFocusedDate.Date, +1);
    VK_UP:    LNewFocusedDate := IncDay(FFocusedDate.Date, -DAYS_IN_WEEK);
    VK_DOWN:  LNewFocusedDate := IncDay(FFocusedDate.Date, +DAYS_IN_WEEK);
    VK_PRIOR: if ssCtrl in Shift then
                LNewFocusedDate := IncYear(FFocusedDate.Date, -1)
              else
                LNewFocusedDate := IncMonth(FFocusedDate.Date, -1);
    VK_NEXT:  if ssCtrl in Shift then
                LNewFocusedDate := IncYear(FFocusedDate.Date, 1)
              else
                LNewFocusedDate := IncMonth(FFocusedDate.Date, 1);
    VK_HOME:
      begin
        if ssCtrl in Shift then
          LNewFocusedDate := Date
        else
          LNewFocusedDate := StartOfTheMonth(FFocusedDate.Date);
      end;
    VK_END:   LNewFocusedDate := EndOfTheMonth(FFocusedDate.Date);
    VK_RETURN, VK_SPACE:
      begin
        DayCellClick(DayOf(FFocusedDate.Date));
        Key := 0;
        Exit;
      end;
  else
    Exit;
  end;

  UpdateFocusedDate(LNewFocusedDate);
  Key := 0;
end;

procedure TANDMR_CDatePicker.DayCellClick(ADay: Integer);
var
  LClickedDate: TDate;
begin
  if ADay = INVALID_DAY then Exit;

  try
    LClickedDate := EncodeDate(FCurrentYear, FCurrentMonth, ADay);
  except
    on EConvertError do Exit;
  end;

  if not IsDateEnabled(LClickedDate) then Exit;

  UpdateFocusedDate(LClickedDate, False);

  if (FStartDate <> 0) and (FEndDate <> 0) then
  begin
    FStartDate := LClickedDate;
    FEndDate := 0;
  end
  else if (FStartDate = 0) then
  begin
    FStartDate := LClickedDate;
    FEndDate := 0;
  end
  else
  begin
    if LClickedDate < FStartDate then
    begin
      FEndDate := FStartDate;
      FStartDate := LClickedDate;
    end
    else
    begin
      FEndDate := LClickedDate;
    end;
  end;

  DoChange;
end;
{$ENDREGION}

//------------------------------------------------------------------------------
// Region: Internal Helper Functions
//------------------------------------------------------------------------------
{$REGION 'Internal Helper Functions'}
function TANDMR_CDatePicker.GetDayFromPoint(const APoint: TPoint): Integer;
var
  Col, Row, FirstDayOffset, DayIndex: Integer;
  LFirstDayOfMonth: TDate;
begin
  Result := INVALID_DAY;
  if (FCellWidth <= 0) or (FCellHeight <= 0) or
     (APoint.Y < FCalendarGridStartY) or (APoint.X < CELL_PADDING) or
     (APoint.X >= (CELL_PADDING + DAYS_IN_WEEK * FCellWidth)) then
    Exit;

  Col := (APoint.X - CELL_PADDING) div FCellWidth;
  Row := (APoint.Y - FCalendarGridStartY) div FCellHeight;

  if (Col < 0) or (Col >= DAYS_IN_WEEK) or (Row < 0) or (Row >= MAX_WEEKS_IN_VIEW) then
    Exit;

  LFirstDayOfMonth := EncodeDate(FCurrentYear, FCurrentMonth, 1);
  FirstDayOffset := DayOfTheWeek(LFirstDayOfMonth) - 1;
  DayIndex := (Row * DAYS_IN_WEEK) + Col - FirstDayOffset + 1;

  if (DayIndex >= 1) and (DayIndex <= DaysInMonth(LFirstDayOfMonth)) then
    Result := DayIndex;
end;

function TANDMR_CDatePicker.IsDateEnabled(const ADate: TDate): Boolean;
begin
  Result := True;
  if (FMinDate <> 0) and (ADate < FMinDate) then Exit(False);
  if (FMaxDate <> 0) and (ADate > FMaxDate) then Exit(False);
end;

procedure TANDMR_CDatePicker.UpdateHotDay(NewHotDay: Integer);
begin
  if FHotDay <> NewHotDay then
  begin
    FHotDay := NewHotDay;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.UpdateFocusedDate(NewDate: TDate; AForceVisible: Boolean);
var
  LOldFocusedDate: TDate;
  LDay: Word;
begin
  if not IsDateEnabled(NewDate) then Exit;

  LOldFocusedDate := 0;
  if FFocusedDate.IsValid then
    LOldFocusedDate := FFocusedDate.Date;

  if SameDate(NewDate, LOldFocusedDate) then Exit;

  FFocusedDate.SetDate(NewDate);
  if AForceVisible or (YearOf(NewDate) <> FCurrentYear) or (MonthOf(NewDate) <> FCurrentMonth) then
  begin
    DecodeDate(NewDate, FCurrentYear, FCurrentMonth, LDay);
  end;
  Invalidate;
end;

procedure TANDMR_CDatePicker.ChangeMonth(ADelta: Integer);
var
  LCurrentDate: TDate;
  LDay: Word; // Dummy variable for the day part, which we don't need here.
begin
  // Create a TDate representing the first day of the currently viewed month.
  LCurrentDate := EncodeDate(FCurrentYear, FCurrentMonth, 1);
  // Calculate the new date by adding the month delta using the IncMonth function.
  LCurrentDate := IncMonth(LCurrentDate, ADelta);
  // Update the current year and month fields from the new date.
  DecodeDate(LCurrentDate, FCurrentYear, FCurrentMonth, LDay);
  // Repaint the control to show the new month.
  Invalidate;
end;

procedure TANDMR_CDatePicker.DoChange;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;
{$ENDREGION}

//------------------------------------------------------------------------------
// Region: Public Methods
//------------------------------------------------------------------------------
{$REGION 'Public Methods'}
procedure TANDMR_CDatePicker.ClearSelection;
begin
  if (FStartDate <> 0) or (FEndDate <> 0) then
  begin
    FStartDate := 0;
    FEndDate := 0;
    DoChange;
  end;
end;

procedure TANDMR_CDatePicker.GoToToday;
begin
  UpdateFocusedDate(Date);
end;
{$ENDREGION}

//------------------------------------------------------------------------------
// Region: Overridden Focus and Message Handlers
//------------------------------------------------------------------------------
{$REGION 'Overridden Focus and Message Handlers'}
procedure TANDMR_CDatePicker.WMSize(var Message: TWMSize);
begin
  inherited;
  CalculateLayout;
  Invalidate;
end;

procedure TANDMR_CDatePicker.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TANDMR_CDatePicker.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  UpdateHotDay(INVALID_DAY);
end;

procedure TANDMR_CDatePicker.CMGotFocus(var Message: TMessage);
begin
  inherited;
  if not FFocusedDate.IsValid then
  begin
    if (FStartDate <> 0) and IsDateEnabled(FStartDate) then
      UpdateFocusedDate(FStartDate)
    else
      GoToToday;
  end;
  Invalidate;
end;

procedure TANDMR_CDatePicker.CMLostFocus(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
{$ENDREGION}

//------------------------------------------------------------------------------
// Region: Property Setters
//------------------------------------------------------------------------------
{$REGION 'Property Setters'}
procedure TANDMR_CDatePicker.SetStartDate(const Value: TDate);
begin
  if FStartDate <> Value then
  begin
    FStartDate := Value;
    if (FEndDate <> 0) and (FStartDate > FEndDate) then
    begin
      FEndDate := FStartDate;
    end;
    DoChange;
  end;
end;

procedure TANDMR_CDatePicker.SetEndDate(const Value: TDate);
begin
  if FEndDate <> Value then
  begin
    FEndDate := Value;
    if (FStartDate <> 0) and (FEndDate < FStartDate) then
    begin
      FStartDate := FEndDate;
    end;
    DoChange;
  end;
end;

procedure TANDMR_CDatePicker.SetMinDate(const Value: TDate);
begin
  if FMinDate <> Value then
  begin
    FMinDate := Value;
    if (FStartDate <> 0) and (FStartDate < FMinDate) then FStartDate := 0;
    if (FEndDate <> 0) and (FEndDate < FMinDate) then FEndDate := 0;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetMaxDate(const Value: TDate);
begin
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    if (FStartDate <> 0) and (FStartDate > FMaxDate) then FStartDate := 0;
    if (FEndDate <> 0) and (FEndDate > FMaxDate) then FEndDate := 0;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetRangeColor(const Value: TColor);
begin
  if FRangeColor <> Value then
  begin
    FRangeColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetTodayBorderColor(const Value: TColor);
begin
  if FTodayBorderColor <> Value then
  begin
    FTodayBorderColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetSelectionFontColor(const Value: TColor);
begin
  if FSelectionFontColor <> Value then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetSelectionColor(const Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetHotBackColor(const Value: TColor);
begin
  if FHotBackColor <> Value then
  begin
    FHotBackColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetHotFontColor(const Value: TColor);
begin
  if FHotFontColor <> Value then
  begin
    FHotFontColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetFocusedBorderColor(const Value: TColor);
begin
  if FFocusedBorderColor <> Value then
  begin
    FFocusedBorderColor := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetShowTodayIndicator(const Value: Boolean);
begin
  if FShowTodayIndicator <> Value then
  begin
    FShowTodayIndicator := Value;
    Invalidate;
  end;
end;

procedure TANDMR_CDatePicker.SetShowRangeHighlight(const Value: Boolean);
begin
  if FShowRangeHighlight <> Value then
  begin
    FShowRangeHighlight := Value;
    Invalidate;
  end;
end;
{$ENDREGION}

end.
