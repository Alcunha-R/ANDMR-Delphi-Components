unit ANDMR_CButtonGroup;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  ANDMR_CButton;

type
  TButtonGroupOrientation = (bgoHorizontal, bgoVertical);

  TANDMR_CButtonGroup = class(TCustomControl)
  private
    FOrientation: TButtonGroupOrientation;
    FButtonSpacing: Integer;
    FActiveButtonIndex: Integer;
    FButtons: TObjectList; // Internal list for buttons
    FActiveButtonColor: TColor;
    FInactiveButtonColor: TColor;
    FActiveButtonFontColor: TColor;
    FInactiveButtonFontColor: TColor;

    procedure SetOrientation(Value: TButtonGroupOrientation);
    procedure SetButtonSpacing(Value: Integer);
    procedure SetActiveButtonIndex(Value: Integer);
    procedure SetActiveButtonColor(Value: TColor);
    procedure SetInactiveButtonColor(Value: TColor);
    procedure SetActiveButtonFontColor(Value: TColor);
    procedure SetInactiveButtonFontColor(Value: TColor);

    procedure UpdateButtonStyles;
    procedure ChildButtonClicked(Sender: TObject);
    procedure RearrangeButtons;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    // Protected methods and properties
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetButton(Index: Integer): TANDMR_CButton;
    function GetButtonCount: Integer;
    function IndexOf(AButton: TANDMR_CButton): Integer; // New method
    property Buttons[Index: Integer]: TANDMR_CButton read GetButton;
    property ButtonCount: Integer read GetButtonCount;
    // Public methods and properties
  published
    property Orientation: TButtonGroupOrientation read FOrientation write SetOrientation default bgoHorizontal;
    property ButtonSpacing: Integer read FButtonSpacing write SetButtonSpacing default 4;
    property ActiveButtonIndex: Integer read FActiveButtonIndex write SetActiveButtonIndex default -1;
    property ActiveButtonColor: TColor read FActiveButtonColor write SetActiveButtonColor default clHighlight;
    property InactiveButtonColor: TColor read FInactiveButtonColor write SetInactiveButtonColor default clBtnFace;
    property ActiveButtonFontColor: TColor read FActiveButtonFontColor write SetActiveButtonFontColor default clHighlightText;
    property InactiveButtonFontColor: TColor read FInactiveButtonFontColor write SetInactiveButtonFontColor default clWindowText;
  end;

procedure Register;

implementation

uses System.Generics.Collections; // Required for TObjectList if not automatically included by Delphi for older versions. Modern Delphi includes it via System.Classes.

procedure Register;
begin
  RegisterComponents('ANDMR Controls', [TANDMR_CButtonGroup]);
end;

{ TANDMR_CButtonGroup }

constructor TANDMR_CButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation := bgoHorizontal;
  FButtonSpacing := 4;
  FActiveButtonIndex := -1;
  FButtons := TObjectList.Create(False); // False: buttons are components, owned by form/group
  Width := 50; // Initial default size
  Height := 50; // Initial default size

  FActiveButtonColor := clHighlight;
  FInactiveButtonColor := clBtnFace;
  FActiveButtonFontColor := clHighlightText;
  FInactiveButtonFontColor := clWindowText;
  // Initialize other default values if necessary
end;

destructor TANDMR_CButtonGroup.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

procedure TANDMR_CButtonGroup.SetOrientation(Value: TButtonGroupOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    RearrangeButtons;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetButtonSpacing(Value: Integer);
begin
  if FButtonSpacing <> Value then
  begin
    FButtonSpacing := Value;
    RearrangeButtons;
    Invalidate;
  end;
end;

procedure TANDMR_CButtonGroup.SetActiveButtonIndex(Value: Integer);
begin
  if FActiveButtonIndex <> Value then
  begin
    FActiveButtonIndex := Value;
    UpdateButtonStyles;
    Invalidate; // Redraw group, possibly to show focus/border changes on the group itself
  end;
end;

procedure TANDMR_CButtonGroup.SetActiveButtonColor(Value: TColor);
begin
  if FActiveButtonColor <> Value then
  begin
    FActiveButtonColor := Value;
    UpdateButtonStyles;
  end;
end;

procedure TANDMR_CButtonGroup.SetInactiveButtonColor(Value: TColor);
begin
  if FInactiveButtonColor <> Value then
  begin
    FInactiveButtonColor := Value;
    UpdateButtonStyles;
  end;
end;

procedure TANDMR_CButtonGroup.SetActiveButtonFontColor(Value: TColor);
begin
  if FActiveButtonFontColor <> Value then
  begin
    FActiveButtonFontColor := Value;
    UpdateButtonStyles;
  end;
end;

procedure TANDMR_CButtonGroup.SetInactiveButtonFontColor(Value: TColor);
begin
  if FInactiveButtonFontColor <> Value then
  begin
    FInactiveButtonFontColor := Value;
    UpdateButtonStyles;
  end;
end;

procedure TANDMR_CButtonGroup.UpdateButtonStyles;
var
  I: Integer;
  AButton: TANDMR_CButton;
begin
  for I := 0 to FButtons.Count - 1 do
  begin
    AButton := TANDMR_CButton(FButtons[I]);
    if I = FActiveButtonIndex then
    begin
      AButton.ActiveColor := Self.ActiveButtonColor;
      AButton.TitleFont.Color := Self.ActiveButtonFontColor;
    end
    else
    begin
      AButton.ActiveColor := Self.InactiveButtonColor;
      AButton.TitleFont.Color := Self.InactiveButtonFontColor;
    end;
    AButton.Invalidate; // Ensure the button repaints
  end;
end;

procedure TANDMR_CButtonGroup.ChildButtonClicked(Sender: TObject);
var
  ClickedButton: TANDMR_CButton;
  I: Integer;
begin
  if Sender is TANDMR_CButton then
  begin
    ClickedButton := TANDMR_CButton(Sender);
    // Find the button in our list and set it as active
    for I := 0 to FButtons.Count - 1 do
    begin
      if TANDMR_CButton(FButtons[I]) = ClickedButton then
      begin
        Self.ActiveButtonIndex := I; // This will call SetActiveButtonIndex
        break;
      end;
    end;
  end;
end;

procedure TANDMR_CButtonGroup.Notification(AComponent: TComponent; Operation: TOperation);
var
  AButton: TANDMR_CButton;
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TANDMR_CButton then
  begin
    AButton := TANDMR_CButton(AComponent);
    if Operation = opInsert then
    begin
      // Ensure Parent property is accessed via a type known to have it (TControl or descendant)
      if (AComponent as TANDMR_CButton).Parent = Self then
      begin
        if FButtons.IndexOf(AButton) = -1 then // Avoid duplicates
        begin
          FButtons.Add(AButton);
          AButton.OnClick := ChildButtonClicked; // Assign click handler
          RearrangeButtons; // This will call UpdateButtonStyles
        end;
      end;
    end
    else if Operation = opRemove then
    begin
      if FButtons.IndexOf(AButton) <> -1 then
      begin
        AButton.OnClick := nil; // Clear click handler
        FButtons.Remove(AButton);
        RearrangeButtons; // This will call UpdateButtonStyles
        // If the removed button was the active one, reset active index
        if FActiveButtonIndex >= FButtons.Count then // Or specifically if FActiveButtonIndex was the index of removed item
        begin
            ActiveButtonIndex := -1; // Or find next valid button
        end
        else if FButtons.Count > 0 then // If list not empty, ensure styles are updated for current active
        begin
             UpdateButtonStyles; // Re-apply style to the current (possibly new) active button
        end
        else // List is empty
        begin
             ActiveButtonIndex := -1; // No button can be active
        end;

      end;
    end;
  end;
end;

procedure TANDMR_CButtonGroup.RearrangeButtons;
var
  I: Integer;
  CurrentX, CurrentY: Integer;
  MaxWidth, MaxHeight: Integer;
  AButton: TANDMR_CButton;
begin
  CurrentX := 0; // Or some internal padding
  CurrentY := 0; // Or some internal padding
  MaxWidth := 0; // Used to determine Group's Width in Vertical
  MaxHeight := 0; // Used to determine Group's Height in Horizontal

  for I := 0 to FButtons.Count - 1 do
  begin
    AButton := TANDMR_CButton(FButtons[I]);
    // Assuming buttons are alNone for now.
    // If Align is used, this logic would need to be much more complex
    // or we might need to enforce alNone for child buttons.

    if Orientation = bgoHorizontal then
    begin
      AButton.SetBounds(CurrentX, CurrentY, AButton.Width, AButton.Height);
      CurrentX := CurrentX + AButton.Width + ButtonSpacing;
      if AButton.Height > MaxHeight then
        MaxHeight := AButton.Height;
    end
    else // bgoVertical
    begin
      AButton.SetBounds(CurrentX, CurrentY, AButton.Width, AButton.Height);
      CurrentY := CurrentY + AButton.Height + ButtonSpacing;
      if AButton.Width > MaxWidth then
        MaxWidth := AButton.Width;
    end;
  end;

  if Orientation = bgoHorizontal then
  begin
    if FButtons.Count > 0 then
      MaxWidth := CurrentX - ButtonSpacing // Remove last spacing
    else
      MaxWidth := 0;
    // If MaxHeight is still 0 (no buttons), use a default or current height
    if MaxHeight = 0 then MaxHeight := Height; // Or a default small height
  end
  else // bgoVertical
  begin
    if FButtons.Count > 0 then
      MaxHeight := CurrentY - ButtonSpacing // Remove last spacing
    else
      MaxHeight := 0;
    // If MaxWidth is still 0 (no buttons), use a default or current width
    if MaxWidth = 0 then MaxWidth := Width; // Or a default small width
  end;

  // Auto-resize the group control itself
  if FButtons.Count > 0 then
  begin
    if Orientation = bgoHorizontal then
    begin
      Self.Width := MaxWidth; // MaxWidth was CurrentX - ButtonSpacing
      Self.Height := MaxHeight; // MaxHeight was max of button heights
      if Self.Height = 0 then Self.Height := inherited Height; // Default if buttons have no height
    end
    else // bgoVertical
    begin
      Self.Width := MaxWidth; // MaxWidth was max of button widths
      Self.Height := MaxHeight; // MaxHeight was CurrentY - ButtonSpacing
      if Self.Width = 0 then Self.Width := inherited Width; // Default if buttons have no width
    end;
  end
  else
  begin
     // Revert to initial size when no buttons are present
     Self.Width := 50;
     Self.Height := 50;
  end;

  UpdateButtonStyles; // Apply styles after rearranging
  Invalidate;
end;

procedure TANDMR_CButtonGroup.Paint;
begin
  inherited Paint;
  // Fill background - Vcl.Themes might override this, this is a basic fill
  Canvas.Brush.Color := clBtnFace; // Or another default color
  Canvas.FillRect(ClientRect);
  // Future: Draw group border, title, etc.
end;

function TANDMR_CButtonGroup.GetButton(Index: Integer): TANDMR_CButton;
begin
  Result := TANDMR_CButton(FButtons[Index]);
end;

function TANDMR_CButtonGroup.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TANDMR_CButtonGroup.IndexOf(AButton: TANDMR_CButton): Integer;
begin
  Result := FButtons.IndexOf(AButton);
end;

end.
