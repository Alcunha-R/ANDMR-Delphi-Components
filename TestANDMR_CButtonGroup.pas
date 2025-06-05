unit TestANDMR_CButtonGroup;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes, // For TColor
  Vcl.Controls, // For TCustomControl and parenting if needed
  DUnitX.TestFramework,
  ANDMR_CButtonGroup,
  ANDMR_CButton,
  ANDMR_ComponentUtils;

type
  // Test fixture for TANDMR_CButtonGroup
  [TestFixture]
  TTestANDMR_CButtonGroup = class(TObject)
  private
    FHostForm: TCustomForm; // Host form for components that need a parent window handle
    FButtonGroup: TANDMR_CButtonGroup;
    procedure SafeFreeButtonGroup;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Test Cases
    [Test]
    procedure TestInitialProperties;
    [Test]
    procedure TestAddButtonsProgrammaticallyAndCount;
    [Test]
    procedure TestRemoveButtonsAndCount;
    [Test]
    procedure TestHorizontalLayout;
    [Test]
    procedure TestVerticalLayout;
    [Test]
    procedure TestActiveButtonSelectionAndStyleViaProperty;
    [Test]
    procedure TestActiveButtonSelectionAndStyleViaClick;
    [Test]
    procedure TestActiveButtonIndexUpdateOnRemove;
    [Test]
    procedure TestButtonSpacingAffectsLayout;
    [Test]
    procedure TestOrientationChangeAffectsLayout;
  end;

implementation

{ TTestANDMR_CButtonGroup }

procedure TTestANDMR_CButtonGroup.Setup;
begin
  // Create a host form if components require a window handle or parenting
  // For many VCL components, this is necessary for them to behave correctly.
  FHostForm := TCustomForm.Create(nil);
  // FHostForm.Show; // Not usually needed for unit tests unless visibility is tested

  FButtonGroup := nil; // Ensure it's nil before each test, if not using local var
end;

procedure TTestANDMR_CButtonGroup.SafeFreeButtonGroup;
var
  i: Integer;
  TempButton: TANDMR_CButton;
begin
  if Assigned(FButtonGroup) then
  begin
    // Free buttons owned by the group first, if any were not parented
    // However, our buttons are parented, so they should be freed when group is freed,
    // or if we free them manually, notification should handle it.
    // For safety, if buttons were created without parent and added, this would be needed:
    // while FButtonGroup.ButtonCount > 0 do
    // begin
    //   FButtonGroup.Buttons[0].Free; // Assuming opRemove handles FButtons list
    // end;
    // Or, more directly if they are components on the form:
    if Assigned(FButtonGroup.FButtons) then // Accessing private FButtons for test cleanup
    begin
      for i := FButtonGroup.FButtons.Count - 1 downto 0 do
      begin
        TempButton := TANDMR_CButton(FButtonGroup.FButtons[i]);
        // If they were parented to FButtonGroup, FButtonGroup.Free will handle them.
        // If they were parented to FHostForm, FHostForm.Free will handle them.
        // If they were created with nil owner and parented to FButtonGroup, FButtonGroup.Free handles.
      end;
    end;
    FButtonGroup.Free;
    FButtonGroup := nil;
  end;
end;

procedure TTestANDMR_CButtonGroup.TearDown;
begin
  SafeFreeButtonGroup;
  FHostForm.Free;
  FHostForm := nil;
end;

procedure TTestANDMR_CButtonGroup.TestInitialProperties;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm); // Parent to host form
  FButtonGroup.Parent := FHostForm; // Set parent for window handle, etc.
  try
    Assert.AreEqual(bgoHorizontal, FButtonGroup.Orientation, 'Initial Orientation');
    Assert.AreEqual(4, FButtonGroup.ButtonSpacing, 'Initial ButtonSpacing');
    Assert.AreEqual(-1, FButtonGroup.ActiveButtonIndex, 'Initial ActiveButtonIndex');
    Assert.AreEqual(0, FButtonGroup.ButtonCount, 'Initial ButtonCount');
    Assert.AreEqual(TColor(clHighlight), FButtonGroup.ActiveButtonColor, 'Initial ActiveButtonColor');
    Assert.AreEqual(TColor(clBtnFace), FButtonGroup.InactiveButtonColor, 'Initial InactiveButtonColor');
    Assert.AreEqual(TColor(clHighlightText), FButtonGroup.ActiveButtonFontColor, 'Initial ActiveButtonFontColor');
    Assert.AreEqual(TColor(clWindowText), FButtonGroup.InactiveButtonFontColor, 'Initial InactiveButtonFontColor');
  finally
    // SafeFreeButtonGroup will be called by TearDown
  end;
end;

procedure TTestANDMR_CButtonGroup.TestAddButtonsProgrammaticallyAndCount;
var
  Button1, Button2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;

  Button1 := TANDMR_CButton.Create(FButtonGroup); // Owner is group
  Button1.Parent := FButtonGroup; // Parent to group, triggers Notification

  Button2 := TANDMR_CButton.Create(FButtonGroup); // Owner is group
  Button2.Parent := FButtonGroup;

  try
    Assert.AreEqual(2, FButtonGroup.ButtonCount, 'ButtonCount after adding two');
    Assert.AreSame(Button1, FButtonGroup.Buttons[0], 'Button 1 reference');
    Assert.AreSame(Button2, FButtonGroup.Buttons[1], 'Button 2 reference');
  finally
    // Buttons are owned by FButtonGroup, will be freed with it.
  end;
end;

procedure TTestANDMR_CButtonGroup.TestRemoveButtonsAndCount;
var
  Button1, Button2, Button3: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;

  Button1 := TANDMR_CButton.Create(FButtonGroup); Button1.Name := 'Button1_TestRemove'; Button1.Parent := FButtonGroup;
  Button2 := TANDMR_CButton.Create(FButtonGroup); Button2.Name := 'Button2_TestRemove'; Button2.Parent := FButtonGroup;
  Button3 := TANDMR_CButton.Create(FButtonGroup); Button3.Name := 'Button3_TestRemove'; Button3.Parent := FButtonGroup;

  Assert.AreEqual(3, FButtonGroup.ButtonCount, 'Initial count for removal test');
  FButtonGroup.ActiveButtonIndex := 1; // Button2 is active
  Assert.AreSame(Button2, FButtonGroup.Buttons[1], 'Button2 should be at index 1');
  Assert.AreEqual(1, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex should be 1');

  // Remove the active button
  Button2.Free; // This should trigger Notification opRemove
  Button2 := nil; // Avoid dangling pointer

  Assert.AreEqual(2, FButtonGroup.ButtonCount, 'ButtonCount after removing active Button2');
  // ActiveButtonIndex should update. The logic in TANDMR_CButtonGroup.Notification is:
  // if FActiveButtonIndex >= FButtons.Count then ActiveButtonIndex := -1;
  // Original FActiveButtonIndex = 1. FButtons.Count becomes 2. 1 < 2 is false.
  // Let's re-check the logic in TANDMR_CButtonGroup:
  // if FActiveButtonIndex = OriginalIndexOfRemoved (1) then NewActiveIndex := -1
  // This seems to be the behavior from my latest reasoning for the component.
  Assert.AreEqual(-1, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex after removing active button');
  Assert.AreSame(Button1, FButtonGroup.Buttons[0], 'Button1 should now be at index 0');
  Assert.AreSame(Button3, FButtonGroup.Buttons[1], 'Button3 should now be at index 1');


  // Remove another button (Button1, which is at index 0)
  FButtonGroup.ActiveButtonIndex := 1; // Make Button3 active (now at index 1)
  Assert.AreEqual(1, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex should be 1 (Button3)');
  Button1.Free;
  Button1 := nil;

  Assert.AreEqual(1, FButtonGroup.ButtonCount, 'ButtonCount after removing Button1');
  // Button3 was at index 1, Button1 (at index 0) was removed.
  // ActiveButtonIndex (1) > OriginalIndexOfRemoved (0) -> NewActiveIndex = ActiveButtonIndex -1 = 0.
  Assert.AreEqual(0, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex after removing Button1 (was Button3)');
  Assert.AreSame(Button3, FButtonGroup.Buttons[0], 'Button3 should now be at index 0 after Button1 removed');
end;


procedure TTestANDMR_CButtonGroup.TestHorizontalLayout;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.Orientation := bgoHorizontal;
  FButtonGroup.ButtonSpacing := 5;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup;
  Btn1.Width := 50; Btn1.Height := 30;

  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup;
  Btn2.Width := 60; Btn2.Height := 25; // Different height

  // RearrangeButtons is called by Parent assignment, and by setting Orientation.
  // If direct call is needed: FButtonGroup.RearrangeButtons;

  Assert.AreEqual(0, Btn1.Left, 'Btn1.Left');
  Assert.AreEqual(0, Btn1.Top, 'Btn1.Top');

  Assert.AreEqual(Btn1.Width + FButtonGroup.ButtonSpacing, Btn2.Left, 'Btn2.Left');
  Assert.AreEqual(0, Btn2.Top, 'Btn2.Top'); // Top aligns for horizontal

  // Group size
  Assert.AreEqual(Btn1.Width + FButtonGroup.ButtonSpacing + Btn2.Width, FButtonGroup.Width, 'Group Width');
  Assert.AreEqual(Max(Btn1.Height, Btn2.Height), FButtonGroup.Height, 'Group Height');
end;

procedure TTestANDMR_CButtonGroup.TestVerticalLayout;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.Orientation := bgoVertical; // Set before adding buttons for clarity
  FButtonGroup.ButtonSpacing := 10;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup;
  Btn1.Width := 40; Btn1.Height := 20;

  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup;
  Btn2.Width := 45; Btn2.Height := 22; // Different width

  Assert.AreEqual(0, Btn1.Left, 'Btn1.Left');
  Assert.AreEqual(0, Btn1.Top, 'Btn1.Top');

  Assert.AreEqual(0, Btn2.Left, 'Btn2.Left'); // Left aligns for vertical
  Assert.AreEqual(Btn1.Height + FButtonGroup.ButtonSpacing, Btn2.Top, 'Btn2.Top');

  // Group size
  Assert.AreEqual(Max(Btn1.Width, Btn2.Width), FButtonGroup.Width, 'Group Width');
  Assert.AreEqual(Btn1.Height + FButtonGroup.ButtonSpacing + Btn2.Height, FButtonGroup.Height, 'Group Height');
end;

procedure TTestANDMR_CButtonGroup.TestActiveButtonSelectionAndStyleViaProperty;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.ActiveButtonColor := clRed;
  FButtonGroup.InactiveButtonColor := clBlue;
  FButtonGroup.ActiveButtonFontColor := clYellow;
  FButtonGroup.InactiveButtonFontColor := clGreen;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'B1';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'B2';

  FButtonGroup.ActiveButtonIndex := 0; // Select Btn1

  Assert.AreEqual(FButtonGroup.ActiveButtonColor, Btn1.ActiveColor, 'Btn1 ActiveColor');
  Assert.AreEqual(FButtonGroup.ActiveButtonFontColor, Btn1.TitleFont.Color, 'Btn1 FontColor');
  Assert.AreEqual(FButtonGroup.InactiveButtonColor, Btn2.ActiveColor, 'Btn2 InactiveColor');
  Assert.AreEqual(FButtonGroup.InactiveButtonFontColor, Btn2.TitleFont.Color, 'Btn2 FontColor');

  FButtonGroup.ActiveButtonIndex := 1; // Select Btn2

  Assert.AreEqual(FButtonGroup.InactiveButtonColor, Btn1.ActiveColor, 'Btn1 InactiveColor after change');
  Assert.AreEqual(FButtonGroup.InactiveButtonFontColor, Btn1.TitleFont.Color, 'Btn1 FontColor after change');
  Assert.AreEqual(FButtonGroup.ActiveButtonColor, Btn2.ActiveColor, 'Btn2 ActiveColor after change');
  Assert.AreEqual(FButtonGroup.ActiveButtonFontColor, Btn2.TitleFont.Color, 'Btn2 FontColor after change');
end;

procedure TTestANDMR_CButtonGroup.TestActiveButtonSelectionAndStyleViaClick;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.ActiveButtonColor := clFuchsia;
  FButtonGroup.InactiveButtonColor := clMaroon;
  FButtonGroup.ActiveButtonFontColor := clAqua;
  FButtonGroup.InactiveButtonFontColor := clOlive;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'BC1';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'BC2';

  // Initially, all should be inactive as ActiveButtonIndex is -1
  Assert.AreEqual(FButtonGroup.InactiveButtonColor, Btn1.ActiveColor, 'Btn1 Initial InactiveColor');
  Assert.AreEqual(FButtonGroup.InactiveButtonFontColor, Btn1.TitleFont.Color, 'Btn1 Initial FontColor');

  Btn1.Click; // Simulate click

  Assert.AreEqual(0, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex after Btn1 click');
  Assert.AreEqual(FButtonGroup.ActiveButtonColor, Btn1.ActiveColor, 'Btn1 ActiveColor after click');
  Assert.AreEqual(FButtonGroup.ActiveButtonFontColor, Btn1.TitleFont.Color, 'Btn1 FontColor after click');
  Assert.AreEqual(FButtonGroup.InactiveButtonColor, Btn2.ActiveColor, 'Btn2 InactiveColor after Btn1 click');
  Assert.AreEqual(FButtonGroup.InactiveButtonFontColor, Btn2.TitleFont.Color, 'Btn2 FontColor after Btn1 click');

  Btn2.Click; // Simulate click on Btn2

  Assert.AreEqual(1, FButtonGroup.ActiveButtonIndex, 'ActiveButtonIndex after Btn2 click');
  Assert.AreEqual(FButtonGroup.InactiveButtonColor, Btn1.ActiveColor, 'Btn1 InactiveColor after Btn2 click');
  Assert.AreEqual(FButtonGroup.InactiveButtonFontColor, Btn1.TitleFont.Color, 'Btn1 FontColor after Btn2 click');
  Assert.AreEqual(FButtonGroup.ActiveButtonColor, Btn2.ActiveColor, 'Btn2 ActiveColor after Btn2 click');
  Assert.AreEqual(FButtonGroup.ActiveButtonFontColor, Btn2.TitleFont.Color, 'Btn2 FontColor after Btn2 click');
end;

procedure TTestANDMR_CButtonGroup.TestActiveButtonIndexUpdateOnRemove;
var
  Btn1, Btn2, Btn3: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;

  // Scenario 1: Remove active button (middle one)
  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'B_R1';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'B_R2';
  Btn3 := TANDMR_CButton.Create(FButtonGroup); Btn3.Parent := FButtonGroup; Btn3.Name := 'B_R3';
  FButtonGroup.ActiveButtonIndex := 1; // Btn2 is active
  Btn2.Free; Btn2 := nil;
  Assert.AreEqual(-1, FButtonGroup.ActiveButtonIndex, 'Scenario 1: Active index after removing active middle button');
  // Free remaining
  Btn1.Free; Btn3.Free;
  Assert.AreEqual(0, FButtonGroup.ButtonCount, 'Button count should be 0 after freeing all');


  // Scenario 2: Remove first button when it's active
  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'B_R4';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'B_R5';
  FButtonGroup.ActiveButtonIndex := 0; // Btn1 is active
  Btn1.Free; Btn1 := nil;
  // Current logic: if active is removed, index becomes -1. Then if list not empty, it updates styles.
  // If we want to select the new button at index 0, the logic in TANDMR_CButtonGroup would need to be:
  // if FActiveButtonIndex = OriginalIndexOfRemoved then NewActiveIndex := if FButtons.Count > 0 then 0 else -1;
  // As per current implementation, it becomes -1
  Assert.AreEqual(-1, FButtonGroup.ActiveButtonIndex, 'Scenario 2: Active index after removing active first button');
  Btn2.Free;

  // Scenario 3: Remove first button when a LATER button is active
  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'B_R6';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'B_R7';
  Btn3 := TANDMR_CButton.Create(FButtonGroup); Btn3.Parent := FButtonGroup; Btn3.Name := 'B_R8';
  FButtonGroup.ActiveButtonIndex := 2; // Btn3 is active
  Btn1.Free; Btn1 := nil; // Remove Btn1
  // Btn3 was at index 2. Btn1 (index 0) removed. Btn3 is now at index 1.
  // ActiveButtonIndex (2) > OriginalIndexOfRemoved (0) -> NewActiveIndex = ActiveButtonIndex - 1 = 1.
  Assert.AreEqual(1, FButtonGroup.ActiveButtonIndex, 'Scenario 3: Active index after removing first button (active was Btn3)');
  Assert.AreSame(Btn3, FButtonGroup.Buttons[1], 'Scenario 3: Btn3 should be at index 1');
  Btn2.Free; Btn3.Free;

  // Scenario 4: Remove last button when it's active
  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup; Btn1.Name := 'B_R9';
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup; Btn2.Name := 'B_R10';
  FButtonGroup.ActiveButtonIndex := 1; // Btn2 is active
  Btn2.Free; Btn2 := nil;
  // Active button removed, becomes -1.
  Assert.AreEqual(-1, FButtonGroup.ActiveButtonIndex, 'Scenario 4: Active index after removing active last button');
  Btn1.Free;
end;

procedure TTestANDMR_CButtonGroup.TestButtonSpacingAffectsLayout;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.Orientation := bgoHorizontal;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup;
  Btn1.Width := 50; Btn1.Height := 30;
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup;
  Btn2.Width := 60; Btn2.Height := 25;

  FButtonGroup.ButtonSpacing := 10; // Change spacing
  // FButtonGroup.RearrangeButtons; // Setter for ButtonSpacing should call it.

  Assert.AreEqual(Btn1.Width + 10, Btn2.Left, 'Btn2.Left after spacing change');
  Assert.AreEqual(Btn1.Width + 10 + Btn2.Width, FButtonGroup.Width, 'Group Width after spacing change');
end;

procedure TTestANDMR_CButtonGroup.TestOrientationChangeAffectsLayout;
var
  Btn1, Btn2: TANDMR_CButton;
begin
  FButtonGroup := TANDMR_CButtonGroup.Create(FHostForm);
  FButtonGroup.Parent := FHostForm;
  FButtonGroup.ButtonSpacing := 5;

  Btn1 := TANDMR_CButton.Create(FButtonGroup); Btn1.Parent := FButtonGroup;
  Btn1.Width := 50; Btn1.Height := 30;
  Btn2 := TANDMR_CButton.Create(FButtonGroup); Btn2.Parent := FButtonGroup;
  Btn2.Width := 60; Btn2.Height := 25;

  // Initial: Horizontal
  Assert.AreEqual(Btn1.Width + 5, Btn2.Left, 'Btn2.Left initial (horizontal)');
  Assert.AreEqual(0, Btn2.Top, 'Btn2.Top initial (horizontal)');

  FButtonGroup.Orientation := bgoVertical; // Change orientation
  // FButtonGroup.RearrangeButtons; // Setter for Orientation should call it.

  Assert.AreEqual(0, Btn2.Left, 'Btn2.Left after orientation change to vertical');
  Assert.AreEqual(Btn1.Height + 5, Btn2.Top, 'Btn2.Top after orientation change to vertical');

  Assert.AreEqual(Max(Btn1.Width, Btn2.Width), FButtonGroup.Width, 'Group Width after vertical');
  Assert.AreEqual(Btn1.Height + 5 + Btn2.Height, FButtonGroup.Height, 'Group Height after vertical');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestANDMR_CButtonGroup);
end.
