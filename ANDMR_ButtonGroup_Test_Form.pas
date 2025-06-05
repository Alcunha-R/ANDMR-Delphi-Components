unit ANDMR_ButtonGroup_Test_Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, // Added Spin for item index
  ANDMR_CButtonGroup, ANDMR_ComponentUtils;

type
  TFormButtonGroupTest = class(TForm)
    LabelDesc: TLabel;
    GroupForManualTest: TANDMR_CButtonGroup;
    MemoLog: TMemo;
    ButtonRunTests: TButton;
    LabelAddItem: TLabel;
    EditAddItemCaption: TEdit;
    ButtonAddItem: TButton;
    LabelItemIndex: TLabel;
    SpinEditItemIndex: TSpinEdit;
    ButtonRemoveItem: TButton;
    ButtonToggleItemEnabled: TButton;
    ButtonToggleItemVisible: TButton;
    procedure ButtonRunTestsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupForManualTestSelectionChanged(Sender: TObject);
    procedure ButtonAddItemClick(Sender: TObject);
    procedure ButtonRemoveItemClick(Sender: TObject);
    procedure ButtonToggleItemEnabledClick(Sender: TObject);
    procedure ButtonToggleItemVisibleClick(Sender: TObject);
  private
    { Private declarations }
    FTestButtonGroup: TANDMR_CButtonGroup; // Instance for testing
    FSelectionChangedFired: Boolean;

    procedure Log(const Msg: string);
    procedure Assert(Condition: Boolean; const Msg: string);

    // Test Procedures
    procedure TestItemManagement;
    procedure TestSelectionLogic;
    procedure TestPropertyDefaultsAndAssignments;
    procedure TestDPIScalingConceptual; // Conceptual tests
    procedure TestEdgeCases;

    procedure SetupNewTestGroup; // Helper to create a fresh group for tests
    procedure GroupForTestingSelectionChanged(Sender: TObject);

  public
    { Public declarations }
  end;

var
  FormButtonGroupTest: TFormButtonGroupTest;

implementation

{$R *.dfm}

procedure TFormButtonGroupTest.Log(const Msg: string);
begin
  MemoLog.Lines.Add(Msg);
end;

procedure TFormButtonGroupTest.Assert(Condition: Boolean; const Msg: string);
begin
  if Condition then
    Log('PASSED: ' + Msg)
  else
    Log('FAILED: ' + Msg);
end;

procedure TFormButtonGroupTest.FormCreate(Sender: TObject);
begin
  // Setup for manual testing group
  GroupForManualTest.Items.Clear;
  GroupForManualTest.Items.Add.Caption := 'Manual One';
  GroupForManualTest.Items.Add.Caption := 'Manual Two';
  GroupForManualTest.Items.Add.Caption := 'Manual Three';
  GroupForManualTest.SelectedItemIndex := 0;

  LabelDesc.Caption := 'Use the controls below to manually interact with the ButtonGroup above. ' +
                       'Use "Run All Tests" for programmatic unit tests.';
  SpinEditItemIndex.MinValue := 0;
  SpinEditItemIndex.MaxValue := Max(0, GroupForManualTest.Items.Count - 1);

  // Create the instance used for automated tests
  FTestButtonGroup := TANDMR_CButtonGroup.Create(Self);
  FTestButtonGroup.Visible := False; // Not visible on form
  FTestButtonGroup.OnSelectionChanged := GroupForTestingSelectionChanged;
end;

procedure TFormButtonGroupTest.GroupForManualTestSelectionChanged(Sender: TObject);
begin
  Log('MANUAL_TEST GroupForManualTest - SelectionChanged: Index ' + IntToStr(GroupForManualTest.SelectedItemIndex) +
      ', Text: "' + GroupForManualTest.SelectedText + '"');
  SpinEditItemIndex.MaxValue := Max(0, GroupForManualTest.Items.Count - 1);
end;

procedure TFormButtonGroupTest.GroupForTestingSelectionChanged(Sender: TObject);
begin
  FSelectionChangedFired := True;
  // Log('TEST_GROUP GroupForTesting - SelectionChanged: Index ' + IntToStr(FTestButtonGroup.SelectedItemIndex));
end;


procedure TFormButtonGroupTest.ButtonAddItemClick(Sender: TObject);
var
  NewCaption: string;
  NewItem: TANDMR_CButtonGroupItem;
begin
  NewCaption := EditAddItemCaption.Text;
  if Trim(NewCaption) = '' then
    NewCaption := 'Item ' + IntToStr(GroupForManualTest.Items.Count + 1);

  NewItem := GroupForManualTest.Items.Add;
  NewItem.Caption := NewCaption;
  Log('MANUAL_TEST Added: "' + NewCaption + '". Count: ' + IntToStr(GroupForManualTest.Items.Count));
  EditAddItemCaption.Text := '';
  SpinEditItemIndex.MaxValue := Max(0, GroupForManualTest.Items.Count - 1);
end;

procedure TFormButtonGroupTest.ButtonRemoveItemClick(Sender: TObject);
var
  IdxToRemove: Integer;
begin
  IdxToRemove := SpinEditItemIndex.Value;
  if (GroupForManualTest.Items.Count > 0) and (IdxToRemove >= 0) and (IdxToRemove < GroupForManualTest.Items.Count) then
  begin
    Log('MANUAL_TEST Removing item at index: ' + IntToStr(IdxToRemove) + ' ("' + GroupForManualTest.Items[IdxToRemove].Caption + '")');
    GroupForManualTest.Items.Delete(IdxToRemove); // TCollection uses Delete
    Log('MANUAL_TEST Item removed. Count: ' + IntToStr(GroupForManualTest.Items.Count));
  end
  else
    Log('MANUAL_TEST Cannot remove. Invalid index or no items.');
  SpinEditItemIndex.MaxValue := Max(0, GroupForManualTest.Items.Count - 1);
end;

procedure TFormButtonGroupTest.ButtonToggleItemEnabledClick(Sender: TObject);
var
  IdxToToggle: Integer;
begin
  IdxToToggle := SpinEditItemIndex.Value;
  if (IdxToToggle >= 0) and (IdxToToggle < GroupForManualTest.Items.Count) then
  begin
    GroupForManualTest.Items[IdxToToggle].Enabled := not GroupForManualTest.Items[IdxToToggle].Enabled;
    Log('MANUAL_TEST Item "' + GroupForManualTest.Items[IdxToToggle].Caption + '" Enabled: ' + BoolToStr(GroupForManualTest.Items[IdxToToggle].Enabled, True));
  end;
end;

procedure TFormButtonGroupTest.ButtonToggleItemVisibleClick(Sender: TObject);
var
  IdxToToggle: Integer;
begin
  IdxToToggle := SpinEditItemIndex.Value;
  if (IdxToToggle >= 0) and (IdxToToggle < GroupForManualTest.Items.Count) then
  begin
    GroupForManualTest.Items[IdxToToggle].Visible := not GroupForManualTest.Items[IdxToToggle].Visible;
    Log('MANUAL_TEST Item "' + GroupForManualTest.Items[IdxToToggle].Caption + '" Visible: ' + BoolToStr(GroupForManualTest.Items[IdxToToggle].Visible, True));
  end;
end;

procedure TFormButtonGroupTest.ButtonRunTestsClick(Sender: TObject);
begin
  MemoLog.Clear;
  Log('--- Starting All Tests ---');

  TestItemManagement;
  TestSelectionLogic;
  TestPropertyDefaultsAndAssignments;
  TestDPIScalingConceptual;
  TestEdgeCases;

  Log('--- All Tests Finished ---');
end;

procedure TFormButtonGroupTest.SetupNewTestGroup;
begin
  FTestButtonGroup.Items.Clear;
  FTestButtonGroup.SelectedItemIndex := -1; // Reset selection
  FSelectionChangedFired := False;
end;

// --- Test Procedures Implementation ---

procedure TFormButtonGroupTest.TestItemManagement;
var
  Item: TANDMR_CButtonGroupItem;
  InitialCount: Integer;
begin
  Log('--- Testing Item Management ---');
  SetupNewTestGroup;

  // TestAddItem
  FTestButtonGroup.Items.Add.Caption := 'Test One';
  FTestButtonGroup.Items.Add.Caption := 'Test Two';
  Assert(FTestButtonGroup.Items.Count = 2, 'TestAddItem: Count should be 2 after adding two items.');

  // TestRemoveItem
  InitialCount := FTestButtonGroup.Items.Count;
  if InitialCount > 0 then
  begin
    FTestButtonGroup.Items.Delete(0);
    Assert(FTestButtonGroup.Items.Count = InitialCount - 1, 'TestRemoveItem: Count should decrement after removing an item.');
    if FTestButtonGroup.Items.Count > 0 then
      Assert(FTestButtonGroup.Items[0].Caption = 'Test Two', 'TestRemoveItem: Remaining item caption check.');
  end;
  SetupNewTestGroup; // Reset for next tests

  // TestAccessItemByIndex
  FTestButtonGroup.Items.Add.Caption := 'AccessMe';
  Assert(Assigned(FTestButtonGroup.Items[0]) and (FTestButtonGroup.Items[0].Caption = 'AccessMe'), 'TestAccessItemByIndex: Accessing item by index.');

  // TestModifyItemCaption
  Item := FTestButtonGroup.Items.Add;
  Item.Caption := 'OldCaption';
  Item.Caption := 'NewCaption';
  Assert(Item.Caption = 'NewCaption', 'TestModifyItemCaption: Caption should be updated.');

  // TestModifyItemEnabled
  Item := FTestButtonGroup.Items.Add;
  Item.Caption := 'EnabledTest';
  Item.Enabled := False;
  Assert(not Item.Enabled, 'TestModifyItemEnabled: Item Enabled should be False.');
  Item.Enabled := True;
  Assert(Item.Enabled, 'TestModifyItemEnabled: Item Enabled should be True.');
  // Test if disabling a selected item deselects it
  SetupNewTestGroup;
  Item := FTestButtonGroup.Items.Add; Item.Caption := 'SelectAndDisable';
  FTestButtonGroup.SelectedItemIndex := 0;
  Assert(FTestButtonGroup.SelectedItemIndex = 0, 'TestModifyItemEnabled: Item initially selected.');
  Item.Enabled := False; // This should trigger update in collection and deselect
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestModifyItemEnabled: Selected item when disabled should become deselected.');


  // TestModifyItemVisible
  SetupNewTestGroup;
  Item := FTestButtonGroup.Items.Add;
  Item.Caption := 'VisibleTest';
  Item.Visible := False;
  Assert(not Item.Visible, 'TestModifyItemVisible: Item Visible should be False.');
  // Test if making a selected item invisible deselects it
  FTestButtonGroup.SelectedItemIndex := 0; // Try to select it (it's invisible, so SetSelectedIndex should prevent this)
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestModifyItemVisible: Attempting to select invisible item should fail.');
  Item.Visible := True;
  Assert(Item.Visible, 'TestModifyItemVisible: Item Visible should be True.');
  FTestButtonGroup.SelectedItemIndex := 0; // Now it should be selectable
  Assert(FTestButtonGroup.SelectedItemIndex = 0, 'TestModifyItemVisible: Visible item should be selectable.');
  Item.Visible := False; // This should trigger update in collection and deselect
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestModifyItemVisible: Selected item when made invisible should become deselected.');


  // TestModifyItemTag
  Item := FTestButtonGroup.Items.Add;
  Item.Tag := 123;
  Assert(Item.Tag = 123, 'TestModifyItemTag: Tag should be updated.');
end;

procedure TFormButtonGroupTest.TestSelectionLogic;
var
  Item1, Item2, Item3: TANDMR_CButtonGroupItem;
begin
  Log('--- Testing Selection Logic ---');
  SetupNewTestGroup;

  Item1 := FTestButtonGroup.Items.Add; Item1.Caption := 'Select One';
  Item2 := FTestButtonGroup.Items.Add; Item2.Caption := 'Select Two';
  Item3 := FTestButtonGroup.Items.Add; Item3.Caption := 'Select Three (Disabled)'; Item3.Enabled := False;
  FTestButtonGroup.Items.Add.Caption := 'Select Four (Invisible)';
  FTestButtonGroup.Items[3].Visible := False;


  // TestSetSelectedIndexProgrammatic & SelectedText
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := 0;
  Assert(FTestButtonGroup.SelectedItemIndex = 0, 'TestSetSelectedIndexProgrammatic: Index should be 0.');
  Assert(FTestButtonGroup.SelectedText = 'Select One', 'TestSetSelectedIndexProgrammatic: SelectedText should be "Select One".');
  Assert(FSelectionChangedFired, 'TestSetSelectedIndexProgrammatic: OnSelectionChanged should have fired.');

  // TestSetSelectedIndexSingleSelection
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := 1;
  Assert(FTestButtonGroup.SelectedItemIndex = 1, 'TestSetSelectedIndexSingleSelection: Index should be 1.');
  Assert(FTestButtonGroup.SelectedText = 'Select Two', 'TestSetSelectedIndexSingleSelection: SelectedText should be "Select Two".');
  Assert(FSelectionChangedFired, 'TestSetSelectedIndexSingleSelection: OnSelectionChanged should have fired again.');

  // TestSelectDisabledItem
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := 2; // Index of "Select Three (Disabled)"
  Assert(FTestButtonGroup.SelectedItemIndex = 1, 'TestSelectDisabledItem: Index should remain 1 (previous selection), cannot select disabled.');
  Assert(not FSelectionChangedFired, 'TestSelectDisabledItem: OnSelectionChanged should NOT have fired.');

  // TestSelectInvisibleItem
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := 3; // Index of "Select Four (Invisible)"
  Assert(FTestButtonGroup.SelectedItemIndex = 1, 'TestSelectInvisibleItem: Index should remain 1, cannot select invisible.');
  Assert(not FSelectionChangedFired, 'TestSelectInvisibleItem: OnSelectionChanged should NOT have fired.');

  // TestOnSelectionChangedEvent (already tested implicitly above)
  Log('PASSED: TestOnSelectionChangedEvent: Verified implicitly by previous tests.');

  // TestSetSelectedIndexOutOfBounds
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := -1;
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestSetSelectedIndexOutOfBounds: Index should be -1 for no selection.');
  Assert(FTestButtonGroup.SelectedText = '', 'TestSetSelectedIndexOutOfBounds: SelectedText should be empty for -1 index.');
  // Only fire if changed from 1 to -1
  Assert(FSelectionChangedFired, 'TestSetSelectedIndexOutOfBounds: OnSelectionChanged should have fired for -1.');

  FTestButtonGroup.SelectedItemIndex := 0; // Reselect for next test
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := FTestButtonGroup.Items.Count + 5; // Out of bounds high
  // Current logic in SetSelectedItemIndex reverts to old index if target is invalid, or clamps.
  // The refined logic: if Value >= FItems.Count then NewIndex := OldIndex
  Assert(FTestButtonGroup.SelectedItemIndex = 0, 'TestSetSelectedIndexOutOfBounds: Index should remain 0 (previous selection) for out of bounds high.');
  Assert(not FSelectionChangedFired, 'TestSetSelectedIndexOutOfBounds: OnSelectionChanged should NOT fire for out of bounds high if index does not change.');

  FTestButtonGroup.SelectedItemIndex := -2; // Out of bounds low (becomes -1)
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestSetSelectedIndexOutOfBounds: Index should become -1 for out of bounds low (-2).');

end;

procedure TFormButtonGroupTest.TestPropertyDefaultsAndAssignments;
var
  TestGroup: TANDMR_CButtonGroup;
begin
  Log('--- Testing Property Defaults and Assignments ---');
  TestGroup := TANDMR_CButtonGroup.Create(nil); // Create without owner for this test
  try
    // TestDefaultColors
    Assert(TestGroup.SelectedItemBackgroundColor = TColor($00D19A6C), 'TestDefaultColors: SelectedItemBackgroundColor default.');
    Assert(TestGroup.HoverFontColor = clNone, 'TestDefaultColors: HoverFontColor default.');
    Assert(TestGroup.DisabledItemBackgroundColor = TColor($00F0F0F0), 'TestDefaultColors: DisabledItemBackgroundColor default.');

    // TestAssignColors
    TestGroup.SelectedItemBackgroundColor := clRed;
    Assert(TestGroup.SelectedItemBackgroundColor = clRed, 'TestAssignColors: Assign SelectedItemBackgroundColor.');
    TestGroup.HoverFontColor := clGreen;
    Assert(TestGroup.HoverFontColor = clGreen, 'TestAssignColors: Assign HoverFontColor.');
    TestGroup.DisabledItemBackgroundColor := clBlue;
    Assert(TestGroup.DisabledItemBackgroundColor = clBlue, 'TestAssignColors: Assign DisabledItemBackgroundColor.');
  finally
    TestGroup.Free;
  end;
end;

procedure TFormButtonGroupTest.TestDPIScalingConceptual;
var
  TestGroup: TANDMR_CButtonGroup;
  OriginalThickness, OriginalRadius: Integer;
begin
  Log('--- Testing DPI Scaling (Conceptual) ---');
  TestGroup := TANDMR_CButtonGroup.Create(nil);
  try
    // These tests don't verify actual scaling, just property access
    OriginalThickness := 2;
    TestGroup.BorderSettings.Thickness := OriginalThickness;
    Assert(TestGroup.BorderSettings.Thickness = OriginalThickness, 'TestDPIScalingConceptual: BorderSettings.Thickness can be set and read.');

    OriginalRadius := 10;
    TestGroup.BorderSettings.CornerRadius := OriginalRadius;
    Assert(TestGroup.BorderSettings.CornerRadius = OriginalRadius, 'TestDPIScalingConceptual: BorderSettings.CornerRadius can be set and read.');

    // In a real DPI change scenario, if these were set at 96 DPI,
    // their values in Paint (e.g., Scaled(OriginalThickness)) would be larger at higher DPIs.
    Log('NOTE: True DPI scaling requires visual inspection or a more complex test environment.');
  finally
    TestGroup.Free;
  end;
end;

procedure TFormButtonGroupTest.TestEdgeCases;
var
  Item: TANDMR_CButtonGroupItem;
begin
  Log('--- Testing Edge Cases ---');
  SetupNewTestGroup;

  // TestNoItems
  Assert(FTestButtonGroup.Items.Count = 0, 'TestNoItems: Group should have 0 items initially.');
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestNoItems: SelectedItemIndex should be -1.');
  Assert(FTestButtonGroup.SelectedText = '', 'TestNoItems: SelectedText should be empty.');
  FTestButtonGroup.SelectedItemIndex := 0; // Try to select
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestNoItems: SelectedItemIndex should remain -1 when no items.');

  // TestAllItemsDisabled
  SetupNewTestGroup;
  FTestButtonGroup.Items.Add.Caption := 'Disabled1'; FTestButtonGroup.Items[0].Enabled := False;
  FTestButtonGroup.Items.Add.Caption := 'Disabled2'; FTestButtonGroup.Items[1].Enabled := False;
  FTestButtonGroup.SelectedItemIndex := 0;
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestAllItemsDisabled: Cannot select first disabled item.');
  FTestButtonGroup.SelectedItemIndex := 1;
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestAllItemsDisabled: Cannot select second disabled item.');

  // TestAllItemsInvisible
  SetupNewTestGroup;
  FTestButtonGroup.Items.Add.Caption := 'Invisible1'; FTestButtonGroup.Items[0].Visible := False;
  FTestButtonGroup.Items.Add.Caption := 'Invisible2'; FTestButtonGroup.Items[1].Visible := False;
  FTestButtonGroup.SelectedItemIndex := 0;
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestAllItemsInvisible: Cannot select first invisible item.');
  FTestButtonGroup.SelectedItemIndex := 1;
  Assert(FTestButtonGroup.SelectedItemIndex = -1, 'TestAllItemsInvisible: Cannot select second invisible item.');

  // TestComponentDisabled
  SetupNewTestGroup;
  FTestButtonGroup.Items.Add.Caption := 'EnabledItem';
  FTestButtonGroup.Enabled := False;
  Assert(not FTestButtonGroup.Enabled, 'TestComponentDisabled: Component is disabled.');
  // Mouse interactions are blocked by VCL if control is disabled.
  // Programmatic selection change might still be possible, let's test current behavior:
  FSelectionChangedFired := False;
  FTestButtonGroup.SelectedItemIndex := 0;
  // Current implementation of SetSelectedItemIndex does not check FTestButtonGroup.Enabled.
  // This means programmatic selection is possible even if component is disabled.
  // The Paint method, however, correctly styles disabled items.
  Assert(FTestButtonGroup.SelectedItemIndex = 0, 'TestComponentDisabled: Programmatic selection is currently possible on disabled component.');
  Assert(FSelectionChangedFired, 'TestComponentDisabled: OnSelectionChanged fires for programmatic change on disabled component.');
  Log('NOTE: TestComponentDisabled - Programmatic selection on a disabled component is possible. Visual state handled by Paint.');

end;

end.
