unit ButtonGroupTestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, // Added ExtCtrls for TLabel
  ANDMR_CButtonGroup, ANDMR_CButton, ANDMR_ComponentUtils; // Ensure ANDMR_ComponentUtils is used

type
  TFormButtonGroupTest = class(TForm)
    bgHorizontal: TANDMR_CButtonGroup;
    bgVertical: TANDMR_CButtonGroup;
    btnAddH: TButton;
    btnAddV: TButton;
    btnClearH: TButton;
    btnClearV: TButton;
    chkHorzOrientation: TCheckBox;
    lblSelectedH: TLabel;
    lblSelectedV: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnAddHClick(Sender: TObject);
    procedure btnAddVClick(Sender: TObject);
    procedure btnClearHClick(Sender: TObject);
    procedure btnClearVClick(Sender: TObject);
    procedure chkHorzOrientationClick(Sender: TObject);
    procedure BgHorizontalClick(Sender: TObject);
    procedure BgVerticalClick(Sender: TObject);
  private
    FHCounter: Integer;
    FVCounter: Integer;
  public
    { Public declarations }
  end;

var
  FormButtonGroupTest: TFormButtonGroupTest;

implementation

{$R *.dfm}

procedure TFormButtonGroupTest.FormCreate(Sender: TObject);
var
  i: Integer;
  NewBtn: TANDMR_CButton;
begin
  FHCounter := 0;
  FVCounter := 0;

  // Add initial buttons to Horizontal Group
  for i := 1 to 3 do
  begin
    Inc(FHCounter);
    NewBtn := bgHorizontal.AddButton('H' + IntToStr(FHCounter));
    // Optionally, customize NewBtn further if needed for testing
    // Example: NewBtn.ImageSettings.Picture.LoadFromFile('some_icon.png');
  end;

  // Add initial buttons to Vertical Group
  for i := 1 to 2 do
  begin
    Inc(FVCounter);
    bgVertical.AddButton('V' + IntToStr(FVCounter));
  end;

  // Assign OnClick handlers to update labels
  bgHorizontal.OnClick := BgHorizontalClick;
  bgVertical.OnClick := BgVerticalClick;

  // Initial label update
  BgHorizontalClick(bgHorizontal);
  BgVerticalClick(bgVertical);
end;

procedure TFormButtonGroupTest.btnAddHClick(Sender: TObject);
begin
  Inc(FHCounter);
  bgHorizontal.AddButton('H' + IntToStr(FHCounter));
end;

procedure TFormButtonGroupTest.btnAddVClick(Sender: TObject);
begin
  Inc(FVCounter);
  bgVertical.AddButton('V' + IntToStr(FVCounter));
end;

procedure TFormButtonGroupTest.btnClearHClick(Sender: TObject);
begin
  bgHorizontal.ClearButtons;
  FHCounter := 0;
  BgHorizontalClick(bgHorizontal); // Update label
end;

procedure TFormButtonGroupTest.btnClearVClick(Sender: TObject);
begin
  bgVertical.ClearButtons;
  FVCounter := 0;
  BgVerticalClick(bgVertical); // Update label
end;

procedure TFormButtonGroupTest.chkHorzOrientationClick(Sender: TObject);
begin
  if chkHorzOrientation.Checked then
    bgHorizontal.Orientation := bgoVertical
  else
    bgHorizontal.Orientation := bgoHorizontal;
end;

procedure TFormButtonGroupTest.BgHorizontalClick(Sender: TObject);
begin
  if Assigned(bgHorizontal.SelectedButton) then
    lblSelectedH.Caption := 'Selected H: ' + bgHorizontal.SelectedButton.Caption
  else
    lblSelectedH.Caption := 'Selected H: None';
end;

procedure TFormButtonGroupTest.BgVerticalClick(Sender: TObject);
begin
  if Assigned(bgVertical.SelectedButton) then
    lblSelectedV.Caption := 'Selected V: ' + bgVertical.SelectedButton.Caption
  else
    lblSelectedV.Caption := 'Selected V: None';
end;

end.
