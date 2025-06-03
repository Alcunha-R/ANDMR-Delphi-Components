unit ANDMR_CheckBox_Test_Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ANDMR_CCheckBox, ANDMR_ComponentUtils; // Make sure ANDMR_CCheckBox is in uses

type
  TFormTestCheckBox = class(TForm)
    cbxLightDefault: TANDMR_CCheckBox;
    cbxDarkSolid: TANDMR_CCheckBox;
    cbxMaterialBordered: TANDMR_CCheckBox;
    cbxFlatChecked: TANDMR_CCheckBox;
    MemoLog: TMemo;
    cbxModernWithImage: TANDMR_CCheckBox;
    cbxIOS_Styled: TANDMR_CCheckBox;
    cbxWin11_Disabled: TANDMR_CCheckBox;
    cbxTransparent: TANDMR_CCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GeneralCheckBoxClick(Sender: TObject);
    procedure GeneralCheckBoxChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestCheckBox: TFormTestCheckBox;

implementation

{$R *.dfm}

procedure TFormTestCheckBox.FormCreate(Sender: TObject);
begin
  // You could assign images or complex properties programmatically here if needed
  // Example for cbxModernWithImage (if you have a TPicture instance ready):
  // if Assigned(cbxModernWithImage.Image.Picture) then
  // begin
  //   // cbxModernWithImage.Image.Picture.LoadFromFile('path_to_your_image.png');
  //   cbxModernWithImage.Image.Visible := True;
  //   cbxModernWithImage.Image.DrawMode := idmProportional;
  //   cbxModernWithImage.Image.Margins.Left := 4;
  // end;

  // Set caption for caption properties for the modern checkbox
  // cbxModernWithImage.CaptionProperties.Text := 'Modern Image'; // DFM sets Caption property
  // cbxModernWithImage.CaptionProperties.Position := cpRight;
  // cbxModernWithImage.CaptionProperties.Offset := Point(6,0);

   // Update caption properties for cbxDarkSolid as Font.Color is set in DFM but CaptionProperties.Color might not be
   cbxDarkSolid.CaptionProperties.Font.Color := clWhite;
   cbxDarkSolid.CaptionProperties.Color := clWhite;


  MemoLog.Clear;
  MemoLog.Lines.Add('TANDMR_CCheckBox Test Application Started.');
  MemoLog.Lines.Add('------------------------------------------');
end;

procedure TFormTestCheckBox.GeneralCheckBoxClick(Sender: TObject);
var
  CheckBox: TANDMR_CCheckBox;
begin
  if Sender is TANDMR_CCheckBox then
  begin
    CheckBox := TANDMR_CCheckBox(Sender);
    MemoLog.Lines.Add(Format('OnClick: %s, Checked: %s', [CheckBox.Name, BoolToStr(CheckBox.Checked, True)]));
  end;
end;

procedure TFormTestCheckBox.GeneralCheckBoxChange(Sender: TObject);
var
  CheckBox: TANDMR_CCheckBox;
begin
  if Sender is TANDMR_CCheckBox then
  begin
    CheckBox := TANDMR_CCheckBox(Sender);
    MemoLog.Lines.Add(Format('OnChange: %s, Checked: %s', [CheckBox.Name, BoolToStr(CheckBox.Checked, True)]));
  end;
end;

end.
