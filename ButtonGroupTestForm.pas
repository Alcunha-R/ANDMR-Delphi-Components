unit ButtonGroupTestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ANDMR_CButtonGroup, ANDMR_CButton;

type
  TFormButtonGroupTest = class(TForm)
    bgHorizontal: TANDMR_CButtonGroup;
    btnH1: TANDMR_CButton;
    btnH2: TANDMR_CButton;
    btnH3: TANDMR_CButton;
    bgVertical: TANDMR_CButtonGroup;
    btnV1: TANDMR_CButton;
    btnV2: TANDMR_CButton;
    btnV3: TANDMR_CButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormButtonGroupTest: TFormButtonGroupTest;

implementation

{$R *.dfm}

procedure TFormButtonGroupTest.FormCreate(Sender: TObject);
begin
  // Code to run on form creation, if any specific setup is needed here.
  // For example, to test changing properties programmatically:
  // bgHorizontal.ButtonSpacing := 10;
  // bgVertical.ActiveButtonColor := clLime;
end;

end.
