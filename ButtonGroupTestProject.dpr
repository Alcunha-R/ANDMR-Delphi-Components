program ButtonGroupTestProject;

uses
  Vcl.Forms,
  ButtonGroupTestForm in 'ButtonGroupTestForm.pas' {FormButtonGroupTest},
  ANDMR_CButtonGroup in 'Source/ANDMR_CButtonGroup.pas',
  ANDMR_CButton in 'Source/ANDMR_CButton.pas',
  ANDMR_ComponentUtils in 'Source/ANDMR_ComponentUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormButtonGroupTest, FormButtonGroupTest);
  Application.Run;
end.
