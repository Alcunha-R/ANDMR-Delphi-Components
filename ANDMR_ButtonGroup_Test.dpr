program ANDMR_ButtonGroup_Test;

uses
  Vcl.Forms,
  ANDMR_ButtonGroup_Test_Form in 'ANDMR_ButtonGroup_Test_Form.pas' {FormButtonGroupTest},
  ANDMR_CButtonGroup in 'Source\ANDMR_CButtonGroup.pas',
  ANDMR_ComponentUtils in 'Source\ANDMR_ComponentUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormButtonGroupTest, FormButtonGroupTest);
  Application.Run;
end.
