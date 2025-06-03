program ANDMR_CheckBox_Test;

uses
  Vcl.Forms,
  ANDMR_CheckBox_Test_Form in 'ANDMR_CheckBox_Test_Form.pas' {FormTestCheckBox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestCheckBox, FormTestCheckBox);
  Application.Run;
end.
