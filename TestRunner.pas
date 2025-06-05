program TestRunner;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  ANDMR_CButtonGroup in 'Source/ANDMR_CButtonGroup.pas',
  ANDMR_CButton in 'Source/ANDMR_CButton.pas',
  ANDMR_ComponentUtils in 'Source/ANDMR_ComponentUtils.pas',
  TestANDMR_CButtonGroup in 'TestANDMR_CButtonGroup.pas';

var
  LRunner: ITestRunner;
  LLogger: ITestLogger;
  LNUnitXMLFileLogger: ITestLogger;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  try
    //Create the runner
    LRunner := TDUnitX.CreateTestRunner;
    LRunner.UseRTTI := True; // Enables attributes like [TestFixture]

    //tell the runner how we will log things
    //Log to the console window
    LLogger := TDUnitXConsoleLogger.Create(True); // True to show results at the end
    LRunner.AddLogger(LLogger);

    //Generate an NUnit compatible XML File
    LNUnitXMLFileLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    LRunner.AddLogger(LNUnitXMLFileLogger);

    LRunner.LoadRegisteredTests; // This will find [TestFixture] classes linked in

    //Run tests
    LRunner.Execute;

    {$IFNDEF TESTINSIGHT}
      if LRunner.Status = trFailed then
        System.ExitCode := 1
      else
        System.ExitCode := 0;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
