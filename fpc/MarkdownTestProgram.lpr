program MarkdownTestProgram;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Interfaces, SysUtils, Forms, consoletestrunner,
  CommonTestBase, MarkdownDaringFireballTests, MarkdownCommonMarkTests, idetester_form;

{$R *.res}

procedure RegisterTests;
begin
  if not getCommandLineParam('mdRoot', MDTestRoot) then
    MDTestRoot := getCurrentDir;
  MarkdownDaringFireballTests.RegisterTests;
  MarkdownCommonMarkTests.RegisterTests;
end;

var
  testApp : TTestRunner;
begin
  RegisterTests;
  if (ParamStr(1) = '-ci') then
  begin
    testApp := TTestRunner.Create(nil);
    testApp.Initialize;
    testApp.Title := 'Markdown Tests';
    testApp.Run;
    testApp.Free;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TTesterForm, TesterForm);
    TesterForm.caption := 'Markdown Tests';
    Application.Run;
    Application.Free;
  end;
end.



