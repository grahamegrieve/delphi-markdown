program MarkdownTestProgram;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Interfaces, SysUtils, Forms, GuiTestRunner, consoletestrunner,
  MarkdownHTMLEntities, MarkdownCommonMark, MarkdownDaringFireball,
  CommonTestBase, MarkdownCommonMarkTests, MarkdownDaringFireballTests;

{$R *.res}

procedure RegisterTests;
begin
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
    Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
    Application.Free;
  end;
end.



