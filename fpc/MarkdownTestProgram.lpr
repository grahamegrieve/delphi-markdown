program MarkdownTestProgram;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF LINUX} cthreads, {$ENDIF}
  Interfaces, SysUtils, Forms, idetester_console,
  CommonTestBase, MarkdownDaringFireballTests, MarkdownCommonMarkTests, idetester_runtime, idetester_form;

{$R *.res}

procedure RegisterTests;
begin
  if not getCommandLineParam('mdRoot', MDTestRoot) then
    MDTestRoot := getCurrentDir;
  MarkdownDaringFireballTests.RegisterTests;
  MarkdownCommonMarkTests.RegisterTests;
end;

var
  testApp : TIdeTesterConsoleRunner;
begin
  RegisterTests;
  if IsRunningIDETests then
    RunIDETests
  else if (ParamStr(1) = '-ci') then
  begin
    testApp := TIdeTesterConsoleRunner.Create(nil);
    testApp.Initialize;
    testApp.Title := 'Markdown Tests';
    testApp.Run;
    testApp.Free;
  end
  else
  begin
    {$IFDEF MSWINDOWS} FreeConsole; {$ENDIF}
    Application.Initialize;
    Application.CreateForm(TIdeTesterForm, IdeTesterForm);
    IdeTesterForm.caption := 'Markdown Tests';
    Application.Run;
    Application.Free;
  end;
end.



