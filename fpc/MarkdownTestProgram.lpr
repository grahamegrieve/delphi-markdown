program MarkdownTestProgram;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Interfaces, Forms, GuiTestRunner, MarkdownHTMLEntities, MarkdownCommonMark,
  MarkdownCommonMarkTests, MarkdownDaringFireballTests, consoletestrunner;

{$R *.res}

var
  testApp : TTestRunner;
begin
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



