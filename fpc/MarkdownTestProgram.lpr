program MarkdownTestProgram;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, MarkdownHTMLEntities, MarkdownCommonMark,
  MarkdownCommonMarkTests, MarkdownDaringFireballTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

