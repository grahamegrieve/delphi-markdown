program MarkdownTestProgram;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestCase1, HTMLEntities, MarkdownCommonMark,
  MarkdownCommonMarkTests, MarkdownDaringFireballTests, DUnitFpc;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

