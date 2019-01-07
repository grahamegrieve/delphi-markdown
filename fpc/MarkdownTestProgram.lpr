program MarkdownTestProgram;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestCase1, MarkdownDaringFireballTests,
  HTMLEntities, MarkdownCommonMark, DUnitFpc;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

