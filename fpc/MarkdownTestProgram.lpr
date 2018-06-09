program MarkdownTestProgram;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestCase1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

