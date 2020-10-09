program MarkdownUITester;

uses
  Vcl.Forms,
  MarkdownUITestForm in 'MarkdownUITestForm.pas' {MarkDownUITest},
  MarkdownCommonMark in '..\source\MarkdownCommonMark.pas',
  MarkdownDaringFireball in '..\source\MarkdownDaringFireball.pas',
  MarkdownHTMLEntities in '..\source\MarkdownHTMLEntities.pas',
  MarkdownProcessor in '..\source\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in '..\source\MarkdownUnicodeUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMarkDownUITest, MarkDownUITest);
  Application.Run;
end.
