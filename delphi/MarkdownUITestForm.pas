unit MarkdownUITestForm;

// VCL Demo by NoSi – https://nosi.de

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMarkDownUITest = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MarkDownUITest: TMarkDownUITest;

implementation

uses
  MarkdownCommonMark, MarkdownDaringFireball, MarkdownProcessor;

{$R *.dfm}

procedure TMarkDownUITest.Button1Click(Sender: TObject);
var
  commonmark : TMarkdownProcessor;
begin
  commonmark := TMarkdownProcessor.CreateDialect(mdCommonMark);
  try
    memo2.Text := commonmark.process(memo1.text);
  finally
    commonmark.Free;
  end;
end;

procedure TMarkDownUITest.Button2Click(Sender: TObject);
var
  DaringFireball : TMArkdownProcessor;
begin
  DaringFireball := TMarkdownProcessor.CreateDialect(mdDaringFireball);
  try
    memo2.Text := DaringFireball.process(memo1.text);
  finally
    DaringFireball.Free;
  end;
end;


end.
