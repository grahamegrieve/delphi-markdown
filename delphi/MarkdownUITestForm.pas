unit MarkdownUITestForm;

// VCL Demo by NoSi – https://nosi.de

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SHDocVw, ActiveX, MSHTML, Vcl.OleCtrls,
  Vcl.ExtCtrls;

type
  TMarkDownUITest = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    WebBrowser1: TWebBrowser;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
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
  Doc: IHTMLDocument2;
  psa: PSafeArray;
  v: variant;
  s: String;
begin
  commonmark := TMarkdownProcessor.CreateDialect(mdCommonMark);
  try
    s := commonmark.process(memo1.text);
    memo2.Text := s;
    if WebBrowser1.Document = nil then
	    WebBrowser1.Navigate('about:blank');
    v := VarArrayCreate([0, 0], varVariant);
    v[0] := s;
    psa := PSafeArray(TVarData(v).VArray);
    Doc := WebBrowser1.Document as IHTMLDocument2;
    Doc.write(psa);
  finally
    commonmark.Free;
  end;
end;

procedure TMarkDownUITest.Button2Click(Sender: TObject);
var
  DaringFireball : TMArkdownProcessor;
  Doc: IHTMLDocument2;
  psa: PSafeArray;
  v: variant;
  s: String;
begin
  DaringFireball := TMarkdownProcessor.CreateDialect(mdDaringFireball);
  try
    s := DaringFireball.process(memo1.text);
    memo2.Text := s;
    if WebBrowser1.Document = nil then
	    WebBrowser1.Navigate('about:blank');
    v := VarArrayCreate([0, 0], varVariant);
    v[0] := s;
    psa := PSafeArray(TVarData(v).VArray);
    Doc := WebBrowser1.Document as IHTMLDocument2;
    Doc.write(psa);
  finally
    DaringFireball.Free;
  end;
end;


end.
