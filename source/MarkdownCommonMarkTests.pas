{
  This code was translated from TxtMark (https://github.com/rjeschke/txtmark)

  Copyright (C) 2015+ Grahame Grieve <grahameg@gmail.com> (pascal port)

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

Unit MarkdownCommonMarkTests;

interface

uses
  Windows, SysUtils, Classes, DUnitX.TestFramework, Character, ShellApi, Generics.Collections,
  RegExpr,
  MarkdownCommonMark;

var
  TestFile : String = 'C:\work\markdown\resources\commonmark\spec.txt';

type
  TCommonMarkTest = class (TObject)
  private
    Fmarkdown : String;
    Fhtml : String;
    Fexample_number : integer;
    Fstart_line : integer;
    Fend_line : integer;
    Fheadertext : String;
  public
    constructor Create(markdown, html : String; example_number, start_line, end_line : integer; headertext : String);
    class function load : TList<TCommonMarkTest>;
    property markdown : String read Fmarkdown write Fmarkdown;
    property html : String read Fhtml write Fhtml;
    property example_number : integer read Fexample_number write Fexample_number;
    property start_line : integer read Fstart_line write Fstart_line;
    property end_line : integer read Fend_line write Fend_line;
    property headertext : String read Fheadertext write Fheadertext;
  end;

  CommonMarkDownParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TMarkdownCommonMarkTests = class
  private
    Ftests : TList<TCommonMarkTest>;
    procedure saveFile(name, content: String);
  public
    [SetUp] procedure setup;
    [TearDown] procedure teardown;

    [CommonMarkDownParserTestCase] procedure TestCase(index : String);
  end;

implementation

{ TMarkdownCommonMarkTests }

procedure TMarkdownCommonMarkTests.setup;
begin
  Ftests := TCommonMarkTest.load;
end;

procedure TMarkdownCommonMarkTests.teardown;
begin
  Ftests.free;
end;

procedure TMarkdownCommonMarkTests.saveFile(name, content: String);
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter('c:\temp') + name;
  bytes := TEncoding.UTF8.GetBytes(content);
  LFileStream := TFilestream.Create(filename, fmCreate);
  try
    LFileStream.write(bytes[0], length(bytes));
  finally
     LFileStream.Free;
  end;
end;

procedure TMarkdownCommonMarkTests.TestCase(index: String);
//var
//  text, compare, , tCompare, tProcessed: String;
//  cmd, params : wideString;
var
  test : TCommonMarkTest;
  processor: TMarkdownCommonMark;
  processed: String;
begin
  test := Ftests[StrToInt(index)];
  processor := TMarkdownCommonMark.Create;
  try
    processed := processor.process(test.markdown);
//    tCompare := tidy(compare);
//    tProcessed := tidy(processed);

    if (test.html.Replace(#10, '') <> processed.Replace(#10, '')) then
    begin
      saveFile('cm-output.expected.html', test.html);
      saveFile('cm-output.actual.html', processed);
    end;
    Assert.AreEqual(test.html.Replace(#10, ''), processed.Replace(#10, ''), 'Outputs differ');
  finally
    processor.Free;
  end;
end;

{ CommonMarkDownParserTestCaseAttribute }

function CommonMarkDownParserTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  i : integer;
  tests : TList<TCommonMarkTest>;
begin
  tests := TCommonMarkTest.load;
  try
    setLength(result, tests.Count);
    for i := 0 to tests.Count - 1 do
    begin
      result[i].Name := tests[i].headerText + ' '+inttostr(tests[i].example_number);
      SetLength(result[i].Values, 1);
      result[i].Values[0] := inttostr(i);
    end;
  finally
    tests.Free;
  end;
end;

{ TCommonMarkTest }

constructor TCommonMarkTest.Create(markdown, html: String; example_number, start_line, end_line: integer; headertext: String);
begin
  inherited Create;
  Fmarkdown := markdown;
  Fhtml := html;
  Fexample_number := example_number;
  Fstart_line := start_line;
  Fend_line := end_line;
  Fheadertext := headertext;
end;

class function TCommonMarkTest.load : TList<TCommonMarkTest>;
var
  lines : TStringList;
  line, l, headertext : String;
  line_number, example_number, start_line, end_line : integer;
  state : integer; // 0 regular text, 1 markdown example, 2 html output
  markdown_lines : TStringBuilder;
  html_lines : TStringBuilder;
  regex : TRegExpr;
begin
  result := TList<TCommonMarkTest>.create;
  line_number := 0;
  state := 0;
  markdown_lines := TStringBuilder.Create;
  html_lines := TStringBuilder.Create;
  regex := TRegExpr.Create;
  lines := TStringList.Create;
  try
    lines.LoadFromFile('C:\work\markdown\resources\commonmark\spec.txt', TEncoding.UTF8);
    regex.Expression := '#+ ';
    regex.Compile;
    for line in lines do
    begin
      inc(line_number);
      l := line.trim();
      if l = '```````````````````````````````` example' then
        state := 1
      else if (state = 2) and (l = '````````````````````````````````') then
      begin
        state := 0;
        example_number := example_number + 1;
        end_line := line_number;
        result.add(TCommonMarkTest.Create(
          markdown_lines.ToString.replace('→', #9),
          html_lines.ToString.replace('→', #9),
          example_number, start_line, end_line, headertext));
        start_line := 0;
        markdown_lines.clear;
        html_lines.Clear;
      end
      else if (l = '.') then
        state := 2
      else if (state = 1) then
      begin
        if (start_line = 0) then
          start_line := line_number - 1;
        markdown_lines.append(line+#10)
      end
      else if (state = 2) then
        html_lines.append(line+#10)
      else if (state = 0) and regex.Exec(line) then
        headertext := l.Substring(3);
    end;
  finally
    lines.Free;
    regex.Free;
    markdown_lines.Free;
    html_lines.Free;
  end;
end;


initialization
//  TDUnitX.RegisterTestFixture(TMarkdownCommonMarkTests);
end.
