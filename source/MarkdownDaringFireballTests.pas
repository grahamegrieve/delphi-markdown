{
  This code was translated from TxtMark (https://github.com/rjeschke/txtmark)

  Copyright (C) 2011-2015 René Jeschke <rene_jeschke@yahoo.de>
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

Unit MarkdownDaringFireballTests;

interface

uses
  Windows, SysUtils, Classes, DUnitX.TestFramework, Character, ShellApi,
  MarkdownDaringFireball;

const
  TEST_NAMEs: array[0..26] of String = (
    'Amps and angle encoding', 'Auto links', 'Backslash escapes', 'Blockquotes with code blocks', 'Code Blocks',
    'Code Spans', 'Hard-wrapped paragraphs with list-like lines', 'Horizontal rules', 'Images',
    'Inline HTML (Advanced)', 'Inline HTML (Simple)', 'Inline HTML comments', 'Links, inline style',
    'Links, reference style', 'Links, shortcut references', 'Nested blockquotes', 'Ordered and unordered lists',
    'Strong and em together', 'Tabs', 'Tidyness', 'Markdown Documentation - Basics', 'Markdown Documentation - Syntax',
    'GitHub Issue 1', 'GitHub Issue 2', 'GitHub Issue 3', 'GitHub Issue 3a', 'GitHub Issue 4');

var
  TestFolder : String = 'C:\work\markdown\resources\df';

type
  MarkDownParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TMarkdownDaringFireballTests = class
  private
    function openFile(name: String): String;
    function tidy(cnt: String): String;
    procedure saveFile(name, content: String);
  public
    [MarkDownParserTestCase]
    procedure TestCase(name : String);
  end;

implementation

{ TMarkdownDaringFireballTests }

function TMarkdownDaringFireballTests.openFile(name: String): String;
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter(TestFolder) + name;
  if FileExists(filename) then
  begin
    LFileStream := TFilestream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      SetLength(bytes, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(bytes[0], LFileStream.Size);
    finally
      LFileStream.Free;
    end;
    result := TEncoding.UTF8.GetString(bytes);
  end
  else
    raise Exception.Create('File "' + filename + '" not found');
end;

procedure TMarkdownDaringFireballTests.saveFile(name, content: String);
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter(TestFolder) + name;
  bytes := TEncoding.UTF8.GetBytes(content);
  LFileStream := TFilestream.Create(filename, fmCreate);
  try
    LFileStream.write(bytes[0], length(bytes));
  finally
     LFileStream.Free;
  end;
end;

function collapseWhitespace(str: String): String;
var
  wasWs: boolean;
  sb: TStringBuilder;
  i: integer;
  ch: char;
begin
  wasWs := false;
  sb := TStringBuilder.Create;
  try
    for i := 0 to str.length - 1 do
    begin
      ch := str[1 + i];
      if (ch.isWhitespace) then
      begin
        if (not wasWs) then
        begin
          sb.append(' ');
          wasWs := true;
        end;
      end
      else
      begin
        wasWs := false;
        sb.append(ch);
      end;
    end;
    result := sb.toString().trim();
  finally
    sb.Free;
  end;
end;

function replaceHacks(str: String): String;
begin
  str := StringReplace(str, ' />' , '/>', [rfReplaceAll]);
  result := StringReplace(str, ' <' , '<', [rfReplaceAll]);
end;

function TMarkdownDaringFireballTests.tidy(cnt: String): String;
begin
  result := replaceHacks(collapseWhitespace(cnt));
end;


{ TMarkdownDaringFireballTests }

procedure TMarkdownDaringFireballTests.TestCase(name: String);
var
  processor: TMarkdownDaringFireball;
  text, compare, processed, tCompare, tProcessed: String;
  cmd, params : wideString;
begin
  processor := TMarkdownDaringFireball.Create;
  try
    text := openFile(name + '.text');
    compare := openFile(name + '.html');
    processed := processor.process(text);
    tCompare := tidy(compare);
    tProcessed := tidy(processed);

    if (tCompare <> tProcessed) then
      saveFile(name+'.html.out', processed);
    Assert.AreEqual(tCompare, tProcessed, 'Outputs differ');
  finally
    processor.Free;
  end;
end;

{ MarkDownParserTestCaseAttribute }

function MarkDownParserTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  i : integer;
begin
  setLength(result, Length(TEST_NAMEs));
  for i := 0 to Length(TEST_NAMEs)- 1 do
  begin
    result[i].Name := TEST_NAMEs[i];
    SetLength(result[i].Values, 1);
    result[i].Values[0] := TEST_NAMEs[i];
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMarkdownDaringFireballTests);
end.
