Unit MarkdownDaringFireballTests;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes, Character,
  {$IFDEF FPC} FPCUnit, TestRegistry {$ELSE} DUnitX.TestFramework {$ENDIF},
  CommonTestBase, MarkdownDaringFireball;

const
  TEST_NAMEs: array[0..26] of String = (
    'Amps and angle encoding', 'Auto links', 'Backslash escapes', 'Blockquotes with code blocks', 'Code Blocks',
    'Code Spans', 'Hard-wrapped paragraphs with list-like lines', 'Horizontal rules', 'Images',
    'Inline HTML (Advanced)', 'Inline HTML (Simple)', 'Inline HTML comments', 'Links, inline style',
    'Links, reference style', 'Links, shortcut references', 'Nested blockquotes', 'Ordered and unordered lists',
    'Strong and em together', 'Tabs', 'Tidyness', 'Markdown Documentation - Basics', 'Markdown Documentation - Syntax',
    'GitHub Issue 1', 'GitHub Issue 2', 'GitHub Issue 3', 'GitHub Issue 3a', 'GitHub Issue 4');

var
  TestFolder : String = 'resources/df';

type
  {$IFDEF FPC}
  TMarkdownDaringFireballTests = class(TTestSuite)
  private
  public
    constructor Create; override;
  end;
  {$ELSE}
  MarkDownParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  {$ENDIF}
  TMarkdownDaringFireballTest = class (TCommonTestSuiteCase)
  private
    function openFile(name: String): String;
    function tidy(cnt: String): String;
    procedure saveFile(name, content: String);
  public
    {$IFNDEF FPC}
    [MarkDownParserTestCase]
    {$ENDIF}
    procedure TestCase(name : String); override;
  end;

  {$IFNDEF FPC}
  [TextFixture]
  {$ENDIF}
  TMarkdownDaringFireballTest2 = class (TCommonTestBaseCase)
  published
    {$IFNDEF FPC}
    [TestCase]
    {$ENDIF}
    procedure TestIssue10;
  end;

procedure RegisterTests;

implementation

function GetWorkingFilename(name : String) : String;
var
  s : String;
begin
  if not getCommandLineParam('mdroot', s) then
    s := GetCurrentDir;
  result := IncludeTrailingPathDelimiter(s)+IncludeTrailingPathDelimiter(TestFolder) + name;
end;

{ TMarkdownDaringFireballTest }

function TMarkdownDaringFireballTest.openFile(name: String): String;
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter(MDTestRoot)+IncludeTrailingPathDelimiter(TestFolder) + name;
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

procedure TMarkdownDaringFireballTest.saveFile(name, content: String);
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := GetWorkingFilename(name);
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
      {$IFDEF FPC}
      if isWhitespace(ch) then
      {$ELSE}
      if (ch.isWhitespace) then
      {$ENDIF}
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

function TMarkdownDaringFireballTest.tidy(cnt: String): String;
begin
  result := replaceHacks(collapseWhitespace(cnt));
end;

procedure TMarkdownDaringFireballTest.TestCase(name: String);
var
  processor: TMarkdownDaringFireball;
  text, compare, processed, tCompare, tProcessed: String;
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
    assertEqual(tCompare, tProcessed, 'Outputs differ');
  finally
    processor.Free;
  end;
end;

{$IFDEF FPC}

{ TMarkdownDaringFireballTests }

constructor TMarkdownDaringFireballTests.Create;
var
  i : integer;
begin
  inherited Create;
  for i := 0 to Length(TEST_NAMEs)- 1 do
    AddTest(TMarkdownDaringFireballTest.Create(TEST_NAMEs[i]));
end;

{$ELSE}

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

{$ENDIF}

{ TMarkdownDaringFireballTest2 }

procedure TMarkdownDaringFireballTest2.TestIssue10;
var
  processor: TMarkdownDaringFireball;
  processed : String;
begin
  processor := TMarkdownDaringFireball.Create;
  try
    processed := processor.process('Please send comments to <email@example.com> ASAP');
    assertEqual('<p>Please send comments to <a href="mailto:email@example.com">email@example.com</a> ASAP</p>'+#10, processed, 'Outputs differ');
  finally
    processor.Free;
  end;
end;

procedure RegisterTests;
// don't use initialization - give other code time to set up directories etc
begin
{$IFNDEF FPC}
  TDUnitX.RegisterTestFixture(TMarkdownDaringFireballTest);
  TDUnitX.RegisterTestFixture(TMarkdownDaringFireballTest2);
{$ELSE}
  RegisterTest('Daring Fireball', TMarkdownDaringFireballTests.create);
  RegisterTest('Daring Fireball 2', TMarkdownDaringFireballTest2);
{$ENDIF}
end;

end.
