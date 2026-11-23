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
  SysUtils, Classes, Character, DateUtils,
  {$IFDEF FPC} FPCUnit, TestRegistry {$ELSE} TestFramework {$ENDIF}, CommonTestBase,
  MarkdownProcessor, MarkdownDaringFireball;

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
  TMarkdownDaringFireballTest = class (TCommonTestSuiteCase)
  private
    function openFile(name: String): String;
    function tidy(cnt: String): String;
    procedure saveFile(name, content: String);
  public
    procedure TestCase(name : String); override;
  end;

  TMarkdownDaringFireballTests = class (TCommonTestSuite)
  private
  public
    constructor Create; override;
  end;

  TMarkdownDaringFireballTest2 = class (TCommonTestBaseCase)
  published
    procedure TestIssue10;

    // security: safe mode (the default)
    procedure TestSafeModeScript;
    procedure TestSafeModeAttributes;
    procedure TestSafeModeHtmlBlock;
    procedure TestSafeModeJavascriptLink;
    procedure TestSafeModeJavascriptAutoLink;
    procedure TestSafeModeDataImage;
    procedure TestUnsafeModePassesHtml;

    // security: stack exhaustion and processing time
    procedure TestLongDocument;
    procedure TestManyParagraphs;
    procedure TestDeepQuotes;
    procedure TestDeepLists;
    procedure TestDeepEmphasis;
    procedure TestPathologicalInlines;
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
    raise EMarkdownProcessor.Create('File "' + filename + '" not found');
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
    // these are reference outputs from Markdown.pl, which passes raw html through
    processor.AllowUnsafe := true;
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

{ TMarkdownDaringFireballTests }

constructor TMarkdownDaringFireballTests.Create;
var
  i : integer;
begin
  inherited Create;
  for i := 0 to Length(TEST_NAMEs) - 1 do
    AddTest(TMarkdownDaringFireballTest.Create(TEST_NAMEs[i]));
end;

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

function dfProcess(src : String; unsafe : boolean = false) : String;
var
  processor: TMarkdownDaringFireball;
begin
  processor := TMarkdownDaringFireball.Create;
  try
    processor.AllowUnsafe := unsafe;
    result := processor.process(src);
  finally
    processor.Free;
  end;
end;

function repeatStr(s : String; count : integer) : String;
var
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.Create;
  try
    for i := 1 to count do
      b.append(s);
    result := b.toString;
  finally
    b.Free;
  end;
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeScript;
begin
  assertEqual('<p>&lt;script>alert(1)&lt;/script></p>'+#10, dfProcess('<script>alert(1)</script>'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeAttributes;
begin
  assertEqual('<p>a &lt;img src=x onerror=alert(1)> b</p>'+#10, dfProcess('a <img src=x onerror=alert(1)> b'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeHtmlBlock;
begin
  assertEqual('<p>&lt;div>hi&lt;/div></p>'+#10, dfProcess('<div>hi</div>'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeJavascriptLink;
begin
  assertEqual('<p><a href="">x</a></p>'+#10, dfProcess('[x](javascript:alert(1))'), 'Outputs differ');
  assertEqual('<p><a href="">x</a></p>'+#10, dfProcess('[x]( JavaScript:alert(1))'), 'Outputs differ');
  assertEqual('<p><a href="">x</a></p>'+#10, dfProcess('[x](data:text/html,abc)'), 'Outputs differ');
  assertEqual('<p><a href="">x</a></p>'+#10, dfProcess('[x][r]'+#10+#10+'[r]: javascript:alert(1)'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeJavascriptAutoLink;
begin
  assertEqual('<p>&lt;javascript:alert(1)></p>'+#10, dfProcess('<javascript:alert(1)>'), 'Outputs differ');
  assertEqual('<p><a href="http://example.com/">http://example.com/</a></p>'+#10, dfProcess('<http://example.com/>'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestSafeModeDataImage;
begin
  assertEqual('<p><img src="data:image/png;base64,AAAA" alt="i" /></p>'+#10, dfProcess('![i](data:image/png;base64,AAAA)'), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestUnsafeModePassesHtml;
begin
  assertEqual('<p>a <b>b</b> c</p>'+#10, dfProcess('a <b>b</b> c', true), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestLongDocument;
var
  s : String;
begin
  // the line list used to be freed recursively - this blew the stack
  s := dfProcess(repeatStr('x'+#10, 200000));
  assertEqual('<p>x', copy(s, 1, 4), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestManyParagraphs;
var
  s : String;
begin
  s := dfProcess(repeatStr('x'+#10+#10, 100000));
  assertEqual('<p>x</p>', copy(s, 1, 8), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestDeepQuotes;
var
  s : String;
begin
  s := dfProcess(repeatStr('>', 50000)+' x');
  assertEqual('<blockquote>', copy(s, 1, 12), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestDeepLists;
var
  s : String;
begin
  s := dfProcess(repeatStr('- ', 50000)+'x');
  assertEqual('<ul>', copy(s, 1, 4), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestDeepEmphasis;
var
  s : String;
begin
  s := dfProcess(repeatStr('_*', 50000));
  assertEqual('<p>', copy(s, 1, 3), 'Outputs differ');
  s := dfProcess(repeatStr('*_[a ', 20000));
  assertEqual('<p>', copy(s, 1, 3), 'Outputs differ');
end;

procedure TMarkdownDaringFireballTest2.TestPathologicalInlines;
const
  INPUTS : array[0..8] of String = ('<', '&', '[', '[](', '[a](<', '[](x (', '`', '<div>'+#10, '<!--'+#10);
var
  i : integer;
  start : TDateTime;
  s : String;
begin
  // each of these used to take time quadratic in the length of the input (seconds to minutes)
  for i := 0 to length(INPUTS) - 1 do
  begin
    start := now;
    s := dfProcess(repeatStr(INPUTS[i], 50000));
    if MilliSecondsBetween(now, start) > 5000 then
      assertFail('Processing "'+INPUTS[i]+'" x 50000 took '+inttostr(MilliSecondsBetween(now, start))+'ms');
    // unsafe mode: unclosed html blocks and comments are still quadratic (not fixed - unsafe mode is for trusted content)
    if (INPUTS[i] <> '<div>'+#10) and (INPUTS[i] <> '<!--'+#10) then
      s := dfProcess(repeatStr(INPUTS[i], 50000), true);
    if MilliSecondsBetween(now, start) > 10000 then
      assertFail('Processing "'+INPUTS[i]+'" x 50000 (unsafe) took '+inttostr(MilliSecondsBetween(now, start))+'ms');
  end;
end;

procedure RegisterTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Markdown.DaringFireball', TMarkdownDaringFireballTests.create);
  RegisterTest('Markdown.DaringFireball2', TMarkdownDaringFireballTest2.Suite);
//  RegisterTest({'Markdown.DaringFireball2', TMarkdownDaringFireballTest2{$IFNDEF FPC}.Create{$ENDIF});
end;

end.
