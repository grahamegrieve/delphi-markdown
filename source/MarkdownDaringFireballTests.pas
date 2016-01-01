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
  Windows, SysUtils, Classes, Character, ShellApi,
  MarkdownDaringFireball;

const
  TEST_NAMEs: array[0..21] of String = (
    'Amps and angle encoding', 'Auto links', 'Backslash escapes', 'Blockquotes with code blocks', 'Code Blocks',
    'Code Spans', 'Hard-wrapped paragraphs with list-like lines', 'Horizontal rules', 'Images',
    'Inline HTML (Advanced)', 'Inline HTML (Simple)', 'Inline HTML comments', 'Links, inline style',
    'Links, reference style', 'Links, shortcut references', 'Nested blockquotes', 'Ordered and unordered lists',
    'Strong and em together', 'Tabs', 'Tidyness', 'Markdown Documentation - Basics', 'Markdown Documentation - Syntax');

type
  TMarkdownDaringFireballTests = class
  private
    class function openFile(path, name: String): String;
    class function tidy(cnt: String): String;
    class procedure saveFile(path, name, content: String); static;
  public
    class procedure tests(path: String);
  end;

implementation

{ TMarkdownDaringFireballTests }

class function TMarkdownDaringFireballTests.openFile(path, name: String): String;
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter(path) + name;
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

class procedure TMarkdownDaringFireballTests.saveFile(path, name, content: String);
var
  filename: String;
  LFileStream: TFilestream;
  bytes: TBytes;
begin
  filename := IncludeTrailingPathDelimiter(path) + name;
  bytes := TEncoding.UTF8.GetBytes(content);
  LFileStream := TFilestream.Create(filename, fmCreate);
  try
    LFileStream.write(bytes[0], length(bytes));
  finally
     LFileStream.Free;
  end;
end;

class procedure TMarkdownDaringFireballTests.tests;
var
  processor: TMarkdownDaringFireball;
  name, text, compare, processed, tCompare, tProcessed: String;
  cmd, params : wideString;
begin
  processor := TMarkdownDaringFireball.Create;
  try
    for name in TEST_NAMEs do
    begin
      write('Test "' + name + '": ');
      text := openFile(path, name + '.text');
      compare := openFile(path, name + '.html');
      processed := processor.process(text);
      tCompare := tidy(compare);
      tProcessed := tidy(processed);

      if (tCompare <> tProcessed) then
      begin
        saveFile('c:\temp', 'target-test.html', compare);
        saveFile('c:\temp', 'output-test.html', processed);
        cmd := 'C:\Program Files (x86)\WinMerge\winMergeU.exe';
        params := 'c:\temp\target-test.html c:\temp\output-test.html';
        ShellExecute(0, 'open', PWideChar(cmd), PWideChar(params), nil, SW_SHOWNORMAL);
        writeln('Failed!');
        writeln('= Source ==================================================');
        writeln(text);
        writeln('= Target ==================================================');
        writeln(compare);
        writeln('= Processed ===============================================');
        writeln(processed);
        writeln('= Target (tidy) ===========================================');
        writeln(tCompare);
        writeln('= Processed (tidy) ========================================');
        writeln(tProcessed);
        writeln('===========================================================');
        raise Exception.Create('Test "' + name + '" failed');
      end
      else
        writeln('Passed');
    end;
  finally
    processor.Free;
  end;
  abort;
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
      if (TCharacter.isWhitespace(ch)) then
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

class function TMarkdownDaringFireballTests.tidy(cnt: String): String;
begin
  result := replaceHacks(collapseWhitespace(cnt));
end;

end.
