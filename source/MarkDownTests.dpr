{
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

{
In order to run this program, you need to have the
following libraries:
* FastMemoryManager
* Jedi Code Library

Fix up the project source paths for your location
}
program MarkDownTests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4,
  FastMM4Messages,
  System.SysUtils,
  JclDebug,
  MarkdownDaringFireball in 'MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in 'MarkdownDaringFireballTests.pas',
  MarkdownProcessor in 'MarkdownProcessor.pas';

var
  testFolder : String;
begin
  JclStartExceptionTracking;
  try
    TestFolder := paramStr(1);
    if not DirectoryExists(testFolder) then
      TestFolder := 'C:\work\markdown\resources';
    TMarkdownDaringFireballTests.tests(IncludeTrailingPathDelimiter(testFolder)+'df');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
