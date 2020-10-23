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

{
In order to run these tests, you need to have the
following libraries:
* FastMemoryManager
* Jedi Code Library
* DUnitX (TestInsite highly recommended)

Fix up the project source paths for your location
}
program MarkDownTests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4 in 'FastMM4.pas',
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ENDIF }
  System.SysUtils,
  DUnitTestRunner,
  Graphics,
  Forms,
  GUITestRunner,
  MarkdownDaringFireball in '..\source\MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in '..\tests\MarkdownDaringFireballTests.pas',
  MarkdownCommonMark in '..\source\MarkdownCommonMark.pas',
  MarkdownCommonMarkTests in '..\tests\MarkdownCommonMarkTests.pas',
  MarkdownProcessor in '..\source\MarkdownProcessor.pas',
  FastMM4Messages in '..\..\fhirserver\dependencies\FMM\FastMM4Messages.pas',
  CommonTestBase in '..\tests\CommonTestBase.pas',
  MarkdownHTMLEntities in '..\source\MarkdownHTMLEntities.pas',
  MarkdownUnicodeUtils in '..\source\MarkdownUnicodeUtils.pas';

procedure RegisterTests;
begin
  MDTestRoot := getCurrentDir;
  MarkdownDaringFireballTests.RegisterTests;
  MarkdownCommonMarkTests.RegisterTests;
end;

var
  form : TGUITestRunner;

begin
  RegisterTests;
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit.RunRegisteredTests;
  exit;
  {$ENDIF}
  TGUITestRunner.runRegisteredTests;

//  DUnitTestRunner.RunRegisteredTests;
end.
