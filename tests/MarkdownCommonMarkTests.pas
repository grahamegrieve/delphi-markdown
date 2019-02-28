Unit MarkdownCommonMarkTests;

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

interface

uses
  Windows, SysUtils, Classes, {$IFDEF FPC} DUnitFpc {$ELSE}DUnitX.TestFramework {$ENDIF}, Character, ShellApi, Generics.Collections,
  Json, MarkdownCommonMark;

var
  TestFileCM: String = 'C:\work\markdown\resources\commonmark\spec.json';
  TestFileGFM: String = 'C:\work\markdown\resources\commonmark\gfm_tests.json';

type
  CommonMarkDownParserTestCaseAttributeBase = class abstract (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  CommonMarkDownParserTestCaseAttribute = class (CommonMarkDownParserTestCaseAttributeBase);
  GFMParserTestCaseAttribute = class (CommonMarkDownParserTestCaseAttributeBase);

  TMarkdownCommonMarkTestBase = class abstract
  protected
    function findTest(name : String) : TJSONObject;
    function tests : TJSONArray; virtual; abstract;
    procedure Test(Name : String);
  end;

  [TextFixture]
  TMarkdownCommonMarkTest = class (TMarkdownCommonMarkTestBase)
  protected
    function tests : TJSONArray; override;
  public
    [CommonMarkDownParserTestCase]
    procedure TestCase(Name : String);
  end;

  [TextFixture]
  TMarkdownGFMTest = class (TMarkdownCommonMarkTestBase)
  protected
    function tests : TJSONArray; override;
  public
    [GFMParserTestCase]
    procedure TestCase(Name : String);
  end;

implementation

var
  gTestsBase : TJSONArray = nil;
  gTestsGFM : TJSONArray = nil;

procedure FreeTests;
begin
  gTestsBase.Free;
  gTestsGFM.Free;
end;

function leftPad(s : String; c : char; l :integer) : String;
begin
  result := s;
  while result.Length < l do
    insert(c, result, 1);
end;

{ CommonMarkDownParserTestCaseAttributeBase }

function CommonMarkDownParserTestCaseAttributeBase.GetCaseInfoArray: TestCaseInfoArray;
var
  f : TFileStream;
  b : TBytes;
  s : String;
  i, c : integer;
  t : TJSONObject;
  tests : TJSONArray;
begin
  if self is CommonMarkDownParserTestCaseAttribute then
  begin
    if gTestsBase = nil then
    begin
      f := TFileStream.Create(TestFileCM, fmOpenRead + fmShareDenyWrite);
      try
        SetLength(b, f.Size);
        f.Read(b[0], f.Size);
      finally
        f.Free;
      end;
      s := TEncoding.UTF8.GetString(b);
      gTestsBase :=  TJSONObject.ParseJSONValue(s) as TJSONArray;
    end;
    tests := gTestsBase;
  end
  else
  begin
    if gTestsGFM = nil then
    begin
      f := TFileStream.Create(TestFileGFM, fmOpenRead + fmShareDenyWrite);
      try
        SetLength(b, f.Size);
        f.Read(b[0], f.Size);
      finally
        f.Free;
      end;
      s := TEncoding.UTF8.GetString(b);
      gTestsGFM :=  TJSONObject.ParseJSONValue(s) as TJSONArray;
    end;
    tests := gTestsGFM;
  end;
  SetLength(result, tests.Count);
  c := 0;
  for i := 0 to tests.Count - 1 do
  begin
    t := tests.Items[i] as TJSONObject;
    if (t.Values['mode'] = nil) then
    begin
      result[c].Name := leftPad(t.Values['example'].ToString, '0', 4);
      SetLength(result[c].Values, 1);
      result[c].Values[0] := result[c].Name;
      inc(c);
    end;
  end;
  SetLength(result, c);
end;

{ TMarkdownCommonMarkTestBase }

function TMarkdownCommonMarkTestBase.findTest(name: String): TJSONObject;
var
  v : TJSONValue;
  o : TJSONObject;
begin
  result := nil;
  for v in tests do
  begin
    o := v as TJSONObject;
    if leftPad((o.Values['example'] as TJSONNumber).value, '0', 4) = name then
      exit(o);
  end;
end;

procedure TMarkdownCommonMarkTestBase.Test(name : String);
var
  test : TJSONObject;
  doc : TCommonMarkDocument;
  src, html, exp : String;
begin
  test := findTest(name);

  writeln(name);
  src := (test.values['markdown'] as TJsonString).value.replace('\n', #10);
  doc := TCommonMarkEngine.parse(src, self is TMarkdownGFMTest);
  try
    html := TCommonMarkEngine.render(doc);
  finally
    doc.Free;
  end;
  exp := (test.values['html'] as TJsonString).value.replace('\n', #10);
//  if (test.Values['mode'] = nil) then
    Assert.isTrue(html = exp);
end;


{ TMarkdownCommonMarkTest }

procedure TMarkdownCommonMarkTest.TestCase(Name: String);
begin
  test(name);
end;

function TMarkdownCommonMarkTest.tests: TJSONArray;
begin
  result := gTestsBase;
end;

{ TMarkdownGFMTest }

procedure TMarkdownGFMTest.TestCase(Name: String);
begin
  test(name);
end;

function TMarkdownGFMTest.tests: TJSONArray;
begin
  result := gTestsGFM;
end;

initialization
  TDUnitX.RegisterTestFixture(TMarkdownCommonMarkTest);
  TDUnitX.RegisterTestFixture(TMarkdownGFMTest);
finalization
  freeTests;
end.
