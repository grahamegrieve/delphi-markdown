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
  Windows, SysUtils, Classes, DUnitX.TestFramework, Character, ShellApi, Generics.Collections,
  Json, MarkdownCommonMark;

var
  TestFile : String = 'C:\work\markdown\resources\commonmark\spec.json';

type
  CommonMarkDownParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TMarkdownCommonMarkTest = class
  private
    function findTest(name : String) : TJSONObject;
  public
    [CommonMarkDownParserTestCase]
    procedure TestCase(Name : String);
  end;

implementation

var
  gTests : TJSONArray = nil;

procedure FreeTests;
begin
  gTests.Free;
end;

function leftPad(s : String; c : char; l :integer) : String;
begin
  result := s;
  while result.Length < l do
    insert(c, result, 1);
end;

{ CommonMarkDownParserTestCaseAttribute }

function CommonMarkDownParserTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  f : TFileStream;
  b : TBytes;
  s : String;
  i : integer;
  t : TJSONObject;
begin
  if gTests = nil then
  begin
    f := TFileStream.Create(TestFile, fmOpenRead + fmShareDenyWrite);
    try
      SetLength(b, f.Size);
      f.Read(b[0], f.Size);
    finally
      f.Free;
    end;
    s := TEncoding.UTF8.GetString(b);
    gTests :=  TJSONObject.ParseJSONValue(s) as TJSONArray;
  end;
  SetLength(result, gTests.Count);
  for i := 0 to gTests.Count - 1 do
  begin
    t := gTests.Items[i] as TJSONObject;
    result[i].Name := leftPad(t.Values['example'].ToString, '0', 4);
    SetLength(result[i].Values, 1);
    result[i].Values[0] := result[i].Name;
  end;
end;

{ TMarkdownCommonMarkTest }

function TMarkdownCommonMarkTest.findTest(name: String): TJSONObject;
var
  v : TJSONValue;
  o : TJSONObject;
begin
  result := nil;
  for v in gTests do
  begin
    o := v as TJSONObject;
    if leftPad((o.Values['example'] as TJSONNumber).value, '0', 4) = name then
      exit(o);
  end;
end;

procedure TMarkdownCommonMarkTest.TestCase(name : String);
var
  test : TJSONObject;
  doc : TMarkdownDocument;
  html, exp : String;
begin
  test := findTest(name);

  writeln(name);
  doc := TCommonMarkEngine.parse((test.values['markdown'] as TJsonString).value.replace('\n', #10));
  try
    html := TCommonMarkEngine.render(doc);
  finally
    doc.Free;
  end;
  exp := (test.values['html'] as TJsonString).value.replace('\n', #10);
  if (test.Values['mode'] = nil) then
    Assert.isTrue(html = exp);
end;


initialization
  TDUnitX.RegisterTestFixture(TMarkdownCommonMarkTest);
finalization
  freeTests;
end.
