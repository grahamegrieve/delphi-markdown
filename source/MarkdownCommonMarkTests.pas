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
  RegExpr, Json,
  MarkdownCommonMark;

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
    result[i].Name := t.Values['example'].ToString;
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
    if (o.Values['example'] as TJSONNumber).value = name then
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

  doc := TCommonMarkParser.parse((test.values['markdown'] as TJsonString).value.replace('\n', #10));
  try
    html := TCommonMarkRenderer.render(doc);
  finally
    doc.Free;
  end;
  exp := (test.values['html'] as TJsonString).value.replace('\n', #10);
  if (test.Values['pass'] <> nil) then
    Assert.isTrue(html = exp);
end;


initialization
  TDUnitX.RegisterTestFixture(TMarkdownCommonMarkTest);
finalization
  freeTests;
end.
