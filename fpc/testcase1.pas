unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, MarkdownDaringFireball;

type

  TMarkdownTests= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TMarkdownTests.TestHookUp;
begin
  Fail('Write your own test');
end;



initialization

  RegisterTest(TMarkdownTests);
end.

