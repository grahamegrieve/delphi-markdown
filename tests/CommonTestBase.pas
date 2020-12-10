unit CommonTestBase;

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
  SysUtils,
  {$IFDEF FPC} FPCUnit {$ELSE} TestFramework {$ENDIF};

var
  MDTestRoot : String;

type
  { TCommonTestCase }
  TCommonTestBaseCase = class (TTestCase)
  protected
    procedure assertEqual(left, right : String; message : String);
    procedure assertFail(message : String);
  end;

  { TCommonTestCase }
  TCommonTestSuiteCase = class (TCommonTestBaseCase)
  protected
    FName : String;
    {$IFDEF FPC}
    function GetTestName: string; override;
    {$ENDIF}
  public
    constructor Create(name : String); {$IFNDEF FPC} reintroduce;
    function GetName: string; override;
    {$ENDIF}
    procedure TestCase(name : String); virtual;
  published
    {$IFDEF FPC}
    procedure Test;
    {$ELSE}
    procedure Run;
    {$ENDIF}
  end;

  TCommonTestSuite = class (TTestSuite)
  private
  public
    constructor Create; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
  end;

function getCommandLineParam(name : String; var res : String) : boolean;

implementation

{ TCommonTestBaseCase }

procedure TCommonTestBaseCase.assertEqual(left, right: String; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(message, left, right);
  {$ELSE}
  CheckEquals(left, right, message);
  {$ENDIF}
end;

procedure TCommonTestBaseCase.assertFail(message: String);
begin
  {$IFDEF FPC}
  TAssert.Fail(message);
  {$ELSE}
  Fail(message);
  {$ENDIF}
end;

{ TCommonTestSuiteCase }

procedure TCommonTestSuiteCase.TestCase(name: String);
begin
  // nothing - override this
end;


constructor TCommonTestSuiteCase.Create(name : String);
begin
  {$IFDEF FPC}
  inherited CreateWith('Test', name);
  {$ELSE}
  inherited Create('Run');
  {$ENDIF}

  FName := name;
end;

{$IFDEF FPC}
function TCommonTestSuiteCase.GetTestName: string;
begin
  Result := FName;
end;

procedure TCommonTestSuiteCase.Test;
begin
  TestCase(FName);
end;

{$ELSE}

function TCommonTestSuiteCase.GetName: string;
begin
  result := FName;
end;

procedure TCommonTestSuiteCase.Run;
begin
  TestCase(FName);
end;

{$ENDIF}


function getCommandLineParam(name : String; var res : String) : boolean;
{$IFDEF FPC}
var
  i : integer;
begin
  result := false;
  for i := 1 to paramCount - 1 do
  begin
    if paramStr(i) = '-'+name then
    begin
      res := paramStr(i+1);
      exit(true);
    end;
  end;
{$ELSE}
begin
  result := FindCmdLineSwitch(name, res, true, [clstValueNextParam]);
{$ENDIF}
end;

{ TCommonTestSuite }

constructor TCommonTestSuite.Create;
begin
  inherited Create;
end;

end.
