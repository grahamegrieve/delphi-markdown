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
Contribution Credits
--------------------

- Pavel Stugel - revisions for support of older Delphi versions and FPC

}


Unit MarkdownProcessor;

interface

uses
  SysUtils;

Type
  TMarkdownProcessorDialect = (mdDaringFireball, mdCommonMark, mdAsciiDoc);

  TMarkdownProcessor = {abstract} class
  protected
    function GetUnSafe: boolean; virtual; abstract;
    procedure SetUnSafe(const Value: boolean); virtual; abstract;
  public
    class function CreateDialect(dialect : TMarkdownProcessorDialect) : TMarkdownProcessor;

    // when Unsafe = true, then the processor can create scripts etc.
    property UnSafe : boolean read GetUnSafe write SetUnSafe;
    function process(source : String) : String; virtual; abstract;
  end;

implementation

uses
  MarkdownDaringFireball{$IFNDEF FPC},
  MarkdownCommonMark {$ENDIF};

{ TMarkdownProcessor }

class function TMarkdownProcessor.CreateDialect(dialect: TMarkdownProcessorDialect): TMarkdownProcessor;
begin
  case dialect of
    mdDaringFireball : result := TMarkdownDaringFireball.Create;
//    mdCommonMark : result := TMarkdownCommonMark.Create;
  else
    raise Exception.Create('Unknown Markdown dialect');
  end;
end;

end.
