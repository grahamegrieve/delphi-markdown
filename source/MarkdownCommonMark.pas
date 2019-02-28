Unit MarkdownCommonMark;

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
How to use this - see MarkdownProcessor; this unit is not intended to be used directly

Still to do:
- link references
- follow up on a few failing tests that I don't understand - see mode : too-hard
- test under FPC

not planned to be supported
- HTML blocks

note: tests related to link references and HTML blocks run (to check that the processing doesn't blow up), but output comparison is never checked

note about GFM:
  the GFM tests and the CommonMark tests disagree about proper
  processing of lists indented more than 3 spaces. This is probably
  just poor quality in the GFM tests, which do have a number of inconsistencies
}

interface

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}

uses
  SysUtils, Classes, Math, Generics.Collections, Character,
  {$IFDEF FPC}
  RegExpr,
  UnicodeData,
  {$ELSE}
  System.RegularExpressions,
  {$ENDIF}
  MarkdownHTMLEntities,
  MarkdownProcessor;

const
  LENGTH_INCREMENT = 116;
  EMAIL_REGEX = '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$';
  TEST_STYLING = false; // set this true to check style tracking while doing unit tests (10% speed hit)

type
  TCommonMarkStyle = (cmUnknown, cmText, cmEntity, cmControlChar, cmDelimiter, cmCode, cmURL, cmTableMarker, cmDel);

  {$IFDEF FPC}

  { TStringBuilder }
  TStringBuilder = class
  private
    FBuild : String;
    FLength : integer;
  public
    procedure clear;
    procedure append(c : char); overload;
    procedure append(s : String); overload;
    function toString : String; override;
  end;

  { TRegEx }
  TRegEx = class
  public
    class function isMatch(cnt, regex : String) : boolean;
  end;

  {$ENDIF}
  TCMWhitespaceMode = (wsLeave, wsTrim, wsStrip);
  // parser location tracking is done to support syntax highlighting. The parser won't report any errors from the markdown
  TLocation = record
    line : integer;
    col : integer;
  end;

  // Abstract Syntax Tree

  // inlines
  TCMTextNode = class
  private
    FName: String;
    FAttrs: TDictionary<String, String>;
    FContent : String;
    FBuild : String;
    FLength : integer;
    FOpener, FCloser : boolean;
    FActive: boolean;
    FPos : TLocation; // start offset in this text
//    Start : Integer;
    function GetAttrs: TDictionary<String, String>;

    function renderAttrs : String;
    procedure render(b : TStringBuilder);
    function getText : String;
    procedure SetName(const Value: String);
    procedure SetActive(const Value: boolean);
  public
    constructor Create(loc : TLocation);
    destructor Destroy; override;

    property name : String read FName write SetName; // '' means just a test node
    property attrs : TDictionary<String,String> read GetAttrs;
    property opener : boolean read FOpener write FOpener;
    property closer : boolean read FCloser write FCloser;
    property active : boolean read FActive write SetActive;

    procedure addText(ch : char); overload;
    procedure addText(s : String); overload;
    procedure removeChars(count : integer);
    function isEmpty : boolean;
  end;

  TCMTextNodes = class (TObjectList<TCMTextNode>)
  public
    function addOpener(loc : TLocation; name : String) : TCMTextNode;
    function addText(loc : TLocation; cnt : String) : TCMTextNode; // if the last is text and active, add to that
    function addTextNode(loc : TLocation; cnt : String) : TCMTextNode; // always make a new node, and make it inactive
    function addCloser(loc : TLocation; name : String) : TCMTextNode;

    // image parsing
    function plainTextSince(node : TCMTextNode) : String;
    procedure removeAfter(node : TCMTextNode);

    // emph processing
    procedure addOpenerAfter(name : String; node : TCMTextNode); // line/col = 0,0
    procedure addCloserBefore(name : String; node : TCMTextNode); // line/col = 0,0
  end;

  // blocks
  TCMBlock = class abstract (TObject)
  private
    FClosed: boolean;
    FLine : integer;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); virtual; abstract;
    function wsMode : TCMWhitespaceMode; virtual;
  public
    constructor Create(line : Integer);
    property closed : boolean read FClosed write FClosed;
    property line : Integer read FLine;
  end;

  TCMContainerBlock = class abstract (TCMBlock)
  private
    FBlocks: TObjectList<TCMBlock>;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    constructor Create(line : Integer);
    destructor Destroy; override;

    property blocks : TObjectList<TCMBlock> read FBlocks;
  end;

  TCommonMarkDocument = class (TCMContainerBlock);

  TCMParagraphBlock = class (TCMContainerBlock)
  private
    FHeader: integer;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; virtual;
    property header : integer read FHeader write FHeader;
  end;

  TCMQuoteBlock = class (TCMParagraphBlock)
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; override;
  end;

  TCMListBlock = class (TCMContainerBlock)
  private
    FOrdered: boolean;
    FStart: String;
    FMarker: String;
    FLoose: boolean;
    FLastIndent: integer;
    FBaseIndent: integer;
    FHasSeenEmptyLine : boolean; // parser state
    function grace : integer;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    property ordered : boolean read FOrdered write FOrdered;
    property baseIndent : integer read FBaseIndent write FBaseIndent;
    property lastIndent : integer read FLastIndent write FLastIndent;
    property start : String read FStart write FStart;
    property marker : String read FMarker write FMarker;
    property loose : boolean read FLoose write FLoose;
  end;

  TCMListItemBlock = class (TCMParagraphBlock)
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; override;
  end;

  TCMHeadingBlock = class (TCMContainerBlock)
  private
    FLevel: integer;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    constructor Create(line, level : Integer);
    property level : integer read FLevel write FLevel;
  end;

  TCMCodeBlock = class (TCMContainerBlock)
  private
    FFenced: boolean;
    FLang: String;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
    function wsMode : TCMWhitespaceMode; override;
  public
    property fenced : boolean read FFenced write FFenced;
    property lang : String read FLang write FLang;
  end;

  // contained blocks are cells
  TCMTableRowBlock = class (TCMContainerBlock)
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  end;

  TTextAlign = (taLeft, taCenter, taRight);

  // contained blocks are rows. The first row is the title row
  TCMTableBlock = class (TCMContainerBlock)
  private
    FColumns: TArray<TTextAlign>;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    property columns : TArray<TTextAlign> read FColumns;
  end;

  TCMLeafBlock = class abstract (TCMBlock);

  TCMThematicBreakBlock = class (TCMLeafBlock)
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  end;

  TCMTextBlock = class (TCMLeafBlock)
  private
    FText: String;
    FNodes : TCMTextNodes;
  protected
    procedure render(parent : TCMBlock; b : TStringBuilder); override;
  public
    constructor Create(line : integer; text : String);
    destructor Destroy; override;
    property text : String read FText write FText;
  end;

  // Parser Infrastructure
  TCommonMarkEngine = class;

  TCMLine = class
  private
    FLine : String;
    FIndex : integer;
    FCursor : integer; // fCursor is the corrected character cursor, not the character offset
    FMark : integer;
    FStyling : boolean;
    FStyles : Array of TCommonMarkStyle;
    FBlockOffset : integer;
    FBlockPreSpaces : integer;
    procedure getOffset(position : integer; var index : integer; var preSpaces : integer);
    procedure reset;
    procedure markBlock(stop : integer);
    procedure markStyle(stop : integer; style : TCommonMarkStyle);
    procedure updateStyle(start, length : integer; style : TCommonMarkStyle);
    procedure markRemainder(style : TCommonMarkStyle);
    function GetStyle(index: integer): TCommonMarkStyle;
    function GetStyleCount: integer;
  public
    constructor create(styling : boolean; line : string; index : integer);
    procedure mark;
    procedure rewind;
    procedure advance(len : integer); // advance x number of spaces or characters
    procedure skipWS; // advance from the current cursor until not pointing at whitespace
    function isEmpty : boolean; // is cursor at end of string
    function focus : String; // what is left after cursor
    function countWS : integer; // whitespace count from cursor
    function isWhitespace : boolean; // if everything after cursor is whitespace
    property Styles[index : integer] : TCommonMarkStyle read GetStyle;
    property StyleCount : integer read GetStyleCount;
  end;

  TCMBlockProcessingContext = (bpGeneral, bpCodeBlock, bpFencedCodeBlock);
  TCMBlockProcessor = class abstract (TObject)
  protected
    FParent : TCMBlockProcessor;

    // this procedure processes a line for nested blocks
    // return true if the block is done.
    // if false, modify the line, removing prepended characters, and remember to grab the characters as you go
    function processLine(line : TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean) : boolean; virtual; abstract;
    function isList(ordered : boolean; marker : String; indent : integer) : boolean; virtual;
    function inListOrQuote : boolean; virtual;
    function parser : TCommonMarkEngine; virtual;
  public
    constructor Create(processor : TCMBlockProcessor);
  end;

  // this anchors the chain
  TCMDocumentProcessor = class (TCMBlockProcessor)
  private
    FParser : TCommonMarkEngine;
  protected
    function processLine(line : TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean) : boolean; override;
    function parser : TCommonMarkEngine; override;
  public
    constructor Create(parser : TCommonMarkEngine);
  end;

  TCMQuoteProcessor = class (TCMBlockProcessor)
  private
    quote : TCMQuoteBlock;
  protected
    function processLine(line : TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean) : boolean; override;
    function inListOrQuote : boolean; override;
  public
    constructor Create(processor : TCMBlockProcessor; q : TCMQuoteBlock);
  end;

  TCMListProcessor = class (TCMBlockProcessor)
  private
    FList : TCMListBlock;
    FItem : TCMListItemBlock;
    FHasContent : boolean;
    FEmptyLine : integer;
  protected
    function processLine(line : TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean) : boolean; override;
    function isList(ordered : boolean; marker : String; indent : integer) : boolean; override;
    function inListOrQuote : boolean; override;
  public
    constructor Create(processor : TCMBlockProcessor; list : TCMListBlock; item : TCMListItemBlock);
    destructor Destroy; override;
  end;

  TCMTextLexer = class
  private
    FText : String;
    FCursor : integer;
    FMark, FMarkCol, FMarkLine : Integer;
    FBuilder : TStringBuilder;
    FLine : integer;
    FCol : integer;
    FStartLine : integer;
    FLines : TObjectList<TCMLine>;
    FStyling : boolean;
    function GetDone: boolean;
    function GetPeek: char;
    function GetPeekNext: char;
    function GetPeekLast: char;
    function GetPeekEndRun: char;
  public
    constructor Create(text : String; lines : TObjectList<TCMLine>; startline : integer; styling : boolean);
    destructor Destroy; override;

    procedure mark;
    procedure rewind;

    property done : boolean read GetDone;
    property peek : char read GetPeek;
    property peekNext : char read GetPeekNext;
    property peekLast : char read GetPeekLast;
    property peekEndRun : char read GetPeekEndRun;
    function peekRun(checkBefore : boolean): String;
    function peekLen(length : integer) : String;
    function peekUntil(chs : TSysCharSet) : String;
    function peekWhile(chs : TSysCharSet) : String;
    function grab(style : TCommonMarkStyle) : char; overload;
    function grabRun(style : TCommonMarkStyle) : String; overload;
    function grab(style : TCommonMarkStyle; length : integer) : String; overload;
    function has(s : string) : boolean;
    function runExistsAfter(s : String) : boolean;
    function runExistsAfterBeforeChar(s : String; c : char) : boolean;
    procedure skipWS;
    function location : TLocation;
  end;

  TCMDelimiterMode = (dmNeither, dmOpener, dmCloser, dmBoth);
  TCMDelimiter = class
  private
    Fmode: TCMDelimiterMode;
    Fdelimiter: String;
    Factive: boolean;
    FNode: TCMTextNode;
  public
    constructor Create(node : TCMTextNode; delimiter : String; mode : TCMDelimiterMode);

    function isOpener : boolean;
    function isCloser : boolean;
    property node : TCMTextNode read FNode;
    property delimiter : String read Fdelimiter write Fdelimiter;
    property active : boolean read Factive write Factive;
    property mode : TCMDelimiterMode read Fmode write Fmode;
    function isEmph : boolean;
  end;

  TCommonMarkEngine = class
  private
    FLines : TObjectList<TCMLine>;
    FCurrentLine : integer;
    FBuilder : TStringBuilder;
    FEntities : TDictionary<String, String>;
    FStack : TObjectList<TCMDelimiter>;
    FStyling : boolean;
    FGFMExtensions : boolean;

    // line operations
    procedure parseLines(src : String);
    function grabLine : TCMLine;
    function peekLine : TCMLine;
    procedure redoLine;
    function done : boolean;

    // string operations
    function allCharsSame(s : String) : boolean;
    function copyTo(s : String; chs : TSysCharSet) : String;
    function after(s : String; chs : TSysCharSet) : String;
    function copyWhile(s : String; chs : TSysCharSet) : String;
    function startsWithWS(s : String; c : char; out length : integer; wsLen : integer = 3) : boolean; overload;
    function startsWithWS(s : String; c : String; out length : integer; wsLen : integer = 3) : boolean; overload;
    function countWS(s : String) : integer;
    function lengthWSCorrected(s : String) : integer;
    function removeWS(s : String; count : integer) : String;
    function stripWhitespace(s : String) : String;
    function htmlEscape(s : String) : String; overload;
    function htmlEscape(c : char) : String; overload;
    function urlEscape(s : String; ignoreExisting : boolean = false) : String; overload;
    function urlEscape(c : char) : String; overload;
    function parseEntityString(entity : String): String;


    function isEndOfTable(line : TCMLine) : boolean;
    procedure parseTableLine(t : TCMTableBlock; line : TCMLine);

    // status
    function inPara(blocks : TObjectList<TCMBlock>; canBeQuote : boolean) : boolean;
    function inList(blocks : TObjectList<TCMBlock>; ordered : boolean; marker : String; indent : integer; grace : integer; out list : TCMListBlock) : boolean;
    function isBlock(cont : TCMBlock; blocks : TObjectList<TCMBlock>; line : String; wsLen : integer = 3) : boolean;

    // block parsing

    function parseThematicBreak(blocks : TObjectList<TCMBlock>; line : TCMLine) : boolean;
    function parseHeader(blocks : TObjectList<TCMBlock>; line : TCMLine) : boolean;
    function parseCodeBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    function parseFencedCodeBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    function parseSeTextHeader(blocks : TObjectList<TCMBlock>; line : TCMLine; isLazy : boolean; processor : TCMBlockProcessor) : boolean;
    function parseQuoteBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    function parseUListBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    function parseOListBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    function parseTableBlock(blocks : TObjectList<TCMBlock>; line : TCMLine; processor : TCMBlockProcessor) : boolean;
    procedure parse(block : TCMContainerBlock; processor : TCMBlockProcessor); overload;

    // link references
    procedure parseLinkReferences;

    // inlines
    function hasEmailAddress(lexer: TCMTextLexer; var len : integer) : boolean;
    procedure parseTextEscape(lexer : TCMTextLexer; nodes: TCMTextNodes; wsMode : TCMWhitespaceMode);
    procedure parseEntity(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode);
    function parseEntityInner(lexer : TCMTextLexer) : String;
    procedure parseBackTick(lexer : TCMTextLexer; nodes: TCMTextNodes; wsMode : TCMWhitespaceMode);
    procedure parseTilde(lexer : TCMTextLexer; nodes: TCMTextNodes; wsMode : TCMWhitespaceMode);
    procedure parseAutoLink(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode);
    procedure parseExtendedAutoLinkWeb(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode; start, linkStart : String);
    procedure parseExtendedAutoLinkEmail(lexer : TCMTextLexer; nodes : TCMTextNodes; len : integer);
    procedure parseDelimiter(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode; canRun : boolean);
    procedure parseCloseDelimiter(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode);
    function processInlineLink(lexer : TCMTextLexer; nodes : TCMTextNodes; del : TCMDelimiter) : boolean;
    procedure parseTextCore(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode);
    procedure parseText(lexer : TCMTextLexer; nodes : TCMTextNodes; wsMode : TCMWhitespaceMode);
    function processText(text : String; wsMode : TCMWhitespaceMode; startLine : integer) : TCMTextNodes;
    procedure parseInline(blocks : TObjectList<TCMBlock>; line : String);
    procedure processInlines(block : TCMBlock; wsMode : TCMWhitespaceMode);
    procedure processEmphasis(nodes : TCMTextNodes; stopDel : TCMDelimiter);

    procedure checkLines;
  public
    Constructor Create;
    Destructor Destroy; override;
    property GFMExtensions : boolean read FGFMExtensions write FGFMExtensions;

    class function process(src : String; gfm : boolean) : String;

    // divided into 2 steps in case some consumer wants to process the syntax tree
    class function parse(src : String; gfm : boolean) : TCommonMarkDocument; overload;
    class function parseStyles(src : String; gfm : boolean) : TObjectList<TCMLine>; overload;
    class function render(doc : TCommonMarkDocument) : String;
  end;

  TCommonMarkProcessor = class (TMarkdownProcessor)
  protected
    function GetUnSafe: boolean; override;
    procedure SetUnSafe(const value: boolean); override;
  public
    function process(source : String) : String; override; // want to process the syntax tree? Use the TCommonMarkEngine Directly
  end;


implementation

function null_loc : TLocation;
begin
  result.line := 0;
  result.col := 0;
end;

procedure debug(s : String);
begin
//  writeln(s);
end;

function isWhitespaceChar(ch: char): boolean;
begin
  result := CharInSet(ch, [#10, #9, ' ']);
end;

function isWhitespace(s: String): boolean;
var
  ch : char;
begin
  result := true;
  for ch in s do
    if not isWhitespaceChar(ch) then
      exit(false);
end;

function isEscapable(ch : char) : boolean;
begin
  result := CharInSet(ch, ['!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?',
        '@', '[', '\', ']', '^', '_', '`', '{', '|', '}', '~']);
end;

function isUnicodePunctuation(ch : char) : boolean;
{$IFDEF FPC}
var
  NType: byte;
{$ENDIF}
begin
  {$IFDEF FPC}
  case ch of
    '0'..'9',
    'a'..'z',
    'A'..'Z',
    '_': exit(false);
  end;

  if Ord(ch)<128 then
    Result:= true
  else if Ord(ch) >= LOW_SURROGATE_BEGIN then
    exit(true)
  else
  begin
    NType:= GetProps(Ord(ch))^.Category;
    Result := not (NType<=UGC_OtherNumber);
  end;
  {$ELSE}
  result := (ch.GetUnicodeCategory in [TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation, TUnicodeCategory.ucClosePunctuation,
    TUnicodeCategory.ucFinalPunctuation, TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation, TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucMathSymbol]);
  {$ENDIF}
end;

{$IFDEF FPC}

{ TRegEx }

class function TRegEx.isMatch(cnt, regex: String): boolean;
var
  r : TRegExpr;
begin
  r := TRegExpr.create(regex);
  try
     result := r.exec(cnt);
  finally
    r.free;
  end;
end;

{ TStringBuilder }

procedure TStringBuilder.clear;
begin
  FLength := 0;
end;

procedure TStringBuilder.append(c: char);
begin
  if FLength+1 > FBuild.Length then
    setLength(FBuild, FBuild.Length+LENGTH_INCREMENT);
  inc(FLength);
  FBuild[FLength] := c;
end;

procedure TStringBuilder.append(s: String);
var
  i : integer;
begin
  if FLength+length(s) > FBuild.Length then
    setLength(FBuild, FBuild.Length+length(s)+LENGTH_INCREMENT);
  for i := 1 to length(s) do
    FBuild[FLength+i] := s[i];
  inc(FLength, length(s));
end;

function TStringBuilder.toString: String;
begin
  result := Copy(FBuild, 1, FLength);
end;

{$ENDIF}

{ TCMTextNode }

procedure TCMTextNode.addText(s: String);
var
  ch : char;
begin
  for ch in s do
    addText(ch);
end;

constructor TCMTextNode.Create;
begin
  inherited Create;
  FActive := true;
  FPos := loc;
end;

procedure TCMTextNode.addText(ch: char);
begin
  if FLength+1 > FBuild.Length then
    setLength(FBuild, FBuild.Length+LENGTH_INCREMENT);
  inc(FLength);
  FBuild[FLength] := ch;
end;

destructor TCMTextNode.Destroy;
begin
  FAttrs.Free;
  inherited;
end;

function TCMTextNode.GetAttrs: TDictionary<String, String>;
begin
  if FAttrs = nil then
    FAttrs := TDictionary<String, String>.create;
  result := FAttrs;
end;

function TCMTextNode.getText: String;
begin
  Active := false;
  result := FContent;
end;

function TCMTextNode.isEmpty: boolean;
begin
  result := FContent = '';
end;

procedure TCMTextNode.removeChars(count : integer);
begin
  active := false;
  delete(FContent, 1, count);
end;

procedure TCMTextNode.render(b: TStringBuilder);
begin
  if FOpener then
  begin
    b.Append('<');
    b.Append(FName);
    b.Append(renderAttrs);
    if FCloser then
      b.Append(' />')
    else
      b.Append('>');
  end
  else if FCloser then
  begin
    b.Append('</');
    b.Append(FName);
    b.Append('>');
  end
  else
  begin
    active := false;
    b.Append(Copy(FContent, 1, FLength));
  end;
end;

function TCMTextNode.renderAttrs: String;
var
  s : String;
begin
  result := '';
  if FAttrs <> nil then
  begin
  // arbitrary order to pass the standard tests
    if FAttrs.TryGetValue('src', s) then
      result := result + ' src="'+s+'"';
    if FAttrs.TryGetValue('alt', s) then
      result := result + ' alt="'+s+'"';
    if FAttrs.TryGetValue('href', s) then
      result := result + ' href="'+s+'"';
    if FAttrs.TryGetValue('title', s) then
      result := result + ' title="'+s+'"';
    for s in FAttrs.Keys do
      if (s <> 'src') and (s <> 'alt') and (s <> 'href') and (s <> 'title') then
        result := result + ' '+s+'="'+FAttrs[s]+'"';
  end;
end;

procedure TCMTextNode.SetActive(const Value: boolean);
begin
  if active <> Value then
  begin
    Factive := value;
    if not active then
      FContent := Copy(FBuild, 1, FLength);
  end;
end;

procedure TCMTextNode.SetName(const Value: String);
begin
  FContent := '';
  FLength := 0;
  FName := Value;
  active := false;
end;

{ TCMTextNodes }

function TCMTextNodes.addCloser(loc : TLocation; name: String): TCMTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TCMTextNode.Create(loc);
  add(result);
  result.name := name;
  result.closer := true;
end;

procedure TCMTextNodes.addCloserBefore(name: String; node: TCMTextNode);
var
  n : TCMTextNode;
begin
  n := TCMTextNode.Create(null_loc);
  insert(IndexOf(node), n);
  n.name := name;
  n.closer := true;
end;

function TCMTextNodes.addOpener(loc : TLocation; name: String): TCMTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TCMTextNode.Create(loc);
  add(result);
  result.name := name;
  result.opener := true;
end;

procedure TCMTextNodes.addOpenerAfter(name: String; node: TCMTextNode);
var
  n : TCMTextNode;
begin
  n := TCMTextNode.Create(null_loc);
  Insert(IndexOf(node)+1, n);
  n.name := name;
  n.opener := true;
end;

function TCMTextNodes.addText(loc : TLocation; cnt: String): TCMTextNode;
begin
  if (count = 0) or (not last.active or last.opener or last.closer) then
  begin
    result := TCMTextNode.Create(loc);
    add(result);
  end
  else
    result := last;
  result.addText(cnt);
end;

function TCMTextNodes.addTextNode(loc : TLocation; cnt: String): TCMTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TCMTextNode.Create(loc);
  add(result);
  result.addText(cnt);
  result.active := false;
end;

function TCMTextNodes.plainTextSince(node: TCMTextNode): String;
var
  i : integer;
begin
  result := '';
  for i := IndexOf(node)+1 to count - 1 do
    if Items[i].name = 'img' then
      result := result + Items[i].attrs['alt']
    else
      result := result + Items[i].getText;
end;

procedure TCMTextNodes.removeAfter(node: TCMTextNode);
var
  i : integer;
  ndx : integer;
begin
  ndx := indexOf(node);
  for i := count - 1 downto ndx + 1 do
    Delete(i);
end;

{ TCMBlock }

constructor TCMBlock.Create(line: Integer);
begin
  Inherited Create;
  FLine := line;
end;

function TCMBlock.wsMode: TCMWhitespaceMode;
begin
  result := wsTrim;
end;

{ TCMContainerBlock }

constructor TCMContainerBlock.Create;
begin
  inherited create(line);
  FBlocks := TObjectList<TCMBlock>.create(true);
end;

destructor TCMContainerBlock.Destroy;
begin
  FBlocks.Free;
  inherited Destroy;
end;

procedure TCMContainerBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c : TCMBlock;
begin
  for c in FBlocks do
    c.render(self, b);
end;

{ TCMParagraphBlock }

function TCMParagraphBlock.isPlainPara: boolean;
begin
  result := FHeader = 0;
end;

procedure TCMParagraphBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c : TCMBlock;
  first : boolean;
begin
  case header of
    0: b.Append('<p>');
    1: b.Append('<h1>');
    2: b.Append('<h2>');
  end;
  first := true;
  for c in FBlocks do
  begin
    if first then
      first := false
    else
      b.Append(#10);
    c.render(self, b);
  end;
  case header of
    0: b.Append('</p>');
    1: b.Append('</h1>');
    2: b.Append('</h2>');
  end;
  b.Append(#10);
end;

{ TCMQuoteBlock }

function TCMQuoteBlock.isPlainPara: boolean;
begin
  result := false;
end;

procedure TCMQuoteBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c : TCMBlock;
begin
  b.Append('<blockquote>'#10);
  for c in FBlocks do
    c.render(self, b);
  b.Append('</blockquote>'#10);
end;

{ TCMListBlock }

function TCMListBlock.grace: integer;
begin
  if ordered then
    result := 2
  else
    result := 1;
end;

procedure TCMListBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c : TCMBlock;
begin
  if not ordered then
    b.Append('<ul>'#10)
  else if start = '1' then
    b.Append('<ol>'#10)
  else
    b.Append('<ol start="'+start+'">'#10);
  for c in FBlocks do
    c.render(self, b);
  if ordered then
    b.Append('</ol>'#10)
  else
    b.Append('</ul>'#10);
end;

{ TCMListItemBlock }

function TCMListItemBlock.isPlainPara: boolean;
begin
  result := false;
end;

procedure TCMListItemBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c, cp : TCMBlock;
  first, rFirst : boolean;
begin
  if Blocks.Count = 0 then
    b.Append('<li>')
  else
  begin
    b.Append('<li>');
    rFirst := true;
    for c in FBlocks do
      if (c is TCMParagraphBlock) and (c as TCMParagraphBlock).isPlainPara and not (parent as TCMListBlock).loose then
      begin
        first := true;
        for cp in (c as TCMParagraphBlock).Blocks do
        begin
          if first then
            first := false
          else
            b.Append(#10);
          cp.render(self, b);
        end;
      end
      else
      begin
        if rFirst then
        begin
          rFirst := false;
          b.Append(#10);
        end;
        c.render(self, b);
      end;
  end;
  b.Append('</li>'#10);
end;

{ TCMHeadingBlock }

constructor TCMHeadingBlock.Create(line, level: Integer);
begin
  Inherited Create(line);
  FLevel := level;
end;

procedure TCMHeadingBlock.render(parent : TCMBlock; b: TStringBuilder);
begin
  b.Append('<h'+inttostr(FLevel)+'>');
  inherited render(parent, b);
  b.Append('</h'+inttostr(FLevel)+'>');
  b.Append(#10);
end;

{ TCMCodeBlock }

procedure TCMCodeBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  c : TCMBlock;
begin
  if lang <> '' then
    b.Append('<pre><code class="language-'+lang+'">')
  else
    b.Append('<pre><code>');
  for c in FBlocks do
  begin
    c.render(self, b);
    b.Append(#10);
  end;
  b.Append('</code></pre>');
  b.Append(#10);
end;

function TCMCodeBlock.wsMode: TCMWhitespaceMode;
begin
  result := wsLeave;
end;

{ TCMThematicBreakBlock }

procedure TCMThematicBreakBlock.render(parent : TCMBlock; b: TStringBuilder);
begin
  b.Append('<hr />');
  b.Append(#10);
end;

{ TCMTextBlock }

constructor TCMTextBlock.Create(line : integer; text: String);
begin
  inherited Create(line);
  FText := text;
end;

destructor TCMTextBlock.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TCMTextBlock.render(parent : TCMBlock; b: TStringBuilder);
var
  n : TCMTextNode;
begin
  for n in FNodes do
    n.render(b);
end;

{ TCMLine }

procedure TCMLine.reset;
var
  i : integer;
begin
  FCursor := 1;
  if FStyling then
  begin
    SetLength(FStyles, length(FLine));
    for i := 0 to length(FLine) - 1 do
      FStyles[i] := cmUnknown;
  end;
end;

procedure TCMLine.rewind;
begin
  FCursor := FMark;
end;

procedure TCMLine.advance(len: integer);
begin
  inc(FCursor, len);
  markBlock(FCursor-1);
end;

function TCMLine.isEmpty: boolean;
var
  index, preSpaces: integer;
begin
  getOffset(FCursor, index, preSpaces);
  result := (preSpaces = 0) and (index > FLine.Length);
end;

function TCMLine.isWhitespace: boolean;
var
  index, preSpaces, i: integer;
begin
  getOffset(FCursor, index, preSpaces);
  result := true;
  for i := index to FLine.Length do
    if not isWhitespaceChar(Fline[i]) then
      exit(false);
end;

procedure TCMLine.mark;
begin
  FMark := FCursor;
end;

procedure TCMLine.markBlock(stop: integer);
var
  i : integer;
begin
  getOffset(stop+1, i, FBlockPreSpaces);
  FBlockOffset := i-1;
  markStyle(FBlockOffset, cmControlChar);
end;

procedure TCMLine.markRemainder(style: TCommonMarkStyle);
var
  i : integer;
begin
  if not FStyling then
    exit;

  i := Length(FStyles) - 1;
  while (i >=0) and (FStyles[i] = cmUnknown) do
  begin
    FStyles[i] := style;
    dec(i);
  end;
end;

procedure TCMLine.markStyle(stop: integer; style: TCommonMarkStyle);
var
  i : integer;
begin
  if not FStyling then
    exit;
  for i := 0 to Min(stop, FLine.length) - 1 do
    FStyles[i] := style;
end;

function TCMLine.countWS: integer;
var
  index, preSpaces, c, len: integer;
begin
  getOffset(FCursor, index, preSpaces);
  result := preSpaces;
  c := FCursor + preSpaces; // for tab stop tracking
  while (index <= FLine.Length) and isWhitespaceChar(FLine[index]) do
  begin
    if FLine[index] = #9 then
    begin
      len := (c - 1) mod 4;
      if len = 0 then
        len := 4;
    end
    else
      len := 1;
    inc(c, len);
    inc(result, len);
    inc(index);
  end;
end;

constructor TCMLine.create(styling : boolean; line: string; index: integer);
begin
  inherited create;
  FStyling := styling;
  FLine := line;
  FIndex := index;
  reset;
end;

function TCMLine.focus: String;
var
  index, preSpaces, i: integer;
begin
  getOffset(FCursor, index, preSpaces);
  SetLength(result, preSpaces);
  for i := 1 to result.Length do
    result[i] := ' ';
  result := result+FLine.Substring(index-1);
end;

procedure TCMLine.getOffset(position : integer; var index, preSpaces: integer);
var
  c, len : integer;
begin
  c := 1;
  index := 1;
  while (c < position) and (index <= Fline.length) do
  begin
    if FLine[index] = #9 then
      len := 4 - ((c - 1) mod 4)
    else
      len := 1;
    inc(index, 1);
    inc(c, len);
  end;
  if index > Fline.length then
    preSpaces := 0
  else
    preSpaces := c - position;
end;

function TCMLine.GetStyle(index: integer): TCommonMarkStyle;
begin
  result := FStyles[index];
end;

function TCMLine.GetStyleCount: integer;
begin
  result := length(FStyles);
end;

procedure TCMLine.skipWS;
var
  index, preSpaces, c, len: integer;
begin
  getOffset(FCursor, index, preSpaces);
  c := FCursor + preSpaces; // for tab stop tracking
  while (index <= FLine.Length) and isWhitespaceChar(FLine[index]) do
  begin
    if FLine[index] = #9 then
    begin
      len := c mod 4;
      if len = 0 then
        len := 4;
    end
    else
      len := 1;
    inc(c, len);
    inc(index);
  end;
  FCursor := c;
end;

procedure TCMLine.updateStyle(start, length: integer; style: TCommonMarkStyle);
var
  i : integer;
begin
  if not FStyling then
    exit;
  for i := start + FBlockOffset to Min(start + FBlockOffset + length, FLine.length) - 1 do
    FStyles[i] := style;
end;

{ TCMBlockProcessor }

constructor TCMBlockProcessor.Create(processor: TCMBlockProcessor);
begin
  inherited Create;
  FParent := processor;
end;

function TCMBlockProcessor.inListOrQuote: boolean;
begin
   result := false;
end;

function TCMBlockProcessor.isList(ordered: boolean; marker: String; indent: integer): boolean;
begin
  result := false;
end;

function TCMBlockProcessor.parser: TCommonMarkEngine;
begin
  result := FParent.parser;
end;

{ TCMDocumentProcessor }

constructor TCMDocumentProcessor.Create(parser : TCommonMarkEngine);
begin
  inherited Create(nil);
  FParser := parser;
end;

function TCMDocumentProcessor.parser: TCommonMarkEngine;
begin
  result := FParser;
end;

function TCMDocumentProcessor.processLine(line: TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean): boolean;
begin
  result := false; // document is only ended by end of source
  isLazy := false;
end;

{ TCMQuoteProcessor }

constructor TCMQuoteProcessor.Create(processor : TCMBlockProcessor; q: TCMQuoteBlock);
begin
  inherited Create(processor);
  quote := q;
end;

function TCMQuoteProcessor.inListOrQuote: boolean;
begin
  result := true;
end;

function TCMQuoteProcessor.processLine(line: TCMLine; root : boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean): boolean;
var
  len : integer;
begin
  result := FParent.processLine(line, false, context, isGFM, isLazy);
  if not result then
  begin
    if parser.startsWithWS(line.focus, '>', len) then
    begin
      line.advance(len);
      if not line.isEmpty and isWhitespaceChar(line.focus[1]) then
        line.advance(1);
    end
    else if root then // only can do that else we walk into descendents
    begin
      // decide whether the block is a continuation, or the end.
      // it's the end if it's empty, or it starts a new kind of block
      // plain text isn't the end of the block (lazy :-( )
      if line.isWhitespace then
        result := true
      else if parser.isBlock(quote, quote.FBlocks, line.focus) then
        result := true
      else if not parser.inPara(quote.blocks, false) then
        result := true
      else
        isLazy := true;
    end;
  end
end;

{ TCMListProcessor }

constructor TCMListProcessor.Create(processor: TCMBlockProcessor; list : TCMListBlock; item: TCMListItemBlock);
begin
  inherited Create(processor);
  Flist := list;
  Fitem := item;
  FEmptyLine := -1;
end;

destructor TCMListProcessor.Destroy;
begin
  if FList.FHasSeenEmptyLine and not FList.FLoose and (FParent is TCMListProcessor) then
    (FParent as TCMListProcessor).FList.FHasSeenEmptyLine := true;
  inherited;
end;

function TCMListProcessor.inListOrQuote: boolean;
begin
  result := true;
end;

function TCMListProcessor.isList(ordered: boolean; marker: String; indent: integer): boolean;
begin
  result := (FList.ordered = ordered) and (FList.marker = marker) and (indent < FList.lastIndent + 1);
end;

function TCMListProcessor.processLine(line: TCMLine; root: boolean; context : TCMBlockProcessingContext; isGFM : boolean; var isLazy : boolean): boolean;
var
  len : integer;
  cws, linc : integer;
begin
  result := FParent.processLine(line, false, context, isGFM, isLazy);
  if not result then
  begin
    cws := line.countWS;
    linc := parser.FCurrentLine;
    if linc = FItem.line then
      line.advance(FList.lastIndent)
    else if cws >= FList.LastIndent then
    begin
      len := cws;
      if len > FList.lastIndent+1 then
        len := FList.baseIndent;
      line.advance(len);
    end
    else if isGFM and (cws > 3) and (line.focus.Trim.StartsWith('-')) then
      result := false
    else if not line.isWhitespace and parser.isBlock(FList, FItem.blocks, line.focus, FList.lastIndent+FList.grace) then
    begin
      result := true;
    end;

    if root then
    begin
      if not line.isWhiteSpace then
      begin
        FHasContent := true;
        if root and not result and FList.FHasSeenEmptyLine then
          FList.loose := true;
      end
      else if not result  then
      begin
        isLazy := true;
        if not FHasContent then
        begin
          if FEmptyLine = -1 then
            FEmptyLine := linc
          else if FEmptyLine <> parser.FCurrentLine then
            result := true;
        end;
        if (context = bpGeneral) and (linc <> FItem.line) then
          FList.FHasSeenEmptyLine := true;
      end;
    end;
  end;
end;


{ TCMTextLexer }

constructor TCMTextLexer.Create(text: String; lines : TObjectList<TCMLine>; startline : integer; styling : boolean);
begin
  inherited Create;
  FText := text;
  FCursor := 1;
  FBuilder := TStringBuilder.create;
  FLines := lines;
  FStartLine := startline;
  FLine := 0;
  FCol := 1;
  FStyling := styling;
end;

destructor TCMTextLexer.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

procedure TCMTextLexer.rewind;
begin
  FCursor := FMark;
  FLine := FMarkLine;
  FCol := FMarkCol;
end;

function TCMTextLexer.runExistsAfter(s: String): boolean;
var
  i, len : integer;
begin
  len := s.Length;
  i := FCursor + len + 1;
  result := false;
  while i+len <= FText.Length + 1 do
  begin
    if (copy(FText, i, len) = s) and (FText[i-1] <> s[1]) and ((i+len = FText.Length+1) or (FText[i+len] <> s[1])) then
      exit(true);
    inc(i);
  end;
end;

function TCMTextLexer.runExistsAfterBeforeChar(s: String; c : char): boolean;
var
  i, len : integer;
begin
  len := s.Length;
  i := FCursor + len;
  result := false;
  while i+len <= FText.Length + 1 do
  begin
    if FText[i] = c then
      exit(false);
    if (copy(FText, i, len) = s) and (FText[i-1] <> s[1]) and ((i+len = FText.Length+1) or (FText[i+len] <> s[1])) then
      exit(true);
    inc(i);
  end;
end;

procedure TCMTextLexer.skipWS;
begin
  while isWhitespaceChar(peek) do
    grab(cmText);
end;

function TCMTextLexer.GetDone: boolean;
begin
  result := FCursor > FText.Length;
end;

function TCMTextLexer.GetPeek: char;
begin
  if done then
    result := #0
  else
    result := FText[FCursor];
end;

function TCMTextLexer.GetPeekEndRun: char;
var
  i : integer;
  c : char;
begin
  if FCursor >= FText.Length then
    result := #0
  else
  begin
    i := FCursor;
    c := FText[i];
    while (i <= FText.Length) and (FText[i] = c) do
      inc(i);
    if i > FText.Length then
      result := #0
    else
      result := FText[i];
  end;
end;

function TCMTextLexer.GetPeekLast: char;
begin
  if FCursor = 1 then
    result := #0
  else
    result := FText[FCursor-1];
end;

function TCMTextLexer.GetPeekNext: char;
begin
  if FCursor >= FText.Length then
    result := #0
  else
    result := FText[FCursor+1];
end;

function TCMTextLexer.peekRun(checkBefore : boolean): String;
var
  i : integer;
  c : char;
begin
  FBuilder.clear;
  c := peek;
  i := FCursor;
  if checkBefore and (i > 2) and (FText[i-1] = c) then
    exit('');

  while (i <= FText.Length) and (FText[i] = c) do
  begin
    FBuilder.Append(c);
    inc(i);
  end;
  result := FBuilder.ToString;
end;

function TCMTextLexer.grab(style : TCommonMarkStyle; length: integer): String;
var
  i : integer;
begin
  FBuilder.clear;
  for i := 1 to length do
    if not done then
      FBuilder.Append(grab(style));
  result := FBuilder.toString;
end;

function TCMTextLexer.grabRun(style : TCommonMarkStyle): String;
var
  c : char;
begin
  FBuilder.Clear;
  c := peek;
  while peek = c do
     FBuilder.append(grab(style));
  result := FBuilder.ToString;
end;

function TCMTextLexer.grab(style : TCommonMarkStyle): char;
var
  line : TCMLine;
begin
  if done then
    result := #0
  else
  begin
    // first task: mark the character we are grabbing with the specified style
    result := FText[FCursor];
    if result <> #10 then
    begin
      line := FLines[FStartLine + FLine];
      if FStyling and (FCol - 1 >= line.FBlockPreSpaces) then
      begin
        if FCol - 1 + line.FBlockOffset - line.FBlockPreSpaces >= length(line.FStyles) then
          writeln('problem');
        line.FStyles[FCol - 1 + line.FBlockOffset - line.FBlockPreSpaces] := style;
      end;
    end;
    inc(FCursor);
    if result = #10 then
    begin
      FCol := 1;
      inc(FLine);
    end
    else
      inc(FCol);
  end;
end;

function TCMTextLexer.has(s: string): boolean;
begin
  result := peekLen(s.Length) = s;
end;

function TCMTextLexer.location: TLocation;
begin
  result.line := FLine + FStartLine;
  result.col := FCol;
end;

procedure TCMTextLexer.mark;
begin
  FMark := FCursor;
  FMarkLine := FLine;
  FMarkCol := FCol;
end;

function TCMTextLexer.peekUntil(chs : TSysCharSet) : String;
var
  i : integer;
begin
  FBuilder.clear;
  i := FCursor;
  while (i <= FText.Length) and not CharInSet(FText[i], chs) do
  begin
    FBuilder.Append(FText[i]);
    inc(i);
  end;
  if (i > FText.Length) then
    result := ''
  else
    result := FBuilder.ToString;
end;

function TCMTextLexer.peekWhile(chs: TSysCharSet): String;
var
  i : integer;
begin
  FBuilder.clear;
  i := FCursor;
  while (i <= FText.Length) and CharInSet(FText[i], chs) do
  begin
    FBuilder.Append(FText[i]);
    inc(i);
  end;
  result := FBuilder.ToString;
end;

function TCMTextLexer.peekLen(length: integer): String;
var
  i : integer;
begin
  FBuilder.clear;
  for i := FCursor to FCursor+length-1 do
  begin
    if i <= FText.Length then
      FBuilder.Append(FText[i]);
  end;
  result := FBuilder.ToString;
end;

{ TCMDelimiter }

constructor TCMDelimiter.Create(node: TCMTextNode; delimiter : String; mode: TCMDelimiterMode);
begin
  inherited create;
  FNode := node;
  FMode := mode;
  Fdelimiter := delimiter;
  FActive := true;
end;

function TCMDelimiter.isCloser: boolean;
begin
  result := mode in [dmCloser, dmBoth];
end;

function TCMDelimiter.isEmph: boolean;
begin
  result := (delimiter <> '[') and (delimiter <> '![');
end;

function TCMDelimiter.isOpener: boolean;
begin
  result := mode in [dmOpener, dmBoth];
end;

{ TCommonMarkEngine }

class function TCommonMarkEngine.parse(src: String; gfm : boolean): TCommonMarkDocument;
var
  this : TCommonMarkEngine;
  doc : TCMDocumentProcessor;
begin
  this := TCommonMarkEngine.Create;
  try
    this.FStyling := TEST_STYLING;
    this.parseLines(src.Replace(#13#10, #10).replace(#13, #10));
    this.GFMExtensions := gfm;
    doc := TCMDocumentProcessor.Create(this);
    try
      result := TCommonMarkDocument.Create(1);
      this.parse(result, doc);
      this.parseLinkReferences;
      this.processInlines(result, wsTrim);
      this.checkLines;
    finally
      doc.Free;
    end;
  finally
    this.free;
  end;
end;

class function TCommonMarkEngine.parseStyles(src: String; gfm : boolean): TObjectList<TCMLine>;
var
  this : TCommonMarkEngine;
  doc : TCMDocumentProcessor;
  obj : TCommonMarkDocument;
begin
  this := TCommonMarkEngine.Create;
  try
    this.FStyling := true;
    this.parseLines(src.Replace(#13#10, #10).replace(#13, #10));
    this.GFMExtensions := gfm;
    doc := TCMDocumentProcessor.Create(this);
    try
      obj := TCommonMarkDocument.Create(1);
      try
        this.parse(obj, doc);
        this.parseLinkReferences;
        this.processInlines(obj, wsTrim);
        result := this.FLines;
        this.FLines := nil;
      finally
        obj.Free;
      end;
    finally
      doc.Free;
    end;
  finally
    this.free;
  end;
end;

class function TCommonMarkEngine.render(doc: TCommonMarkDocument): String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    doc.render(nil, b);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

constructor TCommonMarkEngine.Create;
begin
  inherited Create;
  FGFMExtensions := true;
  FBuilder := TStringBuilder.Create;
  FStack := TObjectList<TCMDelimiter>.Create(true);
  FLines := TObjectList<TCMLine>.create(true);
  FEntities := TDictionary<String, String>.create;
  registerEntities(FEntities);
end;

destructor TCommonMarkEngine.Destroy;
begin
  FEntities.Free;
  FStack.Free;
  FBuilder.Free;
  FLines.Free;
  inherited;
end;

function TCommonMarkEngine.grabLine: TCMLine;
begin
  inc(FCurrentLine);
  result := FLines[FCurrentLine];
  result.reset;
end;

function TCommonMarkEngine.peekLine: TCMLine;
begin
  if FCurrentLine = FLines.Count - 1 then
    result := nil
  else
    result := FLines[FCurrentLine+1];
end;

procedure TCommonMarkEngine.redoLine;
begin
  FLines[FCurrentLine].reset;
  dec(FCurrentLine);
end;

function TCommonMarkEngine.done: boolean;
begin
  result := FCurrentLine = FLines.Count - 1
end;


function TCommonMarkEngine.htmlEscape(c : char): String;
begin
  case c of
    '<' : result := '&lt;';
    '>' : result := '&gt;';
    '"' : result := '&quot;';
    '&' : result := '&amp;';
  else
    result := c;
  end;
end;

function TCommonMarkEngine.htmlEscape(s: String): String;
var
  ch : char;
begin
  FBuilder.Clear;
  for ch in s do
    FBuilder.Append(htmlEscape(ch));
  result := FBuilder.ToString;
end;

function TCommonMarkEngine.after(s: String; chs: TSysCharSet): String;
var
  i : integer;
begin
  if (s = '') then
    exit(s);

  i := 0;
  while (i < length(s)) and CharInSet(s[i+1], chs) do
    inc(i);
  result := s.Substring(i);
end;

function TCommonMarkEngine.allCharsSame(s : String) : boolean;
var
  i : integer;
begin
  result := true;
  for i := 2 to length(s) do
    if s[i] <> s[1] then
      exit(false);
end;

const
  STYLE_CODES : array [TCommonMarkStyle] of String = ('?', '.', '&', 'C', 'D', 'X', 'u', 't', '~');

procedure TCommonMarkEngine.checkLines;
var
  i : integer;
  l : TCMLine;
begin
  if not TEST_STYLING then
    exit;

//  for l in FLines do
//  begin
//    for i := 0 to length(l.FStyles) - 1 do
//      write(STYLE_CODES[l.FStyles[i]]);
//    writeln;
//  end;
  for l in FLines do
    for i := 0 to length(l.FStyles) - 1 do
      if l.FStyles[i] = cmUnknown then
        raise Exception.Create('Unknown style @ '+inttostr(l.FIndex)+', '+inttostr(i));
end;

function TCommonMarkEngine.copyTo(s : String; chs : TSysCharSet) : String;
var
  i : integer;
begin
  i := 1;
  while (i <= s.Length) and not CharInSet(s[i], chs) do
    inc(i);
  if i > s.Length then
    result := s
  else
    result := copy(s, 1, i-1);
end;

function TCommonMarkEngine.copyWhile(s : String; chs : TSysCharSet) : String;
var
  i : integer;
begin
  i := 1;
  while (i <= s.Length) and CharInSet(s[i], chs) do
    inc(i);
  if i > s.Length then
    result := s
  else
    result := copy(s, 1, i-1);
end;

// allows up to 3 spaces before c; returns the length to remove from the front as well as a boolean for whether to match
function TCommonMarkEngine.startsWithWS(s : String; c : char; out length : integer; wsLen : integer = 3) : boolean;
var
  i : integer;
begin
  if s = '' then
    exit(false);

  length := 1;
  for i := 1 to wsLen do
  begin
    if (s.Length >= length) and (s[length] = ' ') then
      inc(length);
  end;
  result := s[length] = c;
end;

function TCommonMarkEngine.startsWithWS(s : String; c : String; out length : integer; wsLen : integer = 3) : boolean;
var
  i : integer;
begin
  if s = '' then
    exit(false);

  length := 1;
  for i := 1 to wsLen do
  begin
    if (s.Length >= length) and (s[length] = ' ') then
      inc(length);
  end;
  result := copy(s, length, c.Length) = c;
end;

function TCommonMarkEngine.lengthWSCorrected(s : String) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 1 to length(s) do
    if s[i] = #9 then
      inc(result, 4)
    else
      inc(result, 1);
end;

function TCommonMarkEngine.countWS(s : String) : integer;
var
  i : integer;
begin
  result := 0;
  i := 0;
  while (i < s.Length) and CharInSet(s[i+1], [' ', #9]) do
  begin
    if s[i+1] = #9 then
      inc(result, 4)
    else
      inc(result);
    inc(i);
  end;
end;

function TCommonMarkEngine.removeWS(s: String; count: integer): String;
var
  i, j, c : integer;
begin
  i := 0;
  c := 0;
  while (i <= s.Length) and (c < count) do
  begin
    inc(i);
    if (i <= s.Length) then
      if s[i] = ' ' then
        inc(c)
      else if s[i] = #9 then
        inc(c, 4)
      else
        break;
  end;
  if c > count then
  begin
    SetLength(result, c-count);
    for j := 1 to result.length do
      result[j] := ' ';
    result := result + s.Substring(i)
  end
  else
    result := s.Substring(i);
end;

function TCommonMarkEngine.stripWhitespace(s: String): String;
var
  ch : Char;
begin
  FBuilder.Clear;
  for ch in s do
    if not isWhitespace(ch) then
      FBuilder.append(ch);
  result := FBuilder.ToString;
end;

function TCommonMarkEngine.urlEscape(c: char): String;
var
  b : TBytes;
  i : integer;
begin
  case c of
    '&' : result := '&amp;'; // not escaped in URL but is escaped in html
    '\', '[', ']', '"', '`' : result := '%'+inttoHex(ord(c), 2).ToUpper;
  else if ord(c) > 126 then
  begin
    b := TEncoding.UTF8.GetBytes(c);
    result := '';
    for i := 0 to length(b) - 1 do
      result := result + '%'+inttoHex(b[i], 2).ToUpper;
  end
  else if ord(c) > 126 then
    result := '%'+inttoHex(ord(c), 2).ToUpper
  else
    result := c;
  end;
end;

function TCommonMarkEngine.urlEscape(s: String; ignoreExisting : boolean = false): String;
var
  ch : char;
  i : integer;
begin
  FBuilder.Clear;
  for i := 1 to s.Length do
  begin
    ch := s[i];
    if ignoreExisting and (ch = '%') and (StrToIntDef('X'+copy(s, i+1, 2),  -1) <> -1) then
      FBuilder.Append(ch)
    else
      FBuilder.Append(urlEscape(ch));
  end;
  result := FBuilder.ToString;
end;


function TCommonMarkEngine.inList(blocks: TObjectList<TCMBlock>; ordered: boolean; marker : String; indent : integer; grace : integer; out list: TCMListBlock): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TCMListBlock);
  if result then
  begin
    list := blocks.Last as TCMListBlock;
    result := not list.closed and (list.ordered = ordered) and (list.marker = marker)
       and (indent <= list.LastIndent + grace);
  end;
end;

function TCommonMarkEngine.inPara(blocks: TObjectList<TCMBlock>; canBeQuote : boolean): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TCMParagraphBlock) and not (blocks.Last as TCMParagraphBlock).closed and ((blocks.Last as TCMParagraphBlock).header = 0);
  if result and not canBeQuote and not (blocks.Last as TCMParagraphBlock).isPlainPara then
    result := false;
end;

function TCommonMarkEngine.isBlock(cont : TCMBlock; blocks : TObjectList<TCMBlock>; line: String; wsLen : integer = 3): boolean;
  function inOrderedList : boolean;
  begin
    result := (cont is TCMListBlock) and (cont as TCMListBlock).ordered;
  end;
var
  len : integer;
  s, t : String;
  p : boolean;
begin
  if startsWithWS(line, '*', len, wsLen) then
    exit(true);
  if startsWithWS(line, '-', len, wsLen) then
    exit(true);
  if startsWithWS(line, '+', len, wsLen) then
    exit(true);
  if startsWithWS(line, '___', len) then
    exit(true);
  if startsWithWS(line, '#', len) then
    exit(true);
  if startsWithWS(line, '`', len) then
    exit(true);
  if startsWithWS(line, '~', len) then
    exit(true);
  if startsWithWS(line, '>', len) then
    exit(true);
  if (countWS(line) >= 4) and not InPara(blocks, false) then // code block
    exit(true);
  p := (blocks.count > 0) and (blocks.last is TCMParagraphBlock) and not (blocks.last.closed);
  // ok now look for ordered list
  len := countWS(line);
  s := removeWS(line, len);
  t := copyWhile(s, ['0'..'9']);
  if t.Length > 0 then
  begin
    if not p or (t = '1') or inOrderedList then
    begin
      s := s.Substring(t.Length);
      if (s <> '') then
      begin
        if (s = ')') or (s = '.') or s.StartsWith(') ') or s.StartsWith('. ') then
          exit(true);
      end;
    end;
  end;

  result := not p;
end;


function TCommonMarkEngine.isEndOfTable(line: TCMLine): boolean;
var
  s : String;
begin
  if line = nil then
    exit(true);
  s := line.FLine;
  result := (s.Trim = '') or s.StartsWith('>') or line.FLine.StartsWith('   ') or s.StartsWith('#') or s.StartsWith('~');
end;

function TCommonMarkEngine.parseThematicBreak(blocks : TObjectList<TCMBlock>; line: TCMLine): boolean;
var
  s : String;
begin
  if line.countWS >= 4 then
    exit(false);
  s := stripWhitespace(line.focus);
  if (s.StartsWith('***') or s.StartsWith('---') or s.StartsWith('___')) and AllCharsSame(s) then
  begin
    blocks.Add(TCMThematicBreakBlock.Create(FCurrentLine));
    line.markStyle(line.FLine.Length, cmControlChar);
    result := true;
  end
  else
    result := false;
end;

function TCommonMarkEngine.parseUListBlock(blocks: TObjectList<TCMBlock>; line: TCMLine; processor: TCMBlockProcessor): boolean;
var
  list : TCMListBlock;
  li : TCMListItemBlock;
  lp : TCMListProcessor;
  i, i2 : integer;
  s, m : String;
begin
  if line.isEmpty then
    exit(false);
  if line.countWS >= 4 then
  begin
    m := after(line.focus, [' ']);
    if (m = '') or not inList(blocks, false, m[1], line.countWS, 1, list) then
      exit(false);
  end;
  i := line.countWS;
  s := line.focus;
  if (s.length < 1+i) then
    exit(false);
  if not CharInSet(s[1+i], ['+', '-', '*']) then
    exit(false);
  m := s[1+i];
  s := s.Substring(1+i);
  if isWhitespace(s) and inPara(blocks, false) then
    exit(false);
  if isWhitespace(s) then // nothing after if it's the only thing on the line
    i2 := 1
  else
  begin
    i2 := countWS(s);
    if (i2 = 0) then
      exit(false);
    if (i2 >= 5) then
      i2 := 1;
  end;
  if not inList(blocks, false, m, i+i2+1, 1, list) then
  begin
    list := TCMListBlock.Create(FCurrentLine);
    blocks.add(list);
    list.ordered := false;
    list.BaseIndent := i+i2+1;
    list.lastIndent := list.BaseIndent;
    list.marker := m;
  end
  else
    list.Lastindent := i+i2+1;
  li := TCMListItemBlock.Create(FCurrentLine);
  list.blocks.Add(li);
  result := true;
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TCMListProcessor.Create(processor, list, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
end;

function TCommonMarkEngine.parseHeader(blocks : TObjectList<TCMBlock>; line: TCMLine): boolean;
var
  len : integer;
  ls, s : String;
  b : TCMHeadingBlock;
begin
  if line.countWS >= 4 then
    exit(false);
  ls := line.focus.Trim;
  if ls.StartsWith('# ') or ls.StartsWith('#'#9) or (ls = '#') then
    len := 1
  else if ls.StartsWith('## ') or ls.StartsWith('##'#9) or (ls = '##') then
    len := 2
  else if ls.StartsWith('### ') or ls.StartsWith('###'#9) or (ls = '###') then
    len := 3
  else if ls.StartsWith('#### ') or ls.StartsWith('####'#9) or (ls = '####') then
    len := 4
  else if ls.StartsWith('##### ') or ls.StartsWith('#####'#9) or (ls = '#####') then
    len := 5
  else if ls.StartsWith('###### ') or ls.StartsWith('######'#9) or (ls = '######') then
    len := 6
  else
    exit(False);
  result := true;
  b := TCMHeadingBlock.Create(FCurrentLine, len);
  blocks.Add(b);
  line.markBlock(len);
  line.skipWS;
  s := ls.substring(len);
  if not isWhitespace(s) then
  begin
    len := length(s);
    while (len > 0) and (s[len] = '#') do
      dec(len);
    if (len = 0) then
      s := ''
    else if (s[len] = ' ') then
      s := copy(s, 1, len-1);
  end;
  line.markRemainder(cmText);
  parseInline(b.blocks, s);
end;

procedure TCommonMarkEngine.parseInline(blocks : TObjectList<TCMBlock>; line : String);
var
  b : TCMTextBlock;
begin
  if (blocks.Count > 0) and (blocks.Last is TCMTextBlock) then
  begin
    b := blocks.Last as TCMTextBlock;
    b.FText := b.FText+#10+line;
  end
  else
    blocks.Add(TCMTextBlock.create(FCurrentLine, line));
end;

function TCommonMarkEngine.parseOListBlock(blocks: TObjectList<TCMBlock>; line: TCMLine; processor: TCMBlockProcessor): boolean;
var
  list : TCMListBlock;
  li : TCMListItemBlock;
  lp : TCMListProcessor;
  i, i2 : integer;
  s,sl, m, ls : String;
begin
  result := false;
  if line.isEmpty then
    exit(false);
  if line.countWS >= 4 then
  begin
    m := after(line.focus, [' ']);
    m := after(m, ['0'..'9']);
    if (m = '') or not inList(blocks, true, m[1], line.countWS, 2, list) then
      exit(false);
  end;
  i := line.countWS;
  line.mark;
  try
    line.skipWS;
    ls := line.focus;
    s := copyWhile(ls, ['0'..'9']);
    if (s = '') or (length(s) > 9) then
      exit(false);
    if not CharInSet(ls[s.Length+1], ['.', ')']) then
      exit(false);
    if inPara(blocks, false) and (s <> '1') then
      exit(false); // rule 267
    m := ls[s.Length+1];
    i := i+s.Length+1;
    sl := ls.Substring(s.Length+1);
    if isWhitespace(sl) and inPara(blocks, false) then
      exit(false);
    result := true;
  finally
    if not result then
      line.rewind;
  end;
  if isWhitespace(sl) then // nothing after if it's the only thing on the line
    i2 := 1
  else
  begin
    i2 := countWS(sl);
    if (i2 = 0) then
      exit(false);
    if (i2 >= 5) then
      i2 := 1;
  end;
  if not inList(blocks, true, m, i+i2, 2, list) then
  begin
    list := TCMListBlock.Create(FCurrentLine);
    blocks.add(list);
    list.ordered := true;
    list.baseIndent := i+i2;
    list.lastIndent := list.baseIndent;
    list.marker := m;
    s := after(s, ['0']);
    if s = '' then
      s := '0';
    list.start := s;
  end
  else
    list.lastIndent := i+i2;
  li := TCMListItemBlock.Create(FCurrentLine);
  list.blocks.Add(li);
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TCMListProcessor.Create(processor, list, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
end;

function TCommonMarkEngine.parseQuoteBlock(blocks: TObjectList<TCMBlock>; line: TCMLine; processor : TCMBlockProcessor): boolean;
var
  s : String;
  q : TCMQuoteBlock;
  qp : TCMQuoteProcessor;
begin
  if line.countWS >= 4 then
    exit(false);
  s := line.focus.Trim;
  if not s.StartsWith('>') then
    exit(false);
  q := TCMQuoteBlock.Create(FCurrentLine);
  blocks.add(q);
  result := true;
  redoline;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  qp := TCMQuoteProcessor.Create(processor, q);
  try
    parse(q, qp);
  finally
    qp.Free;
  end;
  q.closed := true;
end;

function TCommonMarkEngine.parseSeTextHeader(blocks : TObjectList<TCMBlock>; line: TCMLine; isLazy : boolean; processor : TCMBlockProcessor): boolean;
var
  p : TCMParagraphBlock;
  s : String;
begin
  if line.countWS >= 4 then
    exit(false);
  if blocks.Count = 0 then
    exit(false);
  if not inPara(blocks, false) then
    exit(false);
  p := blocks.Last as TCMParagraphBlock;
  if p.closed then
    exit(false);
  if p.header <> 0 then
    exit(false);
  if isLazy and processor.inListOrQuote then
    exit(false);
  s := line.focus.Trim;
  if s = '' then
    exit(false);
  if not allCharsSame(s) then
    exit(false);

  if s[1] = '-' then
  begin
    p.header := 2;
    line.markStyle(line.FLine.Length, cmControlChar);
    exit(true);
  end
  else if s[1] = '=' then
  begin
    p.header := 1;
    line.markStyle(line.FLine.Length, cmControlChar);
    exit(true);
  end
  else
    exit(false);
end;

function TCommonMarkEngine.parseCodeBlock(blocks : TObjectList<TCMBlock>; line: TCMLine; processor : TCMBlockProcessor): boolean;
var
  c : TCMCodeBlock;
  s : String;
  more : boolean;
  isLazy : boolean;
  indent : integer;
begin
  indent := line.countWS;
  if indent < 4 then
    exit(false);
  indent := 4;
  if line.isWhitespace then
    exit(false);
  if inPara(blocks, true) then
    exit(false);
  result := true;
  c := TCMCodeBlock.Create(FCurrentLine);
  blocks.Add(c);
  line.advance(indent);
  s := line.focus;
  repeat
    c.FBlocks.Add(TCMTextBlock.Create(line.FIndex, s));
    if done then
      more := false
    else
    begin
      line := peekLine;
      if (line = nil) or processor.processLine(line, true, bpCodeBlock, FGFMExtensions, isLazy) then
        break;
      s := line.focus;
      if lengthWSCorrected(s) <= indent then
        more := isWhitespace(s)
      else
        more := countWS(s) >= indent;
      if more then
      begin
        line := grabLine;
        processor.processLine(line, true, bpCodeBlock, FGFMExtensions, isLazy);
        line.advance(indent);
        s := line.focus;
      end;
    end;
  until not more;
  // remove any whitespace lines at the end:
  while (c.blocks.Count > 0) and isWhitespace((c.blocks.Last as TCMTextBlock).text) do
    c.blocks.Delete(c.blocks.Count - 1);
end;

function TCommonMarkEngine.parseFencedCodeBlock(blocks: TObjectList<TCMBlock>; line: TCMLine; processor : TCMBlockProcessor): boolean;
var
  toEnd : String;
  isLazy : boolean;
  terminated : boolean;
  function isEnded: boolean;
  var
    s : String;
  begin
    line := peekLine;
    if (line = nil) or processor.processLine(line, true, bpFencedCodeBlock, FGFMExtensions, isLazy) then
    begin
      result := true;
      terminated := true;
    end
    else if line.countWS >= 4 then
      result := false
    else
    begin
      s := line.focus.Trim;
      result := allCharsSame(s) and s.StartsWith(toEnd);
    end;
  end;
var
  c : TCMCodeBlock;
  s, s1, lang : String;
  indent, i, j : integer;
begin
  if line.countWS >= 4 then
    exit(false);
  s := line.focus.Trim;
  if not (s.StartsWith('`') or s.StartsWith('~')) then
    exit(false);
  s := copyWhile(s, [s[1]]);
  if not (s.StartsWith('```') or s.StartsWith('~~~')) or s.Substring(s.Length).Contains('~') then
    exit(false);
  toEnd := s;
  result := true;
  // now, try to find anything off the end of the fence
  s := line.focus;
  indent := 0;
  while s[indent+1] = ' ' do
    inc(indent);
  s := s.subString(indent);
  s := after(s, [toEnd[1]]).trim;
  i := 1;
  while i <= s.length do
  begin
    if s[i] = '\' then
      delete(s, i, 1) // and omit checking the next character
    else if s[i] = '`' then
      exit(false)
    else if s[i] = '&' then
    begin
      j := i+1;
      while (j <= s.Length) and (s[j] <> ';') do
        inc(j);
      if j <= length(s) then
      begin
        s1 := parseEntityString(copy(s, i, j-i+1));
        delete(s, i, j-1);
        insert(s1, s, i);
        inc(i, s1.Length-1);
      end;
    end;
    inc(i);
  end;

  if (s <> '') then
  begin
    lang := copyTo(s, [' ']);
    if lang.contains(toEnd[1]) then
      exit(false);
  end;
  line.markStyle(indent, cmText);
  line.markStyle(indent+toEnd.Length, cmControlChar);
  line.markRemainder(cmText);

  c := TCMCodeBlock.Create(FCurrentLine);
  blocks.Add(c);
  c.fenced := true;
  c.lang := lang;
  terminated := false;

  while not done and not terminated and not isEnded do
  begin
    line := grabLine;
    if (line = nil) or processor.processLine(line, true, bpFencedCodeBlock, FGFMExtensions, isLazy) then
      break;
    s := line.focus;
    if indent > 0 then
    begin
      i := 0;
      while (i < s.Length) and (i < indent) and (s[i+1] = ' ') do
        inc(i);
      if i > 0 then
        s := s.Substring(i);
      line.advance(i);
    end;
    c.FBlocks.Add(TCMTextBlock.Create(FCurrentLine, s));
  end;
  if not done and not terminated then
    grabLine.markRemainder(cmControlChar);
end;

procedure TCommonMarkEngine.parse(block: TCMContainerBlock; processor : TCMBlockProcessor);
var
  line : TCMLine;
  p : TCMParagraphBlock;
  isLazy : boolean;
begin
  while not done do
  begin
    line := grabLine;
    if processor.processLine(line, true, bpGeneral, FGFMExtensions, isLazy) then
    begin
    // ok, we're done with this set of blocks, we'll try the line again one further up
      debug('redo: "'+line.focus+'"');
      redoLine;
      exit;
    end
    else
      debug('Line: "'+line.focus+'"');
    if parseSeTextHeader(block.blocks, line, isLazy, processor) then
    else if parseThematicBreak(block.blocks, line) then
    else if parseHeader(block.blocks, line) then
    else if parseQuoteBlock(block.blocks, line, processor) then
    else if parseUListBlock(block.blocks, line, processor) then
    else if parseOListBlock(block.blocks, line, processor) then
    else if parseCodeBlock(block.blocks, line, processor) then
    else if parseFencedCodeBlock(block.blocks, line, processor) then
    else if FGFMExtensions and parseTableBlock(block.blocks, line, processor) then
    else
    begin
      if inPara(block.blocks, true) then
        p := block.Blocks.Last as TCMParagraphBlock
      else
        p := nil;
      if line.isEmpty or line.isWhitespace then
      begin
        line.markRemainder(cmText);
        if (p <> nil) then
          p.closed := true;
      end
      else
      begin
        if (p = nil) then
        begin
          p := TCMParagraphBlock.Create(FCurrentLine);
          block.blocks.Add(p);
        end;
        parseInLine(p.blocks, line.focus);
      end;
    end;
  end;
end;


procedure TCommonMarkEngine.parseLines(src: String);
var
  i, j : integer;
begin
  FLines.Clear;
  i := 1;
  while (i <= src.Length) do
  begin
    j := i;
    while (i <= src.Length) and (src[i] <> #10) do
      inc(i);
    FLines.Add(TCMLine.Create(FStyling, copy(src, j, i - j), FLines.Count));
    inc(i);
  end;
  FCurrentLine := -1;
end;

procedure TCommonMarkEngine.parseLinkReferences;
begin
  // not doing this for the moment?
end;


procedure TCommonMarkEngine.parseAutoLink(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
  function isAbsoluteUri(s : String) : boolean;
  var
    scheme, tail : String;
    i : integer;
  begin
    if not s.Contains(':') then
      exit(false);
    scheme := s.Substring(0, s.IndexOf(':'));
    if (scheme.Length < 2) or (scheme.length > 32) then
      exit(false);
    if not CharInSet(scheme[1], ['a'..'z', 'A'..'Z']) then
      exit(false);
    for i := 2 to scheme.Length do
      if not CharInSet(scheme[i], ['a'..'z', 'A'..'Z', '0'..'9', '+', '.', '-']) then
        exit(false);
    tail := s.Substring(s.IndexOf(':')+1);
    for i := 1 to tail.Length do
      if CharInSet(tail[i], [' ', #9, #10, '<']) then
        exit(false);
    result := true;
  end;
var
  s : String;
  a : TCMTextNode;
  ok : boolean;
begin
  lexer.mark;
  lexer.grab(cmControlChar);
  s := lexer.peekUntil(['>']);
  if (wsMode <> wsLeave) then
  begin
    ok := true;
    if isAbsoluteURI(s) then
    begin
      a := nodes.addOpener(lexer.location, 'a');
      a.attrs.Add('href', urlEscape(s));
      nodes.addText(lexer.location, htmlEscape(s));
      nodes.addCloser(lexer.location, 'a');
      lexer.grab(cmURL, s.Length);
      lexer.grab(cmControlChar);
    end else if TRegEx.IsMatch(s, EMAIL_REGEX) then
    begin
      a := nodes.addOpener(lexer.location, 'a');
      a.attrs.Add('href', 'mailto:'+htmlEscape(s));
      nodes.addText(lexer.location, htmlEscape(s));
      nodes.addCloser(lexer.location, 'a');
      lexer.grab(cmURL, s.Length);
      lexer.grab(cmControlChar);
    end
    else
      ok := false;
  end
  else
    ok := false;
  if not ok then
  begin
    lexer.rewind;
    nodes.addText(lexer.location, '&lt;');
    lexer.grab(cmText);
  end;
end;

procedure TCommonMarkEngine.parseDelimiter(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode; canRun : boolean);
  function isWSForFLanking(c : char) : boolean;
  begin
    result := CharInSet(c,[#0, #10]) or IsWhiteSpace(c);
  end;

  function isPuncForFLanking(c : char) : boolean;
  begin
    result := CharInSet(c, ['!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\', ']', '^', '_', '`', '{', '|', '}', '~'])
      or isUnicodePunctuation(c);
  end;

  function isLeftFlanking(cb, ca : char) : boolean;
  var
    fws, fp, pws, pp : boolean;
  begin
    fws := isWSForFLanking(ca);
    fp := isPuncForFLanking(ca);
    pws := isWSForFLanking(cb);
    pp := isPuncForFLanking(cb);
   // (a) not followed by Unicode whitespace, and (b) not followed by a punctuation character, or preceded by Unicode whitespace or a punctuation character.
    if fws then
      exit(false)
    else
      result := not (fp) or pws or pp;
  end;

  function isRightFlanking(cb, ca : char) : boolean;
  var
    fws, fp, pws, pp : boolean;
  begin
    fws := isWSForFLanking(ca);
    fp := isPuncForFLanking(ca);
    pws := isWSForFLanking(cb);
    pp := isPuncForFLanking(cb);
    if pws then
      exit(false)
    else
      result := not (pp) or fws or fp;
  end;

var
  s : String;
  cb, ca : char;
  bb, ba : boolean;
  m : TCMDelimiterMode;
  n : TCMTextNode;
  p : TLocation;
begin
  if canRun then
    s := lexer.peekRun(false)
  else if lexer.peek = '!' then
    s := lexer.peekLen(2)
  else
    s := lexer.peek;
  cb := lexer.peekLast;
  p := lexer.location;
  lexer.grab(cmUnknown, s.Length); // have to sort out the style later
  ca := lexer.peek;
  bb := isLeftFlanking(cb, ca);
  ba := isRightFLanking(cb, ca);
  if bb then
  begin
    if ba then
      m := dmBoth
    else
      m := dmOpener
  end
  else if ba then
    m := dmCloser
  else
    m := dmNeither;
  n := nodes.addTextNode(p, s);
  FStack.Add(TCMDelimiter.Create(n, s, m));
end;

procedure TCommonMarkEngine.parseTilde(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
var
  s : String;
  ws, first : boolean;
  ch : char;
begin
  s := lexer.peekRun(false);
  if FGFMExtensions and (length(s) = 2) and lexer.runExistsAfterBeforeChar(s, #10) then
  begin
    nodes.addOpener(lexer.location, 'del');
    lexer.grab(cmControlChar, s.Length);
    ws := false;
    first := true;
    while lexer.peekRun(true) <> s do
    begin
      ch := lexer.peek;
      if CharInSet(ch, [' ', #10]) then
      begin
        ws := true;
        lexer.grab(cmDel);
      end
      else
      begin
        if ws and not first then
          nodes.addText(lexer.location, ' ');
        first := false;
        ws := false;
        nodes.addText(lexer.location, htmlEscape(lexer.grab(cmDel)));
      end;
    end;
    nodes.addCloser(lexer.location, 'del');
    lexer.grab(cmControlChar, s.Length);
  end
  else
    nodes.addText(lexer.location, lexer.grab(cmText, s.Length));
end;

procedure TCommonMarkEngine.parseBackTick(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
var
  s : String;
  ws, first : boolean;
  ch : char;
begin
  s := lexer.peekRun(false);
  if lexer.runExistsAfter(s) then
  begin
    nodes.addOpener(lexer.location, 'code');
    lexer.grab(cmControlChar, s.Length);
    ws := false;
    first := true;
    while lexer.peekRun(true) <> s do
    begin
      ch := lexer.peek;
      if CharInSet(ch, [' ', #10]) then
      begin
        ws := true;
        lexer.grab(cmText);
      end
      else
      begin
        if ws and not first then
          nodes.addText(lexer.location, ' ');
        first := false;
        ws := false;
        nodes.addText(lexer.location, htmlEscape(lexer.grab(cmCode)));
      end;
    end;

    nodes.addCloser(lexer.location, 'code');
    lexer.grab(cmControlChar, s.Length);
  end
  else
    nodes.addText(lexer.location, lexer.grab(cmText, s.Length));
end;

procedure TCommonMarkEngine.parseTextEscape(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
begin
  lexer.mark;
  lexer.grab(cmControlChar);
  if isEscapable(lexer.peek) then
    nodes.addText(lexer.location, htmlEscape(lexer.grab(cmEntity)))
  else if lexer.peek = #10 then
    nodes.addText(lexer.location, '<br />')
  else
  begin
    lexer.rewind;
    nodes.addText(lexer.location, lexer.grab(cmText));
  end;
end;

function TCommonMarkEngine.processInlineLink(lexer: TCMTextLexer; nodes: TCMTextNodes; del : TCMDelimiter): boolean;
var
  lvl : integer;
  url : String;
  marker : char;
  title : String;
  b : TStringBuilder;
  d : TCMDelimiter;
  line : TCMLine;
begin
  result := false;
  b := TStringBuilder.Create;
  lexer.mark;
  try
    lexer.grab(cmDelimiter);
    if lexer.peek <> '(' then
      exit(false);
    lexer.grab(cmControlChar); // (
    lexer.skipWS;
    if lexer.peek = '<' then
    begin
      lexer.grab(cmControlChar);
      while not lexer.done and (lexer.peek <> '>') do
      begin
        if (lexer.peek = '\') and isEscapable(lexer.peekNext) then
        begin
         lexer.grab(cmControlChar);
         if lexer.done then
           exit;
         b.Append(lexer.grab(cmURL));
        end
        else if isWhitespaceChar(lexer.peek) then
        begin
          if GFMExtensions and (lexer.peek = ' ') then
          begin
            b.Append('%20');
            lexer.grab(cmURL);
          end
          else
            exit
        end
        else if lexer.peek = '&' then
          b.Append(parseEntityInner(lexer))
        else
          b.Append(lexer.grab(cmUrl));
      end;
      if lexer.done then
        exit;
      lexer.grab(cmControlChar);
    end
    else
    begin
      lvl := 0;
      while not lexer.done and not ((lexer.peek = ')') and (lvl = 0)) and not isWhitespace(lexer.peek) do
      begin
        if (lexer.peek = '\') and isEscapable(lexer.peekNext) then
        begin
         lexer.grab(cmControlChar);
         if lexer.done then
           exit;
         b.Append(lexer.grab(cmURL));
        end
        else if isWhitespaceChar(lexer.peek) then
          exit
        else if lexer.peek = '&' then
          b.Append(parseEntityInner(lexer))
        else
        begin
          if lexer.peek = '(' then
            inc(lvl)
          else if lexer.peek = ')' then
            dec(lvl);
          b.Append(lexer.grab(cmUrl));
        end;
      end;
      if lexer.done then
        exit;
    end;
    url := urlEscape(b.ToString, true);
    title := '';
    if lexer.peek <> ')' then
    begin
      b.clear;
      lexer.skipWS;
      marker := lexer.grab(cmControlChar);
      if not CharInSet(marker, ['"', '''', '(']) then
        exit;
      if marker = '(' then
        marker := ')';
      while not lexer.done and (lexer.peek <> marker) do
      begin
        if (lexer.peek = '\') then
        begin
          lexer.grab(cmControlChar);
          if lexer.done then
            exit;
          b.Append(htmlEscape(lexer.grab(cmEntity)));
        end
        else if lexer.peek = '&' then
          b.Append(htmlEscape(parseEntityInner(lexer)))
        else if lexer.peek = #10 then
          b.Append(' ')
        else if lexer.peek = '"' then
          b.Append(htmlEscape(lexer.grab(cmControlChar)))
        else
          b.Append(lexer.grab(cmText));
      end;
      if lexer.done then
        exit;
      lexer.grab(cmControlChar);
      title := b.toString;
    end;
    lexer.skipWS;
    if lexer.peek <> ')' then
      exit;
    lexer.grab(cmControlChar);
    line := FLines[del.FNode.FPos.line];
    line.updateStyle(del.FNode.FPos.col-1, length(del.FNode.FContent), cmDelimiter);

    if del.delimiter = '![' then
    begin
      del.Node.name := 'img';
      del.Node.attrs.Add('src', url);
      del.node.opener := true;
      del.node.closer := true;
      if title <> '' then
        del.node.attrs.Add('title', title);
      del.Node.attrs.Add('alt', nodes.plainTextSince(del.node));
      nodes.removeAfter(del.node);
    end
    else
    begin
      del.Node.name := 'a'; // clears the text
      del.Node.opener := true;
      del.Node.attrs.Add('href', url);
      if title <> '' then
        del.node.attrs.Add('title', title);
      nodes.addCloser(lexer.location, 'a');
      for d in FStack do
        if d = del then
          break
        else if d.delimiter = '[' then
          d.active := false;
    end;
    processEmphasis(nodes, del);
    FStack.remove(del);
    result := true;
  finally
    if not result then
      lexer.rewind;
    b.free;
  end;
end;

procedure TCommonMarkEngine.parseCloseDelimiter(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
var
  i : integer;
  d : TCMDelimiter;
  line : TCMLine;
begin
  d := nil;
  for i := FStack.Count - 1 downto 0 do
    if (FStack[i].delimiter = '[') or (FStack[i].delimiter = '![') then
    begin
      d := FStack[i];
      break;
    end;
  if d = nil then
  begin
    nodes.addText(lexer.location, lexer.grab(cmText));
    exit;
  end;
  if not d.active then
  begin
    line := FLines[d.FNode.FPos.line];
    line.updateStyle(d.FNode.FPos.col-1, length(d.FNode.FContent), cmText);
    FStack.Remove(d);
    nodes.addText(lexer.location, lexer.grab(cmText));
    exit;
  end;
  // now, look ahead, do we have
  //  - inline link/image  [text](link title)
  //  - reference link/image [text][ref]
  //  - compact reference link/image [label][]
  //  - shortcut reference link/image [label]
  if not processInlineLink(lexer, nodes, d) then
  begin
    line := FLines[d.FNode.FPos.line];
    line.updateStyle(d.FNode.FPos.col-1, length(d.FNode.FContent), cmText);
    FStack.Remove(d);
    nodes.addText(lexer.location, lexer.grab(cmText));
    exit;
  end;
end;

function TCommonMarkEngine.parseEntityString(entity : String): String;
var
  s, c : String;
  ch : char;
  i : integer;
begin
  s := copy(entity, 2, length(entity)-2);
  if FEntities.TryGetValue(entity, c) then
  begin
    exit(c);
  end
  else if (s <> '') and s.StartsWith('#') and (s.Length <= 9) and (StrToIntDef(s.Substring(1), -1) <> -1) then
  begin
    i := StrToInt(s.Substring(1));
    if i > 65535 then
      ch := #$FFFD
    else
    begin
      ch := char(i);
      if (i = 0) {$IFNDEF FPC}or (ch.GetUnicodeCategory = TUnicodeCategory.ucUnassigned) {$ENDIF} then
        ch := #$FFFD;
    end;
    exit(ch);
  end
  else
    exit('');
end;

function checkForEntity(s : String) : integer;
var
  c : char;
begin
  if s.Contains('&') then
  begin
    s := s.Substring(s.lastIndexOf('&')+1);
    s := s.subString(0, s.Length-1);
    for c in s do
      if not CharInSet(c, ['a'..'z', 'A'..'Z', '0'..'9']) then
        exit(0);
    exit(s.Length+2);
  end
  else
    result := 0;
end;

procedure TCommonMarkEngine.parseExtendedAutoLinkEmail(lexer: TCMTextLexer; nodes: TCMTextNodes; len : integer);
var
  s : String;
  a : TCMTextNode;
begin
  s := lexer.grab(cmUrl, len);

  a := nodes.addOpener(lexer.location, 'a');
  a.attrs.Add('href', 'mailto:'+htmlEscape(s));
  nodes.addText(lexer.location, htmlEscape(s));
  nodes.addCloser(lexer.location, 'a');
end;

procedure TCommonMarkEngine.parseExtendedAutoLinkWeb(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode; start, linkStart : String);
var
  ok : boolean;
  tail : Integer;
  s : string;
  a : TCMTextNode;
begin
  lexer.mark;
  ok := false;
  try
    lexer.grab(cmURL, start.Length);
    s := '';
    while CharInSet(lexer.peek, ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) do
      s := s + lexer.grab(cmUrl);
    if not s.Contains('.') then
      exit;
    ok := true;
    while not lexer.done and not CharInSet(lexer.peek, [' ', #10, '<']) do
      s := s + lexer.grab(cmUrl);
    // is that last char part of the link?
    case s[s.Length] of
      '?', '!', '.', ',', ':', '*', '_', '~': tail := 1;
      ')': if s.CountChar('(') < s.CountChar(')') then tail := 1 else tail := 0;
      ';': tail := checkForEntity(s);
    else
      tail := 0;
    end;
    if tail = 0 then
    begin
      a := nodes.addOpener(lexer.location, 'a');
      a.attrs.Add('href', linkStart+start+htmlEscape(s));
      nodes.addText(lexer.location, start+htmlEscape(s));
      nodes.addCloser(lexer.location, 'a');
    end
    else
    begin
      a := nodes.addOpener(lexer.location, 'a');
      a.attrs.Add('href', linkStart+start+htmlEscape(s.Substring(0, s.Length-tail)));
      nodes.addText(lexer.location, start+htmlEscape(s.Substring(0, s.Length-tail)));
      nodes.addCloser(lexer.location, 'a');
      nodes.addText(lexer.location, htmlEscape(s.Substring(s.length-tail)));
    end;
  finally
    if not ok then
    begin
      lexer.rewind;
      nodes.addText(lexer.location, lexer.grab(cmText));
    end;
  end;
end;

procedure TCommonMarkEngine.parseEntity(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
begin
  if wsMode = wsLeave then // code block
  begin
    nodes.addText(lexer.location, '&amp;');
    lexer.grab(cmEntity);
  end
  else
    nodes.addText(lexer.location, htmlEscape(parseEntityInner(lexer)));
end;

function TCommonMarkEngine.parseEntityInner(lexer: TCMTextLexer): String;
var
  s, c : String;
  ch : char;
  i : integer;
begin
  lexer.grab(cmEntity);
  s := lexer.peekUntil([';']);
  if FEntities.TryGetValue('&'+s+';', c) then
  begin
    lexer.grab(cmEntity, s.Length+1);
    exit(c);
  end
  else if (s <> '') and s.StartsWith('#') and (s.Length <= 9) and (StrToIntDef(s.Substring(1), -1) <> -1) then
  begin
    i := StrToInt(s.Substring(1));
    if i > 65535 then
      ch := #$FFFD
    else
    begin
      ch := char(i);
      if (i = 0) {$IFNDEF FPC} or (ch.GetUnicodeCategory = TUnicodeCategory.ucUnassigned) {$ENDIF} then
        ch := #$FFFD;
    end;
    lexer.grab(cmEntity, s.Length+1);
    exit(ch);
  end
  else
    exit('&');
end;

function TCommonMarkEngine.hasEmailAddress(lexer: TCMTextLexer; var len : integer) : boolean;
var
  s : String;
  state : integer;
  c : char;
begin
  s := lexer.peekWhile(['a'..'z', 'A'..'Z', '0'..'9', '+', '-', '_', '@', '.']);
  if s.CountChar('@') <> 1 then
    exit(false);
  if CharInSet(s[s.Length], ['_', '-']) then
    exit(false);
  if s[s.Length] = '.' then
    s := s.Substring(0, s.Length-1);

  len := s.Length;
  state := 0;
  for c in s do
    if state = 0 then
    begin
      if c = '@' then exit(false) else state := 1;
    end
    else if state = 1 then
    begin
      if c = '@' then state := 2
    end
    else
    begin
      if c = '.' then
        state := 3
      else if c = '+' then
        exit(false);
    end;
  exit(state = 3);
end;

procedure TCommonMarkEngine.parseTextCore(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode: TCMWhitespaceMode);
var
  len : integer;
begin
  if wsMode = wsLeave then // code block
    nodes.addText(lexer.location, htmlEscape(lexer.grab(cmCode)))
  else
  case lexer.peek of
    '\' : parseTextEscape(lexer, nodes, wsMode);
    '<' : parseAutoLink(lexer, nodes, wsMode);
    '>', '"' : nodes.addText(lexer.location, htmlEscape(lexer.grab(cmText)));
    '&' : parseEntity(lexer, nodes, wsMode);
    '`' : parseBackTick(lexer, nodes, wsMode);
    '~' : parseTilde(lexer, nodes, wsMode);
    '*' : parseDelimiter(lexer, nodes, wsMode, true);
    '_' : if isWhitespaceChar(lexer.peekLast) or isEscapable(lexer.peekLast) or (lexer.peekLast = #0) or
             isWhitespaceChar(lexer.peekEndRun) or isEscapable(lexer.peekEndRun) or (lexer.peekEndRun = #0) then
            parseDelimiter(lexer, nodes, wsMode, true)
          else
            nodes.addText(lexer.location, htmlEscape(lexer.grabRun(cmText)));
    '[' : parseDelimiter(lexer, nodes, wsMode, false);
    '!' : if lexer.peekNext = '[' then
            parseDelimiter(lexer, nodes, wsMode, false)
          else
            nodes.addText(lexer.location, lexer.grab(cmText));
    ']' : parseCloseDelimiter(lexer, nodes, wsMode);
  else
    if GFMExtensions and lexer.has('www.') then
      parseExtendedAutoLinkWeb(lexer, nodes, wsMode, 'www.', 'http://')
    else if GFMExtensions and lexer.has('http://') then
      parseExtendedAutoLinkWeb(lexer, nodes, wsMode, 'http://', '')
    else if GFMExtensions and lexer.has('https://') then
      parseExtendedAutoLinkWeb(lexer, nodes, wsMode, 'https://', '')
    else if GFMExtensions and lexer.has('ftp://') then
      parseExtendedAutoLinkWeb(lexer, nodes, wsMode, 'ftp://', '')
    else if GFMExtensions and hasEmailAddress(lexer, len) then
      parseExtendedAutoLinkEmail(lexer, nodes, len)
    else
      nodes.addText(lexer.location, lexer.grab(cmText));
  end;
end;

function TCommonMarkEngine.parseTableBlock(blocks: TObjectList<TCMBlock>; line: TCMLine; processor: TCMBlockProcessor): boolean;
  function cellCount(line : TCMLine) : Integer;
  var
    c : char;
    esc : boolean;
  begin
    result := 0;
    esc := false;
    for c in line.FLine do
    begin
      if not esc and (c = '|') then
        inc(result)
      else if esc then
        esc := false
      else if c = '\' then
        esc := true;
    end;

    if result > 0 then
    begin
      if not line.FLine.Trim.StartsWith('|') then
        inc(result);
      if not line.FLine.Trim.EndsWith('|') or line.FLine.Trim.EndsWith('\|') then
        inc(result);
    end;
  end;
var
  c1, c2, i : integer;
  s : String;
  nl : TCMLine;
  c : char;
  t : TCMTableBlock;
  a : TArray<String>;
begin
  result := false;
  if line.isEmpty then
    exit(false);
  c1 := cellCount(line);
  if c1 = 0 then
    exit(false);
  nl := peekLine;
  if (nl = nil) then
    exit(false);
  for c in nl.FLine.Trim do
    if not CharInSet(c, [' ', '|', '-', ':']) then
      exit(false);
  c2 := cellCount(nl);
  if c1 <> c2 then
    exit(false);
  // ok, we have a table.....
  result := true;
  t := TCMTableBlock.Create(FCurrentLine);
  blocks.Add(t);

  parseTableLine(t, line);
  grabLine; // nl
  nl.markRemainder(cmTableMarker);
  s := nl.FLine.Trim;
  if s.StartsWith('|') then
    s := s.Substring(1);
  if s.EndsWith('|') then
    s := s.Substring(0, s.Length-1);
  a := s.Split(['|']);
  SetLength(t.FColumns, length(a));
  for i := 0 to length(a) - 1 do
  begin
    s := a[i].Trim;
    if s.StartsWith(':') and s.EndsWith(':') then
      t.FColumns[i] := taCenter
    else if s.EndsWith(':') then
      t.FColumns[i] := taRight
    else
      t.FColumns[i] := taLeft;
  end;
  while not isEndOfTable(peekLine) do
    parseTableLine(t, grabLine);
end;

procedure TCommonMarkEngine.parseTableLine(t: TCMTableBlock; line: TCMLine);
var
  l, i : integer;
  esc : boolean;
  r : TCMTableRowBlock;
  procedure addCell(b, e : integer);
  var
    s : String;
  begin
    s := copy(line.FLine, b, e-b);
    s := s.Trim.replace('\|', '|');
    r.blocks.Add(TCMTextBlock.Create(line.FIndex, s));
  end;
begin
  r := TCMTableRowBlock.Create(line.FIndex);
  t.blocks.Add(r);
  line.markRemainder(cmText);
  i := 1;
  l := 1;
  esc := false;
  while i <= line.FLine.Length do
  begin
    if (i = 1) and (line.fLine[i] = '|') then
    begin
      line.updateStyle(l, i+1, cmTableMarker);
      l := i+1;
    end
    else if esc then
      esc := false
    else if line.FLine[i] = '\' then
      esc := true
    else if line.FLine[i] = '|' then
    begin
      addCell(l, i);
      line.updateStyle(l, i+1, cmTableMarker);
      l := i+1;
    end;
    inc(i);
  end;
  if (l < i) then
    addCell(l, i);
end;

procedure TCommonMarkEngine.parseText(lexer: TCMTextLexer; nodes: TCMTextNodes; wsMode : TCMWhitespaceMode);
var
  seenNonWhitespace : boolean;
  whiteSpace : string;
  c : String;
begin
  whiteSpace := '';
  seenNonWhitespace := false;
  while not lexer.done do
  begin
    if (wsMode <> wsLeave) and isWhitespace(lexer.peek) and ((lexer.peek <> #10) or (wsMode = wsStrip)) then
    begin
      c := lexer.grab(cmText);
      if seenNonWhitespace then
      begin
        whitespace := whitespace + c;
      end;
    end
    else
    begin
      if (wsMode <> wsLeave) then
      begin
        if lexer.peek = #10 then
        begin
          seenNonWhitespace := false;
          if whitespace.EndsWith('  ') then
            nodes.addText(lexer.location, '<br />');
          whitespace := '';
        end
        else
        begin
          if whiteSpace <> '' then
          begin
            if wsMode = wsStrip then
              nodes.addText(lexer.location, ' ')
            else
              nodes.addText(lexer.location, whitespace);
            whiteSpace := '';
          end;
          seenNonWhitespace := true;
        end;
      end;
      parseTextCore(lexer, nodes, wsMode);
    end;
  end;
end;

function TCommonMarkEngine.processText(text: String; wsMode : TCMWhitespaceMode; startLine : integer): TCMTextNodes;
var
  lexer : TCMTextLexer;
begin
  result := TCMTextNodes.Create;
  lexer := TCMTextLexer.Create(text, FLines, startLine, FStyling);
  try
    parseText(lexer, result, wsMode);
  finally
    lexer.Free;
  end;
  processEmphasis(result, nil);
  Assert(FStack.count = 0);
end;

procedure TCommonMarkEngine.processInlines(block: TCMBlock; wsMode : TCMWhitespaceMode);
var
  c : TCMBlock;
begin
  if block is TCMTextBlock then
  begin
    (block as TCMTextBlock).FNodes := processText((block as TCMTextBlock).FText, wsMode, block.line);
  end;
  if block is TCMContainerBlock then
    for c in (block as TCMContainerBlock).blocks do
      processInlines(c, block.wsMode);
end;

class function TCommonMarkEngine.process(src: String; gfm : boolean): String;
var
  doc : TCommonMarkDocument;
begin
  doc := parse(src, gfm);
  try
    result := render(doc);
  finally
    doc.Free;
  end;
end;

procedure TCommonMarkEngine.processEmphasis(nodes : TCMTextNodes; stopDel: TCMDelimiter);
var
  current, index, bottom, bottomU, bottomA, bottomW, count, iO, iC, i :  integer;
  strong : boolean;
  n : String;
  o, c : TCMDelimiter;
  ol, cl, line : TCMLine;
  function IsMatch(i, c : TCMDelimiter) : boolean;
  begin
    result := (i.delimiter[1] = c.delimiter[1]) and i.isOpener;
    if result and ((i.mode = dmBoth) or (c.mode = dmBoth)) then
      result := (i.delimiter.length + c.delimiter.length) mod 3 <> 0;
  end;
begin
  if stopDel = nil then
    bottom := 0
  else
    bottom := FStack.IndexOf(stopDel) + 1;
  bottomU := bottom;
  bottomA := bottom;
  current := bottom;
  while current < FStack.count do
  begin
    while (current < FStack.Count) and FStack[current].isEmph and not FStack[current].isCloser do
      inc(current);
    if current = FStack.count then
      break; // we're done.
    if FStack[current].delimiter = '*' then
      bottomW := bottomA
    else
      bottomW := bottomU;
    index := current - 1;
    while (index >= bottomW) and (index >= bottom) and not isMatch(FStack[index], FStack[current]) do
      dec(index);
    if (index >= 0) and (FStack[index].delimiter[1] = FStack[current].delimiter[1]) and (FStack[index].mode in [dmOpener, dmBoth]) then
    begin
      // we have a match
      o := FStack[index];
      c := FStack[current];
      ol := FLines[o.FNode.FPos.line];
      cl := FLines[c.FNode.FPos.line];
      strong := (c.delimiter.Length > 1) and (o.delimiter.Length > 1);
      if strong then n := 'strong' else n := 'em';
      if strong then count := 2 else count := 1;
      nodes.addOpenerAfter(n, o.node);
      nodes.addCloserBefore(n, c.node);
      ol.updateStyle(o.FNode.FPos.col-1+(o.FNode.FContent.Length - count), count, cmDelimiter);
      o.node.removeChars(count);
      cl.updateStyle(c.FNode.FPos.col-1, count, cmDelimiter);
      c.FNode.FPos.col := c.FNode.FPos.col + count;
      c.node.removeChars(count);
      o.delimiter := o.delimiter.Substring(count);
      c.delimiter := c.delimiter.Substring(count);
      iO := FStack.IndexOf(o);
      iC := FStack.IndexOf(c);
      if iC > iO + 1 then
      begin
        for i := iO+1 to iC-iO do
        begin
          line := FLines[FStack[i].FNode.FPos.line];
          line.updateStyle(FStack[i].FNode.FPos.col-1, length(FStack[i].FNode.FContent), cmText);
        end;

        FStack.DeleteRange(iO+1, iC-iO-1);
        dec(current, iC-iO-1);
      end;
      if o.node.isEmpty then
      begin
        nodes.Remove(o.node);
        FStack.Remove(o);
        dec(current);
      end;
      if c.node.isEmpty then
      begin
        nodes.Remove(c.node);
        FStack.Remove(c);
      end;
    end
    else
    begin
      // No match is found
      if FStack[current].delimiter = '*' then
        bottomA := index + 1
      else
        bottomU := index + 1;
      if FStack[current].mode = dmBoth then
        inc(current)
      else
      begin
        cl := FLines[FStack[current].FNode.FPos.line];
        cl.updateStyle(FStack[current].FNode.FPos.col-1, FStack[current].node.FContent.Length, cmText);
        FStack.Delete(current);
      end;
    end;
  end;
  for index := FStack.Count - 1 downto bottom do
  begin
    line := FLines[FStack[index].FNode.FPos.line];
    line.updateStyle(FStack[index].FNode.FPos.col-1, length(FStack[index].FNode.FContent), cmText);
    FStack.Delete(index);
  end;
end;

{ TCommonMarkProcessor }

function TCommonMarkProcessor.GetUnSafe: boolean;
begin
  result := false;
end;

function TCommonMarkProcessor.process(source: String): String;
var
  doc : TCommonMarkDocument;
begin
  doc := TCommonMarkEngine.parse(source, true);
  try
    result := TCommonMarkEngine.render(doc);
  finally
    doc.Free;
  end;

end;

procedure TCommonMarkProcessor.SetUnSafe(const value: boolean);
begin
  if value then
    raise Exception.Create('The common mark processor cannot operate in unsafe mode+');
end;

{ TCMTableBlock }

procedure TCMTableBlock.render(parent: TCMBlock; b: TStringBuilder);
var
  i : integer;
begin
  b.Append('<table>'#10);
  b.Append('<thead>'#10);
  blocks[0].render(self, b);
  b.Append('</thead>'#10);
  if blocks.Count > 1 then
  begin
    b.Append('<tbody>'#10);
    for i := 1 to blocks.Count -1  do
      blocks[i].render(self, b);
    b.Append('</tbody>'#10);
  end;
  b.Append('</table>'#10);
end;

{ TCMTableRowBlock }

procedure TCMTableRowBlock.render(parent: TCMBlock; b: TStringBuilder);
var
  first : boolean;
  i : integer;
  attr : String;
begin
  first := (parent as TCMContainerBlock).blocks.First = self;
  b.Append('<tr>'#10);
  for i := 0 to length((parent as TCMTableBlock).FColumns) - 1 do
  begin
    case (parent as TCMTableBlock).FColumns[i] of
      taLeft : attr := '';
      taCenter : attr := ' align="center"';
      taRight : attr := ' align="right"';
    end;
    if first then
      b.Append('<th'+attr+'>')
    else
      b.Append('<td'+attr+'>');
    if i < blocks.Count then
      blocks[i].render(self, b);
    if first then
      b.Append('</th>'#10)
    else
      b.Append('</td>'#10)
  end;
  b.Append('</tr>'#10);
end;

end.
