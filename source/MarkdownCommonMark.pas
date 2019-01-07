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
Still to do:
- link references
- GFM tables
- follow up on a few failing tests that I don't understand

not planned to be supported
- HTML blocks

note: tests related to link references and HTML blocks run (to check that the processing doesn't blow up), but output comparison is never checked
}

interface

uses
  SysUtils, Classes, Math, Generics.Collections, Character, {$IFDEF FPC} RegExpr {$ELSE} System.RegularExpressions {$ENDIF},
  HTMLEntities,
  MarkdownProcessor;

const
  LENGTH_INCREMENT = 116;
  EMAIL_REGEX = '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$';

// Abstract Syntax Tree
type
  TWhitespaceMode = (wsLeave, wsTrim, wsStrip);

  // inlines
  TTextNode = class
  private
    FName: String;
    FAttrs: TDictionary<String, String>;
    FContent : String;
    FBuild : String;
    FLength : integer;
    FOpener, FCloser : boolean;
    FActive: boolean;
    function GetAttrs: TDictionary<String, String>;

    function renderAttrs : String;
    procedure render(b : TStringBuilder);
    function getText : String;
    procedure SetName(const Value: String);
    procedure SetActive(const Value: boolean);
  public
    constructor Create;
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

  TTextNodes = class (TObjectList<TTextNode>)
  public
    function addOpener(name : String) : TTextNode;
    function addText(cnt : String) : TTextNode; // if the last is text and active, add to that
    function addTextNode(cnt : String) : TTextNode; // always make a new node, and make it inactive
    function addCloser(name : String) : TTextNode;

    // image parsing
    function plainTextSince(node : TTextNode) : String;
    procedure removeAfter(node : TTextNode);

    // emph processing
    procedure addOpenerAfter(name : String; node : TTextNode);
    procedure addCloserBefore(name : String; node : TTextNode);
  end;

  // blocks
  TBlock = class abstract (TObject)
  private
    FClosed: boolean;
    FLine : integer;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); virtual; abstract;
    function wsMode : TWhitespaceMode; virtual;
  public
    constructor Create(line : Integer);
    property closed : boolean read FClosed write FClosed;
    property line : Integer read FLine;
  end;

  TContainerBlock = class abstract (TBlock)
  private
    FBlocks: TObjectList<TBLock>;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    constructor Create(line : Integer);
    destructor Destroy; override;

    property blocks : TObjectList<TBLock> read FBlocks;
  end;

  TMarkdownDocument = class (TContainerBlock);

  TParagraphBlock = class (TContainerBlock)
  private
    FHeader: integer;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; virtual;
    property header : integer read FHeader write FHeader;
  end;

  TQuoteBlock = class (TParagraphBlock)
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; override;
  end;

  TListBlock = class (TContainerBlock)
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
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    property ordered : boolean read FOrdered write FOrdered;
    property baseIndent : integer read FBaseIndent write FBaseIndent;
    property lastIndent : integer read FLastIndent write FLastIndent;
    property start : String read FStart write FStart;
    property marker : String read FMarker write FMarker;
    property loose : boolean read FLoose write FLoose;
  end;

  TListItemBlock = class (TParagraphBlock)
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    function isPlainPara : boolean; override;
  end;

  THeadingBlock = class (TContainerBlock)
  private
    FLevel: integer;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    constructor Create(line, level : Integer);
    property level : integer read FLevel write FLevel;
  end;

  TCodeBlock = class (TContainerBlock)
  private
    FFenced: boolean;
    FLang: String;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
    function wsMode : TWhitespaceMode; override;
  public
    property fenced : boolean read FFenced write FFenced;
    property lang : String read FLang write FLang;
  end;

  TLeafBlock = class abstract (TBlock);

  TThematicBreakBlock = class (TLeafBlock)
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  end;

  TTextBlock = class (TLeafBlock)
  private
    FText: String;
    FNodes : TTextNodes;
  protected
    procedure render(parent : TBlock; b : TStringBuilder); override;
  public
    constructor Create(list : integer; text : String);
    destructor Destroy; override;
    property text : String read FText write FText;
  end;

  // Parser Infrastructure
  TCommonMarkEngine = class;

  TLine = class
  private
    FLine : String;
    FCursor : integer; // fCursor is the corrected character cursor, not the character offset
    FMark : integer;
    procedure getOffset(var index : integer; var preSpaces : integer);
  public
    procedure mark;
    procedure rewind;
    procedure reset(line : String);
    procedure advance(len : integer); // advance x number of spaces or characters
    procedure skipWS; // advance from the current cursor until not pointing at whitespace
    function isEmpty : boolean; // is cursor at end of string
    function focus : String; // what is left after cursor
    function countWS : integer; // whitespace count from cursor
    function isWhitespace : boolean; // if everything after cursor is whitespace
  end;

  TBlockProcessingContext = (bpGeneral, bpCodeBlock, bpFencedCodeBlock);
  TBlockProcessor = class abstract (TObject)
  protected
    FParent : TBlockProcessor;

    // this procedure processes a line for nested blocks
    // return true if the block is done.
    // if false, modify the line, removing prepended characters, and remember to grab the characters as you go
    function processLine(line : TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean) : boolean; virtual; abstract;
    function isList(ordered : boolean; marker : String; indent : integer) : boolean; virtual;
    function inListOrQuote : boolean; virtual;
    function parser : TCommonMarkEngine; virtual;
  public
    constructor Create(processor : TBlockProcessor);
  end;

  // this anchors the chain
  TDocumentProcessor = class (TBlockProcessor)
  private
    FParser : TCommonMarkEngine;
  protected
    function processLine(line : TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean) : boolean; override;
    function parser : TCommonMarkEngine; override;
  public
    constructor Create(parser : TCommonMarkEngine);
  end;

  TQuoteProcessor = class (TBlockProcessor)
  private
    quote : TQuoteBlock;
  protected
    function processLine(line : TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean) : boolean; override;
    function inListOrQuote : boolean; override;
  public
    constructor Create(processor : TBlockProcessor; q : TQuoteBlock);
  end;

  TListProcessor = class (TBlockProcessor)
  private
    FList : TListBlock;
    FItem : TListItemBlock;
    FHasContent : boolean;
    FEmptyLine : integer;
  protected
    function processLine(line : TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean) : boolean; override;
    function isList(ordered : boolean; marker : String; indent : integer) : boolean; override;
    function inListOrQuote : boolean; override;
  public
    constructor Create(processor : TBlockProcessor; list : TListBlock; item : TListItemBlock);
    destructor Destroy; override;
  end;

  TTextLexer = class
  private
    FText : String;
    FCursor : integer;
    FMark, FMarkCol : Integer;
    FBuilder : TStringBuilder;
    FCol : integer;
    function GetDone: boolean;
    function GetPeek: char;
    function GetPeekNext: char;
    function GetPeekLast: char;
    function GetPeekEndRun: char;
  public
    constructor Create(text : String);
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
    function grab : char; overload;
    function grabRun : String; overload;
    function grab(length : integer) : String; overload;
    function has(s : string) : boolean;
    function runExistsAfter(s : String) : boolean;
    procedure skipWS;
  end;

  TDelimiterMode = (dmNeither, dmOpener, dmCloser, dmBoth);
  TDelimiter = class
  private
    Fmode: TDelimiterMode;
    Fdelimiter: String;
    Factive: boolean;
    FNode: TTextNode;
  public
    constructor Create(node : TTextNode; delimiter : String; mode : TDelimiterMode);

    function isOpener : boolean;
    function isCloser : boolean;
    property node : TTextNode read FNode;
    property delimiter : String read Fdelimiter write Fdelimiter;
    property active : boolean read Factive write Factive;
    property mode : TDelimiterMode read Fmode write Fmode;
    function isEmph : boolean;
  end;

  TCommonMarkEngine = class
  private
    FSource : String;
    FCursor : integer;
    FlastLineCursor : Integer;
    FLineNum : integer;
    FBuilder : TStringBuilder;
    FEntities : TDictionary<String, String>;
    FStack : TObjectList<TDelimiter>;

    // line operations
    function grabLine : String;
    function peekLine : String;
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


    // status
    function inPara(blocks : TObjectList<TBlock>; canBeQuote : boolean) : boolean;
    function inList(blocks : TObjectList<TBlock>; ordered : boolean; marker : String; indent : integer; grace : integer; out list : TListBlock) : boolean;
    function isBlock(cont : TBlock; blocks : TObjectList<TBlock>; line : String; wsLen : integer = 3) : boolean;

    // block parsing

    function parseThematicBreak(blocks : TObjectList<TBlock>; line : TLine) : boolean;
    function parseHeader(blocks : TObjectList<TBlock>; line : TLine) : boolean;
    function parseCodeBlock(blocks : TObjectList<TBlock>; line : TLine; processor : TBlockProcessor) : boolean;
    function parseFencedCodeBlock(blocks : TObjectList<TBlock>; line : TLine; processor : TBlockProcessor) : boolean;
    function parseSeTextHeader(blocks : TObjectList<TBlock>; line : TLine; isLazy : boolean; processor : TBlockProcessor) : boolean;
    function parseQuoteBlock(blocks : TObjectList<TBlock>; line : TLine; processor : TBlockProcessor) : boolean;
    function parseUListBlock(blocks : TObjectList<TBlock>; line : TLine; processor : TBlockProcessor) : boolean;
    function parseOListBlock(blocks : TObjectList<TBlock>; line : TLine; processor : TBlockProcessor) : boolean;
    procedure parse(block : TContainerBlock; processor : TBlockProcessor); overload;

    // link references
    procedure parseLinkReferences;

    // inlines
    procedure parseTextEscape(lexer : TTextLexer; nodes: TTextNodes; wsMode : TWhitespaceMode);
    procedure parseEntity(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode);
    function parseEntityInner(lexer : TTextLexer) : String;
    procedure parseBackTick(lexer : TTextLexer; nodes: TTextNodes; wsMode : TWhitespaceMode);
    procedure parseAutoLink(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode);
    procedure parseDelimiter(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode; canRun : boolean);
    procedure parseCloseDelimiter(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode);
    function processInlineLink(lexer : TTextLexer; nodes : TTextNodes; del : TDelimiter) : boolean;
    procedure parseTextCore(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode);
    procedure parseText(lexer : TTextLexer; nodes : TTextNodes; wsMode : TWhitespaceMode);
    function processText(text : String; wsMode : TWhitespaceMode) : TTextNodes;
    procedure parseInline(blocks : TObjectList<TBlock>; line : String);
    procedure processInlines(block : TBlock; wsMode : TWhitespaceMode);
    procedure processEmphasis(nodes : TTextNodes; stopDel : TDelimiter);
  public
    Constructor Create;
    Destructor Destroy; override;
    // divided tinto 2 steps in case some consumer wants to process the syntax tree
    class function parse(src : String) : TMarkdownDocument; overload;
    class function render(doc : TMarkdownDocument) : String;
  end;

  TCommonMarkProcessor = class (TMarkdownProcessor)
  protected
    function GetUnSafe: boolean; override;
    procedure SetUnSafe(const value: boolean); override;
  public
    function process(source : String) : String; override; // want to process the syntax tree? Use the TCommonMarkEngine Directly
  end;


implementation

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

{ TTextNode }

procedure TTextNode.addText(s: String);
var
  ch : char;
begin
  for ch in s do
    addText(ch);
end;

constructor TTextNode.Create;
begin
  inherited Create;
  FActive := true;
end;

procedure TTextNode.addText(ch: char);
begin
  if FLength+1 > FBuild.Length then
    setLength(FBuild, FBuild.Length+LENGTH_INCREMENT);
  inc(FLength);
  FBuild[FLength] := ch;
end;

destructor TTextNode.Destroy;
begin
  FAttrs.Free;
  inherited;
end;

function TTextNode.GetAttrs: TDictionary<String, String>;
begin
  if FAttrs = nil then
    FAttrs := TDictionary<String, String>.create;
  result := FAttrs;
end;

function TTextNode.getText: String;
begin
  Active := false;
  result := FContent;
end;

function TTextNode.isEmpty: boolean;
begin
  result := FContent = '';
end;

procedure TTextNode.removeChars(count : integer);
begin
  active := false;
  delete(FContent, 1, count);
end;

procedure TTextNode.render(b: TStringBuilder);
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

function TTextNode.renderAttrs: String;
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

procedure TTextNode.SetActive(const Value: boolean);
begin
  if active <> Value then
  begin
    Factive := value;
    if not active then
      FContent := Copy(FBuild, 1, FLength);
  end;
end;

procedure TTextNode.SetName(const Value: String);
begin
  FContent := '';
  FLength := 0;
  FName := Value;
  active := false;
end;

{ TTextNodes }

function TTextNodes.addCloser(name: String): TTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TTextNode.Create;
  add(result);
  result.name := name;
  result.closer := true;
end;

procedure TTextNodes.addCloserBefore(name: String; node: TTextNode);
var
  n : TTextNode;
begin
  n := TTextNode.Create;
  insert(IndexOf(node), n);
  n.name := name;
  n.closer := true;
end;

function TTextNodes.addOpener(name: String): TTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TTextNode.Create;
  add(result);
  result.name := name;
  result.opener := true;
end;

procedure TTextNodes.addOpenerAfter(name: String; node: TTextNode);
var
  n : TTextNode;
begin
  n := TTextNode.Create;
  Insert(IndexOf(node)+1, n);
  n.name := name;
  n.opener := true;
end;

function TTextNodes.addText(cnt: String): TTextNode;
begin
  if (count = 0) or (not last.active or last.opener or last.closer) then
  begin
    result := TTextNode.Create;
    add(result);
  end
  else
    result := last;
  result.addText(cnt);
end;

function TTextNodes.addTextNode(cnt: String): TTextNode;
begin
  if count > 0 then
    last.active := false;
  result := TTextNode.Create;
  add(result);
  result.addText(cnt);
  result.active := false;
end;

function TTextNodes.plainTextSince(node: TTextNode): String;
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

procedure TTextNodes.removeAfter(node: TTextNode);
var
  i : integer;
  ndx : integer;
begin
  ndx := indexOf(node);
  for i := count - 1 downto ndx + 1 do
    Delete(i);
end;

{ TBlock }

constructor TBlock.Create(line: Integer);
begin
  Inherited Create;
  FLine := line;
end;

function TBlock.wsMode: TWhitespaceMode;
begin
  result := wsTrim;
end;

{ TContainerBlock }

constructor TContainerBlock.Create;
begin
  inherited create(line);
  FBlocks := TObjectList<TBLock>.create(true);
end;

destructor TContainerBlock.Destroy;
begin
  FBlocks.Free;
  inherited Destroy;
end;

procedure TContainerBlock.render(parent : TBlock; b: TStringBuilder);
var
  c : TBlock;
begin
  for c in FBlocks do
    c.render(self, b);
end;

{ TParagraphBlock }

function TParagraphBlock.isPlainPara: boolean;
begin
  result := FHeader = 0;
end;

procedure TParagraphBlock.render(parent : TBlock; b: TStringBuilder);
var
  c : TBlock;
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

{ TQuoteBlock }

function TQuoteBlock.isPlainPara: boolean;
begin
  result := false;
end;

procedure TQuoteBlock.render(parent : TBlock; b: TStringBuilder);
var
  c : TBlock;
begin
  b.Append('<blockquote>'#10);
  for c in FBlocks do
    c.render(self, b);
  b.Append('</blockquote>'#10);
end;

{ TListBlock }

function TListBlock.grace: integer;
begin
  if ordered then
    result := 2
  else
    result := 1;
end;

procedure TListBlock.render(parent : TBlock; b: TStringBuilder);
var
  c : TBlock;
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

{ TListItemBlock }

function TListItemBlock.isPlainPara: boolean;
begin
  result := false;
end;

procedure TListItemBlock.render(parent : TBlock; b: TStringBuilder);
var
  c, cp : TBlock;
  first, rFirst : boolean;
begin
  if Blocks.Count = 0 then
    b.Append('<li>')
  else
  begin
    b.Append('<li>');
    rFirst := true;
    for c in FBlocks do
      if (c is TParagraphBlock) and (c as TParagraphBlock).isPlainPara and not (parent as TListBlock).loose then
      begin
        first := true;
        for cp in (c as TParagraphBlock).Blocks do
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

{ THeadingBlock }

constructor THeadingBlock.Create(line, level: Integer);
begin
  Inherited Create(line);
  FLevel := level;
end;

procedure THeadingBlock.render(parent : TBlock; b: TStringBuilder);
begin
  b.Append('<h'+inttostr(FLevel)+'>');
  inherited render(parent, b);
  b.Append('</h'+inttostr(FLevel)+'>');
  b.Append(#10);
end;

{ TCodeBlock }

procedure TCodeBlock.render(parent : TBlock; b: TStringBuilder);
var
  c : TBlock;
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

function TCodeBlock.wsMode: TWhitespaceMode;
begin
  result := wsLeave;
end;

{ TThematicBreakBlock }

procedure TThematicBreakBlock.render(parent : TBlock; b: TStringBuilder);
begin
  b.Append('<hr />');
  b.Append(#10);
end;

{ TTextBlock }

constructor TTextBlock.Create(list : integer; text: String);
begin
  inherited Create(line);
  FText := text;
end;

destructor TTextBlock.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TTextBlock.render(parent : TBlock; b: TStringBuilder);
var
  n : TTextNode;
begin
  for n in FNodes do
    n.render(b);
end;

{ TLine }

procedure TLine.reset(line: String);
begin
  FLine := line;
  FCursor := 1;
end;

procedure TLine.rewind;
begin
  FCursor := FMark;
end;

procedure TLine.advance(len: integer);
begin
  inc(FCursor, len);
end;

function TLine.isEmpty: boolean;
var
  index, preSpaces: integer;
begin
  getOffset(index, preSpaces);
  result := (preSpaces = 0) and (index > FLine.Length);
end;

function TLine.isWhitespace: boolean;
var
  index, preSpaces, i: integer;
begin
  getOffset(index, preSpaces);
  result := true;
  for i := index to FLine.Length do
    if not isWhitespaceChar(Fline[i]) then
      exit(false);
end;

procedure TLine.mark;
begin
  FMark := FCursor;
end;

function TLine.countWS: integer;
var
  index, preSpaces, c, len: integer;
begin
  getOffset(index, preSpaces);
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

function TLine.focus: String;
var
  index, preSpaces, i: integer;
begin
  getOffset(index, preSpaces);
  SetLength(result, preSpaces);
  for i := 1 to result.Length do
    result[i] := ' ';
  result := result+FLine.Substring(index-1);
end;

procedure TLine.getOffset(var index, preSpaces: integer);
var
  c, len : integer;
begin
  c := 1;
  index := 1;
  while (c < FCursor) and (index <= Fline.length) do
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
    preSpaces := c - FCursor;
end;

procedure TLine.skipWS;
var
  index, preSpaces, c, len: integer;
begin
  getOffset(index, preSpaces);
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

{ TBlockProcessor }

constructor TBlockProcessor.Create(processor: TBlockProcessor);
begin
  inherited Create;
  FParent := processor;
end;

function TBlockProcessor.inListOrQuote: boolean;
begin
   result := false;
end;

function TBlockProcessor.isList(ordered: boolean; marker: String; indent: integer): boolean;
begin
  result := false;
end;

function TBlockProcessor.parser: TCommonMarkEngine;
begin
  result := FParent.parser;
end;

{ TDocumentProcessor }

constructor TDocumentProcessor.Create(parser : TCommonMarkEngine);
begin
  inherited Create(nil);
  FParser := parser;
end;

function TDocumentProcessor.parser: TCommonMarkEngine;
begin
  result := FParser;
end;

function TDocumentProcessor.processLine(line: TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean): boolean;
begin
  result := false; // document is only ended by end of source
  isLazy := false;
end;

{ TQuoteProcessor }

constructor TQuoteProcessor.Create(processor : TBlockProcessor; q: TQuoteBlock);
begin
  inherited Create(processor);
  quote := q;
end;

function TQuoteProcessor.inListOrQuote: boolean;
begin
  result := true;
end;

function TQuoteProcessor.processLine(line: TLine; root : boolean; context : TBlockProcessingContext; var isLazy : boolean): boolean;
var
  len : integer;
begin
  result := FParent.processLine(line, false, context, isLazy);
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

{ TListProcessor }

constructor TListProcessor.Create(processor: TBlockProcessor; list : TListBLock; item: TListItemBlock);
begin
  inherited Create(processor);
  Flist := list;
  Fitem := item;
end;

destructor TListProcessor.Destroy;
begin
  if FList.FHasSeenEmptyLine and not FList.FLoose and (FParent is TListProcessor) then
    (FParent as TListProcessor).FList.FHasSeenEmptyLine := true;
  inherited;
end;

function TListProcessor.inListOrQuote: boolean;
begin
  result := true;
end;

function TListProcessor.isList(ordered: boolean; marker: String; indent: integer): boolean;
begin
  result := (FList.ordered = ordered) and (FList.marker = marker) and (indent < FList.lastIndent + 1);
end;

function TListProcessor.processLine(line: TLine; root: boolean; context : TBlockProcessingContext; var isLazy : boolean): boolean;
var
  len : integer;
begin
  result := FParent.processLine(line, false, context, isLazy);
  if not result then
  begin
    if parser.FLineNum = FItem.line then
      line.advance(FList.lastIndent)
    else if line.countWS >= FList.LastIndent then
    begin
      len := line.countWS;
      if len > FList.lastIndent+1 then
        len := FList.baseIndent;
      line.advance(len);
    end
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
          if FEmptyLine = 0 then
            FEmptyLine := parser.FLineNum
          else if FEmptyLine <> parser.FLineNum then
            result := true;
        end;
        if (context = bpGeneral) and (parser.FLineNum <> FItem.line) then
          FList.FHasSeenEmptyLine := true;
      end;
    end;
  end
end;


{ TTextLexer }

constructor TTextLexer.Create(text: String);
begin
  inherited Create;
  FText := text;
  FCursor := 1;
  FBuilder := TStringBuilder.create;
end;

destructor TTextLexer.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

procedure TTextLexer.rewind;
begin
  FCursor := FMark;
  FCol := FMarkCol;
end;

function TTextLexer.runExistsAfter(s: String): boolean;
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

procedure TTextLexer.skipWS;
begin
  while isWhitespaceChar(peek) do
    grab;
end;

function TTextLexer.GetDone: boolean;
begin
  result := FCursor > FText.Length;
end;

function TTextLexer.GetPeek: char;
begin
  if done then
    result := #0
  else
    result := FText[FCursor];
end;

function TTextLexer.GetPeekEndRun: char;
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

function TTextLexer.GetPeekLast: char;
begin
  if FCursor = 1 then
    result := #0
  else
    result := FText[FCursor-1];
end;

function TTextLexer.GetPeekNext: char;
begin
  if FCursor >= FText.Length then
    result := #0
  else
    result := FText[FCursor+1];
end;

function TTextLexer.peekRun(checkBefore : boolean): String;
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

function TTextLexer.grab(length: integer): String;
var
  i : integer;
begin
  FBuilder.clear;
  for i := 1 to length do
    if not done then
      FBuilder.Append(grab);
  result := FBuilder.toString;
end;

function TTextLexer.grabRun: String;
var
  c : char;
begin
  FBuilder.Clear;
  c := peek;
  while peek = c do
     FBuilder.append(grab);
  result := FBuilder.ToString;
end;

function TTextLexer.grab: char;
begin
  if done then
    result := #0
  else
  begin
    result := FText[FCursor];
    inc(FCursor);
    if result = #10 then
      FCol := 1
    else
      inc(FCol);
  end;
end;

function TTextLexer.has(s: string): boolean;
begin
  result := peekLen(s.Length) = s;
end;

procedure TTextLexer.mark;
begin
  FMark := FCursor;
  FMarkCol := FCol;
end;

function TTextLexer.peekUntil(chs : TSysCharSet) : String;
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

function TTextLexer.peekLen(length: integer): String;
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

{ TDelimiter }

constructor TDelimiter.Create(node: TTextNode; delimiter : String; mode: TDelimiterMode);
begin
  inherited create;
  FNode := node;
  FMode := mode;
  Fdelimiter := delimiter;
  FActive := true;
end;

function TDelimiter.isCloser: boolean;
begin
  result := mode in [dmCloser, dmBoth];
end;

function TDelimiter.isEmph: boolean;
begin
  result := (delimiter <> '[') and (delimiter <> '![');
end;

function TDelimiter.isOpener: boolean;
begin
  result := mode in [dmOpener, dmBoth];
end;

{ TCommonMarkEngine }

class function TCommonMarkEngine.parse(src: String): TMarkdownDocument;
var
  this : TCommonMarkEngine;
  doc : TDocumentProcessor;
begin
  this := TCommonMarkEngine.Create;
  try
    this.FSource := src.Replace(#13#10, #10).replace(#13, #10);
    this.FCursor := 1;
    this.FLineNum := 0;
    doc := TDocumentProcessor.Create(this);
    try
      result := TMarkdownDocument.Create(1);
      this.parse(result, doc);
      this.parseLinkReferences;
      this.processInlines(result, wsTrim);
    finally
      doc.Free;
    end;
  finally
    this.free;
  end;
end;

class function TCommonMarkEngine.render(doc: TMarkdownDocument): String;
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
  FBuilder := TStringBuilder.Create;
  FStack := TObjectList<TDelimiter>.Create(true);
  FEntities := TDictionary<String, String>.create;
  registerEntities(FEntities);
end;

destructor TCommonMarkEngine.Destroy;
begin
  FEntities.Free;
  FStack.Free;
  FBuilder.Free;
  inherited;
end;

function TCommonMarkEngine.grabLine: String;
begin
  FlastLineCursor := FCursor;
  while (FCursor <= FSource.Length) and (FSource[FCursor] <> #10) do
    inc(FCursor);
  result := copy(FSource, FlastLineCursor, FCursor - FlastLineCursor);
  inc(FLineNum);
  if (FCursor <= FSource.Length) then
    inc(FCursor);
end;

function TCommonMarkEngine.peekLine: String;
var
  i : integer;
begin
  i := FCursor;
  while (i <= FSource.Length) and (FSource[i] <> #10) do
    inc(i);
  result := copy(FSource, FCursor, i - FCursor);
end;

procedure TCommonMarkEngine.redoLine;
begin
  dec(FLineNum);
  FCursor := FlastLineCursor;
end;

function TCommonMarkEngine.done: boolean;
begin
  result := FCursor > length(FSource);
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


function TCommonMarkEngine.inList(blocks: TObjectList<TBlock>; ordered: boolean; marker : String; indent : integer; grace : integer; out list: TListBlock): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TListBlock);
  if result then
  begin
    list := blocks.Last as TListBlock;
    result := not list.closed and (list.ordered = ordered) and (list.marker = marker)
       and (indent <= list.LastIndent + grace);
  end;
end;

function TCommonMarkEngine.inPara(blocks: TObjectList<TBlock>; canBeQuote : boolean): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TParagraphBlock) and not (blocks.Last as TParagraphBlock).closed and ((blocks.Last as TParagraphBlock).header = 0);
  if result and not canBeQuote and not (blocks.Last as TParagraphBlock).isPlainPara then
    result := false;
end;

function TCommonMarkEngine.isBlock(cont : TBlock; blocks : TObjectList<TBlock>; line: String; wsLen : integer = 3): boolean;
  function inOrderedList : boolean;
  begin
    result := (cont is TListBlock) and (cont as TListBlock).ordered;
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
  p := (blocks.count > 0) and (blocks.last is TParagraphBlock) and not (blocks.last.closed);
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


function TCommonMarkEngine.parseThematicBreak(blocks : TObjectList<TBlock>; line: TLine): boolean;
var
  s : String;
begin
  if line.countWS >= 4 then
    exit(false);
  s := stripWhitespace(line.focus);
  if (s.StartsWith('***') or s.StartsWith('---') or s.StartsWith('___')) and AllCharsSame(s) then
  begin
    blocks.Add(TThematicBreakBlock.Create(FlineNum));
    result := true;
  end
  else
    result := false;
end;

function TCommonMarkEngine.parseUListBlock(blocks: TObjectList<TBlock>; line: TLine; processor: TBlockProcessor): boolean;
var
  list : TListBlock;
  li : TListItemBlock;
  lp : TListProcessor;
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
    list := TListBlock.Create(FLineNum);
    blocks.add(list);
    list.ordered := false;
    list.BaseIndent := i+i2+1;
    list.lastIndent := list.BaseIndent;
    list.marker := m;
  end
  else
    list.Lastindent := i+i2+1;
  li := TListItemBlock.Create(FLineNum);
  list.blocks.Add(li);
  result := true;
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TListProcessor.Create(processor, list, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
end;

function TCommonMarkEngine.parseHeader(blocks : TObjectList<TBlock>; line: TLine): boolean;
var
  len : integer;
  ls, s : String;
  b : THeadingBlock;
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
  b := THeadingBlock.Create(FLineNum, len);
  blocks.Add(b);
  s := ls.substring(len).trim;
  if (s <> '') then
  begin
    len := length(s);
    while s[len] = '#' do
      dec(len);
    if (len = 0) then
      s := ''
    else if (s[len] = ' ') then
      s := copy(s, 1, len-1);
  end;
  parseInline(b.blocks, s);
end;

procedure TCommonMarkEngine.parseInline(blocks : TObjectList<TBlock>; line : String);
var
  b : TTextBlock;
begin
  if (blocks.Count > 0) and (blocks.Last is TTextBlock) then
  begin
    b := blocks.Last as TTextBlock;
    b.FText := b.FText+#10+line;
  end
  else
    blocks.Add(TTextBlock.create(FLineNum, line));
end;

function TCommonMarkEngine.parseOListBlock(blocks: TObjectList<TBlock>; line: TLine; processor: TBlockProcessor): boolean;
var
  list : TListBlock;
  li : TListItemBlock;
  lp : TListProcessor;
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
    list := TListBlock.Create(FLineNum);
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
  li := TListItemBlock.Create(FLineNum);
  list.blocks.Add(li);
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TListProcessor.Create(processor, list, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
end;

function TCommonMarkEngine.parseQuoteBlock(blocks: TObjectList<TBlock>; line: TLine; processor : TBlockProcessor): boolean;
var
  s : String;
  q : TQuoteBlock;
  qp : TQuoteProcessor;
begin
  if line.countWS >= 4 then
    exit(false);
  s := line.focus.Trim;
  if not s.StartsWith('>') then
    exit(false);
  q := TQuoteBlock.Create(FLineNum);
  blocks.add(q);
  result := true;
  redoline;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  qp := TQuoteProcessor.Create(processor, q);
  try
    parse(q, qp);
  finally
    qp.Free;
  end;
  q.closed := true;
end;

function TCommonMarkEngine.parseSeTextHeader(blocks : TObjectList<TBlock>; line: TLine; isLazy : boolean; processor : TBlockProcessor): boolean;
var
  p : TParagraphBlock;
  s : String;
begin
  if line.countWS >= 4 then
    exit(false);
  if blocks.Count = 0 then
    exit(false);
  if not inPara(blocks, false) then
    exit(false);
  p := blocks.Last as TParagraphBlock;
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
    exit(true);
  end
  else if s[1] = '=' then
  begin
    p.header := 1;
    exit(true);
  end
  else
    exit(false);
end;

function TCommonMarkEngine.parseCodeBlock(blocks : TObjectList<TBlock>; line: TLine; processor : TBlockProcessor): boolean;
var
  c : TCodeBlock;
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
  c := TCodeBlock.Create(FLineNum);
  blocks.Add(c);
  line.advance(indent);
  s := line.focus;
  repeat
    c.FBlocks.Add(TTextBlock.Create(FLineNum, s));
    if done then
      more := false
    else
    begin
      line.reset(peekLine);
      if processor.processLine(line, true, bpCodeBlock, isLazy) then
        break;
      s := line.focus;
      if lengthWSCorrected(s) <= indent then
        more := isWhitespace(s)
      else
        more := countWS(s) >= indent;
      if more then
      begin
        line.reset(grabLine);
        processor.processLine(line, true, bpCodeBlock, isLazy);
        s := removeWS(line.focus, indent);
      end;
    end;
  until not more;
  // remove any whitespace lines at the end:
  while (c.blocks.Count > 0) and isWhitespace((c.blocks.Last as TTextBlock).text) do
    c.blocks.Delete(c.blocks.Count - 1);
end;

function TCommonMarkEngine.parseFencedCodeBlock(blocks: TObjectList<TBlock>; line: TLine; processor : TBlockProcessor): boolean;
var
  toEnd : String;
  isLazy : boolean;
  terminated : boolean;
  function isEnded: boolean;
  var
    s : String;
  begin
    line.reset(peekLine);
    if processor.processLine(line, true, bpFencedCodeBlock, isLazy) then
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
  c : TCodeBlock;
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

  c := TCodeBlock.Create(FLineNum);
  blocks.Add(c);
  c.fenced := true;
  c.lang := lang;
  terminated := false;

  while not done and not terminated and not isEnded do
  begin
    line.reset(grabLine);
    if processor.processLine(line, true, bpFencedCodeBlock, isLazy) then
      break;
    s := line.focus;
    if indent > 0 then
    begin
      i := 0;
      while (i < s.Length) and (i < indent) and (s[i+1] = ' ') do
        inc(i);
      if i > 0 then
        s := s.Substring(i);
    end;
    c.FBlocks.Add(TTextBlock.Create(FLineNum, s));
  end;
  if not done and not terminated then
    grabLine;
end;

procedure TCommonMarkEngine.parse(block: TContainerBlock; processor : TBlockProcessor);
var
  line : TLine;
  p : TParagraphBlock;
  isLazy : boolean;
begin
  line := TLine.Create;
  try
    while not done do // must be at start of line here
    begin
      line.reset(grabLine);
      if processor.processLine(line, true, bpGeneral, isLazy) then
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
      else
      begin
        if inPara(block.blocks, true) then
          p := block.Blocks.Last as TParagraphBlock
        else
          p := nil;
        if line.isEmpty or line.isWhitespace then
        begin
          if (p <> nil) then
            p.closed := true;
        end
        else
        begin
          if (p = nil) then
          begin
            p := TParagraphBlock.Create(FLineNum);
            block.blocks.Add(p);
          end;
          parseInLine(p.blocks, line.focus);
        end;
      end;
    end;
  finally
    line.Free;
  end;
end;


procedure TCommonMarkEngine.parseLinkReferences;
begin
  // not doing this for the moment?
end;


procedure TCommonMarkEngine.parseAutoLink(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
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
  a : TTextNode;
begin
  lexer.grab;
  s := lexer.peekUntil(['>']);
  if (wsMode <> wsLeave) then
  begin
    if isAbsoluteURI(s) then
    begin
      a := nodes.addOpener('a');
      a.attrs.Add('href', urlEscape(s));
      nodes.addText(htmlEscape(s));
      nodes.addCloser('a');
      lexer.grab(s.Length + 1);
      exit;
    end else if TRegEx.IsMatch(s, EMAIL_REGEX) then
    begin
      a := nodes.addOpener('a');
      a.attrs.Add('href', 'mailto:'+htmlEscape(s));
      nodes.addText(htmlEscape(s));
      nodes.addCloser('a');
      lexer.grab(s.Length + 1);
      exit;
    end
    else
      nodes.addText('&lt;');
  end
  else
    nodes.addText('&lt;');
end;

procedure TCommonMarkEngine.parseDelimiter(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode; canRun : boolean);
  function isWSForFLanking(c : char) : boolean;
  begin
    result := CharInSet(c,[#0, #10]) or IsWhiteSpace(c);
  end;

  function isPuncForFLanking(c : char) : boolean;
  begin
    result := CharInSet(c, ['!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\', ']', '^', '_', '`', '{', '|', '}', '~'])
      or (c.GetUnicodeCategory in [TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation, TUnicodeCategory.ucClosePunctuation,
        TUnicodeCategory.ucFinalPunctuation, TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation, TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucMathSymbol]);
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
  m : TDelimiterMode;
  n : TTextNode;
begin
  if canRun then
    s := lexer.peekRun(false)
  else if lexer.peek = '!' then
    s := lexer.peekLen(2)
  else
    s := lexer.peek;
  cb := lexer.peekLast;
  lexer.grab(s.Length);
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
  n := nodes.addTextNode(s);
  FStack.Add(TDelimiter.Create(n, s, m));
end;

procedure TCommonMarkEngine.parseBackTick(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
var
  s : String;
  ws, first : boolean;
  ch : char;
begin
  s := lexer.peekRun(false);
  if lexer.runExistsAfter(s) then
  begin
    nodes.addOpener('code');
    lexer.grab(s.Length);
    ws := false;
    first := true;
    while lexer.peekRun(true) <> s do
    begin
      ch := lexer.peek;
      if CharInSet(ch, [' ', #10]) then
      begin
        ws := true;
        lexer.grab;
      end
      else
      begin
        if ws and not first then
          nodes.addText(' ');
        first := false;
        ws := false;
        nodes.addText(htmlEscape(lexer.grab));
      end;
    end;
    nodes.addCloser('code');
    lexer.grab(s.Length);
  end
  else
    nodes.addText(lexer.grab(s.Length));
end;

procedure TCommonMarkEngine.parseTextEscape(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
begin
  lexer.grab;
  if isEscapable(lexer.peek) then
    nodes.addText(htmlEscape(lexer.grab))
  else if lexer.peek = #10 then
    nodes.addText('<br />')
  else
    nodes.addText('\');
end;

function TCommonMarkEngine.processInlineLink(lexer: TTextLexer; nodes: TTextNodes; del : TDelimiter): boolean;
var
  lvl : integer;
  url : String;
  marker : char;
  title : String;
  b : TStringBuilder;
  d : TDelimiter;
begin
  result := false;
  if lexer.peek <> '(' then
    exit(false);
  b := TStringBuilder.Create;
  lexer.mark;
  try
    lexer.grab; // (
    lexer.skipWS;
    if lexer.peek = '<' then
    begin
      lexer.grab;
      while not lexer.done and (lexer.peek <> '>') do
      begin
        if (lexer.peek = '\') and isEscapable(lexer.peekNext) then
        begin
         lexer.grab;
         if lexer.done then
           exit;
         b.Append(lexer.grab);
        end
        else if isWhitespaceChar(lexer.peek) then
          exit
        else if lexer.peek = '&' then
          b.Append(parseEntityInner(lexer))
        else
          b.Append(lexer.grab);
      end;
      if lexer.done then
        exit;
      lexer.grab;
    end
    else
    begin
      lvl := 0;
      while not lexer.done and not ((lexer.peek = ')') and (lvl = 0)) and not isWhitespace(lexer.peek) do
      begin
        if (lexer.peek = '\') and isEscapable(lexer.peekNext) then
        begin
         lexer.grab;
         if lexer.done then
           exit;
         b.Append(lexer.grab);
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
          b.Append(lexer.grab);
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
      marker := lexer.grab;
      if not CharInSet(marker, ['"', '''', '(']) then
        exit;
      if marker = '(' then
        marker := ')';
      while not lexer.done and (lexer.peek <> marker) do
      begin
        if (lexer.peek = '\') then
        begin
          lexer.grab;
          if lexer.done then
            exit;
          b.Append(htmlEscape(lexer.grab));
        end
        else if lexer.peek = '&' then
          b.Append(htmlEscape(parseEntityInner(lexer)))
        else if lexer.peek = #10 then
          b.Append(' ')
        else if lexer.peek = '"' then
          b.Append(htmlEscape(lexer.grab))
        else
          b.Append(lexer.grab);
      end;
      if lexer.done then
        exit;
      lexer.grab;
      title := b.toString;
    end;
    lexer.skipWS;
    if lexer.peek <> ')' then
      exit;
    lexer.grab;
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
      nodes.addCloser('a');
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

procedure TCommonMarkEngine.parseCloseDelimiter(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
var
  i : integer;
  d : TDelimiter;
begin
  lexer.grab; // ]
  d := nil;
  for i := FStack.Count - 1 downto 0 do
    if (FStack[i].delimiter = '[') or (FStack[i].delimiter = '![') then
    begin
      d := FStack[i];
      break;
    end;
  if d = nil then
  begin
    nodes.addText(']');
    exit;
  end;
  if not d.active then
  begin
    FStack.Remove(d);
    nodes.addText(']');
    exit;
  end;
  // now, look ahead, do we have
  //  - inline link/image  [text](link title)
  //  - reference link/image [text][ref]
  //  - compact reference link/image [label][]
  //  - shortcut reference link/image [label]
  if not processInlineLink(lexer, nodes, d) then
  begin
    FStack.Remove(d);
    nodes.addText(']');
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
      if (i = 0) or (ch.GetUnicodeCategory = TUnicodeCategory.ucUnassigned) then
        ch := #$FFFD;
    end;
    exit(ch);
  end
  else
    exit('');
end;

procedure TCommonMarkEngine.parseEntity(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
begin
  if wsMode = wsLeave then // code block
  begin
    nodes.addText('&amp;');
    lexer.grab;
  end
  else
    nodes.addText(htmlEscape(parseEntityInner(lexer)));
end;

function TCommonMarkEngine.parseEntityInner(lexer: TTextLexer): String;
var
  s, c : String;
  ch : char;
  i : integer;
begin
  lexer.grab;
  s := lexer.peekUntil([';']);
  if FEntities.TryGetValue('&'+s+';', c) then
  begin
    lexer.grab(s.Length+1);
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
      if (i = 0) or (ch.GetUnicodeCategory = TUnicodeCategory.ucUnassigned) then
        ch := #$FFFD;
    end;
    lexer.grab(s.Length+1);
    exit(ch);
  end
  else
    exit('&');
end;

procedure TCommonMarkEngine.parseTextCore(lexer: TTextLexer; nodes: TTextNodes; wsMode: TWhitespaceMode);
begin
  if wsMode = wsLeave then // code block
    nodes.addText(htmlEscape(lexer.grab))
  else
  case lexer.peek of
    '\' : parseTextEscape(lexer, nodes, wsMode);
    '<' : parseAutoLink(lexer, nodes, wsMode);
    '>', '"' : nodes.addText(htmlEscape(lexer.grab));
    '&' : parseEntity(lexer, nodes, wsMode);
    '`' : parseBackTick(lexer, nodes, wsMode);
    '*' : parseDelimiter(lexer, nodes, wsMode, true);
    '_' : if isWhitespaceChar(lexer.peekLast) or isEscapable(lexer.peekLast) or (lexer.peekLast = #0) or
             isWhitespaceChar(lexer.peekEndRun) or isEscapable(lexer.peekEndRun) or (lexer.peekEndRun = #0) then
            parseDelimiter(lexer, nodes, wsMode, true)
          else
            nodes.addText(htmlEscape(lexer.grabRun));
    '[' : parseDelimiter(lexer, nodes, wsMode, false);
    '!' : if lexer.peekNext = '[' then
            parseDelimiter(lexer, nodes, wsMode, false)
          else
            nodes.addText(lexer.grab);
    ']' : parseCloseDelimiter(lexer, nodes, wsMode);
  else
    nodes.addText(lexer.grab);
  end;
end;

procedure TCommonMarkEngine.parseText(lexer: TTextLexer; nodes: TTextNodes; wsMode : TWhitespaceMode);
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
      c := lexer.grab;
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
            nodes.addText('<br />');
          whitespace := '';
        end
        else
        begin
          if whiteSpace <> '' then
          begin
            if wsMode = wsStrip then
              nodes.addText(' ')
            else
              nodes.addText(whitespace);
            whiteSpace := '';
          end;
          seenNonWhitespace := true;
        end;
      end;
      parseTextCore(lexer, nodes, wsMode);
    end;
  end;
end;

function TCommonMarkEngine.processText(text: String; wsMode : TWhitespaceMode): TTextNodes;
var
  lexer : TTextLexer;
begin
  result := TTextNodes.Create;
  lexer := TTextLexer.Create(text);
  try
    parseText(lexer, result, wsMode);
  finally
    lexer.Free;
  end;
  processEmphasis(result, nil);
  Assert(FStack.count = 0);
end;

procedure TCommonMarkEngine.processInlines(block: TBlock; wsMode : TWhitespaceMode);
var
  c : TBlock;
begin
  if block is TTextBlock then
  begin
    (block as TTextBlock).FNodes := processText((block as TTextBlock).FText, wsMode);
  end;
  if block is TContainerBlock then
    for c in (block as TContainerBlock).blocks do
      processInlines(c, block.wsMode);
end;

procedure TCommonMarkEngine.processEmphasis(nodes : TTextNodes; stopDel: TDelimiter);
var
  current, index, bottom, bottomU, bottomA, bottomW, count, iO, iC :  integer;
  strong : boolean;
  n : String;
  o, c : TDelimiter;
  function IsMatch(i, c : TDelimiter) : boolean;
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
      strong := (c.delimiter.Length > 1) and (o.delimiter.Length > 1);
      if strong then n := 'strong' else n := 'em';
      if strong then count := 2 else count := 1;
      nodes.addOpenerAfter(n, o.node);
      nodes.addCloserBefore(n, c.node);
      o.node.removeChars(count);
      c.node.removeChars(count);
      o.delimiter := o.delimiter.Substring(count);
      c.delimiter := c.delimiter.Substring(count);
      iO := FStack.IndexOf(o);
      iC := FStack.IndexOf(c);
      if iC > iO + 1 then
      begin
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
        FStack.Delete(current);
    end;
  end;
  for index := FStack.Count - 1 downto bottom do
    FStack.Delete(index);
end;

{ TCommonMarkProcessor }

function TCommonMarkProcessor.GetUnSafe: boolean;
begin
  result := false;
end;

function TCommonMarkProcessor.process(source: String): String;
var
  doc : TMarkdownDocument;
begin
  doc := TCommonMarkEngine.parse(source);
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

end.
