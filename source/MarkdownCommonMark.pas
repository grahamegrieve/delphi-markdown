Unit MarkdownCommonMark;

{
currently working:
- headings
- code blocks (+fenced)
- thematic blocks

not supported
- HTML blocks
}
interface

uses
  SysUtils, Generics.Collections;



// Abstract Syntax Tree
type
  TBlock = class abstract (TObject)
  protected
    procedure render(b : TStringBuilder); virtual; abstract;
  end;

  TContainerBlock = class abstract (TBlock)
  private
    FBlocks: TObjectList<TBLock>;
  protected
    procedure render(b : TStringBuilder); override;
  public
    constructor Create;
    destructor Destroy; override;

    property blocks : TObjectList<TBLock> read FBlocks;
  end;

  TMarkdownDocument = class (TContainerBlock);
//  TListItemBlock = class (TContainerBlock);
//  TListBlock = class (TContainerBlock);
//  TLinkReferenceDefinitionGroup = class (TContainerBlock);

  TParagraphBlock = class (TContainerBlock)
  private
    FClosed: boolean;
    FHeader: integer;
  protected
    procedure render(b : TStringBuilder); override;
  public
    property closed : boolean read FClosed write FClosed;
    property header : integer read FHeader write FHeader;
  end;

  TQuoteBlock = class (TParagraphBlock)
  protected
    procedure render(b : TStringBuilder); override;
  end;

  THeadingBlock = class (TContainerBlock)
  private
    FLevel: integer;
  protected
    procedure render(b : TStringBuilder); override;
  public
    constructor Create(level : Integer);
    property level : integer read FLevel write FLevel;
  end;

  TCodeBlock = class (TContainerBlock)
  private
    FFenced: boolean;
    FLang: String;
  protected
    procedure render(b : TStringBuilder); override;
  public
    property fenced : boolean read FFenced write FFenced;
    property lang : String read FLang write FLang;
  end;

  TLeafBlock = class abstract (TBlock);
  TTextBlock = class (TLeafBlock)
  private
    FText: String;
  protected
    procedure render(b : TStringBuilder); override;
  public
    constructor Create(text : String);
    property text : String read FText write FText;
  end;

  TThematicBreakBlock = class (TLeafBlock)
  protected
    procedure render(b : TStringBuilder); override;
  end;
//  TLinkReferenceDefinition = class (TLeafBlock);
//  THtmlBlock = class (TLeafBlock);
//  TFencedCodeBlock = class (TCodeBLock);
//  TBlankLineBlock = class (TBlock);

  // Parser Infrastructure
  TCommonMarkParser = class;

// Parser
  TBlockProcessor = class abstract (TObject)
  protected
    FParent : TBlockProcessor;

    // this procedure processes a line for nested blocks
    //
    // return false if the block is done.
    //
    // if true, modify the line, removing prepended characters, and remember to grab the characters as you go
    function processLine(var line : string; root : boolean) : boolean; virtual; abstract;
    function parser : TCommonMarkParser; virtual;
  public
    constructor Create(processor : TBlockProcessor);
  end;

  // this anchors the chain
  TDocumentProcessor = class (TBlockProcessor)
  private
    FParser : TCommonMarkParser;
  protected
    function processLine(var line : string; root : boolean) : boolean; override;
    function parser : TCommonMarkParser; override;
  public
    constructor Create(parser : TCommonMarkParser);
  end;

  TQuoteProcessor = class (TBlockProcessor)
  private
    quote : TQuoteBlock;
  protected
    function processLine(var line : string; root : boolean) : boolean; override;
  public
    constructor Create(processor : TBlockProcessor; q : TQuoteBlock);
  end;

  TCommonMarkParser = class (TObject)
  private
    FSource : String;
    FCursor : integer;
    FLine : integer;
    FCol : integer;
    FBuilder : TStringBuilder;


    function done : boolean;
    function peek : char; overload;
    function peek(offset : integer) : char; overload;
    function peekStr(length : integer) : String; overload;
    function peekline : String;
    function grab : char; overload;
    function grab(length : integer) : string; overload;
    function has(ch : char) : boolean; overload;
    function has(chs : TSysCharSet) : boolean; overload;
    function has(s : String) : boolean; overload;
    function grabTo(ch : char) : string; overload;
    function grabTo(chs : TSysCharSet) : string; overload;
    function grabWhile(ch : char) : string; overload;
    function grabWhile(chs : TSysCharSet) : string; overload;
    function grabLine: string; overload;

    function isWhitespace(ch : char) : boolean; overload;
    function isWhitespace(s : String) : boolean; overload;
    function stripWhitespace(s : String) : String;
    function escapeHtml(s : String) : String;

    function inPara(blocks : TObjectList<TBlock>; canBeQuote : boolean) : boolean;
    function inQuote(blocks : TObjectList<TBlock>; out q : TQuoteBlock) : boolean;
    function isBlock(line : String) : boolean;

    function parseThematicBreak(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseCodeBlock(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseFencedCodeBlock(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseSeTextHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseQuoteBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    procedure parseInline(blocks : TObjectList<TBlock>; line : String);
    procedure check;
    procedure parse(block : TContainerBlock; processor : TBlockProcessor); overload;
  public
    Constructor Create;
    Destructor Destroy; override;
    class function parse(src : String) : TMarkdownDocument; overload;
  end;

  TCommonMarkRenderer = class (TObject)
  private
  public
    class function render(doc : TMarkdownDocument) : String;
  end;

implementation

{ string utility functions }

function allCharsSame(s : String) : boolean;
var
  i : integer;
begin
  result := true;
  for i := 2 to length(s) do
    if s[i] <> s[1] then
      exit(false);
end;

function stripFinalChars(s : String; chs : TSysCharSet) : String;
var
  i : integer;
begin
  i := length(s);
  while (i > 0) and CharInSet(s[i], chs) do
    dec(i);
  result := copy(s, 1, i);
end;

function copyTo(s : String; chs : TSysCharSet) : String;
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

function copyWhile(s : String; chs : TSysCharSet) : String;
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
function startsWithWS(s : String; c : char; out length : integer) : boolean; overload;
var
  i : integer;
begin
  if s = '' then
    exit(false);

  length := 1;
  for i := 1 to 3 do
  begin
    if (s.Length >= length) and (s[length] = ' ') then
      inc(length);
  end;
  result := s[length] = c;
end;

function startsWithWS(s : String; c : String; out length : integer) : boolean; overload;
var
  i : integer;
begin
  if s = '' then
    exit(false);

  length := 1;
  for i := 1 to 3 do
  begin
    if (s.Length >= length) and (s[length] = ' ') then
      inc(length);
  end;
  result := copy(s, length, c.Length) = c;
end;

{ TCommonMarkParser }

class function TCommonMarkParser.parse(src: String): TMarkdownDocument;
var
  this : TCommonMarkParser;
  doc : TDocumentProcessor;
begin
  this := TCommonMarkParser.Create;
  try
    this.FSource := src.Replace(#13#10, #10).replace(#13, #10);
    this.FCursor := 1;
    this.FLine := 1;
    this.FCol := 1;
    this.check;
    doc := TDocumentProcessor.Create(this);
    try
      result := TMarkdownDocument.Create;
      this.parse(result, doc);
    finally
      doc.Free;
    end;
  finally
    this.free;
  end;
end;

constructor TCommonMarkParser.Create;
begin
  inherited Create;
  FBuilder := TStringBuilder.Create;
end;

destructor TCommonMarkParser.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

function TCommonMarkParser.grab: char;
begin
  if FCursor > length(FSource) then
    raise Exception.Create('Read of end of markdown');
  result := peek;
  inc(FCursor);
  if result = #10 then
  begin
    inc(FLine);
    FCol := 1;
  end
  else if result = #9 then
    inc(FCol, 4)
  else
    Inc(FCol);
end;

procedure TCommonMarkParser.check;
begin
  // really, what can we check?
end;

function TCommonMarkParser.done: boolean;
begin
  result := FCursor > length(FSource);
end;

function TCommonMarkParser.escapeHtml(s: String): String;
var
  ch : char;
begin
  FBuilder.Clear;
  for ch in s do
    case ch of
      '>' : FBuilder.Append('&gt;');
      '<' : FBuilder.Append('&lt;');
    else
      FBuilder.Append(ch);
    end;
  result := FBuilder.ToString;

end;

function TCommonMarkParser.grab(length: integer): string;
var
  i : integer;
begin
  SetLength(result, length);
  for i := 1 to length do
    result[i] := grab;
end;

function TCommonMarkParser.grabTo(chs: TSysCharSet): string;
begin
  FBuilder.Clear;
  while not CharInSet(peek, chs) do
    FBuilder.Append(grab);
  result := FBuilder.ToString;
end;

function TCommonMarkParser.grabWhile(chs: TSysCharSet): string;
begin
  FBuilder.Clear;
  while CharInSet(peek, chs) do
    FBuilder.Append(grab);
  result := FBuilder.ToString;
end;

function TCommonMarkParser.grabWhile(ch: char): string;
begin
  FBuilder.Clear;
  while peek = ch do
    FBuilder.Append(grab);
  result := FBuilder.ToString;
end;

function TCommonMarkParser.grabTo(ch: char): string;
begin
  FBuilder.Clear;
  while peek <> ch do
    FBuilder.Append(grab);
  result := FBuilder.ToString;
end;

function TCommonMarkParser.grabLine: string;
begin
  result := grabTo(#10);
  grab; // read the eoln
end;

function TCommonMarkParser.has(chs: TSysCharSet): boolean;
begin
  result := CharInSet(peek, chs);
end;

function TCommonMarkParser.has(ch: char): boolean;
begin
  result := peek = ch;
end;

function TCommonMarkParser.has(s: String): boolean;
begin
  result := peekStr(length(s)) = s;
end;

function TCommonMarkParser.isWhitespace(ch: char): boolean;
begin
  result := CharInSet(ch, [#10, #9, ' ']);
end;

function TCommonMarkParser.inPara(blocks: TObjectList<TBlock>; canBeQuote : boolean): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TParagraphBlock) and not (blocks.Last as TParagraphBlock).closed and ((blocks.Last as TParagraphBlock).header = 0);
  if result and not canBeQuote and (blocks.Last is TQuoteBlock) then
    result := false;
end;

function TCommonMarkParser.inQuote(blocks: TObjectList<TBlock>; out q: TQuoteBlock): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TQuoteBlock) and not (blocks.Last as TQuoteBlock).closed;
  if result then
    q := blocks.Last as TQuoteBlock;
end;

function TCommonMarkParser.isBlock(line: String): boolean;
var
  l : integer;
begin
  if startsWithWS(line, '***', l) then
    exit(true);
  if startsWithWS(line, '---', l) then
    exit(true);
  if startsWithWS(line, '___', l) then
    exit(true);
  if startsWithWS(line, '#', l) then
    exit(true);
  if startsWithWS(line, '`', l) then
    exit(true);
  if startsWithWS(line, '~', l) then
    exit(true);
  if startsWithWS(line, '>', l) then
    exit(true);
  if line.startsWith('    ') then // code block
    exit(true);
  result := false;
end;

function TCommonMarkParser.isWhitespace(s: String): boolean;
var
  ch : char;
begin
  result := true;
  for ch in s do
    if not isWhitespace(ch) then
      exit(false);
end;

function TCommonMarkParser.peek: char;
begin
  if FCursor > length(FSource) then
    raise Exception.Create('Peek off end of markdown');
  result := FSource[FCursor];
end;

function TCommonMarkParser.peek(offset: integer): char;
begin
  if FCursor + offset > length(FSource) then
    raise Exception.Create('Peek off end of markdown');
  result := FSource[FCursor+offset];
end;

function TCommonMarkParser.peekline: String;
var
  i : integer;
begin
  FBuilder.Clear;
  i := 0;
  while peek(i) <> #10 do
  begin
    FBuilder.Append(peek(i));
    inc(i);
  end;
  result := FBuilder.ToString;
end;

function TCommonMarkParser.peekStr(length: integer): String;
begin
  result := copy(FSource, FCursor, length);
end;

function TCommonMarkParser.stripWhitespace(s: String): String;
var
  ch : Char;
begin
  FBuilder.Clear;
  for ch in s do
    if not isWhitespace(ch) then
      FBuilder.append(ch);
  result := FBuilder.ToString;
end;

function TCommonMarkParser.parseThematicBreak(blocks : TObjectList<TBlock>; line: String): boolean;
begin
  if line.StartsWith('    ') then
    exit(false);
  line := stripWhitespace(line);
  if (line.StartsWith('***') or line.StartsWith('---') or line.StartsWith('___')) and AllCharsSame(line) then
  begin
    grabLine;
    blocks.Add(TThematicBreakBlock.Create);
    result := true;
  end
  else
    result := false;
end;

function TCommonMarkParser.parseHeader(blocks : TObjectList<TBlock>; line: String): boolean;
var
  l : integer;
  s : String;
  b : THeadingBlock;
begin
  if line.StartsWith('    ') then
    exit(false);
  line := line.Trim;
  result := true;
  if line.StartsWith('# ') or (line = '#') then
    l := 1
  else if line.StartsWith('## ') or (line = '##') then
    l := 2
  else if line.StartsWith('### ') or (line = '###') then
    l := 3
  else if line.StartsWith('#### ') or (line = '####') then
    l := 4
  else if line.StartsWith('##### ') or (line = '#####') then
    l := 5
  else if line.StartsWith('###### ') or (line = '######') then
    l := 6
  else
    exit(False);
  grabWhile(' ');
  result := true;
  grab(l);
  if peek <> #10 then
    grab;
  b := THeadingBlock.Create(l);
  blocks.Add(b);
  s := grabLine.trim;
  if (s <> '') then
  begin
    l := length(s);
    while s[l] = '#' do
      dec(l);
    if (l = 0) then
      s := ''
    else if (s[l] = ' ') then
      s := copy(s, 1, l-1);
  end;
  parseInline(b.blocks, s);
end;

procedure TCommonMarkParser.parseInline(blocks : TObjectList<TBlock>; line : String);
begin
  blocks.Add(TTextBlock.create(escapeHtml(line.Trim)));
end;

function TCommonMarkParser.parseQuoteBlock(blocks: TObjectList<TBlock>; line: String; processor : TBlockProcessor): boolean;
var
  s : String;
  q : TQuoteBlock;
  qp : TQuoteProcessor;
begin
  if line.StartsWith('    ') then
    exit(false);
  s := line.Trim;
  if not s.StartsWith('>') then
    exit(false);
  q := TQuoteBlock.Create;
  blocks.add(q);
  result := true;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  qp := TQuoteProcessor.Create(processor, q);
  try
    parse(q, qp);
  finally
    qp.Free;
  end;
end;

function TCommonMarkParser.parseSeTextHeader(blocks : TObjectList<TBlock>; line: String): boolean;
var
  p : TParagraphBlock;
  s, n : String;
begin
  if line.StartsWith('    ') then
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
  s := line.Trim;
  if s = '' then
    exit(false);
  if not allCharsSame(s) then
    exit(false);

  if s[1] = '-' then
  begin
    grabLine;
    p.header := 2;
    exit(true);
  end
  else if s[1] = '=' then
  begin
    grabLine;
    p.header := 1;
    exit(true);
  end
  else
    exit(false);
end;

function TCommonMarkParser.parseCodeBlock(blocks : TObjectList<TBlock>; line: String): boolean;
var
  c : TCodeBlock;
  s : String;
  more : boolean;
begin
  if not line.StartsWith('    ') then
    exit(false);
  if isWhitespace(line) then
    exit(false);
  if inPara(blocks, true) then
    exit(false);
  result := true;
  c := TCodeBlock.Create;
  blocks.Add(c);
  more := true;
  while not done and more do
  begin
    s := grabLine;
    if s.Length > 4 then
      s := s.Substring(4)
    else
      s := '';
    c.FBlocks.Add(TTextBlock.Create(escapehtml(s)));
    if not done then
    begin
      s := peekLine;
      if s.Length <= 4 then
        more := isWhitespace(s)
      else
        more := s.StartsWith('    ');
    end;
  end;
  // remove any whitespace lines at the end:
  while (c.blocks.Count > 0) and isWhitespace((c.blocks.Last as TTextBlock).text) do
    c.blocks.Delete(c.blocks.Count - 1);
end;

function TCommonMarkParser.parseFencedCodeBlock(blocks: TObjectList<TBlock>; line: String): boolean;
var
  toEnd : String;
  function isEnded: boolean;
  var
    s : String;
  begin
    s := peekLine;
    if s.StartsWith('    ') then
      result := false
    else
    begin
      s := s.Trim;
      result := allCharsSame(s) and s.StartsWith(toEnd);
    end;
  end;
var
  c : TCodeBlock;
  s : String;
  indent, i : integer;
begin
  if line.StartsWith('    ') then
    exit(false);
  line := line.Trim;
  if not (line.StartsWith('`') or line.StartsWith('~')) then
    exit(false);
  s := copyWhile(line, [line[1]]);
  if not (s.StartsWith('```') or s.StartsWith('~~~')) or line.Substring(s.Length).Contains('~') then
    exit(false);
  toEnd := s;
  result := true;
  c := TCodeBlock.Create;
  blocks.Add(c);
  c.fenced := true;
  if line.Length > s.Length then
  begin
    s := line.Substring(s.Length).trim;
    c.lang := copyTo(s, [' ']);
  end;
  s := grabLine;
  indent := 0;
  while s[indent+1] = ' ' do
    inc(indent);

  while not done and not isEnded do
  begin
    s := grabLine;
    if indent > 0 then
    begin
      i := 0;
      while (i < s.Length) and (i < indent) and (s[i+1] = ' ') do
        inc(i);
      if i > 0 then
        s := s.Substring(i);
    end;
    c.FBlocks.Add(TTextBlock.Create(escapehtml(s)));
  end;
  if not done then
    grabLine;
end;

procedure TCommonMarkParser.parse(block: TContainerBlock; processor : TBlockProcessor);
var
  line : String;
  p : TParagraphBlock;
  b : TBlock;
  level : integer;
begin
  while not done do // must be at start of line here
  begin
    line := peekline;
    if processor.processLine(line, true) then
      exit; // ok, we're done with this set of blocks

    if parseSeTextHeader(block.blocks, line) then
    else if parseThematicBreak(block.blocks, line) then
    else if parseHeader(block.blocks, line) then
    else if parseCodeBlock(block.blocks, line) then
    else if parseFencedCodeBlock(block.blocks, line) then
    else if parseQuoteBlock(block.blocks, line, processor) then
    else
    begin
      if inPara(block.blocks, true) then
        p := block.Blocks.Last as TParagraphBlock
      else
        p := nil;
      if (line = '') or isWhitespace(line) then
      begin
        if (p <> nil) then
          p.closed := true;
        grabLine;
      end
      else
      begin
        if (p = nil) then
        begin
          p := TParagraphBlock.Create;
          block.blocks.Add(p);
        end;
        parseInLine(p.blocks, grabLine);
      end;
    end;
  end;
end;

{ TCommonMarkRenderer }

class function TCommonMarkRenderer.render(doc: TMarkdownDocument): String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    doc.render(b);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TContainerBlock }

constructor TContainerBlock.Create;
begin
  inherited create;
  FBlocks := TObjectList<TBLock>.create(true);
end;

destructor TContainerBlock.Destroy;
begin
  FBlocks.Free;
  inherited Destroy;
end;

procedure TContainerBlock.render(b: TStringBuilder);
var
  c : TBlock;
begin
  for c in FBlocks do
    c.render(b);
end;

{ TTextBlock }

constructor TTextBlock.Create(text: String);
begin
  inherited Create;
  FText := text;
end;

procedure TTextBlock.render(b: TStringBuilder);
begin
  b.Append(FText);
end;

{ THeadingBlock }

constructor THeadingBlock.Create(level: Integer);
begin
  Inherited Create;
  FLevel := level;
end;

procedure THeadingBlock.render(b: TStringBuilder);
begin
  b.Append('<h'+inttostr(FLevel)+'>');
  inherited render(b);
  b.Append('</h'+inttostr(FLevel)+'>');
  b.Append(#10);
end;

{ TParagraphBlock }

procedure TParagraphBlock.render(b: TStringBuilder);
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
    c.render(b);
  end;
  case header of
    0: b.Append('</p>');
    1: b.Append('</h1>');
    2: b.Append('</h2>');
  end;
  b.Append(#10);
end;

{ TThematicBreakBlock }

procedure TThematicBreakBlock.render(b: TStringBuilder);
begin
  b.Append('<hr />');
  b.Append(#10);
end;

{ TCodeBlock }

procedure TCodeBlock.render(b: TStringBuilder);
var
  c : TBlock;
begin
  if lang <> '' then
    b.Append('<pre><code class="language-'+lang+'">')
  else
    b.Append('<pre><code>');
  for c in FBlocks do
  begin
    c.render(b);
    b.Append(#10);
  end;
  b.Append('</code></pre>');
  b.Append(#10);
end;

{ TQuoteProcessor }

constructor TQuoteProcessor.Create(processor : TBlockProcessor; q: TQuoteBlock);
begin
  inherited Create(processor);
  quote := q;
end;

function TQuoteProcessor.processLine(var line: string; root : boolean): boolean;
var
  l : integer;
begin
  result := FParent.processLine(line, false);
  if not result then
  begin
    if startsWithWS(line, '>', l) then
    begin
      line := line.subString(l);
      parser.grab(l);
    end
    else if root then // only can do that else we walk into descendents
    begin
      // decide whether the block is a continuation, or the end.
      // it's the end if it's empty, or it starts a new kind of block
      // plain text isn't the end of the block (lazy :-( )
      if parser.isWhitespace(line) then
        result := true
      else if parser.isBlock(line) then
        result := true
    end;
  end
end;

{ TBlockProcessor }

constructor TBlockProcessor.Create(processor: TBlockProcessor);
begin
  inherited Create;
  FParent := processor;
end;

function TBlockProcessor.parser: TCommonMarkParser;
begin
  result := FParent.parser;
end;

{ TDocumentProcessor }

constructor TDocumentProcessor.Create(parser : TCommonMarkParser);
begin
  inherited Create(nil);
  FParser := parser;
end;

function TDocumentProcessor.parser: TCommonMarkParser;
begin
  result := FParser;
end;

function TDocumentProcessor.processLine(var line: string; root : boolean): boolean;
begin
  result := false; // document is only ended by end of source
end;

{ TQuoteBlock }

procedure TQuoteBlock.render(b: TStringBuilder);
var
  c : TBlock;
begin
  b.Append('<blockquote>'#10);
  for c in FBlocks do
    c.render(b);
  b.Append('</blockquote>'#10);
end;

end.
