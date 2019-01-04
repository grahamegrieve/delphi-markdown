Unit MarkdownCommonMark;

{
currently working:
- headings
- code blocks (+fenced)
- thematic blocks
- quote blocks in progress

still to do
- lists
- in line styling

not planned to be supported
- HTML blocks
}
interface

uses
  SysUtils, Math, Generics.Collections;



// Abstract Syntax Tree
type
  TBlock = class abstract (TObject)
  private
    FClosed: boolean;
    FLine : integer;
  protected
    procedure render(b : TStringBuilder); virtual; abstract;
  public
    constructor Create(line : Integer);
    property closed : boolean read FClosed write FClosed;
    property line : Integer read FLine;
  end;

  TContainerBlock = class abstract (TBlock)
  private
    FBlocks: TObjectList<TBLock>;
  protected
    procedure render(b : TStringBuilder); override;
  public
    constructor Create(line : Integer);
    destructor Destroy; override;

    property blocks : TObjectList<TBLock> read FBlocks;
  end;

  TMarkdownDocument = class (TContainerBlock);
//  TListItemBlock = class (TContainerBlock);
//  TListBlock = class (TContainerBlock);
//  TLinkReferenceDefinitionGroup = class (TContainerBlock);

  TParagraphBlock = class (TContainerBlock)
  private
    FHeader: integer;
  protected
    procedure render(b : TStringBuilder); override;
  public
    property header : integer read FHeader write FHeader;
  end;

  TQuoteBlock = class (TParagraphBlock)
  protected
    procedure render(b : TStringBuilder); override;
  end;

  TListBlock = class (TContainerBlock)
  private
    FOrdered: boolean;
    FIndent : integer;
    FStart: String;
  protected
    procedure render(b : TStringBuilder); override;
  public
    property ordered : boolean read FOrdered write FOrdered;
    property indent : integer read FIndent write FIndent;
    property start : String read FStart write FStart;
  end;


  TListItemBlock = class (TParagraphBlock)
  protected
    procedure render(b : TStringBuilder); override;
  end;

  THeadingBlock = class (TContainerBlock)
  private
    FLevel: integer;
  protected
    procedure render(b : TStringBuilder); override;
  public
    constructor Create(line, level : Integer);
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
    constructor Create(list : integer; text : String);
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

  TListProcessor = class (TBlockProcessor)
  private
    list : TListBlock;
    item : TListItemBlock;
    FHasContent : boolean;
    FEmptyLineCount : integer;
  protected
    function processLine(var line : string; root : boolean) : boolean; override;
  public
    constructor Create(processor : TBlockProcessor; l : TListBlock; li : TListItemBlock);
  end;

  TCommonMarkParser = class (TObject)
  private
    FSource : String;
    FCursor : integer;
    FlastLineCursor : Integer;
    FLine : integer;
    FBuilder : TStringBuilder;

    // line operations
    function grabLine : String;
    function peekLine : String;
    procedure redoLine;
    function done : boolean;

    // string operations
    function allCharsSame(s : String) : boolean;
    function stripFinalChars(s : String; chs : TSysCharSet) : String;
    function copyTo(s : String; chs : TSysCharSet) : String;
    function after(s : String; chs : TSysCharSet) : String;
    function copyWhile(s : String; chs : TSysCharSet) : String;
    function startsWithWS(s : String; c : char; out length : integer) : boolean; overload;
    function startsWithWS(s : String; c : String; out length : integer) : boolean; overload;
    function countWS(s : String) : integer;
    function isWhitespace(ch : char) : boolean; overload;
    function isWhitespace(s : String) : boolean; overload;
    function stripWhitespace(s : String) : String;
    function escapeHtml(s : String) : String;

    // status
    function inPara(blocks : TObjectList<TBlock>; canBeQuote : boolean) : boolean;
    function inQuote(blocks : TObjectList<TBlock>; out q : TQuoteBlock) : boolean;
    function inList(blocks : TObjectList<TBlock>; ordered : boolean; indent : integer; out l : TListBlock) : boolean;
    function isBlock(blocks : TObjectList<TBlock>; line : String) : boolean;

    function parseThematicBreak(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseCodeBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    function parseFencedCodeBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    function parseSeTextHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseQuoteBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    function parseUListBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    function parseOListBlock(blocks : TObjectList<TBlock>; line : String; processor : TBlockProcessor) : boolean;
    procedure parseInline(blocks : TObjectList<TBlock>; line : String);
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
    this.FLine := 0;
    doc := TDocumentProcessor.Create(this);
    try
      result := TMarkdownDocument.Create(1);
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

function TCommonMarkParser.grabLine: String;
begin
  FlastLineCursor := FCursor;
  while (FCursor <= FSource.Length) and (FSource[FCursor] <> #10) do
    inc(FCursor);
  result := copy(FSource, FlastLineCursor, FCursor - FlastLineCursor);
  inc(FLine);
  if (FCursor <= FSource.Length) then
    inc(FCursor);
end;

function TCommonMarkParser.peekLine: String;
var
  i : integer;
begin
  i := FCursor;
  while (i <= FSource.Length) and (FSource[i] <> #10) do
    inc(i);
  result := copy(FSource, FCursor, i - FCursor);
end;

procedure TCommonMarkParser.redoLine;
begin
  dec(FLine);
  FCursor := FlastLineCursor;
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

{ string utility functions }

function TCommonMarkParser.after(s: String; chs: TSysCharSet): String;
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

function TCommonMarkParser.allCharsSame(s : String) : boolean;
var
  i : integer;
begin
  result := true;
  for i := 2 to length(s) do
    if s[i] <> s[1] then
      exit(false);
end;

function TCommonMarkParser.stripFinalChars(s : String; chs : TSysCharSet) : String;
var
  i : integer;
begin
  i := length(s);
  while (i > 0) and CharInSet(s[i], chs) do
    dec(i);
  result := copy(s, 1, i);
end;

function TCommonMarkParser.copyTo(s : String; chs : TSysCharSet) : String;
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

function TCommonMarkParser.copyWhile(s : String; chs : TSysCharSet) : String;
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
function TCommonMarkParser.startsWithWS(s : String; c : char; out length : integer) : boolean;
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

function TCommonMarkParser.startsWithWS(s : String; c : String; out length : integer) : boolean;
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

function TCommonMarkParser.countWS(s : String) : integer;
begin
  result := 0;
  while (result < s.Length) and (s[result+1] = ' ') do
    inc(result);
end;


function TCommonMarkParser.isWhitespace(ch: char): boolean;
begin
  result := CharInSet(ch, [#10, #9, ' ']);
end;

function TCommonMarkParser.inList(blocks: TObjectList<TBlock>; ordered: boolean; indent : integer; out l: TListBlock): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TListBlock) and not (blocks.Last as TListBlock).closed and ((blocks.Last as TListBlock).ordered = ordered)and ((blocks.Last as TListBlock).indent <= indent);
  if result then
    l := blocks.Last as TListBlock;
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

function TCommonMarkParser.isBlock(blocks : TObjectList<TBlock>; line: String): boolean;
var
  l : integer;
  b : TBlock;
begin
  if startsWithWS(line, '*', l) then
    exit(true);
  if startsWithWS(line, '-', l) then
    exit(true);
  if startsWithWS(line, '+', l) then
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
  if line.startsWith('    ') and not InPara(blocks, false) then // code block
    exit(true);
  if blocks.count = 0 then
    exit(true);
  b := blocks.last;
  if not (b is TParagraphBlock) then
    exit(true);
  result := b.closed;
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
    blocks.Add(TThematicBreakBlock.Create(Fline));
    result := true;
  end
  else
    result := false;
end;

function TCommonMarkParser.parseUListBlock(blocks: TObjectList<TBlock>; line: String; processor: TBlockProcessor): boolean;
var
  l : TListBlock;
  li : TListItemBlock;
  lp : TListProcessor;
  i, i2 : integer;
  s : String;
begin
  if line = '' then
    exit(false);
  if line.StartsWith('    ') then
    exit(false);
  i := countWS(line);
  if not CharInSet(line[1+i], ['+', '-', '*']) then
    exit(false);
  s := line.Substring(1+i);
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
  if not inList(blocks, false, i+i2+1, l) then
  begin
    l := TListBlock.Create(FLine);
    blocks.add(l);
    l.ordered := false;
    l.indent := i+i2+1;
  end;
  li := TListItemBlock.Create(FLine);
  l.blocks.Add(li);
  result := true;
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TListProcessor.Create(processor, l, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
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
  result := true;
  b := THeadingBlock.Create(FLine, l);
  blocks.Add(b);
  s := line.substring(l).trim;
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
  blocks.Add(TTextBlock.create(FLine, escapeHtml(line.Trim)));
end;

function TCommonMarkParser.parseOListBlock(blocks: TObjectList<TBlock>; line: String; processor: TBlockProcessor): boolean;
var
  l : TListBlock;
  li : TListItemBlock;
  lp : TListProcessor;
  i, i2 : integer;
  s,sl : String;
begin
  if line = '' then
    exit(false);
  if line.StartsWith('    ') then
    exit(false);
  i := countWS(line);
  s := copyWhile(line.Substring(i), ['0'..'9']);
  if (s = '') or (length(s) > 9) then
    exit(false);
  if i + s.Length > line.Length then
    exit(false);
  if not CharInSet(line[i+s.Length+1], ['.', ')']) then
    exit(false);
  i := i+s.Length+1;
  sl := line.Substring(i);
  if isWhitespace(sl) and inPara(blocks, false) then
    exit(false);
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
//  i2 := countWS(line.Substring(i));
//  if i2 = 0 then
//    exit(false);
//  if i2 >= 5 then
//    i2 := 1;
  if not inList(blocks, true, i+i2, l) then
  begin
    l := TListBlock.Create(FLine);
    blocks.add(l);
    l.ordered := true;
    l.indent := i+i2;
    s := after(s, ['0']);
    if s = '' then
      s := '0';
    l.start := s;
  end;
  li := TListItemBlock.Create(FLine);
  l.blocks.Add(li);
  result := true;
  redoLine;
  // now, instead of grabbing the >, we recurse and get the processor to do that
  lp := TListProcessor.Create(processor, l, li);
  try
    parse(li, lp);
  finally
    lp.Free;
  end;
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
  q := TQuoteBlock.Create(FLine);
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

function TCommonMarkParser.parseCodeBlock(blocks : TObjectList<TBlock>; line: String; processor : TBlockProcessor): boolean;
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
  c := TCodeBlock.Create(FLine);
  blocks.Add(c);
  s := line.subString(4);
  repeat
    c.FBlocks.Add(TTextBlock.Create(FLine, escapehtml(s)));
    if done then
      more := false
    else
    begin
      s := peekLine;
      if processor.processLine(s, true) then
        break;
      if s.Length <= 4 then
        more := isWhitespace(s)
      else
        more := s.StartsWith('    ');
      if more then
      begin
        s := grabLine;
        processor.processLine(s, true);
        s := s.subString(4);
      end;
    end;
  until not more;
  // remove any whitespace lines at the end:
  while (c.blocks.Count > 0) and isWhitespace((c.blocks.Last as TTextBlock).text) do
    c.blocks.Delete(c.blocks.Count - 1);
end;

function TCommonMarkParser.parseFencedCodeBlock(blocks: TObjectList<TBlock>; line: String; processor : TBlockProcessor): boolean;
var
  toEnd : String;
  function isEnded: boolean;
  var
    s : String;
  begin
    s := peekLine;
    processor.processLine(s, true);
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
  s, lang : String;
  indent, i : integer;
begin
  if line.StartsWith('    ') then
    exit(false);
  s := line.Trim;
  if not (s.StartsWith('`') or s.StartsWith('~')) then
    exit(false);
  s := copyWhile(s, [s[1]]);
  if not (s.StartsWith('```') or s.StartsWith('~~~')) or s.Substring(s.Length).Contains('~') then
    exit(false);
  toEnd := s;
  result := true;
  // now, try to find anything off the end of the fence
  s := line;
  indent := 0;
  while s[indent+1] = ' ' do
    inc(indent);

  s := s.subString(indent);
  s := after(s, [toEnd[1]]).trim;
  if (s <> '') then
  begin
    lang := copyTo(s, [' ']);
    if lang.contains(toEnd[1]) then
      exit(false);
  end;

  c := TCodeBlock.Create(FLine);
  blocks.Add(c);
  c.fenced := true;
  c.lang := lang;

  while not done and not isEnded do
  begin
    s := grabLine;
    processor.processLine(s, true); // cannot return true?
    if indent > 0 then
    begin
      i := 0;
      while (i < s.Length) and (i < indent) and (s[i+1] = ' ') do
        inc(i);
      if i > 0 then
        s := s.Substring(i);
    end;
    c.FBlocks.Add(TTextBlock.Create(FLine, escapehtml(s)));
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
    line := grabLine;
    if processor.processLine(line, true) then
    begin
    // ok, we're done with this set of blocks, we'll try the line again one further up
      redoLine;
      exit;
    end;

    if parseSeTextHeader(block.blocks, line) then
    else if parseThematicBreak(block.blocks, line) then
    else if parseHeader(block.blocks, line) then
    else if parseCodeBlock(block.blocks, line, processor) then
    else if parseFencedCodeBlock(block.blocks, line, processor) then
    else if parseQuoteBlock(block.blocks, line, processor) then
    else if parseUListBlock(block.blocks, line, processor) then
    else if parseOListBlock(block.blocks, line, processor) then
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
      end
      else
      begin
        if (p = nil) then
        begin
          p := TParagraphBlock.Create(FLine);
          block.blocks.Add(p);
        end;
        parseInLine(p.blocks, line);
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
  inherited create(line);
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

constructor TTextBlock.Create(list : integer; text: String);
begin
  inherited Create(line);
  FText := text;
end;

procedure TTextBlock.render(b: TStringBuilder);
begin
  b.Append(FText);
end;

{ THeadingBlock }

constructor THeadingBlock.Create(line, level: Integer);
begin
  Inherited Create(line);
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
    if parser.startsWithWS(line, '>', l) then
    begin
      line := line.subString(l);
      l := parser.countWS(line);
      if l <= 3 then
        line := line.subString(l)
      else // well, one of the spaces belongs to the >
        line := line.subString(1);
    end
    else if root then // only can do that else we walk into descendents
    begin
      // decide whether the block is a continuation, or the end.
      // it's the end if it's empty, or it starts a new kind of block
      // plain text isn't the end of the block (lazy :-( )
      if parser.isWhitespace(line) then
        result := true
      else if parser.isBlock(quote.FBlocks, line) then
        result := true
      else if not parser.inPara(quote.blocks, false) then
        result := true;
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

{ TListBlock }

procedure TListBlock.render(b: TStringBuilder);
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
    c.render(b);
  if ordered then
    b.Append('</ol>'#10)
  else
    b.Append('</ul>'#10);
end;

{ TListItemBlock }

procedure TListItemBlock.render(b: TStringBuilder);
var
  c : TBlock;
  first : boolean;
begin
  if Blocks.Count = 0 then
    b.Append('<li>')
  else if (Blocks.Count = 1) and (Blocks[0] is TParagraphBlock) then
  begin
    b.Append('<li>');
    first := true;
    for c in (Blocks[0] as TParagraphBlock).Blocks do
    begin
      if first then
        first := false
      else
        b.Append(#10);
      c.render(b);
    end;

  end
  else
  begin
    b.Append('<li>'#10);
    for c in FBlocks do
      c.render(b);
  end;
  b.Append('</li>'#10);
end;

{ TListProcessor }

constructor TListProcessor.Create(processor: TBlockProcessor; l : TListBLock; li: TListItemBlock);
begin
  inherited Create(processor);
  list := l;
  item := li;
end;

function TListProcessor.processLine(var line: string; root: boolean): boolean;
var
  l : integer;
begin
  result := FParent.processLine(line, false);
  if not result then
  begin
    if parser.FLine = item.line then
      line := line.subString(list.indent)
    else if parser.countWS(line) >= list.indent then
      line := line.subString(list.indent)
    else if not parser.isWhitespace(line) and parser.isBlock(item.blocks, line) then
    begin
      result := true;
    end;

    if not parser.isWhiteSpace(line) then
      FHasContent := true
    else
    begin
      inc(FEmptyLineCount);
      if not FHasContent and (FEmptyLineCount > 1) then
        result := true;
    end;
  end
end;

{ TBlock }

constructor TBlock.Create(line: Integer);
begin
  Inherited Create;
  FLine := line;
end;

end.
