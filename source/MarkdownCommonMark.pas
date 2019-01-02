Unit MarkdownCommonMark;

interface

uses
  SysUtils, Generics.Collections;


// Abstract Syntax Tree
type
  TMarkdownObject = class abstract (TObject);
  TBlock = class abstract (TMarkdownObject)
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
//  TQuoteBlock = class (TContainerBlock);
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
  protected
    procedure render(b : TStringBuilder); override;
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


// Parser

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

    function inPara(blocks : TObjectList<TBlock>) : boolean;

    function parseThematicBreak(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseCodeBlock(blocks : TObjectList<TBlock>; line : String) : boolean;
    function parseSeTextHeader(blocks : TObjectList<TBlock>; line : String) : boolean;
    procedure parseInline(blocks : TObjectList<TBlock>; line : String);
    procedure check;
    procedure parse(block : TContainerBlock); overload;
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

{ TCommonMarkParser }

class function TCommonMarkParser.parse(src: String): TMarkdownDocument;
var
  this : TCommonMarkParser;
begin
  this := TCommonMarkParser.Create;
  try
    this.FSource := src.Replace(#13#10, #10).replace(#13, #10);
    this.FCursor := 1;
    this.FLine := 1;
    this.FCol := 1;
    this.check;
    result := TMarkdownDocument.Create;
    this.parse(result);
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

function TCommonMarkParser.inPara(blocks: TObjectList<TBlock>): boolean;
begin
  result := (blocks.Count > 0) and (blocks.Last is TParagraphBlock) and not (blocks.Last as TParagraphBlock).closed and ((blocks.Last as TParagraphBlock).header = 0);
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

function TCommonMarkParser.parseSeTextHeader(blocks : TObjectList<TBlock>; line: String): boolean;
var
  p : TParagraphBlock;
  s, n : String;
begin
  if line.StartsWith('    ') then
    exit(false);
  if blocks.Count = 0 then
    exit(false);
  if not (blocks.Last is TParagraphBlock) then
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
  if inPara(blocks) then
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

procedure TCommonMarkParser.parse(block: TContainerBlock);
var
  line : String;
  p : TParagraphBlock;
  b : TBlock;
  level : integer;
begin
  while not done do // must be at start of line here
  begin
    line := peekline;
    if parseSeTextHeader(block.blocks, line) then
    else if parseThematicBreak(block.blocks, line) then
    else if parseHeader(block.blocks, line) then
    else if parseCodeBlock(block.blocks, line) then
    else
    begin
      if inPara(block.blocks) then
        p := block.Blocks.Last as TParagraphBlock
      else
        p := nil;
      if line = '' then
      begin
        if (p <> nil) then
          p.closed := true;
        grabLine;
      end
      else if (p = nil) and isWhitespace(line) then
        grabLine
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
  b.Append('<pre><code>');
  for c in FBlocks do
  begin
    c.render(b);
    b.Append(#10);
  end;
  b.Append('</code></pre>');
  b.Append(#10);
end;

end.
