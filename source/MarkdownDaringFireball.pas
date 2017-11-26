{
  This code was translated from TxtMark (https://github.com/rjeschke/txtmark)

  Copyright (C) 2011-2015 René Jeschke <rene_jeschke@yahoo.de>
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

Unit MarkdownDaringFireball;

interface

uses
  SysUtils, StrUtils, Classes, Character, TypInfo, Math,
  MarkdownProcessor;

type
  THTMLElement = (heNONE, hea, heabbr, heacronym, headdress, heapplet, hearea, heb, hebase, hebasefont, hebdo, hebig, heblockquote, hebody, hebr, hebutton, hecaption, hecite,
    hecode, hecol, hecolgroup, hedd, hedel, hedfn, hediv, hedl, hedt, heem, hefieldset, hefont, heform, heframe, heframeset, heh1, heh2, heh3, heh4, heh5, heh6, hehead, hehr,
    hehtml, hei, heiframe, heimg, heinput, heins, hekbd, helabel, helegend, heli, helink, hemap, hemeta, henoscript, heobject, heol, heoptgroup, heoption, hep, heparam, hepre, heq,
    hes, hesamp, hescript, heselect, hesmall, hespan, hestrike, hestrong, hestyle, hesub, hesup, hetable, hetbody, hetd, hetextarea, hetfoot, heth, hethead, hetitle, hetr, hett,
    heu, heul, hevar);

const
  // pstfix
  ENTITY_NAMES: array[0..249] of String = ('&Acirc;', '&acirc;', '&acute;', '&AElig;', '&aelig;', '&Agrave;', '&agrave;', '&alefsym;', '&Alpha;', '&alpha;', '&amp;', '&and;', '&ang;',
    '&apos;', '&Aring;', '&aring;', '&asymp;', '&Atilde;', '&atilde;', '&Auml;', '&auml;', '&bdquo;', '&Beta;', '&beta;', '&brvbar;', '&bull;', '&cap;', '&Ccedil;', '&ccedil;',
    '&cedil;', '&cent;', '&Chi;', '&chi;', '&circ;', '&clubs;', '&cong;', '&copy;', '&crarr;', '&cup;', '&curren;', '&Dagger;', '&dagger;', '&dArr;', '&darr;', '&deg;', '&Delta;',
    '&delta;', '&diams;', '&divide;', '&Eacute;', '&eacute;', '&Ecirc;', '&ecirc;', '&Egrave;', '&egrave;', '&empty;', '&emsp;', '&ensp;', '&Epsilon;', '&epsilon;', '&equiv;',
    '&Eta;', '&eta;', '&ETH;', '&eth;', '&Euml;', '&euml;', '&euro;', '&exist;', '&fnof;', '&forall;', '&frac12;', '&frac14;', '&frac34;', '&frasl;', '&Gamma;', '&gamma;', '&ge;',
    '&gt;', '&hArr;', '&harr;', '&hearts;', '&hellip;', '&Iacute;', '&iacute;', '&Icirc;', '&icirc;', '&iexcl;', '&Igrave;', '&igrave;', '&image;', '&infin;', '&int;', '&Iota;',
    '&iota;', '&iquest;', '&isin;', '&Iuml;', '&iuml;', '&Kappa;', '&kappa;', '&Lambda;', '&lambda;', '&lang;', '&laquo;', '&lArr;', '&larr;', '&lceil;', '&ldquo;', '&le;',
    '&lfloor;', '&lowast;', '&loz;', '&lrm;', '&lsaquo;', '&lsquo;', '&lt;', '&macr;', '&mdash;', '&micro;', '&middot;', '&minus;', '&Mu;', '&mu;', '&nabla;', '&nbsp;', '&ndash;',
    '&ne;', '&ni;', '&not;', '&notin;', '&nsub;', '&Ntilde;', '&ntilde;', '&Nu;', '&nu;', '&Oacute;', '&oacute;', '&Ocirc;', '&ocirc;', '&OElig;', '&oelig;', '&Ograve;',
    '&ograve;', '&oline;', '&Omega;', '&omega;', '&Omicron;', '&omicron;', '&oplus;', '&or;', '&ordf;', '&ordm;', '&Oslash;', '&oslash;', '&Otilde;', '&otilde;', '&otimes;',
    '&Ouml;', '&ouml;', '&para;', '&part;', '&permil;', '&perp;', '&Phi;', '&phi;', '&Pi;', '&pi;', '&piv;', '&plusmn;', '&pound;', '&Prime;', '&prime;', '&prod;', '&prop;',
    '&Psi;', '&psi;', '&quot;', '&radic;', '&rang;', '&raquo;', '&rArr;', '&rarr;', '&rceil;', '&rdquo;', '&real;', '&reg;', '&rfloor;', '&Rho;', '&rho;', '&rlm;', '&rsaquo;',
    '&rsquo;', '&sbquo;', '&Scaron;', '&scaron;', '&sdot;', '&sect;', '&shy;', '&Sigma;', '&sigma;', '&sigmaf;', '&sim;', '&spades;', '&sub;', '&sube;', '&sum;', '&sup;', '&sup1;',
    '&sup2;', '&sup3;', '&supe;', '&szlig;', '&Tau;', '&tau;', '&there4;', '&Theta;', '&theta;', '&thetasym;', '&thinsp;', '&thorn;', '&tilde;', '&times;', '&trade;', '&Uacute;',
    '&uacute;', '&uArr;', '&uarr;', '&Ucirc;', '&ucirc;', '&Ugrave;', '&ugrave;', '&uml;', '&upsih;', '&Upsilon;', '&upsilon;', '&Uuml;', '&uuml;', '&weierp;', '&Xi;', '&xi;',
    '&Yacute;', '&yacute;', '&yen;', '&Yuml;', '&yuml;', '&Zeta;', '&zeta;', '&zwj;', '&zwnj;');

  // Characters corresponding to ENTITY_NAMES. */
  // pstfix
  ENTITY_CHARS: array[0..249] of integer = ($00C2, $00E2, $00B4, $00C6, $00E6, $00C0, $00E0, $2135, $0391, $03B1, $0026, $2227, $2220, ord(''''), $00C5, $00E5, $2248, $00C3, $00E3, $00C4,
    $00E4, $201E, $0392, $03B2, $00A6, $2022, $2229, $00C7, $00E7, $00B8, $00A2, $03A7, $03C7, $02C6, $2663, $2245, $00A9, $21B5, $222A, $00A4, $2021, $2020, $21D3, $2193, $00B0,
    $0394, $03B4, $2666, $00F7, $00C9, $00E9, $00CA, $00EA, $00C8, $00E8, $2205, $2003, $2002, $0395, $03B5, $2261, $0397, $03B7, $00D0, $00F0, $00CB, $00EB, $20AC, $2203, $0192,
    $2200, $00BD, $00BC, $00BE, $2044, $0393, $03B3, $2265, $003E, $21D4, $2194, $2665, $2026, $00CD, $00ED, $00CE, $00EE, $00A1, $00CC, $00EC, $2111, $221E, $222B, $0399, $03B9,
    $00BF, $2208, $00CF, $00EF, $039A, $03BA, $039B, $03BB, $2329, $00AB, $21D0, $2190, $2308, $201C, $2264, $230A, $2217, $25CA, $200E, $2039, $2018, $003C, $00AF, $2014, $00B5,
    $00B7, $2212, $039C, $03BC, $2207, $00A0, $2013, $2260, $220B, $00AC, $2209, $2284, $00D1, $00F1, $039D, $03BD, $00D3, $00F3, $00D4, $00F4, $0152, $0153, $00D2, $00F2, $203E,
    $03A9, $03C9, $039F, $03BF, $2295, $2228, $00AA, $00BA, $00D8, $00F8, $00D5, $00F5, $2297, $00D6, $00F6, $00B6, $2202, $2030, $22A5, $03A6, $03C6, $03A0, $03C0, $03D6, $00B1,
    $00A3, $2033, $2032, $220F, $221D, $03A8, $03C8, $0022, $221A, $232A, $00BB, $21D2, $2192, $2309, $201D, $211C, $00AE, $230B, $03A1, $03C1, $200F, $203A, $2019, $201A, $0160,
    $0161, $22C5, $00A7, $00AD, $03A3, $03C3, $03C2, $223C, $2660, $2282, $2286, $2211, $2283, $00B9, $00B2, $00B3, $2287, $00DF, $03A4, $03C4, $2234, $0398, $03B8, $03D1, $00DE,
    $00FE, $02DC, $00D7, $2122, $00DA, $00FA, $21D1, $2191, $00DB, $00FB, $00D9, $00F9, $00A8, $03D2, $03A5, $03C5, $00DC, $00FC, $2118, $039E, $03BE, $00DD, $00FD, $00A5, $0178,
    $00FF, $0396, $03B6, $200D, $200C);

  LINK_PREFIXES: array[0..3] of String = ('http', 'https', 'ftp', 'ftps');

  BLOCK_ELEMENTS: set of THTMLElement = [headdress, heblockquote, hedel, hediv, hedl, hefieldset, heform, heh1, heh2, heh3, heh4, heh5, heh6, hehr, heins, henoscript, heol, hep,
    hepre, hetable, heul];

  UNSAFE_ELEMENTS: set of THTMLElement = [heapplet, hehead, hehtml, hebody, heframe, heframeset, heiframe, hescript, heobject];

  BUFFER_INCREMENT_SIZE = 1024;

Type
  TReader = class
  private
    FValue: String;
    FCursor: integer;
  public
    Constructor Create(source: String);
    function read: char;
  end;

{$IFDEF FPC}
  TStringBuilder = class
  private
    FContent : String;
    FLength : Integer;
    FBufferSize : integer;
    function GetChar(index: integer): char;
  public
    Constructor Create;

    procedure Clear;
    procedure Append(value : String); overload;
    procedure Append(value : integer); overload;
    procedure Append(value : TStringBuilder); overload;
    property ch[index : integer] : char read GetChar; default;
    function toString : String; override;
    Property Length : Integer Read FLength;
  end;
{$ENDIF}

  TUtils = class
  public
    // Skips spaces in the given String. return The new position or -1 if EOL has been reached.
    class function skipSpaces(s: String; start: integer): integer;

    // Process the given escape sequence. return The new position.
    class function escape(out_: TStringBuilder; ch: char; position: integer): integer;

    // Reads characters until any 'end' character is encountered. return The new position or -1 if no 'end' char was found.
    class function readUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer; overload;

    // Reads characters until the 'end' character is encountered. return The new position or -1 if no 'end' char was found.
    class function readUntil(out_: TStringBuilder; s: String; start: integer; cend: char): integer; overload;

    // Reads a markdown link. return The new position or -1 if this is no valid markdown link.
    class function readMdLink(out_: TStringBuilder; s: String; start: integer): integer;
    class function readMdLinkId(out_: TStringBuilder; s: String; start: integer): integer;

    // Reads characters until any 'end' character is encountered, ignoring escape sequences.
    class function readRawUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer; overload;

    // Reads characters until the end character is encountered, taking care of HTML/XML strings.
    class function readRawUntil(out_: TStringBuilder; s: String; start: integer; cend: char): integer; overload;

    // Reads characters until any 'end' character is encountered, ignoring escape sequences.
    class function readXMLUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer;

    // Appends the given string encoding special HTML characters.
    class procedure appendCode(out_: TStringBuilder; s: String; start: integer; e: integer);

    // Appends the given string encoding special HTML characters (used in HTML
    class procedure appendValue(out_: TStringBuilder; s: String; start: integer; e: integer);

    // Append the given char as a decimal HTML entity.
    class procedure appendDecEntity(out_: TStringBuilder; value: char);

    // Append the given char as a hexadecimal HTML entity.
    class procedure appendHexEntity(out_: TStringBuilder; value: char);

    // Appends the given mailto link using obfuscation.
    class procedure appendMailto(out_: TStringBuilder; s: String; start: integer; e: integer);

    // Extracts the tag from an XML element.
    class procedure getXMLTag(out_: TStringBuilder; bin: TStringBuilder); overload;

    // Extracts the tag from an XML element.
    class procedure getXMLTag(out_: TStringBuilder; s: String); overload;

    // Reads an XML element.
    // return The new position or -1 if this is no valid XML element.
    class function readXML(out_: TStringBuilder; s: String; start: integer; safeMode: boolean): integer;

    // Appends the given string to the given StringBuilder, replacing '&amp;', '&lt;' and '&gt;' by their respective HTML entities.
    class procedure codeEncode(out_: TStringBuilder; value: String; offset: integer);

    // Removes trailing <code>`</code> or <code>~</code> and trims spaces.
    class function getMetaFromFence(fenceLine: String): String;
  end;

  THTML = class
  public
    class function isLinkPrefix(s: String): boolean;
    class function isEntity(s: String): boolean;
    class function isUnsafeHtmlElement(s: String): boolean;
    class function isHtmlBlockElement(s: String): boolean;
  end;

  TDecorator = class
  private
  public
    procedure openParagraph(out_: TStringBuilder); virtual;
    procedure closeParagraph(out_: TStringBuilder); virtual;

    procedure openBlockQuote(out_: TStringBuilder); virtual;
    procedure closeBlockQuote(out_: TStringBuilder); virtual;

    procedure openCodeBlock(out_: TStringBuilder); virtual;
    procedure closeCodeBlock(out_: TStringBuilder); virtual;

    procedure openCodeSpan(out_: TStringBuilder); virtual;
    procedure closeCodeSpan(out_: TStringBuilder); virtual;

    procedure openHeadline(out_: TStringBuilder; level: integer); virtual;
    procedure closeHeadline(out_: TStringBuilder; level: integer); virtual;

    procedure openStrong(out_: TStringBuilder); virtual;
    procedure closeStrong(out_: TStringBuilder); virtual;

    procedure openEmphasis(out_: TStringBuilder); virtual;
    procedure closeEmphasis(out_: TStringBuilder); virtual;

    procedure openSuper(out_: TStringBuilder); virtual;
    procedure closeSuper(out_: TStringBuilder); virtual;

    procedure openOrderedList(out_: TStringBuilder); virtual;
    procedure closeOrderedList(out_: TStringBuilder); virtual;

    procedure openUnOrderedList(out_: TStringBuilder); virtual;
    procedure closeUnOrderedList(out_: TStringBuilder); virtual;

    procedure openListItem(out_: TStringBuilder); virtual;
    procedure closeListItem(out_: TStringBuilder); virtual;

    procedure horizontalRuler(out_: TStringBuilder); virtual;

    procedure openLink(out_: TStringBuilder); virtual;
    procedure closeLink(out_: TStringBuilder); virtual;

    procedure openImage(out_: TStringBuilder); virtual;
    procedure closeImage(out_: TStringBuilder); virtual;
  end;

  TSpanEmitter = class
  public
    procedure emitSpan(out_: TStringBuilder; content: String); virtual; abstract;
  end;

  TBlockEmitter = class
  public
    procedure emitBlock(out_: TStringBuilder; lines: TStringList; meta: String); virtual; abstract;
  end;

  TConfiguration = class
  private
    Fdecorator: TDecorator;
    FsafeMode: boolean;
    FallowSpacesInFencedDelimiters: boolean;
    FforceExtendedProfile: boolean;
    FcodeBlockEmitter: TBlockEmitter;
    FpanicMode: boolean;
    FspecialLinkEmitter: TSpanEmitter;
  public
    Constructor Create(safe : boolean);
    Destructor Destroy; override;

    property safeMode: boolean read FsafeMode write FsafeMode;
    property panicMode: boolean read FpanicMode write FpanicMode;
    property decorator: TDecorator read Fdecorator write Fdecorator;
    property codeBlockEmitter: TBlockEmitter read FcodeBlockEmitter write FcodeBlockEmitter;
    property forceExtendedProfile: boolean read FforceExtendedProfile write FforceExtendedProfile;
    property allowSpacesInFencedDelimiters: boolean read FallowSpacesInFencedDelimiters write FallowSpacesInFencedDelimiters;
    property specialLinkEmitter: TSpanEmitter read FspecialLinkEmitter write FspecialLinkEmitter;
  end;

  TLineType = (
    // Empty line. */
    ltEMPTY,
    // Undefined content. */
    ltOTHER,
    // A markdown headline. */
    ltHEADLINE, ltHEADLINE1, ltHEADLINE2,
    // A code block line. */
    ltCODE,
    // A list. */
    ltULIST, ltOLIST,
    // A block quote. */
    ltBQUOTE,
    // A horizontal ruler. */
    ltHR,
    // Start of a XML block. */
    ltXML,
    // Fenced code block start/end */
    ltFENCED_CODE);

  TLine = class
  private
    FXmlEndLine: TLine;
    FPrevEmpty: boolean;
    FPrevious: TLine;
    FPosition: integer;
    FValue: string;
    FIsEmpty: boolean;
    FTrailing: integer;
    FNextEmpty: boolean;
    FLeading: integer;
    FNext: TLine;
    function countChars(ch: char): integer;
    function countCharsStart(ch: char; allowSpaces: boolean): integer;
    function readXMLComment(firstLine: TLine; start: integer): integer;
    function checkHTML(): boolean;

  public
    Constructor Create;
    Destructor Destroy; Override;

    // Current cursor position.
    property position: integer read FPosition write FPosition;
    // Leading and trailing spaces.
    property leading: integer read FLeading write FLeading;
    property trailing: integer read FTrailing write FTrailing;
    // Is this line empty?
    property isEmpty: boolean read FIsEmpty write FIsEmpty;
    // This line's value.
    property value: string read FValue write FValue;
    // Previous and next line.
    property previous: TLine read FPrevious write FPrevious;
    property next: TLine read FNext write FNext;

    // Is previous/next line empty?
    property prevEmpty: boolean read FPrevEmpty write FPrevEmpty;
    property nextEmpty: boolean read FNextEmpty write FNextEmpty;

    // Final line of a XML block.
    property xmlEndLine: TLine read FXmlEndLine write FXmlEndLine;

    procedure Init;
    procedure InitLeading;
    function skipSpaces: boolean;
    function readUntil(chend: TSysCharSet): String;
    procedure setEmpty;
    function getLineType(configuration: TConfiguration): TLineType;
    function stripID: String;

  end;

  TLinkRef = class
  private
    FLink: String;
    FTitle: String;
    FIsAbbrev: boolean;
  public
    Constructor Create(link, title: String; isAbbrev: boolean);

    property link: String read FLink write FLink;
    property title: String read FTitle write FTitle;
    property isAbbrev: boolean read FIsAbbrev write FIsAbbrev;
  end;

  TBlockType = (
    // Unspecified. Used for root block and list items without paragraphs.
    btNONE,
    // A block quote.
    btBLOCKQUOTE,
    // A code block.
    btCODE,
    // A fenced code block.
    btFENCED_CODE,
    // A headline.
    btHEADLINE,
    // A list item.
    btLIST_ITEM,
    // An ordered list.
    btORDERED_LIST,
    // A paragraph.
    btPARAGRAPH,
    // A horizontal ruler.
    btRULER,
    // An unordered list.
    btUNORDERED_LIST,
    // A XML block.
    btXML);

  TBlock = class
  private
    FType: TBlockType;
    FId: String;
    FBlocks: TBlock;
    FBlockTail: TBlock;
    FLines: TLine;
    FLineTail: TLine;
    FHlDepth: integer;
    FNext: TBlock;
    FMeta: String;

    procedure AppendLine(line: TLine);
    function split(line: TLine): TBlock;
    procedure removeListIndent(config: TConfiguration);
    function removeLeadingEmptyLines: boolean;
    procedure removeTrailingEmptyLines;
    procedure transfromHeadline;
    procedure expandListParagraphs;
    function hasLines: boolean;
    procedure removeSurroundingEmptyLines;
    procedure removeBlockQuotePrefix;
    procedure removeLine(line: TLine);
  public
    Constructor Create;
    Destructor Destroy; Override;

    // This block's type.
    property type_: TBlockType read FType write FType;

    property lines: TLine read FLines;
    property lineTail: TLine read FLineTail;

    // child blocks.
    property blocks: TBlock read FBlocks;
    property blockTail: TBlock read FBlockTail;

    // Next block.
    property next: TBlock read FNext write FNext;
    // Depth of headline BlockType.
    property hlDepth: integer read FHlDepth write FHlDepth;
    // ID for headlines and list items
    property id: String read FId write FId;
    // Block meta information
    property meta: String read FMeta write FMeta;

  end;

  TMarkToken = (
    // No token.
    mtNONE,
    // &#x2a;
    mtEM_STAR, // x*x
    // _
    mtEM_UNDERSCORE, // x_x
    // &#x2a;&#x2a;
    mtSTRONG_STAR, // x**x
    // __
    mtSTRONG_UNDERSCORE, // x__x
    // `
    mtCODE_SINGLE, // `
    // ``
    mtCODE_DOUBLE, // ``
    // [
    mtLINK, // [
    // &lt;
    mtHTML, // <
    // ![
    mtIMAGE, // ![
    // &amp;
    mtENTITY, // &
    // \
    mtESCAPE, // \x
    // Extended: ^
    mtSUPER, // ^
    // Extended: (C)
    mtX_COPY, // (C)
    // Extended: (R)
    mtX_REG, // (R)
    // Extended: (TM)
    mtX_TRADE, // (TM)
    // Extended: &lt;&lt;
    mtX_LAQUO, // <<
    // Extended: >>
    mtX_RAQUO, // >>
    // Extended: --
    mtX_NDASH, // --
    // Extended: ---
    mtX_MDASH, // ---
    // Extended: &#46;&#46;&#46;
    mtX_HELLIP, // ...
    // Extended: "x
    mtX_RDQUO, // "
    // Extended: x"
    mtX_LDQUO, // "
    // [[
    mtX_LINK_OPEN, // [[
    // ]]
    mtX_LINK_CLOSE // ]]
    );

  // Emitter class responsible for generating HTML output.
  TEmitter = class
  private
    linkRefs: TStringList;
    FConfig: TConfiguration;
    FuseExtensions: boolean;
    procedure emitCodeLines(out_: TStringBuilder; lines: TLine; meta: String; removeIndent: boolean);
    procedure emitRawLines(out_: TStringBuilder; lines: TLine);
    procedure emitMarkedLines(out_: TStringBuilder; lines: TLine);
    function findToken(s: String; start: integer; token: TMarkToken): integer;
    function getToken(s: String; position: integer): TMarkToken;
    function checkLink(out_: TStringBuilder; s: String; start: integer; token: TMarkToken): integer;
    function recursiveEmitLine(out_: TStringBuilder; s: String; start: integer; token: TMarkToken): integer;
    function checkHTML(out_: TStringBuilder; s: String; start: integer): integer;
    class function checkEntity(out_: TStringBuilder; s: String; start: integer): integer;
    class function whitespaceToSpace(c: char): char;
  public
    Constructor Create(config: TConfiguration);
    Destructor Destroy; override;

    procedure addLinkRef(key: String; linkRef: TLinkRef);
    procedure emit(out_: TStringBuilder; root: TBlock);
    procedure emitLines(out_: TStringBuilder; block: TBlock);

  end;

  TMarkdownDaringFireball = class(TMarkdownProcessor)
  private
    FConfig: TConfiguration;
    Femitter: TEmitter;
    FuseExtensions: boolean;
    function readLines(reader : TReader): TBlock;
    procedure initListBlock(root: TBlock);
    procedure recurse(root: TBlock; listMode: boolean);

  protected
    function GetUnSafe: boolean; override;
    procedure SetUnSafe(const value: boolean); override;
  public
    Constructor Create;
    Destructor Destroy; override;

    function process(source: String): String; override;

    property config: TConfiguration read FConfig;
  end;

implementation

Function StringsContains(Const aNames: Array Of String; Const sName: String): boolean;
var
  i: integer;
Begin
  for i := 0 to length(aNames) - 1 do
    if sName <> aNames[i] then
      exit(true);
  result := false;
End;

function StringToEnum(ATypeInfo: PTypeInfo; const AStr: String; defValue: integer): integer;
var
  LTypeData: PTypeData;
  LPChar: PAnsiChar;
  LValue: ShortString;
begin
  LValue := ShortString(AStr);

  if ATypeInfo^.Kind = tkEnumeration then
  begin
    LTypeData := GetTypeData(ATypeInfo);
    if LTypeData^.MinValue <> 0 then
      exit(defValue);
    LPChar := @LTypeData^.NameList[0];
    result := 0;
    while (result <= LTypeData^.MaxValue) and (ShortString(pointer(LPChar)^) <> LValue) do
    begin
      inc(LPChar, ord(LPChar^) + 1); // move to next string
      inc(result);
    end;
    if result > LTypeData^.MaxValue then
      exit(defValue);
  end
  else
    exit(defValue);
end;

{ TMarkdownDaringFireball }

constructor TMarkdownDaringFireball.Create;
begin
  inherited Create;
  FConfig := TConfiguration.Create(true);
  Femitter := TEmitter.Create(config);
end;

destructor TMarkdownDaringFireball.Destroy;
begin
  FConfig.Free;
  Femitter.Free;
  inherited;
end;

function TMarkdownDaringFireball.GetUnSafe: boolean;
begin
  result := not FConfig.safeMode;
end;

procedure TMarkdownDaringFireball.initListBlock(root: TBlock);
var
  line: TLine;
  t: TLineType;
begin
  line := root.lines;
  line := line.next;
  while (line <> nil) do
  begin
    t := line.getLineType(FConfig);
    if ((t = ltOLIST) or (t = ltULIST) or (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and not((t = ltOLIST) or (t = ltULIST))))) then
      root.split(line.previous).type_ := btLIST_ITEM;
    line := line.next;
  end;
  root.split(root.lineTail).type_ := btLIST_ITEM;
end;

function TMarkdownDaringFireball.process(source: String): String;
var
  out_: TStringBuilder;
  parent, block: TBlock;
  rdr : TReader;
begin
  FuseExtensions := config.forceExtendedProfile;
  rdr := TReader.Create(source);
  try
    out_ := TStringBuilder.Create;
    try
      parent := readLines(rdr);
      try
        parent.removeSurroundingEmptyLines;
        recurse(parent, false);
        block := parent.blocks;
        while (block <> nil) do
        begin
          Femitter.emit(out_, block);
          block := block.next;
        end;
        result := out_.ToString;
      finally
        parent.Free;
      end;
    finally
      out_.Free;
    end;
  finally
    rdr.Free;
  end;
end;

function TMarkdownDaringFireball.readLines(reader : TReader): TBlock;
var
  block: TBlock;
  sb: TStringBuilder;
  c, ch: char;
  position, np: integer;
  eol, isLinkRef, lineAdded: boolean;
  lastLinkRef, lr: TLinkRef;
  line: TLine;
  id, link, comment: String;
begin
  block := TBlock.Create;
  sb := TStringBuilder.Create;
  try
    c := reader.read();
    lastLinkRef := nil;
    while (c <> #0) do
    begin
      sb.Clear;
      position := 0;
      eol := false;
      while (not eol) do
      begin
        case c of
          #0:
            eol := true;
          #10:
            begin
              c := reader.read();
              if (c = #13) then
                c := reader.read();
              eol := true;
            end;
          #13:
            begin
              c := reader.read();
              if (c = #10) then
                c := reader.read();
              eol := true;
            end;
          #9:
            begin
              np := position + (4 - (position and 3));
              while (position < np) do
              begin
                sb.append(' ');
                inc(position);
              end;
              c := reader.read();
            end;
        else
          if (c <> '<') or (not FConfig.panicMode) then
          begin
            inc(position);
            sb.append(c);
          end
          else
          begin
            inc(position, 4);
            sb.append('&lt;');
          end;
          c := reader.read();
        end;
      end;

      lineAdded := false;
      line := TLine.Create;
      try
        line.value := sb.ToString();
        line.Init();

        // Check for link definitions
        isLinkRef := false;
        id := '';
        link := '';
        comment := '';
        if (not line.isEmpty) and (line.leading < 4) and (line.value[1 + line.leading] = '[') then
        begin
          line.position := line.leading + 1;
          // Read ID up to ']'
          id := line.readUntil([']']);
          // Is ID valid and are there any more characters?
          if (id <> '') and (line.position + 2 < Length(line.value)) then
          begin
            // Check for ':' ([...]:...)
            if (line.value[1 + line.position + 1] = ':') then
            begin
              line.position := line.position + 2;
              line.skipSpaces();
              // Check for link syntax
              if (line.value[1 + line.position] = '<') then
              begin
                line.position := line.position + 1;
                link := line.readUntil(['>']);
                line.position := line.position + 1;
              end
              else
                link := line.readUntil([' ', #10]);

              // Is link valid?
              if (link <> '') then
              begin
                // Any non-whitespace characters following?
                if (line.skipSpaces()) then
                begin
                  ch := line.value[1 + line.position];
                  // Read comment
                  if (ch = '"') or (ch = '''') or (ch = '(') then
                  begin
                    line.position := line.position + 1;
                    if ch = '(' then
                      comment := line.readUntil([')'])
                    else
                      comment := line.readUntil([ch]);
                    // Valid linkRef only if comment is valid
                    if (comment <> '') then
                      isLinkRef := true;
                  end;
                end
                else
                  isLinkRef := true;
              end;
            end;
          end;
        end;

        if (isLinkRef) then
        begin
          if (LowerCase(id) = '$profile$') then
          begin
            FuseExtensions := LowerCase(link) = 'extended';
            Femitter.FuseExtensions := FuseExtensions;
            lastLinkRef := nil;
          end
          else
          begin
            // Store linkRef and skip line
            lr := TLinkRef.Create(link, comment, (comment <> '') and (Length(link) = 1) and (link[1 + 1] = '*'));
            Femitter.addLinkRef(id, lr);
            if (comment = '') then
              lastLinkRef := lr;
          end;
        end
        else
        begin
          comment := '';
          // Check for multi-line linkRef
          if (not line.isEmpty and (lastLinkRef <> nil)) then
          begin
            line.position := line.leading;
            ch := line.value[1 + line.position];
            if (ch = '"') or (ch = '''') or (ch = '(') then
            begin
              line.position := line.position + 1;
              if ch = '(' then
                comment := line.readUntil([')'])
              else
                comment := line.readUntil([ch]);
            end;
            if (comment <> '') then
              lastLinkRef.title := comment;
            lastLinkRef := nil;
          end;

          // No multi-line linkRef, store line
          if (comment = '') then
          begin
            line.position := 0;
            block.AppendLine(line);
            lineAdded := true;
          end;
        end;
      finally
        if not lineAdded then
          line.Free;
      end;
    end;
    result := block;
  finally
    sb.Free;
  end;
end;

procedure TMarkdownDaringFireball.recurse(root: TBlock; listMode: boolean);
var
  block, list: TBlock;
  line: TLine;
  type_, t: TLineType;
  wasEmpty: boolean;
  bt: TBlockType;
begin
  line := root.lines;
  if (listMode) then
  begin
    root.removeListIndent(FConfig);
    if (FuseExtensions and (root.lines <> nil) and (root.lines.getLineType(FConfig) <> ltCODE)) then
      root.id := root.lines.stripID();
  end;

  while (line <> nil) and line.isEmpty do
    line := line.next;
  if (line = nil) then
    exit;

  while (line <> nil) do
  begin
    type_ := line.getLineType(FConfig);
    case type_ of
      ltOTHER:
        begin
          wasEmpty := line.prevEmpty;
          while (line <> nil) and (not line.isEmpty) do
          begin
            t := line.getLineType(FConfig);
            if (listMode or FuseExtensions) and (t in [ltOLIST, ltULIST]) then
              break;
            if (FuseExtensions and (t in [ltCODE, ltFENCED_CODE])) then
              break;
            if (t in [ltHEADLINE, ltHEADLINE1, ltHEADLINE2, ltHR, ltBQUOTE, ltXML]) then
              break;
            line := line.next;
          end;

          if (line <> nil) and not line.isEmpty then
          begin
            if (listMode and not wasEmpty) then
              bt := btNONE
            else
              bt := btPARAGRAPH;
            if line = nil then
              root.split(root.lineTail).type_ := bt
            else
              root.split(line.previous).type_ := bt;
            root.removeLeadingEmptyLines();
          end
          else
          begin
            if (listMode and ((line = nil) or (not line.isEmpty)) and not wasEmpty) then
              bt := btNONE
            else
              bt := btPARAGRAPH;
            root.removeLeadingEmptyLines();
            if (line <> nil) then
              root.split(line.previous).type_ := bt
            else
              root.split(root.lineTail).type_ := bt;
          end;
          line := root.lines;
        end;
      ltCODE:
        begin
          while (line <> nil) and (line.isEmpty or (line.leading > 3)) do
            line := line.next;
          if (line <> nil) then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.type_ := btCODE;
          block.removeSurroundingEmptyLines();
        end;
      ltXML:
        begin
          if (line.previous <> nil) then
            // FIXME ... this looks wrong
            root.split(line.previous);
          root.split(line.xmlEndLine).type_ := btXML;
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltBQUOTE:
        begin
          while (line <> nil) do
          begin
            if (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and (line.getLineType(FConfig) <> ltBQUOTE))) then
              break;
            line := line.next;
          end;
          if line <> nil then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.type_ := btBLOCKQUOTE;
          block.removeSurroundingEmptyLines();
          block.removeBlockQuotePrefix();
          recurse(block, false);
          line := root.lines;
        end;
      ltHR:
        begin
          if (line.previous <> nil) then
            // FIXME ... this looks wrong
            root.split(line.previous);
          root.split(line).type_ := btRULER;
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltFENCED_CODE:
        begin
          line := line.next;
          while (line <> nil) do
          begin
            if (line.getLineType(FConfig) = ltFENCED_CODE) then
              break;
            // TODO ... is this really necessary? Maybe add a special flag?
            line := line.next;
          end;
          if (line <> nil) then
            line := line.next;
          if line <> nil then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.type_ := btFENCED_CODE;
          block.meta := TUtils.getMetaFromFence(block.lines.value);
          block.lines.setEmpty();
          if (block.lineTail.getLineType(FConfig) = ltFENCED_CODE) then
            block.lineTail.setEmpty();
          block.removeSurroundingEmptyLines();
        end;
      ltHEADLINE, ltHEADLINE1, ltHEADLINE2:
        begin
          if (line.previous <> nil) then
            root.split(line.previous);
          if (type_ <> ltHEADLINE) then
            line.next.setEmpty();
          block := root.split(line);
          block.type_ := btHEADLINE;
          if (type_ <> ltHEADLINE) then
            if type_ = ltHEADLINE1 then
              block.hlDepth := 1
            else
              block.hlDepth := 2;
          if (FuseExtensions) then
            block.id := block.lines.stripID();
          block.transfromHeadline();
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltOLIST, ltULIST:
        begin
          while (line <> nil) do
          begin
            t := line.getLineType(FConfig);
            if (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and (not(t = type_)))) then
              break;
            line := line.next;
          end;
          if line <> nil then
            list := root.split(line.previous)
          else
            list := root.split(root.lineTail);
          if type_ = ltOLIST then
            list.type_ := btORDERED_LIST
          else
            list.type_ := btUNORDERED_LIST;
          list.lines.prevEmpty := false;
          list.lineTail.nextEmpty := false;
          list.removeSurroundingEmptyLines();
          list.lineTail.nextEmpty := false;
          list.lines.prevEmpty := list.lineTail.nextEmpty;
          initListBlock(list);
          block := list.blocks;
          while (block <> nil) do
          begin
            recurse(block, true);
            block := block.next;
          end;
          list.expandListParagraphs();
        end
    else
      line := line.next;
    end;
  end;
end;

procedure TMarkdownDaringFireball.SetUnSafe(const value: boolean);
begin
  FConfig.safeMode := not value;
end;

{ TLine }

constructor TLine.Create;
begin
  inherited;
  FIsEmpty := true;
end;

destructor TLine.Destroy;
begin
  FNext.Free;
  inherited;
end;

{ TConfiguration }

constructor TConfiguration.Create(safe : boolean);
begin
  inherited Create;
  FallowSpacesInFencedDelimiters := true;
  Fdecorator := TDecorator.Create;
  FsafeMode := safe;
end;

destructor TConfiguration.Destroy;
begin
  FcodeBlockEmitter.Free;
  Fdecorator.Free;
  FspecialLinkEmitter.Free;
  inherited;
end;

{ TDecorator }

procedure TDecorator.openParagraph(out_: TStringBuilder);
begin
  out_.append('<p>');
end;

procedure TDecorator.closeParagraph(out_: TStringBuilder);
begin
  out_.append('</p>'#10);
end;

procedure TDecorator.openBlockQuote(out_: TStringBuilder);
begin
  out_.append('<blockquote>');
end;

procedure TDecorator.closeBlockQuote(out_: TStringBuilder);
begin
  out_.append('</blockquote>'#10);
end;

procedure TDecorator.openCodeBlock(out_: TStringBuilder);
begin
  out_.append('<pre><code>');
end;

procedure TDecorator.closeCodeBlock(out_: TStringBuilder);
begin
  out_.append('</code></pre>'#10);
end;

procedure TDecorator.openCodeSpan(out_: TStringBuilder);
begin
  out_.append('<code>');
end;

procedure TDecorator.closeCodeSpan(out_: TStringBuilder);
begin
  out_.append('</code>');
end;

procedure TDecorator.openHeadline(out_: TStringBuilder; level: integer);
begin
  out_.append('<h');
  out_.append(level);
end;

procedure TDecorator.closeHeadline(out_: TStringBuilder; level: integer);
begin
  out_.append('</h');
  out_.append(level);
  out_.append('>'#10);
end;

procedure TDecorator.openStrong(out_: TStringBuilder);
begin
  out_.append('<strong>');
end;

procedure TDecorator.closeStrong(out_: TStringBuilder);
begin
  out_.append('</strong>');
end;

procedure TDecorator.openEmphasis(out_: TStringBuilder);
begin
  out_.append('<em>');
end;

procedure TDecorator.closeEmphasis(out_: TStringBuilder);
begin
  out_.append('</em>');
end;

procedure TDecorator.openSuper(out_: TStringBuilder);
begin
  out_.append('<sup>');
end;

procedure TDecorator.closeSuper(out_: TStringBuilder);
begin
  out_.append('</sup>');
end;

procedure TDecorator.openOrderedList(out_: TStringBuilder);
begin
  out_.append('<ol>'#10);
end;

procedure TDecorator.closeOrderedList(out_: TStringBuilder);
begin
  out_.append('</ol>'#10);
end;

procedure TDecorator.openUnOrderedList(out_: TStringBuilder);
begin
  out_.append('<ul>'#10);
end;

procedure TDecorator.closeUnOrderedList(out_: TStringBuilder);
begin
  out_.append('</ul>'#10);
end;

procedure TDecorator.openListItem(out_: TStringBuilder);
begin
  out_.append('<li');
end;

procedure TDecorator.closeListItem(out_: TStringBuilder);
begin
  out_.append('</li>'#10);
end;

procedure TDecorator.horizontalRuler(out_: TStringBuilder);
begin
  out_.append('<hr/>'#10);
end;

procedure TDecorator.openLink(out_: TStringBuilder);
begin
  out_.append('<a');
end;

procedure TDecorator.closeLink(out_: TStringBuilder);
begin
  out_.append('</a>');
end;

procedure TDecorator.openImage(out_: TStringBuilder);
begin
  out_.append('<img');
end;

procedure TDecorator.closeImage(out_: TStringBuilder);
begin
  out_.append(' />');
end;

{ TEmitter }

constructor TEmitter.Create(config: TConfiguration);
begin
  inherited Create;
  FConfig := config;
  linkRefs := TStringList.Create;
  linkRefs.Sorted := true;
  linkRefs.Duplicates := dupError;
end;

destructor TEmitter.Destroy;
var
  i : integer;
begin
  for i := 0 to linkRefs.Count - 1 do
    linkRefs.Objects[i].Free;
  linkRefs.Free;
  inherited;
end;

procedure TEmitter.addLinkRef(key: String; linkRef: TLinkRef);
var
  k : String;
  i : integer;
begin
  k := LowerCase(key);
  if linkRefs.find(k, i) then
  begin
    linkRefs.Objects[i].Free;
    linkRefs.Objects[i] := linkRef;
  end
  else
    linkRefs.AddObject(k, linkRef);
end;

procedure TEmitter.emit(out_: TStringBuilder; root: TBlock);
var
  block: TBlock;
begin
  root.removeSurroundingEmptyLines();

  case root.type_ of
    btRULER:
      begin
        FConfig.decorator.horizontalRuler(out_);
        exit;
      end;
    btNONE, btXML:
      ; // nothing
    btHEADLINE:
      begin
        FConfig.decorator.openHeadline(out_, root.hlDepth);
        if (FuseExtensions and (root.id <> '')) then
        begin
          out_.append(' id="');
          TUtils.appendCode(out_, root.id, 0, Length(root.id));
          out_.append('"');
        end;
        out_.append('>');
      end;
    btPARAGRAPH:
      FConfig.decorator.openParagraph(out_);
    btCODE, btFENCED_CODE:
      if (FConfig.codeBlockEmitter = nil) then
        FConfig.decorator.openCodeBlock(out_);
    btBLOCKQUOTE:
      FConfig.decorator.openBlockQuote(out_);
    btUNORDERED_LIST:
      FConfig.decorator.openUnOrderedList(out_);
    btORDERED_LIST:
      FConfig.decorator.openOrderedList(out_);
    btLIST_ITEM:
      begin
        FConfig.decorator.openListItem(out_);
        if (FuseExtensions and (root.id <> '')) then
        begin
          out_.append(' id="');
          TUtils.appendCode(out_, root.id, 0, Length(root.id));
          out_.append('"');
        end;
        out_.append('>');
      end;
  end;

  if (root.hasLines()) then
    emitLines(out_, root)
  else
  begin
    block := root.blocks;
    while (block <> nil) do
    begin
      emit(out_, block);
      block := block.next;
    end;
  end;

  case (root.type_) of
    btRULER, btNONE, btXML:
      ; // nothing
    btHEADLINE:
      FConfig.decorator.closeHeadline(out_, root.hlDepth);
    btPARAGRAPH:
      FConfig.decorator.closeParagraph(out_);
    btCODE, btFENCED_CODE:
      if (FConfig.codeBlockEmitter = nil) then
        FConfig.decorator.closeCodeBlock(out_);
    btBLOCKQUOTE:
      FConfig.decorator.closeBlockQuote(out_);
    btUNORDERED_LIST:
      FConfig.decorator.closeUnOrderedList(out_);
    btORDERED_LIST:
      FConfig.decorator.closeOrderedList(out_);
    btLIST_ITEM:
      FConfig.decorator.closeListItem(out_);
  end;
end;

procedure TEmitter.emitLines(out_: TStringBuilder; block: TBlock);
begin
  case (block.type_) of
    btCODE:
      emitCodeLines(out_, block.lines, block.meta, true);
    btFENCED_CODE:
      emitCodeLines(out_, block.lines, block.meta, false);
    btXML:
      emitRawLines(out_, block.lines);
  else
    emitMarkedLines(out_, block.lines);
  end;
end;

function TEmitter.findToken(s: String; start: integer; token: TMarkToken): integer;
var
  position: integer;
begin
  position := start;
  while (position < Length(s)) do
  begin
    if getToken(s, position) = token then
      exit(position);
    inc(position);
  end;
  result := -1;
end;

function TEmitter.checkLink(out_: TStringBuilder; s: String; start: integer; token: TMarkToken): integer;
var
  isAbbrev, useLt, hasLink: boolean;
  position, oldPos, i: integer;
  temp: TStringBuilder;
  name, link, comment, id: String;
  lr: TLinkRef;
begin
  isAbbrev := false;
  if (token = mtLINK) then
    position := start + 1
  else
    position := start + 2;
  temp := TStringBuilder.Create;
  try
    position := TUtils.readMdLinkId(temp, s, position);
    if (position < start) then
      exit(-1);
    name := temp.ToString();
    link := '';
    hasLink := false;
    comment := '';
    oldPos := position;
    inc(position);
    position := TUtils.skipSpaces(s, position);
    if (position < start) then
    begin
      if linkRefs.find(LowerCase(name), i) then
      begin
        lr := TLinkRef(linkRefs.Objects[i]);
        isAbbrev := lr.isAbbrev;
        link := lr.link;
        hasLink := true;
        comment := lr.title;
        position := oldPos;
      end
      else
        exit(-1);
    end
    else if (s[1 + position] = '(') then
    begin
      inc(position);
      position := TUtils.skipSpaces(s, position);
      if (position < start) then
        exit(-1);
      temp.Clear;
      useLt := s[1 + position] = '<';
      if useLt then
        position := TUtils.readUntil(temp, s, position + 1, '>')
      else
        position := TUtils.readMdLink(temp, s, position);
      if (position < start) then
        exit(-1);
      if (useLt) then
        inc(position);
      link := temp.ToString();
      hasLink := true;
      if (s[1 + position] = ' ') then
      begin
        position := TUtils.skipSpaces(s, position);
        if (position > start) and (s[1 + position] = '"') then
        begin
          inc(position);
          temp.Clear;
          position := TUtils.readUntil(temp, s, position, '"');
          if (position < start) then
            exit(-1);
          comment := temp.ToString();
          inc(position);
          position := TUtils.skipSpaces(s, position);
          if (position = -1) then
            exit(-1);
        end;
      end;
      if (s[1 + position] <> ')') then
        exit(-1);
    end
    else if (s[1 + position] = '[') then
    begin
      inc(position);
      temp.Clear;
      position := TUtils.readRawUntil(temp, s, position, ']');
      if (position < start) then
        exit(-1);
      if temp.length > 0 then
        id := temp.ToString()
      else
        id := name;
      if linkRefs.find(LowerCase(id), i) then
      begin
        lr := TLinkRef(linkRefs.Objects[i]);
        link := lr.link;
        hasLink := true;
        comment := lr.title;
      end
    end
    else
    begin
      if linkRefs.find(LowerCase(name), i) then
      begin
        lr := TLinkRef(linkRefs.Objects[i]);
        isAbbrev := lr.isAbbrev;
        link := lr.link;
        hasLink := true;
        comment := lr.title;
        position := oldPos;
      end
      else
        exit(-1);
    end;
    if (not hasLink) then
      exit(-1);

    if (token = mtLINK) then
    begin
      if (isAbbrev) and (comment <> '') then
      begin
        if (not FuseExtensions) then
          exit(-1);
        out_.append('<abbr title:="');
        TUtils.appendValue(out_, comment, 0, Length(comment));
        out_.append('">');
        recursiveEmitLine(out_, name, 0, mtNONE);
        out_.append('</abbr>');
      end
      else
      begin
        FConfig.decorator.openLink(out_);
        out_.append(' href="');
        TUtils.appendValue(out_, link, 0, Length(link));
        out_.append('"');
        if (comment <> '') then
        begin
          out_.append(' title="');
          TUtils.appendValue(out_, comment, 0, Length(comment));
          out_.append('"');
        end;
        out_.append('>');
        recursiveEmitLine(out_, name, 0, mtNONE);
        FConfig.decorator.closeLink(out_);
      end
    end
    else
    begin
      FConfig.decorator.openImage(out_);
      out_.append(' src="');
      TUtils.appendValue(out_, link, 0, Length(link));
      out_.append('" alt="');
      TUtils.appendValue(out_, name, 0, Length(name));
      out_.append('"');
      if (comment <> '') then
      begin
        out_.append(' title="');
        TUtils.appendValue(out_, comment, 0, Length(comment));
        out_.append('"');
      end;
      FConfig.decorator.closeImage(out_);
    end;
    result := position;
  finally
    temp.Free;
  end;
end;

function TEmitter.checkHTML(out_: TStringBuilder; s: String; start: integer): integer;
var
  temp: TStringBuilder;
  position: integer;
  link: String;
begin
  temp := TStringBuilder.Create();
  try
    // Check for auto links
    temp.Clear;
    position := TUtils.readUntil(temp, s, start + 1, [':', ' ', '>', #10]);
    if (position <> -1) and (s[1 + position] = ':') and (THTML.isLinkPrefix(temp.ToString())) then
    begin
      position := TUtils.readUntil(temp, s, position, ['>']);
      if (position <> -1) then
      begin
        link := temp.ToString();
        FConfig.decorator.openLink(out_);
        out_.append(' href="');
        TUtils.appendValue(out_, link, 0, Length(link));
        out_.append('">');
        TUtils.appendValue(out_, link, 0, Length(link));
        FConfig.decorator.closeLink(out_);
        exit(position);
      end;
    end;

    // Check for mailto auto link
    temp.Clear;
    position := TUtils.readUntil(temp, s, start + 1, ['@', ' ', '>', #10]);
    if (position <> -1) and (s[1 + position] = '@') then
    begin
      position := TUtils.readUntil(temp, s, position, '>');
      if (position <> -1) then
      begin
        link := temp.ToString();
        FConfig.decorator.openLink(out_);
        out_.append(' href="');
        TUtils.appendMailto(out_, 'mailto:', 0, 7);
        TUtils.appendMailto(out_, link, 0, Length(link));
        out_.append('">');
        TUtils.appendMailto(out_, link, 0, Length(link));
        FConfig.decorator.closeLink(out_);
        exit(position);
      end;
    end;

    // Check for inline html
    if (start + 2 < Length(s)) then
    begin
      temp.Clear;
      exit(TUtils.readXML(out_, s, start, FConfig.safeMode));
    end;

    result := -1;
  finally
    temp.Free;
  end;
end;

class function TEmitter.checkEntity(out_: TStringBuilder; s: String; start: integer): integer;
var
  position, i: integer;
  c: char;
begin
  position := TUtils.readUntil(out_, s, start, ';');
  if (position < 0) or (out_.length < 3) then
    exit(-1);
  if (out_[1] = '#') then
  begin
    if (out_[2] = 'x') or (out_[2] = 'X') then
    begin
      if (out_.length < 4) then
        exit(-1);
      for i := 3 to out_.length do
      begin
        c := out_[i];
        if ((c < '0') or (c > '9')) and (((c < 'a') or (c > 'f')) and ((c < 'A') or (c > 'F'))) then
          exit(-1);
      end;
    end
    else
    begin
      for i := 2 to out_.length do
      begin
        c := out_[i];
        if (c < '0') or (c > '9') then
          exit(-1);
      end;
    end;
    out_.append(';');
  end
  else
  begin
    for i := 1 to out_.length - 1 do
    begin
      c := out_[i]; // zero based
      if (not c.isLetterOrDigit()) then
        exit(-1);
    end;
    out_.append(';');
    if THTML.isEntity(out_.ToString()) then
      exit(position)
    else
      exit(-1);
  end;

  result := position;
end;

function TEmitter.recursiveEmitLine(out_: TStringBuilder; s: String; start: integer; token: TMarkToken): integer;
var
  position, a, b: integer;
  temp: TStringBuilder;
  mt: TMarkToken;
begin
  position := start;
  temp := TStringBuilder.Create();
  try
    while (position < Length(s)) do
    begin
      mt := getToken(s, position);
      if (token <> mtNONE) and ((mt = token) or ((token = mtEM_STAR) and (mt = mtSTRONG_STAR)) or ((token = mtEM_UNDERSCORE) and (mt = mtSTRONG_UNDERSCORE))) then
        exit(position);

      case mt of
        mtIMAGE, mtLINK:
          begin
            temp.Clear;
            b := checkLink(temp, s, position, mt);
            if (b > 0) then
            begin
              out_.append(temp);
              position := b;
            end
            else
              out_.append(s[1 + position]);
          end;
        mtEM_STAR, mtEM_UNDERSCORE:
          begin
            temp.Clear;
            b := recursiveEmitLine(temp, s, position + 1, mt);
            if (b > 0) then
            begin
              FConfig.decorator.openEmphasis(out_);
              out_.append(temp);
              FConfig.decorator.closeEmphasis(out_);
              position := b;
            end
            else
              out_.append(s[1 + position]);
          end;
        mtSTRONG_STAR, mtSTRONG_UNDERSCORE:
          begin
            temp.Clear;
            b := recursiveEmitLine(temp, s, position + 2, mt);
            if (b > 0) then
            begin
              FConfig.decorator.openStrong(out_);
              out_.append(temp);
              FConfig.decorator.closeStrong(out_);
              position := b + 1;
            end
            else
              out_.append(s[1 + position]);
          end;
        mtSUPER:
          begin
            temp.Clear;
            b := recursiveEmitLine(temp, s, position + 1, mt);
            if (b > 0) then
            begin
              FConfig.decorator.openSuper(out_);
              out_.append(temp);
              FConfig.decorator.closeSuper(out_);
              position := b;
            end
            else
              out_.append(s[1 + position]);
          end;
        mtCODE_SINGLE, mtCODE_DOUBLE:
          begin
            if mt = mtCODE_DOUBLE then
              a := position + 2
            else
              a := position + 1;
            b := findToken(s, a, mt);
            if (b > 0) then
            begin
              if mt = mtCODE_DOUBLE then
                position := b + 1
              else
                position := b + 0;
              while (a < b) and (s[1 + a] = ' ') do
                inc(a);
              if (a < b) then
              begin
                while (s[1 + b - 1] = ' ') do
                  dec(b);
              end;
              FConfig.decorator.openCodeSpan(out_);
              TUtils.appendCode(out_, s, a, b);
              FConfig.decorator.closeCodeSpan(out_);
            end
            else
              out_.append(s[1 + position]);
          end;
        mtHTML:
          begin
            temp.Clear;
            b := checkHTML(temp, s, position);
            if (b > 0) then
            begin
              out_.append(temp);
              position := b;
            end
            else
              out_.append('&lt;');
          end;
        mtENTITY:
          begin
            temp.Clear;
            b := checkEntity(temp, s, position);
            if (b > 0) then
            begin
              out_.append(temp);
              position := b;
            end
            else
              out_.append('&amp;');
          end;
        mtX_LINK_OPEN:
          begin
            temp.Clear;
            b := recursiveEmitLine(temp, s, position + 2, mtX_LINK_CLOSE);
            if (b > 0) and (FConfig.specialLinkEmitter <> nil) then
            begin
              FConfig.specialLinkEmitter.emitSpan(out_, temp.ToString());
              position := b + 1;
            end
            else
              out_.append(s[1 + position]);
          end;
        mtX_COPY:
          begin
            out_.append('&copy;');
            inc(position, 2);
          end;
        mtX_REG:
          begin
            out_.append('&reg;');
            inc(position, 2);
          end;
        mtX_TRADE:
          begin
            out_.append('&trade;');
            inc(position, 3);
          end;
        mtX_NDASH:
          begin
            out_.append('&ndash;');
            inc(position);
          end;
        mtX_MDASH:
          begin
            out_.append('&mdash;');
            inc(position, 2);
          end;
        mtX_HELLIP:
          begin
            out_.append('&hellip;');
            inc(position, 2);
          end;
        mtX_LAQUO:
          begin
            out_.append('&laquo;');
            inc(position);
          end;
        mtX_RAQUO:
          begin
            out_.append('&raquo;');
            inc(position);
          end;
        mtX_RDQUO:
          out_.append('&rdquo;');
        mtX_LDQUO:
          out_.append('&ldquo;');
        mtESCAPE:
          begin
            inc(position);
            out_.append(s[1 + position]);
          end;
        // $FALL-THROUGH$
      else
        out_.append(s[1 + position]);
      end;
      inc(position);
    end;
    result := -1;
  finally
    temp.Free;
  end;
end;

class function TEmitter.whitespaceToSpace(c: char): char;
begin
  if c.isWhitespace() then
    result := ' '
  else
    result := c;
end;

function TEmitter.getToken(s: String; position: integer): TMarkToken;
var
  c0, c, c1, c2, c3: char;
begin

  result := mtNONE;
  if (position > 0) then
    c0 := whitespaceToSpace(s[1 + position - 1])
  else
    c0 := ' ';
  c := whitespaceToSpace(s[1 + position]);
  if (position + 1 < Length(s)) then
    c1 := whitespaceToSpace(s[1 + position + 1])
  else
    c1 := ' ';
  if (position + 2 < Length(s)) then
    c2 := whitespaceToSpace(s[1 + position + 2])
  else
    c2 := ' ';
  if (position + 3 < Length(s)) then
    c3 := whitespaceToSpace(s[1 + position + 3])
  else
    c3 := ' ';

  case (c) of
    '*':
      if (c1 = '*') then
      begin
        if (c0 <> ' ') or (c2 <> ' ') then
          exit(mtSTRONG_STAR)
        else
          exit(mtEM_STAR);
      end
      else if (c0 <> ' ') or (c1 <> ' ') then
        exit(mtEM_STAR)
      else
        exit(mtNONE);
    '_':
      if (c1 = '_') then
      begin
        if (c0 <> ' ') or (c2 <> ' ') then
          exit(mtSTRONG_UNDERSCORE)
        else
          exit(mtEM_UNDERSCORE);
      end
      else if (FuseExtensions) then
      begin
        if (c0.isLetterOrDigit()) and (c0 <> '_') and (c1.isLetterOrDigit()) then
          exit(mtNONE)
        else
          exit(mtEM_UNDERSCORE);
      end
      else if (c0 <> ' ') or (c1 <> ' ') then
        exit(mtEM_UNDERSCORE)
      else
        exit(mtNONE);
    '!':
      if (c1 = '[') then
        exit(mtIMAGE)
      else
        exit(mtNONE);
    '[':
      if (FuseExtensions) and (c1 = '[') then
        exit(mtX_LINK_OPEN)
      else
        exit(mtLINK);
    ']':
      if (FuseExtensions) and (c1 = ']') then
        exit(mtX_LINK_CLOSE)
      else
        exit(mtNONE);
    '`':
      if (c1 = '`') then
        exit(mtCODE_DOUBLE)
      else
        exit(mtCODE_SINGLE);
    '\':
      if CharInSet(c1, ['\', '[', ']', '(', ')', '{', '}', '#', '"', '''', '.', '>', '<', '*', '+', '-', '_', '!', '`', '~', '^']) then
        exit(mtESCAPE)
      else
        exit(mtNONE);
    '<':
      if (FuseExtensions) and (c1 = '<') then
        exit(mtX_LAQUO)
      else
        exit(mtHTML);
    '&':
      exit(mtENTITY);
  else
    if (FuseExtensions) then
      case (c) of
        '-':
          if (c1 = '-') and (c2 = '-') then
            exit(mtX_MDASH)
          else
            exit(mtX_NDASH);
        '^':
          if (c0 = '^') or (c1 = '^') then
            exit(mtNONE)
          else
            exit(mtSUPER);
        '>':
          if (c1 = '>') then
            exit(mtX_RAQUO);
        '.':
          if (c1 = '.') and (c2 = '.') then
            exit(mtX_HELLIP);
        '(':
          begin
            if (c1 = 'C') and (c2 = ')') then
              exit(mtX_COPY);
            if (c1 = 'R') and (c2 = ')') then
              exit(mtX_REG);
            if (c1 = 'T') and (c2 = 'M') and (c3 = ')') then
              exit(mtX_TRADE);
          end;
        '"':
          begin
            if (not c0.isLetterOrDigit()) and (c1 <> ' ') then
              exit(mtX_LDQUO);
            if (c0 <> ' ') and (not c1.isLetterOrDigit()) then
              exit(mtX_RDQUO);
            exit(mtNONE);
          end;
      end;
  end;
end;

procedure TEmitter.emitMarkedLines(out_: TStringBuilder; lines: TLine);
var
  s: TStringBuilder;
  line: TLine;
begin
  s := TStringBuilder.Create();
  try
    line := lines;
    while (line <> nil) do
    begin
      if (not line.isEmpty) then
      begin
//        s.append(line.value.substring(line.leading, line.value.length - line.trailing)); PSTfix
        s.Append( Copy(line.value, line.leading + 1, Length(line.value) - line.trailing));
        if (line.trailing >= 2) then
          s.append('<br />');
      end;
      if (line.next <> nil) then
        s.append(#10);
      line := line.next;
    end;
    recursiveEmitLine(out_, s.ToString(), 0, mtNONE);
  finally
    s.Free;
  end;
end;

procedure TEmitter.emitRawLines(out_: TStringBuilder; lines: TLine);
var
  s: String;
  line: TLine;
  temp: TStringBuilder;
  position, t: integer;
begin
  line := lines;
  if (FConfig.safeMode) then
  begin
    temp := TStringBuilder.Create();
    try
      while (line <> nil) do
      begin
        if (not line.isEmpty) then
          temp.append(line.value);
        temp.append(#10);
        line := line.next;
      end;
      s := temp.ToString();
      position := 0;
      while position < length(s) do
      begin
        if (s[1 + position] = '<') then
        begin
          temp.Clear;
          t := TUtils.readXML(temp, s, position, FConfig.safeMode);
          if (t <> -1) then
          begin
            out_.append(temp);
            position := t;
          end
          else
            out_.append(s[1 + position]);
        end
        else
          out_.append(s[1 + position]);
        inc(position);
      end
    finally
      temp.Free;
    end;
  end
  else
  begin
    while (line <> nil) do
    begin
      if (not line.isEmpty) then
        out_.append(line.value);
      out_.append(#10);
      line := line.next;
    end;
  end;
end;

procedure TEmitter.emitCodeLines(out_: TStringBuilder; lines: TLine; meta: String; removeIndent: boolean);
var
  line: TLine;
  list: TStringList;
  i, sp: integer;
  c: char;
begin
  line := lines;
  if (FConfig.codeBlockEmitter <> nil) then
  begin
    list := TStringList.Create;
    try
      while (line <> nil) do
      begin
        if (line.isEmpty) then
          list.add('')
        else if removeIndent then
//          list.add(line.value.substring(4)) P{STfix
          list.Add( Copy(line.value, 5))
        else
          list.add(line.value);
        line := line.next;
      end;
      FConfig.codeBlockEmitter.emitBlock(out_, list, meta);
    finally
      list.Free
    end
  end
  else
  begin
    while (line <> nil) do
    begin
      if (not line.isEmpty) then
      begin
        if removeIndent then
          sp := 4
        else
          sp := 0;
        for i := sp to Length(line.value) - 1 do
        begin
          c := line.value[1 + i];
          case c of
            '&':
              out_.append('&amp;');
            '<':
              out_.append('&lt;');
            '>':
              out_.append('&gt;');
          else
            out_.append(c);
          end;
        end;
      end;
      out_.append(#10);
      line := line.next;
    end;
  end;
end;

{ TReader }

constructor TReader.Create(source: String);
begin
  inherited Create;
  FValue := source;
  FCursor := 0;
end;

function TReader.read: char;
begin
  inc(FCursor);
  if FCursor > Length(FValue) then
    result := #0
  else
    result := FValue[FCursor];
end;

{ TUtils }

class function TUtils.skipSpaces(s: String; start: integer): integer;
var
  position: integer;
begin
  position := start;
  while (position < Length(s)) and ((s[1 + position] = ' ') or (s[1 + position] = #10)) do
    inc(position);
  if position < Length(s) then
    result := position
  else
    result := -1;
end;

class function TUtils.escape(out_: TStringBuilder; ch: char; position: integer): integer;
begin
  if CharInSet(ch, ['\', '[', ']', '(', ')', '{', '}', '#', '"', '''', '.', '>', '<', '*', '+', '-', '_', '!', '`', '^']) then
  begin
    out_.append(ch);
    result := position + 1;
  end
  else
  begin
    out_.append('\');
    result := position;
  end;
end;

class function TUtils.readUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer;
var
  position: integer;
  ch: char;
begin
  position := start;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if (ch = '\') and (position + 1 < Length(s)) then
      position := escape(out_, s[1 + position + 1], position)
    else
    begin
      if CharInSet(ch, cend) then
        break
      else
        out_.append(ch);
    end;
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readUntil(out_: TStringBuilder; s: String; start: integer; cend: char): integer;
var
  position: integer;
  ch: char;
begin
  position := start;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if (ch = '\') and (position + 1 < Length(s)) then
      position := escape(out_, s[1 + position + 1], position)
    else
    begin
      if (ch = cend) then
        break;
      out_.append(ch);
    end;
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readMdLink(out_: TStringBuilder; s: String; start: integer): integer;
var
  position, counter: integer;
  ch: char;
  endReached: boolean;
begin
  position := start;
  counter := 1;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if (ch = '\') and (position + 1 < Length(s)) then
      position := escape(out_, s[1 + position + 1], position)
    else
    begin
      endReached := false;
      case ch of
        '(':
          inc(counter);
        ' ':
          if (counter = 1) then
            endReached := true;
        ')':
          begin
            dec(counter);
            if (counter = 0) then
              endReached := true;
          end;
      end;
      if (endReached) then
        break;
      out_.append(ch);
    end;
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readMdLinkId(out_: TStringBuilder; s: String; start: integer): integer;
var
  position, counter: integer;
  ch: char;
  endReached: boolean;
begin
  position := start;
  counter := 1;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    endReached := false;
    case ch of
      #10:
        out_.append(' ');
      '[':
        begin
          inc(counter);
          out_.append(ch);
        end;
      ']':
        begin
          dec(counter);
          if (counter = 0) then
            endReached := true
          else
            out_.append(ch);
        end;
    else
      out_.append(ch);
    end;
    if (endReached) then
      break;
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readRawUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer;
var
  position: integer;
  ch: char;
begin
  position := start;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if CharInSet(ch, cend) then
      break;
    out_.append(ch);
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readRawUntil(out_: TStringBuilder; s: String; start: integer; cend: char): integer;
var
  position: integer;
  ch: char;
begin
  position := start;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if (ch = cend) then
      break;
    out_.append(ch);
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class function TUtils.readXMLUntil(out_: TStringBuilder; s: String; start: integer; cend: TSysCharSet): integer;
var
  position : integer;
  ch, stringChar: char;
  inString: boolean;
begin
  position := start;
  inString := false;
  stringChar := #0;
  while (position < Length(s)) do
  begin
    ch := s[1 + position];
    if (inString) then
    begin
      if (ch = '\') then
      begin
        out_.append(ch);
        inc(position);
        if (position < Length(s)) then
        begin
          out_.append(ch);
          inc(position);
        end;
        continue;
      end;
      if (ch = stringChar) then
      begin
        inString := false;
        out_.append(ch);
        inc(position);
        continue;
      end;
    end;
    if CharInSet(ch, ['"', '''']) then
    begin
      inString := true;
      stringChar := ch;
    end;
    if (not inString) then
    begin
      if CharInSet(ch, cend) then
        break;
    end;
    out_.append(ch);
    inc(position);
  end;
  if position = Length(s) then
    result := -1
  else
    result := position;
end;

class procedure TUtils.appendCode(out_: TStringBuilder; s: String; start: integer; e: integer);
var
  i: integer;
  c: char;
begin
  for i := start to e - 1 do
  begin
    c := s[1 + i];
    case c of
      '&':
        out_.append('&amp;');
      '<':
        out_.append('&lt;');
      '>':
        out_.append('&gt;');
    else
      out_.append(c);
    end;
  end;
end;

class procedure TUtils.appendValue(out_: TStringBuilder; s: String; start: integer; e: integer);
var
  i: integer;
  c: char;
begin
  for i := start to e - 1 do
  begin
    c := s[1 + i];
    case c of
      '&':
        out_.append('&amp;');
      '<':
        out_.append('&lt;');
      '>':
        out_.append('&gt;');
      '"':
        out_.append('&quot;');
      '''':
        out_.append('&apos;');
    else
      out_.append(c);
    end;
  end;
end;

class procedure TUtils.appendDecEntity(out_: TStringBuilder; value: char);
begin
  out_.append('&#');
  out_.append(IntToStr(ord(value)));
  out_.append(';');
end;

class procedure TUtils.appendHexEntity(out_: TStringBuilder; value: char);
begin
  out_.append('&#');
  out_.append(IntToHex(ord(value), 2));
  out_.append(';');
end;

class procedure TUtils.appendMailto(out_: TStringBuilder; s: String; start: integer; e: integer);
var
  i: integer;
  c: char;
begin
  for i := start to e - 1 do
  begin
    c := s[1 + i];
    if CharInSet(c, ['&', '<', '>', '"', '''', '@']) then
      appendHexEntity(out_, c)
    else
      out_.append(c);
  end;
end;

class procedure TUtils.getXMLTag(out_: TStringBuilder; bin: TStringBuilder);
var
  position: integer;
begin
  position := 1;
  if (bin[1] = '/') then
    inc(position);
  while (bin[position].isLetterOrDigit()) do
  begin
    out_.append(bin[position]);
    inc(position)
  end;
end;

class procedure TUtils.getXMLTag(out_: TStringBuilder; s: String);
var
  position: integer;
begin
  position := 1;
  if (s[1 + 1] = '/') then
    inc(position);
  while (s[1 + position].isLetterOrDigit()) do
  begin
    out_.append(s[1 + position]);
    inc(position)
  end;
end;

class function TUtils.readXML(out_: TStringBuilder; s: String; start: integer; safeMode: boolean): integer;
var
  position: integer;
  isCloseTag: boolean;
  temp: TStringBuilder;
  tag: String;
begin
  if (s[1 + start + 1] = '/') then
  begin
    isCloseTag := true;
    position := start + 2;
  end
  else if (s[1 + start + 1] = '!') then
  begin
    out_.append('<!');
    exit(start + 1);
  end
  else
  begin
    isCloseTag := false;
    position := start + 1;
  end;

  if (safeMode) then
  begin
    temp := TStringBuilder.Create();
    try
      position := readXMLUntil(temp, s, position, [' ', '/', '>']);
      if (position = -1) then
        exit(-1);
//      tag := temp.ToString().trim().ToLower; PSTFix
        tag := LowerCase( Trim( temp.ToString));
      if (THTML.isUnsafeHtmlElement(tag)) then
        out_.append('&lt;')
      else
        out_.append('<');
      if (isCloseTag) then
        out_.append('/');
      out_.append(temp);
    finally
      temp.Free;
    end;
  end
  else
  begin
    out_.append('<');
    if (isCloseTag) then
      out_.append('/');
    position := readXMLUntil(out_, s, position, [' ', '/', '>']);
  end;
  if (position = -1) then
    exit(-1);
  position := readXMLUntil(out_, s, position, ['/', '>']);
  if (position = -1) then
    exit(-1);

  if (s[1 + position] = '/') then
  begin
    out_.append(' /');
    position := readXMLUntil(out_, s, position + 1, ['>']);
    if (position = -1) then
      exit(-1);
  end;

  if (s[1 + position] = '>') then
  begin
    out_.append('>');
    exit(position);
  end;
  result := -1;
end;

class procedure TUtils.codeEncode(out_: TStringBuilder; value: String; offset: integer);
var
  i: integer;
  c: char;
begin
  for i := offset to Length(value) - 1 do
  begin
    c := value[1 + i];
    case c of
      '&':
        out_.append('&amp;');
      '<':
        out_.append('&lt;');
      '>':
        out_.append('&gt;');
    else
      out_.append(c);
    end;
  end;
end;

class function TUtils.getMetaFromFence(fenceLine: String): String;
var
  i: integer;
  c: char;
begin
  for i := 0 to Length(fenceLine) - 1 do
  begin
    c := fenceLine[1 + i];
    if (not c.isWhitespace()) and (c <> '`') and (c <> '~') then
//      exit(fenceLine.substring(i).trim()); PSTfix
      Exit(  Trim( Copy(fenceLine, i+1)));
  end;
  result := '';
end;

{ THTML }

class function THTML.isHtmlBlockElement(s: String): boolean;
var
  ht: THTMLElement;
begin
  ht := THTMLElement(StringToEnum(TypeInfo(THTMLElement), 'he' + s, ord(heNONE)));
  result := ht in BLOCK_ELEMENTS;
end;

class function THTML.isLinkPrefix(s: String): boolean;
begin
  result := StringsContains(LINK_PREFIXES, s);
end;

class function THTML.isEntity(s: String): boolean;
begin
  result := StringsContains(ENTITY_NAMES, s);
end;

class function THTML.isUnsafeHtmlElement(s: String): boolean;
var
  ht: THTMLElement;
begin
  ht := THTMLElement(StringToEnum(TypeInfo(THTMLElement), s, ord(heNONE)));
  result := ht in UNSAFE_ELEMENTS;
end;

{ TLine }

procedure TLine.Init();
begin
  FLeading := 0;
  while (leading < Length(value)) and (value[1 + leading] = ' ') do
    inc(FLeading);

  if (leading = Length(value)) then
    setEmpty()
  else
  begin
    isEmpty := false;
    trailing := 0;
    while (value[1 + Length(value) - trailing - 1] = ' ') do
      inc(FTrailing);
  end;
end;

procedure TLine.InitLeading();
begin
  FLeading := 0;
  while (leading < Length(value)) and (value[1 + leading] = ' ') do
    inc(FLeading);
  if (leading = Length(value)) then
    setEmpty();
end;

// TODO use Util#skipSpaces
function TLine.skipSpaces(): boolean;
begin
  while (position < Length(value)) and (value[1 + position] = ' ') do
    inc(FPosition);
  result := position < Length(value);
end;

// TODO use Util#readUntil
function TLine.readUntil(chend: TSysCharSet): String;
var
  sb: TStringBuilder;
  position: integer;
  ch, c: char;
begin
  sb := TStringBuilder.Create();
  try
    position := self.position;
    while (position < Length(value)) do
    begin
      ch := value[1 + position];
      if (ch = '\') and (position + 1 < Length(value)) then
      begin
        c := value[1 + position + 1];
        if CharInSet(c, ['\', '[', ']', '(', ')', '{', '}', '#', '"', '''', '.', '>', '*', '+', '-', '_', '!', '`', '~']) then
        begin
          sb.append(c);
          inc(FPosition);
        end
        else
        begin
          sb.append(ch);
          break;
        end;
      end
      else if CharInSet(ch, chend) then
        break
      else
        sb.append(ch);
      inc(position);
    end;

    if (position < Length(value)) then
      ch := value[1 + position]
    else
      ch := #10;
    if CharInSet(ch, chend) then
    begin
      self.position := position;
      result := sb.ToString();
    end
    else
      result := '';
  finally
    sb.Free;
  end;
end;

procedure TLine.setEmpty();
begin
  value := '';
  leading := 0;
  trailing := 0;
  isEmpty := true;
  if (previous <> nil) then
    previous.nextEmpty := true;
  if (next <> nil) then
    next.prevEmpty := true;
end;

function TLine.countChars(ch: char): integer;
var
  count, i: integer;
  c: char;
begin
  count := 0;
  for i := 0 to Length(value) - 1 do
  begin
    c := value[1 + i];
    if (c = ' ') then
      continue;
    if (c = ch) then
    begin
      inc(count);
      continue;
    end;
    count := 0;
    break;
  end;
  result := count;
end;

function TLine.countCharsStart(ch: char; allowSpaces: boolean): integer;
var
  count, i: integer;
  c: char;
begin
  count := 0;
  for i := 0 to Length(value) - 1 do
  begin
    c := value[1 + i];
    if (c = ' ') and (allowSpaces) then
    begin
      continue;
    end;
    if (c = ch) then
      inc(count)
    else
      break;
  end;
  result := count;
end;

function TLine.getLineType(configuration: TConfiguration): TLineType;
var
  i: integer;
begin
  if (isEmpty) then
    exit(ltEMPTY);

  if (leading > 3) then
    exit(ltCODE);

  if (value[1 + leading] = '#') then
    exit(ltHEADLINE);

  if (value[1 + leading] = '>') then
    exit(ltBQUOTE);

  if (configuration.forceExtendedProfile) then
  begin
    if (Length(value) - leading - trailing > 2) then
    begin
      if (value[1 + leading] = '`') and (countCharsStart('`', configuration.allowSpacesInFencedDelimiters) >= 3) then
        exit(ltFENCED_CODE);
      if (value[1 + leading] = '~') and (countCharsStart('~', configuration.allowSpacesInFencedDelimiters) >= 3) then
        exit(ltFENCED_CODE);
    end;
  end;

  if (Length(value) - leading - trailing > 2) and ((value[1 + leading] = '*') or (value[1 + leading] = '-') or (value[1 + leading] = '_')) then
  begin
    if (countChars(value[1 + leading]) >= 3) then
      exit(ltHR);
  end;

  if (Length(value) - leading >= 2) and (value[1 + leading + 1] = ' ') then
  begin
    if CharInSet(value[1 + leading], ['*', '-', '+']) then
      exit(ltULIST);
  end;

  if (Length(value) - leading >= 3) and (value[1 + leading].isDigit()) then
  begin
    i := leading + 1;
    while (i < Length(value)) and (value[1 + i].isDigit()) do
      inc(i);
    if (i + 1 < Length(value)) and (value[1 + i] = '.') and (value[1 + i + 1] = ' ') then
      exit(ltOLIST);
  end;

  if (value[1 + leading] = '<') then
  begin
    if (checkHTML()) then
      exit(ltXML);
  end;

  if (next <> nil) and (not next.isEmpty) then
  begin
    if ((next.value[1 + 0] = '-')) and ((next.countChars('-') > 0)) then
      exit(ltHEADLINE2);
    if ((next.value[1 + 0] = '=')) and ((next.countChars('=') > 0)) then
      exit(ltHEADLINE1);
  end;

  exit(ltOTHER);
end;

function TLine.readXMLComment(firstLine: TLine; start: integer): integer;
var
  line: TLine;
  position: integer;
begin
  line := firstLine;
  if (start + 3 < Length(line.value)) then
  begin
    if (line.value[1 + 2] = '-') and (line.value[1 + 3] = '-') then
    begin
      position := start + 4;
      while (line <> nil) do
      begin
        while (position < Length(line.value)) and (line.value[1 + position] <> '-') do
          inc(position);
        if (position = Length(line.value)) then
        begin
          line := line.next;
          position := 0;
        end
        else
        begin
          if (position + 2 < Length(line.value)) then
          begin
            if (line.value[1 + position + 1] = '-') and (line.value[1 + position + 2] = '>') then
            begin
              xmlEndLine := line;
              exit(position + 3);
            end;
          end;
          inc(position);
        end;
      end;
    end;
  end;
  exit(-1);
end;

// FIXME ... hack
function TLine.stripID(): String;
var
  p, start: integer;
  found: boolean;
  id: String;
begin
  if (isEmpty or (value[1 + Length(value) - trailing - 1] <> '}')) then
    exit('');

  p := leading;
  found := false;
  while (p < Length(value)) and (not found) do
  begin
    case value[1 + p] of
      '\':
        begin
          if (p + 1 < Length(value)) then
          begin
            if (value[1 + p + 1]) = '{' then
            begin
              inc(p);
              break;
            end;
          end;
          inc(p);
          break;
        end;
      '{':
        begin
          found := true;
          break;
        end
    else
      begin
        inc(p);
        break;
      end;
    end;
  end;

  if (found) then
  begin
    if (p + 1 < Length(value)) and (value[1 + p + 1] = '#') then
    begin
      start := p + 2;
      p := start;
      found := false;
      while (p < Length(value)) and (not found) do
      begin
        case (value[1 + p]) of
          '\':
            begin
              if (p + 1 < Length(value)) then
              begin
                if (value[1 + p + 1]) = '}' then
                begin
                  inc(p);
                  break;
                end;
              end;
              inc(p);
              break;
            end;
          '}':
            begin
              found := true;
              break;
            end;
        else
          begin
            inc(p);
            break;
          end;
        end;

        if (found) then
        begin
//          id := value.substring(start, p).trim(); PSTfix
          id := Trim( Copy(value, start + 1, p));
          if (leading <> 0) then
          begin
//            value := value.substring(0, leading) + value.substring(leading, start - 2).trim(); PSTfix
            value := Copy(value, 1, leading) + Trim( Copy( value, leading + 1, start -2));
          end
          else
          begin
//            value := value.substring(leading, start - 2).trim();  PSTFix
            value := Trim( Copy(value, leading +1, start -2));
          end;
          trailing := 0;
          if (Length(id) > 0) then
            exit(id)
          else
            exit('');
        end;
      end;
    end;
  end;
  exit('');
end;

function TLine.checkHTML: boolean;
var
  tags: TStringList;
  temp: TStringBuilder;
  element, tag: String;
  line: TLine;
  newPos: integer;
begin
  result := false;
  tags := TStringList.Create();
  temp := TStringBuilder.Create();
  try
    position := leading;
    if (value.length >= 1 + leading + 1) and (value[1 + leading + 1] = '!') then
    begin
      if (readXMLComment(self, leading) > 0) then
      begin
        exit(true);
      end;
    end;
    position := TUtils.readXML(temp, value, leading, false);
    if (position > -1) then
    begin
      element := temp.ToString();
      temp.Clear;
      TUtils.getXMLTag(temp, element);
      tag := LowerCase(temp.ToString());
      if (not THTML.isHtmlBlockElement(tag)) then
        exit(false);
//      if (tag.equals('hr') or element.endsWith('/>')) then PSTFix
      if (tag = 'hr') or AnsiEndsText('/>', element) then

      begin
        xmlEndLine := self;
        exit(true);
      end;
      tags.add(tag);

      line := self;
      while (line <> nil) do
      begin
        while (position < Length(line.value)) and (line.value[1 + position] <> '<') do
          inc(FPosition);
        if (position >= Length(line.value)) then
        begin
          line := line.next;
          position := 0;
        end
        else
        begin
          temp.Clear;
          newPos := TUtils.readXML(temp, line.value, position, false);
          if (newPos > 0) then
          begin
            element := temp.ToString();
            temp.Clear;
            TUtils.getXMLTag(temp, element);
            tag := LowerCase(temp.ToString());
            if (THTML.isHtmlBlockElement(tag)) and (tag <> 'hr') and (not AnsiEndsText('/>', element)) then
            begin
              if (element[1 + 1] = '/') then
              begin
                if (tags[tags.Count - 1] <> tag) then
                  exit(false);
                tags.Delete(tags.count - 1);
              end
              else
                tags.add(tag);
            end;
            if (tags.count = 0) then
            begin
              xmlEndLine := line;
              break;
            end;
            position := newPos;
          end
          else
          begin
            inc(FPosition);
          end;
        end;
      end;
      result := tags.count = 0;
    end;
  finally
    temp.Free;
    tags.Free;
  end;
end;

{ TLinkRef }

constructor TLinkRef.Create(link, title: String; isAbbrev: boolean);
begin
  inherited Create;
  FLink := link;
  FTitle := title;
  FIsAbbrev := isAbbrev;
end;

{ TBlock }

constructor TBlock.Create;
begin
  inherited;
end;

destructor TBlock.Destroy;
begin
  FLines.Free;
  FBlocks.Free;
  FNext.free;
  inherited;
end;

procedure TBlock.AppendLine(line: TLine);
begin
  if (self.lineTail = nil) then
  begin
    self.FLines := line;
    self.FLineTail := line;
  end
  else
  begin
    self.lineTail.nextEmpty := line.isEmpty;
    line.prevEmpty := self.lineTail.isEmpty;
    line.previous := self.lineTail;
    self.lineTail.next := line;
    self.FLineTail := line;
  end;

end;

procedure TBlock.expandListParagraphs;
var
  outer: TBlock;
  inner: TBlock;
  hasParagraph: boolean;
begin
  if (self.type_ <> btORDERED_LIST) and (self.type_ <> btUNORDERED_LIST) then
    exit;

  outer := self.blocks;
  hasParagraph := false;
  while (outer <> nil) and (not hasParagraph) do
  begin
    if (outer.type_ = btLIST_ITEM) then
    begin
      inner := outer.blocks;
      while (inner <> nil) and (not hasParagraph) do
      begin
        if (inner.type_ = btPARAGRAPH) then
        begin
          hasParagraph := true;
        end;
        inner := inner.next;
      end;
    end;
    outer := outer.next;
  end;

  if (hasParagraph) then
  begin
    outer := self.blocks;
    while (outer <> nil) do
    begin
      if (outer.type_ = btLIST_ITEM) then
      begin
        inner := outer.blocks;
        while (inner <> nil) do
        begin
          if (inner.type_ = btNONE) then
          begin
            inner.type_ := btPARAGRAPH;
          end;
          inner := inner.next;
        end;
      end;
      outer := outer.next;
    end;
  end;
end;

function TBlock.hasLines: boolean;
begin
  result := lines <> nil;
end;

procedure TBlock.removeLine(line: TLine);
begin
  if (line.previous = nil) then
  begin
    self.FLines := line.next;
  end
  else
  begin
    line.previous.next := line.next;
  end;

  if (line.next = nil) then
  begin
    self.FLineTail := line.previous;
  end
  else
  begin
    line.next.previous := line.previous;
  end;
  line.previous := nil;

  line.next := nil;
  line.free;
end;

procedure TBlock.removeBlockQuotePrefix;
var
  line: TLine;
  rem: integer;
begin
  line := self.lines;
  while (line <> nil) do
  begin
    if (not line.isEmpty) then
    begin
      if (line.value[1 + line.leading] = '>') then
      begin
        rem := line.leading + 1;
        if (line.leading + 1 < Length(line.value)) and (line.value[1 + line.leading + 1] = ' ') then
        begin
          inc(rem);
        end;
        line.value := Copy(line.value, rem+1);
        line.InitLeading();
      end;
    end;
    line := line.next;
  end;
end;

function TBlock.removeLeadingEmptyLines: boolean;
var
  wasEmpty: boolean;
  line: TLine;
begin
  wasEmpty := false;
  line := self.lines;
  while (line <> nil) and (line.isEmpty) do
  begin
    self.removeLine(line);
    line := self.lines;
    wasEmpty := true;
  end;
  result := wasEmpty;

end;

procedure TBlock.removeTrailingEmptyLines;
var
  line: TLine;
begin
  line := self.lineTail;
  while (line <> nil) and (line.isEmpty) do
  begin
    self.removeLine(line);
    line := self.lineTail;
  end;
end;

procedure TBlock.removeListIndent(config: TConfiguration);
var
  line: TLine;
begin
  line := self.lines;
  while (line <> nil) do
  begin
    if (not line.isEmpty) then
    begin
      case (line.getLineType(config)) of
        ltULIST:
//          line.value := line.value.substring(line.leading + 2); PSTfix
          line.value := Copy(line.value, line.leading +3);
        ltOLIST:
//          line.value := line.value.substring(line.value.indexOf('.') + 2); pstfix
        line.value := Copy(line.value, pos('.', line.value) + 2);
      else
//        line.value := line.value.substring(Math.min(line.leading, 4)); pstfix
        line.value := Copy(line.value, Math.Min(line.leading + 1, 5));
      end;
      line.InitLeading();
    end;
    line := line.next;
  end;

end;

procedure TBlock.removeSurroundingEmptyLines;
begin
  if (self.lines <> nil) then
  begin
    self.removeTrailingEmptyLines();
    self.removeLeadingEmptyLines();
  end;

end;

function TBlock.split(line: TLine): TBlock;
var
  block: TBlock;
begin
  block := TBlock.Create();
  block.FLines := self.lines;
  block.FLineTail := line;
  self.FLines := line.next;
  line.next := nil;
  if (self.lines = nil) then
  begin
    self.FLineTail := nil;
  end
  else
  begin
    self.lines.previous := nil;
  end;

  if (self.blocks = nil) then
  begin
    self.FBlocks := block;
    self.FBlockTail := block;
  end
  else
  begin
    self.blockTail.next := block;
    self.FBlockTail := block;
  end;
  result := block;
end;

procedure TBlock.transfromHeadline;
var
  level, start, end_: integer;
  line: TLine;
begin
  if (self.hlDepth > 0) then
  begin
    exit;
  end;
  level := 0;
  line := self.lines;
  if (line.isEmpty) then
  begin
    exit;
  end;
  start := line.leading;
  while (start < Length(line.value)) and (line.value[1 + start] = '#') do
  begin
    inc(level);
    inc(start);
  end;
  while (start < Length(line.value)) and (line.value[1 + start] = ' ') do
  begin
    inc(start);
  end;
  if (start >= Length(line.value)) then
  begin
    line.setEmpty();
  end
  else
  begin
    end_ := Length(line.value) - line.trailing - 1;
    while (line.value[1 + end_] = '#') do
    begin
      dec(end_);
    end;
    while (line.value[1 + end_] = ' ') do
    begin
      dec(end_);
    end;
    line.value := Copy(line.value, start+1, end_-start+1);
    line.leading := 0;
    line.trailing := 0;
  end;
  self.hlDepth := Math.min(level, 6);

end;

{$IFDEF FPC}
{ TStringBuilder }

constructor TStringBuilder.Create;
begin
  Inherited;
  FBufferSize := BUFFER_INCREMENT_SIZE;
end;

procedure TStringBuilder.Append(value: TStringBuilder);
begin
  append(value.ToString);
end;

procedure TStringBuilder.Append(value: integer);
begin
  append(inttostr(value));
end;

procedure TStringBuilder.Append(value: String);
begin
  If (value <> '') Then
  Begin
    If FLength + System.Length(value) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + Math.Max(FBufferSize, System.Length(value)));

    Move(value[1], FContent[FLength + 1], System.Length(value) * SizeOf(Char));

    Inc(FLength, System.Length(value));
  End;
end;

procedure TStringBuilder.Clear;
begin
  FContent := '';
  FLength := 0;
end;

function TStringBuilder.GetChar(index: integer): char;
begin
  if (index < 0) or (index >= Length) then
    raise Exception.Create('Out of bounds');
  result := FContent[index+1];
end;


function TStringBuilder.toString: String;
begin
  Result := Copy(FContent, 1, FLength);
end;

{$ENDIF}

end.
