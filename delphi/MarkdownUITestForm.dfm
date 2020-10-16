object MarkDownUITest: TMarkDownUITest
  Left = 0
  Top = 0
  Caption = 'MarkDown UI Tester'
  ClientHeight = 636
  ClientWidth = 937
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 417
    Top = 41
    Width = 8
    Height = 595
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 417
    Height = 595
    Align = alLeft
    Lines.Strings = (
      '# MarkDown Processor Capabilities'
      ''
      '**ATTENTION:** *Not* all options supported, see results.'
      ''
      '## List'
      ''
      '- entry 1'
      '  - entry 2'
      '    - entry 3'
      '- entry 4'
      ''
      '## Enumeration'
      ''
      '1. Entry 1'
      '  1. Entry 2'
      '  1. Entry 3'
      '    1. Entry 4'
      '1. Entry 5'
      ''
      '## Quoting'
      ''
      '> Single'
      ''
      '> > Double'
      ''
      '> > > tripple'
      ''
      '## Hyperlinks'
      ''
      '[This is something to point to](https://www.domain.tld)'
      ''
      '## Fontdesign'
      ''
      'What about **bold**, *italic*, ***bold italic*** `inline code` '
      ''
      'A linefeed without paragraph     '
      
        'by placing five or more spaces before a linebreak without empty ' +
        'line next.'
      ''
      '## Images'
      ''
      '![Alt-Text](/pathto/image.jpg "Title")'
      ''
      ''
      '## Tables'
      ''
      '| Table Header | Table Header | Table Header |'
      '| :--- | :---: | ---: |'
      '| Table Entry | Table Entry | Table Entry |'
      '| Table Entry | Table Entry | Table Entry |'
      '| Table Entry | Table Entry | Table Entry |'
      '| Table Entry | Table Entry | Table Entry |'
      ''
      '## Line'
      ''
      '***'
      ''
      '## Code'
      ''
      '```'
      'This is a code sample'
      '```'
      ''
      ''
      ''
      '## Currently NOT Supported'
      ''
      '### Description/Definition'
      ''
      'Keyword'
      ': Description'
      ': Description'
      ''
      'Keyword'
      ': Description'
      ''
      '### Footnotes'
      ''
      'Details see footnote[^fn]'
      ''
      '[^fn]: This is a footnote.'
      ''
      '### Strike'
      ''
      '~~strike this!~~'
      ''
      '## Superscript'
      ''
      '2^10'
      ''
      '### Subscript'
      ''
      'H~2~O'
      ''
      '### EM-Dash'
      ''
      '---'
      ''
      '### EN-Dash'
      ''
      '--')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 937
    Height = 41
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 144
      Top = 9
      Width = 131
      Height = 25
      Caption = 'Common Mark >'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 7
      Top = 9
      Width = 131
      Height = 25
      Caption = 'DaringFireball >'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 425
    Top = 41
    Width = 512
    Height = 595
    Align = alClient
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 1
      Top = 289
      Width = 510
      Height = 8
      Cursor = crVSplit
      Align = alTop
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 510
      Height = 288
      Align = alTop
      TabOrder = 0
      object Memo2: TMemo
        Left = 1
        Top = 1
        Width = 508
        Height = 286
        Align = alClient
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 297
      Width = 510
      Height = 297
      Align = alClient
      TabOrder = 1
      object WebBrowser1: TWebBrowser
        Left = 1
        Top = 1
        Width = 508
        Height = 295
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 210
        ExplicitTop = 94
        ExplicitWidth = 308
        ExplicitHeight = 244
        ControlData = {
          4C000000813400007D1E00000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
end
