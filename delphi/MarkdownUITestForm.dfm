object MarkDownUITest: TMarkDownUITest
  Left = 0
  Top = 0
  Caption = 'MarkDownUITest'
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
  object Memo1: TMemo
    Left = 32
    Top = 40
    Width = 345
    Height = 561
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
  object Memo2: TMemo
    Left = 520
    Top = 40
    Width = 385
    Height = 561
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 383
    Top = 72
    Width = 131
    Height = 25
    Caption = 'Common Mark >'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 383
    Top = 120
    Width = 131
    Height = 25
    Caption = 'DaringFireball >'
    TabOrder = 3
    OnClick = Button2Click
  end
end
