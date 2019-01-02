# delphi-markdown

Markdown Processor for Delphi. 

## Basic Information

This is a Pascal (Delphi) library that processes to markdown to HTML.
At present the following dialects of markdown are supported:

* The Daring Fireball dialect (see https://daringfireball.net/projects/markdown/) (translated from https://github.com/rjeschke/txtmark)

Wishlist: PEGDown (Github dialect), CommonMark

All you need to use the library is any unicode version of Delphi. To run the 
tests, you also need FastMemoryManager and the Jedi Code Library

## Using the Library

Create a TMarkdownProcessor (MarkdownProcessor.pas) of the dialect you want:

     var
       md : TMarkdownProcessor;
  
       md := TMarkdownProcessor.createDialect(mdDaringFireball)
  
Decide whether you want to allow active content

       md.UnSafe := true;
  
Note: you should only set this to true if you *need* to - active content can be a signficant safety/security issue.  
 
Generate HTML fragments from Markdown content:

       html := md.process(markdown); 
  
Note that the HTML returned is an HTML fragment, not a full HTML page.

## License

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

The DaringFireball Implementation is a port of TxtMark. Copyright (C) 
2011-2015 René Jeschke <rene_jeschke@yahoo.de>. See 
https://github.com/rjeschke/txtmark, also covered by Apache 2.0.

