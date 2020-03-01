# delphi-markdown

Markdown Processor for Pascal (Delphi/FPC)

## Basic Information

This is a Pascal (Delphi) library that processes to markdown to HTML.
At present the following dialects of markdown are supported:

* The Daring Fireball dialect (see https://daringfireball.net/projects/markdown/) (translated from https://github.com/rjeschke/txtmark)
* The CommonMark Specification (see https://spec.commonmark.org/0.28) (not quite complete. Stil to do: link references)
* Github Flavored Markdown

All you need to use the library is any unicode version of Delphi, or FPC (lazarus). To run the 
tests, you also need FastMemoryManager, the Jedi Code Library and DUnitX.

## Using the Library

Create a TMarkdownProcessor (MarkdownProcessor.pas) of the dialect you want:

     var
       md : TMarkdownProcessor;
  
       md := TMarkdownProcessor.createDialect(mdDaringFireball)
  
Decide whether you want to allow active content (not supported for CommonMark)

       md.UnSafe := true;
  
Note: you should only set this to true if you *need* to - active content can be a signficant safety/security issue.  
 
Generate HTML from the Markdown content:

       html := md.process(markdown); 
  
Note that the HTML returned is an HTML fragment, not a full HTML page.

Internally, the library includes a comprehensive set of unit tests. For CommonMark,
the tests are those provided as part of the CommonMark spec.

## License

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
