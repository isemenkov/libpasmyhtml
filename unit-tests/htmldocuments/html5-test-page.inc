const
  Html5TestPage =
    '<!doctype html>'                                             + sLineBreak +
    '<html lang="en">'                                            + sLineBreak +
      '<head>'                                                    + sLineBreak +
        '<meta charset="utf-8">'                                  + sLineBreak +
        '<meta name="viewport" content="width=device-width, '     +
          'initial-scale=1.0">'                                   + sLineBreak +
        '<title>HTML5 Test Page</title>'                          + sLineBreak +
      '</head>'                                                   + sLineBreak +
      '<body>'                                                    + sLineBreak +
        '<div id="top" class="page" role="document">'             + sLineBreak +
          '<header role="banner">'                                + sLineBreak +
            '<h1>HTML5 Test Page</h1>'                            + sLineBreak +
            '<p>This is a test page filled with common HTML '     +
            'elements to be used to provide visual feedback '     +
            'whilst building CSS systems and frameworks.</p>'     + sLineBreak +
          '</header>'                                             + sLineBreak +
          '<nav role="navigation">'                               + sLineBreak +
            '<ul>'                                                + sLineBreak +
              '<li>'                                              + sLineBreak +
                '<a href="#text">Text</a>'                        + sLineBreak +
                  '<ul>'                                          + sLineBreak +
                    '<li><a href="#text__headings">Headings'      +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#text__paragraphs">Paragraphs'  +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#text__blockquotes">Blockquotes'+
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#text__lists">Lists</a></li>'   + sLineBreak +
                    '<li><a href="#text__hr">Horizontal rules</a>'+
                      '</li>'                                     + sLineBreak +
                    '<li><a href="#text__tables">Tabular data</a>'+
                      '</li>'                                     + sLineBreak +
                    '<li><a href="#text__code">Code</a></li>'     + sLineBreak +
                    '<li><a href="#text__inline">Inline elements' +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#text__comments">HTML Comments' +
                      '</a></li>'                                 + sLineBreak +
                  '</ul>'                                         + sLineBreak +
                '</li>'                                           + sLineBreak +
                '<li>'                                            + sLineBreak +
                  '<a href="#embedded">Embedded content</a>'      + sLineBreak +
                    '<ul>'                                        + sLineBreak +
                      '<li><a href="#embedded__images">Images</a>'+
                        '</li>'                                   + sLineBreak +
                      '<li><a href="#embedded__audio">Audio</a>'  +
                        '</li>'                                   + sLineBreak +
                      '<li><a href="#embedded__video">Video</a>'  +
                        '</li>'                                   + sLineBreak +
                      '<li><a href="#embedded__canvas">Canvas</a>'+
                        '</li>'                                   + sLineBreak +
                      '<li><a href="#embedded__meter">Meter</a>'  +
                        '</li>'                                   + sLineBreak +
                      '<li><a href="#embedded__progress">Progress'+
                        '</a></li>'                               + sLineBreak +
                      '<li><a href="#embedded__svg">Inline SVG'   +
                        '</a></li>'                               + sLineBreak +
                      '<li><a href="#embedded__iframe">IFrames'   +
                        '</a></li>'                               + sLineBreak +
                    '</ul>'                                       + sLineBreak +
                '</li>'                                           + sLineBreak +
                '<li>'                                            + sLineBreak +
                  '<a href="#forms">Form elements</a>'            + sLineBreak +
                  '<ul>'                                          + sLineBreak +
                    '<li><a href="#forms__input">Input fields'    +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__select">Select menus'   +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__checkbox">Checkboxes'   +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__radio">Radio buttons'   +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__textareas">Textareas'   +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__html5">HTML5 inputs'    +
                      '</a></li>'                                 + sLineBreak +
                    '<li><a href="#forms__action">Action '        +
                      'buttons</a></li>'                          + sLineBreak +
                '</ul>'                                           + sLineBreak +
              '</li>'                                             + sLineBreak +
            '</ul>'                                               + sLineBreak +
          '</nav>'                                                + sLineBreak +
          '<main role="main">'                                    + sLineBreak +
            '<section id="text">'                                 + sLineBreak +
              '<header><h1>Text</h1></header>'                    + sLineBreak +
              '<article id="text__headings">'                     + sLineBreak +
              '<header>'                                          + sLineBreak +
                '<h1>Headings</h1>'                               + sLineBreak +
              '</header>'                                         + sLineBreak +
              '<div>'                                             + sLineBreak +
                '<h1>Heading 1</h1>'                              + sLineBreak +
                '<h2>Heading 2</h2>'                              + sLineBreak +
                '<h3>Heading 3</h3>'                              + sLineBreak +
                '<h4>Heading 4</h4>'                              + sLineBreak +
                '<h5>Heading 5</h5>'                              + sLineBreak +
                '<h6>Heading 6</h6>'                              + sLineBreak +
              '</div>'                                            + sLineBreak +
              '<footer><p><a href="#top">[Top]</a></p></footer>'  + sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__paragraphs">'                   + sLineBreak +
                '<header><h1>Paragraphs</h1></header>'            + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<p>A paragraph (from the Greek paragraphos, '  +
                  '“to write beside” or “written beside”) is a '  +
                  'self-contained unit of a discourse in writing '+
                  'dealing with a particular point or idea. A '   +
                  'paragraph consists of one or more sentences. ' +
                  'Though not required by the syntax of any '     +
                  'language, paragraphs are usually an expected ' +
                  'part of formal writing, used to organize '     +
                  'longer prose.</p>'                             + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__blockquotes">'                  + sLineBreak +
                '<header><h1>Blockquotes</h1></header>'           + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<blockquote>'                                  + sLineBreak +
                    '<p>A block quotation (also known as a '      +
                    'long quotation or extract) is a quotation '  +
                    'in a written document, that is set off '     +
                    'from the main text as a paragraph, or '      +
                    'block of text.</p>'                          + sLineBreak +
                    '<p>It is typically distinguished visually '  +
                    'using indentation and a different typeface'  +
                    ' or smaller size quotation. It may or may '  +
                    'not include a citation, usually placed at '  +
                    'the bottom.</p>'                             + sLineBreak +
                    '<cite><a href="#!">Said no one, ever.</a>'   +
                    '</cite>'                                     + sLineBreak +
                  '</blockquote>'                                 + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p>'         +
                '</footer>'                                       + sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__lists">'                        + sLineBreak +
                '<header><h1>Lists</h1></header>'                 + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<h3>Definition list</h3>'                      + sLineBreak +
                  '<dl>'                                          + sLineBreak +
                    '<dt>Definition List Title</dt>'              + sLineBreak +
                    '<dd>This is a definition list division.'     +
                    '</dd>'                                       + sLineBreak +
                  '</dl>'                                         + sLineBreak +
                  '<h3>Ordered List</h3>'                         + sLineBreak +
                  '<ol>'                                          + sLineBreak +
                    '<li>List Item 1</li>'                        + sLineBreak +
                    '<li>List Item 2</li>'                        + sLineBreak +
                    '<li>List Item 3</li>'                        + sLineBreak +
                  '</ol>'                                         + sLineBreak +
                  '<h3>Unordered List</h3>'                       + sLineBreak +
                  '<ul>'                                          + sLineBreak +
                    '<li>List Item 1</li>'                        + sLineBreak +
                    '<li>List Item 2</li>'                        + sLineBreak +
                    '<li>List Item 3</li>'                        + sLineBreak +
                  '</ul>'                                         + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p>'         +
                '</footer>'                                       + sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__hr">'                           + sLineBreak +
                '<header><h1>Horizontal rules</h1></header>'      + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<hr>'                                          + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__tables">'                       + sLineBreak +
                '<header><h1>Tabular data</h1></header>'          + sLineBreak +
                '<table>'                                         + sLineBreak +
                  '<caption>Table Caption</caption>'              + sLineBreak +
                  '<thead>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<th>Table Heading 1</th>'                  + sLineBreak +
                      '<th>Table Heading 2</th>'                  + sLineBreak +
                      '<th>Table Heading 3</th>'                  + sLineBreak +
                      '<th>Table Heading 4</th>'                  + sLineBreak +
                      '<th>Table Heading 5</th>'                  + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                  '</thead>'                                      + sLineBreak +
                  '<tfoot>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<th>Table Footer 1</th>'                   + sLineBreak +
                      '<th>Table Footer 2</th>'                   + sLineBreak +
                      '<th>Table Footer 3</th>'                   + sLineBreak +
                      '<th>Table Footer 4</th>'                   + sLineBreak +
                      '<th>Table Footer 5</th>'                   + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                  '</tfoot>'                                      + sLineBreak +
                  '<tbody>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>Table Cell 1</td>'                     + sLineBreak +
                      '<td>Table Cell 2</td>'                     + sLineBreak +
                      '<td>Table Cell 3</td>'                     + sLineBreak +
                      '<td>Table Cell 4</td>'                     + sLineBreak +
                      '<td>Table Cell 5</td>'                     + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>Table Cell 1</td>'                     + sLineBreak +
                      '<td>Table Cell 2</td>'                     + sLineBreak +
                      '<td>Table Cell 3</td>'                     + sLineBreak +
                      '<td>Table Cell 4</td>'                     + sLineBreak +
                      '<td>Table Cell 5</td>'                     + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>Table Cell 1</td>'                     + sLineBreak +
                      '<td>Table Cell 2</td>'                     + sLineBreak +
                      '<td>Table Cell 3</td>'                     + sLineBreak +
                      '<td>Table Cell 4</td>'                     + sLineBreak +
                      '<td>Table Cell 5</td>'                     + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>Table Cell 1</td>'                     + sLineBreak +
                      '<td>Table Cell 2</td>'                     + sLineBreak +
                      '<td>Table Cell 3</td>'                     + sLineBreak +
                      '<td>Table Cell 4</td>'                     + sLineBreak +
                      '<td>Table Cell 5</td>'                     + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                  '</tbody>'                                      + sLineBreak +
                '</table>'                                        + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__code">'                         + sLineBreak +
                '<header><h1>Code</h1></header>'                  + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<p><strong>Keyboard input:</strong> <kbd>Cmd'  +
                  '</kbd></p>'                                    + sLineBreak +
                  '<p><strong>Inline code:</strong> <code>&lt;div'+
                  '&gt;code&lt;/div&gt;</code></p>'               + sLineBreak +
                  '<p><strong>Sample output:</strong> <samp>This '+
                  'is sample output from a computer program.'     +
                  '</samp></p>'                                   + sLineBreak +
                  '<h2>Pre-formatted text</h2>'                   + sLineBreak +
                  '<pre>P R E F O R M A T T E D T E X T'          + sLineBreak +
                  '! " # $ % &amp; '' ( ) * + , - . /'            + sLineBreak +
                  '0 1 2 3 4 5 6 7 8 9 : ; &lt; = &gt; ?'         + sLineBreak +
                  '@ A B C D E F G H I J K L M N O'               + sLineBreak +
                  'P Q R S T U V W X Y Z [ \ ] ^ _'               + sLineBreak +
                  '` a b c d e f g h i j k l m n o'               + sLineBreak +
                  'p q r s t u v w x y z { | } ~ </pre>'          + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__inline">'                       + sLineBreak +
                '<header><h1>Inline elements</h1></header>'       + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<p><a href="#!">This is a text link</a>.</p>'  + sLineBreak +
                  '<p><strong>Strong is used to indicate strong'  +
                  ' importance.</strong></p>'                     + sLineBreak +
                  '<p><em>This text has added emphasis.</em>'     +
                  '</p>'                                          + sLineBreak +
                  '<p>The <b>b element</b> is stylistically '     +
                  'different text from normal text, without '     +
                  'any special importance.</p>'                   + sLineBreak +
                  '<p>The <i>i element</i> is text that is '      +
                  'offset from the normal text.</p>'              + sLineBreak +
                  '<p>The <u>u element</u> is text with an '      +
                  'unarticulated, though explicitly rendered, '   +
                  'non-textual annotation.</p>'                   + sLineBreak +
                  '<p><del>This text is deleted</del> and <ins>'  +
                  'This text is inserted</ins>.</p>'              + sLineBreak +
                  '<p><s>This text has a strikethrough</s>.</p>'  + sLineBreak +
                  '<p>Superscript<sup>®</sup>.</p>'               + sLineBreak +
                  '<p>Subscript for things like H<sub>2</sub>O.'  +
                  '</p>'                                          + sLineBreak +
                  '<p><small>This small text is small for for '   +
                  'fine print, etc.</small></p>'                  + sLineBreak +
                  '<p>Abbreviation: <abbr title="HyperText '      +
                  'Markup Language">HTML</abbr></p>'              + sLineBreak +
                  '<p><q cite="https://developer.mozilla.org/'    +
                  'en-US/docs/HTML/Element/q">This text is a '    +
                  'short inline quotation.</q></p>'               + sLineBreak +
                  '<p><cite>This is a citation.</cite></p>'       + sLineBreak +
                  '<p>The <dfn>dfn element</dfn> indicates a '    +
                  'definition.</p>'                               + sLineBreak +
                  '<p>The <mark>mark element</mark> indicates '   +
                  'a highlight.</p>'                              + sLineBreak +
                  '<p>The <var>variable element</var>, such as '  +
                  '<var>x</var> = <var>y</var>.</p>'              + sLineBreak +
                  '<p>The time element: <time datetime="2013-04'  +
                  '-06T12:32+00:00">2 weeks ago</time></p>'       + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p>'         +
                '</footer>'                                       + sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="text__comments">'                     + sLineBreak +
                '<header><h1>HTML Comments</h1></header>'         + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<p>There is comment here: <!--This comment '   +
                  'should not be displayed--></p>'                + sLineBreak +
                  '<p>There is a comment spanning multiple '      +
                  'tags and lines below here.</p>'                + sLineBreak +
                  '<!--<p><a href="#!">This is a text link. '     +
                  'But it should not be displayed in a comment'   +
                  '</a>.</p>'                                     + sLineBreak +
                  '<p><strong>Strong is used to indicate '        +
                  'strong importance. But, it should not be '     +
                  'displayed in a comment</strong></p>'           + sLineBreak +
                  '<p><em>This text has added emphasis. But, '    +
                  'it should not be displayed in a comment</em>'  +
                  '</p>-->'                                       + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p>'         +
                '</footer>'                                       + sLineBreak +
              '</article>'                                        + sLineBreak +
            '</section>'                                          + sLineBreak +
            '<section id="embedded">'                             + sLineBreak +
              '<header><h1>Embedded content</h1></header>'        + sLineBreak +
              '<article id="embedded__images">'                   + sLineBreak +
                '<header><h2>Images</h2></header>'                + sLineBreak +
                '<div>'                                           + sLineBreak +
                  '<h3>No <code>&lt;figure&gt;</code> element'    +
                  '</h3>'                                         + sLineBreak +
                  '<p><img src="http://placekitten.com/480/480" ' +
                  'alt="Image alt text"></p>'                     + sLineBreak +
                  '<h3>Wrapped in a <code>&lt;figure&gt;</code> ' +
                  'element, no <code>&lt;figcaption&gt;</code>'   +
                  '</h3>'                                         + sLineBreak +
                  '<figure><img src="http://placekitten.com/420/' +
                  '420" alt="Image alt text"></figure>'           + sLineBreak +
                  '<h3>Wrapped in a <code>&lt;figure&gt;</code> ' +
                  'element, with a <code>&lt;figcaption&gt;'      +
                  '</code></h3>'                                  + sLineBreak +
                  '<figure>'                                      + sLineBreak +
                    '<img src="http://placekitten.com/420/420" '  +
                    'alt="Image alt text">'                       + sLineBreak +
                    '<figcaption>Here is a caption for this '     +
                    'image.</figcaption>'                         + sLineBreak +
                  '</figure>'                                     + sLineBreak +
                '</div>'                                          + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__audio">'                    + sLineBreak +
                '<header><h2>Audio</h2></header>'                 + sLineBreak +
                '<div><audio controls="">audio</audio></div>'     + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p>'         +
                '</footer>'                                       + sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__video">'                    + sLineBreak +
                '<header><h2>Video</h2></header>'                 + sLineBreak +
                '<div><video controls="">video</video></div>'     + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__canvas">'                   + sLineBreak +
                '<header><h2>Canvas</h2></header>'                + sLineBreak +
                '<div><canvas>canvas</canvas></div>'              + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__meter">'                    + sLineBreak +
                '<header><h2>Meter</h2></header>'                 + sLineBreak +
                '<div><meter value="2" min="0" max="10">2 out of '+
                '10</meter></div>'                                + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__progress">'                 + sLineBreak +
                '<header><h2>Progress</h2></header>'              + sLineBreak +
                '<div><progress>progress</progress></div>'        + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__svg">'                      + sLineBreak +
                '<header><h2>Inline SVG</h2></header>'            + sLineBreak +
                '<div><svg width="100px" height="100px"><circle ' +
                'cx="100" cy="100" r="100" fill="#1fa3ec">'       +
                '</circle></svg></div>'                           + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
              '<article id="embedded__iframe">'                   + sLineBreak +
                '<header><h2>IFrame</h2></header>'                + sLineBreak +
                '<div><iframe src="index.html" height="300">'     +
                '</iframe></div>'                                 + sLineBreak +
                '<footer><p><a href="#top">[Top]</a></p></footer>'+ sLineBreak +
              '</article>'                                        + sLineBreak +
            '</section>'                                          + sLineBreak +
            '<section id="forms">'                                + sLineBreak +
              '<header><h1>Form elements</h1></header>'           + sLineBreak +
              '<form>'                                            + sLineBreak +
                '<fieldset id="forms__input">'                    + sLineBreak +
                '<legend>Input fields</legend>'                   + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__text">Text Input</label>'   + sLineBreak +
                  '<input id="input__text" type="text" '          +
                  'placeholder="Text Input">'                     + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__password">Password</label>' + sLineBreak +
                  '<input id="input__password" type="password" '  +
                  'placeholder="Type your Password">'             + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__webaddress">Web Address'    +
                  '</label>'                                      + sLineBreak +
                  '<input id="input__webaddress" type="url" '     +
                  'placeholder="http://yoursite.com">'            + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__emailaddress">Email Address'+
                  '</label>'                                      + sLineBreak +
                  '<input id="input__emailaddress" type="email" ' +
                  'placeholder="name@email.com">'                 + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__phone">Phone Number</label>'+ sLineBreak +
                  '<input id="input__phone" type="tel" '          +
                  'placeholder="(999) 999-9999">'                 + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__search">Search</label>'     + sLineBreak +
                  '<input id="input__search" type="search" '      +
                  'placeholder="Enter Search Term">'              + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__text2">Number Input</label>'+ sLineBreak +
                  '<input id="input__text2" type="number" '       +
                  'placeholder="Enter a Number">'                 + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__text3" class="error">Error' +
                  '</label>'                                      + sLineBreak +
                  '<input id="input__text3" class="is-error" '    +
                  'type="text" placeholder="Text Input">'         + sLineBreak +
                '</p>'                                            + sLineBreak +
                '<p>'                                             + sLineBreak +
                  '<label for="input__text4" class="valid">Valid' +
                  '</label>'                                      + sLineBreak +
                  '<input id="input__text4" class="is-valid" '    +
                  'type="text" placeholder="Text Input">'         + sLineBreak +
                '</p>'                                            + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__select">'                   + sLineBreak +
                  '<legend>Select menus</legend>'                 + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="select">Select</label>'          + sLineBreak +
                    '<select id="select">'                        + sLineBreak +
                      '<optgroup label="Option Group">'           + sLineBreak +
                        '<option>Option One</option>'             + sLineBreak +
                        '<option>Option Two</option>'             + sLineBreak +
                        '<option>Option Three</option>'           + sLineBreak +
                      '</optgroup>'                               + sLineBreak +
                    '</select>'                                   + sLineBreak +
                  '</p>'                                          + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__checkbox">'                 + sLineBreak +
                  '<legend>Checkboxes</legend>'                   + sLineBreak +
                  '<ul class="list list--bare">'                  + sLineBreak +
                    '<li><label for="checkbox1"><input '          +
                    'id="checkbox1" name="checkbox" '             +
                    'type="checkbox" checked="checked"> Choice A' +
                    '</label></li>'                               + sLineBreak +
                    '<li><label for="checkbox2"><input '          +
                    'id="checkbox2" name="checkbox" '             +
                    'type="checkbox"> Choice B</label></li>'      + sLineBreak +
                    '<li><label for="checkbox3"><input '          +
                    'id="checkbox3" name="checkbox" '             +
                    'type="checkbox"> Choice C</label></li>'      + sLineBreak +
                  '</ul>'                                         + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__radio">'                    + sLineBreak +
                  '<legend>Radio buttons</legend>'                + sLineBreak +
                  '<ul class="list list--bare">'                  + sLineBreak +
                    '<li><label for="radio1"><input id="radio1" ' +
                    'name="radio" type="radio" class="radio" '    +
                    'checked="checked"> Option 1</label></li>'    + sLineBreak +
                    '<li><label for="radio2"><input id="radio2" ' +
                    'name="radio" type="radio" class="radio"> '   +
                    'Option 2</label></li>'                       + sLineBreak +
                    '<li><label for="radio3"><input id="radio3" ' +
                    'name="radio" type="radio" class="radio"> '   +
                    'Option 3</label></li>'                       + sLineBreak +
                  '</ul>'                                         + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__textareas">'                + sLineBreak +
                  '<legend>Textareas</legend>'                    + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="textarea">Textarea</label>'      + sLineBreak +
                    '<textarea id="textarea" rows="8" cols="48" ' +
                    'placeholder="Enter your message here">'      +
                    '</textarea>'                                 + sLineBreak +
                  '</p>'                                          + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__html5">'                    + sLineBreak +
                  '<legend>HTML5 inputs</legend>'                 + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="ic">Color input</label>'         + sLineBreak +
                    '<input type="color" id="ic" value="#000000">'+ sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="in">Number input</label>'        + sLineBreak +
                    '<input type="number" id="in" min="0" '       +
                    'max="10" value="5">'                         + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="ir">Range input</label>'         + sLineBreak +
                    '<input type="range" id="ir" value="10">'     + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="idd">Date input</label>'         + sLineBreak +
                    '<input type="date" id="idd" '                +
                    'value="1970-01-01">'                         + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="idm">Month input</label>'        + sLineBreak +
                    '<input type="month" id="idm" '               +
                    'value="1970-01">'                            + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="idw">Week input</label>'         + sLineBreak +
                    '<input type="week" id="idw" '                +
                    'value="1970-W01">'                           + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="idt">Datetime input</label>'     + sLineBreak +
                    '<input type="datetime" id="idt" '            +
                    'value="1970-01-01T00:00:00Z">'               + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<label for="idtl">Datetime-local input'      +
                    '</label>'                                    + sLineBreak +
                    '<input type="datetime-local" id="idtl" '     +
                    'value="1970-01-01T00:00">'                   + sLineBreak +
                  '</p>'                                          + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
                '<fieldset id="forms__action">'                   + sLineBreak +
                  '<legend>Action buttons</legend>'               + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<input type="submit" value="<input '         +
                    'type=submit>">'                              + sLineBreak +
                    '<input type="button" value="<input '         +
                    'type=button>">'                              + sLineBreak +
                    '<input type="reset" value="<input '          +
                    'type=reset>">'                               + sLineBreak +
                    '<input type="submit" value="<input '         +
                    'disabled>" disabled>'                        + sLineBreak +
                  '</p>'                                          + sLineBreak +
                  '<p>'                                           + sLineBreak +
                    '<button type="submit">&lt;button '           +
                    'type=submit&gt;</button>'                    + sLineBreak +
                    '<button type="button">&lt;button '           +
                    'type=button&gt;</button>'                    + sLineBreak +
                    '<button type="reset">&lt;button '            +
                    'type=reset&gt;</button>'                     + sLineBreak +
                    '<button type="button" disabled>&lt;button '  +
                    'disabled&gt;</button>'                       + sLineBreak +
                  '</p>'                                          + sLineBreak +
                '</fieldset>'                                     + sLineBreak +
                '<p><a href="#top">[Top]</a></p>'                 + sLineBreak +
              '</form>'                                           + sLineBreak +
            '</section>'                                          + sLineBreak +
          '</main>'                                               + sLineBreak +
          '<footer role="contentinfo">'                           + sLineBreak +
            '<p>Made by <a href="http://twitter.com/cbracco">'    +
            '@cbracco</a>. Code on <a href="http://github.com/'   +
            'cbracco/html5-test-page">GitHub</a>.</p>'            + sLineBreak +
          '</footer>'                                             + sLineBreak +
        '</div>'                                                  + sLineBreak +
      '</body>'                                                   + sLineBreak +
    '</html>';
