const
  TeamTenDotComDocument =
    '<!DOCTYPE html>'                                             + sLineBreak +
    '<html>'                                                      + sLineBreak +
      '<head>'                                                    + sLineBreak +
        '<meta http-equiv="content-type" content="text/html; '    +
        'charset=utf-8">'                                         + sLineBreak +
        '<meta name="viewport" content="width=device-width, '     +
        'initial-scale=1">'                                       + sLineBreak +
        '<title>Mostly avoid unit tests</title>'                  + sLineBreak +
        '<link href="/lawrence/reset.css" rel="stylesheet" '      +
        'type="text/css">'                                        + sLineBreak +
        '<link href="/lawrence/new.css" rel="stylesheet" '        +
        'type="text/css">'                                        + sLineBreak +
        '<link href="/lawrence/css/font-awesome.min.css" '        +
        'rel="stylesheet" type="text/css">'                       + sLineBreak +
        '<link href="unsolicited.css" rel="stylesheet" '          +
        'type="text/css">'                                        + sLineBreak +
        '<link rel="shortcut icon" href="/lawrence/'              +
        'favicon.ico"/>'                                          + sLineBreak +
        '<script>'                                                + sLineBreak +
          '(function(i,s,o,g,r,a,m){i[''GoogleAnalyticsObject'']' +
          '=r;i[r]=i[r]||function(){'                             + sLineBreak +
          '(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new '    +
          'Date();a=s.createElement(o),'                          + sLineBreak +
          'm=s.getElementsByTagName(o)[0];a.async=1;a.src='       +
          'g;m.parentNode.insertBefore(a,m)'                      + sLineBreak +
          '})(window,document,''script'',''https://'              +
          'www.google-analytics.com/analytics.js'',''ga'');'      + sLineBreak +
          'ga(''create'', ''UA-575657-1'', ''auto'');'            +
          'ga(''send'', ''pageview'');'                           + sLineBreak +
        '</script>'                                               + sLineBreak +
      '</head>'                                                   + sLineBreak +
      '<body class="markdown ">'                                  + sLineBreak +
        '<div class="contents">'                                  + sLineBreak +
          '<h1 class="titleLines1">Mostly avoid unit tests</h1>'  + sLineBreak +
          '<blockquote>'                                          + sLineBreak +
            '<p>&ldquo;I don&rsquo;t need tests: tests are for '  +
            'people who write bugs.&rdquo;'                       + sLineBreak +
            '&ndash;<a href="https://twitter.com/hipsterhacker/'  +
            'status/396352411754717184">@hipsterhacker</a></p>'   + sLineBreak +
          '</blockquote>'                                         + sLineBreak +
          '<p>Most of the value of a unit test comes when you '   +
          'change the original'                                   + sLineBreak +
          '(tested) code, but <em>not</em> the test, and can '    +
          'run the test to make'                                  + sLineBreak +
          'sure that all is still well. The tested code must '    +
          'satisfy'                                               + sLineBreak +
          'two requirements for this value to be realized:</p>'   + sLineBreak +
          '<ol>'                                                  + sLineBreak +
            '<li>'                                                + sLineBreak +
              '<p>The code must have a small, stable API.</p>'    + sLineBreak +
            '</li>'                                               + sLineBreak +
            '<li>'                                                + sLineBreak +
              '<p>The code must be complex enough to break '      +
              'easily.</p>'                                       + sLineBreak +
            '</li>'                                               + sLineBreak +
          '</ol>'                                                 + sLineBreak +
          '<p>Most classes do not satisfy both of these, and '    +
          'should not be unit tested.</p>'                        + sLineBreak +
          '<p>For an example of code that violates the first '    +
          'requirement, consider'                                 + sLineBreak +
          'complex code that orchestrates various services. '     +
          'It pulls data from'                                    + sLineBreak +
          'some, passes to others, and has various logic '        +
          'internally. The API here'                              + sLineBreak +
          'is complex. (I&rsquo;m counting its outgoing calls as '+
          'part of its API in'                                    + sLineBreak +
          'the sense that the test must verify that the calls '   +
          'are correct.) Most'                                    + sLineBreak +
          'changes to the code will also include changes to '     +
          'outgoing calls.'                                       + sLineBreak +
          'You can write mock implementations of these services, '+
          'but you&rsquo;ll have'                                 + sLineBreak +
          'to change the mocks when anything is changed about '   +
          'the class. Then'                                       + sLineBreak +
          'in what sense is the test really helpful? It&rsquo;s ' +
          'really just implementing'                              + sLineBreak +
          'the class a second time in a different way. '          +
          'It&rsquo;s not verifying'                              + sLineBreak +
          'that the implementation is correct. It&rsquo;s only '  +
          'verifying that the'                                    + sLineBreak +
          'two implementations match.</p>'                        + sLineBreak +
          '<p>For an example of code that violates the second '   +
          'requirement, look'                                     + sLineBreak +
          'at the classic example of a unit test for a stack '    +
          'class. A stack'                                        + sLineBreak +
          'has a very simple API, one that&rsquo;s practically '  +
          'defined by the'                                        + sLineBreak +
          'term &ldquo;stack&rdquo;. It&rsquo;s straightforward ' +
          'to test a stack&rsquo;s API, verify'                   + sLineBreak +
          'last-in-first-out, and so on. That&rsquo;s why stacks '+
          'are so often'                                          + sLineBreak +
          'used as unit test examples. But so what? How likely '  +
          'is your'                                               + sLineBreak +
          'stack implementation to break after it&rsquo;s '       +
          'initially written?</p>'                                + sLineBreak +
          '<p>For an example of code that&rsquo;s perfect for '   +
          'unit testing, consider'                                + sLineBreak +
          'a date parser. (In fact all text parsers, including '  +
          'compilers, are ideal candidates'                       + sLineBreak +
          'for unit testing.) There&rsquo;s a set of date '       +
          'formats that it can parse,'                            + sLineBreak +
          'like &ldquo;January 1, 2009&rdquo; and &ldquo;'        +
          '01-JAN-09&rdquo; and it must return'                   + sLineBreak +
          'a canonical object representing the date. '            +
          'That&rsquo;s a very small,'                            + sLineBreak +
          'well-defined API that won&rsquo;t change. (It '        +
          'can&rsquo;t change because you'                        + sLineBreak +
          'must parse all the dates you&rsquo;ve correctly '      +
          'parsed before.) If you'                                + sLineBreak +
          'find a bug, you can add it to your test, fix the '     +
          'code, and verify'                                      + sLineBreak +
          'that no previously-parsed dates fail. A text parser '  +
          'has a simple'                                          + sLineBreak +
          'API and often has sufficiently-complex logic to '      +
          'warrant a unit'                                        + sLineBreak +
          'test.</p>'                                             + sLineBreak +
          '<p>For other types of code, your time is better '      +
          'spent carefully re-reading'                            + sLineBreak +
          'your code or having it reviewed by a peer.</p>'        + sLineBreak +
          '<div class="footer">'                                  + sLineBreak +
            '<a href="/lawrence/"><i class="icon-home"></i></a>'  + sLineBreak +
            '<a href="mailto:lk@teamten.com"><i '                 +
            'class="icon-envelope-alt"></i></a>'                  + sLineBreak +
            '<a href="https://www.linkedin.com/pub/'              +
            'lawrence-kesteloot/2/68a/7a3"><i '                   +
            'class="icon-linkedin-sign"></i></a>'                 + sLineBreak +
            '<a href="https://twitter.com/lkesteloot"><i '        +
            'class="icon-twitter-sign"></i></a>'                  + sLineBreak +
            '<a href="https://github.com/lkesteloot"><i '         +
            'class="icon-github-sign"></i></a>'                   + sLineBreak +
          '</div>'                                                + sLineBreak +
        '</div>'                                                  + sLineBreak +
      '</body>'                                                   + sLineBreak +
    '</html>';
