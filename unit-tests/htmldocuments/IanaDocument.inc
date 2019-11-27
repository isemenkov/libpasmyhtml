const
  IanaOrgDocument =
    '<!doctype html>'                                             + sLineBreak +
      '<html>'                                                    + sLineBreak +
        '<head>'                                                  + sLineBreak +
          '<title>IANA — Root Zone Database</title>'              + sLineBreak +
          '<meta charset="utf-8" />'                              + sLineBreak +
          '<meta http-equiv="Content-type" content="text/html; '  +
            'charset=utf-8" />'                                   + sLineBreak +
          '<meta name="viewport" content="width=device-width, '   +
            'initial-scale=1" />'                                 + sLineBreak +
          '<link rel="stylesheet" media="screen" '                +
            'href="/_css/2015.1/screen.css"/>'                    + sLineBreak +
          '<link rel="stylesheet" media="print" '                 +
            'href="/_css/2015.1/print.css"/>'                     + sLineBreak +
          '<link rel="shortcut icon" type="image/ico" '           +
            'href="/_img/bookmark_icon.ico"/>'                    + sLineBreak +
          '<script type="text/javascript" '                       +
            'src="/_js/2013.1/jquery.js"></script>'               + sLineBreak +
          '<script type="text/javascript" '                       +
            'src="/_js/2013.1/iana.js"></script>'                 + sLineBreak +
        '</head>'                                                 + sLineBreak +
        '<body>'                                                  + sLineBreak +
          '<header>'                                              + sLineBreak +
            '<div id="header">'                                   + sLineBreak +
              '<div id="logo">'                                   + sLineBreak +
                '<a href="/"><img src="/_img/2013.1/'             +
                  'iana-logo-header.svg" alt="Homepage"/></a>'    + sLineBreak +
              '</div>'                                            + sLineBreak +
              '<div class="navigation">'                          + sLineBreak +
                '<ul>'                                            + sLineBreak +
                  '<li><a href="/domains">Domains</a></li>'       + sLineBreak +
                  '<li><a href="/numbers">Numbers</a></li>'       + sLineBreak +
                  '<li><a href="/protocols">Protocols</a></li>'   + sLineBreak +
                  '<li><a href="/about">About Us</a></li>'        + sLineBreak +
                '</ul>'                                           + sLineBreak +
              '</div>'                                            + sLineBreak +
            '</div>'                                              + sLineBreak +
          '</header>'                                             + sLineBreak +
          '<div id="body">'                                       + sLineBreak +
            '<div id="main_right">'                               + sLineBreak +
              '<h1>Root Zone Database</h1>'                       + sLineBreak +
              '<p>The Root Zone Database represents the '         +
                'delegation details of top-level domains, '       +
                'including gTLDs such as <span '                  +
                'class="domain">.com</span>, and country-code '   +
                'TLDs such as <span class="domain">.uk</span>. As'+
                'the manager of the DNS root zone, we are '       +
                'responsible for coordinating these delegations ' +
                'in accordance with our <a href="/domains/'       +
                'root/">policies and procedures</a>.</p>'         + sLineBreak +
              '<p>Much of this data is also available via the '   +
                'WHOIS protocol at <span class="domain"><a '      +
                'href="http://whois.iana.org/">whois.iana.org'    +
                '</a></span>.</p>'                                + sLineBreak +
              '<div class="iana-table-frame">'                    + sLineBreak +
                '<div id="iana-tld-nav"></div>'                   + sLineBreak +
                '<table id="tld-table" class="iana-table">'       + sLineBreak +
                  '<thead>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<th>Domain</th>'                           + sLineBreak +
                      '<th>Type</th>'                             + sLineBreak +
                      '<th>TLD Manager</th>'                      + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                  '</thead>'                                      + sLineBreak +
                  '<tbody>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>'                                      + sLineBreak +
                        '<span class="domain tld"><a '            +
                          'href="/domains/root/db/aaa.html">'     +
                          '.aaa</a></span>'                       + sLineBreak +
                      '</td>'                                     + sLineBreak +
                      '<td>generic</td>'                          + sLineBreak +
                      '<td>American Automobile Association, '     +
                        'Inc.</td>'                               + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>'                                      + sLineBreak +
                        '<span class="domain tld"><a '            +
                          'href="/domains/root/db/aarp.html">'    +
                          '.aarp</a></span>'                      + sLineBreak +
                      '</td>'                                     + sLineBreak +
                      '<td>generic</td>'                          + sLineBreak +
                      '<td>AARP</td>'                             + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>'                                      + sLineBreak +
                        '<span class="domain tld"><a '            +
                          'href="/domains/root/db/abarth.html">'  +
                          '.abarth</a></span>'                    + sLineBreak +
                      '</td>'                                     + sLineBreak +
                      '<td>generic</td>'                          + sLineBreak +
                      '<td>Fiat Chrysler Automobiles N.V.</td>'   + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>'                                      + sLineBreak +
                        '<span class="domain tld"><a '            +
                          'href="/domains/root/db/abb.html">'     +
                          '.abb</a></span>'                       + sLineBreak +
                      '</td>'                                     + sLineBreak +
                      '<td>generic</td>'                          + sLineBreak +
                      '<td>ABB Ltd</td>'                          + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                    '<tr>'                                        + sLineBreak +
                      '<td>'                                      + sLineBreak +
                        '<span class="domain tld"><a '            +
                          'href="/domains/root/db/abbott.html">'  +
                          '.abbott</a></span>'                    + sLineBreak +
                      '</td>'                                     + sLineBreak +
                      '<td>generic</td>'                          + sLineBreak +
                      '<td>Abbott Laboratories, Inc.</td>'        + sLineBreak +
                    '</tr>'                                       + sLineBreak +
                  '</tbody>'                                      + sLineBreak +
                '</table>'                                        + sLineBreak +
              '</div>'                                            + sLineBreak +
            '</div>'                                              + sLineBreak +
          '</div>'                                                + sLineBreak +
        '</body>'                                                 + sLineBreak +
      '</html>';
