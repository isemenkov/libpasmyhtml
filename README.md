libPasMyHTML
============
It is object pascal bindings and wrapper around [MyHTML library](https://github.com/lexborisov/myhtml). MyHTML is a fast HTML 5 Parser using Threads.



### Table of contents

* [Requierements](#requirements)
* [Installation](#installation)
* [Usage](#usage)
* [Testing](#testing)
* [Bindings](#bindings)
  * [Usage example](#usage-example)
* [Object wrapper](#object-wrapper)
  * [Usage example](#usage-example-1)



### Requirements

* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional)

Library is tested with latest stable FreePascal Compiler (currently 3.2.0) and Lazarus IDE (currently 2.0.10).



### Installation

Get the sources and add the *source* directory to the *fpc.cfg* file.



### Usage

Clone the repository `git clone https://github.com/isemenkov/libpasmyhtml`.

Add the unit you want to use to the `uses` clause.



### Testing

A testing framework consists of the following ingredients:
1. Test runner project located in `unit-tests` directory.
2. Test cases (FPCUnit based) for additional helpers classes.



### Bindings

[libpasmyhtml.pas](https://github.com/isemenkov/libpasmyhtml/blob/master/source/libpasmyhtml.pas) file contains the MyHTML translated headers to use this library in pascal programs.

#### Usage example

```pascal
  uses
    libpasmyhtml;

  const
    HTMLDocument = '<!DOCTYPE html>'                              + sLineBreak +
    '<html>'                                                      + sLineBreak +
    '<head>'                                                      + sLineBreak +
      '<meta charset="utf-8" />'                                  + sLineBreak +
      '<!--[if lt IE 9]><script src="https://cdnjs.cloudflare.com'+
      '/ajax/libs/html5shiv/3.7.3/html5shiv.min.js"></script>'    +
      '<![endif]-->'                                              + sLineBreak +
      '<title>Document Title</title>'                             + sLineBreak +
      '<meta name="keywords" content="some_keywords keywords" />' + sLineBreak +
      '<meta name="description" content="description" />'         + sLineBreak +
      '<link href="style.css" rel="stylesheet">'                  + sLineBreak +
    '</head>'                                                     + sLineBreak +
    '<body>'                                                      + sLineBreak +
    '</body>'                                                     + sLineBreak +
    '</html>';

  var
    Node : pmyhtml_tree_node_t;
    Title : string;
    Error : mystatus_t;
    HTML : pmyhtml_t;
    Tree : pmyhtml_tree_t;
    Encoding : myencoding_t;

  function FindNextNodeById (ANode : pmyhtml_tree_node_t; AId : myhtml_tags_t) : pmyhtml_tree_node_t; 
    inline;
  begin
    while (ANode <> nil) and (myhtml_node_tag_id(ANode) <> myhtml_tag_id_t(AId)) do
    begin
      ANode := myhtml_node_next(ANode);
    end;
    Result := ANode;
  end;

  function NodeTextValue (ANode : pmyhtml_tree_node_t) : string; inline;
  var
    TextNode : pmyhtml_tree_node_t;
  begin
    if ANode <> nil then
      TextNode := myhtml_node_child(ANode);
      if (TextNode <> nil) and (myhtml_tags_t(myhtml_node_tag_t(TextNode)) = MyHTML_TAG__TEXT) then
      begin
        Result := myhtml_node_text(TextNode, nil);
      end else
        Result := '';
    end else
      Result := '';
  end;

  begin
    HTML := myhtml_create;
    myhtml_init(HTML, MyHTML_OPTIONS_PARSE_MODE_SEPARATELY, 1, 4096);
    Tree := myhtml_tree_create;
    myhtml_tree_init(Tree, HTML);
    myhtml_tree_parse_flags_set(Tree, MyHTML_TREE_PARSE_FLAGS_CLEAN);

    Error := myhtml_parse(Tree, MyENCODING_UTF_8, PChar(HTMLDocument), Length(HTMLDocument));
    if Error = mystatus_t(MyHTML_STATUS_OK) then
    begin
      Node := myhtml_tree_get_node_head(Tree);
      if Node <> nil then
      begin
        Node := myhtml_node_child(Node);
        if Node <> nil then
        begin
          Node := FindNextNodeById(Node, MyHTML_TAG_TITLE);
          if Node <> nil then
          begin
            Title := NodeTextValue(Node);
          end;
        end;
      end;
    end;
    
    myhtml_tree_clean(Tree);
    myhtml_clean(HTML);
    myhtml_tree_destroy(Tree);
    myhtml_destroy(HTML);
  end;

```

### Object wrapper

[pasmyhtml.pas](https://github.com/isemenkov/libpasmyhtml/blob/master/source/pasmyhtml.pas) file contains the MyHTML object wrapper.

#### Usage example

```pascal
  uses
    pasmyhtml;

  const
    HTMLDocument = '<!DOCTYPE html>'                              + sLineBreak +
    '<html>'                                                      + sLineBreak +
    '<head>'                                                      + sLineBreak +
      '<meta charset="utf-8" />'                                  + sLineBreak +
      '<!--[if lt IE 9]><script src="https://cdnjs.cloudflare.com'+
      '/ajax/libs/html5shiv/3.7.3/html5shiv.min.js"></script>'    +
      '<![endif]-->'                                              + sLineBreak +
      '<title>Document Title</title>'                             + sLineBreak +
      '<meta name="keywords" content="some_keywords keywords" />' + sLineBreak +
      '<meta name="description" content="description" />'         + sLineBreak +
      '<link href="style.css" rel="stylesheet">'                  + sLineBreak +
    '</head>'                                                     + sLineBreak +
    '<body>'                                                      + sLineBreak +
    '</body>'                                                     + sLineBreak +
    '</html>';

  var
    Parser : TParser;
    Node : TParser.TTagNode;
    Value : string;

  begin
    Parser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY, MyENCODE_UTF_8, 1, 4096,
      MyHTML_TREE_PARSE_FLAGS_CLEAN);
    Node := Parser.Parse(HTMLDocument, DOCUMENT_HEAD);
    Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TITLE));
    if Node.IsOk then
    begin
      Value := Node.Value;
    end;
  end;

```
