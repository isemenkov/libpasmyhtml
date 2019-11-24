unit myhtmltestcase;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}


interface

uses
  Classes, SysUtils, fpcunit, testregistry, libpasmyhtml, pasmyhtml, fgl;

type

  { TMyHTMLLibraryTestCase }

  TMyHTMLLibraryTestCase = class(TTestCase)
  private
    FHTML : pmyhtml_t;
    FTree : pmyhtml_tree_t;
    FEncoding : myencoding_t;
    FError : mystatus_t;

    FParserOptions : myhtml_options_t;
    FThreadCount : QWord;
    FQueueSize : QWord;
    FFlags : myhtml_tree_parse_flags_t;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function StringTokenize (AString : string) : TStringList;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  published
    procedure TestParseDocument;
    procedure TestTitleTag;
    procedure TestMetaTagCharsetAttributeValue;
    procedure TestMetaTagKeywordsAttributeValue;
    procedure TestMetaTagDescriptionAttributeValue;
    procedure TestLinkTagRelAttribute;
  end;

  { TMyHTMLParserSimpleParseTestCase }

  TMyHTMLParserSimpleParseTestCase = class(TTestCase)
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function TagTitleCallback (ANode : TParser.TTagNode; AData : Pointer)
      : Boolean;
    function TagMetaCallback (ANode : TParser.TTagNode; AData : Pointer)
      : Boolean;
    function TagAttributeCharsetKeyCallback (AAttribute :
      TParser.TTagNodeAttribute; AData : Pointer) : Boolean;
    function TagAttributeContentKeyCallback (AAttribute :
      TParser.TTagNodeAttribute; AData : Pointer) : Boolean;
    function TagAttributeNameKeywordsCallback (AAttribute :
      TParser.TTagNodeAttribute; AData : Pointer) : Boolean;
    function TagLinkCallback (ANode : TParser.TTagNode; AData : Pointer)
      : Boolean;
    function TagAttributeRelStylesheet (AAttribute :
      TParser.TTagNodeAttribute; AData : Pointer) : Boolean;
  published
    procedure TestDocumentParse;
    procedure TestDocumentParseTitle;
    procedure TestDocumentParseTitleCallback;
    procedure TestDocumentParseMetaCharset;
    procedure TestDocumentParseMetaCharsetCallback;
    procedure TestDocumentParseMetaKeywords;
    procedure TestDocumentParseMetaKeywordsCallback;
    procedure TestDocumentParseMetaDescription;
    procedure TestDocumentParseMetaDescriptionCallback;
    procedure TestDocumentParseLinkStylesheet;
    procedure TestDocumentParseLinkStylesheetCallback;
    procedure TestDocumentParseHeader;
    procedure TestDocumentParseContent;
  end;

  { TMyHTMLParserTeamtenTestCase }

  TMyHTMLParserTeamtenTestCase = class(TTestCase)
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestDocumentParseEachLinkCallback(ANode : TParser.TTagNode;
      AData : Pointer);
  published
    procedure TestDocumentParseEachLink;
  end;

  { TMyHTMLParserIanaTestCase }

  TMyHTMLParserIanaTestCase = class(TTestCase)
  private
    type
      TZoneInfo = class
        Name : string;
        Info : string;
        Manager : string;
      end;

      TZoneInfoList = specialize TFPGList<TZoneInfo>;
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestDocumentParseEachNodesTrCallback (ANode : TParser.TTagNode;
      AData : Pointer);
  published
    procedure TestDocumentParseEachNodes;
  end;

{$I htmldocuments/SimpleHTMLDocument.inc}
{$I htmldocuments/myhtmlteamtenparse_document.inc}
{$I htmldocuments/myhtmlianaparse_document.inc}

implementation

{ TMyHTMLParserIanaTestCase }

procedure TMyHTMLParserIanaTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TMyHTMLParserIanaTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

procedure TMyHTMLParserIanaTestCase.TestDocumentParseEachNodes;
var
  ZoneList : TZoneInfoList;
  Node : TParser.TTagNode;
begin
  ZoneList := TZoneInfoList.Create;

  Node := FParser.Parse(iana_org_document, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('body'));

  AssertTrue('Test node contains id "body"', Node.IsOk);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsId(
    'main_right'));

  AssertTrue('Test node contains id "main_right"', Node.IsOk);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsClass(
    'iana-table-frame'));

  AssertTrue('Test node contains id "iana-table-frame"', Node.IsOk);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TABLE));

  AssertTrue('Test node tag TABLE', Node.IsOk);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TBODY));

  AssertTrue('Test node tag TBODY', Node.IsOk);

  Node := Node.EachChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(
        @TestDocumentParseEachNodesTrCallback, Pointer(ZoneList)));

  AssertTrue('Test each node children list', ZoneList.Count = 5);

  AssertTrue('Test list element 0 name', ZoneList[0].Name = '.aaa');
  AssertTrue('Test list element 0 info', ZoneList[0].Info = 'generic');
  AssertTrue('Test list element 0 manager', ZoneList[0].Manager =
    'American Automobile Association, Inc.');

  AssertTrue('Test list element 1 name', ZoneList[1].Name = '.aarp');
  AssertTrue('Test list element 1 info', ZoneList[1].Info = 'generic');
  AssertTrue('Test list element 1 manager', ZoneList[1].Manager = 'AARP');

  AssertTrue('Test list element 2 name', ZoneList[2].Name = '.abarth');
  AssertTrue('Test list element 2 info', ZoneList[2].Info = 'generic');
  AssertTrue('Test list element 2 manager', ZoneList[2].Manager =
    'Fiat Chrysler Automobiles N.V.');

  AssertTrue('Test list element 3 name', ZoneList[3].Name = '.abb');
  AssertTrue('Test list element 3 info', ZoneList[3].Info = 'generic');
  AssertTrue('Test list element 3 manager', ZoneList[3].Manager = 'ABB Ltd');

  AssertTrue('Test list element 4 name', ZoneList[4].Name = '.abbott');
  AssertTrue('Test list element 4 info', ZoneList[4].Info = 'generic');
  AssertTrue('Test list element 4 manager', ZoneList[4].Manager =
    'Abbott Laboratories, Inc.');
end;

procedure TMyHTMLParserIanaTestCase.TestDocumentParseEachNodesTrCallback(
  ANode: TParser.TTagNode; AData: Pointer);
var
  Zone : TZoneInfo;
  Node : TParser.TTagNode;
begin
  AssertTrue('Test node tag type', ANode.Tag = TParser.TTag.MyHTML_TAG_TR);

  Zone := TZoneInfo.Create;

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    MyHTML_TAG_TD));
  AssertTrue('Test node children tag type', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  Zone.Name := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
    MyHTML_TAG_SPAN))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_A))
    .Value;

  Node := Node.NextNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TD));
  AssertTrue('Test node children tag type', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  Zone.Info := Node.Value;

  Node := Node.NextNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TD));
  AssertTrue('Test node children tag type', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  Zone.Manager := Node.Value;

  TZoneInfoList(AData).Add(Zone);

  AssertTrue('Test node tag type', ANode.Tag = TParser.TTag.MyHTML_TAG_TR);
end;

{ TMyHTMLParserTeamtenTestCase }

procedure TMyHTMLParserTeamtenTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TMyHTMLParserTeamtenTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

procedure TMyHTMLParserTeamtenTestCase.TestDocumentParseEachLinkCallback(
  ANode: TParser.TTagNode; AData: Pointer);
begin
  AssertTrue('Test node link callback', ANode.IsOk);

  TStringList(AData).Add(ANode.FirstNodeAttribute(
    TParser.TFilter.Create.AttributeKey('href')).Value);
end;

procedure TMyHTMLParserTeamtenTestCase.TestDocumentParseEachLink;
var
  Node : TParser.TTagNode;
  List : TStringList;
begin
  List := TStringList.Create;
  Node := FParser.Parse(teamten_com, DOCUMENT_HEAD);

  AssertTrue('Head node test', Node.IsOk);

  Node := Node.EachChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_LINK)
    .AttributeKeyValue('rel', 'stylesheet'),
    TParser.TTransform.Create.TagNodeTransform(
      @TestDocumentParseEachLinkCallback, Pointer(List)));

  AssertTrue('Test find elements', List.Count = 4);
  AssertTrue('Test element 1', List[0] = '/lawrence/reset.css');
  AssertTrue('Test element 1', List[1] = '/lawrence/new.css');
  AssertTrue('Test element 1', List[2] = '/lawrence/css/font-awesome.min.css');
  AssertTrue('Test element 1', List[3] = 'unsolicited.css');
end;

{ TMyHTMLParserSimpleTestCase }

procedure TMyHTMLParserSimpleParseTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TMyHTMLParserSimpleParseTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

function TMyHTMLParserSimpleParseTestCase.TagTitleCallback(ANode:
  TParser.TTagNode; AData: Pointer): Boolean;
begin
  Result := ANode.Tag = MyHTML_TAG_TITLE;

  AssertTrue('Title tag callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagMetaCallback(
  ANode: TParser.TTagNode; AData: Pointer): Boolean;
begin
  Result := ANode.Tag = MyHTML_TAG_META;

  AssertTrue('Meta tag callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagAttributeCharsetKeyCallback(
  AAttribute: TParser.TTagNodeAttribute; AData: Pointer): Boolean;
begin
  Result := AAttribute.Key = 'charset';

  AssertTrue('Tag attribute charset callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagAttributeContentKeyCallback(
  AAttribute: TParser.TTagNodeAttribute; AData: Pointer): Boolean;
begin
  Result := AAttribute.Key = 'content';

  AssertTrue('Tag attribute content key callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagAttributeNameKeywordsCallback(
  AAttribute: TParser.TTagNodeAttribute; AData: Pointer): Boolean;
begin
  Result := (AAttribute.Key = 'name') and (AAttribute.Value = 'keywords');

  AssertTrue('Tag attribute name keywords callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagLinkCallback(
  ANode: TParser.TTagNode; AData: Pointer): Boolean;
begin
  AssertTrue('Tag link callback', ANode.Tag = MyHTML_TAG_LINK);
  Result := True;

  AssertTrue('Tag link callback data not nil', AData <> nil);
end;

function TMyHTMLParserSimpleParseTestCase.TagAttributeRelStylesheet(
  AAttribute: TParser.TTagNodeAttribute; AData: Pointer): Boolean;
begin
  Result := (AAttribute.Key = 'rel') and (AAttribute.Value = 'stylesheet');

  AssertTrue('Tag attribute rel stylesheet callback data not nil', AData <>
    nil);
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParse;
begin
  FParser.Parse(SimpleHTMLDocument, DOCUMENT_HTML);
  AssertFalse('Test parse html document', FParser.HasErrors);
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseTitle;
var
  title : TParser.TTagNode;
begin
  title := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TITLE));

  if not title.IsOk then
    Fail('Empty document title node');

  AssertTrue('Test document title', title.Value = 'Document Title');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseTitleCallback;
var
  title : string;
begin
  title := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(
      @TagTitleCallback, @Self))
    .Value;

  AssertTrue('Test document title callback', title = 'Document Title');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaCharset;
var
  charset : string;
begin
  charset := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
      .AttributeKey('charset'))
    .FirstNodeAttribute(TParser.TFilter.Create.AttributeKey('charset'))
    .Value;

  AssertTrue('Test document meta charset attribute', charset = 'utf-8');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaCharsetCallback;
var
  charset : string;
begin
  charset := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(@TagMetaCallback,
      @Self)
      .TagNodeAttributeCallback(@TagAttributeCharsetKeyCallback, @Self))
    .FirstNodeAttribute(TParser.TFilter.Create.TagNodeAttributeCallback(
      @TagAttributeCharsetKeyCallback, @Self))
    .Value;

  AssertTrue('Test document meta charset attribute callback', charset =
    'utf-8');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaKeywords;
var
  Keywords : TStringList;
begin
  Keywords := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
      .AttributeKey('name').AttributeValue('keywords'))
    .FirstNodeAttribute(TParser.TFilter.Create.AttributeKey('content'))
    .ValueList;

  AssertTrue('Test keywords count', Keywords.Count = 2);
  AssertTrue('Test keyword 1', Keywords[0] = 'some_keywords');
  AssertTrue('Test keyword 2', Keywords[1] = 'keywords');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaKeywordsCallback;
var
  Keywords : TStringList;
begin
  Keywords := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(@TagMetaCallback,
      @Self)
      .TagNodeAttributeCallback(@TagAttributeNameKeywordsCallback, @Self))
    .FirstNodeAttribute(TParser.TFilter.Create.TagNodeAttributeCallback(
      @TagAttributeContentKeyCallback, @Self))
    .ValueList;

  AssertTrue('Test keywords count callback', Keywords.Count = 2);
  AssertTrue('Test keyword 1 callback', Keywords[0] = 'some_keywords');
  AssertTrue('Test keyword 2 callback', Keywords[1] = 'keywords');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaDescription;
var
  Description : string;
begin
  Description := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
      .AttributeKeyValue('name', 'description'))
    .FirstNodeAttribute(TParser.TFilter.Create.AttributeKey('content'))
    .Value;

  AssertTrue('Test meta description', Description = 'description');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaDescriptionCallback;
var
  Description : string;
begin
  Description := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(@TagMetaCallback,
      @Self)
      .AttributeKeyValue('name', 'description'))
    .FirstNodeAttribute(TParser.TFilter.Create.TagNodeAttributeCallback(
      @TagAttributeContentKeyCallback, @Self))
    .Value;

  AssertTrue('Test meta description callback', Description = 'description');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseLinkStylesheet;
var
  Stylesheet : string;
  Node : TParser.TTagNode;
  NodeAttribute : TParser.TTagNodeAttribute;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);

  AssertTrue('Test node HEAD', Node.IsOk and (Node.Tag =
    TParser.TTag.MyHTML_TAG_HEAD));

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_LINK)
    .AttributeKey('rel').AttributeValue('stylesheet'));

  AssertTrue('Test node LINK', Node.IsOk and (Node.Tag =
    TParser.TTag.MyHTML_TAG_LINK));

  NodeAttribute := Node.FirstNodeAttribute(TParser.TFilter.Create.AttributeKey(
    'href'));

  AssertTrue('Test node attribute', NodeAttribute.IsOk);

  Stylesheet := NodeAttribute.Value;

  AssertTrue('Test link href', Stylesheet = 'style.css');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseLinkStylesheetCallback;
var
  Stylesheet : string;
begin
  Stylesheet := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_LINK)
      .TagNodeCallback(@TagLinkCallback, @Self)
      .TagNodeAttributeCallback(@TagAttributeRelStylesheet, @Self))
    .FirstNodeAttribute(TParser.TFilter.Create.AttributeKey('href'))
    .Value;

  AssertTrue('Test link href callback', Stylesheet = 'style.css');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseHeader;
var
  Value : string;
begin
  Value := FParser.Parse(SimpleHTMLDocument, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly('wrapper'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly('header'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_STRONG))
    .Value;

  AssertTrue('Test body tag class filter header', Value = 'Header:');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseContent;
var
  Value : string;
begin
  Value := FParser.Parse(SimpleHTMLDocument, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('wrapper'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('middle'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('container'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('content'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_STRONG))
    .Value;

  AssertTrue('Test body tag class filter content', Value = 'Content:');
end;

{ TMyHTMLLibraryTestCase }
{ Test case for some basic library opportunities }

procedure TMyHTMLLibraryTestCase.SetUp;
begin
  FParserOptions := MyHTML_OPTIONS_PARSE_MODE_SEPARATELY;
  FEncoding := MyENCODING_UTF_8;
  FThreadCount := 1;
  FQueueSize := 4096;
  FFlags := MyHTML_TREE_PARSE_FLAGS_CLEAN;
  FError := Cardinal(MyHTML_STATUS_OK);

  FHTML := myhtml_create;
  myhtml_init(FHTML, FParserOptions, FThreadCount, FQueueSize);
  FTree := myhtml_tree_create;
  myhtml_tree_init(FTree, FHTML);
  myhtml_tree_parse_flags_set(FTree, FFlags);
end;

procedure TMyHTMLLibraryTestCase.TearDown;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);
  myhtml_tree_destroy(FTree);
  myhtml_destroy(FHTML);
end;

{ Tokenize string by space symbol }

function TMyHTMLLibraryTestCase.StringTokenize(AString: string
  ): TStringList;
var
  Index : SizeInt;
begin
  Result := TStringList.Create;
  while AString <> '' do
  begin
    AString := TrimLeft(AString);
    Index := Pos(' ', AString);

    if Index <> 0 then
    begin
      Result.Add(Trim(Copy(AString, 0, Index)));
      AString := Copy(AString, Index, Length(AString) - Index + 1);
    end else
    begin
      Result.Add(AString);
      AString := '';
    end;
  end;
end;

{ Test parse document }

procedure TMyHTMLLibraryTestCase.TestParseDocument;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));
end;

{ Test title tag value }

procedure TMyHTMLLibraryTestCase.TestTitleTag;

  { Find node start from ANode by tag node id }
  function FindNextNodeById (ANode : pmyhtml_tree_node_t; AId : myhtml_tags_t)
    : pmyhtml_tree_node_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    while (ANode <> nil) and (myhtml_node_tag_id(ANode) <>
      myhtml_tag_id_t(AId)) do
    begin
      ANode := myhtml_node_next(ANode);
    end;
    Result := ANode;
  end;

  { Get node text value }
  function NodeTextValue (ANode : pmyhtml_tree_node_t) : string;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
  TextNode : pmyhtml_tree_node_t;
  begin
    if ANode <> nil then
    begin
      TextNode := myhtml_node_child(ANode);
      if (TextNode <> nil) and (myhtml_tags_t(myhtml_node_tag_id(TextNode)) =
        MyHTML_TAG__TEXT) then
      begin
        Result := myhtml_node_text(TextNode, nil);
      end else
        Result := '';
    end else
      Result := '';
  end;

var
  Node : pmyhtml_tree_node_t;
  Title : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Error tree head node is nil');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Error children node is nil');

  Node := FindNextNodeById(Node, MyHTML_TAG_TITLE);
  if Node = nil then
    Fail('Error TITLE tag is not found');

  Title := NodeTextValue(Node);
  AssertTrue('Error title text is not correct', Title = 'Document Title');
end;

{ Test meta tag charser attribute value }

procedure TMyHTMLLibraryTestCase.TestMetaTagCharsetAttributeValue;

  { Find node start from ANode by tag node id }
  function FindNextNodeById (ANode : pmyhtml_tree_node_t; AId : myhtml_tags_t)
    : pmyhtml_tree_node_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    while (ANode <> nil) and (myhtml_node_tag_id(ANode) <>
      myhtml_tag_id_t(AId)) do
    begin
      ANode := myhtml_node_next(ANode);
    end;
    Result := ANode;
  end;

  { Find attribute start from AAttribute by node attribute key }
  function FindNextAttributeByKey (ANode : pmyhtml_tree_node_t; AKey :
    string) : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := myhtml_attribute_by_key(ANode, PChar(AKey), Length(AKey));
  end;

  { Find node start from ANode and have attribute with exists key. Return
    attribute or nil }
  function FindNodeByIdAndAttributeByKey (ANode : pmyhtml_tree_node_t; AId :
    myhtml_tags_t; AKey : string) : pmyhtml_tree_attr_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := nil;
    while (ANode <> nil) and (Result = nil) do
    begin
      ANode := FindNextNodeById(ANode, AId);
      Result := FindNextAttributeByKey(ANode, AKey);
    end;
  end;

var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Charset : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Error tree head node is nil');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Error children node is nil');

  Attribute := FindNodeByIdAndAttributeByKey(Node, MyHTML_TAG_META, 'charset');

  if Attribute = nil then
    Fail('Error META tag attribute is nil');

  Charset := myhtml_attribute_value(Attribute, nil);
  AssertTrue('Error charser attribute is not correct', Charset = 'utf-8');
end;

{ Test meta tag keywords attribute value }

procedure TMyHTMLLibraryTestCase.TestMetaTagKeywordsAttributeValue;

  { Find attribute start from AAttribute by node attribute key }
  function FindNextAttributeByKeyValue (ANode : pmyhtml_tree_node_t; AKey :
    string; AValue : string) : pmyhtml_tree_attr_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := myhtml_attribute_by_key(ANode, PChar(AKey), Length(AKey));
    if (Result <> nil) and (myhtml_attribute_value(Result, nil) <> AValue) then
      Result := nil;
  end;

  { Find node start from ANode and have attribute with exists key and value.
    Return node and attribute or nil }
  function FindNodeByIdAndAttributeByKeyValue (ANode : pmyhtml_tree_node_t;
    AId : myhtml_tags_t; AKey : string; AValue : string) : pmyhtml_tree_node_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Attribute: pmyhtml_tree_attr_t;
  begin
    Attribute := nil;
    while (ANode <> nil) and (Attribute = nil) do
    begin
      ANode := myhtml_node_next(ANode);
      if myhtml_node_tag_id(ANode) = myhtml_tag_id_t(AId) then
        Attribute := FindNextAttributeByKeyValue(ANode, AKey, AValue);
    end;
    Result := ANode;
  end;

var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Keywords : TStringList;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Error tree head node is nil');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Error children node is nil');

  Node := FindNodeByIdAndAttributeByKeyValue(Node, MyHTML_TAG_META, 'name',
    'keywords');
  if Node = nil then
    Fail('Error META tag is nil');

  Attribute := myhtml_attribute_by_key(Node, 'content', Length('content'));
  if Attribute = nil then
    Fail('Error META tag attribute is nil');

  Keywords := StringTokenize(myhtml_attribute_value(Attribute, nil));
  AssertTrue('Error keywords list count not correct', Keywords.Count = 2);
  AssertTrue('Error keywords 0 is not correct', Keywords[0] = 'some_keywords');
  AssertTrue('Error keywords 1 is not correct', Keywords[1] = 'keywords');
end;

{ Test meta tag description attribute value }

procedure TMyHTMLLibraryTestCase.TestMetaTagDescriptionAttributeValue;

  { Find attribute start from AAttribute by node attribute key }
  function FindNextAttributeByKeyValue (ANode : pmyhtml_tree_node_t; AKey :
    string; AValue : string) : pmyhtml_tree_attr_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := myhtml_attribute_by_key(ANode, PChar(AKey), Length(AKey));
    if (Result <> nil) and (myhtml_attribute_value(Result, nil) <> AValue) then
      Result := nil;
  end;

  { Find node start from ANode and have attribute with exists key and value.
    Return node and attribute or nil }
  function FindNodeByIdAndAttributeByKeyValue (ANode : pmyhtml_tree_node_t;
    AId : myhtml_tags_t; AKey : string; AValue : string) : pmyhtml_tree_node_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Attribute : pmyhtml_tree_attr_t;
  begin
    Attribute := nil;
    while (ANode <> nil) and (Attribute = nil) do
    begin
      ANode := myhtml_node_next(ANode);
      if myhtml_node_tag_id(ANode) = myhtml_tag_id_t(AId) then
        Attribute := FindNextAttributeByKeyValue(ANode, AKey, AValue);
    end;
    Result := ANode;
  end;

var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Description : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Error tree head node is nil');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Error children node is nil');

  Node := FindNodeByIdAndAttributeByKeyValue(Node, MyHTML_TAG_META, 'name',
    'description');
  if Node = nil then
    Fail('Error META tag is nil');

  Attribute := myhtml_attribute_by_key(Node, 'content', Length('content'));
  if Attribute = nil then
    Fail('Error META tag attribute is nil');

  Description := myhtml_attribute_value(Attribute, nil);
  AssertTrue('Error description attribute is not correct', Description =
    'description');
end;

{ Test link tag rel attribute }

procedure TMyHTMLLibraryTestCase.TestLinkTagRelAttribute;

  { Find attribute start from AAttribute by node attribute key }
  function FindNextAttributeByKeyValue (ANode : pmyhtml_tree_node_t; AKey :
    string; AValue : string) : pmyhtml_tree_attr_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := myhtml_attribute_by_key(ANode, PChar(AKey), Length(AKey));
    if (Result <> nil) and (myhtml_attribute_value(Result, nil) <> AValue) then
      Result := nil;
  end;

  { Find node start from ANode and have attribute with exists key and value.
    Return node and attribute or nil }
  function FindNodeByIdAndAttributeByKeyValue (ANode : pmyhtml_tree_node_t;
    AId : myhtml_tags_t; AKey : string; AValue : string) : pmyhtml_tree_node_t;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Attribute : pmyhtml_tree_attr_t;
  begin
    Attribute := nil;
    while (ANode <> nil) and (Attribute = nil) do
    begin
      ANode := myhtml_node_next(ANode);
      if myhtml_node_tag_id(ANode) = myhtml_tag_id_t(AId) then
        Attribute := FindNextAttributeByKeyValue(ANode, AKey, AValue);
    end;
    Result := ANode;
  end;

var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Stylesheet : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Error tree head node is nil');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Error children node is nil');

  Node := FindNodeByIdAndAttributeByKeyValue(Node, MyHTML_TAG_LINK, 'rel',
    'stylesheet');
  if Node = nil then
    Fail('Error LINK tag is nil');

  Attribute := myhtml_attribute_by_key(Node, 'href', Length('href'));
  if Attribute = nil then
    Fail('Error LINK tag attribute is nil');

  Stylesheet := myhtml_attribute_value(Attribute, nil);
  AssertTrue('Error link href is not correct', Stylesheet = 'style.css');
end;

initialization
  RegisterTest(TMyHTMLLibraryTestCase);
  RegisterTest(TMyHTMLParserSimpleParseTestCase);
  RegisterTest(TMyHTMLParserTeamtenTestCase);
  RegisterTest(TMyHTMLParserIanaTestCase);
end.

