unit myhtmltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, libpasmyhtml, pasmyhtml, fgl;

type

  { TMyHTMLSimpleParseTestCase }

  TMyHTMLSimpleParseTestCase = class(TTestCase)
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
  published
    procedure TestDocumentParse;
    procedure TestDocumentParseTitle;
    procedure TestDocumentParseMetaCharset;
    procedure TestDocumentParseMetaKeywords;
    procedure TestDocumentParseMetaDescription;
    procedure TestDocumentParseLinkStylesheet;
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
  published

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

{$I htmldocuments/myhtmlsimpleparse_document.inc}
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
begin
  ZoneList := TZoneInfoList.Create;

  FParser.Parse(iana_org_document, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('body'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('main_right'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('iana-table-frame'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TABLE))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TR),
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
  FParser.Parse(SimpleParseDocument, DOCUMENT_HTML);
  AssertFalse('Test parse html document', FParser.HasErrors);
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseTitle;
var
  title : TParser.TTagNode;
begin
  title := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TITLE));

  if not title.IsOk then
    Fail('Empty document title node');

  AssertTrue('Test document title', title.Value = 'Document Title');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseTitleCallback;
var
  title : string;
begin
  title := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
    .FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(
      @TagTitleCallback, @Self))
    .Value;

  AssertTrue('Test document title callback', title = 'Document Title');
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseMetaCharset;
var
  charset : string;
begin
  charset := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  charset := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Keywords := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Keywords := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Description := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Description := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Node := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD);

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
  Stylesheet := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
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
  Value := FParser.Parse(SimpleParseDocument, DOCUMENT_BODY)
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
  Value := FParser.Parse(SimpleParseDocument, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('wrapper'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('middle'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('container'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClass('content'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_STRONG))
    .Value;

  AssertTrue('Test body tag class filter content', Value = 'Content:');
end;

{ TMyHTMLSimpleParseTestCase }

procedure TMyHTMLSimpleParseTestCase.SetUp;
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

procedure TMyHTMLSimpleParseTestCase.TearDown;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);
  myhtml_tree_destroy(FTree);
  myhtml_destroy(FHTML);
end;

function TMyHTMLSimpleParseTestCase.StringTokenize(AString: string
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

procedure TMyHTMLSimpleParseTestCase.TestDocumentParse;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));
end;

procedure TMyHTMLSimpleParseTestCase.TestDocumentParseTitle;
var
  Node : pmyhtml_tree_node_t;
  Title : pmycore_string_t;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Empty document head node');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Empty head children node');

  while myhtml_node_tag_id(Node) <> myhtml_tag_id_t(MyHTML_TAG_TITLE) do
  begin
    if Node = nil then
      Fail('Title node not found');
    Node := myhtml_node_next(Node);
  end;

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Title node is empty');

  if myhtml_node_tag_id(Node) = myhtml_tag_id_t(MyHTML_TAG__TEXT) then
  begin
    Title := myhtml_node_string(Node);
    AssertTrue('Test document title',
      string(mycore_string_data(Title)) = 'Document Title');
  end else
    Fail('Test document title');
end;

procedure TMyHTMLSimpleParseTestCase.TestDocumentParseMetaCharset;
var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Find : Boolean;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Empty document head node');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Empty head children node');

  Find := False;
  while (Node <> nil) and (Find = False) do
  begin
    if myhtml_node_tag_id(Node) = myhtml_tag_id_t(MyHTML_TAG_META) then
    begin
      Attribute := myhtml_node_attribute_first(Node);

      while (Attribute <> nil) and (Find = False) do
      begin
        if myhtml_attribute_key(Attribute, nil) = 'charset' then
          Find := True;
      end;

    end;
    Node := myhtml_node_next(Node);
  end;

  if Attribute = nil then
    Fail('Meta node attribute is empty');

  AssertTrue('Test document meta charset attribute',
    myhtml_attribute_value(Attribute, nil) = 'utf-8');
end;

procedure TMyHTMLSimpleParseTestCase.TestDocumentParseMetaKeywords;
var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Find : Boolean;
  Keywords : TStringList;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Empty document head node');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Empty head children node');

  Find := False;
  while (Node <> nil) and (Find = False) do
  begin
    if myhtml_node_tag_id(Node) = myhtml_tag_id_t(MyHTML_TAG_META) then
    begin
      Attribute := myhtml_node_attribute_first(Node);

      while (Attribute <> nil) and (Find = False) do
      begin
        if (myhtml_attribute_key(Attribute, nil) = 'name') and
          (myhtml_attribute_value(Attribute, nil) = 'keywords') then
          Find := True
        else
          Attribute := myhtml_attribute_next(Attribute);
      end;

    end;
    Node := myhtml_node_next(Node);
  end;

  if Attribute = nil then
    Fail('Meta node attribute is empty');

  while Attribute <> nil do
  begin
    if myhtml_attribute_key(Attribute, nil) = 'content' then
    begin
      Keywords := StringTokenize(myhtml_attribute_value(Attribute, nil));
    end;

    Attribute := myhtml_attribute_next(Attribute);
  end;

  AssertTrue('Test kewords count', Keywords.Count = 2);
  AssertTrue('Test keyword 1', Keywords[0] = 'some_keywords');
  AssertTrue('Test keyword 2', Keywords[1] = 'keywords');
end;

procedure TMyHTMLSimpleParseTestCase.TestDocumentParseMetaDescription;
var
  Node : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Find : Boolean;
  Description : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Empty document head node');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Empty head children node');

  Find := False;
  while (Node <> nil) and (Find = False) do
  begin
    if myhtml_node_tag_id(Node) = myhtml_tag_id_t(MyHTML_TAG_META) then
    begin
      Attribute := myhtml_node_attribute_first(Node);

      while (Attribute <> nil) and (Find = False) do
      begin
        if (myhtml_attribute_key(Attribute, nil) = 'name') and
          (myhtml_attribute_value(Attribute, nil) = 'description') then
          Find := True
        else
          Attribute := myhtml_attribute_next(Attribute);
      end;

    end;
    Node := myhtml_node_next(Node);
  end;

  if Attribute = nil then
    Fail('Meta node attribute is empty');

  while Attribute <> nil do
  begin
    if myhtml_attribute_key(Attribute, nil) = 'content' then
    begin
      Description := myhtml_attribute_value(Attribute, nil);
    end;

    Attribute := myhtml_attribute_next(Attribute);
  end;

  AssertTrue('Test meta description', Description = 'description');
end;

procedure TMyHTMLSimpleParseTestCase.TestDocumentParseLinkStylesheet;
var
  Node, Link : pmyhtml_tree_node_t;
  Attribute : pmyhtml_tree_attr_t;
  Find : Boolean;
  Stylesheet : string;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleParseDocument),
    Length(SimpleParseDocument));
  AssertTrue('Test parse html document', FError = mystatus_t(MyHTML_STATUS_OK));

  Node := myhtml_tree_get_node_head(FTree);
  if Node = nil then
    Fail('Empty document head node');

  Node := myhtml_node_child(Node);
  if Node = nil then
    Fail('Empty head children node');

  Find := False;
  while (Node <> nil) and (Find = False) do
  begin
    if myhtml_node_tag_id(Node) = myhtml_tag_id_t(MyHTML_TAG_LINK) then
    begin
      Attribute := myhtml_node_attribute_first(Node);

      while (Attribute <> nil) and (Find = False) do
      begin
        if (myhtml_attribute_key(Attribute, nil) = 'rel') and
          (myhtml_attribute_value(Attribute, nil) = 'stylesheet') then
          begin
            Find := True;
            Link := Node;
          end
        else
          Attribute := myhtml_attribute_next(Attribute);
      end;

    end;
    Node := myhtml_node_next(Node);
  end;

  if Attribute = nil then
    Fail('Link node attribute is empty');

  Attribute := myhtml_node_attribute_first(Link);
  while Attribute <> nil do
  begin
    if myhtml_attribute_key(Attribute, nil) = 'href' then
    begin
      Stylesheet := myhtml_attribute_value(Attribute, nil);
    end;

    Attribute := myhtml_attribute_next(Attribute);
  end;

  AssertTrue('Test link stylesheet', Stylesheet = 'style.css');
end;

initialization
  RegisterTest(TMyHTMLSimpleParseTestCase);
  RegisterTest(TMyHTMLParserSimpleParseTestCase);
  RegisterTest(TMyHTMLParserIanaTestCase);
end.

