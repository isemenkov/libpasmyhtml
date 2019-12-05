unit myhtmltestcase;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}


interface

uses
  Classes, SysUtils, fpcunit, testregistry, libpasmyhtml, pasmyhtml, fgl;

type

  { TMyHTMLLibrarySimpleHTMLTestCase }

  TMyHTMLLibrarySimpleHTMLTestCase = class(TTestCase)
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

  { TParserSimpleHTMLTestCase }

  TParserSimpleHTMLTestCase = class(TTestCase)
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function TagIdEqualCallback (ANode : TParser.TTagNode; AData : Pointer) :
      Boolean;
    function TagAttributeKeyEqualCallback (ANodeAttribute :
      TParser.TTagNodeAttribute; AData : Pointer) : Boolean;
  published
    procedure TestParseDocument;
    procedure TestTitleTag;
    procedure TestTitleTagCallback;
    procedure TestMetaTagCharsetAttributeValue;
    procedure TestMetaTagCharsetAttributeValueCallback;
    procedure TestMetaTagKeywordsAttributeValue;
    procedure TestMetaTagKeywordsAttributeValueCallback;
    procedure TestMetaTagDescriptionAttributeValue;
    procedure TestMetaTagDescriptionAttributeValueCallback;
    procedure TestLinkTagStylesheetAttributeValue;
    procedure TestLinkTagStylesheetAttributeValueCallback;
    procedure TestWrapperHeaderClassDivValue;
    procedure TestWrapperMiddleContainerContentClassDivValue;
  end;

  { TParserTeamtenTestCase }

  TParserTeamtenTestCase = class(TTestCase)
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestEachLinkTagCallback (ANode : TParser.TTagNode;
      AData : Pointer);
  published
    procedure TestTitleTag;
    procedure TestEachLinkTag;
    procedure TestFindAllATag;
    procedure TestH1Tag;
  end;

  { TParserIanaTestCase }

  TParserIanaTestCase = class(TTestCase)
  private
    type
      TZoneInfo = class
        Name : string;
        Info : string;
        Manager : string;
      end;

      TZoneInfoList = specialize TFPGObjectList<TZoneInfo>;
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestEachTrNodesCallback (ANode : TParser.TTagNode;
      AData : Pointer);
    procedure TestReplaceTextNodeCallback (ANode : TParser.TTagNode;
      AData : Pointer);
  published
    procedure TestEachTrNodes;
    procedure TestReplaceTextNode;
  end;

  { TParserHtml5TestCase }

  TParserHtml5TestCase = class(TTestCase)
  private
    FParser : TParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestH1Tag;
  end;

{$I htmldocuments/SimpleHTMLDocument.inc}
{$I htmldocuments/TeamTenDotComDocument.inc}
{$I htmldocuments/IanaDocument.inc}
{$I htmldocuments/Html5TestPage.inc}

implementation

{ TParserHtml5TestCase }

procedure TParserHtml5TestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TParserHtml5TestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

procedure TParserHtml5TestCase.TestH1Tag;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(Html5TestPage, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.AttributeKeyValue(
    'role', 'document'));
  AssertTrue('Error div node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.AttributeKeyValue(
    'role', 'banner'));
  AssertTrue('Error header node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEADER);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_H1));
  AssertTrue('Error h1 node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_H1);

  Value := Node.Value;
  AssertTrue('Error h1 tag value is not correct', Value = 'HTML5 Test Page');
end;

{ TMyHTMLParserIanaTestCase }

procedure TParserIanaTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TParserIanaTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

{ Test each tr node }
procedure TParserIanaTestCase.TestEachTrNodes;
var
  ZoneList : TZoneInfoList;
  Node : TParser.TTagNode;
begin
  ZoneList := TZoneInfoList.Create;

  Node := FParser.Parse(IanaOrgDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsId('body'));
  AssertTrue('Error div class "body" node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsId(
    'main_right'));
  AssertTrue('Error div tag class "main_right" is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsClass(
    'iana-table-frame'));
  AssertTrue('Error div tag class "iana-table-frame" is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TABLE));
  AssertTrue('Error table tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_TABLE);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TBODY));
  AssertTrue('Error tbody tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_TBODY);

  Node := Node.EachChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(
        @TestEachTrNodesCallback, Pointer(ZoneList)));

  AssertTrue('Error children list count', ZoneList.Count = 5);

  AssertTrue('Error list element 0 name is not correct',
    ZoneList[0].Name = '.aaa');
  AssertTrue('Error list element 0 info is not correct',
    ZoneList[0].Info = 'generic');
  AssertTrue('Error list element 0 manager is not correct',
    ZoneList[0].Manager = 'American Automobile Association, Inc.');

  AssertTrue('Error list element 1 name is not correct',
    ZoneList[1].Name = '.aarp');
  AssertTrue('Error list element 1 info is not correct',
    ZoneList[1].Info = 'generic');
  AssertTrue('Error list element 1 manager is not correct',
    ZoneList[1].Manager = 'AARP');

  AssertTrue('Error list element 2 name is not correct',
    ZoneList[2].Name = '.abarth');
  AssertTrue('Error list element 2 info is not correct',
    ZoneList[2].Info = 'generic');
  AssertTrue('Error list element 2 manager is not correct',
    ZoneList[2].Manager = 'Fiat Chrysler Automobiles N.V.');

  AssertTrue('Error list element 3 name is not correct',
    ZoneList[3].Name = '.abb');
  AssertTrue('Error list element 3 info is not correct',
    ZoneList[3].Info = 'generic');
  AssertTrue('Error list element 3 manager is not correct',
    ZoneList[3].Manager = 'ABB Ltd');

  AssertTrue('Error list element 4 name is not correct',
    ZoneList[4].Name = '.abbott');
  AssertTrue('Error list element 4 info is not correct',
    ZoneList[4].Info = 'generic');
  AssertTrue('Error list element 4 manager is not correct',
    ZoneList[4].Manager = 'Abbott Laboratories, Inc.');
end;

{ Test text node with other tags inside }
procedure TParserIanaTestCase.TestReplaceTextNode;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(IanaOrgDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('body'));
  AssertTrue('Error div tag node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly(
    'main_right'));
  AssertTrue('Error div tag node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_P));
  AssertTrue('Error p tag node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_P);

  Value := '';
  Node.EachChildrenNode(nil, TParser.TTransform.Create.TagNodeTransform(
    @TestReplaceTextNodeCallback, @Value));

  AssertTrue('Error value is not correct', Value = 'The Root Zone Database ' +
    'represents the delegation details of top-level domains, including gTLDs ' +
    'such as .com, and country-code TLDs such as .uk. As the manager of the ' +
    'DNS root zone, we are responsible for coordinating these delegations ' +
    'in accordance with our policies and procedures.');
end;

{ Test each tr node callback function }
procedure TParserIanaTestCase.TestEachTrNodesCallback(
  ANode: TParser.TTagNode; AData: Pointer);
var
  Zone : TZoneInfo;
  Node : TParser.TTagNode;
  NameNode : TParser.TTagNode;
begin
  AssertTrue('Error node in each tr tag callback function is nil',
    ANode.IsOk);
  AssertTrue('Error data in each tr tag callback function is nil',
    AData <> nil);
  AssertTrue('Test node tag type', ANode.Tag = TParser.TTag.MyHTML_TAG_TR);

  Zone := TZoneInfo.Create;

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    MyHTML_TAG_TD));
  AssertTrue('Error tag td is nil', Node.IsOk);
  AssertTrue('Error tag id is not correct', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  NameNode := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
    MyHTML_TAG_SPAN));
  AssertTrue('Error span tag is nil', NameNode.IsOk);
  AssertTrue('Error tag id is not correct', NameNode.Tag = MyHTML_TAG_SPAN);

  NameNode := NameNode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    MyHTML_TAG_A));
  AssertTrue('Error a tag is nil', NameNode.IsOk);
  AssertTrue('Error tag id is not correct', NameNode.Tag = MyHTML_TAG_A);

  Zone.Name := NameNode.Value;

  Node := Node.NextNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TD));
  AssertTrue('Error td tag is nil', Node.IsOk);
  AssertTrue('Error tag id is not correct', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  Zone.Info := Node.Value;

  Node := Node.NextNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TD));
  AssertTrue('Error td tag is nil', Node.IsOk);
  AssertTrue('Error tag id is not correct', Node.Tag =
    TParser.TTag.MyHTML_TAG_TD);

  Zone.Manager := Node.Value;

  TZoneInfoList(AData).Add(Zone);
end;

{ Test text node with other tags inside callback function }
procedure TParserIanaTestCase.TestReplaceTextNodeCallback(
  ANode: TParser.TTagNode; AData: Pointer);
var
  Value : ^string;
begin
  AssertTrue('Error node in test replace text node callback function is nil',
    ANode.IsOk);
  AssertTrue('Error data in test replace text node callback function is nil',
    AData <> nil);

  Value := AData;

  if ANode.Tag = MyHTML_TAG__TEXT then
    Value^ := Value^ + ANode.Value
  else
    Value^ := Value^ + ANode.FirstChildrenNode.Value;
end;

{ TParserTeamtenTestCase }

procedure TParserTeamtenTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TParserTeamtenTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

procedure TParserTeamtenTestCase.TestEachLinkTagCallback(
  ANode: TParser.TTagNode; AData: Pointer);
begin
  AssertTrue('Error node in each link tag callback function is nil',
    ANode.IsOk);
  AssertTrue('Error data in each link tag callback function is nil',
    AData <> nil);

  TStringList(AData).Add(ANode.FirstNodeAttribute(
    TParser.TFilter.Create.AttributeKey('href')).Value);
end;

{ Test title tag value }
procedure TParserTeamtenTestCase.TestTitleTag;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(TeamTenDotComDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TITLE));
  AssertTrue('Error title tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_TITLE);

  Value := Node.Value;
  AssertTrue('Error title tag value is not correct', Value =
    'Mostly avoid unit tests');
end;

{ Test link tags contains rel attribute with stylesheet value }
procedure TParserTeamtenTestCase.TestEachLinkTag;
var
  Node : TParser.TTagNode;
  List : TStringList;
begin
  List := TStringList.Create;

  Node := FParser.Parse(TeamTenDotComDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.EachChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_LINK)
    .AttributeKeyValue('rel', 'stylesheet'),
    TParser.TTransform.Create.TagNodeTransform(
      @TestEachLinkTagCallback, Pointer(List)));

  AssertTrue('Error <a> tag list count', List.Count = 4);
  AssertTrue('Error <a> tag 1 attribute value is not correct', List[0] =
    '/lawrence/reset.css');
  AssertTrue('Error <a> tag 2 attribute value is not correct', List[1] =
    '/lawrence/new.css');
  AssertTrue('Error <a> tag 3 attribute value is not correct', List[2] =
    '/lawrence/css/font-awesome.min.css');
  AssertTrue('Error <a> tag 4 attribute value is not correct', List[3] =
    'unsolicited.css');
end;

{ Test find all a tag nodes }
procedure TParserTeamtenTestCase.TestFindAllATag;
var
  Node : TParser.TTagNode;
  List : TParser.TTagNodeList;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
begin
  Node := FParser.Parse(TeamTenDotComDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly(
    'contents'));
  AssertTrue('Error div class="contents" node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly(
    'footer'));
  AssertTrue('Error div class="footer" node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  List := Node.FindAllChildrenNodes(TParser.TFilter.Create.Tag(MyHTML_TAG_A));
  AssertTrue('Error tags list count', List.Count = 5);

  Attribute :=  List[0].FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error attribute is nil', Attribute.IsOk);
  Value := Attribute.Value;
  AssertTrue('Error node attribute href is not correct', Value = '/lawrence/');

  Attribute :=  List[1].FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error attribute is nil', Attribute.IsOk);
  Value := Attribute.Value;
  AssertTrue('Error node attribute href is not correct', Value =
    'mailto:lk@teamten.com');

  Attribute :=  List[2].FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error attribute is nil', Attribute.IsOk);
  Value := Attribute.Value;
  AssertTrue('Error node attribute href is not correct', Value =
    'https://www.linkedin.com/pub/lawrence-kesteloot/2/68a/7a3');

  Attribute :=  List[3].FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error attribute is nil', Attribute.IsOk);
  Value := Attribute.Value;
  AssertTrue('Error node attribute href is not correct', Value =
    'https://twitter.com/lkesteloot');

  Attribute :=  List[4].FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error attribute is nil', Attribute.IsOk);
  Value := Attribute.Value;
  AssertTrue('Error node attribute href is not correct', Value =
    'https://github.com/lkesteloot');
end;

procedure TParserTeamtenTestCase.TestH1Tag;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(TeamTenDotComDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly(
    'contents'));
  AssertTrue('Error div tag class="contents" is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClassOnly('titleLines1'));
  AssertTrue('Error h1 tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_H1);

  Value := Node.Value;
  AssertTrue('Error h1 value is not correct', Value =
    'Mostly avoid unit tests');
end;

{ TParserSimpleHTMLTestCase }

procedure TParserSimpleHTMLTestCase.SetUp;
begin
  FParser := TParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TParserSimpleHTMLTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

function TParserSimpleHTMLTestCase.TagIdEqualCallback(ANode: TParser.TTagNode;
  AData: Pointer): Boolean;
begin
  AssertTrue('Error node in tag id equal callback function is nil',
    ANode <> nil);
  AssertTrue('Error data in tag id equal callback function is nil',
    AData <> nil);

  Result := (ANode <> nil) and (TParser.TTag(AData^) = ANode.Tag);
end;

function TParserSimpleHTMLTestCase.TagAttributeKeyEqualCallback(
  ANodeAttribute: TParser.TTagNodeAttribute; AData: Pointer): Boolean;
begin
  AssertTrue('Error attribute in tag attribute key equal callback is nil',
    ANodeAttribute <> nil);
  AssertTrue('Error data in tag attribute key equal callback is nil',
    AData <> nil);

  Result := (ANodeAttribute <> nil) and (PChar(AData) = ANodeAttribute.Key);
end;

{ Test document parse }
procedure TParserSimpleHTMLTestCase.TestParseDocument;
begin
  FParser.Parse(SimpleHTMLDocument, DOCUMENT_HTML);
  AssertFalse('Error document parse: ' + FParser.Error, FParser.HasErrors);
end;

{ Test title tag value }
procedure TParserSimpleHTMLTestCase.TestTitleTag;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_TITLE));
  AssertTrue('Error title tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_TITLE);

  Value := Node.Value;
  AssertTrue('Error title tag value is not correct', Value = 'Document Title');
end;

{ Test title tag value by callback function }
procedure TParserSimpleHTMLTestCase.TestTitleTagCallback;
var
  Node : TParser.TTagNode;
  Value : string;
  TagId : TParser.TTag = MyHTML_TAG_TITLE;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.TagNodeCallback(
    @TagIdEqualCallback, @TagId));
  AssertTrue('Error title tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_TITLE);

  Value := Node.Value;
  AssertTrue('Error title node value is not correct', Value = 'Document Title');
end;

{ Test meta tag charset attribute value }
procedure TParserSimpleHTMLTestCase.TestMetaTagCharsetAttributeValue;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
    .AttributeKey('charset'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create.AttributeKey(
    'charset'));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'charset');

  Value := Attribute.Value;
  AssertTrue('Error charset attribute is not correct', Value = 'utf-8');
end;

{ Test meta tag charset attribute value by callback functions }
procedure TParserSimpleHTMLTestCase.TestMetaTagCharsetAttributeValueCallback;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
  TagId : TParser.TTag = MyHTML_TAG_META;
  CharsetAttribute : string = 'charset';
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .TagNodeCallback(@TagIdEqualCallback, @TagId)
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(CharsetAttribute)));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(CharsetAttribute)));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'charset');

  Value := Attribute.Value;
  AssertTrue('Error charset attribute is not correct', Value = 'utf-8');
end;

{ Test meta tag keywords attribute value }
procedure TParserSimpleHTMLTestCase.TestMetaTagKeywordsAttributeValue;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Keywords : TStringList;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
    .AttributeKeyValue('name', 'keywords'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('content'));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'content');

  Keywords := Attribute.ValueList;
  AssertTrue('Error keywords list count', Keywords.Count = 2);
  AssertTrue('Error keywords list 0 value is not correct',
    Keywords[0] = 'some_keywords');
  AssertTrue('Error keywords list 1 value is not correct',
    Keywords[1] = 'keywords');
end;

{ Test meta tag keywords attribute value by callback functions }
procedure TParserSimpleHTMLTestCase.TestMetaTagKeywordsAttributeValueCallback;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Keywords : TStringList;
  TagId : TParser.TTag = MyHTML_TAG_META;
  KeywordsAttribute : string = 'content';
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .TagNodeCallback(@TagIdEqualCallback, @TagId)
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(KeywordsAttribute)));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(KeywordsAttribute)));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'content');

  Keywords := Attribute.ValueList;
  AssertTrue('Error keywords list count', Keywords.Count = 2);
  AssertTrue('Error keywords list 0 value is not correct',
    Keywords[0] = 'some_keywords');
  AssertTrue('Error keywords list 0 value is not correct',
    Keywords[1] = 'keywords');
end;

{ Test meta tag description attribute value }
procedure TParserSimpleHTMLTestCase.TestMetaTagDescriptionAttributeValue;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_META)
    .AttributeKeyValue('name', 'description'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('content'));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'content');

  Value := Attribute.Value;
  AssertTrue('Error description attribute is not correct',
    Value = 'description');
end;

{ Test meta tag description attribute value by callback functions }
procedure TParserSimpleHTMLTestCase.TestMetaTagDescriptionAttributeValueCallback;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
  TagId : TParser.TTag = MyHTML_TAG_META;
  DescriptionAttribute : string = 'content';
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .TagNodeCallback(@TagIdEqualCallback, @TagId)
    .AttributeKeyValue('name', 'description'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_META);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(DescriptionAttribute)));
  AssertTrue('Error description attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'content');

  Value := Attribute.Value;
  AssertTrue('Error description attribute is not correct',
    Value = 'description');
end;

{ Test ling tag rel attribute }
procedure TParserSimpleHTMLTestCase.TestLinkTagStylesheetAttributeValue;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_LINK)
    .AttributeKeyValue('rel', 'stylesheet'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_LINK);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .AttributeKey('href'));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'href');

  Value := Attribute.Value;
  AssertTrue('Error stylesheet attribute is not correct',
    Value = 'style.css');
end;

{ Test ling tag rel attribute by callback functions }
procedure TParserSimpleHTMLTestCase.TestLinkTagStylesheetAttributeValueCallback;
var
  Node : TParser.TTagNode;
  Attribute : TParser.TTagNodeAttribute;
  Value : string;
  TagId : TParser.TTag = MyHTML_TAG_LINK;
  StylesheetAttribute : string = 'href';
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_HEAD);
  AssertTrue('Error head node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEAD);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .TagNodeCallback(@TagIdEqualCallback, @TagId)
    .AttributeKeyValue('rel', 'stylesheet'));
  AssertTrue('Error meta tag is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_LINK);

  Attribute := Node.FirstNodeAttribute(TParser.TFilter.Create
    .TagNodeAttributeCallback(@TagAttributeKeyEqualCallback,
      PChar(StylesheetAttribute)));
  AssertTrue('Error charset attribute is nil', Attribute.IsOk);
  AssertTrue('Error not correct attribute key', Attribute.Key = 'href');

  Value := Attribute.Value;
  AssertTrue('Error rel attribute is not correct', Value = 'style.css');
end;

{ Test inner div header value }
procedure TParserSimpleHTMLTestCase.TestWrapperHeaderClassDivValue;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClassOnly('wrapper'));
  AssertTrue('Error div node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClassOnly('header'));
  AssertTrue('Error header node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_HEADER);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_STRONG));
  AssertTrue('Error strong node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_STRONG);

  Value := Node.Value;
  AssertTrue('Error inner div value is not correct', Value = 'Header:');
end;

{ Test inner div header value }
procedure TParserSimpleHTMLTestCase
  .TestWrapperMiddleContainerContentClassDivValue;
var
  Node : TParser.TTagNode;
  Value : string;
begin
  Node := FParser.Parse(SimpleHTMLDocument, DOCUMENT_BODY);
  AssertTrue('Error body node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_BODY);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClass('wrapper'));
  AssertTrue('Error div node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClass('middle'));
  AssertTrue('Error div node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClass('container'));
  AssertTrue('Error div node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_DIV);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create
    .ContainsClass('content'));
  AssertTrue('Error main node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_MAIN);

  Node := Node.FirstChildrenNode(TParser.TFilter.Create.Tag(MyHTML_TAG_STRONG));
  AssertTrue('Error strong node is nil', Node.IsOk);
  AssertTrue('Error not correct tag id', Node.Tag = MyHTML_TAG_STRONG);

  Value := Node.Value;
  AssertTrue('Error inner div value is not correct', Value = 'Content:');
end;

{ TMyHTMLLibraryTestCase }
{ Test case for some basic library opportunities }
procedure TMyHTMLLibrarySimpleHTMLTestCase.SetUp;
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

procedure TMyHTMLLibrarySimpleHTMLTestCase.TearDown;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);
  myhtml_tree_destroy(FTree);
  myhtml_destroy(FHTML);
end;

{ Tokenize string by space symbol }
function TMyHTMLLibrarySimpleHTMLTestCase.StringTokenize(AString: string
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
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestParseDocument;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_parse(FTree, FEncoding, PChar(SimpleHTMLDocument),
    Length(SimpleHTMLDocument));
  AssertTrue('Error document parse',
    FError = mystatus_t(MyHTML_STATUS_OK));
end;

{ Test title tag value }
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestTitleTag;

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
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestMetaTagCharsetAttributeValue;

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
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestMetaTagKeywordsAttributeValue;

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
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestMetaTagDescriptionAttributeValue;

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
procedure TMyHTMLLibrarySimpleHTMLTestCase.TestLinkTagRelAttribute;

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
  RegisterTest(TMyHTMLLibrarySimpleHTMLTestCase);
  RegisterTest(TParserSimpleHTMLTestCase);
  RegisterTest(TParserTeamtenTestCase);
  RegisterTest(TParserIanaTestCase);
  RegisterTest(TParserHtml5TestCase);
end.

