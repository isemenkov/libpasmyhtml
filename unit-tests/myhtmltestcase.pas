unit myhtmltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, libpasmyhtml, pasmyhtml;

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
  published
    procedure TestDocumentParse;
    procedure TestDocumentParseTitle;
  end;

  { TMyHTMLParserSimpleParseTestCase }

  TMyHTMLParserSimpleParseTestCase = class(TTestCase)
  private
    FParser : TMyHTMLParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function TitleFilter (ANode : TMyHTMLParser.TTagNode) : Boolean;
  published
    procedure TestDocumentParse;
    procedure TestDocumentParseTitle;
  end;

{$I htmldocuments/myhtmlsimpleparse_document.inc }

implementation

{ TMyHTMLParserSimpleTestCase }

procedure TMyHTMLParserSimpleParseTestCase.SetUp;
begin
  FParser := TMyHTMLParser.Create(MyHTML_OPTIONS_PARSE_MODE_SEPARATELY,
    MyENCODING_UTF_8, 1, 4096, MyHTML_TREE_PARSE_FLAGS_CLEAN);
end;

procedure TMyHTMLParserSimpleParseTestCase.TearDown;
begin
  FreeAndNil(FParser);
end;

function TMyHTMLParserSimpleParseTestCase.TitleFilter(
  ANode: TMyHTMLParser.TTagNode): Boolean;
begin
  Result := (ANode.GetTag = myhtml_tag_id_t(MyHTML_TAG_TITLE));
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParse;
begin
  FParser.Parse(SimpleParseDocument, DOCUMENT_HTML);
  AssertFalse('Test parse html document', FParser.HasErrors);
end;

procedure TMyHTMLParserSimpleParseTestCase.TestDocumentParseTitle;
var
  title : TMyHTMLParser.TTagNode;
begin
  title := FParser.Parse(SimpleParseDocument, DOCUMENT_HEAD)
    .FirstChildren(@TitleFilter).First;

  if not title.IsOk then
    Fail('Empty document title node');

  AssertTrue('Test document title', title.GetValue = 'Document Title');
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

initialization
  RegisterTest(TMyHTMLSimpleParseTestCase);
  RegisterTest(TMyHTMLParserSimpleParseTestCase);
end.

