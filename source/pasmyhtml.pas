(******************************************************************************)
(*                                libPasMyHTML                                *)
(*                object pascal wrapper around MyHTML library                 *)
(*                    https://github.com/lexborisov/myhtml                    *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpasmyhtml                ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Module:          Unit 'pasmyhtml'                                          *)
(* Functionality:   Provides  TMyHTMLParser class                             *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

unit pasmyhtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpasmyhtml, fgl;

type

  { TMyHTMLParser }

  TMyHTMLParser = class
  public
    type
      (* Start parsing from the next element *)
      TDocumentParseFrom = (
        DOCUMENT,      (* Start parse from document root *)
        DOCUMENT_HTML, (* Start parse from HTML tag element *)
        DOCUMENT_HEAD, (* Start parse from HEAD tag element *)
        DOCUMENT_BODY  (* Start parse from BODY tag element *)
      );

    TTagNodeAttribute = class;
    TTagNode = class;
    TTreeChunk = class;
    TTreeChunkList = class;

    TTagNodeFilterCallback = function (ANode : TTagNode) : Boolean of object;
    TTagNodeAttributeFilterCallback = function (ANodeAttribute :
      TTagNodeAttribute) : Boolean of object;

    { TTagNodeFilter }

    TTagNodeFilter = class
    private
      FTag : myhtml_tag_id_t;

    public
      constructor Create;
      destructor Destroy; override;

      function IsEqual (ATag : pmyhtml_tree_node_t) : Boolean;

      function Callback (ACallback : TTagNodeFitlerCallback) : TTagNodeFilter;
      function Tag (ATag : myhtml_tags_t) : TTagNodeFilter;
    end;


    { TTreeChunk }
    { Document tree chunk }

    TTreeChunk = class
    private
      FNode : pmyhtml_tree_node_t;
      FNextNode : pmyhtml_tree_node_t;
      FNodeFilter : TTagNodeFilterCallback;

      FNextChildrenNode : pmyhtml_tree_node_t;
      FChildrenNodeFilter : TTagNodeFilterCallback;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;

      function IsOk : Boolean;

      function Filter (ANodeFilter : TTagNodeFilter) : TTreeChunkList;



      function FirstNode (ANodeFilter : TTagNodeFilterCallback = nil) :
        TTagNode;
      function NextNode : TTagNode;

      function FindAllNodes (ANodeFilter : TTagNodeFilterCallback = nil) :
        TTreeChunkList;

      function FirstChildrenNode (ANodeFilter : TTagNodeFilterCallback = nil) :
        TTagNode; overload;
      function FirstChildrenNode (ANodeFIlter : TTagNodeFilter) : TTagNode;
        overload;
      function NextChildrenNode : TTagNode;

      function FindAllChildrenNodes (ANodeFilter : TTagNodeFilter) :
        TTreeChunkList;
    end;

    { TTreeChunkList }

    TTreeChunkList = class
    private
      type
        TChunkList = specialize TFPGList<TTreeChunk>;
    private
      FList : TChunkList;
      FListIndex : Integer;
      FAttributeFilter : TTagNodeAttributeFilterCallback;

      function CheckListItem (ANode : pmyhtml_tree_node_t; AAttributeFilter :
        TTagNodeAttributeFilterCallback) : Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      function IsOk : Boolean;

      function FirstNode (AAttributeFilter :
        TTagNodeAttributeFilterCallback = nil) : TTagNode;
      function NextNode : TTagNode;
    end;

    { TTagNodeAttribute }

    TTagNodeAttribute = class
    private
      FAttribute : pmyhtml_tree_attr_t;

      function GetKey : string;
      function GetValue : string;
    public
      constructor Create (AAttribute : pmyhtml_tree_attr_t);
      destructor Destroy; override;

      function IsOk : Boolean;

      property Key : string read GetKey;
      property Value : string read GetValue;
    end;

    { TTagNode }

    TTagNode = class
    private
      FNode : pmyhtml_tree_node_t;

      function GetTag : myhtml_tags_t;
      function GetClassList : TStringList;
      function GetIdList : TStringList;
      function GetValue : string;
      function GetParent : TTreeChunk;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;

      function IsOk : Boolean;

      function FirstAttribute (AAtributeFilter :
        TTagNodeAttributeFilterCallback) : TTagNodeAttribute;
      function NextAttribute : TTagNodeAttribute;

      function FindAttributeByKey (AKey : string) : TTagNodeAttribute;

      property Tag : myhtml_tags_t read GetTag;
      property ClassList : TStringList read GetClassList;
      property IdList : TStringList read GetIdList;
      property Value : string read GetValue;
      property Parent : TTreeChunk read GetParent;
    end;

  private
    FHTML : pmyhtml_t;
    FTree : pmyhtml_tree_t;
    FEncoding : myencoding_t;
    FError : myhtml_status_t;
  public
    constructor Create(
      AParserOptions: myhtml_options_t = MyHTML_OPTIONS_PARSE_MODE_SEPARATELY;
      AEncoding: myencoding_t = MyENCODING_UTF_8;
      AThreadCount: QWord = 1;
      AQueueSize: QWord = 4096;
      AFlags: myhtml_tree_parse_flags_t = MyHTML_TREE_PARSE_FLAGS_CLEAN);
    destructor Destroy; override;

    (* Document parse *)
    function Parse (AHTML : string; AParseFrom : TDocumentParseFrom =
      DOCUMENT_HTML) : TTreeChunk;

    (* Return if parse has errors *)
    function HasErrors : Boolean;

    (* Return error message if it is, or empty string *)
    function Error : string;
  end;

  function StringTokenize (AString : string) : TStringList;

implementation

function StringTokenize(AString: string): TStringList;
var
  Index : SizeInt;
begin
  Result := TStringList.Create;
  while AString <> '' do
  begin
    AString := TrimLeft(AString);
    Index := Pos(AString, #10);

    if Index <> 0 then
    begin
      Result.Add(Trim(Copy(AString, 0, Index)));
      Delete(AString, 0, Index);
    end else
    begin
      Result.Add(AString);
      AString := '';
    end;
  end;
end;

{ TMyHTMLParser }

constructor TMyHTMLParser.Create(AParserOptions: myhtml_options_t;
  AEncoding: myencoding_t; AThreadCount: QWord; AQueueSize: QWord;
  AFlags: myhtml_tree_parse_flags_t);
begin
  FHTML := myhtml_create;
  myhtml_init(FHTML, AParserOptions, AThreadCount, AQueueSize);
  FTree := myhtml_tree_create;
  myhtml_tree_init(FTree, FHTML);
  myhtml_tree_parse_flags_set(FTree, AFlags);
  FEncoding := AEncoding;
  FError := MyHTML_STATUS_OK;
end;

destructor TMyHTMLParser.Destroy;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);
  myhtml_tree_destroy(FTree);
  myhtml_destroy(FHTML);
  inherited Destroy;
end;

function TMyHTMLParser.Parse(AHTML: string; AParseFrom: TDocumentParseFrom
  ): TTreeChunk;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_status_t(myhtml_parse(FTree, FEncoding, PChar(AHTML),
    Length(AHTML)));
  if FError = MyHTML_STATUS_OK then
  begin
    case AParseFrom of
      DOCUMENT :
        Result := TTreeChunk.Create(myhtml_tree_get_document(FTree));
      DOCUMENT_HTML :
        Result := TTreeChunk.Create(myhtml_tree_get_node_html(FTree));
      DOCUMENT_HEAD :
        Result := TTreeChunk.Create(myhtml_tree_get_node_head(FTree));
      DOCUMENT_BODY :
        Result := TTreeChunk.Create(myhtml_tree_get_node_body(FTree));
    end;
  end else
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.HasErrors: Boolean;
begin
  Result := FError <> MyHTML_STATUS_OK;
end;

function TMyHTMLParser.Error: string;
begin
  case FError of
    MyHTML_STATUS_OK :
    begin
      Result := '';
    end;

    MyHTML_STATUS_ERROR :
    begin
      Result := 'MyHTML_STATUS_ERROR';
    end;

    MyHTML_STATUS_ERROR_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_ERROR_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_RULES_ERROR_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_RULES_ERROR_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TOKENIZER_ERROR_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_TOKENIZER_ERROR_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TOKENIZER_ERROR_FRAGMENT_INIT :
    begin
      Result := 'MyHTML_STATUS_TOKENIZER_ERROR_FRAGMENT_INIT';
    end;

    MyHTML_STATUS_TAGS_ERROR_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE';
    end;

    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_MALLOC :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_MCOBJECT_MALLOC';
    end;

    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE_NODE :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE_NODE';
    end;

    MyHTML_STATUS_TAGS_ERROR_CACHE_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_CACHE_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TAGS_ERROR_INDEX_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_TAGS_ERROR_INDEX_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TREE_ERROR_MEMORY_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_TREE_ERROR_MEMORY_ALLOCATION';
    end;

    MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE :
    begin
      Result := 'MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE';
    end;

    MyHTML_STATUS_TREE_ERROR_MCOBJECT_INIT :
    begin
      Result := 'MyHTML_STATUS_TREE_ERROR_MCOBJECT_INIT';
    end;

    MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE_NODE :
    begin
      Result := 'MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE_NODE';
    end;

    MyHTML_STATUS_TREE_ERROR_INCOMING_BUFFER_CREATE :
    begin
      Result := 'MyHTML_STATUS_TREE_ERROR_INCOMING_BUFFER_CREATE';
    end;

    MyHTML_STATUS_ATTR_ERROR_ALLOCATION :
    begin
      Result := 'MyHTML_STATUS_ATTR_ERROR_ALLOCATION';
    end;

    MyHTML_STATUS_ATTR_ERROR_CREATE :
    begin
      Result := 'MyHTML_STATUS_ATTR_ERROR_CREATE';
    end;

    MyHTML_STATUS_STREAM_BUFFER_ERROR_CREATE :
    begin
      Result := 'MyHTML_STATUS_STREAM_BUFFER_ERROR_CREATE';
    end;

    MyHTML_STATUS_STREAM_BUFFER_ERROR_INIT :
    begin
      Result := 'MyHTML_STATUS_STREAM_BUFFER_ERROR_INIT';
    end;

    MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_CREATE :
    begin
      Result := 'MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_CREATE';
    end;

    MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_INIT :
    begin
      Result := 'MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_INIT';
    end;

    MyHTML_STATUS_STREAM_BUFFER_ERROR_ADD_ENTRY :
    begin
      Result := 'MyHTML_STATUS_STREAM_BUFFER_ERROR_ADD_ENTRY';
    end;
  end;
end;

{ TMyHTMLParser.TTagNodeFilter }

constructor TMyHTMLParser.TTagNodeFilter.Create;
begin
  FTag := myhtml_tag_id_t(MyHTML_TAG__UNDEF);
end;

destructor TMyHTMLParser.TTagNodeFilter.Destroy;
begin

  inherited Destroy;
end;

function TMyHTMLParser.TTagNodeFilter.IsEqual(ATag: pmyhtml_tree_node_t
  ): Boolean;
begin
  Result := False;

  if (FTag <> myhtml_tag_id_t(MyHTML_TAG__UNDEF)) and
    (myhtml_node_tag_id(ATag) = FTag) then
    Result := True;


end;

function TMyHTMLParser.TTagNodeFilter.Tag(ATag: myhtml_tags_t
  ): TTagNodeFilter;
begin
  FTag := myhtml_tag_id_t(ATag);
  Result := Self;
end;

{ TMyHTMLParser.TTreeChunkList }

function TMyHTMLParser.TTreeChunkList.CheckListItem(ANode: pmyhtml_tree_node_t;
  AAttributeFilter: TTagNodeAttributeFilterCallback): Boolean;
var
  Attr : pmyhtml_tree_attr_t;
begin
  Result := False;

  Attr := myhtml_node_attribute_first(ANode);
  while (Attr <> nil) and
    (not AAttributeFilter(TTagNodeAttribute.Create(Attr))) do
    Attr := myhtml_attribute_next(Attr);

  if Attr <> nil then
    Result := True;
end;

constructor TMyHTMLParser.TTreeChunkList.Create;
begin
  FList := TChunkList.Create;
end;

destructor TMyHTMLParser.TTreeChunkList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TMyHTMLParser.TTreeChunkList.IsOk: Boolean;
begin
  Result := FList.Count > 0;
end;

function TMyHTMLParser.TTreeChunkList.FirstNode(
  AAttributeFilter: TTagNodeAttributeFilterCallback): TTagNode;
var
  Attr : pmyhtml_tree_attr_t;
begin
  Result := TTagNode.Create(nil);

  if IsOk then
  begin
    FAttributeFilter := AAttributeFilter;
    FListIndex := 0;

    { Apply filter if exists }
    if Assigned(FAttributeFilter) then
    begin
      while  (FListIndex < FList.Count) and
        (not CheckListItem(FList[FListIndex].FNode, FAttributeFilter)) do
        Inc(FListIndex);
    end;

    if FListIndex <= FList.Count then
      Result := TTagNode.Create(FList[FListIndex].FNode);
  end;
end;

function TMyHTMLParser.TTreeChunkList.NextNode: TTagNode;
begin
  Result := TTagNode.Create(nil);

  if IsOk and (FListIndex < FList.Count) then
  begin
    Inc(FListIndex);

    { Apply filter if exists }
    if Assigned(FAttributeFilter) then
    begin
      while  (FListIndex < FList.Count) and
        (not CheckListItem(FList[FListIndex].FNode, FAttributeFilter)) do
        Inc(FListIndex);
    end;

    if FListIndex <= FList.Count then
      Result := TTagNode.Create(FList[FListIndex].FNode);
  end;
end;

{ TMyHTMLParser.TTreeChunk }

constructor TMyHTMLParser.TTreeChunk.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
end;

destructor TMyHTMLParser.TTreeChunk.Destroy;
begin
  inherited Destroy;
end;

function TMyHTMLParser.TTreeChunk.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TMyHTMLParser.TTreeChunk.FirstNode(ANodeFilter: TTagNodeFilterCallback
  ): TTagNode;
begin
  if IsOk then
  begin
    FNodeFilter := ANodeFilter;
    FNextNode := FNode;

    { Apply filter if exists }
    if Assigned(FNodeFilter) then
    begin
      while not FNodeFilter(TTagNode.Create(FNextNode)) do
        FNextNode := myhtml_node_next(FNextNode);
    end;

    Result := TTagNode.Create(FNextNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.NextNode: TTagNode;
begin
  if FNextNode <> nil then
  begin

    { Apply filter if exists }
    if Assigned(FNodeFilter) then
    begin
      while not FNodeFilter(TTagNode.Create(FNextNode)) do
        FNextNode := myhtml_node_next(FNextNode);
    end else
      FNextNode := myhtml_node_next(FNextNode);

    Result := TTagNode.Create(FNextNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.FindAllNodes(ANodeFilter:
  TTagNodeFilterCallback): TTreeChunkList;
var
  Node : pmyhtml_tree_node_t;
begin
  Result := TTreeChunkList.Create;

  if IsOk then
  begin
    Node := FNode;

    while Node <> nil do
    begin
      if Assigned(ANodeFilter) and ANodeFilter(TTagNode.Create(Node)) then
        Result.FList.Add(TTreeChunk.Create(Node));

      Node := myhtml_node_next(Node);
    end;
  end;
end;

function TMyHTMLParser.TTreeChunk.FirstChildrenNode(ANodeFilter:
  TTagNodeFilterCallback): TTagNode;
begin
  if IsOk then
  begin
    FChildrenNodeFilter := ANodeFilter;
    FNextChildrenNode := myhtml_node_child(FNode);

    { Apply filter if exists }
    if Assigned(FChildrenNodeFilter) then
    begin
      while not FChildrenNodeFilter(TTagNode.Create(FNextChildrenNode)) do
        FNextChildrenNode := myhtml_node_next(FNextChildrenNode);
    end;

    Result := TTagNode.Create(FNextChildrenNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.FirstChildrenNode(ANodeFilter: TTagNodeFilter
  ): TTagNode;
begin
 if IsOk then
 begin
   FNextChildrenNode := myhtml_node_child(FNode);

   { Apply filter if exists }
   while not ANodeFilter.IsEqual(FNextChildrenNode) do
     FNextChildrenNode := myhtml_node_next(FNextChildrenNode);

   Result := TTagNode.Create(FNextChildrenNode);
 end else
   Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.NextChildrenNode: TTagNode;
begin
  if FNextChildrenNode <> nil then
  begin

    { Apply filter if exists }
    if Assigned(FChildrenNodeFilter) then
    begin
      while not FNodeFilter(TTagNode.Create(FNextChildrenNode)) do
        FNextChildrenNode := myhtml_node_next(FNextChildrenNode);
    end else
      FNextChildrenNode := myhtml_node_next(FNextChildrenNode);

    Result := TTagNode.Create(FNextChildrenNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.FindAllChildrenNodes(
  ANodeFilter: TTagNodeFilterCallback): TTreeChunkList;
var
  Node : pmyhtml_tree_node_t;
begin
  Result := TTreeChunkList.Create;

  if IsOk then
  begin
    Node := myhtml_node_child(FNode);

    while Node <> nil do
    begin
      if Assigned(ANodeFilter) and ANodeFilter(TTagNode.Create(Node)) then
        Result.FList.Add(TTreeChunk.Create(Node));

      Node := myhtml_node_next(Node);
    end;
  end;
end;

function TMyHTMLParser.TTreeChunk.FindAllChildrenNodes(
  ANodeFilter: TTagNodeFilter): TTreeChunkList;
var
  Node : pmyhtml_tree_node_t;
begin
  Result := TTreeChunkList.Create;

  if IsOk then
  begin
    Node := myhtml_node_child(FNode);

    while Node <> nil do
    begin
      if ANodeFilter.IsEqual(Node) then
        Result.FList.Add(TTreeChunk.Create(Node));

      Node := myhtml_node_next(Node);
    end;
  end;
end;

{ TMyHTMLParser.TTagNode }

function TMyHTMLParser.TTagNode.GetTag: myhtml_tags_t;
begin
  if IsOk then
  begin
    Result := myhtml_tags_t(myhtml_node_tag_id(FNode));
  end else
    Result := MyHTML_TAG__UNDEF;
end;

function TMyHTMLParser.TTagNode.GetClassList: TStringList;
begin
  Result := TStringList.Create;
end;

function TMyHTMLParser.TTagNode.GetIdList: TStringList;
begin
  Result := TStringList.Create;
end;

function TMyHTMLParser.TTagNode.GetValue: string;
var
  TextNode : pmyhtml_tree_node_t;
begin
  Result := '';

  if IsOk then
  begin
    TextNode := myhtml_node_child(FNode);
    if (TextNode <> nil) and (myhtml_node_tag_id(TextNode) =
      myhtml_tag_id_t(MyHTML_TAG__TEXT)) then
    begin
      Result := mycore_string_data(myhtml_node_string(TextNode));
    end;
  end;
end;

function TMyHTMLParser.TTagNode.GetParent: TTreeChunk;
begin
  if IsOk then
  begin
    Result := TTreeChunk.Create(myhtml_node_parent(FNode));
  end else
    Result := TTreeChunk.Create(nil);
end;

constructor TMyHTMLParser.TTagNode.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
end;

destructor TMyHTMLParser.TTagNode.Destroy;
begin
  inherited Destroy;
end;

function TMyHTMLParser.TTagNode.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TMyHTMLParser.TTagNode.FirstAttribute(
  AAtributeFilter: TTagNodeAttributeFilterCallback): TTagNodeAttribute;
begin
  Result := TTagNodeAttribute.Create(nil);
end;

function TMyHTMLParser.TTagNode.NextAttribute: TTagNodeAttribute;
begin
  Result := TTagNodeAttribute.Create(nil);
end;

function TMyHTMLParser.TTagNode.FindAttributeByKey(AKey: string
  ): TTagNodeAttribute;
begin
  Result := TTagNodeAttribute.Create(myhtml_attribute_by_key(FNode, PChar(AKey),
    Length(AKey)));
end;

{ TMyHTMLParser.TTagNodeAttribute }

function TMyHTMLParser.TTagNodeAttribute.GetKey: string;
begin
  if IsOk then
  begin
    Result := myhtml_attribute_key(FAttribute, nil);
  end else
    Result := '';
end;

function TMyHTMLParser.TTagNodeAttribute.GetValue: string;
begin
  if IsOk then
  begin
    Result := myhtml_attribute_value(FAttribute, nil);
  end else
    Result := '';
end;

constructor TMyHTMLParser.TTagNodeAttribute.Create (AAttribute :
  pmyhtml_tree_attr_t);
begin
  FAttribute := AAttribute;
end;

destructor TMyHTMLParser.TTagNodeAttribute.Destroy;
begin
  inherited Destroy;
end;

function TMyHTMLParser.TTagNodeAttribute.IsOk: Boolean;
begin
  Result := FAttribute <> nil;
end;

end.

