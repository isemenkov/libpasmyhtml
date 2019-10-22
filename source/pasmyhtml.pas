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
  Classes, SysUtils, libpasmyhtml;

type

  { TMyHTMLParser }

  TMyHTMLParser = class
  public
    type
      (* Start parsing from the next element *)
      TDocumentParseFrom = (
        DOCUMENT_HTML, (* Start parse from HTML tag element *)
        DOCUMENT_HEAD, (* Start parse from HEAD tag element *)
        DOCUMENT_BODY  (* Start parse from BODY tag element *)
      );

    TTagNode = class;

    TTagFilter = function (ANode : TTagNode) : Boolean of object;

    { TTreeChunk }
    (* Document tree or document tree chunk *)
    TTreeChunk = class
    private
      FNode : pmyhtml_tree_node_t;
      FNextNode : pmyhtml_tree_node_t;
      FFilter : TTagFilter;
      FChildrenFilter : TTagFilter;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;

      function IsOk : Boolean;

      (* Return first tree element node *)
      function First : TTagNode;
      function First (AFilter : TTagFilter) : TTagNode;

      function FirstChildren : TTreeChunk;
      function FirstChildren (AFilter : TTagFilter) : TTreeChunk;

      (* Return next tree element node *)
      function Next : TTagNode;
    end;

    TTagAttribute = class;

    TTagAttributeFilter = function (AAttribute : TTagAttribute) : Boolean of
      object;

    { TTagAttribute }
    (* Tag element attribute *)
    TTagAttribute = class
    private
      FKey, FValue : string;

      function GetValue : TStringList;
      procedure SetValue (AValue : TStringList);
    public
      constructor Create;
      constructor Create (AKey : string; AValue : string);
      destructor Destroy; override;

      property Key : string read FKey write FKey;
      property Value : TStringList read GetValue write SetValue;
    end;

    { TTagNode }
    (* Tag element *)
    TTagNode = class
    private
      FNode : pmyhtml_tree_node_t;
      FNextChildrenNode : pmyhtml_tree_node_t;
      FChildrenFilter : TTagFilter;
      FAttribute : pmyhtml_tree_attr_t;
      FAttributeFilter : TTagAttributeFilter;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;

      function IsOk : Boolean;

      (* Return tag element id *)
      function GetTag : myhtml_tag_id_t;

      (* Return tag element class attributes list *)
      function GetClass : TStringList;

      (* Return tag element id attributes list *)
      function GetId : TStringList;

      (* Return tag element value *)
      function GetValue : string;

      (* Return tag element parent element *)
      function GetParent : TTreeChunk;

      (* Return if tag element has attributes *)
      function HasAttribute : Boolean;

      (* Return if tag element has childrens *)
      function HasChildren : Boolean;

      function FirstAttribute : TTagAttribute;
      function FirstAttribute (AFilter : TTagAttributeFilter) : TTagAttribute;

      function NextAttribute : TTagAttribute;

      (* Return first tag element inner element *)
      function FirstChildren : TTreeChunk;
      function FirstChildren (AFilter : TTagFilter) : TTreeChunk;

      (* Return next tag element inner element *)
      function NextChildren : TTreeChunk;
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

{ TMyHTMLParser.TTagAttribute }

function TMyHTMLParser.TTagAttribute.GetValue: TStringList;
begin
  Result := StringTokenize(FValue);
end;

procedure TMyHTMLParser.TTagAttribute.SetValue(AValue: TStringList);
begin
  FValue := AValue.Text;
end;

constructor TMyHTMLParser.TTagAttribute.Create;
begin
  FKey := '';
  FValue := '';
end;

constructor TMyHTMLParser.TTagAttribute.Create(AKey: string; AValue: string);
begin
  FKey := AKey;
  FValue := AValue;
end;

destructor TMyHTMLParser.TTagAttribute.Destroy;
begin
  inherited Destroy;
end;

{ TMyHTMLParser.TTreeChunk }

constructor TMyHTMLParser.TTreeChunk.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
  FNextNode := nil;
end;

destructor TMyHTMLParser.TTreeChunk.Destroy;
begin
  inherited Destroy;
end;

function TMyHTMLParser.TTreeChunk.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TMyHTMLParser.TTreeChunk.First: TTagNode;
begin
  if IsOk then
  begin
    FNextNode := FNode;
    Result := TTagNode.Create(FNextNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.First(AFilter: TTagFilter): TTagNode;
begin
  FFilter := AFilter;

  if IsOk then
  begin
    FNextNode := FNode;
    if Assigned(FFilter) then
    begin
      while not FFilter(TTagNode.Create(FNextNode)) do
      begin
        FNextNode := myhtml_node_next(FNextNode);
      end;
    end;
    Result := TTagNode.Create(FNextNode);
  end else
    Result := TTagNode.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.FirstChildren: TTreeChunk;
begin
  if IsOk then
  begin
    Result := TTreeChunk.Create(myhtml_node_child(FNode));
  end;
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.FirstChildren(AFilter: TTagFilter
  ): TTreeChunk;
var
  ChildrenNode : pmyhtml_tree_node_t;
begin
  FFilter := AFilter;

  if IsOk then
  begin
    ChildrenNode := myhtml_node_child(FNode);
    if Assigned(FFilter) then
    begin
      while not FFilter(TTagNode.Create(ChildrenNode)) do
      begin
        FNextNode := myhtml_node_next(ChildrenNode);
      end;
    end;
    Result := TTreeChunk.Create(ChildrenNode);
  end else
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.TTreeChunk.Next: TTagNode;
begin
  if FNextNode <> nil then
  begin
    FNextNode := myhtml_node_next(FNextNode);
    Result := TTagNode.Create(FNextNode);
  end else
    Result := TTagNode.Create(nil);
end;

{ TMyHTMLParser.TTagNode }

constructor TMyHTMLParser.TTagNode.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
  FNextChildrenNode := nil;
end;

destructor TMyHTMLParser.TTagNode.Destroy;
begin
  inherited Destroy;
end;

function TMyHTMLParser.TTagNode.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TMyHTMLParser.TTagNode.GetTag: myhtml_tag_id_t;
begin
  if IsOk then
  begin
    Result := myhtml_node_tag_id(FNode);
  end else
    Result := myhtml_tag_id_t(MyHTML_TAG__UNDEF);
end;

function TMyHTMLParser.TTagNode.GetClass: TStringList;
var
  Attr : pmyhtml_tree_attr_t;
begin
  if IsOk then
  begin
    Attr := myhtml_attribute_by_key(FNode, PChar('class'), 5);
    if Attr <> nil then
    begin
      Result := StringTokenize(myhtml_attribute_value(Attr, nil));
    end else
      Result := TStringList.Create;
  end else
    Result := TStringList.Create;
end;

function TMyHTMLParser.TTagNode.GetId: TStringList;
begin

end;

function TMyHTMLParser.TTagNode.GetValue: string;
var
  TextNode : pmyhtml_tree_node_t;
  Value : pmycore_string_t;
begin
  if IsOk then
  begin
    TextNode := myhtml_node_child(FNode);
    if (TextNode <> nil) and (myhtml_node_tag_id(TextNode) =
      myhtml_tag_id_t(MyHTML_TAG__TEXT)) then
    begin
      Value := myhtml_node_string(TextNode);
      Result := mycore_string_data(Value);
    end else
      Result := '';
  end else
    Result := '';
end;

function TMyHTMLParser.TTagNode.GetParent: TTreeChunk;
begin
  if IsOk then
  begin
    Result := TTreeChunk.Create(myhtml_node_parent(FNode));
  end else
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.TTagNode.HasAttribute: Boolean;
begin
  if IsOk then
  begin
    Result := myhtml_node_attribute_first(FNode) <> nil;
  end else
    Result := False;
end;

function TMyHTMLParser.TTagNode.HasChildren: Boolean;
begin
  if IsOk then
  begin
    Result := myhtml_node_child(FNode) <> nil;
  end else
    Result := False;
end;

function TMyHTMLParser.TTagNode.FirstAttribute: TTagAttribute;
begin
  if IsOk then
  begin
    FAttribute := myhtml_node_attribute_first(FNode);
    if FAttribute <> nil then
    begin
      Result := TTagAttribute.Create(myhtml_attribute_key(FAttribute, nil),
        myhtml_attribute_value(FAttribute, nil));
    end else
      Result := TTagAttribute.Create;
  end else
    Result := TTagAttribute.Create;
end;

function TMyHTMLParser.TTagNode.FirstAttribute(AFilter: TTagAttributeFilter
  ): TTagAttribute;
begin
  FAttributeFilter := AFilter;

  if IsOk then
  begin
    FAttribute := myhtml_node_attribute_first(FNode);
    if Assigned(FAttributeFilter) then
    begin
      while not FAttributeFilter(TTagAttribute.Create(
        myhtml_attribute_key(FAttribute, nil),
        myhtml_attribute_value(FAttribute, nil))) do
      begin
        FAttribute := myhtml_attribute_next(FAttribute);
      end;
      Result := TTagAttribute.Create(myhtml_attribute_key(FAttribute, nil),
        myhtml_attribute_value(FAttribute, nil));
    end else
      Result := TTagAttribute.Create(myhtml_attribute_key(FAttribute, nil),
        myhtml_attribute_value(FAttribute, nil));
  end else
    Result := TTagAttribute.Create;
end;

function TMyHTMLParser.TTagNode.NextAttribute: TTagAttribute;
begin
  if IsOk and (FAttribute <> nil) then
  begin
    FAttribute := myhtml_attribute_next(FAttribute);
    Result := TTagAttribute.Create(myhtml_attribute_key(FAttribute, nil),
      myhtml_attribute_value(FAttribute, nil));
  end else
    Result := TTagAttribute.Create;
end;

function TMyHTMLParser.TTagNode.FirstChildren: TTreeChunk;
begin
  if IsOk then
  begin
    FNextChildrenNode := myhtml_node_child(FNode);
    Result := TTreeChunk.Create(FNextChildrenNode);
  end else
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.TTagNode.FirstChildren(AFilter: TTagFilter): TTreeChunk;
begin
  FChildrenFilter := AFilter;

  if IsOk then
  begin
    FNextChildrenNode := myhtml_node_child(FNode);
    if Assigned(FChildrenFilter) then
    begin
      while not FChildrenFilter(TTagNode.Create(FNextChildrenNode)) do
      begin
         FNextChildrenNode := myhtml_node_next(FNextChildrenNode);
      end;
    end;
    Result := TTreeChunk.Create(FNextChildrenNode);
  end else
    Result := TTreeChunk.Create(nil);
end;

function TMyHTMLParser.TTagNode.NextChildren: TTreeChunk;
begin
  if IsOk and (FNextChildrenNode <> nil) then
  begin
    FNextChildrenNode := myhtml_node_next(FNextChildrenNode);
    Result := TTreeChunk.Create(FNextChildrenNode);
  end else
    Result := TTreeChunk.Create(nil);
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

end.

