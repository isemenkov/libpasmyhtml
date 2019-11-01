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
(* Functionality:   Provides  TParser class                                   *)
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

  { TParser }

  TParser = class
  public
    type
      { Start parsing from the next element }
      TDocumentParseFrom = (
        DOCUMENT_ROOT, { Start parse from document root }
        DOCUMENT_HTML, { Start parse from HTML tag element }
        DOCUMENT_HEAD, { Start parse from HEAD tag element }
        DOCUMENT_BODY  { Start parse from BODY tag element }
      );

      { Tag node attribute wrapper }
      TTagNodeAttribute = class;
      TTagNodeAttributeList = specialize TFPGObjectList<TTagNodeAttribute>;

      { Tag node }
      TTagNode = class;
      TTagNodeList = specialize TFPGObjectList<TTagNode>;

      { Filter callback functions }
      TTagNodeFilterCallback = function (ANode : TTagNode; AData : Pointer) :
        Boolean of object;
      TTagNodeAttributeFilterCallback = function (ANodeAttribute :
        TTagNodeAttribute; AData : Pointer) : Boolean of object;

      { Transform callbacks }
      TTagNodeTransformCallback = procedure (ANode : TTagNode; AData : Pointer)
        of object;
      TTagNodeAttributeTransformCallback = procedure (ANodeAttribute :
        TTagNodeAttribute; AData : Pointer) of object;

      { TFilter }

      TFilter = class
      private
        FTag : myhtml_tags_t;

        FTagNodeCallback : TTagNodeFilterCallback;
        FTagNodeData : Pointer;

        FTagNodeAttributeCallback : TTagNodeAttributeFilterCallback;
        FTagNodeAttributeData : Pointer;

        function IsEqual (ANode : pmyhtml_tree_node_t = nil; ANodeAttribute :
          pmyhtml_tree_attr_t = nil) : Boolean; inline;
        function IsSet : Boolean; inline;
      public
        constructor Create;
        destructor Destroy; override;

        function Tag (ATag : myhtml_tags_t) : TFilter;

        function TagNodeCallback (ACallback : TTagNodeFilterCallback; AData :
          Pointer = nil) : TFilter;
        function TagNodeAttributeCallback (ACallback :
          TTagNodeAttributeFilterCallback; AData : Pointer = nil) : TFilter;
      end;

      { TTransform }

      TTransform = class
      private
        FTagNodeCallback : TTagNodeTransformCallback;
        FTagNodeData : Pointer;

        FTagNodeAttributeCallback : TTagNodeAttributeTransformCallback;
        FTagNodeAttributeData : Pointer;
      public
        constructor Create;
        destructor Destroy; override;

        function TagNodeTransform (ACallback : TTagNodeTransformCallback;
          AData : Pointer = nil) : TTransform;
        function TagNodeAttributeTransform (ACallback :
          TTagNodeAttributeTransformCallback; AData : Pointer) : TTransform;
      end;

      { TTagNode }

      TTagNode = class
      private
        FNode : pmyhtml_tree_node_t;
        FNodeFilter : TFilter;
        FNodeTransform : TTransform;

        FChildrenNode : pmyhtml_tree_node_t;
        FChildrenNodeFilter : TFilter;
        FChildrenNodeTransform : TTransform;

        FNodeAttribute : pmyhtml_tree_attr_t;
        FNodeAttributeFilter : TFilter;
        FNodeAttributeTransform : TTransform;

        function FilterNode (ANode : pmyhtml_tree_node_t; AFilter : TFilter) :
          pmyhtml_tree_node_t; inline;
      private
        function GetTag :  myhtml_tags_t; inline;
        function GetValue : string;
      public
        constructor Create (ANode : pmyhtml_tree_node_t);
        destructor Destroy; override;

        function IsOk : Boolean; inline;

        function FirstNode (AFilter : TFilter = nil) : TTagNode;
        function NextNode : TTagNode;
        procedure EachNode (AFilter : TFilter = nil; ATransform : TTransform
          = nil);
        function FindAllNodes (AFilter : TFilter = nil) : TTagNodeList;

        function FirstChildrenNode (AFilter : TFilter = nil) : TTagNode;
        function NextChildrenNode : TTagNode;
        procedure EachChildrenNode (AFilter : TFilter = nil; ATransform :
          TTransform = nil);
        function FindAllChildrenNodes (AFilter : TFilter = nil) : TTagNodeList;

        function FirstNodeAttribute (AFilter : TFilter = nil) :
          TTagNodeAttribute;
        function NextNodeAttribute : TTagNodeAttribute;
        procedure EachNodeAttribute (AFilter : TFilter = nil; ATransform :
          TTransform = nil);
        function FindAllNodeAttributes (AFilter : TFilter = nil) :
          TTagNodeAttributeList;
      public
        property Tag : myhtml_tags_t read GetTag;
        property Value : string read GetValue;
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

        function IsOk : boolean; inline;
      public
        property Key : string read GetKey;
        property Value : string read GetValue;
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
      DOCUMENT_HTML) : TTagNode;

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

{ TParser.TFilter }

function TParser.TFilter.IsEqual(ANode: pmyhtml_tree_node_t;
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True;

  if (ANode = nil) and (ANodeAttribute = nil) then
  begin
    Exit;
  end;

  if ANode <> nil then
  begin
    if FTag <> MyHTML_TAG__UNDEF then
    begin
      Result := myhtml_tags_t(myhtml_node_tag_id(ANode)) = FTag;
      Exit;
    end;

    if Assigned(FTagNodeCallback) and not FTagNodeCallback(TTagNode.Create(
      ANode), FTagNodeData) then
    begin
      Result := False;
      Exit;
    end;
  end;

  if ANodeAttribute <> nil then
  begin
    if Assigned(FTagNodeAttributeCallback) and not FTagNodeAttributeCallback(
      TTagNodeAttribute.Create(ANodeAttribute), FTagNodeAttributeData) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TParser.TFilter.IsSet: Boolean;
begin
  Result := (FTag <> MyHTML_TAG__UNDEF) or Assigned(FTagNodeCallback) or
    Assigned(FTagNodeAttributeCallback);
end;

constructor TParser.TFilter.Create;
begin
  FTag := MyHTML_TAG__UNDEF;
  FTagNodeCallback := nil;
  FTagNodeData := nil;
  FTagNodeAttributeCallback := nil;
  FTagNodeAttributeData := nil;
end;

destructor TParser.TFilter.Destroy;
begin
  inherited Destroy;
end;

function TParser.TFilter.Tag(ATag: myhtml_tags_t): TFilter;
begin
  FTag := ATag;
  Result := Self;
end;

function TParser.TFilter.TagNodeCallback(
  ACallback: TTagNodeFilterCallback; AData: Pointer): TFilter;
begin
  FTagNodeCallback := ACallback;
  FTagNodeData := AData;
  Result := Self;
end;

function TParser.TFilter.TagNodeAttributeCallback(
  ACallback: TTagNodeAttributeFilterCallback; AData: Pointer): TFilter;
begin
  FTagNodeAttributeCallback := ACallback;
  FTagNodeAttributeData := AData;
  Result := Self;
end;

{ TParser.TTransform }

constructor TParser.TTransform.Create;
begin
  FTagNodeCallback := nil;
  FTagNodeData := nil;
  FTagNodeAttributeCallback := nil;
  FTagNodeAttributeData := nil;
end;

destructor TParser.TTransform.Destroy;
begin
  inherited Destroy;
end;

function TParser.TTransform.TagNodeTransform(
  ACallback: TTagNodeTransformCallback; AData: Pointer): TTransform;
begin
  FTagNodeCallback := ACallback;
  FTagNodeData := AData;
  Result := Self;
end;

function TParser.TTransform.TagNodeAttributeTransform(
  ACallback: TTagNodeAttributeTransformCallback; AData: Pointer): TTransform;
begin
  FTagNodeAttributeCallback := ACallback;
  FTagNodeAttributeData := AData;
  Result := Self;
end;

{ TParser.TTagNode }

function TParser.TTagNode.FilterNode(ANode: pmyhtml_tree_node_t;
  AFilter: TFilter): pmyhtml_tree_node_t;
var
  Node : pmyhtml_tree_node_t;
begin
  Node := ANode;

  if AFilter.IsSet then
  begin
    while (Node <> nil) and (not AFilter.IsEqual(Node, nil)) do
    begin
      Node := myhtml_node_next(Node);
    end;
  end;

  Result := Node;
end;

function TParser.TTagNode.GetTag: myhtml_tags_t;
begin
  if IsOk then
  begin
    Result := myhtml_tags_t(myhtml_node_tag_id(FNode));
  end else
    Result := MyHTML_TAG__UNDEF;
end;

function TParser.TTagNode.GetValue: string;
var
  TextNode : pmyhtml_tree_node_t;
begin
  if IsOk then
  begin
    TextNode := myhtml_node_child(FNode);
    if (TextNode <> nil) and (myhtml_tags_t(myhtml_node_tag_id(TextNode)) =
      MyHTML_TAG__TEXT) then
    begin
      Result := myhtml_node_text(TextNode, nil);
    end else
      Result := '';
  end else
    Result := '';
end;

constructor TParser.TTagNode.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
  FChildrenNode := nil;
  FNodeAttribute := nil;

  FNodeFilter := nil;
  FNodeTransform := nil;

  FChildrenNodeFilter := nil;
  FChildrenNodeTransform := nil;

  FNodeAttributeFilter := nil;
  FNodeAttributeTransform := nil;
end;

destructor TParser.TTagNode.Destroy;
begin
  inherited Destroy;
end;

function TParser.TTagNode.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TParser.TTagNode.FirstNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    FNodeFilter := AFilter;
    Result := TTagNode.Create(FilterNode(FNode, FNodeFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.NextNode: TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(myhtml_node_next(FNode));
  end else
    Result := TTagNode.Create(nil);
end;

procedure TParser.TTagNode.EachNode(AFilter: TFilter; ATransform: TTransform);
begin
  if IsOk then
  begin

  end;
end;

function TParser.TTagNode.FindAllNodes(AFilter: TFilter): TTagNodeList;
begin
  if IsOk then
  begin
    Result := TTagNodeList.Create;

  end else
    Result := TTagNodeList.Create;
end;

function TParser.TTagNode.FirstChildrenNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    FChildrenNodeFilter := AFilter;
    FChildrenNode := myhtml_node_child(FNode);
    Result := TTagNode.Create(FilterNode(FChildrenNode, FChildrenNodeFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.NextChildrenNode: TTagNode;
begin
  if FChildrenNode <> nil then
  begin
    FChildrenNode := myhtml_node_next(FChildrenNode);
    Result := TTagNode.Create(FChildrenNode);
  end else
    Result := TTagNode.Create(nil);
end;

procedure TParser.TTagNode.EachChildrenNode(AFilter: TFilter;
  ATransform: TTransform);
begin
  if IsOk then
  begin

  end;
end;

function TParser.TTagNode.FindAllChildrenNodes(AFilter: TFilter): TTagNodeList;
begin
  if IsOk then
  begin
    Result := TTagNodeList.Create;
  end else
    Result := TTagNodeList.Create;
end;

function TParser.TTagNode.FirstNodeAttribute(AFilter: TFilter
  ): TTagNodeAttribute;
begin
 if IsOk then
 begin
   FNodeAttributeFilter := AFilter;
   FNodeAttribute := myhtml_node_attribute_first(FNode);
   Result := TTagNodeAttribute.Create(FNodeAttribute);
 end else
   Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.NextNodeAttribute: TTagNodeAttribute;
begin
  if FNodeAttribute <> nil then
  begin
    Result := TTagNodeAttribute.Create(myhtml_attribute_next(FNodeAttribute));
  end else
    Result := TTagNodeAttribute.Create(nil);
end;

procedure TParser.TTagNode.EachNodeAttribute(AFilter: TFilter;
  ATransform: TTransform);
begin
  if IsOk then
  begin

  end;
end;

function TParser.TTagNode.FindAllNodeAttributes(AFilter: TFilter
  ): TTagNodeAttributeList;
begin
  if IsOk then
  begin
    Result := TTagNodeAttributeList.Create;
  end else
    Result := TTagNodeAttributeList.Create;
end;

{ TParser.TTagNodeAttribute }

function TParser.TTagNodeAttribute.GetKey: string;
begin
  if IsOk then
  begin
    Result := myhtml_attribute_key(FAttribute, nil);
  end else
    Result := '';
end;

function TParser.TTagNodeAttribute.GetValue: string;
begin
  if IsOk then
  begin
    Result := myhtml_attribute_value(FAttribute, nil);
  end else
    Result := '';
end;

constructor TParser.TTagNodeAttribute.Create(AAttribute: pmyhtml_tree_attr_t);
begin
  FAttribute := AAttribute;
end;

destructor TParser.TTagNodeAttribute.Destroy;
begin
  inherited Destroy;
end;

function TParser.TTagNodeAttribute.IsOk: boolean;
begin
  Result := FAttribute <> nil;
end;

{ TParser }

constructor TParser.Create(AParserOptions: myhtml_options_t;
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

destructor TParser.Destroy;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);
  myhtml_tree_destroy(FTree);
  myhtml_destroy(FHTML);
  inherited Destroy;
end;

function TParser.Parse(AHTML: string; AParseFrom: TDocumentParseFrom
  ): TTagNode;
begin
  myhtml_tree_clean(FTree);
  myhtml_clean(FHTML);

  FError := myhtml_status_t(myhtml_parse(FTree, FEncoding, PChar(AHTML),
    Length(AHTML)));
  if FError = MyHTML_STATUS_OK then
  begin
    case AParseFrom of
      DOCUMENT_ROOT :
        Result := TTagNode.Create(myhtml_tree_get_document(FTree));
      DOCUMENT_HTML :
        Result := TTagNode.Create(myhtml_tree_get_node_html(FTree));
      DOCUMENT_HEAD :
        Result := TTagNode.Create(myhtml_tree_get_node_head(FTree));
      DOCUMENT_BODY :
        Result := TTagNode.Create(myhtml_tree_get_node_body(FTree));
    end;
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.HasErrors: Boolean;
begin
  Result := FError <> MyHTML_STATUS_OK;
end;

function TParser.Error: string;
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

