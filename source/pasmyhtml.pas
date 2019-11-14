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
(* Functionality:   Provides  TParser   class  which   present  HTML  parser, *)
(*                  TTagNode class which implements HTML tag and functions to *)
(*                  operations with it.                                       *)
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

      TTag = type myhtml_tags_t;

      { Tag node attribute }
      { Class which implements HTML tag attribute and functions to operation
        with it }
      TTagNodeAttribute = class;
      TTagNodeAttributeList = specialize TFPGObjectList<TTagNodeAttribute>;

      { Tag node }
      { Class implements HTML tag and functions to operation with it }
      TTagNode = class;
      TTagNodeList = specialize TFPGObjectList<TTagNode>;

      { Filter }
      { HTML tag filter callback }
      TTagNodeFilterCallback = function (ANode : TTagNode; AData : Pointer) :
        Boolean of object;

      { HTML tag attribute filter callback }
      TTagNodeAttributeFilterCallback = function (ANodeAttribute :
        TTagNodeAttribute; AData : Pointer) : Boolean of object;

      { Transform }
      { HTML tag node transform callback }
      TTagNodeTransformCallback = procedure (ANode : TTagNode; AData : Pointer)
        of object;

      { HTML tag attribute transform callback }
      TTagNodeAttributeTransformCallback = procedure (ANodeAttribute :
        TTagNodeAttribute; AData : Pointer) of object;

      { TFilter }
      { Class realize filter concept which can be used to find elements }
      TFilter = class
      private
        FTag : myhtml_tags_t;

        FTagNodeCallback : TTagNodeFilterCallback;
        FTagNodeData : Pointer;

        FTagNodeAttributeKey : string;
        FTagNodeAttributeValue : string;

        FTagNodeAttributeClass : TStringList;
        FTagNodeAttributeId : TStringList;

        FTagNodeAttributeCallback : TTagNodeAttributeFilterCallback;
        FTagNodeAttributeData : Pointer;

        { Check if ANode is equal to defined filter }
        function IsEqual (ANode : pmyhtml_tree_node_t = nil; ANodeAttribute :
          pmyhtml_tree_attr_t = nil; ACheckAllAttributes : Boolean = False) :
          Boolean; inline;

        { Return TRUE if filter isn't empty }
        function IsSet : Boolean; inline;
      public
        constructor Create;
        destructor Destroy; override;

        { Set tag id for filtering }
        function Tag (ATag : TTag) : TFilter;

        { Set tag attribute key for filtering }
        function AttributeKey (AKey : string) : TFilter;

        { Set tag attribute value for filtering }
        function AttributeValue (AValue : string) : TFilter;

        { Set class which must be in attribute "class" list }
        { If "class" attrbute is exists new class is added in list }
        function ContainsClass (AClass : string) : TFilter;

        { Set class only which must be in attribute }
        { If "class" attribute is exists in list it is rewriting }
        function ContainsClassOnly (AClass : string) : TFilter;

        { Set id which must be in attribute "id" list }
        { If "id" attrbute is exists new id is added in list }
        function ContainsId (AId : string) : TFilter;

        { Set id only which must be in attribute }
        { If "id" attribute is exists in list it is rewriting }
        function ContainsIdOnly (AId : string) : TFilter;

        { Set tag filtering callback. Set AData pointer to pass it to callback.
          Return self }
        function TagNodeCallback (ACallback : TTagNodeFilterCallback; AData :
          Pointer = nil) : TFilter;

        { Set tag attribute filtering callback. Set AData pointer to pass it to
          callback. Return self }
        function TagNodeAttributeCallback (ACallback :
          TTagNodeAttributeFilterCallback; AData : Pointer = nil) : TFilter;
      end;

      { TTransform }
      { Class provides transform callback wrapper which apply to tag element }
      TTransform = class
      private
        FTagNodeCallback : TTagNodeTransformCallback;
        FTagNodeData : Pointer;

        FTagNodeAttributeCallback : TTagNodeAttributeTransformCallback;
        FTagNodeAttributeData : Pointer;

        { Run transform node callback }
        procedure RunNodeCallback (ATagNode : TTagNode); inline;

        { Run transform node attribute callback }
        procedure RunNodeAttributeCallback (ATagAttribute : TTagNodeAttribute);
          inline;
      public
        constructor Create;
        destructor Destroy; override;

        { Set tag transform callback function. Set AData to pass it to transform
          callback. Return self }
        function TagNodeTransform (ACallback : TTagNodeTransformCallback;
          AData : Pointer = nil) : TTransform;

        { Set tag attribute callback function. Set AData to pass it to transform
          callback. Return self }
        function TagNodeAttributeTransform (ACallback :
          TTagNodeAttributeTransformCallback; AData : Pointer) : TTransform;
      end;

      { TTagNode }
      { Class implements HTML tag and functions to operation with it }
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

        { Apply AFilter to ANode element if it is. Return pmyhtml_tree_node_t if
          element find or nil }
        function FilterNode (ANode : pmyhtml_tree_node_t; AFilter : TFilter) :
          pmyhtml_tree_node_t; inline;

        { Apply AFilter to AAttribute if it is. Return filtered
          pmyhtml_tree_attr_t element if find or nil }
        function FilterAttribute (AAttribute : pmyhtml_tree_attr_t; AFilter :
          TFilter) : pmyhtml_tree_attr_t; inline;
      private
        { Return tag id }
        function GetTag :  TTag; inline;

        { Return tag text value }
        function GetValue : string; inline;
      public
        constructor Create (ANode : pmyhtml_tree_node_t);
        destructor Destroy; override;

        { Return TRUE if tag element is correct }
        function IsOk : Boolean; inline;

        { If AFilter is set return first filtered node else return self }
        function FirstNode (AFilter : TFilter = nil) : TTagNode;

        { If FirstNode AFilter is set apply it to try to find next element else
          return next node or nil if isn't }
        function NextNode : TTagNode;

        { For each node (if AFilter is present for filtered nodes, else for each
          nodes) apply ATransform callback. If ATransform isn't do nothing }
        function EachNode (AFilter : TFilter = nil; ATransform : TTransform
          = nil) : TTagNode;

        { Return all AFilter match nodes list. If AFilter is not present return
          all nodes list }
        function FindAllNodes (AFilter : TFilter = nil) : TTagNodeList;

        { Return first filtered current node children. If AFilter isn't return
          first children node. If node not found return broken node }
        function FirstChildrenNode (AFilter : TFilter = nil) : TTagNode;

        { Return next filtered children node. If node node fount return brken
          node }
        function NextChildrenNode : TTagNode;

        { For each filtered current node childrens applies ATransfrom callback.
          If ATransform isn't do nothing }
        function EachChildrenNode (AFilter : TFilter = nil; ATransform :
          TTransform = nil) : TTagNode;

        { Return all current node filtered childrens list. If AFilter is not
          present return all children nodes list }
        function FindAllChildrenNodes (AFilter : TFilter = nil) : TTagNodeList;

        { Find first node attribute using AFilter if it is else return first
          attribute. If node haven't attributes return broken attribute }
        function FirstNodeAttribute (AFilter : TFilter = nil) :
          TTagNodeAttribute;

        { Return next node attribute appling FirstNodeAttribute AFilter if
          exists. If node attribute not exists return broken attribute }
        function NextNodeAttribute : TTagNodeAttribute;

        { For each node node attribute (if AFilter is present for filtered
          attributes, else for each node attributes) apply ATransform callback.
          If ATransform isn't do nothing }
        function EachNodeAttribute (AFilter : TFilter = nil; ATransform :
          TTransform = nil) : TTagNode;

        { Find all filtered node attributes. If AFilter isn't return all
          attributes }
        function FindAllNodeAttributes (AFilter : TFilter = nil) :
          TTagNodeAttributeList;
      public
        { Return tag id }
        property Tag : TTag read GetTag;

        { Return tag text value }
        property Value : string read GetValue;
      end;

      { TTagNodeAttribute }
      { Class which implements HTML tag attribute and functions to operation
        with it }
      TTagNodeAttribute = class
      private
        FAttribute : pmyhtml_tree_attr_t;

        { Tokenize string by space }
        class function StringTokenize (AString : string) : TStringList;
      private
        { Return attribute key }
        function GetKey : string;

        { Return attribute value }
        function GetValue : string;

        { Return attribute values tokenize by space }
        function GetValueList : TStringList;
      public
        constructor Create (AAttribute : pmyhtml_tree_attr_t);
        destructor Destroy; override;

        { Check if attribute is correct }
        function IsOk : boolean; inline;
      public
        { Return attribute key }
        property Key : string read GetKey;

        { Return attribute value }
        property Value : string read GetValue;

        { Return attribute values tokenize by space }
        property ValueList : TStringList read GetValueList;
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

implementation

{ TParser.TFilter }

function TParser.TFilter.IsEqual(ANode: pmyhtml_tree_node_t;
  ANodeAttribute: pmyhtml_tree_attr_t; ACheckAllAttributes : Boolean): Boolean;

  { Return TRUE if ANode is equal to TFilter.Tag }
  function NodeEqual : Boolean;

    function TagEqual : Boolean; inline;
    begin
      Result := (FTag = MyHTML_TAG__UNDEF) { node id is filtered }
        or (FTag = myhtml_tags_t(myhtml_node_tag_id(ANode)));
    end;

    function TagCallbackEqual : Boolean; inline;
    begin
      { node filter callback is present }
      if Assigned(FTagNodeCallback) then
        Result := FTagNodeCallback(TTagNode.Create(ANode), FTagNodeData)
      else
        Result := True;
    end;

  begin
    if ANode = nil then { node isn't present }
    begin
      if (FTag = MyHTML_TAG__UNDEF) then { node not filtering }
        Result := True
      else
        Result := False;
    end
    else begin
      Result := TagEqual and TagCallbackEqual;
    end;
  end;

  { Check if ANodeAttribute is equal to TFilter.Attribute }
  function NodeAttributeEqual : Boolean;

    function AttributeKeyEqual (AAttribute : pmyhtml_tree_attr_t) : Boolean;
      inline;
    begin
      { node attribute is filtering by key }
      if FTagNodeAttributeKey <> '' then
      begin
        Result := FTagNodeAttributeKey = myhtml_attribute_key(AAttribute, nil);
      end else
        Result := True;
    end;

    function AttributeValueEqual (AAttribute : pmyhtml_tree_attr_t) : Boolean;
      inline;
    begin
      { node attribute is filtering by value }
      if FTagNodeAttributeValue <> '' then
      begin
        Result := FTagNodeAttributeValue =
          myhtml_attribute_value(AAttribute, nil);
      end else
        Result := True;
    end;

    function AttributeClassEqual (AAttribute : pmyhtml_tree_attr_t) : Boolean;
      inline;
    var
      ClassList : TStringList;
      ClassElement : string;
    begin
      if (FTagNodeAttributeClass.Count > 0) and
        (myhtml_attribute_key(AAttribute, nil) = 'class') then
      begin
        ClassList := TTagNodeAttribute.StringTokenize(myhtml_attribute_value(
          AAttribute, nil));
        if (ClassList.Count < FTagNodeAttributeClass.Count) then
          Result := False
        else begin
          Result := True;
          for ClassElement in FTagNodeAttributeClass do
          begin
            if ClassList.IndexOf(ClassElement) = -1 then
            begin
              Result := False;
              Break;
            end;
          end;
        end;
      end else
        Result := True;
    end;

    function AttributeIdEqual (AAttribute : pmyhtml_tree_attr_t) : Boolean;
      inline;
    var
      IdList : TStringList;
      IdElement : string;
    begin
      if (FTagNodeAttributeId.Count > 0) and
        (myhtml_attribute_key(AAttribute, nil) = 'id') then
      begin
        IdList := TTagNodeAttribute.StringTokenize(myhtml_attribute_value(
          AAttribute, nil));
        if (IdList.Count < FTagNodeAttributeId.Count) then
          Result := False
        else begin
          Result := True;
          for IdElement in FTagNodeAttributeId do
          begin
            if IdList.IndexOf(IdElement) = -1 then
            begin
              Result := False;
              Break;
            end;
          end;
        end;
      end else
        Result := True;
    end;

    function AttributeCallbackEqual (AAttribute : pmyhtml_tree_attr_t) :
      Boolean; inline;
    begin
      { node attribute filtering callback is present }
      if Assigned(FTagNodeAttributeCallback) then
        Result := FTagNodeAttributeCallback(TTagNodeAttribute.Create(
          AAttribute), FTagNodeAttributeData)
      else
        Result := True;
    end;

    { Check concrete node attribute }
    function CheckNodeAttribute (AAttribute : pmyhtml_tree_attr_t) : Boolean;
      inline;
    begin
      Result := ((FTagNodeAttributeKey = '') or AttributeKeyEqual(AAttribute))
        and ((FTagNodeAttributeValue = '') or AttributeValueEqual(AAttribute))
        and ((not Assigned(FTagNodeAttributeCallback)) or
          AttributeCallbackEqual(AAttribute))
        and ((FTagNodeAttributeClass.Count = 0) or
          AttributeClassEqual(AAttribute))
        and ((FTagNodeAttributeId.Count = 0) or AttributeIdEqual(AAttribute));
    end;

    { Check all node attributes start from ANodeAttribute }
    function CheckNodeAttributes : Boolean; inline;
    var
      NodeAttr : pmyhtml_tree_attr_t;
    begin
      NodeAttr := ANodeAttribute;

      if ACheckAllAttributes then
      begin
        while (NodeAttr <> nil) and not CheckNodeAttribute(NodeAttr) do
        begin
          NodeAttr := myhtml_attribute_next(NodeAttr);
        end;

        Result := NodeAttr <> nil;
      end else
        Result := CheckNodeAttribute(NodeAttr);
    end;

  begin
    if (ANodeAttribute = nil) then { node is not filtering by tag }
    begin
      if (FTagNodeAttributeKey = '') and (FTagNodeAttributeClass.Count = 0)
        and (FTagNodeAttributeValue = '') and (FTagNodeAttributeId.Count = 0)
      then
        Result := True
      else
        Result := False;
    end
    else begin
      Result := CheckNodeAttributes;
    end;
  end;

begin
  Result := NodeEqual and NodeAttributeEqual;
end;

function TParser.TFilter.IsSet: Boolean;
begin
  Result := (FTag <> MyHTML_TAG__UNDEF) or (FTagNodeAttributeKey <> '') or
    (FTagNodeAttributeClass.Count > 0) or (FTagNodeAttributeId.Count > 0) or
    (FTagNodeAttributeValue <> '') or Assigned(FTagNodeCallback) or
    Assigned(FTagNodeAttributeCallback);
end;

constructor TParser.TFilter.Create;
begin
  FTag := MyHTML_TAG__UNDEF;
  FTagNodeCallback := nil;
  FTagNodeData := nil;
  FTagNodeAttributeKey := '';
  FTagNodeAttributeValue := '';
  FTagNodeAttributeClass := TStringList.Create;
  FTagNodeAttributeId := TStringList.Create;
  FTagNodeAttributeCallback := nil;
  FTagNodeAttributeData := nil;
end;

destructor TParser.TFilter.Destroy;
begin
  FreeAndNil(FTagNodeAttributeClass);
  FreeAndNil(FTagNodeAttributeId);
  inherited Destroy;
end;

function TParser.TFilter.Tag(ATag: TTag): TFilter;
begin
  FTag := ATag;
  Result := Self;
end;

function TParser.TFilter.AttributeKey(AKey: string): TFilter;
begin
  FTagNodeAttributeKey := AKey;
  Result := Self;
end;

function TParser.TFilter.AttributeValue(AValue: string): TFilter;
begin
  FTagNodeAttributeValue := AValue;
  Result := Self;
end;

function TParser.TFilter.ContainsClass(AClass: string): TFilter;
begin
  FTagNodeAttributeClass.AddStrings(TTagNodeAttribute.StringTokenize(AClass));
  Result := Self;
end;

function TParser.TFilter.ContainsClassOnly(AClass: string): TFilter;
begin
  FTagNodeAttributeClass.Clear;
  FTagNodeAttributeClass.AddStrings(TTagNodeAttribute.StringTokenize(AClass));
  Result := Self;
end;

function TParser.TFilter.ContainsId(AId: string): TFilter;
begin
  FTagNodeAttributeId.AddStrings(TTagNodeAttribute.StringTokenize(AId));
  Result := Self;
end;

function TParser.TFilter.ContainsIdOnly(AId: string): TFilter;
begin
  FTagNodeAttributeId.Clear;
  FTagNodeAttributeId.AddStrings(TTagNodeAttribute.StringTokenize(AId));
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

procedure TParser.TTransform.RunNodeCallback(ATagNode: TTagNode);
begin
  if Assigned(FTagNodeCallback) then
    FTagNodeCallback(ATagNode, FTagNodeData);
end;

procedure TParser.TTransform.RunNodeAttributeCallback(
  ATagAttribute: TTagNodeAttribute);
begin
  if Assigned(FTagNodeAttributeCallback) then
    FTagNodeAttributeCallback(ATagAttribute, FTagNodeAttributeData);
end;

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

  if (AFilter <> nil) and AFilter.IsSet then
  begin
    while (Node <> nil) and (not AFilter.IsEqual(Node,
      myhtml_node_attribute_first(Node), True)) do
    begin
      Node := myhtml_node_next(Node);
    end;
  end;

  Result := Node;
end;

function TParser.TTagNode.FilterAttribute(AAttribute: pmyhtml_tree_attr_t;
  AFilter: TFilter): pmyhtml_tree_attr_t;
var
  Attr : pmyhtml_tree_attr_t;
begin
  Attr := AAttribute;

  if AFilter.IsSet then
  begin
    while (Attr <> nil) and (not AFilter.IsEqual(nil, Attr, False)) do
    begin
      Attr := myhtml_attribute_next(Attr);
    end;
  end;

  Result := Attr;
end;

function TParser.TTagNode.GetTag: TTag;
begin
  if IsOk then
  begin
    Result := TTag(myhtml_node_tag_id(FNode));
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
    Result := TTagNode.Create(FilterNode(myhtml_node_next(FNode), FNodeFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.EachNode(AFilter: TFilter; ATransform: TTransform) :
  TTagNode;
var
  Node : TTagNode;
begin
  if IsOk then
  begin
    Node := FirstNode(AFilter);
    while Node.IsOk do
    begin
      ATransform.RunNodeCallback(Node);
      Node := NextNode;
    end;
  end;

  Result := Self;
end;

function TParser.TTagNode.FindAllNodes(AFilter: TFilter): TTagNodeList;
var
  Node : TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNodeList.Create;

    Node := FirstNode(AFilter);
    while Node.IsOk do
    begin
      Result.Add(Node);
      Node := NextNode;
    end;

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
    Result.FChildrenNodeFilter := AFilter;
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.NextChildrenNode: TTagNode;
begin
  if FChildrenNode <> nil then
  begin
    FChildrenNode := myhtml_node_next(FChildrenNode);
    Result := TTagNode.Create(FilterNode(FChildrenNode, FChildrenNodeFilter));
    Result.FChildrenNodeFilter := FChildrenNodeFilter;
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.EachChildrenNode(AFilter: TFilter;
  ATransform: TTransform) : TTagNode;
var
  Node : TTagNode;
begin
  if IsOk then
  begin
    Node := FirstChildrenNode(AFilter);
    while Node.IsOk do
    begin
      ATransform.RunNodeCallback(Node);
      Node := NextChildrenNode;
    end;
  end;

  Result := Self;
end;

function TParser.TTagNode.FindAllChildrenNodes(AFilter: TFilter): TTagNodeList;
var
  Node : TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNodeList.Create;

    Node := FirstChildrenNode(AFilter);
    while Node.IsOk do
    begin
      Result.Add(Node);
      Node := NextChildrenNode;
    end;

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
   Result := TTagNodeAttribute.Create(FilterAttribute(FNodeAttribute,
     FNodeAttributeFilter));
 end else
   Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.NextNodeAttribute: TTagNodeAttribute;
begin
  if FNodeAttribute <> nil then
  begin
    Result := TTagNodeAttribute.Create(FilterAttribute(
      myhtml_attribute_next(FNodeAttribute), FNodeAttributeFilter));
  end else
    Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.EachNodeAttribute(AFilter: TFilter;
  ATransform: TTransform) : TTagNode;
var
  Attribute : TTagNodeAttribute;
begin
  if IsOk then
  begin
    Attribute := FirstNodeAttribute(AFilter);
    while Attribute.IsOk do
    begin
      ATransform.RunNodeAttributeCallback(Attribute);
      Attribute := NextNodeAttribute;
    end;
  end;

  Result := Self;
end;

function TParser.TTagNode.FindAllNodeAttributes(AFilter: TFilter
  ): TTagNodeAttributeList;
var
  Attribute : TTagNodeAttribute;
begin
  if IsOk then
  begin
    Result := TTagNodeAttributeList.Create;

    Attribute := FirstNodeAttribute(AFilter);
    while Attribute.IsOk do
    begin
      Result.Add(Attribute);
      Attribute := NextNodeAttribute;
    end;

  end else
    Result := TTagNodeAttributeList.Create;
end;

{ TParser.TTagNodeAttribute }

class function TParser.TTagNodeAttribute.StringTokenize(AString: string):
  TStringList;
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

function TParser.TTagNodeAttribute.GetValueList: TStringList;
begin
  if IsOk then
  begin
    Result := StringTokenize(myhtml_attribute_value(FAttribute, nil));
  end else
    Result := TStringList.Create;
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
     begin
       { Function myhtml_tree_get_node_body contains bug }
       { When you can take children node its tag id is MyHTML_TAG__TEXT }
       { Result := TTagNode.Create(myhtml_tree_get_node_body(FTree)); }
       Result := TTagNode.Create(myhtml_tree_get_node_html(FTree));
       Result := Result.FirstChildrenNode(TFilter.Create.Tag(MyHTML_TAG_BODY));
     end;
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

