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
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

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

      { Custom filter interface }
      IFilter = class
      protected
        function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; virtual;
          abstract; overload;

        function IsEqual (ANodeAttribute : pmyhtml_tree_attr_t) : Boolean;
          virtual; abstract; overload;
      end;

      TFilter = class;
      TTransform = class;

      { TTagNode }
      { Class implements HTML tag and functions to operation with it }
      TTagNode = class
      public
        type
          { TTagNodeEnumerator }
          { TagNode enumerator }
          TTagNodeEnumerator = class
          protected
            FNode : TTagNode;
            FFilter : TFilter;
            function GetCurrent : TTagNode; inline;
          public
            constructor Create (ANode : TTagNode; AFilter : TFilter = nil);
            function MoveNext : Boolean; inline;
            function GetEnumerator : TTagNodeEnumerator; inline;
            property Current : TTagNode read GetCurrent;
          end;

        { Get TagNode enumerator }
        function GetEnumerator : TTagNodeEnumerator; inline;

        { GetTagNode filtered enumerator }
        function Filter (AFilter : TFilter = nil) : TTagNodeEnumerator; inline;
      private
        FNode : pmyhtml_tree_node_t;
        FNodeAttribute : pmyhtml_tree_attr_t;

        { Apply AFilter to ANode element if it is. Return pmyhtml_tree_node_t if
          element find or nil }
        function FilterNode (ANode : pmyhtml_tree_node_t; AFilter : TFilter) :
          pmyhtml_tree_node_t; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Apply AFilter to ANode element if it is. Return pmyhtml_tree_node_t if
          element find or nil. Try to find node is reverse direction }
        function ReverseFilterNode (ANode : pmyhtml_tree_node_t; AFilter :
          TFilter) : pmyhtml_tree_Node_t; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Apply AFilter to AAttribute if it is. Return filtered
          pmyhtml_tree_attr_t element if find or nil }
        function FilterAttribute (AAttribute : pmyhtml_tree_attr_t; AFilter :
          TFilter) : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;{$ENDIF}

        function ReverseFilterAttribute (AAttribute : pmyhtml_tree_attr_t;
          AFilter : TFIlter) : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;
          {$ENDIF}
      private
        { Return tag id }
        function GetTag :  TTag; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return tag text value }
        function GetValue : string; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return node parent if exists, broken node overwise }
        function GetParent : TTagNode; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return TRUE if element is empty }
        function GetIsEmpty : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return TRUE if element is self-closed }
        function GetIsSelfClose : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get/Set custom data from/to node }
        function GetData : Pointer; {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetData (AData : Pointer); {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create (ANode : pmyhtml_tree_node_t);
        destructor Destroy; override;

        { Return TRUE if tag element is correct }
        function IsOk : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

        { If AFilter is set return first filtered node else return self }
        function FirstNode (AFilter : TFilter = nil) : TTagNode;

        { Try to find next element else return nil if isn't }
        function NextNode (AFilter : TFilter = nil) : TTagNode;

        { Try to find prev element else return nil if isn't }
        function PrevNode (AFilter : TFilter = nil) : TTagNode;

        { For each node (if AFilter is present for filtered nodes, else for each
          nodes) apply ATransform callback. If ATransform isn't do nothing }
        function EachNode (AFilter : TFilter = nil; ATransform : TTransform
          = nil) : TTagNode;

        { Return all AFilter match nodes list. If AFilter is not present return
          all nodes list }
        function FindAllNodes (AFilter : TFilter = nil) : TTagNodeList;

        { Return first filtered current node children. If AFilter isn't present
          return first children node. If node not found return broken node }
        function FirstChildrenNode (AFilter : TFilter = nil) : TTagNode;

        { Return last filtered current node children. If AFilter isn't present
          return last children node. If node not found retun broken node }
        function LastChildrenNode (AFilter : TFilter = nil) : TTagNode;

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

        { Find last node attribute using AFilter if it is else return last
          attribute. If node haven't attributes return broken attribute }
        function LastNodeAttribute (AFilter : TFilter = nil) :
          TTagNodeAttribute;

        { Return next node attribute applying AFilter if exists. If node
          attribute not exists return broken attribute }
        function NextNodeAttribute (AFilter : TFilter = nil) :
          TTagNodeAttribute;

        { Retunr prev node attribute applying AFilter if exists. If node
          attribute not exists return broken attribute }
        function PrevNodeAttribute (AFilter : TFilter = nil) :
          TTagNodeAttribute;

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

        { Return parent node is exists, broken node overwise }
        property Parent : TTagNode read GetParent;

        { Return TRUE if element is void }
        property IsEmpty : Boolean read GetIsEmpty;

        { Return TRUE if element is self-closed }
        property IsSelfClose : Boolean read GetIsSelfClose;

        { Custom data pointer stored in node }
        property Data : Pointer read GetData write SetData;

        { Return tag text value }
        property Value : string read GetValue;

        { Return tag text value through inside tags. Return all tags
          concatinated text values for all nodes }
        function ConcatValue (AFilter : TFilter = nil) : string;
          {$IFNDEF DEBUG}inline;{$ENDIF}
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
        function GetKey : string; inline;

        { Return attribute value }
        function GetValue : string; inline;

        { Return attribute values tokenize by space }
        function GetValueList : TStringList;
      public
        constructor Create (AAttribute : pmyhtml_tree_attr_t);
        destructor Destroy; override;

        { Check if attribute is correct }
        function IsOk : boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        { Return attribute key }
        property Key : string read GetKey;

        { Return attribute value }
        property Value : string read GetValue;

        { Return attribute values tokenize by space }
        property ValueList : TStringList read GetValueList;
      end;

      { TFilter }
      { Class realize filter concept which can be used to find elements }
      TFilter = class
      public
        type

          { TTagIdEqual }

          TTagIdEqual = class (IFilter)
          private
            FTag : TTag;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; override;
              overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (ATag : TTag);
            destructor Destroy; override;
          end;

          { TTagIdExclude }

          TTagIdExclude = class (IFilter)
          private
            FTag : TTag;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; override;
              overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (ATag : TTag);
            destructor Destroy; override;
          end;

          { TTagNodeCallback }

          TTagNodeEqualCallback = class (IFilter)
          private
            FCallback : TTagNodeFilterCallback;
            FData : Pointer;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; override;
              overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (ACallback : TTagNodeFilterCallback; AData :
              Pointer);
            destructor Destroy; override;
          end;

          { TTagNodeAttributeKeyEqual }

          TTagNodeAttributeKeyEqual = class (IFilter)
          private
            FTagNodeAttributeKey : string;
          protected
            function IsEqual ({%H-}ANode : pmyhtml_tree_node_t) : Boolean;
              override; overload;
            function IsEqual (ANodeAttribute : pmyhtml_tree_attr_t) : Boolean;
              override; overload;
          public
            constructor Create (AKey : string);
            destructor Destroy; override;
          end;

          { TTagNodeAttributeValueEqual }

          TTagNodeAttributeValueEqual = class (IFilter)
          private
            FTagNodeAttributeValue : string;
          protected
            function IsEqual ({%H-}ANode : pmyhtml_tree_node_t) : Boolean;
              override; overload;
            function IsEqual (ANodeAttribute : pmyhtml_tree_attr_t) : Boolean;
              override; overload;
          public
            constructor Create (AValue : string);
            destructor Destroy; override;
          end;

          { TTagNodeAttrbuteEqualCallback }

          TTagNodeAttributeEqualCallback = class (IFilter)
          private
            FTagNodeAttributeCallback : TTagNodeAttributeFilterCallback;
            FTagNodeAttributeData : Pointer;
          protected
            function IsEqual ({%H-}ANode : pmyhtml_tree_node_t) : Boolean;
              override; overload;
            function IsEqual (ANodeAttribute : pmyhtml_tree_attr_t) : Boolean;
              override; overload;
          public
            constructor Create (ACallback : TTagNodeAttributeFilterCallback;
              AData : Pointer);
            destructor Destroy; override;
          end;

          { TContainsAttributeOnlyValue }

          TContainsAttributeOnlyValue = class (IFilter)
          private
            FKey : string;
            FValue : string;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; override;
              overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (AKey : string; AValue : string);
            destructor Destroy; override;
          end;

          { TNotContainsAttributeOnlyValue }

          TNotContainsAttributeOnlyValue = class (IFilter)
          private
            FFilter : TContainsAttributeOnlyValue;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean; override;
              overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (AKey : string; AValue : string);
            destructor Destroy; override;
          end;

          { TContainsAttributeAllValues }

          TContainsAttributeAllValues = class (IFilter)
          private
            FKey : string;
            FTagNodeAttributeValueList : TStringList;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean;
              override; overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (AKey : string; AValueList : string); overload;
            constructor Create (AKey : string; AValueList : TStringList);
              overload;
            destructor Destroy; override;
          end;

          { TNotContainsAttributeAllValues }

          TNotContainsAttributeAllValues = class (IFilter)
          private
            FKey : string;
            FTagNodeAttributeValueList : TStringList;
          protected
            function IsEqual (ANode : pmyhtml_tree_node_t) : Boolean;
              override; overload;
            function IsEqual ({%H-}ANodeAttribute : pmyhtml_tree_attr_t) :
              Boolean; override; overload;
          public
            constructor Create (AKey : string; AValueList : string); overload;
            constructor Create (AKey : string; AValueList : TStringList);
              overload;
            destructor Destroy; override;
          end;

      private
        type
          TFiltersList = specialize TFPGObjectList<IFilter>;
      private
        FFiltersList : TFiltersList;

        { Check if ANode is equal to defined filters list }
        function IsEqual (ANode : pmyhtml_tree_node_t = nil) : Boolean;
          overload; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Cheif if ANodeAttribute is equal to defined filters list }
        function IsEqual (ANodeAttribute : pmyhtml_tree_attr_t = nil) :
          Boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return TRUE if filter isn't empty }
        function IsSet : Boolean; inline;
      public
        constructor Create;
        destructor Destroy; override;

        { Set custom filter }
        function CustomFilter (AFilter : IFilter) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

        { Set tag id for filtering }
        function Tag (ATag : TTag) : TFilter; {$IFNDEF DEBUG}inline;{$ENDIF}

        { Node tag id not equal ATag }
        function ExcludeTag (ATag : TTag) : TFilter; {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Set tag attribute key for filtering }
        {!!! Only for attributes filtrations, not tag nodes }
        function AttributeKey (AKey : string) : TFilter; {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Set tag attribute value for filtering }
        {!!! Only for attributes filtrations, not tag nodes }
        function AttributeValue (AValue : string) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

        { Set attribute key and value for filtering }
        {!!! Only for tag nodes filtrations, not attributes }
        function AttributeKeyValue (AKey : string; AValue : string) : TFilter;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Set class which must be in attribute "class" list }
        { If "class" attribute is exists new class is added in list }
        function ContainsClass (AClass : string) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

        { Set class which must not be in attribute "class" list }
        { If "class" attribute is exists new class is added in list }
        function NotContainsClass (AClass : string) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

        { Set class only which must be in attribute }
        { If "class" attribute is exists in list it is rewriting }
        function ContainsClassOnly (AClass : string) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

        { Set class only which must not be in attribute }
        { If "class" attribute is exists in list it is rewriting }
        function NotContainsClassOnly (AClass : string) : TFilter;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Set id which must be in attribute "id" list }
        { If "id" attribute is exists new id is added in list }
        function ContainsId (AId : string) : TFilter; {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Set id which must not be in attribute "id" list }
        { If "id" attribute is exists new id is added in list }
        function NotContainsId (AId : string) : TFilter; {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Set id only which must be in attribute }
        { If "id" attribute is exists in list it is rewriting }
        function ContainsIdOnly (AId : string) : TFilter; {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Set id only which must not be in attribute }
        { If "id" attribute is exists in list it is rewriting }
        function NotContainsIdOnly (AId : string) : TFilter; {$IFNDEF DEBUG}
          inline;{$ENDIF}

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
      public
        type
          { TMutableTagNode }
          { Class implements HTML tag and functions to operation with it }
          TMutableTagNode = class (TTagNode)
          private

          public
            constructor Create (ANode : pmyhtml_tree_node_t);
            destructor Destroy; override;

            { Add child node to current node. If children already exists it will
              be added to the last }
            function AddChildren (ANode : TTagNode) : TMutableTagNode;
              {$IFNDEF DEBUG}inline;{$ENDIF}

            { Add child node immediately before current node }
            function AddChildrenBefore (ANode : TTagNode) : TMutableTagNode;
              {$IFNDEF DEBUG}inline;{$ENDIF}

            { Add child node immediately after the current node }
            function AddChildrenAfter (ANode : TTagNode) : TMutableTagNode;
              {$IFNDEF DEBUG}inline;{$ENDIF}
          end;
      private
        FTagNodeCallback : TTagNodeTransformCallback;
        FTagNodeData : Pointer;

        FTagNodeAttributeCallback : TTagNodeAttributeTransformCallback;
        FTagNodeAttributeData : Pointer;

        { Run transform node callback }
        procedure RunNodeCallback (ATagNode : TTagNode); {$IFNDEF DEBUG}inline;
          {$ENDIF}

        { Run transform node attribute callback }
        procedure RunNodeAttributeCallback (ATagAttribute : TTagNodeAttribute);
          {$IFNDEF DEBUG}inline;{$ENDIF}
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

{ TParser.TTagNode.TTagNodeEnumerator }

function TParser.TTagNode.TTagNodeEnumerator.GetCurrent: TTagNode;
begin
  Result := FNode;
  FNode := FNode.NextNode(FFilter);
end;

constructor TParser.TTagNode.TTagNodeEnumerator.Create (ANode: TTagNode;
  AFilter: TFilter);
begin
  FNode := ANode.NextNode(AFilter);
  FFilter := AFilter;
end;

function TParser.TTagNode.TTagNodeEnumerator.MoveNext: Boolean;
begin
  Result := FNode.IsOk;
end;

function TParser.TTagNode.TTagNodeEnumerator.GetEnumerator: TTagNodeEnumerator;
begin
  Result := Self;
end;

{ TParser.TFilter.TTagIdExclude }

function TParser.TFilter.TTagIdExclude.IsEqual(ANode: pmyhtml_tree_node_t
  ): Boolean;
begin
  Result := (ANode <> nil) and (myhtml_node_tag_id(ANode) <>
    myhtml_tag_id_t(FTag));
end;

function TParser.TFilter.TTagIdExclude.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TTagIdExclude.Create(ATag: TTag);
begin
  FTag := ATag;
end;

destructor TParser.TFilter.TTagIdExclude.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TTransform.TMutableTagNode }

constructor TParser.TTransform.TMutableTagNode.Create(ANode: pmyhtml_tree_node_t
  );
begin
  inherited Create (ANode);
end;

destructor TParser.TTransform.TMutableTagNode.Destroy;
begin
  inherited Destroy;
end;

function TParser.TTransform.TMutableTagNode.AddChildren(ANode: TTagNode
  ): TMutableTagNode;
begin
  if IsOk then
  begin
    myhtml_tree_node_add_child(FNode, ANode.FNode);
  end;
  Result := Self;
end;

function TParser.TTransform.TMutableTagNode.AddChildrenBefore(ANode: TTagNode
  ): TMutableTagNode;
begin
  if IsOk then
  begin
    myhtml_tree_node_insert_before(FNode, ANode.FNode);
  end;
  Result := Self;
end;

function TParser.TTransform.TMutableTagNode.AddChildrenAfter(ANode: TTagNode
  ): TMutableTagNode;
begin
  if IsOk then
  begin
    myhtml_tree_node_insert_after(FNode, ANode.FNode);
  end;
  Result := Self;
end;

{ TParser.TFilter.TNotContainsAttributeAllValues }

function TParser.TFilter.TNotContainsAttributeAllValues.IsEqual(
  ANode: pmyhtml_tree_node_t): Boolean;

  function FindAttribute : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := nil;
    if ANode <> nil then
      Result := myhtml_attribute_by_key(ANode, PChar(FKey), Length(FKey));
  end;

  function CheckAttributeAllValues (AAttribute : pmyhtml_tree_attr_t) : Boolean;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    ValueList : TStringList;
    Value : string;
  begin
    ValueList := TTagNodeAttribute.StringTokenize(myhtml_attribute_value(
      AAttribute, nil));
    Result := True;
    for Value in FTagNodeAttributeValueList do
    begin
      if ValueList.IndexOf(Value) <> -1 then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if FTagNodeAttributeValueList.Count > 0 then
  begin
    NodeAttr := FindAttribute;
    if NodeAttr = nil then
      Result := True
    else
      Result := CheckAttributeAllValues(NodeAttr);
  end else
    Result := True;
end;

function TParser.TFilter.TNotContainsAttributeAllValues.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TNotContainsAttributeAllValues.Create(AKey: string;
  AValueList: string);
begin
  FKey := AKey;
  FTagNodeAttributeValueList := TTagNodeAttribute.StringTokenize(AValueList);
end;

constructor TParser.TFilter.TNotContainsAttributeAllValues.Create(AKey: string;
  AValueList: TStringList);
begin
  FKey := AKey;
  FTagNodeAttributeValueList := AValueList;
end;

destructor TParser.TFilter.TNotContainsAttributeAllValues.Destroy;
begin
  FreeAndNil(FTagNodeAttributeValueList);
  inherited Destroy;
end;

{ TParser.TFilter.TNotContainsAttributeOnlyValue }

function TParser.TFilter.TNotContainsAttributeOnlyValue.IsEqual(
  ANode: pmyhtml_tree_node_t): Boolean;
begin
  Result := not FFilter.IsEqual(ANode);
end;

function TParser.TFilter.TNotContainsAttributeOnlyValue.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TNotContainsAttributeOnlyValue.Create(AKey: string;
  AValue: string);
begin
  FFilter := TContainsAttributeOnlyValue.Create(AKey, AValue);
end;

destructor TParser.TFilter.TNotContainsAttributeOnlyValue.Destroy;
begin
  FreeAndNil(FFilter);
  inherited Destroy;
end;

{ TParser.TFilter.TContainsAttributeAllValues }

function TParser.TFilter.TContainsAttributeAllValues.IsEqual(ANode:
  pmyhtml_tree_node_t): Boolean;

  function FindAttribute : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := nil;
    if ANode <> nil then
      Result := myhtml_attribute_by_key(ANode, PChar(FKey), Length(FKey));
  end;

  function CheckAttributeAllValues (AAttribute : pmyhtml_tree_attr_t) : Boolean;
    {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    ValueList : TStringList;
    Value : string;
  begin
    ValueList := TTagNodeAttribute.StringTokenize(myhtml_attribute_value(
      AAttribute, nil));
    if ValueList.Count >= FTagNodeAttributeValueList.Count then
    begin
      Result := True;
      for Value in FTagNodeAttributeValueList do
      begin
        if ValueList.IndexOf(Value) = -1 then
        begin
          Result := False;
          Break;
        end;
      end;
    end else
      Result := False;
  end;

var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if FTagNodeAttributeValueList.Count > 0 then
  begin
    NodeAttr := FindAttribute;
    Result := (NodeAttr <> nil) and CheckAttributeAllValues(NodeAttr);
  end else
    Result := True;
end;

function TParser.TFilter.TContainsAttributeAllValues.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TContainsAttributeAllValues.Create(AKey : string;
  AValueList: string);
begin
  FKey := AKey;
  FTagNodeAttributeValueList := TTagNodeAttribute.StringTokenize(AValueList);
end;

constructor TParser.TFilter.TContainsAttributeAllValues.Create(AKey : string;
  AValueList: TStringList);
begin
  FKey := AKey;
  FTagNodeAttributeValueList := AValueList;
end;

destructor TParser.TFilter.TContainsAttributeAllValues.Destroy;
begin
  FreeAndNil(FTagNodeAttributeValueList);
  inherited Destroy;
end;

{ TParser.TFilter.TContainsAttributeOnlyValue }

function TParser.TFilter.TContainsAttributeOnlyValue.IsEqual(ANode:
  pmyhtml_tree_node_t): Boolean;

  function FindAttribute : pmyhtml_tree_attr_t; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := nil;
    if ANode <> nil then
      Result := myhtml_attribute_by_key(ANode, PChar(FKey), Length(FKey));
  end;

var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if (FKey <> '') and (FValue <> '') then
  begin
    NodeAttr := FindAttribute;
    Result := (NodeAttr <> nil) and (myhtml_attribute_value(NodeAttr, nil) =
      FValue);
  end else
    Result := True;
end;

function TParser.TFilter.TContainsAttributeOnlyValue.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TContainsAttributeOnlyValue.Create(AKey : string;
  AValue: string);
begin
  FKey := AKey;
  FValue := AValue;
end;

destructor TParser.TFilter.TContainsAttributeOnlyValue.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter.TTagNodeAttributeEqualCallback }

function TParser.TFilter.TTagNodeAttributeEqualCallback.IsEqual(
  ANode: pmyhtml_tree_node_t): Boolean;
var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if ANode <> nil then
    NodeAttr := myhtml_node_attribute_first(ANode)
  else
    NodeAttr := nil;

  while (NodeAttr <> nil) and (not (FTagNodeAttributeCallback(
    TTagNodeAttribute.Create(NodeAttr), FTagNodeAttributeData))) do
    NodeAttr := myhtml_attribute_next(NodeAttr);

  Result := NodeAttr <> nil;
end;

function TParser.TFilter.TTagNodeAttributeEqualCallback.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := FTagNodeAttributeCallback(TTagNodeAttribute.Create(ANodeAttribute),
    FTagNodeAttributeData);
end;

constructor TParser.TFilter.TTagNodeAttributeEqualCallback.Create(
  ACallback: TTagNodeAttributeFilterCallback; AData: Pointer);
begin
  FTagNodeAttributeCallback := ACallback;
  FTagNodeAttributeData := AData;
end;

destructor TParser.TFilter.TTagNodeAttributeEqualCallback.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter.TTagNodeAttributeValueEqual }

function TParser.TFilter.TTagNodeAttributeValueEqual.IsEqual(
  ANode: pmyhtml_tree_node_t): Boolean;
begin
  Result := True; { not filtering by node }
end;

function TParser.TFilter.TTagNodeAttributeValueEqual.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := (ANodeAttribute <> nil) and (myhtml_attribute_value(ANodeAttribute,
    nil) = FTagNodeAttributeValue);
end;

constructor TParser.TFilter.TTagNodeAttributeValueEqual.Create(AValue: string);
begin
  FTagNodeAttributeValue := AValue;
end;

destructor TParser.TFilter.TTagNodeAttributeValueEqual.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter.TTagNodeAttributeKeyEqual }

function TParser.TFilter.TTagNodeAttributeKeyEqual.IsEqual(
  ANode: pmyhtml_tree_node_t): Boolean;
begin
  if ANode <> nil then
    Result := myhtml_attribute_by_key(ANode, PChar(FTagNodeAttributeKey),
      Length(FTagNodeAttributeKey)) <> nil
  else
    Result := False;
end;

function TParser.TFilter.TTagNodeAttributeKeyEqual.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := (ANodeAttribute <> nil) and (myhtml_attribute_key(ANodeAttribute,
    nil) = FTagNodeAttributeKey);
end;

constructor TParser.TFilter.TTagNodeAttributeKeyEqual.Create(AKey : string);
begin
  FTagNodeAttributeKey := AKey;
end;

destructor TParser.TFilter.TTagNodeAttributeKeyEqual.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter.TTagNodeCallback }

function TParser.TFilter.TTagNodeEqualCallback.IsEqual(ANode:
  pmyhtml_tree_node_t): Boolean;
begin
  if Assigned(FCallback) then
    Result := FCallback(TTagNode.Create(ANode), FData)
  else
    Result := False;
end;

function TParser.TFilter.TTagNodeEqualCallback.IsEqual(
  ANodeAttribute: pmyhtml_tree_attr_t): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TTagNodeEqualCallback.Create(
  ACallback: TTagNodeFilterCallback; AData: Pointer);
begin
  FCallback := ACallback;
  FData := AData;
end;

destructor TParser.TFilter.TTagNodeEqualCallback.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter.TTagIdEqual }

function TParser.TFilter.TTagIdEqual.IsEqual(ANode: pmyhtml_tree_node_t
  ): Boolean;
begin
  Result := (ANode <> nil) and (myhtml_node_tag_id(ANode) =
    myhtml_tag_id_t(FTag));
end;

function TParser.TFilter.TTagIdEqual.IsEqual(ANodeAttribute: pmyhtml_tree_attr_t
  ): Boolean;
begin
  Result := True; { not filtering by attribute }
end;

constructor TParser.TFilter.TTagIdEqual.Create(ATag: TTag);
begin
  FTag := ATag;
end;

destructor TParser.TFilter.TTagIdEqual.Destroy;
begin
  inherited Destroy;
end;

{ TParser.TFilter }

function TParser.TFilter.IsEqual(ANode: pmyhtml_tree_node_t): Boolean;
var
  Filter : IFilter;
begin
  Result := True;
  for Filter in FFiltersList do
    Result := Result and Filter.IsEqual(ANode);
end;

function TParser.TFilter.IsEqual(ANodeAttribute: pmyhtml_tree_attr_t
  ): Boolean;
var
  Filter : IFilter;
begin
  Result := True;
  for Filter in FFiltersList do
    Result := Result and Filter.IsEqual(ANodeAttribute);
end;

function TParser.TFilter.IsSet : Boolean;
begin
  Result := FFiltersList.Count > 0;
end;

constructor TParser.TFilter.Create;
begin
  FFiltersList := TFiltersList.Create;
end;

destructor TParser.TFilter.Destroy;
begin
  FreeAndNil(FFiltersList);
  inherited Destroy;
end;

function TParser.TFilter.CustomFilter(AFilter: IFilter): TFilter;
begin
  FFiltersList.Add(AFilter);
  Result := Self;
end;

function TParser.TFilter.Tag(ATag: TTag): TFilter;
begin
  FFiltersList.Add(TTagIdEqual.Create(ATag));
  Result := Self;
end;

function TParser.TFilter.ExcludeTag(ATag: TTag): TFilter;
begin
  FFiltersList.Add(TTagIdExclude.Create(ATag));
  Result := Self;
end;

function TParser.TFilter.AttributeKey(AKey: string): TFilter;
begin
  FFiltersList.Add(TTagNodeAttributeKeyEqual.Create(AKey));
  Result := Self;
end;

function TParser.TFilter.AttributeValue(AValue: string): TFilter;
begin
  FFiltersList.Add(TTagNodeAttributeValueEqual.Create(AValue));
  Result := Self;
end;

function TParser.TFilter.AttributeKeyValue(AKey: string; AValue: string
  ): TFilter;
begin
  FFiltersList.Add(TContainsAttributeOnlyValue.Create(AKey, AValue));
  Result := Self;
end;

function TParser.TFilter.ContainsClass(AClass: string): TFilter;
begin
  FFiltersList.Add(TContainsAttributeAllValues.Create('class', AClass));
  Result := Self;
end;

function TParser.TFilter.NotContainsClass(AClass: string): TFilter;
begin
  FFiltersList.Add(TNotContainsAttributeAllValues.Create('class', AClass));
  Result := Self;
end;

function TParser.TFilter.ContainsClassOnly(AClass: string): TFilter;
begin
  FFiltersList.Add(TContainsAttributeOnlyValue.Create('class', AClass));
  Result := Self;
end;

function TParser.TFilter.NotContainsClassOnly(AClass: string): TFilter;
begin
  FFiltersList.Add(TNotContainsAttributeOnlyValue.Create('class', AClass));
  Result := Self;
end;

function TParser.TFilter.ContainsId(AId: string): TFilter;
begin
  FFiltersList.Add(TContainsAttributeAllValues.Create('id', AId));
  Result := Self;
end;

function TParser.TFilter.NotContainsId(AId: string): TFilter;
begin
  FFiltersList.Add(TNotContainsAttributeAllValues.Create('id', AId));
  Result := Self;
end;

function TParser.TFilter.ContainsIdOnly(AId: string): TFilter;
begin
  FFiltersList.Add(TContainsAttributeOnlyValue.Create('id', AId));
  Result := Self;
end;

function TParser.TFilter.NotContainsIdOnly(AId: string): TFilter;
begin
  FFiltersList.Add(TNotContainsAttributeOnlyValue.Create('id', AId));
  Result := Self;
end;

function TParser.TFilter.TagNodeCallback(
  ACallback: TTagNodeFilterCallback; AData: Pointer): TFilter;
begin
  FFiltersList.Add(TTagNodeEqualCallback.Create(ACallback, AData));
  Result := Self;
end;

function TParser.TFilter.TagNodeAttributeCallback(
  ACallback: TTagNodeAttributeFilterCallback; AData: Pointer): TFilter;
begin
  FFiltersList.Add(TTagNodeAttributeEqualCallback.Create(ACallback, AData));
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

function TParser.TTagNode.FilterNode(ANode : pmyhtml_tree_node_t;
  AFilter : TFilter) : pmyhtml_tree_node_t;
var
  Node : pmyhtml_tree_node_t;
begin
  if AFilter <> nil then
  begin
    Node := ANode;
    while (Node <> nil) and (not AFilter.IsEqual(Node)) do
      Node := myhtml_node_next(Node);
    Result := Node;
  end else
    Result := ANode;
end;

function TParser.TTagNode.ReverseFilterNode(ANode: pmyhtml_tree_node_t;
  AFilter: TFilter): pmyhtml_tree_Node_t;
var
  Node : pmyhtml_tree_node_t;
 begin
  if AFilter <> nil then
  begin
    Node := ANode;
    while (Node <> nil) and (not AFilter.IsEqual(Node)) do
      Node := myhtml_node_prev(Node);
    Result := Node;
  end else
    Result := ANode;
end;

function TParser.TTagNode.FilterAttribute(AAttribute : pmyhtml_tree_attr_t;
  AFilter : TFilter) : pmyhtml_tree_attr_t;
var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if AFilter <> nil then
  begin
    NodeAttr := AAttribute;
    while (NodeAttr <> nil) and (not AFilter.IsEqual(NodeAttr)) do
      NodeAttr := myhtml_attribute_next(NodeAttr);
    Result := NodeAttr;
  end else
    Result := AAttribute;
end;

function TParser.TTagNode.ReverseFilterAttribute(
  AAttribute: pmyhtml_tree_attr_t; AFilter: TFIlter): pmyhtml_tree_attr_t;
var
  NodeAttr : pmyhtml_tree_attr_t;
begin
  if AFilter <> nil then
  begin
    NodeAttr := AAttribute;
    while (NodeAttr <> nil) and (not AFilter.IsEqual(NodeAttr)) do
      NodeAttr := myhtml_attribute_prev(NodeAttr);
    Result := NodeAttr;
  end else
    Result := AAttribute;
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
    if myhtml_tags_t(myhtml_node_tag_id(FNode)) <> MyHTML_TAG__TEXT then
    begin
      TextNode := myhtml_node_child(FNode)
    end else
    begin
      TextNode := FNode;
    end;

    if (TextNode <> nil) and (myhtml_tags_t(myhtml_node_tag_id(TextNode)) =
      MyHTML_TAG__TEXT) then
    begin
      Result := myhtml_node_text(TextNode, nil);
    end else
      Result := '';
  end else
    Result := '';
end;

function TParser.TTagNode.ConcatValue(AFilter: TFilter): string;
var
  Node : TTagNode;
begin
  if IsOk then
  begin
    Result := '';

    Node := FirstChildrenNode(AFilter);
    while Node.IsOk do
    begin
      if Node.Tag = MyHTML_TAG__TEXT then
        Result := Result + Node.Value
      else
        Result := Result + Node.FirstChildrenNode.Value;

      Node := Node.NextNode(AFilter);
    end;
  end else
    Result := '';
end;

constructor TParser.TTagNode.Create(ANode: pmyhtml_tree_node_t);
begin
  FNode := ANode;
  FNodeAttribute := nil;
end;

destructor TParser.TTagNode.Destroy;
begin
  inherited Destroy;
end;

function TParser.TTagNode.IsOk: Boolean;
begin
  Result := FNode <> nil;
end;

function TParser.TTagNode.GetParent: TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(myhtml_node_parent(FNode));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.GetIsEmpty: Boolean;
begin
  Result := myhtml_node_is_void_element(FNode);
end;

function TParser.TTagNode.GetIsSelfClose: Boolean;
begin
  Result := myhtml_node_is_close_self(FNode);
end;

function TParser.TTagNode.GetData: Pointer;
begin
  Result := myhtml_node_get_data(FNode);
end;

procedure TParser.TTagNode.SetData(AData: Pointer);
begin
  myhtml_node_set_data(FNode, AData);
end;

function TParser.TTagNode.GetEnumerator: TTagNodeEnumerator;
begin
  Result := TTagNodeEnumerator.Create(Self);
end;

function TParser.TTagNode.Filter(AFilter: TFilter): TTagNodeEnumerator;
begin
  Result := TTagNodeEnumerator.Create(Self, AFilter);
end;

function TParser.TTagNode.FirstNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(FilterNode(FNode, AFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.NextNode (AFilter : TFilter): TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(FilterNode(myhtml_node_next(FNode), AFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.PrevNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(FilterNode(myhtml_node_prev(FNode), AFilter));
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
      Node := Node.NextNode(AFilter);
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
      Node := Node.NextNode(AFilter);
    end;

  end else
    Result := TTagNodeList.Create;
end;

function TParser.TTagNode.FirstChildrenNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(FilterNode(myhtml_node_child(FNode), AFilter));
  end else
    Result := TTagNode.Create(nil);
end;

function TParser.TTagNode.LastChildrenNode(AFilter: TFilter): TTagNode;
begin
  if IsOk then
  begin
    Result := TTagNode.Create(ReverseFilterNode(myhtml_node_last_child(FNode),
      AFilter));
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
      Node := Node.NextNode(AFilter);
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
      Node := Node.NextNode(AFilter);
    end;
  end else
    Result := TTagNodeList.Create;
end;

function TParser.TTagNode.FirstNodeAttribute(AFilter: TFilter
  ): TTagNodeAttribute;
begin
 if IsOk then
 begin
   FNodeAttribute := myhtml_node_attribute_first(FNode);
   Result := TTagNodeAttribute.Create(FilterAttribute(FNodeAttribute,
     AFilter));
 end else
   Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.LastNodeAttribute(AFilter: TFilter
  ): TTagNodeAttribute;
begin
  if IsOk then
  begin
    FNodeAttribute := myhtml_node_attribute_last(FNode);
    Result := TTagNodeAttribute.Create(ReverseFilterAttribute(FNodeAttribute,
      AFilter));
  end else
    Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.NextNodeAttribute (AFilter : TFilter):
  TTagNodeAttribute;
begin
  if IsOk then
  begin
    Result := TTagNodeAttribute.Create(FilterAttribute(
      myhtml_attribute_next(FNodeAttribute), AFilter));
  end else
    Result := TTagNodeAttribute.Create(nil);
end;

function TParser.TTagNode.PrevNodeAttribute(AFilter: TFilter
  ): TTagNodeAttribute;
begin
  if IsOk then
  begin
    Result := TTagNodeAttribute.Create(FilterAttribute(
      myhtml_attribute_prev(FNodeAttribute), AFilter));
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
      Attribute := NextNodeAttribute(AFilter);
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
      Attribute := NextNodeAttribute(AFilter);
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

