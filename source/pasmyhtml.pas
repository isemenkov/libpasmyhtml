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
      { Start parsing from the next element }
      TDocumentParseFrom = (
        DOCUMENT_ROOT, { Start parse from document root }
        DOCUMENT_HTML, { Start parse from HTML tag element }
        DOCUMENT_HEAD, { Start parse from HEAD tag element }
        DOCUMENT_BODY  { Start parse from BODY tag element }
      );

      { Tag node attribute wrapper }
      TTagNodeAttribute = class;
      TTagNodeAttributeList = class;

      { Tag node }
      TTagNode = class;
      TTagNodeList = class;

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

      TFilter = class
      private

      public
        constructor Create;
        destructor Destroy; override;

        function TagNodeCallback (ACallback : TTagNodeFilterCallback; AData :
          Pointer = nil) : TFilter;
        function TagNodeAttributeCallback (ACallback :
          TTagNodeAttributeCallback; AData : Pointer = nil) : TFilter;
      end;

      TTransform = class
      private

      public
        constructor Create;
        destructor Destroy; override;

        function TagNodeTransform (ACallback : TTagNodeTransformCallback;
          AData : Pointer = nil) : TTransform;
        function TagNodeAttributeTransform (ACallback :
          TTagNodeAttributeTransformCallback; AData : Pointer) : TTransform;
      end;

      TTagNode = class
      private
        FNode : pmyhtml_tree_node_t;

        function GetTag :  myhtml_tags_t;
      public
        constructor Create (ANode : pmyhtml_tree_node_t);
        destructor Destroy; override;

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
      end;

      TTagNodeList = specialize TFPGObjectList<TTagNode>;

      TTagNodeAttribute = class
      private
        FAttribute : pmyhtml_tree_attr_t;

        function GetKey : string;
        function GetValue : string;
      public
        constructor Create (AAttribute : pmyhtml_tree_attr_t);
        destructor Destroy; override;

      public
        property Key : string read GetKey;
        property Value : string read GetValue;
      end;

      TTagNodeAttributeList = specialize TFPGObjectList<TTagNodeAttribute>;
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

