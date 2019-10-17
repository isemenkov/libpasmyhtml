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
(* Functionality:   Provides  TMyHTML class                                   *)
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

  { TMyHTMLTreeChunk }

  TMyHTMLTreeChunk = class
  private
    MyHTMLNode : pmyhtml_tree_node_t;
    MyHTMLNextNode : pmyhtml_tree_node_t;
  public
    constructor Create (Node : pmyhtml_tree_node_t);
    destructor Destroy; override;
  end;

  { TMyHTMLTagNode }

  TMyHTMLTagNode = class
  private
    MyHTMLNode : pmyhtml_tree_node_t;
    MyHTMLNextChildrenNode : pmyhtml_tree_node_t;
  public
    constructor Create (Node : pmyhtml_tree_node_t);
    destructor Destroy; override;
  end;

  { TParser }

  { TMyHTMLParser }

  TMyHTMLParser = class
  public
    type
      TDocumentParseFrom = (
        DOCUMENT_HTML,
        DOCUMENT_HEAD,
        DOCUMENT_BODY
      );
  private
    MyHTML : pmyhtml_t;
    MyHTMLTree : pmyhtml_tree_t;
    MyHTMLEncoding : myencoding_t;
    MyHTMLError : mystatus_t;
  public
    constructor Create(ParserOptions: myhtml_options_t; Encoding: myencoding_t;
      ThreadCount: QWord; QueueSize: QWord; Flags: myhtml_parse_flags_t);
    destructor Destroy; override;

    function Parse (buffer : string; ParseFrom : TDocumentParseFrom =
      DOCUMENT_HTML) : TMyHTMLTreeChunk;
  end;


implementation

{ TMyHTMLTagNode }

constructor TMyHTMLTagNode.Create(Node: pmyhtml_tree_node_t);
begin
  MyHTMLNode := Node;
  MyHTMLNextChildrenNode := nil;
end;

destructor TMyHTMLTagNode.Destroy;
begin
  inherited Destroy;
end;

{ TMyHTMLTreeChunk }

constructor TMyHTMLTreeChunk.Create(Node: pmyhtml_tree_node_t);
begin
  MyHTMLNode := Node;
  MyHTMLNextNode := nil;
end;

destructor TMyHTMLTreeChunk.Destroy;
begin
  inherited Destroy;
end;

{ TMyHTML }

constructor TMyHTMLParser.Create(ParserOptions: myhtml_options_t;
  Encoding: myencoding_t; ThreadCount: QWord; QueueSize: QWord;
  Flags: myhtml_parse_flags_t);
begin
  MyHTML := myhtml_create;
  myhtml_init(MyHTML, ParserOptions, ThreadCount, QueueSize);
  MyHTMLTree := myhtml_tree_create;
  myhtml_tree_init(MyHTMLTree, MyHTML);
  myhtml_tree_parse_flags_set(MyHTMLTree, Flags);
  MyHTMLEncoding := Encoding;
  MyHTMLError := MyHTML_STATUS_OK;
end;

destructor TMyHTMLParser.Destroy;
begin
  myhtml_tree_clean(MyHTMLTree);
  myhtml_clean(MyHTML);
  myhtml_tree_destroy(MyHTMLTree);
  myhtml_destroy(MyHTML);
  inherited Destroy;
end;

function TMyHTMLParser.Parse(buffer: string; ParseFrom: TDocumentParseFrom
  ): TMyHTMLTreeChunk;
begin
  MyHTMLError := myhtml_parse(MyHTMLTree, MyHTMLEncoding, PChar(buffer),
    Length(buffer));
  if MyHTMLError = MyHTML_STATUS_OK then
  begin
    case ParseFrom of
      DOCUMENT_HTML :
          Result := TMyHTMLTreeChunk.Create
            (myhtml_tree_get_node_html(MyHTMLTree));
      DOCUMENT_HEAD :
          Result := TMyHTMLTreeChunk.Create
            (myhtml_tree_get_node_head(MyHTMLTree));
      DOCUMENT_BODY :
          Result := TMyHTMLTreeChunk.Create
            (myhtml_tree_get_node_body(MyHTMLTree));
    end;
  end;
end;

end.

