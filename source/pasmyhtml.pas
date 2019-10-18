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
      TDocumentParseFrom = (
        DOCUMENT_HTML,
        DOCUMENT_HEAD,
        DOCUMENT_BODY
      );

    { TTreeChunk }

    TTreeChunk = class
    private
      FNode : pmyhtml_tree_node_t;
      FNextNode : pmyhtml_tree_node_t;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;
    end;

    { TTagNode }

    TTagNode = class
    private
      FNode : pmyhtml_tree_node_t;
      FNextChildrenNode : pmyhtml_tree_node_t;
    public
      constructor Create (ANode : pmyhtml_tree_node_t);
      destructor Destroy; override;
    end;

  private
    FHTML : pmyhtml_t;
    FTree : pmyhtml_tree_t;
    FEncoding : myencoding_t;
    FError : mystatus_t;
  public
    constructor Create(AParserOptions: myhtml_options_t;
      AEncoding: myencoding_t; AThreadCount: QWord; AQueueSize: QWord;
      AFlags: myhtml_tree_parse_flags_t);
    destructor Destroy; override;

    function Parse (AHTML : string; AParseFrom : TDocumentParseFrom =
      DOCUMENT_HTML) : TTreeChunk;
  end;


implementation

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
  FError := Cardinal(MyHTML_STATUS_OK);
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
  FError := myhtml_parse(FTree, FEncoding, PChar(AHTML),
    Length(AHTML));
  if FError = Cardinal(MyHTML_STATUS_OK) then
  begin
    case AParseFrom of
      DOCUMENT_HTML :
          Result := TTreeChunk.Create(myhtml_tree_get_node_html(FTree));
      DOCUMENT_HEAD :
          Result := TTreeChunk.Create(myhtml_tree_get_node_head(FTree));
      DOCUMENT_BODY :
          Result := TTreeChunk.Create(myhtml_tree_get_node_body(FTree));
    end;
  end;
end;

end.

