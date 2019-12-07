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
(* Module:          Unit 'RootZoneDatabase'                                   *)
(* Functionality:                                                             *)
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

unit RootZoneDatabase;

{$mode objfpc}{$H+}

{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, fgl, pascurl, pasmyhtml;

type
  TRootZoneDatabaseEnumerator = class;

  { TRootZoneDatabase }
  { Database class contains root zones information }
  TRootZoneDatabase = class
  public
    type

      { TDomainZoneInfoType }
      { Root zone information field data type }
      TDomainZoneInfoType = (
        INFO_NAME,
        INFO_ENTITY,
        INFO_ICONPATH,
        INFO_NOTE,
        INFO_DNSNAME,
        INFO_MANAGER,
        INFO_TYPE,
        INFO_LANGUAGE,
        INFO_REGION
      );

      { TInfoElement }
      { Root zone information field }
      TInfoElement = class
      private
        FType : TDomainZoneInfoType;
        FValue : String;
      public
        constructor Create (AType : TDomainZoneInfoType; AValue : string);
      public
        property InfoType : TDomainZoneInfoType read FType;
        property Value : string read FValue;
      end;

      TDomainZone = class;

      { TDomainZoneEnumerator }
      { Domain zone enumerator }
      TDomainZoneEnumerator = class
      protected
        FDomain : TDomainZone;
        FPosition : Integer;
        function GetCurrent : TInfoElement;
      public
        constructor Create (ADomainZone : TDomainZone);
        function MoveNext : Boolean;
        property Current : TInfoElement read GetCurrent;
      end;

      { TDomainZone }
      { Root zone element }
      TDomainZone = class
      protected
        type
          TInfoElementList = specialize TFPGObjectList<TInfoElement>;
      protected
        FInfo : TInfoElementList;
        FName : string;
      public
        function GetEnumerator : TDomainZoneEnumerator;
      public
        constructor Create;
        destructor Destroy; override;
        procedure AddInfo (AElem : TInfoElement);
        procedure Merge (ADomain : TDomainZone);
      public
        property Name : string read FName;
      end;

  protected
    type
      TRootZonesList = specialize TFPGMapObject<string, TDomainZone>;
  protected
    FRootZones : TRootZonesList;
  public
    function GetEnumerator : TRootZoneDatabaseEnumerator;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDomain (ADomain : TDomainZone);
  end;

  { TRootZoneDatabaseEnumerator }
  { Root zone database enumerator }
  TRootZoneDatabaseEnumerator = class
  protected
    FDatabase : TRootZoneDatabase;
    FPosition : Integer;
    function GetCurrent : TRootZoneDatabase.TDomainZone;
  public
    constructor Create (ADatabase : TRootZoneDatabase);
    function MoveNext : Boolean;
    property Current : TRootZoneDatabase.TDomainZone read GetCurrent;
  end;

  { TRootZoneDatabaseParser }
  { Parse database from websites }
  TRootZoneDatabaseParser = class
  private
    const
      { Parse top level domain zones from url }
      IANA_ORG_URL         = 'https://www.iana.org/domains/root/db';
      PUBLICSUFFIX_ORG_URL = 'https://publicsuffix.org/list/'+
                             'effective_tld_names.dat';
      WIKIPEDIA_ORG_URL    = 'https://en.wikipedia.org/wiki/'+
                             'List_of_Internet_top-level_domains';
  private
    FZoneDatabase : TRootZoneDatabase;
    FSession : TSession;
    FParser : TParser;

    function ParseIana : TTimeInterval;
    procedure ParseIanaCallback (ANode : TParser.TTagNode;
      {%H-}AData : Pointer = nil);
    function ParsePublicsuffix : TTimeInterval;
    function ParseWikipedia : TTimeInterval;
    procedure ParseWikipedia1Callback (ANode : TParser.TTagNode;
      {%H-}AData : Pointer = nil);
    procedure ParseWikipedia2Callback (ANode : TParser.TTagNode;
      {%H-}AData : Pointer = nil);
    procedure ParseWikipedia3Callback (ANode : TParser.TTagNode;
      {%H-}AData : Pointer = nil);
    procedure ParseWikipedia4Callback (ANode : TParser.TTagNode;
      {%H-}AData : Pointer = nil);
  public
    function GetEnumerator : TRootZoneDatabaseEnumerator;
  public
    constructor Create (ADatabase : TRootZoneDatabase; ASession : TSession;
      AParser : TParser);
    destructor Destroy; override;
    procedure Parse;
  end;

  { TRootZoneDatabaseStorage }
  { Save / Load database from stream }
  TRootZoneDatabaseStorage = class
  private
    FZoneDatabase : TRootZoneDatabase;
  public
    function GetEnumerator : TRootZoneDatabaseEnumerator;
  public
    constructor Create (ADatabase : TRootZoneDatabase);
    destructor Destroy; override;
    procedure Save (AStream : TStream);
    procedure Load (AStream : TStream);
  end;

implementation

{ TRootZoneDatabaseStorage }

function TRootZoneDatabaseStorage.GetEnumerator: TRootZoneDatabaseEnumerator;
begin
  Result := FZoneDatabase.GetEnumerator;
end;

constructor TRootZoneDatabaseStorage.Create(ADatabase: TRootZoneDatabase);
begin
  FZoneDatabase := ADatabase;
end;

destructor TRootZoneDatabaseStorage.Destroy;
begin
  FreeAndNil(FZoneDatabase);
  inherited Destroy;
end;

procedure TRootZoneDatabaseStorage.Save(AStream: TStream);
begin
  // TODO
end;

procedure TRootZoneDatabaseStorage.Load(AStream: TStream);
begin
  // TODO
end;

{ TRootZoneDatabaseParser }

function TRootZoneDatabaseParser.ParseIana: TTimeInterval;
var
  Response : TResponse;
begin
  FSession.Url := IANA_ORG_URL;
  Response := TResponse.Create(FSession);

  { ...
    <body>
      <div id="body">
        <div id="main_right">
        ...
          <div class="iana-table-frame">
          ...
            <table id="tld-table" class="iana-table">
              ...
              <tbody>
                <tr>
                  ...
                </tr>
                <tr>
                  ...
                </tr>
                ...
  ... }

  FParser.Parse(Response.Content, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('body'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('main_right'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly(
      'iana-table-frame'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TABLE))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseIanaCallback));

  Result := Response.TotalTime;
  FreeAndNil(Response);
end;

procedure TRootZoneDatabaseParser.ParseIanaCallback(ANode: TParser.TTagNode;
  AData: Pointer);

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

var
  Node : TParser.TTagNode;
  DomainZone : TRootZoneDatabase.TDomainZone;
begin
  DomainZone := TRootZoneDatabase.TDomainZone.Create;

  { ...
    <td>
      <span class="domain tld">
        <a href="/domains/root/db/aaa.html">.aaa</a>    [<-- INFO_NAME]
      </span>
    </td>
  ... }

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
    ClearString(Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_SPAN))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_A))
    .Value)));

  { ...
    <td>generic</td>                                    [<-- INFO_TYPE]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_TYPE,
    ClearString(Node.Value)));

  { ...
    <td>American Automobile Association, Inc.</td>      [<-- INFO_MANAGER]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_MANAGER,
    ClearString(Node.Value)));

  if DomainZone.Name <> '' then
    FZoneDatabase.AddDomain(DomainZone);
end;

function TRootZoneDatabaseParser.ParsePublicsuffix: TTimeInterval;

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

  function ReadLine (var AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    AString := TrimLeft(AString);
    Index := Pos(sLineBreak, AString);

    if Index <> 0 then
    begin
      Result := ClearString(Copy(AString, 0, Index));
      AString := Copy(AString, Index, Length(AString) - Index +
        Length(sLineBreak));
    end else
    begin
      Result := ClearString(AString);
      AString := '';
    end;
  end;

  function ClearDomainName (Name : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    if Name[1] = '!' then
      Delete(Name, 1, 1);

    if Name[1] = '*' then
      Delete(Name, 1, 2);

    Result := '.' + Name;
  end;

var
  Response : TResponse;
  DomainZone : TRootZoneDatabase.TDomainZone;
  ListString, LineString : string;
begin
  FSession.Url := PUBLICSUFFIX_ORG_URL;
  Response := TResponse.Create(FSession);

  ListString := Response.Content;
  while ListString <> '' do
  begin
    LineString := ReadLine(ListString);

    if (Length(LineString) >= 2) and (LineString[1] <> '/') and
      (LineString[2] <> '/') then
    begin
      DomainZone := TRootZoneDatabase.TDomainZone.Create;
      DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
        ClearDomainName(LineString)));
      FZoneDatabase.AddDomain(DomainZone);
    end;
  end;

  Result := Response.TotalTime;
  FreeAndNil(Response);
end;

function TRootZoneDatabaseParser.ParseWikipedia: TTimeInterval;
var
  Response : TResponse;
  Node : TParser.TTagNode;
begin
  FSession.Url := WIKIPEDIA_ORG_URL;
  Response := TResponse.Create(FSession);

   { ...
   <body>
     ...
     <div id="content" ...>
       ...
       <div id="bodyContent" ...>
         ...
         <div id="mw-content-text" ...>
           <div class="mw-parser-output">
             ...
             <table class="wikitable sortable">
               ...
   ... }

  Node := FParser.Parse(Response.Content, DOCUMENT_BODY)
    .FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('content'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('bodyContent'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsIdOnly('mw-content-text'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsClassOnly(
      'mw-parser-output'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TABLE).ContainsClass('wikitable'));

  { ...
      ...
      <tbody>
        <tr valign="top">
          ...
        </tr>
        ...
      </tbody>
      ...
    </table>
    ...
  ... }

  Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseWikipedia1Callback));

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TABLE).ContainsClass('wikitable'));
  Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseWikipedia2Callback));

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TABLE).ContainsClass('wikitable'));
  Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseWikipedia3Callback));

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TABLE).ContainsClass('wikitable'));
  Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseWikipedia4Callback));

  Result := Response.TotalTime;
  FreeAndNil(Response);
end;

procedure TRootZoneDatabaseParser.ParseWikipedia1Callback(
  ANode: TParser.TTagNode; AData: Pointer);

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

var
  Node : TParser.TTagNode;
  DomainZone : TRootZoneDatabase.TDomainZone;
begin
  DomainZone := TRootZoneDatabase.TDomainZone.Create;

  { ...
    <tr>
      <td>
       <a href="/wiki/.com" title=".com">.com</a>             [<-- INFO_NAME]
      </td>
  ... }

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
    ClearString(Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_A)).Value)));

  { ...
    <td>commercial</td>                                       [<-- INFO_TYPE]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_TYPE,
    ClearString(Node.ConcatValue)));

  { ...
    <td>
     <a href="/wiki/Verisign" title="Verisign">Verisign</a>   [<-- INFO_MANAGER]
    </td>
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_MANAGER,
    ClearString(Node.ConcatValue)));

  { ...
    <td>
      [... text ... ]                                         [<-- INFO_NOTE]
    </td>
    ...
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NOTE,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.NotContainsClass(
      'noprint')))));

  if DomainZone.Name <> '' then
    FZoneDatabase.AddDomain(DomainZone);
end;

procedure TRootZoneDatabaseParser.ParseWikipedia2Callback(
  ANode: TParser.TTagNode; AData: Pointer);

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

var
  Node : TParser.TTagNode;
  DomainZone : TRootZoneDatabase.TDomainZone;
begin
  DomainZone := TRootZoneDatabase.TDomainZone.Create;

  { ...
    <tr>
      <td>
        <a href="/wiki/.arpa" title=".arpa">.arpa</a>         [<-- INFO_NAME]
      </td>
  ... }

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
    ClearString(Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_A)).Value)));

  { ...
    <td>Address and Routing Parameter Area</td>               [<-- INFO_ENTITY]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_ENTITY,
    ClearString(Node.ConcatValue)));

  { ...
    <td>                                                      [<-- INFO_NOTE]
      Originaly assigned to the
      <a class="mw-redirect" href="https://en.wikipedia.org/wiki/
        Advanced_Research_Projects_Agency" title="Advanced Research Projects
        Agency">Advanced Research Projects Agency</a>
      in the early days on the Internet, .arpa is now exclusively used as an
      <a class="mw-redirect" href="https://en.wikipedia.org/wiki/
        Infrastructure_top-level_domain" title="Infractructure top-level
        domain">Internet infrastructure TLD</a>
      .
    </td>
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NOTE,
    ClearString(Node.ConcatValue)));

  if DomainZone.Name <> '' then
    FZoneDatabase.AddDomain(DomainZone);
end;

procedure TRootZoneDatabaseParser.ParseWikipedia3Callback(
  ANode: TParser.TTagNode; AData: Pointer);

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

var
  Node : TParser.TTagNode;
  DomainZone : TRootZoneDatabase.TDomainZone;
begin
  DomainZone := TRootZoneDatabase.TDomainZone.Create;

  { ...
    <tr>
      <td>
        <a href="/wiki/.ac" title=".ac">.ac</a>               [<-- INFO_NAME]
      </td>
  ... }

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
    ClearString(Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_A)).Value)));

  { ...
    <td>
      <span class="flagicon">
        <img class="thumbborder" alt="" src=".../...png" decoding="async"
          srcset="... 2x" data-file-width="1000" data-file-height="500"
          width="23" height="12">
      </span>
      <a href="/wiki/Ascension_Island" title="Ascension Island">Ascension Island
      </a>                                                    [<-- INFO_ENTITY]
    </td>
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_ENTITY,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.ExcludeTag(
    TParser.TTag.MyHTML_TAG_SPAN)))));

  { ...
    <td></td>                                                 [IGNORED]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));

  { ...
    <td>
      [... text ... ]                                         [<-- INFO_NOTE]
    </td>
    ...
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NOTE,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.NotContainsClass(
      'reference')))));

  if DomainZone.Name <> '' then
    FZoneDatabase.AddDomain(DomainZone);
end;

procedure TRootZoneDatabaseParser.ParseWikipedia4Callback(
  ANode: TParser.TTagNode; AData: Pointer);

  function ClearString (AString : string) : string;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : SizeInt;
  begin
    Result := AString;
    for Index := Length(Result) downto 1 do
    begin
      { Delete controls symbols exclude space }
      if (Ord(Result[Index]) <> Ord(' ')) and (Ord(Result[Index]) <= 32) then
        Delete(Result, Index, 1);

      { Delete multiple spaces }
      if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ')
      then
        Delete(Result, Index, 1);
    end;
  end;

var
  Node : TParser.TTagNode;
  DomainZone : TRootZoneDatabase.TDomainZone;
begin
  DomainZone := TRootZoneDatabase.TDomainZone.Create;

  { ...
    <tr>
      <td>xn--lgbbat1ad8j</td>                                [<-- INFO_NAME]
  ... }

  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME,
    ClearString(Node.Value)));

  { ...
    <td dir="auto">
      <a class="mw_redirect" href="/wiki/..."
        title="...">....</a>                                  [<-- INFO_DNSNAME]
    </td>
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_DNSNAME,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.NotContainsClass(
      'reference')))));

  { ...
    <td>
      <span class="flagicon">
        <img class="thumbborder" alt="" src=".../...png" decoding="async"
          srcset="... 2x" data-file-width="1000" data-file-height="500"
          width="23" height="12">
      </span>
      <a href="/wiki/..." title="...">...</a>                 [<-- INFO_REGION]
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_REGION,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.NotContainsClass(
      'reference')))));

  { ...
    <td>
      <a href="/wiki/Arabic" title="Arabic">Arabic</a>       [<-- INFO_LANGUAGE]
    </td>
  ... }

  Node := Node.NextNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  DomainZone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_LANGUAGE,
    ClearString(Node.ConcatValue(TParser.TFilter.Create.NotContainsClass(
      'reference')))));

  if DomainZone.Name <> '' then
    FZoneDatabase.AddDomain(DomainZone);
end;

function TRootZoneDatabaseParser.GetEnumerator: TRootZoneDatabaseEnumerator;
begin
  Result := FZoneDatabase.GetEnumerator;
end;

constructor TRootZoneDatabaseParser.Create(ADatabase: TRootZoneDatabase;
  ASession : TSession; AParser : TParser);
begin
  FZoneDatabase := ADatabase;
  FSession := ASession;
  FParser := AParser;
end;

destructor TRootZoneDatabaseParser.Destroy;
begin
  FreeAndNil(FZoneDatabase);
  inherited Destroy;
end;

procedure TRootZoneDatabaseParser.Parse;
begin
  //ParseIana;
  //ParsePublicsuffix;
  ParseWikipedia;
end;

{ TRootZoneDatabaseEnumerator }

function TRootZoneDatabaseEnumerator.GetCurrent: TRootZoneDatabase.TDomainZone;
begin
  Result := FDatabase.FRootZones.Data[FPosition];
  Inc(FPosition);
end;

constructor TRootZoneDatabaseEnumerator.Create(ADatabase: TRootZoneDatabase);
begin
  FDatabase := ADatabase;
  FPosition := 0;
end;

function TRootZoneDatabaseEnumerator.MoveNext: Boolean;
begin
  Result := FPosition < FDatabase.FRootZones.Count;
end;

{ TRootZoneDatabase.TDomainZoneEnumerator }

function TRootZoneDatabase.TDomainZoneEnumerator.GetCurrent: TInfoElement;
begin
  Result := FDomain.FInfo.Items[FPosition];
  Inc(FPosition);
end;

constructor TRootZoneDatabase.TDomainZoneEnumerator.Create(
  ADomainZone: TDomainZone);
begin
  FDomain := ADomainZone;
  FPosition := 0;
end;

function TRootZoneDatabase.TDomainZoneEnumerator.MoveNext: Boolean;
begin
  Result := FPosition < FDomain.FInfo.Count;
end;

{ TRootZoneDatabase }

function TRootZoneDatabase.GetEnumerator: TRootZoneDatabaseEnumerator;
begin
  Result := TRootZoneDatabaseEnumerator.Create(Self);
end;

constructor TRootZoneDatabase.Create;
begin
  FRootZones := TRootZonesList.Create;
end;

destructor TRootZoneDatabase.Destroy;
begin
  FreeAndNil(FRootZones);
  inherited Destroy;
end;

procedure TRootZoneDatabase.AddDomain(ADomain: TDomainZone);
var
  Index : Integer;
begin
  Index := FRootZones.IndexOf(ADomain.Name);
  if Index = -1 then
    FRootZones.Add(ADomain.Name, ADomain)
  else
    FRootZones.KeyData[ADomain.Name].Merge(ADomain);
end;

{ TRootZoneDatabase.TDomainZone }

function TRootZoneDatabase.TDomainZone.GetEnumerator: TDomainZoneEnumerator;
begin
  Result := TRootZoneDatabase.TDomainZoneEnumerator.Create(Self);
end;

constructor TRootZoneDatabase.TDomainZone.Create;
begin
  FInfo := TInfoElementList.Create;
end;

destructor TRootZoneDatabase.TDomainZone.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

procedure TRootZoneDatabase.TDomainZone.AddInfo(AElem: TInfoElement);
begin
  if AElem.InfoType = INFO_NAME then
    FName := AElem.Value
  else
    FInfo.Add(AElem);
end;

procedure TRootZoneDatabase.TDomainZone.Merge(ADomain: TDomainZone);

  function SearchElement (AElement : TInfoElement) : Integer;
  {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Index : Integer;
  begin
    Result := -1;
    for Index := 0 to FInfo.Count - 1 do
    begin
      if (FInfo.Items[Index].InfoType = AElement.InfoType) and
         (FInfo.Items[Index].Value = AElement.Value) then
      begin
        Result := Index;
        Break;
      end;
    end;
  end;

var
  Elem : TInfoElement;
begin
  for Elem in ADomain do
  begin
    if SearchElement(Elem) = -1 then
      AddInfo(Elem);
  end;
end;

{ TRootZoneDatabase.TInfoElement }

constructor TRootZoneDatabase.TInfoElement.Create(AType: TDomainZoneInfoType;
  AValue: string);
begin
  FType := AType;
  FValue := AValue;
end;

end.

