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
(* Module:          Unit 'RootZones'                                          *)
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

unit RootZones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl, pascurl, pasmyhtml;

type

  { TRootDomainZones }

  TRootDomainZones = class
  public
    { Parse top level domain zones from next url }
    const
      IANA_ORG_URL = 'https://www.iana.org/domains/root/db';
      PUBLICSUFFIX_ORG_URL = 'https://publicsuffix.org/list/'+
        'effective_tld_names.dat';
      WIKIPEDIA_ORG_URL = 'https://en.wikipedia.org/wiki/'+
        'List_of_Internet_top-level_domains#Country_code_top-level_domains';

    type
      TDomainZoneType = (
        DOMAIN_UNKNOWN,
        DOMAIN_GENERIC,
        DOMAIN_GENERIC_RESTRICTED,
        DOMAIN_COUNTRY_CODE,
        DOMAIN_SPONSORED
      );

      { TDomainZoneInfo }

      TDomainZoneInfo = class
      public
        Name : string;          { domain suffix, like .com or .co.uk }
        Entity : string;        { intended use }
        IconPath : string;      { URL to icon }
        Icon : TPicture;        { icon image }
        Note : string;          { general remarks }
        DNSName : string;       {  }
        Manager : string;       { entity the registry has been delegated to }
        TypeInfo : TDomainZoneType; {  }
        Language : string;      {  }
      public
        procedure LoadFromStream (AStream : TStream);
        procedure SaveToStream (AStream : TStream);
      end;

      TDomainZonesList = specialize TFPGMap<string, TDomainZoneInfo>;

      TLoadZonesListCallback = function (var AList : TDomainZonesList;
        AData : Pointer) : Boolean of object;
      TSaveZonesListCallback = function (var AList : TDomainZonesList;
        AData : Pointer) : Boolean of object;

      TParseDomainZoneCallback = procedure (var AResponse : TResponse)
        of object;
  private
    FSession : TSession;
    FResponse : TResponse;
    FParser : TParser;
    FDomainZones : TDomainZonesList;

    FLoadCallback : TLoadZonesListCallback;
    FLoadCallbackData : Pointer;

    FSaveCallback : TSaveZonesListCallback;
    FSaveCallbackData : Pointer;

    function ClearString (AString : string) : string; inline;

    function GetDomainZonesList : TDomainZonesList; inline;

    procedure ParseIanaOrg (ACallback : TParseDomainZoneCallback = nil);
    procedure ParseIanaOrgCallback (ANode : TParser.TTagNode; AData : Pointer =
      nil);
    procedure ParsePublicSuffixOrg (ACallback : TParseDomainZoneCallback = nil);
    procedure ParseWikipediaOrg (ACallback : TParseDomainZoneCallback = nil);
  public
    constructor Create (ASession : TSession; AParser : TParser);
    destructor Destroy; override;

    function CheckDomainZone (AZone : string) : Boolean; inline;
    function ExtractDomainZone (AURL : string) : TDomainZoneInfo; inline;
    function GetDomainZoneInfo (AZone : string) : TDomainZoneInfo; inline;

    function LoadCallback (ACallback : TLoadZonesListCallback; AData :
      Pointer) : TRootDomainZones; inline;
    function SaveCallback (ACallback : TSaveZonesListCallback; AData :
      Pointer) : TRootDomainZones; inline;

    procedure LoadDomainZones;
    procedure SaveDomainZones;
    procedure ParseDomainZones (ACallback : TParseDomainZoneCallback = nil);
  published
    property DomainZones : TDomainZonesList read GetDomainZonesList;
  end;

implementation

{ TRootDomainZones.TDomainZoneInfo }

procedure TRootDomainZones.TDomainZoneInfo.LoadFromStream(AStream: TStream);

  function ReadStringFromStream : string;
  var
    StrLen : Integer;
  begin
    with AStream do
    begin
      StrLen := ReadDWord;
      SetLength(Result, StrLen);
      ReadBuffer(PChar(Result)^, StrLen);
    end;
  end;

begin
  Name := ReadStringFromStream;
  Entity := ReadStringFromStream;
  IconPath := ReadStringFromStream;

  Note := ReadStringFromStream;
  DNSName := ReadStringFromStream;
  Manager := ReadStringFromStream;
  TypeInfo := TDomainZoneType(AStream.ReadDWord);
  Language := ReadStringFromStream;
end;

procedure TRootDomainZones.TDomainZoneInfo.SaveToStream(AStream: TStream);

  procedure WriteStringToStream (AString : string);
  begin
    with AStream do
    begin
      WriteDWord(Length(AString));
      WriteBuffer(PChar(AString)^, Length(AString));
    end;
  end;

begin
  WriteStringToStream(Name);
  WriteStringToStream(Entity);
  WriteStringToStream(IconPath);

  WriteStringToStream(Note);
  WriteStringToStream(DNSName);
  WriteStringToStream(Manager);
  AStream.WriteDWord(LongWord(TypeInfo));
  WriteStringToStream(Language);
end;

{ TRootDomainZones }

function TRootDomainZones.LoadCallback (ACallback : TLoadZonesListCallback;
  AData : Pointer) : TRootDomainZones;
begin
  if ACallback <> nil then
  begin
    FLoadCallback := ACallback;
    FLoadCallbackData := AData;
  end;
  Result := Self;
end;

function TRootDomainZones.SaveCallback (ACallback : TSaveZonesListCallback;
  AData : Pointer) : TRootDomainZones;
begin
  if ACallback <> nil then
  begin
    FSaveCallback := ACallback;
    FSaveCallbackData := AData;
  end;
  Result := Self;
end;

procedure TRootDomainZones.LoadDomainZones;
begin
  if Assigned(FLoadCallback) then
    FLoadCallback(FDomainZones, FLoadCallbackData);
end;

procedure TRootDomainZones.SaveDomainZones;
begin
  if Assigned(FSaveCallback) then
    FSaveCallback(FDomainZones, FSaveCallbackData);
end;

function TRootDomainZones.ClearString(AString: string): string;
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
    if (Index >= 2) and (Result[Index] = ' ') and (Result[Index - 1] = ' ') then
      Delete(Result, Index, 1);
  end;
end;

function TRootDomainZones.GetDomainZonesList: TDomainZonesList;
begin
  if FDomainZones.Count = 0 then
    LoadDomainZones;

  Result := FDomainZones;
end;

procedure TRootDomainZones.ParseIanaOrg (ACallback : TParseDomainZoneCallback);
var
  TreeChunk : TParser.TTagNode;
begin
  FSession.Url := IANA_ORG_URL;
  FResponse := TResponse.Create(FSession);

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

  TreeChunk := FParser.Parse(FResponse.Content,
    TParser.TDocumentParseFrom.DOCUMENT_BODY);
  TreeChunk.FirstChildrenNode(TParser.TFilter.Create.ContainsId('body'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('main_right'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('iana-table-frame'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TABLE))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(
      TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create.TagNodeTransform(@ParseIanaOrgCallback));

  if Assigned(ACallback) then
    ACallback(FResponse);

  FreeAndNil(FResponse);
end;

procedure TRootDomainZones.ParseIanaOrgCallback(ANode: TParser.TTagNode;
  AData: Pointer);
var
  Node : TParser.TTagNode;
  Zone, ListValue : TDomainZoneInfo;
begin
  Zone := TDomainZoneInfo.Create;

  { ...
    <td>
      <span class="domain tld">
        <a href="/domains/root/db/aaa.html">.aaa</a>    [<-- Zone.Name]
      </span>
    </td>
  ... }
  Node := ANode.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_TD));
  Zone.Name := ClearString(Node.FirstChildrenNode(TParser.TFilter.Create.Tag(
    TParser.TTag.MyHTML_TAG_SPAN))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_A))
    .Value);

  { ...
    <td>generic</td>                                    [<-- Zone.TypeInfo]
  ... }
  Node := Node.NextNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TD));
  case ClearString(Node.Value) of
    'generic' : Zone.TypeInfo := DOMAIN_GENERIC;
    'generic-restricted' : Zone.TypeInfo := DOMAIN_GENERIC_RESTRICTED;
    'country-code' : Zone.TypeInfo := DOMAIN_COUNTRY_CODE;
    'sponsored' : Zone.TypeInfo := DOMAIN_SPONSORED;
  else
    Zone.TypeInfo := DOMAIN_UNKNOWN;
  end;

  { ...
    <td>American Automobile Association, Inc.</td>      [<-- Zone.Manager]
  ... }
  Node := Node.NextNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TD));
  Zone.Manager := ClearString(Node.Value);

  if FDomainZones.IndexOf(Zone.Name) = -1 then
  begin
    FDomainZones.Add(Zone.Name, Zone)
  end else begin
    ListValue := FDomainZones.Data[FDomainZones.IndexOf(Zone.Name)];

    if (Zone.TypeInfo = DOMAIN_UNKNOWN) and (ListValue.TypeInfo <>
      DOMAIN_UNKNOWN) then
      Zone.TypeInfo := ListValue.TypeInfo;
    if ListValue.DNSName <> '' then
      Zone.DNSName := ListValue.DNSName;
    if ListValue.Entity <> '' then
      Zone.Entity := ListValue.Entity;
    if ListValue.IconPath <> '' then
      Zone.IconPath := ListValue.IconPath;
    if (Zone.Manager = '') and (ListValue.Manager <> '') then
      Zone.Manager := ListValue.Manager;
    if ListValue.Note <> '' then
      Zone.Note := ListValue.Note;
    if ListValue.Language <> '' then
      Zone.Language := ListValue.Language;

    FDomainZones.Add(Zone.Name, Zone);
  end;
end;

procedure TRootDomainZones.ParsePublicSuffixOrg (ACallback :
  TParseDomainZoneCallback);

  function ReadLine (var AString : string) : string;
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

var
  ListString, LineString : string;
  Zone, ListValue : TDomainZoneInfo;
begin
  FSession.Url := PUBLICSUFFIX_ORG_URL;
  FResponse := TResponse.Create(FSession);

  ListString := FResponse.Content;
  while ListString <> '' do
  begin
    LineString := ReadLine(ListString);
    if (Length(LineString) >= 2) and (LineString[1] <> '/') and
      (LineString[2] <> '/') then
    begin
      Zone := TDomainZoneInfo.Create;

      if LineString[1] = '!' then
        Delete(LineString, 1, 1);

      if LineString[1] = '*' then
        Delete(LineString, 1, 2);

      Zone.Name := '.' + LineString;
      Zone.TypeInfo := DOMAIN_UNKNOWN;

      if FDomainZones.IndexOf(Zone.Name) = -1 then
      begin
        FDomainZones.Add(Zone.Name, Zone);
      end else
      begin
        ListValue := FDomainZones.Data[FDomainZones.IndexOf(Zone.Name)];

        if ListValue.TypeInfo <> DOMAIN_UNKNOWN then
          Zone.TypeInfo := ListValue.TypeInfo;
        if ListValue.DNSName <> '' then
          Zone.DNSName := ListValue.DNSName;
        if ListValue.Entity <> '' then
          Zone.Entity := ListValue.Entity;
        if ListValue.IconPath <> '' then
          Zone.IconPath := ListValue.IconPath;
        if ListValue.Manager <> '' then
          Zone.Manager := ListValue.Manager;
        if ListValue.Note <> '' then
          Zone.Note := ListValue.Note;
        if ListValue.Language <> '' then
          Zone.Language := ListValue.Language;

        FDomainZones.Add(Zone.Name, Zone);
      end;
    end;
  end;

  if Assigned(ACallback) then
    ACallback(FResponse);

  FreeAndNil(FResponse);
end;

procedure TRootDomainZones.ParseWikipediaOrg (ACallback :
  TParseDomainZoneCallback);
var
  TreeChunk : TParser.TTagNode;
begin
  FSession.Url := WIKIPEDIA_ORG_URL;
  FResponse := TResponse.Create(FSession);

  { ...
    <body>
    ...
      <div id="content" ...>
      ...
        <div id="bodyContent ...>
        ...
          <div id="mw-content-text" ...>
          ...
            <div class="mw-parser-output">
              <table class="wikitable sortable jquery-tablesorter">
                ...
                <tbody>
                  <tr>
                    <td>
                      <a href="/wiki/.com" title=".com">.com</a> [<-- Zone.Name]
                    </td>
                    <td>commercial</td>                          [<-- Zone.Type]
                    <td>
                      <a href="/wiki/Verisign" title="Verisign">Verisign</a>
                      [<-- Zone.Manager]
                    </td>
                    <td>
                      This is an open TLD; any person or entity is permitted to
                      register. Though originally intended for use by for-profit
                      business entities, for a number of reasons it became the
                      "main" TLD for domain names and is currently used by all
                      types of entities including nonprofits, schools, and
                      private individuals. Domain name registrations may be
                      successfully challenged if the holder cannot prove an
                      outside relation justifying reservation of the name,
                        <sup class="noprint Inline-Template Template-Fact">
                          [
                            <i><a href="/wiki/Wikipedia:Citation_needed" title=
                              "Wikipedia:Citation needed"><span title="This
                              claim needs references to reliable sources.
                              (September 2016)">citation needed</span></a></i>
                          ]
                        </sup>
                      to prevent "
                        <a href="/wiki/Cybersquatting" title="Cybersquatting">
                          squatting</a>". It was originally administered by the
                        <a href="/wiki/United_States_Department_of_Defense"
                        title="United States Department of Defense">United
                        States Department of Defense</a>.
                        [<-- Zone.Notes]
                    <td>
                    <td class="table-yes"> ... </td>
                    <td class="table-yes"> ... </td>
                    <td class="table-yes"> ... </td>
                    <td class="table-yes"> ... </td>
                  </tr>
                  ...
                </tbody>
              </table>
              ...
              <table class="wikitable">
                <tbody>
                  <tr style="..."> ... </tr>
                  <tr>
                    <td>
                      <a href="/wiki/.arpa" title=".arpa">.arpa</a>
                      [<-- Zone.Name]
                    </td>
                    <td>"Address and Routing Parameter Area"</td>
                    [<-- Zone.Entity]
                    <td>
                      Originally assigned to the
                      <a class="mw-redirect" href="/wiki/Advanced_Research_
                        Project_Agency" title="Advanced Research Project Agency"
                      >Advanced Research Project Agency</a>
                      in the early days on the Internet, .arpa is now
                      exclusively used as on
                      <a class="mw-redirect" href="/wiki/Infrastructure_top-
                        level_domain" title="Infrastructure top-level domain">
                        Internet infrastructure TLD</a>
                      .
                    [<-- Zone.Notes]
                    </td>
                    <td class="table-no"> ... </td>
                    <td class="table-yes"> ... </td>
                  </tr>
                </tbody>
              </table>
              ...
              <table> ... </table>
              ...
              <table class="wikitable sortable jquery-tablesorter">
                ...
                <tbody>
                  <tr>
                    <td>
                      <a href="/wiki/.ac" title=".ac">.ac</a>    [<-- Zone.Name]
                    </td>
                    <td>
                      <span class="flagicon">
                        <img class="thumbborder" alt="" src="//upload.wikimedia.
                          org/wikipedia/commons/thumb/6/65/Flag_of_Ascension_
                          Island.svg/23px-Flag_of_Ascension_Island.svg.png"
                          decoding="async" srcset="//upload.wikimedia.org/
                          wikipedia/commons/thumb/6/65/Flag_of_Ascension_
                          Island.svg/35px-Flag_of_Ascension_Island.svg.png 1.5x,
                          //upload.wikimedia.org/wikipedia/commons/thumb/6/65/
                          Flag_of_Ascension_Island.svg/46px-Flag_of_Ascension_
                          Island.svg.png 2x" data-file-width="1000"
                          data-file-height="500" width="23" height="12">?
                                                             [<-- Zone.IconPath]
                      </span>
                      <a href="/wiki/Ascension_Island" title="Ascension Island">
                        Ascension Island</a>                   [<-- Zone.Entity]
                    </td>
                    <td></td>
                    <td>
                      Commonly used for academic websites, such as universities.
                      However, .ac is not to be confused with the official
                      academic domains used by several countries such as the
                      <a href="/wiki/United_Kingdom" title="United Kingdom">
                        United Kingdom</a>
                      (
                      <a class="mw-redirect" href="/wiki/.ac.uk" title=".ac.uk"
                      .ac.uk</a>
                      ),
                      <a href="/wiki/India" title="India">India</a>
                      (
                      <a class="mw-redirect" href="/wiki/.ac.in" title=".ac.in">
                      .ac.in</a>
                      ) or
                      <a href="/wiki/Indonesia" title="Indonesia">Indonesia</a>
                      (
                      <a class="mw-redirect" href="/wiki/.ac.id" title=".ac.id">
                      .ac.id</a>
                      ). Also used in the accounting, consulting, and
                      air-conditioning industries.              [<-- Zone.Notes]
                    </td>
                    <td class="table-yes"> ... </td>
                    <td></td>
                    <td class="table-yes> ... </td>
                    <td class="table-no> ... </td>
                  </tr>
                  <tr>
                    ...
                  </tr>
                </tbody>
                ...
              </table>
              ...
  ... }

  TreeChunk : FParser.Parse(FResponse.Content,
    TParser.TDocumentParseFrom.DOCUMENT_BODY);

end;

constructor TRootDomainZones.Create(ASession: TSession; AParser : TParser);
begin
  FSession := ASession;
  FParser := AParser;
  FDomainZones := TDomainZonesList.Create;

  FLoadCallback := nil;
  FLoadCallbackData := nil;
  FSaveCallback := nil;
  FSaveCallbackData := nil;
end;

destructor TRootDomainZones.Destroy;
begin
  FreeAndNil(FDomainZones);
  inherited Destroy;
end;

function TRootDomainZones.CheckDomainZone(AZone: string): Boolean;
begin
  Result := False;
end;

function TRootDomainZones.ExtractDomainZone(AURL: string): TDomainZoneInfo;
var
  Zone : TDomainZoneInfo;
  Index : SizeInt;
begin
  if FDomainZones.Count > 0 then
  begin
    Index := Pos('://', AURL);
    if Index <> 0 then
    begin
      AURL := Copy(AURL, Index + 3, Length(AURL) - Index + 3);
    end;


  end else
    Result := TDomainZoneInfo.Create;
end;

function TRootDomainZones.GetDomainZoneInfo(AZone: string): TDomainZoneInfo;
var
  Zone : TDomainZoneInfo;
  Index : Integer;
begin
  if FDomainZones.Count > 0 then
  begin
    for Index := 0 to FDomainZones.Count - 1 do
      begin
        if AZone[1] <> '.' then
          AZone := '.' + AZone;


      end;
  end else
    Result := TDomainZoneInfo.Create;
end;

procedure TRootDomainZones.ParseDomainZones (ACallback :
  TParseDomainZoneCallback);
begin
  ParsePublicSuffixOrg (ACallback);
  ParseIanaOrg (ACallback);
  ParseWikipediaOrg (ACallback);
end;

end.

