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

      TDomainZonesList = specialize TFPGList<TDomainZoneInfo>;

      TLoadZonesListCallback = function (var AList : TDomainZonesList;
        AData : Pointer) : Boolean of object;
      TSaveZonesListCallback = function (var AList : TDomainZonesList;
        AData : Pointer) : Boolean of object;
  private
    FSession : TSession;
    FResponse : TResponse;
    FParser : TParser;
    FDomainZones : TDomainZonesList;

    FLoadCallback : TLoadZonesListCallback;
    FLoadCallbackData : Pointer;

    FSaveCallback : TSaveZonesListCallback;
    FSaveCallbackData : Pointer;

    function GetDomainZonesList : TDomainZonesList; inline;
    procedure ParseIanaOrg;
    procedure ParsePublicSuffixOrg;
    procedure ParseWikipediaOrg;
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
    procedure ParseDomainZones;
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

function TRootDomainZones.GetDomainZonesList: TDomainZonesList;
begin
  Result := FDomainZones;
end;

procedure TRootDomainZones.ParseIanaOrg;
var
  TreeChunk : TParser.TTagNode;
begin
  FSession.Url := IANA_ORG_URL;
  FResponse := TResponse.Create(FSession);
  TreeChunk := FParser.Parse(FResponse.Content,
    TParser.TDocumentParseFrom.DOCUMENT_BODY);
  TreeChunk.FirstChildrenNode(TParser.TFilter.Create.ContainsId('body'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('main_right'))
    .FirstChildrenNode(TParser.TFilter.Create.ContainsId('iana-table-frame'))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TABLE))
    .FirstChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TBODY))
    .EachChildrenNode(TParser.TFilter.Create.Tag(TParser.TTag.MyHTML_TAG_TR),
      TParser.TTransform.Create);
  FreeAndNil(FResponse);
end;

procedure TRootDomainZones.ParsePublicSuffixOrg;
begin

end;

procedure TRootDomainZones.ParseWikipediaOrg;
begin

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
begin
  Result := TDomainZoneInfo.Create;

  if FDomainZones.Count > 0 then
  begin

  end;
end;

function TRootDomainZones.GetDomainZoneInfo(AZone: string): TDomainZoneInfo;
begin
  Result := TDomainZoneInfo.Create;

  if FDomainZones.Count > 0 then
  begin

  end;
end;

procedure TRootDomainZones.ParseDomainZones;
begin
  ParseIanaOrg;
  ParsePublicSuffixOrg;
  ParseWikipediaOrg;
end;

end.

