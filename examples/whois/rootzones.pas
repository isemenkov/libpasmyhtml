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

  { Parse domain zones from: }
  { https://www.iana.org/domains/root/db }
  { https://publicsuffix.org/list/effective_tld_names.dat }
  { https://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#Country_code_top-level_domains }

  TRootDomainZones = class
  public
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
    FCloseSession : Boolean;
    FResponse : TResponse;
    FParser : TParser;
    FDomainZones : TDomainZonesList;

    FLoadCallback : TLoadZonesListCallback;
    FLoadCallbackData : Pointer;

    FSaveCallback : TSaveZonesListCallback;
    FSaveCallbackData : Pointer;

    function GetDomainZonesList : TDomainZonesList; inline;
    procedure SetSession (ASession : TSession); inline;
  public
    constructor Create; overload;
    constructor Create (ASession : TSession; ADomainZones : TDomainZonesList =
      nil); overload;
    constructor Create (ADomainZones : TDomainZonesList); overload;
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
    property Session : TSession write SetSession;
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
    FLoadCallback(FDomainZones, FLoadCallbackData)
  else begin

  end;
end;

procedure TRootDomainZones.SaveDomainZones;
begin
  if Assigned(FSaveCallback) then
    FSaveCallback(FDomainZones, FSaveCallbackData)
  else begin

  end;
end;

function TRootDomainZones.GetDomainZonesList: TDomainZonesList;
begin
  Result := FDomainZones;
end;

procedure TRootDomainZones.SetSession(ASession: TSession);
begin
  if (FSession <> nil) and FCloseSession then
    FreeAndNil(FSession);

  FSession := ASession;
  FCloseSession := False;
end;

constructor TRootDomainZones.Create;
begin
  FSession := TSession.Create;
  FCloseSession := True;
  FParser := TParser.Create;
  FDomainZones := TDomainZonesList.Create;
  FLoadCallback := nil;
  FLoadCallbackData := nil;
  FSaveCallback := nil;
  FSaveCallbackData := nil;
end;

constructor TRootDomainZones.Create(ASession: TSession; ADomainZones :
  TDomainZonesList);
begin
  if ASession <> nil then
  begin
    FSession := ASession;
    FCloseSession := False;
  end else begin
    FSession := TSession.Create;
    FCloseSession := True;
  end;

  FParser := TParser.Create;

  if ADomainZones <> nil then
    FDomainZones := ADomainZones
  else begin
    FDomainZones := TDomainZonesList.Create;

  end;

  FLoadCallback := nil;
  FLoadCallbackData := nil;
  FSaveCallback := nil;
  FSaveCallbackData := nil;
end;

constructor TRootDomainZones.Create(ADomainZones: TDomainZonesList);
begin
  FSession := TSession.Create;
  FCloseSession := True;
  FParser := TParser.Create;

  if ADomainZones <> nil then
    FDomainZones := ADomainZones
  else begin
    FDomainZones := TDomainZonesList.Create;

  end;

  FLoadCallback := nil;
  FLoadCallbackData := nil;
  FSaveCallback := nil;
  FSaveCallbackData := nil;
end;

destructor TRootDomainZones.Destroy;
begin
  FreeAndNil(FResponse);

  if FCloseSession then
    FreeAndNil(FSession);

  FreeAndNil(FParser);
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

end;

end.

