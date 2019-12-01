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
        INFO_LANGUAGE
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
      public
        property Name : string read FName;
      end;

  protected
    type
      TRootZonesList = specialize TFPGMapObject<string, TDomainZone>;
  protected
    FRootZones : TRootZonesList;
  public

  public
    constructor Create;
    destructor Destroy; override;
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

implementation

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
  Result := FDatabase.FRootZones.Count < FPosition;
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
  Result := FDomain.FInfo.Count < FPosition;
end;

{ TRootZoneDatabase }

constructor TRootZoneDatabase.Create;
begin
  FRootZones := TRootZonesList.Create;
end;

destructor TRootZoneDatabase.Destroy;
begin
  FreeAndNil(FRootZones);
  inherited Destroy;
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

{ TRootZoneDatabase.TInfoElement }

constructor TRootZoneDatabase.TInfoElement.Create(AType: TDomainZoneInfoType;
  AValue: string);
begin
  FType := AType;
  FValue := AValue;
end;

end.

