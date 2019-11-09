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
  TRootDomainZones = class
  public
    type
      TDomaintType = (
        DOMAIN_GENERIC,
        DOMAIN_GENERIC_RESTRICTED,
        DOMAIN_COUNTRY_CODE,
        DOMAIN_SPONSORED
      );

      TDomainInfo = record
        Name : string;          { domain suffix, like .com or .co.uk }
        Entity : string;        { intended use }
        IconPath : string;      { URL to icon }
        Icon : TPicture;        { icon image }
        Note : string;          { general remarks }
        DNSName : string;       {  }
        Manager : string;       { entity the registry has been delegated to }
        TypeInfo : TDomainType; {  }
        Language : string;      {  }
      end;

      TDomainsList = specialize TFPGList<TDomainInfo>;
  private
    FSession : TSession;
    FResponse : TResponse;
    FParser : TParser;
  public
    constructor Create; overload;
    constructor Create(ASession : TSession); overload;

    destructor Destroy; override;

  published

  end;

implementation

end.

