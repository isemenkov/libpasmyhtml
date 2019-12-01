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
(* Project:         'Whois'                                                   *)
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

program Whois;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, pasmyhtml, pascurl, RootZoneDatabase;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FSession : TSession;
    FParser : TParser;
    FRootZonesDatabase : TRootZoneDatabase;
  protected
    procedure DoRun; override;
    procedure PrintHelp;
    procedure ParseRootZones;
    procedure PrintRootZonesList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) '+
    'AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 '+
    'Safari/537.36';


{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'h';
  LongOptions : array [1..3] of string = ('help', 'force-parse-zones',
    'domain-zones-list');
begin
  ErrorMsg:=CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then
    PrintHelp;

  if HasOption('force-parse-zones') then
    ParseRootZones;

  if HasOption('domain-zones-list') then
    PrintRootZonesList;

  NonOptions := TStringList.Create;
  GetNonOptions(ShortOptions, LongOptions, NonOptions);
  if NonOptions.Count = 0 then
  begin
    Terminate;
    Exit;
  end;

  Terminate;
end;

procedure TApplication.PrintHelp;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                libPasMyHTML                                *)'+ sLineBreak +
'(*                object pascal wrapper around MyHTML library                 *)'+ sLineBreak +
'(*                    https://github.com/lexborisov/myhtml                    *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Copyright (c) 2019                                       Ivan Semenkov     *)'+ sLineBreak +
'(* https://github.com/isemenkov/libpasmyhtml                ivan@semenkov.pro *)'+ sLineBreak +
'(*                                                          Ukraine           *)'+ sLineBreak +
'(******************************************************************************)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Usage: whois <options>                                                     *)'+ sLineBreak +
'(*   --force-parse-zones                Force to parse root domain zones      *)'+ sLineBreak +
'(*   --parse-stats                                                            *)'+ sLineBreak +
'(*   --domain-zones-list                                                      *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

procedure TApplication.ParseRootZones;
var
  RootZonesDatabase : TRootZoneDatabaseParser;
begin
  RootZonesDatabase := TRootZoneDatabaseParser.Create(FRootZonesDatabase,
    FSession, FParser);
  RootZonesDatabase.Parse;
end;

procedure TApplication.PrintRootZonesList;
var
  DomainZone : TRootZoneDatabase.TDomainZone;
  Element : TRootZoneDatabase.TInfoElement;
  Counter : Integer;
begin
  Counter := 1;
  for DomainZone in FRootZonesDatabase do
  begin
    Writeln(IntToStr(Counter) + '. Name: ':13, DomainZone.Name);

    for Element in DomainZone do
    begin
      case Element.InfoType of
        INFO_ENTITY   : Writeln('Entity: ':13, Element.Value);
        INFO_ICONPATH : Writeln('Icon path: ':13, Element.Value);
        INFO_NOTE     : Writeln('Note: ':13, Element.Value);
        INFO_DNSNAME  : Writeln('DNS name: ':13, Element.Value);
        INFO_MANAGER  : Writeln('Manager: ':13, Element.Value);
        INFO_TYPE     : Writeln('Type: ':13, Element.Value);
        INFO_LANGUAGE : Writeln('Language: ':13, Element.Value);
      end;
    end;

    Inc(Counter);
    Writeln;
  end;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;

  FSession := TSession.Create;
  FSession.HTTP.UserAgent := USER_AGENT;
  FParser := TParser.Create;
  FRootZonesDatabase := TRootZoneDatabase.Create;
end;

destructor TApplication.Destroy;
begin
  FreeAndNil(FSession);
  FreeAndNil(FParser);
  FreeAndNil(FRootZonesDatabase);
  inherited Destroy;
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='Whois';
  Application.Run;
  Application.Free;
end.

