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
  Classes, SysUtils, CustApp, pasmyhtml, pascurl, RootZones;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FSession : TSession;
    FParser : TParser;
  protected
    procedure DoRun; override;
    procedure PrintHeader;
    procedure PrintHelp;

    procedure ParseRootZones;
    procedure SaveRootDomainZonesCache (ADomainZones :
      TRootDomainZones.TDomainZonesList);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) '+
    'AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 '+
    'Safari/537.36';
  ROOT_ZONES_CACHE_FILE = 'RootDomainZonesCache.bin';

{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'h';
  LongOptions : array [1..2] of string = ('help', 'force-parse-zones');
begin
  ErrorMsg:=CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  PrintHeader;

  if HasOption('h', 'help') then
    PrintHelp;

  if HasOption('force-parse-zones') then
    ParseRootZones;

  NonOptions := TStringList.Create;
  GetNonOptions(ShortOptions, LongOptions, NonOptions);
  if NonOptions.Count = 0 then
  begin
    Terminate;
    Exit;
  end;

  Terminate;
end;

procedure TApplication.PrintHeader;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                libPasMyHTML                                *)'+ sLineBreak +
'(*                object pascal wrapper around MyHTML library                 *)'+ sLineBreak +
'(*                    https://github.com/lexborisov/myhtml                    *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Copyright (c) 2019                                       Ivan Semenkov     *)'+ sLineBreak +
'(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)'+ sLineBreak +
'(*                                                          Ukraine           *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

procedure TApplication.PrintHelp;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Usage:                                                                     *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

procedure TApplication.ParseRootZones;
var
  RootZones : TRootDomainZones;
begin
  RootZones := TRootDomainZones.Create(FSession, FParser);
  RootZones.ParseDomainZones;
  SaveRootDomainZonesCache(RootZones.DomainZones);
  FreeAndNil(RootZones);
end;

procedure TApplication.SaveRootDomainZonesCache (ADomainZones :
  TRootDomainZones.TDomainZonesList);
var
  Zone : TRootDomainZones.TDomainZoneInfo;
  FileStream : TFileStream;
begin
  FileStream := TFileStream.Create(ROOT_ZONES_CACHE_FILE, fmCreate or
    fmOpenWrite);
  //FileStream.Seek(0, soFromBeginning);

  for Zone in ADomainZones do
    Zone.SaveToStream(FileStream);

  FreeAndNil(FileStream);
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FSession := TSession.Create;
  FSession.HTTP.UserAgent := USER_AGENT;
  FParser := TParser.Create;
end;

destructor TApplication.Destroy;
begin
  FreeAndNil(FSession);
  FreeAndNil(FParser);
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

