(******************************************************************************)
(*                                TTagTreeView                                *)
(*              Custom TreeView component for display html tree               *)
(*                 https://github.com/isemenkov/libpasmyhtml                  *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpasmyhtml                ivan@semenkov.pro *)
(*                                                          Ukraine           *)
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

unit TagTreeView;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  Math, BGRABitmap, BGRABitmapTypes, fgl, pasmyhtml;

type

  { TCustomTagTreeView }

  TCustomTagTreeView = class(TScrollingWinControl)
  public
    type
      TTagTreeItem = class;
      TTagTreeItemList = specialize TFPGObjectList<TTagTreeItem>;

      { TTagTreeItem }

      TTagTreeItem = class
      protected
        FParent : TTagTreeItem;
        FChildren : TTagTreeItemList;

        FTagElement : TParser.TTag;
        FTagElementTitle : string;
        FTagElementAttributes : string;
        FTagElementColor : TColor;
        FTagElementData : Pointer;
        FTagElementDrawOffset : Integer;

        procedure SetTagElement (ATag : TParser.TTag);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetTagElementColor (AColor : TColor);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetTagElementData (AData : Pointer);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetTagElementDrawOffset (ADrawOffset : Integer);
          {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create (ANode : TParser.TTagNode; AColor : TColor);
        destructor Destroy; override;

        property Tag : TParser.TTag read FTagElement write SetTagElement;
        property Color : TColor read FTagElementColor write SetTagElementColor;
        property Data : Pointer read FTagElementData write SetTagElementData;
        property DrawOffset : Integer read FTagElementDrawOffset
          write SetTagElementDrawOffset;
      end;

  private
    FBitmap : TBGRABitmap;
    FItems : TCustomTagTreeItemList;
  protected
    function GetItem (AIndex : Integer) : TCustomTagTreeItem;
    procedure SetItem (AIndex : Integer; AItem : TCustomTagTreeItem);
  public
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;
  public
    function AddItem (ANode : TParser.TTagNode) : TCustomTagTreeItem;
  public
    property Items[Index : Integer] : TCustomTagTreeItem read GetItem
      write SetItem;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('libPasMyHTML',[TCustomTagTreeView]);
end;

{ TCustomTagTreeView.TTagTreeItem }

procedure TCustomTagTreeView.TTagTreeItem.SetTagElement(ATag: TParser.TTag);
begin
  FTagElement := ATag;
end;

procedure TCustomTagTreeView.TTagTreeItem.SetTagElementColor(AColor: TColor);
begin
  FTagElementColor := AColor;
end;

procedure TCustomTagTreeView.TTagTreeItem.SetTagElementData(AData: Pointer);
begin
  FTagElementData := AData;
end;

procedure TCustomTagTreeView.TTagTreeItem.SetTagElementDrawOffset(ADrawOffset:
  Integer);
begin
  FTagElementDrawOffset := ADrawOffset;
end;

constructor TCustomTagTreeView.TTagTreeItem.Create(ANode: TParser.TTagNode;
  AColor: TColor);
begin
  FChildren := TTagTreeItemList.Create(True);
end;

destructor TCustomTagTreeView.TTagTreeItem.Destroy;
begin
  inherited Destroy;
end;

destructor TCustomTagTreeViewTTagTreeItem.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

{ TCustomTagTreeView }

constructor TCustomTagTreeView.Create(AOwner: TComponent);
begin
  //FBitmap := TBGRABitmap.Create();
  FItems := TCustomTagTreeItemList.Create(True);
end;

destructor TCustomTagTreeView.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FItems);
  inherited Destroy;
end;

end.
