(******************************************************************************)
(*                              TCustomTreeView                               *)
(*              Custom TreeView component for display html tree               *)
(*                 https://github.com/isemenkov/libpasmyhtml                  *)
(*                                                                            *)
(* Copyright (c) 2019 - 2020                                Ivan Semenkov     *)
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

unit CustomTreeView;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  Math, BGRABitmap, BGRABitmapTypes, fgl;

type

  generic IListModel<Item> = interface

  end;

  IListRenderer = interface

  end;

  generic TiCustomScrollingListControl<IListModel, IListRenderer> = class
    (TScrollingWinControl)
  protected
    Model : IListModel;
    Renderer : IListRenderer;
  public
    //constructor Create (TheOwner : TComponent); override;
    //destructor Destroy; override;
  end;

  { TiTreeViewItem }

  generic TiTreeViewItem<TItemData> = class
  protected
    type
      PTreeViewItem = ^TTreeViewItem;
      TTreeViewItem = specialize TiTreeViewItem<TItemData>;
  protected
    FItemData : TItemData;
    FPrevElement : PTreeViewItem;
    FNextElement : PTreeViewItem;
    FChildrenElement : PTreeViewItem;
    FLastChildrenElement : PTreeViewItem;

    function GetValue : TItemData; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetValue (AItemData : TItemData); {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create (AItemData : TItemData);
    destructor Destroy; override;

    function PrevElement : PTreeViewItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    function NextElement : PTreeViewItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    function HasChildrens : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ChildrenElement : PTreeViewItem; {$IFNDEF DEBUG}inline;{$ENDIF}

    function AddChildren (AItemData : TItemData) : PTreeViewItem;

    property Value : TItemData read GetValue write SetValue;
  end;

  generic TiTreeViewModel<TTreeItem> = class (TInterfacedObject,
    specialize IListModel<TTreeItem>)
  protected


  end;

  TiTreeViewRenderer = class (TInterfacedObject, IListRenderer)

  end;

implementation





{ TiTreeViewItem }

function TiTreeViewItem.HasChildrens: Boolean;
begin
  Result := FChildrenElement <> nil;
end;

function TiTreeViewItem.ChildrenElement: PTreeViewItem;
begin
  Result := FChildrenElement;
end;

function TiTreeViewItem.AddChildren(AItemData: TItemData): PTreeViewItem;
begin
  if not HasChildrens then
  begin
    New(FChildrenElement{%H-});
    FChildrenElement^ := TTreeViewItem.Create(AItemData);
    FLastChildrenElement := FChildrenElement;
    Result := FChildrenElement;
    Exit;
  end;

  New(FLastChildrenElement^.FNextElement{%H-});
  FLastChildrenElement^.FNextElement^ := TTreeViewItem.Create(AItemData);
  FLastChildrenElement^.FNextElement^.FPrevElement := FLastChildrenElement;
  FLastChildrenElement := FLastChildrenElement^.FNextElement;
end;

function TiTreeViewItem.PrevElement: PTreeViewItem;
begin
  Result := FPrevElement;
end;

function TiTreeViewItem.NextElement: PTreeViewItem;
begin
  Result := FNextElement;
end;

function TiTreeViewItem.GetValue: TItemData;
begin
  Result := FItemData;
end;

procedure TiTreeViewItem.SetValue(AItemData: TItemData);
begin
  FItemData := AItemData;
end;

constructor TiTreeViewItem.Create(AItemData: TItemData);
begin
  FItemData := AItemData;
  FPrevElement := nil;
  FNextElement := nil;
  FChildrenElement := nil;
  FLastChildrenElement := nil;
end;

destructor TiTreeViewItem.Destroy;
begin
  inherited Destroy;
end;

end.
