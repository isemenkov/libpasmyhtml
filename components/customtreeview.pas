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
  public
    type
      PTreeItemElement = ^TTreeItemElement;

      { TTreeItemElement }

      TTreeItemElement = class
      protected
        FData : TItemData;
        FChildrensHead : PTreeItemElement;
        FChildrensLast : PTreeItemElement;
        FPrev : PTreeItemElement;
        FNext : PTreeItemElement;
      protected
        function GetValue : TItemData; inline;
        procedure SetValue (AValue : TItemData); inline;
      public
        constructor Create (AData : TItemData);
        destructor Destroy; override;

        function AddChildren (AData : TItemData) : TTreeItemElement;
        function HasNext : Boolean; inline;
        function Next : TTreeItemElement; inline;
        function HasChildrens : Boolean; inline;
        function Children : TTreeItemElement; inline;

        property Value : TItemData read GetValue write SetValue;
      end;
  protected





  public

  end;

  generic TiTreeViewModel<TTreeItem> = class (TInterfacedObject,
    specialize IListModel<TTreeItem>)
  protected
    type
      PTreeItem = ^TTreeItem;
  protected
    FItemsList : specialize TFPGObjectList<TTreeItem>;
    FDrawItemsList : specialize TFPGList<PTreeItem>;
  end;

  TiTreeViewRenderer = class (TInterfacedObject, IListRenderer)

  end;

implementation



{ TiTreeViewItem.TTreeItemElement }

function TiTreeViewItem.TTreeItemElement.GetValue: TItemData;
begin
  Result := FData;
end;

procedure TiTreeViewItem.TTreeItemElement.SetValue(AValue: TItemData);
begin
  FData := AValue;
end;

constructor TiTreeViewItem.TTreeItemElement.Create(AData: TItemData);
begin
  FData := AData;
  FChildrensHead := nil;
  FChildrensLast := nil;
  FPrev := nil;
  FNext := nil;
end;

destructor TiTreeViewItem.TTreeItemElement.Destroy;
begin
  inherited Destroy;
end;

function TiTreeViewItem.TTreeItemElement.AddChildren(AData: TItemData
  ): TTreeItemElement;
var
  Element : TTreeItemElement;
begin
  Element := TTreeItemElement.Create(AData);

  { ItemElement hasn't childrens }
  if FChildrensHead = nil then
  begin
    FChildrensHead := @Element;
    Result := Element;
    Exit;
  end;

  { Exists only one element }
  if FChildrensLast = nil then
  begin
    FChildrensHead^.FNext := @Element;
    FChildrensLast := @Element;
    Element.FPrev := FChildrensHead;
    Result := Element;
    Exit;
  end;

  { Insert next element }
  FChildrensLast^.FNext := @Element;
  Element.FPrev := FChildrensLast;
  FChildrensLast := @Element;
  Result := Element;
end;

function TiTreeViewItem.TTreeItemElement.HasNext: Boolean;
begin
  Result := FNext <> nil;
end;

function TiTreeViewItem.TTreeItemElement.Next: TTreeItemElement;
begin
  Result := FNext^;
end;

function TiTreeViewItem.TTreeItemElement.HasChildrens: Boolean;
begin
  Result := FChildrensHead <> nil;
end;

function TiTreeViewItem.TTreeItemElement.Children: TTreeItemElement;
begin
  Result := FChildrensHead^;
end;

end.
