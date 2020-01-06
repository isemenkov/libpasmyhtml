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
      TCurrentType = specialize TiTreeViewItem<TItemData>;
      PCurrentType = ^TCurrentType;
  protected
    FParent : PCurrentType;
    FChildrens : TList;
    FCollapsed : Boolean;
    FItemData : TItemData;
    FRendererData : Pointer;
    FCustomData : Pointer;

    function IsElementRoot : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function IsElementHasChildrens : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetParent : specialize TiTreeViewItem<TItemData>; {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetParent (AParent : specialize TiTreeViewItem<TItemData>);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetChildren (AIndex : Cardinal) :
      specialize TiTreeViewItem<TItemData>; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetChildren (AIndex : Cardinal; AItem :
      specialize TiTreeViewItem<TItemData>); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetItemData : TItemData; {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    property IsRoot : Boolean read IsElementRoot;
    property HasChildrens : Boolean read IsElementHasChildrens;
    property Parent : specialize TiTreeViewItem<TItemData> read GetParent
      write SetParent;
    property Childrens[Index : Cardinal] : specialize TiTreeViewItem<TItemData>
      read GetChildren write SetChildren;
    property ItemData : TItemData read FItemData write FItemData;
    property RendererData : Pointer read FRendererData write FRendererData;
    property CustomData : Pointer read FCustomData write FCustomData;
    property Collapsed : Boolean read FCollapsed write FCollapsed;

    function AddChildren (AItem : specialize TiTreeViewItem<TItemData>) :
      specialize TiTreeViewItem<TItemData>;
  public
    constructor Create (AItemData : TItemData);
    destructor Destroy; override;
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


{ TiTreeViewItem }

function TiTreeViewItem.IsElementRoot: Boolean;
begin
  Result := (FParent = nil);
end;

function TiTreeViewItem.IsElementHasChildrens: Boolean;
begin
  Result := (FChildrens.Count > 0);
end;

function TiTreeViewItem.GetParent: specialize TiTreeViewItem<TItemData>;
begin
  if not IsRoot then
    Result := TCurrentType(FParent^)
  else
    Result := nil;
end;

procedure TiTreeViewItem.SetParent(AParent:
  specialize TiTreeViewItem<TItemData>);
begin
  FParent := Pointer(AParent);
end;

function TiTreeViewItem.GetChildren(AIndex: Cardinal):
  specialize TiTreeViewItem<TItemData>;
begin
  if FChildrens.Count > AIndex then
    Result := PCurrentType(FChildrens[AIndex])^
  else
    Result := nil;
end;

procedure TiTreeViewItem.SetChildren(AIndex: Cardinal;
  AItem: specialize TiTreeViewItem<TItemData>);
begin
  if FChildrens.Count > AIndex then
    FChildrens[AIndex] := Pointer(AItem);
end;

function TiTreeViewItem.GetItemData: TItemData;
begin
  Result := FItemData;
end;

function TiTreeViewItem.AddChildren(AItem: specialize TiTreeViewItem<TItemData>
  ): specialize TiTreeViewItem<TItemData>;
begin
  FChildrens.Add(Pointer(AItem));
  AItem.FParent := Pointer(Self);
  Result := PCurrentType(FChildrens[FChildrens.Count - 1])^;
end;

constructor TiTreeViewItem.Create (AItemData : TItemData);
begin
  inherited Create;
  FParent := nil;
  FChildrens := TList.Create;
  FCollapsed := False;
  FItemData := AItemData;
  FRendererData := nil;
  FCustomData := nil;
end;

destructor TiTreeViewItem.Destroy;
begin
  inherited Destroy;
  // for Item in FChildrens do
  //   FreeAndNil(Item);
  FreeAndNil(FChildrens);
end;

end.
