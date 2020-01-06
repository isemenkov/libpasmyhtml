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
  Math, BGRABitmap, BGRABitmapTypes, fgl, pasmyhtml;

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
    constructor Create (TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

  generic TiTreeViewItem<TItemData> = class
  protected
    FParent : Pointer;
    FChildrens : TList;
    FCollapsed : Boolean;
    FRendererData : Pointer;
    FCustomData : Pointer;

    function IsElementRoot : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function IsElementHasChildrens : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetParent : specialize TiTreeViewItem<TItemData>; {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetParent (AParent : specialize TiTreeViewItem<TItemData>);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetItem (AIndex : Cardinal) : specialize TiTreeViewItem<TItemData>;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    function SetItem (AIndex : Cardinal; AItem :
      specialize TiTreeViewItem<TItemData>); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetItemData : TItemData; {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    property IsRoot : Boolean read IsElementRoot;
    property HasChildrens : Boolean read IsElementHasChildrens;
    property Parent : specialize TiTreeViewItem<TItemData> read GetParent
      write SetParent;
    property Items[Index : Cardinal] : specialize TiTreeViewItem<TItemData>
      read GetItem write SetItem;
    property Collapsed : Boolean read FCollapsed write FCollapsed;
    property RendererData : Pointer read FRendererData write FRendererData;
    property CustomData : Pointer read FCustomData write FCustomData;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  generic TiTreeViewModel<TiTreeViewItem> = class (TInterfacedObject,
    specialize IListModel<TiTreeViewItem>)
  protected
    type
      PiTreeViewItem = ^TiTreeViewItem;
  protected
    FItemsList : specialize TFPGObjectList<TiTreeViewItem>;
    FDrawItemsList : specialize TFPGList<PiTreeViewItem>;
  end;

  TiTreeViewRenderer = class (TInterfacedObject, IListRenderer)

  end;

implementation


end.
