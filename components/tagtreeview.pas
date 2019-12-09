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
      private
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

        function AddChildren (ANode : TParser.TTagNode; AColor : TColor) :
          TTagTreeItem;

        property Tag : TParser.TTag read FTagElement write SetTagElement;
        property Color : TColor read FTagElementColor write SetTagElementColor;
        property Data : Pointer read FTagElementData write SetTagElementData;
        property DrawOffset : Integer read FTagElementDrawOffset
          write SetTagElementDrawOffset;
        property Childrens : TTagTreeItemList read FChildren;
      end;

  private
    FBitmap : TBGRABitmap;
    FItems : TTagTreeItemList;

  protected
    class function GetControlClassDefaultSize : TSize; override;
    procedure DoOnResize; override;
    procedure RenderControl; virtual;
    procedure CalculateScrollRanges; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;
    procedure Paint; override;

    property Items : TTagTreeItemList read FItems;
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

function TCustomTagTreeView.TTagTreeItem.AddChildren(ANode: TParser.TTagNode;
  AColor: TColor): TTagTreeItem;
begin
  FChildren.Add(TTagTreeItem.Create(ANode, AColor));
  Result := Self;
end;

destructor TCustomTagTreeViewTTagTreeItem.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

{ TCustomTagTreeView }

class function TCustomTagTreeView.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 100;
  Result.cy := 100;
end;

procedure TCustomTagTreeView.DoOnResize;
begin
  // TODO
end;

procedure TCustomTagTreeView.RenderControl;
begin
  // TODO
end;

procedure TCustomTagTreeView.CalculateScrollRanges;
begin
  // TODO
end;

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

procedure TCustomTagTreeView.Paint;
begin
  FBitmap.Draw(Canvas, 0, 0);
  inherited Paint;
end;

end.
