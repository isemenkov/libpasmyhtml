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

  { TiCustomTreeView }

  TiCustomTreeView = class(TScrollingWinControl)
  public
    type
      TiTreeItem = class;
      TiTreeItemList = specialize TFPGObjectList<TiTreeItem>;

      TiTreeItem = class
      private
        FElementParent : TiTreeItem;
        FElementChildrens : TiTreeItemList;

        FElementTitle : string;
        FElementText : string;
        FElementColor : TColor;

        FData : Pointer;

        FDrawElementOffset : Integer;

        procedure SetElementTitle (ATitle : string);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetElementText (AText : string);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetElementColor (AColor : TColor);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetData (AData : Pointer); {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetDrawElementOffset (AOffset : Integer);
          {$IFNDEF DEBUG}inline;{$ENDIF}
      protected
        property Parent : TiTreeItem read FElementParent;
        property Childrens : TiTreeItemList read FElementChildrens;

        property Title : string read FElementTitle write SetElementTitle;
        property Text : string read FElementText write SetElementText;
        property Color : TColor read FElementColor write SetElementColor;

        property Data : Pointer read FData write SetData;

        property DrawOffset : Integer read FDrawElementOffset
          write SetDrawElementOffset;

        property Align;
        property Anchors;
        property AnchorSide;
        property AnchorSideLeft;
        property AnchorSideTop;
        property AnchorSideRight;
        property AnchorSideBottom;
        property BorderStyle;
        property BorderWidth;
        property BorderSpacing;
        property Cursor;
        property HorzScrollBar;
        property VertScrollBar;
        property Enabled;
        property Visible;
        property Left;
        property Height;
        property Top;
        property Width;
      public
        constructor Create (ATitle, AText : string; AColor : TColor);
        destructor Destroy; override;
      end;

  private
    FBitmap : TBGRABitmap;
    FItems : TiTreeItemList;

  protected
    class function GetControlClassDefaultSize : TSize; override;
    procedure DoOnResize; override;
    procedure RenderControl; virtual;
    procedure CalculateScrollRanges; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;
    procedure Paint; override;
  protected
    property Items : TiTreeItemList read FItems;
  end;

  TiCustomHTMLTreeView = class (TiCustomTreeView)
  public
    type
      TiTagTreeItem = class (TiCustomTreeView.TiTreeItem)
      private
        FTagElement : TParser.TTag;
      public
        constructor Create (ANode : TParser.TTagNode; AColor : TColor);
        destructor Destroy; override;

        property Parent;
        property Childrens;
        property Tag : TParser.TTag read FTagElement write SetTagElement;
        property Color;
      end;

  public
    constructor Create (ANode : TParser.TTagNode; AColor : TColor);
    destructor Destroy; override;
  end;




procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('libPasMyHTML',[TCustomTagTreeView]);
end;



end.
