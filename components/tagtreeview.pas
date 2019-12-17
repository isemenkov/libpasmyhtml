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

      { TiTreeItem }

      TiTreeItem = class
      private
        FData: Pointer;
        FElementParent : TiTreeItem;
        FElementChildrens : TiTreeItemList;

        FElementTitle : string;
        FElementText : string;
        FElementColor : TColor;
        FElementCollapsed : Boolean;
        FElementData : Pointer;

        FDrawElementOffset : Integer;

        procedure SetElementTitle (ATitle : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        procedure SetElementText (AText : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        procedure SetElementColor (AColor : TColor); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        procedure SetData (AData : Pointer); {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetDrawElementOffset (AOffset : Integer); {$IFNDEF DEBUG}
          inline;{$ENDIF}
        procedure SetCollapsed (ACollapsed : Boolean); {$IFNDEF DEBUG}inline;
          {$ENDIF}
      protected
        property Parent : TiTreeItem read FElementParent;
        property Childrens : TiTreeItemList read FElementChildrens;

        property Title : string read FElementTitle write SetElementTitle;
        property Text : string read FElementText write SetElementText;
        property Color : TColor read FElementColor write SetElementColor;
        property Collapsed : Boolean read FElementCollapsed write SetCollapsed;

        property Data : Pointer read FData write SetData;

        property DrawOffset : Integer read FDrawElementOffset
          write SetDrawElementOffset;
      public
        constructor Create (ATitle, AText : string; AColor : TColor);
        constructor Create (AParent : TiTreeItem; ATitle, ATest : string;
          AColor : TColor);
        destructor Destroy; override;
      end;

  private
    FBitmap : TBGRABitmap;
    FItems : TiTreeItemList;

    FElementFontAntialias : Boolean;
    FElementFontStyle : TFontStyles;
    FElementMaxTextLength : Cardinal;
    FElementHeight : Cardinal;
    FElementPaddingTop, FElementPaddingRight, FElementPaddingBottom,
      FElementPaddingLeft : Cardinal;
    FElementTitleRoundRect : Cardinal;

    procedure SetElementFontAntialias (AFontAntialias : Boolean); {$IFNDEF
      DEBUG}inline;{$ENDIF}
    procedure SetElementFontStyle (AStyle : TFontStyles); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetElementHeight (AHeight : Cardinal); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    procedure SetElementPaddingTop (APadding : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetElementPaddingRight (APadding : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetElementPaddingBottom (APadding : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetElementPaddingLeft (APadding : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    procedure SetElementRoundRect (ARound : Cardinal); {$IFNDEF DEBUG}inline;
      {$ENDIF}
  protected
    class function GetControlClassDefaultSize : TSize; override;
    procedure DoOnResize; override;
    procedure RenderControl; virtual;
    procedure CalculateScrollRanges; virtual;
  public
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;
    procedure Paint; override;
  protected
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
    property FontAntialias : Boolean read FElementFontAntialias write
      SetElementFontAntialias default True;
    property FontStyle : TFontStyles read FElementFontStyle write
      SetElementFontStyle default [fsBold];
    property Items : TiTreeItemList read FItems;
    property ItemHeight : Cardinal read FElementHeight write SetElementHeight
      default 16;
    property ItemTopPadding : Cardinal read FElementPaddingTop write
      SetElementPaddingTop default 2;
    property ItemRightPadding : Cardinal read FElementPaddingRight write
      SetElementPaddingRight default 2;
    property ItemBottomPadding : Cardinal read FElementPaddingBottom write
      SetElementPaddingBottom default 2;
    property ItemLeftPadding : Cardinal read FElementPaddingLeft write
      SetElementPaddingLeft default 2;
    property ItemRoundRect : Cardinal read FElementTitleRoundRect write
      SetElementRoundRect default 8;
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
        property Collapsed;
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

{ TiCustomTreeView }

class function TiCustomTreeView.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

procedure TiCustomTreeView.DoOnResize;
begin
  RenderControl;
  CalculateScrollRanges;
  Invalidate;
end;

procedure TiCustomTreeView.RenderControl;
begin

end;

procedure TiCustomTreeView.CalculateScrollRanges;
begin
  VertScrollBar.Range := 0;
  HorzScrollBar.Range := 0;
  if FBitmap.Height > ClientHeight then
    VertScrollBar.Range := FBitmap.Height;
  if FBitmap.Width > ClientWidth then
    HorzScrollBar.Range := FBitmap.Width;
end;

constructor TiCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  FBitmap := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  FItems := TiTreeItemList.Create(True);
  FElementMaxTextLength := 0;
  FItemHeight := 16;
end;

destructor TiCustomTreeView.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TiCustomTreeView.Paint;
begin
  FBitmap.Draw(Canvas, 0, 0, False);
  inherited Paint;
end;

{ TiCustomTreeView.TiTreeItem }

procedure TiCustomTreeView.TiTreeItem.SetElementTitle(ATitle: string);
begin
  FElementTitle := ATitle;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementText(AText: string);
begin
  FElementText := AText;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementColor(AColor: TColor);
begin
  FElementColor := AColor;
end;

procedure TiCustomTreeView.TiTreeItem.SetData(AData: Pointer);
begin
  FElementData := AData;
end;

procedure TiCustomTreeView.TiTreeItem.SetDrawElementOffset(AOffset: Integer);
begin
  FDrawElementOffset := AOffset;
end;

function TiCustomTreeView.TiTreeItem.GetCollapsed: Boolean;
begin
  Result := FElementCollapsed;
end;

constructor TiCustomTreeView.TiTreeItem.Create(ATitle, AText: string;
  AColor: TColor);
begin
  FElementParent := nil;
  FElementChildrens := TiTreeItemList.Create(True);
  FElementCollapsed := False;
  FElementTitle := ATitle;
  FElementText := AText;
  FElementColor := AColor;
end;

constructor TiCustomTreeView.TiTreeItem.Create(AParent: TiTreeItem; ATitle,
  ATest: string; AColor: TColor);
begin
  FElementParent := AParent;
  FElementChildrens := TiTreeItemList.Create(True);
  FElementCollapsed := False;
  FElementTitle := ATitle;
  FElementText := AText;
  FElementColor := AColor;
end;

destructor TiCustomTreeView.TiTreeItem.Destroy;
begin
  FreeAndNil(FElementChildrens);
  inherited Destroy;
end;



end.
