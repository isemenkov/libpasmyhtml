(******************************************************************************)
(*                                TTagTreeView                                *)
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
  { TiCustomTreeView }
  TiCustomTreeView = class (TScrollingWinControl)
  public
    type
      { TiCustomTreeViewRenderer }
      { Display TiCustomTreeView renderer. }
      TiCustomTreeViewRenderer = class
      public
        type
          { TSide }
          TSide = (sdTop, sdRight, sdBottom, sdLeft, sdAll);

          { TMarginSide }
          TMarginSide = packed record
            Side : TSide;
            Value : Integer;
          end;

          { TMargin }
          TMargin = packed record
            Top, Right, Bottom, Left : Integer;

            class operator+ (ALeft, ARight : TMargin) : TMargin;
            class operator+ (AMargin : TMargin; AValue : TMarginSide) : TMargin;
          end;

          { TPaddingSide }
          TPaddingSide = packed record
            Side : TSide;
            Value : Integer;
          end;

          { TPadding }
          TPadding = packed record
            Top, Right, Bottom, Left : Integer;

            class operator+ (ALeft, ARight : TPadding) : TPadding;
            class operator+ (APadding : TPadding; AValue : TPaddingSide) :
              TPadding;
          end;

          { TFontProperty }
          TFontProperty = packed record
            Name : string;
            Style : TFontStyle;
            Size : Cardinal;
            Color : TBGRAPixel;
            Quality : TBGRAFontQuality;
            Antialias : Boolean;
            Orientation : Integer;
            Renderer : TBGRACustomFontRenderer;
          end;

          { TBorderLineStyle }
          TBorderLineStyle = packed record
            JoinStyle : TFPPenJoinStyle;
            LineCap : TFPPenEndCap;
            PenStyle : TFPPenStyle;
          end;

          { TiTreeItemView }
          TiTreeItemView = packed record
            Font : TFontProperty;
            Background : TBGRAPixel;
            FocusBorderStyle : TBorderLineStyle;
            FocusBackground : TBGRAPixel;
            Margin : TMargin;
            Padding : TPadding;
          end;

      private
        ItemView : TiTreeItemView;
      protected
        property FontName : string read ItemView.Font.Name;
        property FontStyle : TFontStyle read ItemView.Font.Style;
        property FontSize : Cardinal read ItemView.Font.Size;
        property FontColor : TBGRAPixel read ItemView.Font.Color;
        property FontQuality : TBGRAFontQuality read ItemView.Font.Quality;
        property FontAntialias : Boolean read ItemView.Font.Antialias;
        property FontOrientation : Integer read ItemView.Font.Orientation;
        property FontRenderer : TBGRACustomFontRenderer
          read ItemView.Font.Renderer;
        property ItemBackground : TBGRAPixel read ItemView.Background;
        property ItemFocusBorderStyleJoinStyle : TFPPenJoinStyle
          read ItemView.FocusBorderStyle.JoinStyle;
        property ItemFocusBorderStyleLineCap : TFPPenEndCap
          read ItemView.FocusBorderStyle.LineCap;
        property ItemFocusBorderStylePenStyle : TFPPenStyle
          read ItemView.FocusBorderStyle.PenStyle;

      end;

      { TiCustomTreeViewModel }
      TiCustomTreeViewModel = class
      public
        type
          PiTreeItem = ^TiTreeItem;
          TiTreeItem = class;

          { TiTreeItemList }
          TiTreeItemList = specialize TFPGObjectList<TiTreeItem>;

          { TiDrawTreeItemList }
          TiDrawTreeItemList = specialize TFPGList<PiTreeItem>;

          { TiTreeItem }
          TiTreeItem = class
          public
            type
              TElementText = packed record
                Text : string;
                Data : Pointer;
              end;
          end;
      end;
  end;

  { Type aliases }
  TSide = TiCustomTreeView.TiCustomTreeViewRenderer.TSide;
  TMargin = TiCustomTreeView.TiCustomTreeViewRenderer.TMargin;
  TMarginSide = TiCustomTreeView.TiCustomTreeViewRenderer.TMarginSide;
  TPadding = TiCustomTreeView.TiCustomTreeViewRenderer.TPadding;
  TPaddingSide = TiCustomTreeView.TiCustomTreeViewRenderer.TPaddingSide;
  TFontProperty = TiCustomTreeView.TiCustomTreeViewRenderer.TFontProperty;

  function MarginSide (ASide : TSide; AValue : Integer) : TMarginSide;
  function PaddingSide (ASide : TSide; AValue : Integer) : TPaddingSide;
  function Margin (ATop, ARight, ABottom, ALeft : Integer) : TMargin;

  function Padding (ATop, ARight, ABottom, ALeft : Integer) : TPadding;


implementation

function MarginSide(ASide: TSide; AValue: Integer): TMarginSide;
begin
  Result.Side := ASide;
  Result.Value := AValue;
end;

function PaddingSide(ASide: TSide; AValue: Integer): TPaddingSide;
begin
  Result.Side := ASide;
  Result.Value := AValue;
end;

{ TiCustomTreeView.TiCustomTreeViewRenderer.TPadding }

class operator TiCustomTreeView.TiCustomTreeViewRenderer.TPadding.+(ALeft,
  ARight: TPadding): TPadding;
begin
  Result.Top := ALeft.Top + ARight.Top;
  Result.Right := ALeft.Right + ARight.Right;
  Result.Bottom := ALeft.Bottom + ARight.Bottom;
  Result.Left := ALeft.Left + ARight.Left;
end;

class operator TiCustomTreeView.TiCustomTreeViewRenderer.TPadding.+(
  APadding: TPadding; AValue: TPaddingSide): TPadding;
begin
  case AValue.Side of
    sdTop    : Result.Top := APadding.Top + AValue.Value;
    sdRight  : Result.Right := APadding.Right + AValue.Value;
    sdBottom : Result.Bottom := APadding.Bottom + AValue.Value;
    sdLeft   : Result.Left := APadding.Left + AValue.Value;
  else
    begin
      Result.Top := APadding.Top + AValue.Value;
      Result.Right := APadding.Right + AValue.Value;
      Result.Bottom := APadding.Bottom + AValue.Value;
      Result.Left := APadding.Left + AValue.Value;
    end;
  end;
end;

{ TiCustomTreeView.TiCustomTreeViewRenderer.TMargin }

class operator TiCustomTreeView.TiCustomTreeViewRenderer.TMargin.+(ALeft,
  ARight: TMargin): TMargin;
begin
  Result.Top := ALeft.Top + ARight.Top;
  Result.Right := ALeft.Right + ARight.Right;
  Result.Bottom := ALeft.Bottom + ARight.Bottom;
  Result.Left := ALeft.Left + ARight.Left;
end;

class operator TiCustomTreeView.TiCustomTreeViewRenderer.TMargin.+(
  AMargin: TMargin; AValue: TMarginSide): TMargin;
begin
  case AValue.Side of
    sdTop    : Result.Top := AMargin.Top + AValue.Value;
    sdRight  : Result.Right := AMargin.Right + AValue.Value;
    sdBottom : Result.Bottom := AMargin.Bottom + AValue.Value;
    sdLeft   : Result.Left := AMargin.Left + AValue.Value;
  else
    begin
      Result.Top := AMargin.Top + AValue.Value;
      Result.Right := AMargin.Right + AValue.Value;
      Result.Bottom := AMargin.Bottom + AValue.Value;
      Result.Left := AMargin.Left + AValue.Value;
    end;
  end;
end;


end.
