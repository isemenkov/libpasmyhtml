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

  { TPadding }
  { Stored paddings gap size data }
  TPadding = record
    Top, Right, Bottom, Left : Integer;
  end;

  { TiCustomTreeView }
  { Custom tree view control }
  TiCustomTreeView = class(TScrollingWinControl)
  public
    type
      { Tree view item element type }
      PiTreeItem = ^TiTreeItem;
      TiTreeItem = class;

      { Tree view items list type }
      TiTreeItemList = specialize TFPGObjectList<TiTreeItem>;

      { Pointer list for tree view items }
      TiDrawTreeItemList = specialize TGPGList<PiTreeItem>;

      { TiTreeItem }
      { Tree view item element }
      TiTreeItem = class
      private
        { Current item parent element }
        FElementParent : TiTreeItem;
        { List of current items childrens }
        FElementChildrens : TiTreeItemList;

        { Current element title }
        FElementTitle : string;
        { Current element text }
        FElementText : string;
        { Current element label color }
        FElementColor : TColor;
        { Current element collapsed state }
        FElementCollapsed : Boolean;
        { Item additional user controlled data pointer }
        FElementData : Pointer;
        { Current element start draw offset }
        FDrawElementOffset : Integer;

        { Set current element label title }
        procedure SetElementTitle (ATitle : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Set current element text }
        procedure SetElementText (AText : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Set current element label color }
        procedure SetElementColor (AColor : TColor); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Set current element user data pointer }
        procedure SetData (AData : Pointer); {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element start draw offset data }
        procedure SetDrawElementOffset (AOffset : Integer); {$IFNDEF DEBUG}
          inline;{$ENDIF}
        { Set current element collapse state }
        procedure SetCollapsed (ACollapsed : Boolean); {$IFNDEF DEBUG}inline;
          {$ENDIF}
      protected
        { Element parent item if exists }
        property Parent : TiTreeItem read FElementParent;
        { Element children's elements }
        property Childrens : TiTreeItemList read FElementChildrens;

        { Element label title }
        property Title : string read FElementTitle write SetElementTitle;
        { Element text }
        property Text : string read FElementText write SetElementText;
        { Element label color }
        property Color : TColor read FElementColor write SetElementColor;
        { Element collapse state }
        property Collapsed : Boolean read FElementCollapsed write SetCollapsed;
        { Element user pointer data }
        property Data : Pointer read FData write SetData;
        { Element start draw offset data }
        property DrawOffset : Integer read FDrawElementOffset
          write SetDrawElementOffset;
      public
        constructor Create (ATitle, AText : string; AColor : TColor);
        constructor Create (AParent : TiTreeItem; ATitle, ATest : string;
          AColor : TColor);
        destructor Destroy; override;
      end;

  private
    { Control canvas }
    FBitmap : TBGRABitmap;
    { Control's element items }
    FItems : TiTreeItemList;
    { List of controls draw items }
    FDrawItems : TiDrawTreeItemList;
    { Control element's font antialias }
    FElementFontAntialias : Boolean;
    { Elements max text length }
    FElementMaxTextLength : Cardinal;
    { Contol element heigth size }
    FElementHeight : Cardinal;
    { Control label title element's font style }
    FElementTitleFontStyle : TFontStyles;
    { Control label padding }
    FElementTitlePadding : TPadding;
    { Control label round rect corner radius }
    FElementTitleRoundRect : Cardinal;
    { Control text text element's font style }
    FElementTextFontStyle : TFontStyles;
    { Control text padding }
    FElementTextPadding : TPadding;
    { Control element's draw level offset }
    FElementDrawOffset : Integer;

    { Set control item antialias font }
    procedure SetElementFontAntialias (AFontAntialias : Boolean); {$IFNDEF
      DEBUG}inline;{$ENDIF}
    { Set control item label title font style }
    procedure SetElementTitleFontStyle (AStyle : TFontStyles); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control item height }
    procedure SetElementHeight (AHeight : Cardinal); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Set control label title padding sizes }
    procedure SetElementTitlePadding (APadding : TPadding); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control label title round rect radius size }
    procedure SetElementTitleRoundRect (ARound : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control item text padding }
    procedure SetElementTextPadding (APadding : TPadding); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control text font style }
    procedure SetElementTextFontStyle (AStyle : TFontStyles); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control item draw level offset }
    procedure SetElementDrawOffset (AOffset : Integer); {$IFNDEF DEBUG}inline;
      {$ENDIF}
  protected
    class function GetControlClassDefaultSize : TSize; override;
    procedure DoOnResize; override;
    { Repaint control }
    procedure RenderControl; virtual;
    //procedure Calculate
    { Recalc scroll bars }
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
    property ItemTitleFontStyle : TFontStyles read FElementTitleFontStyle write
      SetElementTitleFontStyle default [fsBold];
    property ItemTextFontStyle : TFontStyles read FElementTextFontStyle write
      SetElementTextFontStyle default [];
    property Items : TiTreeItemList read FItems;
    property ItemHeight : Cardinal read FElementHeight write SetElementHeight
      default 16;
    property ItemTitlePadding : TPadding read FElementTitlePadding write
      SetElementTitlePadding;
    property ItemTitleRoundRect : Cardinal read FElementTitleRoundRect write
      SetElementTitleRoundRect default 8;
    property ItemTextPadding : TPadding read FElementTextPadding write
      SetElementTextPadding;
    property ItemDrawOffset : Integer read FElementDrawOffset write
      SetElementDrawOffset;
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
        property Data;
      end;

  public
    constructor Create (ANode : TParser.TTagNode; AColor : TColor);
    constructor Create (AParent : TiTagTreeItem; ANode : TParser.TTagNode;
      AColor : TColor);
    destructor Destroy; override;
  public
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
    property FontAntialias;
    property FontStyle;
    property Items;
    property ItemHeight;
    property ItemTitlePadding;
    property ItemTitleRoundRect;
    property ItemTextPadding;
    property ItemDrawOffset;
  end;

function Padding (ATop, ARight, ABottom, ALeft : Integer) : TPadding;
procedure Register;

implementation

function Padding(ATop, ARight, ABottom, ALeft: Integer): TPadding;
begin
  with Result do
  begin
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
    Left := ALeft;
  end;
end;

procedure Register;
begin
  RegisterComponents('libPasMyHTML',[TCustomTagTreeView]);
end;

{ TiCustomTreeView }

procedure TiCustomTreeView.SetElementFontAntialias(AFontAntialias: Boolean);
begin
  if FElementFontAntialias <> AFontAntialias then
    FElementFontAntialias := AFontAntialias;
end;

procedure TiCustomTreeView.SetElementTitleFontStyle(AStyle: TFontStyles);
begin
  if FElementTitleFontStyle <> AStyle then
    FElementTitleFontStyle := AStyle;
end;

procedure TiCustomTreeView.SetElementHeight(AHeight: Cardinal);
begin
  if FElementHeight <> AHeight then
    FElementHeight := AHeight;
end;

procedure TiCustomTreeView.SetElementTitlePadding(APadding: TPadding);
begin
  if FElementTitlePadding <> APadding then
    FElementTitlePadding := APadding;
end;

procedure TiCustomTreeView.SetElementTitleRoundRect(ARound: Cardinal);
begin
  if FElementTitleRoundRect <> ARound then
    FElementTitleRoundRect := ARound;
end;

procedure TiCustomTreeView.SetElementTextPadding(APadding: TPadding);
begin
  if FElementTextPadding <> APadding then
    FElementTextPadding := APadding;
end;

procedure TiCustomTreeView.SetElementTextFontStyle(AStyle: TFontStyles);
begin
  if FElementTextFontStyle <> AStyle then
    FElementTextFontStyle := AStyle;
end;

procedure TiCustomTreeView.SetElementDrawOffset(AOffset: Integer);
begin
  if FElementDrawOffset <> AOffset then
    FElementDrawOffset := AOffset;
end;

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

procedure TiCustomTreeView.Calculate;
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
  if FElementTitle <> ATitle then
    FElementTitle := ATitle;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementText(AText: string);
begin
  if FElementText <> AText then
    FElementText := AText;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementColor(AColor: TColor);
begin
  if FElementColor <> AColor then
    FElementColor := AColor;
end;

procedure TiCustomTreeView.TiTreeItem.SetData(AData: Pointer);
begin
  if FElementData <> AData then
    FElementData := AData;
end;

procedure TiCustomTreeView.TiTreeItem.SetDrawElementOffset(AOffset: Integer);
begin
  if FElementDrawOffset <> AOffset then
    FDrawElementOffset := AOffset;
end;

procedure TiCustomTreeView.TiTreeItem.SetCollapsed(ACollapsed: Boolean);
begin
  if FElementCollapsed <> ACollapsed then
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
