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

  { TFontProperty }
  TFontProperty = record
    Font : TFont;
    FontColorOpacity : Byte;
    FontQuality : TBGRAFontQuality;
    FontOrientation : Integer;
    FontAntialias : Boolean;
    FontRenderer : TBGRACustomFontRenderer;
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
      TiDrawTreeItemList = specialize TFPGList<PiTreeItem>;

      { TiTreeItem }
      { Tree view item element }
      TiTreeItem = class
      private
        type
          { Element label data }
          TElementLabel = record
            Text : string;
            BackgroundColor : TBGRAPixel;
            Font : TFontProperty;
          end;

          { Element text data }
          TElementText = record
            Text : string;
            Font : TFontProperty;
          end;
      private
        { Current item parent element }
        FElementParent : TiTreeItem;
        { List of current items childrens }
        FElementChildrens : TiTreeItemList;
        { Current element label data }
        FElementLabel : TElementLabel;
        { Current element text data }
        FElementText : TElementText;
        { Curent element background color }
        FItemBackgroundColor : TColor;
        { Current element collapsed state }
        FItemCollapsed : Boolean;
        { Item additional user controlled data pointer }
        FItemData : Pointer;
        { Current element start draw offset }
        FItemDrawOffset : Integer;

        { Check if current element is root }
        function IsRootElement : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element label text value }
        function GetElementLabelText : string;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element label title text }
        procedure SetElementLabelText (AText : string);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return current element label background color }
        function GetElementLabelBackgroundColor : TBGRAPixel;
        { Set element label background color }
        procedure SetElementLabelBackgroundColor (AColor : TBGRAPixel);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element label font }
        function GetElementLabelFont : TFont;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set element label font parameters }
        procedure SetElementLabelFont (AFont : TFont);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element label font opacity value }
        function GetElementLabelFontOpacity : Byte;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set element label font opacity }
        procedure SetElementLabelFontOpacity (AOpacity : Byte);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element label font quality }
        function GetElementLabelFontQuality : TBGRAFontQuality;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set element label font quality }
        procedure SetElementLabelFontQuality (AQuality : TBGRAFontQuality);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element text value }
        function GetElementText : string;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element text }
        procedure SetElementText (AText : string);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return current element text font }
        function GetElementTextFont : TFont;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element font value }
        procedure SetElementTextFont (AFont : TFont);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return current element font color opacity }
        function GetTextFontColorOpacity : Byte;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element font color opacity }
        procedure SetTextFontColorOpacity (AOpacity : Byte);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return current element font quality }
        function GetTextFontQuality : TBGRAFontQuality;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element font quality }
        procedure SetTextFontQuality (AFontQuality : TBGRAFontQuality);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element background color }
        procedure SetElementBackgroundColor (AColor : TColor);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element user data pointer }
        procedure SetData (AData : Pointer);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element start draw offset data }
        procedure SetDrawElementOffset (AOffset : Integer);
          {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element collapse state }
        procedure SetCollapsed (ACollapsed : Boolean);
          {$IFNDEF DEBUG}inline;{$ENDIF}
      protected
        { Is current item element is root }
        property IsRoot : Boolean read IsRootElement;
        { Item element parent item if exists }
        property Parent : TiTreeItem read FElementParent;
        { Item element children's elements }
        property Childrens : TiTreeItemList read FElementChildrens;
        { Item element label title text }
        property LabelText : string read GetElementLabelText write
          SetElementLabelText;
        { Item element label background color }
        property LabelBackgroundColor : TBGRAPixel read
          GetElementLabelBackgroundColor write SetElementLabelBackgroundColor;
        { Item element label font }
        property LabelFont : TFont read GetElementLabelFont write
          SetElementLabelFont;
        { Item element label font color opacity }
        property LabelFontColorOpacity : Byte read GetElementLabelFontOpacity
          write SetElementLabelFontOpacity;
        { Item element label font quality }
        property LabelFontQuality : TBGRAFontQuality read
          GetElementLabelFontQuality write SetElementLabelFontQuality;
        { Item element text string }
        property Text : string read GetElementText write SetElementText;
        { Item element text font }
        property TextFont : TFont read GetElementTextFont write
          SetElementTextFont;
        { Item element text font color opacity }
        property TextFontColorOpacity : Byte read GetTextFontColorOpacity
          write SetTextFontColorOpacity;
        { Item element text font quality }
        property TextFontQuality : TBGRAFontQuality read GetTextFontQuality
          write SetTextFontQuality;
        { Item element background color }
        property BackgroundColor : TColor read FItemBackgroundColor write
          SetElementBackgroundColor;
        { Element collapse state }
        property IsCollapsed : Boolean read FItemCollapsed write
          SetCollapsed;
        { Element user pointer data }
        property Data : Pointer read FItemData write SetData;
        { Element start draw offset data }
        property DrawOffset : Integer read FItemDrawOffset
          write SetDrawElementOffset;
      public
        constructor Create (ALabelTitle, AText : string; AColor : TColor);
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
    { Draw elements max text length }
    FDrawElementMaxTextLength : Cardinal;
    { Contol element heigth size }
    FElementHeight : Cardinal;
    { Gap between items top and bottom }
    FElementGap : Cardinal;
    { Control label padding }
    FElementLabelPadding : TPadding;
    { Control label round rect corner radius }
    FElementLabelRoundRect : Cardinal;
    { Control text padding }
    FElementTextPadding : TPadding;
    { Control element's draw level offset }
    FElementDrawOffset : Integer;

    { Set control item antialias font }
    procedure SetElementFontAntialias (AFontAntialias : Boolean);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    { Set control item height }
    procedure SetElementHeight (AHeight : Cardinal);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    { Set control label title padding sizes }
    procedure SetElementLabelPadding (APadding : TPadding);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    { Set control label title round rect radius size }
    procedure SetElementLabelRoundRect (ARound : Cardinal);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    { Set control item text padding }
    procedure SetElementTextPadding (APadding : TPadding);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    { Set control item draw level offset }
    procedure SetElementDrawOffset (AOffset : Integer);
      {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function GetControlClassDefaultSize : TSize; override;
  protected
    procedure DoOnResize; override;
    { Repaint control }
    procedure RenderControl; virtual;
    { Calculate control }
    procedure CalculateControl; virtual;
    { Recalc scroll bars }
    procedure CalculateScrollRanges; virtual;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    function AddItem (AItem : TiTreeItem) : TiTreeItem; overload;
    function AddItem (AParent : TiTreeItem; AItem : TiTreeItem) : TiTreeItem;
      overload;
    function AddItem (ALabelTitle, AText : string; ALabelColor : TColor) :
      TiTreeItem; overload;
    function AddItem (AParent : TiTreeItem; ALabelTitle, AText : string;
      ALabelColor : TColor) : TiTreeItem; overload;
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
    { Elements draw text font antialias }
    property FontAntialias : Boolean read FElementFontAntialias write
      SetElementFontAntialias default True;
    { Control elements }
    property Items : TiTreeItemList read FItems;
    { Element item height }
    property ItemHeight : Cardinal read FElementHeight write SetElementHeight
      default 16;
    { Element label padding }
    property ItemLabelPadding : TPadding read FElementLabelPadding write
      SetElementLabelPadding;
    { Element label round rect radius }
    property ItemLabelRoundRect : Cardinal read FElementLabelRoundRect write
      SetElementLabelRoundRect default 8;
    { Element text padding }
    property ItemTextPadding : TPadding read FElementTextPadding write
      SetElementTextPadding;
    { Element draw level offset }
    property ItemDrawOffset : Integer read FElementDrawOffset write
      SetElementDrawOffset;
  end;

  { TiCustomHTMLTreeView }

  TiCustomHTMLTreeView = class (TiCustomTreeView)
  public
    type

      { TiTagTreeItem }

      TiTagTreeItem = class (TiCustomTreeView.TiTreeItem)
      private
        FTagElement : TParser.TTag;
      public
        constructor Create (ANode : TParser.TTagNode; AColor : TColor);
        destructor Destroy; override;

        property Parent;
        property Childrens;
        property Tag : TParser.TTag read FTagElement; //write SetTagElement;
        property IsCollapsed;
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
  end;

operator= (ALeft, ARight : TPadding) : Boolean;
operator= (ALeft, ARight : TFontProperty) : Boolean;

function Padding (ATop, ARight, ABottom, ALeft : Integer) : TPadding; overload;
function Padding (ATopBottom, ARightLeft : Integer) : TPadding; overload;
procedure Register;

implementation

operator=(ALeft, ARight: TPadding) : Boolean;
begin
  Result := (ALeft.Top = ARight.Top) and (ALeft.Right = ARight.Right) and
    (ALeft.Bottom = ARight.Bottom) and (ALeft.Left = ARight.Left);
end;

operator=(ALeft, ARight: TFontProperty): Boolean;
begin
  Result := (ALeft.Font = ARight.Font) and (ALeft.FontColorOpacity =
    ARight.FontColorOpacity) and (ALeft.FontQuality = ARight.FontQuality) and
    (ALeft.FontOrientation = ARight.FontOrientation) and (ALeft.FontAntialias =
    ARight.FontAntialias) and (ALeft.FontRenderer = ARight.FontRenderer);
end;

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

function Padding(ATopBottom, ARightLeft: Integer): TPadding;
begin
  with Result do
  begin
    Top := ATopBottom;
    Right := ARightLeft;
    Bottom := ATopBottom;
    Left := ARightLeft;
  end;
end;

procedure Register;
begin
  RegisterComponents('libPasMyHTML',[TiCustomTreeView]);
end;

{ TiCustomHTMLTreeView }

constructor TiCustomHTMLTreeView.Create(ANode: TParser.TTagNode; AColor: TColor
  );
begin
  //
end;

constructor TiCustomHTMLTreeView.Create(AParent: TiTagTreeItem;
  ANode: TParser.TTagNode; AColor: TColor);
begin
  //
end;

destructor TiCustomHTMLTreeView.Destroy;
begin
  inherited Destroy;
end;

{ TiCustomHTMLTreeView.TiTagTreeItem }

constructor TiCustomHTMLTreeView.TiTagTreeItem.Create(ANode: TParser.TTagNode;
  AColor: TColor);
begin
  //
end;

destructor TiCustomHTMLTreeView.TiTagTreeItem.Destroy;
begin
  inherited Destroy;
end;

{ TiCustomTreeView }

procedure TiCustomTreeView.SetElementFontAntialias(AFontAntialias: Boolean);
begin
  if FElementFontAntialias <> AFontAntialias then
    FElementFontAntialias := AFontAntialias;
end;

procedure TiCustomTreeView.SetElementHeight(AHeight: Cardinal);
begin
  if FElementHeight <> AHeight then
    FElementHeight := AHeight;
end;

procedure TiCustomTreeView.SetElementLabelPadding(APadding: TPadding);
begin
  if FElementLabelPadding <> APadding then
    FElementLabelPadding := APadding;
end;

procedure TiCustomTreeView.SetElementLabelRoundRect(ARound: Cardinal);
begin
  if FElementLabelRoundRect <> ARound then
    FElementLabelRoundRect := ARound;
end;

procedure TiCustomTreeView.SetElementTextPadding(APadding: TPadding);
begin
  if FElementTextPadding <> APadding then
    FElementTextPadding := APadding;
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
  FDrawItems.Clear;
  RenderControl;
  CalculateScrollRanges;
  Invalidate;
end;

procedure TiCustomTreeView.RenderControl;

  procedure DrawItem (ARect : TRect; AElement : TiTreeItem); {$IFNDEF DEBUG}
    inline;{$ENDIF}
  var
    LabelTextSize, TextSize : TSize;
  begin
    FBitmap.FontAntialias := FontAntialias;
    FBitmap.FontHeight := FElementHeight - FElementLabelPadding.Top -
      FElementLabelPadding.Bottom - 2;

    { Draw label }
    FBitmap.FontStyle := AElement.LabelFont.Style;
    LabelTextSize := FBitmap.TextSize(AElement.LabelText);
    FBitmap.FillRoundRect(ARect.Left + AElement.DrawOffset, ARect.Top + 1,
      LabelTextSize.Width + ItemLabelPadding.Left + ItemLabelPadding.Right +
      AElement.DrawOffset, ARect.Bottom - 1, ItemLabelRoundRect,
      ItemLabelRoundRect, AElement.LabelBackgroundColor);
    FBitmap.TextOut(ARect.Left + AElement.DrawOffset + ItemLabelPadding.Left,
      ARect.Top + ItemLabelPadding.Top, AElement.LabelText,
      ColorToBGRA(AElement.LabelFont.Color, AElement.TextFontColorOpacity));

    { Draw text }
    FBitmap.FontStyle := AElement.TextFont.Style;
    TextSize := FBitmap.TextSize(AElement.Text);
    FBitmap.TextOut(ARect.Left + AElement.DrawOffset + ItemLabelPadding.Left +
      LabelTextSize.Width + ItemLabelPadding.Right + ItemTextPadding.Left,
      ARect.Top + ItemTextPadding.Top, AElement.Text,
      ColorToBGRA(AElement.TextFont.Color, AElement.TextFontColorOpacity));
  end;

var
  Index : Integer;
begin
  CalculateControl;

  FBitmap.SetSize(FDrawElementMaxTextLength, Max(FDrawItems.Count *
    FElementHeight, ClientHeight));
  FBitmap.Fill(BGRAWhite);

  for Index := 0 to FDrawItems.Count - 1 do
  begin
    DrawItem(TRect.Create(0, Index * FElementHeight, ClientWidth, Index *
      FElementHeight + FElementHeight), TiTreeItem(FDrawItems[Index]));
  end;
end;

procedure TiCustomTreeView.CalculateControl;

  function GetElementMaxTextLength (AElement : TiTreeItem; AElementLevel :
    Cardinal) : Cardinal; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := AElement.DrawOffset + ItemLabelPadding.Left +
      + ItemLabelPadding.Right + ItemTextPadding.Left + ItemTextPadding.Right;
    FBitmap.FontHeight := ItemHeight - ItemLabelPadding.Top -
      ItemLabelPadding.Bottom - 2;
    FBitmap.FontStyle := AElement.LabelFont.Style;
    Inc(Result, FBitmap.TextSize(AElement.LabelText).Width);
    FBitmap.FontStyle := AElement.TextFont.Style;
    Inc(Result, FBitmap.TextSize(AElement.Text).Width);
  end;

  procedure CalcTextLength (AElement : TiTreeItem; AElementLevel : Cardinal;
    Drawable : Boolean); {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    ElementLength : Cardinal;
  begin
    ElementLength := GetElementMaxTextLength(AElement, AElementLevel);
    FElementMaxTextLength := Max(FElementMaxTextLength, ElementLength);
    if Drawable then
      FDrawElementMaxTextLength := Max(FDrawElementMaxTextLength,
        ElementLength);
  end;

  procedure CalcElement (AElement : TiTreeItem; AElementLevel : Cardinal;
    Drawable : Boolean); {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Item : TiTreeItem;
  begin
    AElement.DrawOffset := ItemDrawOffset * AElementLevel;
    CalcTextLength(AElement, AElementLevel, Drawable);

    if Drawable then
      FDrawItems.Add(Pointer(AElement));

    for Item in AElement.Childrens do
    begin
      CalcElement(Item, AElementLevel + 1, not AElement.IsCollapsed);
    end;
  end;

var
  Item : TiTreeItem;
begin
  FElementMaxTextLength := 0;
  FDrawElementMaxTextLength := 0;

  for Item in FItems do
    CalcElement(Item, 1, True);

  FElementMaxTextLength := Max(FElementMaxTextLength, ClientWidth);
  FDrawElementMaxTextLength := Max(FDrawElementMaxTextLength, ClientWidth);
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
  FDrawItems := TiDrawTreeItemList.Create;
  FElementFontAntialias := True;
  FElementMaxTextLength := 0;
  FDrawElementMaxTextLength := 0;
  FElementHeight := 17;
  FElementLabelPadding := Padding(1, 10);
  FElementLabelRoundRect := 17;
  FElementTextPadding := Padding(1, 5);
  FElementDrawOffset := 20;
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

function TiCustomTreeView.AddItem(AItem: TiTreeItem): TiTreeItem;
begin
  FItems.Add(AItem);
  Result := FItems[FItems.Count - 1];
end;

function TiCustomTreeView.AddItem(AParent: TiTreeItem; AItem: TiTreeItem
  ): TiTreeItem;
begin
  AParent.FElementChildrens.Add(AItem);
  AParent.FItemCollapsed := False;
  Result := AParent.FElementChildrens[AParent.FElementChildrens.Count - 1];
end;

function TiCustomTreeView.AddItem(ALabelTitle, AText: string;
  ALabelColor: TColor): TiTreeItem;
var
  Item : TiTreeItem;
begin
  Item := TiTreeItem.Create(ALabeltitle, AText, ALabelColor);
  Result := AddItem(Item);
end;

function TiCustomTreeView.AddItem(AParent: TiTreeItem; ALabelTitle,
  AText: string; ALabelColor: TColor): TiTreeItem;
var
  Item : TiTreeItem;
begin
  Item := TiTreeItem.Create(ALabelTitle, Atext, ALabelColor);
  Result := AddItem(AParent, Item);
end;

function TiCustomTreeView.TiTreeItem.IsRootElement: Boolean;
begin
  Result := (FElementParent = nil);
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelText: string;
begin
  Result := FElementLabel.Text;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelText(AText: string);
begin
  if FElementLabel.Text <> AText then
  begin
    FElementLabel.Text := AText;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelBackgroundColor: TBGRAPixel;
begin
  Result := FElementLabel.BackgroundColor;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelBackgroundColor(
  AColor: TBGRAPixel);
begin
  if FElementLabel.BackgroundColor <> AColor then
  begin
    FELementLabel.BackgroundColor := AColor;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelFont: TFont;
begin
  Result := FElementLabel.Font.Font;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelFont(AFont: TFont);
begin
  if FElementLabel.Font.Font <> AFont then
  begin
    FElementLabel.Font.Font := AFont;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelFontOpacity: Byte;
begin
  Result := FElementLabel.Font.FontColorOpacity;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelFontOpacity(AOpacity: Byte
  );
begin
  if FElementLabel.Font.FontColorOpacity <> AOpacity then
  begin
    FElementLabel.Font.FontColorOpacity := AOpacity;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelFontQuality: TBGRAFontQuality;
begin
  Result := FElementLabel.Font.FontQuality;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelFontQuality(
  AQuality: TBGRAFontQuality);
begin
  if FElementLabel.Font.FontQuality <> AQuality then
  begin
    FElementLabel.Font.FontQuality := AQuality;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementText: string;
begin
  Result := FElementText.Text;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementText(AText: string);
begin
  if FElementText.Text <> AText then
  begin
    FElementText.Text := AText;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetElementTextFont: TFont;
begin
  Result := FElementText.Font.Font;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementTextFont(AFont: TFont);
begin
  if FElementText.Font.Font <> AFont then
  begin
    FElementText.Font.Font := AFont;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetTextFontColorOpacity: Byte;
begin
  Result := FElementText.Font.FontColorOpacity;
end;

procedure TiCustomTreeView.TiTreeItem.SetTextFontColorOpacity(AOpacity: Byte);
begin
  if FElementText.Font.FontColorOpacity <> AOpacity then
  begin
    FElementText.Font.FontColorOpacity := AOpacity;
  end;
end;

function TiCustomTreeView.TiTreeItem.GetTextFontQuality: TBGRAFontQuality;
begin
  Result := FElementText.Font.FontQuality;
end;

procedure TiCustomTreeView.TiTreeItem.SetTextFontQuality(
  AFontQuality: TBGRAFontQuality);
begin
  if FElementText.Font.FontQuality <> AFontQuality then
  begin
    FElementText.Font.FontQuality := AFontQuality;
  end;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementBackgroundColor(AColor: TColor);
begin
  if FItemBackgroundColor <> AColor then
  begin
    FItemBackgroundColor := AColor;
  end;
end;

procedure TiCustomTreeView.TiTreeItem.SetData(AData: Pointer);
begin
  if FItemData <> AData then
  begin
    FItemData := AData;
  end;
end;

procedure TiCustomTreeView.TiTreeItem.SetDrawElementOffset(AOffset: Integer);
begin
  if FItemDrawOffset <> AOffset then
  begin
    FItemDrawOffset := AOffset;
  end;
end;

procedure TiCustomTreeView.TiTreeItem.SetCollapsed(ACollapsed: Boolean);
begin
  if FItemCollapsed <> ACollapsed then
  begin
    FItemCollapsed := ACollapsed;
  end;
end;

constructor TiCustomTreeView.TiTreeItem.Create(ALabelTitle, AText: string;
  AColor: TColor);
begin
  FElementParent := nil;
  FElementChildrens := TiTreeItemList.Create(True);
  FElementLabel.Text := ALabelTitle;
  FElementText.Text := AText;
  FElementLabel.BackgroundColor := AColor;
  FItemBackgroundColor := clWhite;
  FItemCollapsed := False;
  FItemData := nil;
  FItemDrawOffset := 0;
  with FElementLabel.Font do
  begin
    Font := TFont.Create;
    Font.Color := clBlack;
    Font.Style := [fsBold];
    FontColorOpacity := 255;
    FontQuality := fqSystem;
    FontOrientation := 0;
    FontAntialias := True;
  end;
  with FElementText.Font do
  begin
    Font := TFont.Create;
    Font.Color := clBlack;
    Font.Style := [];
    FontColorOpacity := 255;
    FontQuality := fqSystem;
    FontOrientation := 0;
    FontAntialias := True;
  end;
end;

destructor TiCustomTreeView.TiTreeItem.Destroy;
begin
  FreeAndNil(FElementChildrens);
  FreeAndNil(FElementLabel.Font.Font);
  FreeAndNil(FElementText.Font.Font);
  inherited Destroy;
end;

end.
