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
  { Stored paddings inner gap size data }
  TPadding = record
    Top, Right, Bottom, Left : Integer;
  end;

  { TMargin }
  { Stored margin outer gap size data }
  TMargin = record
    Top, Right, Bottom, Left : Integer;
  end;

  { TFontProperty }
  TFontProperty = record
    FontName : string;
    FontStyle : TFontStyles;
    FontSize : Cardinal;
    FontColor : TBGRAPixel;
    FontQuality : TBGRAFontQuality;
    FontAntialias : Boolean;
    FontOrientation : Integer;
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

      { TiCustomTreeView options }
      TOption = (
        opClickSelection,          { left mouse click is selected item }
        opClickClearEmptySelection, { clear selection if selected item is empty }
        opShowCollapseButton,       { show collapse button }
        opFontAntialias             { use font antialias }
      );
      TOptions = set of TOption;

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
        { Current element collapsed state }
        FElementCollapsed : Boolean;
        { Item additional user controlled data pointer }
        FElementData : Pointer;
        { Current element start draw offset }
        FElementDrawOffset : Integer;

        { Check if current element is root }
        function IsRootElement : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
        { Check if current element has childrens}
        function HasElementChildrens : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
        { Return element label text value }
        function GetElementLabelText : string; {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element label title text }
        procedure SetElementLabelText (AText : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Return current element label background color }
        function GetElementLabelBackgroundColor : TBGRAPixel; {$IFNDEF DEBUG}
          inline;{$ENDIF}
        { Set element label background color }
        procedure SetElementLabelBackgroundColor (AColor : TBGRAPixel); {$IFNDEF
          DEBUG}inline;{$ENDIF}
        { Return element label font }
        function GetElementLabelFont : TFontProperty; {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Set element label font parameters }
        procedure SetElementLabelFont (AFont : TFontProperty); {$IFNDEF DEBUG}
          inline;{$ENDIF}
        { Return element text value }
        function GetElementText : string; {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element text }
        procedure SetElementText (AText : string); {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Return current element text font }
        function GetElementTextFont : TFontProperty; {$IFNDEF DEBUG}inline;
          {$ENDIF}
        { Set current element font value }
        procedure SetElementTextFont (AFont : TFontProperty); {$IFNDEF DEBUG}
          inline;{$ENDIF}
        { Set current element user data pointer }
        procedure SetData (AData : Pointer); {$IFNDEF DEBUG}inline;{$ENDIF}
        { Set current element start draw offset data }
        procedure SetDrawElementOffset (AOffset : Integer); {$IFNDEF DEBUG}
          inline;{$ENDIF}
        { Set current element collapse state }
        procedure SetCollapsed (ACollapsed : Boolean); {$IFNDEF DEBUG}inline;
          {$ENDIF}
      protected
        { Is current item element is root }
        property IsRoot : Boolean read IsRootElement;
        { Item element parent item if exists }
        property Parent : TiTreeItem read FElementParent;
        { Return TRUE if element has childrens }
        property HasChildrens : Boolean read HasElementChildrens;
        { Item element children's elements }
        property Childrens : TiTreeItemList read FElementChildrens;
        { Item element label title text }
        property LabelText : string read GetElementLabelText write
          SetElementLabelText;
        { Item element label background color }
        property LabelBackgroundColor : TBGRAPixel read
          GetElementLabelBackgroundColor write SetElementLabelBackgroundColor;
        { Item element label font options }
        property LabelFont : TFontProperty read GetElementLabelFont write
          SetElementLabelFont;
        { Item element text string }
        property Text : string read GetElementText write SetElementText;
        { Item element text font }
        property TextFont : TFontProperty read GetElementTextFont write
          SetElementTextFont;
        { Element collapse state }
        property Collapsed : Boolean read FElementCollapsed write
          SetCollapsed;
        { Element user pointer data }
        property Data : Pointer read FElementData write SetData;
        { Element start draw offset data }
        property DrawOffset : Integer read FElementDrawOffset
          write SetDrawElementOffset;
      public
        constructor Create (ALabelTitle, AText : string; AColor : TColor);
        destructor Destroy; override;
      end;
  private
    type
      { TiCustomTreeView selected element item }
      TSelectedElement = record
        Element : PiTreeItem;
        ElementLabelColor : TBGRAPixel;
        ElementLabelPrevColor : TBGRAPixel;
      end;

      TElementCollapseButton = record
        Margin : TMargin;
        DrawRoundRectRadius : Cardinal;
      end;

      TElementLabel = record
        Padding : TPadding;
        Margin : TMargin;
        DrawRoundRectRadius : Cardinal;
      end;

      TElementText = record
        Padding : TPadding;
        Margin : TMargin;
      end;
  private
    { Control canvas }
    FBitmapCanvas : TBGRABitmap;
    { All control item elements }
    FItems : TiTreeItemList;
    { Visible control item elements, exclude collapsed elements }
    FDrawItems : TiDrawTreeItemList;
    { Elements max text length }
    FItemMaxTextLength : Cardinal;
    { Draw elements max text length }
    FDrawItemMaxTextLength : Cardinal;
    { Control element heigth size }
    FItemHeight : Cardinal;
    { Control element collapse button }
    FCollapseButton : TElementCollapseButton;
    { Control item label properties }
    FItemLabel : TElementLabel;
    { Control item text properties }
    FItemText : TElementText;
    { Control root element draw offset }
    FRootItemDrawOffset : Integer;
    { Control element's draw level offset }
    FItemDrawOffset : Integer;
    { Control selected element }
    FSelectedItem : TSelectedElement;
    { Control's global options set }
    FOptions : TOptions;

    { Calculate label text width without gaps }
    function GetLabelTextWidth (AItem : TiTreeItem) : Cardinal; {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Calculate item text width without gaps }
    function GetTextWidth (AItem : TiTreeItem) : Cardinal; {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Return TRUE if AItem is drawable }
    function IsItemDrawable (AItem : TiTreeItem) : Boolean; {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Calculate and update item draw offset }
    procedure UpdateItemDrawOffset (AItem : TiTreeItem); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Calculate item draw width }
    procedure UpdateItemLineDrawWidth (AItem : TiTreeItem); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Find draw item for Y coordinate }
    function GetItem (AY : Integer) : TiTreeItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    { Calculate collapse button rect }
    function GetCollapseButtonRect (AItem : TiTreeItem) : TRect; {$IFNDEF DEBUG}
      inline;{$ENDIF}

    { Set control item height }
    procedure SetElementHeight (AHeight : Cardinal); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Set control label title padding sizes }
    procedure SetElementLabelPadding (APadding : TPadding); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control label title margin sizes }
    procedure SetElementLabelMargin (AMargin : TMargin); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Set control label title round rect radius size }
    procedure SetElementLabelRoundRect (ARound : Cardinal); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control item text padding }
    procedure SetElementTextPadding (APadding : TPadding); {$IFNDEF DEBUG}
      inline;{$ENDIF}
    { Set control item text margin }
    procedure SetElementTextMargin (AMargin : TMargin); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Set control item draw level offset }
    procedure SetElementDrawOffset (AOffset : Integer); {$IFNDEF DEBUG}inline;
      {$ENDIF}
    { Set control OnMouseUp callback }
    procedure ControlMouseUp (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    { Control's resize }
    procedure DoOnResize; override;
    { Repaint control }
    procedure RenderControl; virtual;
    { Calculate control }
    procedure CalculateControl; virtual;
    { Recalc scroll bars }
    procedure CalculateScrollRanges; virtual;
  public
    class function GetControlClassDefaultSize : TSize; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { Add element item }
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
    { Control draw bitmap canvas }
    property BitmapCanvas : TBGRABitmap read FBitmapCanvas;
    { Control elements }
    property Items : TiTreeItemList read FItems;
    { Element item height }
    property ItemHeight : Cardinal read FItemHeight write SetElementHeight
      default 17;
    { Element label padding }
    property LabelPadding : TPadding read FItemLabel.Padding write
      SetElementLabelPadding;
    { Element label margin }
    property LabelMargin : TMargin read FItemLabel.Margin write
      SetElementLabelMargin;
    { Element label round rect radius }
    property LabelRoundRect : Cardinal read FItemLabel.DrawRoundRectRadius
      write SetElementLabelRoundRect default 17;
    { Element text padding }
    property TextPadding : TPadding read FItemText.Padding write
      SetElementTextPadding;
    { Element text margin }
    property TextMargin : TMargin read FItemText.Margin write
      SetElementTextMargin;
    { Element draw level offset }
    property ItemDrawOffset : Integer read FItemDrawOffset write
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
  end;

operator= (ALeft, ARight : TPadding) : Boolean;
operator= (ALeft, ARight : TMargin) : Boolean;
operator= (ALeft, ARight : TFontProperty) : Boolean;

{ Return Pading struct with setup values }
function Padding (ATop, ARight, ABottom, ALeft : Integer) : TPadding; overload;
function Padding (ATopBottom, ARightLeft : Integer) : TPadding; overload;

{ Return Margin struct with setup values }
function Margin (ATop, ARight, ABottom, ALeft : Integer) : TMargin; overload;
function Margin (ATopBottom, ARightLeft : Integer) : TMargin; overload;

procedure Register;

implementation

operator=(ALeft, ARight: TPadding) : Boolean;
begin
  Result := (ALeft.Top = ARight.Top) and (ALeft.Right = ARight.Right) and
    (ALeft.Bottom = ARight.Bottom) and (ALeft.Left = ARight.Left);
end;

operator=(ALeft, ARight: TMargin): Boolean;
begin
  Result := (ALeft.Top = ARight.Top) and (ALeft.Right = ARight.Right) and
    (ALeft.Bottom = ARight.Bottom) and (ALeft.Left = ARight.Left);
end;

operator=(ALeft, ARight: TFontProperty): Boolean;
begin
  Result := (ALeft.FontName = ARight.FontName) and (ALeft.FontStyle =
    ARight.FontStyle) and (ALeft.FontSize = ARight.FontSize) and
    (ALeft.FontColor = ARight.FontColor) and (ALeft.FontQuality =
    ARight.FontQuality) and (ALeft.FontAntialias = ARight.FontAntialias) and
    (ALeft.FontOrientation = ARight.FontOrientation);
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

function Margin(ATop, ARight, ABottom, ALeft: Integer): TMargin;
begin
  with Result do
  begin
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
    Left := ALeft;
  end;
end;

function Margin(ATopBottom, ARightLeft: Integer): TMargin;
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

function TiCustomTreeView.GetLabelTextWidth(AItem: TiTreeItem): Cardinal;
begin
  FBitmapCanvas.FontHeight := FItemHeight - FItemLabel.Margin.Top -
    FItemLabel.Padding.Top - FItemLabel.Padding.Bottom -
    FItemLabel.Margin.Bottom;
  FBitmapCanvas.FontStyle := AItem.FElementLabel.Font.FontStyle;
  Result := FBitmapCanvas.TextSize(AItem.FElementLabel.Text).Width;
end;

function TiCustomTreeView.GetTextWidth(AItem: TiTreeItem): Cardinal;
begin
  FBitmapCanvas.FontHeight := FItemHeight - FItemLabel.Margin.Top -
    FItemText.Padding.Top - FItemText.Padding.Bottom -
    FItemText.Margin.Bottom;
  FBitmapCanvas.FontStyle := AItem.FElementText.Font.FontStyle;
  Result := FBitmapCanvas.TextSize(AItem.FElementText.Text).Width;
end;

function TiCustomTreeView.IsItemDrawable(AItem: TiTreeItem): Boolean;
begin
  if AItem.IsRoot then
    Result := True
  else
    Result := (not AItem.Parent.FElementCollapsed) and
      (IsItemDrawable(AItem.Parent));
end;

procedure TiCustomTreeView.SetElementLabelMargin(AMargin: TMargin);
begin
  if AMargin <> FItemLabel.Margin then
    FItemLabel.Margin := AMargin;
end;

procedure TiCustomTreeView.UpdateItemDrawOffset(AItem: TiTreeItem);
begin
  if AItem.IsRoot then
    AItem.FElementDrawOffset := FRootItemDrawOffset
  else
    AItem.FElementDrawOffset := AItem.Parent.FElementDrawOffset +
      FItemDrawOffset;
end;

procedure TiCustomTreeView.UpdateItemLineDrawWidth(AItem: TiTreeItem);
var
  ItemWidth : Cardinal;
begin
  ItemWidth := AItem.FElementDrawOffset + FItemLabel.Margin.Left +
    FItemLabel.Padding.Left + GetLabelTextWidth(AItem) +
    FItemLabel.Padding.Right + FItemLabel.Margin.Right +
    FItemText.Margin.Left + FItemText.Padding.Left + GetTextWidth(AItem) +
    FItemText.Padding.Right + FItemText.Margin.Right;
  FItemMaxTextLength := Max(FItemMaxTextLength, ItemWidth);

  if IsItemDrawable(AItem) then
    FDrawItemMaxTextLength := Max(FDrawItemMaxTextLength, ItemWidth);
end;

function TiCustomTreeView.GetItem(AY: Integer): TiTreeItem;
var
  ItemIndex : Integer;
begin
  ItemIndex := AY div FItemHeight;
  if FDrawItems.Count > ItemIndex then
  begin
    Result := TiTreeItem(FDrawItems[ItemIndex]);
  end else
    Result := nil;
end;

function TiCustomTreeView.GetCollapseButtonRect(AItem: TiTreeItem): TRect;
begin
  Result := Rect(
    { Left }
    AItem.DrawOffset - FCollapseButton.Margin.Right -
    { The collapse button must be a square, so for width size we can use it's
      height size, because it is more easy for calculation }
    (FItemHeight - FCollapseButton.Margin.Top -
    FCollapseButton.Margin.Bottom),
    { Top }
    FCollapseButton.Margin.Top,
    { Right }
    AItem.DrawOffset - FCollapseButton.Margin.Right,
    { Bottom }
    FItemHeight - FCollapseButton.Margin.Bottom
  );
end;

procedure TiCustomTreeView.SetElementHeight(AHeight: Cardinal);
begin
  if FItemHeight <> AHeight then
  begin
    FItemHeight := AHeight;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.SetElementLabelPadding(APadding: TPadding);
begin
  if FItemLabel.Padding <> APadding then
  begin
    FItemLabel.Padding := APadding;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.SetElementTextMargin(AMargin: TMargin);
begin
  if FItemText.Margin <> AMargin then
  begin
    FItemText.Margin := AMargin;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.SetElementLabelRoundRect(ARound: Cardinal);
begin
  if FItemLabel.DrawRoundRectRadius <> ARound then
  begin
    FItemLabel.DrawRoundRectRadius := ARound;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.SetElementTextPadding(APadding: TPadding);
begin
  if FItemText.Padding <> APadding then
  begin
    FItemText.Padding := APadding;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.SetElementDrawOffset(AOffset: Integer);
begin
  if FItemDrawOffset <> AOffset then
  begin
    FItemDrawOffset := AOffset;
    RenderControl;
    Invalidate;
  end;
end;

procedure TiCustomTreeView.ControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function IsCollapseButtonClicked (AItem : TiTreeItem; AX, AY : Integer) :
    Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    CollapseButtonRect : TRect;
  begin
    CollapseButtonRect := GetCollapseButtonRect(AItem);

    Result := (AX >= CollapseButtonRect.Left) and
      (AX <= CollapseButtonRect.Right) and (AY >= CollapseButtonRect.Top) and
      (AY <= CollapseButtonRect.Bottom);
  end;

var
  Item : TiTreeItem;
begin
  with Sender as TiCustomTreeView do
  begin
    if (Button = mbLeft) and (opShowCollapseButton in FOptions) then
    begin
      Item := GetItem(Y);
      if (Item <> nil) then
      begin
        if (IsCollapseButtonClicked(Item, X + HorzScrollBar.Position,
          Y + VertScrollBar.Position - ((Y div FItemHeight) *
          FItemHeight))) then
        begin
          Item.Collapsed := not Item.Collapsed;
        end;

        if opClickSelection in FOptions then
        begin
          if FSelectedItem.Element <> nil then
            TiTreeItem(FSelectedItem.Element).FElementLabel.BackgroundColor:=
              FSelectedItem.ElementLabelPrevColor;

          FSelectedItem.Element := Pointer(Item);
          FSelectedItem.ElementLabelPrevColor :=
            Item.FElementLabel.BackgroundColor;
          Item.FElementLabel.BackgroundColor :=
            FSelectedItem.ElementLabelColor;
        end;
      end else
      begin
        if opClickClearEmptySelection in FOptions then
        begin
          if FSelectedItem.Element <> nil then
            TiTreeItem(FSelectedItem.Element).FElementLabel.BackgroundColor:=
              FSelectedItem.ElementLabelPrevColor;
          FSelectedItem.Element := nil;
        end;
      end;

      RenderControl;
      Invalidate;
    end;
  end;
end;

class function TiCustomTreeView.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

procedure TiCustomTreeView.DoOnResize;
begin
  RenderControl;
  Invalidate;
end;

procedure TiCustomTreeView.RenderControl;

  procedure DrawItem (ARect : TRect; AElement : TiTreeItem); {$IFNDEF DEBUG}
    inline;{$ENDIF}
  var
    LabelTextSize : Cardinal;
    CollapseButtonRect : TRect;
  begin
    FBitmapCanvas.FontHeight := FItemHeight - FItemLabel.Margin.Top -
      FItemLabel.Padding.Top - FItemLabel.Padding.Bottom -
      FItemLabel.Margin.Bottom;

    { Draw collapse button }
    if AElement.HasChildrens and (opShowCollapseButton in FOptions) then
    begin
      CollapseButtonRect := GetCollapseButtonRect(AElement);
      FBitmapCanvas.RoundRect(ARect.Left + CollapseButtonRect.Left,
        ARect.Top + CollapseButtonRect.Top, ARect.Left +
        CollapseButtonRect.Right, ARect.Top + CollapseButtonRect.Bottom,
        FCollapseButton.DrawRoundRectRadius,
        FCollapseButton.DrawRoundRectRadius,
        ColorToBGRA(clLtGray), BGRAPixelTransparent);

      if AElement.Collapsed then
      begin
        { Draw + symbol }
        FBitmapCanvas.DrawLine(ARect.Left + CollapseButtonRect.Left + 3,
          ARect.Top + CollapseButtonRect.Top + CollapseButtonRect.Height div 2,
          ARect.Left + CollapseButtonRect.Right - 4, ARect.Top +
          CollapseButtonRect.Top + CollapseButtonRect.Height div 2, BGRABlack,
          True);
        FBitmapCanvas.DrawLine(ARect.Left + CollapseButtonRect.Left +
          CollapseButtonRect.Width div 2, ARect.Top +
          CollapseButtonRect.Top + 3, ARect.Left + CollapseButtonRect.Left +
          CollapseButtonRect.Width div 2, ARect.Top +
          CollapseButtonRect.Bottom - 4, BGRABlack, True);
      end else
      begin
        { Draw - symbol }
        FBitmapCanvas.DrawLine(ARect.Left + CollapseButtonRect.Left + 3,
          ARect.Top + CollapseButtonRect.Top + CollapseButtonRect.Height div 2,
          ARect.Left + CollapseButtonRect.Right - 4, ARect.Top +
          CollapseButtonRect.Top + CollapseButtonRect.Height div 2, BGRABlack,
          True);
      end;
    end;

    { Draw label }
    FBitmapCanvas.FontStyle := AElement.FElementLabel.Font.FontStyle;
    LabelTextSize := FBitmapCanvas.TextSize(AElement.LabelText).Width;

    FBitmapCanvas.FillRoundRect(ARect.Left + AElement.DrawOffset +
      FItemLabel.Margin.Left, ARect.Top + FItemLabel.Margin.Top,
      AElement.DrawOffset + FItemLabel.Margin.Left +
      FItemLabel.Padding.Left + LabelTextSize + FItemLabel.Padding.Right +
      FItemLabel.Margin.Right, ARect.Bottom - FItemLabel.Margin.Bottom,
      FItemLabel.DrawRoundRectRadius, FItemLabel.DrawRoundRectRadius,
      AElement.LabelBackgroundColor);
    FBitmapCanvas.TextOut(ARect.Left + AElement.DrawOffset +
      FItemLabel.Margin.Left + FItemLabel.Padding.Left,
      ARect.Top + FItemLabel.Margin.Top + FItemLabel.Padding.Top,
      AElement.LabelText, AElement.LabelFont.FontColor);

    { Draw text }
    FBitmapCanvas.FontStyle := AElement.FElementText.Font.FontStyle;
    FBitmapCanvas.TextOut(ARect.Left + AElement.DrawOffset +
      FItemLabel.Margin.Left + FItemLabel.Padding.Left +
      LabelTextSize + FItemLabel.Padding.Right + FItemLabel.Margin.Right +
      FItemText.Margin.Left + FItemText.Padding.Left,
      ARect.Top + FItemText.Margin.Top + FItemText.Padding.Top,
      AElement.Text, AElement.TextFont.FontColor);
  end;

var
  Index : Integer;
begin
  CalculateControl;

  FBitmapCanvas.SetSize(Max(FDrawItemMaxTextLength, ClientWidth),
    Max(FDrawItems.Count * FItemHeight, ClientHeight));
  FBitmapCanvas.Fill(BGRAWhite);
  FBitmapCanvas.FontAntialias := opFontAntialias in FOptions;

  CalculateScrollRanges;

  for Index := 0 to FDrawItems.Count - 1 do
  begin
    DrawItem(TRect.Create(0, Index * FItemHeight, ClientWidth, Index *
      FItemHeight + FItemHeight), TiTreeItem(FDrawItems[Index]));
  end;
end;

procedure TiCustomTreeView.CalculateControl;

  procedure CalcElement (AItem : TiTreeItem); {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    Item : TiTreeItem;
  begin
    UpdateItemDrawOffset(AItem);
    UpdateItemLineDrawWidth(AItem);

    if IsItemDrawable(AItem) then
      FDrawItems.Add(Pointer(AItem));

    for Item in AItem.Childrens do
    begin
      CalcElement(Item);
    end;
  end;

var
  Item : TiTreeItem;
begin
  FItemMaxTextLength := 0;
  FDrawItemMaxTextLength := 0;
  FDrawItems.Clear;

  for Item in FItems do
  begin
    CalcElement(Item);
  end;
end;

procedure TiCustomTreeView.CalculateScrollRanges;
begin
  if FBitmapCanvas.Height > ClientHeight then
    VertScrollBar.Range := FBitmapCanvas.Height
  else
    VertScrollBar.Range := 0;

  if FBitmapCanvas.Width > ClientWidth then
    HorzScrollBar.Range := FBitmapCanvas.Width
  else
    HorzScrollBar.Range := 0;
end;

constructor TiCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  FOptions := [opClickSelection, opShowCollapseButton, opFontAntialias];
  FBitmapCanvas := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  FBitmapCanvas.FontAntialias := opFontAntialias in FOptions;
  FItems := TiTreeItemList.Create(True);
  FDrawItems := TiDrawTreeItemList.Create;
  FItemMaxTextLength := 0;
  FDrawItemMaxTextLength := 0;
  FItemHeight := 17;
  FCollapseButton.Margin := Margin(3, 4, 3, 5);
  FCollapseButton.DrawRoundRectRadius := 6;
  FItemLabel.Padding := Padding(1, 10, 2, 10);
  FItemLabel.Margin := Margin(0, 0, 1, 0);
  FItemLabel.DrawRoundRectRadius := 17;
  FItemText.Padding := Padding(1, 5);
  FItemText.Margin := Margin(0, 0);
  FRootItemDrawOffset := 20;
  FItemDrawOffset := 12;
  FSelectedItem.Element := nil;
  FSelectedItem.ElementLabelColor := ColorToBGRA(clYellow);
  OnMouseUp := @ControlMouseUp;
end;

destructor TiCustomTreeView.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FBitmapCanvas);
  inherited Destroy;
end;

procedure TiCustomTreeView.Paint;
begin
  FBitmapCanvas.Draw(Canvas, 0, 0);
  inherited Paint;
end;

function TiCustomTreeView.AddItem(AItem: TiTreeItem): TiTreeItem;
begin
  FItems.Add(AItem);
  Result := FItems[FItems.Count - 1];
  RenderControl;
end;

function TiCustomTreeView.AddItem(AParent: TiTreeItem; AItem: TiTreeItem
  ): TiTreeItem;
begin
  if AParent <> nil then
  begin
    AParent.FElementChildrens.Add(AItem);
    Result := AParent.FElementChildrens[AParent.FElementChildrens.Count - 1];
    Result.FElementParent := AParent;
    RenderControl;
  end;
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

function TiCustomTreeView.TiTreeItem.HasElementChildrens: Boolean;
begin
  Result := FElementChildrens.Count > 0;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelText: string;
begin
  Result := FElementLabel.Text;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelText(AText: string);
begin
  if FElementLabel.Text <> AText then
    FElementLabel.Text := AText;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelBackgroundColor: TBGRAPixel;
begin
  Result := FElementLabel.BackgroundColor;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelBackgroundColor(
  AColor: TBGRAPixel);
begin
  if FElementLabel.BackgroundColor <> AColor then
    FELementLabel.BackgroundColor := AColor;
end;

function TiCustomTreeView.TiTreeItem.GetElementLabelFont: TFontProperty;
begin
  Result := FElementLabel.Font;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementLabelFont(AFont: TFontProperty);
begin
  if FElementLabel.Font <> AFont then
    FElementLabel.Font := AFont;
end;

function TiCustomTreeView.TiTreeItem.GetElementText: string;
begin
  Result := FElementText.Text;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementText(AText: string);
begin
  if FElementText.Text <> AText then
    FElementText.Text := AText;
end;

function TiCustomTreeView.TiTreeItem.GetElementTextFont: TFontProperty;
begin
  Result := FElementText.Font;
end;

procedure TiCustomTreeView.TiTreeItem.SetElementTextFont(AFont: TFontProperty);
begin
  if FElementText.Font <> AFont then
    FElementText.Font := AFont;
end;

procedure TiCustomTreeView.TiTreeItem.SetData(AData: Pointer);
begin
  if FElementData <> AData then
    FElementData := AData;
end;

procedure TiCustomTreeView.TiTreeItem.SetDrawElementOffset(AOffset: Integer);
begin
  if FElementDrawOffset <> AOffset then
    FElementDrawOffset := AOffset;
end;

procedure TiCustomTreeView.TiTreeItem.SetCollapsed(ACollapsed: Boolean);
begin
  if FElementCollapsed <> ACollapsed then
    FElementCollapsed := ACollapsed;
end;

constructor TiCustomTreeView.TiTreeItem.Create(ALabelTitle, AText: string;
  AColor: TColor);
begin
  FElementParent := nil;
  FElementChildrens := TiTreeItemList.Create(True);
  FElementLabel.Text := ALabelTitle;
  FElementText.Text := AText;
  FElementLabel.BackgroundColor := AColor;
  FElementCollapsed := False;
  FElementData := nil;
  FElementDrawOffset := 0;
  with FElementLabel.Font do
  begin
    FontName := 'Default';
    FontStyle := [fsBold];
    FontColor := BGRABlack;
    FontQuality := fqSystem;
    FontOrientation := 0;
  end;
  with FElementText.Font do
  begin
    FontName := 'Default';
    FontStyle := [];
    FontColor := BGRABlack;
    FontQuality := fqSystem;
    FontOrientation := 0;
  end;
end;

destructor TiCustomTreeView.TiTreeItem.Destroy;
begin
  FreeAndNil(FElementChildrens);
  inherited Destroy;
end;

end.
