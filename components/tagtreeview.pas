

unit TagTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  Math, BGRABitmap, BGRABitmapTypes, fgl, pasmyhtml;

type

  TCustomTagTreeItem = class;
  TCustomTagTreeItemList = specialize TFPGObjectList<TCustomTagTreeItem>;

  { TCustomTagTreeItem }

  TCustomTagTreeItem = class
  private
    FNode : TParser.TTagNode;
    FChildrens : TCustomTagTreeItemList;
  protected
    procedure SetTagNode (ANode : TParser.TTagNode);
  public
    constructor Create (ANode : TParser.TTagNode);
    destructor Destroy; override;
  public
    function AddChildren (ANode : TParser.TTagNode) : TCustomTagTreeItem;
  public
    property TagNode : TParser.TTagNode read FNode write SetTagNode;
  end;

  { TCustomTagTreeView }

  TCustomTagTreeView = class(TScrollingWinControl)
  private
    FBitmap : TBGRABitmap;
    FItems : TCustomTagTreeItemList;
  protected
    function GetItem (AIndex : Integer) : TCustomTagTreeItem;
    procedure SetItem (AIndex : Integer; AItem : TCustomTagTreeItem);
  public
    constructor Create (AOwner : TComponent);
    destructor Destroy; override;
  public
    function AddItem (ANode : TParser.TTagNode) : TCustomTagTreeItem;
  public
    property Items[Index : Integer] : TCustomTagTreeItem read GetItem
      write SetItem;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('libPasMyHTML',[TCustomTagTreeView]);
end;

{ TCustomTagTreeView }

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

{ TCustomTagTreeItem }

procedure TCustomTagTreeItem.SetTagNode(ANode: TParser.TTagNode);
begin
  FNode := ANode;
end;

constructor TCustomTagTreeItem.Create(ANode: TParser.TTagNode);
begin
  FNode := ANode;
  FChildrens := TCustomTagTreeItemList.Create(True);
end;

destructor TCustomTagTreeItem.Destroy;
begin
  FreeAndNil(FChildrens);
  inherited Destroy;
end;

function TCustomTagTreeItem.AddChildren(ANode: TParser.TTagNode
  ): TCustomTagTreeItem;
begin
  Result := FChildrens.Items[FChildrens.Add(ANode)];
end;

end.
