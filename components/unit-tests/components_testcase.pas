unit components_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, CustomTreeView;

type

  { TiTreeViewItemPODTestCase }

  TiTreeViewItemPODTestCase = class(TTestCase)
  public
    type
      generic TestTreeViewItem<TItemData> =
        class(specialize TiTreeViewItem<TItemData>)
      public
        property IsRoot;
        property HasChildrens;
        property Parent;
        property Childrens;
        property ItemData;
        property RendererData;
        property CustomData;
        property Collapsed;
      end;

      TreeViewItemInteger = specialize TestTreeViewItem<Integer>;
  private
    TreeViewItem : TreeViewItemInteger;
  published
    procedure TestCreate;
    procedure TestItemValue;
    procedure TestIsRoot;
    procedure TestHasChildrens;
    procedure TestAddChildrens;
  end;

implementation

{ TiTreeViewItemPODTestCase }

procedure TiTreeViewItemPODTestCase.TestCreate;
begin
  TreeViewItem := TreeViewItemInteger.Create(25);

  AssertTrue('Error TiTreeViewItem is nil', TreeViewItem <> nil);
end;

procedure TiTreeViewItemPODTestCase.TestItemValue;
var
  Value : Integer;
begin
  TreeViewItem := TreeViewItemInteger.Create(12);
  Value := TreeViewItem.ItemData;

  AssertTrue('Error TiTreeViewItem is nil', TreeViewItem <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct', Value = 12);
end;

procedure TiTreeViewItemPODTestCase.TestIsRoot;
begin
  TreeViewItem := TreeViewItemInteger.Create(74);

  AssertTrue('Error TiTreeViewItem is nil', TreeViewItem <> nil);
  AssertTrue('Error TiTreeViewItem by default is root item',
    TreeViewItem.IsRoot = True);
end;

procedure TiTreeViewItemPODTestCase.TestHasChildrens;
begin
  TreeViewItem := TreeViewItemInteger.Create(0);

  AssertTrue('Error TiTreeViewItem is nil', TreeViewItem <> nil);
  AssertTrue('Error TiTreeViewItem by default has no childrens',
    TreeViewItem.HasChildrens = False);
end;

procedure TiTreeViewItemPODTestCase.TestAddChildrens;
var
  Value : Integer;
begin
  TreeViewItem := TreeViewItemInteger.Create(5);

  AssertTrue('Error TiTreeViewItem is nil', TreeViewItem <> nil);
  AssertTrue('Error TiTreeViewItem by default has no childrens',
    TreeViewItem.HasChildrens = False);

  TreeViewItem.AddChildren(TreeViewItemInteger.Create(17));
  AssertTrue('Error TiTreeViewItem haven''t childrens',
    TreeViewItem.HasChildrens = True);

  Value := TreeViewItem.Childrens[0].ItemData;
  AssertTrue('Error children item value is not correct', Value = 17);
end;

initialization
  RegisterTest(TiTreeViewItemPODTestCase);
end.

