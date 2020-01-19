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
      TIntegerTreeViewItem = specialize TiTreeViewItem<Integer>;
  private
    FElement : TIntegerTreeViewItem.TTreeItemElement;
  published
    procedure TestCreate;
    procedure TestElementAdd;
    procedure TestElementChildrens;
  end;

implementation

{ TiTreeViewItemPODTestCase }

procedure TiTreeViewItemPODTestCase.TestCreate;
begin
  FElement := TIntegerTreeViewItem.TTreeItemElement.Create(25);

  AssertTrue('Error TiTreeViewItem is nil', FElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct', FElement.Value = 25);
  AssertFalse('Error TiTreeViewItem have more one value in list',
    FElement.HasNext);
end;

procedure TiTreeViewItemPODTestCase.TestElementAdd;
begin
  FElement := TIntegerTreeViewItem.TTreeItemElement.Create(25);
  FElement.AddChildren(1);

  AssertTrue('Error TiTreeViewItem is nil', FElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct', FElement.Value = 25);
  AssertFalse('Error TiTreeViewItem have more one value in list',
    FElement.HasNext);
  AssertTrue('Error TiTreeViewItem have childrens element',
    FElement.HasChildrens);
  AssertTrue('Error TiTreeViewItem not correct children element value',
    FElement.Children.Value = 1);
end;

procedure TiTreeViewItemPODTestCase.TestElementChildrens;
begin
  FElement := TIntegerTreeViewItem.TTreeItemElement.Create(2);
  FElement.AddChildren(5);
  FElement.AddChildren(-43);
  FElement.AddChildren(190);

  AssertTrue('Error TiTreeViewItem is nil', FElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct', FElement.Value = 2);
  AssertFalse('Error TiTreeViewItem have more one value in list',
    FElement.HasNext);
  AssertTrue('Error TitTreeViewItem children element 0 value is not correct',
    FElement.Children.Value = 5);
  AssertTrue('Error TitTreeViewItem children element 1 value is not correct',
    FElement.Children.Next.Value = -43);
  AssertTrue('Error TitTreeViewItem children element 2 value is not correct',
    FElement.Children.Next.Next.Value = 190);
end;



initialization
  RegisterTest(TiTreeViewItemPODTestCase);
end.

