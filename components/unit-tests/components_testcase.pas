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
    FItemElement : TIntegerTreeViewItem;
  published
    procedure TestCreate;
    procedure TestElementAdd;
    procedure TestElementChildrens;
  end;

implementation

{ TiTreeViewItemPODTestCase }

procedure TiTreeViewItemPODTestCase.TestCreate;
begin
  FItemElement := TIntegerTreeViewItem.Create(25);

  AssertTrue('Error TiTreeViewItem is nil', FItemElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct',
    FItemElement.Value = 25);
  AssertTrue('Error TiTreeViewItem have more one value in list',
    FItemElement.NextElement = nil);
end;

procedure TiTreeViewItemPODTestCase.TestElementAdd;
var
  Child : TIntegerTreeViewItem;
begin
  FItemElement := TIntegerTreeViewItem.Create(25);
  FItemElement.AddChildren(1);

  AssertTrue('Error TiTreeViewItem is nil', FItemElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct',
    FItemElement.Value = 25);
  AssertTrue('Error TiTreeViewItem have more one value in list',
    FItemElement.NextElement = nil);
  AssertTrue('Error TiTreeViewItem have childrens element',
    FItemElement.HasChildrens);

  Child := FItemElement.ChildrenElement^;

  AssertTrue('Error TiTreeViewItem not correct children element value',
    Child.Value = 1);
end;

procedure TiTreeViewItemPODTestCase.TestElementChildrens;
var
  Child : TIntegerTreeViewItem;
begin
  FItemElement := TIntegerTreeViewItem.Create(2);
  FItemElement.AddChildren(5);
  FItemElement.AddChildren(-43);
  FItemElement.AddChildren(190);

  AssertTrue('Error TiTreeViewItem is nil', FItemElement <> nil);
  AssertTrue('Error TiTreeViewItem value is not correct',
    FItemElement.Value = 2);
  AssertTrue('Error TiTreeViewItem have more one value in list',
    FItemElement.NextElement = nil);

  Child := FItemElement.ChildrenElement^;

  AssertTrue('Error TitTreeViewItem children element 0 value is not correct',
    Child.Value = 5);

  Child := Child.NextElement^;

  AssertTrue('Error TitTreeViewItem children element 1 value is not correct',
    Child.Value = -43);

  Child := Child.NextElement^;

  AssertTrue('Error TitTreeViewItem children element 2 value is not correct',
    Child.Value = 190);
end;

initialization
  RegisterTest(TiTreeViewItemPODTestCase);
end.

