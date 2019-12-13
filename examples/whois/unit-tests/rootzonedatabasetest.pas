unit RootZoneDatabaseTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, RootZoneDatabase;

type

  { TInfoElementTestCase }

  TInfoElementTestCase = class(TTestCase)
  private
    FElement : TRootZoneDatabase.TInfoElement;
  published
    procedure TestInfoName;
    procedure TestInfoEntity;
    procedure TestInfoIconPath;
    procedure TestInfoNote;
    procedure TestInfoDNSName;
    procedure TestInfoManager;
    procedure TestInfoType;
    procedure TestInfoLanguage;
  end;

  { TDomainZoneTestCase }

  TDomainZoneTestCase = class(TTestCase)
  private
    FZone : TRootZoneDatabase.TDomainZone;
  published
    procedure TestNameProperty;
    procedure TestDomainZoneNameAndEntity;
    procedure TestDomainZoneElements;
  end;

  { TRootZoneDatabaseTestCase }

  TRootZoneDatabaseTestCase = class(TTestCase)
  private
    FDatabase : TRootZoneDatabase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDatabaseCount;
  end;

implementation

{ TDomainZoneTestCase }
{ Test domain zone name property value }
procedure TDomainZoneTestCase.TestNameProperty;
var
  Element : TRootZoneDatabase.TInfoElement;
begin
  Element := TRootZoneDatabase.TInfoElement.Create(INFO_NAME, 'Name');
  FZone := TRootZoneDatabase.TDomainZone.Create;
  FZone.AddInfo(Element);
  AssertTrue('Error domain zone name is not correct', FZone.Name = 'Name');

  FreeAndNil(FZone);
end;

{ Test domain zone name and entity properties }
procedure TDomainZoneTestCase.TestDomainZoneNameAndEntity;
var
  Element : TRootZoneDatabase.TInfoElement;
  Counter : Integer;
begin
  Counter := 0;
  FZone := TRootZoneDatabase.TDomainZone.Create;

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_NAME, 'Name');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_ENTITY, 'Entity');
  FZone.AddInfo(Element);

  AssertTrue('Error domain zone name is not correct', FZone.Name = 'Name');

  for Element in FZone do
  begin
    AssertTrue('Error domain zone entity element is not correct',
      Element.Value = 'Entity');
    Inc(Counter);
  end;

  AssertTrue('Error not all domain zone elements added in list', Counter = 1);
end;

{ Test domain zone elements }
procedure TDomainZoneTestCase.TestDomainZoneElements;
var
  Element : TRootZoneDatabase.TInfoElement;
  Counter : Integer;
begin
  Counter := 0;
  FZone := TRootZoneDatabase.TDomainZone.Create;

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_NAME, 'Name');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_ENTITY, 'Entity');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_ICONPATH, 'IconPath');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_NOTE, 'Note');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_DNSNAME, 'DNSName');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_MANAGER, 'Manager');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_TYPE, 'Type');
  FZone.AddInfo(Element);

  Element := TRootZoneDatabase.TInfoElement.Create(INFO_LANGUAGE, 'Language');
  FZone.AddInfo(Element);

  AssertTrue('Error domain zone name is not correct', FZone.Name = 'Name');

  for Element in FZone do
    case Element.InfoType of
      INFO_ENTITY : begin
        Inc(Counter);
        AssertTrue('Error domain zone entity element is not correct',
          Element.Value = 'Entity');
      end;
      INFO_ICONPATH : begin
        Inc(Counter);
        AssertTrue('Error domain zone icon path element is not correct',
          Element.Value = 'IconPath');
      end;
      INFO_NOTE : begin
        Inc(Counter);
        AssertTrue('Error domain zone note element is not correct',
          Element.Value = 'Note');
      end;
      INFO_DNSNAME : begin
        Inc(Counter);
        AssertTrue('Error domain zone DNS name element is not correct',
          Element.Value = 'DNSName');
      end;
      INFO_MANAGER : begin
        Inc(Counter);
        AssertTrue('Error domain zone manager element is not correct',
          Element.Value = 'Manager');
      end;
      INFO_TYPE : begin
        Inc(Counter);
        AssertTrue('Error domain zone type element is not correct',
          Element.Value = 'Type');
      end;
      INFO_LANGUAGE : begin
        Inc(Counter);
        AssertTrue('Error domain zone language element is not correct',
          Element.Value = 'Language');
      end
    else
    begin
      Fail('Error domain zone unknown element type');
    end;
  end;

  AssertTrue('Error not all domain zone elements added in list', Counter = 7);
end;

{ TInfoElementTestCase }
{ Test info element name type }
procedure TInfoElementTestCase.TestInfoName;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_NAME, 'Name');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_NAME);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Name');

  FreeAndNil(FElement);
end;

{ Test info element entity type }
procedure TInfoElementTestCase.TestInfoEntity;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_ENTITY, 'Entity');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_ENTITY);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Entity');

  FreeAndNil(FElement);
end;

{ Test info element icon path type }
procedure TInfoElementTestCase.TestInfoIconPath;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_ICONPATH, 'IconPath');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_ICONPATH);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'IconPath');

  FreeAndNil(FElement);
end;

{ Test info element note type }
procedure TInfoElementTestCase.TestInfoNote;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_NOTE, 'Note');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_NOTE);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Note');

  FreeAndNil(FElement);
end;

{ Test info element DNS name type }
procedure TInfoElementTestCase.TestInfoDNSName;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_DNSNAME, 'DNSName');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_DNSNAME);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'DNSName');

  FreeAndNil(FElement);
end;

{ Test info element manager type }
procedure TInfoElementTestCase.TestInfoManager;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_MANAGER, 'Manager');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_MANAGER);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Manager');

  FreeAndNil(FElement);
end;

{ Test info element infotype type }
procedure TInfoElementTestCase.TestInfoType;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_TYPE, 'Type');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_TYPE);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Type');

  FreeAndNil(FElement);
end;

{ Test info element language type }
procedure TInfoElementTestCase.TestInfoLanguage;
begin
  FElement := TRootZoneDatabase.TInfoElement.Create(INFO_LANGUAGE, 'Language');
  AssertTrue('Error info element type is not correct', FElement.InfoType =
    INFO_LANGUAGE);
  AssertTrue('Error info element value is not correct', FElement.Value =
    'Language');

  FreeAndNil(FElement);
end;

{ TRootZoneDatabaseTestCase }

procedure TRootZoneDatabaseTestCase.SetUp;
begin
  FDatabase := TRootZoneDatabase.Create;
end;

procedure TRootZoneDatabaseTestCase.TearDown;
begin
  FreeAndNil(FDatabase);
end;

procedure TRootZoneDatabaseTestCase.TestDatabaseCount;
var
  Zone : TRootZoneDatabase.TDomainZone;
begin
  Zone := TRootZoneDatabase.TDomainZone.Create;
  Zone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NAME, '.test'));
  Zone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_TYPE, 'Test zone'));
  Zone.AddInfo(TRootZoneDatabase.TInfoElement.Create(INFO_NOTE, 'Test zone' +
    'notes'));
  FDatabase.AddDomain(Zone);

  AssertTrue('Error database element count', FDatabase.Count = 1);
end;

initialization
  RegisterTest(TInfoElementTestCase);
  RegisterTest(TDomainZoneTestCase);
  RegisterTest(TRootZoneDatabaseTestCase);
end.

