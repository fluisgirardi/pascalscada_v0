{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do editor de projeto do TOPCUAServer: tags do form para a colecao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(TOPCUAServer design time editor tests: tags of the form into the collection.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ut.opcuaserveredt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, PLCTag, PLCTagNumber, PLCBlock, opcuaserver, opcuaserveredt, testsupport.fakedriver;

type

  { TTestOPCUAServerEditor }

  TTestOPCUAServerEditor = class(TTestCase)
  private
    FForm:TComponent;
    FServer:TOPCUAServer;
    FDriver:TFakeProtocolDriver;
    FA, FB, FC:TPLCTagNumber;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheCandidatesAreTheTagsNotInTheCollection;
    procedure AddedTagsGetAFolderPerDriver;
    procedure WithoutFoldersAndWritable;
    procedure ATagAlreadyThereIsNotAddedAgain;
  end;

implementation

procedure TTestOPCUAServerEditor.SetUp;
begin
  FForm := TComponent.Create(nil);
  FForm.Name := 'Form1';
  FDriver := TFakeProtocolDriver.Create(FForm);
  FDriver.Name := 'ModBus1';
  FServer := TOPCUAServer.Create(FForm);
  FServer.Name := 'OPCUAServer1';
  FA := TPLCTagNumber.Create(FForm);
  FA.Name := 'Speed';
  FA.ProtocolDriver := FDriver;
  FB := TPLCTagNumber.Create(FForm);
  FB.Name := 'Setpoint';
  FC := TPLCTagNumber.Create(FForm);
  FC.Name := 'Level';
  FC.ProtocolDriver := FDriver;
end;

procedure TTestOPCUAServerEditor.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TTestOPCUAServerEditor.TheCandidatesAreTheTagsNotInTheCollection;
var
  l:TFPList;
begin
  FServer.Items.Add.Tag := FB;
  l := OPCUAServerCandidateTags(FServer, FForm);
  try
    AssertEquals('two left', 2, l.Count);
    AssertTrue('in creation order', (l[0]=Pointer(FA)) and (l[1]=Pointer(FC)));
  finally
    l.Free;
  end;
end;

procedure TTestOPCUAServerEditor.AddedTagsGetAFolderPerDriver;
var
  l:TFPList;
begin
  l := OPCUAServerCandidateTags(FServer, FForm);
  try
    AssertEquals('three added', 3, OPCUAServerAddTags(FServer, l, true, false));
  finally
    l.Free;
  end;
  AssertEquals('Speed by its driver', 'ModBus1.Speed', FServer.Items.FindByTag(FA).NodeIdText);
  AssertEquals('a memory tag', 'Memory.Setpoint', FServer.Items.FindByTag(FB).NodeIdText);
  AssertFalse('read only by default', FServer.Items.FindByTag(FA).Writable);
end;

procedure TTestOPCUAServerEditor.WithoutFoldersAndWritable;
var
  l:TFPList;
begin
  l := OPCUAServerCandidateTags(FServer, FForm);
  try
    OPCUAServerAddTags(FServer, l, false, true);
  finally
    l.Free;
  end;
  AssertEquals('no folder', 'Speed', FServer.Items.FindByTag(FA).NodeIdText);
  AssertTrue('writable', FServer.Items.FindByTag(FC).Writable);
end;

procedure TTestOPCUAServerEditor.ATagAlreadyThereIsNotAddedAgain;
var
  l:TFPList;
begin
  l := TFPList.Create;
  try
    l.Add(FA);
    l.Add(FA);
    AssertEquals('once', 1, OPCUAServerAddTags(FServer, l, false, false));
    AssertEquals('one item', 1, FServer.Items.Count);
  finally
    l.Free;
  end;
end;

initialization
  RegisterTest(TTestOPCUAServerEditor);

end.
