unit ut.plcmemorymanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  plcmemorymanager;

type

  { TTestPLCMemoryManager }

  TTestPLCMemoryManager = class(TTestCase)
  private
    FMM: TPLCMemoryManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OneRangeBecomesASingleBlock;
    procedure ASmallHoleJoinsTheBlocks;
    procedure ABigHoleSplitsTheBlocks;
    procedure TheItemLimitBreaksTheBlock;
  end;

implementation

procedure TTestPLCMemoryManager.SetUp;
begin
  FMM := TPLCMemoryManager.Create;
  FMM.MaxHole := 5;
  FMM.MaxBlockItems := 100;
end;

procedure TTestPLCMemoryManager.TearDown;
begin
  FreeAndNil(FMM);
end;

procedure TTestPLCMemoryManager.OneRangeBecomesASingleBlock;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  AssertEquals('number of blocks', 1, Length(FMM.Blocks));
  AssertEquals('start address', 0, FMM.Blocks[0].AddressStart);
  AssertEquals('end address', 9, FMM.Blocks[0].AddressEnd);
  AssertEquals('total size', 10, FMM.Size);
end;

procedure TTestPLCMemoryManager.ASmallHoleJoinsTheBlocks;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(13, 2, 1, 1000);
  AssertEquals('a hole of 3 <= MaxHole must join', 1, Length(FMM.Blocks));
  AssertEquals('end address', 14, FMM.Blocks[0].AddressEnd);
end;

procedure TTestPLCMemoryManager.ABigHoleSplitsTheBlocks;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(100, 2, 1, 1000);
  AssertEquals('a big hole must split', 2, Length(FMM.Blocks));
end;

procedure TTestPLCMemoryManager.TheItemLimitBreaksTheBlock;
begin
  FMM.MaxBlockItems := 20;
  FMM.AddAddress(0, 50, 1, 1000);
  AssertTrue('the block must be sliced into several', Length(FMM.Blocks) > 1);
end;

initialization
  RegisterTest(TTestPLCMemoryManager);

end.
