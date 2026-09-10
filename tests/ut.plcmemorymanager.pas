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
    procedure UmIntervaloViraUmBlocoUnico;
    procedure BuracoPequenoUneOsBlocos;
    procedure BuracoGrandeSeparaOsBlocos;
    procedure LimiteDeItensQuebraOBloco;
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

procedure TTestPLCMemoryManager.UmIntervaloViraUmBlocoUnico;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  AssertEquals('quantidade de blocos', 1, Length(FMM.Blocks));
  AssertEquals('endereco inicial', 0, FMM.Blocks[0].AddressStart);
  AssertEquals('endereco final', 9, FMM.Blocks[0].AddressEnd);
  AssertEquals('tamanho total', 10, FMM.Size);
end;

procedure TTestPLCMemoryManager.BuracoPequenoUneOsBlocos;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(13, 2, 1, 1000);
  AssertEquals('buraco de 3 <= MaxHole deve unir', 1, Length(FMM.Blocks));
  AssertEquals('endereco final', 14, FMM.Blocks[0].AddressEnd);
end;

procedure TTestPLCMemoryManager.BuracoGrandeSeparaOsBlocos;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(100, 2, 1, 1000);
  AssertEquals('buraco grande deve separar', 2, Length(FMM.Blocks));
end;

procedure TTestPLCMemoryManager.LimiteDeItensQuebraOBloco;
begin
  FMM.MaxBlockItems := 20;
  FMM.AddAddress(0, 50, 1, 1000);
  AssertTrue('bloco deve ser fatiado em varios', Length(FMM.Blocks) > 1);
end;

initialization
  RegisterTest(TTestPLCMemoryManager);

end.
