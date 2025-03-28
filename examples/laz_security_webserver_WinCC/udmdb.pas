unit udmdb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, ZConnection, ZDataset, ZAbstractRODataset;

type

  { Tdmdb }

  Tdmdb = class(TDataModule)
    REGISTERsecuritycodeL1046: TZRawStringField;
    REGISTERsecuritycodeLINEAREASPEC: TZIntegerField;
    REGISTERsecuritycodeLINENAME: TZIntegerField;
    REGISTERsecuritycodeLINENUM: TZIntegerField;
    REGISTERsecuritycodeRecordId: TZIntegerField;
    REGISTERsecuritycodeRecordVersion: TZBytesField;
    REGISTERsecuritycodeTEXTID: TZIntegerField;
    SQLServerWINCC: TZConnection;
    BuscaUserName: TZReadOnlyQuery;
    BuscaUserNameDESCRIPTION: TZRawStringField;
    BuscaUserNameEXPTIME: TZIntegerField;
    BuscaUserNameFLAGS: TZIntegerField;
    BuscaUserNameGRPID: TZIntegerField;
    BuscaUserNameID: TZIntegerField;
    BuscaUserNameNAME: TZRawStringField;
    BuscaUserNameNTUSER: TZRawStringField;
    BuscaUserNamePASS: TZRawStringField;
    BuscaUserNamePassWordHash: TZRawStringField;
    BuscaUserNamePWC_ReservedLicense: TZIntegerField;
    BuscaUserNamePWC_StartPicture: TZRawStringField;
    BuscaUserNamePW_Version: TZIntegerField;
    BuscaUserNameWEBSTARTLANG: TZIntegerField;
    BuscaUserNameWEBSTARTPICTURE: TZRawStringField;
    BuscaUserNameWEBUSEHORN: TZIntegerField;
    UIDCanAccessTable: TZReadOnlyQuery;
    UIDCanAccessTableDESCRIPTION: TZRawStringField;
    UIDCanAccessTableEXPTIME: TZIntegerField;
    UIDCanAccessTableFLAGS: TZIntegerField;
    UIDCanAccessTableGRPID: TZIntegerField;
    UIDCanAccessTableID: TZIntegerField;
    UIDCanAccessTableID_1: TZIntegerField;
    UIDCanAccessTableL1046: TZRawStringField;
    UIDCanAccessTableLINEAREASPEC: TZIntegerField;
    UIDCanAccessTableLINENAME: TZIntegerField;
    UIDCanAccessTableLINENUM: TZIntegerField;
    UIDCanAccessTableLINENUM_1: TZIntegerField;
    UIDCanAccessTableNAME: TZRawStringField;
    UIDCanAccessTableNTUSER: TZRawStringField;
    UIDCanAccessTablePASS: TZRawStringField;
    UIDCanAccessTablePassWordHash: TZRawStringField;
    UIDCanAccessTablePERMCOL1: TZIntegerField;
    UIDCanAccessTablePERMCOL2: TZIntegerField;
    UIDCanAccessTablePERMCOL3: TZIntegerField;
    UIDCanAccessTablePERMCOL4: TZIntegerField;
    UIDCanAccessTablePERMCOL5: TZIntegerField;
    UIDCanAccessTablePERMCOL6: TZIntegerField;
    UIDCanAccessTablePERMCOL7: TZIntegerField;
    UIDCanAccessTablePERMCOL8: TZIntegerField;
    UIDCanAccessTablePWC_ReservedLicense: TZIntegerField;
    UIDCanAccessTablePWC_StartPicture: TZRawStringField;
    UIDCanAccessTablePW_Version: TZIntegerField;
    UIDCanAccessTableRecordId: TZIntegerField;
    UIDCanAccessTableRecordVersion: TZBytesField;
    UIDCanAccessTableTEXTID: TZIntegerField;
    UIDCanAccessTableWEBSTARTLANG: TZIntegerField;
    UIDCanAccessTableWEBSTARTPICTURE: TZRawStringField;
    UIDCanAccessTableWEBUSEHORN: TZIntegerField;
    UIDCanAccessTableWHOLELINEPERM: TZIntegerField;
    REGISTERsecuritycode: TZReadOnlyQuery;
  private

  public

  end;

var
  dmdb: Tdmdb;

implementation

{$R *.lfm}

{ Tdmdb }

end.

