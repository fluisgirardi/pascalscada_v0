unit udmdb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset, ZAbstractRODataset;

type

  { Tdmdb }

  Tdmdb = class(TDataModule)
    REGISTERsecuritycodeL1033: TZRawStringField;
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
    UIDCanAccessTableID: TZIntegerField;
    UIDCanAccessTableL1033: TZRawStringField;
    UIDCanAccessTableLINEAREASPEC: TZIntegerField;
    UIDCanAccessTableLINENAME: TZIntegerField;
    UIDCanAccessTableLINENUM: TZIntegerField;
    UIDCanAccessTableLINENUM_1: TZIntegerField;
    UIDCanAccessTablePERMCOL1: TZIntegerField;
    UIDCanAccessTablePERMCOL2: TZIntegerField;
    UIDCanAccessTablePERMCOL3: TZIntegerField;
    UIDCanAccessTablePERMCOL4: TZIntegerField;
    UIDCanAccessTablePERMCOL5: TZIntegerField;
    UIDCanAccessTablePERMCOL6: TZIntegerField;
    UIDCanAccessTablePERMCOL7: TZIntegerField;
    UIDCanAccessTablePERMCOL8: TZIntegerField;
    UIDCanAccessTableRecordId: TZIntegerField;
    UIDCanAccessTableRecordVersion: TZBytesField;
    UIDCanAccessTableTEXTID: TZIntegerField;
    UIDCanAccessTableWHOLELINEPERM: TZIntegerField;
    REGISTERsecuritycode: TZReadOnlyQuery;
  private

  public

  end;

var
  dmdb: Tdmdb;

implementation

{$R *.lfm}

end.

