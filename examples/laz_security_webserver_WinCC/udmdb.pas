unit udmdb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, ZConnection, ZDataset, ZAbstractRODataset;

type

  { Tdmdb }

  Tdmdb = class(TDataModule)
    AuthorizationsFromUserDESCRIPTION: TZRawStringField;
    AuthorizationsFromUserEXPTIME: TZIntegerField;
    AuthorizationsFromUserFLAGS: TZIntegerField;
    AuthorizationsFromUserGRPID: TZIntegerField;
    AuthorizationsFromUserID: TZIntegerField;
    AuthorizationsFromUserID_1: TZIntegerField;
    AuthorizationsFromUserL1046: TZRawStringField;
    AuthorizationsFromUserLINEAREASPEC: TZIntegerField;
    AuthorizationsFromUserLINENAME: TZIntegerField;
    AuthorizationsFromUserLINENUM: TZIntegerField;
    AuthorizationsFromUserLINENUM_1: TZIntegerField;
    AuthorizationsFromUserNAME: TZRawStringField;
    AuthorizationsFromUserNTUSER: TZRawStringField;
    AuthorizationsFromUserPASS: TZRawStringField;
    AuthorizationsFromUserPassWordHash: TZRawStringField;
    AuthorizationsFromUserPERMCOL1: TZIntegerField;
    AuthorizationsFromUserPERMCOL2: TZIntegerField;
    AuthorizationsFromUserPERMCOL3: TZIntegerField;
    AuthorizationsFromUserPERMCOL4: TZIntegerField;
    AuthorizationsFromUserPERMCOL5: TZIntegerField;
    AuthorizationsFromUserPERMCOL6: TZIntegerField;
    AuthorizationsFromUserPERMCOL7: TZIntegerField;
    AuthorizationsFromUserPERMCOL8: TZIntegerField;
    AuthorizationsFromUserPWC_ReservedLicense: TZIntegerField;
    AuthorizationsFromUserPWC_StartPicture: TZRawStringField;
    AuthorizationsFromUserPW_Version: TZIntegerField;
    AuthorizationsFromUserRecordId: TZIntegerField;
    AuthorizationsFromUserRecordVersion: TZBytesField;
    AuthorizationsFromUserTEXTID: TZIntegerField;
    AuthorizationsFromUserWEBSTARTLANG: TZIntegerField;
    AuthorizationsFromUserWEBSTARTPICTURE: TZRawStringField;
    AuthorizationsFromUserWEBUSEHORN: TZIntegerField;
    AuthorizationsFromUserWHOLELINEPERM: TZIntegerField;
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
    AuthorizationsFromUser: TZReadOnlyQuery;
  private

  public

  end;

var
  dmdb: Tdmdb;

implementation

{$R *.lfm}

{ Tdmdb }

end.

