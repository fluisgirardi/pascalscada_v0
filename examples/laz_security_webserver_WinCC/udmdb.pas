unit udmdb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PLCString, ISOTCPDriver, tcp_udpport, DB, ZConnection,
  ZDataset, ZAbstractRODataset;

type

  { Tdmdb }

  Tdmdb = class(TDataModule)
    AuthorizationsFromUser: TZReadOnlyQuery;
    AuthorizationsFromUserDESCRIPTION: TStringField;
    AuthorizationsFromUserEXPTIME: TLongintField;
    AuthorizationsFromUserFLAGS: TLongintField;
    AuthorizationsFromUserGRPID: TLongintField;
    AuthorizationsFromUserID: TLongintField;
    AuthorizationsFromUserID_1: TLongintField;
    AuthorizationsFromUserL1046: TStringField;
    AuthorizationsFromUserLINEAREASPEC: TLongintField;
    AuthorizationsFromUserLINENAME: TLongintField;
    AuthorizationsFromUserLINENUM: TLongintField;
    AuthorizationsFromUserLINENUM_1: TLongintField;
    AuthorizationsFromUserNAME: TStringField;
    AuthorizationsFromUserNTUSER: TStringField;
    AuthorizationsFromUserPASS: TStringField;
    AuthorizationsFromUserPassWordHash: TStringField;
    AuthorizationsFromUserPERMCOL1: TLongintField;
    AuthorizationsFromUserPERMCOL2: TLongintField;
    AuthorizationsFromUserPERMCOL3: TLongintField;
    AuthorizationsFromUserPERMCOL4: TLongintField;
    AuthorizationsFromUserPERMCOL5: TLongintField;
    AuthorizationsFromUserPERMCOL6: TLongintField;
    AuthorizationsFromUserPERMCOL7: TLongintField;
    AuthorizationsFromUserPERMCOL8: TLongintField;
    AuthorizationsFromUserPWC_ReservedLicense: TLongintField;
    AuthorizationsFromUserPWC_StartPicture: TStringField;
    AuthorizationsFromUserPW_Version: TLongintField;
    AuthorizationsFromUserRecordId: TLongintField;
    AuthorizationsFromUserRecordVersion: TBytesField;
    AuthorizationsFromUserTEXTID: TLongintField;
    AuthorizationsFromUserWEBSTARTLANG: TLongintField;
    AuthorizationsFromUserWEBSTARTPICTURE: TStringField;
    AuthorizationsFromUserWEBUSEHORN: TLongintField;
    AuthorizationsFromUserWHOLELINEPERM: TLongintField;
    BuscaUserName: TZReadOnlyQuery;
    BuscaUserNameDESCRIPTION: TStringField;
    BuscaUserNameEXPTIME: TLongintField;
    BuscaUserNameFLAGS: TLongintField;
    BuscaUserNameGRPID: TLongintField;
    BuscaUserNameID: TLongintField;
    BuscaUserNameNAME: TStringField;
    BuscaUserNameNTUSER: TStringField;
    BuscaUserNamePASS: TStringField;
    BuscaUserNamePassWordHash: TStringField;
    BuscaUserNamePWC_ReservedLicense: TLongintField;
    BuscaUserNamePWC_StartPicture: TStringField;
    BuscaUserNamePW_Version: TLongintField;
    BuscaUserNameWEBSTARTLANG: TLongintField;
    BuscaUserNameWEBSTARTPICTURE: TStringField;
    BuscaUserNameWEBUSEHORN: TLongintField;
    REGISTERsecuritycode: TZReadOnlyQuery;
    REGISTERsecuritycodeL1046: TStringField;
    REGISTERsecuritycodeLINEAREASPEC: TLongintField;
    REGISTERsecuritycodeLINENAME: TLongintField;
    REGISTERsecuritycodeLINENUM: TLongintField;
    REGISTERsecuritycodeRecordId: TLongintField;
    REGISTERsecuritycodeRecordVersion: TBytesField;
    REGISTERsecuritycodeTEXTID: TLongintField;
    SQLServerWINCC: TZConnection;
    UIDCanAccessTable: TZReadOnlyQuery;
    UIDCanAccessTableDESCRIPTION: TStringField;
    UIDCanAccessTableEXPTIME: TLongintField;
    UIDCanAccessTableFLAGS: TLongintField;
    UIDCanAccessTableGRPID: TLongintField;
    UIDCanAccessTableID: TLongintField;
    UIDCanAccessTableID_1: TLongintField;
    UIDCanAccessTableL1046: TStringField;
    UIDCanAccessTableLINEAREASPEC: TLongintField;
    UIDCanAccessTableLINENAME: TLongintField;
    UIDCanAccessTableLINENUM: TLongintField;
    UIDCanAccessTableLINENUM_1: TLongintField;
    UIDCanAccessTableNAME: TStringField;
    UIDCanAccessTableNTUSER: TStringField;
    UIDCanAccessTablePASS: TStringField;
    UIDCanAccessTablePassWordHash: TStringField;
    UIDCanAccessTablePERMCOL1: TLongintField;
    UIDCanAccessTablePERMCOL2: TLongintField;
    UIDCanAccessTablePERMCOL3: TLongintField;
    UIDCanAccessTablePERMCOL4: TLongintField;
    UIDCanAccessTablePERMCOL5: TLongintField;
    UIDCanAccessTablePERMCOL6: TLongintField;
    UIDCanAccessTablePERMCOL7: TLongintField;
    UIDCanAccessTablePERMCOL8: TLongintField;
    UIDCanAccessTablePWC_ReservedLicense: TLongintField;
    UIDCanAccessTablePWC_StartPicture: TStringField;
    UIDCanAccessTablePW_Version: TLongintField;
    UIDCanAccessTableRecordId: TLongintField;
    UIDCanAccessTableRecordVersion: TBytesField;
    UIDCanAccessTableTEXTID: TLongintField;
    UIDCanAccessTableWEBSTARTLANG: TLongintField;
    UIDCanAccessTableWEBSTARTPICTURE: TStringField;
    UIDCanAccessTableWEBUSEHORN: TLongintField;
    UIDCanAccessTableWHOLELINEPERM: TLongintField;
    procedure DataModuleCreate(Sender: TObject);
  private

  public

  end;

var
  dmdb: Tdmdb;

implementation

{$R *.lfm}

{ Tdmdb }


procedure Tdmdb.DataModuleCreate(Sender: TObject);
begin

end;

end.

