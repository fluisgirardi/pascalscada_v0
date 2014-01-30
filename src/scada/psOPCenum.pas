unit psOPCenum;

{$IFDEF VER150}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}
{$IFDEF VER170}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
  Classes, Windows, ActiveX, psOPCCOMN;

type
  TOPCRegistryProgressEvent = procedure(Sender: TObject; Current: LongInt;
    Maximum: LongInt) of object;

  TOPCServerList = class(TObject)
  private
    FCATIDs: array of TGUID;
    FComputerName: string;
    FOnProgress: TOPCRegistryProgressEvent;
    FRegistryConnected: Boolean;
    FRegistryOpened: Boolean;
    FServerBrowserObjectCreated: Boolean;
    FServerList: TStringList;
    FUseRegistry: Boolean;
    FUseServerBrowser: Boolean;
    procedure AddItem(ProgID: string; CLSID: TGUID; Description: string;
      Vendor: string);
    procedure DestroyList;
    function GetCLSID(Index: LongInt): TCLSID;
    function GetCount: LongInt;
    function GetDescription(Index: LongInt): string;
    function GetProgID(Index: LongInt): string;
    function GetServerList: TStrings;
    function GetVendor(Index: LongInt): string;
    procedure ListCategory(OPCServerList: IOPCServerList; CATID: TGUID);
    procedure ListCategory2(OPCServerList: IOPCServerList2; CATID: TGUID);
    procedure SetupListFromRegistry;
    procedure SetupListFromServerBrowser;
  public
    constructor Create(ComputerName: string; UseRegistry: Boolean;
      CATIDs: array of TGUID);
    destructor Destroy; override;
    function Update: LongInt;
    property CLSID[Index: LongInt]: TCLSID read GetCLSID;
    property Count: LongInt read GetCount;
    property Description[Index: LongInt]: string read GetDescription;
    property Items: TStrings read GetServerList;
    property OnProgress: TOPCRegistryProgressEvent read FOnProgress
      write FOnProgress;
    property ProgID[Index: LongInt]: string read GetProgID;
    property RegistryConnected: Boolean read FRegistryConnected;
    property RegistryOpened: Boolean read FRegistryOpened;
    property ServerBrowserObjectCreated: Boolean
      read FServerBrowserObjectCreated;
    property Vendor[Index: LongInt]: string read GetVendor;
  end;

implementation

uses
  Registry, ComObj, psOPCDA;

type
  TOPCServerInfo = class(TObject)
    ProgID: string;
    CLSID: TCLSID;
    Description: string;
    Vendor: string;
  end;

procedure TOPCServerList.AddItem(ProgID: string; CLSID: TGUID;
  Description: string; Vendor: string);
var
  ServerInfo: TOPCServerInfo;
  I: LongInt;
begin
  // don't list the same server twice under different ProgIDs,
  // so first check for an identical CLSID in list of servers already found
  for I := 0 to FServerList.Count - 1 do
  begin
    if IsEqualGUID(CLSID, GetCLSID(I)) then Exit;
  end;
  ServerInfo := TOPCServerInfo.Create;
  ServerInfo.ProgID := ProgID;
  ServerInfo.CLSID := CLSID;
  ServerInfo.Description := Description;
  ServerInfo.Vendor := Vendor;
  try
    FServerList.AddObject(ProgID, ServerInfo);
  except
    // it must have been a duplicate
    ServerInfo.Free;
  end;
end;

constructor TOPCServerList.Create(ComputerName: string; UseRegistry: Boolean;
  CATIDs: array of TGUID);
var
  NumCATIDs: LongInt;
  I: LongInt;
begin
  inherited Create;
  FComputerName := ComputerName;
  FUseRegistry := UseRegistry;
  NumCATIDs := Length(CATIDs);
  FUseServerBrowser := NumCATIDs > 0;
  if FUseServerBrowser then
  begin
    SetLength(FCATIDs, NumCATIDs);
    for I := 0 to NumCATIDs - 1 do
    begin
      FCATIDs[I] := CATIDs[I];
    end;
  end;
  FServerList := nil;
end;

destructor TOPCServerList.Destroy;
begin
  DestroyList;
  inherited;
end;

procedure TOPCServerList.DestroyList;
var
  NumServers: LongInt;
  Index: LongInt;
begin
  if FServerList <> nil then
  begin
    NumServers := FServerList.Count;
    for Index := 0 to NumServers - 1 do
    begin
      FServerList.Objects[Index].Free;
      FServerList.Objects[Index] := nil;
    end;
    FServerList.Free;
    FServerList := nil;
  end;
end;

function TOPCServerList.GetCLSID(Index: LongInt): TCLSID;
begin
  Result := TOPCServerInfo(FServerList.Objects[Index]).CLSID;
end;

function TOPCServerList.GetCount: LongInt;
begin
  if (FServerList <> nil) then
  begin
    Result := FServerList.Count;
  end
  else begin
    Result := 0;
  end;
end;

function TOPCServerList.GetDescription(Index: LongInt): string;
begin
  Result := TOPCServerInfo(FServerList.Objects[Index]).Description;
end;

function TOPCServerList.GetProgID(Index: LongInt): string;
begin
  Result := TOPCServerInfo(FServerList.Objects[Index]).ProgID;
end;

function TOPCServerList.GetServerList: TStrings;
begin
  Result := FServerList;
end;

function TOPCServerList.GetVendor(Index: LongInt): string;
begin
  Result := TOPCServerInfo(FServerList.Objects[Index]).Vendor;
end;

procedure TOPCServerList.ListCategory(OPCServerList: IOPCServerList;
  CATID: TGUID);
var
  HR: HResult;
  EnumGUID: IEnumGUID;
  CLSID: TGUID;
  Fetched: UINT;
  PProgID: POleStr;
  PUSerType: POleStr;
begin
  HR := OPCServerList.EnumClassesOfCategories(1, @CATID, 0, nil, EnumGUID);
  if Succeeded(HR) then
  begin
    repeat
      {$IFDEF FPC}
      HR := EnumGUID.Next(1, CLSID, @Fetched);
      {$ELSE}
      HR := EnumGUID.Next(1, CLSID, Fetched);
      {$ENDIF}
      if Succeeded(HR) and (Fetched = 1) then
      begin
        if Succeeded(
          OPCServerList.GetClassDetails(CLSID, PProgID, PUserType)) then
        begin
          try
            AddItem(WideString(PProgID), CLSID, WideString(PUserType), '');
          finally
            CoTaskMemFree(PProgID);
            CoTaskMemFree(PUserType);
          end;
        end;
      end;
    until Failed(HR) or (Fetched <> 1);
  end;
end;

procedure TOPCServerList.ListCategory2(OPCServerList: IOPCServerList2;
  CATID: TGUID);
var
  HR: HResult;
  EnumGUID: IOPCEnumGUID;
  CLSID: TGUID;
  Fetched: UINT;
  PProgID: POleStr;
  PUSerType: POleStr;
  PVerIndProgID: POleStr;
begin
  HR := OPCServerList.EnumClassesOfCategories(1, @CATID, 0, nil, EnumGUID);
  if Succeeded(HR) then
  begin
    repeat
      HR := EnumGUID.Next(1, CLSID, Fetched);
      if Succeeded(HR) and (Fetched = 1) then
      begin
        if Succeeded(
          OPCServerList.GetClassDetails(CLSID, PProgID, PUserType,
            PVerIndProgID)) then
        begin
          try
            AddItem(WideString(PProgID), CLSID, WideString(PUserType), '');
          finally
            CoTaskMemFree(PProgID);
            CoTaskMemFree(PUserType);
          end;
        end;
      end;
    until Failed(HR) or (Fetched <> 1);
  end;
end;

procedure TOPCServerList.SetupListFromRegistry;
var
  Reg: TRegistry;
  KeyList: TStringList;
  NumKeys: LongInt;
  Index: LongInt;
  ProgID: WideString;
  CLSID: TGUID;
  Description: string;
  Vendor: string;
  ComputerName: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    FRegistryConnected := True;
    if Length(FComputerName) > 0 then
    begin
      ComputerName := FComputerName;
      if ComputerName[1] <> '\' then
      begin
        ComputerName := '\\' + ComputerName;
      end;
      if not Reg.RegistryConnect(ComputerName) then
      begin
        FRegistryConnected := False;
      end;
    end;

    if FRegistryConnected and Reg.OpenKeyReadOnly('') then
    begin
      FRegistryOpened := True;
      KeyList := TStringList.Create;
      try
        KeyList.BeginUpdate;
        try
          Reg.GetKeyNames(KeyList);
        finally
          KeyList.EndUpdate;
        end;
        NumKeys := KeyList.Count;
        Reg.CloseKey;

        for Index := 0 to NumKeys - 1 do
        begin
          if Assigned(OnProgress) then OnProgress(Self, Index, NumKeys);
          ProgID := KeyList.Strings[Index];
          if Reg.OpenKeyReadOnly(ProgID) then
          begin
            try
              Description := Reg.ReadString('');
            except
              Description := '';
            end;
            if Reg.OpenKeyReadOnly('OPC') then
            begin
              try
                if Succeeded(
                  CLSIDFromProgID(PWideChar(ProgID), CLSID)) then
                begin
                  // look for optional Vendor identifier
                  Vendor := '';
                  if Reg.OpenKeyReadOnly('Vendor') then
                  begin
                    try
                      Vendor := Reg.ReadString('');
                    except
                      Vendor := '';
                    end;
                  end;
                  AddItem(ProgID, CLSID, Description, Vendor);
                end;
              except
              end;
            end;
            Reg.CloseKey;
          end;
        end;
      finally
        KeyList.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TOPCServerList.SetupListFromServerBrowser;
var
  PUnk: IUnknown;
  OPCServerList: IOPCServerList;
  OPCServerList2: IOPCServerList2;
  NumCATIDs: LongInt;
  I: LongInt;
begin
  try
    if Length(FComputerName) = 0 then
    begin
      PUnk := CreateComObject(CLSID_OPCServerList)
    end
    else begin
      PUnk := CreateRemoteComObject(FComputerName, CLSID_OPCServerList);
    end;
  except
    PUnk := nil;
  end;
  if PUnk = nil then Exit;

  try
    OPCServerList2 := PUnk as IOPCServerList2;
  except
    OPCServerList2 := nil;
    try
      OPCServerList := PUnk as IOPCServerList;
    except
      OPCServerList := nil;
    end;
  end;
  if (OPCServerList2 = nil) and (OPCServerList = nil) then Exit;

  FServerBrowserObjectCreated := True;
  NumCATIDs := Length(FCATIDs);
  for I := 0 to NumCATIDs - 1 do
  begin
    if OPCServerList2 <> nil then
    begin
      ListCategory2(OPCServerList2, FCATIDs[I]);
    end
    else begin
      ListCategory(OPCServerList, FCATIDs[I]);
    end;
  end;
end;

function TOPCServerList.Update: LongInt;
begin
  DestroyList;
  FRegistryConnected := False;
  FRegistryOpened := False;
  FServerBrowserObjectCreated := False;

  FServerList := TStringList.Create;
  FServerList.Sorted := True;
  FServerList.Duplicates := dupError;

  if FUseRegistry then
  begin
    SetupListFromRegistry;
  end;
  if FUseServerBrowser then
  begin
    SetupListFromServerBrowser;
  end;

  Result := GetCount;
end;

end.
