{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2007 by the Free Pascal development team
    Some modifications (c) 2007 by Martin Schreiber

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
	
    ----------------------------------------------------------------
	
    TMemDataset : In-memory dataset.
    - Has possibility to copy Structure/Data from other dataset.
    - Can load/save to/from stream.
    Ideas taken from THKMemTab Component by Harri Kasulke - Hamburg/Germany
    E-mail: harri.kasulke@okay.net	
	
    ----------------------------------------------------------------

    This file is based on memds.pp of FreePascal RTL. It has been
    added on PascalSCADA project to make it easy to keep up to date
    between FPC and Lazarus and provide a always updated In memory
    Dataset for all FPC versions.

 **********************************************************************}
{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}

unit fpc_ps_memds;

interface

uses
 sysutils, classes, db, types;

const
  // Stream Markers.
  MarkerSize  = SizeOf(Integer);

  smEOF       = 0;
  smFieldDefs = 1;
  smData      = 2;

type
  {$IFDEF FPC}
    {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION < 20701)}
      {$DEFINE FPCPS_COMPAT_MODE}
    {$IFEND}
  {$ELSE}
    {$I delphiver.inc}
    {$DEFINE NEED_PTRINT}
    {$IFNDEF DELPHI2009_UP}
      {$DEFINE FPCPS_COMPAT_MODE}
    {$ENDIF}
  {$ENDIF}
  
  {$IFDEF FPCPS_COMPAT_MODE}
  TRecordBuffer = PChar;
  {$ENDIF}

  {$IFDEF NEED_PTRINT}
  ptrint = Integer;
  {$ENDIF}

  FPCPSMDSError=class(Exception);

  PFPCPSRecInfo=^TFPCPSMTRecInfo;
  TFPCPSMTRecInfo=record
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;

  { TMemDataset }

  TFPCPSMemDataset=class(TDataSet)
  private
    FOpenStream : TStream;
    FFileName : String;
    FFileModified : Boolean;
    FStream: TMemoryStream;
    FRecInfoOffset: integer;
    FRecCount: integer;
    FRecSize: integer;
    FRecBufferSize: integer;
    FCurrRecNo: integer;
    FIsOpen: boolean;
    FTableIsCreated: boolean;
    FFilterBuffer: TRecordBuffer;
    ffieldoffsets: PInteger;
    ffieldsizes: PInteger;
    function GetRecordBufferPointer(p:TRecordBuffer; Pos:Integer):TRecordBuffer;
    function GetIntegerPointer(p:PInteger; Pos:Integer):PInteger;

    procedure calcrecordlayout;
    function  MDSGetRecordOffset(ARecNo: integer): longint;
    function  MDSGetFieldOffset(FieldNo: integer): integer;
    function  MDSGetBufferSize(FieldNo: integer): integer;
    function  MDSGetActiveBuffer(var Buffer: TRecordBuffer): Boolean;
    procedure MDSReadRecord(Buffer:TRecordBuffer;ARecNo:Integer);
    procedure MDSWriteRecord(Buffer:TRecordBuffer;ARecNo:Integer);
    procedure MDSAppendRecord(Buffer:TRecordBuffer);
    function  MDSFilterRecord(Buffer:TRecordBuffer): Boolean;
  protected
    // Mandatory
    function  AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function  GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function  IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    // Optional.
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;

    // Own.
    Procedure RaiseError(Fmt : String; Args : Array of const);
    Procedure CheckMarker(F : TStream; Marker : Integer);
    Procedure WriteMarker(F : TStream; Marker : Integer);
    procedure ReadFieldDefsFromStream(F : TStream);
    procedure SaveFieldDefsToStream(F : TStream);
    // These should be overridden if you want to load more data.
    // E.g. index defs.
    Procedure LoadDataFromStream(F : TStream); virtual;
    // If SaveData=False, a size 0 block should be written.
    Procedure SaveDataToStream(F : TStream; SaveData : Boolean); virtual;


  public
    constructor Create(AOwner:tComponent); override;
    destructor Destroy; override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    procedure CreateTable;

    Function  DataSize : Integer;

    procedure Clear(ClearDefs : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    procedure Clear;{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToFile(AFileName : String);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToFile(AFileName : String; SaveData : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToStream(F : TStream); {$IFNDEF FPC} overload; {$ENDIF}
    Procedure SaveToStream(F : TStream; SaveData : Boolean);{$IFNDEF FPC} overload; {$ENDIF}
    Procedure LoadFromStream(F : TStream);
    Procedure LoadFromFile(AFileName : String);
    Procedure CopyFromDataset(DataSet : TDataSet); {$IFNDEF FPC} overload; {$ENDIF}
    Procedure CopyFromDataset(DataSet : TDataSet; CopyData : Boolean); {$IFNDEF FPC} overload; {$ENDIF}

    Property FileModified : Boolean Read FFileModified;

  published
    Property FileName : String Read FFileName Write FFileName;
    property Filtered;
    Property Active;
    Property FieldDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnFilterRecord;
  end;

implementation

ResourceString
  SErrFieldTypeNotSupported = 'Fieldtype of Field "%s" not supported.';
  SErrBookMarkNotFound      = 'Bookmark %d not found.';
  SErrInvalidDataStream     = 'Error in data stream at position %d';
  SErrInvalidMarkerAtPos    = 'Wrong data stream marker at position %d. Got %d, expected %d';
  SErrNoFileName            = 'Filename must not be empty.';

Const
  SizeRecInfo = SizeOf(TFPCPSMTRecInfo);

procedure unsetfieldisnull(nullmask: pbyte; const x: integer);

begin
 inc(nullmask,(x shr 3));
 nullmask^:= nullmask^ or (1 shl (x and 7));
end;


procedure setfieldisnull(nullmask: pbyte; const x: integer);

begin
 inc(nullmask,(x shr 3));
 nullmask^:= nullmask^ and Not (1 shl (x and 7));
end;


function getfieldisnull(nullmask: pbyte; const x: integer): boolean;

begin
 inc(nullmask,(x shr 3));
 result:= nullmask^ and (1 shl (x and 7)) = 0;
end;


{ ---------------------------------------------------------------------
    Stream functions
  ---------------------------------------------------------------------}

Function ReadInteger(S : TStream) : Integer;

begin
  S.ReadBuffer(Result,SizeOf(Result));
end;

Function ReadString(S : TStream) : String;

Var
  L : Integer;

begin
  L:=ReadInteger(S);
  Setlength(Result,L);
  If (L<>0) then
    S.ReadBuffer(Result[1],L);
end;

Procedure WriteInteger(S : TStream; Value : Integer);

begin
  S.WriteBuffer(Value,SizeOf(Value));
end;

Procedure WriteString(S : TStream; Value : String);

Var
  L : Integer;

begin
  L:=Length(Value);
  WriteInteger(S,Length(Value));
  If (L<>0) then
    S.WriteBuffer(Value[1],L);
end;

{ ---------------------------------------------------------------------
    TMemDataset
  ---------------------------------------------------------------------}


constructor TFPCPSMemDataset.Create(AOwner:tComponent);

begin
  inherited create(aOwner);
  FStream:=TMemoryStream.Create;
  FRecCount:=0;
  FRecSize:=0;
  FRecBufferSize:=0;
  FRecInfoOffset:=0;
  FCurrRecNo:=-1;
  BookmarkSize := sizeof(Longint);
  FIsOpen:=False;
end;

Destructor TFPCPSMemDataset.Destroy;
begin
  FStream.Free;
  FreeMem(FFieldOffsets);
  FreeMem(FFieldSizes);
  inherited Destroy;
end;

function TFPCPSMemDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
var
  ReqBookmark: integer;
begin
  Result := False;
  if ABookMark=nil then exit;
  ReqBookmark:=PInteger(ABookmark)^;
  Result := (ReqBookmark>=0) and (ReqBookmark<FRecCount);
end;

function TFPCPSMemDataset.MDSGetRecordOffset(ARecNo: integer): longint;
begin
  Result:=FRecSize*ARecNo
end;

function TFPCPSMemDataset.MDSGetFieldOffset(FieldNo: integer): integer;
begin
 result:= getIntegerpointer(ffieldoffsets, fieldno-1)^;
end;

Procedure TFPCPSMemDataset.RaiseError(Fmt : String; Args : Array of const);

begin
  Raise FPCPSMDSError.CreateFmt(Fmt,Args);
end;

function TFPCPSMemDataset.MDSGetBufferSize(FieldNo: integer): integer;
var
 dt1: tfieldtype;
begin
 dt1:= FieldDefs.Items[FieldNo-1].Datatype;
 case dt1 of
  ftString:   result:=FieldDefs.Items[FieldNo-1].Size+1;
  ftFixedChar:result:=FieldDefs.Items[FieldNo-1].Size+1;
  ftBoolean:  result:=SizeOf(Wordbool);
  ftCurrency,
  ftFloat:    result:=SizeOf(Double);
  ftBCD:      result:=SizeOf(currency);
  ftLargeInt: result:=SizeOf(int64);
  ftSmallInt: result:=SizeOf(SmallInt);
  ftInteger:  result:=SizeOf(longint);
  ftDateTime,
    ftTime,
    ftDate:   result:=SizeOf(TDateTime);
 else
  RaiseError(SErrFieldTypeNotSupported,[FieldDefs.Items[FieldNo-1].Name]);
 end;
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
 Result:=Align(Result,4);
{$ENDIF}
end;

function TFPCPSMemDataset.MDSGetActiveBuffer(var Buffer: TRecordBuffer): Boolean;

begin
 case State of
   dsBrowse:
     if IsEmpty then
       Buffer:=nil
     else
       Buffer:=ActiveBuffer;
  dsEdit,
  dsInsert:
     Buffer:=ActiveBuffer;
  dsFilter:
     Buffer:=FFilterBuffer;
 else
   Buffer:=nil;
 end;
 Result:=(Buffer<>nil);
end;

procedure TFPCPSMemDataset.MDSReadRecord(Buffer:TRecordBuffer;ARecNo:Integer);   //Reads a Rec from Stream in Buffer
begin
  FStream.Position:=MDSGetRecordOffset(ARecNo);
  FStream.ReadBuffer(Buffer^, FRecSize);
end;

procedure TFPCPSMemDataset.MDSWriteRecord(Buffer:TRecordBuffer;ARecNo:Integer);  //Writes a Rec from Buffer to Stream
begin
  FStream.Position:=MDSGetRecordOffset(ARecNo);
  FStream.WriteBuffer(Buffer^, FRecSize);
  FFileModified:=True;
end;

procedure TFPCPSMemDataset.MDSAppendRecord(Buffer:TRecordBuffer);   //Appends a Rec (from Buffer) to Stream
begin
  FStream.Position:=MDSGetRecordOffset(FRecCount);
  FStream.WriteBuffer(Buffer^, FRecSize);
  FFileModified:=True;
end;

//Abstract Overrides
function TFPCPSMemDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result,FRecBufferSize);
end;

procedure TFPCPSMemDataset.FreeRecordBuffer (var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
end;

procedure TFPCPSMemDataset.InternalInitRecord(Buffer: TRecordBuffer);

var
  I : integer;

begin
 fillchar(buffer^,frecsize,0);
end;

procedure TFPCPSMemDataset.InternalDelete;

Var
  TS : TMemoryStream;
  OldPos,NewPos,CopySize1,CopySize2 : Cardinal;

begin
  if (FCurrRecNo<0) or (FCurrRecNo>=FRecCount) then
    Exit;
  // Very inefficient. We should simply move the last part closer to the beginning in
  // The FStream.
  TS:=TMemoryStream.Create;
  Try
    if FCurrRecNo>0 then
      begin
      FStream.Position:=MDSGetRecordOffset(0);      //Delete Rec
      if FCurrRecNo<FRecCount-1 then
        begin
        TS.CopyFrom(FStream, MDSGetRecordOffset(FCurrRecNo)-MDSGetRecordOffset(0));
        FStream.Position:=MDSGetRecordOffset(FCurrRecNo+1);
        TS.CopyFrom(FStream,(MDSGetRecordOffset(FRecCount))-MDSGetRecordOffset(FCurrRecNo+1));
        end
      else
        TS.CopyFrom(FStream,MDSGetRecordOffset(FRecCount-1));
      end
    else
      begin                                  //Delete first Rec
      FStream.Position:=MDSGetRecordOffset(FCurrRecNo+1);
      TS.CopyFrom(FStream,(MDSGetRecordOffset(FRecCount))-MDSGetRecordOffset(FCurrRecNo+1));
      end;
    FStream.loadFromStream(TS);
    Dec(FRecCount);
    if FRecCount=0 then
      FCurrRecNo:=-1
    else
      if FCurrRecNo>=FRecCount then FCurrRecNo:=FRecCount-1;
  Finally
    TS.Free;
  end;
  FFileModified:=True;
end;

procedure TFPCPSMemDataset.InternalInitFieldDefs;

begin
  If (FOpenStream<>Nil) then
    ReadFieldDefsFromStream(FOpenStream);
end;

Procedure TFPCPSMemDataset.CheckMarker(F : TStream; Marker : Integer);

Var
  I,P : Integer;

begin
  P:=F.Position;
  If F.Read(I,MarkerSize)<>MarkerSize then
    RaiseError(SErrInvalidDataStream,[P])
  else
    if (I<>Marker) then
      RaiseError(SErrInvalidMarkerAtPos,[P,I,Marker]);
end;

procedure TFPCPSMemDataset.ReadFieldDefsFromStream(F : TStream);

Var
  I,ACount : Integer;
  FN : String;
  FS : Integer;
  B : Boolean;
  FT : TFieldType;

begin
  CheckMarker(F,smFieldDefs);
  FieldDefs.Clear;
  ACount:=ReadInteger(F);
  For I:=1 to ACount do
    begin
    FN:=ReadString(F);
    FS:=ReadInteger(F);
    FT:=TFieldType(ReadInteger(F));
    B:=ReadInteger(F)<>0;
    TFieldDef.Create(FieldDefs,FN,ft,FS,B,I);
    end;
  CreateTable;
end;

procedure TFPCPSMemDataset.InternalFirst;
begin
  FCurrRecNo:=-1;
end;

procedure TFPCPSMemDataset.InternalLast;
begin
  FCurrRecNo:=FRecCount;
end;

procedure TFPCPSMemDataset.InternalOpen;


begin
  if not FTableIsCreated then CreateTable;
  If (FFileName<>'') then
    FOpenStream:=TFileStream.Create(FFileName,fmOpenRead);
  Try
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(True);
    FCurrRecNo:=-1;
    If (FOpenStream<>Nil) then
      begin
      LoadDataFromStream(FOpenStream);
      CheckMarker(FOpenStream,smEOF);
      end;
  Finally
    FreeAndNil(FOpenStream);
  end;
  FIsOpen:=True;
end;

Procedure TFPCPSMemDataSet.LoadDataFromStream(F : TStream);

Var
  Size : Integer;

begin
  CheckMarker(F,smData);
  Size:=ReadInteger(F);
  FStream.Clear;
  FStream.CopyFrom(F,Size);
  FRecCount:=Size div FRecSize;
  FCurrRecNo:=-1;
end;

Procedure TFPCPSMemDataSet.LoadFromStream(F : TStream);

begin
  Close;
  ReadFieldDefsFromStream(F);
  LoadDataFromStream(F);
  CheckMarker(F,smEOF);
  FFileModified:=False;
end;

Procedure TFPCPSMemDataSet.LoadFromFile(AFileName : String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    LoadFromStream(F);
  Finally
    F.Free;
  end;
end;


Procedure TFPCPSMemDataset.SaveToFile(AFileName : String);

begin
  SaveToFile(AFileName,True);
end;

Procedure TFPCPSMemDataset.SaveToFile(AFileName : String; SaveData : Boolean);

Var
  F : TFileStream;

begin
  If (AFileName='') then
    RaiseError(SErrNoFileName,[]);
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(F,SaveData);
  Finally
    F.Free;
  end;
end;

Procedure TFPCPSMemDataset.WriteMarker(F : TStream; Marker : Integer);

begin
  Writeinteger(F,Marker);
end;

Procedure TFPCPSMemDataset.SaveToStream(F : TStream);

begin
  SaveToStream(F,True);
end;

Procedure TFPCPSMemDataset.SaveToStream(F : TStream; SaveData : Boolean);

begin
  SaveFieldDefsToStream(F);
  If SaveData then
    SaveDataToStream(F,SaveData);
  WriteMarker(F,smEOF);
end;

Procedure TFPCPSMemDataset.SaveFieldDefsToStream(F : TStream);

Var
  I,ACount : Integer;
  FN : String;
  FS : Integer;
  B : Boolean;
  FT : TFieldType;
  FD : TFieldDef;

begin
  WriteMarker(F,smFieldDefs);
  WriteInteger(F,FieldDefs.Count);
  For I:=1 to FieldDefs.Count do
    begin
    FD:=FieldDefs[I-1];
    WriteString(F,FD.Name);
    WriteInteger(F,FD.Size);
    WriteInteger(F,Ord(FD.DataType));
    WriteInteger(F,Ord(FD.Required));
    end;
end;

Procedure TFPCPSMemDataset.SaveDataToStream(F : TStream; SaveData : Boolean);

begin
  if SaveData then
    begin
    WriteMarker(F,smData);
    WriteInteger(F,FStream.Size);
    FStream.Position:=0;
    F.CopyFrom(FStream,FStream.Size);
    FFileModified:=False;
    end
  else
    begin
    WriteMarker(F,smData);
    WriteInteger(F,0);
    end;
end;

procedure TFPCPSMemDataset.InternalClose;

begin
 if (FFileModified) and (FFileName<>'') then begin
  SaveToFile(FFileName,True);
 end;
 FIsOpen:=False;
 FFileModified:=False;
 // BindFields(False);
 if DefaultFields then begin
  DestroyFields;
 end;
end;

procedure TFPCPSMemDataset.InternalPost;
begin
  CheckActive;
  if ((State<>dsEdit) and (State<>dsInsert)) then
    Exit;
  if (State=dsEdit) then
    MDSWriteRecord(ActiveBuffer, FCurrRecNo)
  else
    InternalAddRecord(ActiveBuffer,True);
end;

function TFPCPSMemDataset.IsCursorOpen: Boolean;

begin
  Result:=FIsOpen;
end;

function TFPCPSMemDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

var
  Accepted: Boolean;

begin
  Result:=grOk;
  Accepted:=False;
  if (FRecCount<1) then
    begin
    Result:=grEOF;
    exit;
    end;
  repeat
    case GetMode of
      gmCurrent:
        if (FCurrRecNo>=FRecCount) or (FCurrRecNo<0) then
          Result:=grError;
      gmNext:
        if (FCurrRecNo<FRecCount-1) then
          Inc(FCurrRecNo)
        else
          Result:=grEOF;
      gmPrior:
        if (FCurrRecNo>0) then
          Dec(FCurrRecNo)
        else
          result:=grBOF;
    end;
    if result=grOK then
      begin
      MDSReadRecord(Buffer, FCurrRecNo);
      PFPCPSRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=FCurrRecNo;
      PFPCPSRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag:=bfCurrent;
      if (Filtered) then
        Accepted:=MDSFilterRecord(Buffer) //Filtering
      else
        Accepted:=True;
      if (GetMode=gmCurrent) and not Accepted then
        result:=grError;
      end;
  until (result<>grOK) or Accepted;
end;

function TFPCPSMemDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
 SrcBuffer: TRecordBuffer;
 I: integer;
begin
 I:= Field.FieldNo - 1;
 result:= (I >= 0) and MDSGetActiveBuffer(SrcBuffer) and 
          not getfieldisnull(pointer(srcbuffer),I);
 if result and (buffer <> nil) then 
   begin
   Move(GetRecordBufferPointer((SrcBuffer),getintegerpointer(ffieldoffsets,I)^)^, Buffer^,GetIntegerPointer(FFieldSizes, I)^);
   end;
end;

procedure TFPCPSMemDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
 DestBuffer: TRecordBuffer;
 I,J: integer;

begin
 I:= Field.FieldNo - 1;
 if (I >= 0) and  MDSGetActiveBuffer(DestBuffer) then 
   begin
   if State in [dsEdit, dsInsert, dsNewValue] then
      Field.Validate(Buffer);
   if buffer = nil then 
     setfieldisnull(pointer(destbuffer),I)
   else 
     begin 
     unsetfieldisnull(pointer(destbuffer),I);
     J:=GetIntegerPointer(FFieldSizes, I)^;
     if Field.DataType=ftString then
       Dec(J); // Do not move terminating 0, which is in the size.
     Move(Buffer^,GetRecordBufferPointer((DestBuffer), getIntegerPointer(FFieldOffsets, I)^)^,J);
     dataevent(defieldchange,ptrint(field));
     end;
   end;
end;

function TFPCPSMemDataset.GetRecordSize: Word;

begin
 Result:= FRecSize;
end;

procedure TFPCPSMemDataset.InternalGotoBookmark(ABookmark: Pointer);

var
  ReqBookmark: integer;

begin
  ReqBookmark:=PInteger(ABookmark)^;
  if (ReqBookmark>=0) and (ReqBookmark<FRecCount) then
    FCurrRecNo:=ReqBookmark
  else
    RaiseError(SErrBookMarkNotFound,[ReqBookmark]);
end;

procedure TFPCPSMemDataset.InternalSetToRecord(Buffer: TRecordBuffer);

var
  ReqBookmark: integer;

begin
  ReqBookmark:=PFPCPSRecInfo(Buffer+FRecInfoOffset)^.Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

function TFPCPSMemDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;

begin
  Result:=PFPCPSRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag;
end;

procedure TFPCPSMemDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);

begin
  PFPCPSRecInfo(Buffer+FRecInfoOffset)^.BookmarkFlag := Value;
end;

procedure TFPCPSMemDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);

begin
  if Data<>nil then
    PInteger(Data)^:=PFPCPSRecInfo(Buffer+FRecInfoOffset)^.Bookmark;
end;

procedure TFPCPSMemDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);

begin
  if Data<>nil then
    PFPCPSRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=PInteger(Data)^
  else
    PFPCPSRecInfo(Buffer+FRecInfoOffset)^.Bookmark:=0;
end;

function TFPCPSMemDataset.MDSFilterRecord(Buffer: TRecordBuffer): Boolean;

var
  SaveState: TDatasetState;

begin
  Result:=True;
  if not Assigned(OnFilterRecord) then
    Exit;
  SaveState:=SetTempState(dsFilter);
  Try
    FFilterBuffer:=Buffer;
    OnFilterRecord(Self,Result);
  Finally  
    RestoreState(SaveState);
  end;  
end;

Function TFPCPSMemDataset.DataSize : Integer;

begin
  Result:=FStream.Size;
end;

procedure TFPCPSMemDataset.Clear;

begin
  Clear(True);
end;

procedure TFPCPSMemDataset.Clear(ClearDefs : Boolean);

begin
  FStream.Clear;
  FRecCount:=0;
  FCurrRecNo:=-1;
  if Active then
    Resync([]);
  If ClearDefs then
    begin
    Close;
    FieldDefs.Clear;
    end;
end;

procedure tFPCPSmemdataset.calcrecordlayout;
var
  i,count : integer;
begin
 Count := fielddefs.count;
 // Avoid mem-leak if CreateTable is called twice
 FreeMem(ffieldoffsets);
 Freemem(ffieldsizes);
 {$IFDEF FPC}
 FFieldOffsets:=getmem(Count*sizeof(integer));
 FFieldSizes:=getmem(Count*sizeof(integer));
 {$ELSE}
 getmem(FFieldOffsets, Count*sizeof(integer));
 getmem(FFieldSizes, Count*sizeof(integer));
 {$ENDIF}
 FRecSize:= (Count+7) div 8; //null mask
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
 FRecSize:=Align(FRecSize,4);
{$ENDIF}
 for i:= 0 to Count-1 do
   begin
   GetIntegerPointer(ffieldoffsets, i)^ := frecsize;
   GetIntegerPointer(ffieldsizes,   i)^ := MDSGetbufferSize(i+1);
   FRecSize:= FRecSize+GetIntegerPointeR(FFieldSizes, i)^;
   end;
end;

procedure TFPCPSMemDataset.CreateTable;

begin
  CheckInactive;
  FStream.Clear;
  FRecCount:=0;
  FCurrRecNo:=-1;
  FIsOpen:=False;
  calcrecordlayout;
  FRecInfoOffset:=FRecSize;
  FRecSize:=FRecSize+SizeRecInfo;
  FRecBufferSize:=FRecSize;
  FTableIsCreated:=True;
end;

procedure TFPCPSMemDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);

begin
  MDSAppendRecord(ActiveBuffer);
  InternalLast;
  Inc(FRecCount);
end;

procedure TFPCPSMemDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value>=1) and (Value<=FRecCount) then
    begin
    FCurrRecNo:=Value-1;
    Resync([]);
    end;
end;

Function TFPCPSMemDataset.GetRecNo: Longint;

begin
  UpdateCursorPos;
  if (FCurrRecNo<0) then
    Result:=1
  else
    Result:=FCurrRecNo+1;
end;

Function TFPCPSMemDataset.GetRecordCount: Longint;

begin
  CheckActive;
  Result:=FRecCount;
end;

Procedure TFPCPSMemDataset.CopyFromDataset(DataSet : TDataSet);

begin
  CopyFromDataset(Dataset,True);
end;

Procedure TFPCPSMemDataset.CopyFromDataset(DataSet : TDataSet; CopyData : Boolean);

Var
  I  : Integer;
  F,F1,F2 : TField;
  L1,L2  : TList;
  N : String;

begin
  Clear(True);
  // NOT from fielddefs. The data may not be available in buffers !!
  For I:=0 to Dataset.FieldCount-1 do
    begin
    F:=Dataset.Fields[I];
    TFieldDef.Create(FieldDefs,F.FieldName,F.DataType,F.Size,F.Required,F.FieldNo);
    end;
  CreateTable;
  If CopyData then
    begin
    Open;
    L1:=TList.Create;
    Try
      L2:=TList.Create;
      Try
        For I:=0 to FieldDefs.Count-1 do
          begin
          N:=FieldDefs[I].Name;
          F1:=FieldByName(N);
          F2:=DataSet.FieldByName(N);
          L1.Add(F1);
          L2.Add(F2);
          end;
        Dataset.DisableControls;
        Try
          Dataset.Open;
          While not Dataset.EOF do
            begin
            Append;
            For I:=0 to L1.Count-1 do
              begin
              F1:=TField(L1[i]);
              F2:=TField(L2[I]);
              Case F1.DataType of
                ftFixedChar,
                ftString   : F1.AsString:=F2.AsString;
                ftBoolean  : F1.AsBoolean:=F2.AsBoolean;
                ftFloat    : F1.AsFloat:=F2.AsFloat;
                ftLargeInt : F1.AsInteger:=F2.AsInteger;
                ftSmallInt : F1.AsInteger:=F2.AsInteger;
                ftInteger  : F1.AsInteger:=F2.AsInteger;
                ftDate     : F1.AsDateTime:=F2.AsDateTime;
                ftTime     : F1.AsDateTime:=F2.AsDateTime;
                ftDateTime : F1.AsDateTime:=F2.AsDateTime;
                else         F1.AsString:=F2.AsString;
              end;
              end;
            Try
              Post;
            except
              Cancel;
              Raise;
            end;
            Dataset.Next;
            end;
        Finally
          Dataset.EnableControls;
        end;
      finally
        L2.Free;
      end;
    finally
      l1.Free;
    end;
    end;
end;

function TFPCPSMemDataset.GetRecordBufferPointer(p:TRecordBuffer; Pos:Integer):TRecordBuffer;
begin
  Result:=p;
  inc(Result, Pos);
end;

function TFPCPSMemDataset.GetIntegerPointer(p:PInteger; Pos:Integer):PInteger;
begin
  Result:=p;
  inc(Result, Pos);
end;

end.
