{
 CREATED BY Fabio Luis Girardi
 papelhigienico at gmail dot com

 Based on LazRes tool from Lazarus Project.
}
unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, Buttons, LCLProc, Grids, ComCtrls, Menus, ExtCtrls,
  IniFiles
  {$IFDEF UNIX}, unixutil{$ENDIF};

type

  TResourceFileRec = record
     ResourceName:String;
     FileName:String;
  end;
  TResourceList = array of TResourceFileRec;

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    DlgOpen: TOpenDialog;
    DlgSave: TSaveDialog;
    SelectFiles: TOpenDialog;
    Panel1: TPanel;
    SelectDir: TSelectDirectoryDialog;
    StringGrid1: TStringGrid;
    subdirs: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FActiveFile:String;
    oldSubDirValue:String;
    adddirmask:String;
    List:TStringList;
    function  StreamIsFormInTextFormat(Stream: TMemoryStream): boolean;
    procedure ConvertFormToText(Stream: TMemoryStream);
    function  StreamIsFormInFCLFormat(Stream: TMemoryStream): boolean;
    function  AlreadAdded(FileName:String):boolean;
    procedure finder(path, pattern:String; addSub:boolean);
    function  ExpandList:TResourceList;
    function  ResourceExists(const aList:TResourceList; const ResourceName:String):Boolean;
    procedure AddToList(var aList:TResourceList; const FileName, ResourceName:String);
    function  ExtractResName(FileName:String):String;
    procedure Save(fname:String);
    procedure Load(fname:String);
  end; 

var
  Form1: TForm1;

implementation

{ LazRes functions and procedures}

function TForm1.StreamIsFormInTextFormat(Stream: TMemoryStream): boolean;
const
  FormTextStart = 'object ';
var s: string;
  OldPos: integer;
begin
  SetLength(s,length(FormTextStart));
  OldPos:=Stream.Position;
  Stream.Read(s[1],length(s));
  Result:=AnsiCompareText(s,FormTextStart)=0;
  Stream.Position:=OldPos;
end;

function TForm1.StreamIsFormInFCLFormat(Stream: TMemoryStream): boolean;
const
  FormFCLStart = 'TPF0';
var s: string;
  OldPos: integer;
begin
  SetLength(s,length(FormFCLStart));
  OldPos:=Stream.Position;
  Stream.Read(s[1],length(s));
  Result:=s=FormFCLStart;
  Stream.Position:=OldPos;
end;

procedure TForm1.ConvertFormToText(Stream: TMemoryStream);
var TextStream: TMemoryStream;
begin
  try
    TextStream:=TMemoryStream.Create;
    FormDataToText(Stream,TextStream);
    TextStream.Position:=0;
    Stream.Clear;
    Stream.CopyFrom(TextStream,TextStream.Size);
    Stream.Position:=0;
  except
    on E: Exception do begin
      debugln('ERROR: unable to convert Delphi form to text: '+E.Message);
    end;
  end;
end;

{ Form functions and procedures  }

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
   c,r:Integer;
begin
   //add selected files to the resource list.
   if SelectFiles.Execute then
      for c:= 0 to SelectFiles.Files.Count-1 do
         if not AlreadAdded(SelectFiles.Files[c]) then begin
            r:=StringGrid1.RowCount;
            StringGrid1.RowCount:=StringGrid1.RowCount+1;
            StringGrid1.Rows[r].Strings[0] := 'F';
            StringGrid1.Rows[r].Strings[1] := ExtractResName(SelectFiles.Files[c]);
            StringGrid1.Rows[r].Strings[2] := '';
            StringGrid1.Rows[r].Strings[3] := '';
            StringGrid1.Rows[r].Strings[4] := SelectFiles.Files[c];
         end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  r:Integer;
begin
  if SelectDir.Execute then
    if not AlreadAdded(SelectDir.FileName) then begin
      r:=StringGrid1.RowCount;
      StringGrid1.RowCount:=StringGrid1.RowCount+1;
      StringGrid1.Rows[r].Strings[0] := 'D';
      StringGrid1.Rows[r].Strings[1] := '';
      StringGrid1.Rows[r].Strings[2] := 'No';
      if subdirs.Checked then
        StringGrid1.Rows[r].Strings[2] := 'Yes';
      StringGrid1.Rows[r].Strings[3] := adddirmask;
      StringGrid1.Rows[r].Strings[4] := SelectDir.FileName;
    end;
end;

procedure TForm1.MenuItem21Click(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if FileNameEdit1.Modified or StringGrid1.Modified then
    case MessageDlg('Save modifications?',mtConfirmation,mbYesNoCancel,0) of
      mrYes:
        MenuItem4Click(Sender);
      mrCancel:
        exit;
    end;

  FActiveFile:='';
  FileNameEdit1.Text:='';
  FileNameEdit1.Modified:=false;
  StringGrid1.RowCount:=1;
  StringGrid1.Modified:=false;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  if FileNameEdit1.Modified or StringGrid1.Modified then
    case MessageDlg('Save modifications?',mtConfirmation,mbYesNoCancel,0) of
      mrYes:
        MenuItem4Click(Sender);
      mrCancel:
        exit;
    end;

  if DlgOpen.Execute then begin
     FActiveFile:=DlgOpen.FileName;
     Load(FActiveFile);
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
   if FActiveFile='' then begin
     if not DlgSave.Execute then exit;
     FActiveFile:=DlgSave.FileName;
   end;
   Save(FActiveFile);
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  if DlgSave.Execute then begin
    FActiveFile:=DlgSave.FileName;
    Save(FActiveFile);
  end;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
var
   x:string;
begin
   x:=InputBox('Add directory','Filter files to add',adddirmask);
   if trim(x)<>'' then begin
      adddirmask := x;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FActiveFile:='';
{$IFDEF MSWINDOWS}
  adddirmask := '*.*';
{$ELSE}
  adddirmask := '*';
{$ENDIF}
end;

procedure TForm1.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  MessageDlg(E.Message,mtError,[mbOK],0);
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
   if MessageDlg('Are you sure?',mtConfirmation,mbYesNo,0)=mrYes then
      StringGrid1.RowCount:=1;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var
  ResourceFilename,FullResourceFilename,BinFilename,BinExt,ResourceName,ResourceType:String;
  a:integer;
  ResFileStream,BinFileStream:TFileStream;
  ResMemStream,BinMemStream:TMemoryStream;
  FileList:TResourceList;
  S: String;
begin
  if trim(FileNameEdit1.Text)='' then
    raise Exception.Create('Target resource filename is missing...') ;

  if StringGrid1.RowCount<=1 then
    raise Exception.Create('Cannot create resource. Please add at least one item!');

  FileList:=ExpandList;
  try
    ResourceFilename := ExtractFileName(FileNameEdit1.Text);
    FullResourceFilename := FileNameEdit1.Text;
    // check that all resources exists and are not the destination file
    for a:=0 to High(FileList) do begin
      S := FileList[a].FileName;
      if not FileExistsUTF8(S) then
        raise Exception.Create('ERROR: file not found: ' + S);

      if S = FullResourceFilename then
        raise Exception.Create('ERROR: resourcefilename = file');
    end;

    try
      ResFileStream:=TFileStream.Create(UTF8ToSys(FullResourceFilename),fmCreate);
    except
      raise Exception.Create('ERROR: unable to create file ''' + ResourceFilename + '''');
    end;

    ResMemStream:=TMemoryStream.Create;
    try
      for a:=0 to High(FileList) do begin
        BinFilename:=FileList[a].FileName;
        try
          BinFileStream:=TFileStream.Create(UTF8ToSys(BinFilename),fmOpenRead);
          BinMemStream:=TMemoryStream.Create;
          try
            BinMemStream.CopyFrom(BinFileStream,BinFileStream.Size);
            BinMemStream.Position:=0;
            BinExt:=uppercase(ExtractFileExt(BinFilename));
            ResourceName:=FileList[a].ResourceName;
            if (BinExt='.LFM') or (BinExt='.DFM') or (BinExt='.XFM')
            then begin
              ResourceType:='FORMDATA';
              ConvertFormToText(BinMemStream);
              //ResourceName:=FindLFMClassName(BinMemStream);
              //if ResourceName='' then
                //raise Exception.Create('ERROR: no resourcename');

              LFMtoLRSstream(BinMemStream,ResMemStream);
            end
            else begin
              ResourceType:=copy(BinExt,2,length(BinExt)-1);;

              if ResourceName='' then
                raise Exception.Create('ERROR: no resourcename');

              BinaryToLazarusResourceCode(BinMemStream,ResMemStream
                 ,ResourceName,ResourceType);
            end;
          finally
            BinFileStream.Free;
            BinMemStream.Free;
          end;
        except
          raise Exception.Create('ERROR: unable to read file ''' + BinFilename + '''');
        end;
      end;
      ResMemStream.Position:=0;
      ResFileStream.CopyFrom(ResMemStream,ResMemStream.Size);
    finally
      ResMemStream.Free;
      ResFileStream.Free;
    end;
  finally
    SetLength(FileList,0);
  end;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
var
   r:Integer;
begin
   for r:=StringGrid1.RowCount-1 downto 1 do
      if ((StringGrid1.Cells[0,r]='F') and (not FileExistsUTF8(StringGrid1.Cells[4,r]))) or
         ((StringGrid1.Cells[0,r]='D') and (not DirectoryExistsUTF8(StringGrid1.Cells[4,r]))) then
         StringGrid1.DeleteColRow(false,r);
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
   if MessageDlg('Delete row?',mtConfirmation,mbYesNo,0)=mrYes then
     StringGrid1.DeleteColRow(false,StringGrid1.Row);
end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
begin

end;

procedure TForm1.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  oldSubDirValue:=Value;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
   case aCol of
      0,4:
        CanSelect:=true;
      1:
        CanSelect:=(StringGrid1.Cells[0,aRow]='F');
      2, 3:
        CanSelect:=(StringGrid1.Cells[0,aRow]='D');
   end;
end;

procedure TForm1.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
   case aCol of
      0,4:
        Editor.Enabled:=false;
      1:
        Editor.Enabled:=(StringGrid1.Cells[0,aRow]='F');
      2, 3: begin
        Editor.Enabled:=(StringGrid1.Cells[0,aRow]='D');
      end;
   end;
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  if (StringGrid1.Cells[0,ARow]='D') and (ACol=2) and (value<>'Yes') and (value<>'No') then
     StringGrid1.Cells[ACol,aRow]:=oldSubDirValue;
end;

function TForm1.AlreadAdded(FileName:String):boolean;
var
   r:Integer;
begin
   Result := false;
   for r:=1 to StringGrid1.RowCount-1 do
      if FileName=StringGrid1.Rows[r].Strings[4] then begin
         Result := true;
         exit;
      end;
end;

procedure TForm1.finder(path, pattern:String; addSub:boolean);
var
  info:TSearchRec;
  dirpattern:String;
begin
  {$IFDEF MSWINDOWS}
  dirpattern := pattern;
  {$ELSE}
  dirpattern := '*';
  {$ENDIF}

  if path[Length(path)]<>DirectorySeparator then
    path := path + DirectorySeparator;

  if FindFirst(path+dirpattern, faAnyFile, info)=0 then begin
    repeat
       if ((info.Attr and faDirectory) = faDirectory) then begin
         if addSub and (info.Name<>'.') and (info.Name<>'..') then begin
           finder(path+info.Name+DirectorySeparator, pattern, addSub);
         end;
       end else begin
         {$IFDEF UNIX}
         if FNMatch(pattern,info.Name) then
         {$ENDIF}
           List.Add(path+info.Name);
       end;
    until FindNext(info)<>0;
    FindClose(info);
  end;
end;

function TForm1.ExpandList:TResourceList;
var
  row, res, litem:Integer;
  fname, resname:String;
begin
  SetLength(Result,0);
  for row:=1 to StringGrid1.RowCount-1 do begin
    if StringGrid1.Cells[0,row]='F' then begin

      //check if resource has a name...
      if Trim(StringGrid1.Cells[1,row])='' then begin
        if MessageDlg('Resource with empty name... '+LineEnding+
                      'Filename: '+StringGrid1.Cells[4,row]+LineEnding+
                      'Added as single file'+LineEnding+LineEnding+
                      'What I have to do?',
                      mtError,
                      [mbIgnore,mbAbort],
                      0)=mrAbort then
           raise Exception.Create('Aborted by user!');
        //if ignored, continue on next file....
        continue;
      end;

      //check if another resource with the same name already exists...
      if ResourceExists(Result,StringGrid1.Cells[1,row]) then begin
        if MessageDlg('Another resource with the same name exists... '+LineEnding+
                      'Filename: '+StringGrid1.Cells[4,row]+LineEnding+
                      'Resourcename: '+StringGrid1.Cells[1,row]+LineEnding+
                      'Added as single file'+LineEnding+LineEnding+
                      'What I have to do?',
                      mtError,
                      [mbIgnore,mbAbort],
                      0)=mrAbort then
           raise Exception.Create('Aborted by user!');
        //if ignored, continue on next file....
        continue;
      end;

      AddToList(Result,StringGrid1.Cells[4,row],StringGrid1.Cells[1,row]);
    end else begin
      List:=TStringList.Create;
      List.Clear;

      with StringGrid1 do
        Finder(Cells[4,row],Cells[3,row],UpperCase(Cells[2,row])='YES');
      List.SaveToFile('/home/fabiolg/lista.txt');

      try
        for litem:=0 to List.Count-1 do begin
          fname   := List.Strings[litem];
          resname := ExtractResName(fname);

          //check if resource has a name...
          if Trim(resname)='' then begin
            if MessageDlg('Resource with empty name... '+LineEnding+
                          'Filename: '+fname+LineEnding+
                          'Added as content of folder '+StringGrid1.Cells[4,row]+LineEnding+LineEnding+
                          'What I have to do?',
                          mtError,
                          [mbIgnore,mbAbort],
                          0)=mrAbort then
               raise Exception.Create('Aborted by user!');
            //if ignored, continue on next file....
            continue;
          end;

          //check if another resource with the same name already exists...
          if ResourceExists(Result,resname) then begin
            if MessageDlg('Another resource with the same name exists... '+LineEnding+
                          'Filename: '+fname+LineEnding+
                          'Resourcename: '+resname+LineEnding+
                          'Added as content of folder '+StringGrid1.Cells[4,row]+LineEnding+LineEnding+
                          'What I have to do?',
                          mtError,
                          [mbIgnore,mbAbort],
                          0)=mrAbort then
               raise Exception.Create('Aborted by user!');
            //if ignored, continue on next file....
            continue;
          end;
          AddToList(Result,fname,resname);
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

function  TForm1.ResourceExists(const aList:TResourceList; const ResourceName:String):Boolean;
var
  c:Integer;
begin
  Result := false;

  for c:=0 to High(alist) do
     if aList[c].ResourceName=ResourceName then begin
        Result := true;
        exit;
     end;
end;

procedure TForm1.AddToList(var aList:TResourceList; const FileName, ResourceName:String);
var
  h:Integer;
begin
  h:=Length(aList);
  SetLength(aList,h+1);
  aList[h].FileName:=FileName;
  aList[h].ResourceName:=ResourceName;
end;

function TForm1.ExtractResName(FileName:String):String;
var
  BinFileStream:TFileStream;
  BinMemStream:TMemoryStream;
  BinExt:String;
begin
  BinExt:=uppercase(ExtractFileExt(Filename));
  if (BinExt='.LFM') or (BinExt='.DFM') or (BinExt='.XFM') then begin
    try
      BinFileStream:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
      BinMemStream:=TMemoryStream.Create;

      BinMemStream.CopyFrom(BinFileStream,BinFileStream.Size);
      BinMemStream.Position:=0;

      ConvertFormToText(BinMemStream);
      Result:=FindLFMClassName(BinMemStream);
      if Result='' then
         Result := ExtractFileNameOnly(FileName);
    finally
      BinFileStream.Free;
      BinMemStream.Free;
    end;
  end else
    Result := ExtractFileNameOnly(FileName);
end;

procedure TForm1.Save(fname:String);
var
  reg:TIniFile;
  r:Integer;
  section:String;
begin
  try
    reg:=TIniFile.Create(fname);
    reg.WriteString('ResInfo','Target', FileNameEdit1.Text);
    reg.WriteInteger('ResInfo','ResCount',StringGrid1.RowCount);

    for r:=1 to StringGrid1.RowCount-1 do begin
      section:='ResID'+IntToStr(r);
      reg.WriteString(section,'Type',    StringGrid1.Cells[0,r]);
      reg.WriteString(section,'ResName', StringGrid1.Cells[1,r]);
      reg.WriteString(section,'SubDir',  StringGrid1.Cells[2,r]);
      reg.WriteString(section,'Pattern', StringGrid1.Cells[3,r]);
      reg.WriteString(section,'Filename',StringGrid1.Cells[4,r]);
    end;
    StringGrid1.Modified:=false;
    FileNameEdit1.Modified:=false;
  finally
    reg.Free;
  end;
end;

procedure TForm1.Load(fname:String);
var
  reg:TIniFile;
  r:Integer;
  section:String;
begin
  try
    reg:=TIniFile.Create(fname);
    FileNameEdit1.Text := reg.ReadString('ResInfo','Target', '');
    StringGrid1.RowCount := reg.ReadInteger('ResInfo','ResCount', 1);

    for r:=1 to StringGrid1.RowCount-1 do begin
      section:='ResID'+IntToStr(r);
      StringGrid1.Cells[0,r] := reg.ReadString(section,'Type',    '');
      StringGrid1.Cells[1,r] := reg.ReadString(section,'ResName', '');
      StringGrid1.Cells[2,r] := reg.ReadString(section,'SubDir',  '');
      StringGrid1.Cells[3,r] := reg.ReadString(section,'Pattern', '');
      StringGrid1.Cells[4,r] := reg.ReadString(section,'Filename','');
    end;
    StringGrid1.Modified:=false;
    FileNameEdit1.Modified:=false;
  finally
    reg.Free;
  end;
end;

initialization
  {$I umain.lrs}

end.

