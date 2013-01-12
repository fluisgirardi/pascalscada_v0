program Project1;

{$mode objfpc}{$H+}

uses
  Classes,
  LCLType,
  SysUtils,
  CustApp,
  reswriter,
  bitmapresource,
  versionresource,
  resource;
type

  { TResMaker }

  TResMaker = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TResMaker }

procedure TResMaker.DoRun;
var
  ErrorMsg: String;

  colection:TResources;
  item:TBitmapResource;
  version:TVersionResource;
  st:TMemoryStream;
  c:integer;
  vname:TResourceDesc;
  fname:string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') or (Paramcount<2) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  TResources.RegisterWriter('.dcr',TResResourceWriter);

  colection := TResources.Create;
  st:=TMemoryStream.Create;

  for c:=2 to Paramcount do begin
    try
      if not FileExists(ParamStr(c)) then
        continue;

      fname := ExtractFileName(ParamStr(c));
      fname := LeftStr(fname,Length(fname)-Length(ExtractFileExt(fname)));
      vname := TResourceDesc.Create(fname);

      st.Clear;
      st.LoadFromFile(ParamStr(c));

      item := TBitmapResource.Create(nil,vname);
      item.RawData.Write((PChar (st.Memory) + sizeof (TBitmapFileHeader))^, st.Size - sizeof (TBitmapFileHeader));
      colection.Add(item);
    except
    end;
  end;
  Vname:=TResourceDesc.Create('1');
  version:=TVersionResource.Create(nil, vname);
  version.LangID:=1046;

  colection.Add(version);

  colection.WriteToFile(ParamStr(1));

  // stop program loop
  Terminate;
end;

constructor TResMaker.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TResMaker.Destroy;
begin
  inherited Destroy;
end;

procedure TResMaker.WriteHelp;
begin
  WriteLn('Create resource file to be used on Delphi');
  WriteLn('ONLY Bitmap files are accepted.');
  WriteLn('Usage:');
  WriteLn(ExeName,' outfile.dcr inputfile1.bmp inputfile2.bmp ... inputfileX.bpm');
end;

var
  Application: TResMaker;

begin
  Application:=TResMaker.Create(nil);
  Application.Title:='ResMaker';
  Application.Run;
  Application.Free;
end.

