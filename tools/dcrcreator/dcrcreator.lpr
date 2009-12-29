program dcrcreator;

uses
  Classes,
  sysutils,
  reswriter,
  bitmapresource,
  resource;

procedure WriteHelp;
begin
  WriteLn('Create resource file to be used on Delphi');
  WriteLn('ONLY Bitmap files are accepted.');
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)),' outfile.res inputfile1.bmp inputfile2.bmp ... inputfileX.bpm');
end;

var
   colection:TResources;
   item:TBitmapResource;
   st,outfile:TFileStream;
   c:integer;
   vname:TResourceDesc;
   fname:string;
begin

   if Paramcount<2 then begin
     WriteHelp;
     exit;
   end;

   TResources.RegisterWriter('.dcr',TResResourceWriter);

   colection := TResources.Create;

   for c:=2 to Paramcount do begin
     try
       if not FileExists(ParamStr(c)) then
         continue;

       st:=TFileStream.Create(ParamStr(c),fmOpenRead);

       fname := ExtractFileName(ParamStr(c));
       fname := LeftStr(fname,Length(fname)-Length(ExtractFileExt(fname)));
       vname := TResourceDesc.Create(fname);

       item := TBitmapResource.Create(nil,vname);
       item.SetCustomRawDataStream(st);
       colection.Add(item);
     except
     end;
   end;
   colection.WriteToFile(ParamStr(1))
end.

