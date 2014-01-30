{$i ../common/language.inc}
{:
  @abstract(Implementation of TagAssistant for WestASCII.)
  @author(Juanjo Montero <juanjo.montero@gmail.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - New Unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
unit westasciitagassistant;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, westasciidriver, commontagassistant, ProtocolTypes;

type

  { TWestASCIITagAssistant }

  TWestASCIITagAssistant = class(TCommonTagAssistant)
  private
    FDriver: TWestASCIIDriver;
    public
      //: Opens the Tag Builder of the WestASCII protocol driver
      procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook;
        CreateProc:TCreateTagProc); override;

    published
      property Driver:TWestASCIIDriver read FDriver write FDriver;
end;

implementation

uses
  PLCTagNumber, uwesttagbuilder, Controls, hsstrings, Dialogs;

{ TWestASCIITagAssistant }

procedure TWestASCIITagAssistant.OpenTagEditor(OwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  tplc:TPLCTagNumber;
  ctrl, variable:LongInt;
  frm:TWestTagBuilder;
  sctrl,
  formatmask:String;
begin

  if not Assigned(FDriver) then
    begin
      ShowMessage(SDriverRequired);
      Exit;
    end;


  frm:=TWestTagBuilder.Create(nil);
  try
    if frm.ShowModal=mrOK then begin
      if frm.ZeroFill.Checked and (frm.AdrEnd.Value>9) then
        formatmask:='#00'
      else
        formatmask:='#0';

      for ctrl:=frm.AdrStart.Value to frm.AdrEnd.Value do begin
        sctrl:=FormatFloat(formatmask,ctrl);
        for variable:=0 to $1b do begin
          if frm.Variaveis[variable].Enabled.Checked then begin
            if Pos('%a',frm.Variaveis[variable].TagName.Text)=0 then begin
              frm.Variaveis[variable].TagName.Text := frm.Variaveis[variable].TagName.Text + '%a';
            end;
            tplc := TPLCTagNumber(CreateProc(TPLCTagNumber));
            tplc.Name:=StringReplace(frm.Variaveis[variable].TagName.Text,'%a',sctrl,[rfReplaceAll]);
            tplc.MemAddress := variable;
            tplc.PLCStation:=ctrl;
            tplc.RefreshTime:=frm.Variaveis[variable].Scan.Value;
            tplc.ProtocolDriver := FDriver;
            InsertHook(tplc);
          end;
        end;
      end;
    end;
  finally
    frm.Destroy;
  end;
end;

end.

