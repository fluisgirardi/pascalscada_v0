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

implementation

uses
  Classes, SysUtils, westasciidriver, ProtocolTypes, PLCTagNumber,
  uwesttagbuilder, Controls, Dialogs;

procedure OpenTagEditor(aProtocolDriver, aOwnerOfNewTags: TComponent;
  InsertHook: TAddTagInEditorHook; CreateProc: TCreateTagProc);
var
  tplc:TPLCTagNumber;
  ctrl, variable:LongInt;
  frm:TWestTagBuilder;
  sctrl,
  formatmask:AnsiString;
begin
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
            tplc.ProtocolDriver := TWestASCIIDriver(aProtocolDriver);
            InsertHook(tplc);
          end;
        end;
      end;
    end;
  finally
    frm.Destroy;
  end;
end;

initialization

  SetTagBuilderToolForWest6100Protocol(@OpenTagEditor);

end.

