{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Editor da propriedade ServerCertificate do TS7CommPlusDriver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O botão "..." do Object Inspector abre um diálogo de arquivo (.pem, .crt, .cer, .der),
  converte o que há nele (S7PlusCertificate) e grava na propriedade a string base64 do DER.
  O valor não é digitado: mostra só um resumo. Com um certificado já configurado o diálogo
  pergunta se é para trocá-lo ou removê-lo (propriedade vazia = aceita qualquer certificado).
}
{$ELSE}
{:
  @abstract(Editor of the ServerCertificate property of TS7CommPlusDriver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The "..." button of the Object Inspector opens a file dialog (.pem, .crt, .cer, .der),
  converts what is in it (S7PlusCertificate) and stores the base64 string of the DER in the
  property. The value is not typed: only a summary is shown. With a certificate already set
  the dialog asks whether to replace or remove it (empty property = accepts any certificate).
}
{$ENDIF}
unit s7pluscertpropedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type
  { TS7PlusCertificatePropertyEditor }

  TS7PlusCertificatePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure Edit; override;
  end;

{$IFDEF PORTUGUES}
//: Lê um arquivo de certificado e devolve a string base64 da propriedade; '' (e Count=0) se o
//: arquivo não tem certificado. Levanta exceção se o arquivo não puder ser lido.
{$ELSE}
//: Reads a certificate file and gives the base64 string of the property; '' (and Count=0) when
//: the file holds no certificate. Raises if the file cannot be read.
{$ENDIF}
function S7PlusCertificateFromFile(const AFileName:String; out Count:Integer):String;

implementation

uses
  Dialogs, Controls, hsstrings, S7PlusCertificate;

function S7PlusCertificateFromFile(const AFileName:String; out Count:Integer):String;
var
  FS:TFileStream;
  Data:TBytes;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Data, FS.Size);
    if Length(Data)>0 then
      FS.ReadBuffer(Data[0], Length(Data));
  finally
    FS.Free;
  end;
  Result := S7PlusCertFileToBase64(Data, Count);
end;

{ TS7PlusCertificatePropertyEditor }

function TS7PlusCertificatePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paRevertable];
end;

function TS7PlusCertificatePropertyEditor.GetValue: AnsiString;
var
  Certs:TS7PlusCertDERArray;
  i, Total:Integer;
  B64:AnsiString;
begin
  B64 := GetStrValue;
  if B64='' then begin
    Result := SS7PCertEmpty;
    exit;
  end;
  if not S7PlusCertBase64ToDER(B64, Certs) then begin
    Result := SS7PCertInvalidSummary;
    exit;
  end;
  Total := 0;
  for i:=0 to High(Certs) do
    Inc(Total, Length(Certs[i]));
  Result := Format(SS7PCertSummary, [Length(Certs), Total]);
end;

procedure TS7PlusCertificatePropertyEditor.Edit;
var
  Dlg:TOpenDialog;
  Answer, Count:Integer;
  B64:String;
begin
  if GetStrValue<>'' then begin
    Answer := QuestionDlg(SS7PCertTitle, SS7PCertAsk, mtConfirmation,
                          [mrYes, SS7PCertReplace, mrNo, SS7PCertRemove, mrCancel, SS7PCertCancel], 0);
    if Answer=mrNo then begin
      SetStrValue('');
      exit;
    end;
    if Answer<>mrYes then exit;
  end;

  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Title := SS7PCertTitle;
    Dlg.Filter := SS7PCertFilter;
    Dlg.Options := Dlg.Options+[ofFileMustExist];
    if not Dlg.Execute then exit;

    try
      B64 := S7PlusCertificateFromFile(Dlg.FileName, Count);
    except
      on E:Exception do begin
        MessageDlg(SS7PCertTitle, Format(SS7PCertCannotRead, [E.Message]), mtError, [mbOK], 0);
        exit;
      end;
    end;
    if (B64='') or (Count<=0) then begin
      MessageDlg(SS7PCertTitle, SS7PCertNotACertificate, mtError, [mbOK], 0);
      exit;
    end;
    SetStrValue(B64);
    MessageDlg(SS7PCertTitle, Format(SS7PCertLoaded, [Count]), mtInformation, [mbOK], 0);
  finally
    Dlg.Free;
  end;
end;

end.
