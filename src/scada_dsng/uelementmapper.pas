{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Unit do assistente mapeador de elementos de bloco.
{$ELSE}
//: Unit of Block element mapper wizard.
{$ENDIF}
unit uelementmapper;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin{$IFDEF FPC}, LResources, LCLIntf{$ENDIF};

type
  TfrmMapElements = class(TForm)
    startindex: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    endindex: TSpinEdit;
    elementnames: TEdit;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    ElementsStartFromOne: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

var
  frmMapElements: TfrmMapElements;

implementation

uses hsstrings;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R uelementmapper.lfm}
  {$IFEND}
{$ENDIF}

procedure TfrmMapElements.FormShow(Sender: TObject);
begin
  //teste
end;

procedure TfrmMapElements.FormCreate(Sender: TObject);
begin
  //
end;

procedure TfrmMapElements.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  //
end;

procedure TfrmMapElements.Button2Click(Sender: TObject);
begin
  if endindex.Value<startindex.Value then
    raise Exception.Create(SStartMustBeLessThanEndIndex);
end;

procedure TfrmMapElements.Button1Click(Sender: TObject);
begin
  //
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$i uelementmapper.lrs}
  {$IFEND}
{$ENDIF}

end.
