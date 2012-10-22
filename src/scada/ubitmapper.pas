{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Unit do assistente de mapeamento de bits.
{$ELSE}
//: Unit of BitMapper wizard.
{$ENDIF}
unit ubitmapper;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources,{$ENDIF} SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, Grids, StdCtrls;

type
  TfrmBitMapper = class(TForm)
    StringGrid1: TStringGrid;
    Label1: TLabel;
    eachbitastag: TCheckBox;
    edtNamepattern: TEdit;
    Label2: TLabel;
    bitnamestartsfrom1: TCheckBox;
    bytenamestartsfrom1: TCheckBox;
    Wordnamestartsfrom1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBitMapper: TfrmBitMapper;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R ubitmapper.lfm}
  {$IFEND}
{$ENDIF}

procedure TfrmBitMapper.FormShow(Sender: TObject);
var
  c:Integer;
begin
  eachbitastag.Checked:=true;
  for c:=31 downto 0 do
    StringGrid1.Cols[c].Strings[0]:=IntToStr(31-c);
end;

procedure TfrmBitMapper.Button2Click(Sender: TObject);
begin
  //
end;

procedure TfrmBitMapper.Button1Click(Sender: TObject);
begin
  //
end;

procedure TfrmBitMapper.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$I ubitmapper.lrs}
  {$IFEND}
{$ENDIF}
end.