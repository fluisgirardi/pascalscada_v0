{:
  @abstract(Unit do formulário assistente de criação de estruturas do tag TPLCStruct.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}

unit ustructuremapper;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, us7tagbuilder;

type
  {:
  Assistente de criação de estruturas usando o tag TPLCStruct.

  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  }
  TfrmStructureEditor = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    ScrollBox1: TScrollBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FTagList:TList;
    FItemId:Integer;
    procedure CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
    procedure btnUpClick(Sender:TObject);
    procedure btnDownClick(Sender:TObject);
    procedure btnDelClick(Sender:TObject);
    procedure btnBitsClick(Sender:TObject);
    procedure BitItemDeleted(Sender:TObject);
  public
    { Public declarations }
  end;

var
  frmStructureEditor: TfrmStructureEditor;

implementation

uses Tag;

{$R *.dfm}

procedure TfrmStructureEditor.Button1Click(Sender: TObject);
var
  tag, lastitem:TS7TagItemEditor;
begin
  tag:=TS7TagItemEditor.Create(Self);
  tag.Parent := ScrollBox1;
  tag.PopulateCombo;
  tag.OnCheckNames:=CheckNames;
  tag.OnUpClick:=btnUpClick;
  tag.OnDownClickEvent:=btnDownClick;
  tag.OnDelClickEvent:=btnDelClick;
  tag.OnBitsClickEvent:=btnBitsClick;
  tag.OnDelBitItem:=BitItemDeleted;
  tag.TagScan:=1000;
  tag.TagType:=pttDefault;
  tag.SwapBytes:=false;
  tag.SwapWords:=false;
  if FTagList.Count>0 then begin
    lastitem:=TS7TagItemEditor(FTagList.Items[FTagList.Count-1]);
    tag.Top:=lastitem.Top+lastitem.Height;
  end else begin
    tag.Top:=0;
  end;

  while not tag.AcceptName('StructItem'+IntToStr(FItemId)) do
    inc(FItemId);
  tag.TagName:='StructItem'+IntToStr(FItemId);  

  FTagList.Add(tag);
end;

procedure TfrmStructureEditor.BitBtn2Click(Sender: TObject);
begin
  //
end;

procedure TfrmStructureEditor.BitBtn1Click(Sender: TObject);
begin
  //
end;

procedure TfrmStructureEditor.FormCreate(Sender: TObject);
begin
  FItemId:=1;
  FTagList:=TList.Create;
end;

procedure TfrmStructureEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FTagList.Destroy
end;

procedure TfrmStructureEditor.FormShow(Sender: TObject);
begin
  //
end;

procedure TfrmStructureEditor.CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
begin
  //
end;

procedure TfrmStructureEditor.btnUpClick(Sender:TObject);
begin
  //
end;

procedure TfrmStructureEditor.btnDownClick(Sender:TObject);
begin
  //
end;

procedure TfrmStructureEditor.btnDelClick(Sender:TObject);
begin
  //
end;

procedure TfrmStructureEditor.btnBitsClick(Sender:TObject);
begin
  //
end;

procedure TfrmStructureEditor.BitItemDeleted(Sender:TObject);
begin
  //
end;

end.
