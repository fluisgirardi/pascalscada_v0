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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, us7tagbuilder
  {$IFDEF FPC}, LCLIntf, LResources{$ENDIF}
  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}, windows{$IFEND};

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
    procedure Timer1Timer(Sender: TObject);
  private
    FTagList,
    ItemsToDel:TList;
    FItemId:Integer;
    procedure CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
    procedure btnUpClick(Sender:TObject);
    procedure btnDownClick(Sender:TObject);
    procedure btnDelClick(Sender:TObject);
    procedure btnBitsClick(Sender:TObject);
    procedure BitItemDeleted(Sender:TObject);
    function  GetStructItemsCount:Integer;
    function  GetStructItem(index:Integer):TS7TagItemEditor;
  public
    function HasAtLeastOneValidItem:Boolean;
    property StructItemsCount:Integer read GetStructItemsCount;
    property StructItem[index:integer]:TS7TagItemEditor read GetStructItem;
  end;

var
  frmStructureEditor: TfrmStructureEditor;

implementation

uses Tag, ubitmapper, hsstrings;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
  {$R ustructuremapper.lfm}
  {$IFEND}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

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
  if not HasAtLeastOneValidItem then
    Raise Exception.Create(SYouMustHaveAtLeastOneStructureItem);
end;

procedure TfrmStructureEditor.FormCreate(Sender: TObject);
begin
  FItemId:=1;
  FTagList:=TList.Create;
  ItemsToDel:=TList.Create;
end;

procedure TfrmStructureEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FTagList.Destroy;
  ItemsToDel.Destroy;
end;

procedure TfrmStructureEditor.FormShow(Sender: TObject);
begin
  //
end;

procedure TfrmStructureEditor.CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
var
  t,b:Integer;
begin
  for t:=0 to FTagList.Count-1 do begin
    if FTagList.Items[t]=Sender then continue;
    if TS7TagItemEditor(FTagList.Items[t]).TagName=NewName then begin
      AcceptNewName:=false;
      exit;
    end;
    for b:=0 to TS7TagItemEditor(FTagList.Items[t]).BitCount-1 do begin
      if TS7TagItemEditor(FTagList.Items[t]).Bit[b]=Sender then continue;
      if TTagBitItemEditor(TS7TagItemEditor(FTagList.Items[t]).Bit[b]).TagName=NewName then begin
        AcceptNewName:=false;
        exit;
      end;
    end;
  end;
end;

procedure TfrmStructureEditor.btnUpClick(Sender:TObject);
var
  idx:Integer;
  priortop, actualTop:Integer;
  prior:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := FTagList.IndexOf(Sender);
  if idx>0 then begin
    prior:=TS7TagItemEditor(FTagList.Items[idx-1]);

    priortop:=prior.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    FTagList.Exchange(idx-1, idx);

    (Sender as TS7TagItemEditor).Top:=priortop;
    (Sender as TS7TagItemEditor).TabOrder:=prior.TabOrder;
    prior.Top:=actualTop;
  end;
end;

procedure TfrmStructureEditor.btnDownClick(Sender:TObject);
var
  idx:Integer;
  nexttop, actualTop:Integer;
  next:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := FTagList.IndexOf(Sender);
  if (idx<>-1) and (idx<(FTagList.Count-1)) then begin
    next:=TS7TagItemEditor(FTagList.Items[idx+1]);

    nexttop:=next.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    FTagList.Exchange(idx+1, idx);

    (Sender as TS7TagItemEditor).Top:=nexttop;
    next.TabOrder:=(Sender as TS7TagItemEditor).TabOrder;
    next.Top:=actualTop;
  end;
end;

procedure TfrmStructureEditor.btnDelClick(Sender:TObject);
begin
  if ItemsToDel.IndexOf(Sender)=-1 then
    if MessageDlg('Remove the structure item called "'+TS7TagItemEditor(Sender).TagName+'"?', mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
      ItemsToDel.Add(Sender);
      Timer1.Enabled:=true;
    end;
end;

procedure TfrmStructureEditor.btnBitsClick(Sender:TObject);
var
  frmbit:TfrmBitMapper;
  ti:TTagBitItemEditor;
  s7tageditor:TS7TagItemEditor;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:Integer;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if frmbit.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if frmbit.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if frmbit.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:String;
  var
    n:String;
  begin
    n:=IntToStr(bitnum);
    Result:=frmbit.edtNamepattern.Text;
    Result := StringReplace(Result,'%b',n,[rfReplaceAll]);

    n:=IntToStr(bytenum);
    Result := StringReplace(Result,'%B',n,[rfReplaceAll]);

    n:=IntToStr(wordnum);
    Result := StringReplace(Result,'%w',n,[rfReplaceAll]);

    n:=(Sender as TS7TagItemEditor).TagName;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  s7tageditor := (Sender as TS7TagItemEditor);

  frmbit:=TfrmBitMapper.Create(Self);
  try
    if frmbit.ShowModal=mrOk then begin
      startbit:=31-frmbit.StringGrid1.Selection.Right;
      endbit:=31-frmbit.StringGrid1.Selection.Left;
      curbit:=startbit;
      if frmbit.eachbitastag.Checked then begin
        while curbit<=endbit do begin
          updatenumbers;
          ti:=s7tageditor.AddBit;
          ti.TagName:=GetNewTagBitName;
          ti.EndBit:=curbit;
          ti.StartBit:=curbit;
          inc(curbit);
        end;
      end else begin
        updatenumbers;
        ti:=s7tageditor.AddBit;
        ti.TagName:=GetNewTagBitName;
        ti.EndBit:=endbit;
        ti.StartBit:=startbit;
      end;
    end;
  finally
    frmbit.Destroy;
  end;
end;

procedure TfrmStructureEditor.BitItemDeleted(Sender:TObject);
begin
  //
end;

function  TfrmStructureEditor.GetStructItemsCount:Integer;
begin
  Result:=FTagList.Count;
end;

function  TfrmStructureEditor.GetStructItem(index:Integer):TS7TagItemEditor;
begin
  Result:=TS7TagItemEditor(FTagList.Items[index]);
end;

function TfrmStructureEditor.HasAtLeastOneValidItem:Boolean;
var
  c:Integer;
begin
  Result:=false;
  for c:=0 to StructItemsCount-1 do
    if not StructItem[c].SkipTag then begin
      Result:=true;
      break;
    end;
end;

procedure TfrmStructureEditor.Timer1Timer(Sender: TObject);
var
  c:Integer;
begin
  for c:=ItemsToDel.Count-1 downto 0 do begin
    FTagList.Remove(ItemsToDel.Items[c]);
    TS7TagItemEditor(ItemsToDel.Items[c]).Destroy;
    ItemsToDel.Delete(c);
  end;
  Timer1.Enabled:=false;
  if FTagList.Count=0 then FItemId:=1;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$i ustructuremapper.lrs}
  {$IFEND}
{$ENDIF}

end.