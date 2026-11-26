{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(TpSCADALogMemo: o destino de tela do TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um memo somente leitura que recebe, na thread principal e em lotes, as
  linhas que as regras do logger mandam para a tela (ltScreen). Guarda no
  maximo MaxLines linhas (as mais antigas saem) e, com AutoScroll, mantem a
  ultima linha visivel.
}
{$ELSE}
{:
  @abstract(TpSCADALogMemo: the screen target of TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A read only memo that gets, in the main thread and in batches, the lines the
  logger's rules send to the screen (ltScreen). It keeps at most MaxLines lines
  (the oldest ones go) and, with AutoScroll, keeps the last line in view.
}
{$ENDIF}
unit hmilogmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LazUTF8, pscadalog;

type

  { TpSCADALogMemo }

  {$IFDEF PORTUGUES}
  {:
  Memo que mostra as mensagens de um TpSCADALogger: ligue-o na propriedade
  Display do logger. Uma fonte monoespacada deixa as colunas alinhadas.
  }
  {$ELSE}
  {:
  Memo that shows the messages of a TpSCADALogger: link it to the logger's
  Display property. A monospaced font keeps the columns aligned.
  }
  {$ENDIF}
  TpSCADALogMemo = class(TCustomMemo, IpSCADALogDisplay)
  private
    FAutoScroll:Boolean;
    FMaxLines:Integer;
    procedure SetMaxLines(AValue:Integer);
    procedure TrimToMaxLines;
  public
    constructor Create(AOwner:TComponent); override;
    //: IpSCADALogDisplay: appends the lines (called by the logger in the main thread).
    procedure AddLogLines(ALines:TStrings);
    //: Scrolls to the last line.
    procedure ScrollToEnd;
  published
    //: Lines kept; 0 = no limit. The oldest lines are removed first.
    property MaxLines:Integer read FMaxLines write SetMaxLines default 1000;
    //: Keeps the last line in view when new lines come.
    property AutoScroll:Boolean read FAutoScroll write FAutoScroll default true;

    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly default true;
    property ScrollBars default ssAutoBoth;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap default false;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

{ TpSCADALogMemo }

constructor TpSCADALogMemo.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FMaxLines := 1000;
  FAutoScroll := true;
  ReadOnly := true;
  ScrollBars := ssAutoBoth;
  WordWrap := false;
end;

procedure TpSCADALogMemo.SetMaxLines(AValue:Integer);
begin
  if AValue<0 then AValue := 0;
  if FMaxLines=AValue then exit;
  FMaxLines := AValue;
  if [csLoading, csDesigning]*ComponentState=[] then begin
    Lines.BeginUpdate;
    try
      TrimToMaxLines;
    finally
      Lines.EndUpdate;
    end;
  end;
end;

procedure TpSCADALogMemo.TrimToMaxLines;
var
  excess:Integer;
begin
  if FMaxLines<=0 then exit;
  excess := Lines.Count-FMaxLines;
  while excess>0 do begin
    Lines.Delete(0);
    Dec(excess);
  end;
end;

procedure TpSCADALogMemo.AddLogLines(ALines:TStrings);
var
  i, first:Integer;
begin
  if (ALines=nil) or (ALines.Count=0) then exit;
  Lines.BeginUpdate;
  try
    if (FMaxLines>0) and (ALines.Count>=FMaxLines) then begin
      //the batch alone fills the memo: what is there goes away
      Lines.Clear;
      first := ALines.Count-FMaxLines;
      for i:=first to ALines.Count-1 do
        Lines.Add(ALines[i]);
    end else begin
      //makes room first, so the memo never holds more than MaxLines
      if FMaxLines>0 then begin
        first := Lines.Count+ALines.Count-FMaxLines;
        while first>0 do begin
          Lines.Delete(0);
          Dec(first);
        end;
      end;
      Lines.AddStrings(ALines);
    end;
  finally
    Lines.EndUpdate;
  end;
  if FAutoScroll then
    ScrollToEnd;
end;

procedure TpSCADALogMemo.ScrollToEnd;
begin
  if not HandleAllocated then exit;
  //SelStart counts characters, not bytes
  SelStart := UTF8Length(Text);
end;

end.
