{:
Implementa um controle em forma de ScrollBar para a leitura/escrita de valores
em tags numéricos.
}

unit HMIScrollBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Forms, Controls, Graphics,
  Dialogs, StdCtrls, HMITypes, PLCTag, hsutils, ProtocolTypes, Tag;

type
  {:
  Implementa um controle em forma de ScrollBar para a leitura/escrita de valores
  em tags numéricos.
  }
  THMIScrollBar = class(TScrollBar, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FUpdateOnMove:Boolean;
    FBusy:Boolean;
    FCmdCount:Integer;
    FLastPosition:Integer;
    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    procedure WriteValue(Value:Integer);

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); override;
    {$IF (not defined(WIN32)) and (not defined(WIN64))}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFEND}
  public
    //: @exclude
    destructor Destroy; override;
  published
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
    {:
    Caso @true, escreve seu valor para o tag ainda quando está sendo movido.
    Caso @false, escreve seu valor para o tag somente quando é solto.
    }
    property UpdateOnMove:Boolean read FUpdateOnMove write FUpdateOnMove default false;
  end;

implementation

destructor THMIScrollBar.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   inherited Destroy;
end;

procedure THMIScrollBar.RefreshHMISecurity;
begin

end;

procedure THMIScrollBar.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   if (t<>nil) and ((t as ITagNumeric)=nil) then
      raise Exception.Create('Somente tags numéricos são aceitos!');

   //se ja estou associado a um tag, remove
   if FTag<>nil then begin
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   end;

   //adiona o callback para o novo tag
   if t<>nil then begin
      t.AddCallBacks(Self as IHMITagInterface);
      FTag := t;
      NotifyTagChange(self);
   end;
   FTag := t;
end;

function  THMIScrollBar.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function  THMIScrollBar.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIScrollBar.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
end;

procedure THMIScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
var
   WriteFlag:Boolean;
begin
   WriteFlag:=false;
   Try
      FLastPosition:=ScrollPos;

      if (ScrollCode=scEndScroll) then begin
{$IF defined(WIN32) or defined(WIN64)}
         FBusy:=false;
         FCmdCount:=0;
         WriteFlag:=true;
{$IFEND}
      end else begin
         inc(FCmdCount);
         if FCmdCount>5 then begin
            if FUpdateOnMove then
               WriteFlag:=true;
            FCmdCount:=0;
         end;
      end;
      if WriteFlag then
         WriteValue(ScrollPos);
      
   finally
      inherited Scroll(ScrollCode, ScrollPos);
   end;
end;

{$IF (not defined(WIN32)) and (not defined(WIN64))}
procedure THMIScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    WriteValue(FLastPosition);
  finally
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;
{$IFEND}

procedure THMIScrollBar.WriteValue(Value:Integer);
begin
   if (FTag=nil)  then exit;

   if (FTag as ITagNumeric)<>nil then
      (FTag as ITagNumeric).Value:=Value;
end;

procedure THMIScrollBar.NotifyReadOk;
begin

end;

procedure THMIScrollBar.NotifyReadFault;
begin

end;

procedure THMIScrollBar.NotifyWriteOk;
begin

end;

procedure THMIScrollBar.NotifyWriteFault;
begin

end;

procedure THMIScrollBar.NotifyTagChange(Sender:TObject);
begin
   if not FBusy then begin
      if (FTag=nil) then exit;

      if (FTag as ITagNumeric)<>nil then
         Position := FloatToInteger((FTag as ITagNumeric).Value);
   end;
end;

procedure THMIScrollBar.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
