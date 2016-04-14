{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o componente de escalonamento linear.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements a linear scale.)
}
{$ENDIF}
unit LinearScaleProcessor;

interface

uses
  SysUtils, Classes, ValueProcessor;

type

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Componente de escalas lineares.
  @seealso(TPIPE)
  @seealso(TScaleProcessor)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Linear scale component.
  @seealso(TPIPE)
  @seealso(TScaleProcessor)
  }
  {$ENDIF}
  TLinearScaleProcessor = class(TScaleProcessor)
  private
    function GetSysMin:Double;
    function GetSysMax:Double;
    function GetPLCMin:Double;
    function GetPLCMax:Double;
    procedure SetSysMin(v:double);
    procedure SetSysMax(v:double);
    procedure SetPLCMin(v:double);
    procedure SetPLCMax(v:double);
  protected
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;

    {$IFDEF PORTUGUES}
    {:
    Pega um valor na escala do CLP e converte para a escala do sistema.
    @param(sender Objeto que chamou a função.)
    @param(Entrada Valor do dispositivo a ser convertido.)
    @returns(O valor convertido para a escala do sistema)
    }
    {$ELSE}
    {:
    Convert a value from the device scale to the system scale.
    @param(sender Object that calls the convertion.)
    @param(Entrada Device value to be converted.)
    @returns(The value converted to the system Scale.)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Entrada:Double):Double; override;

    {$IFDEF PORTUGUES}
    {:
    Pega um valor na escala do sistema e converte para a escala do CLP.
    @param(sender Objeto que chamou a função.)
    @param(Entrada Valor do sistema a ser convertido.)
    @returns(O valor convertido para a escala do CLP)
    }
    {$ELSE}
    {:
    Convert a value from the device scale to the system scale.
    @param(sender Object that calls the convertion.)
    @param(Entrada Device value to be converted.)
    @returns(The value converted to the system Scale.)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Saida:Double):Double; override;
  published
    {$IFDEF PORTUGUES}
    //: Valor minimo de escala do sistema (Saida).
    {$ELSE}
    //: Minimum value of the system scale (output).
    {$ENDIF}
    property SysMin:Double read GetSysMin write SetSysMin Stored true;

    {$IFDEF PORTUGUES}
    //: Valor máximo da escala do dispositivo (entrada).
    {$ELSE}
    //: Maximum value of the device (PLC) scale (input).
    {$ENDIF}
    property SysMax:Double read GetSysMax write SetSysMax Stored true;

    {$IFDEF PORTUGUES}
    //: Valor mínimo da escala do dispositivo (entrada).
    {$ELSE}
    //: Minimum value of the device (PLC) scale (input).
    {$ENDIF}
    property PLCMin:Double read GetPLCMin write SetPLCMin Stored true;

    {$IFDEF PORTUGUES}
    //: Valor máximo da escala do dispositivo (entrada).
    {$ELSE}
    //: Maximum value of the device (PLC) scale (input).
    {$ENDIF}
    property PLCMax:Double read GetPLCMax write SetPLCMax Stored true;
  end;

implementation

uses hsstrings;

constructor TLinearScaleProcessor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then begin
    FProperts[0] := 0;
    FProperts[1] := 100;
    FProperts[2] := 0;
    FProperts[3] := 32000;
  end else begin
    FProperts[0] := 0;
    FProperts[1] := 0;
    FProperts[2] := 0;
    FProperts[3] := 0;
  end;
end;

function TLinearScaleProcessor.GetSysMin:Double;
begin
  Result := FProperts[0];
end;

function TLinearScaleProcessor.GetSysMax:Double;
begin
  Result := FProperts[1];
end;

function TLinearScaleProcessor.GetPLCMin:Double;
begin
  Result := FProperts[2];
end;

function TLinearScaleProcessor.GetPLCMax:Double;
begin
  Result := FProperts[3];
end;

procedure TLinearScaleProcessor.SetSysMin(v:double);
begin
  FProperts[0] := v;
end;

procedure TLinearScaleProcessor.SetSysMax(v:double);
begin
  FProperts[1] := v;
end;

procedure TLinearScaleProcessor.SetPLCMin(v:double);
begin
  FProperts[2] := v;
end;

procedure TLinearScaleProcessor.SetPLCMax(v:double);
begin
  FProperts[3] := v;
end;

function  TLinearScaleProcessor.SetInGetOut(Sender:TComponent; Entrada:Double):Double;
var
  divisor:Double;
begin
  divisor := (FProperts[3]-FProperts[2]);
  if divisor=0 then divisor:=1;
  Result := (Entrada-FProperts[2])*(FProperts[1]-FProperts[0])/divisor+FProperts[0];
end;

function TLinearScaleProcessor.SetOutGetIn(Sender:TComponent; Saida:Double):Double;
var
  divisor:Double;
begin
  divisor := (FProperts[1]-FProperts[0]);
  if divisor=0 then divisor:=1;
  Result := (Saida-FProperts[0])*(FProperts[3]-FProperts[2])/divisor+FProperts[2];
end;

procedure TLinearScaleProcessor.Loaded;
begin
  inherited Loaded;
  if (FProperts[0]=FProperts[1]) or (FProperts[2]=FProperts[3]) then
    raise Exception.Create(SinvalidValue);
end;

end.
