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
    FSysMin,
    FSysMax,
    FRawMin,
    FRawMax,
    FSysMinLoaded,
    FSysMaxLoaded,
    FRawMinLoaded,
    FRawMaxLoaded:Double;
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
  FSysMin := 0;
  FSysMax := 100;
  FRawMin := 0;
  FRawMax := 32000;
end;

function TLinearScaleProcessor.GetSysMin:Double;
begin
  Result := FSysMin;
end;

function TLinearScaleProcessor.GetSysMax:Double;
begin
  Result := FSysMax;
end;

function TLinearScaleProcessor.GetPLCMin:Double;
begin
  Result := FRawMin;
end;

function TLinearScaleProcessor.GetPLCMax:Double;
begin
  Result := FRawMax;
end;

procedure TLinearScaleProcessor.SetSysMin(v:double);
begin
  if [csLoading,csReading]*ComponentState=[] then
    FSysMin := v
  else
    FSysMinLoaded := v;
end;

procedure TLinearScaleProcessor.SetSysMax(v:double);
begin
  if [csLoading,csReading]*ComponentState=[] then
    FSysMax := v
  else
    FSysMaxLoaded := v;
end;

procedure TLinearScaleProcessor.SetPLCMin(v:double);
begin
  if [csLoading,csReading]*ComponentState=[] then
    FRawMin := v
  else
    FRawMinLoaded := v;
end;

procedure TLinearScaleProcessor.SetPLCMax(v:double);
begin
  if [csLoading,csReading]*ComponentState=[] then
    FRawMax := v
  else
    FRawMaxLoaded := v;
end;

function  TLinearScaleProcessor.SetInGetOut(Sender:TComponent; Entrada:Double):Double;
var
  divisor:Double;
begin
  divisor := (FRawMax-FRawMin);
  if divisor=0 then divisor:=1;
  Result := (Entrada-FRawMin)*(FSysMax-FSysMin)/divisor+FSysMin;
end;

function TLinearScaleProcessor.SetOutGetIn(Sender:TComponent; Saida:Double):Double;
var
  divisor:Double;
begin
  divisor := (FSysMax-FSysMin);
  if divisor=0 then divisor:=1;
  Result := (Saida-FSysMin)*(FRawMax-FRawMin)/divisor+FRawMin;
end;

procedure TLinearScaleProcessor.Loaded;
begin
  inherited Loaded;

  FSysMin := FSysMinLoaded;
  FSysMax := FSysMaxLoaded;
  FRawMin := FRawMinLoaded;
  FRawMax := FRawMaxLoaded;

  //if (FSysMin=FSysMax) or (FRawMin=FRawMax) then
  //  raise Exception.Create(SinvalidValue);
end;

end.
