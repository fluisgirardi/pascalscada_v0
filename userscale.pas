{:
  @abstract(Escala definida pelo usuário.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit UserScale;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, ValueProcessor;

type
   //: Evento chamado para processar valores.
   TScaleEvent = procedure(Sender:TObject; const Input:Double; var Output:Double) of object;
   {:
   @abstract(Classe para processamento de escalas personalizaveis pelo usuário.)
   @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
   }
   TUserScale = class(TScaleProcessor)
   private
     PPLCToUser:TScaleEvent;
     PUserToPLC:TScaleEvent;
   public
     {:
     @seealso(TScaleProcessor.SetInGetOut)
     @seealso(OnPLCToUser)
     }
     function SetInGetOut(Sender:TComponent; Entrada:Double):Double; override;
     {:
     @seealso(TScaleProcessor.SetOutGetIn)
     @seealso(OnUserToPLC)
     }
     function SetOutGetIn(Sender:TComponent; Saida:Double):Double; override;
   published
     {:
     Evento chamado através da função SetInGetOut para o usuário fazer sua escala
     customizada no sentido Equipamento -> Usuário.
     
     Os parametros do evento correspondem aos seguintes paramestros da função
     SetSetInGetOut:
     
     Sender => Sender da função
     
     Input  => Entrada da função
     
     Output => Resultado da função.
     
     }
     property OnPLCToUser:TScaleEvent read PPLCToUser write PPLCToUser;
     {:
     Evento chamado através da função SetOutGetIn para o usuário fazer sua escala
     customizada no sentido Usuário -> Equipamento.

     Os parametros do evento correspondem aos seguintes paramestros da função
     SetSetInGetOut:

     Sender => Sender da função

     Input  => Saida da função

     Output => Resultado da função.

     }
     property OnUserToPLC:TScaleEvent read PUserToPLC write PUserToPLC;
   end;

implementation

function TUserScale.SetInGetOut(Sender:TComponent; Entrada:Double):Double;
begin
  if Assigned(PPLCToUser) then begin
     Result := Entrada;
     PPLCToUser(Sender,Entrada,Result);
  end else
     Result := Entrada;
end;

function TUserScale.SetOutGetIn(Sender:TComponent; Saida:Double):Double;
begin
  if Assigned(PUserToPLC) then begin
     Result := Saida;
     PUserToPLC(Sender,Saida,Result);
  end else
     Result := Saida;
end;

end.
