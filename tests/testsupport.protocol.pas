{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Apoio aos testes: montagem dos registros que os drivers de
            protocolo recebem - TTagRec e TIOPacket.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os dois sao records com muitos campos, e um campo esquecido vira lixo de
  pilha que faz o teste passar ou falhar por motivo errado. Aqui eles saem
  sempre completamente preenchidos.
}
{$ELSE}
{:
  @abstract(Test support: builds the records the protocol drivers take -
            TTagRec and TIOPacket.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Both are records with many fields, and a forgotten field becomes stack
  garbage that makes the test pass or fail for the wrong reason. Here they
  always come out fully filled in.
}
{$ENDIF}
unit testsupport.protocol;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commtypes, Tag;

{$IFDEF PORTUGUES}
{:
Monta um TTagRec completo. Os campos que os drivers de protocolo nao usam
(Rack, Slot, SubElement, ...) ficam zerados.
@param(aStation Endereco do escravo.)
@param(aReadFunction Funcao de leitura, 0 quando o pedido e' de escrita.)
@param(aWriteFunction Funcao de escrita, 0 quando o pedido e' de leitura.)
@param(aAddress Endereco inicial.)
@param(aSize Quantidade de itens - bits, words, registros.)
}
{$ELSE}
{:
Builds a complete TTagRec. The fields the protocol drivers do not use (Rack,
Slot, SubElement, ...) are zeroed.
@param(aStation Slave address.)
@param(aReadFunction Read function, 0 when the request is a write.)
@param(aWriteFunction Write function, 0 when the request is a read.)
@param(aAddress Start address.)
@param(aSize Item count - bits, words, registers.)
}
{$ENDIF}
function TagRecFor(aStation, aReadFunction, aWriteFunction, aAddress, aSize:LongInt):TTagRec;

{$IFDEF PORTUGUES}
{:
Monta o TIOPacket como ele chega no DecodePkg depois de uma troca bem
sucedida: aSent e' o frame que o driver mandou, aReceived o que voltou.
}
{$ELSE}
{:
Builds the TIOPacket as DecodePkg gets it after a successful exchange: aSent
is the frame the driver sent, aReceived what came back.
}
{$ENDIF}
function IOPacketFor(const aSent, aReceived:BYTES):TIOPacket;

implementation

function TagRecFor(aStation, aReadFunction, aWriteFunction, aAddress, aSize:LongInt):TTagRec;
begin
  Result.ID            := 0;
  Result.Rack          := 0;
  Result.Slot          := 0;
  Result.Station       := aStation;
  Result.File_DB       := 0;
  Result.Address       := aAddress;
  Result.SubElement    := 0;
  Result.Size          := aSize;
  Result.Count         := aSize;
  Result.OffSet        := 0;
  Result.RealOffset    := 0;
  Result.Path          := '';
  Result.ReadFunction  := aReadFunction;
  Result.WriteFunction := aWriteFunction;
  Result.Retries       := 0;
  Result.UpdateTime    := 1000;
  Result.CallBack      := nil;
end;

function IOPacketFor(const aSent, aReceived:BYTES):TIOPacket;
begin
  Result.PacketID            := 1;

  Result.WriteIOResult       := iorOK;
  Result.ToWrite             := Length(aSent);
  Result.Written             := Length(aSent);
  Result.WriteRetries        := 0;
  Result.BufferToWrite       := Copy(aSent, 0, Length(aSent));

  Result.DelayBetweenCommand := 0;

  Result.ReadIOResult        := iorOK;
  Result.ToRead              := Length(aReceived);
  Result.Received            := Length(aReceived);
  Result.ReadRetries         := 0;
  Result.BufferToRead        := Copy(aReceived, 0, Length(aReceived));

  Result.Res1                := nil;
  Result.Res2                := nil;
end;

end.
