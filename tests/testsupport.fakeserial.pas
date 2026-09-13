{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Par de pseudo-terminais, para servir de porta serial de mentira.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A porta serial e' a unica da biblioteca presa a um dispositivo do sistema
  operacional, e nao ha' porta falsa que substitua o caminho de abrir, ler e
  escrever. Um par de pseudo-terminais resolve: o lado escravo aparece em
  /dev/pts como um dispositivo de caractere que aceita os mesmos ajustes de
  termios de uma serial, e o lado mestre fica com os testes, para mandar o que
  o equipamento responderia e conferir o que o driver escreveu.

  So' existe em Unix. No Windows seria preciso um par de portas virtuais
  instalado no sistema, que nao ha' como supor.
}
{$ELSE}
{:
  @abstract(A pseudo terminal pair, to stand in for a serial port.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The serial port is the only one in the library tied to an operating system
  device, and no fake port replaces the open, read and write path. A pseudo
  terminal pair solves it: the slave side shows up under /dev/pts as a
  character device taking the same termios settings a serial port does, and
  the master side stays with the tests, to send what the device would answer
  and to check what the driver wrote.

  Unix only. On Windows it would take a virtual port pair installed on the
  system, which cannot be assumed.
}
{$ENDIF}
unit testsupport.fakeserial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commtypes
  {$IFDEF UNIX}, BaseUnix, Unix, termio{$ENDIF};

type

  { TSerialDeMentira }

  TSerialDeMentira = class
  private
    FMestre:LongInt;
    FCaminhoEscravo:String;
  public
    constructor Create;
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Verdadeiro se o par pode ser criado neste sistema.
    {$ELSE}
    //: True if the pair can be created on this system.
    {$ENDIF}
    class function Disponivel:Boolean;

    {$IFDEF PORTUGUES}
    //: Manda bytes como se fossem do equipamento.
    {$ELSE}
    //: Sends bytes as if they came from the device.
    {$ENDIF}
    procedure Responder(const aBytes:BYTES);

    {$IFDEF PORTUGUES}
    //: Le o que o driver escreveu, esperando ate' aPrazoMs.
    {$ELSE}
    //: Reads what the driver wrote, waiting up to aPrazoMs.
    {$ENDIF}
    function  LerOQueFoiEscrito(aQuantos, aPrazoMs:LongInt):BYTES;

    {$IFDEF PORTUGUES}
    //: Diretorio onde o lado escravo aparece, com a barra final.
    {$ELSE}
    //: Directory where the slave side shows up, with the trailing slash.
    {$ENDIF}
    function  Diretorio:String;

    {$IFDEF PORTUGUES}
    //: Nome do dispositivo do lado escravo, sem o diretorio.
    {$ELSE}
    //: Device name of the slave side, without the directory.
    {$ENDIF}
    function  NomeDoDispositivo:String;

    {$IFDEF PORTUGUES}
    {:
    Os ajustes que o driver de fato aplicou ao dispositivo. O par de
    pseudo-terminais compartilha um unico termios, entao ler pelo lado mestre
    devolve o que foi configurado no lado escravo.
    }
    {$ELSE}
    {:
    The settings the driver actually applied to the device. The pseudo terminal
    pair shares a single termios, so reading from the master side gives back
    what was configured on the slave side.
    }
    {$ENDIF}
    {$IFDEF PORTUGUES}
    //  ATENCAO: um pseudo-terminal nao tem UART. O nucleo aceita a velocidade e
    //  os bits de parada, mas forca CS8 e desliga a paridade, seja qual for o
    //  pedido - conferido com um programa que ajusta o termios direto, sem a
    //  biblioteca no meio. Por isso bits de dados e paridade nao sao medidos
    //  aqui: o que se veria seria o limite do pseudo-terminal, nao o que o
    //  driver pediu.
    {$ELSE}
    //  WARNING: a pseudo terminal has no UART. The kernel takes the speed and
    //  the stop bits, but forces CS8 and clears parity whatever is asked -
    //  checked with a program setting termios directly, with no library in
    //  between. That is why character size and parity are not measured here:
    //  what one would see is the pseudo terminal's limit, not what the driver
    //  asked for.
    {$ENDIF}
    function  CodigoDeVelocidade:Cardinal;
    function  TemDoisBitsDeParada:Boolean;

    //: caminho inteiro do lado escravo
    property  CaminhoEscravo:String read FCaminhoEscravo;
  end;

implementation

{$IFDEF UNIX}
function posix_openpt(oflag:LongInt):LongInt; cdecl; external 'c' name 'posix_openpt';
function grantpt(fd:LongInt):LongInt;         cdecl; external 'c' name 'grantpt';
function unlockpt(fd:LongInt):LongInt;        cdecl; external 'c' name 'unlockpt';
function ptsname(fd:LongInt):PChar;           cdecl; external 'c' name 'ptsname';
//nao ha' declaracao pascal desta na RTL, nem em linux nem em freebsd. O
//parametro e' ponteiro para a estrutura: declara-lo como registro devolve lixo.
//there is no pascal declaration for this in the RTL, neither on linux nor on
//freebsd. The parameter is a pointer to the struct: declaring it as a record
//gives back garbage.
function cfgetospeed(t:Pointer):Cardinal; cdecl; external 'c' name 'cfgetospeed';
{$ENDIF}

constructor TSerialDeMentira.Create;
{$IFDEF UNIX}
var
  nome:PChar;
{$ENDIF}
begin
  inherited Create;
  FMestre:=-1;
  FCaminhoEscravo:='';

  {$IFDEF UNIX}
  FMestre:=posix_openpt(O_RDWR or O_NOCTTY);
  if FMestre<0 then
    raise Exception.Create('serial de mentira: nao consegui abrir o pseudo-terminal');

  if (grantpt(FMestre)<>0) or (unlockpt(FMestre)<>0) then begin
    FpClose(FMestre);
    FMestre:=-1;
    raise Exception.Create('serial de mentira: nao consegui liberar o pseudo-terminal');
  end;

  nome:=ptsname(FMestre);
  if nome=nil then begin
    FpClose(FMestre);
    FMestre:=-1;
    raise Exception.Create('serial de mentira: nao descobri o nome do lado escravo');
  end;
  FCaminhoEscravo:=StrPas(nome);

  //o lado mestre nao pode travar o teste esperando dado que nao vem
  FpFcntl(FMestre, F_SETFL, FpFcntl(FMestre, F_GETFL) or O_NONBLOCK);
  {$ENDIF}
end;

destructor TSerialDeMentira.Destroy;
begin
  {$IFDEF UNIX}
  if FMestre>=0 then
    FpClose(FMestre);
  {$ENDIF}
  inherited Destroy;
end;

class function TSerialDeMentira.Disponivel:Boolean;
begin
  {$IFDEF UNIX}
  Result:=true;
  {$ELSE}
  Result:=false;
  {$ENDIF}
end;

procedure TSerialDeMentira.Responder(const aBytes:BYTES);
begin
  {$IFDEF UNIX}
  if (FMestre>=0) and (Length(aBytes)>0) then
    FpWrite(FMestre, aBytes[0], Length(aBytes));
  {$ENDIF}
end;

function TSerialDeMentira.LerOQueFoiEscrito(aQuantos, aPrazoMs:LongInt):BYTES;
{$IFDEF UNIX}
var
  buf:array[0..1023] of Byte;
  lidos, total, gasto:LongInt;
{$ENDIF}
begin
  Result:=nil;
  {$IFDEF UNIX}
  if FMestre<0 then exit;

  total:=0;
  gasto:=0;
  SetLength(Result, aQuantos);
  while (total<aQuantos) and (gasto<aPrazoMs) do begin
    lidos:=FpRead(FMestre, buf[0], aQuantos-total);
    if lidos>0 then begin
      Move(buf[0], Result[total], lidos);
      inc(total, lidos);
    end else begin
      Sleep(5);
      inc(gasto, 5);
    end;
  end;
  SetLength(Result, total);
  {$ENDIF}
end;

{$IFDEF UNIX}
function TSerialDeMentira.CodigoDeVelocidade:Cardinal;
var
  t:termios;
begin
  Result:=0;
  if (FMestre>=0) and (tcgetattr(FMestre, t)=0) then
    Result:=cfgetospeed(@t);
end;

function TSerialDeMentira.TemDoisBitsDeParada:Boolean;
var
  t:termios;
begin
  Result:=false;
  if (FMestre>=0) and (tcgetattr(FMestre, t)=0) then
    Result:=(t.c_cflag and CSTOPB)<>0;
end;
{$ELSE}
function TSerialDeMentira.CodigoDeVelocidade:Cardinal;   begin Result:=0;     end;
function TSerialDeMentira.TemDoisBitsDeParada:Boolean;   begin Result:=false; end;
{$ENDIF}

function TSerialDeMentira.Diretorio:String;
begin
  Result:=ExtractFilePath(FCaminhoEscravo);
end;

function TSerialDeMentira.NomeDoDispositivo:String;
begin
  Result:=ExtractFileName(FCaminhoEscravo);
end;

end.
