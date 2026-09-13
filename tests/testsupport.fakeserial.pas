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
  {$IFDEF UNIX}, BaseUnix, Unix{$ENDIF};

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

    //: caminho inteiro do lado escravo
    property  CaminhoEscravo:String read FCaminhoEscravo;
  end;

implementation

{$IFDEF UNIX}
function posix_openpt(oflag:LongInt):LongInt; cdecl; external 'c' name 'posix_openpt';
function grantpt(fd:LongInt):LongInt;         cdecl; external 'c' name 'grantpt';
function unlockpt(fd:LongInt):LongInt;        cdecl; external 'c' name 'unlockpt';
function ptsname(fd:LongInt):PChar;           cdecl; external 'c' name 'ptsname';
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

function TSerialDeMentira.Diretorio:String;
begin
  Result:=ExtractFilePath(FCaminhoEscravo);
end;

function TSerialDeMentira.NomeDoDispositivo:String;
begin
  Result:=ExtractFileName(FCaminhoEscravo);
end;

end.
