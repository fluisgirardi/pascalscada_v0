##### Introdução

![](img/THMIDBConnection.png) O `THMIDBConnection` é o componente de acesso a banco de dados do PascalSCADA: conecta a um SGBD e executa comandos SQL **de forma assíncrona**, sem travar a interface enquanto a consulta roda. Fica no pacote `pascalscada_db` (que não depende da LCL, então também funciona em aplicações de console) e é a base de componentes como o [`THMIEventLogger` e o `THMIAlarmLogger`](/pb/event-and-alarm-loggers/), além de servir para telas próprias — cadastro de usuários, receitas, histórico.

Por baixo, ele usa o **SQLdb** do Free Pascal (não mais o ZeosLib, removido como dependência a partir da versão 0.7.7 — veja [Como instalar](/pb/how-install-pascalscada/)). Isso significa que o `THMIDBConnection` já sai pronto para os bancos com conector SQLdb; não é um ORM nem esconde SQL de você — você escreve o comando e ele cuida da fila, da thread e do retorno do resultado.

##### Bancos de dados suportados

A propriedade **`Protocol`** escolhe o banco. No Object Inspector ela vem com uma lista suspensa fixa:

| Protocol | Conector SQLdb usado | Observação |
|---|---|---|
| postgresql | PostgreSQL | |
| sqlite | SQLite3 | `Database` é o caminho do arquivo `.db`/`.sqlite`. |
| mysql | MySQL 5.7 | Só o driver de fio do MySQL 5.7 é suportado no momento — mesmo em servidores mais novos, funciona pelo protocolo de compatibilidade. |
| firebird | Firebird | |

Valores herdados de instalações antigas com sufixo de versão (por exemplo `mysql-5.7`, formato que o ZeosLib usava) continuam funcionando: só a parte antes do traço é considerada para escolher o conector.

##### Propriedades de conexão

| Propriedade | Descrição |
|---|---|
| Protocol | Um dos quatro protocolos da tabela acima. |
| HostName | Endereço ou nome da máquina do banco. Em SQLite não é usado. |
| Port | Porta de conexão. `0` deixa a porta padrão do conector. |
| Database | Nome do banco (Postgres/MySQL/Firebird) ou caminho do arquivo (SQLite). |
| User, Password | Credenciais de acesso. |
| Catalog | Só tem efeito em bancos cujo conector SQLdb tem o conceito de catálogo; na maioria é ignorado. |
| Properties | Lista de `chave=valor` repassada direto para os parâmetros de conexão do SQLdb (por exemplo, opções de charset ou de SSL específicas do conector). |
| LibraryLocation | Caminho de uma biblioteca cliente nativa (`libpq.so`, `libmysqlclient.so`…) quando ela não está no caminho padrão do sistema. |
| Connected | Abre ou fecha a conexão. |
| ReadOnly | Ver seção própria abaixo. |

##### Como os comandos são executados

O `THMIDBConnection` não expõe um `TDataset` conectado ao vivo: ele enfileira comandos SQL numa thread própria (`TProcessSQLCommandThread`) que os executa **em ordem**, um de cada vez, sobre a mesma conexão. Isso evita duas telas concorrendo pela mesma conexão SQLdb (que não é thread-safe) e mantém a interface responsiva mesmo com uma consulta lenta.

Os dois métodos que colocam trabalho na fila:

```pascal
procedure ExecSQL(sql: UTF8String; ReturnDatasetCallback: TReturnDataSetProc;
                   ReturnSync: Boolean = true; NewConnection: Boolean = false);

procedure ExecTransaction(statements: THMIDBConnectionStatementList;
                   ReturnTransactionResult: TReturnTransactionStatementsProc;
                   FreeStatemensAfterExecute: Boolean;
                   ReturnSync: Boolean = true; NewConnection: Boolean = false);
```

* **`ExecSQL`** executa um único comando. Se ele começar com `SELECT`, o resultado chega pronto — um `TFPSBufDataSet` (um dataset em memória, veja abaixo) — em `ReturnDatasetCallback`; para `INSERT`/`UPDATE`/`DELETE`/DDL, passe `nil` no callback ou ignore o parâmetro do dataset. Cada `ExecSQL` roda dentro da sua própria transação, com commit automático ao final se não houver erro.
* **`ExecTransaction`** executa uma **lista** de comandos como uma única transação: se qualquer um falhar, todos são desfeitos. `FreeStatemensAfterExecute` decide se a lista de strings é liberada pelo componente ao terminar. O callback informa sucesso/falha, em qual linha da lista parou (`LineOfError`) e a exceção, se houve uma.
* **`ReturnSync`** decide **em qual thread** o seu callback roda: `True` (padrão) o traz de volta para a thread principal via `Synchronize` — seguro para tocar em controles visuais; `False` chama o callback **na própria thread de banco**, mais rápido mas só seguro para código que não mexe na interface.
* **`NewConnection`**: quando `True`, força fechar e reabrir a conexão antes de rodar o comando. Útil para isolar um comando de um estado de conexão suspeito, ao custo do tempo de reconectar.
* **`GetPendingSQLCommands`** informa quantos comandos ainda estão na fila — dá para usar como indicador visual de fila cheia numa tela que dispara muitas escritas.

O dataset que chega em `ReturnDatasetCallback` (assinatura `procedure(Sender: TObject; DS: TFPSBufDataSet; error: Exception)`) é criado exclusivamente para aquela chamada — você é dono dele a partir daí: ligue-o a um `TDataSource`/`TDBGrid` e libere-o (`DS.Free`) quando não precisar mais. Em caso de erro, `DS` vem `nil` e `error` traz a exceção original do SQLdb.

##### Acesso somente leitura

Com **`ReadOnly = True`**, todo comando que não seja um `SELECT` é recusado antes de chegar ao banco — o callback recebe erro, nada é executado. Use numa tela de consulta ou numa segunda instância de conexão que só deve exibir dados, nunca alterá-los.

##### Acesso síncrono direto

Para os casos em que você quer usar componentes de dados do SQLdb (`TSQLQuery`, `TDataSource`) ligados diretamente à conexão — um `DBGrid` editável, por exemplo — em vez de passar pela fila assíncrona, o `THMIDBConnection` expõe a conexão interna:

```pascal
function  GetSyncConnection: TSQLConnector;
procedure LockSyncConnection;
procedure UnlockSyncConnection;
```

`GetSyncConnection` devolve o `TSQLConnector` usado internamente. Como ele é compartilhado com a thread assíncrona, cerque qualquer uso direto com `LockSyncConnection`/`UnlockSyncConnection` (a mesma seção crítica que a fila usa) para não colidir com um `ExecSQL` em andamento.

##### Formatando literais SQL

Montar comandos SQL por concatenação de string é a porta de entrada clássica para injeção de SQL e para bugs de localidade (um `FloatToStr` que vira vírgula decimal no Brasil e quebra o `INSERT`). Para isso o `THMIDBConnection` tem quatro funções de classe — chamadas sem precisar de uma instância:

| Função | Faz |
|---|---|
| FormatSQLString(str, EmptyIsNull=False) | Envolve o texto em aspas simples e **dobra** cada aspa simples interna (`O'Brien` vira `'O''Brien'`), neutralizando tentativas de injeção. Com `EmptyIsNull = True`, uma string vazia vira o literal `NULL` em vez de `''`. |
| FormatSQLNumber(numero, casasDecimais=0) | Formata sempre com **ponto** como separador decimal, independente da localidade do sistema operacional. |
| FormatPGDatetime(dataHora) | Formata como `'AAAA-MM-DD HH:NN:SS.ZZZ'`, entre aspas — o literal de timestamp aceito pelo PostgreSQL (e pela maioria dos bancos), com milissegundos, sem depender da localidade. |
| FormatSQLUUID(uuid) | Formata um `TGuid` como literal de texto entre aspas, sem chaves — o formato que Postgres e a maioria dos bancos esperam para colunas UUID. |

```pascal
SQL := 'INSERT INTO eventos (mensagem, valor, quando, id) VALUES (' +
        THMIDBConnection.FormatSQLString(Mensagem) + ', ' +
        THMIDBConnection.FormatSQLNumber(Valor, 2) + ', ' +
        THMIDBConnection.FormatPGDatetime(Now) + ', ' +
        THMIDBConnection.FormatSQLUUID(NovoGuid) + ')';
DBConn.ExecSQL(SQL, nil, false);
```

##### Exemplo passo a passo

Gravar um evento de forma assíncrona, sem travar a tela:

1. Solte um `THMIDBConnection`: `Protocol = 'postgresql'`, `HostName`, `Database`, `User`, `Password`, `Connected = True`.
2. No clique de um botão (ou em qualquer evento):

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  SQL: String;
begin
  SQL := 'INSERT INTO eventos (mensagem, quando) VALUES (' +
          THMIDBConnection.FormatSQLString(EdMensagem.Text) + ', ' +
          THMIDBConnection.FormatPGDatetime(Now) + ')';
  HMIDBConnection1.ExecSQL(SQL, nil, false);
end;
```

3. Para uma consulta que devolve dados, implemente o callback:

```pascal
procedure TForm1.CarregarEventos;
begin
  HMIDBConnection1.ExecSQL('SELECT * FROM eventos ORDER BY quando DESC LIMIT 100',
                            @MostrarEventos, true);
end;

procedure TForm1.MostrarEventos(Sender: TObject; DS: TFPSBufDataSet; error: Exception);
begin
  if Assigned(error) then begin
    ShowMessage('Erro: ' + error.Message);
    Exit;
  end;
  DataSource1.DataSet := DS;   // DBGrid1.DataSource = DataSource1
end;
```

Como `ReturnSync = true`, o callback já chega na thread principal — pode ligar `DS` direto num `TDataSource` sem `Synchronize` extra.

##### Exemplos relacionados

* `examples/laz_hmialarms` — `THMIDBConnection` alimentando um `THMIAlarmLogger`, com `DBGrid` mostrando os alarmes ativos.
* `examples/laz_hmieventlogger` — `THMIDBConnection` com `THMIEventLogger`, gravando o histórico de eventos.
