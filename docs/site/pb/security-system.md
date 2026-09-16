##### Introdução

O sistema de segurança do PascalSCADA responde a uma pergunta simples: **o usuário logado pode mexer neste controle?** Ele tem três peças:

1. **Códigos de segurança** (`SecurityCode`) — textos livres que você atribui aos controles da [HCl](/pb/hmi-control-library-hcl/) e às *actions* de segurança: `"setpoints"`, `"manutencao"`, `"receitas"`. Um controle sem código é livre para todos.
2. **O gerenciador de segurança** (`TControlSecurityManager`) — um objeto único por aplicação, criado automaticamente (você o acessa com `GetControlSecurityManager`). Todos os controles com `SecurityCode` se registram nele; a cada login e logout ele pergunta, para cada código, se o usuário atual tem acesso e habilita ou desabilita os controles.
3. **Um componente de gerenciamento de usuários** (paleta **PascalSCADA User Management**) — quem de fato sabe quais usuários existem, valida senha e diz quais códigos cada um pode acessar. Só pode haver **um** por aplicação (o segundo levanta exceção ao ser criado). Há três implementações, descritas abaixo.

Sem componente de gerenciamento de usuários, o gerenciador de segurança responde *sim* a tudo: os `SecurityCode` não têm efeito e nada fica desabilitado. Isso permite montar as telas primeiro e ligar a segurança depois.

##### Como funciona

```
THMIEdit.SecurityCode = "setpoints"
        │ registra o código
        ▼
TControlSecurityManager ──CanAccess("setpoints")?──▶ TxxxUserManagement
        │                                                    │
        ◀──────────────── True / False ──────────────────────┘
        ▼
THMIEdit.Enabled := (resposta) and (Enabled que você definiu)
```

* Ao atribuir um `SecurityCode`, o controle chama `ValidateSecurityCode` (o gerenciador de usuários pode recusar códigos desconhecidos levantando uma exceção) e `RegisterSecurityCode` (para o gerenciador poder listar os códigos usados pela aplicação, o que os gerenciadores de usuários usam para montar as telas de permissões).
* **Login**: `GetControlSecurityManager.Login` abre o diálogo padrão de autenticação (usuário e senha, ou cartão RFID se houver um leitor ligado). Se a senha estiver certa, o gerenciador reavalia todos os controles (`UpdateControls`). Depois de `LoginRetries` tentativas erradas o diálogo trava por `LoginFrozenTime` milissegundos. `Login(usuario, senha, uid)` faz o mesmo sem diálogo.
* **Logout**: `GetControlSecurityManager.Logout` — todos os controles com código voltam a ficar desabilitados.
* Em código, `CanAccess(código)` retorna se o usuário atual tem acesso, e `TryAccess(código)` levanta uma exceção se não tiver — útil no início de um procedimento que não deve rodar sem permissão.

Propriedades e eventos disponíveis em todos os gerenciadores de usuários:

| Membro | Descrição |
|---|---|
| `UserLogged`, `CurrentUserLogin`, `CurrentUserName`, `UID`, `LoggedSince` | Estado do login atual (somente leitura). |
| `LoginRetries` | Tentativas de senha antes de travar o diálogo (0 = nunca trava). |
| `LoginFrozenTime` | Tempo de trava, em ms. |
| `ChipCardReader` | Leitor de cartão/RFID para login sem senha — veja [Login por leitor RFID](/pb/rfid-reader/). |
| `SuccessfulLogin`, `FailureLogin` | Eventos de login com e sem sucesso (para auditoria, por exemplo, com o [registrador de eventos](/pb/event-and-alarm-loggers/)). |
| `UserChanged` | Evento `(Sender, OldUsername, NewUserName)` disparado em login e logout. |

##### Escolhendo o gerenciador de usuários

| Componente | Onde ficam os usuários | Use quando… |
|---|---|---|
| ![](img/TCustomizedUserManagement.png) `TCustomizedUserManagement` | Onde você quiser: você responde a eventos | tem um banco de usuários próprio, um LDAP, um arquivo — qualquer coisa. É o mais usado. |
| ![](img/TCentralUserManagement.png) `TCentralUserManagement` | Em um servidor HTTP/JSON na rede | várias estações precisam compartilhar os mesmos usuários e permissões, com login refletido em todas. |
| ![](img/TWinCCUserManagement.png) `TWinCCUserManagement` | No *User Administrator* do SIMATIC WinCC (só Windows) | a aplicação roda ao lado de um WinCC e deve usar os mesmos usuários e autorizações. |

##### TCustomizedUserManagement {#TCustomizedUserManagement}

![](img/TCustomizedUserManagement.png) Não sabe nada sobre usuários: delega tudo a eventos que você implementa. Os obrigatórios são dois:

```pascal
// Valida usuário e senha. Devolva o UID (qualquer inteiro que identifique o usuário)
// e ValidUser. LoginAction é True quando a chamada vem do diálogo de login e False
// quando vem de uma verificação de "assinatura" (TPascalSCADACheckSpecialTokenAction).
procedure TForm1.UserManagementCheckUserAndPass(user, pass: UTF8String;
  out aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean);
begin
  ValidUser := Users.CheckPassword(user, pass, aUID);
end;

// Diz se o usuário logado tem acesso ao código de segurança.
procedure TForm1.UserManagementCanAccess(securityCode: UTF8String;
  var CanAccess: Boolean);
begin
  CanAccess := Users.HasPermission(UserManagement.UID, securityCode);
end;
```

Os demais eventos:

| Evento | Para quê |
|---|---|
| `OnUIDCanAccess(aUID, securityCode, var CanAccess)` | Como `OnCanAccess`, mas para um usuário específico — usado pela verificação de assinatura, quando quem autoriza não é quem está logado. |
| `OnGetUserName`, `OnGetUserLogin` | Devolvem nome e login do usuário atual, mostrados em `CurrentUserName`/`CurrentUserLogin`. |
| `OnLogout` | Limpe o que guardou sobre o usuário. |
| `OnManageUsersAndGroups` | Abra a sua tela de cadastro de usuários (é o que `Manage` e a `TPascalSCADAManageUsersAction` chamam). |
| `OnValidadeSecurityCode(code)` | Levante uma exceção se o código não existir no seu cadastro — protege contra erros de digitação nos `SecurityCode` dos controles. |
| `OnRegisterSecurityCode(code)` | Chamado para cada código novo que aparece na aplicação; grave-o no banco para ele aparecer na tela de permissões. |
| `OnCheckUserChipCard` | Valida o código de um cartão RFID lido pelo `ChipCardReader`. |

O exemplo `examples/laz_custom_user_management` implementa isso com três usuários fixos em código — bom para entender o mecanismo. O exemplo `examples/laz_customusermanagement` é uma implementação completa com **usuários, grupos e permissões em PostgreSQL** (via Zeos), incluindo as telas de cadastro de usuários, grupos e permissões e o hash MD5 das senhas — o modelo de banco está em `docs/db_user_groups_modelagem.xml`. Use-o como ponto de partida para o seu.

##### TCentralUserManagement {#TCentralUserManagement}

![](img/TCentralUserManagement.png) Cliente de um **servidor de autenticação** HTTP: cada verificação de senha e de permissão vira uma chamada JSON ao servidor, e uma thread acompanha o endpoint `userchanged` para que um login feito em uma estação apareça nas outras.

| Propriedade | Descrição |
|---|---|
| `AuthServer`, `AuthServerPort`, `UseSSL` | Endereço do servidor. |
| `UseCachedAuthorizations` | Guarda as respostas de permissão localmente para não consultar o servidor a cada `CanAccess`; `CachedUpdateCount` mostra quantas vezes o cache foi renovado. |
| `UseCentralUserAsLocalUser` | Se `True`, o usuário logado no servidor (por outra estação) passa a ser o usuário desta estação também — login único na planta. |
| `RaiseExceptOnConnFailure` | Levanta exceção quando o servidor não responde; se `False`, falha silenciosamente negando acesso. |
| `OnUserChanged` | Notificação de troca de usuário vinda do servidor. |

O protocolo é simples — `POST` de um objeto JSON para `/checkuserpwd`, `/uidcanaccess`, `/validadesecuritycode`, `/registersecuritycode`, `/enumsecuritycodes` e `/userchanged`. O exemplo **`examples/laz_security_webserver_WinCC`** é um servidor pronto que expõe o `TWinCCUserManagement` de uma máquina para a rede — ou seja, todas as estações PascalSCADA usam os usuários do WinCC — e serve de modelo para escrever um servidor sobre o seu próprio banco de usuários.

##### TWinCCUserManagement {#TWinCCUserManagement}

![](img/TWinCCUserManagement.png) Só em Windows, com o SIMATIC WinCC instalado na máquina: usa a DLL *UseAdmin* do WinCC para autenticar e consultar as autorizações. O diálogo de login é o padrão do PascalSCADA, mas usuário e senha são validados pelo WinCC; `Manage` abre o *User Administrator* do WinCC.

Como o WinCC identifica autorizações por número (1 = *User administration*, 2 = *Value input*, 1000 = *System change*…), a propriedade **`AuthorizationList`** mapeia cada código de segurança do PascalSCADA para um número, uma linha por código, no formato `número:código`:

```
2:setpoints
1000:manutencao
1001:receitas
```

Um usuário do WinCC com a autorização 2 tem acesso a todos os controles com `SecurityCode = "setpoints"`. Códigos fora da lista são recusados.

##### Actions de segurança

Na aba **PascalSCADA User Management** do editor de `TActionList` (*New standard action*) há seis actions prontas para ligar em botões, itens de menu e barras de ferramentas:

| Action | O que faz |
|---|---|
| `TPascalSCADALoginAction` | Abre o diálogo de login. |
| `TPascalSCADALogoutAction` | Faz logout. Só fica habilitada com alguém logado. |
| `TPascalSCADALogin_LogoutAction` | As duas em um botão só: `WithoutUserLoggedInCaption`/`Hint`/`ImageIndex` quando ninguém está logado e `WithUserLoggedInCaption`/`Hint`/`ImageIndex` quando há usuário — o botão troca de texto e ícone sozinho. |
| `TPascalSCADAManageUsersAction` | Abre a tela de gerenciamento de usuários (`Manage`). |
| `TPascalSCADASecureAction` | Uma action comum com `SecurityCode`: use-a em qualquer botão ou menu que deva exigir permissão. Com `DisableIfNotAuthorized = True` (padrão) ela fica desabilitada para quem não tem o código; com `False` ela fica habilitada, mas `Execute` não roda o `OnExecute` sem permissão. |
| `TPascalSCADACheckSpecialTokenAction` | **Assinatura de supervisor**: ao executar, se o usuário logado não tiver o `SecurityCode` (ou sempre, com `RequireLoginAlways = True`), abre um diálogo pedindo usuário e senha de **outro** usuário que tenha o código. Só então dispara `OnExecute`; o login de quem autorizou fica em `AuthorizedBy` para você registrar no log. O usuário logado não muda. |

`Hint` da action é mostrado no diálogo de assinatura como explicação do que está sendo autorizado.

##### Passo a passo

1. Solte um `TCustomizedUserManagement` no form principal (ou em um datamodule criado antes dos forms). Implemente `OnCheckUserAndPass` e `OnCanAccess`.
2. Solte um `TActionList`, adicione uma `TPascalSCADALogin_LogoutAction` e ligue-a a um botão da barra de ferramentas.
3. Nos controles que precisam de proteção, preencha `SecurityCode` — o mesmo código em vários controles é o normal (`"operacao"` em todos os comandos, `"setpoints"` em todos os `THMIEdit` de ajuste).
4. Em menus e botões comuns, use `TPascalSCADASecureAction` com o código adequado.
5. Para operações críticas (apagar uma receita, forçar uma saída), use `TPascalSCADACheckSpecialTokenAction` com `SecurityCode = "supervisor"`.
6. Rode: tudo com código começa desabilitado; após o login com um usuário que tenha os códigos, os controles habilitam; o logout desabilita de novo.

Em código, quando precisar:

```pascal
uses ControlSecurityManager;

if GetControlSecurityManager.CanAccess('receitas') then
  CarregarReceita;

GetControlSecurityManager.TryAccess('manutencao');  // exceção se não puder
```
