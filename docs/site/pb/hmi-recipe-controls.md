##### Introdução

Uma **receita** é um conjunto nomeado de valores que o operador manda para a máquina de uma vez: os parâmetros de um produto, os setpoints de um forno, a dosagem de um misturador. O pacote `pascalscada_hmi` traz dois componentes que reproduzem o gerenciador de receitas de um IHM dedicado — a biblioteca mora **no PC**, não no CLP:

1. **`THMIRecipeGrid`** (unidade `HMIRecipe`) — a tabela de receitas: uma linha por receita, uma coluna por parâmetro, edição no lugar, gravada em um arquivo de biblioteca de texto puro.
2. **`THMIRecipeButton`** (unidade `HMIRecipeButton`) — um botão por papel, ligado à tabela: adicionar receita, apagar receita, gravar a biblioteca, **download** da receita selecionada para as tags do CLP e **upload** dos valores das tags de volta para a receita.

Os dois falam texto puro: o arquivo da biblioteca é legível, versionável e editável em qualquer editor.

##### A tabela de receitas

Solte um `THMIRecipeGrid` no formulário. Cada item da coleção **`Fields`** vira uma coluna de parâmetro; a coluna do nome fica à esquerda e, por padrão, uma **coluna de sequência** somente leitura (`#`, 1, 2, 3…) entra na frente dela.

| Propriedade | Padrão | Descrição |
|---|---|---|
| `Fields` | vazio | As colunas de parâmetro. Um `TRecipeField` por coluna: `Name` (título da coluna e chave no arquivo), `PLCTag` (a tag que recebe o valor no download e fornece no upload), `Decimals` (arredondamento de exibição, padrão 2), `CheckRange` + `MinValue`/`MaxValue` (limita o valor digitado e avisa pelo `OnError`), `DefaultValue` (valor de uma receita nova). |
| `RecipeFile` | `recipes.ini` | O arquivo da biblioteca. Caminho relativo é resolvido contra o diretório do executável. |
| `AutoLoad` | `True` | Carrega a biblioteca quando a aplicação começa. Desligue para controlar o momento você mesmo (`LoadLibrary`). |
| `ShowRowNumbers` | `True` | A coluna de sequência somente leitura na frente da coluna do nome; os números seguem a ordem da biblioteca e são refeitos depois de um apagar. |
| `RowNumberTitle` | `#` | Título da coluna de sequência. |
| `RowNumberWidth` | 40 | Largura da coluna de sequência. |
| `NameColumnTitle` | `Recipe` | Título da coluna do nome. |
| `OnError` | – | Chamado a cada entrada recusada: nome vazio ou repetido, número inválido, fora de faixa. |
| `OnRecipeChanged` | – | Chamado depois de nome, valor, inclusão ou exclusão. |

A edição é no lugar: a coluna do nome renomeia a receita (nome vazio e repetido são recusados pelo `OnError`), cada coluna de parâmetro aceita um número (ponto ou vírgula). Um valor fora da janela do `CheckRange` é limitado ao limite mais próximo e avisado.

> **`Columns` fica vazia.** As colunas vêm de `Fields`; o modo de colunas da LCL tranca a contagem e quebra a tabela. Uma coleção `Columns` preenchida por engano é descartada pela grade em vez de derrubar tudo — mas deixe vazia no Object Inspector.

##### O arquivo da biblioteca

```
# PascalSCADA recipe library
Version=1

[Recipe 1]
Field 0=1.000000
Field 1=2.000000
$0=1.000000
```

Uma `[seção]` por receita, um `chave=valor` por parâmetro — a chave é o `Name` do campo (`$n` guarda o valor de um campo renomeado ou removido, nada se perde quando a lista de campos muda). `LoadLibrary` e `SaveLibrary` leem e gravam; arquivo ausente não é erro, começa uma biblioteca vazia.

##### Os botões de comando

Solte um `THMIRecipeButton` por papel e aponte `RecipeGrid` para a tabela. A propriedade `Command` escolhe o papel — e o título acompanha enquanto você não tiver digitado um próprio:

| Comando | O que o clique faz |
|---|---|
| `rcAddRecipe` | Acrescenta uma receita (`RecipeName` fixo, ou o próximo nome livre quando vazio). |
| `rcDeleteRecipe` | Apaga a receita (`RecipeName`, ou a linha selecionada quando vazio). |
| `rcSaveLibrary` | Grava o arquivo da biblioteca. |
| `rcDownload` | Escreve os valores da **linha selecionada** na `PLCTag` de cada campo. |
| `rcUpload` | Lê cada `PLCTag` e guarda os valores na **linha selecionada**. |

| Propriedade | Padrão | Descrição |
|---|---|---|
| `RecipeGrid` | – | A tabela que o botão comanda. |
| `Command` | `rcAddRecipe` | O papel, da tabela acima. |
| `RecipeName` | vazio | Nome fixo da receita para incluir/apagar; vazio usa a seleção. |
| `AutoSave` | `True` | Grava a biblioteca depois de incluir, apagar e trazer do CLP. |
| `GlyphUpFile` / `GlyphDownFile` | vazio | Figura do estado solto / apertado (PNG com transparência, BMP, JPG). Caminho relativo é resolvido contra o diretório do executável; deixe `GlyphDownFile` vazio para reusar a figura do estado solto. |
| `GlyphUp` / `GlyphDown` | – | As mesmas figuras como bitmap, coladas no Object Inspector. |
| `OnError` / `OnAfterCommand` | – | Chamados quando o comando não pode rodar (sem tabela, sem linha selecionada) e depois que terminou. |

Clicar um botão sem tabela ligada avisa pelo `OnError` em vez de levantar exceção; no designer os botões não fazem nada.

##### As actions de receita

Os mesmos comandos vêm também como actions (unit `HMIRecipeActions`, categoria **PascalSCADA Recipes** do editor de action list), para menus, barras de ferramentas ou qualquer botão. Elas descendem de `TPascalSCADASecureAction`, então um `SecurityCode` restringe o comando aos usuários que o têm: para os outros a action — e todo controle ligado nela — fica desabilitada (veja `DisableIfNotAuthorized`).

| Action | Comando | Propriedades a mais |
|---|---|---|
| `TPascalSCADAAddRecipeAction` | `rcAddRecipe` | `RecipeName`, `AutoSave` |
| `TPascalSCADADeleteRecipeAction` | `rcDeleteRecipe` | `RecipeName`, `AutoSave` |
| `TPascalSCADASaveRecipeLibraryAction` | `rcSaveLibrary` | – |
| `TPascalSCADADownloadRecipeAction` | `rcDownload` | – |
| `TPascalSCADAUploadRecipeAction` | `rcUpload` | `AutoSave` |

Todas têm `RecipeGrid`, `OnError` e `OnAfterCommand`, com o mesmo sentido do botão, e o título do comando. Um `OnExecute` seu substitui o comando, como nas actions padrão do LCL; a verificação de acesso continua valendo. Um `THMIRecipeButton` com `Action` preenchida executa a action e ignora o próprio `Command`.

O botão e as actions dividem o código que executa os comandos (`TRecipeCommandRunner`, unit `HMIRecipeCommands`): um comando se comporta igual pelos dois.

##### Passo a passo

O [exemplo da biblioteca de receitas](https://github.com/fluisgirardi/pascalscada_v0/tree/master/examples/laz_recipe_library) mostra a montagem completa:

1. Ponha as tags `TPLCTagNumber` no formulário — uma por parâmetro que a máquina recebe.
2. Solte o `THMIRecipeGrid` e acrescente um item de `Fields` por tag, ligando `PLCTag` e ajustando `Name`, `Decimals` e a faixa.
3. Solte quatro ou cinco `THMIRecipeButton`s, aponte cada um à grade e escolha o `Command` (`Add recipe`, `Delete recipe`, `Save library`, `Download`).
4. Mostre os valores das tags com `THMILabel`s para o operador ver o que chegou.
5. Rode: edite a tabela, grave, selecione a linha e faça o download.

##### Perguntas frequentes

* **A grade tem uma coluna e ler `Cells[1, x]` levanta "Index out of range".** Só em compilações antigas: a grade agora mantém ao menos duas colunas e duas linhas em qualquer estado. Atualize o pacote.
* **"Invalid value for property" ao ler `ColWidths` na partida.** Só em compilações antigas: acontecia com exatamente quatro campos, quando o designer omitia do .lfm o `ColCount` igual ao padrão. Atualize o pacote.
* **Onde ficam as receitas?** Em `RecipeFile`, por padrão ao lado do executável. É texto puro — inspecione, edite ou ponha sob controle de versão como quiser.
* **Várias telas podem compartilhar uma biblioteca?** Sim, mas um componente por vez deve ser o dono do arquivo; dê a cada tela o seu `RecipeFile` ou centralize toda gravação em uma grade.
