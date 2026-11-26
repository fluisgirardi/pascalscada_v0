##### Onde conseguir o PascalSCADA

Hoje o desenvolvimento acontece no **GitHub** — é lá que estão a versão mais atual, os drivers novos e as últimas correções (veja o anúncio recente no blog). O SourceForge e o SVN, citados em versões antigas desta página, não são mais usados.

> **Recomendado: instale pelo Online Package Manager (OPM) do Lazarus.** *Package → Online Package Manager*, procure por *PascalSCADA*, marque e clique em *Install* — o OPM baixa o PascalSCADA **0.8.0** e as dependências (inclusive o BGRABitmap) e recompila a IDE sozinho ([veja como, com vídeo](/pb/2017/04/04/pascalscada-on-opm-repository/)). A instalação manual pelo GitHub, descrita abaixo, é para quem quer o código em desenvolvimento.

**GitHub (código em desenvolvimento)**

```
git clone https://github.com/fluisgirardi/pascalscada_v0.git
```

Ou, sem `git`, baixe o [ZIP do branch master](https://github.com/fluisgirardi/pascalscada_v0/archive/refs/heads/master.zip) e descompacte numa pasta de sua escolha.

**Atenção**: instalando pelo GitHub, algumas dependências precisam ser baixadas e instaladas **antes** do PascalSCADA — principalmente o **BGRABitmap**, usado pelos controles gráficos da HCl (pelo *Online Package Manager* do Lazarus). Sem ele instalado primeiro, o pacote do PascalSCADA não compila. Veja a lista completa de dependências em [Como instalar o PascalSCADA](/pb/how-install-pascalscada/#packages).

**Online Package Manager do Lazarus (recomendado)**

Pelo *Package → Online Package Manager* do Lazarus, procure por *PascalSCADA* e instale direto pela IDE, sem baixar nada manualmente. O pacote publicado ali é a versão **0.8.0**, já com os drivers novos e as correções recentes.

Depois de baixar (por qualquer um dos dois caminhos), siga o passo a passo em [Como instalar o PascalSCADA](/pb/how-install-pascalscada/).
