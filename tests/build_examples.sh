#!/bin/sh
# Compila todos os exemplos e diz quais passaram, quais foram pulados e quais
# falharam de verdade.
#
# Builds every example and reports which ones passed, which were skipped and
# which actually failed.
#
#   uso / usage:  tests/build_examples.sh [widgetset] [targetos]
#
# Um exemplo que nao compila mais e' um sinal de que a biblioteca mudou e ele
# ficou para tras - foi assim que se descobriu que treze deles ainda pediam as
# units de HMI ao pacote errado depois que elas foram separadas.
#
# An example that no longer builds is a sign the library moved and it was left
# behind - that is how we found that thirteen of them still asked the wrong
# package for the HMI units after those were split out.
#
# Ha duas formas de pular um exemplo. Pacote de terceiros que falta e'
# detectado sozinho, pelo "Broken dependency" que o lazbuild imprime. Exemplo
# que so faz sentido num sistema vai em tests/examples_skip.txt.
#
# There are two ways an example gets skipped. A missing third party package is
# detected on its own, by the "Broken dependency" lazbuild prints. An example
# that only makes sense on one system goes in tests/examples_skip.txt.

set -u

WIDGETSET="${1:-gtk2}"
ALVO="${2:-linux}"
LAZBUILD="${LAZBUILD:-lazbuild}"
RAIZ="$(cd "$(dirname "$0")/.." && pwd)"
EXCECOES="$RAIZ/tests/examples_skip.txt"
cd "$RAIZ" || exit 1

REGISTRO="$(mktemp)"
trap 'rm -f "$REGISTRO"' EXIT

echo "== registering the PascalSCADA packages"
for pkg in pascalscada_common.lpk pascalscada.lpk pascalscada_db.lpk \
           pascalscada_dsng.lpk pascalscada_hmi.lpk pascalscada_full.lpk; do
    $LAZBUILD --add-package-link "$pkg" >/dev/null 2>&1
done

# O diretorio do Lazarus nao vem de brinde em lugar nenhum: no Debian o
# lazbuild em /usr/bin e' um link para dentro de /usr/lib/lazarus/<versao>, e
# nas outras instalacoes ele mora no proprio diretorio. Quem souber melhor
# passa LAZARUSDIR.
#
# The Lazarus directory is handed to us nowhere: on Debian the /usr/bin
# lazbuild is a link into /usr/lib/lazarus/<version>, and on other installs it
# sits in the directory itself. Whoever knows better passes LAZARUSDIR.
if [ -z "${LAZARUSDIR:-}" ]; then
    caminho=$(command -v "$LAZBUILD" 2>/dev/null)
    if [ -n "$caminho" ]; then
        caminho=$(readlink -f "$caminho" 2>/dev/null || echo "$caminho")
        LAZARUSDIR=$(dirname "$caminho")
    fi
fi

# pacotes que vem com o proprio Lazarus, quando o diretorio dele e' conhecido
# packages shipped with Lazarus itself, when its directory is known
if [ -n "${LAZARUSDIR:-}" ] && [ -d "$LAZARUSDIR/components" ]; then
    echo "== Lazarus packages at $LAZARUSDIR"
    for pkg in components/rtticontrols/runtimetypeinfocontrols.lpk \
               components/tachart/tachartlazaruspkg.lpk \
               components/sqldb/sqldblaz.lpk; do
        [ -f "$LAZARUSDIR/$pkg" ] && \
            $LAZBUILD --add-package-link "$LAZARUSDIR/$pkg" >/dev/null 2>&1
    done
fi

ok=0
pulados=0
falhas=0

echo "== building the examples (widgetset=$WIDGETSET, system=$ALVO)"
for projeto in $(find examples -name "*.lpi" | sort); do

    motivo=$(grep -a -v '^[[:space:]]*#' "$EXCECOES" 2>/dev/null | \
             awk -v alvo="$ALVO" -v proj="$projeto" \
                 '($1==alvo || $1=="todos") && $2==proj {$1="";$2="";sub(/^ +/,"");print;exit}')
    if [ -n "$motivo" ]; then
        pulados=$((pulados+1))
        echo "  skipped $projeto  ($motivo)"
        continue
    fi

    if saida=$($LAZBUILD --widgetset="$WIDGETSET" "$projeto" 2>&1); then
        ok=$((ok+1))
        echo "  ok      $projeto"
        continue
    fi

    # "Broken dependency" e' pacote de terceiros que nao esta' instalado nesta
    # maquina - nao e' o exemplo que esta' quebrado
    # "Broken dependency" means a third party package is not installed here -
    # the example itself is not broken
    quebrado=$(echo "$saida" | grep -a -o 'Broken dependency: [^ ]*' | head -1)
    if [ -n "$quebrado" ]; then
        pulados=$((pulados+1))
        echo "  skipped $projeto  ($quebrado)"
        continue
    fi

    falhas=$((falhas+1))
    echo "  FAILED  $projeto"
    echo "$saida" | grep -a -iE "error|fatal" | head -3 | sed 's/^/            /'
    echo "$projeto" >> "$REGISTRO"
done

echo
echo "== summary: $ok built, $pulados skipped, $falhas failed"

if [ "$falhas" -gt 0 ]; then
    echo "examples that failed:"
    sed 's/^/  /' "$REGISTRO"
    exit 1
fi
exit 0
