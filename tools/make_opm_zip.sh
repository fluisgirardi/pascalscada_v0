#!/usr/bin/env bash
# Builds the zip of PascalSCADA for the Lazarus Online Package Manager (OPM).
#
# Only what the packages need goes in: the .lpk/.pas/.lrs/.lrc of the root, src/
# (fpopen62541 included, without its generators, demo and C logger) and the
# licenses. Examples, tests, docs, artwork and tools stay out. The files come
# from git (git archive), so nothing that is not committed - lib/, backup/,
# binaries, untracked folders - can end up in the zip.
#
# The zip holds a single folder, "pascalscada/", with the packages inside: the
# layout OPM expects.
#
# Usage:
#   tools/make_opm_zip.sh [options]
#     -r, --ref REF        commit, tag or branch to pack (default: HEAD)
#     -o, --out DIR        where to write the zip (default: the current folder)
#     -e, --with-examples  also pack examples/ (much bigger)
#     -c, --check          unpack the zip in a temporary folder and build the
#                          packages from it with lazbuild (LAZBUILD, LAZARUSDIR)
#     -h, --help
#
#   LAZBUILD    lazbuild to use with --check (default: lazbuild)
#   LAZARUSDIR  Lazarus folder, passed to lazbuild with --check (optional)
#   COMPILER    FPC compiler, passed to lazbuild with --check (optional; without it
#               a clean configuration takes the fpc of the PATH)
#
# Examples:
#   tools/make_opm_zip.sh -r 0.8.0
#   LAZBUILD=~/fpclaz/Ffix_Lfix/lazarus/lazbuild LAZARUSDIR=~/fpclaz/Ffix_Lfix/lazarus \
#   COMPILER=~/fpclaz/Ffix_Lfix/fpc/bin/x86_64-linux/fpc.sh \
#     tools/make_opm_zip.sh -r 0.8.0 --check

set -euo pipefail

REF=HEAD
OUT=$PWD
WITH_EXAMPLES=0
CHECK=0
TOP=pascalscada

while [ $# -gt 0 ]; do
    case "$1" in
        -r|--ref)           REF=$2; shift 2 ;;
        -o|--out)           OUT=$2; shift 2 ;;
        -e|--with-examples) WITH_EXAMPLES=1; shift ;;
        -c|--check)         CHECK=1; shift ;;
        -h|--help)          sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 0 ;;
        *) echo "unknown option: $1 (see --help)" >&2; exit 2 ;;
    esac
done

command -v zip >/dev/null || { echo "zip is not installed" >&2; exit 1; }

ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"
git rev-parse --verify --quiet "$REF^{commit}" >/dev/null || { echo "unknown ref: $REF" >&2; exit 1; }
COMMIT=$(git rev-parse --short "$REF^{commit}")

# version of pascalscada.lpk at that ref: <Version Major=".." Minor=".." Release=".." Build=".."/>,
# the attributes that are 0 are not written
VERLINE=$(git show "$REF:pascalscada.lpk" | grep -m1 -E '^ {4}<Version( |/)' || true)
attr() { echo "$VERLINE" | sed -n "s/.*$1=\"\([0-9]*\)\".*/\1/p"; }
MAJOR=$(attr Major); MINOR=$(attr Minor); RELEASE=$(attr Release); BUILD=$(attr Build)
VERSION="${MAJOR:-0}.${MINOR:-0}.${RELEASE:-0}"
[ -n "$BUILD" ] && [ "$BUILD" != 0 ] && VERSION="$VERSION.$BUILD"

# what goes in (paths at the ref)
INCLUDE=(src LICENSE.txt LICENSE.modifiedLGPL.txt README.md)
for f in $(git ls-tree --name-only "$REF" | grep -E '^pascalscada.*\.(lpk|pas|lrs|lrc)$'); do
    INCLUDE+=("$f")
done
[ $WITH_EXAMPLES = 1 ] && INCLUDE+=(examples)

# what is taken out of src: what does not belong to a package
EXCLUDE=(
    "$TOP/src/external/fpopen62541/tools"
    "$TOP/src/external/fpopen62541/pascallog"
    "$TOP/src/external/fpopen62541/.github"
    "$TOP/src/external/fpopen62541/TestOpcUa*"
)

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

echo "== PascalSCADA $VERSION ($REF = $COMMIT)"
git archive --format=tar --prefix="$TOP/" "$REF" -- "${INCLUDE[@]}" | tar -x -C "$WORK"

cd "$WORK"
for p in "${EXCLUDE[@]}"; do rm -rf $p; done
# leftovers that are tracked by mistake
find "$TOP" \( -name backup -o -name lib -o -name __pycache__ \) -type d -prune -exec rm -rf {} +
find "$TOP" \( -name '*.bak' -o -name '*.ppu' -o -name '*.o' -o -name '*.compiled' \
              -o -name '.gitignore' -o -name '.gitattributes' \) -type f -delete

mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)
ZIP="$OUT/pascalscada-$VERSION.zip"
rm -f "$ZIP"
# -X: no extra attributes (uid/gid, times), -9: best compression
zip -q -r -9 -X "$ZIP" "$TOP"

FILES=$(find "$TOP" -type f | wc -l)
echo "   $FILES files, $(du -h "$ZIP" | cut -f1): $ZIP"

if [ $CHECK = 1 ]; then
    echo "== checking: building the packages from the zip"
    LAZBUILD=${LAZBUILD:-lazbuild}
    CHK="$WORK/check"
    mkdir -p "$CHK/pcp"
    unzip -q "$ZIP" -d "$CHK"
    LB=("$LAZBUILD" "--pcp=$CHK/pcp")
    [ -n "${LAZARUSDIR:-}" ] && LB+=("--lazarusdir=$LAZARUSDIR")
    [ -n "${COMPILER:-}" ] && LB+=("--compiler=$COMPILER")
    # a clean configuration: the packages are found in the zip, not in a working copy
    # registered in the IDE. The graphical packages need BGRABitmap, not in a clean
    # configuration: only the non graphical ones are built.
    for pkg in src/external/fpopen62541/fpopen62541.lpk pascalscada_common.lpk \
               pascalscada.lpk pascalscada_db.lpk; do
        "${LB[@]}" --add-package-link "$CHK/$TOP/$pkg" >/dev/null
    done
    for pkg in pascalscada_common.lpk pascalscada.lpk pascalscada_db.lpk; do
        printf '   %-24s' "$pkg"
        if "${LB[@]}" --widgetset=nogui "$CHK/$TOP/$pkg" >"$CHK/build.log" 2>&1; then
            echo ok
        else
            echo FAILED
            grep -E 'Error|Fatal' "$CHK/build.log" | head -20
            exit 1
        fi
    done
fi
