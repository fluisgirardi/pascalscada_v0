#!/usr/bin/env bash
# Downloads libopen62541 for the OPC UA tests of the CI (.github/workflows/tests.yml).
#
#   bash tests/ci_open62541.sh <asset> <directory>
#
# <asset> is a file of the release $OPEN62541_URL, as fpopen62541's workflow
# BuildOpen62541v1.5 (publish) makes it: libopen62541-<version>-linux-<arch>.tar.gz
# or libopen62541-<version>-windows-<arch>.zip. It is unpacked into <directory>.
#
# Without the release (404), it takes the same library from the artifacts of the
# run $OPEN62541_RUN of $OPEN62541_REPO ("open62541 <version> <os> <arch> Release"),
# which needs a token: $GITHUB_TOKEN (the workflow's own reads a public repository).
#
# Downloaded: OPEN62541_LIB (the library) and OPEN62541_REQUIRED=1 go to
# $GITHUB_ENV, so the OPC UA tests must run - a library that does not load fails
# them. Not available (no release, no artifact, network): a warning, and the tests
# stay ignored as they are without a library. The step itself never fails.
set -uo pipefail

asset=${1:?asset}
dir=${2:?directory}
url="${OPEN62541_URL:?OPEN62541_URL}/$asset"

# The library from a workflow artifact into $dir; false when it can not.
from_artifact() {
  [ -n "${OPEN62541_REPO:-}" ] && [ -n "${OPEN62541_RUN:-}" ] && [ -n "${GITHUB_TOKEN:-}" ] || {
    echo "no artifact fallback (OPEN62541_REPO, OPEN62541_RUN and GITHUB_TOKEN needed)"; return 1; }
  local base ver os arch name api list id tmp
  base=${asset#libopen62541-}; base=${base%.tar.gz}; base=${base%.zip}
  IFS=- read -r ver os arch <<<"$base"
  name="open62541 $ver $os $arch Release"
  api="${OPEN62541_API:-https://api.github.com}/repos/$OPEN62541_REPO/actions"
  list=$(curl -sSL --retry 3 -H "Authorization: Bearer $GITHUB_TOKEN" "$api/runs/$OPEN62541_RUN/artifacts?per_page=100")
  id=$(jq -r --arg n "$name" '.artifacts[]? | select(.name==$n and (.expired|not)) | .id' <<<"$list" 2>/dev/null | head -n 1)
  if [ -z "$id" ]; then
    echo "run $OPEN62541_RUN has no artifact \"$name\" ($(jq -r '.message // "\(.total_count) artifacts"' <<<"$list" 2>/dev/null))"
    return 1
  fi
  tmp="$dir/.artifact"
  mkdir -p "$tmp"
  # the API answers with a redirect to the storage (curl does not send the token there)
  if ! curl -fsSL --retry 3 -H "Authorization: Bearer $GITHUB_TOKEN" -o "$tmp/a.zip" "$api/artifacts/$id/zip"; then
    echo "artifact $id ($name) could not be downloaded"; rm -rf "$tmp"; return 1
  fi
  if command -v unzip >/dev/null; then unzip -q -o "$tmp/a.zip" -d "$tmp"; else 7z x -y -o"$tmp" "$tmp/a.zip" >/dev/null; fi
  # the artifact keeps the paths below build/: bin/libopen62541.so*, bin/Release/libopen62541.dll
  cp "$tmp"/bin/libopen62541.so* "$dir"/ 2>/dev/null || cp "$tmp"/bin/Release/libopen62541.dll "$dir"/ 2>/dev/null
  rm -rf "$tmp"
  echo "from the artifact \"$name\" of run $OPEN62541_RUN"
}

mkdir -p "$dir"
code=$(curl -sSL --retry 3 --retry-delay 5 -w '%{http_code}' -o "$dir/$asset" "$url") || code="000"
if [ "$code" != "200" ]; then
  rm -f "$dir/$asset"
  echo "$url answered $code"
  if ! from_artifact; then
    echo "::warning title=libopen62541 not available::$url answered $code and no artifact either: the OPC UA tests will be ignored. The release is made by the workflow BuildOpen62541v1.5 of fpopen62541 (input publish)."
    exit 0
  fi
else
  case "$asset" in
    *.zip)
      if command -v unzip >/dev/null; then
        unzip -q -o "$dir/$asset" -d "$dir"
      else
        7z x -y -o"$dir" "$dir/$asset" >/dev/null
      fi
      ;;
    *)
      tar -C "$dir" -xzf "$dir/$asset"
      ;;
  esac
  rm -f "$dir/$asset"
fi
case "$asset" in
  *.zip) lib="$dir/libopen62541.dll" ;;
  *)     lib="$dir/libopen62541.so" ;;
esac

if [ ! -f "$lib" ]; then
  echo "::warning title=libopen62541 not found::$asset has no $(basename "$lib"): the OPC UA tests will be ignored."
  ls -l "$dir"
  exit 0
fi
ls -l "$dir"
if command -v cygpath >/dev/null; then
  lib=$(cygpath -w "$lib")
fi
echo "libopen62541: $lib"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "OPEN62541_LIB=$lib" >> "$GITHUB_ENV"
  echo "OPEN62541_REQUIRED=1" >> "$GITHUB_ENV"
fi
