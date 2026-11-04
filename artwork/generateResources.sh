#!/bin/sh
#
# Renders the component icons (scalable/t*.svg) into 24x24 PNG/BMP and rebuilds
# the Lazarus resource (../pascalscada.lrs) and the Delphi one (../pascalscada.res).
# Needs Inkscape (0.92 or 1.x) and ImageMagick (convert).
#
# The old files are replaced only after EVERY icon was rendered: a failure leaves
# the current ones untouched.

cd "$(dirname "$0")" || exit 1

if command -v inkscape >/dev/null 2>&1; then
  inkcmd='inkscape';
elif command -v inkscape-devel >/dev/null 2>&1; then
  inkcmd='inkscape-devel';
else
  echo "Inkscape ou o inkscape-devel não foram encontrados!"
  exit 1;
fi;

if ! command -v convert >/dev/null 2>&1; then
  echo "ImageMagick (convert) não foi encontrado!"
  exit 1;
fi;

# Inkscape 1.x dropped --file/--export-png of 0.92
inkmajor=`$inkcmd --version 2>/dev/null | sed -n 's/^[^0-9]*\([0-9][0-9]*\)\..*/\1/p' | head -n 1`;

render() { # render <svg> <png>
  if [ "${inkmajor:-0}" -ge 1 ]; then
    $inkcmd "$1" --export-type=png --export-filename="$2" --export-dpi=45 > /dev/null 2>&1
  else
    $inkcmd --file="$1" --export-png="$2" --export-dpi=45 > /dev/null 2>&1
  fi
}

work=`mktemp -d` || exit 1;
trap 'rm -rf "$work"' EXIT;
mkdir "$work/png" "$work/bmp" "$work/png2";

for l in `find ./scalable/ -iname "t*.svg" | sort`; do
   outfile=`basename "$l" | cut -d"." -f1`;
   echo "Creating $outfile";
   render "$l" "$work/png/$outfile.png"
   if [ ! -s "$work/png/$outfile.png" ]; then
     echo "Failed to render $l, nothing was changed.";
     exit 1;
   fi;
   convert -size 24x24 xc:"#c0c0c0" -background "#c0c0c0" +append "$work/png/$outfile.png" -layers merge -colors 256 -define bmp:bits-per-pixel=8 "$work/bmp/$outfile.bmp" > /dev/null || exit 1;
   convert -size 24x24 xc:"#c0c0c0" -background "#c0c0c0" +append "$work/png/$outfile.png" -layers merge -transparent "#c0c0c0" PNG32:"$work/png2/$outfile.png" > /dev/null || exit 1;
done;

echo "Replacing ./24x24png, ./24x24bmp and ./24x24png2";
for d in 24x24png:png 24x24bmp:bmp 24x24png2:png2; do
  dir=`echo $d | cut -d: -f1`; src=`echo $d | cut -d: -f2`;
  mkdir -p "./$dir";
  rm -f ./$dir/*.png ./$dir/*.bmp;
  cp "$work/$src"/* "./$dir/";
done;

./generatelrs.sh || exit 1
./generateres.sh
