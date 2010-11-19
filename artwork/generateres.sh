#!/bin/sh

echo "Creating Delphi Resource (pascalscada.res)";
if [ -f ../pascalscada.res ]; then
  rm ../pascalscada.res
fi

for l in `find ./24x24bmp/ -iname "t*.bmp"`; do
  linha="$linha $l";
done;

../tools/dcrcreator/dcrcreator ../pascalscada.res $linha
