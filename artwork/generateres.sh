#!/bin/sh

echo "Creating Delphi Resource (scada.res)";
if [ -f ../scada.res ]; then
  rm ../scada.res
fi

for l in `find ./24x24bmp/ -iname "t*.bmp"`; do
  linha="$linha $l";
done;

../tools/dcrcreator/dcrcreator ../scada.res $linha
