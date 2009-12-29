#!/bin/sh

echo "Creating Delphi Resource (scada.dcr)";
rm ../scada.res > /dev/null

for l in `find ./24x24bmp/ -iname "t*.bmp"`; do
  linha="$linha $l";
done;

../tools/dcrcreator/dcrcreator ../scada.res $linha
