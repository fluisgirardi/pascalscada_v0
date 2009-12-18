#!/bin/sh

echo "Gerando Lazarus Resource (scada.lrs)";
linha="";
for l in `find ./24x24xpm/ -iname "t*.xpm"`; do
  linha="$linha $l";
done;

./lazres_linux_x86_64 ../scada.lrs $linha
