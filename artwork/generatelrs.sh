#!/bin/sh

echo "Creating Lazarus Resource (pascalscada.lrs)";
linha="";
for l in `find ./24x24png2/ -iname "t*.png"`; do
  linha="$linha $l";
done;

./lazres_linux_x86_64 ../pascalscada.lrs $linha > /dev/null
