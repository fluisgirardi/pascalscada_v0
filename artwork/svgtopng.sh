#!/bin/sh

echo "Removing files in ./24x24png/*";
rm -rf ./24x24png/*.png

for l in `find ./scalable/ -iname "t*.svg"`; do
   outfile=`echo $l | cut -d"/" -f3 | cut -d"." -f1`;
   echo "Gerando arquivo PNG de $l => ./24x24png/$outfile.png";
   inkscape --file=$l --export-png=./24x24png/$outfile.png --export-dpi=45   
done;
