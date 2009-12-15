#!/bin/sh

echo "Removing files in ./24x24png/*.png";
rm -rf ./24x24png/*.png
echo "Removing files in ./24x24bmp/*.bmp";
rm -rf ./24x24bmp/*.bmp
echo "Removing files in ./24x24xpm/*.xpm";
rm -rf ./24x24bmp/*.xpm

for l in `find ./scalable/ -iname "t*.svg"`; do
   outfile=`echo $l | cut -d"/" -f3 | cut -d"." -f1`;
   echo "Gerando arquivo PNG => ./24x24png/$outfile.png";
   inkscape --file=$l --export-png=./24x24png/$outfile.png --export-dpi=45 > /dev/null
   echo "Gerando arquivo BMP => ./24x24bmp/$outfile.bmp";
   convert -size 24x24 xc:"#c0c0c0" -background "#c0c0c0" +append ./24x24png/$outfile.png -layers merge ./24x24bmp/$outfile.bmp > /dev/null
   echo "Gerando arquivo XPM => ./24x24xpm/$outfile.xpm";   
done;
