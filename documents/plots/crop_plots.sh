#!/bin/bash

mkdir TMPCROP
for file in ./*.pdf
do
    echo Cropping $file
    pdfcrop $file TMPCROP/$file
done

mv TMPCROP/* .
