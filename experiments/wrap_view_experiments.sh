#!/bin/bash

if [ "$1" = "download" ]
then
     echo DOWNLOADING FILES
    cd ../data/experiments
    ./get_data.sh

    mkdir KEEP THROW
    machine=`hostname -s`
    case $machine in
        rocks017)
            mv adult.arff communities.arff diabetes.arff KEEP
        ;;
        rocks018)
            mv internet_usage.arff pendigits.arff vowel.arff KEEP
        ;;
        rocks027)
            mv insurance.arff liver.arff shape.arff KEEP
        ;;
        rocks028)
           # mv breast.arff cover_type.arff glass.arff KEEP
           mv breast.arff glass.arff KEEP
        ;;
    esac
    mv *.arff THROW
    mv KEEP/* .
    cd ../../experiments
fi

#R -f StrengthTest.R
R -f get-target-entropies.R
R -f ViewSelection.R
R -f View-POI-Eval.R

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
