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
            mv adult.arff communities.arff KEEP
        ;;
        rocks018)
            mv pendigits.arff magic.arff KEEP
        ;;
        rocks027)
            mv insurance.arff bank.arff KEEP
        ;;
        rocks028)
           mv breast.arff letrec.arff KEEP
        ;;
    esac
    mv *.arff THROW
    mv KEEP/* .
    cd ../../experiments
fi

R -f get-target-entropies.R
R -f ViewSelection.R
R -f View-POI-Eval.R
R -f StrengthTest.R

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
