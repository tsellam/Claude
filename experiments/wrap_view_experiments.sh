#!/bin/bash

#cd ../data/experiments
#./get_data.sh

#mkdir KEEP
#case $machine in
#	rocks136)
#        mv internet_usage.arff pendigits.arff vowel.arff KEEP
#	;;
#	rocks138)
#        mv adult.arff communities.arff diabetes.arff KEEP
#	;;
#	rocks139)
#        mv insurance.arff liver.arff shape.arff KEEP
#	;;
#	rocks140)
#        mv breast.arff cover_type.arff glass.arff KEEP
#	;;
#esac
#rm *.arff
#mv KEEP/* .

cd ../../experiments
./StrengthTest.R
./ViewSelection.R

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
