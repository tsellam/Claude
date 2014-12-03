#!/bin/bash

cd ../data/experiments
./get_data.sh

mkdir KEEP THROW
machine=`hostname -s`
case $machine in
	rocks136)
        mv internet_usage.arff pendigits.arff vowel.arff KEEP
	;;
	rocks138)
        mv adult.arff communities.arff diabetes.arff KEEP
	;;
	rocks139)
        mv insurance.arff liver.arff shape.arff KEEP
	;;
	rocks141)
       mv breast.arff cover_type.arff glass.arff KEEP
	;;
esac
mv *.arff THROW
mv KEEP/* .
cd ../../experiments

R -f StrengthTest.R
R -f ViewSelection.R

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
