#!/bin/bash

rm -rf *.csv *.arff data.zip*

#####################
# FILES FROM AACHEN #
#####################
# Gets the files
echo Getting small files from Aachen
wget --quiet http://dme.rwth-aachen.de/sites/default/files/public_files/data.zip
unzip data.zip
rm -rf Databases/synth*
mv Databases/real\ world\ data/* .
rm -rf Databases data.zip*

## Converts to CSV
#for file in *.arff
#do
#	sed '/@/d;s/,/;/g;s/;[.0-9]*\s*$//g' ${file} > ${file//.arff/.csv}
#done


######################
## BIG FILES FROM 4S #
######################
## Gets the public files
#echo Getting big files of 4S
#
#wget --quiet http://www.ipd.kit.edu/~muellere/4S/data.zip
#unzip data.zip  real/mutant1.csv real/gisette.csv #real/har.csv
#mv real/*.csv .
#rm -rf real synthetic
#
#rm mutant2*
#
#cut -d';' -f1-2500,5001 gisette.csv > tmp
#mv tmp gisette.csv
#
#cut -d';' -f4827-5409 mutant1.csv > mutant0.csv
#cut -d';' -f1-4000,5409 mutant1.csv > tmp
#mv tmp mutant1.csv


######################
# FILES FROM THE UCI #
######################
echo Getting UCI data sets

# Adult
wget --quiet http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
sed "s/, /;/g" < adult.data > adult.csv
rm adult.data

# Inernet usage survey
wget http://archive.ics.uci.edu/ml/machine-learning-databases/internet_usage-mld/final_general.dat.gz
gunzip -d final_general.dat.gz 
mv  final_general.dat internet_usage.csv
# Creating a class based on age
sed -i 's/^[0-9]* //g' internet_usage.csv
awk '{first = int($1 /10) ; $1 = ""; print $0, first;}' internet_usage.csv > tmp
sed 's/^ //g;s/ /;/g' tmp > internet_usage.csv
# Cleans files
nlines=`head -1 internet_usage.csv | sed 's/;/ /g' | wc -w`
awk -F';' "NF==${nlines} {print}" internet_usage.csv > tmp
mv tmp internet_usage.csv

# Insurance benchmark 
wget http://archive.ics.uci.edu/ml/machine-learning-databases/tic-mld/ticdata2000.txt
sed 's/\t/;/g' ticdata2000.txt > insurance.csv
rm ticdata2000.txt

# Cover Type
wget http://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz
gunzip -d covtype.data.gz
sed 's/,/;/g' covtype.data > cover_type.csv
rm covtype.data

# Crime
wget http://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data
awk -F',' '{if ($128 < 0.2) $128 = 0
            else if ($128 < 0.4) $128 = 1
            else if ($128 < 0.6) $128 = 2
            else if ($128 < 0.8) $128 = 3
            else $128 = 4
            print}' communities.data \
	| sed 's/ /;/g' > communities.csv
rm communities.data


#####################
# CONVERTS THE CSVs #
#####################
for file in *.csv
do
	echo Processing file $file

	cp $file $file.bak

	# If necessary, truncates
	echo Truncates
	if [[ $1 == "small" ]]
	then
		sort -r $file | head -2500 > tmp
		max=`head -1 tmp | sed 's/;/ /g' | wc -w`
		if  (( max > 99 ))
		then
			min=`expr $max - 100`
			cut -d';' -f${min}-${max} tmp > $file
		else
			cp tmp $file
		fi
	fi

	# Converts to clean file
	echo Cleans
	./csv2cleancsv.R $file $file

	# Converts to ARFF
	arff_file=${file//.csv/.arff}
	nrows=`wc -l < $file`
	ncols=`head -1 $file | sed 's/;/ /g' | wc -w`
	echo Converting file with dimensions $nrows and $ncols
	./csv2arff.R $file $arff_file $ncols $nrows FALSE
done

rm *.bak *.true *.csv tmp
