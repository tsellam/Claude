#!/usr/bin/Rscript
options(scipen=999)
library(foreign)

arguments <- commandArgs(trailingOnly = TRUE)
file_in <- arguments[1]
file_out <- arguments[2]
ncols <- arguments[3]
nrows <- arguments[4]
generate_class <- arguments[5]

data <- read.csv(file_in, header=FALSE, sep=";")
data <- data[1:nrows, 1:ncols]
if (!is.na(generate_class)) {
	if (generate_class == TRUE){
		data[[ncol(data) + 1]] <- as.factor(rep("class0", nrow(data)))
		names(data) <- c(1:(ncol(data) - 1), "class")
	} else {
		data[[ncol(data)]] <- as.factor(data[[ncol(data)]])
	}
} else {
	names(data) <- 1:ncol(data)
}

write.arff(data, file = file_out)