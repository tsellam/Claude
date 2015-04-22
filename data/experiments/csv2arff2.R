#!/usr/bin/Rscript
options(scipen=999)
library(foreign)

arguments <- commandArgs(trailingOnly = TRUE)
file_in <- arguments[1]
file_out <- arguments[2]
data <- read.csv(file_in, header=FALSE, sep=";")
data[[ncol(data)]] <- as.factor(data[[ncol(data)]])
write.arff(data, file = file_out)
