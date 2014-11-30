#!/usr/bin/Rscript
options(scipen=999)
library(foreign)

arguments <- commandArgs(trailingOnly = TRUE)
file_in <- arguments[1]
file_out <- arguments[2]

data <- read.csv(file_in, header=FALSE, sep=";")
data <- as.data.frame(lapply(data, as.numeric))
write.table(data, file = file_out, sep = ";", quote=FALSE,
	row.names=FALSE, col.names=FALSE)