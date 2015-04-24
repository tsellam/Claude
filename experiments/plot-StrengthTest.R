#!/usr/bin/Rscript
source("graph-utils.R")

library(dplyr)
library(tidyr)

FOLDER <- "24-04"

#############################
# READING AND PREPROCESSING #
#############################
# Reading File
files <- list.files(FOLDER, pattern = "compare_scores.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")

cat(length(files), " result files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
raw_file <- rbind_all(file_contents)

# Reading entropies
files <- list.files(FOLDER, pattern = "entropies.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), "entropy files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
entropies <- rbind_all(file_contents)

# Joins
raw_file <- raw_file %>%
            mutate(file = sub(".arff", "", file)) %>%
            left_join(entropies, by = c('file' = 'file')) %>%
            mutate(strength = strength / entropy)

############
# PLOTTING #
############
outplotall <- ggplot(raw_file, aes(x = strength, y=F1_score, color=algo)) +
    geom_point(size=0.5) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "Naive Bayes - F1") +
    expand_limits(x = 0, y = 0) +
    facet_grid( . ~ file)

outplotall <- prettify(outplotall)
plot(outplotall)



to_plot_1 <- filter(raw_file, file %in% c("adult", "cover_type", "diabetes"))

outplot1 <- ggplot(to_plot_1, aes(x = strength, y=F1_score, color=algo, shape = algo)) +
            geom_point(size=0.2) +
            stat_smooth(method = "loess") +
            scale_x_continuous(name = "View Score (Normalized)", limits=c(0,1), breaks=c(0,1)) +
            scale_y_continuous(name = "Classification F1", limits=c(0,1)) +
            coord_fixed(ratio = 1) +
            facet_grid( . ~ file, scales = "free_x")

outplot1 <- prettify(outplot1) +
    theme(legend.position = "top")


to_plot_2 <- filter(raw_file, file %in% c("pendigits", "shape", "vowel"))

outplot2 <- ggplot(to_plot_2, aes(x = strength, y=F1_score, color=algo, shape = algo)) +
    geom_point(size=0.2) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "View Score (Normalized)", limits=c(0,1), breaks=c(0,1)) +
    scale_y_continuous(name = "Classification F1", limits=c(0,1)) +
    facet_grid( . ~file, scales = "free_x")

outplot2 <- prettify(outplot2) +
    theme(legend.position = "bottom")



# WRITING
pdf("../documents/plots/tmp_compare-strength-f1.pdf", width = 3.5, height = 4)
outplot <- grid.arrange(outplot1, outplot2, ncol=1)
dev.off()
