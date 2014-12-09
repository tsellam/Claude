#!/usr/bin/Rscript
source("graph-utils.R")

library(dplyr)
library(tidyr)


# READING AND PREPROCESSING
raw_file <- read.delim("~/Projects/TurboGroups/experiments/results-08:12/compare_scores.tsv")
levels(raw_file$file) <-  sub(".arff", "", levels(raw_file$file))
raw_file$F1_score    <- as.numeric(as.character(raw_file$F1_score))
#raw_file$strength    <- as.numeric(as.character(raw_file$strength))

target_entropies <- read.delim("entropies.out")
raw_file <- left_join(raw_file, target_entropies, by = c('file' = 'file'))
raw_file <- mutate(raw_file, strength = strength / entropy)

# PLOTTING

to_plot_all <- filter(raw_file, !file %in% c("insurance", "internet_usage"))

outplotall <- ggplot(to_plot_all, aes(x = strength, y=F1_score, color=algo)) +
    geom_point(size=0.5) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "Naive Bayes - F1") +
    expand_limits(x = 0, y = 0) +
    facet_grid( . ~ file)

outplotall <- prettify(outplotall)
plot(outplotall)




to_plot_1 <- filter(raw_file, file %in% c("breast", "communities", "diabetes"))

outplot1 <- ggplot(to_plot_1, aes(x = strength, y=F1_score, color=algo, shape = algo)) +
            geom_point(size=0.5) +
            stat_smooth(method = "loess") +
            scale_x_continuous(name = "View Score (Normalized)", limits=c(0,1), breaks=c(0,1)) +
            scale_y_continuous(name = "Classification F1", limits=c(0,1)) +
            facet_grid( . ~ file, scales = "free_x")

outplot1 <- prettify(outplot1)

to_plot_2 <- filter(raw_file, file %in% c("pendigits", "shape", "vowel"))

outplot2 <- ggplot(to_plot_2, aes(x = strength, y=F1_score, color=algo, shape = algo)) +
    geom_point(size=0.5) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "View Score (Normalized)", limits=c(0,1), breaks=c(0,1)) +
    scale_y_continuous(name = "Classification F1", limits=c(0,1)) +
    facet_grid( . ~file, scales = "free_x")

outplot2 <- prettify(outplot2)



# WRITING

pdf("../documents/plots/tmp_compare-strength-f1.pdf", width = 3.33, height = 3.33)
outplot <- grid.arrange(outplot1, outplot2, ncol=1)
dev.off()