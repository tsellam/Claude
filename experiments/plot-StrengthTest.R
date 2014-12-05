#!/usr/bin/Rscript
source("graph-utils.R")

library(dplyr)
library(tidyr)


# READING AND PREPROCESSING
#raw_file <- read.delim("results-05:12/compare_scores.agg.out", stringsAsFactors = FALSE)
raw_file <- read.delim("results-05:12/compare_scores.out", stringsAsFactors = FALSE, header = FALSE)
names(raw_file) <- c("file", "x", "y", "strength", "algo", "NB_score")

raw_file <- raw_file %>%
        mutate(file = sub(".arff", "", file),
               strength = strength,
               NB_score = NB_score) %>%
        filter(!(file=="internet_usage" & strength > 0.75) &
               !(file=="insurance" & strength > 0.075) &
               !(file=="liver" & strength < 0.25))


target_entropies <- read.delim("entropies.out", stringsAsFactors = FALSE)
raw_file <- left_join(raw_file, target_entropies, by = c('file' = 'file'))
raw_file <- mutate(raw_file, strength = strength / entropy)

# PLOTTING


to_plot_all <- filter(raw_file, !file %in% c("vowel"))

outplotall <- ggplot(to_plot_all, aes(x = strength, y=NB_score, color=algo)) +
    geom_point(size=0.5) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "Naive Bayes - F1") +
    expand_limits(x = 0, y = 0) +
    facet_grid( . ~ file)

outplotall <- prettify(outplotall)
print(outplotall)




to_plot_1 <- filter(raw_file, file %in% c("adult", "diabetes"))

outplot1 <- ggplot(to_plot_1, aes(x = strength, y=NB_score, color=algo)) +
            geom_point(size=0.5) +
            stat_smooth(method = "loess") +
            scale_x_continuous(name = "") +
            scale_y_continuous(name = "Naive Bayes - F1") +
            expand_limits(x = 0, y = 0) +
            facet_grid( . ~ file, scales = "free_x")

outplot1 <- prettify(outplot1)

to_plot_2 <- filter(raw_file, file %in% c("pendigits", "communities"))

outplot2 <- ggplot(to_plot_2, aes(x = strength, y=NB_score, color=algo)) +
    geom_point(size=0.5) +
    stat_smooth(method = "loess") +
    scale_x_continuous(name = "View Strength") +
    scale_y_continuous(name = "Naive Bayes -  F1") +
    expand_limits(x = 0, y = 0) +
    facet_grid( . ~file, scales = "free_x")

outplot2 <- prettify(outplot2)



# WRITING

pdf("../documents/plots/tmp_compare-strength-f1.pdf", width = 3.33, height = 3.33)
outplot <- grid.arrange(outplot1, outplot2, ncol=1)
dev.off()