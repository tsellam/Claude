#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

###############
# PREPARATION #
###############
log_file <- read.delim("~/Projects/TurboGroups/experiments/results.log")
out_file <- read.delim("~/Projects/TurboGroups/experiments/results.out")

#####################
# Plots view scores #
#####################
to_plot <- out_file %>%
            filter(grepl("- F1", key) | grepl("Strength", key)) %>%
            spread(key, value, convert = TRUE) %>% 
            mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`))

p1 <- ggplot(to_plot, aes(x = file, y = F1, fill = algo)) +
        geom_boxplot()
p1 <- prettify(p1)
print(p1)

#ggsave("../documents/plots/tmp_column-select-score.pdf", algo_accuracy, width = 8.5, height = 2.25)
#ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
#ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 8.5, height = 2.25)
