#!/usr/bin/Rscript
source("graph-utils.R")

library(dplyr)
library(tidyr)

FOLDER <- "25-04"

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

raw_file$file <- factor(raw_file$file, 
                        levels = c("adult", "communities", "musk",
                                   "magic", "pendigits", "bank",
                                   "insurance", "breast", "letrec"),
                        labels = c("USCensus", "Crime", "MuskMolecules",
                                   "MAGICTelescope", "PenDigits", "BankMarketing",
                                   "Insurance", "BreastCancer", "LetterRecog"))

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



to_plot_1 <- filter(raw_file, file %in% c("USCensus", "MAGICTelescope", "PenDigits"))

outplot1 <- ggplot(to_plot_1, aes(x = strength, y=F1_score, color=algo,
                                  shape = algo, linetype = algo)) +
            geom_point(size=0.3) +
            stat_smooth(method = "loess") +
            scale_x_continuous(name = "View Score (Normalized)", limits=c(0,1), breaks=c(0,1)) +
            scale_y_continuous(name = "Classification F1", limits=c(0,1), breaks=c(0,1)) +
            facet_grid( . ~ file) +
            coord_fixed()

outplot1 <- prettify(outplot1) +
    theme(legend.position = "top") +
    scale_colour_manual(values = c(muted("blue"), muted("red")))

print(outplot1)

# WRITING
ggsave("../documents/plots/tmp_compare-strength-f1.pdf", outplot1, width = 10, height = 8, units = "cm")