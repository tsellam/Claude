#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

FOLDER <- "18-04"

###############
# PREPARATION #
###############
# Out files
files <- list.files(FOLDER, pattern = "results.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")

cat(length(files), " result files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
out_file <- rbind_all(file_contents)

# Timing files
files <- list.files(FOLDER, pattern = "results.log", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")

cat(length(files), " log files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
log_file <- rbind_all(file_contents)

# Filters and Prettifies
black_list <-  c("internet_usage.arff", "insurance.arff", "liver.arff")
out_file <- out_file %>%
            filter(!file %in% black_list) %>%
            mutate(file = sub(".arff", "", file))
            
log_file <- log_file %>%
    filter(!file %in% black_list) %>%
    mutate(file = sub(".arff", "", file))

# Entropies
entropies <- read.delim("entropies.out")

#####################
# Plots view scores #
#####################
to_plot <- out_file %>%
            filter(experiment == "VaryAlgos") %>%
            filter(grepl("- F1", key) | grepl("Strength", key)) %>%
            spread(key, value, convert = TRUE) %>%
            mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE))

# to_plot <- to_plot %>%
#            group_by(file, algo) %>%
#            summarize(F1 = mean(F1)) %>%
#            ungroup

p1 <- ggplot(to_plot, aes(x = file, y = F1, fill = algo)) +
        geom_boxplot(outlier.size=0.5, width = .5)
        #geom_bar(position = "dodge", stat = "identity")
p1 <- prettify(p1)
print(p1)

####################
# Plots time spent #
####################
to_plot <- log_file %>%
    filter(experiment == "VaryAlgos") %>%
    filter(key == "Time") %>%
    spread(key, value)

p2 <- ggplot(to_plot, aes(x = file, y = Time, fill = algo)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_cartesian(ylim = c(0,30))
p2 <- prettify(p2)
print(p2)

###############################
# Plots vary beam experiments #
###############################
to_plot <- out_file %>%
    filter(experiment == "VaryBeam" & algo == "Approximative") %>%
    filter(grepl("- F1", key) | grepl("Strength", key)) %>%
    spread(key, value, convert = TRUE) %>%
    mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE))

to_plot <- to_plot %>%
            inner_join(entropies) %>%
            mutate(Strength = Strength / entropy) %>%
            select(-entropy)


p3 <- ggplot(to_plot, aes(x = file, y = Strength, fill = factor(beam_size))) +
    geom_boxplot(outlier.size=0.5, width = .5)
    #geom_bar(position = "dodge", stat = "identity")

p3 <- prettify(p3)
print(p3)


#ggsave("../documents/plots/tmp_column-select-score.pdf", algo_accuracy, width = 8.5, height = 2.25)
#ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
#ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 8.5, height = 2.25)
