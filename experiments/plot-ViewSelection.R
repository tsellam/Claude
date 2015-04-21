#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

FOLDER <- "21-04"

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
black_list <-  c("internet_usage.arff", "insurance.arff")#, "liver.arff")
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

to_plot <- to_plot %>%
           group_by(file, algo) %>%
           summarize(med_F1 = median(F1), min_F1 = min(F1), max_F1 = max(F1)) %>%
           ungroup

p1 <- ggplot(to_plot, aes(x = file, y = med_F1,
                          ymax=max_F1, ymin=min_F1,
                          color = algo, fill= algo)) +
        geom_pointrange(position = position_dodge(width = 0.5), size = 0.75) 
    
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
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryBeam" & algo == "Approximative") %>%
    filter(grepl("- F1", key) | grepl("Strength", key)) %>%
    spread(key, value, convert = TRUE) %>%
    mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE)) %>%
    select(file, beam_size, view, F1, Strength)
    
to_plot <- to_plot %>%
            inner_join(entropies) %>%
            mutate(Strength = Strength / entropy) %>%
            select(-entropy)

to_plot <- to_plot %>%
            group_by(file, beam_size) %>%
            summarise(med_strength = median(Strength),
                      min_strength = min(Strength),
                      max_strength = max(Strength))

# Prepares the log file
time_to_plot <- log_file %>%
    filter(experiment == "VaryBeam" & algo == "Approximative" & key == "Time") %>%
    select(file, beam_size, Time=value)
            
# Joins!
to_plot <- inner_join(to_plot, time_to_plot, by= c("file"="file", "beam_size" = "beam_size"))

p3 <- ggplot(to_plot, aes(x = Time, y = med_strength, ymin=min_strength, ymax=max_strength)) +
    scale_y_continuous(limits=c(0,1)) +
    expand_limits(x = 0, y = 0) +
    facet_grid(. ~ file, scales="free_x") +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.75) +
    geom_ribbon(alpha = 0.3)
    
p3 <- prettify(p3)
print(p3)

##################################
# Prepares diversify experiments #
##################################
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryDeduplication" & algo == "Approximative") %>%
    filter(grepl("- F1", key) | grepl("Strength", key) | grepl("Diversity", key)) %>%
    spread(key, value, convert = TRUE) %>%
    mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE)) %>%
    select(file, dedup, view, F1, Strength, Diversity)

to_plot <- to_plot %>%
    inner_join(entropies) %>%
    mutate(Strength = Strength / entropy) %>%
    select(-entropy)

to_plot <- to_plot %>%
    group_by(file, dedup) %>%
    summarise(med_strength = median(Strength, na.rm = TRUE),
              min_strength = min(Strength, na.rm = TRUE),
              max_strength = max(Strength, na.rm = TRUE),
              Diversity = max(Diversity, na.rm = TRUE),
              F1 = median(F1, na.rm = TRUE))

p4 <- ggplot(to_plot, aes(x=dedup, y=Diversity)) +
        geom_point() +
        geom_line() +
        facet_grid(. ~ file, scales="free_x") 

p4 <- prettify(p4)
print(p4)


#ggsave("../documents/plots/tmp_column-select-score.pdf", algo_accuracy, width = 8.5, height = 2.25)
#ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
#ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 8.5, height = 2.25)
