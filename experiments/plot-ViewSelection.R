#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)
library(xtable)

FOLDER <- "22-04"

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
black_list <-  c("internet_usage.arff")
out_file <- out_file %>%
            filter(!file %in% black_list) %>%
            filter(!algo %in% c("Wrap_NaiveBayes")) %>%
            mutate(file = sub(".arff", "", file)) %>%
            mutate(algo = factor(algo, levels = c("ApproximativePrune",
                                                  "Approximative",
                                                  "Exhaustive",
                                                  "Wrap_kNN",
                                                  "Clique",
                                                  "4S"),
                                        labels = c("ApproximativePrune",
                                                  "Claude",
                                                  "Exact",
                                                  "Wrap 5-NN",
                                                  "Clique",
                                                  "4S")))
            
log_file <- log_file %>%
    filter(!file %in% black_list) %>%
    filter(!algo %in% c("Wrap_NaiveBayes")) %>%
    mutate(file = sub(".arff", "", file)) %>% 
    mutate(algo = factor(algo, levels = c("ApproximativePrune",
                                          "Approximative",
                                          "Exhaustive",
                                          "Wrap_kNN",
                                          "Clique",
                                          "4S"),
                         labels = c("ApproximativePrune",
                                    "Claude",
                                    "Exact",
                                    "Wrap 5-NN",
                                    "Clique",
                                    "4S")))
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

all_combis <-  unique(merge(to_plot$file, to_plot$algo))
colnames(all_combis) <- c("file", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("algo", "file"))
#to_plot[is.na(to_plot)] <- 55

p1 <- ggplot(to_plot, aes(x = file, y = med_F1,
                          ymax=max_F1, ymin=min_F1,
                          color = algo, fill= algo)) +
        geom_pointrange(position = position_dodge(width = 0.5),
                        size = 0.4) +
        scale_x_discrete(name = "Dataset") +
        scale_y_continuous(name = "Accuracy - F1")
        
    
p1 <- prettify(p1)
print(p1)
ggsave("../documents/plots/view-scores.pdf", p1,
       width = 16, height = 3.5, units = "cm")

####################
# Plots time spent #
####################
to_plot <- log_file %>%
    filter(experiment == "VaryAlgos") %>%
    filter(key == "Time") %>%
    spread(key, value)

p2 <- ggplot(to_plot, aes(x = file, y = Time, fill = algo)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_cartesian(ylim = c(0,60))
p2 <- prettify(p2)

print(p2)

to_table <- to_plot %>%
            select(file, algo, Time) %>%
            spread(algo, Time) %>%
            arrange(Claude)
rownames(to_table) <- to_table$file
to_table$file <- NULL

old_n <- colnames(to_table)
to_table <- apply(to_table, 1, function(row){
    out <- as.character(round(row, 2))
    out[is.na(row)] <- "*"
    out[which.min(row)] <- paste0('\\cellcolor{grn} ', out[which.min(row)])
    out[which.max(row)] <- paste0('\\cellcolor{red} ', out[which.max(row)])
    out
})
to_table <- t(to_table)
colnames(to_table) <- old_n

print(xtable(to_table), sanitize.text.function = identity)

###############################
# Plots vary beam experiments #
###############################
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryBeam" & algo == "Claude") %>%
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
    filter(experiment == "VaryBeam" & algo == "Claude" & key == "Time") %>%
    select(file, beam_size, Time=value)
            
# Joins!
to_plot <- inner_join(to_plot, time_to_plot, by= c("file"="file", "beam_size" = "beam_size"))

p3 <- ggplot(to_plot, aes(x = Time, y = med_strength, ymin=min_strength, ymax=max_strength)) +
    scale_y_continuous(limits=c(0,1)) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(name = "Execution Time (s)") +
    scale_y_continuous(name = "Normalized Strength") +
    facet_grid(. ~ file, scales="free_x") +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
    geom_ribbon(alpha = 0.3)
    
p3 <- prettify(p3)
print(p3)
ggsave("../documents/plots/view-vary-beam.pdf", p3,
       width = 16, height = 3.5, units = "cm")

##################################
# Prepares diversify experiments #
##################################
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryDeduplication" & algo == "Claude") %>%
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
        geom_point(size = 0.4) +
        geom_line() +
        facet_grid(. ~ file, scales="free_x")


p4 <- prettify(p4)
print(p4)

ggsave("../documents/plots/view-vary-diversification.pdf", p4,
       width = 16, height = 3.5, units = "cm")        


#ggsave("../documents/plots/tmp_column-select-score.pdf", algo_accuracy, width = 8.5, height = 2.25)
#ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
#ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 8.5, height = 2.25)
