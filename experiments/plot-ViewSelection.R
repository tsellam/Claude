#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

#########
# UTILS #
#########
mark_missing_values <- function(df){
    
}

###############
# PREPARATION #
###############
log_file <- read.delim("~/Projects/TurboGroups/experiments/results-04:12/results.agg.log",
                       stringsAsFactors=FALSE)
out_file <- read.delim("~/Projects/TurboGroups/experiments/results-04:12/results.agg.out",
                       stringsAsFactors=FALSE)

log_file <- log_file %>%
    filter(!(file %in% c("insurance.arff"))) 
out_file <- out_file %>%
    filter(!(file %in% c("insurance.arff")))



log_file$algo <- factor(log_file$algo,
                        levels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"),
                        labels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"))
log_file$size_view <- factor(log_file$size_view, 
                             levels = c(2, 5),
                             labels = c("2 dimensions", "5 dimensions"))
log_file$beam_size <- factor(log_file$beam_size,
                             levels = c(50, 250),
                             labels = c("50", "250"))
out_file <- arrange(out_file, size_view, beam_size, algo)


out_file$algo <- factor(out_file$algo,
                        levels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"),
                        labels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"))
out_file$size_view <- factor(out_file$size_view, 
                             levels = c(2, 5),
                             labels = c("2 dimensions", "5 dimensions"))
out_file$beam_size <- factor(out_file$beam_size, 
                             levels = c(50, 250),
                             labels = c("50", "250"))
out_file <- arrange(out_file, size_view, beam_size, algo)


#############
# PLOTTING #
############
accuracies <- out_file %>%
                filter(key == "Strength") %>%
                mutate(strength = as.numeric(value), 
                       file = sub(".arff", "", file)) %>%
                group_by(file, beam_size, size_view, algo) %>%
                summarize(strength = mean(strength)) %>% 
                data.frame
    
algo_names <- as.character(unique(accuracies$algo))
accuracies <- accuracies %>%
                spread(algo, strength) %>%
                gather_(gather_cols = algo_names,
                        na.rm = FALSE) %>%
                mutate(algo = variable,
                       strength = value,
                       xceed = ifelse(is.na(strength),"X", "")) %>%
                select(-value, -variable) %>%
                data.frame


# Vary Algos
algo_accuracies <- accuracies %>%
                filter(as.numeric(beam_size) == 1) %>%
                group_by(file, beam_size, size_view) %>%
                mutate(strength = strength / max(strength, na.rm = TRUE) * 100) %>%
                data.frame

algo_accuracy <- ggplot(algo_accuracies, aes(x = factor(algo), y=strength, fill=algo, label=xceed)) +
                    scale_x_discrete(labels=NULL, name="") +
                    scale_y_continuous(name = "Strength (% max)", limits=c(0,100)) +
                    geom_bar(stat = "identity", position = "dodge") +
                    geom_text(aes(y=0), 
                              position = position_dodge(width = 0.9),
                              color = "darkgrey",
                              size=1.5) +
                    facet_grid( size_view ~ file)

algo_accuracy <- prettify(algo_accuracy)

plot(algo_accuracy)


# Vary beam size
beam_accuracies <-  accuracies %>%
   filter(as.numeric(size_view) == 1) %>%
    group_by(file) %>%
    mutate(strength = strength / max(strength, na.rm = TRUE) * 100) %>%
    data.frame

beam_accuracy <- ggplot(beam_accuracies, 
                        aes(x = beam_size, y=strength, fill=algo, label=xceed)) +
    scale_x_discrete(name = "Beam Size") +
    scale_y_continuous(name = "Strength (% max)", limits=c(0,100)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(y=0), 
              position = position_dodge(width = 0.9),
              color = "darkgrey",
              size=1.5) +
    facet_grid( . ~ file)

beam_accuracy <- prettify(beam_accuracy)
print(beam_accuracy)


#############
# RUNTIMES! #
#############

runtimes <- log_file %>%
    filter(key == "Time") %>%
    mutate(file = sub(".arff", "", file),
           runtime = value) %>% 
    filter(!algo == "Wrap_NaiveBayes") %>%
    select(-key, -value)

algo_names <- as.character(unique(runtimes$algo))
runtimes <- runtimes %>%
    spread(algo, runtime) %>%
    gather_(gather_cols = algo_names,
            na.rm = FALSE) %>%
    mutate(algo = variable,
           xceed = ifelse(is.na(value) ,"X", "")) %>%
    group_by(file, beam_size) %>%
    mutate(runtime = ifelse(is.na(value), max(value, na.rm=TRUE), value)) %>%
    data.frame


# Vary Algos
algo_runtime <- runtimes %>%
    filter(as.numeric(beam_size) == 1) %>%
    data.frame

algo_runtime <- ggplot(algo_runtime, aes(x = factor(algo), y=runtime, fill=algo, label=xceed)) +
    scale_x_discrete(labels=NULL, name=NULL) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(y=runtime / 2), 
              position = position_dodge(width = 0.9),
              color = "darkgrey",
              size=2) +
    facet_grid( file ~ size_view, scales = "free_y")

algo_runtime <- prettify(algo_runtime)

plot(algo_runtime)


ggsave("../documents/plots/tmp_column-select-score-algo.pdf", algo_accuracy, width = 6.66, height = 2)
ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 3.33, height = 5)